CLASS /mbtools/cl_command_field DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Command Field
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    CLASS-METHODS do_command
      IMPORTING
        !is_help_infos TYPE help_info
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS do_help_start
      IMPORTING
        !iv_called_for_tab TYPE help_info-tabname
      RETURNING
        VALUE(rv_exit)     TYPE abap_bool.

    CLASS-METHODS show_result
      IMPORTING
        !iv_command    TYPE csequence
        !iv_parameters TYPE csequence
        !iv_icon       TYPE icon_d
        !iv_result     TYPE string
        !iv_via_popup  TYPE abap_bool
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

  PROTECTED SECTION.

    CLASS-METHODS execute_command
      IMPORTING
        !iv_input      TYPE csequence
        !iv_via_popup  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.
    CLASS-METHODS popup_command
      IMPORTING
        !iv_input      TYPE csequence
        !iv_icon       TYPE icon_d OPTIONAL
        !iv_result     TYPE string OPTIONAL
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.
  PRIVATE SECTION.

    CONSTANTS:
      c_command_result   TYPE tabname VALUE '/MBTOOLS/COMMAND_RESULT',
      c_break_chars      TYPE string VALUE '=+*/; ' ##NO_TEXT,
      c_max_len_msg      TYPE i VALUE 50,
      c_max_len_result   TYPE i VALUE 75,
      c_max_lines_result TYPE i VALUE 10.

    CLASS-DATA:
      gt_commands TYPE /mbtools/if_command=>ty_commands,
      gs_infos    TYPE help_info.

    CLASS-METHODS input_check
      IMPORTING
        !iv_input         TYPE csequence
      RETURNING
        VALUE(rv_command) TYPE string.

    CLASS-METHODS input_split
      IMPORTING
        !iv_input      TYPE csequence
      EXPORTING
        !ev_command    TYPE string
        !ev_parameters TYPE string.

ENDCLASS.



CLASS /mbtools/cl_command_field IMPLEMENTATION.


  METHOD class_constructor.

    CONSTANTS lc_command_init TYPE c LENGTH 40 VALUE 'INIT' ##NEEDED.

    DATA li_badi TYPE REF TO /mbtools/bc_command_badi.

    " INIT needs to be defined as filter for all command implementations
    GET BADI li_badi
      FILTERS
        command = lc_command_init.

    ASSERT li_badi IS BOUND.

    CALL BADI li_badi->get_commands
      CHANGING
        ct_commands = gt_commands.

  ENDMETHOD.


  METHOD do_command.

    IF is_help_infos-call = 'D' AND is_help_infos-docuid = 'OK'.

      IF gs_infos IS INITIAL.
        gs_infos = is_help_infos.

        SET TITLEBAR 'OBJECT_SELECTION' OF PROGRAM '/MBTOOLS/BC_COMMAND_FIELD'.
        rv_exit = popup_command( is_help_infos-menufunct ).
      ENDIF.

    ELSEIF is_help_infos-call = 'H'.

      SET TITLEBAR 'OBJECT_SELECTION' OF PROGRAM '/MBTOOLS/BC_COMMAND_FIELD'.
      rv_exit = execute_command( is_help_infos-menufunct ).

    ENDIF.

    IF rv_exit = abap_true.
      CLEAR gs_infos.
    ENDIF.

  ENDMETHOD.


  METHOD do_help_start.

    DATA:
      lt_dynpselect   TYPE TABLE OF dselc,
      lt_dynpvaluetab TYPE TABLE OF dval.

    IF iv_called_for_tab = c_command_result AND gs_infos IS NOT INITIAL.

      CALL FUNCTION 'HELP_START'
        EXPORTING
          help_infos   = gs_infos
        TABLES
          dynpselect   = lt_dynpselect
          dynpvaluetab = lt_dynpvaluetab
        EXCEPTIONS
          OTHERS       = 1.

      IF sy-subrc = 0.
        CLEAR gs_infos.
        rv_exit = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD execute_command.

    CONSTANTS lc_max_len TYPE i VALUE 20.

    DATA:
      lv_checked_input TYPE string,
      lv_command       TYPE string,
      lv_parameters    TYPE string,
      li_command       TYPE REF TO /mbtools/bc_command_badi.

    " Check if command is valid
    lv_checked_input = input_check( iv_input ).

    CHECK lv_checked_input IS NOT INITIAL.

    LOG-POINT ID /mbtools/bc
      SUBKEY /mbtools/cl_tool_bc_cl=>c_tool-title
      FIELDS sy-datum sy-uzeit sy-uname.

    " If checked_input is longer than standard ok-code
    IF strlen( lv_checked_input ) > ( lc_max_len - 1 ) AND iv_via_popup = abap_false.
      MESSAGE s002 WITH lc_max_len.
      rv_exit = popup_command( lv_checked_input ).
      RETURN.
    ENDIF.

    " Split command line into command and parameters
    input_split(
      EXPORTING
        iv_input      = lv_checked_input
      IMPORTING
        ev_command    = lv_command
        ev_parameters = lv_parameters ).

    IF lv_command IS INITIAL.
      RETURN.
    ENDIF.

    GET BADI li_command
      FILTERS
        command = lv_command.

    IF li_command IS BOUND.
      CALL BADI li_command->execute
        EXPORTING
          iv_command    = lv_command
          iv_parameters = lv_parameters
          iv_via_popup  = iv_via_popup
        CHANGING
          cv_exit       = rv_exit.
    ENDIF.

  ENDMETHOD.


  METHOD input_check.

    " Standard SAP ok-codes for help_start
    IF iv_input CP 'HC1*' OR iv_input CP 'SG1*' OR iv_input = 'GLOS' OR iv_input CP 'H_*' OR
       iv_input = 'MSDW' OR iv_input = 'STAT' OR iv_input = 'SO75' OR iv_input = 'SO99' OR
       iv_input = 'SMSG' OR iv_input = '-HLDWP' OR iv_input = '-HLD' OR iv_input = 'ERHI' OR
       iv_input = 'DOCU' OR iv_input = 'HF1C' OR iv_input = 'HFND' OR iv_input = 'HNET' OR
       iv_input = '$S_WPB' OR iv_input = '$SHOP' OR strlen( iv_input ) < 3.
      RETURN.
    ENDIF.

    rv_command = iv_input.
    CONDENSE rv_command.
    SHIFT rv_command LEFT DELETING LEADING space.

  ENDMETHOD.


  METHOD input_split.

    " Command shortcut or command word
    READ TABLE gt_commands TRANSPORTING NO FIELDS
      WITH KEY shortcut = iv_input(1).
    IF sy-subrc = 0.
      " Command shortcuts can be followed by parameters immediatly
      ev_command    = iv_input(1).
      ev_parameters = iv_input+1(*).
    ELSE.
      " Command words are separated from parameters by a space
      SPLIT iv_input AT space INTO ev_command ev_parameters.
    ENDIF.

    TRANSLATE ev_command TO UPPER CASE.
    SHIFT ev_parameters LEFT DELETING LEADING space.

  ENDMETHOD.


  METHOD popup_command.

    DATA:
      lv_input  TYPE string,
      lt_result TYPE string_table.

    " Always exit command field, for commands processed via popup
    rv_exit = abap_true.

    " If given, show lt_result of previous command
    IF iv_result IS NOT INITIAL.
      CALL FUNCTION 'RSS_LINE_SPLIT'
        EXPORTING
          i_input                 = iv_result
          i_line_width            = c_max_len_result
          i_delimiters            = ''
          i_break_chars           = c_break_chars
          i_line_comment_char     = ''
          i_comment_char          = ''
        CHANGING
          c_t_lines               = lt_result
        EXCEPTIONS
          input_invalid_type      = 1
          unbalanced_delimiters   = 2
          no_break_position_found = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        MESSAGE e000 WITH 'Error in RSS_LINE_SPLIT' sy-subrc ##NO_TEXT.
        RETURN.
      ENDIF.

      IF lines( lt_result ) > c_max_lines_result.
        MESSAGE w003 WITH c_max_lines_result.
      ENDIF.
    ENDIF.

    CALL FUNCTION '/MBTOOLS/COMMAND_FIELD_POPUP'
      EXPORTING
        iv_input  = iv_input
        iv_icon   = iv_icon
        it_result = lt_result
      IMPORTING
        ev_input  = lv_input
      EXCEPTIONS
        cancelled = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    execute_command(
      iv_input     = lv_input
      iv_via_popup = abap_true ).

  ENDMETHOD.


  METHOD show_result.

    DATA:
      lv_len   TYPE i,
      lv_input TYPE string,
      lt_mess  TYPE TABLE OF sy-msgv1,
      lv_msgv1 TYPE sy-msgv1,
      lv_msgv2 TYPE sy-msgv2,
      lv_msgv3 TYPE sy-msgv3,
      lv_msgv4 TYPE sy-msgv4.

    lv_len = strlen( iv_result ).

    IF lv_len > ( c_max_len_msg * 4 ) OR iv_via_popup = abap_true.
      " Ouput via popup
      CONCATENATE iv_command iv_parameters INTO lv_input SEPARATED BY space.

      rv_exit = popup_command( iv_input  = lv_input
                               iv_icon   = iv_icon
                               iv_result = iv_result ).
    ELSE.
      " Ouput via message statement
      CALL FUNCTION 'RSS_LINE_SPLIT'
        EXPORTING
          i_input                 = iv_result
          i_line_width            = c_max_len_msg
          i_delimiters            = ''
          i_break_chars           = c_break_chars
          i_line_comment_char     = ''
          i_comment_char          = ''
        CHANGING
          c_t_lines               = lt_mess
        EXCEPTIONS
          input_invalid_type      = 1
          unbalanced_delimiters   = 2
          no_break_position_found = 3
          OTHERS                  = 4.
      IF sy-subrc = 0.
        READ TABLE lt_mess INTO lv_msgv1 INDEX 1 ##SUBRC_OK.
        READ TABLE lt_mess INTO lv_msgv2 INDEX 2 ##SUBRC_OK.
        READ TABLE lt_mess INTO lv_msgv3 INDEX 3 ##SUBRC_OK.
        READ TABLE lt_mess INTO lv_msgv4 INDEX 4 ##SUBRC_OK.
      ELSE.
        lv_msgv1 = iv_result+0(*).
        IF lv_len > c_max_len_msg.
          lv_msgv2 = iv_result+c_max_len_msg(*).
        ENDIF.
        IF lv_len > c_max_len_msg * 2.
          lv_msgv3 = iv_result+100(*).
        ENDIF.
        IF lv_len > c_max_len_msg * 3.
          lv_msgv4 = iv_result+150(*).
        ENDIF.
      ENDIF.

      MESSAGE s000 WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4.

      rv_exit = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
