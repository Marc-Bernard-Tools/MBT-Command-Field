CLASS /mbtools/cl_command_field DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Command Field
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.
    TYPE-POOLS icon .

    CLASS-METHODS execute_command
      IMPORTING
        !iv_input      TYPE csequence
        !iv_via_popup  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
    CLASS-METHODS popup_command
      IMPORTING
        !iv_input      TYPE csequence
        !iv_icon       TYPE icon_d OPTIONAL
        !iv_result     TYPE string OPTIONAL
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
    CLASS-METHODS show_result
      IMPORTING
        !iv_command    TYPE csequence
        !iv_parameters TYPE csequence
        !iv_icon       TYPE icon_d
        !iv_result     TYPE string
        !iv_via_popup  TYPE abap_bool
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
    CLASS-METHODS set_infos
      IMPORTING
        !is_infos TYPE help_info .
    CLASS-METHODS get_infos
      RETURNING
        VALUE(rs_infos) TYPE help_info .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS c_break_chars TYPE string VALUE '=+*/; ' ##NO_TEXT.
    CONSTANTS c_max_len_msg TYPE i VALUE 50 ##NO_TEXT.
    CONSTANTS c_max_len_result TYPE i VALUE 75 ##NO_TEXT.
    CONSTANTS c_max_lines_result TYPE i VALUE 10 ##NO_TEXT.
    CLASS-DATA gs_infos TYPE help_info .

    CLASS-METHODS input_check
      IMPORTING
        !iv_input         TYPE csequence
      RETURNING
        VALUE(rv_command) TYPE string .
    CLASS-METHODS input_split
      IMPORTING
        !iv_input      TYPE csequence
      EXPORTING
        !ev_command    TYPE string
        !ev_parameters TYPE string .
ENDCLASS.



CLASS /mbtools/cl_command_field IMPLEMENTATION.


  METHOD execute_command.

    CONSTANTS: lc_max_len TYPE i VALUE 20.

    DATA:
      lv_checked_input TYPE string,
      lv_command       TYPE string,
      lv_parameters    TYPE string,
      ls_cmds          TYPE /mbtools/cmds,
      ls_clskey        TYPE seoclskey,
      lo_command       TYPE REF TO /mbtools/if_command.

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

    SELECT SINGLE * FROM /mbtools/cmds INTO ls_cmds
      WHERE command = lv_command OR shortcut = lv_command.
    IF sy-subrc <> 0.
      " Invalid Command
      IF iv_via_popup = abap_true.
        MESSAGE w001.
        rv_exit = abap_true.
      ELSE.
        rv_exit = abap_false.
      ENDIF.

      RETURN.
    ENDIF.

    ls_clskey-clsname = ls_cmds-clsname.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = ls_clskey
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    IF sy-subrc <> 0.
      MESSAGE e005 WITH ls_cmds-clsname ls_cmds-command.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_command TYPE (ls_cmds-clsname).

    rv_exit = lo_command->execute(
      iv_command    = lv_command
      iv_parameters = lv_parameters
      iv_via_popup  = iv_via_popup ).

  ENDMETHOD.


  METHOD get_infos.
    rs_infos = gs_infos.
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

    DATA ls_cmds TYPE /mbtools/cmds.

    " Command shortcut or command word
    SELECT SINGLE * FROM /mbtools/cmds INTO ls_cmds WHERE shortcut = iv_input(1).
    IF sy-subrc = 0.
      ev_command    = iv_input(1).
      ev_parameters = iv_input+1(*).
    ELSE.
      SPLIT iv_input AT space INTO ev_command ev_parameters.
    ENDIF.

    TRANSLATE ev_command TO UPPER CASE.
    SHIFT ev_parameters LEFT DELETING LEADING space.

  ENDMETHOD.


  METHOD popup_command.

    DATA:
      lt_field  TYPE TABLE OF sval,
      lt_result TYPE TABLE OF sval-value,
      lv_tabix  TYPE n LENGTH 2,
      lv_answer TYPE c LENGTH 1.

    FIELD-SYMBOLS:
      <lv_field>  TYPE sval,
      <lv_result> TYPE any.

    " Always exit command field, for commands processed via popup
    rv_exit = abap_true.

    APPEND INITIAL LINE TO lt_field ASSIGNING <lv_field>.
    <lv_field>-tabname    = '/MBTOOLS/BC_COMMAND'.
    <lv_field>-fieldname  = 'COMMAND'.
    <lv_field>-fieldtext  = icon_greater && 'Command'(040).
    <lv_field>-value      = iv_input.

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

      LOOP AT lt_result ASSIGNING <lv_result> TO c_max_lines_result.
        lv_tabix = sy-tabix.
        APPEND INITIAL LINE TO lt_field ASSIGNING <lv_field>.
        <lv_field>-tabname   = '/MBTOOLS/BC_COMMAND'.
        <lv_field>-fieldname = 'RESULT_' && lv_tabix.
        IF lv_tabix = 1.
          IF iv_icon = icon_message_error_small.
            <lv_field>-fieldtext = iv_icon && 'Error'(042).
          ELSE.
            <lv_field>-fieldtext = iv_icon && 'Result'(041).
          ENDIF.
        ELSE.
          <lv_field>-fieldtext = icon_space.
        ENDIF.
        <lv_field>-field_attr = '02'. "no entry
        <lv_field>-value      = <lv_result>.
      ENDLOOP.

      IF lines( lt_result ) > c_max_lines_result.
        MESSAGE w003 WITH c_max_lines_result.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = /mbtools/cl_tool_bc_cl=>c_tool-title
      IMPORTING
        returncode      = lv_answer
      TABLES
        fields          = lt_field
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error in POPUP_GET_VALUES' ##NO_TEXT.
      RETURN.
    ELSEIF lv_answer = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_field INDEX 1 ASSIGNING <lv_field>.
    ASSERT sy-subrc = 0.

    execute_command( iv_input     = <lv_field>-value
                     iv_via_popup = abap_true ).

  ENDMETHOD.


  METHOD set_infos.
    gs_infos = is_infos.
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
        READ TABLE lt_mess INTO lv_msgv1 INDEX 1.         "#EC CI_SUBRC
        READ TABLE lt_mess INTO lv_msgv2 INDEX 2.         "#EC CI_SUBRC
        READ TABLE lt_mess INTO lv_msgv3 INDEX 3.         "#EC CI_SUBRC
        READ TABLE lt_mess INTO lv_msgv4 INDEX 4.         "#EC CI_SUBRC
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
