************************************************************************
* /MBTOOLS/CL_COMMAND_FIELD
* MBT Command Field
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_command_field DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS icon .

    INTERFACES if_apack_manifest .
    INTERFACES /mbtools/if_manifest .

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.
    CONSTANTS c_title TYPE string VALUE 'MBT Command Field' ##NO_TEXT.
    CONSTANTS c_description TYPE string VALUE 'The world''s first enhancement for the SAP GUI command field' ##NO_TEXT.
    CONSTANTS c_download_id TYPE i VALUE 4409.

    METHODS constructor .
    CLASS-METHODS execute_command
      IMPORTING
        !i_input      TYPE csequence
        !i_via_popup  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
    CLASS-METHODS popup_command
      IMPORTING
        !i_input      TYPE csequence
        !i_icon       TYPE icon_d OPTIONAL
        !i_result     TYPE string OPTIONAL
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
    CLASS-METHODS show_result
      IMPORTING
        !i_command    TYPE csequence
        !i_parameters TYPE csequence
        !i_icon       TYPE icon_d
        !i_result     TYPE string
        !i_via_popup  TYPE abap_bool
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES apack_manifest
      FOR if_apack_manifest~descriptor .
    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    CONSTANTS c_break_chars TYPE string VALUE '=+*/; ' ##NO_TEXT.
    CONSTANTS c_max_len_msg TYPE i VALUE 50 ##NO_TEXT.
    CONSTANTS c_max_len_result TYPE i VALUE 75 ##NO_TEXT.
    CONSTANTS c_max_lines_result TYPE i VALUE 10 ##NO_TEXT.

    DATA: mr_tool TYPE REF TO /mbtools/cl_tools.

    CLASS-METHODS input_check
      IMPORTING
        !i_input         TYPE csequence
      RETURNING
        VALUE(r_command) TYPE string .
    CLASS-METHODS input_split
      IMPORTING
        !i_input      TYPE csequence
      EXPORTING
        !e_command    TYPE string
        !e_parameters TYPE string .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND_FIELD IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mr_tool EXPORTING i_tool = me.

    apack_manifest = mr_tool->apack_manifest.
    mbt_manifest   = mr_tool->mbt_manifest.
  ENDMETHOD.


  METHOD execute_command.

    DATA:
      checked_input  TYPE string,
      command        TYPE string,
      parameters     TYPE string,
      command_object TYPE REF TO /mbtools/if_command.

    " Check if command is valid
    checked_input = input_check( i_input ).

    CHECK NOT checked_input IS INITIAL.

    LOG-POINT ID /mbtools/bc
      SUBKEY c_title
      FIELDS sy-datum sy-uzeit sy-uname.

    " If checked_input is longer than standard ok-code
    IF strlen( checked_input ) > 18 AND i_via_popup = abap_false.
      MESSAGE s002 WITH 20 132.
      r_exit = popup_command( checked_input ).
      RETURN.
    ENDIF.

    " Split command line into command and parameters
    input_split(
      EXPORTING
        i_input      = checked_input
      IMPORTING
        e_command    = command
        e_parameters = parameters ).

    CHECK NOT command IS INITIAL AND NOT parameters IS INITIAL.

    CASE command.
      WHEN /mbtools/if_command_field=>c_commands-find OR
           /mbtools/if_command_field=>c_command_shortcuts-find.

        CREATE OBJECT command_object TYPE /mbtools/cl_command__find.

      WHEN /mbtools/if_command_field=>c_commands-show OR
           /mbtools/if_command_field=>c_command_shortcuts-show.

        CREATE OBJECT command_object TYPE /mbtools/cl_command__show.

      WHEN /mbtools/if_command_field=>c_commands-run OR
           /mbtools/if_command_field=>c_command_shortcuts-run.

        CREATE OBJECT command_object TYPE /mbtools/cl_command__run.

      WHEN /mbtools/if_command_field=>c_commands-calc OR
           /mbtools/if_command_field=>c_command_shortcuts-calc.

        CREATE OBJECT command_object TYPE /mbtools/cl_command__calc.

      WHEN /mbtools/if_command_field=>c_commands-curr OR
           /mbtools/if_command_field=>c_command_shortcuts-curr.

        CREATE OBJECT command_object TYPE /mbtools/cl_command__curr.

      WHEN /mbtools/if_command_field=>c_commands-unit OR
           /mbtools/if_command_field=>c_command_shortcuts-unit.

        CREATE OBJECT command_object TYPE /mbtools/cl_command__unit.

      WHEN OTHERS. " Invalid Command
        IF i_via_popup = abap_true.
          MESSAGE w001.
          r_exit = abap_true.
        ELSE.
          r_exit = abap_false.
        ENDIF.
        RETURN.
    ENDCASE.

    r_exit = command_object->execute( i_command    = command
                                      i_parameters = parameters
                                      i_via_popup  = i_via_popup ).

  ENDMETHOD.


  METHOD input_check.

    " Standard SAP ok-codes for help_start
    IF i_input CP 'HC1*' OR i_input CP 'SG1*' OR i_input = 'GLOS' OR i_input CP 'H_*' OR
       i_input = 'MSDW' OR i_input = 'STAT' OR i_input = 'SO75' OR i_input = 'SO99' OR
       i_input = 'SMSG' OR i_input = '-HLDWP' OR i_input = '-HLD' OR i_input = 'ERHI' OR
       i_input = 'DOCU' OR i_input = 'HF1C' OR i_input = 'HFND' OR i_input = 'HNET' OR
       i_input = '$S_WPB' OR i_input = '$SHOP' OR strlen( i_input ) < 3.
      RETURN.
    ENDIF.

    r_command = i_input.
    CONDENSE r_command.
    SHIFT r_command LEFT DELETING LEADING space.

  ENDMETHOD.


  METHOD input_split.

    " Command shortcut or command word
    IF i_input(1) CA /mbtools/if_command_field=>c_command_shortcuts.
      e_command = i_input(1).
      e_parameters = i_input+1(*).
    ELSE.
      SPLIT i_input AT space INTO e_command e_parameters.
      TRANSLATE e_command TO UPPER CASE.
    ENDIF.

    SHIFT e_parameters LEFT DELETING LEADING space.

  ENDMETHOD.


  METHOD popup_command.

    DATA:
      fields  TYPE TABLE OF sval,
      results TYPE TABLE OF sval-value,
      tabix   TYPE n LENGTH 2,
      answer  TYPE c LENGTH 1.

    FIELD-SYMBOLS:
      <field>  TYPE sval,
      <result> TYPE any.

    " Always exit command field, for commands processed via popup
    r_exit = abap_true.

    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname    = '/MBTOOLS/BC_COMMAND'.
    <field>-fieldname  = 'COMMAND'.
    <field>-fieldtext  = icon_greater && 'Command'(040).
    <field>-value      = i_input.

    " If given, show results of previous command
    IF NOT i_result IS INITIAL.
      CALL FUNCTION 'RSS_LINE_SPLIT'
        EXPORTING
          i_input                 = i_result
          i_line_width            = c_max_len_result
          i_delimiters            = ''
          i_break_chars           = c_break_chars
          i_line_comment_char     = ''
          i_comment_char          = ''
        CHANGING
          c_t_lines               = results
        EXCEPTIONS
          input_invalid_type      = 1
          unbalanced_delimiters   = 2
          no_break_position_found = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        MESSAGE e000 WITH 'Error in RSS_LINE_SPLIT' sy-subrc ##NO_TEXT.
        RETURN.
      ENDIF.

      LOOP AT results ASSIGNING <result> TO c_max_lines_result.
        tabix = sy-tabix.
        APPEND INITIAL LINE TO fields ASSIGNING <field>.
        <field>-tabname   = '/MBTOOLS/BC_COMMAND'.
        <field>-fieldname = 'RESULT_' && tabix.
        IF tabix = 1.
          IF i_icon = icon_message_error_small.
            <field>-fieldtext = i_icon && 'Error'(042).
          ELSE.
            <field>-fieldtext = i_icon && 'Result'(041).
          ENDIF.
        ELSE.
          <field>-fieldtext = icon_space.
        ENDIF.
        <field>-field_attr = '02'. "no entry
        <field>-value      = <result>.
      ENDLOOP.

      IF lines( results ) > c_max_lines_result.
        MESSAGE w003 WITH c_max_lines_result.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = c_title
      IMPORTING
        returncode      = answer
      TABLES
        fields          = fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error in POPUP_GET_VALUES' ##NO_TEXT.
      RETURN.
    ELSEIF answer = 'A'.
      RETURN.
    ENDIF.

    READ TABLE fields INDEX 1 ASSIGNING <field>.
    ASSERT sy-subrc = 0.

    execute_command( i_input     = <field>-value
                     i_via_popup = abap_true ).

  ENDMETHOD.


  METHOD show_result.

    DATA:
      length   TYPE i,
      input    TYPE string,
      messages TYPE TABLE OF sy-msgv1,
      msgv1    TYPE sy-msgv1,
      msgv2    TYPE sy-msgv2,
      msgv3    TYPE sy-msgv3,
      msgv4    TYPE sy-msgv4.

    length = strlen( i_result ).

    IF length > ( c_max_len_msg * 4 ) OR i_via_popup = abap_true.
      " Ouput via popup
      CONCATENATE i_command i_parameters INTO input SEPARATED BY space.

      r_exit = popup_command( i_input  = input
                              i_icon   = i_icon
                              i_result = i_result ).
    ELSE.
      " Ouput via message statement
      CALL FUNCTION 'RSS_LINE_SPLIT'
        EXPORTING
          i_input                 = i_result
          i_line_width            = c_max_len_msg
          i_delimiters            = ''
          i_break_chars           = c_break_chars
          i_line_comment_char     = ''
          i_comment_char          = ''
        CHANGING
          c_t_lines               = messages
        EXCEPTIONS
          input_invalid_type      = 1
          unbalanced_delimiters   = 2
          no_break_position_found = 3
          OTHERS                  = 4.
      IF sy-subrc = 0.
        READ TABLE messages INTO msgv1 INDEX 1.
        READ TABLE messages INTO msgv2 INDEX 2.
        READ TABLE messages INTO msgv3 INDEX 3.
        READ TABLE messages INTO msgv4 INDEX 4.
      ELSE.
        msgv1 = i_result+0(*).
        IF length > c_max_len_msg.
          msgv2 = i_result+c_max_len_msg(*).
        ENDIF.
        IF length > c_max_len_msg * 2.
          msgv3 = i_result+100(*).
        ENDIF.
        IF length > c_max_len_msg * 3.
          msgv4 = i_result+150(*).
        ENDIF.
      ENDIF.

      MESSAGE s000 WITH msgv1 msgv2 msgv3 msgv4.

      r_exit = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
