************************************************************************
* /MBTOOLS/CL_COMMAND_FIELD
* MBT Command Field
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
class /MBTOOLS/CL_COMMAND_FIELD definition
  public
  final
  create public .

public section.
  type-pools ICON .

  interfaces ZIF_APACK_MANIFEST .
  interfaces /MBTOOLS/IF_MANIFEST .

  constants C_VERSION type STRING value '1.0.0' ##NO_TEXT.
  constants C_NAME type STRING value 'MBT_Command_Field' ##NO_TEXT.
  constants C_TITLE type STRING value 'MBT Command Field' ##NO_TEXT.
  constants C_DESCRIPTION type STRING value 'Enhancement for SAP GUI Command Field' ##NO_TEXT.
  constants C_URI type STRING value 'https://marcbernardtools.com/tool/mbt-command-field/' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods EXECUTE_COMMAND
    importing
      !I_INPUT type CSEQUENCE
      !I_VIA_POPUP type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_EXIT) type ABAP_BOOL .
  class-methods POPUP_COMMAND
    importing
      !I_INPUT type CSEQUENCE
      !I_ICON type ICON_D optional
      !I_RESULT type STRING optional
    returning
      value(R_EXIT) type ABAP_BOOL .
  class-methods SHOW_RESULT
    importing
      !I_COMMAND type CSEQUENCE
      !I_PARAMETERS type CSEQUENCE
      !I_ICON type ICON_D
      !I_RESULT type STRING
      !I_VIA_POPUP type ABAP_BOOL
    returning
      value(R_EXIT) type ABAP_BOOL .
  PROTECTED SECTION.

private section.

  aliases APACK_MANIFEST
    for ZIF_APACK_MANIFEST~DESCRIPTOR .
  aliases MBT_MANIFEST
    for /MBTOOLS/IF_MANIFEST~DESCRIPTOR .

  constants C_BREAK_CHARS type STRING value '=+*/; ' ##NO_TEXT.
  constants C_MAX_LEN_MSG type I value 50 ##NO_TEXT.
  constants C_MAX_LEN_RESULT type I value 75 ##NO_TEXT.
  constants C_MAX_LINES_RESULT type I value 10 ##NO_TEXT.

  class-methods INPUT_CHECK
    importing
      !I_INPUT type CSEQUENCE
    returning
      value(R_COMMAND) type STRING .
  class-methods INPUT_SPLIT
    importing
      !I_INPUT type CSEQUENCE
    exporting
      !E_COMMAND type STRING
      !E_PARAMETERS type STRING .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND_FIELD IMPLEMENTATION.


  METHOD constructor.
    " APACK
    apack_manifest = VALUE #(
      group_id    = 'github.com/mbtools/mbt-bc-cl'
      artifact_id = 'com.marcbernardtools.abap.bc_cl'
      version     = c_version
      git_url     = 'https://github.com/mbtools/mbt-bc-cl.git'
    ) ##NO_TEXT.
    " MBT
    mbt_manifest = VALUE #(
      id          = 3
      name        = c_name
      version     = c_version
      title       = c_title
      description = c_description
      mbt_url     = 'https://marcbernardtools.com/tool/mbt-command-field/'
      namespace   = '/MBTOOLS/'
      package     = '/MBTOOLS/BC_CL'
    ) ##NO_TEXT.
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

    LOG-POINT ID /mbtools/bc SUBKEY c_name FIELDS sy-datum sy-uzeit sy-uname.

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

        CREATE OBJECT command_object TYPE /mbtools/cl_command_find.

      WHEN /mbtools/if_command_field=>c_commands-show OR
           /mbtools/if_command_field=>c_command_shortcuts-show.

        CREATE OBJECT command_object TYPE /mbtools/cl_command_show.

      WHEN /mbtools/if_command_field=>c_commands-run OR
           /mbtools/if_command_field=>c_command_shortcuts-run.

        CREATE OBJECT command_object TYPE /mbtools/cl_command_run.

      WHEN /mbtools/if_command_field=>c_commands-calc OR
           /mbtools/if_command_field=>c_command_shortcuts-calc.

        CREATE OBJECT command_object TYPE /mbtools/cl_command_calc.

      WHEN /mbtools/if_command_field=>c_commands-curr OR
           /mbtools/if_command_field=>c_command_shortcuts-curr.

        CREATE OBJECT command_object TYPE /mbtools/cl_command_curr.

      WHEN /mbtools/if_command_field=>c_commands-unit OR
           /mbtools/if_command_field=>c_command_shortcuts-unit.

        CREATE OBJECT command_object TYPE /mbtools/cl_command_unit.

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


  METHOD POPUP_COMMAND.

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
