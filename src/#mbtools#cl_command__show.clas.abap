CLASS /mbtools/cl_command__show DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Command - Show
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    INTERFACES if_badi_interface.
    INTERFACES /mbtools/if_command.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_command TYPE REF TO /mbtools/cl_command.

    CLASS-METHODS show_tool
      IMPORTING
        !is_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS show_message
      IMPORTING
        !iv_message    TYPE /mbtools/if_definitions=>ty_name
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS show_parameter
      IMPORTING
        !iv_parameter  TYPE string
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS show_code
      IMPORTING
        !is_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS is_code_command
      IMPORTING
        !iv_object_name  TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS get_object_from_tcode
      IMPORTING
        !iv_object_name     TYPE csequence
      RETURNING
        VALUE(rs_tadir_key) TYPE /mbtools/if_definitions=>ty_tadir_key.

ENDCLASS.



CLASS /mbtools/cl_command__show IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_value       TYPE string,
      lv_object      TYPE string,
      lv_object_name TYPE string,
      lv_tadir_count TYPE i,
      ls_tadir_key   TYPE /mbtools/if_definitions=>ty_tadir_key.

    " First, check if it's just a property
    lv_value = /mbtools/cl_utilities=>get_profile_parameter( iv_parameters ).

    IF lv_value <> /mbtools/cl_utilities=>c_value-unknown.
      cv_exit = show_parameter( /mbtools/cl_utilities=>get_profile_parameter_name( iv_parameters ) ).
    ELSE.

      " Split parameters into object and object name
      go_command->split(
        EXPORTING
          iv_parameters = iv_parameters
        IMPORTING
          ev_operator   = lv_object
          ev_operand    = lv_object_name ).

      " Find objects
      go_command->select(
        iv_object   = lv_object
        iv_obj_name = lv_object_name ).

      " Add object texts
      go_command->text( ).

      DO.
        " Pick exactly one object
        TRY.
            go_command->pick(
              IMPORTING
                es_tadir_key = ls_tadir_key
                ev_count     = lv_tadir_count ).
          CATCH /mbtools/cx_exception.
            EXIT.
        ENDTRY.

        " Show object definition
        IF is_code_command( lv_object_name ) = abap_true.
          cv_exit = show_code( ls_tadir_key ).
        ELSE.
          cv_exit = show_tool( ls_tadir_key ).
        ENDIF.

        IF lv_tadir_count = 1.
          EXIT.
        ENDIF.
      ENDDO.

    ENDIF.

  ENDMETHOD.


  METHOD /mbtools/if_command~get_commands.

    FIELD-SYMBOLS <ls_command> LIKE LINE OF ct_commands.

    APPEND INITIAL LINE TO ct_commands ASSIGNING <ls_command>.
    <ls_command>-command     = 'SHOW'.
    <ls_command>-shortcut    = '#'.
    <ls_command>-description = 'Show'.

  ENDMETHOD.


  METHOD class_constructor.

    CREATE OBJECT go_command.

  ENDMETHOD.


  METHOD get_object_from_tcode.

    DATA:
      lv_tcode  TYPE tstc-tcode,
      lv_param  TYPE tstcp-param,
      lt_param  TYPE string_table,
      ls_report TYPE srepovari,
      ls_mtdkey TYPE seocpdkey,
      lv_object TYPE c LENGTH 120.

    lv_tcode = iv_object_name.

    rs_tadir_key-pgmid = 'R3TR'.

    " Report transaction
    CALL FUNCTION 'SRT_GET_REPORT_OF_TCODE'
      EXPORTING
        tcode                 = lv_tcode
      IMPORTING
        report_structure      = ls_report
      EXCEPTIONS
        no_report_transaction = 1
        OTHERS                = 2.
    IF sy-subrc = 0 AND ls_report-report IS NOT INITIAL.
      rs_tadir_key-object   = 'PROG'.
      rs_tadir_key-obj_name = ls_report-report.
      RETURN.
    ENDIF.

    " Parameter Transaction
    SELECT SINGLE param INTO lv_param FROM tstcp WHERE tcode = lv_tcode.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT lv_param AT ';' INTO TABLE lt_param.

    FIND REGEX '\\PROGRAM=(.+)\\CLASS' IN TABLE lt_param SUBMATCHES lv_object.
    IF sy-subrc <> 0.
      FIND REGEX 'RS38M-PROGRAMM=(.+)' IN TABLE lt_param SUBMATCHES lv_object ##SUBRC_OK.
    ENDIF.

    IF lv_object IS NOT INITIAL.
      rs_tadir_key-object   = 'PROG'.
      rs_tadir_key-obj_name = lv_object.
      RETURN.
    ENDIF.

    FIND REGEX '\\CLASS=(.+)\\METHOD=(.+)' IN TABLE lt_param SUBMATCHES ls_mtdkey-clsname ls_mtdkey-cpdname.
    IF sy-subrc <> 0.
      FIND REGEX 'CLASS=(.+)' IN TABLE lt_param SUBMATCHES ls_mtdkey-clsname ##SUBRC_OK.
      FIND REGEX 'METHOD=(.+)' IN TABLE lt_param SUBMATCHES ls_mtdkey-cpdname ##SUBRC_OK.
    ENDIF.

    IF ls_mtdkey-clsname IS NOT INITIAL.
      rs_tadir_key-object   = 'CLAS'.
      rs_tadir_key-obj_name = ls_mtdkey-clsname.
      IF ls_mtdkey-cpdname IS NOT INITIAL.
        rs_tadir_key-object   = 'PROG'.
        rs_tadir_key-obj_name = cl_oo_classname_service=>get_method_include( ls_mtdkey ).
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    FIND REGEX 'TABLENAME=(.+)' IN TABLE lt_param SUBMATCHES lv_object.
    IF sy-subrc = 0.
      rs_tadir_key-object   = 'TABL'.
      rs_tadir_key-obj_name = lv_object.
      RETURN.
    ENDIF.

    FIND REGEX 'VIEWNAME=(.+)' IN TABLE lt_param SUBMATCHES lv_object.
    IF sy-subrc = 0.
      rs_tadir_key-object   = 'VIEW'.
      rs_tadir_key-obj_name = lv_object.
      RETURN.
    ENDIF.

    FIND REGEX 'VCLNAME=(.+)' IN TABLE lt_param SUBMATCHES lv_object.
    IF sy-subrc = 0.
      rs_tadir_key-object   = 'VIEW'.
      rs_tadir_key-obj_name = lv_object.
      RETURN.
    ENDIF.

    FIND REGEX 'APPLICATION=(.+)' IN TABLE lt_param SUBMATCHES lv_object.
    IF sy-subrc = 0.
      rs_tadir_key-object   = 'WDYA'.
      rs_tadir_key-obj_name = lv_object.
      RETURN.
    ENDIF.

    FIND REGEX 'TNRO-OBJECT=(.+)' IN TABLE lt_param SUBMATCHES lv_object.
    IF sy-subrc = 0.
      rs_tadir_key-object   = 'NROB'.
      rs_tadir_key-obj_name = lv_object.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD is_code_command.

    DATA lv_len TYPE i.

    lv_len = strlen( iv_object_name ) - 1.

    rv_result = boolc( lv_len > 0 AND iv_object_name+lv_len(1) = '#' ).

  ENDMETHOD.


  METHOD show_code.

    DATA ls_tadir_key LIKE is_tadir_key.

    " Show ABAP code corresponding to object
    ls_tadir_key = is_tadir_key.

    IF ls_tadir_key-pgmid = 'R3TR' AND ls_tadir_key-object = 'TRAN'.
      " transactions -> program / class / view
      ls_tadir_key = get_object_from_tcode( ls_tadir_key-obj_name ).
    ENDIF.

    IF ls_tadir_key = is_tadir_key.
      MESSAGE 'No corresponding ABAP code found' TYPE 'S'.
    ELSE.
      rv_exit = /mbtools/cl_sap=>show_object(
        iv_pgmid    = ls_tadir_key-pgmid
        iv_object   = ls_tadir_key-object
        iv_obj_name = ls_tadir_key-obj_name ).
    ENDIF.

  ENDMETHOD.


  METHOD show_message.

    DATA:
      lv_msgid TYPE sy-msgid,
      lv_msgno TYPE sy-msgno.

    go_command->split_message(
      EXPORTING
        iv_message = iv_message
      IMPORTING
        ev_msgid   = lv_msgid
        ev_msgno   = lv_msgno ).

    IF lv_msgid IS NOT INITIAL AND lv_msgno IS NOT INITIAL.
      " Display message with placeholders for parameters
      MESSAGE ID lv_msgid TYPE 'I' NUMBER lv_msgno
        WITH '&1' '&2' '&3' '&4'.

      rv_exit = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD show_parameter.

    DATA lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'RSPFLDOC'.
    <ls_bdcdata>-dynpro   = '1000'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_CURSOR'.
    <ls_bdcdata>-fval = 'TPFYSTRUCT-NAME'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'TPFYSTRUCT-NAME'.
    <ls_bdcdata>-fval = iv_parameter.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=PDIS'.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      EXPORTING
        tcode                   = 'RZ11'
        mode_val                = 'E'
      TABLES
        using_tab               = lt_bdcdata
      EXCEPTIONS
        call_transaction_denied = 1
        tcode_invalid           = 2
        OTHERS                  = 3.
    IF sy-subrc = 0.
      rv_exit = abap_true.
    ELSE.
      MESSAGE s000 WITH 'Navigation not available'(001).
    ENDIF.

  ENDMETHOD.


  METHOD show_tool.

    " Show message in popup instead of editor
    IF is_tadir_key-pgmid  = /mbtools/if_command_field=>c_pgmid-limu AND
       is_tadir_key-object = /mbtools/if_command_field=>c_objects_limu-mess.

      rv_exit = show_message( is_tadir_key-obj_name ).

    ELSE.

      rv_exit = /mbtools/cl_sap=>show_object(
        iv_pgmid    = is_tadir_key-pgmid
        iv_object   = is_tadir_key-object
        iv_obj_name = is_tadir_key-obj_name ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
