CLASS /mbtools/cl_command__run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Command - Run
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

    CLASS-METHODS run_mbt
      IMPORTING
        !iv_obj_name   TYPE string
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS run_listcube
      IMPORTING
        !is_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS run_tabl
      IMPORTING
        !is_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS run_func
      IMPORTING
        !is_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS run_prog
      IMPORTING
        !is_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    CLASS-METHODS run_tran
      IMPORTING
        !is_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

ENDCLASS.



CLASS /mbtools/cl_command__run IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_object      TYPE string,
      lv_object_name TYPE string,
      lv_tadir_count TYPE i,
      ls_tadir_key   TYPE /mbtools/if_definitions=>ty_tadir_key.

    " Split parameters into object and object name
    go_command->split(
      EXPORTING
        iv_parameters = iv_parameters
      IMPORTING
        ev_operator   = lv_object
        ev_operand    = lv_object_name ).

    IF lv_object IS INITIAL.
      CONCATENATE
        /mbtools/if_command_field=>c_objects_db
        /mbtools/if_command_field=>c_objects_bw
        /mbtools/if_command_field=>c_objects_exec
        INTO lv_object SEPARATED BY ','.
    ENDIF.

    " Select objects
    go_command->select(
      iv_object   = lv_object
      iv_obj_name = lv_object_name ).

    " Check if command is a Marc Bernard Tools
    IF lv_object CS /mbtools/if_command_field=>c_objects_exec-tran.
      cv_exit = run_mbt( lv_object_name ).
      IF cv_exit = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    " Filter table types to ones that work in SE16
    go_command->filter_tabl( ).

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

      " Run object
      CASE ls_tadir_key-object.
        WHEN /mbtools/if_command_field=>c_objects_db-tabl OR
             /mbtools/if_command_field=>c_objects_db-view.

          cv_exit = run_tabl( ls_tadir_key ).

        WHEN /mbtools/if_command_field=>c_objects_exec-prog.

          cv_exit = run_prog( ls_tadir_key ).

        WHEN /mbtools/if_command_field=>c_objects_exec-tran.

          cv_exit = run_tran( ls_tadir_key ).

        WHEN /mbtools/if_command_field=>c_objects_exec-func.

          cv_exit = run_func( ls_tadir_key ).

        WHEN OTHERS.

          cv_exit = run_listcube( ls_tadir_key ).

      ENDCASE.

      IF lv_tadir_count = 1.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD /mbtools/if_command~get_commands.

    FIELD-SYMBOLS <ls_command> LIKE LINE OF ct_commands.

    APPEND INITIAL LINE TO ct_commands ASSIGNING <ls_command>.
    <ls_command>-command     = 'RUN'.
    <ls_command>-shortcut    = '!'.
    <ls_command>-description = 'Run'.

  ENDMETHOD.


  METHOD class_constructor.

    CREATE OBJECT go_command.

  ENDMETHOD.


  METHOD run_func.

    DATA lv_funcname TYPE funcname.

    CHECK is_tadir_key-object = /mbtools/if_command_field=>c_objects_exec-func.

    " Check if function module exists
    SELECT SINGLE funcname FROM tfdir INTO lv_funcname
      WHERE funcname = is_tadir_key-obj_name.
    IF sy-subrc <> 0.
      MESSAGE e004 WITH is_tadir_key-obj_name.
      RETURN.
    ENDIF.

    " Authorization check on S_DEVELOP happens in function RS_TESTFRAME_CALL
    SUBMIT rs_testframe_call WITH funcn = lv_funcname AND RETURN. "#EC CI_SUBMIT

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD run_listcube.

    CHECK /mbtools/if_command_field=>c_objects_bw CS is_tadir_key-object.

    " Authorization check for LISTCUBE
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'LISTCUBE'
      EXCEPTIONS
        ok     = 1
        not_ok = 2
        OTHERS = 3.
    IF sy-subrc = 1.
      " Run LISTCUBE with additional authorization checks on InfoProvider
      SUBMIT rsdd_show_icube
        WITH p_dbagg = abap_true
        WITH p_dta   = is_tadir_key-obj_name
        WITH p_tlogo = is_tadir_key-object
        AND RETURN.                                      "#EC CI_SUBMIT

      rv_exit = abap_true.
    ELSE.
      MESSAGE 'No authorization for LISTCUBE'(001) TYPE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD run_mbt.

    DATA ls_tadir_key TYPE /mbtools/if_definitions=>ty_tadir_key.

    ls_tadir_key-pgmid    = /mbtools/if_command_field=>c_pgmid-r3tr.
    ls_tadir_key-object   = /mbtools/if_command_field=>c_objects_exec-tran.
    ls_tadir_key-obj_name = '/MBTOOLS/' && iv_obj_name.

    SELECT SINGLE tcode FROM tstc INTO ls_tadir_key-obj_name
      WHERE tcode = ls_tadir_key-obj_name.
    IF sy-subrc = 0.
      rv_exit = run_tran( ls_tadir_key ).
    ENDIF.

  ENDMETHOD.


  METHOD run_prog.

    CHECK is_tadir_key-object = /mbtools/if_command_field=>c_objects_exec-prog.

    rv_exit = /mbtools/cl_sap=>run_program( is_tadir_key-obj_name ).

  ENDMETHOD.


  METHOD run_tabl.

    TYPES:
      BEGIN OF ty_dd02l,
        tabname  TYPE dd02l-tabname,
        tabclass TYPE dd02l-tabclass,
        sqltab   TYPE dd02l-sqltab,
      END OF ty_dd02l.

    DATA:
      ls_dd02l TYPE ty_dd02l,
      lv_subrc TYPE sy-subrc.

    CHECK /mbtools/if_command_field=>c_objects_db CS is_tadir_key-object.

    " For tables we check if there's any data to avoid pointless SE16 selection
    SELECT SINGLE tabname tabclass sqltab FROM dd02l INTO ls_dd02l
      WHERE tabname = is_tadir_key-obj_name AND as4local = 'A' ##WARN_OK.
    IF sy-subrc = 0.
      CALL FUNCTION 'DD_EXISTS_DATA'
        EXPORTING
          reftab          = ls_dd02l-sqltab
          tabclass        = ls_dd02l-tabclass
          tabname         = ls_dd02l-tabname
        IMPORTING
          subrc           = lv_subrc
        EXCEPTIONS
          missing_reftab  = 1
          sql_error       = 2
          buffer_overflow = 3
          unknown_error   = 4
          OTHERS          = 5.
      IF sy-subrc = 0 AND lv_subrc = 2.
        MESSAGE s005 WITH ls_dd02l-tabname.
        rv_exit = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    " Run SE16 with authorization check
    CALL FUNCTION 'RS_TABLE_LIST_CREATE'
      EXPORTING
        table_name         = is_tadir_key-obj_name
      EXCEPTIONS
        table_is_structure = 1
        table_not_exists   = 2
        db_not_exists      = 3
        no_permission      = 4
        no_change_allowed  = 5
        table_is_gtt       = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD run_tran.

    CHECK is_tadir_key-object = /mbtools/if_command_field=>c_objects_exec-tran.

    rv_exit = /mbtools/cl_sap=>run_transaction( is_tadir_key-obj_name ).

  ENDMETHOD.
ENDCLASS.
