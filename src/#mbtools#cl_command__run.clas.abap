************************************************************************
* /MBTOOLS/CL_COMMAND_RUN
* MBT Command - Run
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_command__run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /mbtools/if_command .

    ALIASES execute
      FOR /mbtools/if_command~execute .

    METHODS constructor .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES command
      FOR /mbtools/if_command~command .

    METHODS run_listcube
      IMPORTING
        !i_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
    METHODS run_tabl
      IMPORTING
        !i_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
    METHODS run_func
      IMPORTING
        !i_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
    METHODS run_prog
      IMPORTING
        !i_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
    METHODS run_tran
      IMPORTING
        !i_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND__RUN IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      object      TYPE string,
      object_name TYPE string,
      tadir_count TYPE i,
      tadir_key   TYPE /mbtools/if_definitions=>ty_tadir_key.

    " Split parameters into object and object name
    command->split(
      EXPORTING
        i_parameters = i_parameters
      IMPORTING
        e_operator   = object
        e_operand    = object_name ).

    IF object IS INITIAL.
      CONCATENATE
        /mbtools/if_command_field=>c_objects_db
        /mbtools/if_command_field=>c_objects_bw
        /mbtools/if_command_field=>c_objects_exec
        INTO object SEPARATED BY ','.
    ENDIF.

    " Select objects
    command->select(
      EXPORTING
        i_object   = object
        i_obj_name = object_name ).

    " Filter table types to ones that work in SE16
    command->filter_tabl( ).

    " Add object texts
    command->text( ).

    DO.
      " Pick exactly one object
      command->pick(
        IMPORTING
          e_tadir_key = tadir_key
          e_count     = tadir_count
        EXCEPTIONS
          cancelled   = 1
          OTHERS      = 2 ).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      " Run object
      CASE tadir_key-object.
        WHEN /mbtools/if_command_field=>c_objects_db-tabl OR
             /mbtools/if_command_field=>c_objects_db-view.

          r_exit = run_tabl( i_tadir_key = tadir_key ).

        WHEN /mbtools/if_command_field=>c_objects_exec-prog.

          r_exit = run_prog( i_tadir_key = tadir_key ).

        WHEN /mbtools/if_command_field=>c_objects_exec-tran.

          r_exit = run_tran( i_tadir_key = tadir_key ).

        WHEN /mbtools/if_command_field=>c_objects_exec-func.

          r_exit = run_func( i_tadir_key = tadir_key ).

        WHEN OTHERS.

          r_exit = run_listcube( i_tadir_key = tadir_key ).

      ENDCASE.

      IF tadir_count = 1.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.


  METHOD run_func.

    DATA:
      funcname TYPE funcname.

    CHECK i_tadir_key-object = /mbtools/if_command_field=>c_objects_exec-func.

    " Check if function module exists
    SELECT SINGLE funcname FROM tfdir INTO funcname
      WHERE funcname = i_tadir_key-obj_name.
    IF sy-subrc <> 0.
      MESSAGE e004 WITH i_tadir_key-obj_name.
      RETURN.
    ENDIF.

    " Authorization check on S_DEVELOP happens in function RS_TESTFRAME_CALL
    SUBMIT rs_testframe_call WITH funcn = funcname AND RETURN.

    r_exit = abap_true.

  ENDMETHOD.


  METHOD run_listcube.

    CHECK /mbtools/if_command_field=>c_objects_bw CS i_tadir_key-object.

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
        WITH p_dta   = i_tadir_key-obj_name
        WITH p_tlogo = i_tadir_key-object
        AND RETURN.

      r_exit = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD run_prog.

    DATA: trdir_entry TYPE trdir.

    CHECK i_tadir_key-object = /mbtools/if_command_field=>c_objects_exec-prog.

    " Check if executable program exists
    SELECT SINGLE * FROM trdir INTO trdir_entry
      WHERE name = i_tadir_key-obj_name AND subc = '1'.
    IF sy-subrc = 0.
      " Run program with authorization check
      CALL FUNCTION 'SUBMIT_REPORT'
        EXPORTING
          report           = trdir_entry-name
          rdir             = trdir_entry
          ret_via_leave    = abap_true
        EXCEPTIONS
          just_via_variant = 1
          no_submit_auth   = 2
          OTHERS           = 3.
      IF sy-subrc = 0.
        r_exit = abap_true.
      ELSEIF sy-subrc = 2.
        MESSAGE i149(00) WITH trdir_entry-name.
      ENDIF.
    ELSE.
      MESSAGE i541(00) WITH trdir_entry-name.
    ENDIF.

  ENDMETHOD.


  METHOD run_tabl.

    DATA:
      dd02l TYPE dd02l,
      subrc TYPE sy-subrc.

    CHECK /mbtools/if_command_field=>c_objects_db CS i_tadir_key-object.

    " For tables we check if there's any data to avoid pointless SE16 selection
    SELECT SINGLE * FROM dd02l INTO dd02l
      WHERE tabname = i_tadir_key-obj_name AND as4local = 'A'.
    IF sy-subrc = 0.
      CALL FUNCTION 'DD_EXISTS_DATA'
        EXPORTING
          reftab          = dd02l-sqltab
          tabclass        = dd02l-tabclass
          tabname         = dd02l-tabname
        IMPORTING
          subrc           = subrc
        EXCEPTIONS
          missing_reftab  = 1
          sql_error       = 2
          buffer_overflow = 3
          unknown_error   = 4
          OTHERS          = 5.
      IF sy-subrc = 0 AND subrc = 2.
        MESSAGE s005 WITH dd02l-tabname.
        r_exit = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    " Run SE16 with authorization check
    CALL FUNCTION 'RS_TABLE_LIST_CREATE'
      EXPORTING
        table_name         = i_tadir_key-obj_name
      EXCEPTIONS
        table_is_structure = 1
        table_not_exists   = 2
        db_not_exists      = 3
        no_permission      = 4
        no_change_allowed  = 5
        table_is_gtt       = 6
        OTHERS             = 7.
    IF sy-subrc = 0.
      r_exit = abap_true.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD run_tran.

    DATA: tcode TYPE sy-tcode.

    CHECK i_tadir_key-object = /mbtools/if_command_field=>c_objects_exec-tran.

    " Check if transaction exists
    SELECT SINGLE tcode FROM tstc INTO tcode
      WHERE tcode = i_tadir_key-obj_name.
    IF sy-subrc = 0.
      " Run transaction with authorization check
      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
        EXPORTING
          tcode  = tcode
        EXCEPTIONS
          ok     = 0
          not_ok = 2
          OTHERS = 3.
      IF sy-subrc = 0.
        CALL TRANSACTION tcode.                          "#EC CI_CALLTA

        r_exit = abap_true.
      ELSE.
        MESSAGE i172(00) WITH tcode.
      ENDIF.
    ELSE.
      MESSAGE i031(00) WITH tcode.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
