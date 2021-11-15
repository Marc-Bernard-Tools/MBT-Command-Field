CLASS /mbtools/cl_command__show DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Command - Show
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_command.

    ALIASES execute
      FOR /mbtools/if_command~execute.

    METHODS constructor.
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES command
      FOR /mbtools/if_command~mo_command.

    METHODS show_tool
      IMPORTING
        !is_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.
    METHODS show_message
      IMPORTING
        !iv_message    TYPE /mbtools/if_definitions=>ty_name
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.
    METHODS show_parameter
      IMPORTING
        !iv_parameter  TYPE string
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.
ENDCLASS.



CLASS /mbtools/cl_command__show IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_subrc       TYPE sy-subrc,
      lv_value       TYPE string,
      lv_object      TYPE string,
      lv_object_name TYPE string,
      lv_tadir_count TYPE i,
      ls_tadir_key   TYPE /mbtools/if_definitions=>ty_tadir_key.

    " First, check if it's just a property
    lv_value = /mbtools/cl_utilities=>get_profile_parameter( iv_parameters ).

    IF lv_value <> /mbtools/cl_utilities=>c_unknown.
      rv_exit = show_parameter( /mbtools/cl_utilities=>get_profile_parameter_name( iv_parameters ) ).
    ELSE.

      " Split parameters into object and object name
      command->split(
        EXPORTING
          iv_parameters = iv_parameters
        IMPORTING
          ev_operator   = lv_object
          ev_operand    = lv_object_name ).

      " Find objects
      command->select(
        iv_object   = lv_object
        iv_obj_name = lv_object_name ).

      " Add object texts
      command->text( ).

      DO.
        " Pick exactly one object
        TRY.
            command->pick(
              IMPORTING
                es_tadir_key = ls_tadir_key
                ev_count     = lv_tadir_count ).
          CATCH /mbtools/cx_exception.
            EXIT.
        ENDTRY.

        " Show object definition
        rv_exit = show_tool( ls_tadir_key ).

        IF lv_tadir_count = 1.
          EXIT.
        ENDIF.
      ENDDO.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.


  METHOD show_message.

    DATA:
      lv_msgid TYPE sy-msgid,
      lv_msgno TYPE sy-msgno.

    command->split_message(
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
        tcode     = 'RZ11'
        mode_val  = 'E'
      TABLES
        using_tab = lt_bdcdata
      EXCEPTIONS
        OTHERS    = 1.

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
