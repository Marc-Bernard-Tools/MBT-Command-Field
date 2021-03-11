CLASS /mbtools/cl_command__show DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
************************************************************************
* MBT Command - Show
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_command .

    ALIASES execute
      FOR /mbtools/if_command~execute .

    METHODS constructor .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES command
      FOR /mbtools/if_command~mo_command .

    METHODS show_tool
      IMPORTING
        !is_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
    METHODS show_message
      IMPORTING
        !iv_message    TYPE /mbtools/if_definitions=>ty_name
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
ENDCLASS.



CLASS /mbtools/cl_command__show IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_object      TYPE string,
      lv_object_name TYPE string,
      lv_tadir_count TYPE i,
      ls_tadir_key   TYPE /mbtools/if_definitions=>ty_tadir_key.

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
