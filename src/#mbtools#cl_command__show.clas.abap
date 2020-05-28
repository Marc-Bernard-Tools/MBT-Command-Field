************************************************************************
* /MBTOOLS/CL_COMMAND_SHOW
* MBT Command - Show
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_command__show DEFINITION
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

    METHODS show_tool
      IMPORTING
        !i_tadir_key  TYPE /mbtools/if_definitions=>ty_tadir_key
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
    METHODS show_message
      IMPORTING
        !i_message    TYPE /mbtools/if_definitions=>ty_name
      RETURNING
        VALUE(r_exit) TYPE abap_bool .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND__SHOW IMPLEMENTATION.


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

    " Find objects
    command->select(
      EXPORTING
        i_object   = object
        i_obj_name = object_name ).

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

      " Show object definition
      r_exit = show_tool( i_tadir_key = tadir_key ).

      IF tadir_count = 1.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.


  METHOD show_message.

    DATA: msgid TYPE sy-msgid,
          msgno TYPE sy-msgno.

    command->split_message(
      EXPORTING
        i_message = i_message
      IMPORTING
        e_msgid   = msgid
        e_msgno   = msgno ).

    IF NOT msgid IS INITIAL AND NOT msgno IS INITIAL.
      " Display message with placeholders for parameters
      MESSAGE  ID msgid TYPE 'I' NUMBER msgno
        WITH '&1' '&2' '&3' '&4'.

      r_exit = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD show_tool.

    " Show message in popup instead of editor
    IF i_tadir_key-pgmid  = /mbtools/if_command_field=>c_pgmid-limu AND
       i_tadir_key-object = /mbtools/if_command_field=>c_objects_limu-mess.

      r_exit = show_message( i_tadir_key-obj_name ).

    ELSE.

      r_exit = /mbtools/cl_sap=>show_object(
        EXPORTING
          i_pgmid    = i_tadir_key-pgmid
          i_object   = i_tadir_key-object
          i_obj_name = i_tadir_key-obj_name ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
