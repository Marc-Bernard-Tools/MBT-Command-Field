************************************************************************
* /MBTOOLS/CL_COMMAND_SHOW
* MBT Command - Show
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_command_show DEFINITION
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
        !i_tadir_key  TYPE /mbtools/if_command_field=>ty_tadir_key
      RETURNING
        VALUE(r_exit) TYPE abap_bool .

ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND_SHOW IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      object      TYPE string,
      object_name TYPE string,
      tadir_count TYPE i,
      tadir_key   TYPE /mbtools/if_command_field=>ty_tadir_key.

    " Split parameters into object and object name
    CALL METHOD command->split
      EXPORTING
        i_parameters = i_parameters
      IMPORTING
        e_operator   = object
        e_operand    = object_name.

    " Find objects
    CALL METHOD command->select
      EXPORTING
        i_object   = object
        i_obj_name = object_name.

    " Add object texts
    CALL METHOD command->text.

    DO.
      " Pick exactly one object
      CALL METHOD command->pick
        IMPORTING
          e_tadir_key = tadir_key
          e_count     = tadir_count
        EXCEPTIONS
          cancelled   = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      " Show object definition
      CALL METHOD show_tool
        EXPORTING
          i_tadir_key = tadir_key
        RECEIVING
          r_exit      = r_exit.

      IF tadir_count = 1.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.


  METHOD show_tool.

    CALL METHOD /mbtools/cl_sap=>show_object
      EXPORTING
        i_pgmid    = i_tadir_key-pgmid
        i_object   = i_tadir_key-object
        i_obj_name = i_tadir_key-obj_name
      RECEIVING
        r_exit     = r_exit.

  ENDMETHOD.
ENDCLASS.
