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



CLASS /mbtools/cl_command_show IMPLEMENTATION.


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

    DATA:
      e071_obj_name TYPE e071-obj_name.

    " First try: workbench tools
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = i_tadir_key-obj_name
        object_type         = i_tadir_key-object
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc = 0.
      r_exit = abap_true.
      RETURN.
    ENDIF.

    " Second try: transport tool
    e071_obj_name = i_tadir_key-obj_name.

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_action         = 'SHOW'
        iv_pgmid          = i_tadir_key-pgmid
        iv_object         = i_tadir_key-object
        iv_obj_name       = e071_obj_name
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.
    IF sy-subrc = 0.
      r_exit = abap_true.
    ELSE.
      MESSAGE s000 WITH 'Navigation not available'(002).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
