CLASS /mbtools/cl_command__mbt DEFINITION
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
ENDCLASS.



CLASS /mbtools/cl_command__mbt IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_operand TYPE string,
      lv_found   TYPE abap_bool,
      lo_tool    TYPE REF TO /mbtools/cl_tool,
      ls_tool    TYPE /mbtools/cl_tool_manager=>ty_manifest,
      lt_tools   TYPE /mbtools/cl_tool_manager=>ty_manifests.

    " Split parameters into operator and operand
    command->split(
      EXPORTING
        iv_parameters = iv_parameters
      IMPORTING
        ev_operand    = lv_operand ).

    lv_operand = to_upper( lv_operand ).

    " Commands from registered tools
    lt_tools = /mbtools/cl_tool_manager=>select( ).

    LOOP AT lt_tools INTO ls_tool.
      lo_tool = ls_tool-manager.

      IF lv_operand = lo_tool->get_command( ) OR lv_operand = lo_tool->get_shortcut( ).
        lo_tool->launch( ).
        lv_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    " Additional commands from MBT Base
    CASE lv_operand.
      WHEN ''.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/MBT' ).
        lv_found = abap_true.
      WHEN 'INST' OR 'INSTALL'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/MBT_INSTALLER' ).
        lv_found = abap_true.
      WHEN 'SUPP' OR 'SUPPORT'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/MBT_SUPPORT' ).
        lv_found = abap_true.
    ENDCASE.

    IF lv_found IS INITIAL.
      MESSAGE 'Unknown tool.' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.
ENDCLASS.
