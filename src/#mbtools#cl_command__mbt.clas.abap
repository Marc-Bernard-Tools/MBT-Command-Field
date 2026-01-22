CLASS /mbtools/cl_command__mbt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Command - MBT
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

ENDCLASS.



CLASS /mbtools/cl_command__mbt IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_operand TYPE string,
      lv_found   TYPE abap_bool,
      lo_tool    TYPE REF TO /mbtools/cl_tool,
      ls_tool    TYPE /mbtools/if_tool=>ty_manifest,
      lt_tools   TYPE /mbtools/if_tool=>ty_manifests.

    " Split parameters into operator and operand
    go_command->split(
      EXPORTING
        iv_parameters = iv_parameters
      IMPORTING
        ev_operand    = lv_operand ).

    lv_operand = to_upper( lv_operand ).

    " Commands from registered tools
    IF lv_operand IS NOT INITIAL.
      lt_tools = /mbtools/cl_tool_manager=>select( ).

      LOOP AT lt_tools INTO ls_tool.
        lo_tool = ls_tool-manager.

        IF lv_operand = lo_tool->get_command( ) OR lv_operand = lo_tool->get_shortcut( ).
          lo_tool->launch( ).
          lv_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Additional commands from MBT Base
    IF lv_found IS INITIAL.
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
    ENDIF.

    IF lv_found IS INITIAL.
      MESSAGE 'Unknown tool.' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      cv_exit = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD /mbtools/if_command~get_commands.

    FIELD-SYMBOLS <ls_command> LIKE LINE OF ct_commands.

    APPEND INITIAL LINE TO ct_commands ASSIGNING <ls_command>.
    <ls_command>-command     = 'MBT'.
    <ls_command>-shortcut    = '/'.
    <ls_command>-description = 'Launch Marc Bernard Tools'.

  ENDMETHOD.


  METHOD class_constructor.

    CREATE OBJECT go_command.

  ENDMETHOD.
ENDCLASS.
