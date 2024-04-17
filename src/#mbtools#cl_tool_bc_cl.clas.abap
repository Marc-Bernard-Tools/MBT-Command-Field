CLASS /mbtools/cl_tool_bc_cl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Command Field
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_tool.

    CONSTANTS:
      BEGIN OF c_tool,
        version      TYPE string VALUE '1.4.3' ##NO_TEXT,
        title        TYPE string VALUE 'MBT Command Field' ##NO_TEXT,
        description  TYPE string
        VALUE `The world's first enhancement for the SAP GUI command field` ##NO_TEXT,
        bundle_id    TYPE i VALUE 1,
        download_id  TYPE i VALUE 4409,
        has_launch   TYPE abap_bool VALUE abap_true,
        mbt_command  TYPE string VALUE 'COMMAND',
        mbt_shortcut TYPE string VALUE 'CF',
      END OF c_tool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_tool_bc_cl IMPLEMENTATION.


  METHOD /mbtools/if_tool~install.
    RETURN.
  ENDMETHOD.


  METHOD /mbtools/if_tool~launch.
    /mbtools/cl_sap=>run_program( '/MBTOOLS/COMMAND_FIELD_TESTER' ).
  ENDMETHOD.


  METHOD /mbtools/if_tool~title.
    rv_title = c_tool-title.
  ENDMETHOD.


  METHOD /mbtools/if_tool~tool.
    MOVE-CORRESPONDING c_tool TO rs_tool.
  ENDMETHOD.


  METHOD /mbtools/if_tool~uninstall.
    RETURN.
  ENDMETHOD.
ENDCLASS.
