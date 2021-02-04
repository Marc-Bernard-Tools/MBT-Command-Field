CLASS /mbtools/cl_tool_bc_cl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Command Field
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_tool.

    CONSTANTS:
      BEGIN OF c_tool,
        version      TYPE string VALUE '1.0.0' ##NO_TEXT,
        title        TYPE string VALUE 'MBT Command Field' ##NO_TEXT,
        bundle_id    TYPE i VALUE 1,
        download_id  TYPE i VALUE 4409,
        description  TYPE string
        VALUE 'The world''s first enhancement for the SAP GUI command field' ##NO_TEXT,
        has_launch   TYPE abap_bool VALUE abap_true,
        mbt_command  TYPE string VALUE 'COMMAND',
        mbt_shortcut TYPE string VALUE 'CF',
      END OF c_tool.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_tool TYPE REF TO /mbtools/cl_tools.

ENDCLASS.



CLASS /mbtools/cl_tool_bc_cl IMPLEMENTATION.


  METHOD /mbtools/if_tool~launch.
    /mbtools/cl_sap=>run_program( '/MBTOOLS/COMMAND_FIELD_TESTER' ).
  ENDMETHOD.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    /mbtools/if_tool~ms_manifest = mo_tool->ms_manifest.
  ENDMETHOD.
ENDCLASS.
