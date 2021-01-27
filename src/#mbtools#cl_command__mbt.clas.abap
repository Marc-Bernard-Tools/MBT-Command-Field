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
      lv_operator TYPE string,
      lv_operand  TYPE string,
      lv_switch   TYPE abap_bool.

    " Split parameters into operator and operand
    command->split(
      EXPORTING
        iv_parameters = iv_parameters
      IMPORTING
        ev_operator   = lv_operator
        ev_operand    = lv_operand ).

    CASE to_upper( lv_operand ).
      WHEN '' OR 'BASE' OR 'MBT'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/MBT' ).
      WHEN 'INST'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/MBT_INSTALLER' ).
      WHEN 'SUPP'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/MBT_SUPPORT' ).
      WHEN 'LOL'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/TLOGO_LISTER' ).
      WHEN 'ICON'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/ICON_BROWSER' ).
      WHEN 'CL'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/COMMAND_FIELD_TESTER' ).
      WHEN 'CTS'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/CTS_REQ_TESTER' ).
      WHEN 'NOTE'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/NOTE_DISPLAY' ).
      WHEN 'SYSMON'.
        /mbtools/cl_sap=>run_program( '/MBTOOLS/SYSTEM_MONITOR' ).
      WHEN OTHERS.
        MESSAGE 'Unknown tool.' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.
ENDCLASS.
