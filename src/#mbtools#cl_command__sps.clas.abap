CLASS /mbtools/cl_command__sps DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Command - SPS
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



CLASS /mbtools/cl_command__sps IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    CALL FUNCTION 'OCS_UI_DISPLAY_PATCH_LEVEL'
      EXCEPTIONS
        no_component_found = 1
        wrong_component    = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.
ENDCLASS.
