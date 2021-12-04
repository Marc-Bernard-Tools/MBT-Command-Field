CLASS /mbtools/cl_command__sps DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Command - SPS
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_command.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_command__sps IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    CALL FUNCTION 'OCS_UI_DISPLAY_PATCH_LEVEL'
      EXCEPTIONS
        no_component_found = 1
        wrong_component    = 2
        internal_error     = 3
        OTHERS             = 4.

    cv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD /mbtools/if_command~get_commands.

    FIELD-SYMBOLS <ls_command> LIKE LINE OF ct_commands.

    APPEND INITIAL LINE TO ct_commands ASSIGNING <ls_command>.
    <ls_command>-command     = 'SPS'.
    <ls_command>-shortcut    = ''.
    <ls_command>-description = 'Installed Software Components and Product Versions'.

  ENDMETHOD.
ENDCLASS.
