"Name: \FU:HELP_OBJECT_SHOW_FOR_FIELD\SE:BEGIN\EI
ENHANCEMENT 0 /MBTOOLS/BC_COMMAND_FIELD.
************************************************************************
* MBT Command Field
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

  IF /mbtools/cl_switches=>is_active( /mbtools/cl_switches=>c_tool-mbt_command_field ) = abap_true.

    IF /mbtools/cl_command_field=>do_help_start( called_for_tab ) = abap_true.
      RETURN.
    ENDIF.

  ENDIF.

ENDENHANCEMENT.
