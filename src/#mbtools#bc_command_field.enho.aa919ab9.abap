"Name: \FU:HELP_START\SE:BEGIN\EI
ENHANCEMENT 0 /MBTOOLS/BC_COMMAND_FIELD.
************************************************************************
* MBT Command Field
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************

  DATA:
    mbt_exit    TYPE abap_bool,
    mbt_command TYPE string.

  IF /mbtools/cl_switches=>is_active( /mbtools/cl_switches=>c_tool-mbt_command_field ) = abap_true.

    mbt_command = help_infos-menufunct.

    IF help_infos-call = 'D' AND help_infos-docuid = 'OK'.

      IF /mbtools/cl_command_field=>get_infos( ) IS INITIAL.
        /mbtools/cl_command_field=>set_infos( help_infos ).
        SET TITLEBAR 'OBJECT_SELECTION' OF PROGRAM '/MBTOOLS/BC_COMMAND_FIELD'.
        mbt_exit = /mbtools/cl_command_field=>popup_command( iv_input = mbt_command ).
      ENDIF.

    ELSEIF help_infos-call = 'H'.

      SET TITLEBAR 'OBJECT_SELECTION' OF PROGRAM '/MBTOOLS/BC_COMMAND_FIELD'.
      mbt_exit = /mbtools/cl_command_field=>execute_command( iv_input = mbt_command ).

    ENDIF.

    IF mbt_exit = abap_true.
      RETURN.
    ENDIF.

  ENDIF.

ENDENHANCEMENT.
