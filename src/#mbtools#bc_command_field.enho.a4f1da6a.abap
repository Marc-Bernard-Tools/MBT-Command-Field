"Name: \FU:HELP_OBJECT_SHOW_FOR_FIELD\SE:BEGIN\EI
ENHANCEMENT 0 /MBTOOLS/BC_COMMAND_FIELD.
************************************************************************
* MBT Command Field
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************

  DATA:
    mbt_infos        TYPE help_info,
    mbt_dynpselect   TYPE TABLE OF dselc,
    mbt_dynpvaluetab TYPE TABLE OF dval.

  IF /mbtools/cl_switches=>is_active( /mbtools/cl_switches=>c_tool-mbt_command_field ) = abap_true.

    mbt_infos = /mbtools/cl_command_field=>get_infos( ).

    IF called_for_tab = '/MBTOOLS/BC_COMMAND' AND mbt_infos IS NOT INITIAL.

      CALL FUNCTION 'HELP_START'
        EXPORTING
          help_infos   = mbt_infos
        TABLES
          dynpselect   = mbt_dynpselect
          dynpvaluetab = mbt_dynpvaluetab.

      CLEAR mbt_infos.
      /mbtools/cl_command_field=>set_infos( mbt_infos ).
      RETURN.

    ENDIF.

  ENDIF.

ENDENHANCEMENT.
