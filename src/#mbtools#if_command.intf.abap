INTERFACE /mbtools/if_command
  PUBLIC.

************************************************************************
* MBT Command Interface
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  DATA mo_command TYPE REF TO /mbtools/cl_command.

  METHODS execute
    IMPORTING
      !iv_command    TYPE string OPTIONAL
      !iv_parameters TYPE string
      !iv_via_popup  TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(rv_exit) TYPE abap_bool.
ENDINTERFACE.
