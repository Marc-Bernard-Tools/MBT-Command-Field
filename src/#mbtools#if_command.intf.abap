INTERFACE /mbtools/if_command
  PUBLIC.


************************************************************************
* MBT Command Interface
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  INTERFACES if_badi_interface.

  TYPES:
    BEGIN OF ty_command,
      command     TYPE c LENGTH 40,
      shortcut    TYPE c LENGTH 3,
      description TYPE string,
    END OF ty_command,
    ty_commands TYPE STANDARD TABLE OF ty_command WITH KEY command.

  CLASS-METHODS execute
    IMPORTING
      !iv_command    TYPE string OPTIONAL
      !iv_parameters TYPE string
      !iv_via_popup  TYPE abap_bool OPTIONAL
    CHANGING
      !cv_exit       TYPE abap_bool.

  CLASS-METHODS get_commands
    CHANGING
      !ct_commands TYPE ty_commands.

ENDINTERFACE.
