REPORT /mbtools/command_field_tester.
************************************************************************
* MBT Command Field - Tester
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

TABLES: sscrfields.

*-----------------------------------------------------------------------

* Main
SELECTION-SCREEN:
BEGIN OF SCREEN 200 AS SUBSCREEN,
BEGIN OF BLOCK b200 WITH FRAME,
COMMENT /1(77) sc_t200,
COMMENT /1(77) sc_t201,
END OF BLOCK b200,
BEGIN OF BLOCK b210 WITH FRAME.
PARAMETERS p_cmd TYPE string LOWER CASE OBLIGATORY.
SELECTION-SCREEN:
END OF BLOCK b210,
BEGIN OF BLOCK b220 WITH FRAME.
PARAMETERS:
  p_exec  RADIOBUTTON GROUP g1 DEFAULT 'X',
  p_popup RADIOBUTTON GROUP g1.
SELECTION-SCREEN:
END OF BLOCK b220,
END OF SCREEN 200.

*-----------------------------------------------------------------------

* About
SELECTION-SCREEN:
BEGIN OF SCREEN 900 AS SUBSCREEN,
BEGIN OF BLOCK b900 WITH FRAME,
COMMENT /1(50) sc_t900,
COMMENT 60(25) sc_t901,
SKIP,
COMMENT /1(77) sc_t902,
END OF BLOCK b900,
BEGIN OF BLOCK b910 WITH FRAME,
PUSHBUTTON /1(55) sc_docu USER-COMMAND docu,
SKIP,
PUSHBUTTON /1(55) sc_tool USER-COMMAND tool,
SKIP,
PUSHBUTTON /1(55) sc_lice USER-COMMAND lice,
SKIP,
PUSHBUTTON /1(55) sc_home USER-COMMAND home,
END OF BLOCK b910,
END OF SCREEN 900.

*-----------------------------------------------------------------------

* Header
SELECTION-SCREEN:
BEGIN OF BLOCK sc_header,
SKIP,
SKIP,
COMMENT /3(77) sc_t001,
SKIP,
END OF BLOCK sc_header,
BEGIN OF TABBED BLOCK sc_tab FOR 20 LINES,
TAB (40) sc_tab2 USER-COMMAND sc_push2 DEFAULT SCREEN 200,
TAB (40) sc_tab9 USER-COMMAND sc_push9 DEFAULT SCREEN 900,
END OF BLOCK sc_tab.

*-----------------------------------------------------------------------

CONSTANTS:
  c_title TYPE string VALUE /mbtools/cl_tool_bc_cl=>c_tool-title.

DATA:
  gs_help_infos TYPE help_info,
  gv_exit       TYPE abap_bool,
  go_screen     TYPE REF TO /mbtools/cl_screen.

*-----------------------------------------------------------------------

INITIALIZATION.

  IF /mbtools/cl_switches=>is_active( c_title ) = abap_false.
    MESSAGE e004(/mbtools/bc) WITH c_title.
    RETURN.
  ENDIF.

  go_screen = /mbtools/cl_screen=>factory( c_title ).

  go_screen->init(
    IMPORTING
      ev_text      = sc_t001
      ev_about     = sc_tab9
      ev_title     = sc_t900
      ev_version   = sc_t901
      ev_copyright = sc_t902
      ev_docu      = sc_docu
      ev_tool      = sc_tool
      ev_home      = sc_home
      ev_lice      = sc_lice ).

  sc_tab2 = go_screen->header(
    iv_icon = icon_greater
    iv_text = 'Command'(001) ).

  sc_t200 = 'Enter a command in the field below (w/o leading "?") and'(200).
  sc_t201 = 'select how you want to simulate the execution'(201).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  go_screen->ucomm( sscrfields-ucomm ).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  go_screen->banner( ).

*-----------------------------------------------------------------------

START-OF-SELECTION.

  LOG-POINT ID /mbtools/bc SUBKEY c_title FIELDS sy-datum sy-uzeit sy-uname.

  DO.
    CASE abap_true.
      WHEN p_popup.
        gs_help_infos-menufunct = p_cmd.
        gs_help_infos-call      = 'D'.
        gs_help_infos-docuid    = 'OK'.
        gv_exit = /mbtools/cl_command_field=>do_command( gs_help_infos ).
      WHEN p_exec.
        gs_help_infos-menufunct = p_cmd.
        gs_help_infos-call      = 'H'.
        gv_exit = /mbtools/cl_command_field=>do_command( gs_help_infos ).
    ENDCASE.
    IF gv_exit = abap_true.
      EXIT.
    ENDIF.
  ENDDO.
