REPORT /mbtools/command_field_tester.
************************************************************************
* MBT Command Field - Tester
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

TABLES:
  sscrfields.

*-----------------------------------------------------------------------

* Main
SELECTION-SCREEN:
  BEGIN OF SCREEN 200 AS SUBSCREEN,
    BEGIN OF BLOCK b200 WITH FRAME,
      COMMENT /1(77) scr_t200,
      COMMENT /1(77) scr_t201,
    END OF BLOCK b200,
    BEGIN OF BLOCK b210 WITH FRAME.
PARAMETERS:
  p_cmd TYPE string LOWER CASE OBLIGATORY.
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
      COMMENT /1(50) scr_t900,
      COMMENT 60(25) scr_t901,
      SKIP,
      COMMENT /1(77) scr_t902,
    END OF BLOCK b900,
    BEGIN OF BLOCK b910 WITH FRAME,
      PUSHBUTTON /1(55) b_docu USER-COMMAND docu,
      SKIP,
      PUSHBUTTON /1(55) b_tool USER-COMMAND tool,
      SKIP,
      PUSHBUTTON /1(55) b_home USER-COMMAND home,
    END OF BLOCK b910,
  END OF SCREEN 900.

*-----------------------------------------------------------------------

* Header
SELECTION-SCREEN:
  BEGIN OF BLOCK scr_header,
    SKIP,
    SKIP,
    COMMENT /3(77) scr_t001,
    SKIP,
  END OF BLOCK scr_header,
  BEGIN OF TABBED BLOCK scr_tab FOR 20 LINES,
    TAB (40) scr_tab2 USER-COMMAND scr_push2 DEFAULT SCREEN 0200,
    TAB (40) scr_tab9 USER-COMMAND scr_push9 DEFAULT SCREEN 0900,
  END OF BLOCK scr_tab.

*-----------------------------------------------------------------------

CONSTANTS:
  c_title TYPE string VALUE /mbtools/cl_tool_bc_cl=>c_tool-title.

DATA:
  gv_exit   TYPE abap_bool,
  go_screen TYPE REF TO /mbtools/cl_screen.

*-----------------------------------------------------------------------

INITIALIZATION.

  IF /mbtools/cl_switches=>is_active( c_title ) = abap_false.
    MESSAGE e004(/mbtools/bc) WITH c_title.
    RETURN.
  ENDIF.

  go_screen = /mbtools/cl_screen=>factory( c_title ).

  go_screen->init(
    IMPORTING
      ev_text      = scr_t001
      ev_about     = scr_tab9
      ev_title     = scr_t900
      ev_version   = scr_t901
      ev_copyright = scr_t902
      ev_docu      = b_docu
      ev_tool      = b_tool
      ev_home      = b_home ).

  scr_tab2 = go_screen->header(
    iv_icon = icon_greater
    iv_text = 'Command'(001) ).

  scr_t200 = 'Enter a command in the field below (w/o leading "?") and'(200).
  scr_t201 = 'select how you want to simulate the execution'(201).

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
    IF p_popup = abap_true.
      gv_exit = /mbtools/cl_command_field=>popup_command( iv_input = p_cmd ).
    ELSE.
      gv_exit = /mbtools/cl_command_field=>execute_command( iv_input = p_cmd ).
    ENDIF.
    IF gv_exit = abap_true.
      EXIT.
    ENDIF.
  ENDDO.
