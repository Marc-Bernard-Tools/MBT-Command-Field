REPORT /mbtools/command_field.
************************************************************************
* MBT Command Field
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

START-OF-SELECTION.

  MESSAGE s003(/mbtools/bc).

* Callback routine for command field calculator
FORM callback_eval
  USING    i_variable TYPE clike
  CHANGING e_value    TYPE f
           e_subrc    TYPE any ##CALLED.

  DATA:
    lv_subrc TYPE sy-subrc.

  CALL METHOD /mbtools/cl_utilities=>get_property
    EXPORTING
      iv_property    = i_variable
    IMPORTING
      ev_value_float = e_value
      ev_subrc       = lv_subrc.

  e_subrc = lv_subrc.

ENDFORM.                    "callback_eval

* Callback routine for command field object selection
FORM callback_alv
  USING r_ucomm     TYPE sy-ucomm
        rs_selfield TYPE slis_selfield ##CALLED.

  IF r_ucomm = '&IC1'. "double-click
*   Remember selected row and exit
    EXPORT tabindex FROM rs_selfield-tabindex
      TO MEMORY ID /mbtools/cl_command=>c_tabix.
    rs_selfield-exit = abap_true.
  ENDIF.

ENDFORM.                    "callback_alv
