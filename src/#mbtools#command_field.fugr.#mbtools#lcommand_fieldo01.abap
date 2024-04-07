*----------------------------------------------------------------------*
***INCLUDE /MBTOOLS/LCOMMAND_FIELDO01.
*----------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  FIELD-SYMBOLS <lv_field> TYPE any.

  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

  LOOP AT SCREEN.
    IF screen-name CP 'RESULT_OUTPUT*'.
      ASSIGN (screen-name) TO <lv_field>.
      ASSERT sy-subrc = 0.

      IF <lv_field> IS INITIAL.
        screen-active    = abap_false.
        screen-invisible = 1.
      ENDIF.
    ELSEIF screen-name = 'RESULT_LABEL'.
      IF result_label IS INITIAL.
        screen-active    = abap_false.
        screen-invisible = 1.
      ELSE.
        screen-active    = abap_true.
        screen-invisible = 0.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.
