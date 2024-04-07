*----------------------------------------------------------------------*
***INCLUDE /MBTOOLS/LCOMMAND_FIELDI01.
*----------------------------------------------------------------------*

MODULE user_command_0100 INPUT.

  IF ok_code = 'CANCEL'.
    PERFORM clear_popup.
  ENDIF.

  SET SCREEN 0.
  LEAVE SCREEN.

ENDMODULE.

FORM clear_popup.

  CLEAR: ok_code, command_input, result_label,
    result_output01, result_output02, result_output03, result_output04, result_output05,
    result_output06, result_output06, result_output08, result_output09, result_output10.

ENDFORM.
