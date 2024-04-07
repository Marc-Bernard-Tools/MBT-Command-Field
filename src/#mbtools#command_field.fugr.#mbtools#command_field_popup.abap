FUNCTION /mbtools/command_field_popup.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_INPUT) TYPE  CSEQUENCE
*"     REFERENCE(IV_ICON) TYPE  ICON_D OPTIONAL
*"     REFERENCE(IT_RESULT) TYPE  STRING_TABLE OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_INPUT) TYPE  STRING
*"  EXCEPTIONS
*"      CANCELLED
*"----------------------------------------------------------------------

  DATA:
    lv_result    TYPE string,
    lv_start_col TYPE i,
    lv_start_row TYPE i,
    lv_end_col   TYPE i,
    lv_end_row   TYPE i.

  PERFORM clear_popup.

  command_input = iv_input.

  IF it_result IS NOT INITIAL.
    IF iv_icon = icon_message_error_small.
      result_label = icon_message_error_small && 'Error'.
    ELSE.
      result_label = icon_equal && 'Result'.
    ENDIF.

    LOOP AT it_result INTO lv_result.
      CASE sy-tabix.
        WHEN 1.
          result_output01 = lv_result.
        WHEN 2.
          result_output02 = lv_result.
        WHEN 3.
          result_output03 = lv_result.
        WHEN 4.
          result_output04 = lv_result.
        WHEN 5.
          result_output05 = lv_result.
        WHEN 6.
          result_output06 = lv_result.
        WHEN 7.
          result_output07 = lv_result.
        WHEN 8.
          result_output08 = lv_result.
        WHEN 9.
          result_output09 = lv_result.
        WHEN 10.
          result_output10 = lv_result.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  " Centered (only works properly on output lists)
  lv_start_col = ( sy-scols - 120 ) / 2.
  IF lv_start_col < 60.
    lv_start_col = 60.
  ENDIF.
  lv_end_col = lv_start_col + 120.

  lv_start_row = 2.
  lv_end_row = lines( it_result ) + 2.

  CALL SCREEN 100 STARTING AT lv_start_col lv_start_row ENDING AT lv_end_col lv_end_row.

  ev_input = command_input.

  IF ok_code IS INITIAL.
    RAISE cancelled.
  ENDIF.

ENDFUNCTION.
