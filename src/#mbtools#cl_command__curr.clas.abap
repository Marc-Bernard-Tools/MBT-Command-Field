CLASS /mbtools/cl_command__curr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Command - Currency Conversion
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-or-later
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_command .

    ALIASES execute
      FOR /mbtools/if_command~execute .

    METHODS constructor .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES command
      FOR /mbtools/if_command~mo_command .

    METHODS format_result
      IMPORTING
        !iv_local_amount     TYPE tdcurr
        !iv_local_currency   TYPE tcurr_curr
        !iv_foreign_amount   TYPE tdcurr
        !iv_foreign_currency TYPE fcurr_curr
        !iv_exchange_rate    TYPE ukurs_curr
      RETURNING
        VALUE(rv_result)     TYPE string .
ENDCLASS.



CLASS /mbtools/cl_command__curr IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_object           TYPE string,
      lv_input_amount     TYPE c LENGTH 50,
      lv_type_of_rate     TYPE kurst_curr,
      lv_local_amount     TYPE tdcurr,
      lv_local_currency   TYPE tcurr_curr,
      lv_foreign_amount   TYPE tdcurr,
      lv_foreign_currency TYPE fcurr_curr,
      lv_exchange_rate    TYPE ukurs_curr,
      lv_icon             TYPE icon_d,
      lv_rest             TYPE string ##NEEDED,
      lv_result           TYPE string.

    lv_icon = icon_message_error_small.

    " Split parameters into type of rate and object
    command->split(
      EXPORTING
        iv_parameters = iv_parameters
      IMPORTING
        ev_operator   = lv_type_of_rate
        ev_operand    = lv_object ).

    IF lv_type_of_rate IS INITIAL.
      lv_type_of_rate = 'M'.
    ENDIF.

    " 100 EUR in USD
    SPLIT lv_object AT space INTO lv_input_amount lv_local_currency lv_rest lv_foreign_currency.

    TRY.
        lv_local_amount = lv_input_amount.
      CATCH cx_root.
        lv_result = 'Amount not numeric'(001).
    ENDTRY.

    IF lv_result IS INITIAL.
      TRANSLATE lv_local_currency TO UPPER CASE.
      SELECT SINGLE waers FROM tcurc INTO lv_local_currency
        WHERE waers = lv_local_currency.
      IF sy-subrc <> 0.
        lv_result = 'Source currency not found'(002).
      ENDIF.
    ENDIF.

    IF lv_result IS INITIAL.
      TRANSLATE lv_foreign_currency TO UPPER CASE.
      SELECT SINGLE waers FROM tcurc INTO lv_foreign_currency
        WHERE waers = lv_foreign_currency.
      IF sy-subrc <> 0.
        lv_result = 'Target currency not found'(003).
      ENDIF.
    ENDIF.

    IF lv_result IS INITIAL.
      CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
        EXPORTING
          date             = sy-datum
          type_of_rate     = lv_type_of_rate
          foreign_currency = lv_foreign_currency
          local_amount     = lv_local_amount
          local_currency   = lv_local_currency
        IMPORTING
          exchange_rate    = lv_exchange_rate
          foreign_amount   = lv_foreign_amount
        EXCEPTIONS
          no_rate_found    = 1
          overflow         = 2
          no_factors_found = 3
          no_spread_found  = 4
          derived_2_times  = 5
          OTHERS           = 6.
      IF sy-subrc = 0.
        " Format result nicely
        lv_result = format_result( iv_local_amount     = lv_local_amount
                                   iv_local_currency   = lv_local_currency
                                   iv_foreign_amount   = lv_foreign_amount
                                   iv_foreign_currency = lv_foreign_currency
                                   iv_exchange_rate    = lv_exchange_rate ).

        lv_icon = icon_convert.
      ELSEIF sy-subrc BETWEEN 1 AND 5.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_result.
      ELSE.
        lv_result = 'Error in CONVERT_TO_FOREIGN_CURRENCY' ##NO_TEXT.
      ENDIF.
    ENDIF.

    rv_exit = /mbtools/cl_command_field=>show_result( iv_command    = iv_command
                                                      iv_parameters = iv_parameters
                                                      iv_icon       = lv_icon
                                                      iv_result     = lv_result
                                                      iv_via_popup  = iv_via_popup ).

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.


  METHOD format_result.

    DATA:
      lv_input_amount  TYPE c LENGTH 50,
      lv_output_amount TYPE c LENGTH 50,
      lv_output_rate   TYPE c LENGTH 50.

    WRITE iv_local_amount TO lv_input_amount CURRENCY iv_local_currency LEFT-JUSTIFIED.

    CONCATENATE lv_input_amount iv_local_currency INTO lv_input_amount SEPARATED BY space.

    WRITE iv_foreign_amount TO lv_output_amount CURRENCY iv_foreign_currency LEFT-JUSTIFIED.

    CONCATENATE lv_output_amount iv_foreign_currency INTO lv_output_amount SEPARATED BY space.

    WRITE iv_exchange_rate TO lv_output_rate LEFT-JUSTIFIED.

    CONCATENATE '(' 'at'(004) lv_output_rate ')' INTO lv_output_rate SEPARATED BY space.

    CONCATENATE lv_input_amount '=' lv_output_amount lv_output_rate INTO rv_result SEPARATED BY space.

  ENDMETHOD.
ENDCLASS.
