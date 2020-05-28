************************************************************************
* /MBTOOLS/CL_COMMAND_CURR
* MBT Command - Currency Conversion
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_command__curr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /mbtools/if_command .

    ALIASES execute
      FOR /mbtools/if_command~execute .

    METHODS constructor .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES command
      FOR /mbtools/if_command~command .

    METHODS format_result
      IMPORTING
        !i_local_amount     TYPE tdcurr
        !i_local_currency   TYPE tcurr_curr
        !i_foreign_amount   TYPE tdcurr
        !i_foreign_currency TYPE fcurr_curr
        !i_exchange_rate    TYPE ukurs_curr
      RETURNING
        VALUE(r_result)     TYPE string .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND__CURR IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      object           TYPE string,
      input_amount     TYPE c LENGTH 50,
      type_of_rate     TYPE kurst_curr,
      local_amount     TYPE tdcurr,
      local_currency   TYPE tcurr_curr,
      foreign_amount   TYPE tdcurr,
      foreign_currency TYPE fcurr_curr,
      exchange_rate    TYPE ukurs_curr,
      icon             TYPE icon_d,
      result           TYPE string.

    icon = icon_message_error_small.

    " Split parameters into type of rate and object
    command->split(
      EXPORTING
        i_parameters = i_parameters
      IMPORTING
        e_operator   = type_of_rate
        e_operand    = object ).

    IF type_of_rate IS INITIAL.
      type_of_rate = 'M'.
    ENDIF.

    " 100 EUR in USD
    SPLIT object AT space INTO input_amount local_currency sy-lisel foreign_currency.

    TRY.
        local_amount = input_amount.
      CATCH cx_root.
        result =  'Amount not numeric'(001).
    ENDTRY.

    IF result IS INITIAL.
      TRANSLATE local_currency TO UPPER CASE.
      SELECT SINGLE waers FROM tcurc INTO local_currency
        WHERE waers = local_currency.
      IF sy-subrc <> 0.
        result = 'Source currency not found'(002).
      ENDIF.
    ENDIF.

    IF result IS INITIAL.
      TRANSLATE foreign_currency TO UPPER CASE.
      SELECT SINGLE waers FROM tcurc INTO foreign_currency
        WHERE waers = foreign_currency.
      IF sy-subrc <> 0.
        result = 'Target currency not found'(003).
      ENDIF.
    ENDIF.

    IF result IS INITIAL.
      CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
        EXPORTING
          date             = sy-datum
          type_of_rate     = type_of_rate
          foreign_currency = foreign_currency
          local_amount     = local_amount
          local_currency   = local_currency
        IMPORTING
          exchange_rate    = exchange_rate
          foreign_amount   = foreign_amount
        EXCEPTIONS
          no_rate_found    = 1
          overflow         = 2
          no_factors_found = 3
          no_spread_found  = 4
          derived_2_times  = 5
          OTHERS           = 6.
      IF sy-subrc = 0.
        " Format result nicely
        result = format_result( i_local_amount     = local_amount
                                i_local_currency   = local_currency
                                i_foreign_amount   = foreign_amount
                                i_foreign_currency = foreign_currency
                                i_exchange_rate    = exchange_rate ).

        icon = icon_convert.
      ELSEIF sy-subrc BETWEEN 1 AND 5.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO result.
      ELSE.
        result = 'Error in CONVERT_TO_FOREIGN_CURRENCY' ##NO_TEXT.
      ENDIF.
    ENDIF.

    r_exit = /mbtools/cl_command_field=>show_result( i_command    = i_command
                                                     i_parameters = i_parameters
                                                     i_icon       = icon
                                                     i_result     = result
                                                     i_via_popup  = i_via_popup ).

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.


  METHOD format_result.

    DATA:
      input_amount  TYPE c LENGTH 50,
      output_amount TYPE c LENGTH 50,
      output_rate   TYPE c LENGTH 50.

    WRITE i_local_amount TO input_amount CURRENCY i_local_currency LEFT-JUSTIFIED.

    CONCATENATE input_amount i_local_currency INTO input_amount SEPARATED BY space.

    WRITE i_foreign_amount TO output_amount CURRENCY i_foreign_currency LEFT-JUSTIFIED.

    CONCATENATE output_amount i_foreign_currency INTO output_amount SEPARATED BY space.

    WRITE i_exchange_rate TO output_rate LEFT-JUSTIFIED.

    CONCATENATE '(' 'at'(004) output_rate ')' INTO output_rate SEPARATED BY space.

    CONCATENATE input_amount '=' output_amount output_rate INTO r_result SEPARATED BY space.

  ENDMETHOD.
ENDCLASS.
