CLASS /mbtools/cl_command__calc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
************************************************************************
* MBT Command - Calculator
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_command .

    ALIASES execute
      FOR /mbtools/if_command~execute .

    CLASS-METHODS class_constructor .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES command
      FOR /mbtools/if_command~mo_command .

    TYPES:
      ty_p0  TYPE p LENGTH 16 DECIMALS 0 .
    TYPES:
      ty_p7  TYPE p LENGTH 16 DECIMALS 7 .
    TYPES:
      ty_p14 TYPE p LENGTH 16 DECIMALS 14 .

    CONSTANTS c_callback_prog TYPE progname VALUE '/MBTOOLS/BC_COMMAND_FIELD' ##NO_TEXT.
    CONSTANTS c_callback_eval TYPE slis_formname VALUE 'CALLBACK_EVAL' ##NO_TEXT.

    CLASS-DATA mv_max_p0 TYPE ty_p0 .
    CLASS-DATA mv_min_p0 TYPE ty_p0 .
    CLASS-DATA mv_max_p7 TYPE ty_p7 .
    CLASS-DATA mv_min_p7 TYPE ty_p7 .
    CLASS-DATA mv_max_p14 TYPE ty_p14 .
    CLASS-DATA mv_min_p14 TYPE ty_p14 .

    METHODS format_result
      IMPORTING
        !iv_value        TYPE f
      RETURNING
        VALUE(rv_result) TYPE string .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND__CALC IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_subrc   TYPE sy-subrc,
      lv_value   TYPE string,
      lv_formula TYPE c LENGTH 1024, "max for eval
      lv_f       TYPE f,
      lv_c       TYPE c LENGTH 50,
      lv_icon    TYPE icon_d,
      lv_result  TYPE string.

    lv_icon = icon_message_error_small.

    lv_formula = iv_parameters.

    " First, check if it's just a property
    /mbtools/cl_utilities=>get_property(
      EXPORTING
        iv_property = iv_parameters
      IMPORTING
        ev_value    = lv_value
        ev_subrc    = lv_subrc ).

    IF lv_subrc = 0.
      lv_result = lv_value.
      lv_icon   = icon_equal.
    ELSE.
      " Next, evaluate and check if it was a condition (boolean)
      CALL FUNCTION 'EVAL_FORMULA'
        EXPORTING
          formula                 = lv_formula
          program                 = c_callback_prog
          routine                 = c_callback_eval
        IMPORTING
          value                   = lv_c
        EXCEPTIONS
          division_by_zero        = 1
          exp_error               = 2
          formula_table_not_valid = 3
          invalid_expression      = 4
          invalid_value           = 5
          log_error               = 6
          parameter_error         = 7
          sqrt_error              = 8
          units_not_valid         = 9
          missing_parameter       = 10
          OTHERS                  = 11.
      IF sy-subrc = 0 AND ( lv_c = 'TRUE' OR lv_c = 'FALSE' ) ##NO_TEXT.
        lv_result = lv_c.
      ELSE.
        " Otherwise evaluate with numeric result (float)
        CALL FUNCTION 'EVAL_FORMULA'
          EXPORTING
            formula                 = lv_formula
            program                 = c_callback_prog
            routine                 = c_callback_eval
          IMPORTING
            value                   = lv_f
          EXCEPTIONS
            division_by_zero        = 1
            exp_error               = 2
            formula_table_not_valid = 3
            invalid_expression      = 4
            invalid_value           = 5
            log_error               = 6
            parameter_error         = 7
            sqrt_error              = 8
            units_not_valid         = 9
            missing_parameter       = 10
            OTHERS                  = 11.
        IF sy-subrc = 0.
          " Format result nicely like a calculator
          lv_result = format_result( lv_f ).
          lv_icon   = icon_equal.
        ELSEIF sy-subrc BETWEEN 1 AND 4 OR sy-subrc BETWEEN 6 AND 10.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_result.
        ELSEIF sy-subrc = 5.
          lv_result = 'Unknown value'(001).
        ELSE.
          lv_result = 'Error in EVAL_FORMULA' ##NO_TEXT.
        ENDIF.
      ENDIF.
    ENDIF.

    CONCATENATE lv_formula '=' lv_result INTO lv_result SEPARATED BY space.

    rv_exit = /mbtools/cl_command_field=>show_result( iv_command    = iv_command
                                                      iv_parameters = iv_parameters
                                                      iv_icon       = lv_icon
                                                      iv_result     = lv_result
                                                      iv_via_popup  = iv_via_popup ).

  ENDMETHOD.


  METHOD class_constructor.

    DATA:
      lo_p TYPE REF TO data.

    FIELD-SYMBOLS:
      <lv_p> TYPE any.

    lo_p = cl_abap_exceptional_values=>get_max_value( mv_max_p0 ).
    ASSIGN lo_p->* TO <lv_p>.
    mv_max_p0 = <lv_p>.
    lo_p = cl_abap_exceptional_values=>get_min_value( mv_min_p0 ).
    ASSIGN lo_p->* TO <lv_p>.
    mv_min_p0 = <lv_p>.
    lo_p = cl_abap_exceptional_values=>get_max_value( mv_max_p7 ).
    ASSIGN lo_p->* TO <lv_p>.
    mv_max_p7 = <lv_p>.
    lo_p = cl_abap_exceptional_values=>get_min_value( mv_min_p7 ).
    ASSIGN lo_p->* TO <lv_p>.
    mv_min_p7 = <lv_p>.
    lo_p = cl_abap_exceptional_values=>get_max_value( mv_max_p14 ).
    ASSIGN lo_p->* TO <lv_p>.
    mv_max_p14 = <lv_p>.
    lo_p = cl_abap_exceptional_values=>get_min_value( mv_min_p14 ).
    ASSIGN lo_p->* TO <lv_p>.
    mv_min_p14 = <lv_p>.

  ENDMETHOD.


  METHOD format_result.

    DATA:
      lv_p0            TYPE ty_p0,
      lv_p7            TYPE ty_p7,
      lv_p14           TYPE ty_p14,
      ls_defaults      TYPE bapidefaul,
      lt_return        TYPE TABLE OF bapiret2,
      lv_decimal_point TYPE c,
      lv_pretty_result TYPE c LENGTH 100.

    " Format result nicely like a calculator
    lv_p14 = abs( frac( iv_value ) ).
    IF lv_p14 < '0.0000000001' ##LITERAL.
      IF iv_value BETWEEN mv_min_p0 AND mv_max_p0. " no decimals
        lv_p0 = iv_value.
        WRITE lv_p0 TO lv_pretty_result.
      ELSE.
        WRITE iv_value TO lv_pretty_result.
      ENDIF.
    ELSEIF iv_value BETWEEN mv_min_p14 AND mv_max_p14. " most decimals
      lv_p14 = iv_value.
      WRITE lv_p14 TO lv_pretty_result.
    ELSEIF iv_value BETWEEN mv_min_p7 AND mv_max_p7. " compromise
      lv_p7 = iv_value.
      WRITE lv_p7 TO lv_pretty_result.
    ELSE.
      WRITE iv_value TO lv_pretty_result. " float as fall-back
    ENDIF.

    " Temporarily remove sign
    IF iv_value < 0.
      SHIFT lv_pretty_result RIGHT DELETING TRAILING '-'.
    ELSE.
      SHIFT lv_pretty_result RIGHT DELETING TRAILING space.
    ENDIF.

    " Get setting for decimal notation
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = sy-uname
      IMPORTING
        defaults = ls_defaults
      TABLES
        return   = lt_return.

    CASE ls_defaults-dcpfm.
      WHEN space. "1.234.567,89
        lv_decimal_point = ','.
      WHEN 'X'. "1,234,567.89
        lv_decimal_point = '.'.
      WHEN 'Y'. "1 234 567,89
        lv_decimal_point = ','.
    ENDCASE.

    " Remove trailing zeros after decimal point
    IF rv_result CS lv_decimal_point.
      SHIFT lv_pretty_result RIGHT DELETING TRAILING '0'.
    ENDIF.

    " All this and we end up with nothing?
    SHIFT lv_pretty_result LEFT DELETING LEADING space.
    IF lv_pretty_result IS INITIAL.
      lv_pretty_result = '0'.
    ENDIF.

    " Add sign back
    IF iv_value < 0.
      CONCATENATE '-' lv_pretty_result INTO lv_pretty_result.
    ENDIF.

    rv_result = lv_pretty_result.

  ENDMETHOD.
ENDCLASS.
