************************************************************************
* /MBTOOLS/CL_COMMAND_CALC
* MBT Command - Calculator
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
class /MBTOOLS/CL_COMMAND_CALC definition
  public
  final
  create public .

public section.

  interfaces /MBTOOLS/IF_COMMAND .

  aliases EXECUTE
    for /MBTOOLS/IF_COMMAND~EXECUTE .

  class-methods CLASS_CONSTRUCTOR .
  PROTECTED SECTION.

private section.

  aliases COMMAND
    for /MBTOOLS/IF_COMMAND~COMMAND .

  types:
    ty_p0  TYPE p LENGTH 16 DECIMALS 0 .
  types:
    ty_p7  TYPE p LENGTH 16 DECIMALS 7 .
  types:
    ty_p14 TYPE p LENGTH 16 DECIMALS 14 .

  constants C_CALLBACK_PROG type PROGNAME value '/MBTOOLS/BC_COMMAND_FIELD' ##NO_TEXT.
  constants C_CALLBACK_EVAL type SLIS_FORMNAME value 'CALLBACK_EVAL' ##NO_TEXT.
  class-data MAX_P0 type TY_P0 .
  class-data MIN_P0 type TY_P0 .
  class-data MAX_P7 type TY_P7 .
  class-data MIN_P7 type TY_P7 .
  class-data MAX_P14 type TY_P14 .
  class-data MIN_P14 type TY_P14 .

  methods FORMAT_RESULT
    importing
      !I_VALUE type F
    returning
      value(R_RESULT) type STRING .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND_CALC IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      subrc   TYPE sy-subrc,
      value   TYPE string,
      formula TYPE c LENGTH 1024, "max for eval
      f       TYPE f,
      c       TYPE c LENGTH 50,
      icon    TYPE icon_d,
      result  TYPE string.

    icon = icon_message_error_small.

    formula = i_parameters.

    " First, check if it's just a property
    CALL METHOD /mbtools/cl_utilities=>get_property
      EXPORTING
        i_property = i_parameters
      IMPORTING
        e_value    = value
        e_subrc    = subrc.

    IF subrc = 0.
      result = value.
      icon   = icon_equal.
    ELSE.
      " Next, evaluate and check if it was a condition (boolean)
      CALL FUNCTION 'EVAL_FORMULA'
        EXPORTING
          formula                 = formula
          program                 = c_callback_prog
          routine                 = c_callback_eval
        IMPORTING
          value                   = c
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
      IF sy-subrc = 0 AND ( c = 'TRUE' OR c = 'FALSE' ) ##NO_TEXT.
        result = c.
      ELSE.
        " Otherwise evaluate with numeric result (float)
        CALL FUNCTION 'EVAL_FORMULA'
          EXPORTING
            formula                 = formula
            program                 = c_callback_prog
            routine                 = c_callback_eval
          IMPORTING
            value                   = f
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
          result = format_result( f ).
          icon   = icon_equal.
        ELSEIF sy-subrc BETWEEN 1 AND 4 or sy-subrc BETWEEN 6 and 10.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO result.
        ELSEIF sy-subrc = 5.
          result = 'Unknown value'(001).
        ELSE.
          result = 'Error in EVAL_FORMULA' ##NO_TEXT.
        ENDIF.
      ENDIF.
    ENDIF.

    CONCATENATE formula '=' result INTO result SEPARATED BY space.

    r_exit = /mbtools/cl_command_field=>show_result( i_command    = i_command
                                                     i_parameters = i_parameters
                                                     i_icon       = icon
                                                     i_result     = result
                                                     i_via_popup  = i_via_popup ).

  ENDMETHOD.


  METHOD class_constructor.

    DATA pref TYPE REF TO data.

    FIELD-SYMBOLS <p> TYPE any.

    pref = cl_abap_exceptional_values=>get_max_value( max_p0 ).
    ASSIGN pref->* TO <p>.
    max_p0 = <p>.
    pref = cl_abap_exceptional_values=>get_min_value( min_p0 ).
    ASSIGN pref->* TO <p>.
    min_p0 = <p>.
    pref = cl_abap_exceptional_values=>get_max_value( max_p7 ).
    ASSIGN pref->* TO <p>.
    max_p7 = <p>.
    pref = cl_abap_exceptional_values=>get_min_value( min_p7 ).
    ASSIGN pref->* TO <p>.
    min_p7 = <p>.
    pref = cl_abap_exceptional_values=>get_max_value( max_p14 ).
    ASSIGN pref->* TO <p>.
    max_p14 = <p>.
    pref = cl_abap_exceptional_values=>get_min_value( min_p14 ).
    ASSIGN pref->* TO <p>.
    min_p14 = <p>.

  ENDMETHOD.


  METHOD format_result.

    DATA:
      p0            TYPE ty_p0,
      p7            TYPE ty_p7,
      p14           TYPE ty_p14,
      defaults      TYPE bapidefaul,
      return        TYPE TABLE OF bapiret2,
      decimal_point TYPE c,
      pretty_result TYPE c LENGTH 100.

    " Format result nicely like a calculator
    p14 = abs( frac( i_value ) ).
    IF p14 < '0.0000000001'.
      IF i_value BETWEEN min_p0 AND max_p0. " no decimals
        p0 = i_value.
        WRITE p0 TO pretty_result.
      ELSE.
        WRITE i_value TO pretty_result.
      ENDIF.
    ELSEIF i_value BETWEEN min_p14 AND max_p14. " most decimals
      p14 = i_value.
      WRITE p14 TO pretty_result.
    ELSEIF i_value BETWEEN min_p7 AND max_p7. " compromise
      p7 = i_value.
      WRITE p7 TO pretty_result.
    ELSE.
      WRITE i_value TO pretty_result. " float as fall-back
    ENDIF.

    " Temporarily remove sign
    IF i_value < 0.
      SHIFT pretty_result RIGHT DELETING TRAILING '-'.
    ELSE.
      SHIFT pretty_result RIGHT DELETING TRAILING space.
    ENDIF.

    " Get setting for decimal notation
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = sy-uname
      IMPORTING
        defaults = defaults
      TABLES
        return   = return.

    CASE defaults-dcpfm.
      WHEN space. "1.234.567,89
        decimal_point = ','.
      WHEN 'X'. "1,234,567.89
        decimal_point = '.'.
      WHEN 'Y'. "1 234 567,89
        decimal_point = ','.
    ENDCASE.

    " Remove trailing zeros after decimal point
    IF r_result CS decimal_point.
      SHIFT pretty_result RIGHT DELETING TRAILING '0'.
    ENDIF.

    " All this and we end up with nothing?
    SHIFT pretty_result LEFT DELETING LEADING space.
    IF pretty_result IS INITIAL.
      pretty_result = '0'.
    ENDIF.

    " Add sign back
    IF i_value < 0.
      CONCATENATE '-' pretty_result INTO pretty_result.
    ENDIF.

    r_result = pretty_result.

  ENDMETHOD.
ENDCLASS.
