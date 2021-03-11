CLASS /mbtools/cl_command__unit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
************************************************************************
* MBT Command - Unit Conversion
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_command .

    ALIASES execute
      FOR /mbtools/if_command~execute .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES command
      FOR /mbtools/if_command~mo_command .

    TYPES:
      ty_quantity TYPE p LENGTH 16 DECIMALS 5 .

    METHODS format_result
      IMPORTING
        !iv_from_quantity TYPE ty_quantity
        !iv_from_unit     TYPE msehi
        !iv_to_quantity   TYPE ty_quantity
        !iv_to_unit       TYPE msehi
        !iv_denominator   TYPE i
        !iv_numerator     TYPE i
      RETURNING
        VALUE(rv_result)  TYPE string .
ENDCLASS.



CLASS /mbtools/cl_command__unit IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_input_quantity TYPE c LENGTH 50,
      lv_input_unit     TYPE msehi,
      lv_output_unit    TYPE msehi,
      lv_from_quantity  TYPE ty_quantity,
      lv_from_unit      TYPE msehi,
      lv_to_quantity    TYPE ty_quantity,
      lv_to_unit        TYPE msehi,
      lv_denominator    TYPE i,
      lv_numerator      TYPE i,
      lv_icon           TYPE icon_d,
      lv_rest           TYPE string,
      lv_result         TYPE string.

    lv_icon = icon_message_error_small.

    " 100 M in Miles
    SPLIT iv_parameters AT space INTO lv_input_quantity lv_input_unit lv_rest lv_output_unit.

    " Some mapping for common cases
    CASE lv_input_unit.
      WHEN 'C'. "Degrees Celsius
        lv_input_unit = '°C'.
      WHEN 'F'. "Fahrenheit
        lv_input_unit = '°F'.
      WHEN 'FA'. "farad
        lv_input_unit = 'F'.
    ENDCASE.

    CASE lv_output_unit.
      WHEN 'C'. "Degrees Celsius
        lv_output_unit = '°C'.
      WHEN 'F'. "Fahrenheit
        lv_output_unit = '°F'.
      WHEN 'FA'. "farad
        lv_output_unit = 'F'.
    ENDCASE.

    TRY.
        lv_from_quantity = lv_input_quantity.
      CATCH cx_root.
        lv_result = 'Quantity not numeric'(001).
    ENDTRY.

    IF lv_result IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = lv_input_unit
        IMPORTING
          output         = lv_from_unit
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        lv_result = 'Source unit not found'(002).
      ENDIF.
    ENDIF.

    IF lv_result IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = lv_output_unit
        IMPORTING
          output         = lv_to_unit
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        lv_result = 'Target unit not found'(003).
      ENDIF.
    ENDIF.

    IF lv_result IS INITIAL.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input                = lv_from_quantity
          unit_in              = lv_from_unit
          unit_out             = lv_to_unit
        IMPORTING
          denominator          = lv_denominator
          numerator            = lv_numerator
          output               = lv_to_quantity
        EXCEPTIONS
          conversion_not_found = 1
          division_by_zero     = 2
          input_invalid        = 3
          output_invalid       = 4
          overflow             = 5
          type_invalid         = 6
          units_missing        = 7
          unit_in_not_found    = 8
          unit_out_not_found   = 9
          OTHERS               = 10.
      IF sy-subrc = 0.
        " Format result nicely
        lv_result = format_result( iv_from_quantity = lv_from_quantity
                                   iv_from_unit     = lv_from_unit
                                   iv_to_quantity   = lv_to_quantity
                                   iv_to_unit       = lv_to_unit
                                   iv_denominator   = lv_denominator
                                   iv_numerator     = lv_numerator ).

        lv_icon = icon_bw_convert_unit.
      ELSEIF sy-subrc BETWEEN 1 AND 9.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_result.
      ELSE.
        lv_result = 'Error in UNIT_CONVERSION+SIMPLE' ##NO_TEXT.
      ENDIF.
    ENDIF.

    rv_exit = /mbtools/cl_command_field=>show_result( iv_command    = iv_command
                                                      iv_parameters = iv_parameters
                                                      iv_icon       = lv_icon
                                                      iv_result     = lv_result
                                                      iv_via_popup  = iv_via_popup ).

  ENDMETHOD.


  METHOD format_result.

    DATA:
      lv_input_quantity  TYPE c LENGTH 50,
      lv_input_unit      TYPE t006a-mseh6,
      lv_output_quantity TYPE c LENGTH 50,
      lv_output_unit     TYPE t006a-mseh6,
      lv_output_ratio    TYPE c LENGTH 50.

    WRITE iv_from_quantity TO lv_input_quantity UNIT iv_from_unit LEFT-JUSTIFIED.

    CALL FUNCTION 'CONVERSION_EXIT_LUNIT_OUTPUT'
      EXPORTING
        input          = iv_from_unit
      IMPORTING
        output         = lv_input_unit
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      lv_input_unit = iv_from_unit.
    ENDIF.

    CONCATENATE lv_input_quantity lv_input_unit INTO lv_input_quantity SEPARATED BY space.

    CALL FUNCTION 'CONVERSION_EXIT_LUNIT_OUTPUT'
      EXPORTING
        input          = iv_to_unit
      IMPORTING
        output         = lv_output_unit
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      lv_output_unit = iv_to_unit.
    ENDIF.

    WRITE iv_to_quantity TO lv_output_quantity UNIT iv_to_unit LEFT-JUSTIFIED.

    CONCATENATE lv_output_quantity lv_output_unit INTO lv_output_quantity SEPARATED BY space.

    lv_output_ratio = |{ iv_denominator }:{ iv_numerator }|.

    CONCATENATE '(' 'at'(004) lv_output_ratio ')' INTO lv_output_ratio SEPARATED BY space.

    CONCATENATE lv_input_quantity '=' lv_output_quantity lv_output_ratio INTO rv_result SEPARATED BY space.

  ENDMETHOD.
ENDCLASS.
