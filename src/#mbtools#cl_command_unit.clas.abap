************************************************************************
* /MBTOOLS/CL_COMMAND_UNIT
* MBT Command - Unit Conversion
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_command_unit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /mbtools/if_command .

    ALIASES execute
      FOR /mbtools/if_command~execute .

  PROTECTED SECTION.

private section.

  aliases COMMAND
    for /MBTOOLS/IF_COMMAND~COMMAND .

  types:
    ty_quantity TYPE p LENGTH 16 DECIMALS 5 .

  methods FORMAT_RESULT
    importing
      !I_FROM_QUANTITY type TY_QUANTITY
      !I_FROM_UNIT type MSEHI
      !I_TO_QUANTITY type TY_QUANTITY
      !I_TO_UNIT type MSEHI
      !I_DENOMINATOR type I
      !I_NUMERATOR type I
    returning
      value(R_RESULT) type STRING .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND_UNIT IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      object         TYPE string,
      input_quantity TYPE c LENGTH 50,
      input_unit     TYPE msehi,
      output_unit    TYPE msehi,
      from_quantity  TYPE ty_quantity,
      from_unit      TYPE msehi,
      to_quantity    TYPE ty_quantity,
      to_unit        TYPE msehi,
      denominator    TYPE i,
      numerator      TYPE i,
      icon           TYPE icon_d,
      result         TYPE string.

    icon = icon_message_error_small.

    " 100 M in Miles
    SPLIT i_parameters AT space INTO input_quantity input_unit sy-lisel output_unit.

    TRY.
        from_quantity = input_quantity.
      CATCH cx_root.
        result = 'Quantity not numeric'(001).
    ENDTRY.

    IF result IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = input_unit
        IMPORTING
          output         = from_unit
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        result = 'Source unit not found'(002).
      ENDIF.
    ENDIF.

    IF result IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = output_unit
        IMPORTING
          output         = to_unit
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        result = 'Target unit not found'(003).
      ENDIF.
    ENDIF.

    IF result IS INITIAL.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input                = from_quantity
          unit_in              = from_unit
          unit_out             = to_unit
        IMPORTING
          denominator          = denominator
          numerator            = numerator
          output               = to_quantity
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
        result = format_result( i_from_quantity = from_quantity
                                i_from_unit     = from_unit
                                i_to_quantity   = to_quantity
                                i_to_unit       = to_unit
                                i_denominator   = denominator
                                i_numerator     = numerator ).

        icon = icon_bw_convert_unit.
      ELSEIF sy-subrc BETWEEN 1 AND 9.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO result.
      ELSE.
        result = 'Error in UNIT_CONVERSION+SIMPLE' ##NO_TEXT.
      ENDIF.
    ENDIF.

    r_exit = /mbtools/cl_command_field=>show_result( i_command    = i_command
                                                     i_parameters = i_parameters
                                                     i_icon       = icon
                                                     i_result     = result
                                                     i_via_popup  = i_via_popup ).

  ENDMETHOD.


  METHOD format_result.

    DATA:
      input_quantity  TYPE c LENGTH 50,
      output_quantity TYPE c LENGTH 50,
      output_ratio    TYPE c LENGTH 50.

    WRITE i_from_quantity TO input_quantity UNIT i_from_unit LEFT-JUSTIFIED.

    CONCATENATE input_quantity i_from_unit INTO input_quantity SEPARATED BY space.

    WRITE i_to_quantity TO output_quantity UNIT i_to_unit LEFT-JUSTIFIED.

    CONCATENATE output_quantity i_to_unit INTO output_quantity SEPARATED BY space.

    output_ratio = |{ i_denominator }:{ i_numerator }|.

    CONCATENATE '(' 'at'(004) output_ratio ')' INTO output_ratio SEPARATED BY space.

    CONCATENATE input_quantity '=' output_quantity output_ratio INTO r_result SEPARATED BY space.

  ENDMETHOD.
ENDCLASS.
