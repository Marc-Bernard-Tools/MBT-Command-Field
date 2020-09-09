CLASS /mbtools/cl_command__trace DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Command - Show
*
* (c) MBT 2020 https://marcbernardtools.com/
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

    METHODS trace_sat
      IMPORTING
        !iv_switch     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
    METHODS trace_sql
      IMPORTING
        !iv_switch     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND__TRACE IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_operator TYPE string,
      lv_operand  TYPE string,
      lv_switch   TYPE abap_bool.

    " Split parameters into object and object name
    command->split(
      EXPORTING
        iv_parameters = iv_parameters
      IMPORTING
        ev_operator   = lv_operator
        ev_operand    = lv_operand ).

    IF lv_operator IS INITIAL.
      lv_operator = 'SAT'.
    ENDIF.

    CASE to_upper( lv_operand ).
      WHEN '' OR 'ON'.
        lv_switch = abap_true.
      WHEN 'OFF'.
        lv_switch = abap_false.
      WHEN OTHERS.
        MESSAGE 'Unknown trace switch. Use ON or OFF.' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDCASE.

    CASE lv_operator.
      WHEN 'SAT'.
        rv_exit = trace_sat( lv_switch ).
      WHEN 'SQL'.
        rv_exit = trace_sql( lv_switch ).
      WHEN OTHERS.
        MESSAGE 'Unknown type of trace. Use SAT or SQL.' TYPE 'S' DISPLAY LIKE 'E'.
    ENDCASE.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.


  METHOD trace_sat.

    DATA:
      ls_header TYPE satrh_header,
      lx_exc    TYPE REF TO cx_static_check.

    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
                    ID     'OBJTYPE'   FIELD 'SYST'
                    ID     'DEVCLASS'  DUMMY
                    ID     'P_GROUP'   DUMMY
                    ID     'OBJNAME'   DUMMY
                    ID     'ACTVT'     DUMMY.
    IF sy-subrc <> 0.
      MESSAGE s011(s7) DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    CASE iv_switch.
      WHEN abap_true.

        TRY.
            cl_abap_trace_switch=>on(
              p_prog  = sy-cprog
              p_type  = 'R'
              p_vname = 'DEFAULT'
              p_vuser = sy-uname
              p_comm  = 'MBT Performance Trace' ).
*               p_vref  = CL_ABAP_TRACE=>DEFAULT_VARIANT
*                p_particular_units  = abap_false
*               p_x_resolution_low  = ' '
*               p_x_no_autoclose    = ' '
*               p_ureq_suser        = ' '
*               p_ureq_client       = p_ureq_client
*               p_ureq_emode        = p_ureq_emode
*               p_ureq_ptype        = p_ureq_ptype
*               p_ureq_oname        = p_ureq_oname
*               p_ureq_iruns        = p_ureq_iruns
*               p_ureq_edate        = p_ureq_edate
*               p_ureq_etime        = p_ureq_etime

            MESSAGE 'Measurement started' TYPE 'S'.
          CATCH cx_static_check INTO lx_exc.
            MESSAGE lx_exc TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

*        SET RUN TIME ANALYZER ON.

      WHEN abap_false.

        TRY.
            ls_header = cl_abap_trace_switch=>off( veri_check = abap_true ).

          CATCH cx_static_check INTO lx_exc.
            MESSAGE lx_exc TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

*        SET RUN TIME ANALYZER OFF.

    ENDCASE.

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD trace_sql.
  ENDMETHOD.
ENDCLASS.
