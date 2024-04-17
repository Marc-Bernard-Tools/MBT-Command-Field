
CLASS ltcl_command_show DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING
          iv_tcode TYPE csequence
          iv_type  TYPE csequence
          iv_name  TYPE csequence,
      dialog_transaction FOR TESTING,
      report_transaction FOR TESTING,
      oo_transaction FOR TESTING,
      variant_transaction FOR TESTING,
      parameter_transaction FOR TESTING.

ENDCLASS.

CLASS /mbtools/cl_command__show DEFINITION LOCAL FRIENDS ltcl_command_show.

CLASS ltcl_command_show IMPLEMENTATION.

  METHOD test.

    DATA lv_tcode TYPE tstc-tcode.
    DATA ls_act TYPE /mbtools/if_definitions=>ty_tadir_key.

    SELECT SINGLE tcode INTO lv_tcode FROM tstc WHERE tcode = iv_tcode.
    CHECK sy-subrc = 0.

    ls_act = /mbtools/cl_command__show=>get_object_from_tcode( iv_tcode ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_act-object
      exp = iv_type ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_act-obj_name
      exp = iv_name ).

  ENDMETHOD.

  METHOD dialog_transaction.

    test(
      iv_tcode = 'SE16'
      iv_type  = 'PROG'
      iv_name  = 'SAPLSETB' ).

  ENDMETHOD.

  METHOD report_transaction.

    test(
      iv_tcode = 'SE91'
      iv_type  = 'PROG'
      iv_name  = 'RSMESSAGES' ).

  ENDMETHOD.

  METHOD oo_transaction.

    " OO transactional model
    test(
      iv_tcode = 'SE20'
      iv_type  = 'PROG'
      iv_name  = 'CL_ENHANCEMENTS===============CM001' ).

    " Local OO transactional model
    test(
      iv_tcode = 'SE30'
      iv_type  = 'PROG'
      iv_name  = 'SATRA_START' ).

  ENDMETHOD.

  METHOD variant_transaction.

    test(
      iv_tcode = 'SU0'
      iv_type  = 'PROG'
      iv_name  = 'SAPMSUU0O' ).

  ENDMETHOD.

  METHOD parameter_transaction.

    test(
      iv_tcode = 'SCC4'
      iv_type  = 'VIEW'
      iv_name  = 'T000' ).

    test(
      iv_tcode = 'SEGW'
      iv_type  = 'PROG'
      iv_name  = '/IWBEP/R_SBUI_SERVICE_BUILDER' ).

    test(
      iv_tcode = 'SE80'
      iv_type  = 'PROG'
      iv_name  = 'SAPMSEU0' ).

    test(
      iv_tcode = 'RBDAPP01'
      iv_type  = 'PROG'
      iv_name  = 'RBDAPP01' ).

    test(
      iv_tcode = 'SE16T000'
      iv_type  = 'TABL'
      iv_name  = 'T000' ).

    test(
      iv_tcode = 'STRORG'
      iv_type  = 'WDYA'
      iv_name  = 'CTS_ORGANIZER' ).

  ENDMETHOD.

ENDCLASS.
