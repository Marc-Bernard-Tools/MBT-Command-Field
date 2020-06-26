************************************************************************
* /MBTOOLS/CL_SAP
* MBT SAP
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_sap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_domain_value,
        domvalue_l TYPE domvalue_l,
        valpos     TYPE valpos,
        appval     TYPE ddappval,
        ddtext     TYPE val_text,
      END OF ty_domain_value .
    TYPES:
      ty_domain_values TYPE STANDARD TABLE OF ty_domain_value WITH DEFAULT KEY .

    CLASS-METHODS class_constructor .
    CLASS-METHODS get_object_wo_namespace
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE /mbtools/if_definitions=>ty_name .
    CLASS-METHODS get_namespace
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE namespace .
    CLASS-METHODS get_object_text
      IMPORTING
        VALUE(iv_object) TYPE csequence
      RETURNING
        VALUE(rv_text)   TYPE ddtext .
    CLASS-METHODS get_object_texts
      RETURNING
        VALUE(rt_object_texts) TYPE /mbtools/if_definitions=>ty_object_texts .
    CLASS-METHODS get_text_from_domain
      IMPORTING
        !iv_domain     TYPE any DEFAULT 'YESNO'
        !iv_value      TYPE any
      EXPORTING
        VALUE(ev_text) TYPE clike .
    CLASS-METHODS get_values_from_domain
      IMPORTING
        !iv_domain       TYPE any
      RETURNING
        VALUE(rt_values) TYPE ty_domain_values .
    CLASS-METHODS is_devc_deleted
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    CLASS-METHODS is_fugr_deleted
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    CLASS-METHODS is_sap_note
      IMPORTING
        !iv_input        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    CLASS-METHODS is_tobj_deleted
      IMPORTING
        !iv_obj_name     TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    CLASS-METHODS object_name_check
      IMPORTING
        !iv_input        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS show_object
      IMPORTING
        !iv_pgmid      TYPE csequence DEFAULT 'R3TR'
        !iv_object     TYPE csequence
        !iv_obj_name   TYPE csequence
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
    CLASS-METHODS run_transaction
      IMPORTING
        !iv_tcode      TYPE csequence
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
    CLASS-METHODS run_program
      IMPORTING
        !iv_program    TYPE csequence
      RETURNING
        VALUE(rv_exit) TYPE abap_bool .
ENDCLASS.
CLASS /mbtools/cl_sap IMPLEMENTATION.
  METHOD is_sap_note.
  ENDMETHOD.
  METHOD object_name_check.
  ENDMETHOD.
  METHOD class_constructor.
  ENDMETHOD.
  METHOD get_object_text.
  ENDMETHOD.
  METHOD get_object_texts.
  ENDMETHOD.
  METHOD is_devc_deleted.
  ENDMETHOD.
  METHOD is_tobj_deleted.
  ENDMETHOD.
  METHOD get_namespace.
  ENDMETHOD.
  METHOD get_object_wo_namespace.
  ENDMETHOD.
  METHOD show_object.
  ENDMETHOD.
  METHOD get_text_from_domain.
  ENDMETHOD.
  METHOD is_fugr_deleted.
  ENDMETHOD.
  METHOD get_values_from_domain.
  ENDMETHOD.
  METHOD run_transaction.
  ENDMETHOD.
  METHOD run_program.
  ENDMETHOD.
ENDCLASS.