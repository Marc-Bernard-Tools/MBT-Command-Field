************************************************************************
* /MBTOOLS/CL_UTILITIES
* MBT Utilities
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_strv_release_patch,
        release TYPE n LENGTH 3,
        version TYPE n LENGTH 5,
        patch   TYPE n LENGTH 5,
      END OF ty_strv_release_patch .

    CONSTANTS:
      BEGIN OF c_property,
        year             TYPE string VALUE 'YEAR',
        month            TYPE string VALUE 'MONTH',
        day              TYPE string VALUE 'DAY',
        hour             TYPE string VALUE 'HOUR',
        minute           TYPE string VALUE 'MINUTE',
        second           TYPE string VALUE 'SECOND',
        database         TYPE string VALUE 'DB',
        database_release TYPE string VALUE 'DB_RELEASE',
        database_patch   TYPE string VALUE 'DB_PATCH',
        dbsl_release     TYPE string VALUE 'DBSL_RELEASE',
        dbsl_patch       TYPE string VALUE 'DBSL_PATCH',
        hana             TYPE string VALUE 'HANA',
        hana_release     TYPE string VALUE 'HANA_RELEASE',
        hana_sp          TYPE string VALUE 'HANA_SP',
        hana_revision    TYPE string VALUE 'HANA_REVISION',
        hana_patch       TYPE string VALUE 'HANA_PATCH',
        spam_release     TYPE string VALUE 'SPAM_RELEASE',
        spam_version     TYPE string VALUE 'SPAM_VERSION',
        kernel           TYPE string VALUE 'KERNEL',
        kernel_release   TYPE string VALUE 'KERNEL_RELEASE',
        kernel_patch     TYPE string VALUE 'KERNEL_PATCH',
        kernel_bits      TYPE string VALUE 'KERNEL_BITS',
        unicode          TYPE string VALUE 'UNICODE',
      END OF c_property .
    CONSTANTS c_unknown TYPE string VALUE 'UNKNOWN' ##NO_TEXT.

    CLASS-METHODS call_browser
      IMPORTING
        !iv_url TYPE csequence .
    CLASS-METHODS is_batch
      RETURNING
        VALUE(rv_batch) TYPE abap_bool .
    CLASS-METHODS is_system_modifiable
      RETURNING
        VALUE(rv_modifiable) TYPE abap_bool .
    CLASS-METHODS is_system_test_or_prod
      RETURNING
        VALUE(rv_test_prod) TYPE abap_bool .
    CLASS-METHODS is_snote_allowed
      RETURNING
        VALUE(rv_snote_allowed) TYPE abap_bool .
    CLASS-METHODS is_upgrage_running
      RETURNING
        VALUE(rv_upgrade_running) TYPE abap_bool .
    CLASS-METHODS is_spam_locked
      RETURNING
        VALUE(rv_spam_locked) TYPE abap_bool .
    CLASS-METHODS get_property
      IMPORTING
        VALUE(iv_property) TYPE clike
      EXPORTING
        !ev_value          TYPE string
        !ev_value_float    TYPE f
        !ev_value_integer  TYPE i
        !ev_subrc          TYPE sy-subrc .
    CLASS-METHODS get_syst_field
      IMPORTING
        VALUE(iv_field) TYPE clike
      RETURNING
        VALUE(rv_value) TYPE string .
    CLASS-METHODS get_db_release
      EXPORTING
        !es_dbinfo       TYPE dbrelinfo
        !es_hana_release TYPE ty_strv_release_patch .
    CLASS-METHODS get_spam_release
      RETURNING
        VALUE(rs_details) TYPE ty_strv_release_patch .
    CLASS-METHODS get_kernel_release
      RETURNING
        VALUE(rs_details) TYPE ty_strv_release_patch .
    CLASS-METHODS get_swcomp_release
      IMPORTING
        VALUE(iv_component) TYPE clike
      RETURNING
        VALUE(rv_release)   TYPE string .
    CLASS-METHODS get_swcomp_support_package
      IMPORTING
        VALUE(iv_component)       TYPE clike
      RETURNING
        VALUE(rv_support_package) TYPE string .
    CLASS-METHODS get_profile_parameter
      IMPORTING
        VALUE(iv_parameter) TYPE clike
      RETURNING
        VALUE(rv_value)     TYPE string .
ENDCLASS.
CLASS /mbtools/cl_utilities IMPLEMENTATION.
  METHOD call_browser.
  ENDMETHOD.
  METHOD get_db_release.
  ENDMETHOD.
  METHOD get_kernel_release.
  ENDMETHOD.
  METHOD get_profile_parameter.
  ENDMETHOD.
  METHOD get_property.
  ENDMETHOD.
  METHOD get_spam_release.
  ENDMETHOD.
  METHOD get_swcomp_release.
  ENDMETHOD.
  METHOD get_swcomp_support_package.
  ENDMETHOD.
  METHOD is_batch.
  ENDMETHOD.
  METHOD is_snote_allowed.
  ENDMETHOD.
  METHOD is_spam_locked.
  ENDMETHOD.
  METHOD is_system_modifiable.
  ENDMETHOD.
  METHOD is_system_test_or_prod.
  ENDMETHOD.
  METHOD is_upgrage_running.
  ENDMETHOD.
  METHOD get_syst_field.
  ENDMETHOD.
ENDCLASS.