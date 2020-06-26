************************************************************************
* /MBTOOLS/CL_COMMAND_FIND
* MBT Command - Find
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_command__find DEFINITION
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
      FOR /mbtools/if_command~mo_command .

    CONSTANTS c_support_launchpad TYPE string VALUE 'https://launchpad.support.sap.com/' ##NO_TEXT.
    CONSTANTS c_support_apps TYPE string VALUE 'https://apps.support.sap.com/' ##NO_TEXT.
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND__FIND IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      lv_application TYPE string,
      lv_object      TYPE string,
      lv_url_domain  TYPE string,
      lv_url_path    TYPE string,
      lv_url_term    TYPE string,
      lv_url         TYPE string.

    " Split parameters into application and object
    command->split(
      EXPORTING
        iv_parameters = iv_parameters
      IMPORTING
        ev_operator   = lv_application
        ev_operand    = lv_object ).

    " Default is SAP ONE Support Launchpad
    lv_url_domain = c_support_launchpad.

    " SAP Notes
    IF /mbtools/cl_sap=>is_sap_note( lv_object ) = abap_true.
      lv_application = 'NOTE'.
    ENDIF.

    CASE lv_application.
      WHEN 'NOTE' OR 'KBA'.
        " Knowledge Base (SAP Note or KBA)
        IF lv_object CO '0123456789'.
          lv_url_path = '/notes/' ##NO_TEXT.
        ELSE.
          lv_url_path = '/mynotes?tab=Search&q=' ##NO_TEXT.
        ENDIF.
      WHEN 'DOWNLOAD' OR 'DOWN' OR 'DL' OR 'SWDC'.
        " Downloads (Software Center)
        lv_url_path = '/softwarecenter/search/' ##NO_TEXT.
      WHEN 'INCIDENT' OR 'INCI' OR 'INC'.
        " Incidents
        lv_url_path = '/incident/search/' ##NO_TEXT.
      WHEN 'INNOVATION' OR 'INNO'.
        " Innovations
        lv_url_path = '/innovations/?searchTerm=' ##NO_TEXT.
      WHEN 'INSTALLATION' OR 'INSTALL' OR 'INST'.
        " Installations
        IF lv_object CO '0123456789'.
          lv_url_path = '/installation/management/' ##NO_TEXT.
        ELSE.
          lv_url_path = '/installation/management/search/' ##NO_TEXT.
        ENDIF.
      WHEN 'LICENSE' OR 'LIC'.
        " License Keys
        lv_url_path = '/licensekey/search/' ##NO_TEXT.
      WHEN 'PRODUCT' OR 'PROD' OR 'PRD'.
        " Products
        lv_url_path = '/productsearch/' ##NO_TEXT.
      WHEN 'SYSTEM' OR 'SYST' OR 'SYS' OR 'SID'.
        " Systems
        IF lv_object CO '0123456789'.
          lv_url_path = '/systemdata/' ##NO_TEXT.
        ELSE.
          lv_url_path = '/systemdata/search/' ##NO_TEXT.
        ENDIF.
      WHEN 'USER' OR 'USR'.
        " Users
        lv_url_path = '/user/management/user_admin/search/' ##NO_TEXT.
      WHEN 'PAM'.
        " Product Availability Matrix
        lv_url_domain = c_support_apps.
        lv_url_path = '/sap/support/pam/pam.html' ##NO_TEXT.
        lv_url_path = lv_url_path && '?ts=1&o=most_viewed%7Cdesc&st=l&rpp=20&page=1&s=' ##NO_TEXT.
      WHEN OTHERS.
        " Default Cross-Search
        lv_url_path = '/solutions/notesv2/?q=' ##NO_TEXT.
    ENDCASE.

    lv_url_term = cl_http_utility=>escape_url( unescaped = lv_object ).

    CONCATENATE lv_url_domain '#' lv_url_path lv_url_term INTO lv_url.

    /mbtools/cl_utilities=>call_browser( lv_url ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.
ENDCLASS.
