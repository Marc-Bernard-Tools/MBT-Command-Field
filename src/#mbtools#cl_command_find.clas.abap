************************************************************************
* /MBTOOLS/CL_COMMAND_FIND
* MBT Command - Find
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
class /MBTOOLS/CL_COMMAND_FIND definition
  public
  final
  create public .

public section.

  interfaces /MBTOOLS/IF_COMMAND .

  aliases EXECUTE
    for /MBTOOLS/IF_COMMAND~EXECUTE .

  methods CONSTRUCTOR .
  PROTECTED SECTION.

private section.

  aliases COMMAND
    for /MBTOOLS/IF_COMMAND~COMMAND .

  constants C_SUPPORT_LAUNCHPAD type STRING value 'https://launchpad.support.sap.com/' ##NO_TEXT.
  constants C_SUPPORT_APPS type STRING value 'https://apps.support.sap.com/' ##NO_TEXT.
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND_FIND IMPLEMENTATION.


  METHOD /mbtools/if_command~execute.

    DATA:
      application TYPE string,
      object      TYPE string,
      url_domain  TYPE string,
      url_path    TYPE string,
      url_term    TYPE string,
      url         TYPE string.

    " Split parameters into application and object
    CALL METHOD command->split
      EXPORTING
        i_parameters = i_parameters
      IMPORTING
        e_operator   = application
        e_operand    = object.

    " Default is SAP ONE Support Launchpad
    url_domain = c_support_launchpad.

    " SAP Notes
    IF /mbtools/cl_sap=>is_sap_note( object ).
      application = 'NOTE'.
    ENDIF.

    CASE application.
      WHEN 'NOTE' OR 'KBA'.
        " Knowledge Base (SAP Note or KBA)
        IF object CO '0123456789'.
            url_path = '/notes/' ##NO_TEXT.
        ELSE.
            url_path = '/mynotes?tab=Search&q=' ##NO_TEXT.
        ENDIF.
      WHEN 'DOWNLOAD' OR 'DOWN' OR 'DL' OR 'SWDC'.
        " Downloads (Software Center)
        url_path = '/softwarecenter/search/' ##NO_TEXT.
      WHEN 'INCIDENT' OR 'INCI' OR 'INC'.
        " Incidents
        url_path = '/incident/search/' ##NO_TEXT.
      WHEN 'INNOVATION' OR 'INNO'.
        " Innovations
        url_path = '/innovations/?searchTerm=' ##NO_TEXT.
      WHEN 'INSTALLATION' OR 'INSTALL' OR 'INST'.
        " Installations
        IF object CO '0123456789'.
          url_path = '/installation/management/' ##NO_TEXT.
        ELSE.
          url_path = '/installation/management/search/' ##NO_TEXT.
        ENDIF.
      WHEN 'LICENSE' OR 'LIC'.
        " License Keys
        url_path = '/licensekey/search/' ##NO_TEXT.
      WHEN 'PRODUCT' OR 'PROD' OR 'PRD'.
        " Products
        url_path = '/productsearch/' ##NO_TEXT.
      WHEN 'SYSTEM' OR 'SYST' OR 'SYS' OR 'SID'.
        " Systems
        IF object CO '0123456789'.
          url_path = '/systemdata/' ##NO_TEXT.
        ELSE.
          url_path = '/systemdata/search/' ##NO_TEXT.
        ENDIF.
      WHEN 'USER' OR 'USR'.
        " Users
        url_path = '/user/management/user_admin/search/' ##NO_TEXT.
      WHEN 'PAM'.
        " Product Availability Matrix
        url_domain = c_support_apps.
        url_path = '/sap/support/pam/pam.html' ##NO_TEXT.
        url_path = url_path && '?ts=1&o=most_viewed%7Cdesc&st=l&rpp=20&page=1&s=' ##NO_TEXT.
      WHEN OTHERS.
        " Default Cross-Search
        url_path = '/solutions/notesv2/?q=' ##NO_TEXT.
    ENDCASE.

    url_term = cl_http_utility=>escape_url( unescaped = object ).

    CONCATENATE url_domain '#' url_path url_term INTO url.

    /mbtools/cl_utilities=>call_browser( url ).

    r_exit = abap_true.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT command.

  ENDMETHOD.
ENDCLASS.
