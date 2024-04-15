CLASS /mbtools/cl_command DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.


************************************************************************
* MBT Command - Object
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      c_callback_prog TYPE progname VALUE '/MBTOOLS/COMMAND_FIELD',
      c_callback_alv  TYPE slis_formname VALUE 'CALLBACK_ALV',
      c_tabix         TYPE fieldname VALUE '/MBTOOLS/BC_CF_TABIX'.

    METHODS select
      IMPORTING
        !iv_object   TYPE string OPTIONAL
        !iv_obj_name TYPE string.

    METHODS filter_tabl.

    METHODS text.

    METHODS text_fallback.

    METHODS pick
      EXPORTING
        !es_tadir_key TYPE /mbtools/if_definitions=>ty_tadir_key
        !ev_count     TYPE i
      RAISING
        /mbtools/cx_exception.

    METHODS split
      IMPORTING
        !iv_parameters TYPE string
      EXPORTING
        !ev_operator   TYPE csequence
        !ev_operand    TYPE csequence.

    METHODS split_message
      IMPORTING
        !iv_message TYPE csequence
      EXPORTING
        !ev_msgid   TYPE sy-msgid
        !ev_msgno   TYPE sy-msgno.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_object_with_icon_text TYPE tabname VALUE '/MBTOOLS/OBJECT_WITH_ICON_TEXT',
      c_badi_class            TYPE seoclsname VALUE '/MBTOOLS/BC_CTS_REQ_DISPLAY',
      c_badi_method           TYPE seocmpname VALUE 'GET_OBJECT_DESCRIPTIONS',
      c_badi_type             TYPE tabname VALUE '/MBTOOLS/TRWBO_S_E071_TXT',
      c_max_hits              TYPE i VALUE 1000,
      BEGIN OF c_split,
        operator TYPE c LENGTH 1 VALUE ':',
        values   TYPE c LENGTH 1 VALUE ',',
        low_high TYPE c LENGTH 2 VALUE '..',
      END OF c_split.

    CLASS-DATA:
      gt_object_list TYPE /mbtools/if_definitions=>ty_objects_ext,
      gt_tadir_list  TYPE /mbtools/if_definitions=>ty_tadir_keys.

    METHODS name_split
      IMPORTING
        !iv_obj_name    TYPE string
      RETURNING
        VALUE(rr_range) TYPE /mbtools/if_definitions=>ty_name_range.

    METHODS name_adjust
      IMPORTING
        !iv_obj_name       TYPE csequence
      RETURNING
        VALUE(rv_obj_name) TYPE string.

    METHODS object_split
      IMPORTING
        !iv_object      TYPE string
      RETURNING
        VALUE(rr_range) TYPE /mbtools/if_definitions=>ty_object_range.

    METHODS range_derive
      IMPORTING
        !iv_input      TYPE csequence
        !iv_upper_case TYPE abap_bool DEFAULT abap_true
      EXPORTING
        !ev_sign       TYPE clike
        !ev_option     TYPE clike
        !ev_low        TYPE csequence
        !ev_high       TYPE csequence.

    METHODS select_check
      IMPORTING
        !iv_object       TYPE /mbtools/if_definitions=>ty_object
        !iv_sel_objects  TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names    TYPE /mbtools/if_definitions=>ty_name_range
        !iv_if_requested TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS select_object
      IMPORTING
        !iv_object       TYPE /mbtools/if_definitions=>ty_object
        !iv_sel_objects  TYPE /mbtools/if_definitions=>ty_object_range
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS select_add
      IMPORTING
        !iv_pgmid     TYPE /mbtools/if_definitions=>ty_pgmid
        !iv_object    TYPE /mbtools/if_definitions=>ty_object
        !iv_sel_name  TYPE /mbtools/if_definitions=>ty_name OPTIONAL
        !iv_sel_names TYPE /mbtools/if_definitions=>ty_names OPTIONAL.

    METHODS select_bw
      IMPORTING
        !iv_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names   TYPE /mbtools/if_definitions=>ty_name_range
        !iv_obj_name    TYPE string.

    METHODS select_func
      IMPORTING
        !iv_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names   TYPE /mbtools/if_definitions=>ty_name_range.

    METHODS select_mess
      IMPORTING
        !iv_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names   TYPE /mbtools/if_definitions=>ty_name_range.

    METHODS select_meth
      IMPORTING
        !iv_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names   TYPE /mbtools/if_definitions=>ty_name_range.

    METHODS select_reps
      IMPORTING
        !iv_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names   TYPE /mbtools/if_definitions=>ty_name_range.

    METHODS select_basis_users
      IMPORTING
        !iv_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names   TYPE /mbtools/if_definitions=>ty_name_range.

    METHODS select_basis_requests
      IMPORTING
        !iv_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names   TYPE /mbtools/if_definitions=>ty_name_range.

    METHODS select_bw_classic
      IMPORTING
        !iv_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names   TYPE /mbtools/if_definitions=>ty_name_range
        !iv_obj_name    TYPE string.

    METHODS select_bw_hana
      IMPORTING
        !io_search      TYPE REF TO object
        !iv_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !iv_sel_names   TYPE /mbtools/if_definitions=>ty_name_range.

ENDCLASS.



CLASS /mbtools/cl_command IMPLEMENTATION.


  METHOD filter_tabl.

    DATA:
      lr_tabname TYPE RANGE OF tabname,
      lt_names   TYPE TABLE OF tabname.

    FIELD-SYMBOLS:
      <lr_range>     LIKE LINE OF lr_tabname,
      <ls_tadir_key> TYPE adir_key.

    " Get all tabl objects
    LOOP AT gt_tadir_list ASSIGNING <ls_tadir_key>
      WHERE object = /mbtools/if_command_field=>c_objects_db-tabl.

      APPEND INITIAL LINE TO lr_tabname ASSIGNING <lr_range>.
      <lr_range>-sign   = 'I'.
      <lr_range>-option = 'EQ'.
      <lr_range>-low    = <ls_tadir_key>-obj_name.
    ENDLOOP.

    " Only objects that work with SE16
    SELECT DISTINCT tabname FROM dd02l INTO TABLE lt_names
      WHERE tabname IN lr_tabname AND
        (  tabclass = /mbtools/if_command_field=>c_table_class-transp
        OR tabclass = /mbtools/if_command_field=>c_table_class-cluster
        OR tabclass = /mbtools/if_command_field=>c_table_class-pool
        OR tabclass = /mbtools/if_command_field=>c_table_class-view )
      ORDER BY tabname.
    ASSERT sy-subrc >= 0.

    " Reduce object list
    LOOP AT gt_tadir_list ASSIGNING <ls_tadir_key>
      WHERE object = /mbtools/if_command_field=>c_objects_db-tabl.

      READ TABLE lt_names TRANSPORTING NO FIELDS
        WITH TABLE KEY table_line = <ls_tadir_key>-obj_name ##WARN_OK.
      IF sy-subrc <> 0.
        DELETE gt_tadir_list.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD name_adjust.

    DATA lv_len TYPE i.

    lv_len = strlen( iv_obj_name ) - 1.

    " Remove trailing #
    IF lv_len > 0 AND iv_obj_name+lv_len(1) = '#'.
      rv_obj_name = iv_obj_name(lv_len).
    ELSE.
      rv_obj_name = iv_obj_name.
    ENDIF.

  ENDMETHOD.


  METHOD name_split.

    DATA:
      lv_obj_name TYPE tadir-obj_name,
      lt_obj_name TYPE TABLE OF tadir-obj_name.

    FIELD-SYMBOLS <lr_range> LIKE LINE OF rr_range.

    CHECK iv_obj_name IS NOT INITIAL.

    IF iv_obj_name CS c_split-values.
      SPLIT iv_obj_name AT c_split-values INTO TABLE lt_obj_name.
      LOOP AT lt_obj_name INTO lv_obj_name.
        lv_obj_name = name_adjust( lv_obj_name ).

        APPEND INITIAL LINE TO rr_range ASSIGNING <lr_range>.

        range_derive(
          EXPORTING
            iv_input  = lv_obj_name
          IMPORTING
            ev_sign   = <lr_range>-sign
            ev_option = <lr_range>-option
            ev_low    = <lr_range>-low
            ev_high   = <lr_range>-high ).

        <lr_range>-low  = /mbtools/cl_sap=>object_name_check( <lr_range>-low ).
        <lr_range>-high = /mbtools/cl_sap=>object_name_check( <lr_range>-high ).
      ENDLOOP.
    ELSE.
      lv_obj_name = name_adjust( iv_obj_name ).

      APPEND INITIAL LINE TO rr_range ASSIGNING <lr_range>.

      range_derive(
        EXPORTING
          iv_input  = lv_obj_name
        IMPORTING
          ev_sign   = <lr_range>-sign
          ev_option = <lr_range>-option
          ev_low    = <lr_range>-low
          ev_high   = <lr_range>-high ).

      <lr_range>-low  = /mbtools/cl_sap=>object_name_check( <lr_range>-low ).
      <lr_range>-high = /mbtools/cl_sap=>object_name_check( <lr_range>-high ).
    ENDIF.

    SORT rr_range.
    DELETE ADJACENT DUPLICATES FROM rr_range.

  ENDMETHOD.


  METHOD object_split.

    DATA:
      lv_object TYPE tadir-object,
      lt_object TYPE TABLE OF tadir-object.

    FIELD-SYMBOLS <lr_range> LIKE LINE OF rr_range.

    CHECK iv_object IS NOT INITIAL.

    IF iv_object CS c_split-values.
      SPLIT iv_object AT c_split-values INTO TABLE lt_object.
      LOOP AT lt_object INTO lv_object.
        APPEND INITIAL LINE TO rr_range ASSIGNING <lr_range>.

        range_derive(
          EXPORTING
            iv_input  = lv_object
          IMPORTING
            ev_sign   = <lr_range>-sign
            ev_option = <lr_range>-option
            ev_low    = <lr_range>-low
            ev_high   = <lr_range>-high ).
      ENDLOOP.
    ELSE.
      APPEND INITIAL LINE TO rr_range ASSIGNING <lr_range>.

      range_derive(
        EXPORTING
          iv_input  = iv_object
        IMPORTING
          ev_sign   = <lr_range>-sign
          ev_option = <lr_range>-option
          ev_low    = <lr_range>-low
          ev_high   = <lr_range>-high ).
    ENDIF.

    SORT rr_range.
    DELETE ADJACENT DUPLICATES FROM rr_range.

  ENDMETHOD.


  METHOD pick.

    DATA:
      lv_tabindex  TYPE i,
      lv_exit_flag TYPE abap_bool,
      ls_selfield  TYPE slis_selfield,
      lt_fieldcat  TYPE slis_t_fieldcat_alv,
      ls_object    TYPE /mbtools/if_definitions=>ty_object_ext.

    FIELD-SYMBOLS <ls_fieldcat> TYPE slis_fieldcat_alv.

    CLEAR: es_tadir_key, ev_count.

    ev_count = lines( gt_object_list ).

    CASE ev_count.
      WHEN 0.
        MESSAGE 'No object found'(001) TYPE 'S'.
        " Nothing...
      WHEN 1.
        " Exactly one object...
        lv_tabindex = 1.
      WHEN OTHERS.
        " Multiple objects...
        CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
          EXPORTING
            i_structure_name       = c_object_with_icon_text
          CHANGING
            ct_fieldcat            = lt_fieldcat
          EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
        ASSERT sy-subrc = 0.

        READ TABLE gt_object_list INTO ls_object INDEX 1.
        IF sy-subrc = 0 AND ls_object-icon IS INITIAL.
          " Hide icon and text columns if not filled
          LOOP AT lt_fieldcat ASSIGNING <ls_fieldcat>
            WHERE fieldname = 'ICON' OR fieldname = 'TEXT'.
            <ls_fieldcat>-no_out = abap_true.
          ENDLOOP.
        ENDIF.

        " Display object list
        CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
          EXPORTING
            i_callback_program      = c_callback_prog
            i_callback_user_command = c_callback_alv
            i_structure_name        = c_object_with_icon_text
            it_fieldcat             = lt_fieldcat
          IMPORTING
            e_exit_caused_by_caller = lv_exit_flag
          TABLES
            t_outtab                = gt_object_list
          EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
        IF sy-subrc = 0 AND lv_exit_flag = abap_true.
          " Get index of seleced row which was exported in callback routine
          IMPORT tabindex TO ls_selfield-tabindex FROM MEMORY ID c_tabix.
          IF sy-subrc = 0.
            lv_tabindex = ls_selfield-tabindex.
            FREE MEMORY ID c_tabix.
          ENDIF.
        ENDIF.

    ENDCASE.

    IF lv_tabindex BETWEEN 1 AND ev_count.
      READ TABLE gt_object_list INTO ls_object INDEX lv_tabindex.
      ASSERT sy-subrc = 0.

      es_tadir_key-pgmid    = ls_object-pgmid.
      es_tadir_key-object   = ls_object-object.
      es_tadir_key-obj_name = ls_object-obj_name.
    ELSE.
      /mbtools/cx_exception=>raise( 'Cancelled' ).
    ENDIF.

  ENDMETHOD.


  METHOD range_derive.

    CLEAR: ev_sign, ev_option, ev_low, ev_high.

    CHECK iv_input IS NOT INITIAL.

    ev_sign = 'I'.
    IF iv_input CA '?*'.
      IF iv_input(1) = '!'.
        ev_option = 'NP'.
        ev_low    = iv_input+1.
      ELSE.
        ev_option = 'CP'.
        ev_low    = iv_input.
      ENDIF.
    ELSEIF iv_input CS c_split-low_high.
      IF iv_input(1) = '!'.
        ev_option = 'NB'.
        SPLIT iv_input+1 AT c_split-low_high INTO ev_low ev_high.
      ELSE.
        ev_option = 'BT'.
        SPLIT iv_input AT c_split-low_high INTO ev_low ev_high.
      ENDIF.
    ELSEIF iv_input(1) = '!'.
      ev_option = 'NE'.
      ev_low    = iv_input+1.
    ELSE.
      ev_option = 'EQ'.
      ev_low    = iv_input.
    ENDIF.

    SHIFT ev_low  LEFT DELETING LEADING space.
    SHIFT ev_high LEFT DELETING LEADING space.

    IF iv_upper_case = abap_true.
      TRANSLATE ev_low  TO UPPER CASE.
      TRANSLATE ev_high TO UPPER CASE.
    ENDIF.

  ENDMETHOD.


  METHOD select.

    DATA:
      lr_objects TYPE /mbtools/if_definitions=>ty_object_range,
      lr_names   TYPE /mbtools/if_definitions=>ty_name_range,
      lv_msg     TYPE string.

    " Object type selection
    lr_objects = object_split( iv_object ).

    " Object name selection
    lr_names = name_split( iv_obj_name ).

    IF lr_objects IS INITIAL AND lr_names IS INITIAL.
      RETURN.
    ENDIF.

    " Select objects in directory
    SELECT pgmid object obj_name FROM tadir INTO TABLE gt_tadir_list
      UP TO c_max_hits ROWS
      WHERE pgmid    = /mbtools/if_command_field=>c_pgmid-r3tr
        AND object   IN lr_objects
        AND obj_name IN lr_names
        AND delflag  = abap_false ##SUBRC_OK.           "#EC CI_GENBUFF

    " Select reports (includes)
    IF lines( gt_tadir_list ) < c_max_hits.
      select_reps(
        iv_sel_objects = lr_objects
        iv_sel_names   = lr_names ).
    ENDIF.

    " Select function modules
    IF lines( gt_tadir_list ) < c_max_hits.
      select_func(
        iv_sel_objects = lr_objects
        iv_sel_names   = lr_names ).
    ENDIF.

    " Select class/interface methods (if requested)
    IF lines( gt_tadir_list ) < c_max_hits.
      select_meth(
        iv_sel_objects = lr_objects
        iv_sel_names   = lr_names ).
    ENDIF.

    " Select messages (if requested)
    IF lines( gt_tadir_list ) < c_max_hits.
      select_mess(
        iv_sel_objects = lr_objects
        iv_sel_names   = lr_names ).
    ENDIF.

    " Select BW objects using search (if requested)
    IF lines( gt_tadir_list ) < c_max_hits.
      select_bw(
        iv_sel_objects = lr_objects
        iv_sel_names   = lr_names
        iv_obj_name    = iv_obj_name ).
    ENDIF.

    " Select users (if requested)
    IF lines( gt_tadir_list ) < c_max_hits.
      select_basis_users(
        iv_sel_objects = lr_objects
        iv_sel_names   = lr_names ).
    ENDIF.

    " Select transport requests (if requested)
    IF lines( gt_tadir_list ) < c_max_hits.
      select_basis_requests(
        iv_sel_objects = lr_objects
        iv_sel_names   = lr_names ).
    ENDIF.

    " Deduplicate
    SORT gt_tadir_list.
    DELETE ADJACENT DUPLICATES FROM gt_tadir_list.

    " Too many hits
    IF lines( gt_tadir_list ) > c_max_hits.
      DELETE gt_tadir_list FROM c_max_hits.
      lv_msg = |'Selection limited to { c_max_hits } objects'|.
      MESSAGE lv_msg TYPE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD select_add.

    DATA lt_names TYPE /mbtools/if_definitions=>ty_names.

    FIELD-SYMBOLS:
      <ls_tadir_key> TYPE /mbtools/if_definitions=>ty_tadir_key,
      <lv_name>      TYPE /mbtools/if_definitions=>ty_name.

    lt_names = iv_sel_names.

    IF iv_sel_name IS NOT INITIAL.
      APPEND iv_sel_name TO lt_names.
    ENDIF.

    LOOP AT lt_names ASSIGNING <lv_name>.
      APPEND INITIAL LINE TO gt_tadir_list ASSIGNING <ls_tadir_key>.
      <ls_tadir_key>-pgmid    = iv_pgmid.
      <ls_tadir_key>-object   = iv_object.
      <ls_tadir_key>-obj_name = <lv_name>.
    ENDLOOP.

  ENDMETHOD.


  METHOD select_basis_requests.

    DATA lt_names TYPE /mbtools/if_definitions=>ty_names.

    CHECK select_check( iv_object      = /mbtools/if_command_field=>c_objects_basis-request
                        iv_sel_objects = iv_sel_objects
                        iv_sel_names   = iv_sel_names ) = abap_true.

    SELECT trkorr FROM e070 INTO TABLE lt_names WHERE trkorr IN iv_sel_names.

    IF sy-subrc = 0.
      select_add(
        iv_pgmid     = /mbtools/if_command_field=>c_pgmid-basis
        iv_object    = /mbtools/if_command_field=>c_objects_basis-request
        iv_sel_names = lt_names ).
    ENDIF.

  ENDMETHOD.


  METHOD select_basis_users.

    DATA lt_names TYPE /mbtools/if_definitions=>ty_names.

    CHECK select_check( iv_object      = /mbtools/if_command_field=>c_objects_basis-user
                        iv_sel_objects = iv_sel_objects
                        iv_sel_names   = iv_sel_names ) = abap_true.

    SELECT bname FROM usr02 INTO TABLE lt_names WHERE bname IN iv_sel_names.

    IF sy-subrc = 0.
      select_add(
        iv_pgmid     = /mbtools/if_command_field=>c_pgmid-basis
        iv_object    = /mbtools/if_command_field=>c_objects_basis-user
        iv_sel_names = lt_names ).
    ENDIF.

  ENDMETHOD.


  METHOD select_bw.

    DATA lo_search TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_RSOS_SEARCH_METADATA')=>factory
          RECEIVING
            r_r_search_metadata = lo_search.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    IF sy-dbsys = 'HDB' AND lo_search IS BOUND.
      select_bw_hana(
        io_search      = lo_search
        iv_sel_objects = iv_sel_objects
        iv_sel_names   = iv_sel_names ).
    ELSE.
      select_bw_classic(
        iv_sel_objects = iv_sel_objects
        iv_sel_names   = iv_sel_names
        iv_obj_name    = iv_obj_name ).
    ENDIF.

  ENDMETHOD.


  METHOD select_bw_classic.

    DATA:
      lv_trex_search    TYPE abap_bool,
      lo_search         TYPE REF TO cl_rsawbn_ser_search_general,
      lt_search_objs    TYPE cl_rsawbn_ser_search_obj=>ty_tr_search_obj,
      ls_search_param   TYPE cl_rsawbn_ser_search_general=>ty_s_search_param,
      lt_search_results TYPE rsawbn_t_awbobj,
      lv_awbobj         TYPE rstlogo.

    FIELD-SYMBOLS:
      <ls_search_obj>    TYPE cl_rsawbn_ser_search_obj=>ty_sr_search_obj,
      <ls_search_result> TYPE rsawbn_s_awbobj.

    CHECK select_check( iv_object       = /mbtools/if_command_field=>c_objects_bw-all
                        iv_sel_objects  = iv_sel_objects
                        iv_sel_names    = iv_sel_names
                        iv_if_requested = abap_true ) = abap_true.

    CREATE OBJECT lo_search.

    " Set selected BW objects
    lt_search_objs = lo_search->get_tr_search_obj( ).

    LOOP AT lt_search_objs ASSIGNING <ls_search_obj>
      WHERE r_search_obj->is_folder = abap_false.

      lv_awbobj = <ls_search_obj>-r_search_obj->get_awbobj( ).

      IF lv_awbobj IN iv_sel_objects OR /mbtools/if_command_field=>c_objects_bw-all IN iv_sel_objects.
        <ls_search_obj>-r_search_obj->set_selected( abap_true ).
      ELSE.
        <ls_search_obj>-r_search_obj->set_selected( abap_false ).
      ENDIF.

    ENDLOOP.

    TRY.
        CALL METHOD ('CL_RSOS_META_INDEX')=>get_trex_engine_stat
          RECEIVING
            r_trex_engine_state = lv_trex_search.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
    " Search technical names only (use TREX/HANA if available)
    ls_search_param-search_term        = iv_obj_name.
    ls_search_param-technm             = abap_true.
    ls_search_param-descr              = abap_false.
    ls_search_param-exact_search       = abap_true.
    ls_search_param-trex_search        = lv_trex_search.
    ls_search_param-trex_detail_search = abap_false.
    ls_search_param-search_behaviaour  = cl_rsawbn_ser_search=>c_exact_search.

    lo_search->set_o_s_search_param( ls_search_param ).

    " Run search and get results
    lo_search->execute_search( ).

    lt_search_results = lo_search->get_result( ).

    LOOP AT lt_search_results ASSIGNING <ls_search_result>.
      select_add(
        iv_pgmid    = /mbtools/if_command_field=>c_pgmid-r3tr
        iv_object   = <ls_search_result>-r_awbobj->get_awbobj( )
        iv_sel_name = <ls_search_result>-r_awbobj->get_objnm( ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD select_bw_hana.

    " If any of the assigns fail, something in BW changed and this method must be adjusted accordingly

    DATA:
      lr_data  TYPE REF TO data,
      lv_subrc TYPE sy-subrc.

    FIELD-SYMBOLS:
      <lt_where_attributes>  TYPE ANY TABLE,
      <lt_search_attributes> TYPE ANY TABLE,
      <ls_where_attributes>  TYPE any,
      <ls_search_attributes> TYPE any,
      <lv_field>             TYPE any,
      <ls_sel_objects>       LIKE LINE OF iv_sel_objects,
      <ls_sel_names>         LIKE LINE OF iv_sel_names,
      <lt_results>           TYPE ANY TABLE,
      <ls_results>           TYPE any,
      <lv_object>            TYPE any,
      <lv_sel_name>          TYPE any.

    CREATE DATA lr_data TYPE ('RSOS_S_SEARCHATTRIBUTE').
    ASSIGN lr_data->* TO <ls_search_attributes> ##SUBRC_OK.
    CREATE DATA lr_data TYPE ('RSOS_T_SEARCH_ATTRIBUTE').
    ASSIGN lr_data->* TO <lt_search_attributes> ##SUBRC_OK.

    " Search technical name only
    ASSIGN COMPONENT 'ATTRIBUTENM' OF STRUCTURE <ls_search_attributes> TO <lv_field> ##SUBRC_OK.
    <lv_field> = 'TECHNAME'.
    ASSIGN COMPONENT 'REQUESTED' OF STRUCTURE <ls_search_attributes> TO <lv_field> ##SUBRC_OK.
    <lv_field> = abap_true.
    INSERT <ls_search_attributes> INTO TABLE <lt_search_attributes>.

    CREATE DATA lr_data TYPE ('RSOS_S_WHEREATTRIBUTE').
    ASSIGN lr_data->* TO <ls_where_attributes> ##SUBRC_OK.
    CREATE DATA lr_data TYPE ('RSOS_T_WHEREATTRIBUTE').
    ASSIGN lr_data->* TO <lt_where_attributes> ##SUBRC_OK.

    " Limit to list of object types
    LOOP AT iv_sel_objects ASSIGNING <ls_sel_objects>.
      MOVE-CORRESPONDING <ls_sel_objects> TO <ls_where_attributes>.
      ASSIGN COMPONENT 'ATTRIBUTENM' OF STRUCTURE <ls_where_attributes> TO <lv_field> ##SUBRC_OK.
      <lv_field> = 'TLOGO'.
      INSERT <ls_where_attributes> INTO TABLE <lt_where_attributes>.
    ENDLOOP.

    " Limit to list of object names
    LOOP AT iv_sel_names ASSIGNING <ls_sel_names>.
      MOVE-CORRESPONDING <ls_sel_names> TO <ls_where_attributes>.
      ASSIGN COMPONENT 'ATTRIBUTENM' OF STRUCTURE <ls_where_attributes> TO <lv_field> ##SUBRC_OK.
      <lv_field> = 'TECHNAME'.
      INSERT <ls_where_attributes> INTO TABLE <lt_where_attributes>.
    ENDLOOP.

    " Limit to active version of objects
    ASSIGN COMPONENT 'ATTRIBUTENM' OF STRUCTURE <ls_where_attributes> TO <lv_field> ##SUBRC_OK.
    <lv_field> = 'OBJVERS'.
    ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_where_attributes> TO <lv_field> ##SUBRC_OK.
    <lv_field> = 'I'.
    ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_where_attributes> TO <lv_field> ##SUBRC_OK.
    <lv_field> = 'EQ'.
    ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_where_attributes> TO <lv_field> ##SUBRC_OK.
    <lv_field> = 'A'.
    INSERT <ls_where_attributes> INTO TABLE <lt_where_attributes>.

    CALL METHOD io_search->('SEARCH')
      EXPORTING
        i_search_term         = '*'
        i_t_where_attributes  = <lt_where_attributes>
        i_t_search_attributes = <lt_search_attributes>
        i_severity            = 'S'
        i_association_type    = '002'
        i_with_icon           = abap_false
      IMPORTING
        e_r_result            = lr_data
        e_subrc               = lv_subrc.

    CHECK lv_subrc < 8.

    ASSIGN lr_data->* TO <lt_results> ##SUBRC_OK.

    LOOP AT <lt_results> ASSIGNING <ls_results>.
      ASSIGN COMPONENT 'TLOGO' OF STRUCTURE <ls_results> TO <lv_object> ##SUBRC_OK.
      ASSIGN COMPONENT 'TECHNAME' OF STRUCTURE <ls_results> TO <lv_sel_name> ##SUBRC_OK.

      select_add(
        iv_pgmid    = /mbtools/if_command_field=>c_pgmid-r3tr
        iv_object   = |{ <lv_object> }|
        iv_sel_name = |{ <lv_sel_name> }| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD select_check.

    " Check object selection
    IF iv_sel_objects IS INITIAL.
      IF iv_if_requested = abap_true.
        rv_result = abap_false.
      ELSE.
        rv_result = abap_true.
      ENDIF.
    ELSEIF select_object( iv_object      = iv_object
                          iv_sel_objects = iv_sel_objects ) = abap_true.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

    CHECK rv_result = abap_true.

    " Check object name selection
    IF iv_sel_names IS INITIAL OR '*' IN iv_sel_names.
      IF iv_if_requested = abap_true.
        rv_result = abap_false.
      ELSE.
        rv_result = abap_true.
      ENDIF.
    ELSE.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD select_func.

    DATA lt_names TYPE /mbtools/if_definitions=>ty_names.

    CHECK select_check( iv_object      = /mbtools/if_command_field=>c_objects_abap-func
                        iv_sel_objects = iv_sel_objects
                        iv_sel_names   = iv_sel_names ) = abap_true.

    SELECT funcname FROM tfdir INTO TABLE lt_names
      UP TO c_max_hits ROWS
      WHERE funcname IN iv_sel_names.                   "#EC CI_GENBUFF

    IF sy-subrc = 0.
      select_add(
        iv_pgmid     = /mbtools/if_command_field=>c_pgmid-limu
        iv_object    = /mbtools/if_command_field=>c_objects_abap-func
        iv_sel_names = lt_names ).
    ENDIF.

  ENDMETHOD.


  METHOD select_mess.

    TYPES:
      BEGIN OF ty_mess,
        arbgb TYPE t100-arbgb,
        msgnr TYPE t100-msgnr,
      END OF ty_mess.

    DATA:
      lv_name  TYPE /mbtools/if_definitions=>ty_name,
      lr_msgid TYPE RANGE OF sy-msgid,
      lr_msgno TYPE RANGE OF sy-msgno,
      ls_mess  TYPE ty_mess,
      lt_mess  TYPE TABLE OF ty_mess.

    FIELD-SYMBOLS:
      <lr_msgid> LIKE LINE OF lr_msgid,
      <lr_msgno> LIKE LINE OF lr_msgno,
      <lr_names> TYPE LINE OF /mbtools/if_definitions=>ty_name_range.

    CHECK select_check( iv_object      = /mbtools/if_command_field=>c_objects_limu-mess
                        iv_sel_objects = iv_sel_objects
                        iv_sel_names   = iv_sel_names ) = abap_true.

    " Convert <messagev_area><messagev_number> into separate selections
    LOOP AT iv_sel_names ASSIGNING <lr_names>.
      CASE <lr_names>-option.
        WHEN 'EQ'.
          APPEND INITIAL LINE TO lr_msgid ASSIGNING <lr_msgid>.
          <lr_msgid>-sign   = 'I'.
          <lr_msgid>-option = 'EQ'.
          APPEND INITIAL LINE TO lr_msgno ASSIGNING <lr_msgno>.
          <lr_msgno>-sign   = 'I'.
          <lr_msgno>-option = 'EQ'.

          split_message(
            EXPORTING
              iv_message = <lr_names>-low
            IMPORTING
              ev_msgid   = <lr_msgid>-low
              ev_msgno   = <lr_msgno>-low ).
        WHEN 'BT'.
          APPEND INITIAL LINE TO lr_msgid ASSIGNING <lr_msgid>.
          <lr_msgid>-sign   = 'I'.
          <lr_msgid>-option = 'BT'.
          APPEND INITIAL LINE TO lr_msgno ASSIGNING <lr_msgno>.
          <lr_msgno>-sign   = 'I'.
          <lr_msgno>-option = 'BT'.

          split_message(
            EXPORTING
              iv_message = <lr_names>-low
            IMPORTING
              ev_msgid   = <lr_msgid>-low
              ev_msgno   = <lr_msgno>-low ).

          split_message(
            EXPORTING
              iv_message = <lr_names>-high
            IMPORTING
              ev_msgid   = <lr_msgid>-high
              ev_msgno   = <lr_msgno>-high ).
        WHEN 'CP'.
          APPEND INITIAL LINE TO lr_msgid ASSIGNING <lr_msgid>.
          <lr_msgid>-sign   = 'I'.
          <lr_msgid>-option = 'CP'.
          <lr_msgid>-low    = <lr_names>-low.
      ENDCASE.
    ENDLOOP.

    SELECT arbgb msgnr FROM t100 INTO TABLE lt_mess
      UP TO c_max_hits ROWS
      WHERE arbgb IN lr_msgid AND msgnr IN lr_msgno AND sprsl = sy-langu. "#EC CI_GENBUFF

    IF sy-subrc = 0.
      LOOP AT lt_mess INTO ls_mess.
        lv_name = ls_mess-arbgb && ls_mess-msgnr.
        CHECK lv_name IN iv_sel_names.

        select_add(
          iv_pgmid    = /mbtools/if_command_field=>c_pgmid-limu
          iv_object   = /mbtools/if_command_field=>c_objects_limu-mess
          iv_sel_name = lv_name ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD select_meth.

    DATA lt_names TYPE /mbtools/if_definitions=>ty_names.

    CHECK select_check( iv_object       = /mbtools/if_command_field=>c_objects_abap-meth
                        iv_sel_objects  = iv_sel_objects
                        iv_sel_names    = iv_sel_names
                        iv_if_requested = abap_true ) = abap_true.

    SELECT cmpname FROM seocompodf INTO TABLE lt_names
      UP TO c_max_hits ROWS
      WHERE cmpname IN iv_sel_names.                    "#EC CI_NOFIRST

    IF sy-subrc = 0.
      select_add(
        iv_pgmid     = /mbtools/if_command_field=>c_pgmid-limu
        iv_object    = /mbtools/if_command_field=>c_objects_abap-meth
        iv_sel_names = lt_names ).
    ENDIF.

  ENDMETHOD.


  METHOD select_object.

    DATA lt_objects TYPE /mbtools/if_definitions=>ty_objects.

    FIELD-SYMBOLS <lv_object> TYPE /mbtools/if_definitions=>ty_object.

    IF iv_object IN iv_sel_objects.
      rv_result = abap_true.
    ELSEIF iv_object = /mbtools/if_command_field=>c_objects_bw-all.
      SPLIT /mbtools/if_command_field=>c_objects_bw AT ',' INTO TABLE lt_objects.

      LOOP AT lt_objects ASSIGNING <lv_object>.
        IF <lv_object> IN iv_sel_objects.
          rv_result = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD select_reps.

    DATA lt_names TYPE /mbtools/if_definitions=>ty_names.

    CHECK select_check( iv_object      = /mbtools/if_command_field=>c_objects_abap-reps
                        iv_sel_objects = iv_sel_objects
                        iv_sel_names   = iv_sel_names ) = abap_true.

    SELECT name FROM trdir INTO TABLE lt_names
      UP TO c_max_hits ROWS
      WHERE name IN iv_sel_names AND ( subc = 'I' OR subc = 'M' OR subc = 'S' ).

    IF sy-subrc = 0.
      select_add(
        iv_pgmid     = /mbtools/if_command_field=>c_pgmid-limu
        iv_object    = /mbtools/if_command_field=>c_objects_abap-reps
        iv_sel_names = lt_names ).
    ENDIF.

  ENDMETHOD.


  METHOD split.

    CLEAR: ev_operator, ev_operand.

    " Get command operator and operand (similar to Google search operators)
    IF iv_parameters CS c_split-operator.
      SPLIT iv_parameters AT c_split-operator INTO ev_operator ev_operand.
      TRANSLATE ev_operator TO UPPER CASE.
    ELSE.
      ev_operand = iv_parameters.
    ENDIF.

    SHIFT ev_operand LEFT DELETING LEADING space.

  ENDMETHOD.


  METHOD split_message.

    DATA lv_len TYPE i.

    CLEAR: ev_msgid, ev_msgno.

    lv_len = strlen( iv_message ) - 3.
    IF lv_len > 0.
      ev_msgid = iv_message(lv_len).
      ev_msgno = iv_message+lv_len(*).
    ENDIF.

  ENDMETHOD.


  METHOD text.

    DATA:
      lo_badi   TYPE REF TO cl_badi_base, "/mbtools/bc_cts_req_display,
      lo_data   TYPE REF TO data,
      ls_object TYPE /mbtools/if_definitions=>ty_object_ext,
      ls_e071   TYPE trwbo_s_e071,
      lt_e071   TYPE trwbo_t_e071.

    FIELD-SYMBOLS:
      <ls_tadir_key> TYPE /mbtools/if_definitions=>ty_tadir_key,
      <ls_txt>       TYPE any,
      <lt_txt>       TYPE STANDARD TABLE.

    CLEAR gt_object_list.

    " Check if MBT Transport Request is installed and active
    IF /mbtools/cl_switches=>is_active( /mbtools/cl_switches=>c_tool-mbt_transport_request ) = abap_true.
      TRY.
          " Fill icon and description via MBT Transport Request Enhancement (if installed)
          GET BADI lo_badi TYPE (c_badi_class).

          IF lo_badi IS BOUND.
            LOOP AT gt_tadir_list ASSIGNING <ls_tadir_key>.
              MOVE-CORRESPONDING <ls_tadir_key> TO ls_e071.
              INSERT ls_e071 INTO TABLE lt_e071.
            ENDLOOP.

            CREATE DATA lo_data TYPE STANDARD TABLE OF (c_badi_type).
            ASSIGN lo_data->* TO <lt_txt>.
            ASSERT sy-subrc = 0.

            CALL BADI lo_badi->(c_badi_method)
              EXPORTING
                it_e071     = lt_e071
              CHANGING
                ct_e071_txt = <lt_txt>.

            LOOP AT <lt_txt> ASSIGNING <ls_txt>.
              MOVE-CORRESPONDING <ls_txt> TO ls_object.
              INSERT ls_object INTO TABLE gt_object_list.
            ENDLOOP.
          ENDIF.

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF gt_object_list IS INITIAL.
      " Fallback to program id, object and object name only
      text_fallback( ).
    ENDIF.

    SORT gt_object_list BY object obj_name.

  ENDMETHOD.


  METHOD text_fallback.

    DATA ls_object TYPE /mbtools/if_definitions=>ty_object_ext.

    FIELD-SYMBOLS <ls_tadir_key> TYPE /mbtools/if_definitions=>ty_tadir_key.

    LOOP AT gt_tadir_list ASSIGNING <ls_tadir_key>.
      MOVE-CORRESPONDING <ls_tadir_key> TO ls_object.
      INSERT ls_object INTO TABLE gt_object_list.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
