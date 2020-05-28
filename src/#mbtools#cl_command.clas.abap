************************************************************************
* /MBTOOLS/CL_COMMAND_OBJECT
* MBT Command - Object
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_command DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS icon .

    CONSTANTS c_callback_prog TYPE progname VALUE '/MBTOOLS/BC_COMMAND_FIELD' ##NO_TEXT.
    CONSTANTS c_callback_alv TYPE slis_formname VALUE 'CALLBACK_ALV' ##NO_TEXT.
    CONSTANTS c_tabix TYPE char30 VALUE '/MBTOOLS/BC_CF_TABIX' ##NO_TEXT.

    METHODS select
      IMPORTING
        VALUE(i_object)   TYPE string OPTIONAL
        VALUE(i_obj_name) TYPE string .
    METHODS filter_tabl .
    METHODS text .
    METHODS pick
      EXPORTING
        !e_tadir_key TYPE /mbtools/if_definitions=>ty_tadir_key
        !e_count     TYPE i
      EXCEPTIONS
        cancelled .
    METHODS split
      IMPORTING
        !i_parameters TYPE string
      EXPORTING
        !e_operator   TYPE csequence
        !e_operand    TYPE csequence .
    METHODS split_message
      IMPORTING
        !i_message TYPE csequence
      EXPORTING
        !e_msgid   TYPE sy-msgid
        !e_msgno   TYPE sy-msgno .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS c_object_with_icon_text TYPE tabname VALUE '/MBTOOLS/OBJECT_WITH_ICON_TEXT' ##NO_TEXT.
    CONSTANTS c_badi_class TYPE seoclsname VALUE '/MBTOOLS/BC_CTS_REQ_DISPLAY' ##NO_TEXT.
    CONSTANTS c_badi_method TYPE seocmpname VALUE 'GET_OBJECT_DESCRIPTIONS' ##NO_TEXT.
    CONSTANTS c_badi_type TYPE tabname VALUE '/MBTOOLS/TRWBO_S_E071_TXT' ##NO_TEXT.
    CONSTANTS c_max_hits TYPE i VALUE 1000 ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_split,
        operator TYPE c VALUE ':',
        values   TYPE c VALUE ',',
        low_high TYPE c LENGTH 2 VALUE '..',
      END OF c_split .
    CLASS-DATA object_list TYPE /mbtools/if_definitions=>ty_objects_ext .
    CLASS-DATA tadir_list TYPE /mbtools/if_definitions=>ty_tadir_keys .

    METHODS name_split
      IMPORTING
        !i_obj_name    TYPE string
      RETURNING
        VALUE(r_range) TYPE /mbtools/if_definitions=>ty_name_range .
    METHODS object_split
      IMPORTING
        !i_object      TYPE string
      RETURNING
        VALUE(r_range) TYPE /mbtools/if_definitions=>ty_object_range .
    METHODS range_derive
      IMPORTING
        !i_input      TYPE csequence
        !i_upper_case TYPE abap_bool DEFAULT abap_true
      EXPORTING
        !e_sign       TYPE clike
        !e_option     TYPE clike
        !e_low        TYPE csequence
        !e_high       TYPE csequence .
    METHODS select_check
      IMPORTING
        !i_object       TYPE /mbtools/if_definitions=>ty_object
        !i_sel_objects  TYPE /mbtools/if_definitions=>ty_object_range
        !i_sel_names    TYPE /mbtools/if_definitions=>ty_name_range
        !i_if_requested TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_result) TYPE abap_bool .
    METHODS select_object
      IMPORTING
        !i_object       TYPE /mbtools/if_definitions=>ty_object
        !i_sel_objects  TYPE /mbtools/if_definitions=>ty_object_range
      RETURNING
        VALUE(r_result) TYPE abap_bool .
    METHODS select_add
      IMPORTING
        !i_pgmid     TYPE /mbtools/if_definitions=>ty_pgmid
        !i_object    TYPE /mbtools/if_definitions=>ty_object
        !i_sel_name  TYPE /mbtools/if_definitions=>ty_name OPTIONAL
        !i_sel_names TYPE /mbtools/if_definitions=>ty_names OPTIONAL .
    METHODS select_bw
      IMPORTING
        !i_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !i_sel_names   TYPE /mbtools/if_definitions=>ty_name_range
        !i_obj_name    TYPE string .
    METHODS select_func
      IMPORTING
        !i_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !i_sel_names   TYPE /mbtools/if_definitions=>ty_name_range .
    METHODS select_mess
      IMPORTING
        !i_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !i_sel_names   TYPE /mbtools/if_definitions=>ty_name_range .
    METHODS select_meth
      IMPORTING
        !i_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !i_sel_names   TYPE /mbtools/if_definitions=>ty_name_range .
    METHODS select_reps
      IMPORTING
        !i_sel_objects TYPE /mbtools/if_definitions=>ty_object_range
        !i_sel_names   TYPE /mbtools/if_definitions=>ty_name_range .
ENDCLASS.



CLASS /MBTOOLS/CL_COMMAND IMPLEMENTATION.


  METHOD filter_tabl.

    DATA:
      sel_tabname TYPE RANGE OF tabname,
      table_names TYPE TABLE OF tabname.

    FIELD-SYMBOLS:
      <tabname_range> LIKE LINE OF sel_tabname,
      <tadir_key>     TYPE adir_key.

    " Get all tabl objects
    LOOP AT tadir_list ASSIGNING <tadir_key>
      WHERE object = /mbtools/if_command_field=>c_objects_db-tabl.

      APPEND INITIAL LINE TO sel_tabname ASSIGNING <tabname_range>.
      <tabname_range>-sign   = 'I'.
      <tabname_range>-option = 'EQ'.
      <tabname_range>-low    = <tadir_key>-obj_name.
    ENDLOOP.

    " Only objects that work with SE16
    SELECT DISTINCT tabname FROM dd02l INTO TABLE table_names
      WHERE tabname IN sel_tabname AND
        (  tabclass = /mbtools/if_command_field=>c_table_class-transp
        OR tabclass = /mbtools/if_command_field=>c_table_class-cluster
        OR tabclass = /mbtools/if_command_field=>c_table_class-pool
        OR tabclass = /mbtools/if_command_field=>c_table_class-view )
      ORDER BY tabname.

    " Reduce object list
    LOOP AT tadir_list ASSIGNING <tadir_key>
      WHERE object = /mbtools/if_command_field=>c_objects_db-tabl.

      READ TABLE table_names TRANSPORTING NO FIELDS
        WITH TABLE KEY table_line = <tadir_key>-obj_name.
      IF sy-subrc <> 0.
        DELETE tadir_list.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD name_split.

    DATA:
      obj_name  TYPE tadir-obj_name,
      obj_names TYPE TABLE OF tadir-obj_name.

    FIELD-SYMBOLS:
      <range> LIKE LINE OF r_range.

    CHECK NOT i_obj_name IS INITIAL.

    IF i_obj_name CS c_split-values.
      SPLIT i_obj_name AT c_split-values INTO TABLE obj_names.
      LOOP AT obj_names INTO obj_name.
        APPEND INITIAL LINE TO r_range ASSIGNING <range>.

        range_derive(
          EXPORTING
            i_input  = obj_name
          IMPORTING
            e_sign   = <range>-sign
            e_option = <range>-option
            e_low    = <range>-low
            e_high   = <range>-high ).

        <range>-low  = /mbtools/cl_sap=>object_name_check( <range>-low ).
        <range>-high = /mbtools/cl_sap=>object_name_check( <range>-high ).
      ENDLOOP.
    ELSE.
      APPEND INITIAL LINE TO r_range ASSIGNING <range>.

      range_derive(
        EXPORTING
          i_input  = i_obj_name
        IMPORTING
          e_sign   = <range>-sign
          e_option = <range>-option
          e_low    = <range>-low
          e_high   = <range>-high ).

      <range>-low  = /mbtools/cl_sap=>object_name_check( <range>-low ).
      <range>-high = /mbtools/cl_sap=>object_name_check( <range>-high ).
    ENDIF.

    SORT r_range.
    DELETE ADJACENT DUPLICATES FROM r_range.

  ENDMETHOD.


  METHOD object_split.

    DATA:
      object  TYPE tadir-object,
      objects TYPE TABLE OF tadir-object.

    FIELD-SYMBOLS:
      <range> LIKE LINE OF r_range.

    CHECK NOT i_object IS INITIAL.

    IF i_object CS c_split-values.
      SPLIT i_object AT c_split-values INTO TABLE objects.
      LOOP AT objects INTO object.
        APPEND INITIAL LINE TO r_range ASSIGNING <range>.

        range_derive(
          EXPORTING
            i_input  = object
          IMPORTING
            e_sign   = <range>-sign
            e_option = <range>-option
            e_low    = <range>-low
            e_high   = <range>-high ).
      ENDLOOP.
    ELSE.
      APPEND INITIAL LINE TO r_range ASSIGNING <range>.

      range_derive(
        EXPORTING
          i_input  = i_object
        IMPORTING
          e_sign   = <range>-sign
          e_option = <range>-option
          e_low    = <range>-low
          e_high   = <range>-high ).
    ENDIF.

    SORT r_range.
    DELETE ADJACENT DUPLICATES FROM r_range.

  ENDMETHOD.


  METHOD pick.

    DATA:
      tabindex  TYPE i,
      exit_flag TYPE abap_bool,
      selfield  TYPE slis_selfield,
      fieldcat  TYPE slis_t_fieldcat_alv,
      object    TYPE /mbtools/if_definitions=>ty_object_ext.

    FIELD-SYMBOLS:
      <fieldcat> TYPE slis_fieldcat_alv.

    e_count = lines( object_list ).

    IF e_count = 0.
      " Nothing...
    ELSEIF e_count = 1.
      " Exactly one object...
      tabindex = 1.
    ELSE.
      " Multiple objects...
      CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = c_object_with_icon_text
        CHANGING
          ct_fieldcat            = fieldcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      ASSERT sy-subrc = 0.

      READ TABLE object_list INTO object INDEX 1.
      IF sy-subrc = 0 AND object-icon IS INITIAL.
        " Hide icon and text columns if not filled
        LOOP AT fieldcat ASSIGNING <fieldcat> WHERE fieldname = 'ICON' OR fieldname = 'TEXT'.
          <fieldcat>-no_out = abap_true.
        ENDLOOP.
      ENDIF.

      " Display object list
      CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
        EXPORTING
          i_callback_program      = c_callback_prog
          i_callback_user_command = c_callback_alv
          i_structure_name        = c_object_with_icon_text
          it_fieldcat             = fieldcat
        IMPORTING
          e_exit_caused_by_caller = exit_flag
        TABLES
          t_outtab                = object_list
        EXCEPTIONS
          program_error           = 1
          OTHERS                  = 2.
      IF sy-subrc = 0 AND exit_flag = abap_true.
        " Get index of seleced row which was exported in callback routine
        IMPORT tabindex TO selfield-tabindex FROM MEMORY ID c_tabix.
        IF sy-subrc = 0.
          tabindex = selfield-tabindex.
          FREE MEMORY ID c_tabix.
        ENDIF.
      ENDIF.

    ENDIF.

    IF tabindex BETWEEN 1 AND e_count.
      READ TABLE object_list INTO object INDEX tabindex.
      ASSERT sy-subrc = 0.

      e_tadir_key-pgmid    = object-pgmid.
      e_tadir_key-object   = object-object.
      e_tadir_key-obj_name = object-obj_name.
    ELSE.
      RAISE cancelled.
    ENDIF.

  ENDMETHOD.


  METHOD range_derive.

    CHECK NOT i_input IS INITIAL.

    e_sign = 'I'.
    IF i_input CA '?*'.
      IF i_input(1) = '!'.
        e_option = 'NP'.
        e_low    = i_input+1.
      ELSE.
        e_option = 'CP'.
        e_low    = i_input.
      ENDIF.
    ELSEIF i_input CS c_split-low_high.
      IF i_input(1) = '!'.
        e_option = 'NB'.
        SPLIT i_input+1 AT c_split-low_high INTO e_low e_high.
      ELSE.
        e_option = 'BT'.
        SPLIT i_input AT c_split-low_high INTO e_low e_high.
      ENDIF.
    ELSE.
      IF i_input(1) = '!'.
        e_option = 'NE'.
        e_low    = i_input+1.
      ELSE.
        e_option = 'EQ'.
        e_low    = i_input.
      ENDIF.
    ENDIF.

    SHIFT e_low  LEFT DELETING LEADING space.
    SHIFT e_high LEFT DELETING LEADING space.

    IF i_upper_case = abap_true.
      TRANSLATE e_low  TO UPPER CASE.
      TRANSLATE e_high TO UPPER CASE.
    ENDIF.

  ENDMETHOD.


  METHOD select.

    DATA:
      sel_objects TYPE /mbtools/if_definitions=>ty_object_range,
      sel_names   TYPE /mbtools/if_definitions=>ty_name_range,
      names       TYPE /mbtools/if_definitions=>ty_names.

    " Object type selection
    sel_objects = object_split( i_object ).

    " Object name selection
    sel_names = name_split( i_obj_name ).

    IF sel_objects IS INITIAL AND sel_names IS INITIAL.
      RETURN.
    ENDIF.

    " Select objects in directory
    SELECT pgmid object obj_name FROM tadir INTO TABLE tadir_list
      UP TO c_max_hits ROWS
      WHERE pgmid    = /mbtools/if_command_field=>c_pgmid-r3tr
        AND object   IN sel_objects
        AND obj_name IN sel_names
        AND delflag  = abap_false.

    " Select reports (includes)
    IF lines( tadir_list ) < c_max_hits.
      select_reps(
        i_sel_objects = sel_objects
        i_sel_names   = sel_names ).
    ENDIF.

    " Select function modules
    IF lines( tadir_list ) < c_max_hits.
      select_func(
        i_sel_objects = sel_objects
        i_sel_names   = sel_names ).
    ENDIF.

    " Select class/interface methods (if requested)
    IF lines( tadir_list ) < c_max_hits.
      select_meth(
        i_sel_objects = sel_objects
        i_sel_names   = sel_names ).
    ENDIF.

    " Select messages (if requested)
    IF lines( tadir_list ) < c_max_hits.
      select_mess(
        i_sel_objects = sel_objects
        i_sel_names   = sel_names ).
    ENDIF.

    " Select BW objects using search (if requested)
    IF lines( tadir_list ) < c_max_hits.
      select_bw(
        i_sel_objects = sel_objects
        i_sel_names   = sel_names
        i_obj_name    = i_obj_name ).
    ENDIF.

    " Deduplicate
    SORT tadir_list.
    DELETE ADJACENT DUPLICATES FROM tadir_list.

    " Too many hits
    IF lines( tadir_list ) > c_max_hits.
      DELETE tadir_list FROM c_max_hits.
      MESSAGE |'Selection limited to { c_max_hits } objects'| TYPE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD select_add.

    DATA: sel_names TYPE /mbtools/if_definitions=>ty_names.

    FIELD-SYMBOLS:
      <tadir_key> TYPE /mbtools/if_definitions=>ty_tadir_key,
      <sel_name>  TYPE /mbtools/if_definitions=>ty_name.

    sel_names = i_sel_names.

    IF NOT i_sel_name IS INITIAL.
      APPEND i_sel_name TO sel_names.
    ENDIF.

    LOOP AT sel_names ASSIGNING <sel_name>.
      APPEND INITIAL LINE TO tadir_list ASSIGNING <tadir_key>.
      <tadir_key>-pgmid    = i_pgmid.
      <tadir_key>-object   = i_object.
      <tadir_key>-obj_name = <sel_name>.
    ENDLOOP.

  ENDMETHOD.


  METHOD select_bw.

    " Instead of using "BW Search" we will eventually replace it
    " with a simple select on /MBTOOLS/BWDIR

    DATA:
      search         TYPE REF TO cl_rsawbn_ser_search_general,
      search_objs    TYPE cl_rsawbn_ser_search_obj=>ty_tr_search_obj,
      search_param   TYPE cl_rsawbn_ser_search_general=>ty_s_search_param,
      search_results TYPE rsawbn_t_awbobj,
      awbobj         TYPE rstlogo.

    FIELD-SYMBOLS:
      <search_obj>    TYPE cl_rsawbn_ser_search_obj=>ty_sr_search_obj,
      <search_result> TYPE rsawbn_s_awbobj.

    CHECK select_check( i_object       = /mbtools/if_command_field=>c_objects_bw-all
                        i_sel_objects  = i_sel_objects
                        i_sel_names    = i_sel_names
                        i_if_requested = abap_true ).

    CREATE OBJECT search.

    " Set selected BW objects
    search_objs = search->get_tr_search_obj( ).

    LOOP AT search_objs ASSIGNING <search_obj>
      WHERE r_search_obj->is_folder = abap_false.

      awbobj = <search_obj>-r_search_obj->get_awbobj( ).

      IF awbobj IN i_sel_objects OR /mbtools/if_command_field=>c_objects_bw-all IN i_sel_objects.
        <search_obj>-r_search_obj->set_selected( abap_true ).
      ELSE.
        <search_obj>-r_search_obj->set_selected( abap_false ).
      ENDIF.

    ENDLOOP.

    " Search technical names only (use TREX/HANA if available)
    search_param-search_term        = i_obj_name.
    search_param-technm             = abap_true.
    search_param-descr              = abap_false.
    search_param-exact_search       = abap_true.
    search_param-trex_search        = cl_rsos_meta_index=>get_trex_engine_stat( ).
    search_param-trex_detail_search = abap_false.
    search_param-search_behaviaour  = cl_rsawbn_ser_search=>c_exact_search.

    search->set_o_s_search_param( search_param ).

    " Run search and get results
    search->execute_search( ).

    search_results = search->get_result( ).

    LOOP AT search_results ASSIGNING <search_result>.
      select_add(
        i_pgmid    = /mbtools/if_command_field=>c_pgmid-r3tr
        i_object   = <search_result>-r_awbobj->get_awbobj( )
        i_sel_name = <search_result>-r_awbobj->get_objnm( ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD select_check.

    " Check object selection
    IF i_sel_objects IS INITIAL.
      IF i_if_requested = abap_true.
        r_result = abap_false.
      ELSE.
        r_result = abap_true.
      ENDIF.
    ELSEIF select_object( i_object = i_object i_sel_objects = i_sel_objects ).
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.

    CHECK r_result = abap_true.

    " Check object name selection
    IF i_sel_names IS INITIAL OR '*' IN i_sel_names.
      IF i_if_requested = abap_true.
        r_result = abap_false.
      ELSE.
        r_result = abap_true.
      ENDIF.
    ELSE.
      r_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD select_func.

    DATA: sel_names TYPE /mbtools/if_definitions=>ty_names.

    CHECK select_check( i_object      = /mbtools/if_command_field=>c_objects_abap-func
                        i_sel_objects = i_sel_objects
                        i_sel_names   = i_sel_names ).

    SELECT funcname FROM tfdir INTO TABLE sel_names
      UP TO c_max_hits ROWS
      WHERE funcname IN i_sel_names.

    IF sy-subrc = 0.
      select_add(
        i_pgmid     = /mbtools/if_command_field=>c_pgmid-limu
        i_object    = /mbtools/if_command_field=>c_objects_abap-func
        i_sel_names = sel_names ).
    ENDIF.

  ENDMETHOD.


  METHOD select_mess.

    TYPES:
      BEGIN OF ty_mess,
        arbgb TYPE t100-arbgb,
        msgnr TYPE t100-msgnr,
      END OF ty_mess.

    DATA:
      len       TYPE i,
      sel_name  TYPE /mbtools/if_definitions=>ty_name,
      sel_msgid TYPE RANGE OF sy-msgid,
      sel_msgno TYPE RANGE OF sy-msgno,
      sel_mess  TYPE ty_mess,
      sel_messs TYPE TABLE OF ty_mess.

    FIELD-SYMBOLS:
      <sel_msgid> LIKE LINE OF sel_msgid,
      <sel_msgno> LIKE LINE OF sel_msgno,
      <sel_names> TYPE LINE OF /mbtools/if_definitions=>ty_name_range.

    CHECK select_check( i_object      = /mbtools/if_command_field=>c_objects_limu-mess
                        i_sel_objects = i_sel_objects
                        i_sel_names   = i_sel_names ).

    " Convert <message_area><message_number> into separate selections
    LOOP AT i_sel_names ASSIGNING <sel_names>.
      CASE <sel_names>-option.
        WHEN 'EQ'.
          APPEND INITIAL LINE TO sel_msgid ASSIGNING <sel_msgid>.
          <sel_msgid>-sign   = 'I'.
          <sel_msgid>-option = 'EQ'.
          APPEND INITIAL LINE TO sel_msgno ASSIGNING <sel_msgno>.
          <sel_msgno>-sign   = 'I'.
          <sel_msgno>-option = 'EQ'.

          split_message(
            EXPORTING
              i_message = <sel_names>-low
            IMPORTING
              e_msgid   = <sel_msgid>-low
              e_msgno   = <sel_msgno>-low ).
        WHEN 'BT'.
          APPEND INITIAL LINE TO sel_msgid ASSIGNING <sel_msgid>.
          <sel_msgid>-sign   = 'I'.
          <sel_msgid>-option = 'BT'.
          APPEND INITIAL LINE TO sel_msgno ASSIGNING <sel_msgno>.
          <sel_msgno>-sign   = 'I'.
          <sel_msgno>-option = 'BT'.

          split_message(
            EXPORTING
              i_message = <sel_names>-low
            IMPORTING
              e_msgid   = <sel_msgid>-low
              e_msgno   = <sel_msgno>-low ).

          split_message(
            EXPORTING
              i_message = <sel_names>-high
            IMPORTING
              e_msgid   = <sel_msgid>-high
              e_msgno   = <sel_msgno>-high ).
        WHEN 'CP'.
          APPEND INITIAL LINE TO sel_msgid ASSIGNING <sel_msgid>.
          <sel_msgid>-sign   = 'I'.
          <sel_msgid>-option = 'CP'.
          <sel_msgid>-low    = <sel_names>-low.
      ENDCASE.
    ENDLOOP.

    SELECT arbgb msgnr FROM t100 INTO TABLE sel_messs
      UP TO c_max_hits ROWS
      WHERE arbgb IN sel_msgid AND msgnr IN sel_msgno AND sprsl = sy-langu.

    IF sy-subrc = 0.
      LOOP AT sel_messs INTO sel_mess.
        sel_name = sel_mess-arbgb && sel_mess-msgnr.
        CHECK sel_name IN i_sel_names.

        select_add(
          i_pgmid    = /mbtools/if_command_field=>c_pgmid-limu
          i_object   = /mbtools/if_command_field=>c_objects_limu-mess
          i_sel_name = sel_name ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD select_meth.

    DATA: sel_names TYPE /mbtools/if_definitions=>ty_names.

    CHECK select_check( i_object       = /mbtools/if_command_field=>c_objects_abap-meth
                        i_sel_objects  = i_sel_objects
                        i_sel_names    = i_sel_names
                        i_if_requested = abap_true ).

    SELECT cmpname FROM seocompodf INTO TABLE sel_names
      UP TO c_max_hits ROWS
      WHERE cmpname IN i_sel_names.

    IF sy-subrc = 0.
      select_add(
        i_pgmid     = /mbtools/if_command_field=>c_pgmid-limu
        i_object    = /mbtools/if_command_field=>c_objects_abap-meth
        i_sel_names = sel_names ).
    ENDIF.

  ENDMETHOD.


  METHOD select_object.

    DATA: objects TYPE /mbtools/if_definitions=>ty_objects.

    FIELD-SYMBOLS: <object> TYPE /mbtools/if_definitions=>ty_object.

    IF i_object IN i_sel_objects.
      r_result = abap_true.
    ELSEIF i_object = /mbtools/if_command_field=>c_objects_bw-all.
      SPLIT /mbtools/if_command_field=>c_objects_bw AT ',' INTO TABLE objects.

      LOOP AT objects ASSIGNING <object>.
        IF <object> IN i_sel_objects.
          r_result = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD select_reps.

    DATA: sel_names TYPE /mbtools/if_definitions=>ty_names.

    CHECK select_check( i_object      = /mbtools/if_command_field=>c_objects_abap-reps
                        i_sel_objects = i_sel_objects
                        i_sel_names   = i_sel_names ).

    SELECT name FROM trdir INTO TABLE sel_names
      UP TO c_max_hits ROWS
      WHERE name IN i_sel_names AND ( subc = 'I' OR subc = 'M' OR subc = 'S' ).

    IF sy-subrc = 0.
      select_add(
        i_pgmid     = /mbtools/if_command_field=>c_pgmid-limu
        i_object    = /mbtools/if_command_field=>c_objects_abap-reps
        i_sel_names = sel_names ).
    ENDIF.

  ENDMETHOD.


  METHOD split.

    " Get command operator and operand (similar to Google search operators)
    IF i_parameters CS c_split-operator.
      SPLIT i_parameters AT c_split-operator INTO e_operator e_operand.
      TRANSLATE e_operator TO UPPER CASE.
    ELSE.
      e_operand = i_parameters.
    ENDIF.

    SHIFT e_operand LEFT DELETING LEADING space.

  ENDMETHOD.


  METHOD split_message.

    DATA len TYPE i.

    len = strlen( i_message ) - 3.
    IF len > 0.
      e_msgid = i_message(len).
      e_msgno = i_message+len(*).
    ENDIF.

  ENDMETHOD.


  METHOD text.

    DATA:
      mbt_badi TYPE REF TO cl_badi_base, "/mbtools/bc_cts_req_display,
      mbt_data TYPE REF TO data,
      object   TYPE /mbtools/if_definitions=>ty_object_ext,
      e071     TYPE trwbo_s_e071,
      e071_tab TYPE trwbo_t_e071.

    FIELD-SYMBOLS:
      <tadir_key>   TYPE /mbtools/if_definitions=>ty_tadir_key,
      <mbt_txt>     TYPE any,
      <mbt_txt_tab> TYPE STANDARD TABLE.

    CLEAR object_list.

    TRY.
        " Fill icon and description via MBT Transport Request Enhancement (if installed)
        GET BADI mbt_badi TYPE (c_badi_class).

        IF mbt_badi IS BOUND.
          LOOP AT tadir_list ASSIGNING <tadir_key>.
            MOVE-CORRESPONDING <tadir_key> TO e071.
            INSERT e071 INTO TABLE e071_tab.
          ENDLOOP.

          CREATE DATA mbt_data TYPE STANDARD TABLE OF (c_badi_type).
          ASSIGN mbt_data->* TO <mbt_txt_tab>.

          CALL BADI mbt_badi->(c_badi_method)
            EXPORTING
              it_e071     = e071_tab
            CHANGING
              ct_e071_txt = <mbt_txt_tab>.

          LOOP AT <mbt_txt_tab> ASSIGNING <mbt_txt>.
            MOVE-CORRESPONDING <mbt_txt> TO object.
            INSERT object INTO TABLE object_list.
          ENDLOOP.
        ENDIF.

      CATCH cx_root.
        " Fallback to program id, object and object name only
        LOOP AT tadir_list ASSIGNING <tadir_key>.
          MOVE-CORRESPONDING <tadir_key> TO object.
          INSERT object INTO TABLE object_list.
        ENDLOOP.
    ENDTRY.

    SORT object_list BY object obj_name.

  ENDMETHOD.
ENDCLASS.
