************************************************************************
* /MBTOOLS/IF_COMMAND_OBJECT
* MBT Command Field
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
INTERFACE /mbtools/if_command_field
  PUBLIC .

  TYPES ty_tadir_key TYPE adir_key .
  TYPES:
    ty_tadir_keys TYPE STANDARD TABLE OF ty_tadir_key .

  CONSTANTS:
    BEGIN OF c_commands,
      find TYPE string VALUE 'FIND',
      run  TYPE string VALUE 'RUN',
      show TYPE string VALUE 'SHOW',
      calc TYPE string VALUE 'CALC',
      curr TYPE string VALUE 'CURR',
      unit TYPE string VALUE 'UNIT',
    END OF c_commands .
  CONSTANTS:
    BEGIN OF c_command_shortcuts,
      find TYPE c VALUE '?',
      run  TYPE c VALUE '!',
      show TYPE c VALUE '#',
      calc TYPE c VALUE '=',
      curr TYPE c VALUE '$',
      unit TYPE c VALUE '%',
    END OF c_command_shortcuts .
  CONSTANTS:
    BEGIN OF c_pgmid,
      r3tr TYPE pgmid VALUE 'R3TR',
      limu TYPE pgmid VALUE 'LIMU',
    END OF c_pgmid .
  CONSTANTS:
    BEGIN OF c_objects_db,
      tabl TYPE ty_tadir_key-object VALUE 'TABL',
      c1   TYPE c VALUE ',',
      view TYPE ty_tadir_key-object VALUE 'VIEW',
    END OF c_objects_db.
  CONSTANTS:
    BEGIN OF c_objects_bw,
      all  TYPE ty_tadir_key-object VALUE 'BW',
      c0   TYPE c VALUE ',',
      cube TYPE ty_tadir_key-object VALUE 'CUBE',
      c1   TYPE c VALUE ',',
      aggr TYPE ty_tadir_key-object VALUE 'AGGR',
      c2   TYPE c VALUE ',',
      odso TYPE ty_tadir_key-object VALUE 'ODSO',
      c3   TYPE c VALUE ',',
      adso TYPE ty_tadir_key-object VALUE 'ADSO',
      c4   TYPE c VALUE ',',
      iobj TYPE ty_tadir_key-object VALUE 'IOBJ',
      c5   TYPE c VALUE ',',
      iset TYPE ty_tadir_key-object VALUE 'ISET',
      c6   TYPE c VALUE ',',
      mpro TYPE ty_tadir_key-object VALUE 'MPRO',
      c7   TYPE c VALUE ',',
      hybr TYPE ty_tadir_key-object VALUE 'HYBR',
      c8   TYPE c VALUE ',',
      lpoa TYPE ty_tadir_key-object VALUE 'LPOA',
      c9   TYPE c VALUE ',',
      fbpa TYPE ty_tadir_key-object VALUE 'FBPA',
      c10  TYPE c VALUE ',',
      segr TYPE ty_tadir_key-object VALUE 'SEGR',
      c11  TYPE c VALUE ',',
      copr TYPE ty_tadir_key-object VALUE 'COPR',
      c12  TYPE c VALUE ',',
      ainx TYPE ty_tadir_key-object VALUE 'AINX',
      c13  TYPE c VALUE ',',
      hcpr TYPE ty_tadir_key-object VALUE 'HCPR',
      c14  TYPE c VALUE ',',
      trpr TYPE ty_tadir_key-object VALUE 'TRPR',
    END OF c_objects_bw.
  CONSTANTS:
    BEGIN OF c_objects_abap,
      prog TYPE ty_tadir_key-object VALUE 'PROG',
      c1   TYPE c VALUE ',',
      fugr TYPE ty_tadir_key-object VALUE 'FUGR',
      c2   TYPE c VALUE ',',
      func TYPE ty_tadir_key-object VALUE 'FUNC',
      c3   TYPE c VALUE ',',
      intf TYPE ty_tadir_key-object VALUE 'INTF',
      c4   TYPE c VALUE ',',
      clas TYPE ty_tadir_key-object VALUE 'CLAS',
      c5   TYPE c VALUE ',',
      meth TYPE ty_tadir_key-object VALUE 'METH',
      c6   TYPE c VALUE ',',
      type TYPE ty_tadir_key-object VALUE 'TYPE',
    END OF c_objects_abap.
  CONSTANTS:
    BEGIN OF c_objects_exec,
      prog TYPE ty_tadir_key-object VALUE 'PROG',
      c1   TYPE c VALUE ',',
      tran TYPE ty_tadir_key-object VALUE 'TRAN',
      c2   TYPE c VALUE ',',
      func TYPE ty_tadir_key-object VALUE 'FUNC',
    END OF c_objects_exec.
  CONSTANTS:
    BEGIN OF c_table_class,
      transp  TYPE dd02l-tabclass VALUE 'TRANSP',
      c1      TYPE c VALUE ',',
      cluster TYPE dd02l-tabclass VALUE 'CLUSTER',
      c2      TYPE c VALUE ',',
      pool    TYPE dd02l-tabclass VALUE 'POOL',
      c3      TYPE c VALUE ',',
      view    TYPE dd02l-tabclass VALUE 'VIEW',
      c4      TYPE c VALUE ',',
      inttab  TYPE dd02l-tabclass VALUE 'INTTAB',
      c5      TYPE c VALUE ',',
      append  TYPE dd02l-tabclass VALUE 'APPEND',
    END OF c_table_class.
ENDINTERFACE.
