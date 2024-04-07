INTERFACE /mbtools/if_command_field
  PUBLIC .

************************************************************************
* MBT Command Field
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  CONSTANTS:
    BEGIN OF c_pgmid,
      r3tr  TYPE pgmid VALUE 'R3TR',
      limu  TYPE pgmid VALUE 'LIMU',
      basis TYPE pgmid VALUE 'ZZZZ',
    END OF c_pgmid .
  CONSTANTS:
    BEGIN OF c_objects_db,
      tabl TYPE adir_key-object VALUE 'TABL',
      c1   TYPE c VALUE ',',
      view TYPE adir_key-object VALUE 'VIEW',
    END OF c_objects_db.
  CONSTANTS:
    BEGIN OF c_objects_bw,
      all  TYPE adir_key-object VALUE 'BW',
      c0   TYPE c VALUE ',',
      cube TYPE adir_key-object VALUE 'CUBE',
      c1   TYPE c VALUE ',',
      aggr TYPE adir_key-object VALUE 'AGGR',
      c2   TYPE c VALUE ',',
      odso TYPE adir_key-object VALUE 'ODSO',
      c3   TYPE c VALUE ',',
      adso TYPE adir_key-object VALUE 'ADSO',
      c4   TYPE c VALUE ',',
      iobj TYPE adir_key-object VALUE 'IOBJ',
      c5   TYPE c VALUE ',',
      iset TYPE adir_key-object VALUE 'ISET',
      c6   TYPE c VALUE ',',
      mpro TYPE adir_key-object VALUE 'MPRO',
      c7   TYPE c VALUE ',',
      hybr TYPE adir_key-object VALUE 'HYBR',
      c8   TYPE c VALUE ',',
      lpoa TYPE adir_key-object VALUE 'LPOA',
      c9   TYPE c VALUE ',',
      fbpa TYPE adir_key-object VALUE 'FBPA',
      c10  TYPE c VALUE ',',
      segr TYPE adir_key-object VALUE 'SEGR',
      c11  TYPE c VALUE ',',
      copr TYPE adir_key-object VALUE 'COPR',
      c12  TYPE c VALUE ',',
      ainx TYPE adir_key-object VALUE 'AINX',
      c13  TYPE c VALUE ',',
      hcpr TYPE adir_key-object VALUE 'HCPR',
      c14  TYPE c VALUE ',',
      trpr TYPE adir_key-object VALUE 'TRPR',
    END OF c_objects_bw.
  CONSTANTS:
    BEGIN OF c_objects_abap,
      prog TYPE adir_key-object VALUE 'PROG',
      c1   TYPE c VALUE ',',
      reps TYPE adir_key-object VALUE 'REPS',
      c2   TYPE c VALUE ',',
      fugr TYPE adir_key-object VALUE 'FUGR',
      c3   TYPE c VALUE ',',
      func TYPE adir_key-object VALUE 'FUNC',
      c4   TYPE c VALUE ',',
      intf TYPE adir_key-object VALUE 'INTF',
      c5   TYPE c VALUE ',',
      clas TYPE adir_key-object VALUE 'CLAS',
      c6   TYPE c VALUE ',',
      meth TYPE adir_key-object VALUE 'METH',
      c7   TYPE c VALUE ',',
      type TYPE adir_key-object VALUE 'TYPE',
    END OF c_objects_abap.
  CONSTANTS:
    BEGIN OF c_objects_limu,
      mess TYPE adir_key-object VALUE 'MESS',
    END OF c_objects_limu.
  CONSTANTS:
    BEGIN OF c_objects_exec,
      prog TYPE adir_key-object VALUE 'PROG',
      c1   TYPE c VALUE ',',
      tran TYPE adir_key-object VALUE 'TRAN',
      c2   TYPE c VALUE ',',
      func TYPE adir_key-object VALUE 'FUNC',
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
  CONSTANTS:
    " Matches /mbtools/cl_sap=>class_constructor
    BEGIN OF c_objects_basis,
      activity         TYPE adir_key-object VALUE 'ZACT',
      c1               TYPE c VALUE ',',
      client           TYPE adir_key-object VALUE 'ZCLI',
      c2               TYPE c VALUE ',',
      devclass         TYPE adir_key-object VALUE 'ZPCK',
      c3               TYPE c VALUE ',',
      owner            TYPE adir_key-object VALUE 'ZOWN',
      c4               TYPE c VALUE ',',
      project          TYPE adir_key-object VALUE 'ZPRJ',
      c5               TYPE c VALUE ',',
      request          TYPE adir_key-object VALUE 'ZREQ',
      c6               TYPE c VALUE ',',
      system           TYPE adir_key-object VALUE 'ZSYS',
      c7               TYPE c VALUE ',',
      target_group     TYPE adir_key-object VALUE 'ZGRP',
      c8               TYPE c VALUE ',',
      translayer       TYPE adir_key-object VALUE 'ZLAY',
      c9               TYPE c VALUE ',',
      transport_target TYPE adir_key-object VALUE 'ZTGT',
      c10              TYPE c VALUE ',',
      user             TYPE adir_key-object VALUE 'ZUSR',
    END OF c_objects_basis.
ENDINTERFACE.
