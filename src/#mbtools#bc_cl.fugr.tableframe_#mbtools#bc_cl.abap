*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/MBTOOLS/BC_CL
*   generation date: 2021-01-28 at 19:33:39
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/MBTOOLS/BC_CL     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
