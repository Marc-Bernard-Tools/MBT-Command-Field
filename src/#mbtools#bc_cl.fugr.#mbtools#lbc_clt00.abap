*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2021-01-28 at 19:33:43
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: /MBTOOLS/CMDS...................................*
DATA:  BEGIN OF STATUS_/MBTOOLS/CMDS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/MBTOOLS/CMDS                 .
CONTROLS: TCTRL_/MBTOOLS/CMDS
            TYPE TABLEVIEW USING SCREEN '2000'.
*.........table declarations:.................................*
TABLES: */MBTOOLS/CMDS                 .
TABLES: /MBTOOLS/CMDS                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
