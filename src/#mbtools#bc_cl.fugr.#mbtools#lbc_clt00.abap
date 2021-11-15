*---------------------------------------------------------------------*
*    view related data declarations
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
