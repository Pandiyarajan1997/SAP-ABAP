*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZQM_STATUS......................................*
DATA:  BEGIN OF STATUS_ZQM_STATUS                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQM_STATUS                    .
CONTROLS: TCTRL_ZQM_STATUS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZQM_STATUS                    .
TABLES: ZQM_STATUS                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
