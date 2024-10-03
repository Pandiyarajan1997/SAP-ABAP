*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_LETTERS_SIGN................................*
DATA:  BEGIN OF STATUS_ZHR_LETTERS_SIGN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_LETTERS_SIGN              .
CONTROLS: TCTRL_ZHR_LETTERS_SIGN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHR_LETTERS_SIGN              .
TABLES: ZHR_LETTERS_SIGN               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
