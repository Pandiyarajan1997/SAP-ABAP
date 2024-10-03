*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPAINTER_MASTER.................................*
DATA:  BEGIN OF STATUS_ZPAINTER_MASTER               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPAINTER_MASTER               .
CONTROLS: TCTRL_ZPAINTER_MASTER
            TYPE TABLEVIEW USING SCREEN '1111'.
*.........table declarations:.................................*
TABLES: *ZPAINTER_MASTER               .
TABLES: ZPAINTER_MASTER                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
