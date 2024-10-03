*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWATER_QUALITY..................................*
DATA:  BEGIN OF STATUS_ZWATER_QUALITY                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWATER_QUALITY                .
CONTROLS: TCTRL_ZWATER_QUALITY
            TYPE TABLEVIEW USING SCREEN '1111'.
*.........table declarations:.................................*
TABLES: *ZWATER_QUALITY                .
TABLES: ZWATER_QUALITY                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
