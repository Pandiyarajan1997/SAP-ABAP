*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_SCM_HEADER..................................*
DATA:  BEGIN OF STATUS_ZSD_SCM_HEADER                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_SCM_HEADER                .
CONTROLS: TCTRL_ZSD_SCM_HEADER
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZSD_SPL_SALE_HD.................................*
DATA:  BEGIN OF STATUS_ZSD_SPL_SALE_HD               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_SPL_SALE_HD               .
CONTROLS: TCTRL_ZSD_SPL_SALE_HD
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_SCM_HEADER                .
TABLES: *ZSD_SPL_SALE_HD               .
TABLES: ZSD_SCM_HEADER                 .
TABLES: ZSD_SPL_SALE_HD                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
