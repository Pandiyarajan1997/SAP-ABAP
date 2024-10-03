*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDIST_EINV_DTLS.................................*
DATA:  BEGIN OF STATUS_ZDIST_EINV_DTLS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDIST_EINV_DTLS               .
CONTROLS: TCTRL_ZDIST_EINV_DTLS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDIST_EINV_DTLS               .
TABLES: ZDIST_EINV_DTLS                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
