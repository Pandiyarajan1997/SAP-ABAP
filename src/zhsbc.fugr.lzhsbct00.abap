*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHSBC_HOUSE_BANK................................*
DATA:  BEGIN OF STATUS_ZHSBC_HOUSE_BANK              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHSBC_HOUSE_BANK              .
CONTROLS: TCTRL_ZHSBC_HOUSE_BANK
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZHSBC_MAIL......................................*
DATA:  BEGIN OF STATUS_ZHSBC_MAIL                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHSBC_MAIL                    .
CONTROLS: TCTRL_ZHSBC_MAIL
            TYPE TABLEVIEW USING SCREEN '0046'.
*...processing: ZHSBC_PPC.......................................*
DATA:  BEGIN OF STATUS_ZHSBC_PPC                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHSBC_PPC                     .
CONTROLS: TCTRL_ZHSBC_PPC
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZHSBC_SFTP......................................*
DATA:  BEGIN OF STATUS_ZHSBC_SFTP                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHSBC_SFTP                    .
*.........table declarations:.................................*
TABLES: *ZHSBC_HOUSE_BANK              .
TABLES: *ZHSBC_MAIL                    .
TABLES: *ZHSBC_PPC                     .
TABLES: *ZHSBC_SFTP                    .
TABLES: ZHSBC_HOUSE_BANK               .
TABLES: ZHSBC_MAIL                     .
TABLES: ZHSBC_PPC                      .
TABLES: ZHSBC_SFTP                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
