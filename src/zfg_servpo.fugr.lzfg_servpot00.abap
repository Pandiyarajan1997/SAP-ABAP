*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_REGVEN_TXBP.................................*
DATA:  BEGIN OF STATUS_ZMM_REGVEN_TXBP               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_REGVEN_TXBP               .
CONTROLS: TCTRL_ZMM_REGVEN_TXBP
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZMM_SERPO_TAX_BP................................*
DATA:  BEGIN OF STATUS_ZMM_SERPO_TAX_BP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_SERPO_TAX_BP              .
CONTROLS: TCTRL_ZMM_SERPO_TAX_BP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_REGVEN_TXBP               .
TABLES: *ZMM_SERPO_TAX_BP              .
TABLES: ZMM_REGVEN_TXBP                .
TABLES: ZMM_SERPO_TAX_BP               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
