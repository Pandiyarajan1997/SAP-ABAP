FUNCTION ZCUST_CREDIT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------



CALL FUNCTION 'RSRD_F4_REMOVE_DUPLICATES'
  TABLES
    SHLP_TAB          = SHLP_TAB
    RECORD_TAB        = RECORD_TAB
  CHANGING
    SHLP              = SHLP
    CALLCONTROL       = CALLCONTROL
          .



ENDFUNCTION.
