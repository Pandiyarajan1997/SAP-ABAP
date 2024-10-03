FUNCTION ZDESCENDING.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(TOTAL_BR) TYPE  I
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"--------------------------------------------------------------------
"a





  CALL FUNCTION 'F4UT_PARAMETER_SORT'
    EXPORTING
      PARAMETER_SORT  = 'FKDAT'
      LEADING_COLUMNS = '0'
      DESCENDING      = 'X'
    TABLES
      SHLP_TAB        = SHLP_TAB
      RECORD_TAB      = RECORD_TAB
    CHANGING
      SHLP            = SHLP
      CALLCONTROL     = CALLCONTROL.

  CALLCONTROL-SORTOFF = 'X'.









ENDFUNCTION.
