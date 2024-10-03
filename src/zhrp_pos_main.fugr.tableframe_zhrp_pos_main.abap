*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZHRP_POS_MAIN
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZHRP_POS_MAIN      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
