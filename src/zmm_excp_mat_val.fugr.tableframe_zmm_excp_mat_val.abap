*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZMM_EXCP_MAT_VAL
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZMM_EXCP_MAT_VAL   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
