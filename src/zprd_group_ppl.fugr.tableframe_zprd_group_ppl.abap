*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPRD_GROUP_PPL
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPRD_GROUP_PPL     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
