*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDMS_PO_RE_SERIS
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDMS_PO_RE_SERIS   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
