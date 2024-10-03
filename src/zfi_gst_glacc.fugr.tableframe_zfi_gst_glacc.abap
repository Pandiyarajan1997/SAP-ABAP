*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFI_GST_GLACC
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFI_GST_GLACC      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
