*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTRIP_PLANT_MAPP
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTRIP_PLANT_MAPP   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
