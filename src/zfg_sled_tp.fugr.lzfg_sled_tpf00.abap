*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZV_TP_LOG_HEAD..................................*
FORM GET_DATA_ZV_TP_LOG_HEAD.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZMM_TR_POST_HEAD WHERE
(VIM_WHERETAB) .
    CLEAR ZV_TP_LOG_HEAD .
ZV_TP_LOG_HEAD-MANDT =
ZMM_TR_POST_HEAD-MANDT .
ZV_TP_LOG_HEAD-MBLNR =
ZMM_TR_POST_HEAD-MBLNR .
ZV_TP_LOG_HEAD-MJAHR =
ZMM_TR_POST_HEAD-MJAHR .
ZV_TP_LOG_HEAD-MAT_SLIP =
ZMM_TR_POST_HEAD-MAT_SLIP .
ZV_TP_LOG_HEAD-BLDAT =
ZMM_TR_POST_HEAD-BLDAT .
ZV_TP_LOG_HEAD-BUDAT =
ZMM_TR_POST_HEAD-BUDAT .
ZV_TP_LOG_HEAD-CREATED_BY =
ZMM_TR_POST_HEAD-CREATED_BY .
ZV_TP_LOG_HEAD-CREATED_AT =
ZMM_TR_POST_HEAD-CREATED_AT .
<VIM_TOTAL_STRUC> = ZV_TP_LOG_HEAD.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZV_TP_LOG_HEAD .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZV_TP_LOG_HEAD.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZV_TP_LOG_HEAD-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZMM_TR_POST_HEAD WHERE
  MBLNR = ZV_TP_LOG_HEAD-MBLNR AND
  MJAHR = ZV_TP_LOG_HEAD-MJAHR AND
  MAT_SLIP = ZV_TP_LOG_HEAD-MAT_SLIP .
    IF SY-SUBRC = 0.
    DELETE ZMM_TR_POST_HEAD .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZMM_TR_POST_HEAD WHERE
  MBLNR = ZV_TP_LOG_HEAD-MBLNR AND
  MJAHR = ZV_TP_LOG_HEAD-MJAHR AND
  MAT_SLIP = ZV_TP_LOG_HEAD-MAT_SLIP .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZMM_TR_POST_HEAD.
    ENDIF.
ZMM_TR_POST_HEAD-MANDT =
ZV_TP_LOG_HEAD-MANDT .
ZMM_TR_POST_HEAD-MBLNR =
ZV_TP_LOG_HEAD-MBLNR .
ZMM_TR_POST_HEAD-MJAHR =
ZV_TP_LOG_HEAD-MJAHR .
ZMM_TR_POST_HEAD-MAT_SLIP =
ZV_TP_LOG_HEAD-MAT_SLIP .
ZMM_TR_POST_HEAD-BLDAT =
ZV_TP_LOG_HEAD-BLDAT .
ZMM_TR_POST_HEAD-BUDAT =
ZV_TP_LOG_HEAD-BUDAT .
ZMM_TR_POST_HEAD-CREATED_BY =
ZV_TP_LOG_HEAD-CREATED_BY .
ZMM_TR_POST_HEAD-CREATED_AT =
ZV_TP_LOG_HEAD-CREATED_AT .
    IF SY-SUBRC = 0.
    UPDATE ZMM_TR_POST_HEAD ##WARN_OK.
    ELSE.
    INSERT ZMM_TR_POST_HEAD .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZV_TP_LOG_HEAD-UPD_FLAG,
STATUS_ZV_TP_LOG_HEAD-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZV_TP_LOG_HEAD.
  SELECT SINGLE * FROM ZMM_TR_POST_HEAD WHERE
MBLNR = ZV_TP_LOG_HEAD-MBLNR AND
MJAHR = ZV_TP_LOG_HEAD-MJAHR AND
MAT_SLIP = ZV_TP_LOG_HEAD-MAT_SLIP .
ZV_TP_LOG_HEAD-MANDT =
ZMM_TR_POST_HEAD-MANDT .
ZV_TP_LOG_HEAD-MBLNR =
ZMM_TR_POST_HEAD-MBLNR .
ZV_TP_LOG_HEAD-MJAHR =
ZMM_TR_POST_HEAD-MJAHR .
ZV_TP_LOG_HEAD-MAT_SLIP =
ZMM_TR_POST_HEAD-MAT_SLIP .
ZV_TP_LOG_HEAD-BLDAT =
ZMM_TR_POST_HEAD-BLDAT .
ZV_TP_LOG_HEAD-BUDAT =
ZMM_TR_POST_HEAD-BUDAT .
ZV_TP_LOG_HEAD-CREATED_BY =
ZMM_TR_POST_HEAD-CREATED_BY .
ZV_TP_LOG_HEAD-CREATED_AT =
ZMM_TR_POST_HEAD-CREATED_AT .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZV_TP_LOG_HEAD USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZV_TP_LOG_HEAD-MBLNR TO
ZMM_TR_POST_HEAD-MBLNR .
MOVE ZV_TP_LOG_HEAD-MJAHR TO
ZMM_TR_POST_HEAD-MJAHR .
MOVE ZV_TP_LOG_HEAD-MAT_SLIP TO
ZMM_TR_POST_HEAD-MAT_SLIP .
MOVE ZV_TP_LOG_HEAD-MANDT TO
ZMM_TR_POST_HEAD-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZMM_TR_POST_HEAD'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZMM_TR_POST_HEAD TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZMM_TR_POST_HEAD'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*...processing: ZV_TP_LOG_ITEM..................................*
FORM GET_DATA_ZV_TP_LOG_ITEM.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZMM_SLED_343_LOG WHERE
(VIM_WHERETAB) .
    CLEAR ZV_TP_LOG_ITEM .
ZV_TP_LOG_ITEM-MANDT =
ZMM_SLED_343_LOG-MANDT .
ZV_TP_LOG_ITEM-MBLNR =
ZMM_SLED_343_LOG-MBLNR .
ZV_TP_LOG_ITEM-MJAHR =
ZMM_SLED_343_LOG-MJAHR .
ZV_TP_LOG_ITEM-MBLPO =
ZMM_SLED_343_LOG-MBLPO .
ZV_TP_LOG_ITEM-MATNR =
ZMM_SLED_343_LOG-MATNR .
ZV_TP_LOG_ITEM-PLANT =
ZMM_SLED_343_LOG-PLANT .
ZV_TP_LOG_ITEM-STOR_LOC =
ZMM_SLED_343_LOG-STOR_LOC .
ZV_TP_LOG_ITEM-BATCH =
ZMM_SLED_343_LOG-BATCH .
ZV_TP_LOG_ITEM-QTY =
ZMM_SLED_343_LOG-QTY .
ZV_TP_LOG_ITEM-UOM =
ZMM_SLED_343_LOG-UOM .
<VIM_TOTAL_STRUC> = ZV_TP_LOG_ITEM.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZV_TP_LOG_ITEM .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZV_TP_LOG_ITEM.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZV_TP_LOG_ITEM-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZMM_SLED_343_LOG WHERE
  MBLNR = ZV_TP_LOG_ITEM-MBLNR AND
  MJAHR = ZV_TP_LOG_ITEM-MJAHR AND
  MBLPO = ZV_TP_LOG_ITEM-MBLPO .
    IF SY-SUBRC = 0.
    DELETE ZMM_SLED_343_LOG .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZMM_SLED_343_LOG WHERE
  MBLNR = ZV_TP_LOG_ITEM-MBLNR AND
  MJAHR = ZV_TP_LOG_ITEM-MJAHR AND
  MBLPO = ZV_TP_LOG_ITEM-MBLPO .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZMM_SLED_343_LOG.
    ENDIF.
ZMM_SLED_343_LOG-MANDT =
ZV_TP_LOG_ITEM-MANDT .
ZMM_SLED_343_LOG-MBLNR =
ZV_TP_LOG_ITEM-MBLNR .
ZMM_SLED_343_LOG-MJAHR =
ZV_TP_LOG_ITEM-MJAHR .
ZMM_SLED_343_LOG-MBLPO =
ZV_TP_LOG_ITEM-MBLPO .
ZMM_SLED_343_LOG-MATNR =
ZV_TP_LOG_ITEM-MATNR .
ZMM_SLED_343_LOG-PLANT =
ZV_TP_LOG_ITEM-PLANT .
ZMM_SLED_343_LOG-STOR_LOC =
ZV_TP_LOG_ITEM-STOR_LOC .
ZMM_SLED_343_LOG-BATCH =
ZV_TP_LOG_ITEM-BATCH .
ZMM_SLED_343_LOG-QTY =
ZV_TP_LOG_ITEM-QTY .
ZMM_SLED_343_LOG-UOM =
ZV_TP_LOG_ITEM-UOM .
    IF SY-SUBRC = 0.
    UPDATE ZMM_SLED_343_LOG ##WARN_OK.
    ELSE.
    INSERT ZMM_SLED_343_LOG .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZV_TP_LOG_ITEM-UPD_FLAG,
STATUS_ZV_TP_LOG_ITEM-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZV_TP_LOG_ITEM.
  SELECT SINGLE * FROM ZMM_SLED_343_LOG WHERE
MBLNR = ZV_TP_LOG_ITEM-MBLNR AND
MJAHR = ZV_TP_LOG_ITEM-MJAHR AND
MBLPO = ZV_TP_LOG_ITEM-MBLPO .
ZV_TP_LOG_ITEM-MANDT =
ZMM_SLED_343_LOG-MANDT .
ZV_TP_LOG_ITEM-MBLNR =
ZMM_SLED_343_LOG-MBLNR .
ZV_TP_LOG_ITEM-MJAHR =
ZMM_SLED_343_LOG-MJAHR .
ZV_TP_LOG_ITEM-MBLPO =
ZMM_SLED_343_LOG-MBLPO .
ZV_TP_LOG_ITEM-MATNR =
ZMM_SLED_343_LOG-MATNR .
ZV_TP_LOG_ITEM-PLANT =
ZMM_SLED_343_LOG-PLANT .
ZV_TP_LOG_ITEM-STOR_LOC =
ZMM_SLED_343_LOG-STOR_LOC .
ZV_TP_LOG_ITEM-BATCH =
ZMM_SLED_343_LOG-BATCH .
ZV_TP_LOG_ITEM-QTY =
ZMM_SLED_343_LOG-QTY .
ZV_TP_LOG_ITEM-UOM =
ZMM_SLED_343_LOG-UOM .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZV_TP_LOG_ITEM USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZV_TP_LOG_ITEM-MBLNR TO
ZMM_SLED_343_LOG-MBLNR .
MOVE ZV_TP_LOG_ITEM-MJAHR TO
ZMM_SLED_343_LOG-MJAHR .
MOVE ZV_TP_LOG_ITEM-MBLPO TO
ZMM_SLED_343_LOG-MBLPO .
MOVE ZV_TP_LOG_ITEM-MANDT TO
ZMM_SLED_343_LOG-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZMM_SLED_343_LOG'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZMM_SLED_343_LOG TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZMM_SLED_343_LOG'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
