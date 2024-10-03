FUNCTION ZBAPI_SKU_DISCOUNT_CN_NEW .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_MATNR) TYPE  MARA-MATNR OPTIONAL
*"     VALUE(P_KUNNR) TYPE  KNA1-KUNNR OPTIONAL
*"     VALUE(P_VKBUR) TYPE  KNVV-VKBUR OPTIONAL
*"     VALUE(P_KSCHL) TYPE  KONH-KSCHL OPTIONAL
*"  TABLES
*"      IT_DISC_MASTER STRUCTURE  ZSTR_CNF_DISC_MASTER OPTIONAL
*"      S_DATE STRUCTURE  ZSTR_SKU_DATE OPTIONAL
*"      S_MTART TYPE  ZTT_CNF_MTART OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"--------------------------------------------------------------------

*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------
*&    AUTHOR          : VELRAJ
*&    CREATED ON      : 16.04.2015
*&    COMPANY         : CLSS
*&    OBJECTIVE       : THIS FUNCTION MODULE CAN BE USED TO GET DISCOUNT DETAILS USED FOR SALESPRO BUSINESS
*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------


********************************************************************************************************************************
*&----------------------------------------------- Discount Condtion --------------------------------------------------------
* Z004, Z005, ZPRB, ZBSD
* Z004 - Cash Discount-Mtl      - A005, A837, A832
* Z005 - Cash Discount-Slab     - A837, A832, A005
* Z006 - Cash Discount-Slab-Qty - A837, A832, A005
* ZPRB - Product Rebate         - A005, A837, A832
* ZBSD - Special Discount       - A005, A837, A832
* Z007 - T.R DISCOUNT            - A831 , A833,A834,A835
********************************************************************************************************************************

*&-------------------------------------- DECLARATIONS - BEGIN -----------------------------------------------
  DATA: WA_RETURN       TYPE          BAPIRET2,
        WA_DISC_MASTER  TYPE          ZSTR_CNF_DISC_MASTER,
        IT_DISC_TEMP    TYPE TABLE OF ZSTR_CNF_DISC_MASTER,
        WA_DISC_TEMP    TYPE          ZSTR_CNF_DISC_MASTER,
        LV_COUNT        TYPE          I,
        LV_MATNR        TYPE          MARA-MATNR.
  FIELD-SYMBOLS: <FS_DISC> TYPE       ZSTR_CNF_DISC_MASTER.
*&-------------------------------------- DECLARATIONS - END -------------------------------------------------

  PERFORM DISC_MASTER_INITIALIZATION TABLES   R_MTART
                                              S_MTART
                                              R_MATNR
                                              R_WERKS
                                              S_DATE
                                              R_KNUMH
                                              R_KUNNR
                                              R_VKORG
                                              R_KSCHL
                                              R_VKBUR
                                              R_REGIO
                                              RETURN
                                     USING    P_BUKRS
                                              P_MATNR
                                              P_KUNNR
                                              P_VKBUR
                                              P_KSCHL.

*&----------------------------------------- FETCH Data - BEGIN ------------------------------------------------
*&------------------------------------ Get Material List for the plant

  IF NOT R_WERKS[] IS INITIAL.
    PERFORM GET_MARARCRM TABLES R_MATNR R_WERKS R_MTART CHANGING GTH_MARARCRM.
  ELSE.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'E'   'Error!!! - No Relevant Data Found for the given input!!!'.
    EXIT.
  ENDIF.

*&-------------------------------------------------------- discount master (A005, A832, A837)
  IF GTH_MARARCRM  IS NOT INITIAL.
    IF P_KUNNR IS INITIAL AND P_VKBUR IS INITIAL.
      PERFORM GET_A832  TABLES R_KSCHL          R_REGIO R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A832.
      PERFORM GET_A837  TABLES R_KSCHL          R_VKBUR R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A837.
      PERFORM GET_A005  TABLES R_KSCHL R_VKORG  R_KUNNR R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A005.

"""""""""""ADDED BY RAM ON 8/2/15

      PERFORM GET_A831  TABLES R_KSCHL          R_VKBUR R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A831.  "SAELS OFF/MATERIAL
      PERFORM GET_A833  TABLES R_KSCHL          R_KUNNR R_KNUMH R_REGIO RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A833.
      PERFORM GET_A834  TABLES R_KSCHL          R_VKBUR R_KUNNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A834.
      PERFORM GET_A835  TABLES R_KSCHL          R_VKBUR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A835.  "SAELS OFF/MATERIAL
""""""""""ENDED BY RAM ON 8/2/15
    ELSEIF P_KUNNR IS NOT INITIAL AND P_VKBUR IS INITIAL.
      PERFORM GET_A005  TABLES R_KSCHL R_VKORG  R_KUNNR R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A005.
    ELSEIF P_VKBUR IS NOT INITIAL AND P_KUNNR IS INITIAL.
      PERFORM GET_A837  TABLES R_KSCHL          R_VKBUR R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A837.
    ENDIF.
  ENDIF.
*&----------------------------------------- FETCH Data - END ------------------------------------------------


*&------------------------------------------ DATA AGGREGATION- BEGIN ------------------------------------------
  LOOP AT GT_A005 INTO GS_A005.
    CASE GS_A005-KSCHL.
      WHEN 'Z005'.
        WA_DISC_TEMP-TABLE_NAME = 'A005'.
        MOVE-CORRESPONDING GS_A005      TO WA_DISC_TEMP.
        APPEND             WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'Z006'.
        WA_DISC_TEMP-TABLE_NAME = 'A005'.
        MOVE-CORRESPONDING  GS_A005      TO WA_DISC_TEMP.
        APPEND              WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A005'.
        MOVE-CORRESPONDING  GS_A005        TO WA_DISC_MASTER.
        APPEND              WA_DISC_MASTER TO IT_DISC_MASTER.
    ENDCASE.

    CLEAR: WA_DISC_MASTER, WA_DISC_TEMP, GS_A005.
  ENDLOOP.



    LOOP AT GT_A831 INTO GS_A831.
    CASE GS_A831-KSCHL.
      WHEN 'Z007'.
        WA_DISC_TEMP-TABLE_NAME = 'A831'.
        MOVE-CORRESPONDING GS_A831      TO WA_DISC_TEMP.
        APPEND             WA_DISC_TEMP TO IT_DISC_TEMP.
*      WHEN 'Z006'.
*        WA_DISC_TEMP-TABLE_NAME = 'A831'.
*        MOVE-CORRESPONDING  GS_A832      TO WA_DISC_TEMP.
*        APPEND              WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A831'.
        MOVE-CORRESPONDING  GS_A831        TO WA_DISC_MASTER.
        APPEND              WA_DISC_MASTER TO IT_DISC_MASTER.
    ENDCASE.

    CLEAR: WA_DISC_MASTER, WA_DISC_TEMP, GS_A831.
  ENDLOOP.

  LOOP AT GT_A833 INTO GS_A833.
    CASE GS_A833-KSCHL.
      WHEN 'Z007'.
        WA_DISC_TEMP-TABLE_NAME = 'A833'.
        MOVE-CORRESPONDING GS_A833      TO WA_DISC_TEMP.
        APPEND             WA_DISC_TEMP TO IT_DISC_TEMP.
*      WHEN 'Z006'.
*        WA_DISC_TEMP-TABLE_NAME = 'A831'.
*        MOVE-CORRESPONDING  GS_A832      TO WA_DISC_TEMP.
*        APPEND              WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A833'.
        MOVE-CORRESPONDING  GS_A833        TO WA_DISC_MASTER.
        APPEND              WA_DISC_MASTER TO IT_DISC_MASTER.
    ENDCASE.

    CLEAR: WA_DISC_MASTER, WA_DISC_TEMP, GS_A833.
  ENDLOOP.

    LOOP AT GT_A834 INTO GS_A834.
    CASE GS_A834-KSCHL.
      WHEN 'Z007'.
        WA_DISC_TEMP-TABLE_NAME = 'A834'.
        MOVE-CORRESPONDING GS_A834      TO WA_DISC_TEMP.
        APPEND             WA_DISC_TEMP TO IT_DISC_TEMP.
*      WHEN 'Z006'.
*        WA_DISC_TEMP-TABLE_NAME = 'A831'.
*        MOVE-CORRESPONDING  GS_A832      TO WA_DISC_TEMP.
*        APPEND              WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A834'.
        MOVE-CORRESPONDING  GS_A833        TO WA_DISC_MASTER.
        APPEND              WA_DISC_MASTER TO IT_DISC_MASTER.
    ENDCASE.

    CLEAR: WA_DISC_MASTER, WA_DISC_TEMP, GS_A834.
  ENDLOOP .

    LOOP AT GT_A835 INTO GS_A835.
    CASE GS_A835-KSCHL.
      WHEN 'Z007'.
        WA_DISC_TEMP-TABLE_NAME = 'A835'.
        MOVE-CORRESPONDING GS_A835      TO WA_DISC_TEMP.
        APPEND             WA_DISC_TEMP TO IT_DISC_TEMP.
*      WHEN 'Z006'.
*        WA_DISC_TEMP-TABLE_NAME = 'A831'.
*        MOVE-CORRESPONDING  GS_A832      TO WA_DISC_TEMP.
*        APPEND              WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A835'.
        MOVE-CORRESPONDING  GS_A835        TO WA_DISC_MASTER.
        APPEND              WA_DISC_MASTER TO IT_DISC_MASTER.
    ENDCASE.

    CLEAR: WA_DISC_MASTER, WA_DISC_TEMP, GS_A835.
  ENDLOOP .


  LOOP AT GT_A832 INTO GS_A832.
    CASE GS_A832-KSCHL.
      WHEN 'Z005'.
        WA_DISC_TEMP-TABLE_NAME = 'A832'.
        MOVE-CORRESPONDING GS_A832      TO WA_DISC_TEMP.
        APPEND             WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'Z006'.
        WA_DISC_TEMP-TABLE_NAME = 'A832'.
        MOVE-CORRESPONDING  GS_A832      TO WA_DISC_TEMP.
        APPEND              WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A832'.
        MOVE-CORRESPONDING  GS_A832        TO WA_DISC_MASTER.
        APPEND              WA_DISC_MASTER TO IT_DISC_MASTER.
    ENDCASE.

    CLEAR: WA_DISC_MASTER, WA_DISC_TEMP, GS_A832.
  ENDLOOP.

  LOOP AT GT_A837 INTO GS_A837.
    CASE GS_A837-KSCHL.
      WHEN 'Z005'.
        WA_DISC_TEMP-TABLE_NAME = 'A837'.
        MOVE-CORRESPONDING GS_A837      TO WA_DISC_TEMP.
        APPEND             WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN 'Z006'.
        WA_DISC_TEMP-TABLE_NAME = 'A837'.
        MOVE-CORRESPONDING GS_A837      TO WA_DISC_TEMP.
        APPEND             WA_DISC_TEMP TO IT_DISC_TEMP.
      WHEN OTHERS.
        WA_DISC_MASTER-TABLE_NAME = 'A837'.
        MOVE-CORRESPONDING GS_A837        TO WA_DISC_MASTER.
        APPEND             WA_DISC_MASTER TO IT_DISC_MASTER.
    ENDCASE.

    CLEAR: WA_DISC_MASTER, GS_A837.
  ENDLOOP.

  IF NOT IT_DISC_TEMP[] IS INITIAL.
    SORT: IT_DISC_TEMP BY KNUMH.

    SELECT KNUMH
           KOPOS
           KLFN1
           KSTBM
           KBETR
           FROM KONM
           INTO TABLE GT_KONM
           FOR ALL ENTRIES IN IT_DISC_TEMP
           WHERE KNUMH = IT_DISC_TEMP-KNUMH.
    IF SY-SUBRC = 0.
      SORT GT_KONM BY KNUMH.
    ELSE.
      REFRESH GT_KONM.
    ENDIF.

    SELECT KNUMH
           KOPOS
           KLFN1
           KSTBW
           KBETR
           FROM KONW
           INTO TABLE GT_KONW
           FOR ALL ENTRIES IN IT_DISC_TEMP
           WHERE KNUMH = IT_DISC_TEMP-KNUMH.
    IF SY-SUBRC = 0.
      SORT GT_KONW BY KNUMH.
    ELSE.
      REFRESH GT_KONW.
    ENDIF.
  ENDIF.

  LOOP AT IT_DISC_TEMP INTO WA_DISC_TEMP.
    MOVE-CORRESPONDING WA_DISC_TEMP TO WA_DISC_MASTER.

IF WA_DISC_MASTER-KSCHL <> 'Z007' .
    READ TABLE GT_KONM TRANSPORTING NO FIELDS WITH KEY KNUMH = WA_DISC_TEMP-KNUMH BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_INDEX = SY-TABIX.
      CLEAR LV_COUNT.

      LOOP AT GT_KONM INTO GS_KONM FROM LV_INDEX.
        IF GS_KONM-KNUMH = WA_DISC_TEMP-KNUMH.
          LV_COUNT = LV_COUNT + 1.

          WA_DISC_MASTER-KNUMH    = GS_KONM-KNUMH.
          WA_DISC_MASTER-KOPOS    = GS_KONM-KOPOS.
          WA_DISC_MASTER-KBETR    = GS_KONM-KBETR.              " condition value
          WA_DISC_MASTER-SCALE_COUNT = LV_COUNT.
          WA_DISC_MASTER-SCALE_VALUE = GS_KONM-KSTBM.           " quantity scale
          WA_DISC_MASTER-SCALE_UNIT  = WA_DISC_TEMP-KONMS.      " quantity unit

          APPEND WA_DISC_MASTER TO IT_DISC_MASTER.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    READ TABLE GT_KONW TRANSPORTING NO FIELDS WITH KEY KNUMH = WA_DISC_TEMP-KNUMH BINARY SEARCH.
    IF SY-SUBRC = 0.
      LV_INDEX = SY-TABIX.
      CLEAR LV_COUNT.

      LOOP AT GT_KONW INTO GS_KONW FROM LV_INDEX.
        IF GS_KONW-KNUMH = WA_DISC_TEMP-KNUMH.
          LV_COUNT = LV_COUNT + 1.

          WA_DISC_MASTER-KNUMH    = GS_KONW-KNUMH.
          WA_DISC_MASTER-KOPOS    = GS_KONW-KOPOS.
          WA_DISC_MASTER-KBETR    = GS_KONW-KBETR.              " condition value
          WA_DISC_MASTER-SCALE_COUNT = LV_COUNT.
          WA_DISC_MASTER-SCALE_VALUE = GS_KONW-KSTBW.           " quantity scale
          WA_DISC_MASTER-SCALE_UNIT  = WA_DISC_TEMP-KONWS.      " quantity unit

          APPEND WA_DISC_MASTER TO IT_DISC_MASTER.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
    ENDIF.

    IF WA_DISC_MASTER-KSCHL = 'Z007' .
    "  LOOP AT IT_DISC_TEMP INTO WA_DISC_TEMP .
*          WA_DISC_MASTER-KNUMH    = WA_DISC_TEMP-KNUMH.
*          WA_DISC_MASTER-KOPOS    = WA_DISC_TEMP-KOPOS.
*          WA_DISC_MASTER-KBETR    = WA_DISC_TEMP-KBETR.
          APPEND WA_DISC_MASTER TO IT_DISC_MASTER.
     "     ENDLOOP.
    ENDIF.

  ENDLOOP.

  SORT: GTH_MARARCRM       BY MATNR,
        IT_DISC_MASTER    BY MATNR.

  LOOP AT IT_DISC_MASTER ASSIGNING <FS_DISC>.
    IF LV_MATNR = <FS_DISC>-MATNR.
        IF WA_DISC_TEMP-TABLE_NAME  <> 'A834'  AND WA_DISC_TEMP-TABLE_NAME <> 'A835'.
      DELETE IT_DISC_MASTER.
          ENDIF.
    ELSE.
          IF WA_DISC_TEMP-TABLE_NAME  <> 'A834' AND WA_DISC_TEMP-TABLE_NAME <> 'A835' .
      READ TABLE GTH_MARARCRM TRANSPORTING NO FIELDS WITH KEY MATNR = <FS_DISC>-MATNR.
      IF SY-SUBRC <> 0.
        LV_MATNR = <FS_DISC>-MATNR.
        DELETE IT_DISC_MASTER.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
*&------------------------------------------ DATA AGGREGATION- END ------------------------------------------


*&------------------------------------ BAPI Return Messages - BEGIN --------------------------------------------

  IF NOT IT_DISC_MASTER[] IS INITIAL.
    SORT IT_DISC_MASTER BY KSCHL MATNR KNUMH.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'S'   'Success!!! - Discount master is updated!!!'.
  ELSE.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'E'   'Error!!! - Discount master is not updated!!!'.
  ENDIF.
*&------------------------------------ BAPI Return Messages - BEGIN --------------------------------------------

  CLEAR: GV_INDEX_MSG, LV_INDEX, R_KNUMH[], R_KUNNR[], R_MATNR[], R_KSCHL[], R_VKORG[], R_WERKS[], R_MTART[], S_MTART[], S_DATE[].
  CLEAR: GTH_MARARCRM, GS_MARARCRM, GT_A831, GS_A831, GT_A832, GS_A832, GT_A837, GS_A837, GT_A005, GS_A005, R_VKBUR[].

ENDFUNCTION.
