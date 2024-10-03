FUNCTION ZBAPI_SKU_DISCOUNT_NEW_SIS .
*"----------------------------------------------------------------------
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
*"----------------------------------------------------------------------

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

*      DATA : IT_KONH TYPE TABLE OF KONH,
*           WA_KONH TYPE KONH.

*&-------------------------------------- DECLARATIONS - END -------------------------------------------------

data : today(1).
clear today.
if S_DATE[] is INITIAL.
  today = 'X'.
ENDIF.

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
                                              RT_KONH
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

*if R_KNUMH[] is NOT INITIAL.
*  SELECT * FROM KONH INTO TABLE IT_KONH WHERE KNUMH in R_KNUMH AND   KSCHL IN ('Z004', 'Z005', 'Z006', 'ZPRB', 'ZBSD').
*endif.

DATA : NO TYPE STRING,
       R_KNUMH_5_1 TYPE RANGE OF KONH-KNUMH, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       R_KNUMH_LINE_5_1 LIKE LINE OF R_KNUMH_5_1,
       R_KNUMH_832_1 TYPE RANGE OF KONH-KNUMH, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       R_KNUMH_LINE_832_1 LIKE LINE OF R_KNUMH_832_1,
       R_KNUMH_837_1 TYPE RANGE OF KONH-KNUMH, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       R_KNUMH_LINE_837_1 LIKE LINE OF R_KNUMH_837_1,
       R_KNUMH_833_1 TYPE RANGE OF KONH-KNUMH, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       R_KNUMH_LINE_833_1 LIKE LINE OF R_KNUMH_833_1,
       R_KNUMH_831_1 TYPE RANGE OF KONH-KNUMH, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       R_KNUMH_LINE_831_1 LIKE LINE OF R_KNUMH_831_1,
       R_KNUMH_834_1 TYPE RANGE OF KONH-KNUMH, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       R_KNUMH_LINE_834_1 LIKE LINE OF R_KNUMH_834_1,
       R_KNUMH_835_1 TYPE RANGE OF KONH-KNUMH, "#EC CI_USAGE_OK[2220005] " Added by <IT-CAR Tool> during Code Remediation
       R_KNUMH_LINE_835_1 LIKE LINE OF R_KNUMH_835_1.




 LOOP AT RT_KONH INTO WA_KONH WHERE KVEWE = 'A' AND KOTABNR = '5'.
*            CLEAR R_KNUMH_5[].

            R_KNUMH_LINE_5-SIGN   = 'I'.
            R_KNUMH_LINE_5-OPTION = 'EQ'.
            R_KNUMH_LINE_5-LOW    = WA_KONH-KNUMH.
            APPEND R_KNUMH_LINE_5 TO R_KNUMH_5.
            CLEAR: R_KNUMH_LINE_5.

 ENDLOOP.

 DESCRIBE TABLE R_KNUMH_5 LINES NO.

LOOP AT R_KNUMH_5 INTO R_KNUMH_LINE_5.
  IF SY-TABIX = 1.
                R_KNUMH_LINE_5_1-SIGN   = 'I'.
            R_KNUMH_LINE_5_1-OPTION = 'BT'.
            R_KNUMH_LINE_5_1-LOW = R_KNUMH_LINE_5-LOW.
      ENDIF.
        CHECK SY-TABIX = NO.
        R_KNUMH_LINE_5_1-HIGH  = R_KNUMH_LINE_5-LOW.
            APPEND R_KNUMH_LINE_5_1 TO R_KNUMH_5_1.
            CLEAR: R_KNUMH_LINE_5_1.
     ENDLOOP.

CLEAR NO.

  LOOP AT RT_KONH INTO WA_KONH WHERE KVEWE = 'A' AND KOTABNR = '832'.
*            CLEAR R_KNUMH_832[].
            R_KNUMH_LINE_832-SIGN   = 'I'.
            R_KNUMH_LINE_832-OPTION = 'EQ'.
            R_KNUMH_LINE_832-LOW    = WA_KONH-KNUMH.
            APPEND R_KNUMH_LINE_832 TO R_KNUMH_832.
            CLEAR: R_KNUMH_LINE_832.

 ENDLOOP.


  DESCRIBE TABLE R_KNUMH_832 LINES NO.

LOOP AT R_KNUMH_832 INTO R_KNUMH_LINE_832.
  IF SY-TABIX = 1.
                R_KNUMH_LINE_832_1-SIGN   = 'I'.
            R_KNUMH_LINE_832_1-OPTION = 'BT'.
            R_KNUMH_LINE_832_1-LOW = R_KNUMH_LINE_832-LOW.
      ENDIF.
        CHECK SY-TABIX = NO.
        R_KNUMH_LINE_832_1-HIGH  = R_KNUMH_LINE_832-LOW.
            APPEND R_KNUMH_LINE_832_1 TO R_KNUMH_832_1.
            CLEAR: R_KNUMH_LINE_832_1.
     ENDLOOP.

CLEAR NO.
  LOOP AT RT_KONH INTO WA_KONH WHERE KVEWE = 'A' AND KOTABNR = '837'.
*            CLEAR R_KNUMH_837[].
            R_KNUMH_LINE_837-SIGN   = 'I'.
            R_KNUMH_LINE_837-OPTION = 'EQ'.
            R_KNUMH_LINE_837-LOW    = WA_KONH-KNUMH.
            APPEND R_KNUMH_LINE_837 TO R_KNUMH_837.
            CLEAR: R_KNUMH_LINE_837.

 ENDLOOP.


  DESCRIBE TABLE R_KNUMH_837 LINES NO.

LOOP AT R_KNUMH_837 INTO R_KNUMH_LINE_837.
  IF SY-TABIX = 1.
                R_KNUMH_LINE_837_1-SIGN   = 'I'.
            R_KNUMH_LINE_837_1-OPTION = 'BT'.
            R_KNUMH_LINE_837_1-LOW = R_KNUMH_LINE_837-LOW.
      ENDIF.
        CHECK SY-TABIX = NO.
        R_KNUMH_LINE_837_1-HIGH  = R_KNUMH_LINE_837-LOW.
            APPEND R_KNUMH_LINE_837_1 TO R_KNUMH_837_1.
            CLEAR: R_KNUMH_LINE_837_1.
     ENDLOOP.


CLEAR NO.
  LOOP AT RT_KONH INTO WA_KONH WHERE KVEWE = 'A' AND KOTABNR = '831'.
*            CLEAR R_KNUMH_831[].
            R_KNUMH_LINE_831-SIGN   = 'I'.
            R_KNUMH_LINE_831-OPTION = 'EQ'.
            R_KNUMH_LINE_831-LOW    = WA_KONH-KNUMH.
            APPEND R_KNUMH_LINE_831 TO R_KNUMH_831.
            CLEAR: R_KNUMH_LINE_831.

 ENDLOOP.


   DESCRIBE TABLE R_KNUMH_831 LINES NO.

LOOP AT R_KNUMH_831 INTO R_KNUMH_LINE_831.
  IF SY-TABIX = 1.
                R_KNUMH_LINE_831_1-SIGN   = 'I'.
            R_KNUMH_LINE_831_1-OPTION = 'BT'.
            R_KNUMH_LINE_831_1-LOW = R_KNUMH_LINE_831-LOW.
      ENDIF.
        CHECK SY-TABIX = NO.
        R_KNUMH_LINE_831_1-HIGH  = R_KNUMH_LINE_831-LOW.
            APPEND R_KNUMH_LINE_831_1 TO R_KNUMH_831_1.
            CLEAR: R_KNUMH_LINE_831_1.
     ENDLOOP.

CLEAR NO.

   LOOP AT RT_KONH INTO WA_KONH WHERE KVEWE = 'A' AND KOTABNR = '833'.
*            CLEAR R_KNUMH_833[].
            R_KNUMH_LINE_833-SIGN   = 'I'.
            R_KNUMH_LINE_833-OPTION = 'EQ'.
            R_KNUMH_LINE_833-LOW    = WA_KONH-KNUMH.
            APPEND R_KNUMH_LINE_833 TO R_KNUMH_833.
            CLEAR: R_KNUMH_LINE_833.

 ENDLOOP.

   DESCRIBE TABLE R_KNUMH_833 LINES NO.

LOOP AT R_KNUMH_833 INTO R_KNUMH_LINE_833.
  IF SY-TABIX = 1.
                R_KNUMH_LINE_833_1-SIGN   = 'I'.
            R_KNUMH_LINE_833_1-OPTION = 'BT'.
            R_KNUMH_LINE_833_1-LOW = R_KNUMH_LINE_833-LOW.
      ENDIF.
        CHECK SY-TABIX = NO.
        R_KNUMH_LINE_833_1-HIGH  = R_KNUMH_LINE_833-LOW.
            APPEND R_KNUMH_LINE_833_1 TO R_KNUMH_833_1.
            CLEAR: R_KNUMH_LINE_833_1.
     ENDLOOP.

CLEAR NO.
   LOOP AT RT_KONH INTO WA_KONH WHERE KVEWE = 'A' AND KOTABNR = '834'.
*            CLEAR R_KNUMH_834[].
            R_KNUMH_LINE_834-SIGN   = 'I'.
            R_KNUMH_LINE_834-OPTION = 'EQ'.
            R_KNUMH_LINE_834-LOW    = WA_KONH-KNUMH.
            APPEND R_KNUMH_LINE_834 TO R_KNUMH_834.
            CLEAR: R_KNUMH_LINE_834.

 ENDLOOP.

   DESCRIBE TABLE R_KNUMH_834 LINES NO.

LOOP AT R_KNUMH_834 INTO R_KNUMH_LINE_834.
  IF SY-TABIX = 1.
                R_KNUMH_LINE_834_1-SIGN   = 'I'.
            R_KNUMH_LINE_834_1-OPTION = 'BT'.
            R_KNUMH_LINE_834_1-LOW = R_KNUMH_LINE_834-LOW.
      ENDIF.
        CHECK SY-TABIX = NO.
        R_KNUMH_LINE_834_1-HIGH  = R_KNUMH_LINE_834-LOW.
            APPEND R_KNUMH_LINE_834_1 TO R_KNUMH_834_1.
            CLEAR: R_KNUMH_LINE_834_1.
     ENDLOOP.

CLEAR NO.
   LOOP AT RT_KONH INTO WA_KONH WHERE KVEWE = 'A' AND KOTABNR = '835'.
*            CLEAR R_KNUMH_835[].
            R_KNUMH_LINE_835-SIGN   = 'I'.
            R_KNUMH_LINE_835-OPTION = 'EQ'.
            R_KNUMH_LINE_835-LOW    = WA_KONH-KNUMH.
            APPEND R_KNUMH_LINE_835 TO R_KNUMH_835.
            CLEAR: R_KNUMH_LINE_835.

 ENDLOOP.


   DESCRIBE TABLE R_KNUMH_835 LINES NO.

LOOP AT R_KNUMH_835 INTO R_KNUMH_LINE_835.
  IF SY-TABIX = 1.
                R_KNUMH_LINE_835_1-SIGN   = 'I'.
            R_KNUMH_LINE_835_1-OPTION = 'BT'.
            R_KNUMH_LINE_835_1-LOW = R_KNUMH_LINE_835-LOW.
      ENDIF.
        CHECK SY-TABIX = NO.
        R_KNUMH_LINE_835_1-HIGH  = R_KNUMH_LINE_835-LOW.
            APPEND R_KNUMH_LINE_835_1 TO R_KNUMH_835_1.
            CLEAR: R_KNUMH_LINE_835_1.
     ENDLOOP.




*&-------------------------------------------------------- discount master (A005, A832, A837)
  IF GTH_MARARCRM  IS NOT INITIAL.
    IF P_KUNNR IS INITIAL AND P_VKBUR IS INITIAL.
      if today ne 'X'.
      PERFORM GET_A832  TABLES R_KSCHL          R_REGIO R_MATNR R_KNUMH_832_1 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A832.
      DELETE GT_A832 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A837  TABLES R_KSCHL          R_VKBUR R_MATNR R_KNUMH_837_1 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A837.
      DELETE GT_A837 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A005  TABLES R_KSCHL R_VKORG  R_KUNNR R_MATNR R_KNUMH_5_1 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A005.
      DELETE GT_A005 WHERE knumh NOT in R_KNUMH.
      else.
              PERFORM GET_A832  TABLES R_KSCHL          R_REGIO R_MATNR R_KNUMH_832 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A832.
      DELETE GT_A832 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A837  TABLES R_KSCHL          R_VKBUR R_MATNR R_KNUMH_837 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A837.
      DELETE GT_A837 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A005  TABLES R_KSCHL R_VKORG  R_KUNNR R_MATNR R_KNUMH_5 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A005.
      DELETE GT_A005 WHERE knumh NOT in R_KNUMH.
      endif.

"""""""""""ADDED BY RAM ON 8/2/15
if today ne 'X'.
      PERFORM GET_A831  TABLES R_KSCHL          R_VKBUR R_MATNR R_KNUMH_831_1 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A831.  "SAELS OFF/MATERIAL
      DELETE GT_A831 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A833  TABLES R_KSCHL          R_KUNNR R_KNUMH_833_1 R_REGIO RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A833.
      DELETE GT_A833 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A834  TABLES R_KSCHL          R_VKBUR R_KUNNR R_KNUMH_834_1 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A834.
      DELETE GT_A834 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A835  TABLES R_KSCHL          R_VKBUR R_KNUMH_835_1 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A835.  "SAELS OFF/MATERIAL
      DELETE GT_A835 WHERE knumh NOT in R_KNUMH.
      else.
              PERFORM GET_A831  TABLES R_KSCHL          R_VKBUR R_MATNR R_KNUMH_831 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A831.  "SAELS OFF/MATERIAL
      DELETE GT_A831 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A833  TABLES R_KSCHL          R_KUNNR R_KNUMH_833 R_REGIO RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A833.
      DELETE GT_A833 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A834  TABLES R_KSCHL          R_VKBUR R_KUNNR R_KNUMH_834 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A834.
      DELETE GT_A834 WHERE knumh NOT in R_KNUMH.
      PERFORM GET_A835  TABLES R_KSCHL          R_VKBUR R_KNUMH_835 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A835.  "SAELS OFF/MATERIAL
      DELETE GT_A835 WHERE knumh NOT in R_KNUMH.
      endif.
""""""""""ENDED BY RAM ON 8/2/15
    ELSEIF P_KUNNR IS NOT INITIAL AND P_VKBUR IS INITIAL.
      if today ne 'X'.
      PERFORM GET_A005  TABLES R_KSCHL R_VKORG  R_KUNNR R_MATNR R_KNUMH_5_1 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A005.
      DELETE GT_A005 WHERE knumh NOT in R_KNUMH.
      else.
      PERFORM GET_A005  TABLES R_KSCHL R_VKORG  R_KUNNR R_MATNR R_KNUMH_5 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A005.
      DELETE GT_A005 WHERE knumh NOT in R_KNUMH.
       endif.
    ELSEIF P_VKBUR IS NOT INITIAL AND P_KUNNR IS INITIAL.
      if today ne 'X'.
      PERFORM GET_A837  TABLES R_KSCHL          R_VKBUR R_MATNR R_KNUMH_837_1 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A837.
      DELETE GT_A837 WHERE knumh NOT in R_KNUMH.
      else.
      PERFORM GET_A837  TABLES R_KSCHL          R_VKBUR R_MATNR R_KNUMH_837 RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A837.
      DELETE GT_A837 WHERE knumh NOT in R_KNUMH.
      endif.
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

    "IF WA_DISC_MASTER-KSCHL = 'Z007' .
    "  LOOP AT IT_DISC_TEMP INTO WA_DISC_TEMP .
*          WA_DISC_MASTER-KNUMH    = WA_DISC_TEMP-KNUMH.
*          WA_DISC_MASTER-KOPOS    = WA_DISC_TEMP-KOPOS.
*          WA_DISC_MASTER-KBETR    = WA_DISC_TEMP-KBETR.
          APPEND WA_DISC_MASTER TO IT_DISC_MASTER.
     "     ENDLOOP.
    "ENDIF.

  ENDLOOP.

  SORT: GTH_MARARCRM       BY MATNR,
        IT_DISC_MASTER    BY MATNR.

 " LOOP AT IT_DISC_MASTER ASSIGNING <FS_DISC>.
*    IF LV_MATNR = <FS_DISC>-MATNR.
*        IF WA_DISC_TEMP-TABLE_NAME  <> 'A834'  AND WA_DISC_TEMP-TABLE_NAME <> 'A835'.
*      DELETE IT_DISC_MASTER.
*          ENDIF.
*    ELSE.
*          IF WA_DISC_TEMP-TABLE_NAME  <> 'A834' AND WA_DISC_TEMP-TABLE_NAME <> 'A835' .
*      READ TABLE GTH_MARARCRM TRANSPORTING NO FIELDS WITH KEY MATNR = <FS_DISC>-MATNR.
*      IF SY-SUBRC <> 0.
*        LV_MATNR = <FS_DISC>-MATNR.
*        DELETE IT_DISC_MASTER.
*        ENDIF.
*      ENDIF.
*    ENDIF.
"  ENDLOOP.
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
