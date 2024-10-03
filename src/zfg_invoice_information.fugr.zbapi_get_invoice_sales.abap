FUNCTION ZBAPI_GET_INVOICE_SALES .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  TVKO-BUKRS
*"     VALUE(P_KUNNR) TYPE  KNA1-KUNNR OPTIONAL
*"     VALUE(P_CANCEL) TYPE  FLAG OPTIONAL
*"     VALUE(P_SALESRET) TYPE  FLAG OPTIONAL
*"  TABLES
*"      S_DATE STRUCTURE  ZSTR_SKU_DATE OPTIONAL
*"      IT_INVOICE_LIST_VBRK STRUCTURE  ZSTR_SKU_VBRK_LIST1 OPTIONAL
*"      IT_INVOICE_LIST_VBRP STRUCTURE  ZSTR_SKU_VBRP_LIST1 OPTIONAL
*"----------------------------------------------------------------------

*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------
*&    AUTHOR          : Govindarajan M
*&    CREATED ON      : 07.07.2015
*&    OBJECTIVE       : THIS FUNCTION MODULE CAN BE USED TO GET INVOICE LIST FOR A GIVEN INPUT
*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------

  TYPES:  BEGIN OF GTY_VBRK,
            VBELN TYPE VBRK-VBELN,
            FKART TYPE VBRK-FKART,
            KNUMV TYPE VBRK-KNUMV ,
            FKDAT TYPE VBRK-FKDAT,
            KDGRP TYPE VBRK-KDGRP,
            BUKRS TYPE VBRK-BUKRS,
            NETWR TYPE VBRK-NETWR,
            ERNAM TYPE VBRK-ERNAM,
            ERZET TYPE VBRK-ERZET,
            ERDAT TYPE VBRK-ERDAT,
            GJAHR TYPE VBRK-GJAHR,
            KUNRG TYPE VBRK-KUNRG,
            KUNAG TYPE VBRK-KUNAG,
            MWSBK TYPE VBRK-MWSBK ,
            FKSTO TYPE VBRK-FKSTO,
            AEDAT TYPE VBRK-AEDAT,
          END OF GTY_VBRK,

          BEGIN OF GTY_VBRP,
            VBELN TYPE VBRK-VBELN,
        "    KNUMV TYPE VBRK-KNUMV ,  "ADDED BY RAM ON 8/7/2015
            POSNR TYPE VBRP-POSNR,
            FKIMG TYPE VBRP-FKIMG,
            VRKME TYPE VBRP-VRKME,
            NETWR TYPE VBRP-NETWR,
            VGBEL TYPE VBRP-VGBEL,
            VGPOS TYPE VBRP-VGPOS,
            VGTYP TYPE VBRP-VGTYP,
            AUBEL TYPE VBRP-AUBEL,
            AUPOS TYPE VBRP-AUPOS,
            MATNR TYPE VBRP-MATNR,
            MWSBP TYPE VBRP-MWSBP,
            ARKTX TYPE VBRP-ARKTX,
            CHARG TYPE VBRP-CHARG,
            WERKS TYPE VBRP-WERKS,
            LGORT TYPE VBRP-LGORT,
          END OF GTY_VBRP,

          BEGIN OF GTY_KNVP,
          KUNNR TYPE KNVP-KUNNR,
          VKORG TYPE KNVP-VKORG,
          VTWEG TYPE KNVP-VTWEG,
          SPART TYPE KNVP-SPART,
          PARVW TYPE KNVP-PARVW,
          PERNR TYPE KNVP-PERNR,
          END OF GTY_KNVP,

         BEGIN OF GTY_VBRP1,
          VBELN TYPE VBRP-VBELN,
          WERKS TYPE VBRP-WERKS,
         END OF GTY_VBRP1,

          BEGIN OF GTY_KONV,
          KNUMV TYPE KONV-KNUMV,
          KPOSN TYPE KONV-KPOSN ,
          KSCHL TYPE KONV-KSCHL ,
          KAWRT TYPE KONV-KAWRT ,
          KBETR TYPE KONV-KBETR ,
          END OF GTY_KONV .


  DATA: GT_VBRK TYPE STANDARD TABLE OF GTY_VBRK,
        GS_VBRK TYPE                     GTY_VBRK,
        GT_VBRP TYPE STANDARD TABLE OF GTY_VBRP,
        GS_VBRP TYPE                     GTY_VBRP,
        GT_KNVP TYPE STANDARD TABLE OF GTY_KNVP,
        GS_KNVP TYPE                     GTY_KNVP,
        GT_VBRP1 TYPE STANDARD TABLE OF GTY_VBRP1,
        GS_VBRP1 TYPE                     GTY_VBRP1,
        GT_KONV TYPE STANDARD TABLE OF GTY_KONV,
        GS_KONV TYPE                     GTY_KONV,
        GT_KONV1 TYPE STANDARD TABLE OF GTY_KONV,
        GS_KONV1 TYPE                     GTY_KONV,
        GS_INVOICE_LIST_VBRK LIKE IT_INVOICE_LIST_VBRK,
        GS_INVOICE_LIST_VBRP LIKE IT_INVOICE_LIST_VBRP .

  IF P_CANCEL <> 'X' AND P_SALESRET <> 'X'. " Added By Govind On 10-06-2015 for Cancel Invoice Split
    SELECT VBELN "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
           FKART
           KNUMV
           FKDAT
           KDGRP
           BUKRS
           NETWR
           ERNAM
           ERZET
           ERDAT
           GJAHR
           KUNRG
           KUNAG
           MWSBK
           FKSTO
           AEDAT
      INTO TABLE GT_VBRK
      FROM VBRK
      WHERE BUKRS = P_BUKRS
      AND   FKART = 'YBBR'
      AND  ( KDGRP = '09' OR KDGRP = '02')
      AND   ERDAT IN S_DATE.



*    SELECT
*     KNUMV
*     KPOSN
*     KSCHL
*     KAWRT
*     KBETR FROM KONV INTO TABLE GT_KONV FOR ALL ENTRIES IN GT_VBRK WHERE KNUMV = GT_VBRK-KNUMV AND ( KSCHL = 'Y004' OR  KSCHL = 'YBSD' OR KSCHL = 'Y007'
*                                                                                                     OR KSCHL = 'Y029' OR KSCHL = 'YBRD' OR KSCHL = 'YBAD' OR KSCHL = 'ZPRB' ) .
  ENDIF.

  IF P_CANCEL = 'X'. " Added By Govind On 10-06-2015 for Cancel Invoice Split
    SELECT VBELN "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
        FKART
        KNUMV
        FKDAT
        KDGRP
        BUKRS
        NETWR
        ERNAM
        ERZET
        ERDAT
        GJAHR
        KUNRG
        KUNAG
        MWSBK
        FKSTO
        AEDAT
   INTO TABLE GT_VBRK
   FROM VBRK
   WHERE BUKRS = P_BUKRS
   AND   FKART = 'YBBR'
   AND   ( KDGRP = '09' OR KDGRP = '02')
   AND FKSTO = 'X'
   AND   AEDAT IN S_DATE.

  ENDIF.

  IF P_SALESRET = 'X'.
    SELECT VBELN "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
         FKART
         KNUMV
         FKDAT
         KDGRP
         BUKRS
         NETWR
         ERNAM
         ERZET
         ERDAT
         GJAHR
         KUNRG
         KUNAG
         MWSBK
         FKSTO
         AEDAT
    INTO TABLE GT_VBRK
    FROM VBRK
    WHERE BUKRS = P_BUKRS
    AND   FKART = 'YBRE'
    AND   ( KDGRP = '09' OR KDGRP = '02')
     AND   AEDAT IN S_DATE.
  ENDIF.


SELECT
      KUNNR
      VKORG
      VTWEG
      SPART
      PARVW
      PERNR FROM KNVP INTO TABLE GT_KNVP FOR ALL ENTRIES IN GT_VBRK WHERE KUNNR = GT_VBRK-KUNRG AND VKORG = GT_VBRK-BUKRS .

  SELECT
    VBELN
    WERKS
      FROM VBRP INTO TABLE GT_VBRP1 FOR ALL ENTRIES IN GT_VBRK WHERE VBELN = GT_VBRK-VBELN .

  LOOP AT GT_VBRK INTO GS_VBRK.
    GS_INVOICE_LIST_VBRK-VBELN = GS_VBRK-VBELN.
    GS_INVOICE_LIST_VBRK-FKART = GS_VBRK-FKART.
    GS_INVOICE_LIST_VBRK-KNUMV = GS_VBRK-KNUMV .
    GS_INVOICE_LIST_VBRK-FKDAT = GS_VBRK-FKDAT.
    GS_INVOICE_LIST_VBRK-KDGRP = GS_VBRK-KDGRP.
    GS_INVOICE_LIST_VBRK-NETWR = GS_VBRK-NETWR.
    GS_INVOICE_LIST_VBRK-ERNAM = GS_VBRK-ERNAM.
    GS_INVOICE_LIST_VBRK-ERZET = GS_VBRK-ERZET.
    GS_INVOICE_LIST_VBRK-ERDAT = GS_VBRK-ERDAT.
    GS_INVOICE_LIST_VBRk-GJAHR = GS_VBRK-GJAHR.
    GS_INVOICE_LIST_VBRK-KUNRG = GS_VBRK-KUNRG.
    GS_INVOICE_LIST_VBRK-KUNAG = GS_VBRK-KUNAG.
    GS_INVOICE_LIST_VBRK-MWSBK = GS_VBRK-MWSBK.
    GS_INVOICE_LIST_VBRK-FKSTO = GS_VBRK-FKSTO.

    READ TABLE GT_VBRP1 INTO GS_VBRP1 WITH KEY VBELN = GS_VBRK-VBELN.
    IF SY-SUBRC = 0 .
      GS_INVOICE_LIST_VBRK-WERKS = GS_VBRP1-WERKS.
      ENDIF.

    READ TABLE GT_KNVP INTO GS_KNVP WITH KEY KUNNR = GS_VBRK-KUNRG PARVW = 'L5'.
    IF SY-SUBRC = 0.
      GS_INVOICE_LIST_VBRK-PERNR = GS_KNVP-PERNR.
    ENDIF.
    READ TABLE GT_KNVP INTO GS_KNVP WITH KEY KUNNR = GS_VBRK-KUNRG PARVW = 'L3' .
    IF SY-SUBRC = 0 .
      GS_INVOICE_LIST_VBRK-PERNR1 = GS_KNVP-PERNR .
    ENDIF .

    LOOP AT GT_KONV INTO GS_KONV WHERE KNUMV = GS_VBRK-KNUMV .

      GS_INVOICE_LIST_VBRK-TOTDIS = GS_INVOICE_LIST_VBRK-TOTDIS + GS_KONV-KBETR .

    ENDLOOP.

    APPEND GS_INVOICE_LIST_VBRK TO IT_INVOICE_LIST_VBRK.
    CLEAR GS_INVOICE_LIST_VBRK .
  ENDLOOP.

  IF SY-SUBRC = 0.
    IF NOT P_KUNNR IS INITIAL.
      DELETE IT_INVOICE_LIST_VBRK WHERE KUNRG <> P_KUNNR.
    ENDIF.

    CHECK IT_INVOICE_LIST_VBRK[] IS NOT INITIAL.
    SELECT  VBELN
            POSNR
            FKIMG
            VRKME
            NETWR
            VGBEL
            VGPOS
            VGTYP
            AUBEL
            AUPOS
            MATNR
            MWSBP
            ARKTX
            CHARG
            WERKS
            LGORT
      FROM  VBRP
      INTO TABLE IT_INVOICE_LIST_VBRP
      FOR ALL ENTRIES IN IT_INVOICE_LIST_VBRK
      WHERE VBELN = IT_INVOICE_LIST_VBRK-VBELN.
  ENDIF.


*  SELECT
*     KNUMV
*     KPOSN
*     KSCHL
*     KAWRT
*     KBETR FROM KONV INTO TABLE GT_KONV1 FOR ALL ENTRIES IN GT_VBRK WHERE KNUMV = GT_VBRK-KNUMV AND ( KSCHL = 'Y004' OR  KSCHL = 'YBSD' OR KSCHL = 'Y007'
*
*                                                                                            OR KSCHL = 'Y029' OR KSCHL = 'YBRD' OR KSCHL = 'YBAD' OR KSCHL = 'ZPRB' ) .
*
*  SHIFT IT_INVOICE_LIST_VBRP-POSNR LEFT DELETING LEADING '0'.

*  LOOP AT GT_KONV1 INTO GS_KONV1 WHERE KNUMV = IT_INVOICE_LIST_VBRK-KNUMV AND KPOSN = IT_INVOICE_LIST_VBRP-POSNR .
*
*    GS_INVOICE_LIST_VBRP-TOTD1 = GS_INVOICE_LIST_VBRP-TOTD1 + GS_KONV-KBETR .
*
*  ENDLOOP.

ENDFUNCTION.
