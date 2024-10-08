*&---------------------------------------------------------------------*
*& Report  ZDELIVERY_DETAILS_AUTOMAIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZDELIVERY_DETAILS_AUTOMAIL.

*TABLES  : VBAK,
*          VBAP,
*          VBAKUK,
*          ZCUSTMAT_ROL,
*          T001W..

DATA  : S_REPID LIKE SY-REPID.

TYPES : BEGIN OF TY_YMARD_NSAP,
    KUNNR TYPE YMARD_NSAP-KUNNR,
    MATNR TYPE YMARD_NSAP-MATNR,
    ERDAT TYPE YMARD_NSAP-ERDAT,
    ERZET TYPE YMARD_NSAP-ERZET,
    MEINS TYPE YMARD_NSAP-MEINS,
    LABST TYPE YMARD_NSAP-LABST,
    TRAME TYPE YMARD_NSAP-TRAME,
    SKU_STK TYPE YMARD_NSAP-TOT_STK,
    OPEN_ORD TYPE YMARD_NSAP-PEN_ORD,
  END OF TY_YMARD_NSAP.

DATA : IT_YMARD_NSAP TYPE TABLE OF TY_YMARD_NSAP,
      WA_YMARD_NSAP TYPE TY_YMARD_NSAP.

TYPES : BEGIN OF TY_VBAP,
  VBELN TYPE VBAP-VBELN,
  POSNR TYPE VBAP-POSNR,
  MATNR TYPE VBRP-MATNR,
  ARKTX TYPE VBRP-ARKTX,
  NETWR TYPE VBRP-NETWR,
  KWMENG TYPE VBAP-KWMENG,
  WERKS TYPE VBAP-WERKS,
  END OF TY_VBAP.

DATA  : IT_VBAP TYPE TABLE OF TY_VBAP,
        WA_VBAP TYPE TY_VBAP.

TYPES : BEGIN OF TY_VBAK,
  VBELN TYPE VBAKUK-VBELN,    "#EC CI_USAGE_OK[2198647] " Sale Order NoAdded by <IT-CAR Tool> during Code Remediation
  AUDAT TYPE VBAKUK-AUDAT,    "#EC CI_USAGE_OK[2198647] " DateAdded by <IT-CAR Tool> during Code Remediation
  AUART TYPE VBAKUK-AUART,    "#EC CI_USAGE_OK[2198647] " Sales Order TypeAdded by <IT-CAR Tool> during Code Remediation
  NETWR TYPE VBAKUK-NETWR,    "#EC CI_USAGE_OK[2198647] " NetValueAdded by <IT-CAR Tool> during Code Remediation
  VKBUR TYPE VBAKUK-VKBUR,    "#EC CI_USAGE_OK[2198647] " Sales OfficeAdded by <IT-CAR Tool> during Code Remediation
  BSTNK TYPE VBAKUK-BSTNK,    "#EC CI_USAGE_OK[2198647] " PO numberAdded by <IT-CAR Tool> during Code Remediation
  KUNNR TYPE VBAKUK-KUNNR,    "#EC CI_USAGE_OK[2198647] " Customer NumberAdded by <IT-CAR Tool> during Code Remediation
  END OF TY_VBAK.

DATA  : IT_VBAK TYPE TABLE OF TY_VBAK,
      WA_VBAK TYPE TY_VBAK.

DATA  : IT_VBAK1 TYPE TABLE OF TY_VBAK,
      WA_VBAK1 TYPE TY_VBAK.

TYPES : BEGIN OF TY_LIPS,
    VBELN TYPE LIPS-VBELN,
    POSNR TYPE LIPS-POSNR,
    ERDAT TYPE LIPS-ERDAT,
    VGBEL TYPE LIPS-VGBEL,
    VGPOS TYPE LIPS-VGPOS,
    END OF TY_LIPS.

DATA  : IT_LIPS TYPE TABLE OF TY_LIPS,
      WA_LIPS TYPE TY_LIPS.

TYPES : BEGIN OF TY_ROL,
    KUNNR TYPE ZCUSTMAT_ROL-KUNNR,
    MATNR TYPE ZCUSTMAT_ROL-MATNR,
    NEW_VAL TYPE ZCUSTMAT_ROL-NEW_VAL,
    NEW_DAT TYPE ZCUSTMAT_ROL-NEW_DAT,
    END OF TY_ROL.

DATA  : IT_ROL TYPE TABLE OF TY_ROL,
      WA_ROL TYPE TY_ROL.

TYPES : BEGIN OF TY_VBRP,
    VBELN TYPE VBRP-VBELN,
    POSNR TYPE VBRP-POSNR,
    FKIMG TYPE VBRP-FKIMG,
    NETWR TYPE VBRP-NETWR,
    AUBEL TYPE VBRP-AUBEL,
    AUPOS TYPE VBRP-AUPOS,
    END OF TY_VBRP.

DATA  : IT_VBRP TYPE TABLE OF TY_VBRP,
      WA_VBRP TYPE TY_VBRP.

TYPES : BEGIN OF TY_VBFA,
    VBELV TYPE VBFA-VBELV,
    POSNV TYPE VBFA-POSNV,
    VBELN TYPE VBFA-VBELN,
    POSNN TYPE VBFA-POSNN,
    VBTYP_N TYPE VBFA-VBTYP_N,
    END OF TY_VBFA.

DATA  : IT_VBFA TYPE TABLE OF TY_VBFA,
      WA_VBFA TYPE TY_VBFA.

TYPES : BEGIN OF TY_VBRK,
    VBELN TYPE VBRK-VBELN,
    FKART TYPE VBRK-FKART,
    FKDAT TYPE VBRK-FKDAT,
    FKSTO TYPE VBRK-FKSTO,
    DATE_OF_DELIVERY TYPE VBRK-DATE_OF_DELIVERY,
    REMARKS TYPE VBRK-REMARKS,
    END OF TY_VBRK.

DATA  : IT_VBRK TYPE TABLE OF TY_VBRK,
        WA_VBRK TYPE TY_VBRK.

TYPES : BEGIN OF TY_KNA1,
    KUNNR TYPE KNA1-KUNNR,
    LAND1 TYPE KNA1-LAND1,
    NAME1 TYPE KNA1-NAME1,
    REGIO TYPE KNA1-REGIO,
    END OF TY_KNA1.

DATA  : IT_KNA1 TYPE TABLE OF TY_KNA1,
      WA_KNA1 TYPE TY_KNA1.

TYPES : BEGIN OF TY_T005U,
    SPRAS TYPE T005U-SPRAS,
    LAND1 TYPE T005U-LAND1,
    BLAND TYPE T005U-BLAND,
    BEZEI TYPE T005U-BEZEI,
    END OF TY_T005U.

DATA  : IT_T005U TYPE TABLE OF TY_T005U,
      WA_T005U TYPE TY_T005U.

TYPES : BEGIN OF TY_T001W,
    WERKS TYPE T001W-WERKS,
    NAME1 TYPE T001W-NAME1,
    END OF TY_T001W.

DATA  : IT_T001W TYPE TABLE OF TY_T001W,
      WA_T001W TYPE TY_T001W.

TYPES : BEGIN OF TY_FINAL,
    VBELN TYPE VBAKUK-VBELN,  "#EC CI_USAGE_OK[2198647] " SALES ORDER NUMBERAdded by <IT-CAR Tool> during Code Remediation
    ITEM  TYPE VBAP-POSNR, " SALE ORDER ITEM NO
    AUDAT TYPE VBAKUK-AUDAT,  "#EC CI_USAGE_OK[2198647] " ORDER DATEAdded by <IT-CAR Tool> during Code Remediation
    NETWR TYPE VBAKUK-NETWR,  "#EC CI_USAGE_OK[2198647] " NetValueAdded by <IT-CAR Tool> during Code Remediation
    BSTNK TYPE VBAKUK-BSTNK,  "#EC CI_USAGE_OK[2198647] " PO numberAdded by <IT-CAR Tool> during Code Remediation
    VBELN1 TYPE VBRK-VBELN," INVOICE NUMBER
    FKDAT TYPE VBRK-FKDAT, " INVOICE DATE
    KUNNR TYPE VBAKUK-KUNNR,  "#EC CI_USAGE_OK[2198647] " CUSTOMER CODEAdded by <IT-CAR Tool> during Code Remediation
    NAME1 TYPE KNA1-NAME1, " CUSTOMER NAME
    BEZEI TYPE T005U-BEZEI," REGION
    WERKS TYPE VBAP-WERKS, " SALES OFFICE
    MATNR TYPE VBAP-MATNR, " MATERIAL CODE
    ARKTX TYPE VBAP-ARKTX, " MATERIAL DESCRIPTION
    KWMENG TYPE VBAP-KWMENG, " SALE ORDER QUANTITY
    FKIMG TYPE VBRP-FKIMG,   " BILLED QUANTITY
    AUBEL TYPE VBRP-AUBEL, "Sales Document ref
    AUPOS TYPE VBRP-AUPOS, "Sales Document item no ref
    BAL_Q TYPE STRING,   " BALANCE QUANTITY
    BAL_A TYPE STRING,   " BALANCE AMOUNT
    INV_Q TYPE STRING,   " INVOICE QUANTITY
    INV_A TYPE STRING,   " INVOICE AMOUNT
    SAL_Q TYPE STRING,   " SALE ORDER QUANTITY
    SAL_A TYPE STRING,   " SALE ORDER AMOUNT
    D_O_D TYPE VBRK-DATE_OF_DELIVERY, " DATE OF DELIVERY
    REMARKS TYPE VBRK-REMARKS, " REMARKS
    DAYS TYPE CHAR5, " NO OF DAYS
    BTDD TYPE CHAR5, " Billed TO Delivery Date.
    OTDD TYPE CHAR5, " Order To Delivery Date.
    NEW_VAL TYPE STRING, "New_ROL.
    SKU_STK TYPE STRING, "SKUSTOCK
    OPEN_ORD TYPE STRING, "SKUOPENORDER
    END OF TY_FINAL.

DATA  : IT_FINAL TYPE TABLE OF TY_FINAL,
        WA_FINAL TYPE TY_FINAL.

DATA  : IT_FINAL1 TYPE TABLE OF TY_FINAL,
        WA_FINAL1 TYPE TY_FINAL.

DATA  : SAL_OFC TYPE CHAR40.

DATA  : G_TAB_LINES  TYPE I.

DATA  : TEM_QUN TYPE VBRP-FKIMG.
DATA  : TEM_QUN1 TYPE VBRP-FKIMG.
DATA  : G_SENT_TO_ALL TYPE SONV-FLAG.

DATA  : DAYS TYPE I.
DATA  : BTDD TYPE I.
DATA : OTDD TYPE I .

DATA  : I TYPE CHAR5 ,
        J TYPE CHAR5 .

DATA  : BAL_QUN TYPE VBRP-FKIMG,
        BAL_AMT TYPE VBAP-NETWR.

DATA  : NDNI TYPE CHAR100,
        DNI  TYPE CHAR50,
        COUNT TYPE CHAR250.

DATA  : CON_WERKS TYPE CHAR50,
        CON_AUDAT TYPE CHAR50,
        CON_KUNNR TYPE CHAR50.

DATA  : LOW_DT TYPE CHAR10,
        HIG_DT TYPE CHAR10.

DATA  : POS TYPE I VALUE 0 .

DATA  : FLAG TYPE C.

DATA  : IT_HEADER TYPE SLIS_T_LISTHEADER,
        WA_HEADER TYPE SLIS_LISTHEADER.

DATA  : WA_LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA  : AUDAT_LOW TYPE VBAK-AUDAT,
        AUDAT_HIG TYPE VBAK-AUDAT.


DATA : TO_DATE TYPE VBAK-AUDAT.
TO_DATE = SY-DATUM.
DATA  : MON TYPE I.


DATA : LV_STRING TYPE STRING, "declare string
       LV_DATA_STRING TYPE STRING. "declare string

DATA: LIT_BINARY_CONTENT TYPE SOLIX_TAB.
DATA: L_ATTSUBJECT   TYPE SOOD-OBJDES.

DATA  : IT_ATTACHMENT TYPE STANDARD TABLE OF SOLISTI1,
        WA_ATTACHMENT LIKE LINE OF IT_ATTACHMENT.

START-OF-SELECTION.

  PERFORM GET_DAT.
  PERFORM READ_DATA.
  PERFORM BUILD_XLS_DATA_TABLE_DETAIL.


END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  GET_DAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DAT .

  SELECT   VBELN
           AUDAT
           AUART
           NETWR
           VKBUR
           BSTNK
           KUNNR FROM VBAKUK INTO TABLE IT_VBAK WHERE "#EC CI_USAGE_OK[2198647] " Added by <IT-CAR Tool> during Code Remediation
           VKBUR <> ' ' AND VKORG = '1000'
           AND ( AUART = 'YBBR' OR AUART = 'YBDP' ) AND BESTK <> ' ' AND GBSTK <> 'C' AND ERDAT >= '20200501'.
  IF IT_VBAK IS NOT INITIAL.

    SELECT
      KUNNR
      MATNR
      ERDAT
      ERZET
      MEINS
      LABST
      TRAME
      TOT_STK
      PEN_ORD
      FROM YMARD_NSAP INTO TABLE IT_YMARD_NSAP FOR ALL ENTRIES IN IT_VBAK WHERE KUNNR = IT_VBAK-KUNNR AND ERDAT = TO_DATE.

    SELECT
      KUNNR
      MATNR
      NEW_VAL
      NEW_DAT
      FROM ZCUSTMAT_ROL INTO TABLE IT_ROL FOR ALL ENTRIES IN IT_VBAK WHERE KUNNR = IT_VBAK-KUNNR. "and New_dat = SY-DATUM.

    SELECT VBELN
           POSNR
           MATNR
           ARKTX
           NETWR
           KWMENG
           WERKS FROM VBAP INTO TABLE IT_VBAP FOR ALL ENTRIES IN IT_VBAK WHERE VBELN = IT_VBAK-VBELN.
    IF IT_VBAP IS NOT INITIAL.


      SELECT  VBELV
              POSNV
              VBELN
              POSNN
              VBTYP_N  FROM VBFA INTO TABLE IT_VBFA FOR ALL ENTRIES IN IT_VBAP WHERE VBELV = IT_VBAP-VBELN AND POSNV = IT_VBAP-POSNR AND VBTYP_N = 'M' ." AND POSNN = 1 .
*          IF IT_VBFA IS NOT INITIAL.
      SELECT VBELN
           FKART
           FKDAT
           FKSTO
           DATE_OF_DELIVERY
           REMARKS FROM VBRK INTO TABLE IT_VBRK FOR ALL ENTRIES IN IT_VBFA WHERE VBELN = IT_VBFA-VBELN  AND FKSTO <> 'X'.

      SELECT  VBELN
              POSNR
              FKIMG
              NETWR
              AUBEL
              AUPOS FROM VBRP INTO TABLE IT_VBRP FOR ALL ENTRIES IN IT_VBRK WHERE VBELN = IT_VBRK-VBELN.


      SELECT KUNNR
             LAND1
             NAME1
             REGIO FROM KNA1 INTO TABLE IT_KNA1 FOR ALL ENTRIES IN IT_VBAK WHERE KUNNR = IT_VBAK-KUNNR.

      SELECT SPRAS
             LAND1
             BLAND
             BEZEI FROM T005U INTO TABLE IT_T005U FOR ALL ENTRIES IN IT_KNA1 WHERE SPRAS = 'EN' AND LAND1 = IT_KNA1-LAND1 AND BLAND = IT_KNA1-REGIO.
*
*            SELECT WERKS
*                   NAME1 FROM T001W INTO TABLE IT_T001W WHERE WERKS = IT_VBAK-VKBUR.
*          ELSE.
*            MESSAGE 'No Data Found' TYPE 'I' DISPLAY LIKE 'E'.
*            SUBMIT ZDELIVERY_DETAILS VIA SELECTION-SCREEN .
*          ENDIF.
    ELSE.
      MESSAGE 'No Data Found' TYPE 'I' DISPLAY LIKE 'E'.
*      SUBMIT ZDELIVERY_DETAILS VIA SELECTION-SCREEN .
    ENDIF.
  ELSE.
    MESSAGE 'No Data Found' TYPE 'I' DISPLAY LIKE 'E'.
*    SUBMIT ZDELIVERY_DETAILS VIA SELECTION-SCREEN .
  ENDIF.
*    ELSE.
*      MESSAGE 'Please Enter Lessthen One Year Date' TYPE 'I' DISPLAY LIKE 'E'.
*      SUBMIT ZDELIVERY_DETAILS VIA SELECTION-SCREEN .
*    ENDIF.
*  ELSE.
*    MESSAGE 'Please Enter Sales Office And Date Fields' TYPE 'I' DISPLAY LIKE 'E'.
*    SUBMIT ZDELIVERY_DETAILS VIA SELECTION-SCREEN .
*  ENDIF.

ENDFORM.                    " GET_DAT
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA .

  LOOP AT IT_VBAP  INTO WA_VBAP.
    READ TABLE IT_VBAK INTO WA_VBAK WITH KEY VBELN = WA_VBAP-VBELN.
    READ TABLE IT_VBFA INTO WA_VBFA WITH KEY VBELV = WA_VBAP-VBELN POSNV = WA_VBAP-POSNR .
    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBAK-KUNNR.
    READ TABLE IT_T005U INTO WA_T005U WITH KEY LAND1 = WA_KNA1-LAND1 BLAND = WA_KNA1-REGIO .
    READ TABLE IT_YMARD_NSAP INTO WA_YMARD_NSAP WITH KEY KUNNR = WA_VBAK-KUNNR MATNR = WA_VBAP-MATNR.


    WA_FINAL-NAME1 = WA_KNA1-NAME1.
    WA_FINAL-BEZEI = WA_T005U-BEZEI.

    WA_FINAL-VBELN = WA_VBAK-VBELN.
    WA_FINAL-AUDAT = WA_VBAK-AUDAT.
    WA_FINAL-BSTNK = WA_VBAK-BSTNK.
    WA_FINAL-WERKS = WA_VBAK-VKBUR.
    WA_FINAL-KUNNR = WA_VBAK-KUNNR.

    WA_FINAL-NETWR = WA_VBAP-NETWR .
    WA_FINAL-KWMENG = WA_VBAP-KWMENG. "Sale Order Quantity.
    WA_FINAL-SAL_Q = WA_VBAP-KWMENG .
    WA_FINAL-SAL_A = WA_VBAP-NETWR .
    WA_FINAL-ITEM = WA_VBAP-POSNR .
    WA_FINAL-MATNR = WA_VBAP-MATNR.
    WA_FINAL-ARKTX = WA_VBAP-ARKTX.

    WA_FINAL-BAL_Q = WA_VBAP-KWMENG.
    WA_FINAL-BAL_A = WA_VBAP-NETWR.

    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        I_DATE_FROM    = WA_VBAK-AUDAT
*       I_KEY_DAY_FROM =
        I_DATE_TO      = SY-DATUM
*       I_KEY_DAY_TO   =
*       I_FLG_SEPARATE = ' '
      IMPORTING
        E_DAYS         = OTDD
*       E_MONTHS       =
*       E_YEARS        =
      .

    WA_FINAL-OTDD =  OTDD .

    LOOP AT IT_VBRP INTO WA_VBRP WHERE AUBEL EQ WA_VBAP-VBELN AND AUPOS EQ WA_VBAP-POSNR .

      READ TABLE IT_VBRK INTO WA_VBRK WITH KEY VBELN = WA_VBRP-VBELN .
      WA_FINAL-VBELN1 = WA_VBRK-VBELN.
      WA_FINAL-FKDAT = WA_VBRK-FKDAT.
      WA_FINAL-D_O_D = WA_VBRK-DATE_OF_DELIVERY.
      WA_FINAL-REMARKS = WA_VBRK-REMARKS.

      WA_FINAL-INV_Q = WA_VBRP-FKIMG + WA_FINAL-INV_Q .
      WA_FINAL-INV_A = WA_VBRP-NETWR + WA_FINAL-INV_A .
      WA_FINAL-MATNR = WA_VBAP-MATNR.
      WA_FINAL-ARKTX = WA_VBAP-ARKTX.

    ENDLOOP.

    IF WA_FINAL-VBELN1 IS NOT INITIAL.
      BAL_QUN = WA_FINAL-SAL_Q - WA_FINAL-INV_Q .
*            BAL_AMT = WA_FINAL-SAL_A - WA_FINAL-INV_A .
      BAL_AMT = WA_FINAL-NETWR / WA_FINAL-KWMENG .
      WA_FINAL-BAL_Q = BAL_QUN .

      IF BAL_AMT <> 0.
        WA_FINAL-BAL_A = BAL_AMT * BAL_QUN.
      ELSE .
        WA_FINAL-BAL_A = '-' .
      ENDIF.

    ENDIF.

    READ TABLE IT_ROL INTO WA_ROL WITH KEY KUNNR = WA_VBAK-KUNNR MATNR = WA_VBAP-MATNR .
    IF WA_VBAP-MATNR = WA_ROL-MATNR.
      WA_FINAL-NEW_VAL = WA_ROL-NEW_VAL.
    ELSE.
      WA_FINAL-NEW_VAL = 0.
    ENDIF.

    IF WA_VBAP-MATNR = WA_YMARD_NSAP-MATNR.
      WA_FINAL-SKU_STK = WA_YMARD_NSAP-SKU_STK.
      WA_FINAL-OPEN_ORD = WA_YMARD_NSAP-OPEN_ORD.
    ELSE.
      WA_FINAL-SKU_STK = 0.
      WA_FINAL-OPEN_ORD = 0.
    ENDIF.


    APPEND: WA_FINAL TO IT_FINAL .
    CLEAR: WA_VBFA  , WA_VBAK , WA_FINAL , WA_KNA1 , WA_T005U , FLAG  , WA_VBRP , WA_VBRK , WA_ROL ,WA_YMARD_NSAP .
  ENDLOOP.

  SORT IT_FINAL BY VBELN ITEM .

  IF IT_FINAL IS INITIAL.
    MESSAGE 'No Data Found' TYPE 'I' DISPLAY LIKE 'E'.
*    SUBMIT ZDELIVERY_DETAILS VIA SELECTION-SCREEN .
  ENDIF.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_XLS_DATA_TABLE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_XLS_DATA_TABLE_DETAIL .

  DATA: STR1 TYPE CHAR50,
        STR2 TYPE CHAR8,
        STR3 TYPE STRING,
        STR4 TYPE STRING,
        DAT1 TYPE CHAR4,
        DAT2 TYPE CHAR2,
        DAT3 TYPE CHAR2.

  STR1 = 'Pending Order For Dispatch_'.
  STR2 = SY-DATUM.
  STR4 = '/'.
  DAT3 = STR2+0(4).
  DAT2 = STR2+4(2).
  DAT1 = STR2+6(2).

  CONCATENATE STR1 DAT1 STR4 DAT2 STR4 DAT3 INTO STR3.


  DATA:  LO_SEND_REQUEST TYPE REF TO CL_BCS VALUE IS INITIAL.
  CLASS CL_BCS DEFINITION LOAD.
  LO_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).

* Message body and subject
  DATA: LO_DOCUMENT TYPE REF TO CL_DOCUMENT_BCS VALUE IS INITIAL. "document object
  DATA : I_TEXT TYPE BCSY_TEXT. "Table for body
  DATA : W_TEXT LIKE LINE OF I_TEXT. "work area for message body
*Set body
  W_TEXT-LINE = 'Dear Sir/Madam,'.
  APPEND W_TEXT TO I_TEXT.
  CLEAR W_TEXT.
  W_TEXT-LINE = ''.
  APPEND W_TEXT TO I_TEXT.
  CLEAR W_TEXT.
  W_TEXT-LINE = ''.
  APPEND W_TEXT TO I_TEXT.
  CLEAR W_TEXT.
  W_TEXT-LINE = 'Please find the attachment of Open Sales order details at excel'.
  APPEND W_TEXT TO I_TEXT.
  CLEAR W_TEXT.
  W_TEXT-LINE = 'Regards&Thank'.
  APPEND W_TEXT TO I_TEXT.
  CLEAR W_TEXT.

  DATA : S_SUJDATA TYPE STRING.

*    CONCATENATE 'Pending Order For Dispatch_' SY-DATUM INTO S_SUJDATA.
*Create Email document
  LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT( "create document
  I_TYPE = 'TXT' "Type of document HTM, TXT etc
  I_TEXT =  I_TEXT "email body internal table
  I_SUBJECT = 'Pending Order For Dispatch' ). "email subject here p_sub input parameter

* Pass the document to send request
  LO_SEND_REQUEST->SET_DOCUMENT( LO_DOCUMENT ).

  CLEAR : LV_STRING.
  CLASS CL_ABAP_CHAR_UTILITIES DEFINITION LOAD.
*  CONSTANTS:
*  CON_TAB  TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,
*  CON_CRET TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>CR_LF.

  CONCATENATE   'Sales Office'
                'Doc Date'
                'Customer Name'
                'Region'
                'Material Desc'
                'Sale Order Qty'
                'Sale Order Val'
                'Order vs POD'
                'Sales Doc'
                'Customer No'
                'Item'
                'Material No'
                'Order From'
*                'Inv Doc'
*                'Inv Date'
                'Inv Qty'
*                'Inv Value'
                'SKU ROL'
                'DMS STOCK'
                'DMS Open Order'
                'Remaining Qty'
                'Remaining Value'
*                'Date Of Delivery'
*                'Remarks'
*                'Invoice vs POD'
CL_ABAP_CHAR_UTILITIES=>NEWLINE INTO LV_STRING SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

  LOOP AT IT_FINAL INTO  WA_FINAL.

    DATA: DSTR1 TYPE CHAR8,
       DSTR2 TYPE CHAR2,
       DSTR3 TYPE CHAR2,
       DSTR4 TYPE CHAR4,
       DSTR5 TYPE CHAR1,
       DATESTR6 TYPE CHAR10.

    DSTR1 = WA_FINAL-AUDAT.
    DSTR4 = DSTR1+0(4).
    DSTR3 = DSTR1+4(2).
    DSTR2 = DSTR1+6(2).
    DSTR5 = '/'.



    CONCATENATE DSTR2 DSTR5 DSTR3 DSTR5 DSTR4  INTO DATESTR6.

    CONCATENATE LV_STRING
                 WA_FINAL-WERKS CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 DATESTR6 CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-NAME1 CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-BEZEI CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-ARKTX CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-SAL_Q CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-SAL_A CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-OTDD CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-VBELN CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-KUNNR CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-ITEM CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-MATNR CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-BSTNK CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
*                 WA_FINAL-VBELN1 CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
*                 WA_FINAL-FKDAT CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-INV_Q CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
*                 WA_FINAL-INV_A CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-NEW_VAL CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-SKU_STK CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-OPEN_ORD CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-BAL_Q CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                 WA_FINAL-BAL_A CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
*                 WA_FINAL-D_O_D
*                 WA_FINAL-REMARKS
*                 WA_FINAL-BTDD
 CL_ABAP_CHAR_UTILITIES=>NEWLINE INTO LV_STRING.

  ENDLOOP.




  DATA LV_XSTRING TYPE XSTRING .
***Convert string to xstring
*  CALL FUNCTION 'HR_KR_STRING_TO_XSTRING'
*    EXPORTING
**     codepage_to      = '8300'
*      UNICODE_STRING   = LV_STRING
**     OUT_LEN          =
*    IMPORTING
*      XSTRING_STREAM   = LV_XSTRING
*    EXCEPTIONS
*      INVALID_CODEPAGE = 1
*      INVALID_STRING   = 2
*      OTHERS           = 3.
*  IF SY-SUBRC <> 0.
*    IF SY-SUBRC = 1 .
*
*    ELSEIF SY-SUBRC = 2 .
*      WRITE:/ 'invalid string ' .
*    ENDIF.
*  ENDIF.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      TEXT     = LV_STRING
*     mimetype =                   ld_mimetype
*     encoding =                   ld_encoding
    IMPORTING
      BUFFER   = LV_XSTRING
    EXCEPTIONS
      FAILED   = 1
    .  "  SCMS_STRING_TO_XSTRING

  IF SY-SUBRC EQ 0.
    "All OK
  ELSEIF SY-SUBRC EQ 1. "Exception
    "Add code for exception here
  ENDIF.

  DATA: L_ZIPPER TYPE REF TO CL_ABAP_ZIP. " Zip class declerration
  DATA : L_DATA TYPE STRING.


***Xstring to binary
  CREATE OBJECT L_ZIPPER.
  "add file to zip
  CALL METHOD L_ZIPPER->ADD
    EXPORTING
      NAME    = 'Pending Order For Dispatch.xls'"filename
      CONTENT = LV_XSTRING.
  "save zip
  CALL METHOD L_ZIPPER->SAVE
    RECEIVING
      ZIP = LV_XSTRING.

*  * Convert Xstring into Binary
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      BUFFER     = LV_XSTRING
    TABLES
      BINARY_TAB = LIT_BINARY_CONTENT.
*        EXCEPTIONS
*          program_error                     = 1
*          OTHERS                            = 2.

  CLEAR: L_DATA.
  CONCATENATE 'Pending Order For Dispatch_' SY-DATUM INTO L_DATA.
  L_ATTSUBJECT = L_DATA.
  CLEAR L_DATA.
* Create Attachment
  TRY.
    LO_DOCUMENT->ADD_ATTACHMENT( EXPORTING
                                    I_ATTACHMENT_TYPE = 'ZIP'
                                    I_ATTACHMENT_SUBJECT = L_ATTSUBJECT
                                    I_ATT_CONTENT_HEX = LIT_BINARY_CONTENT  ).
  ENDTRY.

*   *Set Sender
  DATA: LO_SENDER TYPE REF TO IF_SENDER_BCS VALUE IS INITIAL.
  TRY.
    LO_SENDER = CL_SAPUSER_BCS=>CREATE( SY-UNAME ). "sender is the logged in user
* Set sender to send request
    LO_SEND_REQUEST->SET_SENDER(
    EXPORTING
    I_SENDER = LO_SENDER ).
*    CATCH CX_ADDRESS_BCS.
****Catch exception here
  ENDTRY.

* *Set recipient
  DATA: LO_RECIPIENT TYPE REF TO IF_RECIPIENT_BCS VALUE IS INITIAL.
*  LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( 'md@sheenlac.in' ). "Here Recipient is email input p_emailsanthoshkumar@sphinaxinfosystems.com
*  TRY.
*    LO_SEND_REQUEST->ADD_RECIPIENT(
*        EXPORTING
*        I_RECIPIENT = LO_RECIPIENT
*        I_EXPRESS = 'X'
*        I_COPY = ' ').
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
*  ENDTRY.
*
*  LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( 'jayaprakash@sheenlac.in' ). "Here Recipient is email input p_email
*  TRY.
*    LO_SEND_REQUEST->ADD_RECIPIENT(
*        EXPORTING
*        I_RECIPIENT = LO_RECIPIENT
*        I_EXPRESS = 'X'
*        I_COPY = ABAP_TRUE ).
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
*  ENDTRY.
*
*  LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( 'hilda@sheenlac.in' ). "Here Recipient is email input p_email
*  TRY.
*    LO_SEND_REQUEST->ADD_RECIPIENT(
*        EXPORTING
*        I_RECIPIENT = LO_RECIPIENT
*        I_EXPRESS = 'X'
*        I_COPY = ABAP_TRUE ).
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
*  ENDTRY.
*
*  LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( 'vairamani@sheenlac.in' ). "Here Recipient is email input p_email
*  TRY.
*    LO_SEND_REQUEST->ADD_RECIPIENT(
*        EXPORTING
*        I_RECIPIENT = LO_RECIPIENT
*        I_EXPRESS = 'X'
*        I_COPY = ABAP_TRUE ).
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
*  ENDTRY.

  LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( 'natarajan@sheenlac.in' ). "Here Recipient is email input p_email
  TRY.
    LO_SEND_REQUEST->ADD_RECIPIENT(
        EXPORTING
        I_RECIPIENT = LO_RECIPIENT
        I_EXPRESS = 'X'
        I_COPY = ABAP_TRUE ).
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
  ENDTRY.

*Set immediate sending
  TRY.
    CALL METHOD LO_SEND_REQUEST->SET_SEND_IMMEDIATELY
      EXPORTING
        I_SEND_IMMEDIATELY = 'X'.
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
  ENDTRY.

  TRY.
** Send email
    LO_SEND_REQUEST->SEND(
    EXPORTING
    I_WITH_ERROR_SCREEN = 'X' ).
    COMMIT WORK.
    IF SY-SUBRC = 0.
      WRITE :/ 'Mail sent successfully'.
    ENDIF.
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
*catch exception here
  ENDTRY.

ENDFORM.                    "BUILD_XLS_DATA_TABLE_DETAIL
