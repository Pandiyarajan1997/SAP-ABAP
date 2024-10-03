*&---------------------------------------------------------------------*
*& Report  ZVENDOR_DETAIL_DATA
*&
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Savariar.S                         *
*& Created On                  : 14/03/2015                            *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 : Mr.Govindarajan M                     *
*& Title                       : Vendor Details Report Document        *
*& Report Name                 : ZVENDOR_DETAIL_DATA                   *
*& Development Id              : kpabap                                *
*& Related Information         : Vendor Details Report                 *
*&---------------------------------------------------------------------*

REPORT ZVENDOR_DETAIL_DATA.

TABLES : LFA1,LFB1,ADRC,ADR2,ADR6, SKAT,SKA1.


TYPES : BEGIN OF GS_LFA1,

        LIFNR TYPE LFA1-LIFNR,
        NAME1 TYPE LFA1-NAME1,
        NAME3 TYPE LFA1-NAME3,
        NAME4 TYPE LFA1-NAME4,
        WERKS TYPE LFA1-WERKS,
        ORT01 TYPE LFA1-ORT01,
        ADRNR TYPE LFA1-ADRNR,
        STCD3 TYPE LFA1-STCD3,
        END OF GS_LFA1.

DATA : GT_LFA1 TYPE TABLE OF GS_LFA1,
       WA_LFA1 TYPE GS_LFA1.

TYPES : BEGIN OF GS_ADRC,

        ADDRNUMBER TYPE ADRC-ADDRNUMBER,
        NAME1      TYPE ADRC-NAME1,
        CITY1      TYPE ADRC-CITY1,
        POSTALAREA TYPE ADRC-POSTALAREA,
        STREET     TYPE ADRC-STREET,
        STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
        HOUSE_NUM1 TYPE ADRC-HOUSE_NUM1,
        TEL_NUMBER TYPE ADRC-TEL_NUMBER,
        POST_CODE1 TYPE ADRC-POST_CODE1,

  END OF GS_ADRC.

DATA : GT_ADRC TYPE TABLE OF GS_ADRC,
       WA_ADRC TYPE GS_ADRC.

TYPES : BEGIN OF GS_ADR2,

        ADDRNUMBER TYPE ADR2-ADDRNUMBER,
        HOME_FLAG  TYPE ADR2-HOME_FLAG,
        TEL_NUMBER TYPE ADR2-TEL_NUMBER,
        TELNR_LONG TYPE ADR2-TELNR_LONG,
        TELNR_CALL TYPE ADR2-TELNR_CALL,
        END OF GS_ADR2.

DATA : GT_ADR2 TYPE TABLE OF GS_ADR2,
       WA_ADR2 TYPE GS_ADR2.

TYPES : BEGIN OF GS_ADR6,

        ADDRNUMBER TYPE ADR6-ADDRNUMBER,
        SMTP_ADDR  TYPE ADR6-SMTP_ADDR,

        END OF GS_ADR6.

DATA : GT_ADR6 TYPE TABLE OF GS_ADR6,
       WA_ADR6 TYPE GS_ADR6.


TYPES : BEGIN OF GS_J_1IMOVEND,

        LIFNR TYPE J_1IMOVEND-LIFNR, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
        J_1ICSTNO TYPE J_1IMOVEND-J_1ICSTNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
        J_1ILSTNO TYPE J_1IMOVEND-J_1ILSTNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
        J_1IPANNO TYPE J_1IMOVEND-J_1IPANNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
        J_1ISERN TYPE J_1IMOVEND-J_1ISERN, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
        END OF GS_J_1IMOVEND.

DATA : GT_J_1IMOVEND TYPE TABLE OF GS_J_1IMOVEND,
       WA_J_1IMOVEND TYPE GS_J_1IMOVEND.


TYPES : BEGIN OF GS_LFB1,

        LIFNR TYPE LFB1-LIFNR,
        BUKRS TYPE LFB1-BUKRS,
        AKONT TYPE LFB1-AKONT,

        END OF GS_LFB1.

DATA : GT_LFB1 TYPE TABLE OF GS_LFB1,
       WA_LFB1 TYPE GS_LFB1.

TYPES : BEGIN OF GS_LFBK ,                      "ADDED BY RAM ON 30/7/2015
            LIFNR TYPE LFBK-LIFNR ,
            BANKL TYPE LFBK-BANKL ,
            BANKN TYPE LFBK-BANKN ,
            BKONT TYPE LFBK-BKONT ,
        END OF GS_LFBK .

DATA : GT_LFBK TYPE TABLE OF GS_LFBK ,
       WA_LFBK TYPE GS_LFBK .

TYPES : BEGIN OF GS_BNKA ,
          BANKL TYPE BNKA-BANKL ,
          BANKA TYPE BNKA-BANKA ,
       END OF GS_BNKA .

DATA : GT_BNKA TYPE TABLE OF GS_BNKA ,
       WA_BNKA TYPE GS_BNKA .

TYPES : BEGIN OF GS_SKA1,                        "Added by sri on 04/03/17
        KTOPL TYPE SKA1-KTOPL,
        SAKNR TYPE SKA1-SAKNR,
        END OF GS_SKA1.


DATA : GT_SKA1 TYPE STANDARD TABLE OF GS_SKA1,
       WA_SKA1 TYPE GS_SKA1.

TYPES : BEGIN OF GS_SKAT,
        SPRAS TYPE SKAT-SPRAS,
        KTOPL TYPE SKAT-KTOPL,
        SAKNR TYPE SKAT-SAKNR,
        TXT50 TYPE SKAT-TXT50,
        END OF GS_SKAT.

DATA : GT_SKAT TYPE STANDARD TABLE OF GS_SKAT,
       WA_SKAT TYPE GS_SKAT.




TYPES : BEGIN OF GS_FINAL,

          LIFNR      TYPE LFA1-LIFNR,
          NAME1      TYPE LFA1-NAME1,
          NAME3      TYPE LFA1-NAME3,
          NAME4      TYPE LFA1-NAME4,
          NAME2      TYPE ADRC-NAME1,
          CITY1      TYPE ADRC-CITY1,
          POSTALAREA TYPE ADRC-POSTALAREA,
          POST_CODE1 TYPE ADRC-POST_CODE1,
          STREET     TYPE ADRC-STREET,
          STREET1    TYPE   ADRC-STR_SUPPL1,
          HOUSE_NUM1 TYPE ADRC-HOUSE_NUM1,
          TEL_NUMBER TYPE ADR2-TEL_NUMBER,
          SMTP_ADDR  TYPE ADR6-SMTP_ADDR,
          STCD3      TYPE LFA1-STCD3,
          J_1ICSTNO  TYPE J_1IMOVEND-J_1ICSTNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
          J_1ILSTNO  TYPE J_1IMOVEND-J_1ILSTNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
          J_1IPANNO  TYPE J_1IMOVEND-J_1IPANNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
          J_1ISERN   TYPE J_1IMOVEND-J_1ISERN, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation

         BANKL TYPE LFBK-BANKL ,
         BANKN TYPE LFBK-BANKN ,
         BKONT TYPE LFBK-BKONT ,
         BANKA TYPE BNKA-BANKA ,


        SPRAS TYPE SKAT-SPRAS,
        KTOPL TYPE SKAT-KTOPL,
        SAKNR TYPE SKAT-SAKNR,
        TXT50 TYPE SKAT-TXT50,


          CONTACTADD(50) TYPE C,

      END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA : GT_SORT TYPE SLIS_T_SORTINFO_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : L_BUKRS TYPE LFB1-BUKRS,
       L_WERKS TYPE LFA1-WERKS,
       L_LIFNR TYPE LFA1-LIFNR.

DATA :  LV_BUKRS TYPE LFB1-BUKRS,
        LV_WERKS TYPE LFA1-WERKS,
        LV_LIFNR  TYPE LFA1-LIFNR.

SELECTION-SCREEN : BEGIN OF BLOCK S WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : SO_BUKRS FOR LV_BUKRS OBLIGATORY,
                 SO_WERKS FOR LV_WERKS NO-DISPLAY,
                 SO_LIFNR FOR LV_LIFNR.

SELECTION-SCREEN : END OF BLOCK S.


AT SELECTION-SCREEN .

  IF SO_BUKRS IS NOT INITIAL .
    SELECT SINGLE BUKRS FROM LFB1 INTO L_BUKRS WHERE BUKRS IN SO_BUKRS .
    IF SY-SUBRC <> 0.
      MESSAGE 'Enter Valid Company Code' TYPE 'E'.
    ENDIF.
  ENDIF.

  IF SO_WERKS IS NOT INITIAL .
    SELECT SINGLE WERKS FROM LFA1 INTO L_WERKS WHERE WERKS IN SO_WERKS .
    IF SY-SUBRC <> 0.
      MESSAGE 'Enter Valid Plant' TYPE 'E'.
    ENDIF.
  ENDIF.

  IF SO_LIFNR IS NOT INITIAL .
    SELECT SINGLE LIFNR FROM LFA1 INTO L_LIFNR WHERE LIFNR IN SO_LIFNR.
    IF SY-SUBRC <> 0.
      MESSAGE 'Enter Valid Vendor' TYPE 'E'.
    ENDIF.
  ENDIF.


START-OF-SELECTION.

  PERFORM GET_DATA.
  PERFORM GET_FIELDCATLOG.
  PERFORM GET_ALV_DISPLAY.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

  SELECT
      LIFNR
      BUKRS
      AKONT
      FROM
      LFB1 INTO TABLE GT_LFB1 WHERE LIFNR IN SO_LIFNR AND BUKRS IN SO_BUKRS.



  IF GT_LFB1[] IS NOT INITIAL.

    SELECT
         LIFNR
         NAME1
         NAME3
         NAME4
         WERKS
         ORT01
         ADRNR
         STCD3
     FROM
         LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN  GT_LFB1 WHERE LIFNR = GT_LFB1-LIFNR AND LIFNR IN SO_LIFNR AND WERKS IN SO_WERKS.
  ENDIF.


  IF GT_LFA1[] IS NOT INITIAL.

    SELECT
    LIFNR
    BANKL
    BANKN
    BKONT
      FROM LFBK INTO TABLE GT_LFBK FOR ALL ENTRIES IN GT_LFB1 WHERE LIFNR = GT_LFB1-LIFNR .     "ADDED BY RAM ON 30/7/15


    SELECT
        ADDRNUMBER
        NAME1
        CITY1
        POSTALAREA
        STREET
        STR_SUPPL1
        HOUSE_NUM1
        TEL_NUMBER
        POST_CODE1
    FROM
      ADRC INTO TABLE GT_ADRC FOR ALL ENTRIES IN GT_LFA1 WHERE ADDRNUMBER = GT_LFA1-ADRNR.


    SELECT
        ADDRNUMBER
        HOME_FLAG
        TEL_NUMBER
        TELNR_LONG
        TELNR_CALL
     FROM
      ADR2 INTO TABLE GT_ADR2 FOR ALL ENTRIES IN GT_LFA1 WHERE ADDRNUMBER = GT_LFA1-ADRNR.

    SELECT
        ADDRNUMBER
        SMTP_ADDR
     FROM
      ADR6 INTO TABLE GT_ADR6 FOR ALL ENTRIES IN GT_LFA1 WHERE ADDRNUMBER = GT_LFA1-ADRNR.


    SELECT
        LIFNR
        J_1ICSTNO
        J_1ILSTNO
        J_1IPANNO
        J_1ISERN
      FROM
        LFA1 INTO TABLE GT_J_1IMOVEND FOR ALL ENTRIES IN GT_LFA1 WHERE LIFNR = GT_LFA1-LIFNR AND LIFNR IN SO_LIFNR.


    SELECT  KTOPL                                                                          "#EC CI_DB_OPERATION_OK[2389136] "Add by on sri 04/03/17Added by <IT-CAR Tool> during Code Remediation
            SAKNR FROM SKA1 INTO TABLE GT_SKA1 "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
            FOR ALL ENTRIES IN GT_LFB1  WHERE SAKNR = GT_LFB1-AKONT.


    SELECT  SPRAS
            KTOPL
            SAKNR
            TXT50 FROM SKAT INTO TABLE GT_SKAT
            FOR ALL ENTRIES IN GT_SKA1 WHERE SAKNR = GT_SKA1-SAKNR AND KTOPL = GT_SKA1-KTOPL.

  ENDIF.


  SELECT BANKL
         BANKA
         FROM BNKA INTO TABLE GT_BNKA
         FOR ALL ENTRIES IN GT_LFBK WHERE BANKL = GT_LFBK-BANKL .




  LOOP AT GT_LFB1 INTO WA_LFB1.

    LOOP AT GT_LFA1 INTO WA_LFA1 WHERE LIFNR = WA_LFB1-LIFNR.
      WA_FINAL-LIFNR = WA_LFA1-LIFNR.
      WA_FINAL-NAME1 = WA_LFA1-NAME1 .
      WA_FINAL-NAME3 = WA_LFA1-NAME3 .
      WA_FINAL-NAME4 = WA_LFA1-NAME4 .
      WA_FINAL-STCD3 = WA_LFA1-STCD3.

      READ TABLE GT_LFBK INTO WA_LFBK WITH KEY LIFNR = WA_LFB1-LIFNR.               "ADDED BY RAM ON 30/7/2015
      IF SY-SUBRC = 0.
        WA_FINAL-BANKL = WA_LFBK-BANKL .
        WA_FINAL-BANKN = WA_LFBK-BANKN .
        WA_FINAL-BKONT = WA_LFBK-BKONT .
      ENDIF .

      LOOP AT GT_BNKA INTO WA_BNKA WHERE BANKL = WA_LFBK-BANKL .
        IF SY-SUBRC = 0.
          WA_FINAL-BANKA = WA_BNKA-BANKA .
        ENDIF .
      ENDLOOP .

      " READ TABLE GT_BNKA INTO WA_BNKA WITH KEY BANKL = WA_LFBK-BANKL .


      READ TABLE GT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = WA_LFA1-ADRNR.

      IF SY-SUBRC = 0.
        WA_FINAL-NAME2 = WA_ADRC-NAME1 .
        WA_FINAL-CITY1 = WA_ADRC-CITY1.
        WA_FINAL-POSTALAREA = WA_ADRC-POSTALAREA.
        WA_FINAL-STREET = WA_ADRC-STREET.
        WA_FINAL-STREET1 =  WA_ADRC-STR_SUPPL1.
        WA_FINAL-HOUSE_NUM1 = WA_ADRC-HOUSE_NUM1.
        WA_FINAL-POST_CODE1 = WA_ADRC-POST_CODE1.
      ENDIF.

      READ TABLE GT_ADR2 INTO WA_ADR2 WITH KEY ADDRNUMBER = WA_LFA1-ADRNR.

      IF SY-SUBRC = 0.
        WA_FINAL-TEL_NUMBER = WA_ADR2-TEL_NUMBER.
      ENDIF.

      READ TABLE GT_ADR6 INTO WA_ADR6 WITH KEY ADDRNUMBER = WA_LFA1-ADRNR.
      IF SY-SUBRC = 0.

        WA_FINAL-SMTP_ADDR = WA_ADR6-SMTP_ADDR.

      ENDIF.

      READ TABLE GT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_LFB1-AKONT.

      IF SY-SUBRC = 0.

        WA_FINAL-SAKNR = WA_SKAT-SAKNR.
        WA_FINAL-TXT50 = WA_SKAT-TXT50.
      ENDIF.




      READ TABLE GT_J_1IMOVEND INTO WA_J_1IMOVEND WITH KEY LIFNR = WA_LFA1-LIFNR.
      IF SY-SUBRC = 0.

        WA_FINAL-J_1ICSTNO = WA_J_1IMOVEND-J_1ICSTNO.
        WA_FINAL-J_1ILSTNO = WA_J_1IMOVEND-J_1ILSTNO.
        WA_FINAL-J_1IPANNO = WA_J_1IMOVEND-J_1IPANNO.
        WA_FINAL-J_1ISERN = WA_J_1IMOVEND-J_1ISERN.

      ENDIF.

      CONCATENATE  WA_FINAL-HOUSE_NUM1 WA_FINAL-STREET WA_FINAL-STREET1 WA_FINAL-CITY1 '-' WA_FINAL-POST_CODE1 INTO WA_FINAL-CONTACTADD SEPARATED BY SPACE.

      SHIFT WA_FINAL-LIFNR LEFT DELETING LEADING '0'.

      APPEND WA_FINAL TO GT_FINAL.

      CLEAR WA_FINAL.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " GET_DATA


*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FIELDCATLOG .


  PERFORM ALV_LAYOUT USING 1 'Vendor Code' 'LIFNR' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 2 'Vendor Type' 'NAME4' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 3 'Vendor Name' 'NAME1' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 28 'Alias Code' 'NAME3' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 5 'Address' 'CONTACTADD' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 7 'Contact No.' 'TEL_NUMBER' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 9 'Email' 'SMTP_ADDR' 'GT_FINAL' '' '' '' '' '' .
*
  PERFORM ALV_LAYOUT USING 11 'G/L Account Number' 'SAKNR' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 13 'G/L Acc Description' 'TXT50' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 14 'GST No' 'STCD3' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 15 'TIN No.' 'J_1ILSTNO' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 17 'CST No.' 'J_1ICSTNO' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 19 'Service Tax No' 'J_1ISERN' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 21 'PAN No.' 'J_1IPANNO' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 23 'Bene Acc No' 'BANKN' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 25 'IFSC Code' 'BANKL' 'GT_FINAL' '' '' '' '' '' .

  PERFORM ALV_LAYOUT USING 27 'Bank Name' 'BANKA' 'GT_FINAL' '' '' '' '' '' .
*
*  PERFORM ALV_LAYOUT USING 20 'HOUSE_NUM1' 'HOUSE_NUM1' 'GT_FINAL' '' '' '' '' '' .
*
*  PERFORM ALV_LAYOUT USING 21 'STREET' 'STREET' 'GT_FINAL' '' '' '' '' '' .
*
*  PERFORM ALV_LAYOUT USING 22 'CITY1' 'CITY1' 'GT_FINAL' '' '' '' '' '' .
*
*  PERFORM ALV_LAYOUT USING 23 'POSTALAREA' 'POSTALAREA' 'GT_FINAL' '' '' '' '' '' .
*
*  PERFORM ALV_LAYOUT USING 24 'CONTACTADD' 'CONTACTADD' 'GT_FINAL' '' '' '' '' '' .


ENDFORM.                    " GET_FIELDCATLOG




*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1         text
*      -->P2         text
*      -->P3         text
*      -->P4         text
*      -->P5         text
*      -->P6         text
*      -->P7         text
*      -->P8         text
*      -->P9         text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT USING P1 P2 P3 P4 P5 P6 P7 P8 P9.

  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-OUTPUTLEN = P6.
  WA_FCAT-KEY = P7.
  WA_FCAT-NO_OUT = P8.
  WA_FCAT-NO_ZERO = P9.

  APPEND WA_FCAT TO GT_FCAT.
  CLEAR WA_FCAT.

ENDFORM.                    "ALV_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  GET_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ALV_DISPLAY .


  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_CALLBACK_TOP_OF_PAGE = 'ALV_CATALOG_HEADER'
      IS_LAYOUT              = LAYOUT
      IT_FIELDCAT            = GT_FCAT[]
      IT_SORT                = GT_SORT[]
      I_DEFAULT              = 'X'
      I_SAVE                 = 'A'
    TABLES
      T_OUTTAB               = GT_FINAL[].
*    EXCEPTIONS
*      OPERATION_NO_AUTHORIZATION = 1.

  IF SY-SUBRC <> 0.
    WRITE: 'FAILURE'.
  ENDIF.



ENDFORM.                    " GET_ALV_DISPLAY






*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_CATALOG_HEADER.

  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.

  DATA :
         LV_BUKRS(100) TYPE C,
*         LV_BEDAT(50) TYPE C,
         LV_LIFNR(100) TYPE C.
*         LV_BEDAT1 TYPE SY-DATUM.

  CLEAR : LV_BUKRS,
          LV_LIFNR.
  IF SO_BUKRS-HIGH IS NOT INITIAL.
    CONCATENATE 'Company  Code :' SO_BUKRS-LOW 'To' SO_BUKRS-HIGH INTO LV_BUKRS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Company Code :' SO_BUKRS-LOW INTO LV_BUKRS SEPARATED BY SPACE.
  ENDIF.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_BUKRS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Vendor Details' .
  APPEND LS_LINE TO LIT_HEADER.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER.

*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*    EXPORTING
*      INPUT  = LV_BEDAT
*    IMPORTING
*      OUTPUT = LV_BEDAT.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.


ENDFORM.                    "ALV_CATALOG_HEADER
