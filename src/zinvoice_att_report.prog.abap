*&---------------------------------------------------------------------*
*& Report  ZINV_ATT_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZINVOICE_ATT_REPORT.
TYPE-POOLS:SLIS.

TABLES:BKPF.
TABLES:BSEG.
TABLES:LFA1.

TYPES:BEGIN OF TY_BSAK,
  BUKRS TYPE BSAK-BUKRS,       "COMPANY CODE
  LIFNR TYPE BSAK-LIFNR,
  BELNR TYPE BSAK-BELNR,       "Accounting Document Number
  GJAHR TYPE BSAK-GJAHR,       "Fiscal Year
  BLART TYPE BSAK-BLART,       "Document Type
  BLDAT TYPE BSAK-BLDAT,       "Document Date in Document
  BUDAT TYPE BSAK-BUDAT,
  AWKEY TYPE BKPF-AWKEY,
  END OF TY_BSAK.

DATA: IT_BSAK TYPE STANDARD TABLE OF TY_BSAK,
      WA_BSAK TYPE TY_BSAK,
      IT_BSIK TYPE STANDARD TABLE OF TY_BSAK,
      WA_BSIK TYPE TY_BSAK.

*TYPES:BEGIN OF TY_BKPF,
*  BUKRS TYPE BKPF-BUKRS,       "COMPANY CODE
*  BELNR TYPE BKPF-BELNR,       "Accounting Document Number
*  GJAHR TYPE BKPF-GJAHR,       "Fiscal Year
*  BLART TYPE BKPF-BLART,       "Document Type
*  BLDAT TYPE BKPF-BLDAT,       "Document Date in Document
*  BUDAT TYPE BKPF-BUDAT,
*  AWKEY TYPE BKPF-AWKEY,
*  END OF TY_BKPF.

*DATA: IT_BKPF TYPE STANDARD TABLE OF TY_BKPF,
*      WA_BKPF TYPE TY_BKPF.

DATA: WA_T001 TYPE T001.

TYPES:BEGIN OF TY_BSEG,
  BUKRS TYPE BSEG-BUKRS,
  BELNR TYPE BSEG-BELNR,
  GJAHR TYPE BSEG-GJAHR,
  BUZEI TYPE BSEG-BUZEI,
  KOART TYPE BSEG-KOART,
  LIFNR TYPE BSEG-LIFNR,
  END OF TY_BSEG.

DATA: IT_BSEG TYPE STANDARD TABLE OF TY_BSEG,
      WA_BSEG TYPE TY_BSEG.

TYPES:BEGIN OF TY_LFA1,
  LIFNR TYPE LFA1-LIFNR,
  NAME1 TYPE LFA1-NAME1,
  END OF TY_LFA1.

DATA: IT_LFA1 TYPE STANDARD TABLE OF TY_LFA1,
      WA_LFA1 TYPE TY_LFA1.

TYPES: BEGIN OF TY_DOFI,
  BUKRS TYPE BKPF-BUKRS,
  BLDAT TYPE BKPF-BLDAT,
  BUDAT TYPE BKPF-BUDAT,
  BELNR TYPE BKPF-BELNR,
  MBLNR TYPE BKPF-BELNR,
  BLART TYPE BKPF-BLART,
  LIFNR TYPE BSEG-LIFNR,
  NAME1 TYPE LFA1-NAME1,
  INVAT TYPE CHAR3,
  INVDT TYPE CHAR10,
  FILENAME TYPE TOAAT-FILENAME,
  CREATOR TYPE TOAAT-CREATOR,
  COOBJECT_ID TYPE TOA03-OBJECT_ID,
  COOBJECT_ID1 TYPE TOA03-OBJECT_ID,
  AWKEY TYPE BKPF-AWKEY,
  INSTID_B TYPE SRGBTBREL-INSTID_B,
  INSTID_B1 TYPE SRGBTBREL-INSTID_B,
  INSTID_B2 TYPE SRGBTBREL-INSTID_B,
  INSTID_B3 TYPE SRGBTBREL-INSTID_B,
  USR_NAM TYPE V_USR_NAME-NAME_TEXT,
  END OF TY_DOFI.

DATA: IT_DOFI TYPE STANDARD TABLE OF TY_DOFI,
      WA_DOFI TYPE TY_DOFI,
      IT1_DOFI TYPE STANDARD TABLE OF TY_DOFI,
      WA1_DOFI TYPE TY_DOFI,
      IT_DOFI1 TYPE STANDARD TABLE OF TY_DOFI,
      WA_DOFI1 TYPE TY_DOFI,
      IT1_DOFI1 TYPE STANDARD TABLE OF TY_DOFI,
      WA1_DOFI1 TYPE TY_DOFI,
      IT_FINAL TYPE STANDARD TABLE OF TY_DOFI,
      WA_FINAL TYPE TY_DOFI.

DATA: REV TYPE SRGBTBREL-INSTID_B .

DATA: RS1 TYPE CHAR12,
      RS2 TYPE CHAR2,
      RS3 TYPE CHAR3.

DATA: SPLIT3 TYPE CHAR3,
      SPLIT2 TYPE CHAR2,
      SPLIT1 TYPE CHAR12.

DATA: CNAME TYPE T001-BUTXT .

TYPES:BEGIN OF TY_RBKP,
  BELNR TYPE RBKP-BELNR,
  GJAHR TYPE RBKP-GJAHR,
  BLART TYPE RBKP-BLART,
  BLDAT TYPE RBKP-BLDAT,
  BUDAT TYPE RBKP-BUDAT,
  BUKRS TYPE RBKP-BUKRS,
  LIFNR TYPE RBKP-LIFNR,
  END OF TY_RBKP.

DATA: IT_RBKP TYPE STANDARD TABLE OF TY_RBKP,
      WA_RBKP TYPE TY_RBKP.

DATA: IT_TOA03 TYPE STANDARD TABLE OF TOA03,
      WA_TOA03 TYPE TOA03.

TYPES:BEGIN OF TY_SRGBTBREL,
  BRELGUID TYPE SRGBTBREL-BRELGUID,
  RELTYPE TYPE SRGBTBREL-RELTYPE,
  INSTID_A TYPE SRGBTBREL-INSTID_A,
  TYPEID_A TYPE SRGBTBREL-TYPEID_A,
  CATID_A TYPE SRGBTBREL-CATID_A,
  INSTID_B TYPE SRGBTBREL-INSTID_B,
  TYPEID_B TYPE SRGBTBREL-TYPEID_B,
  CATID_B TYPE SRGBTBREL-CATID_B,
  LOGSYS_A TYPE SRGBTBREL-LOGSYS_A,
  ARCH_A TYPE SRGBTBREL-ARCH_A,
  LOGSYS_B TYPE SRGBTBREL-LOGSYS_B,
  ARCH_B TYPE SRGBTBREL-ARCH_B,
  UTCTIME TYPE SRGBTBREL-UTCTIME,
  HOMESYS TYPE SRGBTBREL-HOMESYS,
  END OF TY_SRGBTBREL.

DATA: IT_SRGBTBREL TYPE TABLE OF TY_SRGBTBREL,
      WA_SRGBTBREL TYPE TY_SRGBTBREL.

TYPES:BEGIN OF TY_SOOD,
  OBJTP TYPE SOOD-OBJTP,
  OBJYR TYPE SOOD-OBJYR,
  OBJNO TYPE SOOD-OBJNO,
  OBJDES TYPE SOOD-OBJDES,
  OWNNAM TYPE SOOD-OWNNAM,
  END OF TY_SOOD.

DATA: IT_SOOD TYPE TABLE OF TY_SOOD,
      WA_SOOD TYPE TABLE OF TY_SOOD.

TYPES : BEGIN OF TY_TOAAT,
        ARC_DOC_ID TYPE TOAAT-ARC_DOC_ID,
        FILENAME TYPE TOAAT-FILENAME,
        CREATOR TYPE TOAAT-CREATOR,
        DESCR TYPE TOAAT-DESCR,
        CREATIME TYPE TOAAT-CREATIME,
      END OF TY_TOAAT.

DATA : IT_TOAAT TYPE TABLE OF TY_TOAAT,
       WA_TOAAT TYPE TY_TOAAT.


TYPES:BEGIN OF TY_USR_NAM,
  BNAME TYPE V_USR_NAME-BNAME,
  NAME_FIRST TYPE V_USR_NAME-NAME_FIRST,
  NAME_LAST TYPE V_USR_NAME-NAME_LAST,
  NAME_TEXT TYPE V_USR_NAME-NAME_TEXT,
  END OF TY_USR_NAM.

DATA: IT_USR_NAM TYPE TABLE OF TY_USR_NAM,
      WA_USR_NAM TYPE TY_USR_NAM.


DATA: CONCAT(50) TYPE C.
DATA: CONCAT1(50) TYPE C.
DATA: COUNT TYPE I.
DATA: LDAT TYPE SY-DATUM,
      LDAT1 TYPE CHAR15,
      HDAT TYPE SY-DATUM,
      HDAT1 TYPE CHAR15,
      TDAT TYPE CHAR30,
      NOCC TYPE CHAR50.

DATA: SDAT TYPE CHAR10.

DATA: IT_FIELDCAT TYPE STANDARD TABLE OF SLIS_FIELDCAT_ALV,
      WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

* add prasath
DATA: IT_HEADER TYPE SLIS_T_LISTHEADER,
      WA_HEADER TYPE SLIS_LISTHEADER,
      WA1_HEADER TYPE SLIS_LISTHEADER,
      WA2_HEADER TYPE SLIS_LISTHEADER.
* end prasath

DATA: LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN:BEGIN OF BLOCK INVREP WITH FRAME TITLE TEXT-001.
PARAMETERS: P_CCOD LIKE BKPF-BUKRS.
SELECT-OPTIONS: S_DATE FOR BKPF-BLDAT.
SELECT-OPTIONS: S_NUMB FOR BKPF-BELNR.
SELECT-OPTIONS: S_TYPE FOR BKPF-BLART.
"select-options: p_gjahr for bkpf-gjahr.
*PARAMETERS : p_acc  TYPE c RADIOBUTTON GROUP r1 DEFAULT 'X'.
*PARAMETERS : p_miro TYPE c RADIOBUTTON GROUP r1.
SELECTION-SCREEN:END OF BLOCK INVREP.

START-OF-SELECTION.

  PERFORM: GET_DATA.
  PERFORM: READ_DATA.
  PERFORM: FIELDCAT.
  PERFORM: DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

  IF NOT P_CCOD IS INITIAL AND S_DATE IS NOT INITIAL.
    LDAT = S_DATE-LOW.
    HDAT = S_DATE-HIGH.

    SELECT
    A~BUKRS
    A~LIFNR
    A~BELNR
    A~GJAHR
    A~BLART
    A~BLDAT
    A~BUDAT
    B~AWKEY FROM BSAK AS A INNER JOIN BKPF AS B ON B~BUKRS = A~BUKRS AND B~BELNR = A~BELNR AND B~GJAHR = A~GJAHR
      INTO TABLE IT_BSAK WHERE A~BUKRS EQ P_CCOD AND
                                             A~BUDAT IN S_DATE AND
                                             A~BELNR IN S_NUMB AND
                                             A~BLART IN S_TYPE AND ( ( A~BLART IN ('KR','RE','SA' ) AND A~SHKZG = 'H') OR
                                                (  A~BLART IN ('KZ','KG' ) AND A~SHKZG = 'S' ) ) .


    SELECT
    A~BUKRS
    A~LIFNR
    A~BELNR
    A~GJAHR
    A~BLART
    A~BLDAT
    A~BUDAT
    B~AWKEY FROM BSIK AS A INNER JOIN BKPF AS B ON B~BUKRS = A~BUKRS AND B~BELNR = A~BELNR AND B~GJAHR = A~GJAHR
      INTO TABLE IT_BSIK WHERE A~BUKRS EQ P_CCOD AND
                                             A~BUDAT IN S_DATE AND
                                             A~BELNR IN S_NUMB AND
                                             A~BLART IN S_TYPE AND ( ( A~BLART IN ('KR','RE','SA' ) AND A~SHKZG = 'H') OR
                                                (  A~BLART IN ('KZ','KG' ) AND A~SHKZG = 'S' ) ) .


    IF IT_BSAK IS NOT INITIAL OR IT_BSIK IS NOT INITIAL.

*      SELECT
*         BUKRS
*         BELNR
*         GJAHR
*         BLART
*         BLDAT
*         BUDAT
*         AWKEY FROM BKPF INTO TABLE IT_BKPF FOR ALL ENTRIES IN IT_BSAK WHERE BUKRS = IT_BSAK-BUKRS AND BELNR = IT_BSAK-BELNR AND GJAHR = IT_BSAK-GJAHR .

      IF IT_BSAK[] IS NOT INITIAL.
      SELECT
        LIFNR
        NAME1 FROM LFA1
        INTO TABLE IT_LFA1
        FOR ALL ENTRIES IN IT_BSAK WHERE LIFNR = IT_BSAK-LIFNR  .
        TYPES: BEGIN OF ty_obj_id,
              ARCHIV_ID TYPE SAEOBJID,
               END OF ty_obj_id.
      DATA lt_obj_id TYPE STANDARD TABLE OF ty_obj_id.
      DATA lw_obj_id TYPE ty_obj_id.
       LOOP AT it_bsak INTO wa_bsak.
         lw_obj_id-archiv_id = |{ wa_bsak-bukrs }{ wa_bsak-belnr }{ wa_bsak-gjahr }|.
         APPEND lw_obj_id TO lt_obj_id.
         IF WA_BSAK-AWKEY IS NOT INITIAL.
          lw_obj_id-archiv_id = wa_bsak-AWKEY.
          APPEND lw_obj_id TO lt_obj_id.
         ENDIF.
       ENDLOOP.

      ENDIF.
      IF IT_BSIK[] IS NOT INITIAL.
      SELECT
        LIFNR
        NAME1 FROM LFA1
        APPENDING TABLE IT_LFA1
        FOR ALL ENTRIES IN IT_BSIK WHERE LIFNR = IT_BSIK-LIFNR  .
       LOOP AT it_bsik INTO wa_bsik.
         lw_obj_id-archiv_id = |{ wa_bsik-bukrs }{ wa_bsik-belnr }{ wa_bsik-gjahr }|.
         APPEND lw_obj_id TO lt_obj_id.
         IF WA_BSIK-AWKEY IS NOT INITIAL.
          lw_obj_id-archiv_id = WA_BSIK-AWKEY.
          APPEND lw_obj_id TO lt_obj_id.
         ENDIF.
       ENDLOOP.
      ENDIF.
      SORT IT_LFA1 by LIFNR.

      SELECT SINGLE * FROM T001 INTO WA_T001 WHERE BUKRS EQ P_CCOD.


*    SELECT
*      BELNR
*      GJAHR
*      BLART
*      BLDAT
*      BUDAT
*      BUKRS
*      LIFNR FROM RBKP INTO TABLE IT_RBKP WHERE BELNR IN S_NUMB AND
*                                               BUDAT IN S_DATE AND
*                                               BLART IN S_TYPE AND
*                                               BUKRS EQ P_CCOD AND ( BLART EQ 'KR' OR BLART EQ 'KZ' OR BLART EQ 'RE' OR BLART EQ 'KG' OR BLART EQ 'SA' )..

      SELECT * FROM toa03 INTO TABLE IT_TOA03
        FOR ALL ENTRIES IN lt_obj_id
        WHERE sap_object IN ('BKPF','BUS2081') AND
              object_id = lt_obj_id-archiv_id AND
              archiv_id	=	'ZS'.
      IF IT_TOA03 IS NOT INITIAL.

      SELECT
          ARC_DOC_ID
          FILENAME
          CREATOR
          DESCR
          CREATIME
              FROM TOAAT INTO TABLE IT_TOAAT
        FOR ALL ENTRIES IN IT_TOA03
        WHERE ARC_DOC_ID = IT_TOA03-ARC_DOC_ID .

      SELECT
        BNAME
        NAME_FIRST
        NAME_LAST
        NAME_TEXT FROM V_USR_NAME INTO TABLE IT_USR_NAM .
      ENDIF.

    ENDIF.
    IF IT_BSAK IS INITIAL AND IT_BSIK IS INITIAL .
      MESSAGE 'Data Not Found' TYPE 'I'.
*      SUBMIT ZINVOICE_ATT_REPORT VIA SELECTION-SCREEN.
    ENDIF.
  ELSE.
    MESSAGE 'Please Enter CompanyCode And Date' TYPE 'I' .
*    SUBMIT ZINVOICE_ATT_REPORT VIA SELECTION-SCREEN.
  ENDIF.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA .

  DATA: lv_doc_id TYPE SO_ENTRYID,
        lw_doc_data  TYPE  SOFOLENTI1.
  DATA lt_object_header   TYPE soli_tab.
*  DATA lt_object_header TYPE STANDARD TABLE OF object_header.

  LOOP AT IT_BSAK INTO WA_BSAK.
    WA_DOFI-BUKRS = WA_BSAK-BUKRS.
    WA_DOFI-LIFNR = WA_BSAK-LIFNR.
    WA_DOFI-BLDAT = WA_BSAK-BLDAT.
    WA_DOFI-BELNR = WA_BSAK-BELNR.
    WA_DOFI-BLART = WA_BSAK-BLART.
    WA_DOFI-BUDAT = WA_BSAK-BUDAT.
    WA_DOFI-AWKEY = WA_BSAK-AWKEY.

*IF WA_DOFI-BLART EQ 'RE'.
*  LOOP AT IT_BKPF INTO WA_BKPF WHERE BUKRS = WA_BSAK-BUKRS AND  BELNR = WA_BSAK-BELNR  AND GJAHR = WA_BSAK-GJAHR.
*    WA_DOFI-COOBJECT_ID1 = WA_BKPF-AWKEY .
*   ENDLOOP.
* ENDIF.
    "IF IT_BSAK-BLART EQ 'RE' .
    "IF WA_DOFI-BLART NE 'RE'.
    CONCATENATE WA_BSAK-BUKRS WA_BSAK-BELNR  WA_BSAK-GJAHR INTO WA_DOFI-COOBJECT_ID.
    "ENDIF.

    LOOP AT IT_LFA1 INTO WA_LFA1 WHERE LIFNR EQ WA_BSAK-LIFNR.
      WA_DOFI-LIFNR = WA_LFA1-LIFNR.
      WA_DOFI-NAME1 = WA_LFA1-NAME1.

    ENDLOOP.

    APPEND: WA_DOFI TO IT_DOFI.
    CLEAR : WA_DOFI .
  ENDLOOP.

  LOOP AT IT_DOFI INTO WA1_DOFI .
    " IF WA1_DOFI-BLART NE 'RE' .

    READ TABLE IT_TOA03 INTO DATA(LW_TOA03) WITH KEY OBJECT_ID = WA1_DOFI-AWKEY.
    READ TABLE IT_TOA03 INTO WA_TOA03 WITH KEY OBJECT_ID = WA1_DOFI-COOBJECT_ID.
    IF SY-SUBRC EQ 0 OR LW_TOA03 IS NOT INITIAL.
      IF LW_TOA03 IS NOT INITIAL AND SY-SUBRC NE 0.
        MOVE-CORRESPONDING LW_TOA03 to WA_TOA03.
      ENDIF.
      WA1_DOFI-INVAT = 'YES'.

      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
         DATE_INTERNAL                  =  WA_TOA03-AR_DATE
       IMPORTING
         DATE_EXTERNAL                  =  WA1_DOFI-INVDT
*       EXCEPTIONS
*         DATE_INTERNAL_IS_INVALID       = 1
*         OTHERS                         = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      READ TABLE IT_TOAAT INTO WA_TOAAT WITH KEY ARC_DOC_ID = WA_TOA03-ARC_DOC_ID.
      READ TABLE IT_USR_NAM INTO WA_USR_NAM WITH KEY BNAME =  WA_TOAAT-CREATOR.
      WA1_DOFI-FILENAME = WA_TOAAT-FILENAME .
      WA1_DOFI-CREATOR = WA_TOAAT-CREATOR .
      WA1_DOFI-USR_NAM = WA_USR_NAM-NAME_TEXT.

*      WA1_DOFI-INVDT = WA_TOA03-AR_DATE.
    ELSE.
        WA1_DOFI-INVAT = 'NO'.
*      WA1_DOFI-INVDT = 'EMPTY'.
    ENDIF.

*    READ TABLE IT_TOAAT INTO WA_TOAAT WITH KEY ARC_DOC_ID = WA_TOA03-ARC_DOC_ID.
*    WA1_DOFI-FILENAME = WA_TOAAT-FILENAME .
*    WA1_DOFI-CREATOR = WA_TOAAT-CREATOR .
    " ENDIF.

*IF WA1_DOFI-BLART EQ 'RE' .
*    READ TABLE IT_TOA03 INTO WA_TOA03 WITH KEY OBJECT_ID = WA1_DOFI-COOBJECT_ID1.
*    IF SY-SUBRC EQ 0 .
*      WA1_DOFI-INVAT = 'YES'.
*
*      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
*       EXPORTING
*         DATE_INTERNAL                  =  WA_TOA03-AR_DATE
*       IMPORTING
*         DATE_EXTERNAL                  =  WA1_DOFI-INVDT
**       EXCEPTIONS
**         DATE_INTERNAL_IS_INVALID       = 1
**         OTHERS                         = 2
*                .
*      IF SY-SUBRC <> 0.
** Implement suitable error handling here
*      ENDIF.
*
**      WA1_DOFI-INVDT = WA_TOA03-AR_DATE.
*    ELSE.
*      WA1_DOFI-INVAT = 'NO'.
**      WA1_DOFI-INVDT = 'EMPTY'.
*    ENDIF.
*
*    READ TABLE IT_TOAAT INTO WA_TOAAT WITH KEY ARC_DOC_ID = WA_TOA03-ARC_DOC_ID.
*    WA_DOFI-FILENAME = WA_TOAAT-FILENAME .
*    WA1_DOFI-CREATOR = WA_TOAAT-CREATOR .
*    ENDIF.

    APPEND WA1_DOFI TO IT1_DOFI.
    CLEAR:LW_TOA03 ,WA_TOA03,WA1_DOFI.
  ENDLOOP.
  APPEND LINES OF IT1_DOFI TO IT_FINAL.

**************************************************BSIK*********************************************

  LOOP AT IT_BSIK INTO WA_BSIK.
    WA_DOFI1-BUKRS = WA_BSIK-BUKRS.
    WA_DOFI1-LIFNR = WA_BSIK-LIFNR.
    WA_DOFI1-BLDAT = WA_BSIK-BLDAT.
    WA_DOFI1-BELNR = WA_BSIK-BELNR.
    WA_DOFI1-BLART = WA_BSIK-BLART.
    WA_DOFI1-BUDAT = WA_BSIK-BUDAT.
    WA_DOFI1-AWKEY = WA_BSIK-AWKEY.

*IF WA_DOFI1-BLART EQ 'RE'.
*  LOOP AT IT_BKPF INTO WA_BKPF WHERE BUKRS = WA_BSIK-BUKRS AND  BELNR = WA_BSIK-BELNR  AND GJAHR = WA_BSIK-GJAHR.
*    WA_DOFI1-COOBJECT_ID1 = WA_BKPF-AWKEY .
*   ENDLOOP.
* ENDIF.
    "IF IT_BSIK-BLART EQ 'RE' .
    "IF WA_DOFI1-BLART NE 'RE'.
    CONCATENATE WA_BSIK-BUKRS WA_BSIK-BELNR  WA_BSIK-GJAHR INTO WA_DOFI1-COOBJECT_ID.
    "ENDIF.

    LOOP AT IT_LFA1 INTO WA_LFA1 WHERE LIFNR EQ WA_BSIK-LIFNR.
      WA_DOFI1-LIFNR = WA_LFA1-LIFNR.
      WA_DOFI1-NAME1 = WA_LFA1-NAME1.

    ENDLOOP.

    APPEND: WA_DOFI1 TO IT_DOFI1.
    CLEAR : WA_DOFI1 .
  ENDLOOP.

  LOOP AT IT_DOFI1 INTO WA1_DOFI1 .
    " IF WA1_DOFI-BLART NE 'RE' .

    READ TABLE IT_TOA03 INTO LW_TOA03 WITH KEY OBJECT_ID = WA1_DOFI1-AWKEY.
    READ TABLE IT_TOA03 INTO WA_TOA03 WITH KEY OBJECT_ID = WA1_DOFI1-COOBJECT_ID.
    IF SY-SUBRC EQ 0 OR LW_TOA03 IS NOT INITIAL.
      IF LW_TOA03 IS NOT INITIAL AND SY-SUBRC NE 0.
        MOVE-CORRESPONDING LW_TOA03 to WA_TOA03.
      ENDIF.
      WA1_DOFI1-INVAT = 'YES'.

      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
         DATE_INTERNAL                  =  WA_TOA03-AR_DATE
       IMPORTING
         DATE_EXTERNAL                  =  WA1_DOFI1-INVDT
*       EXCEPTIONS
*         DATE_INTERNAL_IS_INVALID       = 1
*         OTHERS                         = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      READ TABLE IT_TOAAT INTO WA_TOAAT WITH KEY ARC_DOC_ID = WA_TOA03-ARC_DOC_ID.
      READ TABLE IT_USR_NAM INTO WA_USR_NAM WITH KEY BNAME =  WA_TOAAT-CREATOR.
      WA1_DOFI1-FILENAME = WA_TOAAT-FILENAME .
      WA1_DOFI1-CREATOR = WA_TOAAT-CREATOR .
      WA1_DOFI1-USR_NAM = WA_USR_NAM-NAME_TEXT.

*      WA1_DOFI-INVDT = WA_TOA03-AR_DATE.
    ELSE.
      WA1_DOFI1-INVAT = 'NO'.
*      WA1_DOFI-INVDT = 'EMPTY'.
    ENDIF.

*    READ TABLE IT_TOAAT INTO WA_TOAAT WITH KEY ARC_DOC_ID = WA_TOA03-ARC_DOC_ID.
*    WA1_DOFI-FILENAME = WA_TOAAT-FILENAME .
*    WA1_DOFI-CREATOR = WA_TOAAT-CREATOR .
    " ENDIF.

*IF WA1_DOFI-BLART EQ 'RE' .
*    READ TABLE IT_TOA03 INTO WA_TOA03 WITH KEY OBJECT_ID = WA1_DOFI-COOBJECT_ID1.
*    IF SY-SUBRC EQ 0 .
*      WA1_DOFI-INVAT = 'YES'.
*
*      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
*       EXPORTING
*         DATE_INTERNAL                  =  WA_TOA03-AR_DATE
*       IMPORTING
*         DATE_EXTERNAL                  =  WA1_DOFI-INVDT
**       EXCEPTIONS
**         DATE_INTERNAL_IS_INVALID       = 1
**         OTHERS                         = 2
*                .
*      IF SY-SUBRC <> 0.
** Implement suitable error handling here
*      ENDIF.
*
**      WA1_DOFI-INVDT = WA_TOA03-AR_DATE.
*    ELSE.
*      WA1_DOFI-INVAT = 'NO'.
**      WA1_DOFI-INVDT = 'EMPTY'.
*    ENDIF.
*
*    READ TABLE IT_TOAAT INTO WA_TOAAT WITH KEY ARC_DOC_ID = WA_TOA03-ARC_DOC_ID.
*    WA_DOFI1-FILENAME = WA_TOAAT-FILENAME .
*    WA1_DOFI-CREATOR = WA_TOAAT-CREATOR .
*    ENDIF.
    APPEND WA1_DOFI1 TO IT1_DOFI1.
    CLEAR: WA1_DOFI1,WA_TOA03,LW_TOA03.
  ENDLOOP.
  APPEND LINES OF IT1_DOFI1 TO IT_FINAL.


  DELETE ADJACENT DUPLICATES FROM IT_FINAL COMPARING ALL FIELDS .

  SORT IT_FINAL BY BUDAT BELNR  .

      " Start insert code by Puratchi
  LOOP AT IT_FINAL ASSIGNING FIELD-SYMBOL(<lfs_final>).
    IF <LFS_FINAL>-AWKEY(10) NE <LFS_FINAL>-BELNR.
      <LFS_FINAL>-MBLNR = <LFS_FINAL>-AWKEY(10).
    ENDIF.
    IF <LFS_FINAL>-INVAT = 'YES'.
      CONTINUE.
    ELSE.
      SELECT SINGLE instid_b
          FROM srgbtbrel
          INTO lv_doc_id
        WHERE instid_a = <lfs_final>-coobject_id.
        IF sy-subrc = 0.
*   --- call office API ---
        CLEAR lw_doc_data.
        CALL FUNCTION 'SO_DOCUMENT_READ_API1'
          EXPORTING
            document_id                = lv_doc_id
*            filter                     = ls_filter
          IMPORTING
            document_data              = lw_doc_data
          TABLES
            object_header              = lt_object_header
*            contents_hex               = lt_content_hex
*            object_content             = lt_content_str
          EXCEPTIONS
            document_id_not_exist      = 1
            operation_no_authorization = 2
            x_error                    = 3
            OTHERS                     = 4.
         IF lw_doc_data IS NOT INITIAL.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
           EXPORTING
             DATE_INTERNAL                  =  lw_doc_data-creat_date
           IMPORTING
             DATE_EXTERNAL                  =  <lfs_final>-invdt
             .

          <lfs_final>-filename = VALUE #( lt_object_header[ 1 ]-line optional ) .
          REPLACE ALL OCCURRENCES OF '&SO_FILENAME=' IN <lfs_final>-filename WITH ''.
          <lfs_final>-creator = lw_doc_data-creat_name.
          <lfs_final>-usr_nam = lw_doc_data-creat_fnam.
          <lfs_final>-invat = 'YES'.
*         ELSE.
*           CALL FUNCTION 'GOS_EXECUTE_SERVICE'
*             EXPORTING
*               ip_service                 =
*               is_object                  =
*               ip_no_commit               =
*               ip_popup                   =
**              IP_RWMOD                   =
**              IT_SERVICE_SELECTION       =
**              IP_VSI_PROFILE             =
**            IMPORTING
**              EP_EVENT                   =
**              EP_STATUS                  =
**              EP_ICON                    =
**              EP_MESSAGE                 =
**              EP_CHECK_ATTA_LIST         =
**            EXCEPTIONS
**              EXECUTION_FAILED           = 1
**              OTHERS                     = 2
*                     .
*           IF sy-subrc <> 0.
** Implement suitable error handling here
*           ENDIF.

         ENDIF.
        ENDIF.
      ENDIF.
  ENDLOOP.
      " End insert code by Puratchi

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT .
*  WA_FIELDCAT-ROW_POS = '1' .
*  WA_FIELDCAT-COL_POS = '0' .
*  WA_FIELDCAT-FIELDNAME = 'BUKRS' .
*  WA_FIELDCAT-TABNAME  = WA_FINAL .
*  WA_FIELDCAT-SELTEXT_M = 'Company Code' .
*  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
*  CLEAR:WA_FIELDCAT.

  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '0' .
  WA_FIELDCAT-FIELDNAME = 'BLART' .
  WA_FIELDCAT-TABNAME  = WA_FINAL .
  WA_FIELDCAT-SELTEXT_M = 'Document Type' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.

  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '1' .
  WA_FIELDCAT-FIELDNAME = 'BUDAT' .
  WA_FIELDCAT-TABNAME  = WA_FINAL .
  WA_FIELDCAT-SELTEXT_M = 'Posting Date' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.


  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '2' .
  WA_FIELDCAT-FIELDNAME = 'BELNR' .
  WA_FIELDCAT-TABNAME  = WA_FINAL .
  WA_FIELDCAT-SELTEXT_M = 'A/C Doc. Number' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.


  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '3' .
  WA_FIELDCAT-FIELDNAME = 'MBLNR' .
  WA_FIELDCAT-TABNAME  = WA_FINAL .
  WA_FIELDCAT-SELTEXT_M = 'MIRO Doc.No' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.


  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '4' .
  WA_FIELDCAT-FIELDNAME = 'LIFNR' .
  WA_FIELDCAT-TABNAME  = WA_FINAL .
  WA_FIELDCAT-SELTEXT_M = '  Offset Account' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.

  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '5' .
  WA_FIELDCAT-FIELDNAME = 'NAME1' .
  WA_FIELDCAT-TABNAME  = WA_FINAL .
  WA_FIELDCAT-SELTEXT_M = 'Offset Account Name' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.

  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '6' .
  WA_FIELDCAT-FIELDNAME = 'INVAT' .
  WA_FIELDCAT-TABNAME  = WA_FINAL.
  WA_FIELDCAT-SELTEXT_L = 'Invoice Attached Yes/No' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.

  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '7' .
  WA_FIELDCAT-FIELDNAME = 'INVDT' .
  WA_FIELDCAT-TABNAME  = WA_FINAL .
  WA_FIELDCAT-SELTEXT_L = 'Invoice Attached Date' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.

  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '8' .
  WA_FIELDCAT-FIELDNAME = 'FILENAME' .
  WA_FIELDCAT-TABNAME  = WA_FINAL .
  WA_FIELDCAT-SELTEXT_M = 'File Name ' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.


  WA_FIELDCAT-ROW_POS = '1' .
  WA_FIELDCAT-COL_POS = '9' .
  WA_FIELDCAT-FIELDNAME = 'USR_NAM' .
  WA_FIELDCAT-TABNAME  = WA_FINAL .
  WA_FIELDCAT-SELTEXT_M = '  User ID ' .
  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
  CLEAR:WA_FIELDCAT.

*  WA_FIELDCAT-ROW_POS = '1' .
*  WA_FIELDCAT-COL_POS = '8' .
*  WA_FIELDCAT-FIELDNAME = '  COOBJECT_ID ' .
*  WA_FIELDCAT-TABNAME  = WA_FINAL .
*  WA_FIELDCAT-SELTEXT_M = '  TEST ' .
*  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
*  CLEAR:WA_FIELDCAT.
*
*  WA_FIELDCAT-ROW_POS = '1' .
*  WA_FIELDCAT-COL_POS = '9' .
*  WA_FIELDCAT-FIELDNAME = 'INSTID_B' .
*  WA_FIELDCAT-TABNAME  = WA_FINAL .
*  WA_FIELDCAT-SELTEXT_M = '  TEST ' .
*  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
*  CLEAR:WA_FIELDCAT.
*
*  WA_FIELDCAT-ROW_POS = '1' .
*  WA_FIELDCAT-COL_POS = '10' .
*  WA_FIELDCAT-FIELDNAME = 'RS1' .
*  WA_FIELDCAT-TABNAME  = WA_FINAL .
*  WA_FIELDCAT-SELTEXT_M = '  TEST ' .
*  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
*  CLEAR:WA_FIELDCAT.
*
*  WA_FIELDCAT-ROW_POS = '1' .
*  WA_FIELDCAT-COL_POS = '11' .
*  WA_FIELDCAT-FIELDNAME = 'RS2' .
*  WA_FIELDCAT-TABNAME  = WA_FINAL .
*  WA_FIELDCAT-SELTEXT_M = '  TEST ' .
*  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
*  CLEAR:WA_FIELDCAT.
*
*  WA_FIELDCAT-ROW_POS = '1' .
*  WA_FIELDCAT-COL_POS = '12' .
*  WA_FIELDCAT-FIELDNAME = 'RS3' .
*  WA_FIELDCAT-TABNAME  = WA_FINAL .
*  WA_FIELDCAT-SELTEXT_M = '  TEST ' .
*  APPEND: WA_FIELDCAT TO IT_FIELDCAT .
*  CLEAR:WA_FIELDCAT.
ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY .

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
   EXPORTING
     DATE_INTERNAL                  = LDAT
   IMPORTING
     DATE_EXTERNAL                  = LDAT1
* EXCEPTIONS
*   DATE_INTERNAL_IS_INVALID       = 1
*   OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
   EXPORTING
     DATE_INTERNAL                  = HDAT
   IMPORTING
     DATE_EXTERNAL                  = HDAT1
* EXCEPTIONS
*   DATE_INTERNAL_IS_INVALID       = 1
*   OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


  CONCATENATE LDAT1 '  TO:  ' HDAT1 INTO TDAT.

  IF S_DATE-HIGH IS INITIAL.
    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
         DATE_INTERNAL                  = S_DATE-LOW
       IMPORTING
         DATE_EXTERNAL                  = SDAT
*   EXCEPTIONS
*     DATE_INTERNAL_IS_INVALID       = 1
*     OTHERS                         = 2
                .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

  IF S_DATE-LOW IS INITIAL.
    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
     EXPORTING
       DATE_INTERNAL                  = S_DATE-HIGH
     IMPORTING
       DATE_EXTERNAL                  = SDAT
*   EXCEPTIONS
*     DATE_INTERNAL_IS_INVALID       = 1
*     OTHERS                         = 2
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

  CNAME = WA_T001-BUTXT.

  WA_HEADER-TYP = 'H'.
  WA_HEADER-KEY = ' '.
  WA_HEADER-INFO = 'Invoice attaching'.
  APPEND: WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  WA_HEADER-TYP = 'S'.
  WA_HEADER-KEY = 'Company Code :'.
  WA_HEADER-INFO = P_CCOD.
  APPEND: WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  WA_HEADER-TYP = 'S'.
  WA_HEADER-KEY = 'Company Name :'.
  WA_HEADER-INFO = CNAME.
  APPEND: WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.
  IF S_DATE-LOW IS NOT INITIAL AND S_DATE-HIGH IS NOT INITIAL.
    WA_HEADER-TYP = 'S'.
    WA_HEADER-KEY = 'Posting Date From :'.
    WA_HEADER-INFO = TDAT.
    APPEND: WA_HEADER TO IT_HEADER.
    CLEAR: WA_HEADER.
  ELSE.
    WA_HEADER-TYP = 'S'.
    WA_HEADER-KEY = 'Posting Date :'.
    WA_HEADER-INFO = SDAT.
    APPEND: WA_HEADER TO IT_HEADER.
    CLEAR: WA_HEADER.
  ENDIF.


  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
    I_CALLBACK_TOP_OF_PAGE            = 'TOP_OF_PAGE'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
      IS_LAYOUT                         = LAYOUT
      IT_FIELDCAT                       = IT_FIELDCAT
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          =  IT_FINAL  "IT_FINAL
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DISPLAY

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER
*     I_LOGO             =
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .
ENDFORM   .                 "TOP_OF_PAGE
