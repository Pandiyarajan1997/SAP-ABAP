*&---------------------------------------------------------------------*
*&  Include           ZVENDOR_DUE_TOP
*&---------------------------------------------------------------------*

TABLES : LFA1, "KNA1, Modified By Govind
         LFB1,  "KNB1,
         BSEG,
         LFC1,  "KNC1,
         BSIK,  "BSID,
         ZBSEGA,
         RFPDO,
         BSAK. "created by prabhu on 02.04.2020

TABLES : RFPDO1.

DATA: TAGE(4)    TYPE P,
*---- NTAGE =  Tage fuer Netto-Faelligkeit ---------------------------*
      NTAGE(4)   TYPE P,
*---- STAGE =  Tage fuer Skonto1-Faelligkeit -------------------------*
      STAGE(4)   TYPE P,
*---- ZTAGE =  Tage fuer voraussichtlichen Zahlungseingang -----------*
      ZTAGE(4)   TYPE P,
*---- UTAGE =  Tage fuer Ueber-Faelligkeit ---------------------------*
      UTAGE(4)   TYPE P.

DATA: BEGIN OF RTAGE,
        NTAGE LIKE NTAGE,
        STAGE LIKE STAGE,
        ZTAGE LIKE ZTAGE,
        UTAGE LIKE UTAGE,
     END   OF RTAGE.

DATA: BEGIN OF GIT_HBSIK OCCURS 10.
        INCLUDE STRUCTURE BSIK.
        INCLUDE STRUCTURE ZBSEGA.
        INCLUDE STRUCTURE RTAGE.


DATA: END OF GIT_HBSIK.

DATA : GS_HBSIK LIKE GIT_HBSIK.


*************************        created by prabhu on 02.04.2020  strats****************************
DATA: BEGIN OF GIT_HBSAK OCCURS 10.
        INCLUDE STRUCTURE BSAK.
        INCLUDE STRUCTURE ZBSEGA.
        INCLUDE STRUCTURE RTAGE.

DATA: END  OF GIT_HBSAK.
DATA : GS_HBSAK LIKE GIT_HBSAK.

*************************        created by prabhu on 02.04.2020  ends****************************

*************************        created by prabhu on 15.07.2020  strats****************************

DATA: BEGIN OF GIT_HBSEG OCCURS 100.
        INCLUDE STRUCTURE BSEG.
        INCLUDE STRUCTURE ZBSEGA_2.
        INCLUDE STRUCTURE RTAGE.
DATA: END  OF GIT_HBSEG.
DATA : GS_HBSEG LIKE GIT_HBSEG.

TYPES:BEGIN OF GTY_FINAL2,
        BUKRS TYPE BSEG-BUKRS,
        BELNR TYPE BSEG-BELNR,
        GJAHR TYPE BSEG-GJAHR,
        KOSTL TYPE BSEG-KOSTL, "created by prabhu on 27.06.2020
        LIFNR TYPE BSEG-LIFNR,

  END OF GTY_FINAL2.

DATA:GIT_FINAL2 TYPE TABLE OF GTY_FINAL2,
     GS_FINAL2 TYPE GTY_FINAL2,
     GIT_FINAL3 TYPE TABLE OF GTY_FINAL2,
     GS_FINAL3 TYPE  GTY_FINAL2.

*************************        created by prabhu on 15.07.2020  ends****************************


TYPES : BEGIN OF GTY_FINAL,
        LIFNR TYPE BSIK-LIFNR,
        NAME1 TYPE LFA1-NAME1,
        SORTL TYPE LFA1-SORTL, " Added By Goivd
        VTYPE TYPE LFA1-NAME4, "Added By Goivd
        BUKRS TYPE BSIK-BUKRS,
        AUGDT TYPE BSIK-AUGDT,  "new add by prabhu on 02.04.2020
        BELNR TYPE BSIK-BELNR,
        XBLNR TYPE BSIK-XBLNR,
        BUZEI TYPE BSIK-BUZEI,
        BUDAT TYPE BSIK-BUDAT,
        BLDAT TYPE BSIK-BLDAT,
        WAERS TYPE BSIK-WAERS,  "new add by prabhu on 02.04.2020
        NETDT TYPE ZBSEGA-NETDT,
        UTAGE LIKE UTAGE,
        GJAHR TYPE BSIK-GJAHR,
        UMSKZ TYPE BSIK-UMSKZ,
        PROFIT TYPE BSEG-PRCTR,
        BLART TYPE BSIK-BLART,
        SGTXT TYPE BSIK-SGTXT,
        ZBD1T TYPE BSIK-ZBD1T,
        ZBD3T TYPE BSIK-ZBD3T,
        ZNDUE_AMT TYPE BSIK-DMBTR,
        DMBTR TYPE BSIK-DMBTR,
        SHKZG TYPE BSIK-SHKZG,  "created by prabhu on 30.04.2020
        ZFBDT TYPE BSIK-ZFBDT,   "created by prabhu on 06.04.2020
        KOSTL TYPE BSIK-KOSTL, "created by prabhu on 27.06.2020
*        BSEG_KOSTL TYPE BSEG-KOSTL, "created by prabhu on 15.07.2020
***        ZBD1T TYPE BSIK-ZBD1T,   "created by prabhu on 06.04.2020
        DMSHB TYPE ZBSEGA-DMSHB,
        SLAB1 TYPE ZBSEGA-DMSHB,
        SLAB2 TYPE ZBSEGA-DMSHB,
        SLAB3 TYPE ZBSEGA-DMSHB,
        SLAB4 TYPE ZBSEGA-DMSHB,
        SLAB5 TYPE ZBSEGA-DMSHB,
        SLAB6 TYPE ZBSEGA-DMSHB,
*****  Created by Prabhu on 31.03.2020 starts***********
        SLAB7 TYPE ZBSEGA-DMSHB,
        SLAB8 TYPE ZBSEGA-DMSHB,
        SLAB9 TYPE ZBSEGA-DMSHB,
        ADVNC_PAY TYPE ZBSEGA-DMSHB,
*****  Created by Prabhu on 31.03.2020 Ends***********
          DUE TYPE ZBSEGA-DMSHB,
        END OF GTY_FINAL.

DATA : GIT_FINAL TYPE TABLE OF GTY_FINAL,
       GS_FINAL TYPE GTY_FINAL.

TYPES : BEGIN OF GTY_LFA1,
        LIFNR TYPE LFA1-LIFNR,
        NAME1 TYPE LFA1-NAME1,
        KTOKK TYPE LFA1-KTOKK,
        SORTL TYPE LFA1-SORTL,
        NAME4 TYPE LFA1-NAME4,
        END OF GTY_LFA1.

DATA : GIT_LFA1 TYPE TABLE OF GTY_LFA1,
       GS_LFA1 TYPE GTY_LFA1.

*****  Created by Prabhu on 1.04.2020 starts***********

TYPES : BEGIN OF GTY_LFA11,
        LIFNR TYPE LFA1-LIFNR,
        NAME1 TYPE LFA1-NAME1,
        KTOKK TYPE LFA1-KTOKK,
        SORTL TYPE LFA1-SORTL,
        NAME4 TYPE LFA1-NAME4,
        END OF GTY_LFA11.

DATA : GIT_LFA11 TYPE TABLE OF GTY_LFA11,
       GS_LFA11 TYPE GTY_LFA11.

TYPES:BEGIN OF GTY_FINAL1,
        LIFNR TYPE BSAK-LIFNR,
        NAME1 TYPE LFA1-NAME1,
        SORTL TYPE LFA1-SORTL,
        VTYPE TYPE LFA1-NAME4,
        BUKRS TYPE BSAK-BUKRS,
        AUGDT TYPE BSAK-AUGDT,
        BELNR TYPE BSAK-BELNR,
        XBLNR TYPE BSAK-XBLNR,
        BUZEI TYPE BSAK-BUZEI,
        BUDAT TYPE BSAK-BUDAT,
        BLDAT TYPE BSAK-BLDAT,
        WAERS TYPE BSAK-WAERS,
        NETDT TYPE ZBSEGA-NETDT,
        UTAGE LIKE UTAGE,
        GJAHR TYPE BSAK-GJAHR,
        UMSKZ TYPE BSAK-UMSKZ,
        PROFIT TYPE BSEG-PRCTR,
        BLART TYPE BSAK-BLART,
        SGTXT TYPE BSAK-SGTXT,
        ZBD1T TYPE BSAK-ZBD1T,
        ZBD3T TYPE BSAK-ZBD3T, " added for basline date on 28.05.2020
        ZNDUE_AMT TYPE BSAK-DMBTR,
        DMBTR TYPE BSAK-DMBTR,
        SHKZG TYPE BSAK-SHKZG,   "created by prabhu on 30.04.2020
        ZFBDT TYPE BSAK-ZFBDT,"BSIK-ZFBDT,   "created by prabhu on 06.04.2020
        KOSTL TYPE BSAK-KOSTL, "created by prabhu on 27.06.2020
*        BSEG_KOSTL TYPE BSEG-KOSTL, "created by prabhu on 15.07.2020
***        ZBD1T TYPE BSIK-ZBD1T,   "created by prabhu on 06.04.2020
        DMSHB TYPE ZBSEGA-DMSHB,
        SLAB1 TYPE ZBSEGA-DMSHB,
        SLAB2 TYPE ZBSEGA-DMSHB,
        SLAB3 TYPE ZBSEGA-DMSHB,
        SLAB4 TYPE ZBSEGA-DMSHB,
        SLAB5 TYPE ZBSEGA-DMSHB,
        SLAB6 TYPE ZBSEGA-DMSHB,
        SLAB7 TYPE ZBSEGA-DMSHB,
        SLAB8 TYPE ZBSEGA-DMSHB,
        SLAB9 TYPE ZBSEGA-DMSHB,
        ADVNC_PAY TYPE ZBSEGA-DMSHB,
        DUE TYPE ZBSEGA-DMSHB,
  END OF GTY_FINAL1.

DATA:GIT_FINAL1 TYPE TABLE OF GTY_FINAL1,
     GS_FINAL1 TYPE GTY_FINAL1.

*****  Created by Prabhu on 1.04.2020 Ends***********

DATA: GIT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_FIELDCAT  TYPE SLIS_FIELDCAT_ALV.

*****  Created by Prabhu on 1.04.2020 starts***********
DATA : IT_SORT TYPE SLIS_T_SORTINFO_ALV ,
       WA_SORT TYPE SLIS_SORTINFO_ALV .

*****  Created by Prabhu on 1.04.2020 Ends***********

TYPES : BEGIN OF GTY_FAGL,
        RYEAR  TYPE FAGLFLEXA-RYEAR,
        DOCNR  TYPE FAGLFLEXA-DOCNR,
        RBUKRS TYPE FAGLFLEXA-RBUKRS,
        DOCLN  TYPE FAGLFLEXA-DOCLN,
        PRCTR  TYPE FAGLFLEXA-PRCTR,
        END OF GTY_FAGL.

DATA : GIT_FAGLFLEXA TYPE TABLE OF GTY_FAGL,
       GS_FAGLFLEXA TYPE GTY_FAGL,
       LV_POS(6) TYPE N.

FIELD-SYMBOLS : <FS_FAGL> TYPE FAGLFLEXA.

************************created by prabhu on 02.04.2020  starts******************
TYPES : BEGIN OF GTY_FAGL1,
        RYEAR  TYPE FAGLFLEXA-RYEAR,
        DOCNR  TYPE FAGLFLEXA-DOCNR,
        RBUKRS TYPE FAGLFLEXA-RBUKRS,
        DOCLN  TYPE FAGLFLEXA-DOCLN,
        PRCTR  TYPE FAGLFLEXA-PRCTR,
        END OF GTY_FAGL1.

DATA : GIT_FAGLFLEXA1 TYPE TABLE OF GTY_FAGL1,
      GS_FAGLFLEXA1 TYPE GTY_FAGL1.

FIELD-SYMBOLS : <FS_FAGL1> TYPE FAGLFLEXA.
************************created by prabhu on 02.04.2020  Ends******************

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS :  DD_KTOKK FOR LFA1-KTOKK,
                  DD_LIFNR FOR LFA1-LIFNR,
                  DD_SORTL FOR LFA1-SORTL MATCHCODE OBJECT ZSORTL NO-DISPLAY,
                  DD_VTYPE FOR LFA1-NAME4 MATCHCODE OBJECT ZNAME4,
                  DD_BUKRS FOR BSIK-BUKRS,
                  DD_GJAHR FOR LFC1-GJAHR,
                  BUDAT    FOR BSIK-BUDAT,
                  NETDT    FOR BSIK-CPUDT.
SELECTION-SCREEN END OF BLOCK B1.


SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-005.
PARAMETERS DD_STIDA TYPE RFPDO-ALLGSTID DEFAULT SY-DATUM.        "created By Prabhu on 31.03.2020
SELECTION-SCREEN END OF BLOCK B3.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(30) TEXT-002 FOR FIELD RASTBIS1.
SELECTION-SCREEN POSITION POS_LOW.
**PARAMETERS: RASTBIS1 LIKE RFPDO1-ALLGROGR DEFAULT '000'.
**PARAMETERS: RASTBIS2 LIKE RFPDO1-ALLGROGR DEFAULT '020'.
**PARAMETERS: RASTBIS3 LIKE RFPDO1-ALLGROGR DEFAULT '040'.
**PARAMETERS: RASTBIS4 LIKE RFPDO1-ALLGROGR DEFAULT '080'.
**PARAMETERS: RASTBIS5 LIKE RFPDO1-ALLGROGR DEFAULT '100'.
*******************created by prabhu on 31.03.2020 starts****************
PARAMETERS: RASTBIS1 LIKE RFPDO1-ALLGROGR DEFAULT '001'. "commented on 28.04.2020
PARAMETERS: RASTBIS2 LIKE RFPDO1-ALLGROGR DEFAULT '015'.
PARAMETERS: RASTBIS3 LIKE RFPDO1-ALLGROGR DEFAULT '030'.
PARAMETERS: RASTBIS4 LIKE RFPDO1-ALLGROGR DEFAULT '060'.
PARAMETERS: RASTBIS5 LIKE RFPDO1-ALLGROGR DEFAULT '090'.
PARAMETERS: RASTBIS6 LIKE RFPDO1-ALLGROGR DEFAULT '120'.
PARAMETERS: RASTBIS7 LIKE RFPDO1-ALLGROGR DEFAULT '180'.
PARAMETERS: RASTBIS8 LIKE RFPDO1-ALLGROGR DEFAULT '365'.
*******************created by prabhu on 31.03.2020 Ends****************
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK PROFIT WITH FRAME TITLE TEXT-004.
SELECT-OPTIONS: PROFIT FOR BSEG-PRCTR.
SELECTION-SCREEN END OF BLOCK PROFIT.
