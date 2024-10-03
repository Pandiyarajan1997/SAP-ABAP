REPORT ZRM07MLBD_STOCK NO STANDARD PAGE HEADING MESSAGE-ID M7 LINE-SIZE 95.
*ENHANCEMENT-POINT RM07MLBD_G4 SPOTS ES_RM07MLBD STATIC.
*ENHANCEMENT-POINT RM07MLBD_G5 SPOTS ES_RM07MLBD.
*ENHANCEMENT-POINT RM07MLBD_G6 SPOTS ES_RM07MLBD STATIC.
*ENHANCEMENT-POINT RM07MLBD_G7 SPOTS ES_RM07MLBD.

************************************************************************
*     REPORT RM07MLBD   (Transaktionscode MB5B)                        *
************************************************************************



TYPE-POOLS:  IMREP,                   " Typen Bestandsführungsreporting
             SLIS.                    " Typen Listviewer

* allow the interactions 'Specify drill-down' etc..         "n890109
TYPE-POOLS : KKBLO.          "Korrektur ALV                 "n890109

"DATA : DBCON(20) TYPE C .

INCLUDE:  ZRM07MLDD.     " reportspezifische Datendefinitionen

* controls the "expensive" checks like authorization, etc.  "n878753
DATA : G_FLAG_LAUNCHED(01)   TYPE  C.                       "n878753

* working fields for the performance improvements           "n921165
DATA : G_FLAG_DB_PARAMETERS(01)        TYPE  C,             "n921165
       G_F_DATABASE(03)      TYPE  C,                       "n921165
                                                            "n921165
       G_CNT_RADIO           TYPE  I,                       "n921165
       G_CNT_ERROR_DBA       TYPE  I.                       "n921165
                                                            "n921165
DATA : G_TABIX_SET           TYPE  SY-TABIX,                "n921165
       G_FLAG_SORTED         TYPE  C.                       "n921165
                                                            "n921165
* these flags allow to ignore multiple stops at dynamic     "n921165
* BREAK-POINTs in LOOPs                                     "n921165
DATA : BEGIN OF G_FLAG_BREAK,                               "n921165
         B1(01)              TYPE  C   VALUE 'X',           "n921165
         B2(01)              TYPE  C   VALUE 'X',           "n921165
         B3(01)              TYPE  C   VALUE 'X',           "n921165
         B4(01)              TYPE  C   VALUE 'X',           "n921165
         B5(01)              TYPE  C   VALUE 'X',           "n921165
         B6(01)              TYPE  C   VALUE 'X',           "n921165
         B7(01)              TYPE  C   VALUE 'X',           "n921165
         B8(01)              TYPE  C   VALUE 'X',           "n921165
       END OF G_FLAG_BREAK.                                 "n921165

DATA:  D_FROM(10) TYPE C,                                   "n1117067
       D_TO(10)   TYPE C.                                   "n1117067

DATA:  G_F_MSEGEX_ACT(1) TYPE C.                            "n1558298

*----------------- note 1481757 typedefinition for error-messages-------*

TYPES: BEGIN OF MBARC_MESSAGE,                              "n1481757
         MSGID LIKE SY-MSGID,                               "n1481757
         MSGNO LIKE SY-MSGNO,                               "n1481757
         MSGV1 LIKE SY-MSGV1,                               "n1481757
         MSGV2 LIKE SY-MSGV2,                               "n1481757
         MSGV3 LIKE SY-MSGV3,                               "n1481757
         MSGV4 LIKE SY-MSGV4,                               "n1481757
       END OF MBARC_MESSAGE.                                "n1481757
TYPES: MBARC_MESSAGE_TAB TYPE MBARC_MESSAGE OCCURS 0.       "n1481757
DATA: ARCHIVE_MESSAGES  TYPE MBARC_MESSAGE_TAB WITH HEADER LINE, "n1481757
      G_FLAG_ANSWER(01)     TYPE  C.                        "n1481757

*----------end of note 1481757 typedefinition for error-messages------*

DATA: GV_SWITCH_EHP6RU TYPE BOOLE_D.

DATA:      DBCON           TYPE DBCON_NAME,                        "n1710850
           DBCON_ACTIVE    TYPE DBCON_NAME.                        "n1710850
CONSTANTS: C_HDB_DBCON_GET TYPE FUNCNAME VALUE 'MM_HDB_DBCON_GET' , "n1710850
           C_HDB_SUBAPPL   TYPE PROGRAM  VALUE 'MB5B'.             "n1710850



*---------------------------------------------------------------------------------------------------------------------
*  TYPES AND DATA DECLARATION TO PRODUCE PLANT DESCRIPTION AND OLD MATERIAL NUMBER "ADDED BY PHILIP"
*----------------------------------------------------------------------------------------------------------------------

TYPES: BEGIN OF TY_MARA,
           MATNR TYPE MARA-MATNR,
           BISMT TYPE MARA-BISMT,
           MTART TYPE MARA-MTART,
           MATKL TYPE MARA-MATKL,
           SPART TYPE MARA-SPART,
           VOLUM TYPE MARA-VOLUM,
      END OF TY_MARA.

TYPES: BEGIN OF TY_T001W_NAME1,
       WERKS TYPE T001W-WERKS,
       NAME1 TYPE T001W-NAME1,
END OF TY_T001W_NAME1.

TYPES : BEGIN OF TY_TSPAT,
        SPART TYPE TSPAT-SPART,
        VTEXT TYPE TSPAT-VTEXT,
        END OF TY_TSPAT.


TYPES: BEGIN OF TY_T023T,
       MATKL TYPE T023T-MATKL,
       WGBEZ TYPE T023T-WGBEZ,
    END OF TY_T023T.

TYPES: BEGIN OF TY_T134T_MTBEZ,
 SPRAS TYPE T134T-SPRAS,
 MTART TYPE T134T-MTART,
 MTBEZ TYPE T134T-MTBEZ,
END OF TY_T134T_MTBEZ.

TYPES : BEGIN OF TY_MARC,
        MATNR TYPE MARC-MATNR,
        WERKS TYPE MARC-WERKS,
        TRAME TYPE MARC-TRAME,
        UMLMC TYPE MARC-UMLMC,
        BWESB TYPE  MARC-BWESB,
        END OF TY_MARC.

TYPES : BEGIN OF TY_MARD,
        MATNR TYPE MARD-MATNR,
        WERKS TYPE MARD-WERKS,
        SPEME TYPE MARD-SPEME,
        LGORT TYPE MARD-LGORT,
        LFGJA TYPE MARD-LFGJA,
        END OF TY_MARD.

TYPES : BEGIN OF TY_T001L,
        WERKS TYPE T001L-WERKS,
        LGORT TYPE T001L-LGORT,
        LGOBE TYPE T001L-LGOBE,
        END OF TY_T001L.

DATA: GT_MARA TYPE STANDARD TABLE OF TY_MARA,
      GS_MARA LIKE LINE OF GT_MARA,
      GT_T001W_NAME TYPE STANDARD TABLE OF TY_T001W_NAME1,
      GS_T001W_NAME LIKE LINE OF GT_T001W_NAME,
      GT_TSPAT TYPE STANDARD TABLE OF TY_TSPAT,
      GS_TSPAT LIKE LINE OF GT_TSPAT,
      GT_T001L TYPE STANDARD TABLE OF TY_T001L,
      GS_T001L LIKE LINE OF GT_T001L,
      GT_MARC TYPE STANDARD TABLE OF TY_MARC,
      GS_MARC LIKE LINE OF GT_MARC,
      GT_MARD TYPE STANDARD TABLE OF TY_MARD,
      GS_MARD LIKE LINE OF GT_MARD,
      GT_T023T TYPE STANDARD TABLE OF TY_T023T,
      GS_T023T LIKE LINE OF GT_T023T,
      GT_T134T_MTBEZ TYPE STANDARD TABLE OF TY_T134T_MTBEZ,
      GS_T134T_MTBEZ LIKE LINE OF GT_T134T_MTBEZ,
      GT_LIST     TYPE VRM_VALUES,
      GWA_LIST    TYPE VRM_VALUE,
      GT_VALUES   TYPE TABLE OF DYNPREAD,                     " INTERNAL TABLE FOR LIST BOX
      GWA_VALUES  TYPE DYNPREAD.                              " WORK AREA FOR LIST BOX


DATA : SUM_VOLUM TYPE MARA-VOLUM,
       SUM_OQTY TYPE MARA-VOLUM,
       SUM_RQTY TYPE MARA-VOLUM,
       SUM_IQTY TYPE MARA-VOLUM,
       TRANS_VAL TYPE P DECIMALS 3,
       BLOCK_VAL TYPE  MBEW-SALK3.


*-----------------------------------------------------------"n571473
* define the selection screen here                          "n571473
*-----------------------------------------------------------"n571473


*SELECTION-SCREEN: BEGIN OF BLOCK B1.
*
*PARAMETERS: CATGORY AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND ABC MODIF ID TB1.            " SELECTION SCREEN PARAMETER FOR INVOICE AND CUSTOMER BALANCES
*
*SELECTION-SCREEN: END OF BLOCK B1.


SELECTION-SCREEN BEGIN OF BLOCK DATABASE-SELECTION
          WITH FRAME TITLE TEXT-001.
*  Text-001: Datenbankabgrenzungen
SELECT-OPTIONS: MATNR FOR MARD-MATNR MEMORY ID MAT
                                     MATCHCODE OBJECT MAT1.
IF cl_immpn_cust=>check_mpn_active( ) = abap_true.
  SELECT-OPTIONS: mfrpn FOR mara-mfrpn MEMORY ID mpn MATCHCODE OBJECT htn.
ENDIF.

*ENHANCEMENT-POINT RM07MLBD_01 SPOTS ES_RM07MLBD STATIC.
SELECT-OPTIONS:
                BUKRS FOR T001-BUKRS  MEMORY ID BUK,
                HKONT FOR BSEG-HKONT  MODIF  ID HKT,
                SPART FOR MARA-SPART,
                WERKS FOR T001W-WERKS MEMORY ID WRK,
                MATKL FOR MARA-MATKL,
                LGORT FOR T001L-LGORT,
                CHARG FOR MCHB-CHARG,
                BWTAR FOR MBEW-BWTAR,
                BWART FOR MSEG-BWART.
PARAMETERS SOBKZ LIKE MSEG-SOBKZ DEFAULT '' NO-DISPLAY.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: DATUM FOR MKPF-BUDAT NO-EXTENSION.


*  Datumsintervall für Selektion
SELECTION-SCREEN END OF BLOCK DATABASE-SELECTION.


*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BESTANDSART
WITH FRAME TITLE TEXT-002.
*  Text-002: Bestandsart

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS LGBST LIKE AM07M-LGBST RADIOBUTTON GROUP BART MODIF ID SB2.
SELECTION-SCREEN COMMENT 4(50) TEXT-010 FOR FIELD LGBST MODIF ID SB2.
*  Text-010: Lagerort-/Chargenbestand
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS BWBST LIKE AM07M-BWBST RADIOBUTTON GROUP BART MODIF ID SB2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(50) TEXT-011 FOR FIELD BWBST MODIF ID SB2.
*  Text-011: bewerteter Bestand
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS SBBST LIKE AM07M-SBBST RADIOBUTTON GROUP BART MODIF ID SB2 .
SELECTION-SCREEN COMMENT 4(50) TEXT-012 FOR FIELD SBBST MODIF ID SB2.
*  Text-012: Sonderbestand
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BESTANDSART.

* improved definition of parameters for scope of list       "n599218

SELECTION-SCREEN BEGIN OF BLOCK LISTUMFANG
  WITH FRAME TITLE TEXT-003.  "Listumfang

* the following 3 parameters became obsolete do not use     "n599218
* anymor. They are still here to inform the user about      "n599218
* that he is using old variants or SUBMIT commands          "n599218
PARAMETERS :                                                "n599218
  XONUL  LIKE AM07M-XONUL            NO-DISPLAY,            "n599218
  XVBST  LIKE AM07M-XVBST            NO-DISPLAY,            "n599218
  XNVBST LIKE AM07M-XNVBS            NO-DISPLAY.            "n599218

* 7 new categories for the scope of list                    "n599218
*                                                           "n599218
* cat. I docs I stock on   I    I stock on I Parameter      "n599218
*      I      I start date I    I end date I                "n599218
* -----+------+------------+----+----------+----------      "n599218
*  1   I yes  I =  zero    I =  I =  zero  I pa_wdzer       "n599218
*  2   I yes  I =  zero    I <> I <> zero  I pa_wdzew       "n599218
*  3   I yes  I <> zero    I <> I =  zero  I pa_wdwiz       "n599218
*  4   I yes  I <> zero    I <> I <> zero  I pa_wdwuw       "n599218
*  5   I yes  I <> zero    I =  I <> zero  I pa_wdwew       "n599218
*      I      I            I    I          I                "n599218
*  6   I no   I =  zero    I =  I =  zero  I pa_ndzer       "n599218
*  7   I no   I <> zero    I =  I <> zero  I pa_ndsto       "n599218
*                                                           "n599218
* definition of the pushbutton : show or hide the following "n599218
* parameters for the scope of list                          "n599218
SELECTION-SCREEN PUSHBUTTON /1(20) PB_LIU                   "n599218
                           USER-COMMAND LIU MODIF ID LIU.   "n599218
                                                            "n599218
* text line : materials with movements                      "n599218
SELECTION-SCREEN BEGIN OF LINE.                             "n599218
SELECTION-SCREEN COMMENT 1(55) TEXT-072                     "n599218
                         MODIF ID LIU.                      "n599218
SELECTION-SCREEN END OF LINE.                               "n599218
                                                            "n599218
* with movements / start = zero  =  end = zero              "n599218
*  1   I yes  I =  zero    I =  I =  zero  I pa_wdzer       "n599218
SELECTION-SCREEN BEGIN OF LINE.                             "n599218
SELECTION-SCREEN POSITION 2.                                "n599218
PARAMETERS : PA_WDZER    LIKE AM07M-MB5B_XONUL              "n599218
                         MODIF ID LIU.                      "n599218
*   text-083 : no opening stock ; no closing stock          "n599218
SELECTION-SCREEN COMMENT 5(70) TEXT-083                     "n599218
                         FOR FIELD PA_WDZER                 "n599218
                         MODIF ID LIU.                      "n599218
SELECTION-SCREEN END OF LINE.                               "n599218
                                                            "n599218
* with movements / start = zero  =  end <> zero             "n599218
*  2   I yes  I =  zero    I <> I <> zero  I pa_wdzew       "n599218
SELECTION-SCREEN BEGIN OF LINE.                             "n599218
SELECTION-SCREEN POSITION 2.                                "n599218
PARAMETERS : PA_WDZEW    LIKE AM07M-MB5B_XONUL              "n599218
                         MODIF ID LIU.                      "n599218
*   text-084 : no opening stock ; with closing stock        "n599218
SELECTION-SCREEN COMMENT 5(70) TEXT-084                     "n599218
                         FOR FIELD PA_WDZEW                 "n599218
                         MODIF ID LIU.                      "n599218
SELECTION-SCREEN END OF LINE.                               "n599218
                                                            "n599218
* with movements / start stock <> 0 / end stock = 0         "n599218
*  3   I yes  I <> zero    I <> I =  zero  I pa_wdwiz       "n599218
SELECTION-SCREEN BEGIN OF LINE.                             "n599218
SELECTION-SCREEN POSITION 2.                                "n599218
PARAMETERS : PA_WDWIZ    LIKE AM07M-MB5B_XONUL              "n599218
                         MODIF ID LIU.                      "n599218
*   text-085 : with opening stock ; no closing stock        "n599218
SELECTION-SCREEN COMMENT 5(70) TEXT-085                     "n599218
                         FOR FIELD PA_WDWIZ                 "n599218
                         MODIF ID LIU.                      "n599218
SELECTION-SCREEN END OF LINE.                               "n599218
                                                            "n599218
* with movements / with start and end stocks / different    "n599218
*  4   I yes  I <> zero    I <> I <> zero  I pa_wdwuw       "n599218
SELECTION-SCREEN BEGIN OF LINE.                             "n599218
SELECTION-SCREEN POSITION 2.                                "n599218
PARAMETERS : PA_WDWUW    LIKE AM07M-MB5B_XONUL              "n599218
                         MODIF ID LIU.                      "n599218
*   with opening stock ; with closing stock ; changed       "n599218
SELECTION-SCREEN COMMENT 5(70) TEXT-086                     "n599218
                         FOR FIELD PA_WDWUW                 "n599218
                         MODIF ID LIU.                      "n599218
SELECTION-SCREEN END OF LINE.                               "n599218
                                                            "n599218
* with movements / with start and end stock / equal         "n599218
*  5   I yes  I <> zero    I =  I <> zero  I pa_wdwew       "n599218
SELECTION-SCREEN BEGIN OF LINE.                             "n599218
SELECTION-SCREEN POSITION 2.                                "n599218
PARAMETERS : PA_WDWEW    LIKE AM07M-MB5B_XONUL              "n599218
                         MODIF ID LIU.                      "n599218
*   with opening stock ; with closing stock ; non-changed   "n599218
SELECTION-SCREEN COMMENT 5(70) TEXT-087                     "n599218
                         FOR FIELD PA_WDWEW                 "n599218
                         MODIF ID LIU.                      "n599218
SELECTION-SCREEN END OF LINE.                               "n599218
                                                            "n599218
* text line : materials without movements                   "n599218
SELECTION-SCREEN BEGIN OF LINE.                             "n599218
SELECTION-SCREEN COMMENT 1(55) TEXT-073                     "n599218
                         MODIF ID LIU.                      "n599218
SELECTION-SCREEN END OF LINE.                               "n599218
                                                            "n599218
* materials without movements / stocks = zero               "n599218
*  6   I no   I =  zero    I =  I =  zero  I pa_ndzer       "n599218
SELECTION-SCREEN BEGIN OF LINE.                             "n599218
SELECTION-SCREEN POSITION 2.                                "n599218
PARAMETERS : PA_NDZER    LIKE AM07M-MB5B_XONUL              "n599218
                         MODIF ID LIU.                      "n599218
*   text-083 : no opening stock ; no closing stock          "n599218
SELECTION-SCREEN COMMENT 5(70) TEXT-083                     "n599218
                         FOR FIELD PA_NDZER                 "n599218
                         MODIF ID LIU.                      "n599218
SELECTION-SCREEN END OF LINE.                               "n599218
                                                            "n599218
* materials without movements / with start or end stock     "n599218
*  7   I no   I <> zero    I =  I <> zero  I pa_ndsto       "n599218
SELECTION-SCREEN BEGIN OF LINE.                             "n599218
SELECTION-SCREEN POSITION 2.                                "n599218
PARAMETERS : PA_NDSTO    LIKE AM07M-MB5B_XONUL              "n599218
                         MODIF ID LIU.                      "n599218
*   with opening stock ; with closing stock ; non-changed   "n599218
SELECTION-SCREEN COMMENT 5(70) TEXT-087                     "n599218
                         FOR FIELD PA_NDSTO                 "n599218
                         MODIF ID LIU.                      "n599218
SELECTION-SCREEN END OF LINE.                               "n599218
                                                            "n599218
SELECTION-SCREEN END OF BLOCK LISTUMFANG.

*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK EINSTELLUNGEN
   WITH FRAME TITLE TEXT-068.  "Settings

* parameter for totals only - hierseq. list
* corresponding display variant
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS XSUM          LIKE AM07M-XSUM MODIF ID SB3.
SELECTION-SCREEN COMMENT 4(60) TEXT-090 FOR FIELD XSUM MODIF ID SB3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(30) TEXT-091 FOR FIELD PA_SUVAR MODIF ID SB3.
SELECTION-SCREEN POSITION 40.
PARAMETERS: PA_SUVAR LIKE DISVARIANT-VARIANT MODIF ID SB3.
SELECTION-SCREEN END OF LINE.

* parameter for totals only - flat list + corresponding display variant
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS PA_SUMFL LIKE AM07M-XSUM DEFAULT 'X' MODIF ID SB3.
SELECTION-SCREEN COMMENT 4(60) TEXT-092 FOR FIELD PA_SUMFL MODIF ID SB3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(30) TEXT-091 FOR FIELD PA_SFLVA MODIF ID SB3.
SELECTION-SCREEN POSITION 40.
PARAMETERS: PA_SFLVA LIKE DISVARIANT-VARIANT MODIF ID SB3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS XCHAR LIKE AM07M-XCHRG MODIF ID SB3.
SELECTION-SCREEN COMMENT 4(50) TEXT-015 FOR FIELD XCHAR MODIF ID SB3.
*  Text-015: nur chargenpflichtige Materialien
*  Das Kennzeichen 'xchar' bestimmt die Art der Listausgabe entweder
*  auf Material- oder Chargenebene.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.                             "838360_v
SELECTION-SCREEN POSITION 4.
PARAMETERS XNOMCHB LIKE AM07M-MB5B_XNOMCHB MODIF ID SB3.
SELECTION-SCREEN COMMENT 6(50) TEXT-089 FOR FIELD XNOMCHB MODIF ID SB3.
*  Text-089: Auch Chargen ohne Bestandssegment
SELECTION-SCREEN END OF LINE.                               "838360_^

* the function "No reversal movements" is only         "n571473
* available from relaese 4.5B and higher               "n571473
* ( TEXT-026 : No reversal movements )                 "n571473
SELECTION-SCREEN BEGIN OF LINE.                             "n571473
SELECTION-SCREEN POSITION 1.                                "n571473
PARAMETERS NOSTO LIKE AM07M-NOSTO MODIF ID SB3.             "n571473
SELECTION-SCREEN COMMENT 4(50) TEXT-026                     "n571473
                       FOR FIELD NOSTO MODIF ID SB3.        "n571473
SELECTION-SCREEN END OF LINE.                               "n571473

SELECTION-SCREEN END OF BLOCK EINSTELLUNGEN.

*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK LISTE WITH FRAME TITLE TEXT-040.
PARAMETERS: P_VARI LIKE DISVARIANT-VARIANT MODIF ID SB5.
SELECTION-SCREEN END OF BLOCK LISTE.

*----------------------------------------------------------------------*

* with these new parameters allow the user to determine     "n921165
* the best database access; these parameters will appear    "n921165
* only when the installed database system is :              "n921165
* DB6, Informix, or MaxDB                                   "n921165
                                                            "n921165
* define database access for best runtime                   "n921165
SELECTION-SCREEN BEGIN OF BLOCK DB                          "n921165
                             WITH FRAME TITLE TEXT-111.     "n921165
                                                            "n921165
* Database determines best access                           "n921165
SELECTION-SCREEN : BEGIN OF LINE.                           "n921165
PARAMETERS : PA_DBSTD    LIKE  AM07M-XSELK                  "n921165
                         MODIF ID DBA                       "n921165
                         DEFAULT 'X'                        "n921165
                         RADIOBUTTON GROUP DB.              "n921165
SELECTION-SCREEN : COMMENT 3(70) TEXT-112                   "n921165
                         FOR FIELD PA_DBSTD                 "n921165
                         MODIF ID DBA.                      "n921165
SELECTION-SCREEN : END OF LINE.                             "n921165
                                                            "n921165
* Access via Material number                                "n921165
SELECTION-SCREEN : BEGIN OF LINE.                           "n921165
PARAMETERS : PA_DBMAT    LIKE  AM07M-XSELK                  "n921165
                         MODIF ID DBA                       "n921165
                         RADIOBUTTON GROUP DB.              "n921165
SELECTION-SCREEN : COMMENT 3(70) TEXT-113                   "n921165
                         FOR FIELD PA_DBMAT                 "n921165
                         MODIF ID DBA.                      "n921165
SELECTION-SCREEN : END OF LINE.                             "n921165
                                                            "n921165
* Access via Posting Date                                   "n921165
SELECTION-SCREEN : BEGIN OF LINE.                           "n921165
PARAMETERS : PA_DBDAT    LIKE  AM07M-XSELK                  "n921165
                         MODIF ID DBA                       "n921165
                         RADIOBUTTON GROUP DB.              "n921165
SELECTION-SCREEN : COMMENT 3(70) TEXT-114                   "n921165
                         FOR FIELD PA_DBDAT                 "n921165
                         MODIF ID DBA.                      "n921165
SELECTION-SCREEN : END OF LINE.                             "n921165
                                                            "n921165
SELECTION-SCREEN END OF BLOCK DB.                           "n921165

*------------------------ begin of note 1481757 ------------------------*
*---------- selection-sreen for archive --------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK ARCH WITH FRAME TITLE TEXT-131. "n1481757
                                                            "n1481757
* create checkbox on the screen                                 "n1481757
                                                            "n1481757
PARAMETERS: ARCHIVE  TYPE MB5B_ARCHIVE AS CHECKBOX DEFAULT ' ' "n1481757
                             USER-COMMAND US_ARCHIVE MODIF ID SB4. "n1481757
                                                            "n1481757
*  parameter for the archive info structure                     "n1481757
PARAMETERS : PA_AISTR    LIKE AIND_STR1-ARCHINDEX MODIF ID SB4. "n1481757
                                                            "n1481757
SELECTION-SCREEN END OF BLOCK ARCH.                         "n1481757

* -------------------- end of selection-sreen for archive---------------*

* ------------------- F4-Help --------- get info-structure -------------*
* datadefinition                                               "n1481757
DATA: G_F_F4_MODE(01) TYPE  C,                              "n1481757
      G_F_F4_ARCHINDEX LIKE  AIND_STR1-ARCHINDEX.           "n1481757
                                                            "n1481757

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F4_FOR_VARIANT.

*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_SFLVA.
  PERFORM                    VARIANT_VALUE_REQUEST_F4
                             USING  PA_SFLVA  G_S_VARI_SUMFL.

*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_SUVAR.
  PERFORM                    VARIANT_VALUE_REQUEST_F4
                             USING  PA_SUVAR G_S_VARI_SUMHQ.


*-----------------------------------------------------------"n599218
* INITIALIZATION                                            "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218
* pay attentions : this process time will not be processed  "n599218
* in the webreporting mode                                  "n599218

INITIALIZATION.

************************* INITIALIZATION CODE**************************"ADDED BY Govind

  %_SPART_%_APP_%-TEXT = 'Division'.
  %_MATKL_%_APP_%-TEXT = 'Material Group'.

  CLEAR : G_S_VARI_SUMHQ, G_S_VARI_SUMFL.
  REPID = SY-REPID.
  VARIANT_SAVE = 'A'.

*ENHANCEMENT-POINT RM07MLBD_03 SPOTS ES_RM07MLBD.
* preprae the working areas for the variants
  MOVE  : REPID              TO  G_S_VARI_SUMHQ-REPORT,
          'SUHQ'             TO  G_S_VARI_SUMHQ-HANDLE,
          REPID              TO  G_S_VARI_SUMFL-REPORT,
          'SUFL'             TO  G_S_VARI_SUMFL-HANDLE.

  MOVE-CORRESPONDING : G_S_VARI_SUMHQ  TO  G_S_VARI_SUMHQ_DEF,
                       G_S_VARI_SUMFL  TO  G_S_VARI_SUMFL_DEF.

  PERFORM  GET_THE_DEFAULT_VARIANT
                             USING  PA_SFLVA
                                    G_S_VARI_SUMFL
                                    G_S_VARI_SUMFL_DEF.

  PERFORM  GET_THE_DEFAULT_VARIANT
                             USING  PA_SUVAR
                                    G_S_VARI_SUMHQ
                                    G_S_VARI_SUMHQ_DEF.

  PERFORM INITIALISIERUNG.

* get the parameters from the last run                      "n547170
  PERFORM                    ESDUS_GET_PARAMETERS.          "n547170

* set flag when INITILIZATION is processed
  MOVE  'X'        TO  G_FLAG_INITIALIZATION.

* check switch FIN_LOCRU_SFWS_UI_02 activation
  GV_SWITCH_EHP6RU = CL_FIN_LOCRU_SWITCH_CHECK=>FIN_LOCRU_SFWS_UI_02( ).

*-----------------------------------------------------------"n599218
* AT SELECTION-SCREEN                                       "n599218
*-----------------------------------------------------------"n599218

*----------- Prüfung der eingegebenen Selektionsparameter, ------------*
*---------------------- Berechtigungsprüfung --------------------------*

  LOOP AT SCREEN.

    IF SCREEN-GROUP1 = 'TB1'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.


AT SELECTION-SCREEN.

* the user will get the info about the old variant only     "n921165
* once                                                      "n921165
  IF  G_CNT_ERROR_DBA = 1.                                  "n921165
    IF  NOT SY-SLSET IS INITIAL.                            "n921165
*     Variant & of program & is not the current version     "n921165
      MESSAGE I634(DB)       WITH  SY-SLSET SY-REPID.       "n921165
    ENDIF.                                                  "n921165
  ENDIF.                                                    "n921165
                                                            "n921165
* if the installed database system is not DB6, Informix or  "n921165
* MAxDB the parameter PA_DBSTD must be set                  "n921165
  IF  G_FLAG_DB_PARAMETERS IS INITIAL.                      "n921165
    IF  PA_DBSTD =  'X'      AND                            "n921165
        PA_DBMAT IS INITIAL  AND                            "n921165
        PA_DBDAT IS INITIAL.                                "n921165
*     ok                                                    "n921165
    ELSE.                                                   "n921165
*     send warning when the user applied an old variant     "n921165
      IF  NOT SY-SLSET IS INITIAL.                          "n921165
        ADD  1               TO  G_CNT_ERROR_DBA.           "n921165
*       Variant & of program & is not the current version   "n921165
        MESSAGE I634(DB)     WITH  SY-SLSET SY-REPID.       "n921165
      ENDIF.                                                "n921165
    ENDIF.                                                  "n921165
  ENDIF.                                                    "n921165
                                                            "n921165
* check choosen database access agaist restrictions         "n921165
* text-095 : Mismatch Database access - restriction         "n921165
  IF  G_FLAG_DB_PARAMETERS = 'X'.                           "n921165
                                                            "n921165
    IF      PA_DBMAT = 'X'.                                 "n921165
*     access via material number : material entered ?       "n921165
      IF  MATNR[] IS INITIAL.                               "n921165
        SET CURSOR         FIELD  PA_DBMAT.                 "n921165
        MESSAGE  W895      WITH  TEXT-115.                  "n921165
      ENDIF.                                                "n921165
                                                            "n921165
    ELSEIF  PA_DBDAT = 'X'.                                 "n921165
*     access via posting data : posting date entered ?      "n921165
      IF  DATUM-LOW  IS INITIAL AND                         "n921165
          DATUM-HIGH IS INITIAL.                            "n921165
        SET CURSOR         FIELD  PA_DBDAT.                 "n921165
        MESSAGE  W895      WITH  TEXT-115.                  "n921165
      ENDIF.                                                "n921165
                                                            "n921165
    ENDIF.                                                  "n921165
  ENDIF.                                                    "n921165

* the following 3 parameters XONUL, XVBST, and XNVBST       "n599218
* became obsolete; send error when they should be filled.   "n599218
* This could be possible if the user works with old         "n599218
* selection variants or this report is launched by a        "n599218
* SUBMIT command                                            "n599218
  IF  XONUL  IS INITIAL  AND                                "n599218
      XVBST  IS INITIAL  AND                                "n599218
      XNVBST IS INITIAL.                                    "n599218
*  ok, the old parameters are empty                         "n599218
  ELSE.                                                     "n599218
*   text-088 : note 599218 : obsolete parameter used        "n599218
    MESSAGE E895             WITH  TEXT-088.                "n599218
  ENDIF.

* did the user hit the pushbutton "Category" ?              "n599218
  CASE     SSCRFIELDS-UCOMM.                                "n599218
    WHEN  'LIU '.                                           "n599218
*     yes, the pushbutton "Category" was hit                "n599218
      IF  G_FLAG_STATUS_LIU  =  C_HIDE.                     "n599218
*       show the 7 parameters on the selection srceen       "n599218
        MOVE  C_SHOW         TO  G_FLAG_STATUS_LIU.         "n599218
      ELSE.                                                 "n599218
*       hide the 7 paramaters                               "n599218
        MOVE  C_HIDE         TO  G_FLAG_STATUS_LIU.         "n599218
      ENDIF.                                                "n599218
  ENDCASE.                                                  "n599218

* carry out the "expensive" checks, like authorization,     "n878753
* only after the user wants to launch this report. In the   "n878753
* case an error message was sent the user can correct the   "n878753
* entries and go on with "ENTER". That means the system     "n878753
* field SY-UCOMM is initial. This correction should make    "n878753
* sure that all checks are done when this report is         "n878753
* launched.                                                 "n878753
  IF  SY-UCOMM = 'ONLI'  OR                                 "n878753
      SY-UCOMM = 'PRIN'  OR                                 "n878753
      SY-UCOMM = 'SJOB'.                                    "n878753
    MOVE  'X'                TO  G_FLAG_LAUNCHED.           "n878753
  ENDIF.                                                    "n878753
                                                            "n878753
  CHECK : G_FLAG_LAUNCHED = 'X'.                            "n878753

* only one sum function can be processed
  IF  XSUM     = 'X' AND
      PA_SUMFL = 'X'.
    SET CURSOR               FIELD 'XSUM'.
*   select one sum list only
    MESSAGE  E895            WITH  TEXT-093.
  ENDIF.

  PERFORM EINGABEN_PRUEFEN.

  SET CURSOR                 FIELD 'PA_SFLVA'.
  PERFORM  VARIANT_CHECK_EXISTENCE
                             USING     PA_SFLVA
                                       G_S_VARI_SUMFL
                                       G_S_VARI_SUMFL_DEF.

  SET CURSOR                 FIELD 'PA_SUVAR'.
  PERFORM  VARIANT_CHECK_EXISTENCE
                             USING     PA_SUVAR
                                       G_S_VARI_SUMHQ
                                       G_S_VARI_SUMHQ_DEF.

* check whether FI summarization is active and other        "n547170
* restrictions could deliver wrong results                  "n547170
  PERFORM                    F0800_CHECK_RESTRICTIONS.      "n547170

* - the user wants to surpress the reversal movements :     "n497992
*   process warning M7 392                                  "n497992
  IF NOT NOSTO IS INITIAL.                                  "n497992
*   emerge warning ?                                        "n497992
    CALL FUNCTION            'ME_CHECK_T160M'               "n497992
        EXPORTING                                           "n497992
          I_ARBGB          = 'M7'                           "n497992
          I_MSGNR          = '392'                          "n497992
        EXCEPTIONS                                          "n497992
          NOTHING          = 0                              "n497992
          OTHERS           = 1.                             "n497992
                                                            "n497992
    IF SY-SUBRC <> 0.                                       "n497992
      SET CURSOR               FIELD  'NOSTO'.              "n497992
*       to surpress the reversal movements could cause ...  "n497992
      MESSAGE                  W392.                        "n497992
    ENDIF.                                                  "n497992
  ENDIF.                                                    "n497992

* carry out special authotity check for the tax auditor     "n547170
  PERFORM                    TPC_CHECK_TAX_AUDITOR.         "n547170

* does the user wants a selection via company code or a plant ?
* fill range table g_ra_werks
  REFRESH : G_RA_BWKEY,  G_RA_WERKS, G_T_ORGAN.
  CLEAR   : G_RA_BWKEY,  G_RA_WERKS, G_T_ORGAN, G_S_ORGAN.
  REFRESH : G_0000_RA_BWKEY,  G_0000_RA_WERKS,  G_0000_RA_BUKRS.
  CLEAR   : G_0000_RA_BWKEY,  G_0000_RA_WERKS,  G_0000_RA_BUKRS.

  DESCRIBE TABLE  BUKRS      LINES  G_F_CNT_LINES_BUKRS.
  DESCRIBE TABLE  WERKS      LINES  G_F_CNT_LINES_WERKS.

  IF  G_F_CNT_LINES_BUKRS  > 0  OR
      G_F_CNT_LINES_WERKS  > 0.
*   fill range tables for the CREATION OF TABLE G_T_ORGAN
    MOVE : WERKS[]           TO  G_0000_RA_WERKS[],
           BUKRS[]           TO  G_0000_RA_BUKRS[].

    PERFORM  F0000_CREATE_TABLE_G_T_ORGAN
                             USING  C_ERROR.
  ENDIF.

* ----- begin of note "n1481757 ---- check archive-info-structure----*
  DATA: G_FLAG_EXIST_AS TYPE C.                             "n1481757
  DATA : G_FLAG_TOO_MANY_SEL(01) TYPE C.                    "n1481757
  DATA: G_V_FIELDNAME TYPE FIELDNAME.                       "n1481757
                                                            "n1481757
* process the MM docs from the new AS archive               "n1481757
  IF ARCHIVE = 'X'.                                         "n1481757
    PERFORM CHECK_EXISTENCE_AS USING G_FLAG_EXIST_AS.       "n1481757
    IF  G_FLAG_EXIST_AS = 'X'.                              "n1481757
      PERFORM CHECK_ARCHIVE_INDEX USING G_FLAG_TOO_MANY_SEL "n1481757
                                        G_V_FIELDNAME.      "n1481757
      " Materialbelege aus dem Archiv auslesen              "n1481757
      IF G_FLAG_TOO_MANY_SEL = 'X'.                         "n1481757
        MESSAGE W432  WITH  G_V_FIELDNAME PA_AISTR.         "n1481757
*     Eingrenzungen für Feld &1 wirken nicht                "n1481757
      ENDIF.                                                "n1481757
    ENDIF.                                                  "n1481757
  ENDIF.                                                    "n1481757
* ----- end of note "n1481757 ----- check archive-info-structure ---*

* save the parameters of this run                           "n547170
  PERFORM                    ESDUS_SAVE_PARAMETERS.         "n547170

*-----------------------------------------------------------"n599218
* AT SELECTION-SCREEN OUTPUT                                "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218
*
AT SELECTION-SCREEN OUTPUT.                                 "n599218


  LOOP AT SCREEN.
*
    IF  SCREEN-GROUP1 = 'LIU'.

      SCREEN-ACTIVE = '0'.
      SCREEN-INPUT  = '0'.
      MODIFY SCREEN.

    ENDIF.
*
*    IF SCREEN-GROUP1 = 'SB2'.
*      SCREEN-ACTIVE = '0'.
*      SCREEN-INPUT  = '0'.
*      MODIFY SCREEN.
*
*    ENDIF.

*    IF SCREEN-GROUP1 = 'SB3'.
*      SCREEN-ACTIVE = '0'.
*      SCREEN-INPUT  = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF SCREEN-GROUP1 = 'SB4'.
*      SCREEN-ACTIVE = '0'.
*      SCREEN-INPUT  = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*
*
*    IF SCREEN-GROUP1 = 'DBA'.
*      SCREEN-ACTIVE = '0'.
*      SCREEN-INPUT = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF SCREEN-GROUP1 = 'SB5'.
*      SCREEN-ACTIVE = '0'.
*      SCREEN-INPUT = '0'.
*      MODIFY SCREEN.
*    ENDIF.
  ENDLOOP.



* check whether the database access parameters fulfil the   "n921165
* radiobutton rules / in the case this report was launched  "n921165
* with a selection variant, the settings of this variant    "n921165
* have been set already                                     "n921165
  IF  G_FLAG_DB_PARAMETERS = 'X'.                           "n921165
    CLEAR                    G_CNT_RADIO.                   "n921165
    IF  PA_DBSTD = 'X'.  ADD 1    TO G_CNT_RADIO.  ENDIF.   "n921165
    IF  PA_DBMAT = 'X'.  ADD 1    TO G_CNT_RADIO.  ENDIF.   "n921165
    IF  PA_DBDAT = 'X'.  ADD 1    TO G_CNT_RADIO.  ENDIF.   "n921165
                                                            "n921165
    IF  G_CNT_RADIO = 1.                                    "n921165
*     ok                                                    "n921165
    ELSE.                                                   "n921165
*     offended against radiobutton rules : set default      "n921165
      ADD  1                 TO  G_CNT_ERROR_DBA.           "n921165
      MOVE : 'X'             TO  PA_DBSTD.                  "n921165
      CLEAR :                PA_DBMAT, PA_DBDAT.            "n921165
    ENDIF.                                                  "n921165
  ENDIF.                                                    "n921165

  IF  G_FLAG_INITIALIZATION IS INITIAL.                     "n599218
*   the process time INITIALIZATION was not done, so        "n599218
*   carry out the functions here                            "n599218
    MOVE  'X'                TO G_FLAG_INITIALIZATION.      "n599218
                                                            "n599218
    PERFORM                  INITIALISIERUNG.               "n599218
                                                            "n599218
*   get the parameters from the last run                    "n599218
    PERFORM                  ESDUS_GET_PARAMETERS.          "n599218
  ENDIF.                                                    "n599218
                                                            "n599218
* how to handle the 7 paramaters for the scope of list ?    "n599218
  LOOP AT SCREEN.                                           "n599218
*   modify the selection screen                             "n599218
    CASE    SCREEN-GROUP1.                                  "n599218
      WHEN  'LIU'.                                          "n599218
        IF  G_FLAG_STATUS_LIU  = C_SHOW.                    "n599218
          SCREEN-ACTIVE = '1'.         "show parameters     "n599218
        ELSE.                                               "n599218
          SCREEN-ACTIVE = '0'.         "Hide parameters     "n599218
        ENDIF.                                              "n599218
                                                            "n599218
        MODIFY SCREEN.                                      "n599218

      WHEN  'DBA'.                                          "n921165
*       show or hide the parametes for the database access  "n921165
        IF  G_FLAG_DB_PARAMETERS = 'X'.                     "n921165
          SCREEN-ACTIVE = '1'.         "show parameters     "n921165
        ELSE.                                               "n921165
          SCREEN-ACTIVE = '0'.         "Hide parameters     "n921165
        ENDIF.                                              "n921165
                                                            "n921165
        MODIFY SCREEN.                                      "n921165

      WHEN  'HKT'.
*       show or hide HKONT parameter
        IF GV_SWITCH_EHP6RU = 'X'.
          SCREEN-ACTIVE = '1'.
        ELSE.
          SCREEN-ACTIVE = '0'.
        ENDIF.
        MODIFY SCREEN.

    ENDCASE.                                                "n599218
  ENDLOOP.                                                  "n599218
                                                            "n599218
* adapt the icon on the pushbutton depending on the status  "n599218
  CASE    G_FLAG_STATUS_LIU.                                "n599218
    WHEN  C_HIDE.                                           "n599218
      MOVE  TEXT-081         TO  PB_LIU.  "@0E\Q@ Scope ... "n599218
    WHEN  C_SHOW.                                           "n599218
      MOVE  TEXT-082         TO  PB_LIU.  "@0H\Q@ Scope ... "n599218
    WHEN  OTHERS.                                           "n599218
  ENDCASE.                                                  "n599218
                                                            "n599218
*-----------------------------------------------------------"n599218

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*

START-OF-SELECTION.
DATA: gv_optimization_active TYPE abap_bool.
DATA: gv_where_clause   TYPE string,
      gv_not_authorized TYPE string.

* it makes no sence to carry out this report with an old    "n921165
* and incorrect selection variant                           "n921165
  IF  G_CNT_ERROR_DBA > 0.                                  "n921165
    IF  NOT SY-SLSET IS INITIAL.                            "n921165
*     Variant & of program & is not the current version     "n921165
      MESSAGE E634(DB)       WITH  SY-SLSET SY-REPID.       "n921165
    ENDIF.                                                  "n921165
  ENDIF.                                                    "n921165

* create the title line

* If no date is given at all, the range is set to the maximum
* extend (1.1.0000 - 31.12.9999).
* If only datum-low is set, it is interpreted as the day for
* which the analysis is wanted --> datum-high is filled up.
  IF DATUM-LOW IS INITIAL.
    DATUM-LOW = '00000101'.
    IF DATUM-HIGH IS INITIAL.
      DATUM-HIGH = '99991231'.
    ENDIF.
  ELSE.
    IF DATUM-HIGH IS INITIAL.
      DATUM-HIGH = DATUM-LOW.
    ENDIF.
  ENDIF.
*  Begin of changes of note 1117067                        "n1117067
*  MOVE: datum-low(4)    TO jahrlow,
*        datum-low+4(2)  TO monatlow,
*        datum-low+6(2)  TO taglow,
*        datum-high(4)   TO jahrhigh,
*        datum-high+4(2) TO monathigh,
*        datum-high+6(2) TO taghigh.
*  SET TITLEBAR 'MAN'
*  WITH taglow monatlow jahrlow taghigh monathigh jahrhigh.
* Conversion of the dates from the internal to the external view
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      DATE_INTERNAL            = DATUM-LOW
    IMPORTING
      DATE_EXTERNAL            = D_FROM
    EXCEPTIONS
      DATE_INTERNAL_IS_INVALID = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      DATE_INTERNAL            = DATUM-HIGH
    IMPORTING
      DATE_EXTERNAL            = D_TO
    EXCEPTIONS
      DATE_INTERNAL_IS_INVALID = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  SET TITLEBAR 'MAN'
  WITH D_FROM D_TO.
*  End of changes of note 1117067                          "n1117067


* create the headlines using the titelbar                   "n599218
  PERFORM                    CREATE_HEADLINE.               "n599218

* calculate the offsets for the list header
  PERFORM                    CALCULATE_OFFSETS.

* for the representation of tied empties                    "n547170
  PERFORM                    F0700_PREPARE_TIED_EMPTIES.    "n547170

* create table g_t_mseg_fields with the names of all
* wanted fields from MSEG and MKPF
  PERFORM                    F0300_GET_FIELDS.

  BREAK-POINT                ID MMIM_REP_MB5B.              "n921164
* dynamic break-point : is IS-OIL active ?                  "n921164
                                                            "n599218 A
* check whether this is a IS-OIL system                     "n599218 A
  PERFORM                    CHECK_IS_OIL_SYSTEM.           "n599218 A
                                                            "n599218 A
  IF  G_FLAG_IS_OIL_ACTIVE = 'X'.           "IS-OIL ?       "n599218 A
*   the 2 IS-OIL specific data fields will be inserted into "n599218 A
*   working table G_T_MSEG_FIELDS. Then these fields will   "n599218 A
*   transported from database table MSEG, too               "n599218 A
    APPEND  'MSEG~OIGLCALC'  TO  G_T_MSEG_FIELDS.           "n599218 A
    APPEND  'MSEG~OIGLSKU'   TO  G_T_MSEG_FIELDS.           "n599218 A
  ENDIF.                                                    "n599218 A

* create the ALV fieldcatalog for the main list always
  MOVE  'G_T_BELEGE'         TO  G_F_TABNAME.

  PERFORM                    F0400_CREATE_FIELDCAT.

* do not print the ALV-statistics and selection criteria
  CLEAR                      G_S_PRINT.
  G_S_PRINT-NO_PRINT_SELINFOS   = 'X'.
  G_S_PRINT-NO_PRINT_LISTINFOS = 'X'."

* create the range table for the storage location
  PERFORM                    F0600_CREATE_RANGE_LGORT.

* - show the current activity and the progress              "n599218
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'                 "n599218
    EXPORTING                                               "n599218
      TEXT = TEXT-063.       "Reading current stocks        "n599218

* get the stock tables
  PERFORM                    AKTUELLE_BESTAENDE.

  PERFORM TABELLEN_LESEN.

* - show the current activity and the progress              "n599218
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'                 "n599218
    EXPORTING                                               "n599218
      TEXT = TEXT-064.       "Reading MM documents          "n599218

  PERFORM                    F1000_SELECT_MSEG_MKPF.

  PERFORM                    BELEGSELEKTION.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*

END-OF-SELECTION.

* results of all the autority checks
  PERFORM                    F9100_AUTH_PLANT_RESULT.

* - show the current activity and the progress              "n599218
  IF BWBST = 'X'.                                           "n599218
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'               "n599218
      EXPORTING                                             "n599218
        TEXT = TEXT-066.     "Calculating Stocks and Values "n599218
  ELSE.                                                     "n599218
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'               "n599218
      EXPORTING                                             "n599218
        TEXT = TEXT-067.     "Calculating Stocks            "n599218
  ENDIF.                                                    "n599218

  PERFORM SUMMEN_BILDEN.

  PERFORM BESTAENDE_BERECHNEN.

  PERFORM LISTUMFANG.

* - show the current activity and the progress              "n599218
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'                 "n599218
    EXPORTING                                               "n599218
      TEXT = TEXT-065.       "Preparing list output         "n599218

* stop if table bestand is empty
  DESCRIBE TABLE BESTAND     LINES G_F_CNT_LINES.

  IF  G_F_CNT_LINES IS INITIAL.
*   Keinen Eintrag zu den Suchbegriffen gefunden/selektiert
    MESSAGE                  S083.
*   perform                  anforderungsbild.
  ELSE.
*   process log function if the use is a tax auditor        "n555246
*   and the database selection was successful               "n555246
    IF  G_FLAG_TPCUSER = '1'.                               "n555246
      PERFORM                TPC_WRITE_LOG.                 "n555246
    ENDIF.                                                  "n555246

    PERFORM FELDGRUPPEN_AUFBAUEN.

*   sort table with header data per material
    IF BWBST IS INITIAL.
      SORT BESTAND BY MATNR WERKS CHARG.
    ELSE.
      SORT BESTAND BY MATNR BWKEY.
    ENDIF.

*   which function does the user want ?
    IF      XSUM = 'X'.
*     hierseq. alv with sums
      PERFORM                CREATE_TABLE_TOTALS_HQ.

      PERFORM                CREATE_FIELDCAT_TOTALS_HQ.

      PERFORM                ALV_HIERSEQ_LIST_TOTALS.

    ELSEIF  PA_SUMFL = 'X'.
*     show the sums only in a flat ALV
      PERFORM                CREATE_TABLE_TOTALS_FLAT.

      PERFORM                CREATE_FIELDCAT_TOTALS_FLAT.
*-----------------------------------------------------------------
* Coding to produce plant description and old material number
* to the output                         ADDED BY GOVIND M
*------------------------------------------------------------------
      PERFORM                CREATE_EXTRA_FIELDS.

      PERFORM                ALV_FLAT_LIST_SUMS_ONLY.

    ELSE.
*     display the full list using the APPEND ALV
      PERFORM                BESTAENDE_AUSGEBEN.
    ENDIF.
  ENDIF.

  CLEAR: G_T_MSEG_LEAN, G_T_BSIM_LEAN, BESTAND.             "n443935

*&---------------------------------------------------------------------*
*&   PF_STATUS_SET_TOTALS
*&---------------------------------------------------------------------*

FORM PF_STATUS_SET_TOTALS                                   "#EC CALLED
                   USING     EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'STANDARD'   EXCLUDING EXTAB.

ENDFORM.                     "PF_STATUS_SET_TOTALS

*----------------------------------------------------------------------*
*    user_parameters_save
*----------------------------------------------------------------------*

FORM USER_PARAMETERS_SAVE.

  GET PARAMETER ID 'BUK'     FIELD  G_SAVE_PARAMS-BUKRS.
  GET PARAMETER ID 'WRK'     FIELD  G_SAVE_PARAMS-WERKS.
  GET PARAMETER ID 'MAT' "#EC CI_FLDEXT_OK[2215424]
      FIELD  G_SAVE_PARAMS-MATNR.
  GET PARAMETER ID 'CHA'     FIELD  G_SAVE_PARAMS-CHARG.
  GET PARAMETER ID 'BLN'     FIELD  G_SAVE_PARAMS-BELNR.
  GET PARAMETER ID 'BUK'     FIELD  G_SAVE_PARAMS-BUKRS.
  GET PARAMETER ID 'GJR'     FIELD  G_SAVE_PARAMS-GJAHR.

ENDFORM.                     "user_parameters_save

*----------------------------------------------------------------------*
*    user_parameters_restore
*----------------------------------------------------------------------*

FORM USER_PARAMETERS_RESTORE.

  SET PARAMETER ID 'BUK'     FIELD  G_SAVE_PARAMS-BUKRS.
  SET PARAMETER ID 'WRK'
      FIELD  G_SAVE_PARAMS-WERKS.
  SET PARAMETER ID 'MAT'    "#EC CI_FLDEXT_OK[2215424]
    FIELD  G_SAVE_PARAMS-MATNR.
  SET PARAMETER ID 'CHA'     FIELD  G_SAVE_PARAMS-CHARG.
  GET PARAMETER ID 'BLN'     FIELD  G_SAVE_PARAMS-BELNR.
  GET PARAMETER ID 'BUK'     FIELD  G_SAVE_PARAMS-BUKRS.
  GET PARAMETER ID 'GJR'     FIELD  G_SAVE_PARAMS-GJAHR.

ENDFORM.                     "user_parameters_restore

*&---------------------------------------------------------------------*
*&   USER_COMMAND_TOTALS
*&---------------------------------------------------------------------*

FORM USER_COMMAND_TOTALS                                    "#EC CALLED
                   USING     R_UCOMM     LIKE  SY-UCOMM
                             RS_SELFIELD TYPE  SLIS_SELFIELD.

  CLEAR                      G_S_BESTAND_KEY.

  IF      RS_SELFIELD-TABNAME = 'G_T_TOTALS_HEADER'.
*   get the selected entry from table G_T_TOTALS
    READ TABLE G_T_TOTALS_HEADER
      INTO  G_S_TOTALS_HEADER
        INDEX RS_SELFIELD-TABINDEX.

    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING  G_S_TOTALS_HEADER
                             TO  G_S_BESTAND_KEY.
    ENDIF.

  ELSEIF  RS_SELFIELD-TABNAME = 'G_T_TOTALS_ITEM'.
*   get the selected entry from table G_T_TOTALS
    READ TABLE G_T_TOTALS_ITEM
      INTO  G_S_TOTALS_ITEM
        INDEX RS_SELFIELD-TABINDEX.

    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING  G_S_TOTALS_ITEM
                             TO  G_S_BESTAND_KEY.
    ENDIF.

  ELSEIF  RS_SELFIELD-TABNAME = 'G_T_TOTALS_FLAT'.
*   get the selected entry from table G_T_TOTALS
    READ TABLE G_T_TOTALS_FLAT
      INTO  G_S_TOTALS_FLAT
        INDEX RS_SELFIELD-TABINDEX.

    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING  G_S_TOTALS_FLAT
                             TO  G_S_BESTAND_KEY.
    ENDIF.
  ENDIF.

  IF G_S_BESTAND_KEY IS INITIAL.   "notinh found ?
*   Place the cursor on a table line
    MESSAGE                  S322.
    EXIT.
  ENDIF.

* get the line from the main table BESTAND depending on the mode
  IF BWBST IS INITIAL.
*   sort sequence = matnr werks charg
    READ TABLE BESTAND
      WITH KEY  MATNR = G_S_BESTAND_KEY-MATNR
                WERKS = G_S_BESTAND_KEY-WERKS
                CHARG = G_S_BESTAND_KEY-CHARG
                             BINARY SEARCH.

  ELSE.
*   sort sequence = matnr bwkey
    READ TABLE BESTAND
      WITH KEY  MATNR = G_S_BESTAND_KEY-MATNR
                BWKEY = G_S_BESTAND_KEY-BWKEY
                             BINARY SEARCH.
  ENDIF.

  IF SY-SUBRC IS INITIAL.
    MOVE-CORRESPONDING BESTAND     TO  G_S_BESTAND_DETAIL.
    APPEND  G_S_BESTAND_DETAIL     TO  G_T_BESTAND_DETAIL.

    PERFORM                  CREATE_TABLE_FOR_DETAIL.

    PERFORM                  LIST_OUTPUT_DETAIL.
  ENDIF.

ENDFORM.                     " USER_COMMAND_TOTALS

*&---------------------------------------------------------------------*
* list_output_detail
*&---------------------------------------------------------------------*

FORM LIST_OUTPUT_DETAIL.

* build the auxiliary interface tables for the ALV

  IF  G_CUST_COLOR = 'X'.              "colorize numeric fields ?
    LAYOUT-COLTAB_FIELDNAME = 'FARBE_PRO_FELD'.
  ELSE.
    LAYOUT-INFO_FIELDNAME   = 'FARBE_PRO_ZEILE'.
  ENDIF.

  LAYOUT-F2CODE = '9PBP'.

  IF NOT BWBST IS INITIAL.
    LAYOUT-MIN_LINESIZE = '92'.
  ENDIF.

  EVENTS-NAME = 'TOP_OF_PAGE'.
  EVENTS-FORM = 'UEBERSCHRIFT_DETAIL'.
  APPEND EVENTS.

  IF  G_FLAG_BREAK-B3 = 'X'.                                "n921164
    BREAK-POINT              ID MMIM_REP_MB5B.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  ENDIF.                                                    "n921164

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      I_INTERFACE_CHECK        = G_FLAG_I_CHECK             "n599218
      I_CALLBACK_PROGRAM       = REPID
      I_CALLBACK_PF_STATUS_SET = 'STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IS_LAYOUT                = LAYOUT
      IT_FIELDCAT              = FIELDCAT[]
      IT_SPECIAL_GROUPS        = GRUPPEN[]
      IT_SORT                  = SORTTAB[]
      I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
      IS_VARIANT               = VARIANTE
      IT_EVENTS                = EVENTS[]
      IS_PRINT                 = G_S_PRINT
    TABLES
      T_OUTTAB                 = G_T_BELEGE1
    EXCEPTIONS
      OTHERS                   = 2.

* does the ALV return with an error ?
  IF  NOT SY-SUBRC IS INITIAL.         "Fehler vom ALV ?
    MESSAGE ID SY-MSGID TYPE  'S'     NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                     " list_output_detail

*&---------------------------------------------------------------------*
*&   TOP_OF_PAGE_TOTALS
*&---------------------------------------------------------------------*

FORM TOP_OF_PAGE_TOTALS.                                    "#EC CALLED

* go on when the report runs in print mode -> last line
  CHECK NOT SY-PRDSN IS INITIAL.

  DATA: LR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

*... (1) create the information to be displayed by using
*        the ALV Form elements
  PERFORM TOP_OF_PAGE_TOTALS_RENDER  CHANGING LR_CONTENT.

*... (2) Sending the information to the ALV
*        Once the inforation to be displayed has been
*        created the information has to be sent to the ALV
*        This is done by calling the static method
*        CL_SALV_FORM_CONTENT=>SET( <content> ) with the content
*        which is to be displayed.
*        Alternativly the function module REUSE_ALV_COMMENTARY_WRITE
*        can still be used.
  CL_SALV_FORM_CONTENT=>SET( LR_CONTENT ).

ENDFORM.                     "TOP_OF_PAGE_TOTALS

*&---------------------------------------------------------------------*
*&   TOP_OF_PAGE_TOTALS_RENDER
*&---------------------------------------------------------------------*

FORM TOP_OF_PAGE_TOTALS_RENDER
         CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA: LR_GRID      TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_FLOW      TYPE REF TO CL_SALV_FORM_LAYOUT_FLOW,
        L_TEXT(500)  TYPE C,
        L_CHAR(500)  TYPE C.

*... create a grid
  CREATE OBJECT LR_GRID.

  LR_FLOW = LR_GRID->CREATE_FLOW( ROW = 1  COLUMN = 1 ).

  IF  BWBST IS INITIAL.                                     "n599218
*   stocks only                                             "n599218
    WRITE : SY-PAGNO NO-SIGN      TO  G_S_HEADER_77-PAGE.   "n599218
    MOVE  : G_S_HEADER_77         TO  L_TEXT.
  ELSE.                                                     "n599218
*   stocks and values                                       "n599218
    WRITE : SY-PAGNO NO-SIGN      TO  G_S_HEADER_91-PAGE.   "n599218
    MOVE  : G_S_HEADER_91         TO  L_TEXT.               "n599218
  ENDIF.                                                    "n599218

* add line to object
  LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).

* copy whole header object
  CR_CONTENT = LR_GRID.

ENDFORM.                     " TOP_OF_PAGE_TOTALS_RENDER

*----------------------------------------------------------------------*
* top_of_page_render.
*----------------------------------------------------------------------*

FORM TOP_OF_PAGE_RENDER.

* interface structurebegin of g_s_bestand.

  DATA: LR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

*... (1) create the information to be displayed by using
*        the ALV Form elements
  PERFORM CREATE_ALV_FORM_CONTENT_TOP CHANGING LR_CONTENT.

*... (2) Sending the information to the ALV
*        Once the inforation to be displayed has been
*        created the information has to be sent to the ALV
*        This is done by calling the static method
*        CL_SALV_FORM_CONTENT=>SET( <content> ) with the content
*        which is to be displayed.
*        Alternativly the function module REUSE_ALV_COMMENTARY_WRITE
*        can still be used.
  CL_SALV_FORM_CONTENT=>SET( LR_CONTENT ).

ENDFORM.                     " top_of_page_render

*----------------------------------------------------------------------*
* create_alv_form_content_top
*----------------------------------------------------------------------*
* baustelle

FORM CREATE_ALV_FORM_CONTENT_TOP
                   CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA: LR_GRID      TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_FLOW      TYPE REF TO CL_SALV_FORM_LAYOUT_FLOW,
        L_TEXT(500)  TYPE C,
        L_CHAR(500)  TYPE C.

  DATA: L_ROW          TYPE I,
        L_FIGURE(24)   TYPE C,
        L_FLAG_TIED_EMPTIES(01)   TYPE C.

  DATA: L_F_TEXT(60)        TYPE  C.                        "n999530

*----------------------------------------------------------------------*


*... create a grid
  CREATE OBJECT LR_GRID.

* the current data are in interface structure g_s_bestand.

* in the case the report run in print or background mode
* --> print the old headlines

  IF NOT SY-PRDSN IS INITIAL.
    ADD  1                    TO  L_ROW.
    LR_FLOW = LR_GRID->CREATE_FLOW( ROW = L_ROW  COLUMN = 1 ).

    IF  BWBST IS INITIAL.
*     stocks only
      WRITE : SY-PAGNO NO-SIGN    TO  G_S_HEADER_77-PAGE.
      MOVE  : G_S_HEADER_77       TO  L_TEXT.
    ELSE.
*     stocks and values
      WRITE : SY-PAGNO NO-SIGN    TO  G_S_HEADER_91-PAGE.
      MOVE  : G_S_HEADER_91       TO  L_TEXT.
    ENDIF.

*   add line to object
    LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).

    ADD  1                    TO  L_ROW.
  ENDIF.

* first line : plant or valuation area ---------------------------------
  ADD  1                    TO  L_ROW.
  LR_FLOW = LR_GRID->CREATE_FLOW( ROW = L_ROW  COLUMN = 1 ).

  IF  BWBST IS INITIAL.
    PERFORM  F2200_READ_T001 USING G_S_BESTAND-WERKS.       "n999530

    WRITE G_S_BESTAND-WERKS  TO L_F_TEXT.                   "n999530
    CONDENSE L_F_TEXT.                                      "n999530
    CONCATENATE L_F_TEXT     T001W-NAME1                    "n999530
                             INTO  L_F_TEXT                 "n999530
                             SEPARATED BY SPACE.            "n999530

    MOVE : TEXT-020          TO  L_TEXT,
           L_F_TEXT          TO  L_TEXT+G_OFFSET_HEADER.    "n999530
  ELSE.
*   show valuation area
    MOVE : TEXT-025          TO  L_TEXT,
           G_S_BESTAND-BWKEY TO  L_TEXT+G_OFFSET_HEADER.
  ENDIF.

* add line to object
  LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).

* second line : material number ----------------------------------------
  ADD   1                    TO  L_ROW.
  LR_FLOW = LR_GRID->CREATE_FLOW( ROW = L_ROW  COLUMN = 1 ).

  MOVE  : TEXT-021           TO  L_TEXT.
  WRITE : G_S_BESTAND-MATNR  TO  L_TEXT+G_OFFSET_HEADER.

* add line to object
  LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).

* third line : material short text -------------------------------------
  ADD   1                    TO  L_ROW.
  LR_FLOW = LR_GRID->CREATE_FLOW( ROW = L_ROW  COLUMN = 1 ).

  PERFORM  F2100_MAT_TEXT    USING  G_S_BESTAND-MATNR.

  MOVE : TEXT-022            TO  L_TEXT,
         G_S_MAKT-MAKTX      TO  L_TEXT+G_OFFSET_HEADER.

* add line to object
  LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).

* fourth line : batch if required --------------------------------------
  IF XCHAR = 'X'.
    ADD   1                  TO  L_ROW.
    LR_FLOW = LR_GRID->CREATE_FLOW( ROW = L_ROW  COLUMN = 1 ).

    MOVE : TEXT-023           TO  L_TEXT,
           G_S_BESTAND-CHARG  TO  L_TEXT+G_OFFSET_HEADER.

*   add line to object
    LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).
  ENDIF.

* line : stock and value on start date ------------------------------
* with one empty line
  ADD  2                     TO  L_ROW.
  LR_FLOW = LR_GRID->CREATE_FLOW( ROW = L_ROW  COLUMN = 1 ).

* convert unit of measurement from internal to external format "n1018717
  WRITE : G_S_BESTAND-MEINS       TO  L_F_MEINS_EXTERNAL.   "n1018717

  CLEAR                           L_TEXT.
  IF BWBST IS INITIAL.
*   stock on start date
    MOVE : G_DATE_LINE_FROM       TO  L_TEXT.
    WRITE  G_S_BESTAND-ANFMENGE   TO L_FIGURE
                                  UNIT  G_S_BESTAND-MEINS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_QTY(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  L_F_MEINS_EXTERNAL     TO  L_TEXT+G_OFFSET_UNIT.  "n1018717
*ENHANCEMENT-POINT EHP605_RM07MLBD_01 SPOTS ES_RM07MLBD .
  ELSE.
* stocks and values on start date
    MOVE : G_DATE_LINE_FROM       TO  L_TEXT.
    WRITE  G_S_BESTAND-ANFMENGE   TO L_FIGURE
                                  UNIT  G_S_BESTAND-MEINS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_QTY(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  L_F_MEINS_EXTERNAL     TO  L_TEXT+G_OFFSET_UNIT.  "n1018717


    WRITE G_S_BESTAND-ANFWERT     TO L_FIGURE
                                  CURRENCY  G_S_BESTAND-WAERS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_VALUE(24).
    MOVE  G_S_BESTAND-WAERS       TO  L_TEXT+G_OFFSET_CURR.
  ENDIF.

*   add line to object
  LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).

* line : total quantity and value of goods receipts --------------------
  ADD  1                     TO  L_ROW.
  LR_FLOW = LR_GRID->CREATE_FLOW( ROW = L_ROW  COLUMN = 1 ).

  CLEAR                           L_TEXT.

  IF BWBST IS INITIAL.
*   total quantities of goods receipts
    MOVE : TEXT-005               TO  L_TEXT+2.
    WRITE  G_S_BESTAND-SOLL       TO L_FIGURE
                                  UNIT  G_S_BESTAND-MEINS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_QTY(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  L_F_MEINS_EXTERNAL      TO  L_TEXT+G_OFFSET_UNIT. "n1018717
*ENHANCEMENT-POINT EHP605_RM07MLBD_02 SPOTS ES_RM07MLBD .
  ELSE.
*   total quantities and values of goods receipts
    MOVE : TEXT-030               TO  L_TEXT+2.
    WRITE  G_S_BESTAND-SOLL       TO L_FIGURE
                                  UNIT  G_S_BESTAND-MEINS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_QTY(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  L_F_MEINS_EXTERNAL      TO  L_TEXT+G_OFFSET_UNIT. "n1018717

*ENHANCEMENT-POINT EHP605_RM07MLBD_03 SPOTS ES_RM07MLBD .
    WRITE G_S_BESTAND-SOLLWERT     TO L_FIGURE
                                  CURRENCY  G_S_BESTAND-WAERS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_VALUE(24).
    MOVE  G_S_BESTAND-WAERS       TO  L_TEXT+G_OFFSET_CURR.
  ENDIF.

* add line to object
  LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).

* line : total quantity and value of goods issues ----------------------
  ADD  1                     TO  L_ROW.
  LR_FLOW = LR_GRID->CREATE_FLOW( ROW = L_ROW  COLUMN = 1 ).

  CLEAR                           L_TEXT.

  IF BWBST IS INITIAL.
*   total quantities of goods issues
    MOVE : TEXT-006               TO  L_TEXT+2.
    COMPUTE  G_S_BESTAND-HABEN    =  G_S_BESTAND-HABEN * -1.
    WRITE  G_S_BESTAND-HABEN      TO L_FIGURE
                                  UNIT  G_S_BESTAND-MEINS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_QTY(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  L_F_MEINS_EXTERNAL      TO  L_TEXT+G_OFFSET_UNIT. "n1018717
*ENHANCEMENT-POINT EHP605_RM07MLBD_04 SPOTS ES_RM07MLBD .
  ELSE.
*   total quantities of goods issues
    MOVE : TEXT-031               TO  L_TEXT+2.
    COMPUTE  G_S_BESTAND-HABEN    =  G_S_BESTAND-HABEN * -1.
    WRITE  G_S_BESTAND-HABEN      TO L_FIGURE
                                  UNIT  G_S_BESTAND-MEINS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_QTY(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  L_F_MEINS_EXTERNAL      TO  L_TEXT+G_OFFSET_UNIT. "n1018717
*ENHANCEMENT-POINT EHP605_RM07MLBD_05 SPOTS ES_RM07MLBD .
    COMPUTE G_S_BESTAND-HABENWERT  =  G_S_BESTAND-HABENWERT * -1.
    WRITE G_S_BESTAND-HABENWERT   TO L_FIGURE
                                  CURRENCY  G_S_BESTAND-WAERS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_VALUE(24).
    MOVE  G_S_BESTAND-WAERS       TO  L_TEXT+G_OFFSET_CURR.
  ENDIF.

* add line to object
  LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).

* line : stock and value on end date ------------------------------
  ADD  1                     TO  L_ROW.
  LR_FLOW = LR_GRID->CREATE_FLOW( ROW = L_ROW  COLUMN = 1 ).

  CLEAR                           L_TEXT.

  IF BWBST IS INITIAL.
*   stock on end date
    MOVE : G_DATE_LINE_TO         TO  L_TEXT.
    WRITE  G_S_BESTAND-ENDMENGE   TO L_FIGURE
                                  UNIT  G_S_BESTAND-MEINS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_QTY(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  L_F_MEINS_EXTERNAL      TO  L_TEXT+G_OFFSET_UNIT. "n1018717
*ENHANCEMENT-POINT EHP605_RM07MLBD_06 SPOTS ES_RM07MLBD .
  ELSE.
* stocks and values on end date
    MOVE : G_DATE_LINE_TO         TO  L_TEXT.
    WRITE  G_S_BESTAND-ENDMENGE   TO L_FIGURE
                                  UNIT  G_S_BESTAND-MEINS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_QTY(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  L_F_MEINS_EXTERNAL      TO  L_TEXT+G_OFFSET_UNIT. "n1018717
*ENHANCEMENT-POINT EHP605_RM07MLBD_07 SPOTS ES_RM07MLBD .
    WRITE G_S_BESTAND-ENDWERT     TO L_FIGURE
                                  CURRENCY  G_S_BESTAND-WAERS.
    MOVE  L_FIGURE                TO  L_TEXT+G_OFFSET_VALUE(24).
    MOVE  G_S_BESTAND-WAERS       TO  L_TEXT+G_OFFSET_CURR.
  ENDIF.

* add line to object
  LR_FLOW->CREATE_TEXT( TEXT = L_TEXT ).

* copy whole header object
  CR_CONTENT = LR_GRID.

ENDFORM.                    " create_alv_form_content_top

*----------------------------------------------------------------------*
*    create_table_totals_hq
*----------------------------------------------------------------------*

FORM CREATE_TABLE_TOTALS_HQ.

* create output table
  LOOP AT BESTAND.
*   part 1 : create header table g_t_totals_header
    MOVE-CORRESPONDING  BESTAND        TO  G_S_TOTALS_HEADER.
    MOVE  SOBKZ                        TO  G_S_TOTALS_HEADER-SOBKZ.

    PERFORM  F2100_MAT_TEXT  USING  BESTAND-MATNR.
    MOVE  G_S_MAKT-MAKTX     TO  G_S_TOTALS_HEADER-MAKTX.

    IF  BWBST IS INITIAL.
*     mode : stocks or special stocks
      PERFORM  F2200_READ_T001 USING BESTAND-WERKS.

      MOVE  T001W-NAME1      TO  G_S_TOTALS_HEADER-NAME1.
    ELSE.
*     mode : valuated stocks
      IF  CURM = '3'.
*       valuation level is company code
        SELECT SINGLE BUTXT  FROM T001
                             INTO G_F_BUTXT
          WHERE  BUKRS = BESTAND-BWKEY.

        IF SY-SUBRC IS INITIAL.
          MOVE  G_F_BUTXT    TO  G_S_TOTALS_HEADER-NAME1.
        ELSE.
          CLEAR              G_S_TOTALS_HEADER-NAME1.
        ENDIF.
      ELSE.
*       valuation level is plant -> take the name of the plant
        PERFORM  F2200_READ_T001 USING BESTAND-WERKS.

        MOVE  T001W-NAME1    TO  G_S_TOTALS_HEADER-NAME1.
      ENDIF.
    ENDIF.

    APPEND  G_S_TOTALS_HEADER     TO  G_T_TOTALS_HEADER.

*   part 2 : create 4 lines in item table g_t_totals_item
    CLEAR                         G_S_TOTALS_ITEM.
    MOVE : BESTAND-BWKEY          TO  G_S_TOTALS_ITEM-BWKEY,
           BESTAND-WERKS          TO  G_S_TOTALS_ITEM-WERKS,
           BESTAND-MATNR          TO  G_S_TOTALS_ITEM-MATNR,
           BESTAND-CHARG          TO  G_S_TOTALS_ITEM-CHARG,
           BESTAND-MEINS          TO  G_S_TOTALS_ITEM-MEINS,
           BESTAND-WAERS          TO  G_S_TOTALS_ITEM-WAERS.

*   line with the stock on start date
    MOVE : G_DATE_LINE_FROM       TO  G_S_TOTALS_ITEM-STOCK_TYPE,
           BESTAND-ANFMENGE       TO  G_S_TOTALS_ITEM-MENGE,
           BESTAND-ANFWERT        TO  G_S_TOTALS_ITEM-WERT.
*ENHANCEMENT-POINT EHP605_RM07MLBD_08 SPOTS ES_RM07MLBD .
    PERFORM                       CREATE_TABLE_TOTALS_HQ_1.

*   line with the good receipts
    IF  BWBST = 'X'.
      MOVE : TEXT-030             TO  G_S_TOTALS_ITEM-STOCK_TYPE+2,
             BESTAND-SOLL         TO  G_S_TOTALS_ITEM-MENGE,
             BESTAND-SOLLWERT     TO  G_S_TOTALS_ITEM-WERT.
    ELSE.
      MOVE : TEXT-005             TO  G_S_TOTALS_ITEM-STOCK_TYPE+2,
             BESTAND-SOLL         TO  G_S_TOTALS_ITEM-MENGE.
    ENDIF.
*ENHANCEMENT-POINT EHP605_RM07MLBD_09 SPOTS ES_RM07MLBD .
    PERFORM                       CREATE_TABLE_TOTALS_HQ_1.

*   line with the good issues
    IF  BWBST = 'X'.
      MOVE : TEXT-031             TO  G_S_TOTALS_ITEM-STOCK_TYPE+2.
      G_S_TOTALS_ITEM-MENGE       = BESTAND-HABEN      * -1.
      G_S_TOTALS_ITEM-WERT        = BESTAND-HABENWERT  * -1.
    ELSE.
      MOVE : TEXT-006             TO  G_S_TOTALS_ITEM-STOCK_TYPE+2.
      G_S_TOTALS_ITEM-MENGE       = BESTAND-HABEN      * -1.
    ENDIF.
*ENHANCEMENT-POINT EHP605_RM07MLBD_10 SPOTS ES_RM07MLBD .
    PERFORM                       CREATE_TABLE_TOTALS_HQ_1.

*   line with the tock on end date
    MOVE : G_DATE_LINE_TO         TO  G_S_TOTALS_ITEM-STOCK_TYPE,
           BESTAND-ENDMENGE       TO  G_S_TOTALS_ITEM-MENGE,
           BESTAND-ENDWERT        TO  G_S_TOTALS_ITEM-WERT.
*ENHANCEMENT-POINT EHP605_RM07MLBD_11 SPOTS ES_RM07MLBD .
    PERFORM                       CREATE_TABLE_TOTALS_HQ_1.
  ENDLOOP.

ENDFORM.                     " create_table_totals_hq

*----------------------------------------------------------------------*
* create_table_totals_hq_1.
*----------------------------------------------------------------------*

* colorize the numeric fields depending on the sign and append the
* entries into table G_T_TOTALS_ITEM

FORM CREATE_TABLE_TOTALS_HQ_1.

  REFRESH                    G_T_COLOR.
  CLEAR                      G_S_COLOR.

* colorize the quntities always
  IF      G_S_TOTALS_ITEM-MENGE > 0.
*   positive value -> green
    MOVE : 'MENGE'           TO  G_S_COLOR-FIELDNAME,
           '5'               TO  G_S_COLOR-COLOR-COL,    "green
           '0'               TO  G_S_COLOR-COLOR-INT.
    APPEND  G_S_COLOR        TO  G_T_COLOR.

    MOVE : 'MEINS'           TO  G_S_COLOR-FIELDNAME,
           '5'               TO  G_S_COLOR-COLOR-COL,    "green
           '0'               TO  G_S_COLOR-COLOR-INT.
    APPEND  G_S_COLOR        TO  G_T_COLOR.

  ELSEIF  G_S_TOTALS_ITEM-MENGE < 0.
*   negative value -> red
    MOVE : 'MENGE'           TO  G_S_COLOR-FIELDNAME,
           '6'               TO  G_S_COLOR-COLOR-COL,    "red
           '0'               TO  G_S_COLOR-COLOR-INT.
    APPEND  G_S_COLOR        TO  G_T_COLOR.

    MOVE : 'MEINS'           TO  G_S_COLOR-FIELDNAME,
           '6'               TO  G_S_COLOR-COLOR-COL,    "red
           '0'               TO  G_S_COLOR-COLOR-INT.
    APPEND  G_S_COLOR        TO  G_T_COLOR.
  ENDIF.

  IF  BWBST = 'X'.
*  colorize the values only in mode valuated stock
    IF      G_S_TOTALS_ITEM-WERT > 0.
*     positive value -> green
      MOVE : 'WERT'          TO  G_S_COLOR-FIELDNAME,
             '5'             TO  G_S_COLOR-COLOR-COL,    "green
             '0'             TO  G_S_COLOR-COLOR-INT.
      APPEND  G_S_COLOR      TO  G_T_COLOR.

      MOVE : 'WAERS'         TO  G_S_COLOR-FIELDNAME,
             '5'             TO  G_S_COLOR-COLOR-COL,    "green
             '0'             TO  G_S_COLOR-COLOR-INT.
      APPEND  G_S_COLOR      TO  G_T_COLOR.

    ELSEIF  G_S_TOTALS_ITEM-WERT < 0.
*     negative value -> red
      MOVE : 'WERT'          TO  G_S_COLOR-FIELDNAME,
             '6'             TO  G_S_COLOR-COLOR-COL,    "red
             '0'             TO  G_S_COLOR-COLOR-INT.
      APPEND  G_S_COLOR      TO  G_T_COLOR.

      MOVE : 'WAERS'         TO  G_S_COLOR-FIELDNAME,
             '6'             TO  G_S_COLOR-COLOR-COL,    "red
             '0'             TO  G_S_COLOR-COLOR-INT.
      APPEND  G_S_COLOR      TO  G_T_COLOR.
    ENDIF.
  ENDIF.

  IF  G_T_COLOR[] IS INITIAL.
    CLEAR :                  G_S_TOTALS_ITEM-COLOR.
  ELSE.
*   customizing : set the color information
    IF  G_CUST_COLOR  = 'X'.
      MOVE  G_T_COLOR[]      TO  G_S_TOTALS_ITEM-COLOR.
    ENDIF.
  ENDIF.

  ADD   1                    TO  G_S_TOTALS_ITEM-COUNTER.
  APPEND  G_S_TOTALS_ITEM    TO  G_T_TOTALS_ITEM.
  CLEAR :                    G_S_TOTALS_ITEM-STOCK_TYPE.

ENDFORM.                     " create_table_totals_hq_1.

*----------------------------------------------------------------------*
*    create_table_for_detail
*----------------------------------------------------------------------*

FORM CREATE_TABLE_FOR_DETAIL.

  STATICS : L_FLAG_SORTED(01)     TYPE  C.
  DATA    : L_TABIX               LIKE  SY-TABIX.

* sort table with the documents
  IF  L_FLAG_SORTED IS INITIAL.
    SORT  G_T_MSEG_LEAN
      BY MATNR WERKS CHARG BUDAT MBLNR ZEILE BELNR.
    MOVE  'X'                TO  L_FLAG_SORTED.
  ENDIF.

  REFRESH                    G_T_BELEGE1.

* find the first entry with this material number
  READ TABLE G_T_MSEG_LEAN   INTO  G_S_MSEG_LEAN
    WITH KEY MATNR = G_S_BESTAND_DETAIL-MATNR
      BINARY SEARCH.

  IF  SY-SUBRC IS INITIAL.
    MOVE  SY-TABIX           TO  L_TABIX.

    LOOP AT G_T_MSEG_LEAN   INTO  G_S_MSEG_LEAN
                             FROM L_TABIX.

*     leave this loop when the material number changes
      IF  G_S_MSEG_LEAN-MATNR  NE  G_S_BESTAND_DETAIL-MATNR.
        EXIT.
      ENDIF.

      IF  BWBST IS INITIAL.
        CHECK : G_S_MSEG_LEAN-WERKS = BESTAND-WERKS.        "n1390970
        CHECK : XCHAR               IS INITIAL       OR
                G_S_MSEG_LEAN-CHARG = BESTAND-CHARG.
        MOVE-CORRESPONDING G_S_MSEG_LEAN
                             TO  G_T_BELEGE1.

*       enrich some fields with color and numeric fields with sign
        PERFORM  F9500_SET_COLOR_AND_SIGN
                       USING  G_T_BELEGE1  'G_T_BELEGE1'.
        APPEND                G_T_BELEGE1.
      ELSE.
*       get the valuation area for this plant
        PERFORM  F9300_READ_ORGAN
                   USING     C_WERKS   G_S_MSEG_LEAN-WERKS.

        CHECK : G_S_ORGAN-BWKEY = BESTAND-BWKEY.            "184465
        MOVE-CORRESPONDING  G_S_MSEG_LEAN
                             TO  G_T_BELEGE1.

*       enrich some fields with color and numeric fields with sign
        PERFORM  F9500_SET_COLOR_AND_SIGN
                       USING  G_T_BELEGE1  'G_T_BELEGE1'.

        APPEND               G_T_BELEGE1.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                     " create_table_for_detail

*----------------------------------------------------------------------*
* create_table_totals_flat
*----------------------------------------------------------------------*

FORM CREATE_TABLE_TOTALS_FLAT.

* create output table G-T-totals_flat
  LOOP AT BESTAND.
    REFRESH                  G_T_COLOR.
    MOVE-CORRESPONDING  BESTAND        TO  G_S_TOTALS_FLAT.
    MOVE  SOBKZ                        TO  G_S_TOTALS_FLAT-SOBKZ.

    PERFORM  F2100_MAT_TEXT  USING  BESTAND-MATNR.

*   show the GI with negative sign
    G_S_TOTALS_FLAT-HABEN         = G_S_TOTALS_FLAT-HABEN     * -1.

    MOVE : G_S_MAKT-MAKTX    TO  G_S_TOTALS_FLAT-MAKTX,
           DATUM-LOW         TO  G_S_TOTALS_FLAT-START_DATE,
           DATUM-HIGH        TO  G_S_TOTALS_FLAT-END_DATE.

    PERFORM  COLORIZE_TOTALS_FLAT   USING 'ANFMENGE'.
    PERFORM  COLORIZE_TOTALS_FLAT   USING 'SOLL'.
    PERFORM  COLORIZE_TOTALS_FLAT   USING 'HABEN'.
    PERFORM  COLORIZE_TOTALS_FLAT   USING 'ENDMENGE'.
*ENHANCEMENT-POINT EHP605_RM07MLBD_12 SPOTS ES_RM07MLBD .

    IF  BWBST = 'X'.
      G_S_TOTALS_FLAT-HABENWERT     = G_S_TOTALS_FLAT-HABENWERT * -1.

      PERFORM  COLORIZE_TOTALS_FLAT   USING 'ANFWERT'.
      PERFORM  COLORIZE_TOTALS_FLAT   USING 'SOLLWERT'.
      PERFORM  COLORIZE_TOTALS_FLAT   USING 'HABENWERT'.
      PERFORM  COLORIZE_TOTALS_FLAT   USING 'ENDWERT'.
    ENDIF.

    IF  G_T_COLOR[] IS INITIAL.
      CLEAR                  G_S_TOTALS_FLAT-COLOR.
    ELSE.
      MOVE  G_T_COLOR[]      TO  G_S_TOTALS_FLAT-COLOR.
    ENDIF.

* get the name of this plant                                "n999530
    PERFORM F2200_READ_T001  USING  G_S_TOTALS_FLAT-WERKS.  "n999530

    MOVE  T001W-NAME1        TO  G_S_TOTALS_FLAT-NAME1.     "n999530

    APPEND  G_S_TOTALS_FLAT  TO  G_T_TOTALS_FLAT.
  ENDLOOP.

ENDFORM.                     " create_table_totals_flat

*----------------------------------------------------------------------*
*   colorize_totals_flat
*----------------------------------------------------------------------*

FORM  COLORIZE_TOTALS_FLAT   USING L_FIELDNAME TYPE ANY.

  DATA : L_F_FIELDNAME(30)   TYPE C.
  FIELD-SYMBOLS : <L_FS_FIELD>.

* customizing : set the color information
  CHECK : G_CUST_COLOR  = 'X'.

  CONCATENATE  'G_S_TOTALS_FLAT-' L_FIELDNAME
                             INTO L_F_FIELDNAME.
  ASSIGN (L_F_FIELDNAME)     TO  <L_FS_FIELD>.

  CHECK SY-SUBRC IS INITIAL.

  IF      <L_FS_FIELD> > 0.
    MOVE : L_FIELDNAME       TO  G_S_COLOR-FIELDNAME,
           '5'               TO  G_S_COLOR-COLOR-COL,    "green
           '0'               TO  G_S_COLOR-COLOR-INT.
    APPEND  G_S_COLOR        TO  G_T_COLOR.

  ELSEIF  <L_FS_FIELD> < 0.
    MOVE : L_FIELDNAME       TO  G_S_COLOR-FIELDNAME,
         '6'               TO  G_S_COLOR-COLOR-COL,    "red
         '0'               TO  G_S_COLOR-COLOR-INT.
    APPEND  G_S_COLOR        TO  G_T_COLOR.

  ENDIF.

ENDFORM.                     " colorize_totals_flat

*----------------------------------------------------------------------*
* create_fieldcat_totals_flat
*----------------------------------------------------------------------*

FORM CREATE_FIELDCAT_TOTALS_FLAT.

  CLEAR : G_S_FIELDCAT,      G_F_COL_POS.

  IF  BWBST = 'X'.
*   valuated stock
    PERFORM FC_S_FLAT USING 'BWKEY' 'MBEW' 'BWKEY '.
  ELSE.
*   take the plant
    PERFORM FC_S_FLAT USING 'WERKS' 'MARC' 'WERKS'.

    IF  XCHAR = 'X'.
*     take the batch number
      PERFORM FC_S_FLAT USING 'CHARG' 'MCHB' 'CHARG'.
    ENDIF.
  ENDIF.

  IF BWBST IS INITIAL.                                      "n999530
    MOVE  'X'                 TO  G_S_FIELDCAT-NO_OUT.      "n999530
    MOVE : TEXT-024           TO  G_S_FIELDCAT-SELTEXT_L,   "n999530
          'L'                 TO  G_S_FIELDCAT-DDICTXT,     "n999530
          30                  TO  G_S_FIELDCAT-OUTPUTLEN.   "n999530
    PERFORM FC_S_FLAT USING 'NAME1' 'T001W' 'NAME1'.        "n999530
  ENDIF.                                                    "n999530

  PERFORM FC_S_FLAT USING 'NAME1' 'T001W' 'NAME1'.
  PERFORM FC_S_FLAT USING 'MATKL' 'MARA' 'MATKL' .
  PERFORM FC_S_FLAT USING 'WGBEZ' 'T023T' 'WGBEZ' .
  PERFORM FC_S_FLAT USING 'LGORT' 'MARD' 'LGORT' . " Added By Govind On 07/01/2015 - Storage Location
  PERFORM FC_S_FLAT USING 'LGOBE' 'T001L' 'LGOBE' . " Added By Govind On 07/01/2015 - Storage Location
  PERFORM FC_S_FLAT USING 'SPART' 'MARA' 'SPART' .
  PERFORM FC_S_FLAT USING 'VTEXT' 'TSPAT' 'VTEXT' .
  PERFORM FC_S_FLAT USING 'MATNR' 'MARA' 'MATNR'.

*  MOVE  'X'                  TO  G_S_FIELDCAT-NO_OUT.
  PERFORM FC_S_FLAT USING 'MAKTX' 'MAKT' 'MAKTX'.
  PERFORM FC_S_FLAT USING 'MTART' 'MARA' 'MTART' .

  PERFORM FC_S_FLAT USING 'MTBEZ' 'T134T' 'MTBEZ' .
  IF  SOBKZ IS INITIAL.
    MOVE  'X'                TO  G_S_FIELDCAT-NO_OUT.
  ENDIF.

  PERFORM FC_S_FLAT USING 'SOBKZ' 'MSLB' 'SOBKZ'.

**********************************************************************
**** START OF NOTE 1064332                                   "n1064332
**** new logic for fields start_date & end_date              "n1064332
**********************************************************************

  ADD  : 1                   TO  G_F_COL_POS.
  MOVE : 'START_DATE'        TO  G_S_FIELDCAT-FIELDNAME,
         G_F_COL_POS         TO  G_S_FIELDCAT-COL_POS,
         'G_T_TOTALS_FLAT'   TO  G_S_FIELDCAT-TABNAME,
         TEXT-094            TO  G_S_FIELDCAT-SELTEXT_L, "from date
         TEXT-094            TO  G_S_FIELDCAT-SELTEXT_M, "from date
         TEXT-094            TO  G_S_FIELDCAT-SELTEXT_S, "from date
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         15                  TO  G_S_FIELDCAT-OUTPUTLEN,
         'D'                 TO  G_S_FIELDCAT-INTTYPE,
         'DATS'              TO  G_S_FIELDCAT-DATATYPE.
* fields l_ref_* are no longer needed
*         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
*         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.

  APPEND  G_S_FIELDCAT       TO  G_T_FIELDCAT_TOTALS_FLAT.
  CLEAR                      G_S_FIELDCAT.


  ADD  : 1                   TO  G_F_COL_POS.
  MOVE : 'END_DATE'          TO  G_S_FIELDCAT-FIELDNAME,
         G_F_COL_POS         TO  G_S_FIELDCAT-COL_POS,
         'G_T_TOTALS_FLAT'   TO  G_S_FIELDCAT-TABNAME,
         TEXT-095            TO  G_S_FIELDCAT-SELTEXT_L, "from date
         TEXT-095            TO  G_S_FIELDCAT-SELTEXT_M, "from date
         TEXT-095            TO  G_S_FIELDCAT-SELTEXT_S, "from date
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         15                  TO  G_S_FIELDCAT-OUTPUTLEN,
         'D'                 TO  G_S_FIELDCAT-INTTYPE,
         'DATS'              TO  G_S_FIELDCAT-DATATYPE.
* fields l_ref_* are no longer needed
*         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
*         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.

  APPEND  G_S_FIELDCAT       TO  G_T_FIELDCAT_TOTALS_FLAT.
  CLEAR                      G_S_FIELDCAT.

  PERFORM FC_S_FLAT USING 'VOLUM' 'MARA' 'VOLUM' .
* old logic for fields start_date and end_date
*  move : text-094            to  g_s_fieldcat-selText_l, "from date
*         'L'                 to  g_s_fieldcat-ddictxt,
*         15                  to  g_s_fieldcat-outputlen.
*  perform fc_s_flat using 'START_DATE' 'MKPF' 'BUDAT'.
*
*  move : text-095            to  g_s_fieldcat-selText_l, "to date
*         'L'                 to  g_s_fieldcat-ddictxt,
*         15                  to  g_s_fieldcat-outputlen.
*  perform fc_s_flat using 'END_DATE' 'MKPF' 'BUDAT'.
*
**********************************************************************
**** END OF NOTE 1064332                                     "n1064332
**** new logic for fields start_date & end_date              "n1064332
**********************************************************************

* Always use the text from the text symbol                   "n1333069
  MOVE : TEXT-096            TO  G_S_FIELDCAT-SELTEXT_L, "opening stock
         TEXT-096            TO  G_S_FIELDCAT-SELTEXT_M,    "n1333069
         TEXT-096            TO  G_S_FIELDCAT-SELTEXT_S,    "n1333069
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         23                  TO  G_S_FIELDCAT-OUTPUTLEN,
         'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1399766
         'MEINS'             TO  G_S_FIELDCAT-QFIELDNAME.
  MOVE : 'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1441785
         'P'                 TO  G_S_FIELDCAT-INTTYPE,      "n1441785
         13                  TO  G_S_FIELDCAT-INTLEN.       "n1441785
  PERFORM FC_S_FLAT USING 'ANFMENGE' '' ''.                 "n1333069


  "added by govind on 19.05.2014
  MOVE : 'Opening Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_L, "end stock
        'Opening Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_M, "n1333069
         'Opening Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_S, "n1333069
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         23                 TO  G_S_FIELDCAT-OUTPUTLEN,
         'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1399766
         'MEINS'             TO  G_S_FIELDCAT-QFIELDNAME.   "n1333069
  MOVE : 'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1441785
         'P'                 TO  G_S_FIELDCAT-INTTYPE,      "n1441785
         13                  TO  G_S_FIELDCAT-INTLEN.       "n1441785
  PERFORM FC_S_FLAT USING 'OPNVOLUM' '' '' .

  MOVE : TEXT-097            TO  G_S_FIELDCAT-SELTEXT_L, "sum receipts
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         23                  TO  G_S_FIELDCAT-OUTPUTLEN,
         'MEINS'             TO  G_S_FIELDCAT-QFIELDNAME.
  PERFORM FC_S_FLAT USING 'SOLL' 'MSEG' 'MENGE'.


  "added by govind on 19.05.2014
  MOVE : 'Receipt Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_L, "end stock
        'Receipt Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_M, "n1333069
         'Receipt Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_S, "n1333069
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         23                 TO  G_S_FIELDCAT-OUTPUTLEN,
         'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1399766
         'MEINS'             TO  G_S_FIELDCAT-QFIELDNAME.   "n1333069
  MOVE : 'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1441785
         'P'                 TO  G_S_FIELDCAT-INTTYPE,      "n1441785
         13                  TO  G_S_FIELDCAT-INTLEN.       "n1441785
  PERFORM FC_S_FLAT USING 'RPVOLUM' '' '' .


  MOVE : TEXT-098            TO  G_S_FIELDCAT-SELTEXT_L, "sum issues
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         23                  TO  G_S_FIELDCAT-OUTPUTLEN,
         'MEINS'             TO  G_S_FIELDCAT-QFIELDNAME.
  PERFORM FC_S_FLAT USING 'HABEN' 'MSEG' 'MENGE'.

  "added by govind on 19.05.2014
  MOVE : 'Issue Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_L, "end stock
        'Issue Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_M, "n1333069
         'Issue Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_S, "n1333069
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         23                 TO  G_S_FIELDCAT-OUTPUTLEN,
         'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1399766
         'MEINS'             TO  G_S_FIELDCAT-QFIELDNAME.   "n1333069
  MOVE : 'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1441785
         'P'                 TO  G_S_FIELDCAT-INTTYPE,      "n1441785
         13                  TO  G_S_FIELDCAT-INTLEN.       "n1441785
  PERFORM FC_S_FLAT USING 'ISVOLUM' '' '' .

* Always use the text from the text symbol                   "n1333069
  MOVE : TEXT-099            TO  G_S_FIELDCAT-SELTEXT_L, "end stock
         TEXT-099            TO  G_S_FIELDCAT-SELTEXT_M,    "n1333069
         TEXT-099            TO  G_S_FIELDCAT-SELTEXT_S,    "n1333069
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         23                  TO  G_S_FIELDCAT-OUTPUTLEN,
         'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1399766
         'MEINS'             TO  G_S_FIELDCAT-QFIELDNAME.   "n1333069
  MOVE : 'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1441785
         'P'                 TO  G_S_FIELDCAT-INTTYPE,      "n1441785
         13                  TO  G_S_FIELDCAT-INTLEN.       "n1441785
  PERFORM FC_S_FLAT USING 'ENDMENGE' '' ''.


  "added by govind on 19.05.2014
  MOVE : 'Closing Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_L, "end stock
        'Closing Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_M, "n1333069
         'Closing Stk Qty In Ltrs'            TO  G_S_FIELDCAT-SELTEXT_S, "n1333069
         'L'                 TO  G_S_FIELDCAT-DDICTXT,
         23                 TO  G_S_FIELDCAT-OUTPUTLEN,
         'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1399766
         'MEINS'             TO  G_S_FIELDCAT-QFIELDNAME.   "n1333069
  MOVE : 'QUAN'              TO  G_S_FIELDCAT-DATATYPE,     "n1441785
         'P'                 TO  G_S_FIELDCAT-INTTYPE,      "n1441785
         13                  TO  G_S_FIELDCAT-INTLEN.       "n1441785
  PERFORM FC_S_FLAT USING 'ENDVOLUM' '' '' .

  PERFORM FC_S_FLAT USING 'MEINS' 'MARA' 'MEINS'.

  IF  BWBST = 'X'.
*   process the values, too
    MOVE : TEXT-100     TO  G_S_FIELDCAT-SELTEXT_L, "opening value
           TEXT-100          TO  G_S_FIELDCAT-SELTEXT_M,    "n1333069
           TEXT-100          TO  G_S_FIELDCAT-SELTEXT_S,    "n1333069
           'L'               TO  G_S_FIELDCAT-DDICTXT,
           23                TO  G_S_FIELDCAT-OUTPUTLEN,
           'WAERS'           TO  G_S_FIELDCAT-CFIELDNAME.
    MOVE : 'QUAN'            TO  G_S_FIELDCAT-DATATYPE,     "n1441785
           'P'               TO  G_S_FIELDCAT-INTTYPE,      "n1441785
           13                TO  G_S_FIELDCAT-INTLEN.       "n1441785
    PERFORM FC_S_FLAT USING 'ANFWERT' '' ''.                "n1333069

    MOVE : TEXT-101     TO  G_S_FIELDCAT-SELTEXT_L,  "sum GR values
           'L'          TO  G_S_FIELDCAT-DDICTXT,
           23           TO  G_S_FIELDCAT-OUTPUTLEN,
           'WAERS'      TO  G_S_FIELDCAT-CFIELDNAME.
    PERFORM FC_S_FLAT USING 'SOLLWERT' 'MSEG' 'DMBTR'.

    MOVE : TEXT-102     TO  G_S_FIELDCAT-SELTEXT_L,  "sum GI values
           'L'          TO  G_S_FIELDCAT-DDICTXT,
           23           TO  G_S_FIELDCAT-OUTPUTLEN,
           'WAERS'      TO  G_S_FIELDCAT-CFIELDNAME.
    PERFORM FC_S_FLAT USING 'HABENWERT' 'MSEG' 'DMBTR'.

    MOVE : TEXT-103     TO  G_S_FIELDCAT-SELTEXT_L,   "end value
           TEXT-103          TO  G_S_FIELDCAT-SELTEXT_M,    "n1333069
           TEXT-103          TO  G_S_FIELDCAT-SELTEXT_S,    "n1333069
           'L'               TO  G_S_FIELDCAT-DDICTXT,
           23                TO  G_S_FIELDCAT-OUTPUTLEN,
           'WAERS'           TO  G_S_FIELDCAT-CFIELDNAME.
    MOVE : 'QUAN'            TO  G_S_FIELDCAT-DATATYPE,     "n1441785
           'P'               TO  G_S_FIELDCAT-INTTYPE,      "n1441785
           13                TO  G_S_FIELDCAT-INTLEN.       "n1441785
    PERFORM FC_S_FLAT USING 'ENDWERT' '' ''.                "n1333069

*    PERFORM FC_S_FLAT USING 'WAERS' 'T001' 'WAERS'.
  ENDIF.

*ENHANCEMENT-POINT EHP605_RM07MLBD_13 SPOTS ES_RM07MLBD .

ENDFORM.                     " create_fieldcat_totals_flat.



*---------------------------------------------------------------------------------------------------*
*    FORM WHICH CONTAINS THE CODING FOR ADDING THE EXTRA COLOUMNS
*    PLANT NAME, OLD MATERIAL NUMBER, MATERIAL TYPE AND MATERIAL TYPE DESCRIPTION   " ADDED BY PHILIP
*---------------------------------------------------------------------------------------------------*


FORM CREATE_EXTRA_FIELDS.
*BREAK-POINT.
  SELECT MATNR
         BISMT
         MTART
         MATKL
         SPART
         VOLUM FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN G_T_TOTALS_FLAT WHERE MATNR = G_T_TOTALS_FLAT-MATNR AND SPART IN SPART
    AND MATKL IN MATKL   .


  SELECT SPRAS MTART MTBEZ FROM T134T INTO TABLE GT_T134T_MTBEZ FOR ALL ENTRIES IN GT_MARA WHERE MTART = GT_MARA-MTART AND SPRAS = 'EN'.

  SELECT WERKS NAME1 FROM T001W INTO TABLE GT_T001W_NAME FOR ALL ENTRIES IN G_T_TOTALS_FLAT WHERE WERKS = G_T_TOTALS_FLAT-BWKEY.


    SELECT SPART VTEXT FROM  TSPAT INTO TABLE GT_TSPAT FOR ALL ENTRIES IN GT_MARA WHERE SPART = GT_MARA-SPART .
    SELECT MATKL WGBEZ FROM T023T INTO TABLE GT_T023T FOR ALL ENTRIES IN GT_MARA WHERE MATKL = GT_MARA-MATKL.


  IF GT_MARA[] IS NOT INITIAL.
    SELECT MATNR
         WERKS
         SPEME LGORT  LFGJA FROM MARD INTO TABLE GT_MARD FOR ALL ENTRIES IN GT_MARA WHERE MATNR = GT_MARA-MATNR AND  LFGJA GT 2013.
  ENDIF.

  IF GT_MARD[] IS NOT INITIAL .
    SELECT WERKS
          LGORT
          LGOBE FROM T001L INTO TABLE GT_T001L FOR ALL ENTRIES IN GT_MARD WHERE   LGORT = GT_MARD-LGORT.
  ENDIF.

  LOOP AT G_T_TOTALS_FLAT  INTO G_S_TOTALS_FLAT .

    READ TABLE GT_MARA INTO GS_MARA  WITH KEY MATNR = G_S_TOTALS_FLAT-MATNR .

    IF SY-SUBRC = 0.

      MOVE GS_MARA-BISMT TO G_S_TOTALS_FLAT-BISMT.

      MOVE GS_MARA-MTART TO G_S_TOTALS_FLAT-MTART.
      MOVE GS_MARA-MATKL TO G_S_TOTALS_FLAT-MATKL.
      MOVE GS_MARA-SPART TO G_S_TOTALS_FLAT-SPART.
      MOVE GS_MARA-VOLUM TO G_S_TOTALS_FLAT-VOLUM.
      MODIFY G_T_TOTALS_FLAT FROM G_S_TOTALS_FLAT TRANSPORTING BISMT MTART MATKL SPART VOLUM.

    ENDIF.



    IF SY-SUBRC = 0.
      READ TABLE GT_T023T INTO GS_T023T WITH KEY MATKL = G_S_TOTALS_FLAT-MATKL."GS_MARA-MATKL.
      MOVE GS_T023T-WGBEZ TO G_S_TOTALS_FLAT-WGBEZ.
      MODIFY G_T_TOTALS_FLAT FROM G_S_TOTALS_FLAT TRANSPORTING WGBEZ.
    ENDIF.


    READ TABLE GT_T001W_NAME INTO GS_T001W_NAME WITH KEY WERKS = G_S_TOTALS_FLAT-BWKEY.

    IF SY-SUBRC = 0.

      MOVE GS_T001W_NAME-NAME1 TO G_S_TOTALS_FLAT-NAME1.

      MODIFY G_T_TOTALS_FLAT FROM G_S_TOTALS_FLAT TRANSPORTING NAME1.

    ENDIF.


    READ TABLE GT_T134T_MTBEZ INTO GS_T134T_MTBEZ WITH KEY MTART = G_S_TOTALS_FLAT-MTART.

    IF SY-SUBRC = 0.

      MOVE GS_T134T_MTBEZ-MTBEZ TO G_S_TOTALS_FLAT-MTBEZ.

      MODIFY G_T_TOTALS_FLAT FROM G_S_TOTALS_FLAT TRANSPORTING MTBEZ.

    ENDIF.



    SUM_VOLUM =  GS_MARA-VOLUM * G_S_TOTALS_FLAT-ENDMENGE / 1000 .
    SUM_OQTY =  GS_MARA-VOLUM * G_S_TOTALS_FLAT-ANFMENGE / 1000.
    SUM_RQTY = GS_MARA-VOLUM * G_S_TOTALS_FLAT-SOLL / 1000.
    SUM_IQTY = GS_MARA-VOLUM * G_S_TOTALS_FLAT-HABEN / 1000.



    MOVE SUM_VOLUM  TO G_S_TOTALS_FLAT-ENDVOLUM.
    MOVE SUM_OQTY TO G_S_TOTALS_FLAT-OPNVOLUM.
    MOVE SUM_RQTY TO G_S_TOTALS_FLAT-RPVOLUM .
    MOVE SUM_IQTY TO G_S_TOTALS_FLAT-ISVOLUM.

*    MOVE TRANS_VAL TO G_S_TOTALS_FLAT-T_VAL.
*    MOVE BLOCK_VAL  TO G_S_TOTALS_FLAT-B_VAL.

    MODIFY G_T_TOTALS_FLAT FROM G_S_TOTALS_FLAT TRANSPORTING ENDVOLUM OPNVOLUM RPVOLUM ISVOLUM  .

    READ TABLE GT_TSPAT INTO GS_TSPAT WITH KEY SPART = G_S_TOTALS_FLAT-SPART.
    IF SY-SUBRC = 0.
      MOVE GS_TSPAT-VTEXT TO G_S_TOTALS_FLAT-VTEXT.
      MODIFY G_T_TOTALS_FLAT FROM G_S_TOTALS_FLAT TRANSPORTING  VTEXT.
    ENDIF.

    READ TABLE GT_MARD INTO GS_MARD WITH KEY MATNR = G_S_TOTALS_FLAT-MATNR .
    IF SY-SUBRC = 0.
      MOVE GS_MARD-LGORT TO G_S_TOTALS_FLAT-LGORT.
      MODIFY  G_T_TOTALS_FLAT FROM G_S_TOTALS_FLAT TRANSPORTING LGORT .
    ENDIF.
    READ TABLE GT_T001L INTO GS_T001L WITH KEY LGORT = G_S_TOTALS_FLAT-LGORT  .
    IF SY-SUBRC = 0.
      MOVE GS_T001L-LGOBE  TO G_S_TOTALS_FLAT-LGOBE.
      MODIFY  G_T_TOTALS_FLAT FROM G_S_TOTALS_FLAT TRANSPORTING LGOBE .
    ENDIF.
  ENDLOOP.
ENDFORM. " CREATE_EXTRA_FIELDS.
*----------------------------------------------------------------------*
*    FC_S_FLAT
*----------------------------------------------------------------------*

FORM FC_S_FLAT     USING     L_FIELDNAME     TYPE FIELDNAME
                             L_REF_TABNAME   TYPE DDOBJNAME
                             L_REF_FIELDNAME TYPE FIELDNAME.

  ADD  : 1                   TO  G_F_COL_POS.
  MOVE : L_FIELDNAME         TO  G_S_FIELDCAT-FIELDNAME,
         G_F_COL_POS         TO  G_S_FIELDCAT-COL_POS,
         'G_T_TOTALS_FLAT'   TO  G_S_FIELDCAT-TABNAME,
         L_REF_TABNAME       TO  G_S_FIELDCAT-REF_TABNAME,
         L_REF_FIELDNAME     TO  G_S_FIELDCAT-REF_FIELDNAME.
*ENHANCEMENT-POINT FC_S_FLAT_01 SPOTS ES_RM07MLBD.
  APPEND  G_S_FIELDCAT       TO  G_T_FIELDCAT_TOTALS_FLAT.
  CLEAR                      G_S_FIELDCAT.

ENDFORM.                     "fc_s_flat

*----------------------------------------------------------------------*
*    alv_flat_list_sums_only
*----------------------------------------------------------------------*

FORM ALV_FLAT_LIST_SUMS_ONLY.

* assign the form routines to the events
  MOVE :  'PF_STATUS_SET'         TO  G_T_EVENTS_TOTALS_FLAT-NAME,
          'PF_STATUS_SET_TOTALS'  TO  G_T_EVENTS_TOTALS_FLAT-FORM.
  APPEND                              G_T_EVENTS_TOTALS_FLAT.

  MOVE :  'USER_COMMAND'          TO  G_T_EVENTS_TOTALS_FLAT-NAME,
          'USER_COMMAND_TOTALS'   TO  G_T_EVENTS_TOTALS_FLAT-FORM.
  APPEND                              G_T_EVENTS_TOTALS_FLAT.

  MOVE : 'TOP_OF_PAGE'            TO  G_T_EVENTS_TOTALS_FLAT-NAME,
         'TOP_OF_PAGE_TOTALS'     TO  G_T_EVENTS_TOTALS_FLAT-FORM.
  APPEND                              G_T_EVENTS_TOTALS_FLAT.

  MOVE :       'END_OF_LIST'      TO  G_T_EVENTS_TOTALS_FLAT-NAME,
         'PRINT_END_OF_LIST'      TO  G_T_EVENTS_TOTALS_FLAT-FORM.
  APPEND                              G_T_EVENTS_TOTALS_FLAT.

* handling for double click
  G_S_LAYOUT_TOTALS_FLAT-F2CODE           = '9PBP'.
  G_S_LAYOUT_TOTALS_FLAT-COLTAB_FIELDNAME = 'COLOR'.

  IF  G_FLAG_BREAK-B6 = 'X'.                                "n921164
    BREAK-POINT              ID MMIM_REP_MB5B.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  ENDIF.                                                    "n921164

* liste aufbauen
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      I_INTERFACE_CHECK  = G_FLAG_I_CHECK
      I_CALLBACK_PROGRAM = REPID
      IS_LAYOUT          = G_S_LAYOUT_TOTALS_FLAT
      IT_FIELDCAT        = G_T_FIELDCAT_TOTALS_FLAT[]
      IT_SORT            = G_T_SORTTAB
      I_DEFAULT          = 'X'  "allow default variant
      I_SAVE             = 'A'
      IS_VARIANT         = G_S_VARI_SUMFL
      IT_EVENTS          = G_T_EVENTS_TOTALS_FLAT[]
      IS_PRINT           = G_S_PRINT
    TABLES
      T_OUTTAB           = G_T_TOTALS_FLAT
    EXCEPTIONS
      OTHERS             = 1.

  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                     "alv_flat_list_sums_only

*----------------------------------------------------------------------*
*    create_fieldcat_totals_hq
*----------------------------------------------------------------------*
*baustelle
FORM CREATE_FIELDCAT_TOTALS_HQ.

* create fieldcat
  CLEAR : G_S_FIELDCAT,      G_F_COL_POS.

* part 1 : for the header table
  IF  BWBST = 'X'.                                          "n999530
    PERFORM  FC_HQ
        USING   'G_T_TOTALS_HEADER' 'BWKEY'  'MBEW'  'BWKEY'.
  ENDIF.                                                    "n999530

  IF BWBST IS INITIAL.                                      "n999530
    PERFORM  FC_HQ
        USING   'G_T_TOTALS_HEADER' 'WERKS'  'MARC'  'WERKS'.

    MOVE : 'X'                 TO  G_S_FIELDCAT-NO_OUT.
    MOVE : 30                  TO  G_S_FIELDCAT-OUTPUTLEN,  "n999530
           TEXT-024            TO  G_S_FIELDCAT-SELTEXT_L,  "n999530
           'L'                 TO  G_S_FIELDCAT-DDICTXT.    "n999530
    PERFORM  FC_HQ
      USING  'G_T_TOTALS_HEADER'  'NAME1'  'T001W'  'NAME1'.
  ENDIF.                                                    "n999530

  PERFORM  FC_HQ
    USING  'G_T_TOTALS_HEADER'  'MATNR'  'MARA'  'MATNR'.

  IF  SOBKZ IS INITIAL.
    MOVE : 'X'               TO  G_S_FIELDCAT-NO_OUT.
  ENDIF.

  PERFORM  FC_HQ
    USING  'G_T_TOTALS_HEADER'  'SOBKZ'  'MSLB'  'SOBKZ'.

  PERFORM  FC_HQ
    USING  'G_T_TOTALS_HEADER'  'MAKTX'  'MAKT'  'MAKTX'.

  IF  XCHAR IS INITIAL.
    MOVE : 'X'               TO  G_S_FIELDCAT-NO_OUT.
  ENDIF.

  PERFORM  FC_HQ
      USING  'G_T_TOTALS_HEADER'  'CHARG'  'MCHB'  'CHARG'.

*ENHANCEMENT-POINT EHP605_RM07MLBD_14 SPOTS ES_RM07MLBD .

** part 2 : for the item table

* hidden key fields
  MOVE : 'X'               TO  G_S_FIELDCAT-NO_OUT.
  PERFORM  FC_HQ
      USING   'G_T_TOTALS_ITEM' 'BWKEY'  'MBEW'  'BWKEY'.

  MOVE : 'X'               TO  G_S_FIELDCAT-NO_OUT.
  PERFORM  FC_HQ
      USING   'G_T_TOTALS_ITEM' 'WERKS'  'MARC'  'WERKS'.

  MOVE : 'X'               TO  G_S_FIELDCAT-NO_OUT.
  PERFORM  FC_HQ
    USING  'G_T_TOTALS_ITEM'  'MATNR'  'MARA'  'MATNR'.

  MOVE : 'X'               TO  G_S_FIELDCAT-NO_OUT.
  PERFORM  FC_HQ
      USING  'G_T_TOTALS_ITEM'  'CHARG'  'MCHB'  'CHARG'.

  MOVE : 'X'               TO  G_S_FIELDCAT-NO_OUT.
  PERFORM  FC_HQ
      USING  'G_T_TOTALS_ITEM'  'COUNTER'  ' '  ''.

  MOVE : 40                   TO  G_S_FIELDCAT-OUTPUTLEN.
  PERFORM  FC_HQ
      USING  'G_T_TOTALS_ITEM'  'STOCK_TYPE' SPACE SPACE.

* do not allow to form sums for the column quantity         "n951316
  MOVE : 'X'            TO  G_S_FIELDCAT-NO_SUM.            "n951316
  MOVE : 23             TO  G_S_FIELDCAT-OUTPUTLEN,
         TEXT-104      TO  G_S_FIELDCAT-SELTEXT_L,  "quantities
         'L'            TO  G_S_FIELDCAT-DDICTXT,
         'MENGE_D'      TO  G_S_FIELDCAT-ROLLNAME,
         'MEINS'        TO  G_S_FIELDCAT-QFIELDNAME.
  PERFORM  FC_HQ
    USING  'G_T_TOTALS_ITEM'  'MENGE' SPACE SPACE.

  PERFORM  FC_HQ
    USING  'G_T_TOTALS_ITEM'  'MEINS'   'MARA'  'MEINS'.

  IF  BWBST = 'X'.
*   with valuation
*   do not allow to form sums for the column value          "n951316
    MOVE : 'X'          TO  G_S_FIELDCAT-NO_SUM.            "n951316
    MOVE : 23           TO  G_S_FIELDCAT-OUTPUTLEN,
           TEXT-105     TO  G_S_FIELDCAT-SELTEXT_L,   "values
           'L'          TO  G_S_FIELDCAT-DDICTXT,
           'DMBTR'      TO  G_S_FIELDCAT-ROLLNAME,
           'WAERS'      TO  G_S_FIELDCAT-CFIELDNAME.
    PERFORM  FC_HQ
      USING  'G_T_TOTALS_ITEM'  'WERT' SPACE SPACE.

    PERFORM  FC_HQ
      USING  'G_T_TOTALS_ITEM'  'WAERS'   'T001'  'WAERS'.
  ENDIF.

ENDFORM.                     "create_fieldcat_totals_hq

*----------------------------------------------------------------------*
*    FC_HQ
*----------------------------------------------------------------------*

FORM FC_HQ         USING     L_TABNAME        TYPE  DDOBJNAME
                             L_FIELDNAME      TYPE  FIELDNAME
                             L_REF_TABNAME    TYPE  DDOBJNAME
                             L_REF_FIELDNAME  TYPE  FIELDNAME.

  ADD  : 1                   TO  G_F_COL_POS.
  MOVE : L_FIELDNAME         TO  G_S_FIELDCAT-FIELDNAME,
         G_F_COL_POS         TO  G_S_FIELDCAT-COL_POS,
         L_TABNAME           TO  G_S_FIELDCAT-TABNAME,
         L_REF_TABNAME       TO  G_S_FIELDCAT-REF_TABNAME,
         L_REF_FIELDNAME     TO  G_S_FIELDCAT-REF_FIELDNAME.
* ENHANCEMENT-POINT FC_HQ_01 SPOTS ES_RM07MLBD.

  APPEND  G_S_FIELDCAT       TO  G_T_FIELDCAT_TOTALS_HQ.
  CLEAR                      G_S_FIELDCAT.

ENDFORM.                     "fc_hq

*----------------------------------------------------------------------*
* alv_hierseq_list_totals
*----------------------------------------------------------------------*

FORM ALV_HIERSEQ_LIST_TOTALS.

* fill layout : consider double click and color subtables
  G_S_LAYOUT_TOTALS_HQ-COLTAB_FIELDNAME = 'COLOR'.
  G_S_LAYOUT_TOTALS_HQ-F2CODE           = '9PBP'.

* create other tables and structures
  MOVE : 'BWKEY'             TO  G_S_KEYINFO_TOTALS_HQ-HEADER01,
         'BWKEY'             TO  G_S_KEYINFO_TOTALS_HQ-ITEM01,

         'WERKS'             TO  G_S_KEYINFO_TOTALS_HQ-HEADER02,
         'WERKS'             TO  G_S_KEYINFO_TOTALS_HQ-ITEM02,

         'MATNR'             TO  G_S_KEYINFO_TOTALS_HQ-HEADER03,
         'MATNR'             TO  G_S_KEYINFO_TOTALS_HQ-ITEM03,

         'CHARG'             TO  G_S_KEYINFO_TOTALS_HQ-HEADER04,
         'CHARG'             TO  G_S_KEYINFO_TOTALS_HQ-ITEM04,

         'COUNTER'           TO  G_S_KEYINFO_TOTALS_HQ-ITEM05.

* create the events table
  MOVE : 'PF_STATUS_SET'          TO  EVENTS_HIERSEQ-NAME,
         'PF_STATUS_SET_TOTALS'  TO  EVENTS_HIERSEQ-FORM.
  APPEND                              EVENTS_HIERSEQ.

  MOVE : 'USER_COMMAND'           TO  EVENTS_HIERSEQ-NAME,
         'USER_COMMAND_TOTALS'    TO  EVENTS_HIERSEQ-FORM.
  APPEND                              EVENTS_HIERSEQ.

  MOVE : 'TOP_OF_PAGE'            TO  EVENTS_HIERSEQ-NAME,
         'TOP_OF_PAGE_TOTALS'     TO  EVENTS_HIERSEQ-FORM.
  APPEND                              EVENTS_HIERSEQ.

  MOVE :       'END_OF_LIST'      TO  EVENTS_HIERSEQ-NAME,
         'PRINT_END_OF_LIST'      TO  EVENTS_HIERSEQ-FORM.
  APPEND                              EVENTS_HIERSEQ.

* create the sort table g_t_SORT_totals_hq
  CLEAR                           G_S_SORT_TOTALS_HQ.
  MOVE : 'G_T_TOTALS_ITEM'        TO  G_S_SORT_TOTALS_HQ-TABNAME,
         'X'                      TO  G_S_SORT_TOTALS_HQ-UP.

  MOVE  'BWKEY'                   TO  G_S_SORT_TOTALS_HQ-FIELDNAME.
  ADD     1                       TO  G_S_SORT_TOTALS_HQ-SPOS.
  APPEND  G_S_SORT_TOTALS_HQ      TO  G_T_SORT_TOTALS_HQ.

  MOVE  'WERKS'                   TO  G_S_SORT_TOTALS_HQ-FIELDNAME.
  ADD     1                       TO  G_S_SORT_TOTALS_HQ-SPOS.
  APPEND  G_S_SORT_TOTALS_HQ      TO  G_T_SORT_TOTALS_HQ.

  MOVE  'MATNR'                   TO  G_S_SORT_TOTALS_HQ-FIELDNAME.
  ADD     1                       TO  G_S_SORT_TOTALS_HQ-SPOS.
  APPEND  G_S_SORT_TOTALS_HQ      TO  G_T_SORT_TOTALS_HQ.

  MOVE  'CHARG'                   TO  G_S_SORT_TOTALS_HQ-FIELDNAME.
  ADD     1                       TO  G_S_SORT_TOTALS_HQ-SPOS.
  APPEND  G_S_SORT_TOTALS_HQ      TO  G_T_SORT_TOTALS_HQ.

  MOVE  'COUNTER'                 TO  G_S_SORT_TOTALS_HQ-FIELDNAME.
  ADD     1                       TO  G_S_SORT_TOTALS_HQ-SPOS.
  APPEND  G_S_SORT_TOTALS_HQ      TO  G_T_SORT_TOTALS_HQ.

  IF  G_FLAG_BREAK-B7 = 'X'.                                "n921164
    BREAK-POINT              ID MMIM_REP_MB5B.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  ENDIF.                                                    "n921164

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      I_INTERFACE_CHECK  = G_FLAG_I_CHECK
      I_CALLBACK_PROGRAM = REPID
      IT_EVENTS          = EVENTS_HIERSEQ[]
      IS_LAYOUT          = G_S_LAYOUT_TOTALS_HQ
      IS_PRINT           = G_S_PRINT
      IT_FIELDCAT        = G_T_FIELDCAT_TOTALS_HQ
      IT_SORT            = G_T_SORT_TOTALS_HQ
      I_DEFAULT          = 'X'
      I_SAVE             = 'A'
      IS_VARIANT         = G_S_VARI_SUMHQ
      I_TABNAME_HEADER   = 'G_T_TOTALS_HEADER'
      I_TABNAME_ITEM     = 'G_T_TOTALS_ITEM'
      IS_KEYINFO         = G_S_KEYINFO_TOTALS_HQ
    TABLES
      T_OUTTAB_HEADER    = G_T_TOTALS_HEADER[]
      T_OUTTAB_ITEM      = G_T_TOTALS_ITEM[]
    EXCEPTIONS
      OTHERS             = 1.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                     " alv_hierseq_list_totals

*-----------------------------------------------------------"n599218


*  Aus dieser Unterroutine heraus werden implizit, d.h. in der Schleife
*  über alle selektierten Bestände, die zugehörigen Materialbelege
*  aufgerufen.
*  Die Bestände werden zum Anfangs- und Enddatum als Summen zu
*  folgendem Schlüssel, der im Listkopf geführt wird, ausgegeben:
*  Buchungskreis bzw. Werk, Material, Charge. Nicht-chargenpflichtige
*  Materialien werden auf Materialebene angezeigt.
*  Es folgt jeweils eine Liste mit den einzelnen Belegpositionen.

*********************** Ende HAUPTPROGRAMM *****************************
*
************************* FORMROUTINEN *********************************

*&---------------------------------------------------------------------*
*&      Form  EINGABEN_PRUEFEN
*&---------------------------------------------------------------------*
*       Prüfung der Eingaben auf dem Selektionsbild                    *
*----------------------------------------------------------------------*
FORM EINGABEN_PRUEFEN.

* check the entries only in releases >= 46B
  CALL FUNCTION 'MMIM_ENTRYCHECK_MAIN'
    TABLES
      IT_MATNR = MATNR"#EC CI_FLDEXT_OK[2215424]

      IT_WERKS = WERKS
      IT_LGORT = LGORT
      IT_BWART = BWART
      IT_BUKRS = BUKRS.

*  Die Selektionseingaben Buchungskreis und Werk werden hierarchisch
*  verstanden, d.h. es werden nur Werke innerhalb der angegebenen
*  Buchungskreise selektiert.
*  Lagerort-/Chargenbestand: Da die Werksbezeichnung eindeutig ist,
*  finden alle Selektionen auf Werksebene bzw. - falls mindestens ein
*  Lagerort eingegeben wurde - auf der Ebene der eingegebenen Lagerorte
*  statt. Die Ausgabe erfolgt auf Werksebene des Materials / der Charge.
*  Bewerteter Bestand: Die Ausgabe erfolgt auf Bewertungskreisebene,
*  d.h. je nach Einstellung in der Tabelle TCURM auf Werks- oder
*  Buchungskreisebene.

*  Feststellen, ob der Bewertungskreis auf Buchungskreis- oder
*  Werksebene liegt:
*  tcurm-bwkrs_cus = 1  =>  Bewertungskreis auf Werksebene,
*  tcurm-bwkrs_cus = 3  =>  Bewertungskreis auf Buchungskreisebene.
  SELECT BWKRS_CUS"#EC CI_NOORDER
     "Added by SPLABAP during code remediation
    FROM TCURM INTO CURM. ENDSELECT.

  IF XCHAR = ' ' AND NOT CHARG-LOW IS INITIAL.
    XCHAR = 'X'.
  ENDIF.
  IF XCHAR = ' ' AND NOT XNOMCHB IS INITIAL.                "838360_v
    XCHAR = 'X'.
  ENDIF.                                                    "838360_^

  IF SBBST = 'X' AND SOBKZ IS INITIAL.
    MESSAGE E286.
*   Bitte ein Sonderbestandskennzeichen eingeben.
  ELSEIF SBBST = ' ' AND NOT SOBKZ IS INITIAL.
    CLEAR SOBKZ.
    MESSAGE W287.
*   Sonderbestandskennzeichen wird zurückgesetzt.
  ENDIF.

* reset the entries for plant when valuation area is        "n599218
* company code and mode is valuated stock                   "n599218
  IF     CURM  = '3'      AND                               "n599218
         BWBST = 'X'.                                       "n599218
    IF  NOT WERKS[]  IS INITIAL.                            "n599218
*     reset the restricts for plants                        "n599218
      CLEAR                  WERKS.                         "n599218
      REFRESH                WERKS.                         "n599218
*     text-074 : valuation area = company code              "n599218
*     text-075 : entries for plant will be reset            "n599218
      MESSAGE W010(AD) WITH TEXT-074 TEXT-075 SPACE SPACE.  "n599218
    ENDIF.                                                  "n599218
  ENDIF.                                                    "n599218

  IF BWBST = 'X' AND NOT CHARG IS INITIAL
    OR BWBST = 'X' AND NOT XCHAR IS INITIAL.
    CLEAR CHARG. REFRESH CHARG.
    MESSAGE W285.
*   Charge wird zurückgesetzt.
  ENDIF.
  IF BWBST = 'X' AND NOT LGORT IS INITIAL.
    CLEAR LGORT. REFRESH LGORT.
    MESSAGE W284.
*   Lagerort wird zurückgesetzt.
  ENDIF.

* consider and prepare select-options depending on the required
* special stock indicator
  REFRESH                    G_RA_SOBKZ.
  CLEAR                      G_RA_SOBKZ.

  IF      LGBST = 'X'.       "only Storage loc./batch stock
*   create ranges table : select only sobkz = space
    PERFORM F0500_APPEND_RA_SOBKZ   USING  C_SPACE.

  ELSEIF  BWBST = 'X'.       "only valuated stocks
*   take all special stock indicators / the record selection
*   will be done after the database selection

  ELSEIF  SBBST = 'X'.       "only special stocks
    PERFORM F0500_APPEND_RA_SOBKZ   USING  SOBKZ.

*ENHANCEMENT-SECTION     RM07MLBD_02 SPOTS ES_RM07MLBD.
    IF      SOBKZ  =  'O'  OR
            SOBKZ  =  'V'  OR
            SOBKZ  =  'W'  OR
            SOBKZ  =  'E'  OR
            SOBKZ  =  'K'  OR
            SOBKZ  =  'M'  OR
            SOBKZ  =  'Q'  OR
            SOBKZ  =  'T'.
*     ok; no aktion taken
    ELSE.
      SET CURSOR             FIELD  'SOBKZ'.
*     Sonderbestandskennzeichen nicht vorhanden
      MESSAGE                E221.
    ENDIF.
*END-ENHANCEMENT-SECTION.
  ENDIF.

  IF BWBST = 'X' AND NOT BWART IS INITIAL.
    CLEAR BWART. REFRESH BWART.
    MESSAGE W298.
*   Bewegungsart wird zurückgesetzt
  ENDIF.
  IF BWBST = ' ' AND NOT BWTAR IS INITIAL.
    CLEAR BWTAR. REFRESH BWTAR.
    MESSAGE W288.
*   Bewertungsart wird zurückgesetzt.
  ENDIF.

  IF GV_SWITCH_EHP6RU = ABAP_TRUE AND NOT HKONT[] IS INITIAL.
    IF BWBST = ' '.
*     G/L account will be reset, if stock type is not Valuated Stock
      CLEAR HKONT. REFRESH HKONT.
      MESSAGE W481.
    ELSE.
*     Company code or plant should be filled to build G_T_ORGAN table
      IF BUKRS[] IS INITIAL AND WERKS[] IS INITIAL.
        SET CURSOR FIELD 'HKONT-LOW'.
        MESSAGE E480.
      ENDIF.
    ENDIF.
  ENDIF.

* The function "no cancellations" is not possible
* for valuated stock
*   for the selection of the reversal movements only in release >=45B
  IF NOSTO = 'X' AND BWBST = 'X'.                           "204463
    MESSAGE E151(E1) WITH 'VALUATED_STOCK'                  "204463
                       'NO_CANCELLATIONS'.                  "204463
  ENDIF.                                                    "204463

  IF NOT P_VARI IS INITIAL.
    MOVE VARIANTE TO DEF_VARIANTE.
    MOVE P_VARI TO DEF_VARIANTE-VARIANT.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        I_SAVE     = VARIANT_SAVE
      CHANGING
        CS_VARIANT = DEF_VARIANTE.
    VARIANTE = DEF_VARIANTE.
  ELSE.
*   the user wants no initial display variant               "n599218
    IF  NOT ALV_DEFAULT_VARIANT  IS INITIAL.                "n599218
*     but the SAP-LIST-VIEWER will apply the existing       "n599218
*     initial display variant / emerge warning 393 ?        "n599218
      CALL FUNCTION          'ME_CHECK_T160M'               "n599218
        EXPORTING                                           "n599218
          I_ARBGB            = 'M7'                         "n599218
          I_MSGNR            = '393'                        "n599218
        EXCEPTIONS                                          "n599218
          NOTHING            = 0                            "n599218
          OTHERS             = 1.                           "n599218
                                                            "n599218
      IF SY-SUBRC <> 0.                                     "n599218
*       list will be created using the initial layout &     "n599218
        MESSAGE W393(M7)     WITH  ALV_DEFAULT_VARIANT.     "n599218
      ENDIF.                                                "n599218
    ENDIF.                                                  "n599218

    CLEAR VARIANTE.
    VARIANTE-REPORT = REPID.
  ENDIF.

ENDFORM.                               " EINGABEN_PRÜFEN

*----------------------------------------------------------------------*
*    VARIANT_CHECK_EXISTENCE
*----------------------------------------------------------------------*

FORM VARIANT_CHECK_EXISTENCE
         USING  L_VARI       LIKE  DISVARIANT-VARIANT
                LS_VARI      LIKE  DISVARIANT
                LS_VARI_DEF  LIKE  DISVARIANT.


  MOVE  L_VARI               TO  LS_VARI-VARIANT.

  IF  NOT L_VARI IS INITIAL.
*   parameter for the variant is filled.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        I_SAVE     = 'A'
      CHANGING
        CS_VARIANT = LS_VARI.

*   in the case the variant does not exist this function
*   module sends the error message directly
  ELSE.
*   the user wants no initial display variant
    IF  NOT LS_VARI_DEF-VARIANT  IS INITIAL.
*     but the SAP-LIST-VIEWER will apply the existing       "n599218
*     initial display variant / emerge warning 393 ?        "n599218
      CALL FUNCTION          'ME_CHECK_T160M'               "n599218
        EXPORTING                                           "n599218
          I_ARBGB            = 'M7'                         "n599218
          I_MSGNR            = '393'                        "n599218
        EXCEPTIONS                                          "n599218
          NOTHING            = 0                            "n599218
          OTHERS             = 1.                           "n599218
                                                            "n599218
      IF SY-SUBRC <> 0.                                     "n599218
*       list will be created using the initial layout &     "n599218
        MESSAGE W393(M7)     WITH  LS_VARI_DEF-VARIANT.     "n599218
      ENDIF.                                                "n599218
    ENDIF.                                                  "n599218
  ENDIF.

ENDFORM.                     "VARIANT_CHECK_EXISTENCE

*----------------------------------------------------------------------*
*    get_the_default_VARIANT
*----------------------------------------------------------------------*

FORM GET_THE_DEFAULT_VARIANT
         USING     L_VARI      LIKE  DISVARIANT-VARIANT
                   LS_VARI     LIKE  DISVARIANT
                   LS_VARI_DEF LIKE  DISVARIANT.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = VARIANT_SAVE
    CHANGING
      CS_VARIANT = LS_VARI_DEF
    EXCEPTIONS
      NOT_FOUND  = 2.

  IF SY-SUBRC = 0.
*   save the initial, e.g. default variant
    MOVE-CORRESPONDING LS_VARI_DEF     TO  LS_VARI.
    MOVE : LS_VARI_DEF-VARIANT         TO  L_VARI.
  ENDIF.

ENDFORM.                     "VARIANT_VALUE_REQUEST_F4

*----------------------------------------------------------------------*
*    VARIANT_VALUE_REQUEST_F4
*----------------------------------------------------------------------*

FORM VARIANT_VALUE_REQUEST_F4
         USING     L_VARI    LIKE  DISVARIANT-VARIANT
                   LS_VARI   LIKE  DISVARIANT.

  DATA : LS_VARI_RETURN      LIKE  DISVARIANT.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT          = LS_VARI
      I_SAVE              = 'A'
*     it_default_fieldcat =
    IMPORTING
      E_EXIT              = VARIANT_EXIT
      ES_VARIANT          = LS_VARI_RETURN
    EXCEPTIONS
      NOT_FOUND           = 2.

  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF VARIANT_EXIT = SPACE.
      MOVE  LS_VARI_RETURN-VARIANT   TO  L_VARI.
    ENDIF.
  ENDIF.

ENDFORM.                     "VARIANT_VALUE_REQUEST_F4

*&---------------------------------------------------------------------*

*---------------------- bewerteter Bestand ----------------------------*

FORM AKTUELLE_BST_BWBST.

* define local working areas  / for the result of the       "n450764
* database selections and the control break                 "n450764
  DATA : L_T_MBEW              TYPE  STAB_MBEW,             "n450764
         L_S_MBEW              TYPE  STYPE_MBEW,            "n450764
                                                            "n450764
         L_S_MBEW_SPLIT        TYPE  STYPE_MBEW,            "n450764
         L_S_MBEW_NORMAL       TYPE  STYPE_MBEW,            "n450764
         L_FLAG_SPLIT(01)      TYPE C.                      "n450764
                                                            "n450764
* build Valuation Class restriction table
  IF GV_SWITCH_EHP6RU = ABAP_TRUE.
    PERFORM BUILD_BKLAS_SELECTION USING l_t_mbew.
  ENDIF.

* read the matching valuation entries                       "n450764
  SELECT MATNR BWKEY BWTAR LBKUM SALK3 BWTTY  FROM MBEW     "n1227439
         INTO CORRESPONDING FIELDS OF TABLE L_T_MBEW        "n450764
         WHERE  MATNR  IN  MATNR                            "n450764
           AND  BWKEY  IN  G_RA_BWKEY                       "n450764
           AND  BWTAR  IN  BWTAR                            "n450764
           AND  BKLAS  IN  IBKLAS.
                                                            "n450764
                                                            "n450764
* read the matching valuation records of the valuated       "n450764
* special stock sales order                                 "n450764
  SELECT MATNR BWKEY BWTAR BWTTY                            "n1227439
         SUM( LBKUM ) AS LBKUM                              "n450764
         SUM( SALK3 ) AS SALK3        FROM  EBEW            "n450764
         APPENDING CORRESPONDING FIELDS OF TABLE L_T_MBEW   "n450764
         WHERE  MATNR  IN  MATNR                            "n450764
           AND  BWKEY  IN  G_RA_BWKEY                       "n450764
           AND  BWTAR  IN  BWTAR                            "n450764
           AND  BKLAS  IN  IBKLAS
           AND  SOBKZ <> 'T'                                "SIT
         GROUP BY  MATNR  BWKEY BWTAR BWTTY.                "n450764
                                                            "n450764
* read the matching valuation records of the valuated       "n450764
* special stock projects                                    "n450764
  SELECT MATNR BWKEY BWTAR BWTTY                            "n1227439
         SUM( LBKUM ) AS LBKUM                              "n450764
         SUM( SALK3 ) AS SALK3        FROM  QBEW            "n450764
         APPENDING CORRESPONDING FIELDS OF TABLE L_T_MBEW   "n450764
         WHERE  MATNR  IN  MATNR                            "n450764
           AND  BWKEY  IN  G_RA_BWKEY                       "n450764
           AND  BWTAR  IN  BWTAR                            "n450764
           AND  BKLAS  IN  IBKLAS
         GROUP BY  MATNR  BWKEY BWTAR BWTTY.                "n450764

* read the matching valuation records of the valuated       "n497992
* special subcontractor stock OBEW                          "n497992
  SELECT MATNR BWKEY BWTAR BWTTY                            "n1227439
         SUM( LBKUM ) AS LBKUM                              "n497992
         SUM( SALK3 ) AS SALK3         FROM  OBEW           "n497992
         APPENDING CORRESPONDING FIELDS OF TABLE L_T_MBEW   "n497992
         WHERE  MATNR  IN  MATNR                            "n497992
           AND  BWKEY  IN  G_RA_BWKEY                       "n497992
           AND  BWTAR  IN  BWTAR                            "n497992
           AND  BKLAS  IN  IBKLAS
         GROUP BY  MATNR  BWKEY BWTAR BWTTY.                "n497992

  IF L_T_MBEW[] IS INITIAL.                                 "n1560727
    MESSAGE S289.                                           "n1560727
*   Kein Material in Selektion vorhanden.                      "n1560727
    PERFORM ANFORDERUNGSBILD.                               "n1560727
  ENDIF.                                                    "n1560727

* create table g_t_organ if it is still empty
  IF  G_T_ORGAN[] IS INITIAL.                               "n433765
*   create working table G_0000_RA_BWKEY with the valuation areas
    LOOP AT L_T_MBEW         INTO  L_S_MBEW.                "n450764
      ON CHANGE OF L_S_MBEW-BWKEY.  "#EC CI_SORTED
      "Added by SPLABAP during code remediation
                              "n450764
        MOVE : L_S_MBEW-BWKEY                               "n450764
                             TO  G_0000_RA_BWKEY-LOW,       "n450764
               'I'           TO  G_0000_RA_BWKEY-SIGN,      "n450764
               'EQ'          TO  G_0000_RA_BWKEY-OPTION.    "n450764
        COLLECT              G_0000_RA_BWKEY.               "n450764
      ENDON.                                                "n450764
    ENDLOOP.

    PERFORM  F0000_CREATE_TABLE_G_T_ORGAN
                             USING  C_NO_ERROR.
  ENDIF.

  SORT  L_T_MBEW             BY  MATNR  BWKEY.              "n450764
                                                            "n450764
  LOOP AT L_T_MBEW           INTO  L_S_MBEW.                "n450764
*   check if MBEW record is a mother segment (splitval)     "n1227439
    IF  L_S_MBEW-BWTAR IS INITIAL                           "n1227439
        AND NOT L_S_MBEW-BWTTY IS INITIAL.                  "n1227439
      CLEAR L_S_MBEW-LBKUM.                                 "n1227439
      CLEAR L_S_MBEW-SALK3.                                 "n1227439
    ENDIF.                                                  "n1227439
*   process a single entry / add the stock and value        "n450764
    IF  L_S_MBEW-BWTAR IS INITIAL.                          "n450764
      MOVE : L_S_MBEW-MATNR  TO  L_S_MBEW_NORMAL-MATNR,     "n450764
             L_S_MBEW-BWKEY  TO  L_S_MBEW_NORMAL-BWKEY.     "n450764
      ADD :  L_S_MBEW-LBKUM  TO  L_S_MBEW_NORMAL-LBKUM,     "n450764
             L_S_MBEW-SALK3  TO  L_S_MBEW_NORMAL-SALK3.     "n450764
    ELSE.                                                   "n450764
*     material has split valuation                          "n450764
      MOVE : 'X'             TO  L_FLAG_SPLIT,              "n450764
             L_S_MBEW-MATNR  TO  L_S_MBEW_SPLIT-MATNR,      "n450764
             L_S_MBEW-BWKEY  TO  L_S_MBEW_SPLIT-BWKEY.      "n450764
      ADD :  L_S_MBEW-LBKUM  TO  L_S_MBEW_SPLIT-LBKUM,      "n450764
             L_S_MBEW-SALK3  TO  L_S_MBEW_SPLIT-SALK3.      "n450764
    ENDIF.                                                  "n450764
                                                            "n450764
*   control break after material and valuation area         "n450764
    AT END OF BWKEY.                                        "n450764
*     create a entry for the next working table             "n450764
      IF  L_FLAG_SPLIT = 'X'.                               "n450764
*       if the material has split valuation, take only      "n450764
*       the sums from the entries with valuation type       "n450764
        MOVE-CORRESPONDING  L_S_MBEW_SPLIT  TO  G_S_MBEW.   "n450764
      ELSE.                                                 "n450764
        MOVE-CORRESPONDING  L_S_MBEW_NORMAL TO  G_S_MBEW.   "n450764
      ENDIF.                                                "n450764
                                                            "n450764
*     check the authority                                   "n450764
      PERFORM  F9300_READ_ORGAN                             "n450764
                   USING     C_BWKEY   G_S_MBEW-BWKEY.      "n450764
                                                            "n450764
      IF SY-SUBRC IS INITIAL.                               "n450764
*       enrich the entries with the field currency key      "n450764
        MOVE G_S_ORGAN-WAERS TO  G_S_MBEW-WAERS.            "n450764
        APPEND  G_S_MBEW     TO  G_T_MBEW.                  "n450764
                                                            "n450764
*       create the key table for the material texts         "n450764
        PERFORM  F9400_MATERIAL_KEY                         "n450764
                             USING  G_S_MBEW-MATNR.         "n450764
      ENDIF.                                                "n450764
                                                            "n450764
*     clear the working areas for the next group            "n450764
      CLEAR : L_FLAG_SPLIT, L_S_MBEW_NORMAL, L_S_MBEW_SPLIT. "n450764
    ENDAT.                                                  "n450764
  ENDLOOP.                                                  "n450764

* no entries left in table g_t_mbew ?
  IF  G_T_MBEW[] IS INITIAL.                                "n450764
    MESSAGE S289.
*     Kein Material in Selektion vorhanden.
    PERFORM ANFORDERUNGSBILD.
  ENDIF.

ENDFORM.                     "aktuelle_bst_bwbst

*&---------------------------------------------------------------------*
*&      Form  BEWEGUNGSARTEN_LESEN
*&---------------------------------------------------------------------*
*       Lesen der Tabellen zur Bewegungsart                            *
*----------------------------------------------------------------------*

FORM  BEWEGUNGSARTEN_LESEN.

  DATA: BEGIN OF K2 OCCURS 0,
    BWART LIKE T156S-BWART,
  END OF K2.
  REFRESH K2.

* select the movement types from the selected documents
  LOOP AT G_T_MSEG_LEAN      INTO  G_S_MSEG_LEAN.
    MOVE  G_S_MSEG_LEAN-BWART          TO  K2-BWART.
    COLLECT                            K2.
  ENDLOOP.

* Read data for movement type from new tables
* T156SY/C/Q with function module from release >=46B
  DATA: T_ST156S         LIKE ST156S OCCURS 0
                         WITH HEADER LINE.

  REFRESH IT156.
  LOOP AT K2.
    REFRESH T_ST156S.

    CALL FUNCTION 'MB_CONTROL_MOVETYPE_GET'
      EXPORTING
        I_BWART              = K2-BWART
      TABLES
        TE_ST156S_TAB        = T_ST156S
      EXCEPTIONS
        INCONSISTENT_ENTRIES = 1
        NO_ENTRIES_FOUND     = 2
        OTHERS               = 3.

    IF SY-SUBRC = 0.                                        "311588
      LOOP AT T_ST156S.
        MOVE-CORRESPONDING T_ST156S TO IT156.
        APPEND IT156.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT IT156 BY BWART WERTU MENGU SOBKZ KZBEW KZZUG KZVBR.

  SELECT * FROM T156M INTO CORRESPONDING FIELDS OF TABLE IT156X
           FOR ALL ENTRIES IN IT156  WHERE BUSTM EQ IT156-BUSTM.

  LOOP AT IT156.
    CLEAR IT156-LBBSA.
    READ TABLE IT156X WITH KEY BUSTM = IT156-BUSTM.
    IT156-LBBSA = IT156X-LBBSA.
    MODIFY IT156.
  ENDLOOP.

  DATA: RC TYPE I.                                          "147374
  LOOP AT G_T_MSEG_LEAN      INTO  G_S_MSEG_LEAN.
*   find and delete reversal movements / only in releases >= 45B
    IF NOT NOSTO IS INITIAL AND
       NOT ( G_S_MSEG_LEAN-SMBLN IS INITIAL OR
             G_S_MSEG_LEAN-SMBLP IS INITIAL ).
      MOVE-CORRESPONDING  G_S_MSEG_LEAN
                             TO  STORNO.

      APPEND STORNO.
      DELETE                 G_T_MSEG_LEAN.
      CONTINUE.
    ENDIF.

    READ TABLE IT156 WITH KEY BWART = G_S_MSEG_LEAN-BWART
                              WERTU = G_S_MSEG_LEAN-WERTU
                              MENGU = G_S_MSEG_LEAN-MENGU
                              SOBKZ = G_S_MSEG_LEAN-SOBKZ
                              KZBEW = G_S_MSEG_LEAN-KZBEW
                              KZZUG = G_S_MSEG_LEAN-KZZUG
                              KZVBR = G_S_MSEG_LEAN-KZVBR
                             BINARY SEARCH.

    RC = SY-SUBRC.                                          "147374
    IF  G_S_MSEG_LEAN-BUSTM = SPACE AND
        G_S_MSEG_LEAN-BUSTW = SPACE AND
        RC                  = 0.                            "147374
      MOVE : IT156-BUSTM     TO  G_S_MSEG_LEAN-BUSTM,       "147374
             IT156-BUSTW     TO  G_S_MSEG_LEAN-BUSTW.       "147374
    ENDIF.

    IF RC = 0.                                              "147374
      MOVE : IT156-LBBSA     TO  G_S_MSEG_LEAN-LBBSA.

      IF NOT IT156-BWAGR IS INITIAL.
        MOVE : IT156-BWAGR   TO  G_S_MSEG_LEAN-BWAGR.
      ELSE.
        MOVE : 'REST'        TO  G_S_MSEG_LEAN-BWAGR.
      ENDIF.
    ELSE.
      MOVE : 'REST'          TO  G_S_MSEG_LEAN-BWAGR.
    ENDIF.                                                  "147374

    MODIFY  G_T_MSEG_LEAN    FROM  G_S_MSEG_LEAN.
  ENDLOOP.

ENDFORM.                    " BEWEGUNGSARTEN_LESEN

*&---------------------------------------------------------------------*
*&      Form  SUMMEN_BILDEN
*&---------------------------------------------------------------------*
*       Bestandssummen zur Berechnung der Bestände                     *
*       zu 'datum-low' und 'datum-high'                                *
*----------------------------------------------------------------------*
FORM SUMMEN_BILDEN.
* Some explanatory words on the strategy of material
* counting/valuation:
* ======================================================
* 1) Stock overview (no valuation):
*    The material document is accepted, if is has not been created
*    automatically or if it is not related to movements out of
*    the stock. For example, if a stock transfer is posted, the
*    system creates a material document with two lines: Out of
*    the old stock (accepted) and into the transfer stock (rejected,
*    because the material is not yet visible in the target location).
*    When the movement into the stock is posted, this is accepted.
* 2) Valuated stock:
*    a) Movements within a single plant (MA05, MA06 =
*       movement types 313-316) are ignored.
*    b) The moving of material out of a plant (303/304)
*       is counted and valuated in the emitting plant and
*       the target plant. The moving in
*       (305/306) is ignored, because
*       the valuated stock appears in the target at the
*       very moment of leaving the emitter.
*    c) Material documents without valuation string are ignored.
*------------- Summen von 'datum-high' bis Gegenwart ------------------*
  IF NOT INDEX_2 IS INITIAL.
    IF BWBST = ' '.
      IF XCHAR = ' '.
        SORT IMSWEG BY WERKS MATNR SHKZG.          "auf Materialebene
        LOOP AT IMSWEG.
          IF ( IMSWEG-XAUTO IS INITIAL ) OR
             ( IMSWEG-BUSTM <> 'MA02' AND IMSWEG-BUSTM <> 'MA05' ).
            MOVE-CORRESPONDING IMSWEG TO WEG_MAT.
            COLLECT WEG_MAT.
          ELSE.
            DELETE IMSWEG.
          ENDIF.
        ENDLOOP.
      ELSEIF XCHAR = 'X'.
        SORT IMSWEG BY WERKS MATNR CHARG SHKZG.    "auf Chargenebene
        LOOP AT IMSWEG.
          IF ( IMSWEG-XAUTO IS INITIAL ) OR
             ( IMSWEG-BUSTM <> 'MA02' AND IMSWEG-BUSTM <> 'MA05' ).
            MOVE-CORRESPONDING IMSWEG TO WEG_CHAR.
            COLLECT WEG_CHAR.
          ELSE.
            DELETE IMSWEG.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ELSEIF BWBST = 'X'.
      SORT IMSWEG BY WERKS MATNR SHKZG.
      LOOP AT IMSWEG.
*       consider special gain/loss-handling of IS-OIL       "n497992

**# IF EXIST OI001
**" IF ( imsweg-bustm <> 'MEU1' )    OR                 "n497992
**"   ( imsweg-bustm = 'MEU1'                           "n497992
**"   AND not imsweg-OIGLCALC IS INITIAL                "n497992
**"   AND not imsweg-OIGLSKU IS INITIAL ).              "n497992
**"          MOVE-CORRESPONDING imsweg TO mat_weg.      "n497992
**"          COLLECT mat_weg.                           "n497992
**" ELSE.                                               "n497992
**"          DELETE             imsweg.                 "n497992
**" ENDIF .                                             "n497992
**# ELSE
*     MOVE-CORRESPONDING imsweg TO mat_weg.             "n497992
*     COLLECT mat_weg.                                  "n497992
**# ENDIF
*       IS-OIL specific functions without ABAP preprocessor "n599218 A
        IF  G_FLAG_IS_OIL_ACTIVE = 'X'.     "IS-OIL ?       "n599218 A
          IF ( IMSWEG-BUSTM <> 'MEU1' )    OR               "n599218 A
             ( IMSWEG-BUSTM = 'MEU1'                        "n599218 A
               AND NOT IMSWEG-OIGLCALC IS INITIAL           "n599218 A
               AND NOT IMSWEG-OIGLSKU IS INITIAL ).         "n599218 A
            MOVE-CORRESPONDING IMSWEG TO MAT_WEG.           "n599218 A
            COLLECT MAT_WEG.                                "n599218 A
          ELSE.                                             "n599218 A
            DELETE           IMSWEG.                        "n599218 A
          ENDIF.                                            "n599218 A
        ELSE.                                               "n599218 A
          MOVE-CORRESPONDING IMSWEG TO MAT_WEG.             "n599218 A
          COLLECT MAT_WEG.                                  "n599218 A
        ENDIF.                                              "n599218 A

      ENDLOOP.

      LOOP AT MAT_WEG.
        IF CURM = '1'.
          MAT_WEG-BWKEY = MAT_WEG-WERKS.
        ELSEIF CURM = '3'.
*
*         look for the corresponding valuation area
*         READ TABLE organ WITH KEY werks = mat_weg-werks.
*         mat_weg-bwkey = organ-bwkey.
          PERFORM  F9300_READ_ORGAN
                   USING     C_WERKS   MAT_WEG-WERKS.

          MOVE : G_S_ORGAN-BWKEY   TO  MAT_WEG-BWKEY.
        ENDIF.
        MODIFY MAT_WEG.
      ENDLOOP.
      IF CURM = '3'.
        SORT MAT_WEG BY BWKEY MATNR SHKZG.
        LOOP AT MAT_WEG.
          MOVE-CORRESPONDING MAT_WEG TO MAT_WEG_BUK.
          COLLECT MAT_WEG_BUK.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

*------------- Summen von 'datum-low' bis 'datum-high' ----------------*
  IF BWBST = ' '.
    IF XCHAR = ' '.                    "auf Materialebene

      SORT  G_T_MSEG_LEAN    BY WERKS MATNR SHKZG DESCENDING.

      LOOP AT G_T_MSEG_LEAN  INTO  G_S_MSEG_LEAN.
        IF ( G_S_MSEG_LEAN-XAUTO IS INITIAL ) OR
           ( G_S_MSEG_LEAN-BUSTM <> 'MA02' AND
             G_S_MSEG_LEAN-BUSTM <> 'MA05' ).
          MOVE-CORRESPONDING G_S_MSEG_LEAN   TO  SUM_MAT.
          COLLECT            SUM_MAT.
        ELSE.
          DELETE             G_T_MSEG_LEAN.
        ENDIF.
      ENDLOOP.

    ELSEIF XCHAR = 'X'.                "auf Chargenebene
      SORT  G_T_MSEG_LEAN    BY WERKS MATNR CHARG SHKZG DESCENDING.

      LOOP AT G_T_MSEG_LEAN  INTO  G_S_MSEG_LEAN.
        IF ( G_S_MSEG_LEAN-XAUTO IS INITIAL ) OR
           ( G_S_MSEG_LEAN-BUSTM <> 'MA02' AND
             G_S_MSEG_LEAN-BUSTM <> 'MA05' ).
          MOVE-CORRESPONDING  G_S_MSEG_LEAN
                             TO  SUM_CHAR.
          COLLECT            SUM_CHAR.
        ELSE.
          DELETE             G_T_MSEG_LEAN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ELSEIF BWBST = 'X'.
    SORT  G_T_MSEG_LEAN      BY WERKS MATNR SHKZG DESCENDING.
    LOOP AT G_T_MSEG_LEAN    INTO  G_S_MSEG_LEAN.
*       consider special gain/loss-handling of IS-OIL       "n497992

**# IF EXIST OI001
**"    IF ( G_S_MSEG_LEAN-bustm <> 'MEU1' )    OR       "n497992
**"       ( G_S_MSEG_LEAN-bustm = 'MEU1'                "n497992
**"       AND not G_S_MSEG_LEAN-OIGLCALC IS INITIAL     "n497992
**"       AND not G_S_MSEG_LEAN-OIGLSKU IS INITIAL ).   "n497992
**# ENDIF
*      MOVE-CORRESPONDING  G_S_MSEG_LEAN
*                             TO  MAT_SUM.
*      COLLECT                MAT_SUM.
**# IF EXIST OI001
**"    ELSE.                                            "n497992
**"      DELETE               G_T_MSEG_LEAN.            "n497992
**"    ENDIF.                                           "n497992
**# ENDIF
*     IS-OIL specific functions without ABAP preprocessor   "n599218 A
      IF  G_FLAG_IS_OIL_ACTIVE = 'X'.       "IS-OIL ?       "n599218 A
        IF ( G_S_MSEG_LEAN-BUSTM <> 'MEU1' )    OR          "n599218 A
           ( G_S_MSEG_LEAN-BUSTM = 'MEU1'                   "n599218 A
           AND NOT G_S_MSEG_LEAN-OIGLCALC IS INITIAL        "n599218 A
           AND NOT G_S_MSEG_LEAN-OIGLSKU IS INITIAL ).      "n599218 A
          MOVE-CORRESPONDING  G_S_MSEG_LEAN                 "n599218 A
                             TO  MAT_SUM.                   "n599218 A
          COLLECT            MAT_SUM.                       "n599218 A
        ELSE.                                               "n599218 A
          DELETE             G_T_MSEG_LEAN.                 "n599218 A
        ENDIF.                                              "n599218 A
      ELSE.                                                 "n599218 A
        MOVE-CORRESPONDING  G_S_MSEG_LEAN                   "n599218 A
                             TO  MAT_SUM.                   "n599218 A
        COLLECT              MAT_SUM.                       "n599218 A
      ENDIF.                                                "n599218 A
    ENDLOOP.

    LOOP AT MAT_SUM.
      IF CURM = '1'.
        MAT_SUM-BWKEY = MAT_SUM-WERKS.
      ELSEIF CURM = '3'.
        PERFORM  F9300_READ_ORGAN
                   USING     C_WERKS   MAT_SUM-WERKS.

        MOVE : G_S_ORGAN-BWKEY     TO  MAT_SUM-BWKEY.
      ENDIF.
      MODIFY MAT_SUM.
    ENDLOOP.

    IF CURM = '3'.            "Materialbelege auf Buchungskreisebene
      SORT MAT_SUM BY BWKEY MATNR SHKZG DESCENDING.
      LOOP AT MAT_SUM.
        MOVE-CORRESPONDING MAT_SUM TO MAT_SUM_BUK.
        COLLECT MAT_SUM_BUK.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                               " SUMMEN_BILDEN

*&---------------------------------------------------------------------*
*&      Form  BELEGSELEKTION
*&---------------------------------------------------------------------*

FORM BELEGSELEKTION.

* does the user wants the valuated stocks ?
  IF BWBST = 'X'.
*   delete the MM-documents without values
    PERFORM UNBEWERTET_WEG.

    IF G_T_MBEW[] IS INITIAL.                               "n450764
      MESSAGE S289.
*     Kein Material in Selektion vorhanden.
      PERFORM ANFORDERUNGSBILD.
    ENDIF.

*   select the corresponding FI-documents
    PERFORM                  FI_BELEGE_LESEN.
  ENDIF.

  IF SBBST IS INITIAL.
    PERFORM                  KONTIERT_AUSSORTIEREN.
  ENDIF.

  PERFORM                    BEWEGUNGSARTEN_LESEN.

* does the user want no reversal movements ? only in releases >=45B
  IF NOT NOSTO IS INITIAL.
    PERFORM                  STORNO.
  ENDIF.

* does the user wants the valuated stocks ?
  IF BWBST = 'X'.
    PERFORM                  BELEGE_ERGAENZEN.
  ENDIF.

  PERFORM                    BELEGE_SORTIEREN.

ENDFORM.                     "BELEGSELEKTION

*&--------------------------------------------------------------------*
*&   FELDGRUPPEN_AUFBAUEN
*&--------------------------------------------------------------------*
*& create table GRUPPEN with the parameter for special groups         *
*&--------------------------------------------------------------------*

FORM FELDGRUPPEN_AUFBAUEN.

* Gruppendefinitionen Positionsfelder
  GRUPPEN-SP_GROUP = 'M'.
  GRUPPEN-TEXT = TEXT-050.
  APPEND GRUPPEN.
  GRUPPEN-SP_GROUP = 'B'.
  GRUPPEN-TEXT = TEXT-051.
  APPEND GRUPPEN.
  GRUPPEN-SP_GROUP = 'D'.
  GRUPPEN-TEXT = TEXT-052.
  APPEND GRUPPEN.
  GRUPPEN-SP_GROUP = 'O'.
  GRUPPEN-TEXT = TEXT-053.
  APPEND GRUPPEN.
  GRUPPEN-SP_GROUP = 'K'.
  GRUPPEN-TEXT = TEXT-054.
  APPEND GRUPPEN.
  GRUPPEN-SP_GROUP = 'E'.
  GRUPPEN-TEXT = TEXT-055.
  APPEND GRUPPEN.
  GRUPPEN-SP_GROUP = 'V'.
  GRUPPEN-TEXT = TEXT-056.
  APPEND GRUPPEN.
  GRUPPEN-SP_GROUP = 'F'.
  GRUPPEN-TEXT = TEXT-057.
  APPEND GRUPPEN.
  GRUPPEN-SP_GROUP = 'S'.
  GRUPPEN-TEXT = TEXT-058.
  APPEND GRUPPEN.
  LAYOUT-GROUP_BUTTONS = ' '.

ENDFORM.                               " FELDGRUPPEN_AUFBAUEN.

*&---------------------------------------------------------------------*
*&      Form  UEBERSCHRIFT
*&---------------------------------------------------------------------*

FORM UEBERSCHRIFT.                                          "#EC CALLED

  MOVE-CORRESPONDING  BESTAND
                             TO  G_S_BESTAND.
  PERFORM                    TOP_OF_PAGE_RENDER.

ENDFORM.                               " UEBERSCHRIFT

*&---------------------------------------------------------------------*
*&      Form  UEBERSCHRIFT1
*&---------------------------------------------------------------------*

FORM UEBERSCHRIFT1.                                         "#EC CALLED

  MOVE-CORRESPONDING  BESTAND1
                             TO  G_S_BESTAND.
  PERFORM                    TOP_OF_PAGE_RENDER.

ENDFORM.                               " UEBERSCHRIFT1

*&---------------------------------------------------------------------*
*&      Form  UEBERSCHRIFT_DETAIL
*&---------------------------------------------------------------------*

FORM UEBERSCHRIFT_DETAIL.                                   "#EC CALLED

  MOVE-CORRESPONDING  G_S_BESTAND_DETAIL
                             TO  G_S_BESTAND.

  PERFORM                    TOP_OF_PAGE_RENDER.

ENDFORM.                               " UEBERSCHRIFT_DETAIL

*&---------------------------------------------------------------------*
*&      Form  STORNO
*&---------------------------------------------------------------------*
*       Stornobewegungen vernachlässigen
*----------------------------------------------------------------------*

* delete the reversal movements from the working
* table with the documents / only in releases >=45B
FORM STORNO.

  LOOP AT STORNO.
    DELETE G_T_MSEG_LEAN
             WHERE MBLNR = STORNO-SMBLN                     "204463
               AND MJAHR = STORNO-SJAHR                     "204463
               AND ZEILE = STORNO-SMBLP.                    "204463
  ENDLOOP.

ENDFORM.                   " STORNO

*----------------------------------------------------------------------*
* F0400_CREATE_FIELDCAT
*----------------------------------------------------------------------*
*
* create field catalog for the ALV
* take only the field of structure MSEG_LEAN who are in working
* table g_f_mseg_fields

* --> input    name of ALV input data table
* <-- output   table wilh the field catalog
*
*----------------------------------------------------------------------*

FORM F0400_CREATE_FIELDCAT.

  CLEAR                      G_S_FIELDCAT.

* lagerort                   storage location
* the following special stocks O, V, W need no storage location
  IF  SOBKZ = 'O'  OR
      SOBKZ = 'V'  OR
      SOBKZ = 'W'.
  ELSE.
    G_S_FIELDCAT-FIELDNAME     = 'LGORT'.
    G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
    G_S_FIELDCAT-SP_GROUP      = 'O'.
    PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.
  ENDIF.

* Bewegungsart               movement type
  G_S_FIELDCAT-FIELDNAME     = 'BWART'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.

* Sonderbestandskennzeichen  Special stock indicator
  G_S_FIELDCAT-FIELDNAME     = 'SOBKZ'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.

* Nummer des Materialbelegs  Number of material document
  G_S_FIELDCAT-FIELDNAME     = 'MBLNR'.
  G_S_FIELDCAT-REF_TABNAME   = 'MKPF'.
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.

* Position im Materialbeleg  Item in material document
  G_S_FIELDCAT-FIELDNAME     = 'ZEILE'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.

  IF BWBST = 'X'.
*   Nummer Buchhaltungsbeleg   Accounting document number
    G_S_FIELDCAT-FIELDNAME     = 'BELNR'.
    G_S_FIELDCAT-REF_TABNAME   = 'BSIM'.
    G_S_FIELDCAT-SP_GROUP      = 'O'.
    PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.
  ENDIF.

* Buchungsdatum im Beleg     Posting date in the document
  G_S_FIELDCAT-FIELDNAME     = 'BUDAT'.
  G_S_FIELDCAT-REF_TABNAME   = 'MKPF'.
  G_S_FIELDCAT-SP_GROUP      = 'D'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'MENGE'.     " Menge
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Quantity
  G_S_FIELDCAT-QFIELDNAME    = 'MEINS'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'MEINS'.     " Basismengeneinheit
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Base unit of measure
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.

  IF NOT BWBST IS INITIAL.   "mit bewertetem Bestand
*   Betrag in Hauswaehrung   Amount in local currency
    G_S_FIELDCAT-FIELDNAME     = 'DMBTR'.
    G_S_FIELDCAT-REF_TABNAME   = 'BSIM'.
    G_S_FIELDCAT-CFIELDNAME    = 'WAERS'.
    G_S_FIELDCAT-SP_GROUP      = 'M'.
    PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.
  ENDIF.                                          "note 201670

* g_s_fieldcat-fieldname     = 'WAERS'.     " Waehrungs-schluessel
* g_s_fieldcat-ref_tabname   = 'T001'.      " Currency Key
* g_s_fieldcat-sp_group      = 'M'.
* perform  f0410_fieldcat    using  c_take   c_out.

* the following fields are always in g_s_mseg_lean, but they are
* hidden in the list
  G_S_FIELDCAT-FIELDNAME     = 'MJAHR'.     " Materialbelegjahr
  G_S_FIELDCAT-REF_TABNAME   = 'MKPF'.      " Material doc. year
  G_S_FIELDCAT-SP_GROUP      = 'D'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'GJAHR'.     " Geschäftsjahr
  G_S_FIELDCAT-REF_TABNAME   = 'BKPF'.      " Fiscal Year
  G_S_FIELDCAT-SP_GROUP      = 'D'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'VGART'.    " Vorgangsart
  G_S_FIELDCAT-REF_TABNAME   = 'MKPF'.     " Transaction/event type
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'USNAM'.    " Name des Benutzers
  G_S_FIELDCAT-REF_TABNAME   = 'MKPF'.     " User name
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'CPUDT'.    " Tag der Erfassung
  G_S_FIELDCAT-REF_TABNAME   = 'MKPF'.     " Acc. doc. entry date
  G_S_FIELDCAT-SP_GROUP      = 'D'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'CPUTM'.     " Uhrzeit der Erfassung
  G_S_FIELDCAT-REF_TABNAME   = 'MKPF'.      " Time of entry
  G_S_FIELDCAT-SP_GROUP      = 'D'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'SHKZG'.    " Soll-/Haben-Kennzeichen
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.     " Debit/credit indicator
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'BWTAR'.     " Bewertungsart
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Valuation type
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

* Kennzeichen Bewertung Sonderbestand
* Indicator: valuation of special stock
  G_S_FIELDCAT-FIELDNAME     = 'KZBWS'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'CHARG'.     " Chargennummer
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Batch number
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'BUKRS'.     " Buchungskreis
  G_S_FIELDCAT-REF_TABNAME   = 'T001'.      " Company code
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  IF GV_SWITCH_EHP6RU = ABAP_TRUE AND BWBST = 'X'.
*   G/L account
    G_S_FIELDCAT-FIELDNAME     = 'HKONT'.
    G_S_FIELDCAT-REF_TABNAME   = 'BSEG'.
    G_S_FIELDCAT-SP_GROUP      = 'O'.
    PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.
  ENDIF.

  G_S_FIELDCAT-FIELDNAME     = 'KZBEW'.     " Bewegungskennzeichen
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Movement indicator
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'KZVBR'.     " Kennz. Verbrauchsbuchung
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Consumption posting
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'KZZUG'.     " Zugangskennzeichen
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Receipt indicator
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'BUSTM'. " Buchungsstring für Mengen
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.  " Posting string for quantities
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'BUSTW'.    " Buchungsstring für Werte
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.     " Posting string for values
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

* Kennzeichen: Mengenfortschreibung im Materialstammsatz
* Quantity Updating in Material Master Record
  G_S_FIELDCAT-FIELDNAME     = 'MENGU'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

* Kennzeichen: Wertfortschreibung im Materialstammsatz
* Value Updating in Material Master Record
  G_S_FIELDCAT-FIELDNAME     = 'WERTU'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

* Bewegungsartengruppe zur Bestandsauswertung
* Movement type group for stock analysis
  G_S_FIELDCAT-FIELDNAME     = 'BWAGR'.

* the reference table changed in release 46B
  G_S_FIELDCAT-REF_TABNAME   = 'T156Q'.

  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.

* process 'goods receipt/issue slip' as hidden field        "n450596
  G_S_FIELDCAT-FIELDNAME     = 'XABLN'.                     "n450596
  G_S_FIELDCAT-REF_TABNAME   = 'MKPF'.                      "n450596
  G_S_FIELDCAT-SP_GROUP      = 'S'.                         "n450596
  PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.      "n450596

* the following fields will be processed if they are in working table
* g_t_mseg_fields         Customer Exit :
* these fields can be activated in include RM07MLBD_CUST_FIELDS

  G_S_FIELDCAT-FIELDNAME     = 'INSMK'.    " Bestandsart
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.     " stock type
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'LIFNR'.    " Kontonummer Lieferant
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.     " vendor's account number
  G_S_FIELDCAT-SP_GROUP      = 'E'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'KUNNR'.    " Kontonummer des Kunden
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.   " account number of customer
  G_S_FIELDCAT-SP_GROUP      = 'V'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* process the sales order number and sales order item  "n599218
* depending on the release                             "n599218
* release          field                               "n599218
* 4.5B and higher  MAT_KDAUF, MAT_KDPOS                "n599218
* 4.0B             KDAUF,     KDPOS                    "n599218
*                                                      "n599218
  G_S_FIELDCAT-FIELDNAME   = 'MAT_KDAUF'.                   "n599218
  G_S_FIELDCAT-REF_TABNAME = 'MSEG'.                        "n599218
  G_S_FIELDCAT-SP_GROUP    = 'V'.                           "n599218
  PERFORM  F0410_FIELDCAT  USING  C_CHECK  C_NO_OUT.        "n599218
                                                            "n599218
  G_S_FIELDCAT-FIELDNAME   = 'MAT_KDPOS'.                   "n599218
  G_S_FIELDCAT-REF_TABNAME = 'MSEG'.                        "n599218
  G_S_FIELDCAT-SP_GROUP    = 'V'.                           "n599218
  PERFORM  F0410_FIELDCAT  USING  C_CHECK  C_NO_OUT.        "n599218

  G_S_FIELDCAT-FIELDNAME     = 'KDAUF'.     " Kundenauftragsnummer
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Sales Order Number
  G_S_FIELDCAT-SP_GROUP      = 'V'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'KDPOS'.    " Positionsnummer
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.     " Item number in Sales Order
  G_S_FIELDCAT-SP_GROUP      = 'V'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Einteilung Kundenauftrag   Delivery schedule for sales order
  G_S_FIELDCAT-FIELDNAME     = 'KDEIN'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'F'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Menge in Erfassungsmengeneinheit   Quantity in unit of entry
  G_S_FIELDCAT-FIELDNAME     = 'ERFMG'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-QFIELDNAME    = 'ERFME'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'ERFME'.     " Erfassungsmengeneinheit
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Unit of entry
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Menge in Bestellpreismengeneinheit
* Quantity in purchase order price unit
  G_S_FIELDCAT-FIELDNAME     = 'BPMNG'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-QFIELDNAME    = 'BPRME'.
  G_S_FIELDCAT-SP_GROUP      = 'E'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'BPRME'.     " Bestellpreismengeneinheit
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Order price unit
  G_S_FIELDCAT-SP_GROUP      = 'E'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'EBELN'.     " Bestellnummer
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Purchase order number
  G_S_FIELDCAT-SP_GROUP      = 'E'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Positionsnummer des Einkaufsbelegs
* Item Number of Purchasing Document
  G_S_FIELDCAT-FIELDNAME     = 'EBELP'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'E'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'SJAHR'.     " Materialbelegjahr
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Material doc. year
  G_S_FIELDCAT-SP_GROUP      = 'D'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'SMBLN'.     " Nummer des Materialbelegs
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Number of material doc.
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'SMBLP'.     " Position im Materialbeleg
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Item in material document
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'ELIKZ'.  " Endlieferungskennzeichen
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.   "Delivery completed" indicator
  G_S_FIELDCAT-SP_GROUP      = 'E'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'SGTXT'.     " Positionstext
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Item Text
  G_S_FIELDCAT-SP_GROUP      = 'E'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'WEMPF'.     " Warenempfänger
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Goods recipient
  G_S_FIELDCAT-SP_GROUP      = 'V'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'ABLAD'.     " Abladestelle
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Unloading point
  G_S_FIELDCAT-SP_GROUP      = 'V'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'GSBER'.     " Geschäftsbereich
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Business Area
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Geschäftsbereich des Geschäftspartners
* Trading partner's business area
  G_S_FIELDCAT-FIELDNAME     = 'PARGB'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'PARBU'.   " Verrechnender Buchungskreis
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.    " Clearing company code
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'KOSTL'.     " Kostenstelle
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Cost Center
  G_S_FIELDCAT-SP_GROUP      = 'K'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'AUFNR'.     " Auftragsnummer
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Order Number
  G_S_FIELDCAT-SP_GROUP      = 'K'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'ANLN1'.     " Anlagen-Hauptnummer
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Main asset number
  G_S_FIELDCAT-SP_GROUP      = 'K'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Nummer der Reservierung / des Sekundärbedarfs
* Number of reservation/dependent requirements
  G_S_FIELDCAT-FIELDNAME     = 'RSNUM'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Positionsnummer der Reservierung / des Sekundärbedarfs
* Item number of reservation/dependent requirements
  G_S_FIELDCAT-FIELDNAME     = 'RSPOS'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Final issue for this reservation
  G_S_FIELDCAT-FIELDNAME     = 'KZEAR'.    " Kennzeichen: Endausfassung
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* empfangendes/abgebendes Material
* Receiving/issuing material
  G_S_FIELDCAT-FIELDNAME     = 'UMMAT'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Empfangendes/Abgebendes Werk
* Receiving plant/issuing plant
  G_S_FIELDCAT-FIELDNAME     = 'UMWRK'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Empfangender/Abgebender Lagerort
* Receiving/issuing storage location
  G_S_FIELDCAT-FIELDNAME     = 'UMLGO'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'UMCHA'.  " Empfangende/Abgebende Charge
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.   " Receiving/issuing batch
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Empfangende/Abgebende Bwertungsart
* Valuation type of transfer batch
  G_S_FIELDCAT-FIELDNAME     = 'UMBAR'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Sonderbestandskennzeichen der Umlagerung
* Special stock indicator for physical stock transfer
  G_S_FIELDCAT-FIELDNAME     = 'UMSOK'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Kennzeichen Wareneingang unbewertet
* Goods receipt, non-valuated
  G_S_FIELDCAT-FIELDNAME     = 'WEUNB'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Kennzeichen: Grund der Bewegung
* Reason for movement
  G_S_FIELDCAT-FIELDNAME     = 'GRUND'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'KSTRG'.    " Kostenträger
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.  " Cost Object
  G_S_FIELDCAT-SP_GROUP      = 'K'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Nummer für Ergebnisobjekte (CO-PA)
* Profitability segment number (CO-PA)
  G_S_FIELDCAT-FIELDNAME     = 'PAOBJNR'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'K'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'PRCTR'.     " Profit Center
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Profit Center
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Projektstrukturplanelement (PSP-Element)
* Work breakdown structure element (WBS element)

* process the WBS element depends on the release       "n599218
* release          field                               "n599218
* 4.5B and higher  MAT_PSPNR                           "n599218
* 4.0B             PS_PSP_PNR                          "n599218
*                                                      "n599218
  G_S_FIELDCAT-FIELDNAME   = 'MAT_PSPNR'.                   "n599218
  G_S_FIELDCAT-REF_TABNAME = 'MSEG'.                        "n599218
  G_S_FIELDCAT-SP_GROUP    = 'K'.                           "n599218
  PERFORM  F0410_FIELDCAT  USING  C_CHECK  C_NO_OUT.        "n599218

  G_S_FIELDCAT-FIELDNAME     = 'PS_PSP_PNR'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'K'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Netzplannummer für Kontierung
* Network Number for Account Assignment
  G_S_FIELDCAT-FIELDNAME     = 'NPLNR'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'K'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Plannummer zu Vorgängen im Auftrag
* Routing number for operations in the order
  G_S_FIELDCAT-FIELDNAME     = 'AUFPL'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'K'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'AUFPS'.   " Nummer der Auftragsposition
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.    " Order item number
  G_S_FIELDCAT-SP_GROUP      = 'K'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Wareneingangsmenge in Bestellmengeneinheit
* Goods receipt quantity in order unit
  G_S_FIELDCAT-FIELDNAME     = 'BSTMG'.
  G_S_FIELDCAT-QFIELDNAME    = 'BSTME'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'E'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'BSTME'.    " Bestellmengeneinheit
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.     " Order unit
  G_S_FIELDCAT-SP_GROUP      = 'E'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Extern eingegebener Buchungsbetrag in Hauswährung
* Externally entered posting amount in local currency
  G_S_FIELDCAT-FIELDNAME     = 'EXBWR'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-CFIELDNAME    = 'WAERS'.
  G_S_FIELDCAT-SP_GROUP      = 'S'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Wert zu Verkaufspreisen mit Mehrwertsteuer
* Value at sales prices including value-added tax
  G_S_FIELDCAT-FIELDNAME     = 'VKWRT'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-CFIELDNAME    = 'WAERS'.
  G_S_FIELDCAT-SP_GROUP      = 'V'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Verfallsdatum oder Mindesthaltbarkeitsdatum
* Shelf Life Expiration Date
  G_S_FIELDCAT-FIELDNAME     = 'VFDAT'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Extern eingegebener Verkaufswert in Hauswährung
* Externally entered sales value in local currency
  G_S_FIELDCAT-FIELDNAME     = 'EXVKW'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-CFIELDNAME    = 'WAERS'.
  G_S_FIELDCAT-SP_GROUP      = 'S'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'PPRCTR'.    " Partner-Profit Center
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.      " Partner-Profit Center
  G_S_FIELDCAT-SP_GROUP      = 'O'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Material, auf dem der Bestand geführt wird
* Material on which stock is managed
  G_S_FIELDCAT-FIELDNAME     = 'MATBF'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Empfangendes/Abgebendes Material
* Receiving/issuing material
  G_S_FIELDCAT-FIELDNAME     = 'UMMAB'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Gesamter bewerteter Bestand
* Total valuated stock before the posting
  G_S_FIELDCAT-FIELDNAME     = 'LBKUM'.
  G_S_FIELDCAT-QFIELDNAME    = 'MEINS'.                    "note 201670
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Wert des gesamten bewerteten Bestandes
* Value of total valuated stock before the posting
  G_S_FIELDCAT-FIELDNAME     = 'SALK3'.
  G_S_FIELDCAT-CFIELDNAME    = 'WAERS'.                    "note 201670
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'B'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

  G_S_FIELDCAT-FIELDNAME     = 'VPRSV'.    " Preissteuerungskennzeichen
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.     " Price control indicator
  G_S_FIELDCAT-SP_GROUP      = 'S'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Wert zu Verkaufspreisen ohne Mehrwertsteuer
* Value at sales prices excluding value-added tax
  G_S_FIELDCAT-FIELDNAME     = 'VKWRA'.
  G_S_FIELDCAT-CFIELDNAME    = 'WAERS'.                   "note 201670
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'S'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Ursprungszeile im Materialbeleg
* Original line in material document
  G_S_FIELDCAT-FIELDNAME     = 'URZEI'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'S'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Menge in Mengeneinheit aus Lieferschein
* Quantity in unit of measure from delivery note
  G_S_FIELDCAT-FIELDNAME     = 'LSMNG'.
  G_S_FIELDCAT-QFIELDNAME    = 'LSMEH'.                  "note 201670
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* Mengeneinheit aus Lieferschein
* Unit of measure from delivery note
  G_S_FIELDCAT-FIELDNAME     = 'LSMEH'.
  G_S_FIELDCAT-REF_TABNAME   = 'MSEG'.
  G_S_FIELDCAT-SP_GROUP      = 'M'.
  PERFORM  F0410_FIELDCAT    USING  C_CHECK  C_NO_OUT.

* if the field catalog contains a field with values in currency,
* add the currency to to field-catalogue
  DATA : L_CNT_WAERS_ACTIVE  TYPE I,                        "n497992
         L_CNT_WAERS_TOTAL   TYPE I.                        "n497992
                                                            "n497992
  LOOP AT FIELDCAT           INTO  G_S_FIELDCAT.            "n497992
    CHECK : G_S_FIELDCAT-CFIELDNAME    = 'WAERS'.           "n497992
*   this field has a reference to the currency key          "n497992
    ADD  1                   TO  L_CNT_WAERS_TOTAL.         "n497992
                                                            "n497992
    CHECK : G_S_FIELDCAT-NO_OUT IS INITIAL.                 "n497992
*   this field is active                                    "n497992
    ADD  1                   TO  L_CNT_WAERS_ACTIVE.        "n497992
  ENDLOOP.                                                  "n497992
                                                            "n497992
  IF    L_CNT_WAERS_ACTIVE > 0.                             "n497992
*   there is at least one active reference field            "n497992
*   declare currency key WAERS active, too                  "n497992
    G_S_FIELDCAT-FIELDNAME     = 'WAERS'.   "Currency Key   "n497992
    G_S_FIELDCAT-REF_TABNAME   = 'T001'.                    "n497992
    G_S_FIELDCAT-SP_GROUP      = 'M'.                       "n497992
    PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_OUT.       "n497992
                                                            "n497992
  ELSEIF  L_CNT_WAERS_TOTAL > 0.                            "n497992
*   there are only hidden reference fields                  "n497992
*   declare currency key WAERS hidden, too                  "n497992
    G_S_FIELDCAT-FIELDNAME     = 'WAERS'.   "Currency Key   "n497992
    G_S_FIELDCAT-REF_TABNAME   = 'T001'.                    "n497992
    G_S_FIELDCAT-SP_GROUP      = 'M'.                       "n497992
    PERFORM  F0410_FIELDCAT    USING  C_TAKE   C_NO_OUT.    "n497992
  ENDIF.                                                    "n497992
*ENHANCEMENT-POINT RM07MLBD_04 SPOTS ES_RM07MLBD.

*ENHANCEMENT-POINT EHP605_RM07MLBD_15 SPOTS ES_RM07MLBD .

ENDFORM.                     "f0400_create_fieldcat

*----------------------------------------------------------------------*
*    F0410_FIELDCAT
*----------------------------------------------------------------------*

FORM F0410_FIELDCAT
         USING  L_F_CHECK
                L_F_NO_OUT   TYPE      SLIS_FIELDCAT_MAIN-NO_OUT.

  DATA : L_F_CONTINUE(01)    TYPE C,
         L_F_TYPE(01)        TYPE C,
         L_F_FIELDNAME       TYPE      STYPE_FIELDS.

  FIELD-SYMBOLS : <L_FS>.

  IF  L_F_CHECK = C_TAKE.
*   take this entry without check
    MOVE  'X'                TO  L_F_CONTINUE.
  ELSE.
*   create key and look for fieldname
    CONCATENATE              G_S_FIELDCAT-REF_TABNAME
                             '~'
                             G_S_FIELDCAT-FIELDNAME
                             INTO L_F_FIELDNAME.

    READ TABLE G_T_MSEG_FIELDS         INTO G_S_MSEG_FIELDS
                             WITH KEY
                             FIELDNAME = L_F_FIELDNAME
                             BINARY SEARCH.

    IF SY-SUBRC IS INITIAL.
      MOVE  'X'              TO  L_F_CONTINUE.
    ELSE.
*     additional fields are displayed in wrong format :     "n480130
*     clear the working area for the field catalog when     "n480130
*     the current field should not be processed             "n480130
      CLEAR                  G_S_FIELDCAT.                  "n480130
      CLEAR                  L_F_CONTINUE.
    ENDIF.
  ENDIF.

* append entry to field catalog if field is in structure
* else leave this routine
  IF L_F_CONTINUE IS INITIAL.
    CLEAR                    G_S_FIELDCAT.
    EXIT.
  ENDIF.

  IF  NOT L_F_NO_OUT IS INITIAL.
    MOVE  L_F_NO_OUT         TO  G_S_FIELDCAT-NO_OUT.
  ENDIF.

  ADD  : 1                   TO  G_F_COL_POS.
  MOVE : G_F_COL_POS         TO  G_S_FIELDCAT-COL_POS,
         G_F_TABNAME         TO  G_S_FIELDCAT-TABNAME.
  APPEND G_S_FIELDCAT        TO  FIELDCAT.

* create the table with the fields who will be enriched with colors
* and sign
  IF  G_S_FIELDCAT-FIELDNAME  =  'MENGE'  OR
      G_S_FIELDCAT-FIELDNAME  =  'MEINS'  OR
      G_S_FIELDCAT-FIELDNAME  =  'DMBTR'  OR
      G_S_FIELDCAT-FIELDNAME  =  'WAERS'  OR
      G_S_FIELDCAT-FIELDNAME  =  'ERFMG'  OR
      G_S_FIELDCAT-FIELDNAME  =  'ERFME'  OR

      G_S_FIELDCAT-FIELDNAME  =  'BPMNG'  OR
      G_S_FIELDCAT-FIELDNAME  =  'BPRME'  OR
      G_S_FIELDCAT-FIELDNAME  =  'BSTMG'  OR
      G_S_FIELDCAT-FIELDNAME  =  'BSTME'  OR
      G_S_FIELDCAT-FIELDNAME  =  'EXBWR'  OR
      G_S_FIELDCAT-FIELDNAME  =  'VKWRT'  OR

      G_S_FIELDCAT-FIELDNAME  =  'EXVKW'  OR
      G_S_FIELDCAT-FIELDNAME  =  'VKWRA'  OR
      G_S_FIELDCAT-FIELDNAME  =  'LSMNG'  OR
      G_S_FIELDCAT-FIELDNAME  =  'LSMEH'  OR
      G_S_FIELDCAT-FIELDNAME  =  'SHKZG'.

*   look for the type of this field
    CONCATENATE              G_S_FIELDCAT-REF_TABNAME
                             '-'
                             G_S_FIELDCAT-FIELDNAME
                             INTO L_F_FIELDNAME.

    ASSIGN  (L_F_FIELDNAME)  TO <L_FS>.

    IF  SY-SUBRC IS INITIAL.
      DESCRIBE FIELD <L_FS>    TYPE  L_F_TYPE.
      MOVE : G_S_FIELDCAT-FIELDNAME
                             TO  G_T_COLOR_FIELDS-FIELDNAME,
           L_F_TYPE          TO  G_T_COLOR_FIELDS-TYPE.
      APPEND                 G_T_COLOR_FIELDS.
    ENDIF.
  ENDIF.

*ENHANCEMENT-POINT EHP605_RM07MLBD_16 SPOTS ES_RM07MLBD .

  CLEAR                      G_S_FIELDCAT.

ENDFORM.                     "F0410_FIELDCAT

*&----------------------------------------------------------"n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  belege_ergaenzen_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BELEGE_ERGAENZEN_2.                                    "n443935
                                                            "n443935
* control break : process the entries from a group          "n443935
                                                            "n443935
* look for the matching FI documents with set and get       "n443935
  MOVE : G_S_MSEG_OLD-MBLNR  TO  MATKEY-MBLNR,              "n443935
         G_S_MSEG_OLD-MJAHR  TO  MATKEY-MJAHR.              "n443935
      SORT   G_T_BSIM_LEAN."Added by SPLABAP during code remediation                                                    "n443935
  READ  TABLE G_T_BSIM_LEAN  INTO  G_S_BSIM_LEAN            "n443935
                   WITH KEY  BUKRS  =  G_S_MSEG_OLD-BUKRS   "n443935
                             BWKEY  =  G_S_MSEG_OLD-BWKEY   "n443935
                             MATNR  =  G_S_MSEG_OLD-MATNR   "n443935
                             BWTAR  =  G_S_MSEG_OLD-BWTAR   "n443935
                             SHKZG  =  G_S_MSEG_OLD-SHKZG   "n443935
                             MEINS  =  G_S_MSEG_OLD-MEINS   "n443935
                             BUDAT  =  G_S_MSEG_OLD-BUDAT   "n443935
                             BLART  =  G_S_MSEG_OLD-BLART   "n443935
                             AWKEY  =  MATKEY               "n443935
                             BINARY SEARCH.                 "n443935
                                                            "n443935
  IF SY-SUBRC IS INITIAL.                                   "n443935
    MOVE  SY-TABIX           TO  G_F_TABIX_START.           "n443935
                                                            "n443935
*   continue with sequential read of working table          "n443935
    LOOP AT G_T_BSIM_LEAN    INTO  G_S_BSIM_LEAN            "n443935
                             FROM  G_F_TABIX_START.         "n443935
                                                            "n443935
      IF  G_S_BSIM_LEAN-BUKRS  =  G_S_MSEG_OLD-BUKRS  AND   "n443935
          G_S_BSIM_LEAN-BWKEY  =  G_S_MSEG_OLD-BWKEY  AND   "n443935
          G_S_BSIM_LEAN-MATNR  =  G_S_MSEG_OLD-MATNR  AND   "n443935
          G_S_BSIM_LEAN-BWTAR  =  G_S_MSEG_OLD-BWTAR  AND   "n443935
          G_S_BSIM_LEAN-SHKZG  =  G_S_MSEG_OLD-SHKZG  AND   "n443935
          G_S_BSIM_LEAN-MEINS  =  G_S_MSEG_OLD-MEINS  AND   "n443935
          G_S_BSIM_LEAN-BUDAT  =  G_S_MSEG_OLD-BUDAT  AND   "n443935
          G_S_BSIM_LEAN-BLART  =  G_S_MSEG_OLD-BLART  AND   "n443935
          G_S_BSIM_LEAN-AWKEY  =  MATKEY.                   "n443935
*       select all matching entries                         "n443935
        ADD   1              TO  G_CNT_BSIM_ENTRIES.        "n443935
        MOVE-CORRESPONDING  G_S_BSIM_LEAN                   "n443935
                             TO  G_S_BSIM_WORK.             "n443935
        MOVE  SY-TABIX       TO  G_S_BSIM_WORK-TABIX.       "n443935
        APPEND G_S_BSIM_WORK TO  G_T_BSIM_WORK.             "n443935
      ELSE.                                                 "n443935
        EXIT.                                               "n443935
      ENDIF.                                                "n443935
    ENDLOOP.                                                "n443935
  ENDIF.                                                    "n443935

  IF  G_FLAG_BREAK-B1 = 'X'.                                "n921164
    BREAK-POINT                ID MMIM_REP_MB5B.            "n921164
*   dynamic break-point : results in contol break           "n921164
  ENDIF.

* how many matching entries from BSIM found ?               "n443935
  IF      G_CNT_BSIM_ENTRIES IS INITIAL.                    "n443935
*   no BSIM entries found -> no action.                     "n443935
                                                            "n443935
  ELSEIF  G_CNT_BSIM_ENTRIES = 1  AND                       "n443935
          G_CNT_MSEG_ENTRIES = 1.                           "n443935
*   the ideal case 1 MM and 1 FI document;                  "n443935
*   mark this FI doc for deletion                           "n443935
    LOOP AT G_T_BSIM_WORK    INTO  G_S_BSIM_WORK.           "n443935
      READ  TABLE  G_T_BSIM_LEAN  INTO  G_S_BSIM_LEAN       "n443935
                             INDEX  G_S_BSIM_WORK-TABIX.    "n443935
                                                            "n443935
      CHECK : SY-SUBRC IS INITIAL.                          "n443935
      MOVE  : 'D'            TO  G_S_BSIM_LEAN-ACCESSED.    "n443935
      MODIFY  G_T_BSIM_LEAN  FROM  G_S_BSIM_LEAN            "n443935
                             INDEX  G_S_BSIM_WORK-TABIX     "n443935
                             TRANSPORTING  ACCESSED.        "n451923
                                                            "n443935
*     set the FI doc number into the entry of the MM doc    "n443935
      READ  TABLE  G_T_MSEG_WORK  INTO  G_S_MSEG_WORK       "n443935
                             INDEX  1.                      "n443935
      CHECK : SY-SUBRC IS INITIAL.                          "n443935
                                                            "n443935
      MOVE : G_S_BSIM_WORK-BELNR                            "n443935
                             TO  G_S_MSEG_WORK-BELNR,       "n443935
             G_S_BSIM_WORK-GJAHR                            "n443935
                             TO  G_S_MSEG_WORK-GJAHR.       "n443935
      IF GV_SWITCH_EHP6RU = ABAP_TRUE.
        MOVE: G_S_BSIM_WORK-BUZEI
                             TO  G_S_MSEG_WORK-BUZEI.
        MOVE-CORRESPONDING G_S_BSIM_WORK TO G_T_BSEG_KEY.
        APPEND G_T_BSEG_KEY.
      ENDIF.

*     consider special gain/loss-handling of IS-OIL         "n497992
**# IF EXIST OI001
**"    if  g_s_mseg_work-oiglcalc = 'L'  and            "n497992
**"        g_s_mseg_work-shkzg    = 'H'  and            "n497992
**"        g_s_mseg_work-dmbtr    = 0.                  "n497992
**"      move  g_s_bsim_work-dmbtr                      "n497992
**"                  to  g_s_mseg_work-dmbtr.           "n497992
**"    endif.                                           "n497992
**"                                                     "n497992
**"    MODIFY G_T_MSEG_work                             "n497992
**"                 FROM  G_S_MSEG_work                 "n497992
**"                 INDEX  1                            "n497992
**"                 TRANSPORTING BELNR GJAHR dmbtr.     "n497992
**# ELSE
*      MODIFY G_T_MSEG_work  FROM  G_S_MSEG_work        "n443935
*                            INDEX  1                   "n443935
*                            TRANSPORTING  BELNR GJAHR. "n443935
**# ENDIF
*     IS-OIL specific functions without ABAP preprocessor   "n599218 A
      IF  G_FLAG_IS_OIL_ACTIVE = 'X'.       "IS-OIL ?       "n599218 A
        IF  G_S_MSEG_WORK-OIGLCALC = 'L'  AND               "n599218 A
            G_S_MSEG_WORK-SHKZG    = 'H'  AND               "n599218 A
            G_S_MSEG_WORK-DMBTR    = 0.                     "n599218 A
          MOVE  G_S_BSIM_WORK-DMBTR                         "n599218 A
                             TO  G_S_MSEG_WORK-DMBTR.       "n599218 A
        ENDIF.                                              "n599218 A
                                                            "n599218 A
        MODIFY G_T_MSEG_WORK                                "n599218 A
                   FROM  G_S_MSEG_WORK                      "n599218 A
                   INDEX  1                                 "n599218 A
                   TRANSPORTING BELNR GJAHR BUZEI DMBTR.
      ELSE.                                                 "n599218 A
        MODIFY G_T_MSEG_WORK  FROM  G_S_MSEG_WORK           "n599218 A
                            INDEX  1                        "n599218 A
                            TRANSPORTING  BELNR GJAHR BUZEI.
      ENDIF.                                                "n599218 A

    ENDLOOP.                                                "n443935
                                                            "n443935
  ELSE.                                                     "n443935
*   there are a lot of MM docs                              "n443935
    PERFORM                  BELEGE_ERGAENZEN_SEVERAL_DOCS. "n443935
                                                            "n443935
  ENDIF.                                                    "n443935
                                                            "n443935
* copy the number and fiscal year into the matching         "n451923
* entry of the main table G_T_MSEG_LEAN                     "n451923
  LOOP AT G_T_MSEG_WORK      INTO  G_S_MSEG_WORK.           "n451923
*   only with useful FI doc data                            "n451923
    CHECK : NOT G_S_MSEG_WORK-BELNR IS INITIAL.             "n451923
                                                            "n443935
*   read the original entry and change it                   "n451923
    READ TABLE G_T_MSEG_LEAN INTO  G_S_MSEG_UPDATE          "n451923
                             INDEX G_S_MSEG_WORK-TABIX.     "n451923
                                                            "n443935
    CHECK : SY-SUBRC IS INITIAL.   "entry found ?           "n451923
    MOVE  : G_S_MSEG_WORK-BELNR                             "n451923
                             TO  G_S_MSEG_UPDATE-BELNR,     "n451923
            G_S_MSEG_WORK-GJAHR                             "n451923
                             TO  G_S_MSEG_UPDATE-GJAHR.     "n451923
    IF GV_SWITCH_EHP6RU = ABAP_TRUE.
      MOVE: G_S_MSEG_WORK-BUZEI
                             TO  G_S_MSEG_UPDATE-BUZEI.
      MOVE-CORRESPONDING G_S_MSEG_WORK TO G_T_BSEG_KEY.
      APPEND G_T_BSEG_KEY.
    ENDIF.

*   consider special gain/loss-handling of IS-OIL           "n497992
**# IF EXIST OI001
**"  move  g_s_mseg_work-dmbtr                          "n497992
**"                 to  g_s_mseg_update-dmbtr.          "n497992
**"                                                     "n497992
**"  MODIFY G_T_MSEG_lean                               "n497992
**"                 FROM  G_S_MSEG_update               "n497992
**"                 index g_s_mseg_work-tabix           "n497992
**"                 TRANSPORTING BELNR GJAHR dmbtr.     "n497992
**# ELSE
*    modify  g_t_mseg_lean  from  g_s_mseg_update       "n451923
*                           index g_s_mseg_work-tabix   "n451923
*                           transporting  belnr gjahr.  "n451923
**# ENDIF
*   IS-OIL specific functions without ABAP preprocessor     "n599218 A
    IF  G_FLAG_IS_OIL_ACTIVE = 'X'.        "IS-OIL ?       "n599218 A
      MOVE  G_S_MSEG_WORK-DMBTR                             "n599218 A
                             TO  G_S_MSEG_UPDATE-DMBTR.     "n599218 A
                                                            "n599218 A
      MODIFY G_T_MSEG_LEAN                                  "n599218 A
                 FROM  G_S_MSEG_UPDATE                      "n599218 A
                 INDEX G_S_MSEG_WORK-TABIX                  "n599218 A
                 TRANSPORTING BELNR GJAHR BUZEI DMBTR.
    ELSE.                                                   "n599218 A
      MODIFY G_T_MSEG_LEAN FROM  G_S_MSEG_UPDATE            "n599218 A
                           INDEX G_S_MSEG_WORK-TABIX        "n599218 A
                           TRANSPORTING  BELNR GJAHR BUZEI.
    ENDIF.                                                  "n599218 A

  ENDLOOP.                                                  "n451923

  PERFORM                    BELEGE_ERGAENZEN_CLEAR.        "n443935
                                                            "n443935
ENDFORM.                     "belege_ergaenzen_2            "n443935
                                                            "n443935
*&----------------------------------------------------------"n443935
*& belege_ergaenzen_clear
*&----------------------------------------------------------"n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  belege_ergaenzen_clear
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BELEGE_ERGAENZEN_CLEAR.                                "n443935
                                                            "n443935
* clear working areas for the next group                    "n443935
  REFRESH : G_T_BSIM_WORK,   G_T_MSEG_WORK.                 "n443935
  CLEAR   : G_CNT_MSEG_ENTRIES, G_CNT_MSEG_DONE,            "n443935
            G_CNT_BSIM_ENTRIES.                             "n443935
                                                            "n443935
ENDFORM.                     "belege_ergaenzen_clear.       "n443935
                                                            "n443935
*&----------------------------------------------------------"n443935
*    belege_ergaenzen_several_docs
*&----------------------------------------------------------"n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  belege_ergaenzen_several_docs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BELEGE_ERGAENZEN_SEVERAL_DOCS.                         "n443935

*ENHANCEMENT-POINT EHP605_RM07MLBD_17 SPOTS ES_RM07MLBD STATIC .
                                                            "n443935
* first step : the entries must hit quantity and value      "n443935
  LOOP AT G_T_MSEG_WORK    INTO  G_S_MSEG_WORK.             "n443935
                                                            "n443935
*   look for a matching FI doc                              "n443935
    LOOP AT G_T_BSIM_WORK  INTO  G_S_BSIM_WORK.             "n443935
*     ingnore the entries for deletion                      "n443935
      CHECK : G_S_BSIM_WORK-ACCESSED NE 'D'.                "n443935
                                                            "n443935
      IF  G_S_BSIM_WORK-MENGE = G_S_MSEG_WORK-MENGE  AND    "n443935
          G_S_BSIM_WORK-DMBTR = G_S_MSEG_WORK-DMBTR.        "n443935
*       mark the entries                                    "n443935
        ADD  1               TO  G_CNT_MSEG_DONE.           "n443935
        MOVE : G_S_BSIM_WORK-BELNR                          "n443935
                             TO  G_S_MSEG_WORK-BELNR,       "n443935
               G_S_BSIM_WORK-GJAHR                          "n443935
                             TO  G_S_MSEG_WORK-GJAHR.       "n443935
        IF GV_SWITCH_EHP6RU = ABAP_TRUE.
          MOVE: G_S_BSIM_WORK-BUZEI
                             TO  G_S_MSEG_WORK-BUZEI.
          MOVE-CORRESPONDING G_S_BSIM_WORK TO G_T_BSEG_KEY.
          APPEND G_T_BSEG_KEY.
        ENDIF.

        MODIFY G_T_MSEG_WORK  FROM  G_S_MSEG_WORK           "n443935
                             TRANSPORTING  BELNR GJAHR BUZEI.
                                                            "n443935
*       mark the entries for deletion                       "n443935
        MOVE    'D'          TO  G_S_BSIM_WORK-ACCESSED.    "n443935
        MODIFY  G_T_BSIM_WORK  FROM  G_S_BSIM_WORK          "n443935
                             TRANSPORTING  ACCESSED.        "n451923
        EXIT. "#EC CI_NOORDER
        "Added by SPLABAP during code remediation
                       "Stop at the firts hit         "n443935
      ENDIF.                                                "n443935
    ENDLOOP.                                                "n443935
  ENDLOOP.                                                  "n443935

  IF  G_FLAG_BREAK-B2 = 'X'.                                "n921164
    BREAK-POINT                ID MMIM_REP_MB5B.            "n921164
*   dynamic break-point : in control break                  "n921164
  ENDIF.                                                    "n921164

  IF  G_CNT_MSEG_ENTRIES  NE G_CNT_MSEG_DONE.               "n443935
*   there are MM docs without FI doc left                   "n443935
                                                            "n443935
*     subtract the quantity and value from MM doc from      "n443935
*     the fields of the FI doc                              "n443935
    LOOP AT G_T_MSEG_WORK  INTO  G_S_MSEG_WORK.             "n443935
                                                            "n443935
*       take only the entries without FI doc number         "n443935
      CHECK : G_S_MSEG_WORK-BELNR IS INITIAL.               "n443935
                                                            "n443935
      LOOP AT G_T_BSIM_WORK  INTO  G_S_BSIM_WORK.           "n443935
*         ingnore the entries for deletion                  "n443935
        CHECK : G_S_BSIM_WORK-ACCESSED NE 'D'.              "n443935
                                                            "n443935
        IF G_S_BSIM_WORK-MENGE GE G_S_MSEG_WORK-MENGE AND   "n443935
           G_S_BSIM_WORK-DMBTR GE G_S_MSEG_WORK-DMBTR.      "n443935
                                                            "n443935
          SUBTRACT :                                        "n443935
            G_S_MSEG_WORK-MENGE FROM  G_S_BSIM_WORK-MENGE,  "n443935
            G_S_MSEG_WORK-DMBTR FROM  G_S_BSIM_WORK-DMBTR.  "n443935
                                                            "n443935
          IF  G_S_BSIM_WORK-MENGE  IS INITIAL  AND          "n443935
              G_S_BSIM_WORK-DMBTR  IS INITIAL.              "n443935
*           mark the entry for deletion                     "n443935
            MOVE    'D'      TO  G_S_BSIM_WORK-ACCESSED.    "n443935
          ELSE.                                             "n443935
*           set the flag for check the merge process        "n443935
            MOVE    'X'      TO  G_S_BSIM_WORK-ACCESSED.    "n443935
          ENDIF.                                            "n443935
                                                            "n443935
          MODIFY  G_T_BSIM_WORK  FROM  G_S_BSIM_WORK        "n443935
*           change quantity and value in working table, too  "n747306
            TRANSPORTING  ACCESSED MENGE DMBTR.             "n747306
                                                            "n443935
*         mark the entries                                  "n443935
          ADD  1             TO  G_CNT_MSEG_DONE.           "n443935
          MOVE : G_S_BSIM_WORK-BELNR                        "n443935
                             TO  G_S_MSEG_WORK-BELNR,       "n443935
                 G_S_BSIM_WORK-GJAHR                        "n443935
                             TO  G_S_MSEG_WORK-GJAHR.       "n443935
          IF GV_SWITCH_EHP6RU = ABAP_TRUE.
            MOVE: G_S_BSIM_WORK-BUZEI
                             TO  G_S_MSEG_WORK-BUZEI.
            MOVE-CORRESPONDING G_S_BSIM_WORK TO G_T_BSEG_KEY.
            APPEND G_T_BSEG_KEY.
          ENDIF.

          MODIFY G_T_MSEG_WORK  FROM  G_S_MSEG_WORK         "n443935
                             TRANSPORTING  BELNR GJAHR BUZEI.
          EXIT. "#EC CI_NOORDER
          "Added by SPLABAP during code remediation             "Stop at the first hit         "n443935
        ENDIF.                                              "n443935
      ENDLOOP.                                              "n443935
    ENDLOOP.                                                "n443935
  ENDIF.                                                    "n443935
                                                            "n443935
* mark the processed FI docs for deletion                   "n443935
  LOOP AT G_T_BSIM_WORK    INTO  G_S_BSIM_WORK.             "n443935
    CHECK   G_S_BSIM_WORK-ACCESSED = 'D'.                   "n443935
                                                            "n443935
    READ  TABLE  G_T_BSIM_LEAN  INTO  G_S_BSIM_LEAN         "n443935
                             INDEX  G_S_BSIM_WORK-TABIX.    "n443935
                                                            "n443935
    CHECK : SY-SUBRC IS INITIAL.                            "n443935
    MOVE  : 'D'              TO  G_S_BSIM_LEAN-ACCESSED.    "n443935
    MODIFY  G_T_BSIM_LEAN    FROM   G_S_BSIM_LEAN           "n443935
                             INDEX  G_S_BSIM_WORK-TABIX     "n443935
                             TRANSPORTING  ACCESSED.        "n451923
  ENDLOOP.                                                  "n443935

*ENHANCEMENT-POINT EHP605_RM07MLBD_18 SPOTS ES_RM07MLBD .
                                                            "n443935
ENDFORM.                     "belege_ergaenzen_several_docs "n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND                                             *
*&---------------------------------------------------------------------*

FORM USER_COMMAND                                           "#EC CALLED
                   USING     R_UCOMM      LIKE  SY-UCOMM
                             RS_SELFIELD  TYPE  SLIS_SELFIELD.

  TYPES: BEGIN OF TY_S_SEL,
           MBLNR LIKE  MSEG-MBLNR,
           MJAHR LIKE  MSEG-MJAHR,
           ZEILE LIKE  MSEG-ZEILE,
           BUKRS LIKE  MSEG-BUKRS,
           BELNR LIKE  MSEG-BELNR,
           GJAHR LIKE  MSEG-GJAHR,
         END OF TY_S_SEL,

         TY_T_SEL TYPE TY_S_SEL OCCURS 0.

  DATA: L_VALUE(10) TYPE C,                                 "n1583816
        LS_SEL      TYPE TY_S_SEL,
        LT_SEL      TYPE TY_T_SEL,
        L_LINES     LIKE SY-TABIX,
        LS_FC       TYPE SLIS_FIELDCAT_ALV,
        LT_FC       TYPE SLIS_T_FIELDCAT_ALV,
        LS_SELFIELD TYPE SLIS_SELFIELD,
        L_FI_DOC    TYPE C  LENGTH 1.                       "n1511550

* Unfortunately the output list of this report consists
* of several ALVs, one started at the end-event of the other.
* This abstrucse programming style was chosen to create a list
* layout similar to the one in release 3.1. Now this causes a severe
* problem: When selecting a line, we do not know which ALV (and there-
* for which line in table IMSEG) has been selected. We can only use
* the value of the selected field to access the data-table.
* In case of ambiguities, a popup has to be transmitted where the
* user has to reselect the document he wants to see. This is
* difficult to understand, if you do not know the problems of
* programming ABAP.

  CASE R_UCOMM.
    WHEN '9PBP'.
*     Get line of IMSEG which "look" like the one selected
      L_VALUE = RS_SELFIELD-VALUE.
      CHECK NOT L_VALUE IS INITIAL.                         "204872
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'           "n1604106
        EXPORTING                                           "n1604106
          INPUT  = L_VALUE                                  "n1604106
        IMPORTING                                           "n1604106
          OUTPUT = L_VALUE.                                 "n1604106
      IF RS_SELFIELD-SEL_TAB_FIELD = 'G_T_BELEGE-MBLNR' OR
         RS_SELFIELD-SEL_TAB_FIELD = 'G_T_BELEGE1-MBLNR'.
        LOOP AT G_T_MSEG_LEAN          INTO  G_S_MSEG_LEAN
                                       WHERE MBLNR = L_VALUE.
          LS_SEL-MBLNR = G_S_MSEG_LEAN-MBLNR.
          LS_SEL-MJAHR = G_S_MSEG_LEAN-MJAHR.
          LS_SEL-ZEILE = G_S_MSEG_LEAN-ZEILE.
          COLLECT LS_SEL INTO LT_SEL.
        ENDLOOP.

      ELSEIF RS_SELFIELD-SEL_TAB_FIELD = 'G_T_BELEGE-BELNR' OR
             RS_SELFIELD-SEL_TAB_FIELD = 'G_T_BELEGE1-BELNR'.
        L_FI_DOC = 'X'.
        LOOP AT G_T_MSEG_LEAN          INTO  G_S_MSEG_LEAN
                                       WHERE BELNR = L_VALUE.
          LS_SEL-BELNR = G_S_MSEG_LEAN-BELNR.
          LS_SEL-GJAHR = G_S_MSEG_LEAN-GJAHR.
          LS_SEL-BUKRS = G_S_MSEG_LEAN-BUKRS.
          COLLECT LS_SEL INTO LT_SEL.
        ENDLOOP.

      ENDIF.
      SORT LT_SEL BY MJAHR MBLNR ZEILE BUKRS BELNR GJAHR.
*     Read first line. If L_LINES = 1, LS_SEL is filled properly.
      READ TABLE LT_SEL INTO LS_SEL INDEX 1.
      DESCRIBE TABLE LT_SEL LINES L_LINES.
*     If no line found, the cursor was not on a useful value.
      IF L_LINES = 0.
        MESSAGE S270.
        EXIT.
      ENDIF.
*     If more than one line found, it gets difficult. We send a popup
*     where the user may select a single line.
      IF L_LINES > 1.
*       Create fieldcatalog
        DEFINE FC_ADD.
          LS_FC-FIELDNAME     = &1.
          LS_FC-REF_TABNAME   = &2.
          LS_FC-REF_FIELDNAME = &3.
          APPEND LS_FC TO LT_FC.
        END-OF-DEFINITION.
        CLEAR LS_SEL.
        IF L_FI_DOC IS INITIAL.
          FC_ADD 'MBLNR' 'MKPF' 'MBLNR'.
          FC_ADD 'MJAHR' 'MKPF' 'MJAHR'.
          FC_ADD 'ZEILE' 'MSEG' 'ZEILE'.
        ELSE.
          FC_ADD 'BUKRS' 'BKPF' 'BUKRS'.
          FC_ADD 'BELNR' 'BKPF' 'BELNR'.
          FC_ADD 'GJAHR' 'BKPF' 'GJAHR'.
        ENDIF.

        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
            I_ZEBRA     = 'X'
            I_TABNAME   = 'LT_SEL'
            IT_FIELDCAT = LT_FC
          IMPORTING
            ES_SELFIELD = LS_SELFIELD
          TABLES
            T_OUTTAB    = LT_SEL.
*       Read table with the unique index.
        READ TABLE LT_SEL INTO LS_SEL INDEX LS_SELFIELD-TABINDEX.
        IF SY-SUBRC <> 0.
          EXIT.
        ENDIF.
      ENDIF.

*     read and save the user parameters before calling
*     transaction MIGO or FB03
      PERFORM                USER_PARAMETERS_SAVE.

*     Now call the corresponding application. LS_SEL is always filled
*     correctly.
      IF L_FI_DOC IS INITIAL.

*     call the display transcation MIGO for the MM document "TEST
        CALL FUNCTION 'MIGO_DIALOG'                         "n547170
          EXPORTING                                         "n547170
            I_ACTION            = 'A04'                     "n547170
            I_REFDOC            = 'R02'                     "n547170
            I_NOTREE            = 'X'                       "n547170
            I_NO_AUTH_CHECK     = ' '                       "n547170
            I_DEADEND           = 'X'                       "n547170
            I_SKIP_FIRST_SCREEN = 'X'                       "n547170
            I_OKCODE            = 'OK_GO'                   "n547170
            I_MBLNR             = LS_SEL-MBLNR              "n547170
            I_MJAHR             = LS_SEL-MJAHR              "n547170
            I_ZEILE             = LS_SEL-ZEILE.             "n547170
      ELSE.
        SET PARAMETER ID 'BLN' FIELD LS_SEL-BELNR.
        SET PARAMETER ID 'BUK' FIELD LS_SEL-BUKRS.
        SET PARAMETER ID 'GJR' FIELD LS_SEL-GJAHR.
        CALL TRANSACTION 'FB03'               "#EC CI_CALLTA  "n1511550
                             AND SKIP FIRST SCREEN.         "n1511550
      ENDIF.

*     restore the former user parameters
      PERFORM                USER_PARAMETERS_RESTORE.

  ENDCASE.

ENDFORM.                               " USER_COMMAND

*-----------------------------------------------------------"n547170
*    esdus_get_parameters                                   "n547170
*-----------------------------------------------------------"n547170

FORM ESDUS_GET_PARAMETERS.                                  "n547170
*-----------------------------------------------------------"n547170
* Initialization of the user defaults for the checkboxes
* read the settings from table ESDUS
*-----------------------------------------------------------

* only in dialog mode
  CHECK : SY-BATCH IS INITIAL.

  DATA : L_CNT_RADIOBUTTON   TYPE I.

* get the parameters from the last run from table ESDUS as
* default values  in release 4.6 and higher

  IF OREF_SETTINGS IS INITIAL.
    CREATE OBJECT OREF_SETTINGS
      EXPORTING
        I_ACTION = 'RM07MLBD'.
  ENDIF.

** get the parameters from the last run
  LGBST    = OREF_SETTINGS->GET( 'LGBST'  ).
  BWBST    = OREF_SETTINGS->GET( 'BWBST'  ).
  SBBST    = OREF_SETTINGS->GET( 'SBBST'  ).
  XCHAR    = OREF_SETTINGS->GET( 'XCHAR'  ).
  XSUM     = OREF_SETTINGS->GET( 'XSUM'   ).
  PA_SUMFL = OREF_SETTINGS->GET( 'PA_SUMFL'   ).
  NOSTO    = OREF_SETTINGS->GET( 'NOSTO'  ).
  PA_AISTR  = OREF_SETTINGS->GET( 'PA_AISTR' ).             "n1481757

**  get the parameters for the list categories              "n599218
  PA_WDZER = OREF_SETTINGS->GET( 'PA_WDZER' ).              "n599218
  PA_WDZEW = OREF_SETTINGS->GET( 'PA_WDZEW' ).              "n599218
  PA_WDWIZ = OREF_SETTINGS->GET( 'PA_WDWIZ' ).              "n599218
  PA_WDWUW = OREF_SETTINGS->GET( 'PA_WDWUW' ).              "n599218
  PA_WDWEW = OREF_SETTINGS->GET( 'PA_WDWEW' ).              "n599218
  PA_NDSTO = OREF_SETTINGS->GET( 'PA_NDSTO' ).              "n599218
  PA_NDZER = OREF_SETTINGS->GET( 'PA_NDZER' ).              "n599218
  XNOMCHB  = OREF_SETTINGS->GET( 'XNOMCHB' ).               "838360

**  check radiobutton rules
  IF  NOT LGBST IS INITIAL.
    ADD  1                 TO  L_CNT_RADIOBUTTON.
  ENDIF.

  IF  NOT BWBST IS INITIAL.
    ADD  1                 TO  L_CNT_RADIOBUTTON.
  ENDIF.

  IF  NOT SBBST IS INITIAL.
    ADD  1                 TO  L_CNT_RADIOBUTTON.
  ENDIF.

  IF  L_CNT_RADIOBUTTON NE 1.
**    offend against radiobutton rules ?
**    yes -> set the first and delete the rest
    MOVE : 'X'             TO  LGBST.
    CLEAR :                BWBST, SBBST.
  ENDIF.

* at the first time ( or in a lower release ) all seven     "n599218
* list categories will be initial --> activate them all     "n599218
  PERFORM                    F0850_EMPTY_PARAMETERS.        "n599218
                                                            "n599218
  IF  G_CNT_EMPTY_PARAMETER = 7.                            "n599218
    MOVE : 'X'               TO  PA_WDZER,                  "n599218
           'X'               TO  PA_WDZEW,                  "n599218
           'X'               TO  PA_WDWIZ,                  "n599218
           'X'               TO  PA_WDWUW,                  "n599218
           'X'               TO  PA_WDWEW,                  "n599218
           'X'               TO  PA_NDSTO,                  "n599218
           'X'               TO  PA_NDZER.                  "n599218
  ENDIF.                                                    "n599218

ENDFORM.                     "esdus_get_parameters          "n547170

*-----------------------------------------------------------"n547170
*    esdus_save_parameters                                  "n547170
*-----------------------------------------------------------"n547170

FORM ESDUS_SAVE_PARAMETERS.                                 "n547170
                                                            "n547170
* only in dialog mode
  CHECK : SY-BATCH IS INITIAL.

* Save the settings in release 4.6 and higher
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'LGBST'
    I_ACTIVE = LGBST ).
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'BWBST'
    I_ACTIVE = BWBST ).
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'SBBST'
    I_ACTIVE = SBBST ).
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'XCHAR'
    I_ACTIVE = XCHAR ).
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'XNOMCHB'     "838360
                                  I_ACTIVE  =  XNOMCHB  ).  "838360


*    CALL METHOD oref_settings->set( i_element = 'XONUL'
*                                    i_active  =  xonul   ).
*
*    CALL METHOD oref_settings->set( i_element = 'XVBST'
*                                    i_active  =  XVBST   ).
*    CALL METHOD oref_settings->set( i_element = 'XNVBST'
*                                    i_active  =  xnvbst  ).

*   save the list categories                                "n599218
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'PA_WDZER'    "n599218
                                  I_ACTIVE  =  PA_WDZER ).  "n599218
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'PA_WDZEW'    "n599218
                                  I_ACTIVE  =  PA_WDZEW ).  "n599218
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'PA_WDWIZ'    "n599218
                                  I_ACTIVE  =  PA_WDWIZ ).  "n599218
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'PA_WDWUW'    "n599218
                                  I_ACTIVE  =  PA_WDWUW ).  "n599218
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'PA_WDWEW'    "n599218
                                  I_ACTIVE  =  PA_WDWEW ).  "n599218

  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'PA_NDSTO'    "n599218
                                  I_ACTIVE  =  PA_NDSTO ).  "n599218
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'PA_NDZER'    "n599218
                                  I_ACTIVE  =  PA_NDZER ).  "n599218

  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'XSUM'
    I_ACTIVE = XSUM ).
  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'PA_SUMFL'
    I_ACTIVE = PA_SUMFL ).

  CALL METHOD OREF_SETTINGS->SET( I_ELEMENT = 'NOSTO'
    I_ACTIVE = NOSTO ).

  CALL METHOD OREF_SETTINGS->FLUSH.

*   carry out the database updates only; the normal commit  "n599218
*   command does not allow to record this transaction for   "n599218
*   a batch input session using transaction SHDB            "n599218
  CALL FUNCTION 'DB_COMMIT'.                                "n599218

ENDFORM.                     "esdus_save_parameters         "n547170

*-----------------------------------------------------------"n547170
                                                            "n599218 A
*&---------------------------------------------------------------------*
*&      Form  check_is_oil_system
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CHECK_IS_OIL_SYSTEM.                                   "n599218 A
                                                            "n599218 A
  MOVE  'OI001'              TO  G_F_DCOBJDEF_NAME.         "n599218 A
  CLEAR : G_FLAG_IS_OIL_ACTIVE, G_CNT_IS_OIL.               "n599218 A
                                                            "n599218 A
* does database OI001 exist in this system ?                "n599218 A
  CALL FUNCTION  'DDIF_NAMETAB_GET'                         "n599218 A
    EXPORTING                                               "n599218 A
      TABNAME           = G_F_DCOBJDEF_NAME                 "n599218 A
    TABLES                                                  "n599218 A
      X031L_TAB         = G_T_X031L                         "n599218 A
    EXCEPTIONS                                              "n599218 A
      OTHERS            = 1.                                "n599218 A
                                                            "n599218 A
  CHECK SY-SUBRC IS INITIAL.      "OI001 is available ?     "n599218 A
                                                            "n599218 A
* check definition of MM document item MSEG                 "n599218 A
  MOVE  'MSEG'               TO  G_F_DCOBJDEF_NAME.         "n599218 A
                                                            "n599218 A
  CALL FUNCTION 'DDIF_NAMETAB_GET'                          "n599218 A
    EXPORTING                                               "n599218 A
      TABNAME           = G_F_DCOBJDEF_NAME                 "n599218 A
    TABLES                                                  "n599218 A
      X031L_TAB         = G_T_X031L                         "n599218 A
    EXCEPTIONS                                              "n599218 A
      OTHERS            = 1.                                "n599218 A
                                                            "n599218 A
  CHECK SY-SUBRC IS INITIAL.      "structure MSEG found     "n599218 A
                                                            "n599218 A
* check whether the IS-OIL specific fields are available    "n599218 A
  LOOP AT G_T_X031L          INTO  G_S_X031L.               "n599218 A
    CASE  G_S_X031L-FIELDNAME.                              "n599218 A
      WHEN  'OIGLCALC'.                                     "n599218 A
        ADD   1              TO  G_CNT_IS_OIL.              "n599218 A
                                                            "n599218 A
      WHEN  'OIGLSKU'.                                      "n599218 A
        ADD   2              TO  G_CNT_IS_OIL.              "n599218 A
    ENDCASE.                                                "n599218 A
  ENDLOOP.                                                  "n599218 A
                                                            "n599218 A
* in the case structure MSEG comprises both fields          "n599218 A
* -> activate the IS-OIL function                           "n599218 A
  IF    G_CNT_IS_OIL = 3.                                   "n599218 A
    MOVE  'X'                TO  G_FLAG_IS_OIL_ACTIVE.      "n599218 A
  ENDIF.                                                    "n599218 A
                                                            "n599218 A
ENDFORM.                     "check_is_oil_system.          "n599218 A
                                                            "n599218 A
*----------------------------------------------------------------------*
*  calculate_offsets.
*----------------------------------------------------------------------*

* calculate the offsets for the list header

FORM CALCULATE_OFFSETS.

*  working area
  DATA : L_TEXT(132)         TYPE C.

* get the maximal length of the text elements to be used
  PERFORM  GET_MAX_TEXT_LENGTH USING  TEXT-020.
  PERFORM  GET_MAX_TEXT_LENGTH USING  TEXT-021.
  PERFORM  GET_MAX_TEXT_LENGTH USING  TEXT-022.
  PERFORM  GET_MAX_TEXT_LENGTH USING  TEXT-023.
  PERFORM  GET_MAX_TEXT_LENGTH USING  TEXT-025.

  G_OFFSET_HEADER            =  G_F_LENGTH_MAX + 3.

  CLEAR                      G_F_LENGTH_MAX.

  IF  BWBST IS INITIAL.
*     stocks and quantities only
    MOVE   TEXT-007          TO  G_DATE_LINE_FROM-TEXT.
    WRITE : DATUM-LOW        TO  G_DATE_LINE_FROM-DATUM DD/MM/YYYY.
    CONDENSE                 G_DATE_LINE_FROM.
    PERFORM  GET_MAX_TEXT_LENGTH USING  G_DATE_LINE_FROM.

    MOVE  TEXT-005           TO  G_TEXT_LINE-TEXT.
    PERFORM  GET_MAX_TEXT_LENGTH USING  G_TEXT_LINE.

    MOVE  TEXT-006           TO  G_TEXT_LINE.
    PERFORM  GET_MAX_TEXT_LENGTH USING  G_TEXT_LINE.

    MOVE   TEXT-007          TO  G_DATE_LINE_TO-TEXT.
    WRITE : DATUM-HIGH       TO  G_DATE_LINE_TO-DATUM DD/MM/YYYY.
    CONDENSE                 G_DATE_LINE_TO.
  ELSE.
*     stocks, quantities, and values
    MOVE   TEXT-008          TO  G_DATE_LINE_FROM-TEXT.
    WRITE : DATUM-LOW        TO  G_DATE_LINE_FROM-DATUM DD/MM/YYYY.
    CONDENSE                 G_DATE_LINE_FROM.

*     the start and end dates were shown incorrectly in the "n856424
*     headlines in the mode valuated stock                  "n856424
    PERFORM  GET_MAX_TEXT_LENGTH USING  G_DATE_LINE_FROM.   "n856424

    MOVE  TEXT-030           TO  G_TEXT_LINE-TEXT.
    PERFORM  GET_MAX_TEXT_LENGTH USING  G_TEXT_LINE.

    MOVE  TEXT-031           TO  G_TEXT_LINE-TEXT.
    PERFORM  GET_MAX_TEXT_LENGTH USING  G_TEXT_LINE.

    MOVE   TEXT-008          TO  G_DATE_LINE_TO-TEXT.
    WRITE : DATUM-HIGH       TO  G_DATE_LINE_TO-DATUM DD/MM/YYYY.
    CONDENSE                 G_DATE_LINE_TO.
  ENDIF.

* calculate the offsets for the following columns
  G_OFFSET_QTY               =  G_F_LENGTH_MAX +  2.
  G_OFFSET_UNIT              =  G_OFFSET_QTY   + 25.
  G_OFFSET_VALUE             =  G_OFFSET_UNIT  +  8.
  G_OFFSET_CURR              =  G_OFFSET_VALUE + 25.

*ENHANCEMENT-POINT EHP605_RM07MLBD_19 SPOTS ES_RM07MLBD .

ENDFORM.                     " calculate_offsets.

*----------------------------------------------------------------------*
*    get_max_text_length
*----------------------------------------------------------------------*

FORM GET_MAX_TEXT_LENGTH         USING L_TEXT TYPE ANY.

  G_F_LENGTH = STRLEN( L_TEXT ).

  IF  G_F_LENGTH > G_F_LENGTH_MAX.
    MOVE  G_F_LENGTH         TO  G_F_LENGTH_MAX.
  ENDIF.

ENDFORM.                     " get_max_text_length

*----------------------------------------------------------------------*

* contains FORM routines without preprocessor commands and  "n547170
* no text elements                                          "n547170
INCLUDE                      RM07MLBD_FORM_01.              "n547170

INCLUDE                      RM07MLBD_FORM_02.              "n547170

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_AISTR.          "n1481757
                                                            "n1481757
* look and show the active archive info structures F4 help      "n1481757
                                                            "n1481757
  MOVE   'X'                 TO  G_F_F4_MODE.               "n1481757
  EXPORT  G_F_F4_MODE        TO  MEMORY ID 'MB51_F4_MODE'.  "n1481757
                                                            "n1481757
* start this report in F4 mode without any parameters           "n1481757
  SUBMIT ('RM07DOCS')         AND RETURN.     "#EC CI_SUBMIT  "n1511550
                                                            "n1481757
* get the selected archive info structure                       "n1481757
  IMPORT  G_F_F4_ARCHINDEX   FROM  MEMORY                   "n1481757
                             ID 'MB51_F4_ARCHINDEX'.        "n1481757
  MOVE    G_F_F4_ARCHINDEX   TO  PA_AISTR.                  "n1481757
                                                            "n1481757
  CLEAR                      G_F_F4_MODE.                   "n1481757
  EXPORT  G_F_F4_MODE        TO  MEMORY ID 'MB51_F4_MODE'.  "n1481757
                                                            "n1481757
* save archive info structure for the next run                  "n1481757
  IF  ARCHIVE   =  'X'.                                     "n1481757
    IF  SY-BATCH IS INITIAL.  " only in dialog mode             "n1481757
                                                            "n1481757
      IF NOT OREF_SETTINGS IS INITIAL.                      "n1481757
*       this object is already known -> Save the settings       "n1481757
        CALL METHOD                                         "n1481757
          OREF_SETTINGS->SET( I_ELEMENT = 'PA_AISTR'        "n1481757
                              I_ACTIVE  =  PA_AISTR  ).     "n1481757
                                                            "n1481757
        CALL METHOD OREF_SETTINGS->FLUSH.                       "n1481757
                                                            "n1481757
*       carry out the database updates only; the normal         "n1481757
*       commit command does not allow to record this            "n1481757
*       transaction for a batch input session using             "n1481757
*       transaction SHDB                                        "n1481757
        CALL FUNCTION 'DB_COMMIT'.                          "n1481757
      ENDIF.                                                "n1481757
    ENDIF.                                                  "n1481757
  ENDIF.                                                    "n1481757

*----- end of note 1481757 ----  F4-Help ----- get info-structure -----*


************************ HAUPTPROGRAMM *********************************

*---------------- F4-Hilfe für Reportvariante -------------------------*
