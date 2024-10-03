*&---------------------------------------------------------------------*
*&  Include           ZHSBC_CSV_UPDATE_TOP
*&---------------------------------------------------------------------*

***--types declaratios
TYPES: BEGIN OF TY_CSV_FILE,
       TEXT TYPE STRING,
       END OF TY_CSV_FILE.

TYPES: BEGIN OF TY_RAW,
       TEXT(3000) TYPE C,
       END OF TY_RAW.

TYPES : BEGIN OF S_TEXT,
        LINE TYPE CHAR255,
        END OF S_TEXT.

TYPES : BEGIN OF TY_FNAME,
       TEXT(300) TYPE C,
       END OF TY_FNAME.

TYPES : BEGIN OF TY_BSEG,
        BUKRS TYPE BSEG-BUKRS,
        BELNR TYPE BSEG-BELNR,
        GJAHR TYPE BSEG-GJAHR,
        KIDNO TYPE BSEG-KIDNO,
  END OF TY_BSEG.

TYPES:  BEGIN OF TY_TEXT,
      FIELD1(50)   TYPE C,
      FIELD2(50)   TYPE C,
      FIELD3(50)   TYPE C,
      FIELD4(50)   TYPE C,
      FIELD5(50)   TYPE C,
      FIELD6(50)   TYPE C,
      FIELD7(50)   TYPE C,
      FIELD8(50)   TYPE C,
      FIELD9(50)   TYPE C,
      FIELD10(50)  TYPE C,
      FIELD11(50)  TYPE C,
      FIELD12(50)  TYPE C,
      FIELD13(50)  TYPE C,
      FIELD14(50)  TYPE C,
      FIELD15(50)  TYPE C,
      FIELD16(50)  TYPE C,
      FIELD17(50)  TYPE C,
      FIELD18(50)  TYPE C,
      FIELD19(50)  TYPE C,
      FIELD20(50)  TYPE C,
      FIELD21(50)  TYPE C,
      FIELD22(50)  TYPE C,
      FIELD23(50)  TYPE C,
      FIELD24(50)  TYPE C,
      FIELD25(50)  TYPE C,
      FIELD26(50)  TYPE C,
      FIELD27(50)  TYPE C,
      FIELD28(50)  TYPE C,
      FIELD29(50)  TYPE C,
      FIELD30(50)  TYPE C,
      END OF TY_TEXT.

*      ****--Indicator/flag Declaration
DATA: G_POPUP_ANSWER.

***--Global Constant declaration
CONSTANTS: ABAP_TRUE      TYPE C VALUE 'X',
           C_CLOSE               VALUE '1',
           ABAP_FALSE     TYPE C VALUE ' '.

***Data declaration for file extention checking
DATA : GV_CSV_ERROR           TYPE C ,
       GV_FNAME_TEMP(255)     TYPE C,
       GV_PATH(255)           TYPE C,
       STR1(255)              TYPE C,
       STR2(255)              TYPE C.



***--Global internal table declaration

DATA: GIT_RAW                TYPE TABLE OF TY_RAW,
      GIT_UPLOAD             TYPE TABLE OF TY_RAW,
      GIT_SP_RET             TYPE TABLE OF S_TEXT,
      GIT_FILE_LIST          TYPE TABLE OF TY_FNAME,
      GIT_LOGFILE            TYPE TABLE OF TY_FNAME,
      GIT_RESULT             TYPE TABLE OF S_TEXT,
      GIT_BSEG               TYPE TABLE OF TY_BSEG,
      GIT_TEXT               TYPE TABLE OF TY_TEXT.

***--global work area declaration
DATA: WA_LOGFILE    TYPE TY_FNAME,
      WA_RESULT     TYPE S_TEXT,
      WA_RAW        TYPE TY_RAW,
      WA_UPLOAD     TYPE TY_RAW,
      WA_TEXT       TYPE TY_TEXT,
      GV_FILENAME   TYPE STRING,
      GV_TEMP       TYPE STRING,
      WA_HSBC TYPE ZHSBC_ITEMS,
      GIT_ACCCHG    TYPE TABLE OF ACCCHG,
      WA_ACCCHG     LIKE LINE  OF GIT_ACCCHG,
      WA_BSEG TYPE TY_BSEG,
      STR_LEN(2)    TYPE N,
      STR_LEN1(2)   TYPE N.

DATA : DIR TYPE STRING ,
           RES,
           RES1 TYPE I.


DATA : LV_PORT1 TYPE STRING,
       LV_PORT2 TYPE STRING,
       LV_PORT3 TYPE STRING,
       LV_PORT4 TYPE STRING,
       LV_PORT5 TYPE STRING.



DATA : WA_SFTP TYPE ZHSBC_SFTP,
       LV_DIR TYPE EPS2FILI-NAME ," EPSF-EPSDIRNAM,
       IT_FILE_LIST TYPE TABLE OF EPS2FILI,"EPSFILI,
       LT_FILE_TABLE     TYPE TABLE OF SDOKPATH,
       WA_FILE_LIST TYPE EPS2FILI,"EPSFILI,
       FILE_COUNT TYPE I,
       TEMP TYPE STRING,
       X.

DATA : RETCODE TYPE SY-SUBRC,
        FILENAME TYPE STRING,
        SIZE TYPE SY-TABIX,
        FILE TYPE LOCALFILE.

TYPES: BEGIN OF TY_FINAL,

  FILE_NAME TYPE EPSFILI-NAME,
  CONTENT TYPE STRING,
  XFILE  TYPE XSTRING,

END OF TY_FINAL.

DATA : IT_FINAL TYPE TABLE OF TY_FINAL,
       WA_FINAL TYPE TY_FINAL,
       IT_FILE TYPE TABLE OF SMUM_XMLTB,
       WA_FILE TYPE SMUM_XMLTB,
       FI_WA_BSEG       TYPE          BSEG,
       FI_IT_BSEG      TYPE TABLE OF BSEG,
       RET TYPE TABLE OF BAPIRET2,
       TEMP1 TYPE STRING.

DATA :  LO_DOC TYPE REF TO CL_XML_DOCUMENT.

DATA : ACTIVITY TYPE CHAR30,
       FP TYPE AUTHB-FILENAME.

TYPES: BEGIN OF  FI_TY_FINAL,
       BUKRS            TYPE BUKRS,
       BELNR            TYPE BSEG-BELNR,
       GJAHR            TYPE GJAHR,
       P_CODE(22)       TYPE C,
       REASON           TYPE ZAXIS_REV_STATUS,
       BANK_REF(25)     TYPE C,
       STATUS_CODE(20)  TYPE C,
       END OF FI_TY_FINAL.

DATA: GV_AWREF             TYPE    ACCHD-AWREF,
      GV_AWORG             TYPE    ACCHD-AWORG,
      FI_IT_FINAL     TYPE TABLE OF FI_TY_FINAL,
      FI_WA_FINAL      TYPE          FI_TY_FINAL,
      GIT_FINAL_CC  TYPE TABLE OF FI_TY_FINAL,
      WA_FINAL_CC   TYPE          FI_TY_FINAL,
      WA_BKPF       TYPE  BKPF,
      WA_BSEG_FI       TYPE  BSEG,
      BKPF_CC       TYPE  BKPF,
      GV_TEXT              TYPE    STRING,
      GIT_BSEG_FI      TYPE TABLE OF BSEG,
      GIT_BKPF      TYPE TABLE OF BKPF,
      GIT_BVOR      TYPE TABLE OF BVOR.

TABLES : BKPF,
         BSEG,
         BSAK,
         BSIS,
         BVOR.
