*&---------------------------------------------------------------------*
*& Report  ZSIV_BDC_Y005_CUST1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSIV_BDC_Y005_CUST1
NO STANDARD PAGE HEADING LINE-SIZE 255.
TYPES TRUXS_T_TEXT_DATA(4096) TYPE C OCCURS 0.

TYPE-POOLS : SLIS .

TABLES: T100.
TYPE-POOLS: TRUXS.

TYPES: BEGIN OF TY_VK11,
  KSCHL TYPE RV13A-KSCHL,
   VKORG  TYPE KOMG-VKORG,
    VTWEG  TYPE  KOMG-VTWEG,
        KUNNR TYPE  KOMG-KUNNR,
           MATNR   TYPE  KOMG-MATNR,
      KONWA TYPE  KONP-KONWA,
           KMEIN      TYPE KONP-KMEIN,
* DATAB TYPE RV13A-DATAB,
*DATBI TYPE  RV13A-DATBI,
  DATAB(10) TYPE C,
DATBI(10) TYPE  C,
*  DATAB3(10) TYPE C,
*DATBI4(10) TYPE  C,
* DATAB1 TYPE RV13A-DATAB,
* DATBI1 TYPE RV13A-DATBI,

  KSTBM(15) TYPE C,
        KSTBM1(15) TYPE C,
        KSTBM2(15) TYPE C,
        KSTBM3(15) TYPE C,
        KSTBM4(15) TYPE C,
* KSTBM  TYPE KONM-KSTBM,
*  KSTBM1 TYPE    KONM-KSTBM,
*KSTBM2 TYPE  KONM-KSTBM,
*KSTBM3 TYPE  KONM-KSTBM,
* KSTBM4 TYPE KONM-KSTBM,
KONMS  TYPE  RV13A-KONMS,
   KBETR(11) TYPE C,
          KBETR1(11) TYPE C,
          KBETR2(11) TYPE C,
          KBETR3(11) TYPE C,
          KBETR4(11) TYPE C,
* KBETR TYPE KONM-KBETR,
* KBETR1 TYPE KONM-KBETR,
* KBETR2 TYPE KONM-KBETR,
* KBETR3 TYPE KONM-KBETR,
* KBETR4 TYPE KONM-KBETR,
*  DATAB1(10) TYPE C,
*DATBI2(10) TYPE  C,
*DATAB5 TYPE  RV13A-DATAB,
* DATBI2 TYPE RV13A-DATBI,
  END OF TY_VK11.



DATA: ITAB TYPE  TABLE OF TY_VK11,
      WA_ITAB TYPE  TY_VK11.

DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

*DATA : IT_BDCDATA TYPE TABLE OF BDCDATA,
*       WA_BDCDATA TYPE BDCDATA.
DATA:   IT_BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

DATA: I_BDCMSG  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE ,
        EXP_CMNMSG_05 TYPE C ,
         C_DISMODE   TYPE CHAR1 VALUE 'N'.

DATA : RAW TYPE TABLE OF ALSMEX_TABLINE ,
       WRAW TYPE  ALSMEX_TABLINE ,
       WRA TYPE  ALSMEX_TABLINE .

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:   WA_MESSAGE LIKE BDCMSGCOLL.

TYPES : BEGIN OF TY_FINAL,
        STATUS(10) TYPE C,
        MESSAGE(50) TYPE C,
        FIELD(132)  TYPE C,
        END OF TY_FINAL.


DATA : IT_FINAL TYPE TABLE OF TY_FINAL,
       WA_FINAL TYPE TY_FINAL.


DATA : FCAT TYPE SLIS_T_FIELDCAT_ALV,
       W_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA: S_LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_FILE TYPE RLGRAP-FILENAME.

*PARAMETERS: P_file LIKE RLGRAP-FILENAME OBLIGATORY.
  PARAMETERS P_FILE TYPE IBIPPARMS-PATH OBLIGATORY.
  PARAMETERS: C_MODE   LIKE CTU_PARAMS-DISMODE DEFAULT 'S'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.


*BREAK-POINT .

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'P_FILE'
    IMPORTING
      FILE_NAME     = P_FILE.

*  T_FILE = P_FILE.


START-OF-SELECTION.


LV_FILE = P_FILE.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*    I_FIELD_SEPERATOR          = 'x'
    I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             =  IT_RAW
      I_FILENAME                 = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA       =  ITAB[]  "#EC CI_FLDEXT_OK[2215424]    "Added by SPLABAP during code remediation
 EXCEPTIONS
   CONVERSION_FAILED          = 1
  OTHERS                     = 2
            .

  IF SY-SUBRC <> 0.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*include bdcrecx1.


*  PERFORM OPEN_GROUP.
  LOOP AT ITAB INTO WA_ITAB.


    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV13A-KSCHL'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RV13A-KSCHL'
                                    WA_ITAB-KSCHL .      "'Y005'.
    PERFORM BDC_DYNPRO      USING 'SAPLV14A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV130-SELKZ(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=WEIT'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1005'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONP-KBETR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=PSTF'.
    PERFORM BDC_FIELD       USING 'KOMG-VKORG'
                                  WA_ITAB-VKORG ."'1000'.
    PERFORM BDC_FIELD       USING 'KOMG-VTWEG'
                                 WA_ITAB-VTWEG. " '20'.
    PERFORM BDC_FIELD       USING 'KOMG-KUNNR'
                                 WA_ITAB-KUNNR ." '10000059'.
    PERFORM BDC_FIELD       USING 'KOMG-MATNR(01)'
                                 WA_ITAB-MATNR. " 'NCTHT022A1'.
    PERFORM BDC_FIELD       USING 'KONP-KONWA(01)'
                                 WA_ITAB-KONWA. "'INR'.
    PERFORM BDC_FIELD       USING 'KONP-KMEIN(01)'
                                 WA_ITAB-KMEIN." 'L'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB(01)'
                                 WA_ITAB-DATAB." '01.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI(01)'
                                WA_ITAB-DATBI. "  '30.07.2015'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0303'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONM-KBETR(05)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB'
                                WA_ITAB-DATAB."  '01.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI'
                                  WA_ITAB-DATBI." '30.07.2015'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(01)'
                                 WA_ITAB-KSTBM." '                 10'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(02)'
                                WA_ITAB-KSTBM1."  '                 20'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(03)'
                                 WA_ITAB-KSTBM2. " '                 30'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(04)'
                                WA_ITAB-KSTBM3."  '                 40'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(05)'
                                WA_ITAB-KSTBM4."  '                 50'.
    PERFORM BDC_FIELD       USING 'RV13A-KONMS(01)'
                                WA_ITAB-KONMS."  'L'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(01)'
                                WA_ITAB-KBETR."  '               6'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(02)'
                                WA_ITAB-KBETR1."  '               7'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(03)'
                                WA_ITAB-KBETR2."  '               8'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(04)'
                                WA_ITAB-KBETR3."  '               9'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(05)'
                                 WA_ITAB-KBETR4." '              10'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0303'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONM-KSTBM(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB'
                                WA_ITAB-DATAB." '01.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI'
                                 WA_ITAB-DATBI."  '30.07.2015'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1005'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KOMG-MATNR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=SICH'.
*  PERFORM BDC_TRANSACTION USING 'VK11'.

*    CALL TRANSACTION 'vk11' USING IT_BDCDATA MODE 'A' UPDATE 'S'.
CALL TRANSACTION 'VK11' USING IT_BDCDATA
                            MODE C_MODE
                            UPDATE 's' MESSAGES INTO I_BDCMSG.
    REFRESH IT_BDCDATA.

*  READ TABLE MESSTAB INTO WA_MESSAGE WITH KEY MSGTYP = 'E'.
 READ TABLE BDCMSGCOLL INTO WA_MESSAGE WITH KEY MSGTYP = 'E'.


    IF SY-SUBRC = 0.

    WA_FINAL-STATUS = 'Error'.
    WA_FINAL-FIELD  = MESSTAB-FLDNAME.
    CONCATENATE WA_ITAB-KSCHL 'Ended with Error' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

    ELSE.

    WA_FINAL-STATUS = 'Success'.

    CONCATENATE WA_ITAB-KSCHL 'Saved Successfully' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

    ENDIF.


    APPEND WA_FINAL TO IT_FINAL.
    CLEAR : WA_FINAL , MESSTAB.
    REFRESH : MESSTAB[].


  ENDLOOP.



 CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'STATUS' .
  W_FCAT-COL_POS = 1 .
  W_FCAT-SELTEXT_L = 'Status' .
  W_FCAT-OUTPUTLEN = 10 .
  W_FCAT-TABNAME = 'IT_FINAL'.
  APPEND W_FCAT TO FCAT.

  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'MESSAGE' .
  W_FCAT-COL_POS = 2 .
  W_FCAT-SELTEXT_L = 'Message' .
  W_FCAT-TABNAME = 'IT_FINAL'.
W_FCAT-OUTPUTLEN = 50 .
  APPEND W_FCAT TO FCAT.

  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'FIELD' .
  W_FCAT-COL_POS = 3 .
  W_FCAT-SELTEXT_L = 'Field Name' .
  W_FCAT-TABNAME = 'IT_FINAL'.
W_FCAT-OUTPUTLEN = 50 .
  APPEND W_FCAT TO FCAT.
    S_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

 CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
    IS_LAYOUT                         = S_LAYOUT
    IT_FIELDCAT                       = FCAT
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
      T_OUTTAB                          = IT_FINAL
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.







FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = PROGRAM.
  IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND  IT_BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = FNAM.
  IT_BDCDATA-FVAL = FVAL.
  APPEND  IT_BDCDATA.

ENDFORM.                    "BDC_FIELD


*&---------------------------------------------------------------------*
*&      Form  UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM UPLOAD .
*CALL FUNCTION 'DOWNLOAD'
* EXPORTING
**   BIN_FILESIZE                  = ' '
**   CODEPAGE                      = ' '
*   FILENAME                      = 'C:\Users\user\Desktop\Error\Dataerror.txt '
**   FILETYPE                      = ' '
**   ITEM                          = ' '
**   MODE                          = ' '
**   WK1_N_FORMAT                  = ' '
**   WK1_N_SIZE                    = ' '
**   WK1_T_FORMAT                  = ' '
**   WK1_T_SIZE                    = ' '
**   FILEMASK_MASK                 = ' '
**   FILEMASK_TEXT                 = ' '
**   FILETYPE_NO_CHANGE            = ' '
**   FILEMASK_ALL                  = ' '
**   FILETYPE_NO_SHOW              = ' '
**   SILENT                        = 'S'
**   COL_SELECT                    = ' '
**   COL_SELECTMASK                = ' '
**   NO_AUTH_CHECK                 = ' '
** IMPORTING
**   ACT_FILENAME                  =
**   ACT_FILETYPE                  =
**   FILESIZE                      =
**   CANCEL                        =
*  TABLES
*    DATA_TAB                      = I_ERR
**   FIELDNAMES                    =
** EXCEPTIONS
**   INVALID_FILESIZE              = 1
**   INVALID_TABLE_WIDTH           = 2
**   INVALID_TYPE                  = 3
**   NO_BATCH                      = 4
**   UNKNOWN_ERROR                 = 5
**   GUI_REFUSE_FILETRANSFER       = 6
**   OTHERS                        = 7
*          .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
*ENDFORM.
