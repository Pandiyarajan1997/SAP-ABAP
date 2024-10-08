*&---------------------------------------------------------------------*
*& Report  ZSALES_MONTHWISE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSTO_SUPPLY_BASE.
TYPES : BEGIN OF MS_MSEG,
        BUDAT_MKPF TYPE  MSEG-BUDAT_MKPF,
        MBLNR TYPE  MSEG-MBLNR,
        SMBLN TYPE  MSEG-SMBLN,
        SHKZG TYPE  MSEG-SHKZG,
        BWART TYPE  MSEG-BWART,
        WERKS TYPE  MSEG-WERKS,
        DMBTR TYPE MSEG-DMBTR,
        UMWRK TYPE  MSEG-UMWRK,
        END OF MS_MSEG.

DATA:MT_MSEG TYPE TABLE OF MS_MSEG,
     WA_MSEG TYPE MS_MSEG.

TYPES : BEGIN OF TS_MSEG,
        BUDAT_MKPF TYPE  MSEG-BUDAT_MKPF,
        MBLNR TYPE  MSEG-MBLNR,
        SMBLN TYPE  MSEG-SMBLN,
        SHKZG TYPE  MSEG-SHKZG,
        BWART TYPE  MSEG-BWART,
        WERKS TYPE  MSEG-WERKS,
        UMWRK TYPE  MSEG-UMWRK,
        END OF TS_MSEG.

DATA:TT_MSEG TYPE TABLE OF TS_MSEG,
     WAT_MSEG TYPE TS_MSEG.


TYPES : BEGIN OF TCAL_MSEG,
        MBLNR TYPE  MSEG-MBLNR,
        DMBTR TYPE MSEG-DMBTR,
        END OF TCAL_MSEG.

DATA: TCALT_MSEG TYPE TABLE OF TCAL_MSEG,
      WATCAL_MSEG TYPE TCAL_MSEG.



TYPES : BEGIN OF CAN_MSEG,
        SMBLN TYPE  MSEG-SMBLN,
        DMBTR TYPE MSEG-DMBTR,
        END OF CAN_MSEG.

DATA:TCAN_MSEG TYPE TABLE OF CAN_MSEG,
     WATCAN_MSEG TYPE CAN_MSEG.

TYPES : BEGIN OF ES_J_1IMOCOMP,
        WERKS TYPE  J_1IMOCOMP-WERKS,
        J_1ILSTNO TYPE  J_1IMOCOMP-J_1ILSTNO,
        END OF ES_J_1IMOCOMP.

DATA:MT_J_1I TYPE TABLE OF ES_J_1IMOCOMP,
     WA_J_1I TYPE ES_J_1IMOCOMP.


TYPES : BEGIN OF ES_T001W,
        WERKS TYPE  T001W-WERKS,
        NAME1 TYPE  T001W-NAME1,
        END OF ES_T001W.

DATA:ET_T001W TYPE TABLE OF ES_T001W,
     WA_T001W TYPE ES_T001W.

TYPES : BEGIN OF FS_FINAL,
        WERKS TYPE MSEG-WERKS,
        SNAME1 TYPE T001W-NAME1,
        J_1ILSTNO TYPE J_1IMOCOMP-J_1ILSTNO,
        UMWRK TYPE MSEG-UMWRK,
        RNAME1 TYPE T001W-NAME1,
        BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
        MBLNR TYPE MSEG-MBLNR,
        DMBTR TYPE MSEG-DMBTR,
        END OF FS_FINAL.

DATA : FT_FINAL TYPE TABLE OF FS_FINAL,
       WA_FINAL TYPE FS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA :  GT_LIST     TYPE VRM_VALUES,
      GWA_LIST    TYPE VRM_VALUE,
      GT_VALUES   TYPE TABLE OF DYNPREAD,                     " INTERNAL TABLE FOR LIST BOX
      GWA_VALUES  TYPE DYNPREAD,                              " WORK AREA FOR LIST BOX
      GV_SELECTED_VALUE(10) TYPE C.

DATA : OR_WERKS TYPE T001W-WERKS,
       OR_UMWRK TYPE T001W-WERKS,
       OR_BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
       OR_MBLNR TYPE MSEG-MBLNR.

DATA : L_WERKS TYPE T001W-WERKS,
       L_UMWRK TYPE T001W-WERKS,
       L_BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
       L_MBLNR TYPE MSEG-MBLNR.

SELECTION-SCREEN: BEGIN OF BLOCK B1.

PARAMETERS: PS_PARM AS LISTBOX VISIBLE LENGTH 50 USER-COMMAND ABC MODIF ID TB1.            " SELECTION SCREEN PARAMETER FOR INVOICE AND CUSTOMER BALANCES

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN : BEGIN OF BLOCK S1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : SO_WERKS FOR OR_WERKS  OBLIGATORY,
                 SO_UMWRK FOR OR_UMWRK,
                 SO_BUDAT FOR OR_BUDAT_MKPF ,
                 SO_MBLNR  FOR OR_MBLNR.
SELECTION-SCREEN : END OF BLOCK S1.

AT SELECTION-SCREEN .
  IF SO_WERKS IS NOT INITIAL .
    SELECT SINGLE WERKS FROM T001W INTO L_WERKS WHERE WERKS IN SO_WERKS .
    IF SY-SUBRC <> 0.
      MESSAGE 'Enter Valid Issue Plant' TYPE 'E'.
*    MESSAGE E001(YMSG) .
    ENDIF.
  ENDIF.

IF SO_UMWRK IS NOT INITIAL .
    SELECT SINGLE WERKS FROM T001W INTO L_UMWRK WHERE WERKS IN SO_UMWRK .
    IF SY-SUBRC <> 0.
      MESSAGE 'Enter Valid Receive Plant' TYPE 'E'.
*    MESSAGE E001(YMSG) .
    ENDIF.
  ENDIF.

  IF SO_BUDAT IS NOT INITIAL .
    SELECT SINGLE BUDAT_MKPF FROM MSEG INTO L_BUDAT_MKPF WHERE BUDAT_MKPF IN SO_BUDAT.
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Date.' TYPE 'E' .
    ENDIF.
  ENDIF.

  IF SO_MBLNR IS NOT INITIAL .
    SELECT SINGLE MBLNR FROM MSEG INTO L_MBLNR WHERE MBLNR IN SO_MBLNR .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Material Doc No.' TYPE 'E' .
    ENDIF.
  ENDIF.




INITIALIZATION.

  GWA_LIST-KEY = '1'.
  GWA_LIST-TEXT = 'STOCK TRANSFER REPORT '.
  APPEND GWA_LIST TO GT_LIST.
  CLEAR: GWA_LIST.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID              = 'PS_PARM'
        VALUES          = GT_LIST
      EXCEPTIONS
        ID_ILLEGAL_NAME = 1
        OTHERS          = 2.


  IF PS_PARM IS INITIAL.                                                 " TO SET SET THE INITIAL VALUE SHOWN IN LIST BOX AS 'INVOICE'

    PS_PARM = '1'.

  ENDIF.


  LOOP AT SCREEN.


    IF SCREEN-NAME = 'PS_PARM'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

 START-OF-SELECTION.
 PERFORM GET_DATA.
 PERFORM FIELD_CATLOG .
 DATA : GT_SORT TYPE SLIS_T_SORTINFO_ALV,
        WA_SORT TYPE SLIS_SORTINFO_ALV.

 END-OF-SELECTION.
 PERFORM ALV_DISPLAY.



FORM GET_DATA.

SELECT
  BUDAT_MKPF
  MBLNR
  SMBLN
  SHKZG
  BWART
  WERKS
  DMBTR
  UMWRK
FROM
  MSEG INTO TABLE  MT_MSEG WHERE  BWART  IN ('351' ,'352') AND WERKS IN SO_WERKS AND UMWRK IN SO_UMWRK AND  BUDAT_MKPF IN SO_BUDAT AND MBLNR IN SO_MBLNR.

SELECT
  BUDAT_MKPF
  MBLNR
  SMBLN
  SHKZG
  BWART
  WERKS
  UMWRK
FROM
  MSEG INTO TABLE  TT_MSEG WHERE  BWART = '351' AND SHKZG ='H'  AND WERKS IN SO_WERKS AND UMWRK IN SO_UMWRK AND  BUDAT_MKPF IN SO_BUDAT AND MBLNR IN SO_MBLNR.

SELECT
  WERKS
  NAME1
FROM
  T001W INTO TABLE ET_T001W.
SELECT
  WERKS
  J_1ILSTNO
FROM
  J_1IMOCOMP INTO TABLE MT_J_1I.

LOOP AT MT_MSEG INTO WA_MSEG WHERE BWART = '351' AND SHKZG = 'H' AND WERKS IN SO_WERKS AND UMWRK IN SO_UMWRK .
        IF SY-SUBRC = 0.
        WATCAL_MSEG-MBLNR = WA_MSEG-MBLNR.
        LOOP AT MT_MSEG INTO WA_MSEG WHERE MBLNR = WATCAL_MSEG-MBLNR AND BWART = '351' AND SHKZG = 'H' .
          IF SY-SUBRC = 0.
           WATCAL_MSEG-DMBTR = WATCAL_MSEG-DMBTR + WA_MSEG-DMBTR.
           ENDIF.
        ENDLOOP.
        ENDIF.
        APPEND WATCAL_MSEG TO TCALT_MSEG.
        CLEAR WA_MSEG.
        CLEAR WATCAL_MSEG.
ENDLOOP.
DELETE TCALT_MSEG WHERE DMBTR = 0.
DELETE ADJACENT DUPLICATES FROM TCALT_MSEG COMPARING MBLNR .


 LOOP AT MT_MSEG INTO WA_MSEG WHERE BWART = '352' AND SHKZG = 'S' .
        IF SY-SUBRC = 0.
        WATCAN_MSEG-SMBLN =  WA_MSEG-SMBLN.
        LOOP AT MT_MSEG INTO WA_MSEG WHERE SMBLN =  WATCAN_MSEG-SMBLN  AND BWART = '352' AND SHKZG = 'S' .
         IF SY-SUBRC = 0.
          WATCAN_MSEG-DMBTR = WATCAN_MSEG-DMBTR + WA_MSEG-DMBTR.
         ENDIF.
        ENDLOOP.
        ENDIF.
        APPEND WATCAN_MSEG TO TCAN_MSEG.
        CLEAR WA_MSEG.
        CLEAR WATCAN_MSEG.      " Work area clear 19/09/2014
  ENDLOOP.
  DELETE TCAN_MSEG WHERE DMBTR = 0.
  DELETE ADJACENT DUPLICATES FROM TCAN_MSEG COMPARING SMBLN .


SORT TT_MSEG BY MBLNR.
DELETE ADJACENT DUPLICATES FROM TT_MSEG COMPARING MBLNR.
LOOP AT TT_MSEG INTO WAT_MSEG.
         MOVE-CORRESPONDING WAT_MSEG TO WA_FINAL.
     READ TABLE ET_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS.
            WA_FINAL-SNAME1 = WA_T001W-NAME1.
     READ TABLE MT_J_1I INTO WA_J_1I WITH KEY WERKS = WA_FINAL-WERKS.
            WA_FINAL-J_1ILSTNO = WA_J_1I-J_1ILSTNO.
     READ TABLE ET_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-UMWRK.
            WA_FINAL-RNAME1 = WA_T001W-NAME1.
     READ TABLE TCALT_MSEG INTO WATCAL_MSEG WITH KEY MBLNR = WA_FINAL-MBLNR.
           WA_FINAL-DMBTR = WATCAL_MSEG-DMBTR.
     READ TABLE TCAN_MSEG INTO WATCAN_MSEG WITH KEY SMBLN = WA_FINAL-MBLNR.
            IF WATCAN_MSEG-SMBLN = WA_FINAL-MBLNR.
            WA_FINAL-DMBTR = WA_FINAL-DMBTR - WATCAN_MSEG-DMBTR.
            ENDIF.
     APPEND WA_FINAL TO FT_FINAL.
       CLEAR WA_FINAL.
ENDLOOP.
SORT FT_FINAL BY MBLNR.
DELETE ADJACENT DUPLICATES FROM FT_FINAL COMPARING MBLNR.


ENDFORM.

FORM FIELD_CATLOG .
  PERFORM ALV_LAYOUT USING 1 'Supplying Plant' 'WERKS' 'FT_FINAL' '' '15' '' ''.
  PERFORM ALV_LAYOUT USING 2 'Supplying Plant Name' 'SNAME1' 'FT_FINAL' '' '30' '' ''.
  PERFORM ALV_LAYOUT USING 3 'Supplying Plant TIN NO' 'J_1ILSTNO' 'FT_FINAL' '' '20' '' ''.
  PERFORM ALV_LAYOUT USING 4 'Receiving Plant' 'UMWRK' 'FT_FINAL' '' '15' '' ''.
  PERFORM ALV_LAYOUT USING 5 'Receiving Plant Name' 'RNAME1' 'FT_FINAL' '' '30' '' ''.
  PERFORM ALV_LAYOUT USING 6 'Posting Date' 'BUDAT_MKPF' 'FT_FINAL' '' '15' '' ''.
  PERFORM ALV_LAYOUT USING 7 'Document NO' 'MBLNR' 'FT_FINAL' '' '15' '' ''.
  PERFORM ALV_LAYOUT USING 8 'Amount' 'DMBTR' 'FT_FINAL' '' '20' '' ''.
 ENDFORM.

FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5 P6 P7 P8.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-OUTPUTLEN = P6.
  WA_FCAT-KEY = P7.
  WA_FCAT-NO_OUT = P8.
  APPEND WA_FCAT TO GT_FCAT.
ENDFORM.

FORM ALV_CATALOG_HEADER.

  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.
  DATA :LV_BEDAT(50) TYPE C,
   LV_WERKS(100) TYPE C,
    LV_UMWRK(100) TYPE C.
* LV_LIFNR(100) TYPE C,

*         LV_BEDAT(50) TYPE C.
*         LV_BEDAT1 TYPE SY-DATUM.
*
  CLEAR : LV_WERKS.
  CLEAR : LV_UMWRK.
*  IF SO_LIFNR-HIGH IS NOT INITIAL.
*    CONCATENATE 'Vendor No :' SO_LIFNR-LOW 'To' SO_LIFNR-HIGH INTO LV_LIFNR SEPARATED BY SPACE.
*  ELSE.
*    CONCATENATE 'Vendor No :' SO_LIFNR-LOW INTO LV_LIFNR SEPARATED BY SPACE.
*  ENDIF.
*
  IF SO_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Supplying Plant Code :' SO_WERKS-LOW 'To' SO_WERKS-HIGH INTO LV_WERKS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Supplying Plant Code :' SO_WERKS-LOW INTO LV_WERKS SEPARATED BY SPACE.
  ENDIF.
   IF SO_UMWRK-HIGH IS NOT INITIAL.
    CONCATENATE 'Receiving Plant Code :' SO_UMWRK-LOW 'To' SO_UMWRK-HIGH INTO LV_UMWRK SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Receiving Plant Code :' SO_UMWRK-LOW INTO LV_UMWRK SEPARATED BY SPACE.
  ENDIF.
*
*
*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY = ' '.
*  LS_LINE-INFO = LV_LIFNR.
*  APPEND LS_LINE TO LIT_HEADER.
*
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_WERKS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_UMWRK.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Stock Transfer Report' .
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_BEDAT.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      INPUT  = LV_BEDAT
    IMPORTING
      OUTPUT = LV_BEDAT.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'

  EXPORTING
    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.


ENDFORM.

FORM ALV_DISPLAY .

LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
           LAYOUT-ZEBRA = 'X'.

DELETE FT_FINAL WHERE DMBTR = 0.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
    I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
    I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ''
   IS_LAYOUT                         = LAYOUT
    IT_FIELDCAT                      = GT_FCAT[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
    IT_SORT                           = GT_SORT[]
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
    I_DEFAULT                         = 'X'
    I_SAVE                            = 'A'
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
*   IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = FT_FINAL[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
