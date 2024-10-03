report ZCWBQM_PGM_NEW
       no standard page heading line-size 255.

"include bdcrecx1.

"start-of-selection.

TYPES TRUXS_T_TEXT_DATA(4096) TYPE C OCCURS 0.

TABLES: T100.
TYPE-POOLS: TRUXS.
TYPES: BEGIN OF TY_QP02,
           NOS(5) TYPE C,
           STTAG(10) TYPE C ,
           MATNR TYPE MARA-MATNR ,
           WERKS TYPE VBRP-WERKS ,
           PLNAL(2) TYPE C,
       "    ENTRY_ACT(3) TYPE C,
           VERWMERKM(20) TYPE C,
          " MKVERSION(2) TYPE C,
           "  STELLEN(30) TYPE C,
           SOLLWERT(4) TYPE C,
           TOLERANZUN(3) TYPE C,
           TOLERANZOB(3) TYPE C,
           PROBEMGEH(30) TYPE C,
           PRUEFEINH(30) TYPE C,
           STICHPRVER(10) TYPE C,


          " QUANTITAT(1) TYPE C,
           "   KURZTEXT(200) TYPE C,

        "   PLAUSIUNTE(4) TYPE C,
         "  PLAUSIOBEN(10) TYPE C,

             AUSWMENGE(20) TYPE C,
          " AUSWMGWRK1(4) TYPE C,

  END OF TY_QP02 .


    DATA: IT_TAB TYPE  TABLE OF TY_QP02,
        WA_ITAB TYPE  TY_QP02.

  DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

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

DATA : LV_EN TYPE C .

DATA : LV_NUM TYPE I .

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
      I_TAB_CONVERTED_DATA       =  IT_TAB[]"#EC CI_FLDEXT_OK[2215424] "Added by SPLABAP during code remediation
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


 LOOP AT IT_TAB INTO WA_ITAB.

IF WA_ITAB-NOS = '1' .
"perform open_group.


perform bdc_dynpro      using 'SAPLCPSC' '1100'.
perform bdc_field       using 'BDC_CURSOR'
                              'CWB_WORKAREA-WORK_AREA'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENT1'.
perform bdc_field       using 'CWB_WORKAREA-WORK_AREA'
                              'Q_TSK_000000000010'.
perform bdc_dynpro      using 'SAPLCPSC' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=CHAO'.
perform bdc_field       using 'CWB_VALIDITY-DATUV'
                              WA_ITAB-STTAG . "'23.09.2016'.
perform bdc_field       using 'CWB_VALIDITY-DATUB'
                              '31.12.9999'.
perform bdc_field       using 'CWB_EFFECTIVITY-STTAG'
                              WA_ITAB-STTAG . "'23.09.2016'.
perform bdc_field       using 'BDC_CURSOR'
                              'TSK_CLASS_SEL-PLNTY'.
perform bdc_field       using 'MTK_CLASS_SEL-MATNR'
                               WA_ITAB-MATNR .  "'10000004'.
perform bdc_field       using 'MTK_CLASS_SEL-WERKS'
                              WA_ITAB-WERKS . "'1005'.
perform bdc_field       using 'TSK_CLASS_SEL-PLNTY'
                              'Q'.
perform bdc_dynpro      using 'SAPLCQOV' '1100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENT1'.
perform bdc_field       using 'BDC_CURSOR'
                              'CWB_CONTEXT-PLNAL'.
perform bdc_field       using 'CWB_CONTEXT-MATNR'
                              WA_ITAB-MATNR . " '10000004'.
perform bdc_field       using 'CWB_CONTEXT-WERKS'
                              WA_ITAB-WERKS . " '1005'.
perform bdc_field       using 'CWB_CONTEXT-PLNTY'
                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
perform bdc_field       using 'CWB_CONTEXT-PLNAL'
                              WA_ITAB-PLNAL . "'1'.
perform bdc_dynpro      using 'SAPLCQOV' '1100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=INSERT'.
perform bdc_field       using 'BDC_CURSOR'
                              'CHA_CLASS_VIEW-CHA_LOCK(01)'.
perform bdc_field       using 'G_FLG_MARK(01)'
                              'X'.
perform bdc_field       using 'CWB_CONTEXT-MATNR'
                              WA_ITAB-MATNR . "  '10000004'.
perform bdc_field       using 'CWB_CONTEXT-WERKS'
                               WA_ITAB-WERKS . "  '1005'.
perform bdc_field       using 'CWB_CONTEXT-PLNTY'
                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
perform bdc_field       using 'CWB_CONTEXT-PLNAL'
                                WA_ITAB-PLNAL .  " '1'.
perform bdc_field       using 'CWB_CONTEXT-PLNFL'
                              '0'.
perform bdc_field       using 'CWB_CONTEXT-IDENT'
                              WA_ITAB-PLNAL .  " '00000001'.
perform bdc_field       using 'CWB_CONTEXT-VORNR'
                              '0010'.
perform bdc_dynpro      using 'SAPLCQOV' '1100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENT1'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'CHA_CLASS_VIEW-TOLERANZOB(01)'.
perform bdc_field       using 'CHA_CLASS_DATA-VERWMERKM(01)'
                              WA_ITAB-VERWMERKM . "'FLOW_PT'.
perform bdc_field       using 'CHA_CLASS_VIEW-QPMK_WERKS(01)'
                              WA_ITAB-WERKS . "'1005'.
*perform bdc_field       using 'CHA_CLASS_DATA-MKVERSION(01)'
*                              WA_ITAB-MKVERSION .  "'2'.
perform bdc_field       using 'CHA_CLASS_DATA-STICHPRVER(01)'
                              WA_ITAB-STICHPRVER . "'FIX-01'.
perform bdc_field       using 'CHA_CLASS_VIEW-SOLLWERT(01)'
                              WA_ITAB-SOLLWERT.  "'5'.
perform bdc_field       using 'CHA_CLASS_DATA-STELLEN(01)'
                              '2'.
perform bdc_field       using 'CHA_CLASS_VIEW-MASSEINHSW(01)'
                              '%'.
perform bdc_field       using 'CHA_CLASS_VIEW-TOLERANZUN(01)'
                              WA_ITAB-TOLERANZUN .  " '1'.
perform bdc_field       using 'CHA_CLASS_VIEW-TOLERANZOB(01)'
                               WA_ITAB-TOLERANZOB . "'10'.
*perform bdc_field       using 'CHA_CLASS_DATA-QUANTITAT(01)'
*                              'X'.
*perform bdc_field       using 'CWB_CONTEXT-MATNR'
*                              '10000004'.
*perform bdc_field       using 'CWB_CONTEXT-WERKS'
*                              '1005'.
*perform bdc_field       using 'CWB_CONTEXT-PLNTY'
*                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
*perform bdc_field       using 'CWB_CONTEXT-PLNAL'
*                              '1'.
*perform bdc_field       using 'CWB_CONTEXT-PLNFL'
*                              '0'.
*perform bdc_field       using 'CWB_CONTEXT-IDENT'
*                              '00000001'.
*perform bdc_field       using 'CWB_CONTEXT-VORNR'
*                              '0010'.
*perform bdc_dynpro      using 'SAPLCQOV' '1100'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=INSERT'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'CHA_CLASS_VIEW-CHA_LOCK(01)'.
*perform bdc_field       using 'G_FLG_MARK(01)'
*                              'X'.
*perform bdc_field       using 'CWB_CONTEXT-MATNR'
*                              '10000004'.
*perform bdc_field       using 'CWB_CONTEXT-WERKS'
*                              '1005'.
*perform bdc_field       using 'CWB_CONTEXT-PLNTY'
*                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
*perform bdc_field       using 'CWB_CONTEXT-PLNAL'
*                              '1'.
*perform bdc_field       using 'CWB_CONTEXT-PLNFL'
*                              '0'.
*perform bdc_field       using 'CWB_CONTEXT-IDENT'
*                              '00000001'.
*perform bdc_field       using 'CWB_CONTEXT-VORNR'
*                              '0010'.
*perform bdc_dynpro      using 'SAPLCQOV' '1100'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=LT03'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'CHA_CLASS_LINK-LTX_LINK(03)'.
*perform bdc_field       using 'CHA_CLASS_DATA-VERWMERKM(01)'
*                              'CLARITY'.
*perform bdc_field       using 'CHA_CLASS_VIEW-QPMK_WERKS(01)'
*                              '1005'.
*perform bdc_field       using 'CHA_CLASS_DATA-MKVERSION(01)'
*                              '1'.
*perform bdc_field       using 'CHA_CLASS_DATA-STICHPRVER(01)'
*                              'FIX-01'.
*perform bdc_field       using 'CHA_CLASS_DATA-QUANTITAT(01)'
*                              'X'.
*perform bdc_field       using 'CHA_CLASS_DATA-AUSWMENGE1(01)'
*                              'APPEAR-S'.
*perform bdc_field       using 'CWB_CONTEXT-MATNR'
*                              '10000004'.
*perform bdc_field       using 'CWB_CONTEXT-WERKS'
*                              '1005'.
*perform bdc_field       using 'CWB_CONTEXT-PLNTY'
*                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
*perform bdc_field       using 'CWB_CONTEXT-PLNAL'
*                              '1'.
*perform bdc_field       using 'CWB_CONTEXT-PLNFL'
*                              '0'.
*perform bdc_field       using 'CWB_CONTEXT-IDENT'
*                              '00000001'.
*perform bdc_field       using 'CWB_CONTEXT-VORNR'
*                              '0010'.
*perform bdc_dynpro      using 'SAPLSTXX' '1100'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'RSTXT-TXLINE(02)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=TXBA'.
*perform bdc_dynpro      using 'SAPLCQOV' '1100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=SAVE'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'CWB_CONTEXT-MATNR'.
*perform bdc_field       using 'CWB_CONTEXT-MATNR'
*                              '10000004'.
*perform bdc_field       using 'CWB_CONTEXT-WERKS'
*                              '1005'.
*perform bdc_field       using 'CWB_CONTEXT-PLNTY'
*                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
*perform bdc_field       using 'CWB_CONTEXT-PLNAL'
*                              WA_ITAB-PLNAL . "'1'.
*perform bdc_field       using 'CWB_CONTEXT-PLNFL'
*                              '0'.
*perform bdc_field       using 'CWB_CONTEXT-IDENT'
*                              '00000001'.
*perform bdc_field       using 'CWB_CONTEXT-VORNR'
*                              '0010'.
"perform bdc_transaction using 'CWBQM'.

"perform close_group.



CALL TRANSACTION 'CWBQM' USING IT_BDCDATA
                            MODE C_MODE
                            UPDATE 's' MESSAGES INTO I_BDCMSG.

ENDIF.

IF WA_ITAB-NOS = 2 .

perform bdc_dynpro      using 'SAPLCPSC' '1100'.
perform bdc_field       using 'BDC_CURSOR'
                              'CWB_WORKAREA-WORK_AREA'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENT1'.
perform bdc_field       using 'CWB_WORKAREA-WORK_AREA'
                              'Q_TSK_000000000010'.
perform bdc_dynpro      using 'SAPLCPSC' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=CHAO'.
perform bdc_field       using 'CWB_VALIDITY-DATUV'
                              WA_ITAB-STTAG . "'23.09.2016'.
perform bdc_field       using 'CWB_VALIDITY-DATUB'
                              '31.12.9999'.
perform bdc_field       using 'CWB_EFFECTIVITY-STTAG'
                              WA_ITAB-STTAG . "'23.09.2016'.
perform bdc_field       using 'BDC_CURSOR'
                              'TSK_CLASS_SEL-PLNTY'.
perform bdc_field       using 'MTK_CLASS_SEL-MATNR'
                               WA_ITAB-MATNR .  "'10000004'.
perform bdc_field       using 'MTK_CLASS_SEL-WERKS'
                              WA_ITAB-WERKS . "'1005'.
perform bdc_field       using 'TSK_CLASS_SEL-PLNTY'
                              'Q'.
perform bdc_dynpro      using 'SAPLCQOV' '1100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENT1'.
perform bdc_field       using 'BDC_CURSOR'
                              'CWB_CONTEXT-PLNAL'.
perform bdc_field       using 'CWB_CONTEXT-MATNR'
                              WA_ITAB-MATNR . " '10000004'.
perform bdc_field       using 'CWB_CONTEXT-WERKS'
                              WA_ITAB-WERKS . " '1005'.
perform bdc_field       using 'CWB_CONTEXT-PLNTY'
                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
perform bdc_field       using 'CWB_CONTEXT-PLNAL'
                              WA_ITAB-PLNAL . "'1'.
perform bdc_dynpro      using 'SAPLCQOV' '1100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=INSERT'.
perform bdc_field       using 'BDC_CURSOR'
                              'CHA_CLASS_VIEW-CHA_LOCK(01)'.
perform bdc_field       using 'G_FLG_MARK(01)'
                              'X'.
perform bdc_field       using 'CWB_CONTEXT-MATNR'
                              WA_ITAB-MATNR . "  '10000004'.
perform bdc_field       using 'CWB_CONTEXT-WERKS'
                               WA_ITAB-WERKS . "  '1005'.
perform bdc_field       using 'CWB_CONTEXT-PLNTY'
                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
perform bdc_field       using 'CWB_CONTEXT-PLNAL'
                                WA_ITAB-PLNAL .  " '1'.
perform bdc_field       using 'CWB_CONTEXT-PLNFL'
                              '0'.
perform bdc_field       using 'CWB_CONTEXT-IDENT'
                               WA_ITAB-PLNAL .  " '00000001'.
perform bdc_field       using 'CWB_CONTEXT-VORNR'
                              '0010'.
perform bdc_dynpro      using 'SAPLCQOV' '1100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENT1'.


*perform bdc_field       using 'BDC_CURSOR'
*                              'CHA_CLASS_DATA-PROBEMGEH(01)'.
*perform bdc_field       using 'CHA_CLASS_DATA-VERWMERKM(01)'
*                              WA_ITAB-VERWMERKM . "'CLARITY'.
*perform bdc_field       using 'CHA_CLASS_VIEW-QPMK_WERKS(01)'
*                               WA_ITAB-WERKS . "'1005'.
*perform bdc_field       using 'CHA_CLASS_DATA-STICHPRVER(01)'
*                              WA_ITAB-STICHPRVER ."'FIX-01'.
*perform bdc_field       using 'CHA_CLASS_DATA-QUANTITAT(01)'
*                              'X'.
*perform bdc_field       using 'CHA_CLASS_DATA-AUSWMENGE1(01)'
*                              WA_ITAB-AUSWMENGE1 . "'CHPLC'.
*perform bdc_field       using 'CWB_CONTEXT-MATNR'
*                              WA_ITAB-MATNR . "'10000004'.
*perform bdc_field       using 'CWB_CONTEXT-WERKS'
*                              WA_ITAB-WERKS . "'1005'.
*perform bdc_field       using 'CWB_CONTEXT-PLNTY'
*                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
*perform bdc_field       using 'CWB_CONTEXT-PLNAL'
*                              WA_ITAB-PLNAL . "'2'.
*perform bdc_field       using 'CWB_CONTEXT-PLNFL'
*                              '0'.
*perform bdc_field       using 'CWB_CONTEXT-IDENT'
*                              WA_ITAB-PLNAL . "'00000002'.
*perform bdc_field       using 'CWB_CONTEXT-VORNR'
*                              '0010'.
*perform bdc_dynpro      using 'SAPLCQOV' '1100'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=SAVE'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'CHA_CLASS_DATA-PROBEMGEH(01)'.
*perform bdc_field       using 'CWB_CONTEXT-MATNR'
*                              WA_ITAB-MATNR . "'10000004'.
*perform bdc_field       using 'CWB_CONTEXT-WERKS'
*                              WA_ITAB-WERKS . "'1005'.
*perform bdc_field       using 'CWB_CONTEXT-PLNTY'
*                              'Q'.
*perform bdc_field       using 'CWB_CONTEXT-PLNNR'
*                              '47'.
*perform bdc_field       using 'CWB_CONTEXT-PLNAL'
*                              WA_ITAB-PLNAL . "'2'.
*perform bdc_field       using 'CWB_CONTEXT-PLNFL'
*                              '0'.
*perform bdc_field       using 'CWB_CONTEXT-IDENT'
*                              WA_ITAB-PLNAL . "'00000002'.
*perform bdc_field       using 'CWB_CONTEXT-VORNR'
*                              '0010'.



perform bdc_field       using 'BDC_CURSOR'
                              'CHA_CLASS_VIEW-TOLERANZOB(01)'.
perform bdc_field       using 'CHA_CLASS_DATA-VERWMERKM(01)'
                              WA_ITAB-VERWMERKM . "'FLOW_PT'.
perform bdc_field       using 'CHA_CLASS_VIEW-QPMK_WERKS(01)'
                              WA_ITAB-WERKS . "'1005'.
*perform bdc_field       using 'CHA_CLASS_DATA-MKVERSION(01)'
*                              WA_ITAB-MKVERSION .  "'2'.
perform bdc_field       using 'CHA_CLASS_DATA-STICHPRVER(01)'
                              WA_ITAB-STICHPRVER . "'FIX-01'.
*perform bdc_field       using 'CHA_CLASS_VIEW-SOLLWERT(01)'
*                              WA_ITAB-SOLLWERT.  "'5'.
*perform bdc_field       using 'CHA_CLASS_DATA-STELLEN(01)'
*                              '2'.
*perform bdc_field       using 'CHA_CLASS_VIEW-MASSEINHSW(01)'
*                              '%'.
*perform bdc_field       using 'CHA_CLASS_VIEW-TOLERANZUN(01)'
*                              WA_ITAB-TOLERANZUN .  " '1'.
*perform bdc_field       using 'CHA_CLASS_VIEW-TOLERANZOB(01)'
*                               WA_ITAB-TOLERANZOB . "'10'.

perform bdc_field       using 'CHA_CLASS_DATA-AUSWMENGE1(01)'
                               WA_ITAB-AUSWMENGE .

perform bdc_field       using 'BDC_OKCODE'
                              '=SAVE'.



  CALL TRANSACTION 'CWBQM' USING IT_BDCDATA
                            MODE C_MODE
                            UPDATE 's' MESSAGES INTO I_BDCMSG.

ENDIF.


REFRESH IT_BDCDATA.

READ TABLE BDCMSGCOLL INTO WA_MESSAGE WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.

    WA_FINAL-STATUS = 'Error'.
    WA_FINAL-FIELD  = MESSTAB-FLDNAME.
    CONCATENATE WA_FINAL-FIELD 'Ended with Error' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

    ELSE.

    WA_FINAL-STATUS = 'Success'.

    CONCATENATE WA_FINAL-FIELD 'Saved Successfully' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

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
*       I_INTERFACE_CHECK                 = ' '
*       I_BYPASSING_BUFFER                = ' '
*       I_BUFFER_ACTIVE                   = ' '
*       I_CALLBACK_PROGRAM                = ' '
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME                  =
*       I_BACKGROUND_ID                   = ' '
*       I_GRID_TITLE                      =
*       I_GRID_SETTINGS                   =
       IS_LAYOUT                         = S_LAYOUT
       IT_FIELDCAT                       = FCAT
*       IT_EXCLUDING                      =
*       IT_SPECIAL_GROUPS                 =
*       IT_SORT                           =
*       IT_FILTER                         =
*       IS_SEL_HIDE                       =
*       I_DEFAULT                         = 'X'
*       I_SAVE                            = ' '
*       IS_VARIANT                        =
*       IT_EVENTS                         =
*       IT_EVENT_EXIT                     =
*       IS_PRINT                          =
*       IS_REPREP_ID                      =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE                 = 0
*       I_HTML_HEIGHT_TOP                 = 0
*       I_HTML_HEIGHT_END                 = 0
*       IT_ALV_GRAPHICS                   =
*       IT_HYPERLINK                      =
*       IT_ADD_FIELDCAT                   =
*       IT_EXCEPT_QINFO                   =
*       IR_SALV_FULLSCREEN_ADAPTER        =
*     IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        T_OUTTAB                          = IT_FINAL
*     EXCEPTIONS
*       PROGRAM_ERROR                     = 1
*       OTHERS                            = 2
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0301   text
*      -->P_0302   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO  USING PROGRAM DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = PROGRAM.
  IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND  IT_BDCDATA.

ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0306   text
*      -->P_0307   text
*----------------------------------------------------------------------*
FORM BDC_FIELD  USING FNAM FVAL.

  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = FNAM.
  IT_BDCDATA-FVAL = FVAL.
  APPEND  IT_BDCDATA.
ENDFORM.                    " BDC_FIELD
