
REPORT Z_QAREJECTION_REPORT.

*&----------------------------------------------------------------------------*
*& Report :  Z_QAREJECTION_REPORT
*&----------------------------------------------------------------------------*
*& Title  :  Quality Rejection Report
*&----------------------------------------------------------------------------*
*& Report Author         : Jestop Jeswin Charles(Abap Consultant)
*& Functional Consultant :
*& Company               : Sphinax Info Systems (SIS)
*& Report Creation Date  : 15.10.2020
*& Transaction Code      :
*& Request Number        :
*&----------------------------------------------------------------------------*

INCLUDE: Z_QAREJECTION_REPORT_VARI_TOP,
         Z_QAREJECTION_REPORT_SS_TOP.

START-OF-SELECTION.

  PERFORM : DATA_RETRIVAL,
            LISTHEADER,
            F_CAT,
            DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_RETRIVAL .



  SELECT LIFNR
         MATNR
         WERKS
         BWART
         GRUND
         MBLNR
         ZEILE
         MJAHR
         BUDAT_MKPF
         EBELN
         EBELP
         CHARG
         MENGE
         MEINS
         TCODE2_MKPF FROM MSEG INTO TABLE IT_MSEG WHERE  WERKS IN S_WERKS AND
                                                         MATNR IN S_MATNR AND
                                                         LIFNR IN S_LIFNR AND
                                                         EBELN IN S_EBELN AND
                                                         BUDAT_MKPF IN S_BUDAT AND
                                                         BWART = '101' AND TCODE2_MKPF = 'MIGO_GR'.

  IF IT_MSEG IS NOT INITIAL.


    SELECT PRUEFLOS
           MBLNR
           MJAHR
           ZEILE FROM QAMB INTO TABLE IT_QAMB1
                 FOR ALL ENTRIES IN IT_MSEG
                 WHERE MBLNR = IT_MSEG-MBLNR AND
                       MJAHR = IT_MSEG-MJAHR AND
                       ZEILE = IT_MSEG-ZEILE.

    SELECT PRUEFLOS
         MBLNR
         MJAHR
         ZEILE FROM QAMB INTO TABLE IT_QAMB2
               FOR ALL ENTRIES IN IT_QAMB1
               WHERE PRUEFLOS = IT_QAMB1-PRUEFLOS.

      SELECT PRUEFLOS FROM QMEL INTO TABLE IT_QMEL
                      FOR ALL ENTRIES IN IT_QAMB2
                       WHERE PRUEFLOS = IT_QAMB2-PRUEFLOS AND
                             QMART = 'F2'.

    SELECT LIFNR
       MATNR
       WERKS
       BWART
       GRUND
       MBLNR
       ZEILE
       MJAHR
       BUDAT_MKPF
       EBELN
       EBELP
       CHARG
       MENGE
       MEINS
       TCODE2_MKPF FROM MSEG INTO TABLE IT_MSEG1 FOR ALL ENTRIES IN IT_QAMB2
                                                 WHERE  MBLNR = IT_QAMB2-MBLNR AND
                                                        MJAHR = IT_QAMB2-MJAHR AND
                                                        ZEILE = IT_QAMB2-ZEILE AND
                                                        BWART = '350'.

    DATA I TYPE I.

    LOOP AT  IT_QAMB2 INTO WA_QAMB2.

      READ TABLE IT_MSEG INTO WA_MSEG WITH KEY MBLNR = WA_QAMB2-MBLNR
                                               MJAHR = WA_QAMB2-MJAHR
                                               ZEILE = WA_QAMB2-ZEILE
                                               BWART = '101'.

      IF SY-SUBRC = 0.
        I = I + 1.
        WA_FINAL-PRUEFLOS = WA_QAMB2-PRUEFLOS.
        WA_FINAL-LIFNR = WA_MSEG-LIFNR.

        SHIFT wa_mseg-matnr LEFT DELETING LEADING '0'.

        WA_FINAL-MATNR = WA_MSEG-MATNR.
        WA_FINAL-WERKS = WA_MSEG-WERKS.
        WA_FINAL-BUDAT = WA_MSEG-BUDAT.
        WA_FINAL-MBLNR = WA_MSEG-MBLNR.
        WA_FINAL-MJAHR = WA_MSEG-MJAHR.
        WA_FINAL-EBELN = WA_MSEG-EBELN.

        WA_FINAL-EBELP = WA_MSEG-EBELP.

        WA_FINAL-CHARG = WA_MSEG-CHARG.

        WA_FINAL-MENGE = WA_MSEG-MENGE.

        WA_FINAL-MEINS = WA_MSEG-MEINS.


      ENDIF.

      READ TABLE IT_MSEG1 INTO WA_MSEG1 WITH KEY MBLNR = WA_QAMB2-MBLNR
                                               MJAHR = WA_QAMB2-MJAHR
                                               ZEILE = WA_QAMB2-ZEILE
                                               BWART = '350'.

      IF SY-SUBRC = 0.

        I = I + 1.
        WA_FINAL-MENGE1 = WA_MSEG1-MENGE.

        WA_FINAL-MEINS1 = WA_MSEG1-MEINS.
        WA_FINAL-BWART = WA_MSEG1-BWART.
        WA_FINAL-GRUND = WA_MSEG1-GRUND.

        READ TABLE IT_QMEL INTO WA_QMEL WITH KEY PRUEFLOS = WA_FINAL-PRUEFLOS.

        IF SY-SUBRC = 0.

           WA_FINAL-CATEG = 'Online Rejection'.

        ELSE.

          WA_FINAL-CATEG = 'GRN Rejection'.

        ENDIF.
      ENDIF.

      AT END OF PRUEFLOS. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation

        IF I = 2.

          APPEND WA_FINAL TO IT_FINAL.
          CLEAR WA_FINAL.

        ELSE.

          CLEAR I.

        ENDIF.
      ENDAT.

      CLEAR: WA_QAMB2, WA_MSEG1, WA_MSEG.

    ENDLOOP.


    IF IT_FINAL IS NOT INITIAL.


      SELECT LIFNR NAME1
             FROM LFA1 INTO TABLE IT_LFA1
             FOR ALL ENTRIES IN IT_FINAL
             WHERE LIFNR = IT_FINAL-LIFNR.


      SELECT MATNR MAKTX FROM MAKT
                         INTO TABLE IT_MAKT
                         FOR ALL ENTRIES IN IT_FINAL
                         WHERE MATNR = IT_FINAL-MATNR.


      SELECT WERKS
             NAME1 FROM T001W INTO TABLE IT_T001W
                   FOR ALL ENTRIES IN IT_FINAL
                   WHERE WERKS = IT_FINAL-WERKS.

      SELECT BWART
             GRUND
             GRTXT FROM T157E INTO TABLE IT_T157E
                   FOR ALL ENTRIES IN IT_FINAL
                   WHERE BWART = IT_FINAL-BWART
                   AND   GRUND = IT_FINAL-GRUND.

      SELECT LIFNR
             MATNR
             CHARG
             LICHA FROM MCH1 INTO TABLE IT_MCH1
                   FOR ALL ENTRIES IN IT_FINAL
                   WHERE LIFNR = IT_FINAL-LIFNR AND
                         MATNR = IT_FINAL-MATNR AND
                         CHARG = IT_FINAL-CHARG.

      SELECT MBLNR
             MJAHR
             XBLNR FROM mkpf INTO TABLE it_mkpf
                   FOR ALL ENTRIES IN it_final
                   where mblnr = it_final-mblnr and
                         mjahr = it_final-mjahr.



    LOOP AT IT_FINAL INTO WA_FINAL.

        READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_FINAL-LIFNR.

        WA_FINAL-NAME = WA_LFA1-NAME1.

        READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_FINAL-MATNR.

        WA_FINAL-MAKTX = WA_MAKT-MAKTX.

        READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS.

        WA_FINAL-NAME1 = WA_T001W-NAME1.

        READ TABLE IT_T157E INTO WA_T157E WITH KEY BWART = WA_FINAL-BWART
                                                   GRUND = WA_FINAL-GRUND.

        TRANSLATE wa_t157e-grtxt TO UPPER CASE.

        WA_FINAL-GRTXT = WA_T157E-GRTXT.

        READ TABLE IT_MCH1 INTO WA_MCH1 WITH KEY LIFNR = WA_FINAL-LIFNR
                                                 MATNR = WA_FINAL-MATNR
                                                 CHARG = WA_FINAL-CHARG.

        WA_FINAL-LICHA = WA_MCH1-LICHA.

        READ TABLE it_mkpf INTO wa_mkpf WITH key mblnr = wa_final-mblnr
                                                 mjahr = wa_final-mjahr.

        wa_final-XBLNR = wa_mkpf-XBLNR.


        MODIFY IT_FINAL FROM WA_FINAL.
        CLEAR: WA_FINAL, WA_LFA1 , WA_MAKT, WA_T001W, WA_T157E.

      ENDLOOP.

      SORT IT_FINAL BY BUDAT EBELN.

    ENDIF.

  ELSE.

    MESSAGE 'No records found' TYPE 'E'.

  ENDIF.

ENDFORM.                    "DATA_RETRIVAL
*&---------------------------------------------------------------------*
*&      Form  F_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CAT .

  DATA: SNO TYPE I.

  SNO = 1.

  WA_FCAT-COL_POS = SNO.
  WA_FCAT-FIELDNAME = 'LIFNR'.
  WA_FCAT-SELTEXT_S = 'VENDOR'.
  WA_FCAT-SELTEXT_M = 'VENDOR'.
  WA_FCAT-SELTEXT_L = 'VENDOR'.
  WA_FCAT-KEY       = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS = SNO.
  WA_FCAT-FIELDNAME = 'NAME'.
  WA_FCAT-SELTEXT_S = 'VENDOR NAME'.
  WA_FCAT-SELTEXT_M = 'VENDOR NAME'.
  WA_FCAT-SELTEXT_L = 'VENDOR NAME'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS = SNO.
  WA_FCAT-FIELDNAME = 'MATNR'.
  WA_FCAT-SELTEXT_S = 'MATERIAL'.
  WA_FCAT-SELTEXT_M = 'MATERIAL'.
  WA_FCAT-SELTEXT_L = 'MATERIAL'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS = SNO.
  WA_FCAT-FIELDNAME = 'MAKTX'.
  WA_FCAT-SELTEXT_S = 'MATERIAL DES'.
  WA_FCAT-SELTEXT_M = 'MATERIAL DESCRIPTION'.
  WA_FCAT-SELTEXT_L = 'MATERIAL DESCRIPTION'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'WERKS'.
  WA_FCAT-SELTEXT_S = 'PLANT'.
  WA_FCAT-SELTEXT_M = 'PLANT'.
  WA_FCAT-SELTEXT_L = 'PLANT'.


  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'NAME1'.
  WA_FCAT-SELTEXT_S = 'NAME'.
  WA_FCAT-SELTEXT_M = 'NAME'.
  WA_FCAT-SELTEXT_L = 'NAME'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'MBLNR'.
  WA_FCAT-SELTEXT_S = 'MIGO NO'.
  WA_FCAT-SELTEXT_M = 'MIGO NO'.
  WA_FCAT-SELTEXT_L = 'MIGO NO'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'BUDAT'.
  WA_FCAT-SELTEXT_S = 'POSTING DATE'.
  WA_FCAT-SELTEXT_M = 'POSTING DATE'.
  WA_FCAT-SELTEXT_L = 'POSTING DATE'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.


  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'EBELN'.
  WA_FCAT-SELTEXT_S = 'PO NO'.
  WA_FCAT-SELTEXT_M = 'PO NUMBER'.
  WA_FCAT-SELTEXT_L = 'PURCHASE ORDER NUMBER'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

   WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'XBLNR'.
  WA_FCAT-SELTEXT_S = 'VENDOR INV'.
  WA_FCAT-SELTEXT_M = 'VENDOR INVOICE'.
  WA_FCAT-SELTEXT_L = 'VENDOR INVOICE'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.





  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'PRUEFLOS'.
  WA_FCAT-SELTEXT_S = 'LOT NO'.
  WA_FCAT-SELTEXT_M = 'QUALITY LOT NUMBER'.
  WA_FCAT-SELTEXT_L = 'QUALITY LOT NUMBER'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'CHARG'.
  WA_FCAT-SELTEXT_S = 'SAP BAT NO'.
  WA_FCAT-SELTEXT_M = 'SAP BATCH NUM'.
  WA_FCAT-SELTEXT_L = 'SAP BATCH NUMBER'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'LICHA'.
  WA_FCAT-SELTEXT_S = 'VENDOR BAT NO'.
  WA_FCAT-SELTEXT_M = 'VENDOR BATCH NUM'.
  WA_FCAT-SELTEXT_L = 'VENDOR BATCH NUMBER'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'MENGE'.
  WA_FCAT-SELTEXT_S = 'RECEIVED QTY'.
  WA_FCAT-SELTEXT_M = 'RECEIVED QUANTITY'.
  WA_FCAT-SELTEXT_L = 'RECEIVED QUANTITY'.
  WA_FCAT-DO_SUM   = 'X'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'MEINS'.
  WA_FCAT-SELTEXT_S = 'UNIT'.
  WA_FCAT-SELTEXT_M = 'UNIT OF MEASUREMENT'.
  WA_FCAT-SELTEXT_L = 'UNIT OF MEASUREMENT'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'MENGE1'.
  WA_FCAT-SELTEXT_S = 'REJECTED QTY'.
  WA_FCAT-SELTEXT_M = 'REJECTED QUANTITY'.
  WA_FCAT-SELTEXT_L = 'REJECTED QUANTITY'.
  WA_FCAT-DO_SUM   = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.


  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'MEINS1'.
  WA_FCAT-SELTEXT_S = 'UNIT'.
  WA_FCAT-SELTEXT_M = 'UNIT OF MEASUREMENT'.
  WA_FCAT-SELTEXT_L = 'UNIT OF MEASUREMENT'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'GRTXT'.
  WA_FCAT-SELTEXT_S = 'REASON'.
  WA_FCAT-SELTEXT_M = 'REJECTION REASON'.
  WA_FCAT-SELTEXT_L = 'REJECTION REASON'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.



  WA_FCAT-COL_POS   = SNO.
  WA_FCAT-FIELDNAME = 'CATEG'.
  WA_FCAT-SELTEXT_S = 'CATEGORY'.
  WA_FCAT-SELTEXT_M = 'CATEGORY'.
  WA_FCAT-SELTEXT_L = 'CATEGORY'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  SNO = SNO + 1.

************************LAYOUT*****************


  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.







ENDFORM.                    " F_CAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = SY-REPID
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
      I_CALLBACK_TOP_OF_PAGE            = 'TOP_OF_PAGE'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      IS_LAYOUT                         = LAYOUT
      IT_FIELDCAT                       = IT_FCAT
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     IT_SORT                           =
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
*     I_SAVE                            = ' '
*     IS_VARIANT                        =
      IT_EVENTS                         = IT_EVENT
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = IT_FINAL
*   EXCEPTIONS
*     PROGRAM_ERROR                     = 1
*     OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  LISTHEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LISTHEADER .

  DATA : FRMDT(10) TYPE C,
           TODT(10) TYPE C,
           V_STR    TYPE STRING.


  WA_LISTHEADER-TYP = 'H'.
  WA_LISTHEADER-KEY = ' '.
  WA_LISTHEADER-INFO = 'Quality Rejection Report'.
  APPEND WA_LISTHEADER TO IT_LISTHEADER.
  CLEAR WA_LISTHEADER.



  IF S_BUDAT-LOW NE '00000000' AND  S_BUDAT-HIGH NE '00000000'      .
    WRITE S_BUDAT-LOW  TO FRMDT USING EDIT MASK '__.__.____'.
    WRITE S_BUDAT-HIGH TO TODT  USING EDIT MASK '__.__.____'.
    CONCATENATE 'FROM'
                 FRMDT
                 'TO'
                 TODT
           INTO V_STR SEPARATED BY SPACE.
  ELSEIF S_BUDAT-LOW NE '00000000' AND  S_BUDAT-HIGH EQ '00000000'      .
    WRITE S_BUDAT-LOW  TO FRMDT USING EDIT MASK '__.__.____'.
    CONCATENATE 'ON'
                 FRMDT
           INTO V_STR SEPARATED BY SPACE.
  ELSEIF S_BUDAT-LOW EQ '00000000' AND  S_BUDAT-HIGH NE '00000000'      .
    WRITE S_BUDAT-HIGH  TO TODT USING EDIT MASK '__.__.____'.
    CONCATENATE 'ON'
                 TODT
           INTO V_STR SEPARATED BY SPACE.
  ENDIF.

  WA_LISTHEADER-TYP = 'S'.
  WA_LISTHEADER-KEY = ' '.
  WA_LISTHEADER-INFO = V_STR.
  APPEND WA_LISTHEADER TO IT_LISTHEADER.
  CLEAR WA_LISTHEADER.

  WA_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_EVENT-FORM = 'TOP_OF_PAGE'.
  APPEND WA_EVENT TO IT_EVENT.
  CLEAR WA_EVENT.




ENDFORM.                    " LISTHEADER

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_LISTHEADER[].

ENDFORM. "top_of_page
