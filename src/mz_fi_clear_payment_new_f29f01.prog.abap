
*----------------------------------------------------------------------*
***INCLUDE MZ_FI_CLEAR_PAYMENT_NEW_F29F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F29FBZ3_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CHECK  text
*----------------------------------------------------------------------*
FORM F29FBZ3_BDC  CHANGING P_LV_CHECK TYPE ANY.

  FREE: MESSTAB[], GS_MESSTAB.
  CLEAR: P_LV_CHECK.

  DATA: LV_MVAR1      TYPE BALM-MSGV1,
        LV_MVAR2      TYPE BALM-MSGV1,
        LV_MVAR3      TYPE BALM-MSGV1,
        LV_MVAR4      TYPE BALM-MSGV1.

  DATA: LV_MVAR5      TYPE BALM-MSGV1,
      LV_MVAR6      TYPE BALM-MSGV1,
      LV_MVAR7      TYPE BALM-MSGV1,
      LV_MVAR8      TYPE BALM-MSGV1.

  CALL FUNCTION 'BPAR_P_FI_CUSTOMER_DEQUEUE'
    EXPORTING
      CUSTOMER       = GS_TAB-KUNNR
    EXCEPTIONS
      SYSTEM_FAILURE = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
*     Implement suitable error handling here
  ENDIF.


  IF GS_HEADER-SPLGL EQ 'A' OR GS_HEADER-SPLGL EQ 'H'.


    PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0111'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  GV_FWRBTR. "'BSEG-WRBTR'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'BKPF-BLDAT'
                                  GV_BLDAT. "GS_HEADER-BLDAT. "'30.09.2020'.
    PERFORM BDC_FIELD       USING 'BKPF-BLART'
                                  'DZ'.
    PERFORM BDC_FIELD       USING 'BKPF-BUKRS'
                                   GS_HEADER-BUKRS. "BUKRS. "'1000'.
    PERFORM BDC_FIELD       USING 'BKPF-BUDAT'
                                  GV_BUDAT. "GS_HEADER-BUDAT. "'30.09.2020'.
    PERFORM BDC_FIELD       USING 'BKPF-MONAT'
                                  '7'.
    PERFORM BDC_FIELD       USING 'BKPF-WAERS'
                                  'INR'.
    PERFORM BDC_FIELD       USING 'BKPF-XBLNR'
                                  GS_HEADER-XBLNR. "'MARIADOSS'.
    PERFORM BDC_FIELD       USING 'RF05A-NEWKO'
                                   GS_TAB-KUNNR. "'10000059'.
    PERFORM BDC_FIELD       USING 'RF05A-UMSKZ'
                                  GS_HEADER-SPLGL. "'A'.

*    IF GS_HEADER-SPLGL EQ 'S'.
*    PERFORM BDC_FIELD       USING 'RF05A-UMSKZ'
*                                  'S'.
*    ENDIF.
    PERFORM BDC_FIELD       USING 'RF05A-KONTO'
                                   GS_HEADER-NEWKO. "'24000600'.
    WRITE GS_TAB-WRBTR TO GV_FWRBTR.
    CONDENSE GV_FWRBTR.
    PERFORM BDC_FIELD       USING 'BSEG-WRBTR'
                                   GV_FWRBTR. ""GS_TAB-WRBTR. "'3330'.
    PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0304'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  GS_HEADER-XTEXT. "'BSEG-SGTXT'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BU'.

    PERFORM BDC_FIELD       USING 'BSEG-WRBTR'
                                  GV_FWRBTR. ""GS_TAB-WRBTR. "'3330'.
    PERFORM BDC_FIELD       USING 'BSEG-WMWST'
                                  ''.
    PERFORM BDC_FIELD       USING 'BSEG-MWSKZ'
                                  'O0'.
    PERFORM BDC_FIELD       USING 'RF05A-XMWST'
                                  'X'.
    PERFORM BDC_FIELD       USING 'BSEG-ZFBDT'
                                  LV_DATE. "'30.09.2020'.
    PERFORM BDC_FIELD       USING 'BSEG-SGTXT'
                                  GS_HEADER-XTEXT. "'MARIADOSS'.
    CALL TRANSACTION 'F-29'
                             USING  BDCDATA
                             MODE   'N'
                             UPDATE 'A'
                             MESSAGES INTO MESSTAB.


    WAIT UP TO 2 SECONDS.
    COMMIT WORK AND WAIT.


  ELSE .

    "IF GS_HEADER-SPLGL EQ 'A' OR GS_HEADER-SPLGL EQ 'H'.

    PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0123'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RF05A-XINPR'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'BKPF-BUKRS'
                                  GS_HEADER-BUKRS. "BUKRS. "'1000'.
    PERFORM BDC_FIELD       USING 'BKPF-BLART'
                                  'DZ'.
    PERFORM BDC_FIELD       USING 'BKPF-BUDAT'
                                  GV_BUDAT . "GS_HEADER-BUDAT. "'30.09.2020'.
    PERFORM BDC_FIELD       USING 'BKPF-MONAT'
                                  '7'.
    PERFORM BDC_FIELD       USING 'RF05A-KONTO'
                                  GS_HEADER-NEWKO." "'24000600'.
    PERFORM BDC_FIELD       USING 'BKPF-WAERS'
                                  'INR'.
    PERFORM BDC_FIELD       USING 'BKPF-XBLNR'
                                  GS_HEADER-XBLNR. "'MARIADOSS'.
    PERFORM BDC_FIELD       USING 'BKPF-BLDAT'
                                  GV_BLDAT . "GS_HEADER-BLDAT. "'30.09.2020'.
    PERFORM BDC_FIELD       USING 'RF05A-XINPR'
                                  'X'.
    PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0124'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'BSEG-WRBTR'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM BDC_FIELD       USING 'RF05A-AGKON'
                                  GS_TAB-KUNNR. "KUNNR. "'10000059'.
    PERFORM BDC_FIELD       USING 'BKPF-BLDAT'
                                  GV_BLDAT . "GS_HEADER-BLDAT. "'30.09.2020'.
    WRITE GS_TAB-WRBTR TO GV_FWRBTR.
    CONDENSE GV_FWRBTR.
    PERFORM BDC_FIELD       USING 'BSEG-WRBTR'
                                  GV_FWRBTR. "GS_TAB-WRBTR. "'1555'.
    PERFORM BDC_FIELD       USING 'BKPF-WAERS'
                                  'INR'.
    PERFORM BDC_FIELD       USING 'BKPF-XBLNR'
                                  GS_HEADER-XTEXT."'MARIADOSS'.
    PERFORM BDC_DYNPRO      USING 'SAPDF05X' '3100'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RF05A-AKOBT'.
    PERFORM BDC_FIELD       USING 'RF05A-ABPOS'
                                  '1'.
    PERFORM BDC_FIELD       USING 'RF05A-AKOBT'
                                  GV_FWRBTR."GS_TAB-WRBTR. "'               1555.00'.
    PERFORM BDC_DYNPRO      USING 'SAPDF05X' '3100'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RF05A-AKOBT'.
    PERFORM BDC_FIELD       USING 'RF05A-ABPOS'
                                  '1'.
    PERFORM BDC_FIELD       USING 'RF05A-AKOBT'
                                  GV_FWRBTR."GS_TAB-WRBTR. "'           1555.00'.
    CALL TRANSACTION 'FBZ3' USING  BDCDATA
                             MODE   'N'
                             UPDATE 'A'
                             MESSAGES INTO MESSTAB.
*
    WAIT UP TO 2 SECONDS.
    .
  ENDIF.
  CLEAR : GV_FWRBTR.
  CLEAR: GS_MESSTAB, GS_MESSAGE.
  READ TABLE MESSTAB INTO GS_MESSTAB WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC = 0 .
    P_LV_CHECK = 'E'.
    LV_MVAR1 = GS_MESSTAB-MSGV1.
    LV_MVAR2 = GS_MESSTAB-MSGV2.
    LV_MVAR3 = GS_MESSTAB-MSGV3.
    LV_MVAR4 = GS_MESSTAB-MSGV4.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        LANGUAGE               = SY-LANGU
        MSG_ID                 = GS_MESSTAB-MSGID
        MSG_NO                 = GS_MESSTAB-MSGNR
        MSG_VAR1               = LV_MVAR1
        MSG_VAR2               = LV_MVAR2
        MSG_VAR3               = LV_MVAR3
        MSG_VAR4               = LV_MVAR4
      IMPORTING
        MSG_TEXT               = GS_MESSAGE-MESSAGE
      EXCEPTIONS
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        OTHERS                 = 3.
    GS_MESSAGE-MESS_TYPE = 'E'.
    APPEND GS_MESSAGE TO GT_MESSAGE.
  ENDIF.

  READ TABLE MESSTAB INTO GS_MESSTAB WITH KEY MSGTYP = 'S' MSGNR = '312'.
  IF SY-SUBRC = 0 .
    P_LV_CHECK = 'S'.
    LV_MVAR5 = GS_MESSTAB-MSGV1.
    LV_MVAR6 = GS_MESSTAB-MSGV2.
    LV_MVAR7 = GS_MESSTAB-MSGV3.
    LV_MVAR8 = GS_MESSTAB-MSGV4.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        LANGUAGE               = SY-LANGU
        MSG_ID                 = GS_MESSTAB-MSGID
        MSG_NO                 = GS_MESSTAB-MSGNR
        MSG_VAR1               = LV_MVAR5
        MSG_VAR2               = LV_MVAR6
        MSG_VAR3               = LV_MVAR7
        MSG_VAR4               = LV_MVAR8
      IMPORTING
        MSG_TEXT               = GS_MESSAGE-MESSAGE
      EXCEPTIONS
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        OTHERS                 = 3.
    GS1_MESSAGE-MESS_TYPE = 'S'.
    GS1_MESSAGE-KUNNR = GS_TAB-KUNNR.
    GS1_MESSAGE-WRBTR = GS_TAB-WRBTR.
    GS1_MESSAGE-BELNR = GS_MESSTAB-MSGV1..
    GS1_MESSAGE-STATUS = 'Success' .
    APPEND GS1_MESSAGE TO GT1_MESSAGE.
  ENDIF.

  APPEND LINES OF GT1_MESSAGE TO GT_MESSAGE.

  FREE :BDCDATA[].
  CLEAR: GS_MESSAGE,GS_TAB.
  CLEAR: GS1_MESSAGE.
ENDFORM.                    " F29FBZ3_BDC
