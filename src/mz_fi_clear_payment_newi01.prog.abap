*&---------------------------------------------------------------------*
*&  Include           MZ_FI_FB05_INCOMING_PAYMENTI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  DATA: LV_INDEX   TYPE SY-TABIX,
        LV_LINES    TYPE SY-TABIX.

  CASE OK_CODE.
    WHEN 'ENTER'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GS_HEADER-NEWKO
        IMPORTING
          OUTPUT = GS_HEADER-NEWKO.
* desc for GL ac
      SELECT TXT50 INTO GS_HEADER-GLDESC UP TO 1 ROWS FROM SKAT WHERE SPRAS = SY-LANGU
                                    AND SAKNR = GS_HEADER-NEWKO ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

      IF SY-SUBRC = 0.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = GS_HEADER-NEWKO
          IMPORTING
            OUTPUT = GS_HEADER-NEWKO.
      ENDIF.
    WHEN '&POST1' OR '&SAVE1'.
      APPEND LINES OF GT_TAB TO GT1_TAB.
      SORT GT1_TAB BY KUNNR.
      DELETE ADJACENT DUPLICATES FROM GT1_TAB COMPARING KUNNR .
      DELETE GT_TAB WHERE KUNNR IS INITIAL.
      CLEAR GS_TAB.
* loop all the entries in table control of selection screen

        LV_DATE1 = SY-DATUM  .

  CONCATENATE LV_DATE1+6(2) LV_DATE1+4(2) LV_DATE1+0(4) INTO LV_DATE
  SEPARATED BY '.' .

    WRITE GS_HEADER-BUDAT TO GV_BUDAT.
  WRITE GS_HEADER-BLDAT TO GV_BLDAT.

      LOOP AT GT_TAB INTO GS_TAB.
        FREE: GV_SCLEAR, GV_VCLEAR, GV_NCLEAR, GS_BAPIRETURN, GT_LINEITEMS, GT_OPENITEMS, GT_OPENITEMSC, GT_AMNT, GS_AMNT,
             GV_PARTIAL, GV_EXCESS, GV_EXACT, GV_WRBTR, GV_WRBTRC, GV_WRBTRO, GT_FCAT, GT_CUSVEN, GT_LINEITEMSV.
* first clear records using F.13

        "IF GS_HEADER-SPLGL EQ 'A' OR GS_HEADER-SPLGL EQ 'H'.


        PERFORM F29FBZ3_BDC CHANGING CHECK.

        "PERFORM F13_BDC CHANGING CHECK.

**
** process invoice where partials exist and the total of partials match with invoice value.
** Process docs in gt_amnt where net amount is 0. this is to process for root doc with 0 net amount.
*        IF GS_TAB-BELNR IS INITIAL .
** get open items for customer
*          PERFORM BAPI_AR_ACC_GETOPENITEMS USING GS_HEADER-BUKRS GS_TAB-KUNNR GS_TAB-BELNR
*                                           CHANGING  GS_BAPIRETURN GT_LINEITEMS .
*          IF GT_LINEITEMS[] IS NOT INITIAL.
*
*            PERFORM DETERMINE_DUE_DATE USING GT_LINEITEMS
*                                        CHANGING GT_OPENITEMS.
** calculate due dates of each open items and sort so that oldest invoice processed first.
*            PERFORM PROCESS_DOCUMENTS CHANGING GT_OPENITEMS.
** each record in gt_amnt is processed
*            DELETE GT_AMNT WHERE NET_AMNT NE 0.
*            IF GT_AMNT[] IS NOT INITIAL.
*              GV_SCLEAR = 'X'.
*              CLEAR : GV_VCLEAR, GV_NCLEAR.
*              FREE: GT_OPENITEMSC[].
** bdc for f-32
*              PERFORM F32_BDC_CN CHANGING CHECK.
*
*            ENDIF.
*
*          ENDIF.
*          FREE: GS_BAPIRETURN, GT_LINEITEMS, GT_OPENITEMS, GT_OPENITEMSC, GT_AMNT, GV_WRBTR, GV_WRBTRC, GV_WRBTRO, GT_FCAT, GT_CUSVEN, GT_LINEITEMSV.
*        ENDIF.
**
** select vendor if the customer has one
*        SELECT KUNNR LIFNR FROM LFA1 INTO TABLE GT_CUSVEN
*                             WHERE KUNNR = GS_TAB-KUNNR
*                             AND LIFNR NE ' '.              "Customer as a vendor scenario
*
** KR docs( customer as a vendor ) considered similar to credit notes
*        IF GT_CUSVEN[] IS NOT INITIAL AND GS_TAB-BELNR IS INITIAL .
** get open items for customer
*          PERFORM BAPI_AR_ACC_GETOPENITEMS USING GS_HEADER-BUKRS GS_TAB-KUNNR GS_TAB-BELNR
*                                           CHANGING  GS_BAPIRETURN GT_LINEITEMS .
*          IF GT_LINEITEMS[] IS NOT INITIAL.
** for vendor open items
** " If there are customer open items available, then vendor open items also needs to be checked
*            FREE : GT_LINEITEMSV[].
** get open items for vendor
*            PERFORM BAPI_AP_ACC_GETOPENITEMS USING GS_HEADER-BUKRS GS_TAB-KUNNR
*                                             CHANGING  GS_BAPIRETURN.
*            IF NOT GT_LINEITEMSV[] IS INITIAL.
*
*              PERFORM DETERMINE_DUE_DATE USING GT_LINEITEMS
*                                          CHANGING GT_OPENITEMS.
** calculate due dates of each open items and sort so that oldest invoice processed first.
*              PERFORM PROCESS_DOCUMENTS CHANGING GT_OPENITEMS.
** each record in gt_amnt is processed
*              IF GT_AMNT[] IS NOT INITIAL.
** if credit notes exist
*                IF NOT GT_OPENITEMSC[] IS INITIAL.
*                  PERFORM CHECK_CREDITNOTES.
** bdc for f-32 only KR doc scenario
*                  IF NOT GT_OPENITEMSC[] IS INITIAL.
*                    GV_VCLEAR = 'X'.
*                    CLEAR : GV_SCLEAR, GV_NCLEAR.
*
*                    PERFORM F32_BDC_CN CHANGING CHECK.
*
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*          FREE: GS_BAPIRETURN, GT_LINEITEMS, GT_OPENITEMS, GT_OPENITEMSC, GT_AMNT, GV_WRBTR, GV_WRBTRC, GV_WRBTRO, GT_FCAT, GT_CUSVEN, GT_LINEITEMSV.
*        ENDIF.
**
** F-32 for other credit notes/excess payments/un-addresed partial payment docs.
*        IF GS_TAB-BELNR IS INITIAL .
** get open items for customer
*          PERFORM BAPI_AR_ACC_GETOPENITEMS USING GS_HEADER-BUKRS GS_TAB-KUNNR GS_TAB-BELNR
*                                           CHANGING  GS_BAPIRETURN GT_LINEITEMS .
*          IF GT_LINEITEMS[] IS NOT INITIAL.
*
*            PERFORM DETERMINE_DUE_DATE USING GT_LINEITEMS
*                                        CHANGING GT_OPENITEMS.
** calculate due dates of each open items and sort so that oldest invoice processed first.
*            PERFORM PROCESS_DOCUMENTS CHANGING GT_OPENITEMS.
** each record in gt_amnt is processed
*            IF GT_AMNT[] IS NOT INITIAL.
** if credit notes exist
*              IF NOT GT_OPENITEMSC[] IS INITIAL.
*                PERFORM CHECK_CREDITNOTES.
** bdc for f-32
*                IF NOT GT_OPENITEMSC[] IS INITIAL.
*                  GV_NCLEAR = 'X'.
*                  CLEAR : GV_VCLEAR, GV_SCLEAR.
*
*                  PERFORM F32_BDC_CN CHANGING CHECK.
*
*                ENDIF.
*              ENDIF.
*            ENDIF.
*
*          ENDIF.
*          FREE: GS_BAPIRETURN, GT_LINEITEMS, GT_OPENITEMS, GT_OPENITEMSC, GT_AMNT, GV_WRBTR, GV_WRBTRC, GV_WRBTRO, GT_FCAT, GT_CUSVEN, GT_LINEITEMSV.
*        ENDIF.
*
**
** pass amount entered for payment
*        GV_WRBTR = GS_TAB-WRBTR.
** get open items again because f-32 might have run in the case of credit note scenario.
*
*        PERFORM BAPI_AR_ACC_GETOPENITEMS USING GS_HEADER-BUKRS GS_TAB-KUNNR GS_TAB-BELNR
*                                         CHANGING  GS_BAPIRETURN GT_LINEITEMS .
*
*        IF GT_LINEITEMS[] IS  INITIAL.
*          IF GV_WRBTR GT 0.
** post as excess payment by customer
*            PERFORM FB05_BDC_EXCESS CHANGING CHECK.
*          ENDIF.
*          CONTINUE.
*        ENDIF.
** get due dates
*        PERFORM DETERMINE_DUE_DATE USING GT_LINEITEMS
*                                    CHANGING GT_OPENITEMS.
** calculate due dates of each open items and sort so that oldest invoice processed first.
*        PERFORM PROCESS_DOCUMENTS CHANGING GT_OPENITEMS.
*
*        IF GT_AMNT[] IS  INITIAL.
*          IF GV_WRBTR GT 0.
** post as excess payment by customer
*
*            PERFORM FB05_BDC_EXCESS CHANGING CHECK.
*
*          ENDIF.
*          CONTINUE.
*        ENDIF.
*
*        CLEAR : GS_AMNT, GV_PWRBTR.
*        DESCRIBE TABLE GT_AMNT LINES LV_LINES.
** process documents
*
** if Bill to Bill scenario.
*        IF GS_TAB-BELNR IS NOT INITIAL.
*          LOOP AT GT_AMNT INTO GS_AMNT.
*            LV_INDEX = SY-TABIX.
*            IF GV_WRBTR LE 0.
*              EXIT.
*            ENDIF.
*            IF GS_AMNT-PREV IS NOT INITIAL.
*              GS_MESSAGE-MESS_TYPE = 'E'.
*              GS_MESSAGE-MESSAGE = 'Previous Partial payment done on this Bill'.
*              MOVE-CORRESPONDING GS_TAB TO GS_MESSAGE.
*              APPEND GS_MESSAGE TO GT_MESSAGE.
*              EXIT.
*            ENDIF.
*            IF GV_WRBTR LT GS_AMNT-NET_AMNT OR GV_WRBTR GT GS_AMNT-NET_AMNT.
*              GS_MESSAGE-MESS_TYPE = 'E'.
*              GS_MESSAGE-MESSAGE =  'Amount should be equal to Invoice amount less discount'.
*              MOVE-CORRESPONDING GS_TAB TO GS_MESSAGE.
*              APPEND GS_MESSAGE TO GT_MESSAGE.
*              EXIT.
*            ENDIF.
*            GS_AMNT-PROC = 'X'.
*            MODIFY GT_AMNT FROM GS_AMNT INDEX LV_INDEX TRANSPORTING PROC.
*            CLEAR GS_AMNT.
*          ENDLOOP.
*
*          GV_EXACT = 'X'.  " here in this casen only one root document is processed
*
** Non Bill to Bill scenario
*        ELSE.                       " Non Bill to Bill scenario
*          FREE :  GV_EXACT, GV_PARTIAL, GV_EXCESS.
*          LOOP AT GT_AMNT INTO GS_AMNT.
*            LV_INDEX = SY-TABIX.
*
*            GS_AMNT-PROC = 'X'.
*            MODIFY GT_AMNT FROM GS_AMNT INDEX LV_INDEX TRANSPORTING PROC.
*
** check for the required root docs to process updating 'PROC' indicator
** Also check if it is an exact/partial/excess case sceanrio.
*            IF GV_WRBTR LT GS_AMNT-NET_AMNT.  " amount not sufficient to knock off complete doc
*              GV_WRBTR1 = GS_AMNT-NET_AMNT - GV_WRBTR.
*              GV_PARTIAL = 'X'.
*              EXIT.
*            ELSEIF GV_WRBTR EQ GS_AMNT-NET_AMNT.
*              GV_EXACT = 'X'.
*              GV_WRBTR = GV_WRBTR - GS_AMNT-NET_AMNT.
*              GV_WRBTR1 = 0.
*              EXIT.
*            ELSE.
*              IF LV_LINES = LV_INDEX.    " for last item
*                GV_WRBTR1 =  GV_WRBTR - GS_AMNT-NET_AMNT .
*                GV_EXCESS = 'X'.
*              ENDIF.
*              GV_WRBTR = GV_WRBTR - GS_AMNT-NET_AMNT.
*            ENDIF.
*
*            CLEAR :GS_AMNT, GV_WRBTR1.
*          ENDLOOP.
*
*        ENDIF.
** perform FB05 BDC
*        DELETE GT_AMNT WHERE PROC NE 'X'.
*        IF NOT GT_AMNT[] IS INITIAL.
*          PERFORM PROGRESS_INDICATOR USING 'Posting and clearing' 'for Customer:' GS_TAB-KUNNR 'is in progress'.
*          PERFORM FB05_BDC CHANGING CHECK.
*        ENDIF.

        CLEAR: GV_PARTIAL, GV_EXCESS, GV_EXACT, GV_WRBTR, GV_WRBTR1, GS_TAB.
      ENDLOOP.


      LOOP AT GT1_TAB INTO GS1_TAB.

        PERFORM BAPI_AR_ACC_GETOPENITEMS USING GS_HEADER-BUKRS GS1_TAB-KUNNR GS_TAB-BELNR
                                                CHANGING  GS_BAPIRETURN GT_LINEITEMS .


     LOOP AT GT_LINEITEMS INTO GS_LINEITEMS.
          SHIFT GS_LINEITEMS-DOC_NO LEFT DELETING LEADING '0'.
          MODIFY GT_LINEITEMS FROM GS_LINEITEMS TRANSPORTING DOC_NO.
          CLEAR GS_LINEITEMS.
      ENDLOOP.

      DATA : IN TYPE SY-TABIX.

      LOOP AT GT_LINEITEMS INTO GS_LINEITEMS WHERE SP_GL_IND EQ 'A'. "Excess "WHERE SPL .
        IN = SY-TABIX.
        MOVE-CORRESPONDING GS_LINEITEMS TO WA_HITEMS.
        APPEND WA_HITEMS TO GT_HITEMS.
      ENDLOOP.

      LV1_COUNT = LINES( GT_HITEMS ).

      LOOP AT GT_LINEITEMS INTO GS_LINEITEMS WHERE DB_CR_IND = 'S'. "WHERE DB_CR_IND = 'S'. "WHERE SPL .
        MOVE-CORRESPONDING GS_LINEITEMS TO WA_SITEMS.
        APPEND WA_SITEMS TO GT_SITEMS.
      ENDLOOP.
      LV2_COUNT = LINES( GT_SITEMS ).

      SORT GT_SITEMS DESCENDING BY PSTNG_DATE. "descending by .
      S_COUNT = 1.
      H_COUNT = 1.

"""""""""""""For payment adjustment
    IF GT_HITEMS IS NOT INITIAL.
  "LOOP AT GT_HITEMS INTO WA_HITEMS . "where sy-tabix = 1 . " WHERE INDEX EQ '1' ." FROM H_COUNT ." WHERE SY-TABIX = H_COUNT. "USING KEY INDEX ." WHERE DOC_NO = '0100000013'.
  WHILE LV1_COUNT <> 0 .
    READ TABLE GT_HITEMS INTO WA_HITEMS INDEX 1 .
    IF SY-SUBRC EQ 0 .
      L1_BELNR = WA_HITEMS-DOC_NO .
      L1_AMT = WA_HITEMS-AMOUNT ."#EC CI_FLDEXT_OK[2610650]
      "Added by SPLABAP during code remediation
      " LOOP AT GT_SITEMS INTO WA_SITEMS." WHERE DOC_NO = '0100000014'.
      READ TABLE GT_SITEMS INTO WA_SITEMS INDEX 1 .
      IF SY-SUBRC EQ 0 .
        L2_BELNR = WA_SITEMS-DOC_NO.
        L2_AMT = WA_SITEMS-AMOUNT."#EC CI_FLDEXT_OK[2610650]
"Added by SPLABAP during code remediation
        BAL_AMT1 = L1_AMT - L2_AMT.

        BAL_AMT = BAL_AMT1.

        CONDENSE BAL_AMT.

        IF L1_AMT => L2_AMT .

          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0131'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-XPOS1(03)'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM BDC_FIELD       USING 'RF05A-AGKON'
                                        GS1_TAB-KUNNR. "GS_KUNNR. "'10000047'.
          PERFORM BDC_FIELD       USING 'BKPF-BUDAT'
                                        GV_BUDAT. "GS_HEADER-BUDAT. " LV_DATE. "'19.09.2020'.
          PERFORM BDC_FIELD       USING 'BKPF-MONAT'
                                        '6'.
          PERFORM BDC_FIELD       USING 'BKPF-BUKRS'
                                        GS_HEADER-BUKRS. " BUKRS. "'1000'.
          PERFORM BDC_FIELD       USING 'BKPF-WAERS'
                                        'INR'.
          PERFORM BDC_FIELD       USING 'RF05A-AGUMS'
                                        'A'.
          PERFORM BDC_FIELD       USING 'RF05A-XNOPS'
                                        'X'.
          PERFORM BDC_FIELD       USING 'RF05A-XPOS1(01)'
                                        ''.
          PERFORM BDC_FIELD       USING 'RF05A-XPOS1(03)'
                                        'X'.
          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0731'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-SEL01(02)'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=PA'.
          PERFORM BDC_FIELD       USING 'RF05A-SEL01(01)'
                                        L1_BELNR."'1400000007'.
          PERFORM BDC_FIELD       USING 'RF05A-SEL01(02)'
                                        L2_BELNR. "'9000000024'.
          PERFORM BDC_DYNPRO      USING 'SAPDF05X' '3100'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=KMD'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-DIFFB'.
          PERFORM BDC_FIELD       USING 'RF05A-ABPOS'
                                        '1'.
          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0700'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-NEWBW'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM BDC_FIELD       USING 'RF05A-NEWBS'
                                        '19'.
          PERFORM BDC_FIELD       USING 'RF05A-NEWKO'
                                        GS1_TAB-KUNNR. "GS_KUNNR. "'10000047'.
          PERFORM BDC_FIELD       USING 'RF05A-NEWUM'
                                        'A'.
          PERFORM BDC_FIELD       USING 'RF05A-NEWBW'
                                        '105'.
          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0304'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'BSEG-SGTXT'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM BDC_FIELD       USING 'BSEG-WRBTR'
                                        BAL_AMT."'2777.22'.
          PERFORM BDC_FIELD       USING 'BSEG-MWSKZ'
                                        'O0'.
          PERFORM BDC_FIELD       USING 'BSEG-ZFBDT'
                                        LV_DATE. "'19.09.2020'.
          PERFORM BDC_FIELD       USING 'BSEG-SGTXT'
                                        'HGYH'.
          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0304'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'BSEG-ZLSPR'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=BS'.
          PERFORM BDC_FIELD       USING 'BSEG-WRBTR'
                                        BAL_AMT."'2,777.22'.
          PERFORM BDC_FIELD       USING 'BSEG-MWSKZ'
                                        'O0'.
          PERFORM BDC_FIELD       USING 'BSEG-ZFBDT'
                                        LV_DATE. "'19.09.2020'.
          PERFORM BDC_FIELD       USING 'BSEG-ZLSPR'
                                        ''.
          PERFORM BDC_FIELD       USING 'BSEG-SGTXT'
                                        'HGYH'.
          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0700'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-NEWBS'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=BU'.

          CALL TRANSACTION 'F-32'
                            USING  BDCDATA
                             MODE   'N'
                             UPDATE 'A'
                             MESSAGES INTO MESSTAB.
           WAIT UP TO 2 SECONDS.
          COMMIT WORK AND WAIT.

        ENDIF.


        IF L1_AMT < L2_AMT .

          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0131'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'BKPF-BUDAT'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=PA'.
          PERFORM BDC_FIELD       USING 'RF05A-AGKON'
                                        GS1_TAB-KUNNR. "GS_KUNNR. "'10000059'.
          PERFORM BDC_FIELD       USING 'BKPF-BUDAT'
                                        GV_BUDAT. "GS_HEADER-BUDAT."'30.09.2020'.
          PERFORM BDC_FIELD       USING 'BKPF-MONAT'
                                        '7'.
          PERFORM BDC_FIELD       USING 'BKPF-BUKRS'
                                        GS_HEADER-BUKRS. "GS_BUKRS. "'1000'.
          PERFORM BDC_FIELD       USING 'BKPF-WAERS'
                                        'INR'.
          PERFORM BDC_FIELD       USING 'RF05A-AGUMS'
                                        'A'.
          PERFORM BDC_FIELD       USING 'RF05A-XNOPS'
                                        'X'.
          PERFORM BDC_FIELD       USING 'RF05A-XPOS1(01)'
                                        ''.
          PERFORM BDC_FIELD       USING 'RF05A-XPOS1(03)'
                                        'X'.
          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0731'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-SEL01(02)'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=PA'.
          PERFORM BDC_FIELD       USING 'RF05A-SEL01(01)'
                                        L1_BELNR. " '701000000'.
          PERFORM BDC_FIELD       USING 'RF05A-SEL01(02)'
                                        L2_BELNR."'1400000002'.
          PERFORM BDC_DYNPRO      USING 'SAPDF05X' '3100'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=PART'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-AKOBT'.
          PERFORM BDC_FIELD       USING 'RF05A-ABPOS'
                                        '1'.
          BAL_AMT = -1 * BAL_AMT .
          PERFORM BDC_FIELD       USING 'RF05A-AKOBT'
                                        BAL_AMT. "'         6,946.00-'.
          PERFORM BDC_DYNPRO      USING 'SAPDF05X' '3100'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=BS'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-AKOBT'.
          PERFORM BDC_FIELD       USING 'RF05A-ABPOS'
                                        '1'.
          PERFORM BDC_FIELD       USING 'RF05A-AKOBT'
                                        BAL_AMT." '         6,946.00-'.
          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0700'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-AZEI1(01)'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=PI'.
          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0301'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'BSEG-SGTXT'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=RW'.
          PERFORM BDC_FIELD       USING 'BSEG-ZTERM'
                                        'NT07'.
          PERFORM BDC_FIELD       USING 'BSEG-ZFBDT'
                                        LV_DATE. "'30.09.2020'.
          PERFORM BDC_FIELD       USING 'BSEG-SGTXT'
                                        '701000000'.
          PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0700'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RF05A-NEWBS'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=BU'.

          CALL TRANSACTION 'F-32'
                            USING  BDCDATA
                             MODE   'N'
                             UPDATE 'A'
                             MESSAGES INTO MESSTAB.
          WAIT UP TO 2 SECONDS.
          COMMIT WORK AND WAIT.

        ENDIF.
        CLEAR : GV_BUDAT,GV_BLDAT.
        CLEAR : GT_LINEITEMS ,GS_LINEITEMS,GT_HITEMS,GT_SITEMS,WA_HITEMS,WA_SITEMS.
        CLEAR : L1_BELNR,L2_BELNR,L1_AMT,L2_AMT,BAL_AMT.
        REFRESH BDCDATA.

        PERFORM BAPI_AR_ACC_GETOPENITEMS USING GS_HEADER-BUKRS GS_TAB-KUNNR GS_TAB-BELNR
                                                  CHANGING  GS_BAPIRETURN GT_LINEITEMS .

        LOOP AT GT_LINEITEMS INTO GS_LINEITEMS.
          SHIFT GS_LINEITEMS-DOC_NO LEFT DELETING LEADING '0'.
          MODIFY GT_LINEITEMS FROM GS_LINEITEMS TRANSPORTING DOC_NO.
          CLEAR GS_LINEITEMS.
        ENDLOOP.

        LOOP AT GT_LINEITEMS INTO GS_LINEITEMS WHERE SP_GL_IND EQ 'A'. "Excess "WHERE SPL .
          MOVE-CORRESPONDING GS_LINEITEMS TO WA_HITEMS.
          APPEND WA_HITEMS TO GT_HITEMS.
        ENDLOOP.
        IF GT_HITEMS IS NOT INITIAL.
          S_COUNT = 1.
        ENDIF.
        LV1_COUNT = LINES( GT_HITEMS ).
*      LV1_COUNT = LINES( GT_LINEITEMS ).
*
        LOOP AT GT_LINEITEMS INTO GS_LINEITEMS WHERE DB_CR_IND = 'S'. "WHERE SPL .
          MOVE-CORRESPONDING GS_LINEITEMS TO WA_SITEMS.
          APPEND WA_SITEMS TO GT_SITEMS.
        ENDLOOP.
        IF GT_SITEMS IS NOT INITIAL.
          H_COUNT = 1.
        ENDIF.
        SORT GT_SITEMS DESCENDING BY PSTNG_DATE. "descending by .
        " MODE P_MODE
        "UPDATE 's' MESSAGES INTO I_BDCMSG.
      ENDIF.
    ENDIF.
  ENDWHILE.
  " ENDLOOP.
ENDIF.

      ENDLOOP.


      IF GT_MESSAGE[] IS NOT INITIAL.

        PERFORM ALV_FREE.
        PERFORM CONTAINER_FREE CHANGING GV_CONTAINER.

        PERFORM CREATE_CONTAINER.   " 'CONTAINER'.
        PERFORM ALV_OBJECT_CREATE.

        PERFORM FILL_CUSTOMER_NAME.
        PERFORM BUILD_FCAT.
        PERFORM SET_TABLE_FOR_DISPLAY.
      ENDIF.

      FREE: GS_BAPIRETURN, GT_LINEITEMS, GT_OPENITEMS, GT_AMNT, GV_WRBTR.
    WHEN '&RESET'.
      CLEAR TC_9000-LINES.
      PERFORM ALV_FREE.
      PERFORM CONTAINER_FREE CHANGING GV_CONTAINER.
      FREE :GT_MESSAGE, GS_MESSAGE,
          GT_TAB, GS_HEADER, GV_CONTAINER, GV_ALV, GT_FCAT.
    WHEN '&BACK1'.
      LEAVE SCREEN.
    WHEN 'TC_9000_INSR'.
      CLEAR GS_TAB.
      APPEND GS_TAB TO GT_TAB.
    WHEN 'TC_9000_DELE'.
      CLEAR GS_TAB.
      LOOP AT GT_TAB INTO GS_TAB WHERE MARK = 'X'.
        DELETE GT_TAB FROM GS_TAB.                       "#EC INDEX_NUM
      ENDLOOP.
  ENDCASE.

  CLEAR OK_CODE.

ENDMODULE.                    "user_command_9000 INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC_9000'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TC_9000_MARK INPUT.

  DATA: G_TC_9000_WA2 LIKE LINE OF GT_TAB.
  IF TC_9000-LINE_SEL_MODE = 1
  AND GS_TAB-MARK = 'X'.
    LOOP AT GT_TAB INTO G_TC_9000_WA2
      WHERE MARK = 'X'.
      G_TC_9000_WA2-MARK = ''.
      MODIFY GT_TAB
        FROM G_TC_9000_WA2
        TRANSPORTING MARK.
    ENDLOOP.
  ENDIF.

  MODIFY GT_TAB
    FROM GS_TAB
    INDEX TC_9000-CURRENT_LINE
    TRANSPORTING MARK.

ENDMODULE.                    "tc_9000_mark INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_9000'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TC_9000_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TC_9000'
                              'GT_TAB'
                              'MARK'
                     CHANGING OK_CODE.


  IF GT_TAB[] IS INITIAL.
    TC_9000-TOP_LINE = 1.
    TC_9000-CURRENT_LINE = 1.
  ENDIF.

  SY-UCOMM = OK_CODE.
ENDMODULE.                    "tc_9000_user_command INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  LEAVE PROGRAM.
ENDMODULE.                    "exit_9000 INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_9000_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_9000_MODIFY INPUT.

  DATA:       LS_TAB    TYPE TY_TAB,
              LV_BELNR TYPE BELNR_D,
              LV_KUNNR TYPE KUNNR,
              LV_KUNNR1 TYPE KUNNR.

  CONDENSE GS_TAB-KUNNR.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = GS_TAB-KUNNR
    IMPORTING
      OUTPUT = GS_TAB-KUNNR.

  CONDENSE GS_TAB-BELNR.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = GS_TAB-BELNR
    IMPORTING
      OUTPUT = GS_TAB-BELNR.

*  customer or belnr entered
  IF GS_TAB-KUNNR IS NOT INITIAL OR GS_TAB-BELNR IS NOT INITIAL.
* both customer and belnr entered
    IF GS_TAB-KUNNR IS NOT INITIAL AND GS_TAB-BELNR IS NOT INITIAL.
      SELECT SINGLE BELNR FROM BSID INTO  LV_BELNR WHERE KUNNR = GS_TAB-KUNNR
                                                          AND BELNR = GS_TAB-BELNR
                                                          AND BUKRS = GS_HEADER-BUKRS
                                                          AND GJAHR = GS_HEADER-GJAHR.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE 'Customer and Bill/DR Memo mismatch for the fiscal year entered' TYPE 'E'.
      ENDIF.
    ELSEIF GS_TAB-KUNNR IS NOT INITIAL.
      SELECT SINGLE KUNNR FROM KNB1 INTO LV_KUNNR WHERE KUNNR = GS_TAB-KUNNR
                                                   AND BUKRS = GS_HEADER-BUKRS.
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE 'Customer not available in Company Code' TYPE 'E'.
      ENDIF.
      CLEAR LS_TAB.
      READ TABLE GT_TAB INTO LS_TAB WITH KEY KUNNR = GS_TAB-KUNNR.
      IF SY-SUBRC IS INITIAL.
        IF LS_TAB-BELNR IS INITIAL AND GS_TAB-BELNR IS INITIAL.
          IF NOT TC_9000-CURRENT_LINE EQ SY-TABIX.
            MESSAGE 'Customer No. already entered' TYPE 'E'.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSEIF GS_TAB-BELNR IS NOT INITIAL.
      SELECT KUNNR UP TO 1 ROWS FROM BSID INTO LV_KUNNR1 WHERE BELNR = GS_TAB-BELNR
                                                   AND BUKRS = GS_HEADER-BUKRS
                                                   AND GJAHR = GS_HEADER-GJAHR ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE 'Bill No does not exist for the fiscal year entered' TYPE 'E'.
      ELSE.
        GS_TAB-KUNNR = LV_KUNNR1.
      ENDIF.
      CLEAR LS_TAB.
      READ TABLE GT_TAB INTO LS_TAB WITH KEY BELNR = GS_TAB-BELNR .
      IF SY-SUBRC IS INITIAL.
        IF LS_TAB-BELNR IS INITIAL AND GS_TAB-BELNR IS INITIAL.
          IF NOT TC_9000-CURRENT_LINE EQ SY-TABIX.
            MESSAGE 'Bill No. already entered' TYPE 'E'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'Enter Customer or Bill/DR Memo' TYPE 'E'.
  ENDIF.

  MODIFY GT_TAB   INDEX TC_9000-CURRENT_LINE   FROM GS_TAB.

ENDMODULE.                    "tc_9000_modify INPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_9000_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HEADER_9000_MODIFY INPUT.

  DATA: LV_GJAHR TYPE GJAHR,
        LV_BUKRS TYPE BUKRS,
        LV_WAERS TYPE WAERS.

  SELECT SINGLE BUKRS FROM T001 INTO LV_BUKRS WHERE BUKRS = GS_HEADER-BUKRS.
  IF SY-SUBRC IS INITIAL.
    SELECT SINGLE WAERS FROM T001 INTO LV_WAERS WHERE BUKRS = GS_HEADER-BUKRS
                                                  AND WAERS = GS_HEADER-WAERS.
    IF SY-SUBRC IS NOT INITIAL.
      SET CURSOR FIELD 'GS_HEADER-WAERS'.
      MESSAGE 'Enter valid Currency' TYPE 'E'.
    ENDIF.
  ELSE.
    SET CURSOR FIELD 'GS_HEADER-BUKRS'.
    MESSAGE 'Invalid Company Code' TYPE 'E'.
  ENDIF.

  IF GS_HEADER-XBLNR IS INITIAL.
    SET CURSOR FIELD 'GS_HEADER-XBLNR'.
    MESSAGE 'Enter Reference' TYPE 'E'.
  ENDIF.

  IF GS_HEADER-XTEXT IS INITIAL.     "added on 12/3
    SET CURSOR FIELD 'GS_HEADER-xtext'.
    MESSAGE 'Enter Reference Text' TYPE 'E'.
  ENDIF.

  PERFORM BLDAT_CHECK USING GS_HEADER-BLDAT
                      CHANGING CHECK.
  IF NOT CHECK IS INITIAL.
    SET CURSOR FIELD 'GS_HEADER-BLDAT'.
    MESSAGE 'Entered Document date is more than 2 months old' TYPE 'E'.
  ENDIF.

* get fiscal year and period - (requires date and company code)
  CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
    EXPORTING
      COMPANYCODEID = GS_HEADER-BUKRS
      POSTING_DATE  = GS_HEADER-BLDAT
    IMPORTING
      FISCAL_YEAR   = LV_GJAHR
      FISCAL_PERIOD = GS_HEADER-MONAT.

  IF GS_HEADER-GJAHR IS  INITIAL.
    GS_HEADER-GJAHR = LV_GJAHR.
  ENDIF.

ENDMODULE.                    "header_9000_modify INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_NEWKO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_NEWKO INPUT.

  PERFORM PAI_F4_NEWKO.

ENDMODULE.                    "f4_newko INPUT
