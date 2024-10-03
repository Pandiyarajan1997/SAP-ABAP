*&---------------------------------------------------------------------*
*&  Include           ZAXIS_PAY_EXTRACTION
*&---------------------------------------------------------------------*
*& Text         DATA READING FOR EXTRACTION AND EXTRACTION OF FILE
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  extract_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EXTRACT_DATA .

  CLEAR : F_FIELD_EMPTY,GV_NO_INV.

  SORT G_TAB_BSAK BY BUKRS AUGBL.

***  IF G_TAB_BSAK_PYMNT[] IS NOT INITIAL.
****----------------------------------------------------------------------*
****        Select the required payments to extract
****----------------------------------------------------------------------*
***    FREE MEMORY ID 'BSAK_PAY'.
***    EXPORT GIT_BSAK_PYMNT  = G_TAB_BSAK_PYMNT  TO MEMORY ID 'BSAK_PAY'.
***
***    SUBMIT ZAXIS_FILTER_RECORDS AND RETURN.
***    IMPORT GIT_BSAK_PYMNT  = G_TAB_BSAK_PYMNT  FROM MEMORY ID 'BSAK_PAY'.
***
***    IF G_TAB_BSAK_PYMNT[] IS INITIAL.
***      PERFORM POP_UP_ERROR USING TEXT-125.          " NO PAYMENTS SELECTED FOR THE EXTRACTION.
***    ENDIF.
***
***  ENDIF.  "G_TAB_BSAK_PYMNT IS INITIAL AT ROW 23


*----------------------------------------------------------------------*
*        construct file for valid payments
*----------------------------------------------------------------------*

  SORT G_TAB_BSAK_PYMNT BY BUKRS AUGBL AUGDT.

  LOOP AT G_TAB_BSAK_PYMNT INTO BSAK1.

*&---------------------------------------------------------------------*
* READ THE PAYMENT REALTED INFORMATION FOR THE PAYMENT HEADER RECORD
*----------------------------------------------------------------------*

    PERFORM READ_HEADER_TABLES USING BSAK1.


    PERFORM READ_ACC_DETAILS.

    PERFORM FIND_PAYMENT_METHOD.

    PERFORM READ_CUSTOM_TABLES.

    PERFORM FIND_GLOBAL_CUSTOME_DATA.


    IF F_PAY_MTD_ERROR IS INITIAL.   " if payment method was find then alow to further

      CLEAR ZAXIS_STR_HEADER.

      ASSIGN ZAXIS_STR_HEADER TO <FS_OUTPUTSTRUC>.
      CLEAR : L_WA_OUTPUTREC,LV_PAY.
      PERFORM EXTRACT_FIELDS_FROM_MAPPING USING 'H'
                                                C_NO_HDR_FIELDS
                                                C_HDR_DELIMITER.


      IF F_FIELD_EMPTY IS INITIAL.    " IF ALL THE FIELDS ARE PROPER THEN ONLY APPEND TO FILE OTHER WISE GIVE ERROR

        APPEND L_WA_OUTPUTREC TO G_IT_OUTPUT.
        CLEAR : L_WA_OUTPUTREC,LV_PAY.

*&---------------------------------------------------------------------*
* READ THE invoice details and populate the values
*----------------------------------------------------------------------*

        LOOP AT G_TAB_BSAK  INTO BSAK
                 WHERE AUGBL = BSAK1-BELNR
                   AND AUGDT = BSAK1-AUGDT.

          PERFORM READ_DETAIL_TABLES USING BSAK.

          CLEAR ZAXIS_STR_DETAIL.
          ASSIGN ZAXIS_STR_DETAIL TO <FS_OUTPUTSTRUC>.

          PERFORM EXTRACT_FIELDS_FROM_MAPPING USING 'D'
                                                 C_NO_DTL_FIELDS
                                                 C_DTL_DELIMITER.


          APPEND L_WA_OUTPUTREC TO G_IT_OUTPUT.
          CLEAR  L_WA_OUTPUTREC.

          ADD 1 TO GV_NO_INV.

        ENDLOOP.
*&---------------------------------------------------------------------*
*       append payment details to display and log.
*----------------------------------------------------------------------*

        PERFORM APPEND_DISPLAY USING TEXT-S01     " ERROR
                                     ' '
                                     C_GRN_COLOUR.
        PERFORM APPEND_LOG .
      ENDIF.
    ENDIF."F_PAY_MTD_ERROR CHECKING.

    CLEAR : GV_NO_INV,
            GV_VENDOR_NAME,
            GV_ALT_LIFNR,
            GV_ACC_NO,
            GV_IFSC_CODE,
            GV_PAYMENT,
            F_PAY_MTD_ERROR,
            F_FIELD_EMPTY,
            GV_PRODUCT_CODE,
            HEADER.

  ENDLOOP.

ENDFORM.                    "extract_data


*&---------------------------------------------------------------------*
*&      Form  read_header_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BSAK     text
*----------------------------------------------------------------------*
FORM READ_HEADER_TABLES  USING P_BSAK .

  PERFORM READ_TABLE_BKPF USING P_BSAK
                        CHANGING BKPF.

  PERFORM READ_TABLE_BVOR USING P_BSAK
                       CHANGING BVOR.

  PERFORM READ_TABLE_REGUH USING P_BSAK
                        CHANGING REGUH.

  PERFORM READ_TABLE_PAYR  CHANGING P_BSAK.

  PERFORM READ_TABLE_BSIS_BSAS  USING P_BSAK
                             CHANGING BSIS.

  PERFORM READ_TABLE_BSEC  USING P_BSAK
                           CHANGING BSEC.

  IF BSEC IS INITIAL.

    PERFORM READ_TABLE_LFA1  USING    P_BSAK
                             CHANGING LFA1.

    PERFORM READ_TABLE_LFB1  USING    P_BSAK
                             CHANGING LFA1
                                      LFB1.

    PERFORM READ_ALT_PAYEE   USING    P_BSAK
                             CHANGING LFA1
                                      LFB1.

    PERFORM READ_TABLE_LFBK  USING  P_BSAK
                          CHANGING LFBK BNKA.

    PERFORM READ_TABLE_ADRC  USING  LFA1-ADRNR
                            CHANGING ADRC.

  ELSE.

    CLEAR : ADRC, LFBK.
*--One time vendor address details
    PERFORM ONE_TIME_VENDOR_ADDRESS.


    MOVE-CORRESPONDING BSEC TO LFBK.
    MOVE-CORRESPONDING BSEC TO LFA1.
    MOVE-CORRESPONDING BSEC TO LFB1.

    MOVE BSAK1-LIFNR TO LFA1-LIFNR.
    MOVE BSAK1-LIFNR TO LFB1-LIFNR.
    MOVE BSAK1-LIFNR TO LFBK-LIFNR.


    PERFORM READ_ALT_PAYEE   USING    P_BSAK
                             CHANGING LFA1
                                      LFB1.
    IF GV_ALT_LIFNR IS NOT INITIAL.
      PERFORM READ_TABLE_LFBK  USING  P_BSAK
                               CHANGING LFBK BNKA.

      PERFORM READ_TABLE_ADRC  USING  LFA1-ADRNR
                               CHANGING ADRC.
    ENDIF.   " GV_ALT_LIFNR
  ENDIF.     " bsec is initial


  PERFORM READ_TABLE_BNKA  USING    LFBK
                           CHANGING BNKA.
  PERFORM READ_TABLE_T012  USING    P_BSAK
                           CHANGING T012.
  PERFORM READ_TABLE_T012K USING    P_BSAK
                           CHANGING T012K.
  PERFORM READ_TABLE_BSEG  USING    P_BSAK
                           CHANGING BSEG.
  PERFORM READ_TABLE_WITH_ITEM  USING    P_BSAK
                           CHANGING WITH_ITEM.

  PERFORM READ_CHANGED_DATA USING P_BSAK.

ENDFORM.                    " read_header_tables

*&---------------------------------------------------------------------*
*&      Form  READ_CUSTOM_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CUSTOM_TABLES .

  PERFORM READ_TABLE_PRINT_LOCATION USING LFA1
                          CHANGING ZAXIS_TAB_PAYLOC.

  ""-----------reading field seperator-----------------
  READ TABLE G_TAB_ZAXIS_TAB_SEP INTO ZAXIS_TAB_SEP
                                 WITH KEY BUKRA = P_BUKRS.


  ""-----------reading house bank table for corpcode and file start
  READ TABLE GIT_TAB_HB INTO ZAXIS_TAB_HB WITH KEY BUKRS = P_BUKRS
                                                   HBKID = S_HBKID-LOW.

ENDFORM.                    " READ_CUSTOM_TABLES

*&---------------------------------------------------------------------*
*&      Form  FIND_PAYMENT_METHOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_PAYMENT_METHOD .
  CLEAR : WA_CONVERS,F_PAY_MTD_ERROR.
  CLEAR : WA_CONVERS,WA_PAY_MTD.

*&---------------------------------------------------------------------*
*-----------PAYMENT SEGRIGATION Based on Selection screen--------------*
*----------------------------------------------------------------------*
  IF P_PAY IS NOT INITIAL. "(TAKING THE PAYMENT METHOD FROM SELCTION SCREEN)

    IF P_AUTO = ABAP_TRUE. "'X'.
      PERFORM SELECTION_SCREEN_PAYMENT USING  'AUTO'
                                               P_PAY.
    ELSE.
      PERFORM SELECTION_SCREEN_PAYMENT USING  'MANU'
                                            P_PAY.
    ENDIF.

   ELSEIF WA_SEL_CHANGE-PAYMENT_TYPE IS NOT INITIAL AND F_EDIT_PAY_MTD EQ 'Y'.
    IF P_AUTO = ABAP_TRUE. "'X'.
      PERFORM SELECTION_SCREEN_PAYMENT USING  'AUTO'
                                               WA_SEL_CHANGE-PAYMENT_TYPE.

    ELSE.
      PERFORM SELECTION_SCREEN_PAYMENT USING  'MANU'
                                              WA_SEL_CHANGE-PAYMENT_TYPE.

    ENDIF.

  ELSEIF P_AUTO = ABAP_TRUE. "'X'.
*&---------------------------------------------------------------------*
*-----------PAYMENT SEGRIGATION FOR AUTOMATIC PAYMENTS-----------------*
*----------------------------------------------------------------------*

    READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FIELD2'
                                                    OLDVALUE  = 'F110'.
    IF WA_CONVERS-NEWVALUE = 'AUTO_PAY' AND REGUH-RZAWE IS NOT INITIAL.

      PERFORM AUTOMATIC_PAYMENT_SEGRIGATION USING 'AUTO'
                                                  REGUH-RZAWE.
    ELSE.
      PERFORM MANUAL_PAYMENT_SEGRIGATION USING 'AUTO'.
    ENDIF.  "   wa_tab_convers_tmp-newvalue = 'AUTO'.

*&---------------------------------------------------------------------*
*-----------PAYMENT SEGRIGATION FOR MANUAL PAYMENTS-----------------*
*----------------------------------------------------------------------*

  ELSE.

    CLEAR : WA_PAY_MTD ,WA_CONVERS.
    READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FIELD2'
                                                      OLDVALUE = 'F-53'.
    IF WA_CONVERS-NEWVALUE = 'AUTO_PAY' AND BSAK1-ZLSCH IS NOT INITIAL .

      PERFORM AUTOMATIC_PAYMENT_SEGRIGATION USING 'MANU'
                                                  BSAK1-ZLSCH.
    ELSE.
      PERFORM MANUAL_PAYMENT_SEGRIGATION USING 'MANU'.
    ENDIF.  " auto and maunal segrigation


  ENDIF."P_PAY is not initial.

  CLEAR : WA_CONVERS,WA_PAY_MTD.
ENDFORM.                    " FIND_PAYMENT_METHOD

*&---------------------------------------------------------------------*
*&      Form  READ_ACC_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ACC_DETAILS .

*--------fetch the bank acc no field from MAPPING TABLE
  IF WA_SEL_CHANGE IS NOT INITIAL AND F_EDIT_BANK EQ 'Y'..

    GV_ACC_NO = WA_SEL_CHANGE-ACC_NO.
    GV_IFSC_CODE = WA_SEL_CHANGE-IFSC.

  ELSE.


    CLEAR  : WA_CONVERS.
    READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY BANK_CODE   = C_BANK_CODE
                                                   RECORD_TYPE = 'H'
                                                   FIELDNAME   = 'VEND'
                                                   OLDVALUE    = 'ACC_NO'.

    IF  WA_CONVERS-NEWVALUE IS NOT INITIAL.
      ASSIGN (WA_CONVERS-NEWVALUE) TO <FS_ACCNO>.
    ELSE .
      WA_CONVERS-NEWVALUE = 'LFBK-BANKN'.
      ASSIGN (WA_CONVERS-NEWVALUE) TO <FS_ACCNO>.
    ENDIF.
    GV_ACC_NO = <FS_ACCNO>.

*-----fetch the bank ifsc code field from MAPPING TABLE

    CLEAR  : WA_CONVERS.
    READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY BANK_CODE   = C_BANK_CODE
                                                    RECORD_TYPE = 'H'
                                                    FIELDNAME   = 'VEND'
                                                    OLDVALUE    = 'IFSC_CODE'.

    IF  WA_CONVERS-NEWVALUE IS NOT INITIAL.
      ASSIGN (WA_CONVERS-NEWVALUE) TO <FS_IFSCCODE>.
    ELSE .
      WA_CONVERS-NEWVALUE = 'LFBK-BKREF'.
      ASSIGN (WA_CONVERS-NEWVALUE) TO <FS_IFSCCODE>.
    ENDIF.
    GV_IFSC_CODE = <FS_IFSCCODE>.


*-------fetch the rtgs time validation from MAPPING TABLE

    CLEAR : WA_CONVERS.
    READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FIELD2'
                                                    OLDVALUE  = 'RTGS_TIME_VAL'.
    RTGS_TIME_VAL = WA_CONVERS-NEWVALUE.

*-------fetch the rtgs cut of time from MAPPING TABLE

    CLEAR : WA_CONVERS.
    READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FIELD2'
                                                    OLDVALUE  = 'RTGS_CUT_TIME'.
    RTGS_CUT_TIME = WA_CONVERS-NEWVALUE.

*------fetch the bank rtgs amount limit from MAPPING TABLE

    CLEAR : WA_CONVERS.
    READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FIELD2'
                                                    OLDVALUE  = 'RT'.
    RTGS_AMT_LIMIT = WA_CONVERS-NEWVALUE.

*------fetch the bank IMPS amount limit from MAPPING TABLE

    CLEAR : WA_CONVERS.
    READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FIELD2'
                                                    OLDVALUE  = 'IMPS_AMT_LIMIT'.
    IMPS_AMT_LIMIT = WA_CONVERS-NEWVALUE.
  ENDIF.
ENDFORM.                    " READ_ACC_DETAILS
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_PAYMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0669   text
*      -->P_P_PAY  text
*----------------------------------------------------------------------*
FORM SELECTION_SCREEN_PAYMENT  USING    P_IND
                                        P_P_PAY.

  CASE P_P_PAY.    " SELECTION SCREEN PAYMENT.

    WHEN C_TRF.  "For Transfers.
      PERFORM MANUAL_BANK_TRANSFER USING P_IND.

    WHEN C_NEFT.
      PERFORM MANUAL_BANK_TRANSFER_NEFT USING P_IND.

    WHEN C_RTGS.
      PERFORM MANUAL_BANK_TRANSFER_RTGS USING P_IND.

    WHEN C_FT.
      PERFORM MANUAL_BANK_TRANSFER_FT USING P_IND.

    WHEN C_IMPS.
      PERFORM MANUAL_BANK_TRANSFER_IMPS USING P_IND.

    WHEN OTHERS.
      PERFORM FIND_PAYMENT USING  P_IND
                                  P_P_PAY
                                    ' '.
  ENDCASE.

ENDFORM.                    " SELECTION_SCREEN_PAYMENT

*&---------------------------------------------------------------------*
*&      Form  AUTOMATIC_PAYMENT_SEGRIGATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0514   text
*      -->P_REGUH_RZAWE  text
*----------------------------------------------------------------------*
FORM AUTOMATIC_PAYMENT_SEGRIGATION  USING    P_IND
                                             SAP_PAY_MTD.
  CLEAR : WA_PAY_MTD.
  READ TABLE GIT_PAY_MTD INTO WA_PAY_MTD WITH KEY BUKRS = P_BUKRS
                                            SAP_PAY_MTD = SAP_PAY_MTD
                                            MANU_AUTO   = P_IND.
  IF SY-SUBRC EQ 0.
    CASE WA_PAY_MTD-PAYMENT_TYPE.

      WHEN C_TRF.  "For Transfers.
        PERFORM MANUAL_BANK_TRANSFER USING P_IND.

      WHEN C_NEFT.
        PERFORM MANUAL_BANK_TRANSFER_NEFT USING P_IND.

      WHEN C_RTGS.
        PERFORM MANUAL_BANK_TRANSFER_RTGS USING P_IND.

      WHEN C_FT.
        PERFORM MANUAL_BANK_TRANSFER_FT USING P_IND.

      WHEN C_IMPS.
        PERFORM MANUAL_BANK_TRANSFER_IMPS USING P_IND.

      WHEN OTHERS.
        PERFORM FIND_PAYMENT USING  P_IND
                                    WA_PAY_MTD-PAYMENT_TYPE
                                     WA_PAY_MTD-PRODUCT_CODE.
    ENDCASE.
  ELSE.   "if payment method was not maintaines in ztable then trough error.
    F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
    PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                 TEXT-E16     " Corresponding Payment method was not maintained in Payment Method table please check
                                 C_RED_COLOUR.
    PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                 TEXT-E16     " Corresponding Payment method was not maintained in Payment Method table please check
                                 C_RED_COLOUR.
  ENDIF.

ENDFORM.                    " AUTOMATIC_PAYMENT_SEGRIGATION
*&---------------------------------------------------------------------*
*&      Form  MANUAL_PAYMENT_SEGRIGATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MANUAL_PAYMENT_SEGRIGATION USING P_IND.

***---PAYMENT LOGIC BASED ON CHEQUE AND AC NO AND IFSC CODE.--------*******
  IF PAYR-CHECT IS NOT INITIAL.  " CHEQUE PAYMENT
    PERFORM FIND_PAYMENT USING P_IND
                               C_CC
                               C_CC_PRD_CODE.

  ELSEIF GV_ACC_NO IS NOT INITIAL AND GV_IFSC_CODE IS NOT INITIAL .  " NOT CHEQUE PAYMENT electronic pay

    PERFORM MANUAL_BANK_TRANSFER USING P_IND.

  ELSE. " dd payment
    PERFORM FIND_PAYMENT USING P_IND
                               C_DD
                               C_DD_PRD_CODE.

  ENDIF. " PAYR TABLE
ENDFORM.                    " MANUAL_PAYMENT_SEGRIGATION


*&---------------------------------------------------------------------*
*&      Form  MANUAL_BANK_TRANSFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MANUAL_BANK_TRANSFER USING P_IND .

  CLEAR : WA_PAY_MTD.
  IF GV_ACC_NO IS NOT INITIAL AND GV_IFSC_CODE IS NOT INITIAL .
    IF GV_IFSC_CODE+0(4) EQ 'UTIB'.

      PERFORM FIND_PAYMENT USING P_IND
                                 C_FT
                                 C_FT_PRD_CODE.
    ELSE.  "SEGRIGATE RTGS AND NEFT PAYMENTS.
      CLEAR : WA_PAY_MTD.
      READ TABLE GIT_PAY_MTD INTO WA_PAY_MTD WITH KEY BUKRS        = P_BUKRS
                                                      PAYMENT_TYPE = C_RTGS
                                                      MANU_AUTO    = P_IND.
      IF SY-SUBRC = 0.
        "Commented by SPLABAP during code remediation
*        IF BSAK1-DMBTR GE RTGS_AMT_LIMIT.
        IF BSAK1-DMBTR GE conv DMBTR( RTGS_AMT_LIMIT ).
          "Added by SPLABAP during code remediation

          IF RTGS_TIME_VAL = 'Y' AND SY-UZEIT GE RTGS_CUT_TIME. "CHECK THE SYSTEM TIME TIME AFTER 3.30 NO RTGS REQUIRED.

            PERFORM FIND_PAYMENT USING P_IND
                                       C_NEFT
                                       C_NEFT_PRD_CODE.

          ELSE.
            PERFORM FIND_PAYMENT USING P_IND
                                       C_RTGS
                                       C_RTGS_PRD_CODE.
          ENDIF.

        ELSE .   "AMOUNT IS LESS THAN RTGS LIMIT.

          PERFORM FIND_PAYMENT USING P_IND
                                     C_NEFT
                                     C_NEFT_PRD_CODE.

        ENDIF. " DMBTR
      ELSE .    "NO RTGS MADE DIRECT NEFT PAYMENT.

        PERFORM FIND_PAYMENT USING P_IND
                                   C_NEFT
                                   C_NEFT_PRD_CODE.

      ENDIF.   "SY-SUBRC.
    ENDIF. " IFSC CODE
  ELSE.   "BANK DETAILS ARE NOT PROPER THEN THROUGH ERROR MESSAGE.
    F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
    GV_PRODUCT_CODE = C_NEFT_PRD_CODE.
    PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                 TEXT-E15     " Vendor Bank Account Number or IFSC Code was not found in vendor master please maintain required details in Vendor Master
                                 C_RED_COLOUR.
    PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                 TEXT-E15     " Vendor Bank Account Number or IFSC Code was not found in vendor master please maintain required details in Vendor Master
                                 C_RED_COLOUR.
  ENDIF.
ENDFORM.                    " MANUAL_BANK_TRANSFER


*&---------------------------------------------------------------------*
*&      Form  MANUAL_BANK_TRANSFER_NEFT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0636   text
*----------------------------------------------------------------------*
FORM MANUAL_BANK_TRANSFER_NEFT  USING P_IND.

  IF GV_ACC_NO IS NOT INITIAL AND GV_IFSC_CODE IS NOT INITIAL .
    IF GV_IFSC_CODE+0(4) EQ 'UTIB'.

      PERFORM FIND_PAYMENT USING P_IND
                                 C_FT
                                 C_FT_PRD_CODE.
    ELSE.  "NEFT PAYMENTS.
      PERFORM FIND_PAYMENT USING P_IND
                                 C_NEFT
                                 C_NEFT_PRD_CODE.

    ENDIF. " IFSC CODE
  ELSE.   "BANK DETAILS ARE NOT PROPER THEN THROUGH ERROR MESSAGE.
    F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
    GV_PRODUCT_CODE = C_NEFT_PRD_CODE.
    PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                 TEXT-E15     " Vendor Bank Account Number or IFSC Code was not found in vendor master please maintain required details in Vendor Master
                                 C_RED_COLOUR.
    PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                 TEXT-E15     " Vendor Bank Account Number or IFSC Code was not found in vendor master please maintain required details in Vendor Master
                                 C_RED_COLOUR.
  ENDIF.

ENDFORM.                    " MANUAL_BANK_TRANSFER_NEFT

*&---------------------------------------------------------------------*
*&      Form  MANUAL_BANK_TRANSFER_RTGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0636   text
*----------------------------------------------------------------------*
FORM MANUAL_BANK_TRANSFER_RTGS  USING P_IND.


  IF GV_ACC_NO IS NOT INITIAL AND GV_IFSC_CODE IS NOT INITIAL .
    IF GV_IFSC_CODE+0(4) EQ 'UTIB'.

      PERFORM FIND_PAYMENT USING P_IND
                                 C_FT
                                 C_FT_PRD_CODE.
    ELSE.  "RTGS PAYMENTS.
"Commented by SPLABAP during code remediation
*      IF BSAK1-DMBTR GE RTGS_AMT_LIMIT.
      IF BSAK1-DMBTR GE conv DMBTR( RTGS_AMT_LIMIT ).
        "Added by SPLABAP during code remediation

        IF RTGS_TIME_VAL = 'Y' AND SY-UZEIT GE RTGS_CUT_TIME. "CHECK THE SYSTEM TIME TIME AFTER 3.30 NO RTGS REQUIRED.

          PERFORM FIND_PAYMENT USING P_IND
                                     C_NEFT
                                     C_NEFT_PRD_CODE.

        ELSE.
          PERFORM FIND_PAYMENT USING P_IND
                                     C_RTGS
                                     C_RTGS_PRD_CODE.
        ENDIF.
      ELSE.
        F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
        GV_PRODUCT_CODE = C_RTGS_PRD_CODE.
        PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                     TEXT-E31     " Payment amount not reached the RTGS Limit
                                     C_RED_COLOUR.
        PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                     TEXT-E31     " Payment amount not reached the RTGS Limit
                                     C_RED_COLOUR.
      ENDIF.  "BSAK1-DMBTR
    ENDIF. " IFSC CODE
  ELSE.   "BANK DETAILS ARE NOT PROPER THEN THROUGH ERROR MESSAGE.
    F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
    GV_PRODUCT_CODE = C_RTGS_PRD_CODE.
    PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                 TEXT-E15     " Vendor Bank Account Number or IFSC Code was not found in vendor master please maintain required details in Vendor Master
                                 C_RED_COLOUR.
    PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                 TEXT-E15     " Vendor Bank Account Number or IFSC Code was not found in vendor master please maintain required details in Vendor Master
                                 C_RED_COLOUR.
  ENDIF.

ENDFORM.                    " MANUAL_BANK_TRANSFER_RTGS

*&---------------------------------------------------------------------*
*&      Form  Manual FT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0636   text
*----------------------------------------------------------------------*
FORM MANUAL_BANK_TRANSFER_FT  USING P_IND.


  IF GV_ACC_NO IS NOT INITIAL AND GV_IFSC_CODE IS NOT INITIAL .
    IF GV_IFSC_CODE+0(4) EQ 'UTIB'.

      PERFORM FIND_PAYMENT USING P_IND
                                 C_FT
                                 C_FT_PRD_CODE.
    ELSE.  "error message.
      F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
      GV_PRODUCT_CODE = C_FT_PRD_CODE.
      PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                   TEXT-E30     " Bank To Bank Transfer not possible with different bank Please select NEFT/RTGS
                                   C_RED_COLOUR.
      PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                   TEXT-E30     " Bank To Bank Transfer not possible with different bank Please select NEFT/RTGS
                                   C_RED_COLOUR.

    ENDIF. " IFSC CODE
  ELSE.   "BANK DETAILS ARE NOT PROPER THEN THROUGH ERROR MESSAGE.
    F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
    GV_PRODUCT_CODE = C_FT_PRD_CODE.
    PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                 TEXT-E15     " Vendor Bank Account Number or IFSC Code was not found in vendor master please maintain required details in Vendor Master
                                 C_RED_COLOUR.
    PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                 TEXT-E15     " Vendor Bank Account Number or IFSC Code was not found in vendor master please maintain required details in Vendor Master
                                 C_RED_COLOUR.
  ENDIF.

ENDFORM.                    " Manual FT

*&---------------------------------------------------------------------*
*&      Form  MANUAL_BANK_TRANSFER_IMPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IND  text
*----------------------------------------------------------------------*
FORM MANUAL_BANK_TRANSFER_IMPS  USING    P_P_IND.

*  IF BSAK1-DMBTR LE IMPS_AMT_LIMIT.
  IF BSAK1-DMBTR LE conv DMBTR( IMPS_AMT_LIMIT ).
    "Added by SPLABAP during code remediation
    PERFORM FIND_PAYMENT USING P_P_IND
                               C_IMPS
                               C_IMPS_PRD_CODE.
  ELSE.
    F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
    GV_PRODUCT_CODE = C_IMPS_PRD_CODE.
    PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                 TEXT-E36     " IMPS Amount limit exceded for this payment please select RTGS/NEFT/IFT
                                 C_RED_COLOUR.
    PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                 TEXT-E36     " IMPS Amount limit exceded for this payment please select RTGS/NEFT/IFT
                                 C_RED_COLOUR.

  ENDIF.

ENDFORM.                    " MANUAL_BANK_TRANSFER_IMPS

*&---------------------------------------------------------------------*
*&      Form  FIND_PAYMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IND  text
*      -->P_C_FT  text
*      -->P_C_FT_PRD_CODE  text
*----------------------------------------------------------------------*
FORM FIND_PAYMENT  USING    P_P_IND
                            P_AXIS_PAY
                            P_PRD_CODE.

  CLEAR : WA_PAY_MTD1.
  READ TABLE GIT_PAY_MTD INTO WA_PAY_MTD1 WITH KEY BUKRS        = P_BUKRS
                                                  PAYMENT_TYPE = P_AXIS_PAY
                                                  MANU_AUTO    = P_P_IND.
  IF SY-SUBRC = 0.
    GV_PAYMENT      = P_AXIS_PAY.
    GV_PRODUCT_CODE = WA_PAY_MTD1-PRODUCT_CODE.
  ELSE.
    F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
    GV_PRODUCT_CODE = WA_PAY_MTD1-PRODUCT_CODE.
    PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                 TEXT-E16     " Corresponding Payment method was not maintained in Payment Method table please check
                                 C_RED_COLOUR.
    PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                 TEXT-E16     " Corresponding Payment method was not maintained in Payment Method table please check
                                 C_RED_COLOUR.
  ENDIF.
ENDFORM.                    " FIND_PAYMENT

*&---------------------------------------------------------------------*
*&      Form  FIND_GLOBAL_CUSTOME_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_GLOBAL_CUSTOME_DATA .

  ""---------FIND VENDOR NAME-------------
  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY BANK_CODE   = C_BANK_CODE
                                                  RECORD_TYPE = 'H'
                                                  FIELDNAME   = 'VEND'
                                                  OLDVALUE    = 'VEN_NAME'.
  IF SY-SUBRC = 0 AND WA_CONVERS-NEWVALUE IS NOT INITIAL.
    ASSIGN (WA_CONVERS-NEWVALUE) TO <FS_VEN_NAME>.
  ELSE .
    WA_CONVERS-NEWVALUE = 'ADRC-NAME1'.
    ASSIGN (WA_CONVERS-NEWVALUE) TO <FS_VEN_NAME>.
  ENDIF.
  GV_VENDOR_NAME = <FS_VEN_NAME>.

  IF GV_VENDOR_NAME IS INITIAL.
    GV_VENDOR_NAME = ADRC-NAME1.
  ENDIF.

  ""----APPEND ALL THE FLOBAL DATA TO HEDDER STRUCTURE TO FURTHER USE-----
  GV_CORP_CODE           = ZAXIS_TAB_HB-CORP_CODE.
  HEADER-CORP_CODE       = ZAXIS_TAB_HB-CORP_CODE.
  HEADER-VENDOR_NAME     = GV_VENDOR_NAME.
  HEADER-ACC_NO          = GV_ACC_NO.
  HEADER-IFSC_CODE       = GV_IFSC_CODE.
  HEADER-PRODUCT_CODE    = GV_PRODUCT_CODE.
  HEADER-PAYMENT         = GV_PAYMENT.

  ""----IF PAYMENT METHOD WAS TRANSFER THEN NO NEED OF PRINT AND PAY LOCATIONS.
  IF GV_PAYMENT NE C_CC AND GV_PAYMENT NE C_DD.
    CLEAR : ZAXIS_TAB_PAYLOC.
  ELSEIF GV_PAYMENT EQ C_CC.
    CLEAR : ZAXIS_TAB_PAYLOC-PAY_LOC.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Check the Ifsc Code Length for Transfers AND SPL CHARECTERS
*&---------------------------------------------------------------------*
  IF GV_PAYMENT NE 'CC' AND GV_PAYMENT NE 'DD'.
    DATA : LV_STR_LEN TYPE I.
    DATA :  L_VALID(100),
            L_NOT_VALID(100).

***-------"REMOVE SPL CHARECTERS IN ACC NO AND IFSC CODE FIELD----------*

    CLEAR : L_VALID,L_NOT_VALID.
    L_VALID  = TEXT-C06.   "'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'.
    PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                             L_NOT_VALID
                                    CHANGING GV_IFSC_CODE.
    CONDENSE GV_IFSC_CODE NO-GAPS.

    CLEAR : L_VALID,L_NOT_VALID.
    L_VALID  = TEXT-C07.   "0 123456789
    PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                             L_NOT_VALID
                                    CHANGING GV_ACC_NO.
    CLEAR : L_VALID,L_NOT_VALID.

***-----------IFSC CODE LENGHT CHECKING-----------------------------***

    LV_STR_LEN = STRLEN( GV_IFSC_CODE ).
    IF LV_STR_LEN NE 11.
      F_PAY_MTD_ERROR = ABAP_TRUE. "'X'.
      PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                   TEXT-E25     " vendor IFSC Code might be wrong, it should be 11 chreceters please check
                                   C_RED_COLOUR.
      PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                     TEXT-E25     " vendor IFSC Code might be wrong, it should be 11 chreceters please check
                                     C_RED_COLOUR.
    ENDIF.
  ENDIF.

ENDFORM.                    " FIND_GLOBAL_CUSTOME_DATA


*&---------------------------------------------------------------------*
*&      Form  extract_fields_from_mapping
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->REC_TYPE   text
*      -->NO_FIELDS  text
*      -->FILETYPE   text
*      -->DELIMITER  text
*----------------------------------------------------------------------*
FORM EXTRACT_FIELDS_FROM_MAPPING  USING    REC_TYPE
                                           NO_FIELDS
                                           DELIMITER.

  DATA :          L_FIELD(30),
                  L_FIELDNO(10),
                  L_OUT_FIELD(100),
                  L_OFFSET TYPE I,
                  L_LEN TYPE I,
                  MAXFIELD(30),
                  L_VAL1(255),
                  L_VAL2(255).


  L_FIELDNO = NO_FIELDS.
  CONDENSE L_FIELDNO.
  CLEAR MAXFIELD.
  CONCATENATE 'FIELD' L_FIELDNO INTO MAXFIELD.


  DO NO_FIELDS TIMES.

    L_FIELDNO = SY-INDEX.
    CONDENSE L_FIELDNO.

    CLEAR L_FIELD.
    CONCATENATE 'FIELD' L_FIELDNO INTO L_FIELD.
    CLEAR L_OUT_FIELD.

    ASSIGN COMPONENT SY-INDEX OF STRUCTURE <FS_OUTPUTSTRUC> TO <FS_OUT>.

    READ TABLE G_TAB_ZAXIS_TAB_FLDLENG INTO WA_ZAXIS_TAB_FLDLENG
                                   WITH KEY RECORD_TYPE = REC_TYPE
                                            FIELDNAME   = L_FIELD.
    CHECK SY-SUBRC EQ 0.
    LOOP AT G_TAB_ZAXIS_TAB_MAP INTO WA_ZAXIS_TAB_MAP
                                     WHERE RECORD_TYPE = REC_TYPE
                                     AND FIELDNAME   = L_FIELD.

      CLEAR L_OUT_FIELD.

      IF WA_ZAXIS_TAB_MAP-CONSTANTVALUE IS NOT INITIAL .
        <FS_OUT> = WA_ZAXIS_TAB_MAP-CONSTANTVALUE .
        G_FIELD_CHECK = WA_ZAXIS_TAB_MAP-CONSTANTVALUE .
        EXIT. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
      ENDIF.

      CONCATENATE <FS_OUT> L_OUT_FIELD INTO <FS_OUT> SEPARATED BY SPACE.
      SHIFT <FS_OUT> LEFT DELETING LEADING SPACE.

    ENDLOOP.
*&---------------------------------------------------------------------*
*       EXTRACT EACH FILED VALUE.
*----------------------------------------------------------------------*
    CASE REC_TYPE.

      WHEN 'H'.                                                "Header

        PERFORM ZAXIS_036_HDR USING  L_FIELD
                                     BSAK1  REGUH  PAYR
                                     LFA1  LFB1   ADRC
                                     T001
                                     BNKA  LFBK   T012  T012K
                                     BSEC  BSEG ZAXIS_TAB_PAYLOC
                                     ZAXIS_STR_HEADER.

      WHEN 'D'.                                                "Detail

        PERFORM ZAXIS_036_DTL   USING L_FIELD
                                      BSAK  REGUH  PAYR
                                      LFA1  LFB1   ADRC
                                      T001
                                      BNKA  LFBK   T012  T012K
                                      BSEC  BSEG
                                      REGUP *BSAK
                                      ZAXIS_STR_HEADER ZAXIS_TAB_SEP
                                      ZAXIS_STR_DETAIL.

    ENDCASE.

*&---------------------------------------------------------------------*
*       FIELD VALIDATIONS FOR SPL CHAR ETC.
*----------------------------------------------------------------------*

    IF <FS_OUT> NE ' '.

      PERFORM FIELD_VALIDATION USING REC_TYPE
                                     L_FIELD
                            CHANGING <FS_OUT>.

    ENDIF.
*&---------------------------------------------------------------------*
*       MANDATORY FIELD VALIDATIONS FOR ALL FIELDS.
*----------------------------------------------------------------------*

    IF <FS_OUT> = ' ' AND WA_ZAXIS_TAB_MAP-MANDATORY = 'X'.

      IF ( GV_PAYMENT = 'DD' OR GV_PAYMENT = 'CC' ) AND
        ( WA_ZAXIS_TAB_MAP-FIELDNAME = 'FIELD11' OR WA_ZAXIS_TAB_MAP-FIELDNAME = 'FIELD19' ).

      ELSE.
        READ TABLE G_TAB_ZAXIS_TAB_FLDLENG INTO WA_ZAXIS_TAB_FLDLENG
                           WITH KEY FIELDNAME   = WA_ZAXIS_TAB_MAP-FIELDNAME
                                    RECORD_TYPE = WA_ZAXIS_TAB_MAP-RECORD_TYPE.

        IF SY-SUBRC = 0.
          CLEAR : GV_TEXT,GV_TEXT1.
          F_FIELD_EMPTY = 'X'.

          IF WA_ZAXIS_TAB_MAP-RECORD_TYPE = 'D'.
            GV_TEXT = TEXT-021.
          ELSEIF WA_ZAXIS_TAB_MAP-RECORD_TYPE = 'H'.
            GV_TEXT = TEXT-022.
          ENDIF.
          CONCATENATE 'Data Missing :' GV_TEXT 'Field Number -'
                      WA_ZAXIS_TAB_MAP-FIELDNAME
                      WA_ZAXIS_TAB_FLDLENG-MEANING
                      INTO GV_TEXT1 SEPARATED BY SPACE.

          PERFORM APPEND_DISPLAY USING TEXT-E01     " ERROR
                                       GV_TEXT1
                                       C_RED_COLOUR.

          PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
                                         GV_TEXT1
                                         C_RED_COLOUR.
        ENDIF.
      ENDIF.
    ENDIF.


*&---------------------------------------------------------------------*
*       FIELD LENGTH CHECKING FOR ALL FIELDS.
*----------------------------------------------------------------------*

* DATA'S TO OUTPUT STRUCTURE

    L_OFFSET = STRLEN( L_WA_OUTPUTREC ).
    L_LEN    = STRLEN( <FS_OUT> ).

    IF L_LEN <= WA_ZAXIS_TAB_FLDLENG-MAXLENGTH.

      WRITE <FS_OUT> TO L_WA_OUTPUTREC+L_OFFSET(L_LEN).

    ELSE.

      L_LEN = WA_ZAXIS_TAB_FLDLENG-MAXLENGTH .
      <FS_OUT> = <FS_OUT>(L_LEN).
      L_LEN = STRLEN( <FS_OUT> ).
      WRITE <FS_OUT> TO L_WA_OUTPUTREC+L_OFFSET(L_LEN)
      RIGHT-JUSTIFIED.

    ENDIF.


*&---------------------------------------------------------------------*
*       ADDING DELIMITER OPTION FOR EACH FIELDS.
*----------------------------------------------------------------------*


    IF L_FIELD NE MAXFIELD.

      L_OFFSET = STRLEN( L_WA_OUTPUTREC ).
      WRITE DELIMITER TO L_WA_OUTPUTREC+L_OFFSET(1).

    ENDIF.

  ENDDO.

ENDFORM.                    " extract_fields_from_mapping


*&---------------------------------------------------------------------*
*&      Form  read_detail_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BSAK     text
*----------------------------------------------------------------------*
FORM READ_DETAIL_TABLES  USING    P_BSAK TYPE BSAK.

  CLEAR REGUP.
  READ TABLE G_TAB_REGUP INTO REGUP WITH KEY LAUFD = REGUH-LAUFD
                                        LAUFI = REGUH-LAUFI
                                        XVORL = ' '
                                        LIFNR = REGUH-LIFNR
                                        KUNNR = REGUH-KUNNR
                                        EMPFG = REGUH-EMPFG
                                        VBLNR = REGUH-VBLNR
                                        BELNR = P_BSAK-BELNR
                                        BUZEI = P_BSAK-BUZEI.

ENDFORM.                    " read_detail_tables


*&---------------------------------------------------------------------*
*&      Form  FIELD_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_REC_TYPE  text
*      -->P_L_FIELD  text
*      <--P_<FS_OUT>  text
*----------------------------------------------------------------------*
FORM FIELD_VALIDATION  USING    P_REC_TYPE
                                P_L_FIELD
                       CHANGING P_FS_OUT.

  DATA :  L_VALID(100),
          L_NOT_VALID(100).
  CLEAR : L_VALID,L_NOT_VALID.

  CASE P_REC_TYPE.
*&---------------------------------------------------------------------*
*       for headder fields
*----------------------------------------------------------------------*
    WHEN 'H'.
**---------"for all fields we need to remove these  4 char { ',",`,^ }---

      CLEAR : L_VALID,L_NOT_VALID.
      L_NOT_VALID = TEXT-C01.     "    '"`^$%;~
      PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                               L_NOT_VALID
                                      CHANGING P_FS_OUT.
      CLEAR : L_VALID,L_NOT_VALID.

      CASE P_L_FIELD.    " FOR EACH FIELD

        WHEN 'FIELD1'.
        WHEN 'FIELD2'.

*&---------------------------------------------------------------------*
*       for CORP Code
*       L_VALID  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'.
*----------------------------------------------------------------------*
        WHEN 'FIELD3'.
          L_VALID = TEXT-C06.     " alpha numerics
          PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                                   L_NOT_VALID
                                          CHANGING P_FS_OUT.
          CONDENSE P_FS_OUT NO-GAPS.
*&---------------------------------------------------------------------*
*       for Customer Reference Number
*       L_VALID = TEXT-C02.     " alpha numerics and -/\
*----------------------------------------------------------------------*
        WHEN 'FIELD4'.    "Customer Reference Number


          L_VALID = TEXT-C02.     " alpha numerics and _-/\
          PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                                   L_NOT_VALID
                                          CHANGING P_FS_OUT.
          CONDENSE P_FS_OUT NO-GAPS.

*&---------------------------------------------------------------------*
*       for Debit Account Number
*       L_VALID     = '0123456789'.
*----------------------------------------------------------------------*

        WHEN 'FIELD5'.    "Debit Account Number

          L_VALID  = TEXT-C03.
          PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                                   L_NOT_VALID
                                          CHANGING P_FS_OUT.
          CONDENSE P_FS_OUT NO-GAPS.


        WHEN 'FIELD6'.      "posting date

*&---------------------------------------------------------------------*
*       for Transaction Currency
*       L_VALID  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.
*----------------------------------------------------------------------*
        WHEN 'FIELD7'.    "Transaction Currency
          L_VALID  = TEXT-C04.
          PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                                   L_NOT_VALID
                                          CHANGING P_FS_OUT.
          CONDENSE P_FS_OUT NO-GAPS.

        WHEN 'FIELD8'.    "Transaction Amount
*&---------------------------------------------------------------------*
*       for Beneficiary Name
*       L_VALID = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz.#&@0123456789'.
*----------------------------------------------------------------------*
        WHEN 'FIELD9'.    "Beneficiary Name

          IF GV_PAYMENT NE 'CC' AND GV_PAYMENT NE 'DD'.

            L_VALID = TEXT-C05.
            PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                                     L_NOT_VALID
                                            CHANGING P_FS_OUT.
          ENDIF.

*&---------------------------------------------------------------------*
*       for Beneficiary Code / Vendor Code
*       L_VALID  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'.
*----------------------------------------------------------------------*
        WHEN 'FIELD10'.   "Beneficiary Code / Vendor Code

          L_VALID  = TEXT-C06.
          PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                                   L_NOT_VALID
                                          CHANGING P_FS_OUT.
          CONDENSE P_FS_OUT NO-GAPS.

*&---------------------------------------------------------------------*
*       for Beneficiary Account Number
*----------------------------------------------------------------------*
        WHEN 'FIELD11'.   "Beneficiary Account Number
          L_VALID  = TEXT-C07.
          PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                                   L_NOT_VALID
                                          CHANGING P_FS_OUT.

        WHEN 'FIELD12'.       "Benefciary Account Type

*&---------------------------------------------------------------------*
*       for ADDRESS FILEDS.(13,14,15,16,17,18)
*       L_VALID  = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789/,_-@#&?:;,.|\/-_$%()[]{}
*----------------------------------------------------------------------*
        WHEN 'FIELD13' OR 'FIELD14' OR 'FIELD15' OR 'FIELD16' OR 'FIELD17' OR 'FIELD18'.
          L_VALID  = TEXT-C08.
          PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                                   L_NOT_VALID
                                          CHANGING P_FS_OUT.

*&---------------------------------------------------------------------*
*       for Beneficiary IFSC Code
*       L_VALID  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'.
*----------------------------------------------------------------------*
        WHEN 'FIELD19'.   "Beneficiary IFSC Code
          L_VALID  = TEXT-C06.
          PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                                   L_NOT_VALID
                                          CHANGING P_FS_OUT.
          CONDENSE P_FS_OUT NO-GAPS.

        WHEN 'FIELD20'.
        WHEN 'FIELD21'.
        WHEN 'FIELD22'.
        WHEN 'FIELD23'.
        WHEN 'FIELD24'.
        WHEN 'FIELD25'.
        WHEN 'FIELD26'.
        WHEN 'FIELD27'.
        WHEN 'FIELD28'.
        WHEN 'FIELD29'.
        WHEN 'FIELD30'.
        WHEN 'FIELD31'.
        WHEN 'FIELD32'.                                     "Extra 1
        WHEN 'FIELD33'.                                     "Extra 2
        WHEN 'FIELD34'.                                     "Extra 3
        WHEN 'FIELD35'.                                     "Extra 4
        WHEN 'FIELD36'.                                     "Extra 5
        WHEN 'FIELD37'.     "PayType VEND/CUST HOT CODED
        WHEN 'FIELD38'.
        WHEN 'FIELD39'.
        WHEN 'FIELD40'.      "User ID
        WHEN 'FIELD41'.
        WHEN 'FIELD42'.
      ENDCASE.       "  P_L_FIELD  FOR EACH FIELD

*&---------------------------------------------------------------------*
*       for INVOICE fields
*----------------------------------------------------------------------*
    WHEN 'I'.
**---------"for all fields we need to remove these  4 char { ',",`,^ }---

      CLEAR : L_VALID,L_NOT_VALID.
      L_NOT_VALID = TEXT-C01.     " '"`^
      PERFORM NEW_SPECIAL_CHAR_CHECKING USING  L_VALID
                                               L_NOT_VALID
                                      CHANGING P_FS_OUT.
      CLEAR : L_VALID,L_NOT_VALID.


      CASE P_L_FIELD.    " FOR EACH FIELD
        WHEN 'FIELD1'.
        WHEN 'FIELD2'.
        WHEN 'FIELD3'.
        WHEN 'FIELD4'.
        WHEN 'FIELD5'.
        WHEN 'FIELD6'.
        WHEN 'FIELD7'.
        WHEN 'FIELD8'.
      ENDCASE.       "  P_L_FIELD  FOR EACH FIELD
  ENDCASE.   "P_REC_TYPE   RCORD TYPE

  CLEAR : L_VALID,L_NOT_VALID.
ENDFORM.                    " FIELD_VALIDATION

*&---------------------------------------------------------------------*
*&      Form  NEW_SPECIAL_CHAR_CHECKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_VALID  text
*      -->P_L_NOT_VALID  text
*      <--P_P_FS_OUT  text
*----------------------------------------------------------------------*
FORM NEW_SPECIAL_CHAR_CHECKING  USING    P_L_VALID
                                         P_L_NOT_VALID
                                CHANGING P_P_FS_OUT.
  DATA: L_CHAR   TYPE C,
        L_POS    TYPE I,
        L_STRLEN TYPE I.

  CLEAR : L_POS, L_STRLEN, L_CHAR.
  L_STRLEN    =  STRLEN( P_P_FS_OUT ).

*&---------------------------------------------------------------------*
*       BASED ON VALID CHAR
*----------------------------------------------------------------------*
  IF P_L_VALID IS NOT INITIAL.

    DO L_STRLEN TIMES.
      L_CHAR = P_P_FS_OUT+L_POS(1).

      IF P_L_VALID NS L_CHAR.
        P_P_FS_OUT+L_POS(1) = ' '.
      ENDIF.

      L_POS = L_POS + 1.
    ENDDO.
*&---------------------------------------------------------------------*
*       REMOVING INVALID CHAR
*----------------------------------------------------------------------*
  ELSEIF P_L_NOT_VALID IS NOT INITIAL.

    DO L_STRLEN TIMES.
      L_CHAR = P_P_FS_OUT+L_POS(1).

      IF P_L_NOT_VALID CS L_CHAR.
        P_P_FS_OUT+L_POS(1) = ' '.
      ENDIF.

      L_POS = L_POS + 1.
    ENDDO.

  ENDIF.     "P _L_VALID IS NOT INITIAL.

ENDFORM.                    " NEW_SPECIAL_CHAR_CHECKING
