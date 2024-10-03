*&---------------------------------------------------------------------*
*&  Include           ZAXIS_PAY_EXTR_FORM4
*======================================================================*
*       text          FETCH GLOBAL DATA AND LOG DETAILS ETC
*======================================================================*


*&---------------------------------------------------------------------*
*&      Form  CLARING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLARING_DATA .

  REFRESH G_TAB_ZAXIS_TAB_CTRLTAB.
  REFRESH G_TAB_ZAXIS_TAB_MAP.
  REFRESH G_TAB_ZAXIS_TAB_FLDLENG.
  REFRESH G_TAB_ZAXIS_TAB_CONVERS.
  REFRESH GIT_TAB_HB.
  REFRESH GIT_DISPLAY.
  REFRESH GIT_EXCEPTION.

  REFRESH : G_TAB_REGUH,
            G_TAB_REGUV,
            G_TAB_BSAK,
            G_TAB_BSIS,
            G_TAB_BSAS.



  CLEAR : REGUH,
          BKPF,
          BSEG,
          BSAK,
          BSIK,
          BSAD,
          BSID,
          BSIS,
          BSAS,
          LFA1,
          LFB1,
          LFBK,
          KNA1,
          KNBK,
          KNB1,
          BNKA,
          T012,
          T012K.

  CLEAR : WA_ZAXIS_TAB_CTRLTAB.

  CASE SY-TCODE.

    WHEN  C_TCODE_EXTR.
      SY-TITLE = TEXT-500. "'Axis Bank Vendor Payments Extraction' .

    WHEN  C_TCODE_REXTR.
      SY-TITLE = TEXT-501.  "'Axis Bank Vendor Payments Re-Extraction' .

  ENDCASE.
ENDFORM.                    " CLARING_DATA

*&---------------------------------------------------------------------*
*&      Form  CHECK_MANDATORY_INPUTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_MANDATORY_INPUTS .

  IF P_BUKRS IS INITIAL.
    PERFORM POP_UP_ERROR USING TEXT-100.
  ENDIF.

  IF S_HBKID[] IS INITIAL OR S_HKTID[] IS INITIAL .
    PERFORM POP_UP_ERROR USING TEXT-101.
  ENDIF.


  IF P_AUTO = ABAP_TRUE. "'X'.                   "Automatic

    IF S_LAUFI[] IS INITIAL AND S_LAUFD[] IS INITIAL.
      PERFORM POP_UP_ERROR USING TEXT-103.
    ENDIF.

  ELSE.                              "Manual

    IF P_GJAHR IS INITIAL.                                     " if fiscal is empty
      PERFORM POP_UP_ERROR USING TEXT-104.
    ELSEIF S_VBLNR[] IS INITIAL AND S_BUDAT[] IS INITIAL.      "* IF DOCUMENT NUMBER & POSTING DATE IS EMPTY
      PERFORM POP_UP_ERROR USING TEXT-105.
    ENDIF.

  ENDIF.

  SELECT * FROM T001 INTO TABLE G_TAB_T001 WHERE BUKRS = P_BUKRS.
  IF G_TAB_T001[] IS INITIAL.
    PERFORM POP_UP_ERROR USING TEXT-107.
  ENDIF.

  SELECT * FROM ZAXIS_TAB_HB INTO TABLE GIT_TAB_HB
                WHERE BUKRS = P_BUKRS
                 AND HBKID IN S_HBKID.

  IF GIT_TAB_HB[] IS INITIAL.
    PERFORM POP_UP_ERROR USING TEXT-102.
  ENDIF.

* FILL THE G/L ACCOUNT FROM T012K.
  PERFORM FILL_RANGES_FOR_GL.

  IF P_PAY IS NOT INITIAL.
    IF P_AUTO NE ABAP_TRUE. "'X'.       " FOR MANUAL PAYMENTS
      SELECT SINGLE * FROM ZAXIS_PAY_MTD INTO WA_PAY_MTD
                                         WHERE BUKRS        = P_BUKRS
                                           AND MANU_AUTO    = 'MANU'
                                           AND PAYMENT_TYPE = P_PAY.
      IF SY-SUBRC NE 0.
        PERFORM POP_UP_ERROR USING TEXT-120 .      "Please Select A allowed Payment Method From Input or add this Payment Method in ZTABLE
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                    " CHECK_MANDATORY_INPUTS



*&---------------------------------------------------------------------*
*&      Form  fill_ranges_for_gl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_RANGES_FOR_GL .

  REFRESH R_HKONT.

* FETCH THE G/L ACCOUNT BASED ON HOUSE BANK ID

  SELECT HKONT FROM T012K INTO R_HKONT_LINE-LOW
                  WHERE BUKRS = P_BUKRS
                   AND HBKID IN S_HBKID
                   AND  HKTID IN S_HKTID.

    R_HKONT_LINE-SIGN = 'I'.
    R_HKONT_LINE-OPTION = 'EQ'.
    APPEND R_HKONT_LINE TO R_HKONT.
    CLEAR : R_HKONT_LINE.

  ENDSELECT.

  CLEAR :  R_HKONT_LINE.

* FETCH THE G/L ACCOUNT TO BE POSTED.

  SELECT UKONT FROM T042I INTO R_HKONT_LINE-LOW
                    WHERE ZBUKR = P_BUKRS
                      AND HBKID IN S_HBKID
                      AND HKTID IN S_HKTID ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

    R_HKONT_LINE-SIGN = 'I'.
    R_HKONT_LINE-OPTION = 'EQ'.

    APPEND R_HKONT_LINE TO R_HKONT.
    CLEAR : R_HKONT_LINE.

  ENDSELECT.

  CLEAR :  R_HKONT_LINE.

  SORT R_HKONT BY LOW.
  DELETE ADJACENT DUPLICATES FROM R_HKONT COMPARING ALL FIELDS.

  IF R_HKONT[] IS INITIAL.
    PERFORM POP_UP_ERROR USING TEXT-E24.   "House Bank or Account Id might be wrong, please check.
  ENDIF.

ENDFORM.                    " fill_ranges_for_gl
*&---------------------------------------------------------------------*
*&      Form  FETCH_LOG_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FETCH_LOG_DETAILS .
  IF SY-TCODE = C_TCODE_EXTR.
    IF P_AUTO EQ ABAP_TRUE. "'X'.
      SELECT * FROM ZAXIS_TAB_CTLTA1 INTO TABLE G_TAB_ZAXIS_TAB_CTRLTAB
                                   WHERE   BUKRS = P_BUKRS
                                       AND HBKID IN S_HBKID
                                       AND HKTID IN S_HKTID
                                       AND LAUFD IN S_LAUFD
                                       AND LAUFI IN S_LAUFI.
    ELSE.

      SELECT * FROM ZAXIS_TAB_CTLTA1 INTO TABLE G_TAB_ZAXIS_TAB_CTRLTAB
                                     WHERE BUKRS = P_BUKRS
                                        AND HBKID IN S_HBKID
                                        AND HKTID IN S_HKTID
                                        AND BELNR IN S_VBLNR
                                        AND GJAHR EQ P_GJAHR.
    ENDIF.


  ELSEIF SY-TCODE = C_TCODE_REXTR.

    IF P_AUTO EQ ABAP_TRUE. "'X'.
      SELECT * FROM ZAXIS_TAB_CTLTA1 INTO TABLE G_TAB_ZAXIS_TAB_CTRLTAB
                                   WHERE   BUKRS = P_BUKRS
                                       AND HBKID IN S_HBKID
                                       AND HKTID IN S_HKTID
                                       AND LAUFD IN S_LAUFD
                                       AND LAUFI IN S_LAUFI
                                       AND GSBER IN S_GSBER
                                       AND PRCTR IN S_PRCTR.
    ELSE.

      SELECT * FROM ZAXIS_TAB_CTLTA1 INTO TABLE G_TAB_ZAXIS_TAB_CTRLTAB
                                     WHERE BUKRS = P_BUKRS
                                        AND HBKID IN S_HBKID
                                        AND HKTID IN S_HKTID
                                        AND BELNR IN S_VBLNR
                                        AND BUDAT IN S_BUDAT
                                        AND GJAHR EQ P_GJAHR
                                        AND GSBER IN S_GSBER
                                        AND PRCTR IN S_PRCTR.
    ENDIF.

    IF G_TAB_ZAXIS_TAB_CTRLTAB[] IS INITIAL.
      PERFORM POP_UP_ERROR USING TEXT-106.   "Data was not yet extracted, Please use extraction option
    ENDIF.

  ENDIF.


SORT G_TAB_ZAXIS_TAB_CTRLTAB BY BUKRS BELNR GJAHR. " Added by <IT-CAR Tool> during Code Remediation
SORT G_TAB_ZAXIS_TAB_CTRLTAB BY BUKRS BELNR GJAHR. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM G_TAB_ZAXIS_TAB_CTRLTAB COMPARING BUKRS BELNR GJAHR.
  SORT G_TAB_ZAXIS_TAB_CTRLTAB BY BUKRS BELNR GJAHR.

ENDFORM.                    " FETCH_LOG_DETAILS
*&---------------------------------------------------------------------*
*&      Form  FETCH_CONFIGURATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FETCH_CONFIGURATIONS .


*&---------------------------------------------------------------------*
*       fetch mapping table data
*----------------------------------------------------------------------*
  IF P_AUTO = ABAP_TRUE. "'X'.

    SELECT * FROM ZAXIS_TAB_MAP INTO TABLE G_TAB_ZAXIS_TAB_MAP
                                       WHERE BANK_CODE = C_BANK_CODE AND
                                       ( MANU_AUTO = 2 OR MANU_AUTO = 3 ).

  ELSE.

    SELECT * FROM ZAXIS_TAB_MAP INTO TABLE G_TAB_ZAXIS_TAB_MAP
                                       WHERE BANK_CODE = C_BANK_CODE AND
                                       ( MANU_AUTO = 1 OR MANU_AUTO = 3 ).

  ENDIF.

* IF MAPPING TABLE IS INITIAL THEN THROW THE ERROR
  IF G_TAB_ZAXIS_TAB_MAP[] IS INITIAL.
    PERFORM POP_UP_ERROR USING TEXT-108.      ""Field Mapping Table was empty, Please Check.
  ENDIF.

*&---------------------------------------------------------------------*
*       fetch fieldlength table data
*----------------------------------------------------------------------*

  SELECT * FROM ZAXIS_TAB_FLDLEN INTO TABLE G_TAB_ZAXIS_TAB_FLDLENG
                                      WHERE BANK_CODE = C_BANK_CODE.
  IF SY-SUBRC NE 0.
    PERFORM POP_UP_ERROR USING TEXT-109.   "NO FIELD LENGTHS MAINTAINED, please check.
  ENDIF.


*&---------------------------------------------------------------------*
*       fetch fieldlength table data
*----------------------------------------------------------------------*

  SELECT * FROM ZAXIS_TAB_CONVER INTO TABLE G_TAB_ZAXIS_TAB_CONVERS.
  IF SY-SUBRC NE 0.
    PERFORM POP_UP_ERROR USING TEXT-110.   "Convertion table should not be empty please check.

  ENDIF.
  GIT_CONVERS[] = G_TAB_ZAXIS_TAB_CONVERS[].

  SELECT * FROM ZAXIS_PAY_MTD INTO TABLE GIT_PAY_MTD.
  SELECT * FROM ZAXIS_USERS   INTO TABLE GIT_USERS.

  SELECT * FROM ZAXIS_TAB_SEP INTO TABLE G_TAB_ZAXIS_TAB_SEP
                                    WHERE BUKRA = P_BUKRS.

ENDFORM.                    " FETCH_CONFIGURATIONS
*&---------------------------------------------------------------------*
*&      Form  USER_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM USER_VALIDATION .

  CLEAR : WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY BANK_CODE   = C_BANK_CODE
                                                  RECORD_TYPE = 'H'
                                                  FIELDNAME =  'VEND'
                                                  OLDVALUE  =  'USER'.
  IF WA_CONVERS-NEWVALUE = 'Y'.

    SELECT SINGLE * FROM ZAXIS_USERS INTO WA_USERS WHERE BUKRS = P_BUKRS
                                                 AND USER_NAME = SY-UNAME.

    IF SY-SUBRC NE 0.

      PERFORM POP_UP_ERROR USING TEXT-119.          " This user is not authorized to extract the Axis Bank Payments

    ENDIF.
  ENDIF.

ENDFORM.                    " USER_VALIDATION

*&---------------------------------------------------------------------*
*&      Form  read_table_bKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BSAK  text
*      <--P_BKPF  text
*----------------------------------------------------------------------*
FORM READ_TABLE_BKPF  USING    P_P_BSAK STRUCTURE BSAK
                      CHANGING P_BKPF .

  CLEAR P_BKPF.

  READ TABLE G_TAB_BKPF INTO P_BKPF WITH KEY BELNR = P_P_BSAK-AUGBL
                                             BUKRS = P_P_BSAK-BUKRS
                                             GJAHR = P_P_BSAK-GJAHR.

ENDFORM.                    " read_table_bkpf


*&---------------------------------------------------------------------*
*&      Form  read_table_bvor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BSAK  text
*      <--P_BVOR  text
*----------------------------------------------------------------------*
FORM READ_TABLE_BVOR  USING    P_P_BSAK STRUCTURE BSAK
                      CHANGING P_BVOR .

  CLEAR P_BVOR.

* READ THE CORRESPONDING COMPANY CODE
* BASED ON THE DOCUMENT NUMBER.

  READ TABLE G_TAB_BVOR INTO P_BVOR WITH KEY BELNR = P_P_BSAK-BELNR.

ENDFORM.                    " read_table_bvor

*&---------------------------------------------------------------------*
*&      Form  read_table_reguh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BSAK     text
*      -->P_REGUH    text
*----------------------------------------------------------------------*
FORM READ_TABLE_REGUH  USING    P_BSAK STRUCTURE BSAK
                       CHANGING P_REGUH STRUCTURE REGUH.

  CLEAR P_REGUH.

  READ TABLE G_TAB_REGUH INTO P_REGUH
                      WITH KEY ZBUKR = P_BUKRS
                               VBLNR = P_BSAK-AUGBL
                               ZALDT = P_BSAK-AUGDT.

ENDFORM.                    " read_table_reguh


*&---------------------------------------------------------------------*
*&      Form  read_table_payr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BSAK     text
*      -->P_PAYR     text
*----------------------------------------------------------------------*
FORM READ_TABLE_PAYR  CHANGING P_BSAK STRUCTURE BSAK.

  CLEAR PAYR.

  READ TABLE G_TAB_PAYR INTO PAYR WITH KEY ZBUKR = P_BUKRS
                                           VBLNR = P_BSAK-BELNR
                                           GJAHR = P_BSAK-GJAHR.

  IF SY-SUBRC EQ 0.

    MOVE : PAYR-HBKID TO P_BSAK-HBKID,
           PAYR-HKTID TO G_HKTID.

  ENDIF.

ENDFORM.                    " read_table_payr


*&---------------------------------------------------------------------*
*&      Form  read_table_bsis_bsas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BSAK     text
*      -->P_BSIS     text
*----------------------------------------------------------------------*
FORM READ_TABLE_BSIS_BSAS  USING    P_BSAK STRUCTURE BSAK
                           CHANGING P_BSIS STRUCTURE BSIS.

  CLEAR P_BSIS.

  READ TABLE G_TAB_BSIS INTO P_BSIS WITH KEY BUKRS = P_BUKRS
                                        BELNR = P_BSAK-BELNR
                                        GJAHR = P_BSAK-GJAHR.

  IF SY-SUBRC NE 0.

    READ TABLE G_TAB_BSAS INTO P_BSIS WITH KEY BUKRS = P_BUKRS
                                        BELNR = P_BSAK-BELNR
                                        GJAHR = P_BSAK-GJAHR."#EC ENHOK

  ENDIF.

ENDFORM.                    " read_table_bsis_bsas

*&---------------------------------------------------------------------*
*&      Form  read_table_bsec
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BSAK     text
*      -->P_BSEC     text
*----------------------------------------------------------------------*
FORM READ_TABLE_BSEC  USING    P_BSAK STRUCTURE BSAK
                      CHANGING P_BSEC STRUCTURE BSEC.

  CLEAR P_BSEC.

  READ TABLE G_TAB_BSEC INTO P_BSEC WITH KEY BUKRS = P_BUKRS
                                             BELNR = P_BSAK-BELNR
                                             GJAHR = P_BSAK-GJAHR.

ENDFORM.                    " read_table_bsec
*&---------------------------------------------------------------------*
*&      Form  read_table_lfa1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LFA1     text
*----------------------------------------------------------------------*
FORM READ_TABLE_LFA1  USING    P_BSAK STRUCTURE BSAK
                      CHANGING P_LFA1 STRUCTURE LFA1.

  DATA : L_LNRZB LIKE LFB1-LNRZB,
         WA_LFB1 TYPE LFB1.

  CLEAR P_LFA1.

  IF P_BSAK-EMPFB IS INITIAL.     " for normal payment.
    READ TABLE G_TAB_LFA1 INTO P_LFA1 WITH KEY LIFNR = P_BSAK-LIFNR.
  ELSE.                           " for individual paye payment.
    READ TABLE G_TAB_LFA1 INTO P_LFA1 WITH KEY LIFNR = P_BSAK-EMPFB.
  ENDIF.

ENDFORM.                    " read_table_lfa1
*&---------------------------------------------------------------------*
*&      Form  read_table_lfb1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LFB1     text
*----------------------------------------------------------------------*
FORM READ_TABLE_LFB1  USING     P_BSAK STRUCTURE BSAK
                      CHANGING  P_LFA1 STRUCTURE LFA1
                                P_LFB1 STRUCTURE LFB1.

  CLEAR P_LFB1.

  IF P_BSAK-EMPFB IS INITIAL.     " for normal payment.
    READ TABLE G_TAB_LFB1 INTO P_LFB1 WITH KEY
                                      LIFNR = P_BSAK-LIFNR
                                      BUKRS = P_BUKRS.
  ELSE.                           " for individual paye payment.
    READ TABLE G_TAB_LFB1 INTO P_LFB1 WITH KEY
                                      LIFNR = P_BSAK-EMPFB
                                      BUKRS = P_BUKRS.
  ENDIF.


ENDFORM.                    " read_table_lfb1



*&---------------------------------------------------------------------*
*&      Form  READ_alt_PAYEE
*&---------------------------------------------------------------------*
*--fetch the ALTERNATE PAYEE field from conversion table---------------*
*----------------------------------------------------------------------*
FORM READ_ALT_PAYEE  USING     P_BSAK STRUCTURE BSAK
                     CHANGING  LFA1 STRUCTURE LFA1
                               LFB1 STRUCTURE LFB1.

  CLEAR : WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY BANK_CODE   = C_BANK_CODE
                                                  RECORD_TYPE = 'H'
                                                  FIELDNAME   = 'VEND'
                                                  OLDVALUE    = 'ALT_PAYE'.

  IF SY-SUBRC = 0 AND WA_CONVERS-NEWVALUE IS NOT INITIAL.
    ASSIGN (WA_CONVERS-NEWVALUE) TO <FS_ALT_PAYE>.
  ELSE .
    WA_CONVERS-NEWVALUE = 'LFB1-LNRZB'.
    ASSIGN (WA_CONVERS-NEWVALUE) TO <FS_ALT_PAYE>.
  ENDIF.

  CLEAR : GV_ALT_LIFNR.
  GV_ALT_LIFNR = <FS_ALT_PAYE>.

  IF GV_ALT_LIFNR IS NOT INITIAL.
    CLEAR : LFA1,LFB1.

    READ TABLE G_TAB_LFA1 INTO LFA1 WITH KEY LIFNR = GV_ALT_LIFNR.
    IF SY-SUBRC NE 0.
      SELECT SINGLE * FROM LFA1 INTO LFA1 WHERE LIFNR = GV_ALT_LIFNR.
    ENDIF.

    READ TABLE G_TAB_LFB1 INTO LFB1 WITH KEY LIFNR = GV_ALT_LIFNR
                                             BUKRS = P_BUKRS.
    IF SY-SUBRC NE 0.
      SELECT SINGLE * FROM LFB1 INTO LFB1 WHERE LIFNR = GV_ALT_LIFNR
                                            AND BUKRS = P_BUKRS.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_alt_PAYEE
*&---------------------------------------------------------------------*
*&      Form  read_table_lfbk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LFBK     text
*----------------------------------------------------------------------*
FORM READ_TABLE_LFBK  USING    P_BSAK STRUCTURE BSAK
                      CHANGING P_LFBK STRUCTURE LFBK
                               P_BNKA STRUCTURE BNKA.

  CLEAR P_LFBK.


  IF P_BSAK-BVTYP IS INITIAL.
    READ TABLE G_TAB_LFBK INTO P_LFBK WITH KEY LIFNR = LFA1-LIFNR.
  ELSE.
    READ TABLE G_TAB_LFBK INTO P_LFBK WITH KEY LIFNR = LFA1-LIFNR
                                               BVTYP = P_BSAK-BVTYP.
  ENDIF.

ENDFORM.                    " read_table_lfbk

*&---------------------------------------------------------------------*
*&      Form  read_table_adrc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ADRNR      text
*      -->P_BSAK     text
*      -->P_ADRC     text
*----------------------------------------------------------------------*
FORM READ_TABLE_ADRC  USING    ADRNR
                      CHANGING P_ADRC STRUCTURE ADRC.

  CLEAR P_ADRC.

  READ TABLE G_TAB_ADRC INTO P_ADRC WITH KEY ADDRNUMBER = ADRNR.


ENDFORM.                    " read_tabSle_adrc

*&---------------------------------------------------------------------*
*&      Form  read_table_bnka
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LFBK     text
*      -->P_BNKA     text
*----------------------------------------------------------------------*
FORM READ_TABLE_BNKA  USING    P_LFBK STRUCTURE LFBK
                      CHANGING P_BNKA STRUCTURE BNKA.


  READ TABLE G_TAB_BNKA INTO P_BNKA WITH KEY BANKS = P_LFBK-BANKS
                                             BANKL = P_LFBK-BANKL.

  IF SY-SUBRC NE 0.

    SELECT SINGLE * FROM BNKA INTO P_BNKA WHERE BANKS = P_LFBK-BANKS AND
                                                BANKL = P_LFBK-BANKL.

  ENDIF.


ENDFORM.                    " read_table_bnka

*&---------------------------------------------------------------------*
*&      Form  read_table_t012
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BSAK     text
*      -->P_T012     text
*----------------------------------------------------------------------*
FORM READ_TABLE_T012  USING    P_BSAK STRUCTURE BSAK
                      CHANGING P_T012 STRUCTURE T012.

  CLEAR P_T012.

  DATA : WA_HBKID LIKE S_HBKID,
         LV_HBKID LIKE T012-HBKID.

*read house bank id from the range

  IF P_BSAK-HBKID IS INITIAL.

    READ TABLE S_HBKID INTO WA_HBKID INDEX 1.
    LV_HBKID =  WA_HBKID-LOW .

    READ TABLE G_TAB_T012 INTO P_T012 WITH KEY BUKRS = P_BUKRS
                                               HBKID = LV_HBKID.
  ELSE.

    READ TABLE G_TAB_T012 INTO P_T012 WITH KEY BUKRS = P_BUKRS
                                               HBKID = P_BSAK-HBKID.
  ENDIF.

ENDFORM.                    " read_table_t012

*&---------------------------------------------------------------------*
*&      Form  read_table_t012k
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BSAK     text
*      -->P_T012K    text
*----------------------------------------------------------------------*
FORM READ_TABLE_T012K  USING    P_BSAK STRUCTURE BSAK
                      CHANGING  P_T012K STRUCTURE T012K.

  CLEAR P_T012K.

  DATA : WA_HBKID LIKE S_HBKID,
         WA_HKTID LIKE S_HKTID,
         LV_HBKID LIKE T012-HBKID,
         LV_HKTID LIKE T012K-HKTID,
         LV_NAME1 LIKE T012-NAME1.

*read the house bank details from the screen if not in the payment
*document.

  IF P_BSAK-HBKID IS INITIAL OR G_HKTID IS INITIAL.

    READ TABLE S_HBKID INTO WA_HBKID INDEX 1.
    LV_HBKID  =  WA_HBKID-LOW .

    READ TABLE S_HKTID INTO WA_HKTID INDEX 1.
    LV_HKTID  =  WA_HKTID-LOW.

    READ TABLE G_TAB_T012K INTO P_T012K WITH KEY BUKRS = P_BUKRS
                                                 HBKID = LV_HBKID
                                                 HKTID = LV_HKTID.


  ELSE.

    READ TABLE G_TAB_T012K INTO P_T012K WITH KEY BUKRS = P_BUKRS
                                                 HBKID = P_BSAK-HBKID
                                                 HKTID = G_HKTID.
  ENDIF.

  SELECT SINGLE ORT01 FROM BNKA INTO LV_NAME1  WHERE BANKS = T012-BANKS
                                                 AND BANKL =  T012-BANKL
                                                 .

*move the city to the name1 field
  T012-NAME1 = LV_NAME1.

ENDFORM.                    " read_table_t012k

*&---------------------------------------------------------------------*
*&      Form  read_table_bseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BSAK     text
*      -->P_BSEG     text
*----------------------------------------------------------------------*
FORM READ_TABLE_BSEG  USING    P_BSAK STRUCTURE BSAK
                      CHANGING P_BSEG STRUCTURE BSEG.

  CLEAR P_BSEG.

  READ TABLE G_TAB_BSEG INTO P_BSEG WITH KEY BUKRS = P_BUKRS
                                             BELNR = P_BSAK-BELNR
                                             GJAHR = P_BSAK-GJAHR
                                             BUZEI = P_BSAK-BUZEI.

ENDFORM.                    " read_table_bseg


*&---------------------------------------------------------------------*
*&      Form  read_table_with_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BSAK  text
*      <--P_WITH_ITEM  text
*----------------------------------------------------------------------*
FORM READ_TABLE_WITH_ITEM  USING     P_BSAK  STRUCTURE BSAK
                           CHANGING P_WITH_ITEM  STRUCTURE WITH_ITEM.

  CLEAR P_WITH_ITEM.

  READ TABLE G_TAB_WITH_ITEM INTO P_WITH_ITEM WITH KEY BUKRS = P_BUKRS
                                                       AUGBL = P_BSAK-AUGBL
                                                       AUGDT = P_BSAK-AUGDT.

ENDFORM.                    " read_table_with_item

*&---------------------------------------------------------------------*
*&      Form  READ_CHANGED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BSAK  text
*----------------------------------------------------------------------*
FORM READ_CHANGED_DATA USING P_BSAK TYPE BSAK.
  CLEAR : WA_SEL_CHANGE.
  READ TABLE GIT_SEL_DISPLAY INTO WA_SEL_CHANGE WITH KEY  AUGBL = P_BSAK-AUGBL
                                                          GJAHR = P_BSAK-GJAHR
                                                          BUKRS = P_BSAK-BUKRS
                                                          CHK_BOX = 'X'.
ENDFORM.                    " READ_CHANGED_DATA

*&---------------------------------------------------------------------*
*&      Form  read_table_print_location
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LFA1  text
*      <--P_ZAXIS_TAB_PAYLOC  text
*----------------------------------------------------------------------*
FORM READ_TABLE_PRINT_LOCATION  USING    P_LFA1 TYPE LFA1
                                CHANGING P_ZAXIS_TAB_PAYLOC.

  DATA : LV_CITY TYPE ORT01_GP.

  CLEAR : P_ZAXIS_TAB_PAYLOC , LV_CITY.

  IF P_PRINT IS NOT INITIAL.
    READ TABLE G_TAB_ZAXIS_TAB_PAYLOC INTO P_ZAXIS_TAB_PAYLOC
                                      WITH KEY BUKRS        = P_BUKRS
                                               HBKID        = S_HBKID-LOW
                                               PAYMENT_TYPE = GV_PAYMENT
                                               SAP_CITY     = P_PRINT.
  ELSE.
    READ TABLE G_TAB_ZAXIS_TAB_PAYLOC INTO P_ZAXIS_TAB_PAYLOC
                                    WITH KEY BUKRS        = P_BUKRS
                                             HBKID        = S_HBKID-LOW
                                             PAYMENT_TYPE = GV_PAYMENT.
  ENDIF.

ENDFORM.                    " read_table_print_location


*&---------------------------------------------------------------------*
*&      Form  ONE_TIME_VENDOR_ADDRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ONE_TIME_VENDOR_ADDRESS .
  ADRC-NAME1         = BSEC-NAME1.
  ADRC-CITY1         = BSEC-ORT01.
  ADRC-REGION        = BSEC-REGIO.
  ADRC-COUNTRY       = BSEC-LAND1.
  ADRC-POST_CODE1    = BSEC-PSTLZ.
  ADRC-STREET        = BSEC-STRAS.

ENDFORM.                    " ONE_TIME_VENDOR_ADDRESS
