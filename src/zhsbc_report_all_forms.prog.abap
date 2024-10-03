*----------------------------------------------------------------------*
***INCLUDE ZHSBC_REPORT_ALL_FORMS.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F4HELP_HBKID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0010   text
*      <--P_S_HBKID_LOW  text
*----------------------------------------------------------------------*
FORM F4HELP_HBKID  USING    VALUE(PP_HBKID)
                   CHANGING P_S_HBKID_LOW.


  TYPES: BEGIN OF TY_HBKID,
       BUKRS TYPE ZHSBC_HOUSE_BANK-BUKRS,
       HBKID TYPE ZHSBC_HOUSE_BANK-HBKID,
       HKTID TYPE ZHSBC_HOUSE_BANK-HKTID,
       BANKN TYPE ZHSBC_HOUSE_BANK-BANKN,
       SWIFT TYPE ZHSBC_HOUSE_BANK-SWIFT,
       HKONT TYPE ZHSBC_HOUSE_BANK-HKONT,
       REFZL TYPE ZHSBC_HOUSE_BANK-REFZL,
     END OF TY_HBKID.


   DATA : IT_HBKID  TYPE TABLE OF TY_HBKID,
         WA_HBKID TYPE TY_HBKID.

  CLEAR : IT_DYNPREAD[],IT_RETURN[],IT_HBKID[].


  PERFORM DYNP_VALUES_READ USING 'P_BUKRS'.

   SELECT BUKRS
         HBKID
         HKTID
         BANKN
         SWIFT
         HKONT
         REFZL FROM ZHSBC_HOUSE_BANK INTO TABLE IT_HBKID  WHERE BUKRS EQ P_BUKRS.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'HBKID'
      DYNPPROG        = SY-CPROG
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = PP_HBKID
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_HBKID
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE IT_RETURN INTO WA_RETURN INDEX 1.
    IF SY-SUBRC EQ 0.
      P_HBKID  = WA_RETURN-FIELDVAL.
    ENDIF.
  ENDIF.





ENDFORM.                    " F4HELP_HBKID
*&---------------------------------------------------------------------*
*&      Form  F4HELP_HKTID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0022   text
*      <--P_P_HKTID  text
*----------------------------------------------------------------------*
FORM F4HELP_HKTID  USING    VALUE(P_0022)
                   CHANGING P_P_HKTID.


  TYPES : BEGIN OF TY_HKTID,
          BUKRS TYPE BUKRS,
          HBKID TYPE HBKID,
          HKTID TYPE HKTID,
          END OF TY_HKTID.
  DATA : IT_HKTID TYPE TABLE OF TY_HKTID,
         WA_HKTID TYPE TY_HKTID.
  CLEAR : IT_HKTID[],IT_RETURN[],WA_RETURN.

  PERFORM DYNP_VALUES_READ USING 'P_BUKRS'.


  IF P_HBKID IS INITIAL.
    MOVE 'P_HBKID' TO WA_DYNPREAD-FIELDNAME.
    APPEND WA_DYNPREAD TO IT_DYNPREAD.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME               = SY-REPID
        DYNUMB               = SY-DYNNR
      TABLES
        DYNPFIELDS           = IT_DYNPREAD
      EXCEPTIONS
        INVALID_ABAPWORKAREA = 1
        INVALID_DYNPROFIELD  = 2
        INVALID_DYNPRONAME   = 3
        INVALID_DYNPRONUMMER = 4
        INVALID_REQUEST      = 5
        NO_FIELDDESCRIPTION  = 6
        INVALID_PARAMETER    = 7
        UNDEFIND_ERROR       = 8
        DOUBLE_CONVERSION    = 9
        STEPL_NOT_FOUND      = 10
        OTHERS               = 11.
    IF SY-SUBRC <> 0.

    ELSE.
      READ TABLE IT_DYNPREAD INTO WA_DYNPREAD WITH KEY FIELDNAME = 'P_HBKID'.
      IF SY-SUBRC EQ 0.
        P_HBKID = WA_DYNPREAD-FIELDVALUE.
      ENDIF.
    ENDIF.
  ENDIF.
*  IF sy-tcode EQ c_tcode_extr.
   SELECT BUKRS
         HBKID
         HKTID
         FROM ZHSBC_HOUSE_BANK INTO TABLE IT_HKTID
         WHERE BUKRS EQ P_BUKRS
         AND   HBKID EQ P_HBKID.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'HKTID'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = P_0022
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_HKTID
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.

  ELSE.
    READ TABLE  IT_RETURN INTO WA_RETURN INDEX 1.
    IF SY-SUBRC EQ 0.
      P_HKTID = WA_RETURN-FIELDVAL.
    ENDIF.
  ENDIF.




ENDFORM.                    " F4HELP_HKTID
*&---------------------------------------------------------------------*
*&      Form  DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE      text
*      -->(P_1308)   text
*----------------------------------------------------------------------*
FORM DYNP_VALUES_READ  USING    VALUE(P_1308).

  MOVE P_1308 TO WA_DYNPREAD-FIELDNAME.
  APPEND WA_DYNPREAD TO IT_DYNPREAD.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME               = SY-REPID
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = IT_DYNPREAD
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      INVALID_PARAMETER    = 7
      UNDEFIND_ERROR       = 8
      DOUBLE_CONVERSION    = 9
      STEPL_NOT_FOUND      = 10
      OTHERS               = 11.
  IF SY-SUBRC <> 0.

  ELSE.
    READ TABLE IT_DYNPREAD INTO WA_DYNPREAD WITH KEY FIELDNAME = P_1308.
    IF SY-SUBRC EQ 0.
      P_BUKRS = WA_DYNPREAD-FIELDVALUE.
    ENDIF.
  ENDIF.


  SELECT * FROM T001 INTO TABLE G_TAB_T001 WHERE BUKRS = P_BUKRS.







ENDFORM.                    " DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_RETRIVAL .

*  IF P_BUKRS IS INITIAL.
*
*    MESSAGE 'Enter Company code........ ' TYPE 'E'.
*
*  ENDIF.

  IF P_VENDOR = 'X'.

    IF  P_HBKID IS INITIAL.


      SELECT * FROM ZHSBC_ITEMS INTO TABLE IT_FINAL WHERE BUKRS = P_BUKRS AND
                                                          BUDAT IN S_BUDAT AND
                                                          LIFNR IN S_LIFNR AND
                                                          EXTR_DATE IN S_EXDATE AND
                                                          REX_DATE IN S_REDATE and
                                                          method = 'VE'.

    ELSE.

      CLEAR WA_T012K.
      SELECT SINGLE BUKRS
                    HBKID
                    HKTID
                    BANKN
                    BKONT
                    WAERS
                    HKONT FROM T012K INTO WA_T012K
                          WHERE BUKRS = P_BUKRS AND
                                HBKID = P_HBKID AND
                                HKTID = P_HKTID.
*
*      WA_T012K-HKONT  =  WA_T012K-HKONT + 2.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WA_T012K-HKONT
*        IMPORTING
*          OUTPUT = WA_T012K-HKONT.


      SELECT * FROM ZHSBC_ITEMS INTO TABLE IT_FINAL WHERE BUKRS = P_BUKRS AND
                                                      BUDAT IN S_BUDAT AND
                                                      LIFNR IN S_LIFNR AND
                                                      EXTR_DATE IN S_EXDATE AND
                                                      REX_DATE IN S_REDATE AND
                                                      HKONT = WA_T012K-HKONT and
                                                      method = 'VE'.
    ENDIF.


ELSEIF P_GL = 'X'.



IF  P_HBKID IS INITIAL.

  SELECT * FROM ZHSBC_ITEMS INTO TABLE IT_FINAL WHERE BUKRS = P_BUKRS AND
                                                      BUDAT IN S_BUDAT AND
                                                      LIFNR IN S_LIFNR AND
                                                      EXTR_DATE IN S_EXDATE AND
                                                      REX_DATE IN S_REDATE AND
                                                      HKONT in S_HKONT and
                                                      method = 'GL'.

    ELSE.

      CLEAR WA_T012K.
      SELECT SINGLE BUKRS
                    HBKID
                    HKTID
                    BANKN
                    BKONT
                    WAERS
                    HKONT FROM T012K INTO WA_T012K
                          WHERE BUKRS = P_BUKRS AND
                                HBKID = P_HBKID AND
                                HKTID = P_HKTID .

*      WA_T012K-HKONT  =  WA_T012K-HKONT + 2.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WA_T012K-HKONT
*        IMPORTING
*          OUTPUT = WA_T012K-HKONT.


       SELECT * FROM ZHSBC_ITEMS INTO TABLE IT_FINAL WHERE BUKRS = P_BUKRS AND
                                                      BUDAT IN S_BUDAT AND
                                                      LIFNR IN S_LIFNR AND
                                                      EXTR_DATE IN S_EXDATE AND
                                                      REX_DATE IN S_REDATE AND
                                                      HKONT = WA_T012K-HKONT and
                                                      method = 'GL'.

  ENDIF.

endif.

  IF  it_final IS INITIAL.

    Message 'No Records Found......' TYPE 'E'.

  ENDIF.



 IF SY-SUBRC = 0.


      PERFORM FCAT.
      PERFORM ALV.

    ENDIF.




ENDFORM.                    " DATA_RETRIVAL
*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FCAT .


  DATA : I  TYPE I.

  I = 0.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'BUKRS'.
  WA_FCAT-SELTEXT_S = 'Company'.
  WA_FCAT-SELTEXT_M = 'Company Code'.
  WA_FCAT-SELTEXT_L = 'Company Code'.
  WA_FCAT-KEY = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'BELNR'.
  WA_FCAT-SELTEXT_S = 'Doc No'.
  WA_FCAT-SELTEXT_M = 'Document Number'.
  WA_FCAT-SELTEXT_L = 'Document Number'.
  WA_FCAT-KEY = 'X'.
  WA_FCAT-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'GJAHR'.
  WA_FCAT-SELTEXT_S = 'Year'.
  WA_FCAT-SELTEXT_M = 'Fiscal Year'.
  WA_FCAT-SELTEXT_L = 'Fiscal Year'.
  WA_FCAT-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'BATCH'.
  WA_FCAT-SELTEXT_S = 'Batch Reference'.
  WA_FCAT-SELTEXT_M = 'Batch Reference'.
  WA_FCAT-SELTEXT_L = 'Batch Reference'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT..

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'XMLNA'.
  WA_FCAT-SELTEXT_S = 'File Name'.
  WA_FCAT-SELTEXT_M = 'File Name'.
  WA_FCAT-SELTEXT_L = 'File Name'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'BUDAT'.
  WA_FCAT-SELTEXT_S = 'Posting Date'.
  WA_FCAT-SELTEXT_M = 'Posting Date'.
  WA_FCAT-SELTEXT_L = 'Posting Date'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'BLDAT'.
  WA_FCAT-SELTEXT_S = 'Document Date'.
  WA_FCAT-SELTEXT_M = 'Document Date'.
  WA_FCAT-SELTEXT_L = 'Document Date'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'BLART'.
  WA_FCAT-SELTEXT_S = 'Document Type'.
  WA_FCAT-SELTEXT_M = 'Document Type'.
  WA_FCAT-SELTEXT_L = 'Document Type'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'HKONT'.
  WA_FCAT-SELTEXT_S = 'G/L Acc'.
  WA_FCAT-SELTEXT_M = 'General Ledger Account'.
  WA_FCAT-SELTEXT_L = 'General Ledger Account'.
*wa_fcat-KEY = 'X'.
*  WA_FCAT-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'HBKID'.
  WA_FCAT-SELTEXT_S = 'House Bank'.
  WA_FCAT-SELTEXT_M = 'House Bank'.
  WA_FCAT-SELTEXT_L = 'House Bank'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'HKTID'.
  WA_FCAT-SELTEXT_S = 'Account ID'.
  WA_FCAT-SELTEXT_M = 'Account ID'.
  WA_FCAT-SELTEXT_L = 'Account ID'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'PTYPE'.
  WA_FCAT-SELTEXT_S = 'Payment Type'.
  WA_FCAT-SELTEXT_M = 'Payment Type'.
  WA_FCAT-SELTEXT_L = 'Payment Type'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'PURCD'.
  WA_FCAT-SELTEXT_S = 'Purpose Code'.
  WA_FCAT-SELTEXT_M = 'Purpose Code'.
  WA_FCAT-SELTEXT_L = 'Purpose Code'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'TRDAT'.
  WA_FCAT-SELTEXT_S = 'Transaction Date'.
  WA_FCAT-SELTEXT_M = 'Transaction Date'.
  WA_FCAT-SELTEXT_L = 'Transaction Date'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'UNAME'.
  WA_FCAT-SELTEXT_S = 'User Name'.
  WA_FCAT-SELTEXT_M = 'User Name'.
  WA_FCAT-SELTEXT_L = 'User Name'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'TCODE'.
  WA_FCAT-SELTEXT_S = 'TCODE'.
  WA_FCAT-SELTEXT_M = 'Transaction Code'.
  WA_FCAT-SELTEXT_L = 'Transaction Code'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.



  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'CACNO'.
  WA_FCAT-SELTEXT_S = 'Company Acc no'.
  WA_FCAT-SELTEXT_M = 'Company Acc no'.
  WA_FCAT-SELTEXT_L = 'Company Acc no'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'CIFSC'.
  WA_FCAT-SELTEXT_S = 'Swift Code'.
  WA_FCAT-SELTEXT_M = 'Swift Code'.
  WA_FCAT-SELTEXT_L = 'Swift Code'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

if P_VENDOR is NOT INITIAL.
  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'LIFNR'.
  WA_FCAT-SELTEXT_S = 'Vendor'.
  WA_FCAT-SELTEXT_M = 'Vendor'.
  WA_FCAT-SELTEXT_L = 'Vendor'.
*wa_fcat-KEY = 'X'.
*  WA_FCAT-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'VNAME'.
  WA_FCAT-SELTEXT_S = 'Vendor Name'.
  WA_FCAT-SELTEXT_M = 'Vendor Name'.
  WA_FCAT-SELTEXT_L = 'Vendor Name'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.



  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'VACNO'.
  WA_FCAT-SELTEXT_S = 'Vendor Bank'.
  WA_FCAT-SELTEXT_M = 'Vendor Acct Number'.
  WA_FCAT-SELTEXT_L = 'Vendor Acct Number'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'VIFSC'.
  WA_FCAT-SELTEXT_S = 'Vendor IFSC'.
  WA_FCAT-SELTEXT_M = 'Vendor IFSC'.
  WA_FCAT-SELTEXT_L = 'Vendor IFSC'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
ELSEIF P_GL is NOT INITIAL.

   I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'HKONT1'.
  WA_FCAT-SELTEXT_S = 'GL Account'.
  WA_FCAT-SELTEXT_M = 'GL Account'.
  WA_FCAT-SELTEXT_L = 'GL Account'.
*wa_fcat-KEY = 'X'.
*  WA_FCAT-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'NAME'.
  WA_FCAT-SELTEXT_S = ' Name'.
  WA_FCAT-SELTEXT_M = ' Name'.
  WA_FCAT-SELTEXT_L = ' Name'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.



  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'VACNO'.
  WA_FCAT-SELTEXT_S = 'Bank'.
  WA_FCAT-SELTEXT_M = 'Acct Number'.
  WA_FCAT-SELTEXT_L = 'Acct Number'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'VIFSC'.
  WA_FCAT-SELTEXT_S = 'IFSC'.
  WA_FCAT-SELTEXT_M = 'IFSC'.
  WA_FCAT-SELTEXT_L = 'IFSC'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


endif.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'TRAMT'.
  WA_FCAT-SELTEXT_S = 'Amount'.
  WA_FCAT-SELTEXT_M = 'Transaction Amount'.
  WA_FCAT-SELTEXT_L = 'Transaction Amount'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

    I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'TDS'.
  WA_FCAT-SELTEXT_S = 'TDS Amount'.
  WA_FCAT-SELTEXT_M = 'TDS Amount'.
  WA_FCAT-SELTEXT_L = 'TDS Amount'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'VEMIL'.
  WA_FCAT-SELTEXT_S = 'Mail'.
  WA_FCAT-SELTEXT_M = 'Mail'.
  WA_FCAT-SELTEXT_L = 'Mail'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'REFER'.
  WA_FCAT-SELTEXT_S = 'Reference'.
  WA_FCAT-SELTEXT_M = 'Reference'.
  WA_FCAT-SELTEXT_L = 'Reference'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'EXTR_DATE'.
  WA_FCAT-SELTEXT_S = 'Ext Date'.
  WA_FCAT-SELTEXT_M = 'Extraction Date'.
  WA_FCAT-SELTEXT_L = 'Extraction Date'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'EXTR_TIME'.
  WA_FCAT-SELTEXT_S = 'Ext Time'.
  WA_FCAT-SELTEXT_M = 'Time of Extraction'.
  WA_FCAT-SELTEXT_L = 'Time of Extraction'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'EXTR_USER'.
  WA_FCAT-SELTEXT_S = 'Ext User '.
  WA_FCAT-SELTEXT_M = 'Username of Extraction'.
  WA_FCAT-SELTEXT_L = 'Username of Extraction'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'REX_NO'.
  WA_FCAT-SELTEXT_S = 'Re-Ext No'.
  WA_FCAT-SELTEXT_M = 'No of Re-Extraction'.
  WA_FCAT-SELTEXT_L = 'No of Re-Extraction'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'REX_DATE'.
  WA_FCAT-SELTEXT_S = 'RE-Ext Date'.
  WA_FCAT-SELTEXT_M = 'RE-Extraction Date'.
  WA_FCAT-SELTEXT_L = 'RE-Extraction Date'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'REX_TIME'.
  WA_FCAT-SELTEXT_S = 'RE-Ext Time'.
  WA_FCAT-SELTEXT_M = 'Time of RE-Extraction'.
  WA_FCAT-SELTEXT_L = 'Time of RE-Extraction'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'REX_USER'.
  WA_FCAT-SELTEXT_S = 'RE-Ext User '.
  WA_FCAT-SELTEXT_M = 'Username of RE-Extraction'.
  WA_FCAT-SELTEXT_L = 'Username of RE-Extraction'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'REX_BATCH'.
  WA_FCAT-SELTEXT_S = 'Re-Ext Batch'.
  WA_FCAT-SELTEXT_M = 'Re-Extraction Batch'.
  WA_FCAT-SELTEXT_L = 'Re-Extraction Batch'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'REX_XMLNA'.
  WA_FCAT-SELTEXT_S = 'Re-Ext File Name'.
  WA_FCAT-SELTEXT_M = 'Re-Extraction File Name'.
  WA_FCAT-SELTEXT_L = 'Re-Extraction File Name'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'DOWNLOAD_STATUS'.
  WA_FCAT-SELTEXT_S = 'Status'.
  WA_FCAT-SELTEXT_M = 'Status'.
  WA_FCAT-SELTEXT_L = 'Status'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  CLEAR WA_FCAT.
  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'DOWNLOAD_STATUS1'.
  WA_FCAT-SELTEXT_S = 'Status'.
  WA_FCAT-SELTEXT_M = 'Status'.
  WA_FCAT-SELTEXT_L = 'Status'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'SYID'.
  WA_FCAT-SELTEXT_S = 'STS ID'.
  WA_FCAT-SELTEXT_M = 'STS ID'.
  WA_FCAT-SELTEXT_L = 'STS ID'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'RFID'.
  WA_FCAT-SELTEXT_S = 'Reference ID'.
  WA_FCAT-SELTEXT_M = 'Reference ID'.
  WA_FCAT-SELTEXT_L = 'Reference ID'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'STATUS'.
  WA_FCAT-SELTEXT_S = 'Status'.
  WA_FCAT-SELTEXT_M = 'Status'.
  WA_FCAT-SELTEXT_L = 'Status'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'REA_CODE'.
  WA_FCAT-SELTEXT_S = 'Reason Code'.
  WA_FCAT-SELTEXT_M = 'Reason Code'.
  WA_FCAT-SELTEXT_L = 'Reason Code'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'REASON'.
  WA_FCAT-SELTEXT_S = 'Reason'.
  WA_FCAT-SELTEXT_M = 'Reason'.
  WA_FCAT-SELTEXT_L = 'Reason'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


    I = I + 1.
  WA_FCAT-COL_POS = I.
  WA_FCAT-FIELDNAME = 'ID'.
  WA_FCAT-SELTEXT_S = 'ID'.
  WA_FCAT-SELTEXT_M = 'ID'.
  WA_FCAT-SELTEXT_L = 'ID'.
*wa_fcat-KEY = 'X'.
*wa_fcat-HOTSPOT = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.








  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  WA_LAYOUT-ZEBRA = 'X'.

ENDFORM.                    " FCAT
*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = SY-REPID
*     I_CALLBACK_PF_STATUS_SET          = ' '
      I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      IS_LAYOUT                         = WA_LAYOUT
      IT_FIELDCAT                       = IT_FCAT
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     IT_SORT                           =
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
*     I_SAVE                            = ' '
*     IS_VARIANT                        =
*     IT_EVENTS                         =
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


ENDFORM.                    " ALV

FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.



  CASE R_UCOMM.
    WHEN '&IC1'. "standard Function code for double click
      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'BELNR'.

          clear wa_final.
          READ TABLE it_final INTO wa_final index RS_SELFIELD-TABINDEX.

           IF  sy-subrc = 0.

             set PARAMETER ID 'BLN' FIELD wa_final-belnr.
             set PARAMETER ID 'BUK' FIELD wa_final-BUKRS.
             set PARAMETER ID 'GJR' FIELD wa_final-GJAHR.

             CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

           ENDIF.

      ENDCASE.
  ENDCASE.
ENDFORM.
