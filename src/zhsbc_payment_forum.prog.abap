*&---------------------------------------------------------------------*
*&      Module  DATA_RETRIVE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DATA_RETRIVE OUTPUT.


  CASE BACK_SCREEN.
    WHEN '101'.
      IF V = 0.
        PERFORM EXTRACTION_DATA.
      ENDIF.
    WHEN '102'.
      IF V = 0.
        PERFORM REEXTRACTION_DATA.
      ENDIF.
    WHEN '103'.
      IF V = 0.
        PERFORM EXTRACTION_DATA..
      ENDIF.
    WHEN '104'.
      IF V = 0.
        PERFORM REEXTRACTION_DATA.
      ENDIF.
  ENDCASE.


ENDMODULE.                 " DATA_RETRIVE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.


  IF S_BELNR IS INITIAL.

    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'DOC'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.

  IF S_BUDAT IS INITIAL.

    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'DAT'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.














ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CLEARING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEARING .

  CLEAR :WA_BKPF, WA_BSEG, WA_T012K, WA_LFBK, WA_LFA1, WA_ADR6, WA_SELECT,
         WA_ADRC , WA_FCAT , WA_FCAT_SELECT, WA_FINAL , WA_IMPS , WA_LAYOUT, WA_LAYOUT_SELECT,
         WA_SORT, WA_T001, WA_XML, WA_IMPS, HEAD_IMPS , HEAD_XML , WA_HSBC_ITEMS, WA_DYNPREAD, WA_FINAL, WA_HOUSE,
         WA_HSBC_ITEMS, WA_IMPS, WA_OUTPUT, WA_RETURN, WA_SELECT, WA_SFTP, WA_SORT, WA_XML, WA_ZHSBC.

  REFRESH : IT_BKPF ,IT_BSEG, IT_T012K,IT_LFBK, IT_LFA1, IT_ADR6 ,IT_SELECT ,
            IT_ADRC , IT_FCAT, IT_FCAT_SELECT, IT_FINAL, IT_IMPS ,IT_SORT, IT_T001, IT_XML, IT_HSBC_ITEMS,IT_XML,
            IT_DYNPREAD, IT_HOUSE, IT_HSBC_ITEMS, IT_IMPS, IT_RETURN, IT_ZHSBC, IT_SORT, IT_OUTPUT.

  CLEAR :  TEMP_ID , TEMP_ID1 ,XML_NO, XML_TOTAL , IMPS_NO, IMPS_TOTAL, FILE_NAME_IMPS, FILE_NAME_XML,
          FNAME, FNAME1, BATCH, ADD, ALV_GRID, AMOUNT_EXCEED.

  IF ALV_CONTAINER IS BOUND.

    CALL METHOD ALV_CONTAINER->FREE( ).
    CLEAR ALV_CONTAINER.

  ENDIF.

  IF ALV_SELECT_CONTAINER IS BOUND.

    CALL METHOD ALV_SELECT_CONTAINER->FREE( ).
    CLEAR ALV_SELECT_CONTAINER.

  ENDIF.

  CLEAR :    LO_IXML   ,
             LO_DOC    ,
             M_XMLDOC  ,
             LO_DOCUMENT        ,
             LO_CSTMRCDTTRFINITN,
             LO_GRPHDR          ,
             LO_AUTHSTN         ,
             LO_INITGPTY        ,
             LO_ID              ,
             LO_ORGID           ,
             LO_OTHR            ,
             LO_PMTINF          ,
             LO_PMTTPINF        ,
             LO_SVCLVL          ,
             LO_DBTR            ,
             LO_PSTLADR         ,
             LO_DBTRACCT        ,
             LO_DBTRAGT         ,
             LO_FININSTNID      ,
             LO_CDTTRFTXINF     ,
             LO_PMTID           ,
             LO_AMT             ,
             LO_CDTRAGT         ,
             LO_BIC             ,
             LO_CLRSYSMMBID     ,
             LO_CDTRACCT        ,
             LO_RLTDRMTINF      ,
             LO_RMTLCTNPSTLADR  ,
             LO_ADR             ,
             LO_INSTDAMT        ,
             LO_RMTINF          ,
             LO_RFRDDOCINF      ,
             LO_RFRDDOCAMT      ,
             LO_CDTRREFINF      ,
             LO_TP              ,
             LO_CDORPRTRY       ,
             LO_DUEPYBLAMT      ,
             LO_CDTR            ,
             LO_STRD             .


ENDFORM.                    " CLEARING
*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FCAT .

  REFRESH : IT_FCAT.

  WA_FCAT-COL_POS = '1'.
  WA_FCAT-SCRTEXT_S = 'Clr Doc'.
  WA_FCAT-SCRTEXT_M = 'Clearing Doc'.
  WA_FCAT-SCRTEXT_L = 'Clearing Document'.
  WA_FCAT-FIELDNAME = 'BELNR'.
*  WA_FCAT-OUTPUTLEN = '15'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.

*  WA_FCAT-COL_POS = '2'.
*  WA_FCAT-SCRTEXT_S = 'Item'.
*  WA_FCAT-SCRTEXT_M = 'Line Item'.
*  WA_FCAT-SCRTEXT_L = 'Line Item'.
*  WA_FCAT-FIELDNAME = 'BUZEI'.
**  WA_FCAT-OUTPUTLEN = '15'.
*
*  APPEND WA_FCAT TO IT_FCAT.
*  CLEAR  WA_FCAT.


  WA_FCAT-COL_POS = '3'.
  WA_FCAT-SCRTEXT_S = 'Acc No'.
  WA_FCAT-SCRTEXT_M = 'Account No'.
  WA_FCAT-SCRTEXT_L = 'Account Number'.
  WA_FCAT-FIELDNAME = 'VBANKN'.
*  WA_FCAT-OUTPUTLEN = '18'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.


  WA_FCAT-COL_POS = '4'.
  WA_FCAT-SCRTEXT_S = 'IFSC'.
  WA_FCAT-SCRTEXT_M = 'IFSC Code'.
  WA_FCAT-SCRTEXT_L = 'IFSC Code'.
  WA_FCAT-FIELDNAME = 'VBANKL'.
*  WA_FCAT-OUTPUTLEN = '16'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.

  WA_FCAT-COL_POS = '5'.
  WA_FCAT-SCRTEXT_S = 'Pay Md'.
  WA_FCAT-SCRTEXT_M = 'Payment Method'.
  WA_FCAT-SCRTEXT_L = 'Payment Method'.
  WA_FCAT-FIELDNAME = 'TYPE'.
*  WA_FCAT-OUTPUTLEN = '6'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.


  WA_FCAT-COL_POS = '6'.
  WA_FCAT-SCRTEXT_S = 'Pur Code'.
  WA_FCAT-SCRTEXT_M = 'Purpose Code'.
  WA_FCAT-SCRTEXT_L = 'Purpose Code'.
  WA_FCAT-FIELDNAME = 'PPC'.
*  WA_FCAT-OUTPUTLEN = '6'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.

  IF BACK_SCREEN = '101' OR BACK_SCREEN = '102'.

    WA_FCAT-COL_POS = '7'.
    WA_FCAT-SCRTEXT_S = 'Vendor Code'.
    WA_FCAT-SCRTEXT_M = 'Vendor Code'.
    WA_FCAT-SCRTEXT_L = 'Vendor Code'.
    WA_FCAT-FIELDNAME = 'LIFNR'.
*  WA_FCAT-OUTPUTLEN = '15'.

    APPEND WA_FCAT TO IT_FCAT.
    CLEAR  WA_FCAT.


    WA_FCAT-COL_POS = '8'.
    WA_FCAT-SCRTEXT_S = 'Vendor Name'.
    WA_FCAT-SCRTEXT_M = 'Vendor Name'.
    WA_FCAT-SCRTEXT_L = 'Vendor Name'.
    WA_FCAT-FIELDNAME = 'VKOINH'.
*  WA_FCAT-OUTPUTLEN = '25'.

    APPEND WA_FCAT TO IT_FCAT.
    CLEAR  WA_FCAT.

  ELSEIF BACK_SCREEN = '103' OR BACK_SCREEN = '104'.

    WA_FCAT-COL_POS = '7'.
    WA_FCAT-SCRTEXT_S = 'GL Account'.
    WA_FCAT-SCRTEXT_M = 'GL Account'.
    WA_FCAT-SCRTEXT_L = 'GL Account'.
    WA_FCAT-FIELDNAME = 'HKONT1'.
*  WA_FCAT-OUTPUTLEN = '15'.

    APPEND WA_FCAT TO IT_FCAT.
    CLEAR  WA_FCAT.


    WA_FCAT-COL_POS = '8'.
    WA_FCAT-SCRTEXT_S = 'Name'.
    WA_FCAT-SCRTEXT_M = 'Name'.
    WA_FCAT-SCRTEXT_L = 'Name'.
    WA_FCAT-FIELDNAME = 'VKOINH'.
*  WA_FCAT-OUTPUTLEN = '25'.

    APPEND WA_FCAT TO IT_FCAT.
    CLEAR  WA_FCAT.

  ENDIF.

  WA_FCAT-COL_POS = '9'.
  WA_FCAT-SCRTEXT_S = 'Amount'.
  WA_FCAT-SCRTEXT_M = 'Amount'.
  WA_FCAT-SCRTEXT_L = 'Amount'.
  WA_FCAT-FIELDNAME = 'PSWBT'.
  WA_FCAT-DFIELDNAME = 'TYPE_REF'.
  WA_FCAT-DO_SUM = 'X'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.

  WA_FCAT-COL_POS = '10'.
  WA_FCAT-SCRTEXT_S = 'Currency'.
  WA_FCAT-SCRTEXT_M = 'Currency Key'.
  WA_FCAT-SCRTEXT_L = 'Currency Key'.
  WA_FCAT-FIELDNAME = 'PSWSL'.


  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.

  WA_FCAT-COL_POS = '11'.
  WA_FCAT-SCRTEXT_S = 'Doc Type'.
  WA_FCAT-SCRTEXT_M = 'Document Type'.
  WA_FCAT-SCRTEXT_L = 'Document Type'.
  WA_FCAT-FIELDNAME = 'BLART'.
*  WA_FCAT-OUTPUTLEN = '6'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.

  WA_FCAT-COL_POS = '12'.
  WA_FCAT-SCRTEXT_S = 'User Name'.
  WA_FCAT-SCRTEXT_M = 'User Name'.
  WA_FCAT-SCRTEXT_L = 'User Name'.
  WA_FCAT-FIELDNAME = 'USNAM'.
*  WA_FCAT-OUTPUTLEN = '15'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.

  WA_FCAT-COL_POS = '13'.
  WA_FCAT-SCRTEXT_S = 'Tcode'.
  WA_FCAT-SCRTEXT_M = 'Tcode'.
  WA_FCAT-SCRTEXT_L = 'Tcode'.
  WA_FCAT-FIELDNAME = 'TCODE'.
*  WA_FCAT-OUTPUTLEN = '10'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.



  WA_FCAT-COL_POS = '14'.
  WA_FCAT-SCRTEXT_S = 'Company Code'.
  WA_FCAT-SCRTEXT_M = 'Company Code'.
  WA_FCAT-SCRTEXT_L = 'Company Code'.
  WA_FCAT-FIELDNAME = 'BUKRS'.
*  WA_FCAT-OUTPUTLEN = '5'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.

  WA_FCAT-COL_POS = '15'.
  WA_FCAT-SCRTEXT_S = 'Fiscal Year'.
  WA_FCAT-SCRTEXT_M = 'Fiscal Year'.
  WA_FCAT-SCRTEXT_L = 'Fiscal Year'.
  WA_FCAT-FIELDNAME = 'GJAHR'.
*  WA_FCAT-OUTPUTLEN = '5'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.


  WA_FCAT-COL_POS = '16'.
  WA_FCAT-SCRTEXT_S = 'Posting Date'.
  WA_FCAT-SCRTEXT_M = 'Posting Date'.
  WA_FCAT-SCRTEXT_L = 'Posting Date'.
  WA_FCAT-FIELDNAME = 'BLDAT'.
*  WA_FCAT-OUTPUTLEN = '10'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.



  WA_FCAT-COL_POS = '17'.
  WA_FCAT-SCRTEXT_S = 'Email'.
  WA_FCAT-SCRTEXT_M = 'Email ID'.
  WA_FCAT-SCRTEXT_L = 'Email ID'.
  WA_FCAT-FIELDNAME = 'EMAIL'.
*  WA_FCAT-OUTPUTLEN = '20'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.


  WA_FCAT-COL_POS = '18'.
  WA_FCAT-SCRTEXT_S = 'Reference'.
  WA_FCAT-SCRTEXT_M = 'Reference'.
  WA_FCAT-SCRTEXT_L = 'Reference'.
  WA_FCAT-FIELDNAME = 'REF'.
*  WA_FCAT-OUTPUTLEN = '50'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.

  WA_FCAT-COL_POS = '19'.
  WA_FCAT-SCRTEXT_S = 'XML File'.
  WA_FCAT-SCRTEXT_M = 'XML File'.
  WA_FCAT-SCRTEXT_L = 'XML File'.
  WA_FCAT-FIELDNAME = 'TYPE_REF'.
  WA_FCAT-NO_OUT = 'X'.
*  WA_FCAT-OUTPUTLEN = '15'.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR  WA_FCAT.


  WA_SORT-SPOS = '1'.
  WA_SORT-FIELDNAME = 'TYPE_REF'.
  WA_SORT-UP = 'X'.
  WA_SORT-SUBTOT = 'X'.

  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

  WA_SORT-SPOS = '1'.
  WA_SORT-FIELDNAME = 'BELNR'.
  WA_SORT-UP = 'X'.

  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.






  WA_LAYOUT-ZEBRA = 'X'.
  WA_LAYOUT-CWIDTH_OPT = 'X'.


ENDFORM.                    " FCAT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANCEL' OR 'BACK'.
      CALL SCREEN '100'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  FOLDER_CREATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DIR  text
*      <--P_RES  text
*      <--P_RES1  text
*----------------------------------------------------------------------*
FORM FOLDER_CREATION  USING    P_DIR
                      CHANGING P_RES
                               P_RES1.


  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
    EXPORTING
      DIRECTORY            = P_DIR
    RECEIVING
      RESULT               = P_RES
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      WRONG_PARAMETER      = 3
      NOT_SUPPORTED_BY_GUI = 4
      OTHERS               = 5.
  IF P_RES <> 'X'.
    CLEAR P_RES.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
      EXPORTING
        DIRECTORY                = P_DIR
      CHANGING
        RC                       = P_RES1
      EXCEPTIONS
        DIRECTORY_CREATE_FAILED  = 1
        CNTL_ERROR               = 2
        ERROR_NO_GUI             = 3
        DIRECTORY_ACCESS_DENIED  = 4
        DIRECTORY_ALREADY_EXISTS = 5
        PATH_NOT_FOUND           = 6
        UNKNOWN_ERROR            = 7
        NOT_SUPPORTED_BY_GUI     = 8
        WRONG_PARAMETER          = 9
        OTHERS                   = 10.
    IF P_RES1 = 0.

      P_RES = 'X'.

    ENDIF.


  ENDIF.





ENDFORM.                    " FOLDER_CREATION
*&---------------------------------------------------------------------*
*&      Form  GET_HBKID_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_HBKID_HELP USING VALUE(PP_HBKID) .

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
*      CLEAR WA_HBKID.
*      READ TABLE IT_HBKID INTO WA_HBKID WITH KEY BUKRS = P_BUKRS
*                                                 HBKID = P_HBKID.
*
*      P_HKTID = WA_HBKID-HKTID.

*      CALL SCREEN SY-DYNNR.



    ENDIF.
  ENDIF.



ENDFORM.                    " GET_HBKID_HELP
*&---------------------------------------------------------------------*
*&      Form  DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1308   text
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
*&      Module  HBKID_HELP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HBKID_HELP INPUT.

  PERFORM GET_HBKID_HELP USING 'P_HBKID'.


ENDMODULE.                 " HBKID_HELP  INPUT
*&---------------------------------------------------------------------*
*&      Module  HKTID_HELP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HKTID_HELP INPUT.

  PERFORM GET_HKTID_HELP USING 'P_HKTID'.




ENDMODULE.                 " HKTID_HELP  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_HKTID_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1523   text
*----------------------------------------------------------------------*
FORM GET_HKTID_HELP  USING    VALUE(P_1523).

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
        CLEAR P_HBKID.
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
      DYNPROFIELD     = P_1523
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

ENDFORM.                    " GET_HKTID_HELP
*&---------------------------------------------------------------------*
*&      Form  EXTRACTION_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXTRACTION_DATA .

  REFRESH : IT_T001, IT_ADRC, IT_BKPF.

  SELECT BUKRS
       ADRNR FROM T001 INTO TABLE IT_T001 WHERE BUKRS = P_BUKRS.

  SELECT ADDRNUMBER
         NAME1
         CITY1
         POST_CODE1
         STREET
         HOUSE_NUM1
         STR_SUPPL2
         STR_SUPPL3
         COUNTRY    FROM ADRC INTO TABLE IT_ADRC
                    FOR ALL ENTRIES IN IT_T001
                    WHERE ADDRNUMBER = IT_T001-ADRNR.


  SELECT BUKRS
         BELNR
         GJAHR
         BLART
         BLDAT
         BUDAT
         USNAM
         TCODE FROM BKPF INTO TABLE IT_BKPF
               WHERE BUKRS = P_BUKRS AND
                     GJAHR = P_GJAHR AND
                     BELNR IN S_BELNR AND
                     BUDAT IN S_BUDAT AND BLART = 'KZ'.

  IF IT_BKPF IS INITIAL.

    MESSAGE 'No records found' TYPE 'W'.
    CALL SCREEN BACK_SCREEN.

  ENDIF.

  IF P_HBKID IS NOT INITIAL .

    IF P_HKTID IS INITIAL.

      DATA : WA_HK TYPE ZHSBC_HOUSE_BANK.
      CLEAR WA_HK.
      SELECT *
             UP TO 1 ROWS FROM ZHSBC_HOUSE_BANK INTO WA_HK
             WHERE BUKRS EQ P_BUKRS
             AND   HBKID EQ P_HBKID ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

      P_HKTID = WA_HK-HKTID.

    ENDIF.

    DATA : IT_BSEG1 TYPE TABLE OF TY_BSEG.

    REFRESH : IT_BSEG1.

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

    WA_T012K-HKONT  =  WA_T012K-HKONT + 2.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_T012K-HKONT
      IMPORTING
        OUTPUT = WA_T012K-HKONT.

    IF BACK_SCREEN = '101'.

      SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
           BELNR
           GJAHR
           BUZEI
           BUZID
           BSCHL
           KOART
           SHKZG
           PSWBT
           PSWSL
           HKONT
           LIFNR
           KTOSL FROM BSEG INTO TABLE IT_BSEG1
                 FOR ALL ENTRIES IN IT_BKPF
                 WHERE BUKRS = IT_BKPF-BUKRS AND
                       BELNR = IT_BKPF-BELNR AND
                       GJAHR = IT_BKPF-GJAHR AND
                       HKONT = WA_T012K-HKONT AND
                       KOART = 'S' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

      IF IT_BSEG1 IS NOT INITIAL.

        REFRESH : IT_BSEG.

        SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
               BELNR
               GJAHR
               BUZEI
               BUZID
               BSCHL
               KOART
               SHKZG
               PSWBT
               PSWSL
               HKONT
               LIFNR
               KTOSL FROM BSEG INTO TABLE IT_BSEG
                     FOR ALL ENTRIES IN IT_BSEG1
                     WHERE BUKRS = IT_BSEG1-BUKRS AND
                           BELNR = IT_BSEG1-BELNR AND
                           GJAHR = IT_BSEG1-GJAHR ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
      ELSE.

        MESSAGE 'No records Found.....' TYPE 'S'.
        CALL SCREEN BACK_SCREEN.

      ENDIF.

    ELSEIF BACK_SCREEN = '103'.

      REFRESH : IT_BSEG1.
      SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
             BELNR
             GJAHR
             BUZEI
             BUZID
             BSCHL
             KOART
             SHKZG
             PSWBT
             PSWSL
             HKONT
             LIFNR
             KTOSL FROM BSEG INTO TABLE IT_BSEG1
                   FOR ALL ENTRIES IN IT_BKPF
                   WHERE BUKRS = IT_BKPF-BUKRS AND
                         BELNR = IT_BKPF-BELNR AND
                         GJAHR = IT_BKPF-GJAHR AND
*                         HKONT = WA_T012K-HKONT AND
                         KOART  NE 'S' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

      LOOP AT IT_BKPF INTO WA_BKPF.

        READ TABLE IT_BSEG1 TRANSPORTING NO FIELDS WITH KEY BUKRS = WA_BKPF-BUKRS
                                                            BELNR = WA_BKPF-BELNR
                                                            GJAHR = WA_BKPF-GJAHR .

        IF SY-SUBRC = 0.

          DELETE IT_BKPF WHERE BUKRS = WA_BKPF-BUKRS AND
                               BELNR = WA_BKPF-BELNR AND
                               GJAHR = WA_BKPF-GJAHR .

        ENDIF.


        CLEAR WA_BKPF.


      ENDLOOP.

      IF IT_BKPF IS NOT INITIAL.

        REFRESH : IT_BSEG.

        SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
            BELNR
            GJAHR
            BUZEI
            BUZID
            BSCHL
            KOART
            SHKZG
            PSWBT
            PSWSL
            HKONT
            LIFNR
            KTOSL FROM BSEG INTO TABLE IT_BSEG
                  FOR ALL ENTRIES IN IT_BKPF
                  WHERE BUKRS = IT_BKPF-BUKRS AND
                        BELNR = IT_BKPF-BELNR AND
                        GJAHR = IT_BKPF-GJAHR AND
*                      HKONT = WA_T012K-HKONT AND
                        KOART  = 'S' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
      ELSE.

        MESSAGE 'No records Found' TYPE 'W'.
        CALL SCREEN BACK_SCREEN.

      ENDIF.

    ENDIF.


  ELSE.

    DATA : IT_HBANK TYPE TABLE OF ZHSBC_HOUSE_BANK,
           WA_HBANK TYPE ZHSBC_HOUSE_BANK,
           IT_BSEG2 TYPE TABLE OF TY_BSEG,
           WA_BSEG2 TYPE TY_BSEG.


    REFRESH : IT_HBANK, IT_BSEG2.

    CLEAR : WA_HBANK, WA_BSEG2.

    REFRESH : S_HKONT.

    SELECT * FROM ZHSBC_HOUSE_BANK INTO TABLE IT_HBANK ORDER BY PRIMARY KEY.

    LOOP AT IT_HBANK INTO WA_HBANK.

      AT FIRST. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation

        S_HKONT-SIGN = 'I'.
        S_HKONT-OPTION = 'EQ'.

      ENDAT.

      WA_HBANK-HKONT = WA_HBANK-HKONT + 2.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_HBANK-HKONT
        IMPORTING
          OUTPUT = WA_HBANK-HKONT.

      S_HKONT-LOW = WA_HBANK-HKONT.
      APPEND S_HKONT.
      CLEAR WA_HBANK.

    ENDLOOP.





*
*    SELECT BUKRS
*           BELNR
*           GJAHR
*           BUZEI
*           BUZID
*           KOART
*           PSWBT
*           HKONT
*           LIFNR FROM BSEG INTO TABLE IT_BSEG
*                 FOR ALL ENTRIES IN IT_BKPF
*                 WHERE BUKRS = IT_BKPF-BUKRS AND
*                       BELNR = IT_BKPF-BELNR AND
*                       GJAHR = IT_BKPF-GJAHR.

    SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
      BELNR
      GJAHR
      BUZEI
      BUZID
      BSCHL
      KOART
      SHKZG
      PSWBT
      PSWSL
      HKONT
      LIFNR
      KTOSL FROM BSEG INTO TABLE IT_BSEG2
            FOR ALL ENTRIES IN IT_BKPF
            WHERE BUKRS = IT_BKPF-BUKRS AND
                  BELNR = IT_BKPF-BELNR AND
                  GJAHR = IT_BKPF-GJAHR AND
                  KOART = 'S' AND HKONT IN S_HKONT ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

*    LOOP AT  IT_BSEG2 INTO WA_BSEG2.
*
*      READ TABLE IT_HBANK INTO WA_HBANK WITH KEY WA_BSEG-HKONT.
*
*      IF SY-SUBRC <> 0.
*
*        DELETE IT_BSEG2 WHERE BUKRS = WA_BSEG2-BUKRS AND
*                             BELNR = WA_BSEG2-BELNR AND
*                             GJAHR = WA_BSEG2-GJAHR AND
*                             BUZEI = WA_BSEG2-BUZEI.
*
*      ENDIF.
*
*    ENDLOOP.

    IF IT_BSEG2 IS NOT INITIAL.

      SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
             BELNR
             GJAHR
             BUZEI
             BUZID
             BSCHL
             KOART
             SHKZG
             PSWBT
             PSWSL
             HKONT
             LIFNR
             KTOSL FROM BSEG INTO TABLE IT_BSEG
                   FOR ALL ENTRIES IN IT_BSEG2
                   WHERE BUKRS = IT_BSEG2-BUKRS AND
                         BELNR = IT_BSEG2-BELNR AND
                         GJAHR = IT_BSEG2-GJAHR ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
*        ***********************
      SELECT
        BUKRS_CLR
        BELNR_CLR
        GJAHR_CLR
        BUKRS
        BELNR
        GJAHR
        FROM BSE_CLR INTO TABLE IT_BSE_CLR
        FOR ALL ENTRIES IN IT_BSEG2
        WHERE BUKRS_CLR = IT_BSEG2-BUKRS AND
              BELNR_CLR = IT_BSEG2-BELNR AND
              GJAHR_CLR = IT_BSEG2-GJAHR.

      IF IT_BSE_CLR IS NOT INITIAL.
        SELECT
          BUKRS
          BELNR
          GJAHR
          XBLNR
          FROM BKPF INTO TABLE IT_BKPF_REF
          FOR ALL ENTRIES IN IT_BSE_CLR
          WHERE BUKRS = IT_BSE_CLR-BUKRS AND
            BELNR = IT_BSE_CLR-BELNR AND
            GJAHR = IT_BSE_CLR-GJAHR..

      ENDIF.
*        ***********************
    ELSE.

      MESSAGE 'No records Found' TYPE 'W'.

      CALL SCREEN BACK_SCREEN.

    ENDIF.


  ENDIF.

*      SELECT BUKRS
*             HBKID
*             HKTID
*             BANKN
*             BKONT
*             WAERS
*             HKONT FROM T012K INTO TABLE IT_T012K
*                   FOR ALL ENTRIES IN IT_BSEG
*                   WHERE BUKRS = IT_BSEG-BUKRS AND
*                         HKONT = IT_BSEG-HKONT.
  IF BACK_SCREEN = '101'.

    REFRESH : IT_LFBK, IT_LFA1, IT_ADR6.

    SELECT LIFNR
           BANKS
           BANKL
           BANKN
           KOINH FROM LFBK INTO TABLE IT_LFBK
                 FOR ALL ENTRIES IN IT_BSEG
                 WHERE LIFNR = IT_BSEG-LIFNR.

    SELECT LIFNR
           NAME1
           ADRNR FROM LFA1 INTO TABLE IT_LFA1
                 FOR ALL ENTRIES IN IT_BSEG
                 WHERE LIFNR = IT_BSEG-LIFNR.

    SELECT ADDRNUMBER
           SMTP_ADDR  FROM ADR6 INTO TABLE IT_ADR6
                      FOR ALL ENTRIES IN IT_LFA1
                      WHERE ADDRNUMBER = IT_LFA1-ADRNR .

  ENDIF.

  SELECT MAX( BATCH ) FROM ZHSBC_HEAD INTO BATCH.

  IF SY-SUBRC = 0 AND BATCH IS NOT INITIAL.

    CLEAR ADD.
    ADD = BATCH.
    ADD = ADD + 1.
    CLEAR BATCH.
    BATCH = ADD.

  ELSE.

    BATCH = '1700000001'.

  ENDIF.



  SORT IT_BSEG BY BELNR ASCENDING KOART DESCENDING.

  DATA : TEMP_GL TYPE BSEG-HKONT,
         LV_SUM  TYPE BSEG-PSWBT,
         IT_HOS TYPE TABLE OF ZHSBC_HOUSE_BANK,
         WA_HOS TYPE ZHSBC_HOUSE_BANK.

  CLEAR WA_HOS.
  REFRESH : IT_HOS.

  SELECT * FROM ZHSBC_HOUSE_BANK INTO TABLE IT_HOS.

  IF BACK_SCREEN = '101'.

    LOOP AT IT_BKPF INTO WA_BKPF.


      READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BUKRS = WA_BKPF-BUKRS
                                               BELNR = WA_BKPF-BELNR
                                               GJAHR = WA_BKPF-GJAHR
                                               KOART = 'S'.
      IF SY-SUBRC = 0.

        CLEAR TEMP_GL.
        IF WA_BSEG-HKONT IS NOT INITIAL.
          TEMP_GL = WA_BSEG-HKONT - 2.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = TEMP_GL
          IMPORTING
            OUTPUT = WA_SELECT-HKONT.

        WA_SELECT-BUKRS = WA_BSEG-BUKRS.
        WA_SELECT-GJAHR = WA_BKPF-GJAHR.
        WA_SELECT-BELNR = WA_BSEG-BELNR.
        WA_SELECT-BUDAT = WA_BKPF-BUDAT.
        WA_SELECT-BLDAT = WA_BKPF-BLDAT.
        WA_SELECT-BLART = WA_BKPF-BLART.
        WA_SELECT-USNAM = WA_BKPF-USNAM.
        WA_SELECT-TCODE = WA_BKPF-TCODE.
        WA_SELECT-TYPE  = 'NEFT'.

        READ TABLE IT_HOS INTO WA_HOS WITH KEY HKONT = WA_SELECT-HKONT.

        IF SY-SUBRC = 0.

          CLEAR WA_BSEG.
          READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BUKRS = WA_BKPF-BUKRS
                                                   BELNR = WA_BKPF-BELNR
                                                   GJAHR = WA_BKPF-GJAHR
                                                   KOART = 'K'.
          IF SY-SUBRC = 0.

            WA_SELECT-LIFNR = WA_BSEG-LIFNR.
            WA_SELECT-PSWSL = WA_BSEG-PSWSL.
            READ TABLE IT_LFBK INTO WA_LFBK WITH KEY LIFNR = WA_BSEG-LIFNR.
            IF SY-SUBRC = 0.
              WA_SELECT-VBANKN = WA_LFBK-BANKN.
              WA_SELECT-VBANKL = WA_LFBK-BANKL.

            ENDIF.

            READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_BSEG-LIFNR.

            IF SY-SUBRC = 0.
              WA_SELECT-VKOINH = WA_LFA1-NAME1.
              READ TABLE IT_ADR6 INTO WA_ADR6 WITH KEY ADDRNUMBER = WA_LFA1-ADRNR.
              WA_SELECT-EMAIL = WA_ADR6-SMTP_ADDR.
            ENDIF.

            CLEAR: WA_BSEG, LV_SUM.

            LOOP AT IT_BSEG INTO WA_BSEG WHERE BUKRS = WA_BKPF-BUKRS AND
                                               BELNR = WA_BKPF-BELNR AND
                                               GJAHR = WA_BKPF-GJAHR AND
                                               KOART = 'K'.

              IF WA_BSEG-SHKZG = 'H'.

                WA_BSEG-PSWBT = WA_BSEG-PSWBT * ( -1 ).

              ENDIF.

              LV_SUM = LV_SUM + WA_BSEG-PSWBT.
              CLEAR WA_BSEG.

            ENDLOOP.

            WA_SELECT-PSWBT  = LV_SUM.

            IF WA_SELECT-PSWBT < '200000.00'.

              WA_SELECT-DROPDOWN = '2'.

            ELSEIF WA_SELECT-PSWBT > '5000000.00'.

              WA_SELECT-DROPDOWN = '4'.

              CLEAR WA_SELECT-TYPE.

              WA_SELECT-TYPE  = 'RTGS'.

            ELSE.

              WA_SELECT-DROPDOWN = '1'.

*          ELSEIF WA_SELECT-PSWBT >= '1000000.00'.
*
*            WA_SELECT-DROPDOWN = '1'.
*
*          ELSEIF WA_SELECT-PSWBT < '1000000.00'.
*
*            WA_SELECT-DROPDOWN = '4'.

            ENDIF.

*          IF  WA_SELECT-PSWBT >= '5000000.00'.
*
*            AMOUNT_EXCEED = 'X'.
*
*          ENDIF.



            "TDS Amount *****************************************************************
            DATA : TEMP_TDS TYPE BSEG-PSWBT,
******************************************************
                   TEMP_REF(140),
                   TEMP_REF_TEXT(140),
                   I TYPE INT2.

            CLEAR : TEMP_TDS,TEMP_REF.
            LOOP AT IT_BSE_CLR INTO WA_BSE_CLR WHERE BUKRS_CLR = WA_BKPF-BUKRS AND
                                                      BELNR_CLR = WA_BKPF-BELNR AND
                                                      GJAHR_CLR = WA_BKPF-GJAHR.
              LOOP AT IT_BKPF_REF INTO WA_BKPF_REF  WHERE BUKRS = WA_BSE_CLR-BUKRS AND
                                                      BELNR = WA_BSE_CLR-BELNR AND
                                                      GJAHR = WA_BSE_CLR-GJAHR.
                I = 0.
                TEMP_REF = WA_BKPF_REF-XBLNR.
                IF I = 0.
                  CONCATENATE TEMP_REF '' INTO TEMP_REF_TEXT.
                ELSEIF I => 1.
                  CONCATENATE TEMP_REF ',' INTO TEMP_REF_TEXT.
                ENDIF.
                CLEAR : WA_BKPF_REF.
                I = I + 1.
                IF TEMP_REF IS NOT INITIAL.
                  CONCATENATE TEMP_REF_TEXT ',' WA_SELECT-REF_BKPF INTO WA_SELECT-REF_BKPF.
                ENDIF.
                CLEAR:WA_BKPF_REF.
              ENDLOOP.
              CLEAR:WA_BSE_CLR,TEMP_REF,I.
            ENDLOOP.

**************************************************


            LOOP AT IT_BSEG INTO WA_BSEG WHERE BUKRS = WA_BKPF-BUKRS AND
                                              BELNR = WA_BKPF-BELNR AND
                                              GJAHR = WA_BKPF-GJAHR AND
                                              KTOSL = 'WIT'.

              TEMP_TDS = TEMP_TDS + WA_BSEG-PSWBT.

              CLEAR WA_BSEG.

            ENDLOOP.

            WA_SELECT-TDS = TEMP_TDS.

            WA_SELECT-DROPDOWN1 = '3'.

            CLEAR : LS_STYLEROW, WA_SELECT-FIELD_STYLE.
            LS_STYLEROW-FIELDNAME = 'PPC' .
            LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
            "set field to disabled
            APPEND LS_STYLEROW  TO WA_SELECT-FIELD_STYLE.

*     MODIFY IT_SELECT FROM WA_SELECT.


            APPEND WA_SELECT TO IT_SELECT.
            CLEAR WA_SELECT.
          ENDIF.
        ENDIF.
      ENDIF.


    ENDLOOP.

  ELSEIF BACK_SCREEN = '103'.

    CLEAR: WA_T001, WA_ADRC.

    REFRESH : IT_HOUSE.

    SELECT  * FROM ZHSBC_HOUSE_BANK INTO TABLE IT_HOUSE WHERE HBKID = P_HBKID.


    READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = P_BUKRS.

    READ TABLE IT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = WA_T001-ADRNR.

    LOOP AT  IT_BKPF INTO WA_BKPF.

      READ TABLE IT_BSEG INTO WA_BSEG WITH KEY   BUKRS = WA_BKPF-BUKRS
                                                 BELNR = WA_BKPF-BELNR
                                                 GJAHR = WA_BKPF-GJAHR
                                                 HKONT = WA_T012K-HKONT
                                                 KOART = 'S' BSCHL = '50' .

      IF SY-SUBRC = 0.


        CLEAR TEMP_GL.
        IF WA_BSEG-HKONT IS NOT INITIAL.
          TEMP_GL = WA_BSEG-HKONT - 2.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = TEMP_GL
          IMPORTING
            OUTPUT = WA_SELECT-HKONT.

        WA_SELECT-BUKRS = WA_BSEG-BUKRS.
        WA_SELECT-GJAHR = WA_BKPF-GJAHR.
        WA_SELECT-BELNR = WA_BSEG-BELNR.
        WA_SELECT-BUDAT = WA_BKPF-BUDAT.
        WA_SELECT-BLDAT = WA_BKPF-BLDAT.
        WA_SELECT-BLART = WA_BKPF-BLART.
        WA_SELECT-USNAM = WA_BKPF-USNAM.
        WA_SELECT-TCODE = WA_BKPF-TCODE.
        WA_SELECT-TYPE  = 'NEFT'.
        WA_SELECT-PSWSL  = WA_BSEG-PSWSL.
        WA_SELECT-PSWBT  = WA_BSEG-PSWBT.
        WA_SELECT-VKOINH = WA_ADRC-NAME1.

*        READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BUKRS = WA_BKPF-BUKRS
*                                                 BELNR = WA_BKPF-BELNR
*                                                 GJAHR = WA_BKPF-GJAHR
*                                                 KOART = 'K' BSCHL <> '50' .
*        IF SY-SUBRC = 0.
*
*          WA_SELECT-LIFNR = WA_BSEG-LIFNR.
*          WA_SELECT-PSWSL = WA_BSEG-PSWSL.
*
        CLEAR WA_BSEG.
        LOOP AT IT_BSEG INTO WA_BSEG WHERE   BUKRS = WA_BKPF-BUKRS AND
                                             BELNR = WA_BKPF-BELNR AND
                                             GJAHR = WA_BKPF-GJAHR AND
                                             KOART = 'S' AND BSCHL <> '50' .

          EXIT.

        ENDLOOP.

        IF WA_BSEG IS NOT INITIAL.

          IF WA_BSEG-HKONT IS NOT INITIAL.
            CLEAR TEMP_GL.
            TEMP_GL = WA_BSEG-HKONT - 1.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = TEMP_GL
            IMPORTING
              OUTPUT = WA_SELECT-HKONT1.

          CLEAR WA_HOUSE.
          READ TABLE IT_HOUSE INTO WA_HOUSE WITH KEY HKONT = WA_SELECT-HKONT.

          IF  SY-SUBRC = 0.

            WA_SELECT-EMAIL = WA_HOUSE-EMAIL.

          ENDIF.


          IF WA_SELECT-PSWBT < '200000.00'.

            WA_SELECT-DROPDOWN = '2'.

          ELSEIF WA_SELECT-PSWBT > '5000000.00'.

            WA_SELECT-DROPDOWN = '4'.

            CLEAR WA_SELECT-TYPE.

            WA_SELECT-TYPE  = 'RTGS'.

          ELSE.

            WA_SELECT-DROPDOWN = '1'.

*          ELSEIF WA_SELECT-PSWBT >= '1000000.00'.
*
*            WA_SELECT-DROPDOWN = '1'.
*
*          ELSEIF WA_SELECT-PSWBT < '1000000.00'.
*
*            WA_SELECT-DROPDOWN = '4'.

          ENDIF.
*
*        IF  WA_SELECT-PSWBT >= '5000000.00'.
*
*          AMOUNT_EXCEED = 'X'.
*
*        ENDIF.


          WA_SELECT-DROPDOWN1 = '3'.

          CLEAR : LS_STYLEROW, WA_SELECT-FIELD_STYLE.
          LS_STYLEROW-FIELDNAME = 'PPC' .
          LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          "set field to disabled
          APPEND LS_STYLEROW  TO WA_SELECT-FIELD_STYLE.

*     MODIFY IT_SELECT FROM WA_SELECT.
*******************************************
*            DATA : TEMP_TDS TYPE BSEG-PSWBT,
*                   TEMP_REF(140),
*                   I TYPE INT2.

          CLEAR : TEMP_TDS,TEMP_REF.
          LOOP AT IT_BSE_CLR INTO WA_BSE_CLR WHERE BUKRS_CLR = WA_BKPF-BUKRS AND
                                                    BELNR_CLR = WA_BKPF-BELNR AND
                                                    GJAHR_CLR = WA_BKPF-GJAHR.
            LOOP AT IT_BKPF_REF INTO WA_BKPF_REF  WHERE BUKRS = WA_BSE_CLR-BUKRS AND
                                                    BELNR = WA_BSE_CLR-BELNR AND
                                                    GJAHR = WA_BSE_CLR-GJAHR.
              I = 0.
              TEMP_REF = WA_BKPF_REF-XBLNR.
              IF I = 0.
                CONCATENATE TEMP_REF '' INTO TEMP_REF_TEXT.
              ELSEIF I => 1.
                CONCATENATE TEMP_REF ',' INTO TEMP_REF_TEXT.
              ENDIF.
              CLEAR : WA_BKPF_REF.
              I = I + 1.
              IF TEMP_REF IS NOT INITIAL.
                CONCATENATE TEMP_REF_TEXT ',' WA_SELECT-REF_BKPF INTO WA_SELECT-REF_BKPF.
              ENDIF.
              CLEAR:WA_BKPF_REF.
            ENDLOOP.
            CLEAR:WA_BSE_CLR,TEMP_REF,I.

          ENDLOOP.
          CLEAR:WA_BSE_CLR,TEMP_REF,I.
***************************************

          APPEND WA_SELECT TO IT_SELECT.
          CLEAR WA_SELECT.

        ENDIF.

      ENDIF.
    ENDLOOP.
    REFRESH : IT_HOUSE.
    CLEAR: WA_T001, WA_ADRC.

    TYPES : BEGIN OF TY_MAIN,

               BUKRS TYPE T012K-BUKRS,
               HBKID TYPE T012K-HBKID,
               HKTID TYPE T012K-HKTID,
               HKONT TYPE T012K-HKONT,
               BANKN TYPE T012K-BANKN,
               BANKL TYPE T012-BANKL,
               TEXT1 TYPE T012T-TEXT1,
            END OF TY_MAIN.

    DATA : IT_MAIN TYPE TABLE OF TY_MAIN,
           WA_MAIN TYPE TY_MAIN.

    REFRESH IT_MAIN.
    CLEAR WA_MAIN.

    SELECT T1~BUKRS
           T1~HBKID
           T1~HKTID
           T1~HKONT
           T1~BANKN
           T2~BANKL
           T3~TEXT1 FROM T012K AS T1 JOIN T012 AS T2 ON T1~BUKRS = T2~BUKRS AND T1~HBKID = T2~HBKID JOIN
                         T012T AS T3 ON T1~BUKRS = T3~BUKRS AND T1~HBKID = T3~HBKID AND T1~HKTID = T3~HKTID
                         INTO TABLE IT_MAIN
                         FOR ALL ENTRIES IN IT_SELECT
                         WHERE T1~HKONT = IT_SELECT-HKONT1 AND T1~BUKRS = IT_SELECT-BUKRS.

    LOOP AT IT_SELECT INTO WA_SELECT.
      CLEAR WA_MAIN.
      READ TABLE IT_MAIN INTO WA_MAIN WITH KEY HKONT = WA_SELECT-HKONT1.
      IF SY-SUBRC = 0.

        WA_SELECT-VBANKN = WA_MAIN-BANKN.
        WA_SELECT-VBANKL = WA_MAIN-BANKL.

*      WA_SELECT-VBANKL = WA_main-text1.

        MODIFY IT_SELECT FROM WA_SELECT.
        CLEAR WA_SELECT.

      ENDIF.

    ENDLOOP.


  ENDIF.



  SELECT BUKRS
         GJAHR
         BELNR FROM ZHSBC_ITEMS INTO TABLE IT_ZHSBC
               FOR ALL ENTRIES IN IT_SELECT
               WHERE BUKRS = IT_SELECT-BUKRS AND
                     BELNR = IT_SELECT-BELNR AND
                     GJAHR = IT_SELECT-GJAHR.



  LOOP AT IT_ZHSBC INTO WA_ZHSBC.

    DELETE IT_SELECT WHERE BUKRS = WA_ZHSBC-BUKRS AND
                          BELNR = WA_ZHSBC-BELNR AND
                          GJAHR = WA_ZHSBC-GJAHR.
    CLEAR WA_ZHSBC.

  ENDLOOP.



*  LOOP AT IT_BSEG INTO WA_BSEG.
*
*    IF WA_BSEG-KOART = 'S'.
*      CLEAR TEMP_GL.
*      IF WA_BSEG-HKONT IS NOT INITIAL.
*        TEMP_GL = WA_BSEG-HKONT - 2.
*      ENDIF.
*
*    ELSEIF WA_BSEG-KOART = 'K'.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*         EXPORTING
*           INPUT         = TEMP_GL
*        IMPORTING
*          OUTPUT        = wa_Select-HKONT.
*
*      WA_SELECT-BUKRS = WA_BSEG-BUKRS.
*      WA_SELECT-BELNR = WA_BSEG-BELNR.
*      WA_SELECT-BUZEI = WA_BSEG-BUZEI.
*      WA_SELECT-LIFNR = WA_BSEG-LIFNR.
*
*      READ TABLE IT_LFBK INTO WA_LFBK WITH KEY LIFNR = WA_BSEG-LIFNR.
*
*      WA_SELECT-VBANKN = WA_LFBK-BANKN.
*      WA_SELECT-VBANKL = WA_LFBK-BANKL.
*      WA_SELECT-VKOINH = WA_LFBK-KOINH.
*      WA_SELECT-PSWBT  = WA_BSEG-PSWBT.
*
*      IF WA_SELECT-PSWBT < '200000.00'.
*
*        WA_SELECT-DROPDOWN = '2'.
*
*      ELSE.
*
*        WA_SELECT-DROPDOWN = '1'.
*
*      ENDIF.
*
*      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_BSEG-LIFNR.
*
*      IF SY-SUBRC = 0.
*
*        READ TABLE IT_ADR6 INTO WA_ADR6 WITH KEY ADDRNUMBER = WA_LFA1-ADRNR.
*
*        WA_SELECT-EMAIL = WA_ADR6-SMTP_ADDR.
*
*      ENDIF.
*
*      READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BUKRS = WA_BSEG-BUKRS BELNR = WA_BSEG-BELNR GJAHR = WA_BSEG-GJAHR.
*
*      WA_SELECT-BLART = WA_BKPF-BLART.
*      WA_SELECT-USNAM = WA_BKPF-USNAM.
*      WA_SELECT-TCODE = WA_BKPF-TCODE.
*      WA_SELECT-GJAHR = WA_BKPF-GJAHR.
*      WA_SELECT-BUDAT = WA_BKPF-BUDAT.
*      WA_SELECT-BLDAT = WA_BKPF-BLDAT.
*      WA_SELECT-TYPE = 'NEFT'.
*      APPEND WA_SELECT TO IT_SELECT.
*
*    ENDIF.
*
*    CLEAR : WA_BSEG, WA_BKPF, WA_ADR6, WA_LFA1, WA_LFBK.
*
*  ENDLOOP.

  SELECT BUKRS
         HBKID
         HKTID
         BANKN
         BKONT
         WAERS
         HKONT FROM T012K INTO TABLE IT_T012K
               FOR ALL ENTRIES IN IT_SELECT
               WHERE HKONT = IT_SELECT-HKONT AND
                     BUKRS = P_BUKRS.

  DATA : IT_HOU TYPE TABLE OF ZHSBC_HOUSE_BANK,
         WA_HOU TYPE ZHSBC_HOUSE_BANK.

  CLEAR WA_HOU.

  REFRESH : IT_HOU.

  SELECT * FROM ZHSBC_HOUSE_BANK INTO TABLE IT_HOU.


  LOOP AT IT_SELECT INTO WA_SELECT.


*    READ TABLE IT_T012K INTO WA_T012K WITH KEY HKONT = WA_SELECT-HKONT.
*
*    IF  SY-SUBRC = 0.
*
*      WA_SELECT-BANKN = WA_T012K-BANKN.
*      WA_SELECT-BKONT = WA_T012K-BKONT.
*      WA_SELECT-HBKID = WA_T012K-HBKID.
*      WA_SELECT-HKTID = WA_T012K-HKTID.
*
*      MODIFY IT_SELECT FROM WA_SELECT.
*
*    ENDIF.
*    CLEAR : WA_SELECT , WA_T012K.

    READ TABLE IT_HOU INTO WA_HOU WITH KEY HKONT = WA_SELECT-HKONT.
    IF  SY-SUBRC = 0.
      WA_SELECT-BANKN = WA_HOU-BANKN.
      WA_SELECT-BKONT = WA_HOU-SWIFT.
      WA_SELECT-HBKID = WA_HOU-HBKID.
      WA_SELECT-HKTID = WA_HOU-HKTID.
      MODIFY IT_SELECT FROM WA_SELECT.
    ENDIF.
    CLEAR : WA_SELECT , WA_HOU.
  ENDLOOP.

  IF P_HBKID IS NOT INITIAL.

    DELETE IT_SELECT WHERE HBKID NE P_HBKID.

  ENDIF.

  IF P_HKTID IS NOT INITIAL.

    DELETE IT_SELECT WHERE HKTID NE P_HKTID.

  ENDIF.

  DELETE ADJACENT DUPLICATES FROM IT_SELECT COMPARING ALL FIELDS.

  DELETE IT_SELECT WHERE HKONT IS INITIAL OR
                          BANKN IS INITIAL OR
*                         BKONT IS INITIAL OR
*                          LIFNR IS INITIAL OR
                          VBANKL IS INITIAL OR
                          VBANKN IS INITIAL OR PSWBT <= 0.

  IF  IT_SELECT IS INITIAL.

    MESSAGE 'No records found' TYPE 'W'.
    CALL SCREEN BACK_SCREEN.

  ENDIF.


ENDFORM.                    " EXTRACTION_DATA
*&---------------------------------------------------------------------*
*&      Form  REEXTRACTION_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REEXTRACTION_DATA .

  SELECT MAX( BATCH ) FROM ZHSBC_HEAD INTO BATCH.

  IF SY-SUBRC = 0 AND BATCH IS NOT INITIAL.

    CLEAR ADD.
    ADD = BATCH.
    ADD = ADD + 1.
    CLEAR BATCH.
    BATCH = ADD.

  ELSE.

    BATCH = '1700000001'.

  ENDIF.

  SELECT BUKRS
       ADRNR FROM T001 INTO TABLE IT_T001 WHERE BUKRS = P_BUKRS.

  SELECT ADDRNUMBER
         NAME1
         CITY1
         POST_CODE1
         STREET
         HOUSE_NUM1
         STR_SUPPL2
         STR_SUPPL3
         COUNTRY    FROM ADRC INTO TABLE IT_ADRC
                    FOR ALL ENTRIES IN IT_T001
                    WHERE ADDRNUMBER = IT_T001-ADRNR.

  IF P_HBKID IS NOT INITIAL.

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

*    WA_T012K-HKONT  =  WA_T012K-HKONT + 2.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = WA_T012K-HKONT
*      IMPORTING
*        OUTPUT = WA_T012K-HKONT.

    IF BACK_SCREEN = '102'.

      SELECT * FROM ZHSBC_ITEMS
               INTO TABLE IT_HSBC_ITEMS
               WHERE BUKRS = P_BUKRS
               AND   GJAHR = P_GJAHR
               AND   METHOD = 'VE'
               AND   HKONT = WA_T012K-HKONT
               AND   BUDAT IN S_BUDAT
               AND   BELNR IN S_BELNR
               AND   STATUS = 'RJCT' AND REX_BATCH EQ ' '.
    ELSE.

      SELECT * FROM ZHSBC_ITEMS
              INTO TABLE IT_HSBC_ITEMS
              WHERE BUKRS = P_BUKRS
              AND   GJAHR = P_GJAHR
              AND   METHOD = 'GL'
              AND   HKONT = WA_T012K-HKONT
              AND   BUDAT IN S_BUDAT
              AND   BELNR IN S_BELNR
              AND   STATUS = 'RJCT' AND REX_BATCH EQ ' '.

    ENDIF.


  ELSE.

    IF BACK_SCREEN = '102'.

      SELECT * FROM ZHSBC_ITEMS
              INTO TABLE IT_HSBC_ITEMS
              WHERE BUKRS = P_BUKRS
              AND   GJAHR = P_GJAHR
              AND   METHOD = 'VE'
              AND   BUDAT IN S_BUDAT
              AND   BELNR IN S_BELNR
              AND   STATUS = 'RJCT' AND REX_BATCH EQ ' '.

    ELSE.

      SELECT * FROM ZHSBC_ITEMS
            INTO TABLE IT_HSBC_ITEMS
            WHERE BUKRS = P_BUKRS
            AND   GJAHR = P_GJAHR
            AND   METHOD = 'GL'
            AND   BUDAT IN S_BUDAT
            AND   BELNR IN S_BELNR
            AND   STATUS = 'RJCT' AND REX_BATCH EQ ' '.

    ENDIF.

  ENDIF.


  IF IT_HSBC_ITEMS IS NOT INITIAL .

    LOOP AT IT_HSBC_ITEMS INTO WA_HSBC_ITEMS.

      WA_SELECT-BUKRS       = WA_HSBC_ITEMS-BUKRS       .
      WA_SELECT-BELNR       = WA_HSBC_ITEMS-BELNR       .
      WA_SELECT-GJAHR       = WA_HSBC_ITEMS-GJAHR       .
      WA_SELECT-HKONT       = WA_HSBC_ITEMS-HKONT       .
      WA_SELECT-HBKID       = WA_HSBC_ITEMS-HBKID       .
      WA_SELECT-HKTID       = WA_HSBC_ITEMS-HKTID       .
      WA_SELECT-BANKN       = WA_HSBC_ITEMS-CACNO      .
      WA_SELECT-BKONT       = WA_HSBC_ITEMS-CIFSC       .
      WA_SELECT-LIFNR       = WA_HSBC_ITEMS-LIFNR       .
      WA_SELECT-VBANKL      = WA_HSBC_ITEMS-VIFSC      .
      WA_SELECT-VBANKN      = WA_HSBC_ITEMS-VACNO      .
      WA_SELECT-VKOINH      = WA_HSBC_ITEMS-VNAME      .
      WA_SELECT-PSWBT       = WA_HSBC_ITEMS-TRAMT       .
      WA_SELECT-PSWSL       = WA_HSBC_ITEMS-PSWSL.
      WA_SELECT-EMAIL       = WA_HSBC_ITEMS-VEMIL       .
      WA_SELECT-TYPE        = WA_HSBC_ITEMS-PTYPE        .
      WA_SELECT-PPC         = WA_HSBC_ITEMS-PURCD         .
      WA_SELECT-BLART       = WA_HSBC_ITEMS-BLART       .
      WA_SELECT-BUDAT       = WA_HSBC_ITEMS-BUDAT       .
      WA_SELECT-BLDAT       = WA_HSBC_ITEMS-BLDAT       .
      WA_SELECT-USNAM       = WA_HSBC_ITEMS-UNAME       .
      WA_SELECT-TCODE       = WA_HSBC_ITEMS-TCODE       .
      WA_SELECT-REF         = WA_HSBC_ITEMS-REFER         .
      WA_SELECT-HKONT1      = WA_HSBC_ITEMS-HKONT1.
*       wa_select-DROPDOWN    = wa_hsbc_items-DROPDOWN    .
*       wa_select-DROPDOWN1   = wa_hsbc_items-DROPDOWN1   .
*       wa_select-FIELD_STYLE = wa_hsbc_items-FIELD_STYLE .



      IF WA_SELECT-PSWBT < '200000.00'.

        WA_SELECT-DROPDOWN = '2'.

      ELSEIF WA_SELECT-PSWBT > '5000000.00'.

        WA_SELECT-DROPDOWN = '4'.

        CLEAR WA_SELECT-TYPE.

        WA_SELECT-TYPE  = 'RTGS'.

      ELSE.

        WA_SELECT-DROPDOWN = '1'.

*          ELSEIF WA_SELECT-PSWBT >= '1000000.00'.
*
*            WA_SELECT-DROPDOWN = '1'.
*
*          ELSEIF WA_SELECT-PSWBT < '1000000.00'.
*
*            WA_SELECT-DROPDOWN = '4'.

      ENDIF.

*      IF  WA_SELECT-PSWBT >= '1000000.00'.
*
*        AMOUNT_EXCEED = 'X'.
*
*      ENDIF.


      WA_SELECT-DROPDOWN1 = '3'.

      CLEAR : LS_STYLEROW, WA_SELECT-FIELD_STYLE.
      LS_STYLEROW-FIELDNAME = 'PPC' .
      LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      "set field to disabled
      APPEND LS_STYLEROW  TO WA_SELECT-FIELD_STYLE.

*     MODIFY IT_SELECT FROM WA_SELECT.


      APPEND WA_SELECT TO IT_SELECT.
      CLEAR : WA_SELECT , WA_HSBC_ITEMS.

    ENDLOOP.






  ELSE.

    MESSAGE 'No records Found' TYPE 'W'.
    CALL SCREEN '102'.

  ENDIF.













ENDFORM.                    " REEXTRACTION_DATA
*&---------------------------------------------------------------------*
*&      Module  VALIDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDATE INPUT.

  IF BACK_SCREEN = '103' OR BACK_SCREEN = '104'.

    IF P_HBKID IS INITIAL OR P_HKTID IS INITIAL.

      MESSAGE 'Enter House Bank and Account ID....' TYPE 'E'.

    ENDIF.

  ENDIF.

ENDMODULE.                 " VALIDATE  INPUT
