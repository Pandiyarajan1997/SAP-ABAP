*----------------------------------------------------------------------*
***INCLUDE ZHSBC_PAYMENT_XML.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  XML_DATA_CONVERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM XML_DATA_CONVERS .

  SELECT SINGLE * FROM ZHSBC_SFTP INTO WA_SFTP.


        CALL FUNCTION 'SXPG_COMMAND_CHECK'
      EXPORTING
        COMMANDNAME                      = 'ZHSBC_SFTP'
*       OPERATINGSYSTEM                  = SY-OPSYS
*       TARGETSYSTEM                     =
*       ADDITIONAL_PARAMETERS            =
*       DESTINATION                      =
*       LONG_PARAMS                      =
*       EXT_USER                         = SY-UNAME
*     IMPORTING
*       PROGRAMNAME                      =
*       DEFINED_PARAMETERS               =
*       ALL_PARAMETERS                   =
     EXCEPTIONS
       NO_PERMISSION                    = 1
       COMMAND_NOT_FOUND                = 2
       PARAMETERS_TOO_LONG              = 3
       SECURITY_RISK                    = 4
       WRONG_CHECK_CALL_INTERFACE       = 5
       X_ERROR                          = 6
       TOO_MANY_PARAMETERS              = 7
       PARAMETER_EXPECTED               = 8
       ILLEGAL_COMMAND                  = 9
       COMMUNICATION_FAILURE            = 10
       SYSTEM_FAILURE                   = 11
       OTHERS                           = 12
              .
    IF SY-SUBRC <> 0.

      IF  sy-subrc = 1.

        MESSAGE 'No Authorization to Access OS Command.....' TYPE 'S'.

        call SCREEN '100'.

      else.

        MESSAGE 'OS Command Connection gets failed.....' TYPE 'S'.
        call SCREEN '100'.

      ENDIF.

    ENDIF.

    data : activity TYPE char30,
       FP TYPE AUTHB-FILENAME.

cleAR : FP, activity.

activity = 'WRITE'.
FP = WA_SFTP-FORUM_PATH.

    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
  EXPORTING
*   PROGRAM                =
    ACTIVITY               = activity
    FILENAME               = FP
 EXCEPTIONS
   NO_AUTHORITY           = 1
   ACTIVITY_UNKNOWN       = 2
   OTHERS                 = 3
          .
IF SY-SUBRC <> 0.

MESSAGE 'No authorization to access Application server file' TYPE 'S'.
call screen '100'.

ENDIF.


  IF SY-SUBRC <> 0 OR WA_SFTP-FORUM_PATH IS INITIAL OR WA_SFTP-FAILED_PATH IS INITIAL OR WA_SFTP-SUCCESS_PATH IS INITIAL .

    MESSAGE 'Please Maintain the Server Connectivity in ZHSBC' TYPE 'W'.
    call screen '100'.

  ENDIF.

  IF P_HBKID IS NOT INITIAL AND P_HKTID IS NOT INITIAL.

    REFRESH : IT_HOUSE.

    SELECT * FROM ZHSBC_HOUSE_BANK INTO TABLE IT_HOUSE WHERE BUKRS = P_BUKRS AND
                                                             HBKID = P_HBKID AND
                                                             HKTID = P_HKTID ORDER BY PRIMARY KEY.
    CLEAR : WA_HOUSE.
    READ TABLE IT_HOUSE INTO WA_HOUSE INDEX 1. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation


  ELSE.

    READ TABLE IT_FINAL INTO WA_FINAL INDEX 1.

    SELECT * FROM ZHSBC_HOUSE_BANK INTO TABLE IT_HOUSE WHERE BUKRS = P_BUKRS AND
                                                             HBKID = WA_FINAL-HBKID AND
                                                             HKTID = WA_FINAL-HKTID ORDER BY PRIMARY KEY.
    CLEAR : WA_HOUSE.
    READ TABLE IT_HOUSE INTO WA_HOUSE INDEX 1. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation

  ENDIF.

  IF IT_HOUSE IS INITIAL.

    MESSAGE 'Kindly Maintain House Bank details in ZHSBC' TYPE 'W'.
    CALL SCREEN '100'.

  ENDIF.


  SORT IT_FINAL BY TYPE_REF TYPE BELNR.

  READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = P_BUKRS.

  READ TABLE IT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = WA_T001-ADRNR.

  DATA : TEMP_TEXT TYPE STRING,
         TEMP_NAME_XML TYPE STRING.
*         temps TYPE string.

  CONCATENATE 'SHEENLAC_HSBC' SY-DATUM SY-UZEIT INTO TEMP_NAME_XML.
  CONDENSE TEMP_NAME_XML.

  CLEAR : FILE_NAME_XML.

  FILE_NAME_XML = TEMP_NAME_XML.

  REFRESH : it_xml.
  clear wa_xml.


  LOOP AT IT_FINAL INTO WA_FINAL WHERE TYPE_REF = '1'.             "NEFT/RTGS Record


    WA_XML-MANDT = SY-MANDT.
    WA_XML-BATCH = BATCH.
    WA_XML-BNKID = WA_HOUSE-ABCID.
    WA_XML-XMLNA = TEMP_NAME_XML.
    WA_XML-BUKRS = WA_FINAL-BUKRS.
    WA_XML-GJAHR = WA_FINAL-GJAHR.
    WA_XML-BELNR = WA_FINAL-BELNR.
    WA_XML-BUDAT = WA_FINAL-BUDAT.
    WA_XML-BLDAT = WA_FINAL-BLDAT.
    WA_XML-BLART = WA_FINAL-BLART.
*    WA_XML-BUZEI = WA_FINAL-BUZEI.
    WA_XML-HKONT = WA_FINAL-HKONT.
    WA_XML-HBKID = WA_FINAL-HBKID.
    WA_XML-HKTID = WA_FINAL-HKTID .
    WA_XML-PAYID = 'TRF'.

    WA_XML-PTYPE = WA_FINAL-TYPE.
    WA_XML-TRDAT = SY-DATUM.

    WA_XML-CMDET = WA_ADRC-NAME1.
    WA_XML-CADR1 = WA_ADRC-HOUSE_NUM1.
    WA_XML-UNAME = WA_FINAL-USNAM.
    WA_XML-TCODE = WA_FINAL-TCODE.

    CLEAR TEMP_TEXT.
    CONCATENATE WA_ADRC-STREET WA_ADRC-STR_SUPPL3 INTO TEMP_TEXT.
    CONDENSE TEMP_TEXT.

    WA_XML-CADR2 = TEMP_TEXT.
    WA_XML-CADR3 = WA_ADRC-CITY1.
    WA_XML-CPOST = WA_ADRC-POST_CODE1.
    WA_XML-CCODE = WA_ADRC-COUNTRY.
    WA_XML-CACNO = WA_HOUSE-BANKN.
    WA_XML-CIFSC = WA_HOUSE-SWIFT.
    WA_XML-TRCHA = 'DEBT'.
    WA_XML-TRAMT = WA_FINAL-PSWBT.
if back_screen = '101' or back_screen = '102'.
    WA_XML-LIFNR = WA_FINAL-LIFNR.
ELSEIF back_screen = '103' or back_screen = '104'.
    WA_XML-hkont1 = WA_FINAL-hkont1.
endif.
    WA_XML-VIFSC = WA_FINAL-VBANKL.
    WA_XML-VNAME = WA_FINAL-VKOINH.
    WA_XML-VCCOD = WA_ADRC-COUNTRY..
    WA_XML-VACNO = WA_FINAL-VBANKN.
    WA_XML-ADVTY = 'EMAL'.
    WA_XML-VEMIL = WA_FINAL-EMAIL.
    IF WA_FINAL-TYPE = 'RTGS'.
      WA_XML-PURCD = WA_FINAL-PPC.
    ENDIF.
*    WA_XML-REFER = WA_FINAL-REF.
    WA_XML-REFER = WA_FINAL-REF_BKPF.
    WA_XML-PSWSL = WA_FINAL-PSWSL.
    clear cky.
    cky = wa_final-pswsl.
    WA_XML-EXTR_DATE = SY-DATUM.
    WA_XML-EXTR_TIME = SY-UZEIT.
    WA_XML-EXTR_USER = SY-UNAME.
    WA_XML-TDS = WA_FINAL-TDS.

    APPEND WA_XML TO IT_XML.
    CLEAR: WA_FINAL, WA_XML.

  ENDLOOP.

  CLEAR TEMP_NAME_XML.
  CONCATENATE 'SHEENLAC_IMPS' SY-DATUM SY-UZEIT INTO TEMP_NAME_XML.
  CONDENSE TEMP_NAME_XML.
  FILE_NAME_IMPS = TEMP_NAME_XML.

  LOOP AT IT_FINAL INTO WA_FINAL WHERE TYPE_REF = '2'.

    WA_IMPS-MANDT = SY-MANDT.
    WA_IMPS-BATCH = BATCH.
    WA_IMPS-BNKID = WA_HOUSE-ABCID.
    WA_IMPS-XMLNA = TEMP_NAME_XML.
    WA_IMPS-BUKRS = WA_FINAL-BUKRS.
    WA_IMPS-GJAHR = WA_FINAL-GJAHR.
    WA_IMPS-BELNR = WA_FINAL-BELNR.
    WA_IMPS-BUDAT = WA_FINAL-BUDAT.
    WA_IMPS-BLDAT = WA_FINAL-BLDAT.
    WA_IMPS-BLART = WA_FINAL-BLART.
*    WA_IMPS-BUZEI = WA_FINAL-BUZEI.
    WA_IMPS-PAYID = 'TRF'.

    WA_IMPS-PTYPE = WA_FINAL-TYPE.
    WA_IMPS-TRDAT = SY-DATUM.
    WA_IMPS-TRAMT = WA_FINAL-PSWBT.

    WA_IMPS-CMDET = WA_ADRC-NAME1.
    WA_IMPS-CADR1 = WA_ADRC-HOUSE_NUM1.
    WA_IMPS-UNAME = WA_FINAL-USNAM.
    WA_IMPS-TCODE = WA_FINAL-TCODE.

    CLEAR TEMP_TEXT.
    CONCATENATE WA_ADRC-STREET WA_ADRC-STR_SUPPL3 INTO TEMP_TEXT.
    CONDENSE TEMP_TEXT.

    WA_IMPS-CADR2 = TEMP_TEXT.
    WA_IMPS-CADR3 = WA_ADRC-CITY1.
    WA_IMPS-CPOST = WA_ADRC-POST_CODE1.
    WA_IMPS-CCODE = WA_ADRC-COUNTRY.
    WA_IMPS-CACNO = WA_HOUSE-BANKN.
    WA_IMPS-CIFSC = WA_HOUSE-SWIFT.
    WA_IMPS-TRCHA = 'DEBT'.
    WA_IMPS-HKONT = WA_FINAL-HKONT.
    WA_IMPS-HBKID = WA_FINAL-HBKID.
    WA_IMPS-HKTID = WA_FINAL-HKTID .
   if back_screen = '101' or back_screen = '102'.
    WA_imps-LIFNR = WA_FINAL-LIFNR.
ELSEIF back_screen = '103' or back_screen = '104'.
    WA_imps-hkont1 = WA_FINAL-hkont1.
endif.
    WA_IMPS-TRAMT = WA_FINAL-PSWBT.
    WA_IMPS-VIFSC = WA_FINAL-VBANKL.
    WA_IMPS-VNAME = WA_FINAL-VKOINH.
    WA_IMPS-VCCOD = WA_ADRC-COUNTRY..
    WA_IMPS-VACNO = WA_FINAL-VBANKN.
    WA_IMPS-PSWSL = wa_final-pswsl.

    clear cky.
    cky = wa_final-pswsl.

    WA_IMPS-EXTR_DATE = SY-DATUM.
    WA_IMPS-EXTR_TIME = SY-UZEIT.
    WA_IMPS-EXTR_USER = SY-UNAME.
    APPEND WA_IMPS TO IT_IMPS.
    CLEAR: WA_IMPS , WA_FINAL.


  ENDLOOP.







ENDFORM.                    " XML_DATA_CONVERS
*&---------------------------------------------------------------------*
*&      Form  XML_FILE_GENERATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM XML_FILE_GENERATION .

  DATA : TEMP TYPE STRING,
         TEMP1 TYPE STRING,
         TEMPS TYPE STRING.

  data : wa_mail TYPE zhsbc_mail.

  clear wa_mail.

    SELECT * UP TO 1 ROWS FROM zhsbc_mail INTO wa_mail ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

  IF IT_XML IS NOT INITIAL.


    TEMP = XML_NO.
    TEMP1 = XML_TOTAL.
    CONDENSE : TEMP, TEMP1.




    DATA : INITIAL,
          INVOICE_NO TYPE STRING,
          LV_LIFNR TYPE BSEG-LIFNR.

    DATA: LV_MES TYPE I.

    LOOP AT IT_XML INTO WA_XML.

      IF INITIAL IS INITIAL.

        LO_IXML = CL_IXML=>CREATE( ).

        LO_DOC  = LO_IXML->CREATE_DOCUMENT( ).


        LO_DOCUMENT  = LO_DOC->CREATE_SIMPLE_ELEMENT_NS(
                                            NAME    = 'Document'
                                            PARENT  = LO_DOC ).

        LO_DOCUMENT->SET_ATTRIBUTE_NS(
                                       NAME = 'xmlns'
                                       VALUE = 'urn:iso:std:iso:20022:tech:xsd:pain.001.001.03' ).

        LO_DOCUMENT->SET_ATTRIBUTE_NS( PREFIX = 'xmlns'
                                    NAME = 'xsi'
                                    VALUE = 'http://www.w3.org/2001/XMLSchema-instance' ).

        LO_CSTMRCDTTRFINITN  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'CstmrCdtTrfInitn'
                                            PARENT  = LO_DOCUMENT ).




        LO_GRPHDR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                             NAME    = 'GrpHdr'
                                             PARENT  = LO_CSTMRCDTTRFINITN ).
        CLEAR TEMP_ID.
        CONDENSE WA_XML-XMLNA.
        TEMP_ID = WA_XML-XMLNA.
        CONDENSE : TEMP_ID.
        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'MsgId'
                                            PARENT  = LO_GRPHDR
                                            VALUE   = TEMP_ID  ).

        CLEAR TEMP_ID.

        CONCATENATE SY-DATUM(4)'-' SY-DATUM+4(2)'-' SY-DATUM+6(2) 'T' SY-UZEIT(2) ':' SY-UZEIT+2(2) ':' SY-UZEIT+4(2) INTO TEMP_ID.
        CONDENSE TEMP_ID.
        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'CreDtTm'
                                            PARENT  = LO_GRPHDR
                                            VALUE   = TEMP_ID ).

        LO_AUTHSTN  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'Authstn'
                                            PARENT  = LO_GRPHDR ).

        CLEAR :TEMP_ID.
        TEMP_ID = 'ILEV'.
        CONDENSE TEMP_ID.
        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Cd'
                                            PARENT  = LO_AUTHSTN
                                            VALUE   = TEMP_ID ).
        CLEAR :TEMP_ID.
        TEMP_ID = XML_NO.
        CONDENSE TEMP_ID.
        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'NbOfTxs'
                                            PARENT  = LO_GRPHDR
                                            VALUE   = TEMP_ID ).
        CLEAR :TEMP_ID.
        TEMP_ID = XML_TOTAL.
        CONDENSE TEMP_ID.
        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'CtrlSum'
                                            PARENT  = LO_GRPHDR
                                            VALUE   = TEMP_ID  ).

        LO_INITGPTY = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'InitgPty'
                                            PARENT  = LO_GRPHDR ).

        LO_ID = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'Id'
                                            PARENT  = LO_INITGPTY ).

        LO_ORGID = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'OrgId'
                                            PARENT  = LO_ID ).

        LO_OTHR = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'Othr'
                                            PARENT  = LO_ORGID ).
        CLEAR :TEMP_ID.

        TEMP_ID = WA_XML-BNKID.
        CONDENSE TEMP_ID.

        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Id'
                                            PARENT  = LO_OTHR
                                            VALUE   = TEMP_ID ).

        INITIAL = 'X'.

      ENDIF.

************************************************               Payment Info              ******************************************************************



      LO_PMTINF  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'PmtInf'
                                          PARENT  = LO_CSTMRCDTTRFINITN ).


      CLEAR :INVOICE_NO.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT  = WA_XML-BUZEI
*          IMPORTING
*            OUTPUT = WA_XML-BUZEI.

      INVOICE_NO = WA_XML-BELNR .
      CONDENSE INVOICE_NO.

      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'PmtInfId'
                                          PARENT  = LO_PMTINF
                                          VALUE   = INVOICE_NO ).


      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-PAYID.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'PmtMtd'
                                          PARENT  = LO_PMTINF
                                          VALUE   = TEMP_ID ).

      LO_PMTTPINF  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                        NAME    = 'PmtTpInf'
                                        PARENT  = LO_PMTINF ).

      LO_SVCLVL  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                         NAME    = 'SvcLvl'
                                         PARENT  = LO_PMTTPINF  ).

      IF WA_XML-PTYPE = 'NEFT'.
        CLEAR :TEMP_ID.
        TEMP_ID = 'URNS'.
        CONDENSE TEMP_ID.
        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Cd'
                                          PARENT  = LO_SVCLVL
                                          VALUE   = TEMP_ID ).


      ELSEIF WA_XML-PTYPE = 'RTGS'.

        CLEAR :TEMP_ID.
        TEMP_ID = 'URGP'.
        CONDENSE TEMP_ID.
        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Cd'
                                          PARENT  = LO_SVCLVL
                                          VALUE   = TEMP_ID ).

      ENDIF.





      CLEAR :TEMP_ID.
      CONCATENATE  WA_XML-TRDAT(4) '-' WA_XML-TRDAT+4(2)'-' WA_XML-TRDAT+6(2) INTO TEMP_ID .
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'ReqdExctnDt'
                                          PARENT  = LO_PMTINF
                                          VALUE   = TEMP_ID ).

      LO_DBTR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'Dbtr'
                                       PARENT  = LO_PMTINF ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CMDET.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Nm'
                                          PARENT  = LO_DBTR
                                          VALUE   = TEMP_ID ).

      LO_PSTLADR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'PstlAdr'
                                           PARENT  = LO_DBTR  ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CADR2.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'StrtNm'
                                          PARENT  = LO_PSTLADR
                                          VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CADR1.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'BldgNb'
                                          PARENT  = LO_PSTLADR
                                          VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CPOST.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'PstCd'
                                          PARENT  = LO_PSTLADR
                                          VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CADR3.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'TwnNm'
                                          PARENT  = LO_PSTLADR
                                          VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CCODE.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                          PARENT  = LO_PSTLADR
                                          VALUE   = TEMP_ID ).


      LO_DBTRACCT  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                     NAME    = 'DbtrAcct'
                                     PARENT  = LO_PMTINF ).


      LO_ID = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                        NAME    = 'Id'
                                        PARENT  = LO_DBTRACCT ).

      LO_OTHR = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'Othr'
                                          PARENT  = LO_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CACNO.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Id'
                                          PARENT  = LO_OTHR
                                          VALUE   = TEMP_ID ).


      LO_DBTRAGT  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'DbtrAgt'
                                          PARENT  = LO_PMTINF ).

      LO_FININSTNID  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'FinInstnId'
                                           PARENT  = LO_DBTRAGT ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CIFSC.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'BIC'
                                          PARENT  = LO_FININSTNID
                                          VALUE   = TEMP_ID ).



      LO_PSTLADR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'PstlAdr'
                                           PARENT  = LO_FININSTNID  ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CCODE.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                          PARENT  = LO_PSTLADR
                                          VALUE   = TEMP_ID ).


      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-TRCHA.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'ChrgBr'
                                          PARENT  = LO_PMTINF
                                          VALUE   = TEMP_ID ).

      LO_CDTTRFTXINF  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                        NAME    = 'CdtTrfTxInf'
                                        PARENT  = LO_PMTINF ).

      LO_PMTID  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                         NAME    = 'PmtId'
                                         PARENT  = LO_CDTTRFTXINF ).

*      CLEAR :TEMP_ID.
*      TEMP_ID = WA_xml-CMREF.

      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'InstrId'
                                          PARENT  = LO_PMTID
                                          VALUE   = INVOICE_NO ).

*      CLEAR :TEMP_ID.
*      TEMP_ID = WA_xml-PYREF.

      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'EndToEndId'
                                          PARENT  = LO_PMTID
                                          VALUE   = INVOICE_NO ).

      LO_AMT  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                         NAME    = 'Amt'
                                         PARENT  = LO_CDTTRFTXINF ).


      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-TRAMT .
      CONDENSE TEMP_ID.

      LO_INSTDAMT  = LO_DOC->CREATE_SIMPLE_ELEMENT_NS(
                                            NAME    = 'InstdAmt'
                                            PARENT  = LO_AMT
                                            VALUE = TEMP_ID ).

      LO_INSTDAMT->SET_ATTRIBUTE_NS(
                                     NAME = 'Ccy'
                                     VALUE = 'INR' ).

      LO_CDTRAGT  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'CdtrAgt'
                                       PARENT  = LO_CDTTRFTXINF ).

      LO_FININSTNID  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'FinInstnId'
                                       PARENT  = LO_CDTRAGT ).


*        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'BIC'
*                                             PARENT  = LO_FININSTNID
*                                       ).


      LO_CLRSYSMMBID = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'ClrSysMmbId'
                                       PARENT  = LO_FININSTNID ).



      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-VIFSC.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'MmbId'
                                            PARENT  = LO_CLRSYSMMBID
                                            VALUE   = TEMP_ID ).

      LO_PSTLADR = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'PstlAdr'
                                       PARENT  = LO_FININSTNID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CCODE.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                            PARENT  = LO_PSTLADR
                                            VALUE   = TEMP_ID ).

      LO_CDTR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'Cdtr'
                                          PARENT  = LO_CDTTRFTXINF  ).


      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-VNAME.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Nm'
                                            PARENT  = LO_CDTR
                                            VALUE   = TEMP_ID ).

      LO_PSTLADR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'PstlAdr'
                                           PARENT  = LO_CDTR ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CCODE.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                            PARENT  = LO_PSTLADR
                                            VALUE   = TEMP_ID ).

      LO_CDTRACCT  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'CdtrAcct'
                                           PARENT  = LO_CDTTRFTXINF  ).

      LO_ID  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'Id'
                                           PARENT  = LO_CDTRACCT ).

      LO_OTHR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'Othr'
                                          PARENT  = LO_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-VACNO.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Id'
                                            PARENT  =  LO_OTHR
                                            VALUE   = TEMP_ID ).

      IF wa_xml-VEMIL is NOT INITIAL.

      LO_RLTDRMTINF  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'RltdRmtInf'
                                           PARENT  = LO_CDTTRFTXINF  ).
      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-ADVTY.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RmtLctnMtd'
                                            PARENT  =  LO_RLTDRMTINF
                                            VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-VEMIL.
      CONDENSE TEMP_ID.

      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RmtLctnElctrncAdr'
                                           PARENT  =  LO_RLTDRMTINF
                                           VALUE   = TEMP_ID ).



*
*     IF wa_mail-EMAIL_2 is NOT INITIAL.
*
*       CLEAR :TEMP_ID.
*      TEMP_ID = WA_XML-ADVTY.
*      CONDENSE TEMP_ID.
*      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RmtLctnMtd'
*                                            PARENT  =  LO_RLTDRMTINF
*                                            VALUE   = TEMP_ID ).
*
*      CLEAR :TEMP_ID.
*      TEMP_ID = wa_mail-EMAIL_2.
*      CONDENSE TEMP_ID.
*
*      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RmtLctnElctrncAdr'
*                                           PARENT  =  LO_RLTDRMTINF
*                                           VALUE   = TEMP_ID ).
*
*
*      ENDIF.

*        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RmtLctnPstlAdr'
*                                               PARENT  =  LO_RLTDRMTINF
*                                                ).

      LO_RMTLCTNPSTLADR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'RmtLctnPstlAdr'
                                            PARENT  = LO_RLTDRMTINF ).
      IF WA_XML-VNAME IS NOT INITIAL.

        CLEAR :TEMP_ID.
        TEMP_ID = WA_XML-VNAME.
        CONDENSE TEMP_ID.

        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Nm'
                                             PARENT  =  LO_RMTLCTNPSTLADR
                                             VALUE   = TEMP_ID ).
      ENDIF.

      LO_ADR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'Adr'
                                            PARENT  = LO_RMTLCTNPSTLADR ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CCODE.
      CONDENSE TEMP_ID.

      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                           PARENT  =  LO_ADR
                                           VALUE   = TEMP_ID ).

       endif.
       IF wa_mail-EMAIL_1 is NOT INITIAL.

        LO_RLTDRMTINF  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'RltdRmtInf'
                                           PARENT  = LO_CDTTRFTXINF  ).

        CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-ADVTY.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RmtLctnMtd'
                                            PARENT  =  LO_RLTDRMTINF
                                            VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = wa_mail-EMAIL_1.
      CONDENSE TEMP_ID.

      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RmtLctnElctrncAdr'
                                           PARENT  =  LO_RLTDRMTINF
                                           VALUE   = TEMP_ID ).

      LO_RMTLCTNPSTLADR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'RmtLctnPstlAdr'
                                            PARENT  = LO_RLTDRMTINF ).
      IF WA_XML-cmdet IS NOT INITIAL.

        CLEAR :TEMP_ID.
        TEMP_ID = WA_XML-cmdet.
        CONDENSE TEMP_ID.

        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Nm'
                                             PARENT  =  LO_RMTLCTNPSTLADR
                                             VALUE   = TEMP_ID ).
      ENDIF.

      LO_ADR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'Adr'
                                            PARENT  = LO_RMTLCTNPSTLADR ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CCODE.
      CONDENSE TEMP_ID.

      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                           PARENT  =  LO_ADR
                                           VALUE   = TEMP_ID ).


      ENDIF.


  IF wa_mail-EMAIL_2 is NOT INITIAL.

        LO_RLTDRMTINF  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'RltdRmtInf'
                                           PARENT  = LO_CDTTRFTXINF  ).

        CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-ADVTY.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RmtLctnMtd'
                                            PARENT  =  LO_RLTDRMTINF
                                            VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = wa_mail-EMAIL_2.
      CONDENSE TEMP_ID.

      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RmtLctnElctrncAdr'
                                           PARENT  =  LO_RLTDRMTINF
                                           VALUE   = TEMP_ID ).

      LO_RMTLCTNPSTLADR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'RmtLctnPstlAdr'
                                            PARENT  = LO_RLTDRMTINF ).
      IF WA_XML-CMDET IS NOT INITIAL.

        CLEAR :TEMP_ID.
        TEMP_ID = WA_XML-cmdet.
        CONDENSE TEMP_ID.

        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Nm'
                                             PARENT  =  LO_RMTLCTNPSTLADR
                                             VALUE   = TEMP_ID ).
      ENDIF.

      LO_ADR  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'Adr'
                                            PARENT  = LO_RMTLCTNPSTLADR ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-CCODE.
      CONDENSE TEMP_ID.

      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                           PARENT  =  LO_ADR
                                           VALUE   = TEMP_ID ).


      ENDIF.




      LO_RMTINF  = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'RmtInf'
                                           PARENT  = LO_CDTTRFTXINF  ).


      CLEAR :TEMP_ID.

      IF WA_XML-PTYPE = 'RTGS'.

        CONCATENATE 'PPC' WA_XML-PURCD INTO TEMP_ID SEPARATED BY ' '.
        CONDENSE TEMP_ID.

        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ustrd'
                                    PARENT  =  LO_RMTINF
                                    VALUE   = TEMP_ID ).
      ENDIF.


*        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ustrd'
*                                              PARENT  =  LO_RMTINF
*                                              VALUE   = TEMP_ID ).
      IF WA_XML-REFER IS NOT INITIAL.

        CLEAR :TEMP_ID.
        TEMP_ID = WA_XML-REFER.
        CONDENSE TEMP_ID.
        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ustrd'
                                              PARENT  =  LO_RMTINF
                                             VALUE   = TEMP_ID ).
      ENDIF.

      LO_STRD = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'Strd'
                                           PARENT  = LO_RMTINF ).

      LO_RFRDDOCINF = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'RfrdDocInf'
                                           PARENT  = LO_STRD  ).

*        LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Tp'
*                                              PARENT  =   LO_RFRDDOCINF
*                                               ).
      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-BELNR.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Nb'
                                            PARENT  =   LO_RFRDDOCINF
                                            VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      CONCATENATE  WA_XML-BLDAT(4) '-' WA_XML-BLDAT+4(2)'-' WA_XML-BLDAT+6(2) INTO TEMP_ID .
*      TEMP_ID = WA_XML-BLDAT.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'RltdDt'
                                            PARENT  =  LO_RFRDDOCINF
                                            VALUE   = TEMP_ID ).

      LO_RFRDDOCAMT = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'RfrdDocAmt'
                                           PARENT  = LO_STRD  ).

      DATA : TEMP_ADD TYPE BSEG-PSWBT.
      CLEAR TEMP_ADD.
      CLEAR :TEMP_ID.

      TEMP_ADD = WA_XML-TRAMT + WA_XML-TDS.
      TEMP_ID = TEMP_ADD.
      CONDENSE TEMP_ID.

      LO_DUEPYBLAMT  = LO_DOC->CREATE_SIMPLE_ELEMENT_NS(
                                            NAME    = 'DuePyblAmt'
                                            PARENT  = LO_RFRDDOCAMT
                                            VALUE = TEMP_ID ).

      LO_DUEPYBLAMT->SET_ATTRIBUTE_NS(
                                     NAME = 'Ccy'
                                     VALUE = 'INR' ).



      LO_CDTRREFINF = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'CdtrRefInf'
                                           PARENT  = LO_STRD  ).

      LO_TP = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                             NAME    = 'Tp'
                                             PARENT  = LO_CDTRREFINF ).


      LO_CDORPRTRY = LO_DOC->CREATE_SIMPLE_ELEMENT(
                                             NAME    = 'CdOrPrtry'
                                             PARENT  = LO_TP ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-TDS.
      CONCATENATE '/TDSA/' TEMP_ID INTO TEMP_ID.
*      TEMP_ID = '/TDSA/1.00'.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Prtry'
                                            PARENT  =  LO_CDORPRTRY
                                            VALUE   = TEMP_ID ).


      CLEAR :TEMP_ID.
      TEMP_ID = WA_XML-TRAMT.
       CONCATENATE '/NAMT/' TEMP_ID INTO TEMP_ID.
*      TEMP_ID = '/NAMT/99.0'.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'Ref'
                                            PARENT  =  LO_CDTRREFINF
                                            VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      CONCATENATE '/NARR/ ' WA_XML-REFER INTO TEMP_ID.
      CONDENSE TEMP_ID.
      LO_DOC->CREATE_SIMPLE_ELEMENT( NAME    = 'AddtlRmtInf'
                                            PARENT  =  LO_STRD
                                            VALUE   = TEMP_ID ).



    ENDLOOP.


    CREATE OBJECT M_XMLDOC.
    M_XMLDOC->CREATE_WITH_DOM( DOCUMENT = LO_DOC ).

    CALL METHOD M_XMLDOC->SET_ENCODING
      EXPORTING
        CHARSET = 'UTF-8'.


*************************************************************************************************

    DATA : DIR TYPE STRING ,
           RES,
           RES1 TYPE I.

    DIR = 'D:\HSBC'.
    CONDENSE DIR.
    PERFORM FOLDER_CREATION USING DIR CHANGING RES RES1.

    IF RES = 'X'.

      CLEAR RES.
      DIR = 'D:\HSBC\XML FILE\'.
      CONDENSE DIR.
      PERFORM FOLDER_CREATION USING DIR CHANGING RES RES1.

    ENDIF.

    IF RES = 'X'.

      CLEAR RES.
      DIR = 'D:\HSBC\XML FILE\PAYMENT IN\'.
      CONDENSE DIR.
      PERFORM FOLDER_CREATION USING DIR CHANGING RES RES1.

    ENDIF.

    IF RES <> 'X'.

      DIR = 'D:\'.

    ENDIF.




************************************************************************************************
    CONDENSE DIR.
    CONCATENATE DIR FILE_NAME_XML '.xml' INTO FNAME.




    M_XMLDOC->EXPORT_TO_FILE( FNAME ).
    IF  SY-SUBRC = 0.

      DATA : IP1 TYPE STRING,
           CNAME1 TYPE STRING.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_COMPUTER_NAME
        CHANGING
          COMPUTER_NAME        = CNAME1
       EXCEPTIONS
         CNTL_ERROR           = 1
         ERROR_NO_GUI         = 2
         NOT_SUPPORTED_BY_GUI = 3
         OTHERS               = 4
              .
      IF SY-SUBRC <> 0.
*      Implement suitable error handling here
      ENDIF.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_IP_ADDRESS
        RECEIVING
          IP_ADDRESS           = IP1
       EXCEPTIONS
         CNTL_ERROR           = 1
         ERROR_NO_GUI         = 2
         NOT_SUPPORTED_BY_GUI = 3
         OTHERS               = 4
              .
      IF SY-SUBRC = 0.
*      Implement suitable error handling here

      CLEAR : TEMP_STATUS_XML.
      CONCATENATE 'Success' FNAME 'on' CNAME1 'IP' IP1 INTO TEMP_STATUS_XML SEPARATED BY ' '.
      CONDENSE TEMP_STATUS_XML.

        ENDIF.

    ENDIF.

    DATA : STREAM TYPE STRING,
           XSTREAM TYPE XSTRING,
           RETCODE TYPE SYSUBRC,
           SIZE TYPE SY-TABIX.


    clear : stream ,retcode, size.


    CALL METHOD M_XMLDOC->RENDER_2_STRING
      EXPORTING
        PRETTY_PRINT = 'X'
      IMPORTING
        RETCODE      = RETCODE
        STREAM       = STREAM
        SIZE         = SIZE.

    SFILE = STREAM.


    SHIFT STREAM BY 40 PLACES.

    CONDENSE STREAM.

    CONCATENATE '﻿<?xml version="1.0" encoding="utf-8"?>' STREAM INTO STREAM.

    condense stream.

*    IF stream(1) = '#'.
*
*     SHIFT STREAM BY 1 PLACES.
*
*    ENDIF.


*
*    CALL METHOD M_XMLDOC->RENDER_2_XSTRING
*      EXPORTING
*        PRETTY_PRINT = 'X'
*      IMPORTING
*        RETCODE      = RETCODE
*        STREAM       = XSTREAM
*        SIZE         = SIZE
*        .
*
*
*XFILE = Xstream.

*    PERFORM FTP_CONNECT.

    DATA: FILE TYPE STRING,
          LV_SOURCE TYPE SAPB-SAPPFAD,
          LV_TARGET_F TYPE SAPB-SAPPFAD,
          LV_TARGET_S TYPE SAPB-SAPPFAD.

    clear : file , LV_SOURCE, LV_TARGET_F, LV_TARGET_S.

    CONCATENATE WA_SFTP-FORUM_PATH FILE_NAME_XML '.xml' INTO FILE.
    CONDENSE FILE.

    LV_SOURCE = FILE.

    OPEN DATASET FILE FOR OUTPUT IN TEXT MODE ENCODING UTF-8.

*    if sy-subrc = 0.

    TRANSFER STREAM TO FILE.

    CLOSE DATASET FILE.
*
*    else.
*
*      MESSAGE 'ERROR : File has not created in Application server ' TYPE 'S'.
*      call screen '100'.
*
*
*    endif.

    CLEAR : STATUS_XML.

    DATA : SH TYPE STRING.
    DATA : ADD TYPE SXPGCOLIST-PARAMETERS.

    CLEAR SH.
    CONCATENATE FILE_NAME_XML '.xml' INTO SH.

    CONDENSE SH.

    ADD = SH.

    data : status TYPE EXTCMDEXEX-STATUS,
           EXITCODE  TYPE EXTCMDEXEX-EXITCODE.

    clear : status, EXITCODE.




    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        COMMANDNAME                         = 'ZHSBC_SFTP'
        ADDITIONAL_PARAMETERS               = ADD
*       OPERATINGSYSTEM                     = SY-OPSYS
*       TARGETSYSTEM                        = SY-HOST
*       DESTINATION                         =
*       STDOUT                              = 'X'
*       STDERR                              = 'X'
*       TERMINATIONWAIT                     = 'X'
*       TRACE                               =
*       DIALOG                              =
     IMPORTING
       STATUS                              = STATUS
       EXITCODE                            = EXITCODE
*     TABLES
*       EXEC_PROTOCOL                       =
     EXCEPTIONS
       NO_PERMISSION                       = 1
       COMMAND_NOT_FOUND                   = 2
       PARAMETERS_TOO_LONG                 = 3
       SECURITY_RISK                       = 4
       WRONG_CHECK_CALL_INTERFACE          = 5
       PROGRAM_START_ERROR                 = 6
       PROGRAM_TERMINATION_ERROR           = 7
       X_ERROR                             = 8
       PARAMETER_EXPECTED                  = 9
       TOO_MANY_PARAMETERS                 = 10
       ILLEGAL_COMMAND                     = 11
       WRONG_ASYNCHRONOUS_PARAMETERS       = 12
       CANT_ENQ_TBTCO_ENTRY                = 13
       JOBCOUNT_GENERATION_ERROR           = 14
       OTHERS                              = 15
              .

    IF sy-subrc = 1.

      MESSAGE 'No Authorization to Access OS Command.....' TYPE 'E'.

    ENDIF.




    IF status = 'E'.

      DATA : TEMP_STRING TYPE STRING.

      clear temp_string.

      CONCATENATE WA_SFTP-FAILED_PATH FILE_NAME_XML INTO  TEMP_STRING .
      CONDENSE TEMP_STRING.

      LV_TARGET_F = TEMP_STRING.

      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
        EXPORTING
          SOURCEPATH             = LV_SOURCE
          TARGETPATH             = LV_TARGET_F
* IMPORTING
*   LENGTH                 =
 EXCEPTIONS
   ERROR_FILE             = 1
   NO_AUTHORIZATION       = 2
   OTHERS                 = 3
                .
      IF SY-SUBRC = 0.

        DELETE DATASET LV_SOURCE.

      ENDIF.

     MESSAGE 'Failed to Upload file........' TYPE 'S'.
     call SCREEN '100'.

    ELSE.

      DATA : TEMP_STRING1 TYPE STRING.

      clear temp_string1.

      CONCATENATE WA_SFTP-SUCCESS_PATH FILE_NAME_XML INTO  TEMP_STRING1 .
      CONDENSE TEMP_STRING1.

      LV_TARGET_S = TEMP_STRING1.

      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
        EXPORTING
          SOURCEPATH             = LV_SOURCE
          TARGETPATH             = LV_TARGET_S
* IMPORTING
*   LENGTH                 =
 EXCEPTIONS
   ERROR_FILE             = 1
   NO_AUTHORIZATION       = 2
   OTHERS                 = 3
                .

      IF SY-SUBRC = 0.

        DELETE DATASET LV_SOURCE.

      ENDIF.

      CLEAR TEMPS.
      TEMPS = TEMP_STATUS_XML.
      CLEAR TEMP_STATUS_XML.
      CONCATENATE TEMPS 'Success:SAP Server on Path:' FILE 'Success:' BATCH ' Sent to HSBC' INTO TEMP_STATUS_XML SEPARATED BY ' ' .

      CONDENSE TEMP_STATUS_XML.

      PERFORM TABLE_UPDATE_XML.

    ENDIF.


  ENDIF.

  IF IT_IMPS IS NOT INITIAL.


    CLEAR : TEMP, TEMP1.
    TEMP = IMPS_NO.
    TEMP1 = IMPS_TOTAL.
    CONDENSE : TEMP, TEMP1.

    CLEAR INITIAL.

    LOOP AT IT_IMPS INTO WA_IMPS.

      IF INITIAL IS INITIAL.

        LO_IXML1 = CL_IXML=>CREATE( ).

        LO_DOC1  = LO_IXML1->CREATE_DOCUMENT( ).


        LO_DOCUMENT1  = LO_DOC1->CREATE_SIMPLE_ELEMENT_NS(
                                            NAME    = 'Document'
                                            PARENT  = LO_DOC1 ).

        LO_DOCUMENT1->SET_ATTRIBUTE_NS(
                                       NAME = 'xmlns'
                                       VALUE = 'urn:iso:std:iso:20022:tech:xsd:pain.001.001.03' ).

        LO_DOCUMENT1->SET_ATTRIBUTE_NS( PREFIX = 'xmlns'
                                    NAME = 'xsi'
                                    VALUE = 'http://www.w3.org/2001/XMLSchema-instance' ).

        LO_CSTMRCDTTRFINITN1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'CstmrCdtTrfInitn'
                                            PARENT  = LO_DOCUMENT1 ).




        LO_GRPHDR1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                             NAME    = 'GrpHdr'
                                             PARENT  = LO_CSTMRCDTTRFINITN1 ).
        CLEAR TEMP_ID.
        CONDENSE WA_IMPS-XMLNA.
        TEMP_ID = WA_IMPS-XMLNA.
        CONDENSE : TEMP_ID.
        LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'MsgId'
                                            PARENT  = LO_GRPHDR1
                                            VALUE   = TEMP_ID  ).

        CLEAR TEMP_ID.

        CONCATENATE SY-DATUM(4)'-' SY-DATUM+4(2)'-' SY-DATUM+6(2) 'T' SY-UZEIT(2) ':' SY-UZEIT+2(2) ':' SY-UZEIT+4(2) INTO TEMP_ID.
        CONDENSE TEMP_ID.
        LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'CreDtTm'
                                            PARENT  = LO_GRPHDR1
                                            VALUE   = TEMP_ID ).

        LO_AUTHSTN1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'Authstn'
                                            PARENT  = LO_GRPHDR1 ).

        CLEAR :TEMP_ID.
        TEMP_ID = 'FDET'.
        CONDENSE TEMP_ID.
        LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Cd'
                                            PARENT  = LO_AUTHSTN1
                                            VALUE   = TEMP_ID ).
        CLEAR :TEMP_ID.
        TEMP_ID = IMPS_NO.
        CONDENSE TEMP_ID.

        LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'NbOfTxs'
                                            PARENT  = LO_GRPHDR1
                                            VALUE   = TEMP_ID ).
        CLEAR :TEMP_ID.
        TEMP_ID = IMPS_TOTAL.
        CONDENSE TEMP_ID.
        LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'CtrlSum'
                                            PARENT  = LO_GRPHDR1
                                            VALUE   = TEMP_ID  ).

        LO_INITGPTY1 = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'InitgPty'
                                            PARENT  = LO_GRPHDR1 ).

        LO_ID1 = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'Id'
                                            PARENT  = LO_INITGPTY1 ).

        LO_ORGID1 = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'OrgId'
                                            PARENT  = LO_ID1 ).

        LO_OTHR1 = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                            NAME    = 'Othr'
                                            PARENT  = LO_ORGID1 ).
        CLEAR :TEMP_ID.

        TEMP_ID = WA_IMPS-BNKID.
        CONDENSE TEMP_ID.
        CONDENSE TEMP_ID.
        LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Id'
                                            PARENT  = LO_OTHR1
                                            VALUE   = TEMP_ID ).

         LO_PMTINF1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'PmtInf'
                                          PARENT  = LO_CSTMRCDTTRFINITN1 ).


        clear temp_id.
        temp_id = BATCH.
        CONDENSE temp_id.

        LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'PmtInfId'
                                          PARENT  = LO_PMTINF1
                                          VALUE   = temp_id ).

              CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-PAYID.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'PmtMtd'
                                          PARENT  = LO_PMTINF1
                                          VALUE   = TEMP_ID ).

      LO_PMTTPINF1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                        NAME    = 'PmtTpInf'
                                        PARENT  = LO_PMTINF1 ).

      LO_SVCLVL1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                         NAME    = 'SvcLvl'
                                         PARENT  = LO_PMTTPINF1  ).


      CLEAR :TEMP_ID.
      TEMP_ID = 'IMPS'.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Cd'
                                        PARENT  = LO_SVCLVL1
                                        VALUE   = TEMP_ID ).


      CLEAR :TEMP_ID.
      CONCATENATE  WA_IMPS-TRDAT(4) '-' WA_IMPS-TRDAT+4(2)'-' WA_IMPS-TRDAT+6(2) INTO TEMP_ID .
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'ReqdExctnDt'
                                          PARENT  = LO_PMTINF1
                                          VALUE   = TEMP_ID ).

      LO_DBTR1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'Dbtr'
                                       PARENT  = LO_PMTINF1 ).
      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CMDET.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Nm'
                                          PARENT  = LO_DBTR1
                                          VALUE   = TEMP_ID ).

      LO_PSTLADR1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'PstlAdr'
                                          PARENT  = LO_DBTR1  ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CADR2.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'StrtNm'
                                          PARENT  = LO_PSTLADR1
                                          VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CADR1.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'BldgNb'
                                          PARENT  = LO_PSTLADR1
                                          VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CPOST.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'PstCd'
                                          PARENT  = LO_PSTLADR1
                                          VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CADR3.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'TwnNm'
                                          PARENT  = LO_PSTLADR1
                                          VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CCODE.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                          PARENT  = LO_PSTLADR1
                                          VALUE   = TEMP_ID ).


      LO_DBTRACCT1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                     NAME    = 'DbtrAcct'
                                     PARENT  = LO_PMTINF1 ).


      LO_ID1 = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                        NAME    = 'Id'
                                        PARENT  = LO_DBTRACCT1 ).

      LO_OTHR1 = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'Othr'
                                          PARENT  = LO_ID1 ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CACNO.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Id'
                                          PARENT  = LO_OTHR1
                                          VALUE   = TEMP_ID ).


      LO_DBTRAGT1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'DbtrAgt'
                                          PARENT  = LO_PMTINF1 ).

      LO_FININSTNID1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'FinInstnId'
                                           PARENT  = LO_DBTRAGT1 ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CIFSC.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'BIC'
                                          PARENT  = LO_FININSTNID1
                                          VALUE   = TEMP_ID ).



      LO_PSTLADR1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'PstlAdr'
                                           PARENT  = LO_FININSTNID1  ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CCODE.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                          PARENT  = LO_PSTLADR1
                                          VALUE   = TEMP_ID ).


      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-TRCHA.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'ChrgBr'
                                          PARENT  = LO_PMTINF1
                                          VALUE   = TEMP_ID ).



        INITIAL = 'X'.

      ENDIF.

************************************************               Payment Info              ******************************************************************






      CLEAR :INVOICE_NO.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WA_IMPS-BUZEI
*        IMPORTING
*          OUTPUT = WA_IMPS-BUZEI.

*      CONCATENATE WA_IMPS-BELNR WA_IMPS-BUZEI INTO INVOICE_NO.

      INVOICE_NO = WA_IMPS-BELNR.
      CONDENSE INVOICE_NO.




      LO_CDTTRFTXINF1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                        NAME    = 'CdtTrfTxInf'
                                        PARENT  = LO_PMTINF1 ).

      LO_PMTID1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                         NAME    = 'PmtId'
                                         PARENT  = LO_CDTTRFTXINF1 ).

*      CLEAR :TEMP_ID.
*      TEMP_ID = WA_xml-CMREF.

      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'InstrId'
                                          PARENT  = LO_PMTID1
                                          VALUE   = INVOICE_NO ).

*      CLEAR :TEMP_ID.
*      TEMP_ID = WA_xml-PYREF.

      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'EndToEndId'
                                          PARENT  = LO_PMTID1
                                          VALUE   = INVOICE_NO ).

      LO_AMT1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                         NAME    = 'Amt'
                                         PARENT  = LO_CDTTRFTXINF1 ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-TRAMT.
      CONDENSE TEMP_ID.


      LO_INSTDAMT1  = LO_DOC1->CREATE_SIMPLE_ELEMENT_NS(
                                            NAME    = 'InstdAmt'
                                            PARENT  = LO_AMT1
                                            VALUE = TEMP_ID ).

      LO_INSTDAMT1->SET_ATTRIBUTE_NS(
                                     NAME = 'Ccy'
                                     VALUE = 'INR' ).

      LO_CDTRAGT1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'CdtrAgt'
                                       PARENT  = LO_CDTTRFTXINF1 ).

      LO_FININSTNID1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'FinInstnId'
                                       PARENT  = LO_CDTRAGT1 ).

*
*      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'BIC'
*                                           PARENT  = LO_FININSTNID1
*                                     ).


      LO_CLRSYSMMBID1 = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'ClrSysMmbId'
                                       PARENT  = LO_FININSTNID1 ).



      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-VIFSC.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'MmbId'
                                            PARENT  = LO_CLRSYSMMBID1
                                            VALUE   = TEMP_ID ).

      LO_PSTLADR1 = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                       NAME    = 'PstlAdr'
                                       PARENT  = LO_FININSTNID1 ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CCODE.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                            PARENT  = LO_PSTLADR1
                                            VALUE   = TEMP_ID ).

      LO_CDTR1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'Cdtr'
                                          PARENT  = LO_CDTTRFTXINF1 ).


      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-VNAME.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Nm'
                                            PARENT  = LO_CDTR1
                                            VALUE   = TEMP_ID ).

      LO_PSTLADR1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'PstlAdr'
                                           PARENT  = LO_CDTR1 ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-CCODE.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Ctry'
                                            PARENT  = LO_PSTLADR1
                                            VALUE   = TEMP_ID ).

      LO_CDTRACCT1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'CdtrAcct'
                                           PARENT  = LO_CDTTRFTXINF1 ).

      LO_ID1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                           NAME    = 'Id'
                                           PARENT  = LO_CDTRACCT1 ).

      LO_OTHR1  = LO_DOC1->CREATE_SIMPLE_ELEMENT(
                                          NAME    = 'Othr'
                                          PARENT  = LO_ID1 ).

      CLEAR :TEMP_ID.
      TEMP_ID = WA_IMPS-VACNO.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Id'
                                            PARENT  =  LO_OTHR1
                                            VALUE   = TEMP_ID ).

      CLEAR :TEMP_ID.
      TEMP_ID = 'INR'.
      CONDENSE TEMP_ID.
      LO_DOC1->CREATE_SIMPLE_ELEMENT( NAME    = 'Ccy'
                                            PARENT  =  LO_CDTRACCT1
                                            VALUE   = TEMP_ID ).



    ENDLOOP.


    CREATE OBJECT M_XMLDOC1.
    M_XMLDOC1->CREATE_WITH_DOM( DOCUMENT = LO_DOC1 ).

    CALL METHOD M_XMLDOC1->SET_ENCODING
      EXPORTING
        CHARSET = 'UTF-8'.

*************************************************************************************************

    DATA : DIRI TYPE STRING ,
           RESI,
           RES1I TYPE I.

    DIRI = 'D:\HSBC'.
    CONDENSE DIRI.
    PERFORM FOLDER_CREATION USING DIRI CHANGING RESI RES1I.

    IF RESI = 'X'.

      CLEAR RES.
      DIRI = 'D:\HSBC\XML FILE\'.
      CONDENSE DIRI.
      PERFORM FOLDER_CREATION USING DIRI CHANGING RESI RES1I.

    ENDIF.

    IF RESI = 'X'.

      CLEAR RESI.
      DIRI = 'D:\HSBC\XML FILE\PAYMENT IN\'.
      CONDENSE DIRI.
      PERFORM FOLDER_CREATION USING DIRI CHANGING RESI RES1I.

    ENDIF.

    IF RESI <> 'X'.

      DIRI = 'D:\'.

    ENDIF.




************************************************************************************************
    CONDENSE DIRI.
    CLEAR FNAME.
    CONCATENATE DIRI FILE_NAME_IMPS '.xml' INTO FNAME.

    M_XMLDOC1->EXPORT_TO_FILE( FNAME ).

    IF  SY-SUBRC = 0.

      DATA : IP TYPE STRING,
             CNAME TYPE STRING.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_COMPUTER_NAME
        CHANGING
          COMPUTER_NAME        = CNAME
*       EXCEPTIONS
*         CNTL_ERROR           = 1
*         ERROR_NO_GUI         = 2
*         NOT_SUPPORTED_BY_GUI = 3
*         OTHERS               = 4
              .
      IF SY-SUBRC <> 0.
*      Implement suitable error handling here
      ENDIF.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_IP_ADDRESS
        RECEIVING
          IP_ADDRESS           = IP
*       EXCEPTIONS
*         CNTL_ERROR           = 1
*         ERROR_NO_GUI         = 2
*         NOT_SUPPORTED_BY_GUI = 3
*         OTHERS               = 4
              .
      IF SY-SUBRC <> 0.
*      Implement suitable error handling here
      ENDIF.
      CLEAR : TEMP_STATUS_IMPS.
      CONCATENATE 'Success' FNAME 'on' CNAME 'IP' IP INTO TEMP_STATUS_IMPS SEPARATED BY ' '.
      CONDENSE TEMP_STATUS_IMPS.
    ENDIF.

    DATA : STREAM1 TYPE STRING,
           RETCODE1 TYPE SYSUBRC,
           SIZE1 TYPE SY-TABIX.

    CALL METHOD M_XMLDOC1->RENDER_2_STRING
      EXPORTING
        PRETTY_PRINT = 'X'
      IMPORTING
        RETCODE      = RETCODE1
        STREAM       = STREAM1
        SIZE         = SIZE1.



    SHIFT STREAM1 BY 40 PLACES.

    CONDENSE STREAM1.

    CONCATENATE '﻿<?xml version="1.0" encoding="utf-8"?>' STREAM1 INTO STREAM1.


*
*    CALL METHOD M_XMLDOC->RENDER_2_XSTRING
*      EXPORTING
*        PRETTY_PRINT = 'X'
*      IMPORTING
*        RETCODE      = RETCODE
*        STREAM       = XSTREAM
*        SIZE         = SIZE
*        .
*
*
*XFILE = Xstream.

*    PERFORM FTP_CONNECT.

    DATA: FILE1 TYPE STRING,
          LV_SOURCE1 TYPE SAPB-SAPPFAD,
          LV_TARGET_F1 TYPE SAPB-SAPPFAD,
          LV_TARGET_S1 TYPE SAPB-SAPPFAD.

    CONCATENATE WA_SFTP-FORUM_PATH FILE_NAME_IMPS '.xml' INTO FILE1.
    CONDENSE FILE1.

    LV_SOURCE1 = FILE1.

    OPEN DATASET FILE1 FOR OUTPUT IN TEXT MODE ENCODING UTF-8.

    TRANSFER STREAM1 TO FILE1.

    CLOSE DATASET FILE1.

    CLEAR : STATUS_IMPS.

    DATA : SH1 TYPE STRING.
    DATA : ADD1 TYPE SXPGCOLIST-PARAMETERS.

    CLEAR SH1.
    CONCATENATE FILE_NAME_IMPS '.xml' INTO SH1.

    CONDENSE SH1.

    ADD1 = SH1.

    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        COMMANDNAME                         = 'ZHSBC_SFTP'
       ADDITIONAL_PARAMETERS               =  ADD1
*       OPERATINGSYSTEM                     = SY-OPSYS
*       TARGETSYSTEM                        = SY-HOST
*       DESTINATION                         =
*       STDOUT                              = 'X'
*       STDERR                              = 'X'
*       TERMINATIONWAIT                     = 'X'
*       TRACE                               =
*       DIALOG                              =
*     IMPORTING
*       STATUS                              =
*       EXITCODE                            =
*     TABLES
*       EXEC_PROTOCOL                       =
*     EXCEPTIONS
*       NO_PERMISSION                       = 1
*       COMMAND_NOT_FOUND                   = 2
*       PARAMETERS_TOO_LONG                 = 3
*       SECURITY_RISK                       = 4
*       WRONG_CHECK_CALL_INTERFACE          = 5
*       PROGRAM_START_ERROR                 = 6
*       PROGRAM_TERMINATION_ERROR           = 7
*       X_ERROR                             = 8
*       PARAMETER_EXPECTED                  = 9
*       TOO_MANY_PARAMETERS                 = 10
*       ILLEGAL_COMMAND                     = 11
*       WRONG_ASYNCHRONOUS_PARAMETERS       = 12
*       CANT_ENQ_TBTCO_ENTRY                = 13
*       JOBCOUNT_GENERATION_ERROR           = 14
*       OTHERS                              = 15
              .
    IF SY-SUBRC <> 0.

      DATA : TEMP_STRING11 TYPE STRING.

      CONCATENATE WA_SFTP-FAILED_PATH FILE_NAME_IMPS INTO  TEMP_STRING11 .
      CONDENSE TEMP_STRING1.

      LV_TARGET_F1 = TEMP_STRING11.

      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
        EXPORTING
          SOURCEPATH             = LV_SOURCE1
          TARGETPATH             = LV_TARGET_F1
* IMPORTING
*   LENGTH                 =
* EXCEPTIONS
*   ERROR_FILE             = 1
*   NO_AUTHORIZATION       = 2
*   OTHERS                 = 3
                .
      IF SY-SUBRC = 0.

        DELETE DATASET LV_SOURCE1.

      ENDIF.

      STATUS_IMPS = 'X'.

    ELSE.

      DATA : TEMP_STRING12 TYPE STRING.

      CONCATENATE WA_SFTP-SUCCESS_PATH FILE_NAME_IMPS INTO  TEMP_STRING12 .
      CONDENSE TEMP_STRING11.

      LV_TARGET_S1 = TEMP_STRING12.

      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
        EXPORTING
          SOURCEPATH             = LV_SOURCE1
          TARGETPATH             = LV_TARGET_S1
* IMPORTING
*   LENGTH                 =
* EXCEPTIONS
*   ERROR_FILE             = 1
*   NO_AUTHORIZATION       = 2
*   OTHERS                 = 3
                .

      IF SY-SUBRC = 0.

        DELETE DATASET LV_SOURCE1.

      ENDIF.

      CLEAR TEMPS.
      TEMPS = TEMP_STATUS_IMPS.
      CLEAR TEMP_STATUS_IMPS.

      CONCATENATE TEMPS 'Success:SAP Server on Path:' FILE1 'Success:' BATCH ' Sent to HSBC' INTO TEMP_STATUS_IMPS SEPARATED BY ' ' .

      CONDENSE TEMP_STATUS_IMPS.
      PERFORM TABLE_UPDATE_IMPS.
    ENDIF.


  ENDIF.

  CLEAR : TEMP.

*  CONCATENATE BATCH 'has transferred to HSBC Successfully' INTO TEMP SEPARATED BY ' '.
*
*  IF TEMP_STATUS_IMPS IS NOT INITIAL.
*    CLEAR TEMPS.
*    TEMPS = TEMP_STATUS_IMPS.
*    CLEAR TEMP_STATUS_IMPS.
*    CONCATENATE TEMPS '-------->' TEMP INTO TEMP_STATUS_IMPS SEPARATED BY ' ' .
*    PERFORM TABLE_UPDATE_IMPS.
*  ENDIF.
*
*  IF TEMP_STATUS_XML IS NOT INITIAL.
*    CLEAR TEMPS.
*    TEMPS = TEMP_STATUS_XML.
*    CLEAR TEMP_STATUS_XML.
*    CONCATENATE TEMPS '-------->' TEMP INTO TEMP_STATUS_XML SEPARATED BY ' ' .
*    PERFORM TABLE_UPDATE_XML.
*
*  ENDIF.

  DATA : STATUS_MESSAGE TYPE STRING.



  IF STATUS_XML IS INITIAL AND STATUS_IMPS IS INITIAL.

    CONCATENATE BATCH 'has sent to HSBC Sucessfully' INTO STATUS_MESSAGE SEPARATED BY ' '.

    MESSAGE STATUS_MESSAGE TYPE 'S'.
    CALL SCREEN '100'.

  ELSEIF STATUS_XML IS NOT INITIAL AND STATUS_IMPS IS INITIAL.

    CONCATENATE BATCH 'NEFT/RTGS Payment Has failed but IMPS payment has sent sucessfully' INTO STATUS_MESSAGE SEPARATED BY ' '.

    MESSAGE STATUS_MESSAGE TYPE 'S'.
    CALL SCREEN '100'.

  ELSEIF STATUS_XML IS INITIAL AND STATUS_IMPS IS NOT INITIAL.

    CONCATENATE BATCH 'IMPS Payment Has failed but NEFT/RTGS payment has sent sucessfully' INTO STATUS_MESSAGE SEPARATED BY ' '.

    MESSAGE STATUS_MESSAGE TYPE 'S'.
    CALL SCREEN '100'.

  ELSEIF STATUS_XML IS NOT INITIAL AND STATUS_IMPS IS NOT INITIAL.

    CONCATENATE BATCH 'Failed' INTO STATUS_MESSAGE SEPARATED BY ' '.

    MESSAGE STATUS_MESSAGE TYPE 'E'.
    CALL SCREEN '100'.

  ENDIF.







ENDFORM.                    " XML_FILE_GENERATION
