*&---------------------------------------------------------------------*
*& Report  ZHSBC_PAYMENT_RE_EXTRACTION
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZHSBC_PAYMENT_PULL.

DATA : WA_HEAD TYPE ZHSBC_H_RETURN,
       IT_HEAD TYPE TABLE OF ZHSBC_H_RETURN,

       IT_ITEMS TYPE TABLE OF ZHSBC_LI_RETURN,
       WA_ITEMS TYPE ZHSBC_LI_RETURN,

       WA_XML TYPE ZHSBC_ITEMS,
       WA_HXML TYPE ZHSBC_HEAD,

       it_xml TYPE TABLE OF zhsbc_items.



data : lv_port1 TYPE string,
       lv_port2 TYPE string,
       lv_port3 TYPE string,
       lv_port4 TYPE string,
       lv_port5 TYPE string.



DATA : WA_SFTP TYPE ZHSBC_SFTP,
       LV_DIR TYPE EPSF-EPSDIRNAM,
       IT_FILE_LIST TYPE TABLE OF EPSFILI,
       WA_FILE_LIST TYPE EPSFILI,
       FILE_COUNT TYPE I,
       TEMP TYPE STRING,
*************************
       GV_CSV_ERROR           TYPE C ,
       GV_FNAME_TEMP(255)     TYPE C,
       GV_PATH(255)           TYPE C,
       STR1(255)              TYPE C,
       STR2(255)              TYPE C,
       X.
************************

DATA : RETCODE TYPE SY-SUBRC,
        FILENAME TYPE STRING,
        SIZE TYPE SY-TABIX,
        FILE TYPE LOCALFILE.

TYPES: BEGIN OF TY_FINAL,

  FILE_NAME TYPE EPSFILI-NAME,
  CONTENT TYPE STRING,
  XFILE  TYPE XSTRING,

END OF TY_FINAL.

DATA : IT_FINAL TYPE TABLE OF TY_FINAL,
       WA_FINAL TYPE TY_FINAL,
       IT_FILE TYPE TABLE OF SMUM_XMLTB,
       WA_FILE TYPE SMUM_XMLTB,
       RET TYPE TABLE OF BAPIRET2,
       TEMP1 TYPE STRING.

DATA :  LO_DOC TYPE REF TO CL_XML_DOCUMENT.

SELECT SINGLE * FROM ZHSBC_SFTP INTO WA_SFTP.

IF WA_SFTP-LOG_FPATH IS INITIAL OR WA_SFTP-LOG_SPATH IS INITIAL OR WA_SFTP-LOG_FPATH IS INITIAL.

  MESSAGE 'Please Maintain Server Connectivity in ZHSBC' TYPE 'E'.

ENDIF.

data : activity TYPE char30,
       FP TYPE AUTHB-FILENAME.

cleAR : FP, activity.

activity = 'WRITE'.
FP = WA_SFTP-LOG_FPATH.

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

MESSAGE 'No authorization to access Application server file' TYPE 'E'.

ENDIF.

CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
  EXPORTING
    COMMANDNAME                         = 'ZHSBC_GET_SFTP'
*   ADDITIONAL_PARAMETERS               =
*   OPERATINGSYSTEM                     = SY-OPSYS
*   TARGETSYSTEM                        = SY-HOST
*   DESTINATION                         =
*   STDOUT                              = 'X'
*   STDERR                              = 'X'
*   TERMINATIONWAIT                     = 'X'
*   TRACE                               =
*   DIALOG                              =
* IMPORTING
*   STATUS                              =
*   EXITCODE                            =
* TABLES
*   EXEC_PROTOCOL                       =
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
IF SY-SUBRC <> 0.

  IF  sy-subrc = 1.

    WRITE : 'No Authorization to Access OS command'.

    exit.


  else.

     WRITE : 'Connectivity to HSBC server failed [OS Command]'.
     exit.

  ENDIF.



ENDIF.

CLEAR LV_DIR.

LV_DIR = WA_SFTP-LOG_PATH.

REFRESH : IT_FILE_LIST.

CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
  EXPORTING
    DIR_NAME                     = LV_DIR
*   FILE_MASK                    = ' '
* IMPORTING
*   DIR_NAME                     =
*   FILE_COUNTER                 =
*   ERROR_COUNTER                =
  TABLES
    DIR_LIST                     = IT_FILE_LIST
 EXCEPTIONS
   INVALID_EPS_SUBDIR           = 1
   SAPGPARAM_FAILED             = 2
   BUILD_DIRECTORY_FAILED       = 3
   NO_AUTHORIZATION             = 4
   READ_DIRECTORY_FAILED        = 5
   TOO_MANY_READ_ERRORS         = 6
   EMPTY_DIRECTORY_LIST         = 7
   OTHERS                       = 8
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.



IF IT_FILE_LIST IS NOT INITIAL.

  CLEAR : FILE_COUNT.

  FILE_COUNT = LINES( IT_FILE_LIST ).

  WRITE : / FILE_COUNT , ' Number of Files Retrived From HSBC'.

  WRITE : / 'Files :'.

  SORT IT_FILE_LIST BY SIZE.

  LOOP AT IT_FILE_LIST INTO WA_FILE_LIST.
********************
     DATA: SHARED_PATH            TYPE RLGRAP-FILENAME,
            SHARED_FILE            TYPE STRING.
      CLEAR : SHARED_FILE.
      CONCATENATE LV_DIR WA_FILE_LIST-NAME INTO SHARED_FILE.
      GV_FNAME_TEMP = SHARED_FILE.
      PERFORM CHECK_FILE_EXT CHANGING GV_FNAME_TEMP
                                      GV_CSV_ERROR.
      IF GV_CSV_ERROR IS NOT INITIAL.
************************
*    SKIP 1.
    WRITE :/7 WA_FILE_LIST-NAME , '  Size :' ,WA_FILE_LIST-SIZE.


    CLEAR WA_FINAL.
    WA_FINAL-FILE_NAME = WA_FILE_LIST-NAME.
    CLEAR TEMP.
    CONCATENATE WA_SFTP-LOG_PATH WA_FILE_LIST-NAME INTO TEMP.
    CONDENSE TEMP.

    OPEN DATASET TEMP FOR INPUT IN TEXT MODE ENCODING DEFAULT.

    DO.

      READ DATASET TEMP INTO TEMP1.

      IF SY-SUBRC = 0.


        CONCATENATE WA_FINAL-CONTENT TEMP1 INTO WA_FINAL-CONTENT.

        CLEAR TEMP1.

      ELSE.

        EXIT.

      ENDIF.

    ENDDO.



    CLOSE DATASET TEMP.

    IF LO_DOC IS BOUND.

      CALL METHOD LO_DOC->FREE.

    ENDIF.
    CREATE OBJECT LO_DOC.

    CALL METHOD LO_DOC->PARSE_STRING
      EXPORTING
        STREAM  = WA_FINAL-CONTENT
      RECEIVING
        RETCODE = RETCODE.

    CLEAR FILENAME.

        DATA : DIR TYPE STRING ,
           RES,
           RES1 TYPE I.

    clear : dir, res, res1.

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
      DIR = 'D:\HSBC\XML FILE\HSBC RETURN\'.
      CONDENSE DIR.
      PERFORM FOLDER_CREATION USING DIR CHANGING RES RES1.

    ENDIF.

    IF RES <> 'X'.

      DIR = 'D:\'.

    ENDIF.

    CONCATENATE Dir WA_FINAL-FILE_NAME '.xml' INTO FILENAME.
    CONDENSE FILENAME.

    FILE = FILENAME.

    CALL METHOD LO_DOC->EXPORT_TO_FILE
      EXPORTING
        FILENAME = FILE
      RECEIVING
        RETCODE  = RETCODE.

    CALL METHOD LO_DOC->RENDER_2_XSTRING
      EXPORTING
        PRETTY_PRINT = 'X'
      IMPORTING
        RETCODE      = RETCODE
        STREAM       = WA_FINAL-XFILE
        SIZE         = SIZE.

    REFRESH IT_FILE.

    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        XML_INPUT = WA_FINAL-XFILE
      TABLES
        XML_TABLE = IT_FILE
        RETURN    = RET.


    DATA : LV_MID    TYPE CHAR15,
           LV_DAT    TYPE CHAR20,
           LV_BIC    TYPE CHAR15,
           LV_FILE   TYPE ZHSBC_ITEMS-XMLNA,
           LV_TOT    TYPE I,
           LV_TOTAMT TYPE BSEG-PSWBT,
           LV_BELNR TYPE BSEG-BELNR,
           LV_SISID TYPE CHAR20,
           LV_RFID  TYPE CHAR15,
           LV_STATUS TYPE CHAR5,
           LV_REA_CODE TYPE CHAR5,
           LV_REASON TYPE CHAR50,
           LV_AMT    TYPE BSEG-PSWBT,
           LV_CKY TYPE CHAR3,
           LV_EXTDATE TYPE CHAR10,
           LV_VIFSC TYPE BANKK,
           LV_VNAME TYPE KOINH_FI,
           I TYPE I,
           LV_ID TYPE CHAR15,
           lv_reason_code TYPE char5,
           lv_rea TYPE char50.


    CLEAR :                   LV_MID   ,
                              LV_DAT   ,
                              LV_BIC   ,
                              LV_FILE  ,
                              LV_TOT   ,
                              LV_TOTAMT,
                              lv_port1,
                              lv_port2,
                              lv_port3,
                              lv_port4,
                              lv_port5.

    SPLIT WA_FILE_LIST-NAME AT '.' into lv_port1 lv_port2 . "lv_port3 lv_port4 lv_port5.

    condense lv_port1.

    IF lv_port1(4) = 'ACK2'.

      LOOP AT IT_FILE INTO WA_FILE WHERE TYPE = 'V'.


        CASE WA_FILE-CNAME.
          WHEN 'MsgId'.
            LV_MID = WA_FILE-CVALUE.
          WHEN 'CreDtTm'.
            LV_DAT = WA_FILE-CVALUE.
          WHEN 'BICOrBEI'.
            LV_BIC = WA_FILE-CVALUE.
          WHEN 'OrgnlMsgId'.
            LV_FILE = WA_FILE-CVALUE.
          WHEN 'OrgnlNbOfTxs'.
            LV_TOT = WA_FILE-CVALUE.
          WHEN 'OrgnlCtrlSum'.
            LV_TOTAMT = WA_FILE-CVALUE.    "#EC CI_FLDEXT_OK[2610650]

            CLEAR WA_HEAD.

*            WA_HEAD-MID = LV_MID.
*            WA_HEAD-DATSTAMP = LV_DAT.
*            WA_HEAD-BIC = LV_BIC.
*            WA_HEAD-XMLNA = LV_FILE.
*            WA_HEAD-TRNUM = LV_TOT.
*            WA_HEAD-TOAMT = LV_TOTAMT.
*            WA_HEAD-PSWSL = 'INR'.
*            WA_HEAD-MANDT = SY-MANDT.
*
*            MODIFY ZHSBC_H_RETURN from WA_HEAD.
*            CLEAR WA_HEAD.
*
*            CLEAR WA_HXML.

            SELECT * UP TO 1 ROWS FROM ZHSBC_HEAD INTO WA_HXML WHERE XMLNA = LV_FILE ORDER BY PRIMARY KEY.
            ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

            IF SY-SUBRC = 0.
              WA_HXML-MID = LV_MID.
              WA_HXML-DATSTAMP =  LV_DAT.
              WA_HXML-BIC = LV_BIC.

              UPDATE ZHSBC_HEAD FROM WA_HXML.
              CLEAR WA_HXML.
            ENDIF.
          WHEN 'OrgnlPmtInfId'.

            LV_BELNR = WA_FILE-CVALUE.

          WHEN 'StsId'.

            LV_SISID = WA_FILE-CVALUE.

          WHEN 'OrgnlInstrId'.

            LV_RFID = WA_FILE-CVALUE.

          WHEN 'TxSts'.

            LV_STATUS = WA_FILE-CVALUE.

          WHEN 'Cd'.

            LV_REA_CODE = WA_FILE-CVALUE.

          WHEN 'AddtlInf'.

            LV_REASON = WA_FILE-CVALUE.

          WHEN 'InstdAmt'.

            LV_AMT = WA_FILE-CVALUE.    "#EC CI_FLDEXT_OK[2610650]

          WHEN 'Ccy'.

            LV_CKY = WA_FILE-CVALUE.
          WHEN 'ReqdExctnDt'.

            LV_EXTDATE = WA_FILE-CVALUE.


          WHEN 'Id'.

            LV_ID = WA_FILE-CVALUE.
            I = 0.

          WHEN 'MmbId'.

            LV_VIFSC = WA_FILE-CVALUE.
            I = 2.
          WHEN 'Nm'.


            IF I = 2.

              LV_VNAME = WA_FILE-CVALUE.

              WA_ITEMS-MANDT       = SY-MANDT.
              WA_ITEMS-MID         = LV_MID.
              WA_ITEMS-XMLNA       = LV_FILE.
              WA_ITEMS-BELNR       = LV_BELNR.
              WA_ITEMS-BIC         = LV_BIC.
              WA_ITEMS-DATSTAMP    = LV_DAT.
              WA_ITEMS-SYID        = LV_SISID.
              WA_ITEMS-RFID        = LV_RFID.
              WA_ITEMS-STATUS      = LV_STATUS.
              WA_ITEMS-REA_CODE    = LV_REA_CODE.
              WA_ITEMS-REASON      = LV_REASON.
              WA_ITEMS-AMOUNT      = LV_AMT.
              WA_ITEMS-CKY         = LV_CKY.
              WA_ITEMS-EXT_DATE    = LV_EXTDATE.
              WA_ITEMS-ID          = LV_ID.
              WA_ITEMS-VIFSC       = LV_VIFSC.
              WA_ITEMS-VNAME       = LV_VNAME.

              INSERT INTO ZHSBC_LI_RETURN VALUES WA_ITEMS.

              CLEAR WA_XML.

              SELECT * UP TO 1 ROWS FROM ZHSBC_ITEMS INTO WA_XML WHERE XMLNA = LV_FILE AND
                                                                        BELNR = LV_BELNR ORDER BY PRIMARY KEY.
              ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

              IF SY-SUBRC = 0.
                WA_XML-SYID     = LV_SISID.
                WA_XML-RFID     = LV_RFID.
                WA_XML-STATUS   = LV_STATUS.
                WA_XML-REA_CODE = LV_REA_CODE.
                WA_XML-REASON   = LV_REASON.
                WA_XML-ID       = LV_ID.

                UPDATE ZHSBC_ITEMS FROM WA_XML.

                IF SY-SUBRC = 0.

*                  SKIP 3.

                  IF  LV_STATUS = 'RJCT'.

                    WRITE: /20 LV_FILE,' ' ,LV_BELNR, 'Rejected for', LV_REASON.

                  ELSE.

                    WRITE: /20 LV_FILE,' ' ,LV_BELNR, 'Payment has sucessfully Done'.

                  ENDIF.


                ENDIF.

              ENDIF.
              CLEAR:  WA_ITEMS, LV_BELNR, LV_SISID, LV_RFID, LV_STATUS, LV_REA_CODE, LV_REASON , LV_AMT, LV_CKY, LV_EXTDATE, LV_ID , LV_VIFSC, LV_VNAME.


            ENDIF.


        ENDCASE.

      ENDLOOP.

      DATA : TEMP_STRING TYPE STRING,
                   SOURCE TYPE SAPB-SAPPFAD,
                   TARGET TYPE SAPB-SAPPFAD.

      CLEAR: TEMP_STRING , SOURCE, TARGET.

      CONCATENATE WA_SFTP-LOG_SPATH WA_FILE_LIST-NAME INTO  TEMP_STRING .
      CONDENSE TEMP_STRING.

      TARGET = TEMP_STRING.
      SOURCE = TEMP.

      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
        EXPORTING
          SOURCEPATH       = SOURCE
          TARGETPATH       = TARGET
*        IMPORTING
*          LENGTH           = EXCEPTIONS
*          ERROR_FILE       = 1
*          NO_AUTHORIZATION = 2
*          OTHERS           = 3.
.
      IF SY-SUBRC = 0.

        DELETE DATASET SOURCE.

      ENDIF.


      CLEAR : TEMP, TEMP1, TEMP_STRING.

    ELSE.


      LOOP AT IT_FILE INTO WA_FILE WHERE TYPE = 'V'.


        CASE WA_FILE-CNAME.
          WHEN 'MsgId'.
            LV_MID = WA_FILE-CVALUE.
          WHEN 'CreDtTm'.
            LV_DAT = WA_FILE-CVALUE.
          WHEN 'BICOrBEI'.
            LV_BIC = WA_FILE-CVALUE.
          WHEN 'OrgnlMsgId'.
            LV_FILE = WA_FILE-CVALUE.
          WHEN 'OrgnlNbOfTxs'.
            LV_TOT = WA_FILE-CVALUE.
          WHEN 'OrgnlCtrlSum'.
            LV_TOTAMT = WA_FILE-CVALUE.   "#EC CI_FLDEXT_OK[2610650]
          WHEN 'GrpSts'.
            LV_STATUS = WA_FILE-CVALUE.

          if lv_status = 'ACCP'.
            CLEAR WA_HEAD.

            WA_HEAD-MID = LV_MID.
            WA_HEAD-DATSTAMP = LV_DAT.
            WA_HEAD-BIC = LV_BIC.
            WA_HEAD-XMLNA = LV_FILE.
            WA_HEAD-TRNUM = LV_TOT.
            WA_HEAD-TOAMT = LV_TOTAMT.
            WA_HEAD-PSWSL = 'INR'.
            WA_HEAD-MANDT = SY-MANDT.
            WA_HEAD-STATUS = LV_STATUS.

            INSERT INTO ZHSBC_H_RETURN VALUES WA_HEAD.
            CLEAR WA_HEAD.



*            IF LV_STATUS = 'ACCP'.

              WRITE : /20 LV_FILE , ' File has Accepted by HSBC '.
*
*            ELSE.
*
*              WRITE : /20 LV_FILE , ' File has not Accepted by HSBC .  Error code :' , LV_STATUS.
*
*            ENDIF.

            CLEAR:  WA_ITEMS, LV_BELNR, LV_SISID, LV_RFID, LV_STATUS, LV_REA_CODE, LV_REASON , LV_AMT, LV_CKY, LV_EXTDATE, LV_ID , LV_VIFSC, LV_VNAME.
            endif.

            WHEN 'Cd'.

              lv_reason_code = WA_FILE-CVALUE.

            when 'AddtlInf'.

              lv_rea = WA_FILE-CVALUE.

              CLEAR WA_HEAD.

            WA_HEAD-MID = LV_MID.
            WA_HEAD-DATSTAMP = LV_DAT.
            WA_HEAD-BIC = LV_BIC.
            WA_HEAD-XMLNA = LV_FILE.
            WA_HEAD-TRNUM = LV_TOT.
            WA_HEAD-TOAMT = LV_TOTAMT.
            WA_HEAD-PSWSL = 'INR'.
            WA_HEAD-MANDT = SY-MANDT.
            WA_HEAD-STATUS = LV_STATUS.
            wa_head-REA_CODE = LV_REASON_CODE.
            wa_head-REASON = LV_REA.

            INSERT INTO ZHSBC_H_RETURN VALUES WA_HEAD.
            CLEAR WA_HEAD.

            refresh it_xml.
            CONDENSE lv_file.
            SELECT * from zhsbc_items INTO TABLE it_XML WHERE XMLNA = lv_file.

            if sy-subrc = 0.
              clear wa_xml.
              LOOP AT it_xml INTO wa_xml.

                wa_xml-status = lv_status.
                wa_xml-REA_CODE = LV_REASON_CODE.
                wa_xml-REASON  = LV_REA.

                MODIFY zhsbc_items FROM wa_xml.

                clear wa_xml.

              ENDLOOP.

             ENDIF.




              WRITE : /20 LV_FILE , ' File has not Accepted by HSBC .  Error code : ' , LV_REA_CODE ,'-', lv_rea.


            CLEAR:  WA_ITEMS, LV_BELNR, LV_SISID, LV_RFID, LV_STATUS, LV_REA_CODE, LV_REASON , LV_AMT, LV_CKY, LV_EXTDATE, LV_ID , LV_VIFSC, LV_VNAME, LV_REA, LV_REA_CODE.




        ENDCASE.

      ENDLOOP.

      DATA : TEMP_STRING1 TYPE STRING,
            SOURCE1 TYPE SAPB-SAPPFAD,
            TARGET1 TYPE SAPB-SAPPFAD.

      CLEAR: TEMP_STRING1 , SOURCE1, TARGET1.

      CONCATENATE WA_SFTP-LOG_fPATH WA_FILE_LIST-NAME INTO  TEMP_STRING1 .
      CONDENSE TEMP_STRING1.

      TARGET1 = TEMP_STRING1.
      SOURCE1 = TEMP.

      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
        EXPORTING
          SOURCEPATH             = SOURCE1
          TARGETPATH             = TARGET1
* IMPORTING
*   LENGTH                 =
* EXCEPTIONS
*   ERROR_FILE             = 1
*   NO_AUTHORIZATION       = 2
*   OTHERS                 = 3
                .
      IF SY-SUBRC = 0.

        DELETE DATASET SOURCE1.

      ENDIF.









    ENDIF.




  ENDIF.




  ENDLOOP.


ELSE.

  WRITE : 'Did not Return log Files yet from HSBC'.

ENDIF.

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
    OTHERS               = 5
          .
  IF P_RES <> 'X'.
    CLEAR P_RES.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
      EXPORTING
        DIRECTORY                = P_DIR
      CHANGING
        RC                       =  P_RES1
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
      OTHERS                   = 10
            .
    IF P_RES1 = 0.

      P_RES = 'X'.

    ENDIF.


  ENDIF.





ENDFORM.                    " FOLDER_CREATION
************************
*&---------------------------------------------------------------------*
*&      Form  CHECK_FILE_EXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_FNAME_TEMP  text
*      <--P_GV_CSV_ERROR  text
*----------------------------------------------------------------------*
FORM CHECK_FILE_EXT  CHANGING    P_FNAME
                      C_FLAG.
  DATA : LV_FILE_NAME(255) TYPE C.

  CLEAR : C_FLAG.
  CALL FUNCTION 'STRING_REVERSE'
    EXPORTING
      STRING  = P_FNAME
      LANG    = SY-LANGU
    IMPORTING
      RSTRING = P_FNAME.

  IF P_FNAME CS '\'.
    SPLIT P_FNAME AT '\' INTO LV_FILE_NAME GV_PATH.
  ELSE.
    SPLIT P_FNAME AT '/' INTO LV_FILE_NAME GV_PATH.
  ENDIF.
  SPLIT LV_FILE_NAME AT '.' INTO STR1 STR2.

  CALL FUNCTION 'STRING_REVERSE'
    EXPORTING
      STRING  = STR1
      LANG    = SY-LANGU
    IMPORTING
      RSTRING = STR1.
  IF STR1 NS 'csv' OR STR1 NS 'CSV'.
    C_FLAG = ABAP_TRUE.
  ENDIF.

ENDFORM.
*************************
