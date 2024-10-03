*&---------------------------------------------------------------------*
*& Report  ZHSBC_CSV_UTR_PULL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZHSBC_CSV_UTR_PULL.

INCLUDE ZHSBC_CSV_UPDATE_TOP.


SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE  TEXT-003.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(23) TEXT-004 MODIF ID MAK FOR FIELD P_MANU.
PARAMETERS: P_AUTO RADIOBUTTON GROUP R1 USER-COMMAND FLG DEFAULT 'X' MODIF ID MAK.  "Manual Upload

SELECTION-SCREEN COMMENT 40(17) TEXT-001 MODIF ID MAK FOR FIELD P_MANU.
PARAMETERS: P_MANU RADIOBUTTON GROUP R1  MODIF ID MAK .     "Automatic MIS Upload

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B0.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: MIS_PATH TYPE RLGRAP-FILENAME MODIF ID MAT.
SELECTION-SCREEN END OF BLOCK B1.


AT SELECTION-SCREEN OUTPUT.
  PERFORM MANUAL_AUTO_UOLOAD.


****F4 help for file upload
AT SELECTION-SCREEN ON VALUE-REQUEST FOR MIS_PATH.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'MIS_PATH'
    IMPORTING
      FILE_NAME     = MIS_PATH.


*&---------------------------------------------------------------------*
*&      Form  MANUAL_AUTO_UOLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MANUAL_AUTO_UOLOAD .

  LOOP AT SCREEN.
    IF P_AUTO EQ 'X'.
      IF SCREEN-GROUP1 = 'MAT'.
        SCREEN-ACTIVE = '0'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " MANUAL_AUTO_UOLOAD

START-OF-SELECTION.



  IF P_MANU = ABAP_TRUE.
    PERFORM AUTHCHECK.
    DATA: LV_PATH TYPE STRING.
    CLEAR: GV_CSV_ERROR.

    LV_PATH       = MIS_PATH.
    GV_FNAME_TEMP = MIS_PATH.

    PERFORM CHECK_FILE_EXT USING GV_FNAME_TEMP
                            CHANGING GV_CSV_ERROR.

    IF GV_CSV_ERROR IS INITIAL.

      CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        FILENAME                      =  LV_PATH
*     FILETYPE                      = 'ASC'
*     HAS_FIELD_SEPARATOR           = ' '
*   HEADER_LENGTH                 = 0
*     READ_BY_LINE                  = 'X'
*     DAT_MODE                      = ' '
*     CODEPAGE                      = ' '
*     IGNORE_CERR                   = ABAP_TRUE
*     REPLACEMENT                   = '#'
*     CHECK_BOM                     = ' '
*     VIRUS_SCAN_PROFILE            =
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     FILELENGTH                    =
*     HEADER                        =
      TABLES
        DATA_TAB                      =  GIT_UPLOAD[]
     EXCEPTIONS
       FILE_OPEN_ERROR               = 1
       FILE_READ_ERROR               = 2
       NO_BATCH                      = 3
       GUI_REFUSE_FILETRANSFER       = 4
       INVALID_TYPE                  = 5
       NO_AUTHORITY                  = 6
       UNKNOWN_ERROR                 = 7
       BAD_DATA_FORMAT               = 8
       HEADER_NOT_ALLOWED            = 9
       SEPARATOR_NOT_ALLOWED         = 10
       HEADER_TOO_LONG               = 11
       UNKNOWN_DP_ERROR              = 12
       ACCESS_DENIED                 = 13
       DP_OUT_OF_MEMORY              = 14
       DISK_FULL                     = 15
       DP_TIMEOUT                    = 16
       OTHERS                        = 17
              .
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ELSE.
        IF SY-SUBRC EQ 0.
          DELETE GIT_UPLOAD[] INDEX 1.
        ENDIF.
        APPEND LINES OF GIT_UPLOAD[] TO GIT_RAW[].
        REPLACE ALL OCCURRENCES OF '"' IN TABLE GIT_RAW WITH SPACE IN CHARACTER MODE.

        IF GIT_RAW IS NOT INITIAL.
          LOOP AT GIT_RAW INTO WA_RAW.

            SEARCH WA_RAW-TEXT FOR ','.

            IF SY-SUBRC = 0.

              SPLIT WA_RAW-TEXT AT ',' INTO WA_TEXT-FIELD1  WA_TEXT-FIELD2  WA_TEXT-FIELD3
                                            WA_TEXT-FIELD4  WA_TEXT-FIELD5  WA_TEXT-FIELD6
                                            WA_TEXT-FIELD7  WA_TEXT-FIELD8  WA_TEXT-FIELD9
                                            WA_TEXT-FIELD10 WA_TEXT-FIELD11 WA_TEXT-FIELD12
                                            WA_TEXT-FIELD13 WA_TEXT-FIELD14 WA_TEXT-FIELD15
                                            WA_TEXT-FIELD16 WA_TEXT-FIELD17 WA_TEXT-FIELD18
                                            WA_TEXT-FIELD19 WA_TEXT-FIELD20 WA_TEXT-FIELD21
                                            WA_TEXT-FIELD22 WA_TEXT-FIELD23 WA_TEXT-FIELD24
                                            WA_TEXT-FIELD25 WA_TEXT-FIELD26 WA_TEXT-FIELD27
                                            WA_TEXT-FIELD28 WA_TEXT-FIELD29 WA_TEXT-FIELD30.
            ENDIF.
            APPEND WA_TEXT TO GIT_TEXT.
            CLEAR: WA_TEXT,WA_RAW.
          ENDLOOP.

        ENDIF.
        LOOP AT GIT_TEXT INTO WA_TEXT.

          SELECT * UP TO 1 ROWS FROM ZHSBC_ITEMS INTO WA_HSBC WHERE BELNR = WA_TEXT-FIELD4 ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

          MOVE-CORRESPONDING WA_HSBC TO FI_WA_FINAL.
          FI_WA_FINAL-BELNR = WA_HSBC-BELNR.
          FI_WA_FINAL-BUKRS =  WA_HSBC-BUKRS.
          FI_WA_FINAL-GJAHR =  WA_HSBC-GJAHR.
          APPEND FI_WA_FINAL  TO FI_IT_FINAL.
          CLEAR :WA_HSBC,FI_WA_FINAL.

          PERFORM FETCH_REQ_DATA.
        ENDLOOP.
*          IF FI_WA_FINAL IS NOT INITIAL.
        LOOP AT FI_IT_FINAL INTO FI_WA_FINAL .


          CALL FUNCTION 'FI_DOCUMENT_READ' "#EC CI_USAGE_OK[2628706] " Added by <IT-CAR Tool> during Code Remediation
            EXPORTING
              I_BUKRS     = FI_WA_FINAL-BUKRS
              I_BELNR     = FI_WA_FINAL-BELNR
              I_GJAHR     = FI_WA_FINAL-GJAHR
            TABLES
              T_BSEG      = GIT_BSEG_FI
            EXCEPTIONS
              WRONG_INPUT = 1
              NOT_FOUND   = 2
              OTHERS      = 3.
          IF SY-SUBRC = 0.

            "CLEAR:  LV_SPCL,LV_BUZEI. "LV_COUNT,

            LOOP AT GIT_BSEG_FI INTO WA_BSEG_FI.   " WHERE koart = 'K'.   "only vendor line item

              CLEAR : GV_AWREF,GV_AWORG,BKPF,BVOR.
              REFRESH GIT_ACCCHG[].


              READ TABLE GIT_BKPF INTO BKPF WITH KEY BUKRS = WA_BSEG_FI-BUKRS
                                                     BELNR = WA_BSEG_FI-BELNR
                                                     GJAHR = WA_BSEG_FI-GJAHR.
              READ TABLE GIT_BVOR INTO BVOR WITH KEY BVORG = BKPF-BVORG
                                                     GJAHR = WA_BSEG_FI-GJAHR.
              IF SY-SUBRC = 0.
                GV_AWREF = BVOR-BELNR.
                CONCATENATE BVOR-BUKRS WA_BSEG_FI-GJAHR INTO GV_AWORG.
              ELSE.
                GV_AWREF = WA_BSEG_FI-BELNR.
                CONCATENATE WA_BSEG_FI-BUKRS WA_BSEG_FI-GJAHR INTO GV_AWORG.
              ENDIF.

*                IF WA_FINAL-P_CODE IS NOT INITIAL.
*
*                  PERFORM GET_REQ_LEN_UTR USING    WA_FINAL-P_CODE
*                                          CHANGING GV_UTR_NUMBER.
              READ TABLE GIT_TEXT INTO  WA_TEXT WITH KEY FIELD4 = WA_BSEG_FI-BELNR.

              WA_ACCCHG-FDNAME = 'KIDNO' .               "'ZUONR'.
              WA_ACCCHG-OLDVAL = WA_BSEG_FI-KIDNO.          "WA_BSEG-ZUONR.
              WA_ACCCHG-NEWVAL = WA_TEXT-FIELD13.        "WA_FINAL-P_CODE.
              APPEND WA_ACCCHG TO GIT_ACCCHG.
              CLEAR WA_ACCCHG.
*                ENDIF.

              CALL FUNCTION 'FI_DOCUMENT_CHANGE'
                EXPORTING
                  I_AWTYP              = 'BKPF'
                  I_AWREF              = GV_AWREF
                  I_AWORG              = GV_AWORG
*                 I_AWSYS              = ' '
*                 I_KUNNR              = ' '
*                 I_LIFNR              = ' '
*                 I_HKONT              = ' '
*                 I_OBZEI              = ' '
                  I_BUZEI              = WA_BSEG_FI-BUZEI
*                 I_BSEGC              =
*                 X_LOCK               = 'X'
*                 I_BUKRS              =
*                 I_BELNR              =
*                 I_GJAHR              =
                TABLES
                  T_ACCCHG             = GIT_ACCCHG
                EXCEPTIONS
                  NO_REFERENCE         = 1
                  NO_DOCUMENT          = 2
                  MANY_DOCUMENTS       = 3
                  WRONG_INPUT          = 4
                  OVERWRITE_CREDITCARD = 5
                  OTHERS               = 6.




              IF SY-SUBRC = 0.
*                  PERFORM APPEND_DISPLAY USING WA_BSEG
*                                               WA_FINAL
*                                               TEXT-500     "'SUCCESSFULLY UPDATED IN SAP'
*                                               C_GRN_COLOUR.
                WRITE: /100 WA_BSEG_FI-BELNR,' ',WA_BSEG_FI-BUZEI, 'File UTR has sucessfully Done'.
              ELSEIF SY-SUBRC = 4.
*                  PERFORM APPEND_DISPLAY USING WA_BSEG
*                                               WA_FINAL
*                                               TEXT-501     "'DOCMENT NOT FOUND IN SAP, PLEASE CHECK'
*                                               C_RED_COLOUR.
                WRITE: /100 WA_BSEG_FI-BELNR,' ',WA_BSEG_FI-BUZEI,' ' ,TEXT-501, 'File UTR Unsucessful'.
              ELSE.
                CLEAR GV_TEXT.
                CONCATENATE SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO GV_TEXT.
*                  PERFORM APPEND_DISPLAY USING WA_BSEG
*                                               WA_FINAL
*                                               GV_TEXT
*                                               C_RED_COLOUR.
                WRITE: /100 LV_PATH,' ' ,WA_TEXT-FIELD4, ' ' ,GV_TEXT, 'File UTR Unsucessful'.
              ENDIF.

              CLEAR: WA_BSEG.

            ENDLOOP.

          ELSE.
*              PERFORM APPEND_DISPLAY USING WA_BSEG
*                                           WA_FINAL
*                                           TEXT-501     " 'DOCMENT NOT FOUND IN SAP, PLEASE CHECK'
*                                           C_RED_COLOUR.
            WRITE: /100 LV_PATH,' ' ,WA_TEXT-FIELD4,' ',TEXT-501, 'File UTR Unsucessful'.
          ENDIF.



*            SELECT SINGLE * FROM ZHSBC_ITEMS INTO WA_HSBC WHERE BELNR = WA_TEXT-FIELD4.
*            IF WA_HSBC IS NOT INITIAL.
*              UPDATE BSEG SET KIDNO = WA_TEXT-FIELD13 WHERE BUKRS =  WA_HSBC-BUKRS AND BELNR = WA_HSBC-BELNR AND GJAHR = WA_HSBC-GJAHR.
*            ENDIF.
*            IF SY-SUBRC = 0.
*              WRITE: /100 LV_PATH,' ' ,WA_HSBC-BELNR, 'File UTR has sucessfully Done'.
*            ELSE.
*              WRITE: /100 LV_PATH,' ' ,WA_TEXT-FIELD4, 'File UTR Unsucessful'.
*            ENDIF.
          CLEAR:WA_HSBC,WA_TEXT,FI_WA_FINAL,WA_FINAL_CC,WA_BSEG_FI.
          REFRESH GIT_BSEG_FI.
*          ENDIF.
        ENDLOOP.


      ENDIF.
    ELSE.
      PERFORM POP_UP_ERROR USING 'File Not Uploaded....'.
    ENDIF.

  ELSEIF P_AUTO = ABAP_TRUE.
    PERFORM AUTHCHECK.

    SELECT SINGLE * FROM ZHSBC_SFTP INTO WA_SFTP.

    IF WA_SFTP-LOG_FPATH IS INITIAL OR WA_SFTP-LOG_SPATH IS INITIAL OR WA_SFTP-LOG_FPATH IS INITIAL.

      MESSAGE 'Please Maintain Server Connectivity in ZHSBC' TYPE 'E'.

    ENDIF.

    CLEAR : FP, ACTIVITY.
    ACTIVITY = 'WRITE'.
    FP = WA_SFTP-LOG_FPATH.

    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
*       PROGRAM          =
        ACTIVITY         = ACTIVITY
        FILENAME         = FP
      EXCEPTIONS
        NO_AUTHORITY     = 1
        ACTIVITY_UNKNOWN = 2
        OTHERS           = 3.
    IF SY-SUBRC <> 0.

      MESSAGE 'No authorization to access Application server file' TYPE 'E'.

    ENDIF.



    CLEAR LV_DIR.
    LV_DIR = WA_SFTP-LOG_PATH.

    REFRESH : IT_FILE_LIST.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        IV_DIR_NAME                  = LV_DIR
*       FILE_MASK                    = ' '
*     IMPORTING
*       DIR_NAME                     =
*       FILE_COUNTER                 =
*       ERROR_COUNTER                =
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
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    LOOP AT IT_FILE_LIST INTO WA_FILE_LIST.
*  DATA: GV_FNAME_TEMP(255)     TYPE C,
*        GV_CSV_ERROR           TYPE C.
*        LV_DIR.
      DATA: SHARED_PATH            TYPE RLGRAP-FILENAME,
            SHARED_FILE            TYPE STRING.
      CLEAR : SHARED_FILE.
      CONCATENATE LV_DIR WA_FILE_LIST-NAME INTO SHARED_FILE.
      GV_FNAME_TEMP = SHARED_FILE.
      PERFORM CHECK_FILE_EXT USING GV_FNAME_TEMP
                             CHANGING GV_CSV_ERROR.
      IF GV_CSV_ERROR IS INITIAL.
        DATA: WA_FILE_DATA TYPE TEXT4096.
*              wa_testfile  TYPE ty_testfile,
*              wa_testfile_file TYPE ty_testfile_file.
        DATA: TB_FILE_DATA TYPE TABLE OF TEXT4096.
*      tb_testfile TYPE TABLE OF ty_testfile,
*      tb_testfile_file TYPE TABLE OF ty_testfile_file.
        CLASS CL_ABAP_CHAR_UTILITIES DEFINITION LOAD.
* To read output in specified format from application server
        PERFORM UPLOAD_APP_SERVER.
        DATA: LV_FILE_SEPARATOR TYPE C.
        LV_FILE_SEPARATOR = CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

        CALL FUNCTION 'TEXT_CONVERT_TEX_TO_SAP'
          EXPORTING
            I_FIELD_SEPERATOR    = LV_FILE_SEPARATOR
            I_TAB_RAW_DATA       = TB_FILE_DATA
          TABLES
            I_TAB_CONVERTED_DATA = GIT_UPLOAD[]
          EXCEPTIONS
            CONVERSION_FAILED    = 1
            OTHERS               = 2.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          IF SY-SUBRC EQ 0.
            DELETE GIT_UPLOAD[] INDEX 1.
          ENDIF.
          APPEND LINES OF GIT_UPLOAD[] TO GIT_RAW[].
          REPLACE ALL OCCURRENCES OF '"' IN TABLE GIT_RAW WITH SPACE IN CHARACTER MODE.

          IF GIT_RAW IS NOT INITIAL.
            LOOP AT GIT_RAW INTO WA_RAW.
              SEARCH WA_RAW-TEXT FOR ','.
              IF SY-SUBRC = 0.
                SPLIT WA_RAW-TEXT AT ',' INTO WA_TEXT-FIELD1  WA_TEXT-FIELD2  WA_TEXT-FIELD3
                                              WA_TEXT-FIELD4  WA_TEXT-FIELD5  WA_TEXT-FIELD6
                                              WA_TEXT-FIELD7  WA_TEXT-FIELD8  WA_TEXT-FIELD9
                                              WA_TEXT-FIELD10 WA_TEXT-FIELD11 WA_TEXT-FIELD12
                                              WA_TEXT-FIELD13 WA_TEXT-FIELD14 WA_TEXT-FIELD15
                                              WA_TEXT-FIELD16 WA_TEXT-FIELD17 WA_TEXT-FIELD18
                                              WA_TEXT-FIELD19 WA_TEXT-FIELD20 WA_TEXT-FIELD21
                                              WA_TEXT-FIELD22 WA_TEXT-FIELD23 WA_TEXT-FIELD24
                                              WA_TEXT-FIELD25 WA_TEXT-FIELD26 WA_TEXT-FIELD27
                                              WA_TEXT-FIELD28 WA_TEXT-FIELD29 WA_TEXT-FIELD30.
              ENDIF.
              APPEND WA_TEXT TO GIT_TEXT.
              CLEAR: WA_TEXT,WA_RAW.
            ENDLOOP.
          ENDIF.

          LOOP AT GIT_TEXT INTO WA_TEXT.
             SELECT * UP TO 1 ROWS FROM ZHSBC_ITEMS INTO WA_HSBC WHERE BELNR = WA_TEXT-FIELD4 ORDER BY PRIMARY KEY.
             ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

          MOVE-CORRESPONDING WA_HSBC TO FI_WA_FINAL.
          FI_WA_FINAL-BELNR = WA_HSBC-BELNR.
          FI_WA_FINAL-BUKRS =  WA_HSBC-BUKRS.
          FI_WA_FINAL-GJAHR =  WA_HSBC-GJAHR.
          APPEND FI_WA_FINAL  TO FI_IT_FINAL.
          CLEAR :WA_HSBC,FI_WA_FINAL.

          PERFORM FETCH_REQ_DATA.
*            SELECT SINGLE * FROM ZHSBC_ITEMS INTO WA_HSBC WHERE BELNR = WA_TEXT-FIELD4.
*            IF WA_HSBC IS NOT INITIAL.
*              UPDATE BSEG SET KIDNO = WA_TEXT-FIELD13 WHERE BUKRS =  WA_HSBC-BUKRS AND BELNR = WA_HSBC-BELNR AND GJAHR = WA_HSBC-GJAHR.
*            ENDIF.
*            CLEAR:WA_HSBC,WA_TEXT.
          ENDLOOP.
          LOOP AT FI_IT_FINAL INTO FI_WA_FINAL .


          CALL FUNCTION 'FI_DOCUMENT_READ' "#EC CI_USAGE_OK[2628706] " Added by <IT-CAR Tool> during Code Remediation
            EXPORTING
              I_BUKRS     = FI_WA_FINAL-BUKRS
              I_BELNR     = FI_WA_FINAL-BELNR
              I_GJAHR     = FI_WA_FINAL-GJAHR
            TABLES
              T_BSEG      = GIT_BSEG_FI
            EXCEPTIONS
              WRONG_INPUT = 1
              NOT_FOUND   = 2
              OTHERS      = 3.
          IF SY-SUBRC = 0.

            "CLEAR:  LV_SPCL,LV_BUZEI. "LV_COUNT,

            LOOP AT GIT_BSEG_FI INTO WA_BSEG_FI.   " WHERE koart = 'K'.   "only vendor line item

              CLEAR : GV_AWREF,GV_AWORG,BKPF,BVOR.
              REFRESH GIT_ACCCHG[].


              READ TABLE GIT_BKPF INTO BKPF WITH KEY BUKRS = WA_BSEG_FI-BUKRS
                                                     BELNR = WA_BSEG_FI-BELNR
                                                     GJAHR = WA_BSEG_FI-GJAHR.
              READ TABLE GIT_BVOR INTO BVOR WITH KEY BVORG = BKPF-BVORG
                                                     GJAHR = WA_BSEG_FI-GJAHR.
              IF SY-SUBRC = 0.
                GV_AWREF = BVOR-BELNR.
                CONCATENATE BVOR-BUKRS WA_BSEG_FI-GJAHR INTO GV_AWORG.
              ELSE.
                GV_AWREF = WA_BSEG_FI-BELNR.
                CONCATENATE WA_BSEG_FI-BUKRS WA_BSEG_FI-GJAHR INTO GV_AWORG.
              ENDIF.

*                IF WA_FINAL-P_CODE IS NOT INITIAL.
*
*                  PERFORM GET_REQ_LEN_UTR USING    WA_FINAL-P_CODE
*                                          CHANGING GV_UTR_NUMBER.
              READ TABLE GIT_TEXT INTO  WA_TEXT WITH KEY FIELD4 = WA_BSEG_FI-BELNR.

              WA_ACCCHG-FDNAME = 'KIDNO' .               "'ZUONR'.
              WA_ACCCHG-OLDVAL = WA_BSEG_FI-KIDNO.          "WA_BSEG-ZUONR.
              WA_ACCCHG-NEWVAL = WA_TEXT-FIELD13.        "WA_FINAL-P_CODE.
              APPEND WA_ACCCHG TO GIT_ACCCHG.
              CLEAR WA_ACCCHG.
*                ENDIF.

              CALL FUNCTION 'FI_DOCUMENT_CHANGE'
                EXPORTING
                  I_AWTYP              = 'BKPF'
                  I_AWREF              = GV_AWREF
                  I_AWORG              = GV_AWORG
*                 I_AWSYS              = ' '
*                 I_KUNNR              = ' '
*                 I_LIFNR              = ' '
*                 I_HKONT              = ' '
*                 I_OBZEI              = ' '
                  I_BUZEI              = WA_BSEG_FI-BUZEI
*                 I_BSEGC              =
*                 X_LOCK               = 'X'
*                 I_BUKRS              =
*                 I_BELNR              =
*                 I_GJAHR              =
                TABLES
                  T_ACCCHG             = GIT_ACCCHG
                EXCEPTIONS
                  NO_REFERENCE         = 1
                  NO_DOCUMENT          = 2
                  MANY_DOCUMENTS       = 3
                  WRONG_INPUT          = 4
                  OVERWRITE_CREDITCARD = 5
                  OTHERS               = 6.




              IF SY-SUBRC = 0.
                DATA : MOVEFILENAME TYPE CHAR255.
          MOVEFILENAME = WA_FILE_LIST-NAME.
          CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
              EXPORTING
               COMMANDNAME                         = 'ZHSBC_COPY'
               ADDITIONAL_PARAMETERS               = MOVEFILENAME
*                 OPERATINGSYSTEM                     = SY-OPSYS
*                 TARGETSYSTEM                        = SY-HOST
*                 DESTINATION                         =
*                 STDOUT                              = 'X'
*                 STDERR                              = 'X'
*                 TERMINATIONWAIT                     = 'X'
*                 TRACE                               =
*                 DIALOG                              =
*               IMPORTING
*                 STATUS                              =
*                 EXITCODE                            =
*               TABLES
*                 EXEC_PROTOCOL                       =
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
            IF  SY-SUBRC = 1.
              WRITE : 'No Authorization to Access OS command'.
              EXIT.
            ELSE.
              WRITE : 'Connectivity to HSBC server failed [OS Command]'.
              EXIT.
            ENDIF.
          ENDIF.
*                  PERFORM APPEND_DISPLAY USING WA_BSEG
*                                               WA_FINAL
*                                               TEXT-500     "'SUCCESSFULLY UPDATED IN SAP'
*                                               C_GRN_COLOUR.
                WRITE: /100 WA_BSEG_FI-BELNR,' ',WA_BSEG_FI-BUZEI, 'File UTR has sucessfully Done'.
              ELSEIF SY-SUBRC = 4.
*                  PERFORM APPEND_DISPLAY USING WA_BSEG
*                                               WA_FINAL
*                                               TEXT-501     "'DOCMENT NOT FOUND IN SAP, PLEASE CHECK'
*                                               C_RED_COLOUR.
                WRITE: /100 WA_BSEG_FI-BELNR,' ',WA_BSEG_FI-BUZEI,' ' ,TEXT-501, 'File UTR Unsucessful'.
              ELSE.
                CLEAR GV_TEXT.
                CONCATENATE SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO GV_TEXT.
*                  PERFORM APPEND_DISPLAY USING WA_BSEG
*                                               WA_FINAL
*                                               GV_TEXT
*                                               C_RED_COLOUR.
                WRITE: /100 LV_PATH,' ' ,WA_TEXT-FIELD4, ' ' ,GV_TEXT, 'File UTR Unsucessful'.
              ENDIF.

              CLEAR: WA_BSEG.

            ENDLOOP.

          ELSE.
*              PERFORM APPEND_DISPLAY USING WA_BSEG
*                                           WA_FINAL
*                                           TEXT-501     " 'DOCMENT NOT FOUND IN SAP, PLEASE CHECK'
*                                           C_RED_COLOUR.
            WRITE: /100 LV_PATH,' ' ,WA_TEXT-FIELD4,' ',TEXT-501, 'File UTR Unsucessful'.
          ENDIF.



*            SELECT SINGLE * FROM ZHSBC_ITEMS INTO WA_HSBC WHERE BELNR = WA_TEXT-FIELD4.
*            IF WA_HSBC IS NOT INITIAL.
*              UPDATE BSEG SET KIDNO = WA_TEXT-FIELD13 WHERE BUKRS =  WA_HSBC-BUKRS AND BELNR = WA_HSBC-BELNR AND GJAHR = WA_HSBC-GJAHR.
*            ENDIF.
*            IF SY-SUBRC = 0.
*              WRITE: /100 LV_PATH,' ' ,WA_HSBC-BELNR, 'File UTR has sucessfully Done'.
*            ELSE.
*              WRITE: /100 LV_PATH,' ' ,WA_TEXT-FIELD4, 'File UTR Unsucessful'.
*            ENDIF.
          CLEAR:WA_HSBC,WA_TEXT,FI_WA_FINAL,WA_FINAL_CC,WA_BSEG_FI.
          REFRESH GIT_BSEG_FI.
*          ENDIF.
        ENDLOOP.

        ENDIF.

*        IF SY-SUBRC = 0.



*          DATA : TEMP_STRING TYPE STRING,
*                   SOURCE TYPE STRING,
*                   TARGET TYPE STRING.
*
*          CLEAR: TEMP_STRING , SOURCE, TARGET.
*          CONCATENATE WA_SFTP-LOG_SPATH WA_FILE_LIST-NAME INTO  TEMP_STRING .
*          CONDENSE TEMP_STRING.
*
*          TARGET = TEMP_STRING.
*          SOURCE = SHARED_FILE.
*          PERFORM MOVE_FILE USING SOURCE TARGET .
*        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.


*  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  CHECK_FILE_EXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_FNAME_TEMP  text
*      <--P_GV_CSV_ERROR  text
*----------------------------------------------------------------------*
FORM CHECK_FILE_EXT  USING    P_FNAME
                     CHANGING C_FLAG.
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

ENDFORM.                    " CHECK_FILE_EXT


*&---------------------------------------------------------------------*
*&      Form  POP_UP_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_107  text
*----------------------------------------------------------------------*
FORM POP_UP_ERROR  USING    P_TEXT_107.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = TEXT-E02  "'Error Message'
      TEXT_QUESTION         = P_TEXT_107
      TEXT_BUTTON_1         = 'Close'(E05)
      ICON_BUTTON_1         = 'ICON_DUMMY'
      TEXT_BUTTON_2         = 'Exit'(E06)
      ICON_BUTTON_2         = 'ICON_INCOMPLETE'
      DISPLAY_CANCEL_BUTTON = ' '
      IV_QUICKINFO_BUTTON_1 = TEXT-E03 "'Close this Window'
      IV_QUICKINFO_BUTTON_2 = TEXT-E04 "'Exit the Entire Transaction'
    IMPORTING
      ANSWER                = G_POPUP_ANSWER.

  IF G_POPUP_ANSWER = C_CLOSE.
    STOP.
  ELSE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.                    " POP_UP_ERROR


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
*&      Form  upload_app_server
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM UPLOAD_APP_SERVER.
  DATA: LV_APP_SERVER_FILE TYPE STRING.
  LV_APP_SERVER_FILE = SHARED_FILE.
* To read file from Application server
  OPEN DATASET LV_APP_SERVER_FILE FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  DO.
    READ DATASET LV_APP_SERVER_FILE INTO WA_FILE_DATA.
    IF SY-SUBRC = 0.
      APPEND WA_FILE_DATA TO TB_FILE_DATA.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CLOSE DATASET LV_APP_SERVER_FILE.
ENDFORM.                    "upload_app_server



*&---------------------------------------------------------------------*
*&      Form  move_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_S_FILEPATH  text
*      -->PV_T_FILEPATH  text
*----------------------------------------------------------------------*
FORM MOVE_FILE USING PV_S_FILEPATH TYPE STRING
                     PV_T_FILEPATH TYPE STRING.

  DATA: LV_EOF_REACHED   TYPE FLAG,
        LV_BUFFER(20480),
        LV_BUFLEN        TYPE I.

  "open source and target files and start copying
  OPEN DATASET PV_S_FILEPATH FOR INPUT IN BINARY MODE.
  IF SY-SUBRC NE 0.
    "Add error handling
  ENDIF.

  OPEN DATASET PV_T_FILEPATH FOR OUTPUT IN BINARY MODE.
  IF SY-SUBRC NE 0.
    "Add error handling
  ENDIF.

  CLEAR LV_EOF_REACHED.
  DO.
    CLEAR LV_BUFFER.

    READ DATASET PV_S_FILEPATH
    INTO LV_BUFFER LENGTH LV_BUFLEN.

    IF SY-SUBRC = 4.
      LV_EOF_REACHED = ABAP_TRUE.
    ELSEIF SY-SUBRC > 4.
      "Add error handling
    ENDIF.

    TRANSFER LV_BUFFER TO PV_T_FILEPATH
    LENGTH LV_BUFLEN.

    IF SY-SUBRC NE 0.
      "Add error handling

    ENDIF.

    IF LV_EOF_REACHED = ABAP_TRUE.
      EXIT.
    ENDIF.
  ENDDO.

  "Delete the source file
  DELETE DATASET PV_S_FILEPATH.

  "Close the connection
  CLOSE DATASET PV_S_FILEPATH.
  CLOSE DATASET PV_T_FILEPATH.
  IF SY-SUBRC <> 0.
    "Add error handling
  ENDIF.
ENDFORM.  "MOVE_FILE

*&---------------------------------------------------------------------*
*&      Form  FETCH_REQ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FETCH_REQ_DATA .
* ***  PERFORM GET_GL_ACCOUNTS.  "commented on 10102017 for updating all line item

  IF FI_IT_FINAL[] IS NOT INITIAL.
    SELECT * FROM BKPF INTO TABLE GIT_BKPF FOR ALL ENTRIES IN FI_IT_FINAL
                                     WHERE BUKRS = FI_IT_FINAL-BUKRS
                                      AND  BELNR = FI_IT_FINAL-BELNR
                                      AND  GJAHR = FI_IT_FINAL-GJAHR ORDER BY PRIMARY KEY.
    IF GIT_BKPF[] IS NOT INITIAL .
      SELECT * FROM BVOR INTO TABLE GIT_BVOR FOR ALL ENTRIES IN GIT_BKPF
                                    WHERE BVORG  = GIT_BKPF-BVORG
                                      AND BUKRS <> GIT_BKPF-BUKRS
                                      AND GJAHR  = GIT_BKPF-GJAHR.
      DELETE GIT_BVOR[] WHERE BVORG IS INITIAL.
      IF GIT_BVOR[] IS NOT INITIAL .
        SELECT * FROM BKPF APPENDING TABLE GIT_BKPF         " temp folder to store cc payments
                                      FOR ALL ENTRIES IN GIT_BVOR
                                      WHERE BUKRS = GIT_BVOR-BUKRS
                                      AND   GJAHR = GIT_BVOR-GJAHR
                                      AND   BVORG = GIT_BVOR-BVORG ORDER BY PRIMARY KEY.

      ENDIF.
    ENDIF.
**--------Append Cross Company Payment Doc to Final&log table--------***

    LOOP AT FI_IT_FINAL INTO FI_WA_FINAL.

      CLEAR :BKPF_CC,WA_FINAL_CC."WA_MAIN_LOG.
      READ TABLE GIT_BKPF INTO BKPF WITH KEY BUKRS = FI_WA_FINAL-BUKRS
                                             BELNR = FI_WA_FINAL-BELNR
                                             GJAHR = FI_WA_FINAL-GJAHR.
      IF BKPF-BVORG IS NOT INITIAL.   "if cross comp payments are there then add
        LOOP AT GIT_BKPF INTO BKPF_CC WHERE BUKRS <> BKPF-BUKRS
                                        AND BVORG =  WA_BKPF-BVORG
                                        AND GJAHR =  WA_BKPF-GJAHR.


          MOVE-CORRESPONDING FI_WA_FINAL TO WA_FINAL_CC.
          WA_FINAL_CC-BELNR = BKPF_CC-BELNR.
          WA_FINAL_CC-BUKRS = BKPF_CC-BUKRS.
          APPEND WA_FINAL_CC TO GIT_FINAL_CC.

*          MOVE-CORRESPONDING WA_FINAL_CC TO WA_MAIN_LOG.
*          WA_MAIN_LOG-UTR_NUMBER =  WA_FINAL-P_CODE.
*          APPEND WA_MAIN_LOG  TO GIT_MAIN_LOG.
          CLEAR :BKPF_CC,WA_FINAL_CC."WA_MAIN_LOG.
          EXIT. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
        ENDLOOP.
      ENDIF.
      CLEAR :BKPF_CC,BKPF,WA_FINAL,WA_FINAL_CC.
    ENDLOOP.
    APPEND LINES OF GIT_FINAL_CC TO FI_IT_FINAL.
  ENDIF.

ENDFORM.                    " FETCH_REQ_DATA
*&---------------------------------------------------------------------*
*&      Form  AUTHCHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AUTHCHECK .
  AUTHORITY-CHECK OBJECT 'ZCSV_DOWN'
  ID 'ACTVT' FIELD '01'.
  IF SY-SUBRC NE 0.
    MESSAGE 'NO AUTHORIZATION FOR UPLOAD RECORDS.' TYPE 'S'.
  ENDIF.
ENDFORM.                    " AUTHCHECK
