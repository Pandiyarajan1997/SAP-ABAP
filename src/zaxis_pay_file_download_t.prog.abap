*----------------------------------------------------------------------*
***INCLUDE ZAXIS_PAY_FILE_DOWNLOAD .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Text         FILE DOWNLOADING AND UPDATING LOG INTERNAL TABLE
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Form  download_and_updatelog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_AND_UPDATELOG .

  CLEAR: L_FILE_COUNT,
         L_DIR_COUNT,
         L_WA_TAB_SDOKPATH,
         L_SOURCE,
         L_DESTINATION,
         L_RETURN,
         L_FILEPATH_IMPORT,
         L_FILEPATH_PROCESSED.


  REFRESH: L_TAB_SDOKPATH,  L_TAB1_SDOKPATH.

  CLEAR : DOWNLOAD_ERROR,
          SP_DOWNLOAD_ERROR,
          LP_DOWNLOAD_ERROR,
          FTP_DOWNLOAD_ERROR,
          SHARED_DOWNLOAD_ERROR,
          LP_FILEPATH,
          SP_FILEPATH,
          SHARED_FILE,
          LP_FILE,
          SP_FILE,
          GV_TEXT1.
*----------------------------------------------------------------------*
*  -->  p1        LOCAL FILE DOWNLOAD
*----------------------------------------------------------------------*

  CONCATENATE ZAXIS_TAB_HB-FILE_START '_' SY-DATUM SY-UZEIT INTO GV_FILENAME.
  CLEAR : WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                              BANK_CODE = ' '
                              FIELDNAME = ' '"P_BUKRS
                              OLDVALUE  = 'LP_IND'.

  IF SY-SUBRC EQ 0 AND WA_CONVERS-NEWVALUE EQ 'Y'.

    PERFORM LOCAL_FILE_DOWNLOAD.

  ENDIF.  "LP INDICATOR CHECKING.

*----------------------------------------------------------------------*
*  -->  p1        SERVER FILE DOWNLOAD
*----------------------------------------------------------------------*
  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                              BANK_CODE = ' '
                              FIELDNAME = ' '"P_BUKRS
                              OLDVALUE  = 'SP_IND'.


  IF SY-SUBRC EQ 0 AND WA_CONVERS-NEWVALUE EQ 'Y'.

    PERFORM SERVER_FILE_DOWNLOAD.

  ENDIF.  "SP INDICATOR CHECKING.

*----------------------------------------------------------------------*
*  -->  p1        FTP FILE DOWNLOAD
*----------------------------------------------------------------------*
  CLEAR : WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                              BANK_CODE = ' '
                              FIELDNAME = ' '"P_BUKRS
                              OLDVALUE  = 'FTP_IND'.


  IF SY-SUBRC EQ 0 AND WA_CONVERS-NEWVALUE EQ 'Y'.

    PERFORM FTP_DOWNLOAD.

  ENDIF.

*----------------------------------------------------------------------*
*  -->     FILE DOWNLOAD IN SHARED FOLDER FOR SCHEDULOR BASED
*----------------------------------------------------------------------*

  CLEAR : WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                              BANK_CODE = ' '
                              FIELDNAME = ' '"P_BUKRS
                              OLDVALUE  = 'SHARED_PATH_IND'.

  IF SY-SUBRC EQ 0 AND WA_CONVERS-NEWVALUE EQ 'Y'.

    PERFORM SHARED_FILE_DOWNLOAD.

  ENDIF.


*----------------------------------------------------------------------*
*  -->  WRITE A LOG FILE IN LOCAL MACHINE.
*----------------------------------------------------------------------*
  IF GIT_LOGFILE[] IS NOT INITIAL.

    PERFORM LOCAL_LOG_FILE_DOWNLOAD.

  ENDIF.    "GIT_LOGFILE[] IS INITIAL CHECKING.
*----------------------------------------------------------------------*
*  -->  UPDATE LOG TABLE AFTER SUCCESSFULL DOWNLOADING OF FILE
*----------------------------------------------------------------------*

  IF DOWNLOAD_ERROR NE ABAP_TRUE. "'X'.

    LOOP AT  GIT_LOG_TABLE ASSIGNING <FS_LOG> .
      <FS_LOG>-FILENAME         = GV_FILENAME.
      <FS_LOG>-DOWNLOAD_STATUS  = GV_TEXT1.
      <FS_LOG>-LOG_STATUS       = GV_TEXT2.

    ENDLOOP.

    LOOP AT  GIT_DISPLAY ASSIGNING <FS_DISPLAY> .
      <FS_DISPLAY>-FILENAME = GV_FILENAME.
    ENDLOOP.

    MESSAGE TEXT-527 TYPE 'S'. " 'FILE DOWNLODED SUCCESSFULLY PLEASE GO BACK'
  ENDIF.

  FREE MEMORY ID 'PAT'.
  L_PATH1 = GV_FILENAME.
  CONDENSE L_PATH1 NO-GAPS.
  EXPORT L_PATH1 FROM L_PATH1 TO MEMORY ID 'PAT'.


ENDFORM.                    " download_and_updatelog


*&---------------------------------------------------------------------*
*&      Form  FTP_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FTP_DOWNLOAD .


  DATA :  USERNAME(30)        TYPE C ,
          PASSWORD(30)        TYPE C ,
          IP(64)              TYPE C ,
          WA_CMD(120)         TYPE C,
          WA_KEY              TYPE I VALUE 26101957,
          WA_HDL              TYPE I,
          WA_SLEN             TYPE I,
          LV_DATE             TYPE SY-DATUM,
          C_DEST              TYPE RFCDES-RFCDEST VALUE 'SAPFTP',
          FTP_FILENAME(100)   TYPE C,
          FTP_PATH(100)       TYPE C,
          FTP_TEXT            TYPE CHAR255.

  TYPES: BEGIN OF TEXT,
         LINE(120) TYPE C,
         END OF TEXT.

  DATA: G_RESULT TYPE TABLE OF TEXT." WITH HEADER LINE.

  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FTP'
                                                  OLDVALUE  = 'IP'
                                                  BANK_CODE = ' '.
  IP = WA_CONVERS-NEWVALUE.

  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FTP'
                                                  OLDVALUE  = 'USERNAME'
                                                  BANK_CODE = ' '.
  USERNAME = WA_CONVERS-NEWVALUE.


  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FTP'
                                                  OLDVALUE  = 'PASSWORD'
                                                  BANK_CODE = ' '.
  PASSWORD = WA_CONVERS-NEWVALUE.

  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY FIELDNAME = 'FTP'
                                                  OLDVALUE  = 'FTPPATH'
                                                  BANK_CODE = ' '.
  FTP_PATH = WA_CONVERS-NEWVALUE.



  WA_SLEN = STRLEN( PASSWORD ).

  CALL FUNCTION 'HTTP_SCRAMBLE'
    EXPORTING
      SOURCE      = PASSWORD
      SOURCELEN   = WA_SLEN
      KEY         = WA_KEY
    IMPORTING
      DESTINATION = PASSWORD.

  CALL FUNCTION 'FTP_CONNECT'
    EXPORTING
      USER                   = USERNAME
      PASSWORD               = PASSWORD
*       ACCOUNT                =
      HOST                   = IP
      RFC_DESTINATION        = C_DEST
   IMPORTING
     HANDLE                 = WA_HDL
   EXCEPTIONS
     NOT_CONNECTED          = 1
     OTHERS                 = 2
            .
  IF SY-SUBRC <> 0.

    DOWNLOAD_ERROR    = ABAP_TRUE. "'X'.
    FTP_DOWNLOAD_ERROR = ABAP_TRUE. "'X'.
    PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-512. " 'FTP SERVER CONNECTION ERROR'.

    CONCATENATE TEXT-513 IP TEXT-514 INTO FTP_TEXT SEPARATED BY SPACE.  " FTP Connection to,'Not Established'
    PERFORM WRITE_LOG USING FTP_TEXT.
    PERFORM POP_UP_ERROR USING TEXT-E33.  " FTP ERROR : A Connection with the FTP server could not be established

  ELSE.

    CONCATENATE TEXT-513 IP TEXT-515 INTO FTP_TEXT SEPARATED BY SPACE.
    PERFORM WRITE_LOG USING FTP_TEXT.

  ENDIF.

* Create file on FTP server

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = TEXT-516. " 'Create file on FTP Server'.



*Change directory to the FTP directory
  CONCATENATE 'cd' FTP_PATH INTO WA_CMD SEPARATED BY SPACE.


  CALL FUNCTION 'FTP_COMMAND'
    EXPORTING
      HANDLE        = WA_HDL
      COMMAND       = WA_CMD
    TABLES
      DATA          = G_RESULT
    EXCEPTIONS
      TCPIP_ERROR   = 1
      COMMAND_ERROR = 2
      DATA_ERROR    = 3
      OTHERS        = 4.
  IF SY-SUBRC <> 0.
    CONCATENATE TEXT-517 WA_CMD TEXT-518 INTO  " FTP Command'', 'NOT EXECUITED'
                              FTP_TEXT SEPARATED BY SPACE.
    MESSAGE  FTP_TEXT  TYPE 'S'.
    PERFORM WRITE_LOG USING FTP_TEXT.
  ELSE.
    CONCATENATE TEXT-517 WA_CMD TEXT-519 INTO FTP_TEXT SEPARATED BY SPACE. " FTP Command',EXECUITED
    PERFORM WRITE_LOG USING FTP_TEXT.
  ENDIF.


*Change TO ASCII MODE.

  CALL FUNCTION 'FTP_COMMAND'
    EXPORTING
      HANDLE        = WA_HDL
      COMMAND       = 'ascii'
    TABLES
      DATA          = G_RESULT
    EXCEPTIONS
      TCPIP_ERROR   = 1
      COMMAND_ERROR = 2
      DATA_ERROR    = 3.


  FTP_FILENAME = GV_FILENAME.
  CALL FUNCTION 'FTP_R3_TO_SERVER'
    EXPORTING
      HANDLE               = WA_HDL
      FNAME                = FTP_FILENAME
*   BLOB_LENGTH           =
     CHARACTER_MODE       = 'X'
   TABLES
*   BLOB                  =
     TEXT                 = G_IT_OUTPUT[]   " Payment File data table.
   EXCEPTIONS
     TCPIP_ERROR          = 1
     COMMAND_ERROR        = 2
     DATA_ERROR           = 3
     OTHERS               = 4
            .

  CASE SY-SUBRC.

    WHEN '0'.
      PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-520. "'DATA DOWNLOADED FTP SERVER'.
      """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
      CONCATENATE TEXT-M05 ' :-->' FTP_PATH FTP_FILENAME INTO FTP_TEXT SEPARATED BY SPACE..
      PERFORM WRITE_LOG USING FTP_TEXT.

    WHEN OTHERS.
      DOWNLOAD_ERROR    = ABAP_TRUE. "'X'.
      FTP_DOWNLOAD_ERROR = ABAP_TRUE. "'X'.
      PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-521. "'FTP SERVER DATA WRITING ERROR'.

      """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
      CONCATENATE TEXT-E35 ' :-->' FTP_PATH FTP_FILENAME INTO FTP_TEXT SEPARATED BY SPACE..
      PERFORM WRITE_LOG USING FTP_TEXT.

      PERFORM POP_UP_ERROR USING TEXT-E35. "FTP SERVER DATA WRITING ERROR due to TCPIP_ERROR/COMMAND_ERROR/DATA_ERROR
  ENDCASE.


  CALL FUNCTION 'FTP_DISCONNECT'
    EXPORTING
      HANDLE = WA_HDL.

  CALL FUNCTION 'RFC_CONNECTION_CLOSE'
    EXPORTING
      DESTINATION = C_DEST
    EXCEPTIONS
      OTHERS      = 1.


ENDFORM.                    " FTP_DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  LOCAL_FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOCAL_FILE_DOWNLOAD .
  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                                FIELDNAME = ' '
                                BANK_CODE = ' '
                                OLDVALUE  = 'LP'.
  IF SY-SUBRC EQ 0 AND WA_CONVERS-NEWVALUE IS NOT INITIAL.

    CONCATENATE WA_CONVERS-NEWVALUE 'IMPORT\'     INTO LP_FILEPATH.
    CONCATENATE WA_CONVERS-NEWVALUE 'PROCESSED\'  INTO L_FILEPATH_PROCESSED.
    CONCATENATE LP_FILEPATH GV_FILENAME INTO LP_FILE.


*----------------------------------------------------------------------*
*  check wheter processed folder present or not if not create it.
*----------------------------------------------------------------------*

    DIR = L_FILEPATH_PROCESSED.
    DATA: RC TYPE C.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST   " checking folder
      EXPORTING
        DIRECTORY            = DIR
      RECEIVING
        RESULT               = RC
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        WRONG_PARAMETER      = 3
        NOT_SUPPORTED_BY_GUI = 4
        OTHERS               = 5.

    IF RC NE ABAP_TRUE. "'X'.

      CALL FUNCTION 'TMP_GUI_CREATE_DIRECTORY'         " creating folder
        EXPORTING
          DIRNAME  = L_FILEPATH_PROCESSED
          NO_FLUSH = ' '
        EXCEPTIONS
          FAILED   = 1
          OTHERS   = 2.                                     "#EC *

    ENDIF.

***** FM: FOR READING FILE COUNT FROM IMPORT FOLDER
*
    L_FILEPATH_IMPORT = LP_FILEPATH.

    CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
      EXPORTING
        DIRECTORY  = L_FILEPATH_IMPORT
        FILTER     = '*.*'
      IMPORTING
        FILE_COUNT = L_FILE_COUNT
        DIR_COUNT  = L_DIR_COUNT
      TABLES
        FILE_TABLE = L_TAB_SDOKPATH
        DIR_TABLE  = L_TAB1_SDOKPATH
      EXCEPTIONS
        CNTL_ERROR = 1
        OTHERS     = 2.

    IF SY-SUBRC = 0.

      LOOP AT L_TAB_SDOKPATH INTO L_WA_TAB_SDOKPATH.

        CONCATENATE L_FILEPATH_PROCESSED L_WA_TAB_SDOKPATH-PATHNAME
                                                      INTO L_DESTINATION.
        CONCATENATE L_FILEPATH_IMPORT  L_WA_TAB_SDOKPATH-PATHNAME
                                                   INTO L_SOURCE.

* FM: FOR FILE COPY FROM IMPORT FOLDER TO PROCESSED FOLDER

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
          EXPORTING
            SOURCE               = L_SOURCE
            DESTINATION          = L_DESTINATION
            OVERWRITE            = ABAP_TRUE  "'X'
          EXCEPTIONS
            CNTL_ERROR           = 1
            ERROR_NO_GUI         = 2
            WRONG_PARAMETER      = 3
            DISK_FULL            = 4
            ACCESS_DENIED        = 5
            FILE_NOT_FOUND       = 6
            DESTINATION_EXISTS   = 7
            UNKNOWN_ERROR        = 8
            PATH_NOT_FOUND       = 9
            DISK_WRITE_PROTECT   = 10
            DRIVE_NOT_READY      = 11
            NOT_SUPPORTED_BY_GUI = 12
            OTHERS               = 13.
        IF SY-SUBRC = 0.
          """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
          CONCATENATE 'OLD FILE' L_SOURCE TEXT-530          "WAS COPIED TO PROCESSED FOLDER
                      INTO WA_LOGFILE-TEXT
                      SEPARATED BY SPACE.
          APPEND WA_LOGFILE TO GIT_LOGFILE.
          CLEAR WA_LOGFILE.
          """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
        ENDIF.

* FM: FOR FILE DELETE FROM IMPORT FOLDER

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
          EXPORTING
            FILENAME             = L_SOURCE
          CHANGING
            RC                   = L_RETURN
          EXCEPTIONS
            FILE_DELETE_FAILED   = 1
            CNTL_ERROR           = 2
            ERROR_NO_GUI         = 3
            FILE_NOT_FOUND       = 4
            ACCESS_DENIED        = 5
            UNKNOWN_ERROR        = 6
            NOT_SUPPORTED_BY_GUI = 7
            WRONG_PARAMETER      = 8
            OTHERS               = 9.
        IF L_RETURN = 0.
          """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
          CONCATENATE 'OLD FILE' L_SOURCE TEXT-531       "'WAS DELETED FROM IMPORT FOLDER'
                      INTO WA_LOGFILE-TEXT
                      SEPARATED BY SPACE.
          APPEND WA_LOGFILE TO GIT_LOGFILE.
          CLEAR WA_LOGFILE.
          """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
        ENDIF.
      ENDLOOP.

* FM: FOR READING FILE COUNT FROM IMPORT FOLDER

      CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
        EXPORTING
          DIRECTORY  = L_FILEPATH_IMPORT
          FILTER     = '*.*'
        IMPORTING
          FILE_COUNT = L_FILE_COUNT
          DIR_COUNT  = L_DIR_COUNT
        TABLES
          FILE_TABLE = L_TAB_SDOKPATH
          DIR_TABLE  = L_TAB1_SDOKPATH
        EXCEPTIONS
          CNTL_ERROR = 1
          OTHERS     = 2.

      IF SY-SUBRC  NE 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ELSE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME                  = LP_FILE
        FILETYPE                  = 'ASC'
        HEADER                    = '00'
        WRITE_LF                  = ABAP_TRUE "'X'
        IGNORE_CERR               = ABAP_TRUE "'X'
        REPLACEMENT               = '#'
        TRUNC_TRAILING_BLANKS_EOL = ABAP_TRUE "'X'
      TABLES
        DATA_TAB                  = G_IT_OUTPUT[]
      EXCEPTIONS
        FILE_WRITE_ERROR          = 1
        NO_BATCH                  = 2
        GUI_REFUSE_FILETRANSFER   = 3
        INVALID_TYPE              = 4
        NO_AUTHORITY              = 5
        UNKNOWN_ERROR             = 6
        HEADER_NOT_ALLOWED        = 7
        SEPARATOR_NOT_ALLOWED     = 8
        FILESIZE_NOT_ALLOWED      = 9
        HEADER_TOO_LONG           = 10
        DP_ERROR_CREATE           = 11
        DP_ERROR_SEND             = 12
        DP_ERROR_WRITE            = 13
        UNKNOWN_DP_ERROR          = 14
        ACCESS_DENIED             = 15
        DP_OUT_OF_MEMORY          = 16
        DISK_FULL                 = 17
        DP_TIMEOUT                = 18
        FILE_NOT_FOUND            = 19
        DATAPROVIDER_EXCEPTION    = 20
        CONTROL_FLUSH_ERROR       = 21
        OTHERS                    = 22.
    IF SY-SUBRC <> 0.
*        DOWNLOAD_ERROR    = 'X'.
      LP_DOWNLOAD_ERROR = ABAP_TRUE. "'X'.
      PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-505.      "'ERROR IN LOCAL PATH DOWNLOAD'

    ELSE.
      PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-506.        "'DATA DOWNLOADED IN LOCAL PATH'.

      """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
      CONCATENATE TEXT-M03 LP_FILE INTO WA_LOGFILE-TEXT SEPARATED BY SPACE.
      APPEND WA_LOGFILE TO GIT_LOGFILE.
      CLEAR WA_LOGFILE.
      """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
    ENDIF.

  ELSE.
    PERFORM POP_UP_ERROR USING TEXT-115.          "please maintain the local path in the convertion table
*      DOWNLOAD_ERROR    = 'X'.
    LP_DOWNLOAD_ERROR = ABAP_TRUE. "'X'.
    PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-507.  " 'LOCAL PATH NOT MAINTAINED'.
  ENDIF.   "LP PATH CHECKING.

ENDFORM.                    " LOCAL_FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  SERVER_FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SERVER_FILE_DOWNLOAD .

  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                                FIELDNAME = ' '
                                BANK_CODE = ' '
                                OLDVALUE  = 'SP'.

  IF SY-SUBRC EQ 0 AND WA_CONVERS-NEWVALUE IS NOT INITIAL.

    SP_FILEPATH = WA_CONVERS-NEWVALUE.
    CONCATENATE SP_FILEPATH GV_FILENAME INTO SP_FILE.


    OPEN DATASET  SP_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT."+SISL1

    IF SY-SUBRC <> 0.
*        WRITE:/ 'CANNOT OPEN OUTPUT FILE', SP_FILE.
      CLEAR : GV_TEXT.
      CONCATENATE TEXT-508 SP_FILE INTO GV_TEXT SEPARATED BY ':-->'.  " 'CANNOT OPEN OUTPUT FILE'
      DOWNLOAD_ERROR    = ABAP_TRUE. " 'X'.
      SP_DOWNLOAD_ERROR = ABAP_TRUE. "'X'.
      PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-509.         " 'ERROR WHILE DOWNLOAD THE DATA IN APPLICATION SERVER PATH'
      PERFORM POP_UP_ERROR USING GV_TEXT.
      EXIT.

    ELSE.

      LOOP AT G_IT_OUTPUT[] INTO WA_OUTPUT .
        TRANSFER WA_OUTPUT TO SP_FILE.
      ENDLOOP .
      PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-510.  "'DATA DOWNLOADED IN APPLICATION SERVER PATH'.

      """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
      CONCATENATE TEXT-M02 SP_FILE INTO WA_LOGFILE-TEXT SEPARATED BY SPACE.
      APPEND WA_LOGFILE TO GIT_LOGFILE.
      CLEAR WA_LOGFILE.
      """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""

      CLOSE DATASET SP_FILE.
    ENDIF.
  ELSE.
    PERFORM POP_UP_ERROR USING TEXT-116.          "please maintain the server path in the convertion table

    DOWNLOAD_ERROR = ABAP_TRUE. "'X'.
    SP_DOWNLOAD_ERROR = ABAP_TRUE. "'X'.
    PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-511. " 'APPLICATION SERVER PATH NOT MAINTAINED'.

  ENDIF.   "SP PATH CHECKING.
ENDFORM.                    " SERVER_FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  SHARED_FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHARED_FILE_DOWNLOAD .

  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                                FIELDNAME = ' '
                                BANK_CODE = ' '
                                OLDVALUE  = 'SHARED_PATH'.
  IF SY-SUBRC EQ 0 AND WA_CONVERS-NEWVALUE IS NOT INITIAL.

    CONCATENATE WA_CONVERS-NEWVALUE GV_FILENAME INTO SHARED_FILE.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME                  = SHARED_FILE
        FILETYPE                  = 'ASC'
        HEADER                    = '00'
        WRITE_LF                  = ABAP_TRUE "'X'
        IGNORE_CERR               = ABAP_TRUE " 'X'
        REPLACEMENT               = '#'
        TRUNC_TRAILING_BLANKS_EOL = ABAP_TRUE "'X'
      TABLES
        DATA_TAB                  = G_IT_OUTPUT[]
      EXCEPTIONS
        FILE_WRITE_ERROR          = 1
        NO_BATCH                  = 2
        GUI_REFUSE_FILETRANSFER   = 3
        INVALID_TYPE              = 4
        NO_AUTHORITY              = 5
        UNKNOWN_ERROR             = 6
        HEADER_NOT_ALLOWED        = 7
        SEPARATOR_NOT_ALLOWED     = 8
        FILESIZE_NOT_ALLOWED      = 9
        HEADER_TOO_LONG           = 10
        DP_ERROR_CREATE           = 11
        DP_ERROR_SEND             = 12
        DP_ERROR_WRITE            = 13
        UNKNOWN_DP_ERROR          = 14
        ACCESS_DENIED             = 15
        DP_OUT_OF_MEMORY          = 16
        DISK_FULL                 = 17
        DP_TIMEOUT                = 18
        FILE_NOT_FOUND            = 19
        DATAPROVIDER_EXCEPTION    = 20
        CONTROL_FLUSH_ERROR       = 21
        OTHERS                    = 22.
    IF SY-SUBRC <> 0.    " IF ANY ERROR IN DOWNLOAD.
      DOWNLOAD_ERROR    = ABAP_TRUE. "'X'.
      SHARED_DOWNLOAD_ERROR = ABAP_TRUE. "'X'.
      PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-522. "'ERROR IN SHARED PATH DOWNLOAD'.
      PERFORM POP_UP_ERROR USING TEXT-E34. "'Error while downloading file into Shared Path
    ELSE.      " SUCCESS FULLY DOWNLOADED THEN UPDATE THE STATUS

      PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-523. "'DATA DOWNLOADED IN SHARED PATH'.
      """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
      CONCATENATE TEXT-M04 SHARED_FILE INTO WA_LOGFILE-TEXT SEPARATED BY SPACE.
      APPEND WA_LOGFILE TO GIT_LOGFILE.
      CLEAR WA_LOGFILE.
      """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
    ENDIF.
  ELSE.    " SHARED PATH WAS NOT THERE THEN TROUGH ERROR.
    DOWNLOAD_ERROR    = ABAP_TRUE. "'X'.
    SHARED_DOWNLOAD_ERROR = ABAP_TRUE. "'X'.
    PERFORM APPEND_DOWNLOAD_STATUS USING TEXT-E32 ."'Shared Path was not maintained to download the payment file

    PERFORM POP_UP_ERROR USING TEXT-E32. "'Shared Path was not maintained to download the payment file
  ENDIF.

ENDFORM.                    " SHARED_FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  LOCAL_LOG_FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOCAL_LOG_FILE_DOWNLOAD .

  CLEAR WA_CONVERS.
  READ TABLE GIT_CONVERS INTO WA_CONVERS WITH KEY
                                FIELDNAME = ' '
                                BANK_CODE = ' '
                                OLDVALUE  = 'LOG'.
  IF SY-SUBRC EQ 0 AND WA_CONVERS-NEWVALUE IS NOT INITIAL.
    CONCATENATE GV_FILENAME '_log' '.txt' INTO GV_LOG_FILENAME.
    CONCATENATE WA_CONVERS-NEWVALUE GV_LOG_FILENAME INTO LP_LOG_FILE.


    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME                  = LP_LOG_FILE
        FILETYPE                  = 'ASC'
        HEADER                    = '00'
        WRITE_LF                  = ABAP_TRUE "'X'
        IGNORE_CERR               = ABAP_TRUE "'X'
        REPLACEMENT               = '#'
        TRUNC_TRAILING_BLANKS_EOL = ABAP_TRUE "'X'
      TABLES
        DATA_TAB                  = GIT_LOGFILE[]
      EXCEPTIONS
        FILE_WRITE_ERROR          = 1
        NO_BATCH                  = 2
        GUI_REFUSE_FILETRANSFER   = 3
        INVALID_TYPE              = 4
        NO_AUTHORITY              = 5
        UNKNOWN_ERROR             = 6
        HEADER_NOT_ALLOWED        = 7
        SEPARATOR_NOT_ALLOWED     = 8
        FILESIZE_NOT_ALLOWED      = 9
        HEADER_TOO_LONG           = 10
        DP_ERROR_CREATE           = 11
        DP_ERROR_SEND             = 12
        DP_ERROR_WRITE            = 13
        UNKNOWN_DP_ERROR          = 14
        ACCESS_DENIED             = 15
        DP_OUT_OF_MEMORY          = 16
        DISK_FULL                 = 17
        DP_TIMEOUT                = 18
        FILE_NOT_FOUND            = 19
        DATAPROVIDER_EXCEPTION    = 20
        CONTROL_FLUSH_ERROR       = 21
        OTHERS                    = 22.

    IF SY-SUBRC <> 0.

      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

    ELSE.
      CONCATENATE TEXT-524 LP_LOG_FILE INTO GV_TEXT2 SEPARATED BY ':-->'. " 'LOG WRITEN SUCCESSFULLY IN'
    ENDIF.

  ELSE.       " IF LOG PATH WAS EMPTY IN CONVERTION TABLE.
    GV_TEXT2 = TEXT-525. "'PLEASE MAINTAIN LOG PATH IN CONVERTION TABLE'.
  ENDIF.   "log PATH CHECKING.

ENDFORM.                    " LOCAL_LOG_FILE_DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  APPEND_DOWNLOAD_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0424   text
*----------------------------------------------------------------------*
FORM APPEND_DOWNLOAD_STATUS  USING    VALUE(P_0424).


  IF GV_TEXT1 IS NOT INITIAL.
    CONCATENATE GV_TEXT1 '&' P_0424 INTO GV_TEXT1 SEPARATED BY SPACE.
  ELSE.
    GV_TEXT1 = P_0424.
  ENDIF.

ENDFORM.                    " APPEND_DOWNLOAD_STATUS
*&---------------------------------------------------------------------*
*&      Form  WRITE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FTP_TEXT  text
*----------------------------------------------------------------------*
FORM WRITE_LOG  USING    P_FTP_TEXT.

  """---------TO WRITE THE LOG FILE IN LOCAL MACHINE----------------""
  WA_LOGFILE-TEXT = P_FTP_TEXT.
  APPEND WA_LOGFILE TO GIT_LOGFILE.
  CLEAR WA_LOGFILE.

ENDFORM.                    " WRITE_LOG
