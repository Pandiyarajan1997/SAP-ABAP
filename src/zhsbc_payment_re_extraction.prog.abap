*&---------------------------------------------------------------------*
*& Report  ZHSBC_PAYMENT_RE_EXTRACTION
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZHSBC_PAYMENT_RE_EXTRACTION.


DATA : WA_SFTP TYPE ZHSBC_SFTP,
       LV_DIR TYPE EPSF-EPSDIRNAM,
       IT_FILE_LIST TYPE TABLE OF EPSFILI,
       WA_FILE_LIST TYPE EPSFILI,
       FILE_COUNT TYPE I,
       TEMP TYPE STRING,
       X.

TYPES: BEGIN OF ty_final,

  file_name TYPE epsfili-NAME,
  Content TYPE string,
  xfile  TYPE xstring,

END OF ty_final.

data : it_final TYPE TABLE OF ty_final,
       wa_final TYPE ty_final,
       it_file TYPE TABLE OF SMUM_XMLTB,
       ret TYPE table of BAPIRET2.

SELECT SINGLE * FROM ZHSBC_SFTP INTO WA_SFTP.

IF WA_SFTP-LOG_FPATH IS INITIAL OR WA_SFTP-LOG_SPATH IS INITIAL OR WA_SFTP-LOG_FPATH IS INITIAL.

  MESSAGE 'Please Maintain Server Connectivity in ZHSBC' TYPE 'E'.

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
* EXCEPTIONS
*   NO_PERMISSION                       = 1
*   COMMAND_NOT_FOUND                   = 2
*   PARAMETERS_TOO_LONG                 = 3
*   SECURITY_RISK                       = 4
*   WRONG_CHECK_CALL_INTERFACE          = 5
*   PROGRAM_START_ERROR                 = 6
*   PROGRAM_TERMINATION_ERROR           = 7
*   X_ERROR                             = 8
*   PARAMETER_EXPECTED                  = 9
*   TOO_MANY_PARAMETERS                 = 10
*   ILLEGAL_COMMAND                     = 11
*   WRONG_ASYNCHRONOUS_PARAMETERS       = 12
*   CANT_ENQ_TBTCO_ENTRY                = 13
*   JOBCOUNT_GENERATION_ERROR           = 14
*   OTHERS                              = 15
          .
IF SY-SUBRC <> 0.

  WRITE : 'Connectivity to HSBC server failed'.

ENDIF.

CLEAR LV_DIR.
LV_DIR = WA_SFTP-LOG_PATH.


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
* EXCEPTIONS
*   INVALID_EPS_SUBDIR           = 1
*   SAPGPARAM_FAILED             = 2
*   BUILD_DIRECTORY_FAILED       = 3
*   NO_AUTHORIZATION             = 4
*   READ_DIRECTORY_FAILED        = 5
*   TOO_MANY_READ_ERRORS         = 6
*   EMPTY_DIRECTORY_LIST         = 7
*   OTHERS                       = 8
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.

IF IT_FILE_LIST IS NOT INITIAL.

  FILE_COUNT = LINES( IT_FILE_LIST ).

  WRITE : / FILE_COUNT , ' Number of Files Retrived From HSBC'.

  WRITE : / 'Files :'.

  LOOP AT IT_FILE_LIST INTO WA_FILE_LIST.

    WRITE : / WA_FILE_LIST-NAME.

    IF X = 1.

       clear wa_final.
       wa_final-FILE_NAME = WA_FILE_LIST-NAME.
       clear temp.
       CONCATENATE WA_SFTP-LOG_PATH WA_FILE_LIST-NAME INTO temp.
       CONDENSE temp.

      OPEN DATASET temp FOR INPUT IN TEXT MODE ENCODING DEFAULT.

      READ DATASET temp INTO wa_final-CONTENT.

      append wa_final to it_final.


      CLOSE DATASET temp.

    ENDIF.

  ENDLOOP.


  LOOP AT it_final INTO wa_final.


    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        TEXT           = wa_final-CONTENT
*       MIMETYPE       = ' '
*       ENCODING       =
     IMPORTING
        BUFFER         = wa_final-xfile
*     EXCEPTIONS
*       FAILED         = 1
*       OTHERS         = 2
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        XML_INPUT       = wa_final-xfile
      TABLES
        XML_TABLE       =  it_file
        RETURN          = ret
              .








  ENDLOOP.

ELSE.

  WRITE : 'Didnt Return log Files yet from HSBC'.

ENDIF.
