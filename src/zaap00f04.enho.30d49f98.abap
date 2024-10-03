"Name: \PR:RQCAAP01\FO:USER_VARIANT\SE:BEGIN\EI
ENHANCEMENT 0 ZAAP00F04.
*
  DATA: OPTION TYPE ITCPO.

OPTION-TDGETOTF = 'X'.

 IF SY-TCODE = 'QA33' or SY-TCODE = 'QC21'.



    CALL FUNCTION 'OPEN_FORM'
     EXPORTING
*       APPLICATION                       = 'TX'
*       ARCHIVE_INDEX                     =
*       ARCHIVE_PARAMS                    =
       DEVICE                            = 'PRINTER'
       DIALOG                            = SPACE
       FORM                              = 'ZQM_QCERT_01'
       LANGUAGE                          = SY-LANGU
        OPTIONS                           = OPTION
*       MAIL_SENDER                       =
*       MAIL_RECIPIENT                    =
*       MAIL_APPL_OBJECT                  =
*       RAW_DATA_INTERFACE                = '*'
*       SPONUMIV                          =
*     IMPORTING
*       LANGUAGE                          =
*       NEW_ARCHIVE_PARAMS                =
*       RESULT                            =
*     EXCEPTIONS
*       CANCELED                          = 1
*       DEVICE                            = 2
*       FORM                              = 3
*       OPTIONS                           = 4
*       UNCLOSED                          = 5
*       MAIL_OPTIONS                      = 6
*       ARCHIVE_ERROR                     = 7
*       INVALID_FAX_NUMBER                = 8
*       MORE_PARAMS_NEEDED_IN_BATCH       = 9
*       SPOOL_ERROR                       = 10
*       CODEPAGE                          = 11
*       OTHERS                            = 12
              .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

*   CALL FUNCTION 'START_FORM'
*    EXPORTING
**      ARCHIVE_INDEX          =
*       FORM                   = 'ZQM_QCERT_01'
**       LANGUAGE               = ''
**      STARTPAGE              = ' '
**      PROGRAM                = ' '
**      MAIL_APPL_OBJECT       =
**    IMPORTING
**      LANGUAGE               =
**    EXCEPTIONS
**      FORM                   = 1
**      FORMAT                 = 2
**      UNENDED                = 3
**      UNOPENED               = 4
**      UNUSED                 = 5
**      SPOOL_ERROR            = 6
**      CODEPAGE               = 7
**      OTHERS                 = 8
*             .
*   IF SY-SUBRC <> 0.
** Implement suitable error handling here
*   ENDIF.





SELECT * from qals INTO qals WHERE PRUEFLOS = p_lot.
 ENDSELECT.

MARA-MATNR = QALS-MATNR.

VBDPL-CHARG = QALS-CHARG.

SELECT * FROM MAKT INTO MAKT WHERE MATNR = MARA-MATNR.
ENDSELECT.



  MOVE qals to ls_qals.

   PERFORM INITIALIZE_AFTER_PROCESSING.
*
*   CALL FUNCTION 'END_FORM'
**    IMPORTING
**      RESULT                         =
**    EXCEPTIONS
**      UNOPENED                       = 1
**      BAD_PAGEFORMAT_FOR_PRINT       = 2
**      SPOOL_ERROR                    = 3
**      CODEPAGE                       = 4
**      OTHERS                         = 5
*             .
*   IF SY-SUBRC <> 0.
** Implement suitable error handling here
*   ENDIF.

DATA: IT_OTF TYPE TABLE OF ITCOO,
      BIN_FILESIZE TYPE I,
      PDFTAB TYPE TABLE OF TLINE,
      FILE_NAME TYPE STRING,
      FILE_PATH TYPE STRING,
      FULL_PATH TYPE STRING,
      RESULT TYPE ITCPP.



   CALL FUNCTION 'CLOSE_FORM'

    IMPORTING
      RESULT                         = RESULT
*      RDI_RESULT                     =
     TABLES
       OTFDATA                        = IT_OTF
*    EXCEPTIONS
*      UNOPENED                       = 1
*      BAD_PAGEFORMAT_FOR_PRINT       = 2
*      SEND_ERROR                     = 3
*      SPOOL_ERROR                    = 4
*      CODEPAGE                       = 5
*      OTHERS                         = 6
             .
   IF SY-SUBRC <> 0.
* Implement suitable error handling here
   ENDIF.

*   SET PARAMETER ID '' FIELD WA_RESULT-TDSPOOLID .
*   CALL TRANSACTION 'ZSPOOL_PDF'.

CALL FUNCTION 'CONVERT_OTF'
 EXPORTING
   FORMAT                      = 'PDF'
*   MAX_LINEWIDTH               = 132
*   ARCHIVE_INDEX               = ' '
*   COPYNUMBER                  = 0
*   ASCII_BIDI_VIS2LOG          = ' '
*   PDF_DELETE_OTFTAB           = ' '
*   PDF_USERNAME                = ' '
*   PDF_PREVIEW                 = ' '
*   USE_CASCADING               = ' '
*   MODIFIED_PARAM_TABLE        =
 IMPORTING
   BIN_FILESIZE                =  BIN_FILESIZE
*   BIN_FILE                    =
  TABLES
    OTF                         = IT_OTF
    LINES                       = PDFTAB
* EXCEPTIONS
*   ERR_MAX_LINEWIDTH           = 1
*   ERR_FORMAT                  = 2
*   ERR_CONV_NOT_POSSIBLE       = 3
*   ERR_BAD_OTF                 = 4
*   OTHERS                      = 5
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.


CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
*  EXPORTING
*    WINDOW_TITLE              =
*    DEFAULT_EXTENSION         =
*    DEFAULT_FILE_NAME         =
*    WITH_ENCODING             =
*    FILE_FILTER               =
*    INITIAL_DIRECTORY         =
*    PROMPT_ON_OVERWRITE       = 'X'
  CHANGING
    FILENAME                  =  FILE_NAME
    PATH                      =  FILE_PATH
    FULLPATH                  =  FULL_PATH
*    USER_ACTION               =
*    FILE_ENCODING             =
*  EXCEPTIONS
*    CNTL_ERROR                = 1
*    ERROR_NO_GUI              = 2
*    NOT_SUPPORTED_BY_GUI      = 3
*    INVALID_DEFAULT_FILE_NAME = 4
*    OTHERS                    = 5
        .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.


if full_path is INITIAL.

 MESSAGE 'Invalid' TYPE 'E'.

endif.

DATA: len TYPE i,
      substr TYPE string.

len = STRLEN( FULL_PATH ) - 4.

if len > 4.
substr = FULL_PATH+len(4).
endif.
IF substr ne '.PDF' and  substr ne '.pdf' .

CONCATENATE FULL_PATH '.PDF' INTO FULL_PATH.

ENDIF.


CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
   BIN_FILESIZE                    = BIN_FILESIZE
    FILENAME                        = FULL_PATH                 "'D:\myfile.pdf'
   FILETYPE                        = 'BIN'
*   APPEND                          = ' '
*   WRITE_FIELD_SEPARATOR           = ' '
*   HEADER                          = '00'
*   TRUNC_TRAILING_BLANKS           = ' '
*   WRITE_LF                        = 'X'
*   COL_SELECT                      = ' '
*   COL_SELECT_MASK                 = ' '
*   DAT_MODE                        = ' '
*   CONFIRM_OVERWRITE               = ' '
*   NO_AUTH_CHECK                   = ' '
*   CODEPAGE                        = ' '
*   IGNORE_CERR                     = ABAP_TRUE
*   REPLACEMENT                     = '#'
*   WRITE_BOM                       = ' '
*   TRUNC_TRAILING_BLANKS_EOL       = 'X'
*   WK1_N_FORMAT                    = ' '
*   WK1_N_SIZE                      = ' '
*   WK1_T_FORMAT                    = ' '
*   WK1_T_SIZE                      = ' '
*   WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*   SHOW_TRANSFER_STATUS            = ABAP_TRUE
*   VIRUS_SCAN_PROFILE              = '/SCET/GUI_DOWNLOAD'
* IMPORTING
*   FILELENGTH                      =
  TABLES
    DATA_TAB                        = PDFTAB
*   FIELDNAMES                      =
* EXCEPTIONS
*   FILE_WRITE_ERROR                = 1
*   NO_BATCH                        = 2
*   GUI_REFUSE_FILETRANSFER         = 3
*   INVALID_TYPE                    = 4
*   NO_AUTHORITY                    = 5
*   UNKNOWN_ERROR                   = 6
*   HEADER_NOT_ALLOWED              = 7
*   SEPARATOR_NOT_ALLOWED           = 8
*   FILESIZE_NOT_ALLOWED            = 9
*   HEADER_TOO_LONG                 = 10
*   DP_ERROR_CREATE                 = 11
*   DP_ERROR_SEND                   = 12
*   DP_ERROR_WRITE                  = 13
*   UNKNOWN_DP_ERROR                = 14
*   ACCESS_DENIED                   = 15
*   DP_OUT_OF_MEMORY                = 16
*   DISK_FULL                       = 17
*   DP_TIMEOUT                      = 18
*   FILE_NOT_FOUND                  = 19
*   DATAPROVIDER_EXCEPTION          = 20
*   CONTROL_FLUSH_ERROR             = 21
*   OTHERS                          = 22
          .
IF SY-SUBRC = 0.


MESSAGE 'DOWNLOADED' TYPE 'S'.

REFRESH gt_final.
clear : gs_final , LV_SNO , LV_SNO1.
EXIT.


ENDIF.






 ENDIF.




ENDENHANCEMENT.
