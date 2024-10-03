*&---------------------------------------------------------------------*
*&  Include           ZAXIS_PAY_EXTR_MAIN
*&---------------------------------------------------------------------*

INITIALIZATION.

  CASE SY-TCODE.

    WHEN  C_TCODE_EXTR.
      SY-TITLE = TEXT-500. "'Axis Bank Vendor Payments Extraction' .

    WHEN  C_TCODE_REXTR.
      SY-TITLE = TEXT-501. "'Axis Bank Vendor Payments Re-Extraction' .

    WHEN OTHERS.
      PERFORM POP_UP_ERROR USING TEXT-131.   "This Tcode was Not allowed Kindly use Extraction or Re-Extraction Tcode

  ENDCASE.


START-OF-SELECTION.
  IF P_AUTO = ABAP_TRUE.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM CLARING_DATA.               " for clearing global variable and global tables

  PERFORM CHECK_MANDATORY_INPUTS.     " checking selction screen data

  PERFORM FETCH_LOG_DETAILS.          "* PAYMENT VALIDATION (EXTRACTION & RE-EXTRACTION)

  PERFORM FETCH_CONFIGURATIONS.       " getting required configuration tables.

  PERFORM USER_VALIDATION.            " USER validation to allow the extraction

  PERFORM SELECT_DATA.                " PAYMENT DETAILS (EXTRACTION & RE-EXTRACTION)


*&---------------------------------------------------------------------------*
*&     if data Found with provided input then construct file else give error
*&---------------------------------------------------------------------------*

  IF G_TAB_BSAK_PYMNT[] IS NOT INITIAL.

    PERFORM SELECT_TRANSACTIONS.

    PERFORM EXTRACT_DATA.                    "* PAYMENT EXTRACTION & RE-EXTRACTION

  ELSE.
    IF GIT_DISPLAY[] IS NOT INITIAL.
      PERFORM DISPLAY_ALV.                  " error display
      EXIT.
    ELSE.
      PERFORM POP_UP_ERROR USING TEXT-114.  " Document does not exist or input details might be wroung please check the details..
    ENDIF.
  ENDIF.


*&-----------------------------------------------------------------------*
*&     if no errors and no test run then download the file other wise
*&            show the display with error and success data
*&-----------------------------------------------------------------------*

  IF P_TEST = ABAP_FALSE AND GIT_EXCEPTION[] IS INITIAL  " abap_false = ' '
                  AND G_IT_OUTPUT[]   IS NOT INITIAL.

    PERFORM DOWNLOAD_AND_UPDATELOG.   " DOWNLOAD THE FILE AND LOG UPDATION AND CREATING LOG FILE

    PERFORM DISPLAY_ALV.              " TO DISPLAY THE ALV

    PERFORM AUTO_PROCESS.             " to push the files to axis bank ftp server

    IF SP_DOWNLOAD_ERROR      NE ABAP_TRUE AND FTP_DOWNLOAD_ERROR NE ABAP_TRUE AND
       SHARED_DOWNLOAD_ERROR  NE ABAP_TRUE.   " abap_true = 'X'

      MODIFY ZAXIS_TAB_CTLTA1 FROM TABLE GIT_LOG_TABLE.  " if file push to bank server then update the log data bsae.
    ELSE.
      PERFORM POP_UP_ERROR USING TEXT-130. "'Error While Downloading The File So Kindly Check The File Download Path & User Permissions'.
    ENDIF.

  ELSE.
    PERFORM DISPLAY_ALV.              " TEST RUN DISPLAY
  ENDIF.
