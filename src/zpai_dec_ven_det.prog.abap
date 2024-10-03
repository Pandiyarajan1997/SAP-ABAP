

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5009  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_5009 INPUT.


  CASE OK_CODE.

    WHEN  'BACK' .
      LEAVE PROGRAM.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'EXECUTE'.
      CALL SCREEN 5000.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_5009  INPUT


*&---------------------------------------------------------------------*
*&      Module  GET_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA INPUT.



  LOOP AT GT_FINAL INTO WA_FINAL.
    WA_FINAL-BAL_LIMIT =  WA_FINAL-PURCHASE_LIMIT - WA_FINAL-AMOUNT.
    WA_FINAL-BALANCE     =   WA_FINAL-BAL_LIMIT.

    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
      CHANGING
        VALUE = WA_FINAL-BALANCE.

    MODIFY  GT_FINAL FROM WA_FINAL  INDEX SY-TABIX TRANSPORTING BALANCE .
  ENDLOOP.




ENDMODULE.                 " GET_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_5000 INPUT.

  CASE OK_CODE.

    WHEN 'BACK'.
      REFRESH GT_FINAL3.
      REFRESH   S_LIFNR[].
      REFRESH GT_FINAL.
      LEAVE TO SCREEN 5009.

    WHEN 'EXIT'.
      LEAVE PROGRAM .

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
      CLEAR OK_CODE.

      SELECT
        LIFNR
        BASED_LIMIT
        PURCHASE_LIMIT FROM ZVEN_DEBIT_TAB
            INTO TABLE GT1_DB_FINAL .

      LOOP AT GT_FINAL INTO WA_FINAL.

        WA2_DB_FINAL-LIFNR    =      WA_FINAL-LIFNR .
        WA2_DB_FINAL-NAME1    =      WA_FINAL-NAME1 .
        WA2_DB_FINAL-ORT01    =      WA_FINAL-ORT01 .

        WA2_DB_FINAL-BASED_LIMIT = WA_FINAL-BASED_LIMIT.

        IF WA2_DB_FINAL-BASED_LIMIT IS INITIAL OR WA2_DB_FINAL-BASED_LIMIT EQ '0.00' .
          READ TABLE GT1_DB_FINAL INTO WA1_DB_FINAL WITH KEY LIFNR = WA_FINAL-LIFNR .
          WA2_DB_FINAL-BASED_LIMIT = WA_FINAL-PURCHASE_LIMIT.
        ENDIF.

        WA2_DB_FINAL-AMOUNT   =      WA_FINAL-AMOUNT.
        WA2_DB_FINAL-PURCHASE_LIMIT =  WA_FINAL-PURCHASE_LIMIT.
        WA2_DB_FINAL-LV_DAYS1   =  WA_FINAL-LV_DAYS1  .
        WA2_DB_FINAL-ALLOWED_DAYS = WA_FINAL-ALLOWED_DAYS.
        WA2_DB_FINAL-BALANCE   =  WA_FINAL-BALANCE.

        APPEND WA2_DB_FINAL TO GT2_DB_FINAL.
      ENDLOOP.

      LOOP AT GT2_DB_FINAL INTO WA2_DB_FINAL.
        MOVE-CORRESPONDING  WA2_DB_FINAL TO ZVEN_DEBIT_TAB.
        MODIFY ZVEN_DEBIT_TAB.
      ENDLOOP.
      IF SY-SUBRC = 0.
        MESSAGE 'Record updated sucessfully' TYPE 'S'.
      ENDIF.


    WHEN 'EXCEL'.
      CLEAR OK_CODE.
      TYPES : BEGIN OF WA_HEADER,
              NAME TYPE C LENGTH 30,
            END OF WA_HEADER.
      DATA: T_HEADER TYPE STANDARD TABLE OF WA_HEADER,
            W_HEADER TYPE WA_HEADER.

      DATA: LV_FILENAME TYPE STRING,
            LV_PATH TYPE STRING,
            LV_FULLPATH TYPE STRING,
            LV_RESULT TYPE I,
            LV_DEFAULT_FNAME TYPE STRING,
            LV_FNAME TYPE LOCALFILE.

      W_HEADER-NAME = 'VENDOR CODE'.
      APPEND W_HEADER TO T_HEADER.
      W_HEADER-NAME = 'NAME OF VENDOR'.
      APPEND W_HEADER TO T_HEADER.
      W_HEADER-NAME = 'LOCATION'.
      APPEND W_HEADER TO T_HEADER.
      W_HEADER-NAME = 'BASE PURCHASE LIMIT'.
      APPEND W_HEADER TO T_HEADER.
      W_HEADER-NAME = 'PURCHASE LIMIT'.
      APPEND W_HEADER TO T_HEADER.
      W_HEADER-NAME = 'CURRENT LEDGER BALANCE'.
      APPEND W_HEADER TO T_HEADER.
      W_HEADER-NAME = 'MAXIMUM NO OF DAYS'.
      APPEND W_HEADER TO T_HEADER.
      W_HEADER-NAME = 'ALLOWED DAYS'.
      APPEND W_HEADER TO T_HEADER.
      W_HEADER-NAME = 'BALANCE LIMIT'.
      APPEND W_HEADER TO T_HEADER.


      CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
        EXPORTING
          WINDOW_TITLE      = 'File Directory'
          DEFAULT_EXTENSION = 'XLS'
          INITIAL_DIRECTORY = 'D:\'
        CHANGING
          FILENAME          = LV_FILENAME
          PATH              = LV_PATH
          FULLPATH          = LV_FULLPATH
          USER_ACTION       = LV_RESULT.

      LV_FNAME = LV_FULLPATH.


      CALL FUNCTION 'WS_DOWNLOAD'
        EXPORTING
*         BIN_FILESIZE        = ' '
          FILENAME            = LV_FNAME
          FILETYPE            = 'DAT'
        TABLES
          DATA_TAB            = GT_FINAL3
          FIELDNAMES          = T_HEADER
        EXCEPTIONS
          FILE_OPEN_ERROR     = 1
          FILE_WRITE_ERROR    = 2
          INVALID_FILESIZE    = 3
          INVALID_TABLE_WIDTH = 4
          INVALID_TYPE        = 5
          NO_BATCH            = 6
          UNKNOWN_ERROR       = 7
          OTHERS              = 8.


  ENDCASE.


ENDMODULE.                 " USER_COMMAND_5000  INPUT

*&---------------------------------------------------------------------*
*&      Module  TAB_CONT_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_CONT_MODIFY INPUT.

  MODIFY GT_FINAL FROM WA_FINAL INDEX TAB-CURRENT_LINE.


ENDMODULE.                 " TAB_CONT_MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FIND INPUT.


  DATA :FIELDS TYPE STANDARD TABLE OF SVAL WITH HEADER LINE.
  REFRESH FIELDS.


  CASE OK_CODE.

    WHEN 'FIND'.
      CLEAR OK_CODE.

      DATA : LT_TAB TYPE STANDARD TABLE OF SVAL,
             LS_TAB TYPE SVAL.

      REFRESH LT_TAB.
      DATA LT_RESULTS TYPE MATCH_RESULT_TAB.
      DATA LS_RESULTS TYPE MATCH_RESULT.

      LS_TAB-TABNAME   = 'LFA1'.
      LS_TAB-FIELDTEXT = 'SEARCH'.
      LS_TAB-FIELDNAME = 'LIFNR'.
      APPEND LS_TAB TO LT_TAB.


      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          POPUP_TITLE  = 'Vendor'
          START_COLUMN = '1'
        TABLES
          FIELDS       = LT_TAB.


      IF SY-SUBRC = 0.
        READ TABLE LT_TAB INTO LS_TAB INDEX 1.

        IF LS_TAB-VALUE IS INITIAL.
          EXIT.
        ELSE.
          IF SY-SUBRC = 0.
            FIND ALL OCCURRENCES OF LS_TAB-VALUE IN TABLE GT_LFA1
            RESULTS  LT_RESULTS.

            IF SY-SUBRC = 0.
              READ TABLE LT_RESULTS INTO LS_RESULTS INDEX 1.
              TAB-TOP_LINE = LS_RESULTS-LINE.
              DELETE LT_RESULTS INDEX 1.
            ELSE.
              MESSAGE 'Record Not Found' TYPE 'E'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " FIND  INPUT
