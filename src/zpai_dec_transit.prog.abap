*&---------------------------------------------------------------------*
*&  Include           ZPAI_DEC_TRANSIT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9193  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9193 INPUT."
  CASE OK_CODE.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
      CLEAR OK_CODE.

      CLEAR P_KEY.

      LOOP AT GT_CUS_TABLE1 INTO WA_CUS_TABLE1.
        MOVE-CORRESPONDING WA_CUS_TABLE1 TO ZTRAM_LOG_TABLE.
        MODIFY ZTRAM_LOG_TABLE .

      ENDLOOP.

      IF ZTRAM_LOG_TABLE IS NOT INITIAL.
        IF  SY-SUBRC = 0.
          MESSAGE 'Record Updates Sucessfully' TYPE 'S'.
        ELSE.
          MESSAGE 'Record is not Update' TYPE 'S'.
        ENDIF.
      ENDIF.



  ENDCASE.


ENDMODULE.                 " USER_COMMAND_9193  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_DATA_APPEND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_TABLE_DATA_APPEND INPUT.


  IF WA_FINAL-ST_DAYS LE WA_FINAL-MN_DAYS1.
    WA_FINAL-STATUS = 'YES'.
  ELSEIF WA_FINAL-ST_DAYS  GE WA_FINAL-MN_DAYS1.
    WA_FINAL-STATUS = 'NO'.
  ENDIF.

  MODIFY GT_FINAL FROM WA_FINAL INDEX TAB-CURRENT_LINE.



ENDMODULE.                 " READ_TABLE_DATA_APPEND  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIND_TAB_PLANT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FIND_TAB_PLANT INPUT.

  DATA : LT_RESULTS TYPE MATCH_RESULT_TAB,
         LS_RESULTS TYPE MATCH_RESULT.


  DATA : LT_TAB TYPE STANDARD TABLE OF SVAL,
         LS_TAB TYPE SVAL.

  REFRESH LT_TAB.



  IF OK_CODE =  'FIND'.

    CLEAR  OK_CODE.


    LS_TAB-TABNAME   = 'T001W'.
    LS_TAB-FIELDNAME = 'WERKS'.
    APPEND LS_TAB TO LT_TAB.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
*         NO_VALUE_CHECK        = ' '
       POPUP_TITLE           = 'Find'
       START_COLUMN          = '1'
*       start_row             = '2'
*       IMPORTING
*         RETURNCODE            =
      TABLES
        FIELDS                = LT_TAB
     EXCEPTIONS
       ERROR_IN_FIELDS       = 1
       OTHERS                = 2.

    IF SY-SUBRC = 0.
      READ TABLE LT_TAB INTO LS_TAB INDEX 1.
      IF SY-SUBRC = 0.
        READ TABLE GT_T001W INTO WA_T001W  WITH KEY WERKS = LS_TAB-VALUE.
        IF SY-SUBRC = 0.
          TAB-TOP_LINE = SY-TABIX.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.






ENDMODULE.                 " FIND_TAB_PLANT  INPUT
*&---------------------------------------------------------------------*
*&      Module  ON_ENTER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ON_ENTER INPUT.

  PERFORM SELECT_QUERY_T001W.
ENDMODULE.                 " ON_ENTER  INPUT
*&---------------------------------------------------------------------*
*&      Module  SELECT_ALL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECT_ALL INPUT.
  DATA : LV_FLAG6(1) TYPE C.

  IF P_KEY = '01'.
    IF LV_FLAG6 = ' '.
      LOOP AT GT_FINAL INTO WA_FINAL.
        WA_FINAL-CHECK1 = 'X'.
        MODIFY GT_FINAL FROM WA_FINAL  TRANSPORTING CHECK1.
      ENDLOOP.
      LV_FLAG6 = 'X'.
    ENDIF.
    CLEAR LV_FLAG5.

  ELSEIF P_KEY = '02'.
    IF LV_FLAG5 = ' '.
      LOOP AT GT_FINAL INTO WA_FINAL.
        WA_FINAL-CHECK1 = ''.
        MODIFY GT_FINAL FROM WA_FINAL  TRANSPORTING CHECK1.
      ENDLOOP.
      LV_FLAG5 = 'X'.

    ENDIF.
    CLEAR LV_FLAG6.
  ENDIF.







ENDMODULE.                 " SELECT_ALL  INPUT
*&---------------------------------------------------------------------*
*&      Module  MOVE_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOVE_DATA INPUT.

  LOOP AT GT_FINAL INTO WA_FINAL.

*    IF WA_FINAL-CHECK = 'X'.
*      WA_CUS_TABLE1-STATUS = 'YES'.
*    ELSEIF WA_FINAL-CHECK = ' '.
*      WA_CUS_TABLE1-STATUS = 'NO'.
*    ENDIF.

    IF WA_FINAL-CHECK1 = 'X'.
      WA_CUS_TABLE1-STATUS1 = 'YES'.
    ELSEIF WA_FINAL-CHECK1 = ' '.
      WA_CUS_TABLE1-STATUS1 = 'NO'.
    ENDIF.

    WA_CUS_TABLE1-STATUS = WA_FINAL-STATUS.
    WA_CUS_TABLE1-WERKS = WA_FINAL-WERKS.
    WA_CUS_TABLE1-NAME1 = WA_FINAL-NAME1.
    WA_CUS_TABLE1-ST_DAYS = WA_FINAL-ST_DAYS.
    WA_CUS_TABLE1-MN_DAYS1 = WA_FINAL-MN_DAYS1.
    APPEND WA_CUS_TABLE1 TO GT_CUS_TABLE1.
  ENDLOOP.




ENDMODULE.                 " MOVE_DATA  INPUT
