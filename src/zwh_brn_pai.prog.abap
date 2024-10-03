*&---------------------------------------------------------------------*
*&  Include           ZWH_BRN_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9191  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9191 INPUT.

  CASE OK_CODE.

    WHEN 'BACK'.
      LEAVE PROGRAM.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
      LOOP AT GT_FINAL INTO WA_FINAL.
        MOVE-CORRESPONDING WA_FINAL TO ZWH_BRN.
          MODIFY ZWH_BRN.
        IF SY-SUBRC = 0.
          MESSAGE 'Record Sucessfully Update' TYPE 'S'.
        ENDIF.

      ENDLOOP.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9191  INPUT
*&---------------------------------------------------------------------*
*&      Module  ENTER_RECORD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ENTER_RECORD INPUT.


  LOOP AT GT_T001 INTO WA_T001 WHERE BUKRS = WA_FINAL-BUKRS.
    WA_FINAL-BUKRS = WA_T001-BUKRS.
    WA_FINAL-BUTXT = WA_T001-BUTXT.
    APPEND WA_FINAL TO GT_FINAL.
  ENDLOOP.



ENDMODULE.                 " ENTER_RECORD  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_RECORD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_RECORD INPUT.


  LOOP AT GT_T001W INTO WA_T001W WHERE WERKS = WA_FINAL-WERKS.
    WA_FINAL-BUKRS = WA_FINAL-BUKRS.
    WA_FINAL-BUTXT = WA_FINAL-BUTXT.
    WA_FINAL-WERKS = WA_T001W-WERKS.
    WA_FINAL-NAME1 = WA_T001W-NAME1.
    MODIFY GT_FINAL FROM WA_FINAL INDEX TAB-CURRENT_LINE.
  ENDLOOP.



ENDMODULE.                 " MODIFY_RECORD  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_BR_RECORD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_BR_RECORD INPUT.


  LOOP AT GT_T001W INTO WA_T001W WHERE WERKS = WA_FINAL-WERKS1.
    WA_FINAL-BUKRS = WA_FINAL-BUKRS.
    WA_FINAL-BUTXT = WA_FINAL-BUTXT.
    WA_FINAL-WERKS = WA_T001W-WERKS.
    WA_FINAL-NAME1 = WA_T001W-NAME1.
    WA_FINAL-WERKS1 = WA_T001W-WERKS.
    WA_FINAL-NAME1_1 = WA_T001W-NAME1.
    MODIFY GT_FINAL FROM WA_FINAL INDEX TAB-CURRENT_LINE TRANSPORTING NAME1_1 WERKS1.
  ENDLOOP.






ENDMODULE.                 " MODIFY_BR_RECORD  INPUT
