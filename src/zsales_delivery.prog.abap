*&---------------------------------------------------------------------*
*& Report  ZSALES_DELIVERY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSALES_DELIVERY.

TYPE-POOLS: VRM.

DATA: IT_TYPE TYPE VRM_VALUES,
      WA_TYPE LIKE LINE OF IT_TYPE .

SELECTION-SCREEN: BEGIN OF BLOCK SD WITH FRAME TITLE TEXT-001.
PARAMETERS: LIST TYPE C AS LISTBOX VISIBLE LENGTH 26 OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK SD.

INITIALIZATION.
  WA_TYPE-KEY = '1' .
  WA_TYPE-TEXT = 'Customer Delivery Details' .
  APPEND:WA_TYPE TO IT_TYPE.

  WA_TYPE-KEY = '2' .
  WA_TYPE-TEXT = 'STO ( Inter / Intra )' .
  APPEND:WA_TYPE TO IT_TYPE.

  WA_TYPE-KEY = '3' .
  WA_TYPE-TEXT = 'Stock Transfer Order Details' .
  APPEND:WA_TYPE TO IT_TYPE.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'LIST'
      VALUES          = IT_TYPE
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

START-OF-SELECTION.

  PERFORM USER_CMD USING LIST.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  USER_CMD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM USER_CMD USING LIST ."RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE LIST.
    WHEN '1'.
      CALL TRANSACTION 'ZCUST_DELIVERY'.
    WHEN '2'.
      CALL TRANSACTION 'ZINV_DEL_DETAILS'.
    WHEN '3'.
      CALL TRANSACTION 'ZSTO_DEL_DETAILS'.
  ENDCASE.

ENDFORM.                    "USER_CMD
