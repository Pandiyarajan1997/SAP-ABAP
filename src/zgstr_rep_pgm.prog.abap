*&---------------------------------------------------------------------*
*& Report  ZREPORTS_INTERFACE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZGSTR_REP_PGM.


DATA: GT_LIST     TYPE VRM_VALUES,
      GWA_LIST    TYPE VRM_VALUE,
      GT_VALUES   TYPE TABLE OF DYNPREAD,                     " INTERNAL TABLE FOR LIST BOX
      GWA_VALUES  TYPE DYNPREAD,                              " WORK AREA FOR LIST BOX
      GV_SELECTED_VALUE(10) TYPE C.



SELECTION-SCREEN: BEGIN OF BLOCK B.



SELECTION-SCREEN: END OF BLOCK B.


SELECTION-SCREEN: BEGIN OF BLOCK B1.

PARAMETERS: PS_PARM AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND ABC MODIF ID TB1.            " SELECTION SCREEN PARAMETER FOR INVOICE AND CUSTOMER BALANCES

SELECTION-SCREEN: END OF BLOCK B1.



*--------------------------------------------------------------*
*At Selection Screen Ouput ON List Box PS_PARM
*--------------------------------------------------------------*


AT SELECTION-SCREEN ON PS_PARM.
  CLEAR: GWA_VALUES, GT_VALUES.
  REFRESH GT_VALUES.
  GWA_VALUES-FIELDNAME = 'PS_PARM'.
  APPEND GWA_VALUES TO GT_VALUES.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME             = SY-CPROG
      DYNUMB             = SY-DYNNR
      TRANSLATE_TO_UPPER = 'X'
    TABLES
      DYNPFIELDS         = GT_VALUES.

  READ TABLE GT_VALUES INDEX 1 INTO GWA_VALUES.
  IF SY-SUBRC = 0 AND GWA_VALUES-FIELDVALUE IS NOT INITIAL.
    READ TABLE GT_LIST INTO GWA_LIST
                      WITH KEY KEY = GWA_VALUES-FIELDVALUE.
    IF SY-SUBRC = 0.
      GV_SELECTED_VALUE = GWA_LIST-TEXT.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------*
*Initialization
*--------------------------------------------------------------*

INITIALIZATION.


  GWA_LIST-KEY = '1'.
  GWA_LIST-TEXT = 'GSTR1'.
  APPEND GWA_LIST TO GT_LIST.
  CLEAR: GWA_LIST.

  GWA_LIST-KEY = '2'.
  GWA_LIST-TEXT = 'GSTR2'.
  APPEND GWA_LIST TO GT_LIST.
  CLEAR: GWA_LIST.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID              = 'PS_PARM'
        VALUES          = GT_LIST
      EXCEPTIONS
        ID_ILLEGAL_NAME = 1
        OTHERS          = 2.


*START-OF-SELECTION.

AT SELECTION-SCREEN.

  IF GWA_VALUES-FIELDVALUE = 1.

    CALL TRANSACTION 'ZGSTR_SALES'.

  ENDIF.

  IF GWA_VALUES-FIELDVALUE = 2.

    CALL TRANSACTION 'ZGSTR_PUR'.

  ENDIF.
