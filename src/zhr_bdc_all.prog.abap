
*&---------------------------------------------------------------------*
*& Report  ZHR_BDC_ALL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZHR_BDC_ALL.

DATA:
     GT_LIST     TYPE VRM_VALUES,
      GWA_LIST    TYPE VRM_VALUE,

      GT_VALUES   TYPE TABLE OF DYNPREAD,                     " INTERNAL TABLE FOR LIST BOX

      GWA_VALUES  TYPE DYNPREAD,                              " WORK AREA FOR LIST BOX

      GV_SELECTED_VALUE(30) TYPE C.



SELECTION-SCREEN: BEGIN OF BLOCK B.

SELECTION-SCREEN: END OF BLOCK B.

SELECTION-SCREEN: BEGIN OF BLOCK B1.

PARAMETERS: PS_PARM AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND ABC MODIF ID TB1.
SELECTION-SCREEN: END OF BLOCK B1.



*--------------------------------------------------------------*

*At

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
*
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

  GWA_LIST-TEXT = ' Passport number'.

  APPEND GWA_LIST TO GT_LIST.

  CLEAR: GWA_LIST.

  GWA_LIST-KEY = '2'.

  GWA_LIST-TEXT = ' Driving licence'.

  APPEND GWA_LIST TO GT_LIST.

  CLEAR: GWA_LIST.

  GWA_LIST-KEY = '3'.

  GWA_LIST-TEXT = ' Blood Group'.

  APPEND GWA_LIST TO GT_LIST.

  CLEAR: GWA_LIST.

  GWA_LIST-KEY = '4'.

  GWA_LIST-TEXT = ' Family member (father)'.

  APPEND GWA_LIST TO GT_LIST.

  CLEAR: GWA_LIST.

  GWA_LIST-KEY = '5'.

  GWA_LIST-TEXT = ' Family member (mother)'.

  APPEND GWA_LIST TO GT_LIST.

  CLEAR: GWA_LIST.

  GWA_LIST-KEY = '6'.

  GWA_LIST-TEXT = 'Family member (spouse)'.

  APPEND GWA_LIST TO GT_LIST.

  CLEAR: GWA_LIST.


  GWA_LIST-KEY = '7'.

  GWA_LIST-TEXT = 'Family member (child1)'.

  APPEND GWA_LIST TO GT_LIST.

  CLEAR: GWA_LIST.

  GWA_LIST-KEY = '8'.

  GWA_LIST-TEXT = ' Family member (child2)'.

  APPEND GWA_LIST TO GT_LIST.

  CLEAR: GWA_LIST.

  GWA_LIST-KEY = '9'.

  GWA_LIST-TEXT = 'Address(permanent)'.

  APPEND GWA_LIST TO GT_LIST.

  CLEAR: GWA_LIST.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'PS_PARM'
      VALUES          = GT_LIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.



AT SELECTION-SCREEN.

  IF GWA_VALUES-FIELDVALUE = 1.

    CALL TRANSACTION 'ZHR_PASSPORT'.

  ENDIF.

  IF GWA_VALUES-FIELDVALUE = 2.

    CALL TRANSACTION 'ZHR_DRIVINGLICENSE'.

  ENDIF.

  IF GWA_VALUES-FIELDVALUE = 3.

    CALL TRANSACTION 'ZHR_MEDICAL1'.

  ENDIF.

  IF GWA_VALUES-FIELDVALUE = 4.

    CALL TRANSACTION 'ZHR_FATHER'.

  ENDIF.

  IF GWA_VALUES-FIELDVALUE = 5.

  CALL TRANSACTION 'ZHR_MOTHER'.

  ENDIF.

  IF GWA_VALUES-FIELDVALUE = 6.

    CALL TRANSACTION 'ZHR_SPOUSE'.

  ENDIF.

  IF GWA_VALUES-FIELDVALUE = 7.

    CALL TRANSACTION 'ZHR_CHILD1'.

  ENDIF.

  IF GWA_VALUES-FIELDVALUE = 8.

    CALL TRANSACTION 'ZHR_CHILD2'.

  ENDIF.

  IF GWA_VALUES-FIELDVALUE = 9 .

    CALL TRANSACTION 'ZHR_PERADDRESS'.

  ENDIF.
