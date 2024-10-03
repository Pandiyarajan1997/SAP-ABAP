*----------------------------------------------------------------------*
***INCLUDE LZGET_DMS_DATA_APPF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  RAISE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_0160   text
*      -->P_0161   text
*----------------------------------------------------------------------*
FORM RAISE_MESSAGE  TABLES   RETURN STRUCTURE BAPIRET2
                               "Insert correct name for <...>
                    USING    VALUE(P_0314)
                             VALUE(P_0315).

       DATA: WA_RETURN TYPE BAPIRET2.
DATA: GV_INDEX_MSG TYPE BAPIRET2-NUMBER.

  GV_INDEX_MSG = GV_INDEX_MSG + 1.

  WA_RETURN-TYPE    =  P_0314.
  WA_RETURN-NUMBER  =  GV_INDEX_MSG.
  WA_RETURN-MESSAGE =  P_0315.
  APPEND WA_RETURN TO RETURN.
  CLEAR WA_RETURN.

ENDFORM.                    " RAISE_MESSAGE
