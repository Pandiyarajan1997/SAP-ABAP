*----------------------------------------------------------------------*
***INCLUDE LZFG_PO_MAPPINGF02.
*----------------------------------------------------------------------*
*Added By: Samsudeen M
*Added On: 22.05.2023
*Purpose: Deletion Restrictions
*----------------------------------------------------------------------*
FORM deletion.
  DATA: l_msg TYPE string.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = 'ZMM_POERROR'
      msgnr               = '010'
    IMPORTING
      message_text_output = l_msg.
  MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
ENDFORM.
