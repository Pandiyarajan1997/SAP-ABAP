*----------------------------------------------------------------------*
***INCLUDE LZFG_VENDORF04.
*----------------------------------------------------------------------*
FORM deletion.
  DATA: l_msg TYPE string.
  l_msg = |Deleting Entries is not allowed|.
  MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
ENDFORM.
