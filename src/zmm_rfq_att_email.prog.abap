CALL FUNCTION 'ZMM_RFQ_ATT_MAIL'
  EXPORTING
    i_ekko                =  im_ekko
    i_mail_id             =  L_EMAIL
  TABLES
    IM_EKPO               = L_EKPO
 EXCEPTIONS
   NO_MAIL_ID_FOUND       = 1
   OTHERS                 = 2.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
