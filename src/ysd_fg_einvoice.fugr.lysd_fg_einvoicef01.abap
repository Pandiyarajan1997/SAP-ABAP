*----------------------------------------------------------------------*
***INCLUDE LZSD_FG_EINVOICEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ERROR_MSG1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ERROR_MSG  text
*      -->P_V_ERR  text
*----------------------------------------------------------------------*
FORM error_msg1  TABLES   p_error_msg1 STRUCTURE bapiret2
                  USING   p_response   TYPE string.
  TYPES: BEGIN OF t_error,
            error_code    TYPE  string,
            error_message TYPE string,
            error_source  TYPE string,
          END OF t_error.

  DATA: it_error   TYPE TABLE OF t_error,
        wa_error   TYPE t_error,
        it_bapiret TYPE TABLE OF bapiret2,
        wa_bapiret TYPE bapiret2.

*  CONCATENATE '[' P_RESPONSE ']' INTO P_RESPONSE.

  CLEAR:v_str.
  SPLIT p_response AT '"error_code":"'   INTO v_str wa_error-error_code.
  CLEAR: v_str.
  SPLIT p_response AT '"error_message":"' INTO v_str1 wa_error-error_message.
  CLEAR: v_str.
  SPLIT p_response AT '"error_source":"' INTO v_str1 wa_error-error_source.

  IF wa_error-error_message IS INITIAL.
    SPLIT p_response AT '"errors":["' INTO v_str1 wa_error-error_message.
  ENDIF.
*  LOOP AT IT_ERROR INTO WA_ERROR.
  wa_bapiret-id         = wa_error-error_code.
  wa_bapiret-message    = wa_error-error_message.
  wa_bapiret-message_v1 = wa_error-error_source.
  APPEND wa_bapiret TO p_error_msg1.
  CLEAR wa_bapiret.
*  ENDLOOP.

ENDFORM.                    " ERROR_MSG1
