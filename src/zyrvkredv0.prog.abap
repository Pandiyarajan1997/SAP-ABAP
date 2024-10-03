*-------------------------------------------------------------------
***INCLUDE RVKREDV0 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  VBKRED_FUELLEN
*&---------------------------------------------------------------------*
FORM VBKRED_FUELLEN.

* Steuersenkungsgesetz
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      FUNCNAME           = 'CA_CHECK_DATE'
    EXCEPTIONS
      FUNCTION_NOT_EXIST = 1
      OTHERS             = 2.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'CA_CHECK_DATE'
      EXPORTING
        I_APPLK                 = 'SD'
        I_ORGUNIT               = VBKRED-VKORG
        I_USER                  = SY-UNAME
        I_PROGRAM               = 'RVKRED01'
        I_FROM_DATE             = VBKRED-ERDAT
     EXCEPTIONS
       NO_AUTHORITY_DATE       = 1
       NO_AUTHORITY_PROG       = 2
       WRONG_PARAMETER         = 3
       OTHERS                  = 4.
    IF SY-SUBRC = 1 or
       SY-SUBRC = 2.
      EXIT.
    ENDIF.
  ENDIF.

  POSTAB = VBKRED.
  POSTAB-ACTIV = '0'.
  PERFORM AUTHORITY_CHECK_DOCUMENT CHANGING VBKRED
                                            AUTH_VBUK_RC.
  PERFORM AUTHORITY_CHECK_CUSTOMER USING    VBKRED
                                   CHANGING AUTH_KNKK_RC.
  IF AUTH_KNKK_RC NE 0
  OR AUTH_VBUK_RC NE 0.
    POSTAB-SELKZ = '-'.
  ELSE.
    POSTAB-SELKZ = ' '.
  ENDIF.
  POSTAB-ZAEHL = TABIX.
  APPEND POSTAB.
  TABIX = SY-TABIX.
ENDFORM.                    " VBKRED_FUELLEN
