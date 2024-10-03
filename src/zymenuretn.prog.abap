DATA: MENU_RETN(8).
FORM RETURN_TO_MENU.
  data: program like trdir-name,
        tcode like tstc-tcode.

* geändert zu 4.6 G.S.
*  IF SY-CALLD = SPACE.
    GET PARAMETER ID 'MEN' FIELD program.
    IF MENU_RETN NE SPACE.
      CALL FUNCTION 'PROGRAM_TCODE_SPLIT'
           EXPORTING
                PROGRAM                  = program
           IMPORTING
*               NAMESPACE                =
                TRANSACTIONCODE          = tcode
           EXCEPTIONS
                NO_AREA_MENU             = 1
                DELIMITER_WRONG_POSITION = 2
                DELIMITER_WRONG_NUMBER   = 3
                NO_VALID_NAMESPACE       = 4
                OTHERS                   = 5.
      if sy-subrc = 0.
         LEAVE TO TRANSACTION tcode.
      else.
         set screen 0. leave screen.
      endif.
    ELSE.
     SET SCREEN 0.
     LEAVE SCREEN.
    ENDIF.
*  ELSE.
    LEAVE.
*  ENDIF.

ENDFORM.
