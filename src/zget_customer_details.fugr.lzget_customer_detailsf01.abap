*----------------------------------------------------------------------*
***INCLUDE LZGET_CUSTOMER_DETAILSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form POSTAB_FELDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM postab_felder .
*------- Initialisieren ----------------------------------------------*
  CLEAR: ZBAPIRFPOS.
  IF BSID-BUKRS NE T001-BUKRS.
    PERFORM T001_LESEN USING BSID-BUKRS.
  ENDIF.

*------- Felder übertragen und Sonderfelder ermitteln ----------------*
  MOVE-CORRESPONDING BSID TO ZBAPIRFPOS.
  CALL FUNCTION 'CONVERSION_EXIT_KONPR_OUTPUT'
    EXPORTING
      INPUT  = BSID-PROJK
    IMPORTING
      OUTPUT = ZBAPIRFPOS-PROJK.
  MOVE-CORRESPONDING BSID TO BSEGP.
  ZBAPIRFPOS-KOART = 'D'.
  ZBAPIRFPOS-KONTO = BSID-KUNNR.
  ZBAPIRFPOS-DMSHB = BSID-DMBTR.
  ZBAPIRFPOS-WRSHB = BSID-WRBTR.
  PERFORM POSTAB_SONDERFELDER(RFEPOSSF).
  MOVE-CORRESPONDING ZBAPIRFPOS TO POSTAB.

ENDFORM.
