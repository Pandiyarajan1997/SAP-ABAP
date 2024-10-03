*---------------------------------------------------------------------*
*        FORM IN_FELDTAB_EINTRAGEN                                    *
*---------------------------------------------------------------------*
*        Feldeigenschaften in FELDTAB eintragen.                      *
*---------------------------------------------------------------------*
FORM IN_FELDTAB_EINTRAGEN.
  CLEAR FELDTAB.
  FELDTAB-KTEXT = DD03P-FIELDNAME.

* ------ Schluesselwort lang, mittel, kurz oder Langtext nehmen --------
  FELDTAB-LTEXT = DD03P-SCRTEXT_L.
  IF FELDTAB-LTEXT NE DD03P-SCRTEXT_L
  OR FELDTAB-LTEXT EQ SPACE.
    FELDTAB-LTEXT = DD03P-SCRTEXT_M.
    IF FELDTAB-LTEXT NE DD03P-SCRTEXT_M
    OR FELDTAB-LTEXT EQ SPACE.
      FELDTAB-LTEXT = DD03P-SCRTEXT_S.
      IF FELDTAB-LTEXT NE DD03P-SCRTEXT_S
      OR FELDTAB-LTEXT EQ SPACE.
        FELDTAB-LTEXT = DD03P-DDTEXT.
      ENDIF.
    ENDIF.
  ENDIF.
  FELDTAB-DATATYPE  = DD03P-DATATYPE.
  FELDTAB-INTTYPE   = DD03P-INTTYPE.
  FELDTAB-OUTPUTLEN = DD03P-OUTPUTLEN.
  FELDTAB-DECIMALS  = DD03P-DECIMALS.
  FELDTAB-REFTABLE  = DD03P-REFTABLE.
  FELDTAB-REFFIELD  = DD03P-REFFIELD.
  FELDTAB-CONVEXIT  = DD03P-CONVEXIT.
  FELDTAB-REPTEXT   = DD03P-REPTEXT.
  FELDTAB-LOWERCASE = DD03P-LOWERCASE.
  APPEND FELDTAB.
ENDFORM.

*eject
