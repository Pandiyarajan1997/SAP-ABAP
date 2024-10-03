*----------------------------------------------------------------------*
*   INCLUDE RVKALVAT                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       FORM USER_COMMAND                                              *
*----------------------------------------------------------------------*
*       AT USER COMMAND                                                *
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                  RS_SELFIELD TYPE SLIS_SELFIELD.


* Get the entry that is selected and not the entry where the cursor is.
  READ TABLE POSTAB WITH KEY SELKZ = 'X'.
  IF SY-SUBRC NE 0.
    READ TABLE POSTAB INDEX RS_SELFIELD-TABINDEX.
    IF SY-SUBRC NE 0.
      READ TABLE POSTAB INDEX 1.
    ENDIF.
  ENDIF.

* REPLACE 'POSTAB-' WITH ' ' INTO RS_SELFIELD-SEL_TAB_FIELD.
* CONDENSE RS_SELFIELD-SEL_TAB_FIELD NO-GAPS.
* FELD = RS_SELFIELD-SEL_TAB_FIELD.
  RS_SELFIELD-REFRESH = 'X'.
  IF SY-SUBRC = 0.
    CASE R_UCOMM.
      WHEN 'ABSA'.                     " Absage
        PERFORM OKCODE_ABSA.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'ABSR'.                     " Rücknahme Absage
        PERFORM OKCODE_ABSR.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'AEND'.                     " Ändern
        PERFORM OKCODE_AEND.
      WHEN 'DETA'.                     " Auswählen, Detail
        REPLACE 'POSTAB-' WITH ' ' INTO RS_SELFIELD-SEL_TAB_FIELD.
        CONDENSE RS_SELFIELD-SEL_TAB_FIELD NO-GAPS.
        FELD = RS_SELFIELD-SEL_TAB_FIELD.
        PERFORM OKCODE_AUSW USING RS_SELFIELD-TABINDEX.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN '&IC1'.                     " Doppelklick
        READ TABLE POSTAB INDEX RS_SELFIELD-TABINDEX.
        FELD = RS_SELFIELD-FIELDNAME.
        PERFORM OKCODE_AUSW USING RS_SELFIELD-TABINDEX.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'BELA'.                     " Belegänderungen
        PERFORM OKCODE_BELA.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'BELF'.                     " Belegfluss
        PERFORM OKCODE_BELF.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'BELG'.                     " Anzeige Beleg
        PERFORM OKCODE_BELG.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'BELS'.                     " Belegstatus
        PERFORM OKCODE_BELS USING  RS_SELFIELD-TABINDEX.
        RS_SELFIELD-REFRESH = ' '.
      WHEN 'CCAU'.                     " Zahlungskarte author.
        PERFORM OKCODE_CCAU.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'CCAR'.                     " Zahlungskarte zurück.
        PERFORM OKCODE_CCAR.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'EF17'.                     " Neue Selektion
        PERFORM OKCODE_EF17.
      WHEN 'FIS0'.                     " FIS
        CALL TRANSACTION 'F.30'.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'FREI'.                     " Belege Freigeben.
        PERFORM OKCODE_FREIGABE.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'FRER'.                     " Freigabe zurücknehmen
        PERFORM OKCODE_FRER.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'F.31'.                     " Kreditübersicht
        PERFORM OKCODE_F31.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'KUST'.                     " Anzeige Kundenstamm
        PERFORM OKCODE_KUST.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'KUS0'.                     " Anzeige Kundenstamm
        PERFORM OKCODE_FD32 USING UEBERSICHT.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'KUS1'.                     " Anzeige Kundenstamm
        PERFORM OKCODE_FD32 USING ALLG_DATEN.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'KUS2'.                     " Anzeige Kundenstamm
        PERFORM OKCODE_FD32 USING KONTROLLBD.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'LPRO'.                     " Langtext Protokoll
        PERFORM PROTOKOLL_LANGTEXT.
      WHEN 'LEGE'.                     " Legende anzeigen
        PERFORM OKCODE_LEGE_ALV.
        RS_SELFIELD-REFRESH = ' '.
      WHEN 'PANS'.                     " Anzeige Partneranschrift
        PERFORM OKCODE_PANS.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'PROT'.                     " Anzeige Partneranschrift
        PERFORM OKCODE_PROT_ALV.
        RS_SELFIELD-REFRESH = ' '.
      WHEN 'PRUE'.                     " Kreditlimitpruefung
        PERFORM OKCODE_PRUE.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'PRUR'.                     " Prüfung zuruecknehmen
        PERFORM OKCODE_PRUR.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'PRKN'.                     " Kreditneuvergabe
        PERFORM OKCODE_PRKN.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'PRZN'.                     " Kreditneuvergabe zurueck
        PERFORM OKCODE_PRZN.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'REP1'.                     " offene Aufträge
        PERFORM OKCODE_REP1.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'REP2'.                     " offene Lieferungen/Fakt
        PERFORM OKCODE_REP2.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'SAVE'.                     " Sichern Buchungsregeln
        PERFORM OKCODE_SAVE.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'VKM1'.                     " Gesperrte
        PERFORM OKCODE_VKM1.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'VKM2'.                     " Freigegebene
        PERFORM OKCODE_VKM2.
      WHEN 'VKM5'.                     " Kreditstammblatt
        PERFORM OKCODE_VKM5.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'WEIT'.                     " Weiterleiten
        PERFORM OKCODE_WEIT.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'WEIR'.                     " Rücknahme Weiterleiten
        PERFORM OKCODE_WEIR.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN 'ZUKV'.                     " Anzeige WKV
        PERFORM OKCODE_ZUKV.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
      WHEN OTHERS.
        PERFORM USER_OKCODE.
        RS_SELFIELD-col_stable = 'X'.
        RS_SELFIELD-row_stable = 'X'.
    ENDCASE.
  ENDIF.

ENDFORM.
