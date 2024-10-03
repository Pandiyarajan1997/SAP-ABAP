*******  INCLUDE zum SAPMa15b: Daten ***********************************
PROGRAM ZZZZZZZZ NO STANDARD PAGE HEADING MESSAGE-ID VR     "
                 LINE-SIZE 140.

* ALV
TYPE-POOLS: SLIS.
TYPE-POOLS: ICON.

INCLUDE RVREUSE_GLOBAL_DATA.

TABLES:  VBKRED,                       " Listanzeige Einzelposten
         VBKREDET,                     " Detail              P30K028860
         VBAK,                         " Verkaufsbeleg - Kopf
         VBKD,                         " Verkaufsbeleg - Kaufm. Daten
         LIKP,                         " Lieferung
         LIPS,                         " Lieferpositionen
         RSD2X,                                             "
         TPAR,                         " Partnertabelle
         TPART,                        " Partnertext
         KNA1,                         " Kundenstamm
         ADRS,                         " Adressdaten
         TINPA,                        " Index Partner
         TVIND,                        " Index Material
         T014T,                        " Langtext Kreditkontrollbereich
         T024B,                         "#EC CI_USAGE_OK[2227014] " Langtext BearbeitergruppeAdded by <IT-CAR Tool> during Code Remediation
         TVAGT,                        " Absagegründe
         T691C,                        " Langtext Kreditmanagmentgruppe
         KVBUK,                        " Arbeitsstruktur Belegstatus
         TSTCT.                        " Bezeichnung Transaktion

TABLES:
         VBADR,                        " Adresse eines Partners
         DD03P,                        " Tabellenfelder
         DOKHL,                        " Nachricht Langtext
         VBCO6,                        " Kommunikation mit DB RV_DOCUMEN
         VBUK,                         " Kopfstatusinformation
         VBUP,                         " Positionsstatusinformation
         VBPA,                         " Schnittstelle zur Adresse
         VBSTT,                        " Statustexte
         RKAKY,                        " Kommunikation FB RK_POPUP_LIST_
         RV75A.                        " Dynpro-/Arbeitsfelder

TABLES:  INDX.                         " Indx

TABLES:
         T180B,                        " Summenvarianten
         T180C,                        " Bez. der Summenvarianten
         T180G,                        " Erlaubte Summenvarianten
         T180P,                        " Auswahlfelder
         T180S,                        " Felder zu Anzeigevarianten
         T180T,                        " Bezeichnung Anzeigevarianten
         T180U,                        " Überschriften Anzeigevarianten
         T180V,                        " Vorschlagswerte f. Anz.Variante
         T180Z,                        " Anzeigevarianten, Zugriffsarten
         T181T.                        " RV spezifische Texte

DATA: BEGIN OF XVBKREDET OCCURS 100.                        "P30K028860
      INCLUDE STRUCTURE VBKREDET.                           "P30K028860
*      **         Start of addition by anila on 27.02.2017
  data:         UNAME type UNAME,
           REGIO type REGIO,
           ZWIDY type ZWIDY.
**           End of addition  by anila on 27.02.2017
DATA: END   OF XVBKREDET.                                   "P30K028860


*-----------------------------------------------------------------------
*       Datenfelder fuer das Programm RVKRED01.
*       ---------------------------------------
*       Teil 1 :  Tabellen      ( BEGIN OF ... OCCURS )
*       Teil 2 :  Strukturen    ( BEGIN OF ... )
*       Teil 3 :  Einzelfelder  ( variabel )
*       Teil 4 :  Konstanten
*       Teil 5 :  Field-Symbols
*-----------------------------------------------------------------------

*eject
*-----------------------------------------------------------------------
*       Teil 1 :  Tabellen      ( BEGIN OF ... OCCURS )
*-----------------------------------------------------------------------

DATA: BEGIN OF XT180A OCCURS 10.       "Felder der Arbeitsstruktur
        INCLUDE STRUCTURE T180A.
DATA: END   OF XT180A.

DATA: BEGIN OF XT180S OCCURS 10.       "Aufbau Zeile
        INCLUDE STRUCTURE T180S.
DATA: END   OF XT180S.

DATA: BEGIN OF XT180U OCCURS 10.       "Aufbau Überschriften
        INCLUDE STRUCTURE T180U.
DATA: END   OF XT180U.

* ----- Tabelle der aktuellen Summen (im Summiermodus OBEN) ------------
DATA:    BEGIN OF AKTSUM OCCURS 100,
           SUMMARY(1)        TYPE C,   " Kz.: Eintrag hell ?
           FELD1(40)         TYPE C,   " Fixwert Stufe 1
           FELD2(40)         TYPE C,   " Fixwert Stufe 2
           FELD3(40)         TYPE C,   " Fixwert Stufe 3
           CMWAE             LIKE VBKRED-CMWAE,  " Währung KBereich
           AMTBL             LIKE VBKRED-AMTBL,  " freigeg. Kredwert
           KWKKB             LIKE VBKRED-KWKKB,  " Kreditwert in KWAER
           KWKKD             LIKE VBKRED-KWKKD,  " Horizont  P30K028660
           ANZPO(3)          TYPE P,   " Anzahl Posten
         END   OF AKTSUM.

* ----- Tabelle der Waehrungen und der Anzahl ihrer Dezimalen ----------
DATA:    BEGIN OF DEZTAB OCCURS 10,
           CMWAE             LIKE VBKRED-CMWAE,   " Waehrung
           DEZIM(1)          TYPE N,   " Anzahl Dezimalen
           FAKTOR(3)         TYPE P,   " Faktor (1-1000)
         END   OF DEZTAB.


* ----- Tabelle der naechsten Summen (im SPLIT unten) ------------------
DATA:    BEGIN OF NXTSUM OCCURS 100,
           FELD1(40)         TYPE C,   " Wert auf Level 1
           FELD2(40)         TYPE C,   " Wert auf Level 2
           FELD3(40)         TYPE C,   " Wert auf Level 3
           CMWAE             LIKE VBKRED-CMWAE,  " Währung KBereich
           AMTBL             LIKE VBKRED-AMTBL,  " freigeg. Kredwert
           KWKKB             LIKE VBKRED-KWKKB,  " Kreditwert in KWAER
           KWKKD             LIKE VBKRED-KWKKD,  "Horizont   P30K028660
           ANZPO(3)          TYPE P,   " Anzahl Posten
         END   OF NXTSUM.

* ------ Tabelle der Einzelposten  -------------------------------------
DATA:    BEGIN OF POSTAB OCCURS 200.
           INCLUDE STRUCTURE VBKRED.
DATA:      STATUS_BEL type ICON-id,
           ABGRU_NEW LIKE VBAP-ABGRU,
           SBGRP_NEW LIKE VBAK-SBGRP,
***         Start of addition by anila on 27.02.2017
           status type char200,
           UNAME type char200,
           REGIO type REGIO,
           ZWIDY type ZWIDY,
***           End of addition  by anila on 27.02.2017
           COL(3)   TYPE C.            " Farbfeld für ALV
DATA:    END   OF POSTAB.

data: gt_exc type table of ALV_S_QINF.

*-----------------------Farbstruktur für ALV---------------------------*
DATA: BEGIN OF FARB ,
      FARB1(1) VALUE 'C',
      FARB2(1),
      FARB3(1) VALUE '0'.
DATA: END OF FARB.

* ----- Tabelle der Summen ---------------------------------------------
DATA:    BEGIN OF SUMMEN OCCURS 100,
           FELD1(40)         TYPE C,   " Wert Kriterium 1
           FELD2(40)         TYPE C,   " Wert Kriterium 2
           FELD3(40)         TYPE C,   " Wert Kriterium 3
           CMWAE             LIKE VBKRED-CMWAE,  " Währung KBereich
           AMTBL             LIKE VBKRED-AMTBL,  " freigeg. Kredwert
           KWKKB             LIKE VBKRED-KWKKB,  " Kreditwert in KWAER
           KWKKD             LIKE VBKRED-KWKKD,  "Horizont   P30K028660
           ANZPO(3)          TYPE P,   " Anzahl Posten
         END   OF SUMMEN.

* ------ Gerettete POSTAB beim Wechsel ANZEIGEN-AENDERN (wg. Berecht.)--
DATA:    BEGIN OF XPOSTAB OCCURS 200.
        INCLUDE STRUCTURE VBKRED.                           " 6979
DATA:      STATUS_BEL  type  rvkred_status_alv,
           ABGRU_NEW LIKE VBAP-ABGRU,
           SBGRP_NEW LIKE VBAK-SBGRP.
DATA:    END   OF XPOSTAB.

* ------ Gerettete POSTAB beim Wechsel Selektieren und F12            --
DATA:    BEGIN OF YPOSTAB OCCURS 200,
           LSIND             LIKE SY-LSIND,      " Stufe
           ZAEHL   LIKE VBVFI-ZAEHL,   " Indexzaehler JW
           VBELN   LIKE VBKRED-VBELN,
           VBTYP  LIKE VBKRED-VBTYP,
           SELKZ   LIKE VBKRED-SELKZ,   " Selektionskennzeichen
           STATUS_BEL  type  rvkred_status_alv,
           ABGRU_NEW LIKE VBAP-ABGRU,
           SBGRP_NEW LIKE VBAK-SBGRP,
         END   OF YPOSTAB.

DATA:    BEGIN OF ZPOSTAB OCCURS 200,
           LSIND             LIKE SY-LSIND,      " Stufe
           ZAEHL   LIKE VBVFI-ZAEHL,   " Indexzaehler JW
           VBELN   LIKE VBKRED-VBELN,
           VBTYP  LIKE VBKRED-VBTYP,
           SELKZ   LIKE VBKRED-SELKZ,   " Selektionskennzeichen
           STATUS_BEL  type  rvkred_status_alv,
           ABGRU_NEW LIKE VBAP-ABGRU,
           SBGRP_NEW LIKE VBAK-SBGRP,
         END   OF ZPOSTAB.

* ------ Bearbeitete Belege.                         -------------------
DATA:    BEGIN OF FPOSTAB OCCURS 50,   "Freigegebene
           VBELN  LIKE VBKRED-VBELN,
           VBTYP  LIKE VBKRED-VBTYP,
           ZAEHL   LIKE VBVFI-ZAEHL,   " Indexzaehler JW
           STATUS_BEL  type  rvkred_status_alv,
           ABGRU_NEW LIKE VBAP-ABGRU,
           SBGRP_NEW LIKE VBAK-SBGRP,
           FUNCTION,
         END   OF FPOSTAB.
*------ gepufferte VBUK -----------------------------------------------
DATA: BEGIN OF LVBUK OCCURS 0001.
INCLUDE STRUCTURE VBUK .
DATA: END OF LVBUK .

*eject
*-----------------------------------------------------------------------
*       Teil 2 :  Strukturen    ( BEGIN OF ... )
*-----------------------------------------------------------------------

* ------ Schluessel fuer INDX ------------------------------------------
DATA:    BEGIN OF INDXID,                                   " 6979
           PROGR(8)      TYPE C VALUE 'RVKRED01',
           SPRAS         LIKE SY-LANGU," Sprache
         END   OF INDXID.

* ------ Gerettete Belegzeile (Hide) -----------------------------------
DATA:    BEGIN OF POSTAB_HIDE.                              " 6979
        INCLUDE STRUCTURE VBKRED.      " Listanzeigen-Struktur
DATA:    END   OF POSTAB_HIDE.

*------- Logischer Schluessel der POSTAB --------  6175 --------------
DATA:    BEGIN OF POSTAB_KEY,
           MANDT             LIKE VBKRED-MANDT,   " Mandant
           KKBER             LIKE VBKRED-KKBER,   " Kreditkontrollbereic
           VBELN             LIKE VBKRED-VBELN,   " Beleg
           POSNR             LIKE VBKRED-POSNR,   " Position
         END   OF POSTAB_KEY.

*------- Schlüssel für VBUK ------------------------------------------
DATA: BEGIN OF VBUK_KEY,
   MANDT LIKE VBUK-MANDT,
   VBELN LIKE VBUK-VBELN,
END OF VBUK_KEY.

*------- Gerette Felder (z.B. Gruppenendverarbeitung) ----------------
DATA:    BEGIN OF OLD,
           KTEXT(10)         TYPE C,   " Feldname
           AMTBL             LIKE VBKRED-AMTBL,  " freigeg. Kredwert
           ANZPO(3)          TYPE P,   " Anzahl Posten
         END OF OLD.
*eject
*-----------------------------------------------------------------------
*       Teil 3 :  Einzelfelder
*-----------------------------------------------------------------------
DATA:    VIEWNAME LIKE DFIES-TABNAME VALUE 'VBKRED'."Structur der POSTAB


*-----------------------------------------------------------------------
*       Teil 4 :  Konstanten
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
*       Teil 5 :  Field-Symbols
*-----------------------------------------------------------------------


*-----------------------------------------------------------------------
*        COMMON DATA
*-----------------------------------------------------------------------

* ------ STATAB und FELDTAB --------------------------------------------
DATA:    BEGIN OF COMMON PART CP2.
INCLUDE MV75AC02.
DATA:    END   OF COMMON PART.


*       ---------------------------------------
*       Teil 1 :  Tabellen      ( BEGIN OF ... OCCURS )
*       Teil 2 :  Strukturen    ( BEGIN OF ... )
*       Teil 3 :  Einzelfelder  ( variabel )
*       Teil 4 :  Konstanten
*       Teil 5 :  Field-Symbols
*-----------------------------------------------------------------------

*eject
*-----------------------------------------------------------------------
*       Teil 1 :  Tabellen      ( BEGIN OF ... OCCURS )
*-----------------------------------------------------------------------
* ----- Tabelle der bereits angezeigten Auftragskoepfe -----------------
*         (Um redundante Ausgeben bei größerer max. Zugriffstiefe
*         zu vermeiden)
DATA:    BEGIN OF ANZ_KOP OCCURS 500,
           MANDT             LIKE VBKRED-MANDT,   " Mandant
           KKBER             LIKE VBKRED-KKBER,   " Kreditkontrollbereic
           VBELN             LIKE VBKRED-VBELN,   " Beleg
         END   OF ANZ_KOP.

* ----- Tabelle der bereits angezeigten Auftragspositionen -------------
*         (Um redundante Ausgeben bei größerer max. Zugriffstiefe
*         zu vermeiden)
DATA:    BEGIN OF ANZ_POS OCCURS 500,
           MANDT             LIKE VBKRED-MANDT,   " Mandant
           KKBER             LIKE VBKRED-KKBER,   " Kreditkontrollbereic
           VBELN             LIKE VBKRED-VBELN,   " Beleg
           POSNR             LIKE VBKRED-POSNR,   " Position
         END   OF ANZ_POS.

* ----- Tabelle der Ausgabefelder und ihrer Positionen -----------------
DATA:    BEGIN OF AUSTAB OCCURS 20,
           ZEINR             LIKE T180S-ZEINR,   " Zeile bei mehrz. Ausg
           FNAME             LIKE T180S-FNAME,   " Name des Feldes
           OFFSE(3)          TYPE N,   " Offset in Ausgabezeil
           LENGT             LIKE T180S-LENGT,   " Ausgabelänge
         END   OF AUSTAB.

* ----- Tabelle der Selektionseingaben ---------------------------------
DATA:    BEGIN OF EINTAB OCCURS 6.
        INCLUDE STRUCTURE RKASE.
DATA:    END   OF EINTAB.

* ----- Tabelle der Selektionseingaben vom Einstiegsbild ---------------
DATA:    BEGIN OF EINTAB_EIN OCCURS 6.
        INCLUDE STRUCTURE RKASE.
DATA:    END   OF EINTAB_EIN.

* ----- Tabelle der nichterlaubten OKCODES -----------------------------
DATA:    BEGIN OF EXCTAB OCCURS 1,                          " 8801
           OKCOD(4)          TYPE C,                        "
         END   OF EXCTAB.                                   "

* ----- Feldpositionen bei der Ausgabe der Zwischenzeile (bei Zw.summen)
DATA:    BEGIN OF FPOTAB OCCURS 5,
           POSIT(3)          TYPE N,   " Ausgabeposition
           INTTYPE           LIKE DD03P-INTTYPE, " interner Datentyp
           DATATYPE          LIKE DD03P-DATATYPE," interner Datentyp
           DECIMALS          LIKE DD03P-DECIMALS," interner Datentyp
           REFFIELD          LIKE DD03P-REFFIELD," interner Datentyp
           CONVEXIT          LIKE RKAFT-CONVEXIT," Konvertierungsroutine
         END   OF FPOTAB.

* ----- Temporär: Tabelle der SY-LSTAT-Informationen pro Liststufe -----
DATA:    BEGIN OF LSTTAB OCCURS 10,
           LSIND             LIKE SY-LSIND,   " Liststufe
           LSTAT(100)        TYPE C,   " SY-LSTAT       15014
         END   OF LSTTAB.

* ----- Einträge Protokoll ---------------------------------------------
DATA:    BEGIN OF NEWS OCCURS 10,      " Protokoll
           VBELN LIKE VBKRED-VBELN,
           TEXT(90),
           MSGTY LIKE SY-MSGTY,
           MSGID LIKE SY-MSGID,
           MSGNO LIKE SY-MSGNO.
DATA:    END   OF NEWS.

* ----- Tabelle der Selektions-Felder ----------------------------------
DATA:    BEGIN OF SEFTAB OCCURS 3.
        INCLUDE STRUCTURE RKASF.
DATA:    END   OF SEFTAB.
* ----- Tabelle der Selektions-Felder vom Einstiegsbild (Nachlesen)-----
DATA:    BEGIN OF SEFTAB_EIN OCCURS 3.
        INCLUDE STRUCTURE RKASF.
DATA:    END   OF SEFTAB_EIN.
* ----- Tabelle der Selektionskriterien --------------------------------
DATA:    BEGIN OF SELTAB OCCURS 6.
        INCLUDE STRUCTURE RKASK.
DATA:    END   OF SELTAB.
* ----- Tabelle der Selektionskriterien vom Einstiegsbild (Nachlesen)---
DATA:    BEGIN OF SELTAB_EIN OCCURS 6.
        INCLUDE STRUCTURE RKASK.
DATA:    END   OF SELTAB_EIN.

* ------ Tabelle der Feldnamen, nach denen sortiert werden kann --------
DATA:    BEGIN OF SORTAB OCCURS 10.
        INCLUDE STRUCTURE RKAST.
DATA:    END   OF SORTAB.
* ------ Tabelle der Feldnamen, nach denen summiert werden kann --------
DATA:    BEGIN OF SUMTAB OCCURS 10.
        INCLUDE STRUCTURE RKAST.
DATA:    END   OF SUMTAB.

* ------ Tabelle fuer die Verwaltung der 'Summier-Modi' ----------------
DATA:    BEGIN OF SUVTAB OCCURS 10,
           SUMKZ(1)          TYPE C,   " Summenkennzeichen
           KTEXT(10)         TYPE C,   " Feldname
         END   OF SUVTAB.

* ----- Tabelle der Ueberschriftszeilen -----------------------------
DATA:    BEGIN OF UEBTAB OCCURS 10,
           ZEINR             LIKE T180S-ZEINR,   " Zeile bei mehrz. Ausg
           LTEXT(134)                        ,   " Titelzeile
         END   OF UEBTAB.
* ------ Tabelle der Feldnamen, die im dyn. Window angeboten werden ----
DATA:    BEGIN OF WINTAB OCCURS 1.
        INCLUDE STRUCTURE RKAWT.       " Feldname
DATA:    END   OF WINTAB.

* ------ Dummy-Tabelle für Funktionsbaustein ---------------------------
DATA: BEGIN OF X OCCURS 1,
       DUMMY,
      END OF X.

*eject
*-----------------------------------------------------------------------
*       Teil 2 :  Strukturen    ( BEGIN OF ... )
*-----------------------------------------------------------------------
*------- Schluessel für Zugriff auf die interne Tabelle ANZ_POS--------
DATA:    BEGIN OF AUPOS,                                    " JW
           MANDT             LIKE VBKRED-MANDT,   " Mandant
           KKBER             LIKE VBKRED-KKBER,   " Kreditkontrollbereic
           VBELN             LIKE VBKRED-VBELN,   " Beleg
           POSNR             LIKE VBKRED-POSNR,   " Position
         END OF   AUPOS.
*------- Beim Suchen oder Summieren festgelegte Fixwerte --------------
DATA:    BEGIN OF FIX,
           WERT(40)          TYPE C,   " Im Split 'fixiert'
           WERT1(40)         TYPE C,   " Fixwert Stufe 1
           WERT2(40)         TYPE C,   " Fixwert Stufe 2
           WERT3(40)         TYPE C,   " Fixwert Stufe 3
         END   OF FIX.
*------- Felder zur internen Steuerung (IS) ---------------------------
DATA:    BEGIN OF IS,
           ACTIV(4)          TYPE C,   " Selektionssteuerung
           AKTIV(4)          TYPE C,   " Selektionssteuerung
           ENDE(3)           TYPE N,   " der Seite bei Eintrag
                                       "   Nr. .. aus AKTSUM
           NLEVL(1)          TYPE N,   " Next Level
           TABIX             LIKE SY-TABIX,      " akt. AKTSUM-Eintrag
           VARNR_AUSTAB      LIKE T180S-VARNR,   " Welche Var. in AUSTAB
           VARNR_UEBTAB      LIKE T180S-VARNR,   " Welche Var. in UEBTAB
                                                            " 6175
           DYWF1             LIKE T180S-FNAME,   " 1. Feld im dyn.Window
           SPLTL_UEBTAB(3)   TYPE N,   " Spaltelinks des DYWI
           DYWZE_UEBTAB      LIKE T180S-ZEINR,   " Ausgabezeile des DYWI
                                                            " 6979
         END   OF IS.

*------- Im SY-LSTAT gestackte Statusinformation (LS = List-Status) --*
*                                                                     *
*     Stelle  Inhalt        Bedeutung / Werte                         *
*     ------  ---------     -----------------                         *
*       1     PF-STATUS     Doku siehe RFPOSFS0, STATUS_SETZEN        *
*                                                                     *
*       2     Sortierung    X  = absteigende Sortierung               *
*                                                                     *
*     3 - 5   Variante                                                *
*                                                                     *
*     6 - 7   Cursorline    Zeilennummer des Cursors.                 *
*                                                                     *
*       8     Current level im Summiermodus.                          *
*                                                                     *
*       9     Next level    im Summiermodus.                          *
*                                                                     *
*    10 - 12  Startindex    im Summiermodus.                          *
*                                                                     *
*    13 - 15  Akt. Index    im Summiermodus.                          *
*                                                                     *
*      16     Art der Listüberschrift.                                *
*                 G = Grundliste
*                 M = Summieren
*                 O = Sortieren
*                 U = Suchen
*                 N = Nach Summen ( = wie bei Suchen )
*                                                                     *
*      17     Einheit, in der Summen gezeigt werden.                  *
*                 F = Fremdwährung (WRSHB, Default)
*                 H = Hauswährung (DMSHB)
*                 B = Bewertete Hauswährung (BWWRT)
*                                                                     *
*     18-19   Spaltelinks (für dyn. Window)                           *
*
*     20-21   Spalterechts (für dyn. Window)                          *
*                (könnte auch errechnet werden, vgl. IS-ENDE)
*
*     22-31   Feldname (des 1. Feldes im dyn. Window)
*
*      32     Liste mit Zwischentiteln?
*
*      33     Liste mit Zwischensummen?
*
*     34-43   Feldname (des 1. Sortierfeldes bei Zwischensummen)
*
*     44-53   Feldname (des 2. Sortierfeldes bei Zwischensummen)
*
*     54-63   Feldname (des 3. Sortierfeldes bei Zwischensummen)
*
*     64      Zeile, in der das Zusatzfeld steht                " 6175
*
*     65-74   Feldname (des 1. Sortierfeldes bei Zwischensummen) "15014
*
*     45-84   Feldname (des 2. Sortierfeldes bei Zwischensummen) "15014
*
*     85-94   Feldname (des 3. Sortierfeldes bei Zwischensummen) "15014
* ------------------------------------------------------------------- *
DATA:    BEGIN OF LS,
           LSTAT(1)          TYPE C,   " Status (CUA)
           XDESC(1)          TYPE C,   " Sort. absteigend?
           VARNR(3)          TYPE C,   " Zeilenaufbau
           CLINE(3)          TYPE N,   " Cursorzeile
           CLEVL(1)          TYPE N,   " Current Level
           NLEVL(1)          TYPE N,   " Next Level
           START(3)          TYPE N,   " der Seite bei Eintrag
                                       "   Nr. .. aus AKTSUM
           INDEX(9)          TYPE N,   " akt. Zeilennummer
           TITLE(1)          TYPE C,   " Art des Titels
           SUNIT(1)          TYPE C,   " Einheit (HW,FW,...)
           SPLTL(3)          TYPE N,   " Spaltelinks
           SPLTR(3)          TYPE N,   " Spalterechts
           DYWF1             LIKE T180S-FNAME,   " 1. Feld im dyn.Window
           ZTITL(1)          TYPE C,   " Liste mit Zw.titeln?
           ZSUMM(1)          TYPE C,   " Liste mit Zw.summen?
           SRTF1             LIKE T180S-FNAME,   " 1. Sortfeld (Zw.Sum)
           SRTF2             LIKE T180S-FNAME,   " 2. Sortfeld (Zw.Sum)
           SRTF3             LIKE T180S-FNAME,   " 3. Sortfeld (Zw.Sum)
           DYWZE             LIKE T180S-ZEINR,   " Zeile des dyn. Window
           SEFF1             LIKE T180S-FNAME,   " 1. Suchfeld  "15014
           SEFF2             LIKE T180S-FNAME,   " 2. Suchfeld  "15014
           SEFF3             LIKE T180S-FNAME,   " 3. suchfeld  "15014
         END   OF LS.

*------- Maximale Zugriffs(Lese)-Tiefe --------------------------------
DATA:    BEGIN OF MAX,
           ANZTP(1)          TYPE C    " Maximaler Anzeigetyp
                               VALUE '1',
           ZUART             LIKE VBCOM-ZUART    " Maximale Lesetiefe
                               VALUE 'D',
           NAME_DAZU         LIKE VBCOM-NAME_DAZU "Name gelesen?
                               VALUE ' ',
           STAT_DAZU         LIKE VBCOM-STAT_DAZU "STATUS gelesen? 6175
                               VALUE ' ',
         END   OF MAX.
*eject
*-----------------------------------------------------------------------
*       Teil 3 :  Einzelfelder
*-----------------------------------------------------------------------
DATA:   AUTH_VBUK_RC LIKE SY-SUBRC.
DATA:   AUTH_KNKK_RC LIKE SY-SUBRC.
DATA:   AUTH_MESSAGE_VBUK_SENT.
DATA:   AUTH_MESSAGE_KNKK_SENT.
DATA: BEGIN OF MESSAGE_KNKK_VAR1,
        KKBER LIKE VBKRED-KKBER,
        FIL1,
        SBGRP LIKE VBKRED-SBGRP,
        FIL2,
        CTLPC LIKE VBKRED-CTLPC,
      END   OF MESSAGE_KNKK_VAR1.

DATA:    SUBRC LIKE SY-SUBRC.
DATA:    ZAEHL        LIKE SY-TABIX.   " Postabindex JW
DATA:    HLP_CASE(1) VALUE ' '.        " Fallunterscheidung
                                       " für FORM ZEILE_VERAENDERN

DATA:    ACTION(1)           TYPE C,   " in Actionbar
         AFFLG(1)            TYPE C,   " Ausg.zeile gefuellt ?
         AKTYP               LIKE T185F-AKTYP    " Aktivität 3: Eint.)
                                  VALUE 'A',
         ANSWER(1)           TYPE C,   " Antwort POPUP_TO_DECI
         ANZGR(1)            TYPE C,   " Anzeigegruppe
         ANZQU               TYPE P,   " Anzahl qual. Posten
         ANZTP(1)            TYPE C.   " Anzeigetyp (1: Kopf,
DATA:    BLANKS(40)          TYPE C,   " BLANKS
         BTRGN(18)           TYPE C,   " Betrag nach Komma
         BTRGV(18)           TYPE C,   " Betrag vor Komma
         BETRAG(18)          TYPE C.   " Betrag in Char-Feld

DATA:    CHAR(100)           TYPE C,   " Hilfsfeld (allg.)
         CHART(134)          TYPE C,   " Hilfsfeld Topline
                                                            " 8940
         CHARW(100)          TYPE C,   " Hilfsfeld Wert ausgeb
                                                            " 8940
         CHAR2(2)            TYPE C,   " Hilfsf. offene/alle A
         CHAR3(16)           TYPE C,   " Hilfsfeld
         CHAR4(21)           TYPE C,   " Hilfsfeld
         CNT_EPWIN(2)        TYPE N,   " Einzelposten im
                                       "   Window (im SPLIT).
         CNT_FIELD           TYPE I,   " Anzahl Felder
         CNT_LNUMM           TYPE I,   " laufende Nummer
         CNT_WAEHR           TYPE I,   " Zeilen mit gleicher
                                       "   Waehrung.
         CRS_FIELD(30)       TYPE C,   " Cursor-Position
         CRS_OFFS(2)         TYPE C,   " Cursor-Offset
         CRS_LINE            LIKE SY-STEPL,      " Cursor-Zeile
         CRS_CUCOL           LIKE SY-STEPL,      " Cursor-Spalte
         CRS_LINEOLD         LIKE SY-STEPL.      " Cursor-Zeile gemerkt

DATA:    DATE                TYPE D,   " Datum
         DATUM               TYPE D,   " Datum (f. INDX)
         DEFAULTOPTION(1)    TYPE C,   " Vorschl. POPUP_TO_DEC
         DEZIMALZEICHEN(1)   TYPE C,   " Dezimalzeichen
         DUMMY(1)            TYPE C,   " Listzeile
         OLD_DUMMY(1)        TYPE C.   " Listzeile

DATA:    ERROR(1)            TYPE C,   " Kennz.: Error?
         EXFLG(1)            TYPE C.   " EXIT aus Schleife ?

DATA:    FELD(10)            TYPE C,   " Feldname
         FETYP(3)            TYPE C,   " Feldtypen
         FIRSTROW(1)         TYPE C,   " Erste Ausgabezeile?
         FNAME(21)           TYPE C.   " Feldname + Tabpraefix

DATA:    HILF1(134)          TYPE C,   " 1. Titelzeile
         HILF2(134)          TYPE C,   " 2. Titelzeile
         HILFZ(35)           TYPE C.   " Zwischensummen, Wert
                                                            " 8550
DATA:    I                   TYPE I,   " Schleifenzaehler
         INDEX               TYPE I.   " im STEP-LOOP


DATA:    K(1)                TYPE C.   " Platzhalter

DATA:    L(1)                TYPE C,   " Platzhalter
         LENG1(3)            TYPE N,   " Bez. in 1.Titelzeile
         LENG2(3)            TYPE N,   " akt. Feld
         LFDNR               TYPE P,   " Lfd. Nr. bei Ausgabe
         LSIND               LIKE SY-LSIND,   " Gewaehlter Listindex
                                                            " 9653
         LOOPC               TYPE I.   " Anzahl Zeilen in LOOP

DATA:    MCOBJ(4)            TYPE C.   " Matchcode  ehler

DATA:    N                   TYPE I,   " Schleifenzaehler
         NACHL(1)            TYPE C,   " Nachlesen noetig?
         NEFE1(5)            TYPE N,   " Rechenfeld
         NEFE2(5)            TYPE N.   " Rechenfeld

DATA:    OK-CODE(5)          TYPE C,   " OK-CODE
         O_ZUART             LIKE T180Z-ZUART.   " alte Zugriffsart

DATA:    PFELD(40)           TYPE C.   " Parkfeld (Inhalt)

DATA:    QUFLG(1)            TYPE C.   " Posten qualifiziert ?

DATA:    REFE(16)            TYPE P,   " Rechenfeld
         REFE1(16)           TYPE P,   " Rechenfeld
         REFE2(16)           TYPE P,   " Rechenfeld
         REFE3(16)           TYPE P,   " Rechenfeld
         REFE4(16)           TYPE P,   " Rechenfeld
         REFE5(16)           TYPE P DECIMALS 3,  " ... mit 3 Nachkommast
         REPORT              LIKE SY-REPID,      " Programmname
         RCODE(2)            TYPE P.   " Returncode

DATA:    SELKZFLAG           TYPE N,   " Eingabebereites Feld
         SPRACHE             LIKE SY-LANGU.      " Sprache (f. INDX)

DATA:    TABIX               LIKE SY-TABIX,      " SY-TABIX der POSTAB
         TCODE               LIKE SY-TCODE,      " Transaktions-Code
         TFILL               LIKE SY-TFILL,      " Eintraege in int. Tab
                                       "   nach DESCRIBE
         TOP01(134)          TYPE C,   " 1. Titelzeile " 8940
         TOP02(134)          TYPE C,   " 2. Titelzeile " 8940
         TOP03(134)          TYPE C,   " 3. Titelzeile " 8940
         TOP06(134)          TYPE C,   " 6. Titelzeile "YLX
         TOP07(134)          TYPE C,   " 7. Titelzeile "YLX
         TOPHILF(134)        TYPE C,   " Hilfsfeld für Strichz
         TOPDATE             TYPE D,   " Datum in TOP-OF-LINE
         TOPFLAG(1)          TYPE C,   " Überschriftszeile?
         OLD_TOPFLAG(1)      TYPE C,   " Überschriftszeile?
         TRVOG               LIKE T180Z-TRVOG.   " Transaktionsvorggrup
*        TZEILE(80)          TYPE C.             " Erste Titelzeile"8940

DATA:    VARID(8)            TYPE C.   " Varinten-ID

DATA:    WERTL(100)          TYPE C,   " Wert, wie auf Liste
                                       " ausgegeben    " 6979
         WNLVO(3)            TYPE N VALUE '07',  " Window Line von oben
         WNLBO(3)            TYPE N VALUE '13',  " Window Line bis oben
         WNLVU(3)            TYPE N VALUE '17',  " Window Line von unten
         WNLBU(3)            TYPE N VALUE '22'.  " Window Line bis unten

DATA:    XBACK(1)            TYPE C,   " Automatisches BACK?
                                                            " 10314
         XEXIT(1)            TYPE C,   " Abbrechen ?
         XFLAG(1)            TYPE C,   " allgemeines Flag
         XSPLT(1)            TYPE C,   " Split-screen = X
                                                            " 9329
         XSELK(1)            TYPE C,   " X=Selektieren
         XSTRT(3)            TYPE N.   " Rettfeld SU-START

DATA:    ZEILE(134)          TYPE C,   " Ausgabezeile
         ZEILE2(134)         TYPE C,   " Zweite Titelzeile
         ZEILE_DYWI(1)       TYPE N,   " Ausgabezeile DYWI
         ZEINR(1)            TYPE N,   " Nummer der Ausgabezei
         ZEILEN(3)           TYPE N.   " Anzahl Ausgabezeilen
*------- Felder fuer CALL SCREEN ---------------------------------------
DATA:    SPALTELINKS(3)      TYPE N,   " des dynam. Windows
         SPALTERECHTS(3)     TYPE N,   " des dynam. Windows
         ZEILEOBEN(3)        TYPE N,   " des dynam. Windows
         ZEILEUNTEN(3)       TYPE N.   " des dynam. Windows

*------- Status der Listzeile (bei Massenaenderung) --------------------
DATA:    STATUS(1)           TYPE C.   " Status der Listzeile
                                       " 1 = hell
                                       " 2 = dunkel, aenderb.
*------- Neue Felder fuer Guifizierung  nach 2.1d     ------------------
DATA:    RECHTS_ZWISCHENSUMME LIKE SY-CUCOL      " Ende Zwischensumme
         VALUE 82.
DATA:    LINKS_ZWISCHENSUMME  LIKE SY-CUCOL       "Anfang Zwischensumme
         VALUE 2.

DATA:    CHANGE_COLOR        TYPE C,   " Farbwechsel
         SPACESIZE           LIKE SY-LINSZ.      " Format ausblenden
DATA:    U_LAENGE            LIKE SY-TABIX.      " Überschriftslänge



*eject
*-----------------------------------------------------------------------
*       Teil 4 :  Konstanten
*-----------------------------------------------------------------------
DATA:    HNULL2(2)           TYPE X VALUE '0000', " Fuer Vergl. Begru
         NULL10(10)          TYPE C VALUE '0000000000',
         NUMERICS(11)        TYPE C VALUE ' 0123456789'.

DATA:    HEXNULL(1)          TYPE X VALUE '00',  " Hex. Null
         NULL(1)             TYPE C VALUE '0',   " Char. 0
         EINS(1)             TYPE C VALUE '1',   " Char. 1
         ZWEI(1)             TYPE C VALUE '2',   " Char. 2
         DREI(1)             TYPE C VALUE '3',   " Char. 3
         VIER(1)             TYPE C VALUE '4',   " Char. 4
         FUENF(1)            TYPE C VALUE '5',   " Char. 5
         SECHS(1)            TYPE C VALUE '6',   " Char. 6
         ON(1)               TYPE C VALUE '1',
         OFF(1)              TYPE C VALUE '0'.

DATA: UEBERSICHT VALUE 0,              "Ansprungmarken FD32
      ALLG_DATEN VALUE 1,
      KONTROLLBD VALUE 2.
*eject
*-----------------------------------------------------------------------
*       Teil 5 :  Field-Symbols
*-----------------------------------------------------------------------

FIELD-SYMBOLS:
         <BISWERT>,                    " Selektion bis ...
         <EWERTL>,                     " edit. Wert in Liste
                                                            " 8940
         <EWERTT>,                     " edit. Wert in Uebersc
                                                            " 8940
         <FELD1>, <FELD2>, <FELD3>,    " POSTAB-Felder
         <FIX1>,  <FIX2>,  <FIX3>,     " Fixierte Werte
         <LAENGE>,                     " Länge
         <POSITION>,                   " in AKTIV-Leiste
         <SUCHFELD>,                   " um Laenge zu modifiz.
         <TEXT>,                       " dazugeblendeter Text
         <TITEL>,                      " Titel in dyn. Window
         <TITEL1>, <TITEL2>,           " Titelzeile
         <ULINE>,                      " Unterstriche
         <VONWERT>,                    " Selektion von ...
         <WAERS>,                      " Waehrungsschl.
         <WERT1>,                      " Wert bei Einzelsel.
         <WERT2>,                      " Wert bei Einzelsel.
         <NUMMER1>,                    " Schlüssel1 Topline
         <NUMMER2>,                    " Schlüssel2 Topline
         <STRUKTUR>,                   "K11K026370    " Ausgabestruktur
         <WIND1>.                      " Zusatzfeld1

DATA:
      HLP_ANSWER,                      "Antwort aus Popup_to_confirm
      FLG_WEITER.                      "Antwort aus sichern_notwendig

DATA: SELKZ LIKE POSTAB-SELKZ.         " Selektionsfeld
DATA: HIDE_SELKZ LIKE POSTAB-SELKZ.    " Selektionsfeld

* Felder für dynamische Ausgabe:

DATA: POS TYPE I,                      "Position
      LEN TYPE I,                      "Länge
      COL TYPE I,                      "Farbe
      INT TYPE I.                      "Intensität

DATA: COL_ABSAGEN      TYPE I VALUE 6,
      COL_FREIGEBEN    TYPE I VALUE 5,
      COL_PRUEFEN      TYPE I VALUE 3,
      COL_WEITERLEITEN  TYPE I VALUE 4,
      COL_NORMAL        TYPE I VALUE 2.

DATA: GV_MEMORY_ID LIKE SY-REPID VALUE 'MV75AST0'.
DATA  FLG_KKL.    "Entscheidungspapameter, ob Hierarchie oder Einfach
*----------------- Deklarationen für ALV ------------------------------*
CONSTANTS:
KR_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOPLINE_STANDARD'.
*--------------------------- DATA TO BE DISPLAY -----------------------*
DATA: K_REPID LIKE SY-REPID VALUE 'RVKRED01',
      KR_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.  "Listenkopftabelle
DATA:        K_SAVE(1) TYPE C,
             KR_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV, "Feldkatalogtabelle
             KR_LAYOUT   TYPE SLIS_LAYOUT_ALV,      "Layoutstruktur
             KR_KEYINFO  TYPE SLIS_KEYINFO_ALV,
             KR_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV,  "Gruppentabelle
             KR_EVENTS   TYPE SLIS_T_EVENT,         "Ereignistabelle
        K_TABNAME_HEADER TYPE SLIS_TABNAME VALUE 'POSTAB',
        K_TABNAME_ITEM   TYPE SLIS_TABNAME VALUE 'XVBKREDET', "Hierach
             K_DEFAULT(1) TYPE C,
             K_EXIT(1) TYPE C,         "Importpara. für F4 FB
             KR_VARIANT LIKE DISVARIANT,     "Anzeigevariantestrucktur
             K_VARIANT LIKE DISVARIANT,"Variant_init.
             K_STATUS TYPE SLIS_FORMNAME VALUE 'STANDARD_KR01',
             K_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.

data: begin of gt_col_lege occurs 0.
           include structure rvkred01_alv.
data:  end of   gt_col_lege .
