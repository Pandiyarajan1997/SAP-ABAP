*&---------------------------------------------------------------------*
*& Subroutine Pool   YMM_PO_DOMESTIC_PRNT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

PROGRAM YMM_PO_DOMESTIC_PRNT.

TYPE-POOLS:   ADDI, MEEIN,
              MMPUR.
TABLES: NAST,                          "Messages
        *NAST,                         "Messages
        TNAPR,                         "Programs & Forms
        ITCPO,                         "Communicationarea for Spool
        ARC_PARAMS,                    "Archive parameters
        TOA_DARA,                      "Archive parameters
        ADDR_KEY.                      "Adressnumber for ADDRESS

TYPE-POOLS SZADR.
TABLES : VARPOSR,
         RWERTER,
         MTXH.

DATA  UMBRUCH TYPE I VALUE 4.
DATA  HEADERFLAG.
DATA  BEGIN OF VARTAB OCCURS 15.
        INCLUDE STRUCTURE VARPOSR.
DATA  END   OF VARTAB.
DATA  TABN TYPE I.
DATA  TABA TYPE I.
DATA  EBELPH LIKE EKPO-EBELP.
DATA  BIS TYPE I.
DATA  XMAX TYPE I.
DATA  TAB LIKE VARPOSR-YZEILE.
DATA  DIFF TYPE I.
DATA  LDAT_SAM LIKE EKET-EINDT.

DATA: S, V TYPE I.
DATA: SAMPR LIKE PEKPOV-NETPR, VARPR LIKE PEKPOV-NETPR.

* Struktur zur Variantenbildung

DATA: BEGIN OF WERTETAB OCCURS 30.
        INCLUDE STRUCTURE RWERTER.
DATA: ATZHL LIKE ECONF_OUT-ATZHL,
      END OF WERTETAB.

* Interne Tabelle fuer Konditionen
DATA: BEGIN OF KOND OCCURS 30.
        INCLUDE STRUCTURE KOMVD.
DATA: END OF KOND.
* Hilfsfelder
DATA:
      MERKNAMEX(15) TYPE C,            "Merkmalname x-Achse
      MERKNAMEY(15) TYPE C,            "Merkmalname y-Achse
      MERKNRX LIKE RWERTER-NR,         "Int. Merkmal x-Achse
      MERKNRY LIKE RWERTER-NR,         "Int. Merkmal y-Achse
      I TYPE I VALUE 1,
      NR LIKE CAWN-ATINN.
DATA: INSERTERROR(1),SUM TYPE I,MENGE TYPE I,GSUMH TYPE I, XMAXH TYPE I.
DATA: GSUMV TYPE I.
* Matrixflag
DATA: M_FLAG VALUE 'x'.

*- Tabellen -----------------------------------------------------------*
TABLES: CPKME,
        EKVKP,
        EKKO,
        PEKKO,
        RM06P,
        EKPO,
        PEKPO,
        PEKPOV,
        PEKPOS,
        EKET,
        EKEK,
        EKES,
        EKEH,
        EKKN,
        EKPA,
        EKBE,
        EINE, *EINE,
        LFA1,
        LIKP,
       *LFA1,
        KNA1,
        KOMK,
        KOMP,
        KOMVD,
        EKOMD,
        ECONF_OUT,
        THEAD, *THEAD,
        SADR,
        MDPA,
        MDPM,
        MKPF,
        TINCT,
        TTXIT,
        TMSI2,
        TQ05,
        TQ05T,
        T001,
        T001W,
        T006, *T006,
        T006A, *T006A,
        T024,
        T024E,
        T027A,
        T027B,
        T052,
        T161N,
        T163D,
        T166A,
        T165P,
        T166C,
        T166K,
        T166P,
        T166T,
        T166U,
        T165M,
        T165A,
        TMAMT,
       *MARA,                                               "HTN 4.0C
        MARA,
        MARC,
        MT06E,
        MAKT,
        VBAK,
        VBKD,
       *VBKD,
        VBAP.
TABLES: DRAD,
        DRAT.
TABLES: ADDR1_SEL,
        ADDR1_VAL.
TABLES: V_HTNM, RAMPL,TMPPF.           "HTN-Abwicklung

TABLES: STXH.              "schnellerer Zugriff auf Texte Dienstleistung

TABLES: T161.              "Abgebotskennzeichen für Dienstleistung

*- INTERNE TABELLEN ---------------------------------------------------*
*- Tabelle der Positionen ---------------------------------------------*
DATA: BEGIN OF XEKPO OCCURS 10.
        INCLUDE STRUCTURE EKPO.
DATA:     BSMNG LIKE EKES-MENGE,
      END OF XEKPO.

*- Key für xekpo ------------------------------------------------------*
DATA: BEGIN OF XEKPOKEY,
         MANDT LIKE EKPO-MANDT,
         EBELN LIKE EKPO-EBELN,
         EBELP LIKE EKPO-EBELP,
      END OF XEKPOKEY.

*- Tabelle der Einteilungen -------------------------------------------*
DATA: BEGIN OF XEKET OCCURS 10.
        INCLUDE STRUCTURE EKET.
DATA:     FZETE LIKE PEKPO-WEMNG,
      END OF XEKET.

*- Tabelle der Einteilungen temporär ----------------------------------*
DATA: BEGIN OF TEKET OCCURS 10.
        INCLUDE STRUCTURE BEKET.
DATA: END OF TEKET.

DATA: BEGIN OF ZEKET.
        INCLUDE STRUCTURE EKET.
DATA:  END OF ZEKET.

*- Tabelle der Positionszusatzdaten -----------------------------------*
DATA: BEGIN OF XPEKPO OCCURS 10.
        INCLUDE STRUCTURE PEKPO.
DATA: END OF XPEKPO.

*- Tabelle der Positionszusatzdaten -----------------------------------*
DATA: BEGIN OF XPEKPOV OCCURS 10.
        INCLUDE STRUCTURE PEKPOV.
DATA: END OF XPEKPOV.

*- Tabelle der Zahlungbedingungen--------------------------------------*
DATA: BEGIN OF ZBTXT OCCURS 5,
         LINE(50),
      END OF ZBTXT.

*- Tabelle der Merkmalsausprägungen -----------------------------------*
DATA: BEGIN OF TCONF_OUT OCCURS 50.
        INCLUDE STRUCTURE ECONF_OUT.
DATA: END OF TCONF_OUT.

*- Tabelle der Konditionen --------------------------------------------*
DATA: BEGIN OF TKOMV OCCURS 50.
        INCLUDE STRUCTURE KOMV.
DATA: END OF TKOMV.

DATA: BEGIN OF TKOMK OCCURS 1.
        INCLUDE STRUCTURE KOMK.
DATA: END OF TKOMK.

DATA: BEGIN OF TKOMVD OCCURS 50.       "Belegkonditionen
        INCLUDE STRUCTURE KOMVD.
DATA: END OF TKOMVD.

DATA: BEGIN OF TEKOMD OCCURS 50.       "Stammkonditionen
        INCLUDE STRUCTURE EKOMD.
DATA: END OF TEKOMD.

*- Tabelle der Bestellentwicklung -------------------------------------*
DATA: BEGIN OF XEKBE OCCURS 10.
        INCLUDE STRUCTURE EKBE.
DATA: END OF XEKBE.

*- Tabelle der Bezugsnebenkosten --------------------------------------*
DATA: BEGIN OF XEKBZ OCCURS 10.
        INCLUDE STRUCTURE EKBZ.
DATA: END OF XEKBZ.

*- Tabelle der WE/RE-Zuordnung ----------------------------------------*
DATA: BEGIN OF XEKBEZ OCCURS 10.
        INCLUDE STRUCTURE EKBEZ.
DATA: END OF XEKBEZ.

*- Tabelle der Positionssummen der Bestellentwicklung -----------------*
DATA: BEGIN OF TEKBES OCCURS 10.
        INCLUDE STRUCTURE EKBES.
DATA: END OF TEKBES.

*- Tabelle der Bezugsnebenkosten der Bestandsführung ------------------*
DATA: BEGIN OF XEKBNK OCCURS 10.
        INCLUDE STRUCTURE EKBNK.
DATA: END OF XEKBNK.

*- Tabelle für Kopieren Positionstexte (hier wegen Infobestelltext) ---*
DATA: BEGIN OF XT165P OCCURS 10.
        INCLUDE STRUCTURE T165P.
DATA: END OF XT165P.

*- Tabelle der Kopftexte ----------------------------------------------*
DATA: BEGIN OF XT166K OCCURS 10.
        INCLUDE STRUCTURE T166K.
DATA: END OF XT166K.

*- Tabelle der Positionstexte -----------------------------------------*
DATA: BEGIN OF XT166P OCCURS 10.
        INCLUDE STRUCTURE T166P.
DATA: END OF XT166P.

*- Tabelle der Anahngstexte -------------------------------------------*
DATA: BEGIN OF XT166A OCCURS 10.
        INCLUDE STRUCTURE T166A.
DATA: END OF XT166A.

*- Tabelle der Textheader ---------------------------------------------*
DATA: BEGIN OF XTHEAD OCCURS 10.
        INCLUDE STRUCTURE THEAD.
DATA: END OF XTHEAD.

DATA: BEGIN OF XTHEADKEY,
         TDOBJECT LIKE THEAD-TDOBJECT,
         TDNAME LIKE THEAD-TDNAME,
         TDID LIKE THEAD-TDID,
      END OF XTHEADKEY.

DATA: BEGIN OF QM_TEXT_KEY OCCURS 5,
         TDOBJECT LIKE THEAD-TDOBJECT,
         TDNAME LIKE THEAD-TDNAME,
         TDID LIKE THEAD-TDID,
         TDTEXT LIKE TTXIT-TDTEXT,
      END OF QM_TEXT_KEY.

*- Tabelle der Nachrichten alt/neu ------------------------------------*
DATA: BEGIN OF XNAST OCCURS 10.
        INCLUDE STRUCTURE NAST.
DATA: END OF XNAST.

DATA: BEGIN OF YNAST OCCURS 10.
        INCLUDE STRUCTURE NAST.
DATA: END OF YNAST.

*------ Struktur zur Übergabe der Adressdaten --------------------------
DATA:    BEGIN OF ADDR_FIELDS.
        INCLUDE STRUCTURE SADRFIELDS.
DATA:    END OF ADDR_FIELDS.

*------ Struktur zur Übergabe der Adressreferenz -----------------------
DATA:    BEGIN OF ADDR_REFERENCE.
        INCLUDE STRUCTURE ADDR_REF.
DATA:    END OF ADDR_REFERENCE.

*------ Tabelle zur Übergabe der Fehler -------------------------------
DATA:    BEGIN OF ERROR_TABLE OCCURS 10.
        INCLUDE STRUCTURE ADDR_ERROR.
DATA:    END OF ERROR_TABLE.

*------ Tabelle zur Übergabe der Adressgruppen ------------------------
DATA:    BEGIN OF ADDR_GROUPS OCCURS 3.
        INCLUDE STRUCTURE ADAGROUPS.
DATA:    END OF ADDR_GROUPS.

*- Tabelle der Aenderungsbescheibungen --------------------------------*
DATA: BEGIN OF XAEND OCCURS 10,
         EBELP LIKE EKPO-EBELP,
         ZEKKN LIKE EKKN-ZEKKN,
         ETENR LIKE EKET-ETENR,
         CTXNR LIKE T166C-CTXNR,
         ROUNR LIKE T166C-ROUNR,
         INSERT,
         FLAG_ADRNR,
      END OF XAEND.

DATA: BEGIN OF XAENDKEY,
         EBELP LIKE EKPO-EBELP,
         ZEKKN LIKE EKKN-ZEKKN,
         ETENR LIKE EKET-ETENR,
         CTXNR LIKE T166C-CTXNR,
         ROUNR LIKE T166C-ROUNR,
         INSERT,
         FLAG_ADRNR,
      END OF XAENDKEY.

*- Tabelle der Textänderungen -----------------------------------------*
DATA: BEGIN OF XAETX OCCURS 10,
         EBELP LIKE EKPO-EBELP,
         TEXTART LIKE CDSHW-TEXTART,
         CHNGIND LIKE CDSHW-CHNGIND,
      END OF XAETX.

*- Tabelle der geänderten Adressen ------------------------------------*
DATA: BEGIN OF XADRNR OCCURS 5,
         ADRNR LIKE SADR-ADRNR,
         TNAME LIKE CDSHW-TABNAME,
         FNAME LIKE CDSHW-FNAME,
      END OF XADRNR.

*- Tabelle der gerade bearbeiteten aktive Komponenten -----------------*
DATA BEGIN OF MDPMX OCCURS 10.
        INCLUDE STRUCTURE MDPM.
DATA END OF MDPMX.

*- Tabelle der gerade bearbeiteten Sekundärbedarfe --------------------*
DATA BEGIN OF MDSBX OCCURS 10.
        INCLUDE STRUCTURE MDSB.
DATA END OF MDSBX.

*- Struktur des Archivobjekts -----------------------------------------*
DATA: BEGIN OF XOBJID,
        OBJKY  LIKE NAST-OBJKY,
        ARCNR  LIKE NAST-OPTARCNR,
      END OF XOBJID.

* Struktur für zugehörigen Sammelartikel
DATA: BEGIN OF SEKPO.
        INCLUDE STRUCTURE EKPO.
DATA:   FIRST_VARPOS,
      END OF SEKPO.

*- Struktur für Ausgabeergebnis zB Spoolauftragsnummer ----------------*
DATA: BEGIN OF RESULT.
        INCLUDE STRUCTURE ITCPP.
DATA: END OF RESULT.

*- Struktur für Internet NAST -----------------------------------------*
DATA: BEGIN OF INTNAST.
        INCLUDE STRUCTURE SNAST.
DATA: END OF INTNAST.

*- HTN-Abwicklung
DATA: BEGIN OF HTNMAT OCCURS 0.
        INCLUDE STRUCTURE V_HTNM.
DATA:  REVLV LIKE RAMPL-REVLV,
      END OF HTNMAT.

DATA  HTNAMP LIKE RAMPL  OCCURS 0 WITH HEADER LINE.

*- Hilfsfelder --------------------------------------------------------*
DATA: HADRNR(8),                       "Key TSADR
      ELEMENTN(30),                    "Name des Elements
      SAVE_EL(30),                     "Rettfeld für Element
      RETCO LIKE SY-SUBRC,             "Returncode Druck
      INSERT,                          "Kz. neue Position
      H-IND LIKE SY-TABIX,             "Hilfsfeld Index
      H-IND1 LIKE SY-TABIX,            "Hilfsfeld Index
      F1 TYPE F,                       "Rechenfeld
      H-MENGE LIKE EKPO-MENGE,         "Hilfsfeld Mengenumrechnung
      H-MENG1 LIKE EKPO-MENGE,         "Hilfsfeld Mengenumrechnung
      H-MENG2 LIKE EKPO-MENGE,         "Hilfsfeld Mengenumrechnung
      AB-MENGE LIKE EKES-MENGE,        "Hilfsfeld bestätigte Menge
      KZBZG LIKE KONP-KZBZG,           "Staffeln vorhanden?
      HDATUM LIKE EKET-EINDT,          "Hilfsfeld Datum
      HMAHNZ LIKE EKPO-MAHNZ,          "Hilfsfeld Mahnung
      ADDRESSNUM LIKE EKPO-ADRN2,      "Hilfsfeld Adressnummer
      TABLINES LIKE SY-TABIX,          "Zähler Tabelleneinträge
      ENTRIES  LIKE SY-TFILL,          "Zähler Tabelleneinträge
      HSTAP,                           "statistische Position
      HSAMM,                           "Positionen mit Sammelartikel
      HLOEP,                           "Gelöschte Positionen im Spiel
      HKPOS,                           "Kondition zu löschen
      KOPFKOND,                        "Kopfkonditionen vorhanden
      NO_ZERO_LINE,                    "keine Nullzeilen
      XDRFLG LIKE T166P-DRFLG,         "Hilfsfeld Textdruck
      XPROTECT,                        "Kz. protect erfolgt
      ARCHIV_OBJECT LIKE TOA_DARA-AR_OBJECT, "für opt. Archivierung
      TEXTFLAG,                        "Kz. druckrel. Positionstexte
      FLAG,                            "allgemeines Kennzeichen
      SPOOLID(10),                     "Spoolidnummer
      XPROGRAM LIKE SY-REPID,          "Programm
      LVS_RECIPIENT LIKE SWOTOBJID,    "Internet
      LVS_SENDER LIKE SWOTOBJID,       "Internet
      TIMEFLAG,                        "Kz. Uhrzeit bei mind. 1 Eint.
      H_VBELN LIKE VBAK-VBELN,
      H_VBELP LIKE VBAP-POSNR.

*- Drucksteuerung -----------------------------------------------------*
DATA: AENDERNSRV.
DATA: XDRUVO.                          "Druckvorgang
DATA: NEU  VALUE '1',                  "Neudruck
      AEND VALUE '2',                  "Änderungsdruck
      MAHN VALUE '3',                  "Mahnung
      ABSA VALUE '4',                  "Absage
      LPET VALUE '5',                  "Lieferplaneinteilung
      LPMA VALUE '6',                  "Mahnung Lieferplaneinteilung
      AUFB VALUE '7',                  "Auftragsbestätigung
      LPAE VALUE '8',                  "Änderung Lieferplaneinteilung
      LPHE VALUE '9',                  "Historisierte Einteilungen
      PREISDRUCK,                      "Kz. Gesamtpreis drucken
      KONTRAKT_PREIS,                  "Kz. Kontraktpreise drucken
      WE   VALUE 'E'.                  "Wareneingangswert

*- Hilfsfelder Lieferplaneinteilung -----------------------------------*
DATA:
      XLPET,                           "Lieferplaneinteilung
      XFZ,                             "Fortschrittszahlendarstellung
      XOFFEN,                          "offene WE-Menge
      XLMAHN,                          "Lieferplaneinteilungsmahnung
      FZFLAG,                          "KZ. Abstimmdatum erreicht
      XNOAEND,                         "keine Änderungsbelege da  LPET
      XETDRK,                        "Druckrelevante Positionen da LPET
      XETEFZ LIKE EKET-MENGE,          "Einteilungsfortschrittszahl
      XWEMFZ LIKE EKET-MENGE,          "Lieferfortschrittszahl
      XABRUF LIKE EKEK-ABRUF,          "Alter Abruf
      P_ABART LIKE EKEK-ABART.         "Abrufart

*data: sum-euro-price like komk-fkwrt.                       "302203
DATA: SUM-EURO-PRICE LIKE KOMK-FKWRT_EURO.                  "302203
DATA: EURO-PRICE LIKE EKPO-EFFWR.

*- Hilfsfelder für Ausgabemedium --------------------------------------*
DATA: XDIALOG,                         "Kz. POP-UP
      XSCREEN,                         "Kz. Probeausgabe
      XFORMULAR LIKE TNAPR-FONAM,      "Formular
      XDEVICE(10).                     "Ausgabemedium

*- Hilfsfelder für QM -------------------------------------------------*
DATA: QV_TEXT_I LIKE TQ09T-KURZTEXT,   "Bezeichnung Qualitätsvereinb.
      TL_TEXT_I LIKE TQ09T-KURZTEXT,   "Bezeichnung Technische Lieferb.
      ZG_KZ.                           "Zeugnis erforderlich

*- Hilfsfelder für Änderungsbeleg -------------------------------------*
DATA: OBJECTID              LIKE CDHDR-OBJECTID,
      TCODE                 LIKE CDHDR-TCODE,
      PLANNED_CHANGE_NUMBER LIKE CDHDR-PLANCHNGNR,
      UTIME                 LIKE CDHDR-UTIME,
      UDATE                 LIKE CDHDR-UDATE,
      USERNAME              LIKE CDHDR-USERNAME,
      CDOC_PLANNED_OR_REAL  LIKE CDHDR-CHANGE_IND,
      CDOC_UPD_OBJECT       LIKE CDHDR-CHANGE_IND VALUE 'U',
      CDOC_NO_CHANGE_POINTERS LIKE CDHDR-CHANGE_IND.


*- Common-Part für Änderungsbeleg -------------------------------------*
*include zzfm06lccd.
DATA:    BEGIN OF COMMON PART FM06LCCD.

*------- Tabelle der Änderunsbelegzeilen (temporär) -------------------*
DATA: BEGIN OF EDIT OCCURS 50.             "Änderungsbelegzeilen temp.
        INCLUDE STRUCTURE CDSHW.
DATA: END OF EDIT.

DATA: BEGIN OF EDITD OCCURS 50.             "Änderungsbelegzeilen temp.
        INCLUDE STRUCTURE CDSHW.            "für Dienstleistungen
DATA: END OF EDITD.


*------- Tabelle der Änderunsbelegzeilen (Ausgabeform) ----------------*
DATA: BEGIN OF AUSG OCCURS 50.             "Änderungsbelegzeilen
        INCLUDE STRUCTURE CDSHW.
DATA:   CHANGENR LIKE CDHDR-CHANGENR,
        UDATE    LIKE CDHDR-UDATE,
        UTIME    LIKE CDHDR-UTIME,
      END OF AUSG.

*------- Tabelle der Änderunsbelegköpfe -------------------------------*
DATA: BEGIN OF ICDHDR OCCURS 50.           "Änderungbelegköpfe
        INCLUDE STRUCTURE CDHDR.
DATA: END OF ICDHDR.

*------- Key Tabelle der Änderunsbelegköpfe --------------------------*
DATA: BEGIN OF HKEY,                       "Key für ICDHDR
        MANDT LIKE CDHDR-MANDANT,
        OBJCL LIKE CDHDR-OBJECTCLAS,
        OBJID LIKE CDHDR-OBJECTID,
        CHANG LIKE CDHDR-CHANGENR,
      END OF HKEY.

*------- Key der geänderten Tabelle für Ausgabe ----------------------*
DATA: BEGIN OF EKKEY,                    "Tabellenkeyausgabe
        EBELN LIKE EKKO-EBELN,
        EBELP LIKE EKPO-EBELP,
        ZEKKN LIKE EKKN-ZEKKN,
        ETENR LIKE EKET-ETENR,
        ABRUF LIKE EKEK-ABRUF,
        EKORG LIKE EKPA-EKORG,           "Änderungsbelege Partner
        LTSNR LIKE EKPA-LTSNR,           "Änderungsbelege Partner
        WERKS LIKE EKPA-WERKS,           "Änderungsbelege Partner
        PARVW LIKE EKPA-PARVW,           "Änderungsbelege Partner
        PARZA LIKE EKPA-PARZA,           "Änderungsbelege Partner
        CONSNUMBER LIKE ADR2-CONSNUMBER, "Änderungsbelege Adressen
        COMM_TYPE  LIKE ADRT-COMM_TYPE,  "Änderungsbelege Adressen
      END OF EKKEY.

DATA:    END OF COMMON PART.
*- Direktwerte --------------------------------------------------------*
************************************************************************
*          Direktwerte                                                 *
************************************************************************
*------- Werte zu Trtyp und Aktyp:
CONSTANTS:  HIN VALUE 'H',             "Hinzufuegen
            VER VALUE 'V',             "Veraendern
            ANZ VALUE 'A',             "Anzeigen
            ERW VALUE 'E'.             "Bestellerweiterung

CONSTANTS:
* BSTYP
  BSTYP-INFO VALUE 'I',
  BSTYP-ORDR VALUE 'W',
  BSTYP-BANF VALUE 'B',
  BSTYP-BEST VALUE 'F',
  BSTYP-ANFR VALUE 'A',
  BSTYP-KONT VALUE 'K',
  BSTYP-LFPL VALUE 'L',
  BSTYP-LERF VALUE 'Q',

* BSAKZ
  BSAKZ-NORM VALUE ' ',
  BSAKZ-TRAN VALUE 'T',
  BSAKZ-RAHM VALUE 'R',
* BSAKZ-BEIS VALUE 'B',  "not used
* BSAKZ-KONS VALUE 'K',  "not used
* BSAKZ-LOHN VALUE 'L', "not used
* BSAKZ-STRE VALUE 'S', "not used
* BSAKZ-MENG VALUE 'M', "not used
* BSAKZ-WERT VALUE 'W', "not used
* PSTYP
  PSTYP-LAGM VALUE '0',
  PSTYP-BLNK VALUE '1',
  PSTYP-KONS VALUE '2',
  PSTYP-LOHN VALUE '3',
  PSTYP-MUNB VALUE '4',
  PSTYP-STRE VALUE '5',
  PSTYP-TEXT VALUE '6',
  PSTYP-UMLG VALUE '7',
  PSTYP-WAGR VALUE '8',
  PSTYP-DIEN VALUE '9',

* Kzvbr
  KZVBR-ANLA VALUE 'A',
  KZVBR-UNBE VALUE 'U',
  KZVBR-VERB VALUE 'V',
  KZVBR-EINZ VALUE 'E',
  KZVBR-PROJ VALUE 'P',

* ESOKZ
  ESOKZ-PIPE VALUE 'P',
  ESOKZ-LOHN VALUE '3',
  ESOKZ-KONSI VALUE '2',               "konsi
  ESOKZ-CHARG VALUE '1',               "sc-jp
  ESOKZ-NORM VALUE '0'.

CONSTANTS:
* Handling von Unterpositionsdaten
       SIHAN-NIX  VALUE ' ',           "keine eigenen Daten
       SIHAN-ANZ  VALUE '1', "Daten aus Hauptposition kopiert, nicht änd
       SIHAN-KOP  VALUE '2', "Daten aus Hauptposition kopiert, aber ände
       SIHAN-EIG  VALUE '3'. "eigene Daten (nicht aus Hauptposition kopi

* Unterpositionstypen
CONSTANTS:
  UPTYP-HPO VALUE ' ',                 "Hauptposition
  UPTYP-VAR VALUE '1',                 "Variante
  UPTYP-NRI VALUE '2',           "Naturalrabatt Inklusive (=Dreingabe)
  UPTYP-LER VALUE '3',                 "Leergut
  UPTYP-NRE VALUE '4',           "Naturalrabatt Exklusive (=Draufgabe)
  UPTYP-LOT VALUE '5',                 "Lot Position
  UPTYP-DIS VALUE '6',                 "Display Position
  UPTYP-VKS VALUE '7',                 "VK-Set Position
  UPTYP-MPN VALUE '8',                 "Austauschposition (A&D)
  UPTYP-SLS VALUE '9',           "Vorkommisionierungsposition (retail)
  UPTYP-DIV VALUE 'X'.           "HP hat UP's mit verschiedenen Typen

* Artikeltypen
CONSTANTS:
  ATTYP-SAM(2) VALUE '01',             "Sammelartikel
  ATTYP-VAR(2) VALUE '02',             "Variante
  ATTYP-WE1(2) VALUE '20',             "Wertartikel
  ATTYP-WE2(2) VALUE '21',             "Wertartikel
  ATTYP-WE3(2) VALUE '22',             "Wertartikel
  ATTYP-VKS(2) VALUE '10',             "VK-Set
  ATTYP-LOT(2) VALUE '11',             "Lot-Artikel
  ATTYP-DIS(2) VALUE '12'.             "Display

* Konfigurationsherkunft
CONSTANTS:
  KZKFG-FRE VALUE ' ',                 "Konfiguration sonst woher
  KZKFG-KAN VALUE '1',                 "noch nicht konfiguriert
  KZKFG-EIG VALUE '2'.                 "Eigene Konfiguration

CONSTANTS:
  C_JA   TYPE C VALUE 'X',
  C_NEIN TYPE C VALUE ' '.

* Vorgangsart, welche Anwendung den Fkt-Baustein aufruft
CONSTANTS:
  CVA_AB(1) VALUE 'B',     "Automatische bestellung (aus banfen)
  CVA_WE(1) VALUE 'C',                 "Wareneingang
  CVA_BU(1) VALUE 'D',     "Übernahme bestellungen aus fremdsystem
  CVA_AU(1) VALUE 'E',                 "Aufteiler
  CVA_KB(1) VALUE 'F',                 "Kanban
  CVA_FA(1) VALUE 'G',                 "Filialauftrag
  CVA_DR(1) VALUE 'H',                                      "DRP
  CVA_EN(1) VALUE '9',                 "Enjoy
  CVA_AP(1) VALUE '1',                                      "APO
  CVA_ED(1) VALUE 'T'.     "EDI-Eingang Auftragsbestätigung Update Preis

* Status des Einkaufsbeleges (EKKO-STATU)
CONSTANTS:
  CKS_AG(1) VALUE 'A',                 "Angebot vorhanden für Anfrage
  CKS_AB(1) VALUE 'B',     "Automatische Bestellung (aus Banfen) ME59
  CKS_WE(1) VALUE 'C',                 "Bestellung aus Wareneingang
  CKS_BU(1) VALUE 'D',                 "Bestellung aus Datenübernahme
  CKS_AU(1) VALUE 'E',     "Bestellung aus Aufteiler (IS-Retail)
  CKS_KB(1) VALUE 'F',                 "Bestellung aus Kanban
  CKS_FA(1) VALUE 'G',     "Bestellung aus Filialauftrag (IS-Retail)
  CKS_DR(1) VALUE 'H',                 "Bestellung aus DRP
  CKS_BA(1) VALUE 'I',                 "Bestellung aus BAPI
  CKS_AL(1) VALUE 'J',                 "Bestellung aus ALE-Szenario
  CKS_SB(1) VALUE 'S',                 "Sammelbestellung (IS-Retail)
  CKS_AP(1) VALUE '1',                                      "APO
  CKS_EN(1) VALUE '9',                 "Enjoy Bestellung
  CKS_FB(1) VALUE 'X'.                 "Bestellung aus Funktionsbaustein

* Vorgang aus T160
CONSTANTS:
  VORGA-ANGB(2) VALUE 'AG',   "Angebot zur Anfrage    ME47, ME48
  VORGA-LPET(2) VALUE 'LE',   "Lieferplaneinteilung   ME38, ME39
  VORGA-FRGE(2) VALUE 'EF',   "Einkaufsbelegfreigabe  ME28, ME35, ME45
  VORGA-FRGB(2) VALUE 'BF',   "Banffreigabe           ME54, ME55
  VORGA-BGEN(2) VALUE 'BB',            "Best. Lief.unbekannt   ME25
  VORGA-ANHA(2) VALUE 'FT',   "Textanhang             ME24, ME26,...
  VORGA-BANF(2) VALUE 'B ',   "Banf                   ME51, ME52, ME53
  VORGA-ANFR(2) VALUE 'A ',   "Anfrage                ME41, ME42, ME43
  VORGA-BEST(2) VALUE 'F ',   "Bestellung             ME21, ME22, ME23
  VORGA-KONT(2) VALUE 'K ',   "Kontrakt               ME31, ME32, ME33
  VORGA-LFPL(2) VALUE 'L ',   "Lieferplan             ME31, ME32, ME33
  VORGA-MAHN(2) VALUE 'MA',            "Liefermahnung          ME91
  VORGA-AUFB(2) VALUE 'AB'.            "Bestätigungsmahnung    ME92

* Felder für Feldauswahl (früher FMMEXCOM)
DATA:       ENDMASKE(210) TYPE C,
            KMASKE(140) TYPE C,
            AUSWAHL0 TYPE BREFN,
            AUSWAHL1 TYPE BREFN,
            AUSWAHL2 TYPE BREFN,
            AUSWAHL3 TYPE BREFN,
            AUSWAHL4 TYPE BREFN,
            AUSWAHL5 TYPE BREFN,
            AUSWAHL6 TYPE BREFN.

* Sonderbestandskennzeichen
CONSTANTS:
  SOBKZ-KDEIN VALUE 'E',               "Kundeneinzel
  SOBKZ-PREIN VALUE 'Q',               "Projekteinzel
  SOBKZ-LOHNB VALUE 'O'.               "Lohnbearbeiterbeistell

* Min-/Maxwerte für Datenelemente
CONSTANTS:
* offener Rechnungseingangswert / Feldlänge: 13 / Dezimalstellen: 2
  C_MAX_OREWR       LIKE RM06A-OREWR   VALUE '99999999999.99',
  C_MAX_OREWR_F     TYPE F             VALUE '99999999999.99',
  C_MAX_OREWR_X(15) TYPE C             VALUE '**************',

  C_MAX_PROZ_P(3)   TYPE P DECIMALS 2  VALUE '999.99',      "@80545
  C_MAX_PROZ_X(6)   TYPE C             VALUE '******',      "@80545

  C_MAX_MENGE       LIKE EKPO-MENGE  VALUE '9999999999.999', "@83886
  C_MAX_MENGE_F     TYPE F           VALUE '9999999999.999', "@83886

  C_MAX_NETWR       LIKE EKPO-NETWR  VALUE '99999999999.99', "@83886
  C_MAX_NETWR_F     TYPE F           VALUE '99999999999.99'. "@83886


* Distribution Indicator Account assignment
CONSTANTS:
  C_DIST_IND-SINGLE   VALUE ' ',       "no multiple = single
  C_DIST_IND-QUANTITY VALUE '1',       "quantity distribution
  C_DIST_IND-PERCENT  VALUE '2'.       "percentag

* Datendefinitionen für Dienstleistungen
TABLES: ESLH,
        ESLL,
        ML_ESLL,
        RM11P.

DATA  BEGIN OF GLIEDERUNG OCCURS 50.
        INCLUDE STRUCTURE ML_ESLL.
DATA  END   OF GLIEDERUNG.

DATA  BEGIN OF LEISTUNG OCCURS 50.
        INCLUDE STRUCTURE ML_ESLL.
DATA  END   OF LEISTUNG.

DATA  RETURN.

*- interne Tabelle für Abrufköpfe -------------------------------------*
DATA: BEGIN OF XEKEK          OCCURS 20.
        INCLUDE STRUCTURE IEKEK.
DATA: END OF XEKEK.

*- interne Tabelle für Abrufköpfe alt----------------------------------*
DATA: BEGIN OF PEKEK          OCCURS 20.
        INCLUDE STRUCTURE IEKEK.
DATA: END OF PEKEK.

*- interne Tabelle für Abrufeinteilungen ------------------------------*
DATA: BEGIN OF XEKEH          OCCURS 20.
        INCLUDE STRUCTURE IEKEH.
DATA: END OF XEKEH.

*- interne Tabelle für Abrufeinteilungen ------------------------------*
DATA: BEGIN OF TEKEH          OCCURS 20.
        INCLUDE STRUCTURE IEKEH.
DATA: END OF TEKEH.

*- Zusatztabelle Abruf nicht vorhanden XEKPO---------------------------*
DATA: BEGIN OF XEKPOABR OCCURS 20,
         MANDT LIKE EKPO-MANDT,
         EBELN LIKE EKPO-EBELN,
         EBELP LIKE EKPO-EBELP,
      END OF XEKPOABR.

*-- Daten Hinweis 39234 -----------------------------------------------*
*- Hilfstabelle Einteilungen ------------------------------------------*
DATA: BEGIN OF HEKET OCCURS 10.
        INCLUDE STRUCTURE EKET.
DATA:       TFLAG LIKE SY-CALLD,
      END OF HEKET.

*- Key für HEKET ------------------------------------------------------*
DATA: BEGIN OF HEKETKEY,
         MANDT LIKE EKET-MANDT,
         EBELN LIKE EKET-EBELN,
         EBELP LIKE EKET-EBELP,
         ETENR LIKE EKET-ETENR,
      END OF HEKETKEY.

DATA: H_SUBRC LIKE SY-SUBRC,
      H_TABIX LIKE SY-TABIX,
      H_FIELD LIKE CDSHW-F_OLD,
      H_EINDT LIKE RVDAT-EXTDATUM.
DATA  Z TYPE I.

* Defintionen für Formeln

TYPE-POOLS MSFO.

DATA: VARIABLEN TYPE MSFO_TAB_VARIABLEN WITH HEADER LINE.

DATA: FORMEL TYPE MSFO_FORMEL.

* Definition für Rechnungsplan

DATA: TFPLTDR LIKE FPLTDR OCCURS 0 WITH HEADER LINE.

DATA: FPLTDR LIKE FPLTDR.

* Definiton Defaultschema für Dienstleistung

CONSTANTS: DEFAULT_KALSM LIKE T683-KALSM VALUE 'MS0000',
           DEFAULT_KALSM_STAMM LIKE T683-KALSM VALUE 'MS0001'.

DATA: BSTYP LIKE EKKO-BSTYP,
      BSART LIKE EKKO-BSART.


DATA DKOMK LIKE KOMK.

* Defintion für Wartungsplan
TABLES: RMIPM.

DATA: MPOS_TAB LIKE MPOS OCCURS 0 WITH HEADER LINE,
      ZYKL_TAB LIKE MMPT OCCURS 0 WITH HEADER LINE.

DATA: PRINT_SCHEDULE.

DATA: BEGIN OF D_TKOMVD OCCURS 50.
        INCLUDE STRUCTURE KOMVD.
DATA: END OF D_TKOMVD.
DATA: BEGIN OF D_TKOMV OCCURS 50.
        INCLUDE STRUCTURE KOMV.
DATA: END OF D_TKOMV.


* Definition Drucktabellen blockweises Lesen

DATA: LEISTUNG_THEAD LIKE STXH OCCURS 1 WITH HEADER LINE.
DATA: GLIEDERUNG_THEAD LIKE STXH OCCURS 1 WITH HEADER LINE. "HS

DATA: BEGIN OF THEAD_KEY,
        MANDT    LIKE SY-MANDT,
        TDOBJECT LIKE STXH-TDOBJECT,
        TDNAME   LIKE STXH-TDNAME,
        TDID     LIKE STXH-TDID,
        TDSPRAS  LIKE STXH-TDSPRAS.
DATA: END OF THEAD_KEY.

RANGES: R1_TDNAME FOR STXH-TDNAME,
        R2_TDNAME FOR STXH-TDNAME.

DATA: BEGIN OF DOKTAB OCCURS 0.
        INCLUDE STRUCTURE DRAD.
DATA  DKTXT LIKE DRAT-DKTXT.
DATA: END OF DOKTAB.

*  Additionals Tabelle (CvB/4.0c)
DATA: L_ADDIS_IN_ORDERS TYPE LINE OF ADDI_BUYING_PRINT_ITAB
                                        OCCURS 0 WITH HEADER LINE.
*  Die Additionals-Strukturen müssen bekannt sein
TABLES: WTAD_BUYING_PRINT_ADDI,"#EC CI_USAGE_OK[2371631]
   WTAD_BUYING_PRINT_EXTRA_TEXT."#EC CI_USAGE_OK[2371631]
"Added by SPLABAP during code remediation

DATA: LS_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ.
DATA: LS_BIL_INVOICE TYPE LBBIL_INVOICE.
DATA: LF_FM_NAME            TYPE RS38L_FNAM.
DATA: LS_CONTROL_PARAM      TYPE SSFCTRLOP.
DATA: LS_COMPOSER_PARAM     TYPE SSFCOMPOP.
DATA: LS_RECIPIENT          TYPE SWOTOBJID.
DATA: LS_SENDER             TYPE SWOTOBJID.
DATA: LF_FORMNAME           TYPE TDSFNAME.
DATA: LS_ADDR_KEY           LIKE ADDR_KEY,
      DUNWITHEKET           TYPE XFELD.

DATA: L_ZEKKO                  LIKE EKKO,
      L_XPEKKO                 LIKE PEKKO,
      L_XEKPO                LIKE TABLE OF EKPO,
      L_WA_XEKPO             LIKE EKPO.

DATA: L_XEKPA LIKE EKPA OCCURS 0,
      L_WA_XEKPA LIKE EKPA.
DATA: L_XPEKPO  LIKE PEKPO OCCURS 0,
      L_WA_XPEKPO LIKE PEKPO,
      L_XEKET   LIKE TABLE OF EKET WITH HEADER LINE,
      L_XEKKN  LIKE TABLE OF EKKN WITH HEADER LINE,
      L_XEKEK  LIKE TABLE OF EKEK WITH HEADER LINE,
      L_XEKEH   LIKE TABLE OF EKEH WITH HEADER LINE,
      L_XKOMK LIKE TABLE OF KOMK WITH HEADER LINE,
      L_XTKOMV  TYPE KOMV OCCURS 0,
      L_WA_XTKOMV TYPE KOMV.


DATA   LS_SSFCOMPOP  TYPE     SSFCOMPOP.
*----------------------------------------------------------------------*
* Subroutines for the Print Program
*----------------------------------------------------------------------*
*INCLUDE /SMBA0/AA_FM06PE02.
*&---------------------------------------------------------------------*
*&  Include           /SMBA0/AA_FM06PE02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT_ADDRESS
*&---------------------------------------------------------------------*
FORM GET_PLANT_ADDRESS USING    P_WERKS LIKE T001W-WERKS
                       CHANGING P_ADRNR
                                P_SADR  LIKE SADR.
* parameter P_ADRNR without type since there are several address
* fields with different domains

  DATA: L_EKKO LIKE EKKO,
        L_ADDRESS LIKE ADDR1_VAL.

  CHECK NOT P_WERKS IS INITIAL.
  L_EKKO-RESWK = P_WERKS.
  L_EKKO-BSAKZ = 'T'.
  CALL FUNCTION 'MM_ADDRESS_GET'
    EXPORTING
      I_EKKO    = L_EKKO
    IMPORTING
      E_ADDRESS = L_ADDRESS
      E_SADR    = P_SADR.
  P_ADRNR = L_ADDRESS-ADDRNUMBER.

ENDFORM.                    " GET_PLANT_ADDRESS

*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMER_ADDRESS
*&---------------------------------------------------------------------*
FORM GET_CUSTOMER_ADDRESS USING    P_KUNNR LIKE EKPO-KUNNR
                          CHANGING P_ADRNR.
* parameter P_ADRNR without type since there are several address
* fields with different domains

  DATA: L_ADRNR LIKE KNA1-ADRNR.

  CHECK NOT P_KUNNR IS INITIAL.
  SELECT SINGLE ADRNR FROM  KNA1 INTO (L_ADRNR)
         WHERE  KUNNR  = P_KUNNR.
  IF SY-SUBRC EQ 0.
    P_ADRNR = L_ADRNR.
  ELSE.
    CLEAR P_ADRNR.
  ENDIF.

ENDFORM.                    " GET_CUSTOMER_ADDRESS

*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR_ADDRESS
*&---------------------------------------------------------------------*
FORM GET_VENDOR_ADDRESS USING    P_EMLIF LIKE LFA1-LIFNR
                        CHANGING P_ADRNR.
* parameter P_ADRNR without type since there are several address
* fields with different domains

  DATA: L_LFA1 LIKE LFA1.

  CHECK NOT P_EMLIF IS INITIAL.
  CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_00'
    EXPORTING
      I_LFA1_LIFNR     = P_EMLIF
      I_DATA           = 'X'
      I_PARTNER        = ' '
    IMPORTING
      A_LFA1           = L_LFA1
    EXCEPTIONS
      VENDOR_NOT_FOUND = 1.
  IF SY-SUBRC EQ 0.
    P_ADRNR = L_LFA1-ADRNR.
  ELSE.
    CLEAR P_ADRNR.
  ENDIF.

ENDFORM.                               " GET_VENDOR_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  get_addr_key
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_BIL_INVOICE_HD_ADR  text
*      <--P_CS_ADDR_KEY  text
*----------------------------------------------------------------------*
FORM GET_ADDR_KEY
     USING    L_XEKKO LIKE EKKO
     CHANGING L_ADDR_KEY LIKE ADDR_KEY.

  DATA: L_LFA1 LIKE LFA1.

  IF L_XEKKO-LIFNR NE SPACE.
    CALL FUNCTION 'MM_ADDRESS_GET'
      EXPORTING
        I_EKKO = L_XEKKO
      IMPORTING
        E_SADR = SADR
      EXCEPTIONS
        OTHERS = 1.
    MOVE-CORRESPONDING SADR TO L_LFA1.
    IF SY-SUBRC = 0.
      MOVE L_LFA1-ADRNR TO L_ADDR_KEY-ADDRNUMBER.
    ENDIF.
  ENDIF.

ENDFORM.                               " get_addr_key
*&---------------------------------------------------------------------*
*&      Form  protocol_update_I
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROTOCOL_UPDATE_I.
  CHECK XSCREEN = SPACE.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      MSG_ARBGB = SYST-MSGID
      MSG_NR    = SYST-MSGNO
      MSG_TY    = SYST-MSGTY
      MSG_V1    = SYST-MSGV1
      MSG_V2    = SYST-MSGV2
      MSG_V3    = SYST-MSGV3
      MSG_V4    = SYST-MSGV4
    EXCEPTIONS
      OTHERS    = 1.
ENDFORM.                               " protocol_update_I
*&---------------------------------------------------------------------*
*&      Form  add_smfrm_prot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_SMFRM_PROT.
  DATA: LT_ERRORTAB             TYPE TSFERROR.
  DATA: LF_MSGNR                TYPE SY-MSGNO.
  DATA:  L_S_LOG          TYPE BAL_S_LOG,
         P_LOGHANDLE      TYPE BALLOGHNDL,
       L_S_MSG          TYPE BAL_S_MSG.
  FIELD-SYMBOLS: <FS_ERRORTAB>  TYPE LINE OF TSFERROR.
* get smart form protocoll
  CALL FUNCTION 'SSF_READ_ERRORS'
    IMPORTING
      ERRORTAB = LT_ERRORTAB.
  SORT LT_ERRORTAB.
* delete adjacent duplicates from lt_errortab comparing errnumber.
* add smartform protocoll to nast protocoll
  LOOP AT LT_ERRORTAB ASSIGNING <FS_ERRORTAB>.
    CLEAR LF_MSGNR.
    LF_MSGNR = <FS_ERRORTAB>-ERRNUMBER.
    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
      EXPORTING
        MSG_ARBGB = <FS_ERRORTAB>-MSGID
        MSG_NR    = LF_MSGNR
        MSG_TY    = <FS_ERRORTAB>-MSGTY
        MSG_V1    = <FS_ERRORTAB>-MSGV1
        MSG_V2    = <FS_ERRORTAB>-MSGV2
        MSG_V3    = <FS_ERRORTAB>-MSGV3
        MSG_V4    = <FS_ERRORTAB>-MSGV4
      EXCEPTIONS
        OTHERS    = 1.
  ENDLOOP.

* open the application log
  L_S_LOG-EXTNUMBER    = SY-UNAME.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      I_S_LOG      = L_S_LOG
    IMPORTING
      E_LOG_HANDLE = P_LOGHANDLE
    EXCEPTIONS
      OTHERS       = 1.
  IF SY-SUBRC <> 0.
  ENDIF.

  LOOP AT LT_ERRORTAB ASSIGNING <FS_ERRORTAB>.
    MOVE-CORRESPONDING <FS_ERRORTAB> TO L_S_MSG.
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        I_LOG_HANDLE = P_LOGHANDLE
        I_S_MSG      = L_S_MSG
      EXCEPTIONS
        OTHERS       = 1.
    IF SY-SUBRC <> 0.
    ENDIF.
  ENDLOOP.

** Function module to display error logs during
** smart form processing
** Notice , the function 'BAL_DSP_LOG_DISPLAY' can
** not be used when you using output dispatch time
** 4 (Send immediately), so the statement is comment
** out by default.

** You can enable the function call statement
** if your form can not be output and you want to
** see the error log. Set output dispatch time to 3
** before save your order, then print or preview the
** output.
  DATA LV_DEBUG.
  IF NOT LV_DEBUG IS INITIAL.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'.
  ENDIF.


ENDFORM.                               " add_smfrm_prot

*&--------------------------------------------------------------------*
*&      Form  ENTRY_NEU
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*

FORM ENTRY_NEU USING ENT_RETCO ENT_SCREEN.

  DATA: L_DRUVO LIKE T166K-DRUVO.
  DATA: LS_XFZ  TYPE CHAR1.

  XSCREEN = ENT_SCREEN.

  CLEAR ENT_RETCO.
  IF NAST-AENDE EQ SPACE.
    L_DRUVO = '1'.
  ELSE.
    L_DRUVO = '2'.
  ENDIF.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF XSCREEN EQ 'X' AND ( NAST-TDARMOD EQ '2' OR NAST-TDARMOD EQ '3').
    NAST-TDARMOD = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM PROCESSING_PO USING NAST LS_XFZ
                     CHANGING ENT_RETCO ENT_SCREEN L_DRUVO.

ENDFORM.                    "entry_neu

*&--------------------------------------------------------------------*
*&      Form  ENTRY_MAHN
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM ENTRY_MAHN USING ENT_RETCO ENT_SCREEN.

  DATA: L_DRUVO LIKE T166K-DRUVO.
  DATA: LS_XFZ  TYPE CHAR1.

  XSCREEN = ENT_SCREEN.

  L_DRUVO = '3'.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF XSCREEN EQ 'X' AND ( NAST-TDARMOD EQ '2' OR NAST-TDARMOD EQ '3').
    NAST-TDARMOD = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM PROCESSING_PO USING NAST LS_XFZ
                     CHANGING ENT_RETCO ENT_SCREEN L_DRUVO.

ENDFORM.                    "entry_mahn
*&--------------------------------------------------------------------*
*&      Form  ENTRY_AUFB
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*

FORM ENTRY_AUFB USING ENT_RETCO ENT_SCREEN.

  DATA: L_DRUVO LIKE T166K-DRUVO.
  DATA: LS_XFZ  TYPE CHAR1.

  XSCREEN = ENT_SCREEN.

  L_DRUVO = '7'.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF XSCREEN EQ 'X' AND ( NAST-TDARMOD EQ '2' OR NAST-TDARMOD EQ '3').
    NAST-TDARMOD = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM PROCESSING_PO USING NAST LS_XFZ
                     CHANGING ENT_RETCO ENT_SCREEN L_DRUVO.

ENDFORM.                    "entry_aufb

*&--------------------------------------------------------------------*
*&      Form  ENTRY_ABSA
*&--------------------------------------------------------------------*
* RFQ: Quotation Rejection
* Angebotsabsage
*----------------------------------------------------------------------*
FORM ENTRY_ABSA USING ENT_RETCO ENT_SCREEN.

*********************************************
  DATA: L_DRUVO LIKE T166K-DRUVO.
  DATA: LS_XFZ  TYPE CHAR1.

  XSCREEN = ENT_SCREEN.
  L_DRUVO = '4'.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF XSCREEN EQ 'X' AND ( NAST-TDARMOD EQ '2' OR NAST-TDARMOD EQ '3').
    NAST-TDARMOD = '1'.
  ENDIF.
*<<< End of Insertion <<<
  PERFORM PROCESSING_PO
          USING     NAST
                    LS_XFZ
          CHANGING  ENT_RETCO
                    ENT_SCREEN
                    L_DRUVO.

ENDFORM.                    "entry_absa

*&--------------------------------------------------------------------*
*&      Form  ENTRY_LPET
*&--------------------------------------------------------------------*
* Delivery Schedule: Traditional DLS type, without forecast or JIT
* Lieferplaneinteilung
*----------------------------------------------------------------------*
FORM ENTRY_LPET USING ENT_RETCO ENT_SCREEN.

  DATA: L_DRUVO LIKE T166K-DRUVO.
  DATA: LS_XFZ  TYPE CHAR1.

  CLEAR ENT_RETCO.
  CLEAR LS_XFZ.

  IF NAST-AENDE EQ SPACE.
    L_DRUVO = '5'.
  ELSE.
    L_DRUVO = '8'.
  ENDIF.

  XSCREEN = ENT_SCREEN.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF XSCREEN EQ 'X' AND ( NAST-TDARMOD EQ '2' OR NAST-TDARMOD EQ '3').
    NAST-TDARMOD = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM PROCESSING_PO
          USING     NAST
                    LS_XFZ
          CHANGING  ENT_RETCO
                    ENT_SCREEN
                    L_DRUVO.

ENDFORM.                    "entry_lpet

*&--------------------------------------------------------------------*
*&      Form  ENTRY_LPMA
*&--------------------------------------------------------------------*
* Dlivery Schedule: Urging/Reminder
* Mahnung
FORM ENTRY_LPMA USING ENT_RETCO ENT_SCREEN.

  DATA: L_DRUVO LIKE T166K-DRUVO.
  DATA: LS_XFZ  TYPE CHAR1.

  CLEAR ENT_RETCO.
  L_DRUVO = '6'.

  XSCREEN = ENT_SCREEN.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF XSCREEN EQ 'X' AND ( NAST-TDARMOD EQ '2' OR NAST-TDARMOD EQ '3').
    NAST-TDARMOD = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM PROCESSING_PO
          USING     NAST
                    LS_XFZ
          CHANGING  ENT_RETCO
                    ENT_SCREEN
                    L_DRUVO.

ENDFORM.                    "entry_lpma
*&--------------------------------------------------------------------*
*&      Form  ENTRY_LPHE_CD
*&--------------------------------------------------------------------*
* Delivery Schedule: Forecast delivery without accumnlation value
FORM ENTRY_LPHE_CD USING ENT_RETCO ENT_SCREEN.

  DATA: L_DRUVO LIKE T166K-DRUVO.
  DATA: LS_XFZ  TYPE CHAR1.

  CLEAR ENT_RETCO.
  L_DRUVO = '9'.

  XSCREEN = ENT_SCREEN.
*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF XSCREEN EQ 'X' AND ( NAST-TDARMOD EQ '2' OR NAST-TDARMOD EQ '3').
    NAST-TDARMOD = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM PROCESSING_PO
          USING     NAST
                    LS_XFZ
          CHANGING  ENT_RETCO
                    ENT_SCREEN
                    L_DRUVO.


ENDFORM.                    "entry_lphe_cd

*&--------------------------------------------------------------------*
*&      Form  ENTRY_LPJE_CD
*&--------------------------------------------------------------------*
* Delivery Schedule: JIT delivery without accumnlation value
FORM ENTRY_LPJE_CD USING ENT_RETCO ENT_SCREEN.

  DATA: L_DRUVO LIKE T166K-DRUVO.
  DATA: LS_XFZ  TYPE CHAR1.

  CLEAR ENT_RETCO.
  L_DRUVO = 'A'.

** Get print preview indicator from import parameter
  XSCREEN = ENT_SCREEN.

** if it is print preview, Print Archiving mode should be set to
** "Print Only"
  IF XSCREEN EQ 'X' AND ( NAST-TDARMOD EQ '2' OR NAST-TDARMOD EQ '3').
    NAST-TDARMOD = '1'.
  ENDIF.

  PERFORM PROCESSING_PO
          USING     NAST
                    LS_XFZ
          CHANGING  ENT_RETCO
                    ENT_SCREEN
                    L_DRUVO.

ENDFORM.                    "entry_lpje_cd

*&--------------------------------------------------------------------*
*&      Form  PROCESSING_PO
*&--------------------------------------------------------------------*
* Purchase Order Processing Subroutine
FORM PROCESSING_PO
                USING IV_NAST TYPE NAST
                      IV_XFZ  TYPE CHAR1
                CHANGING ENT_RETCO
                         ENT_SCREEN
                         IV_DRUVO TYPE T166K-DRUVO.
  DATA: LS_PRINT_DATA_TO_READ TYPE LBBIL_PRINT_DATA_TO_READ.
  DATA: LS_BIL_INVOICE TYPE LBBIL_INVOICE.
  DATA: LF_FM_NAME            TYPE RS38L_FNAM.
  DATA: LS_CONTROL_PARAM      TYPE SSFCTRLOP.
  DATA: LS_COMPOSER_PARAM     TYPE SSFCOMPOP.
  DATA: LS_RECIPIENT          TYPE SWOTOBJID.
  DATA: LS_SENDER             TYPE SWOTOBJID.
  DATA: LF_FORMNAME           TYPE TDSFNAME.
  DATA: LS_ADDR_KEY           LIKE ADDR_KEY.
  DATA: LS_JOB_INFO           TYPE SSFCRESCL.
  DATA: L_SPOOLID             TYPE RSPOID.

  DATA: L_FROM_MEMORY,
        L_DOC   TYPE MEEIN_PURCHASE_DOC_PRINT,
        L_NAST  LIKE NAST.



  CALL FUNCTION 'ME_READ_PO_FOR_PRINTING'
    EXPORTING
      IX_NAST        = IV_NAST
      IX_SCREEN      = ENT_SCREEN
    IMPORTING
      EX_RETCO       = ENT_RETCO
      EX_NAST        = L_NAST
      DOC            = L_DOC
    CHANGING
      CX_DRUVO       = IV_DRUVO
      CX_FROM_MEMORY = L_FROM_MEMORY.
  CHECK ENT_RETCO EQ 0.

  L_NAST-AENDE = IV_NAST-AENDE.

***********************
  DATA LV_NAST LIKE NAST.
  IF IV_DRUVO EQ AEND OR IV_DRUVO EQ LPAE.
    SELECT DATVR UHRVR INTO (LV_NAST-DATVR, LV_NAST-UHRVR) FROM NAST
             WHERE KAPPL EQ IV_NAST-KAPPL
               AND KSCHL EQ IV_NAST-KSCHL
               AND OBJKY EQ IV_NAST-OBJKY
               AND VSTAT EQ '1'
       ORDER BY DATVR DESCENDING UHRVR DESCENDING.
      EXIT.
    ENDSELECT.

* If no NEU-Message could be found above, we read any NEU-message of
* that purchase document and use the latest timestamp of these messages
* for the fields *nast-datvr and *nast-uhrvr.
    IF SY-SUBRC NE 0.                                   "Begin of 549924
      DATA: NEU_MESSAGETYPES LIKE T161M OCCURS 0 WITH HEADER LINE.
      DATA: BEGIN OF TIME_TAB OCCURS 0,
              DATVR LIKE NAST-DATVR,
              UHRVR LIKE NAST-UHRVR,
            END OF TIME_TAB.

* Read all messagetypes that are allowed for NEU-Messages
      SELECT * FROM T161M INTO TABLE NEU_MESSAGETYPES
        WHERE KVEWE EQ 'B'
        AND   KAPPL EQ IV_NAST-KAPPL
        AND   KSCHL NE IV_NAST-KSCHL
        AND   DRUVO EQ NEU.
      IF SY-SUBRC EQ 0.
        LOOP AT NEU_MESSAGETYPES.
          SELECT DATVR UHRVR INTO TIME_TAB FROM NAST
             WHERE KAPPL EQ IV_NAST-KAPPL
               AND KSCHL EQ NEU_MESSAGETYPES-KSCHL
               AND OBJKY EQ IV_NAST-OBJKY
               AND VSTAT EQ '1'
             ORDER BY DATVR DESCENDING UHRVR DESCENDING.
            EXIT.
          ENDSELECT.
          IF SY-SUBRC EQ 0.
            APPEND TIME_TAB.
          ENDIF.
        ENDLOOP.
        SORT TIME_TAB BY DATVR DESCENDING UHRVR DESCENDING.
        READ TABLE TIME_TAB INDEX 1.
        IF SY-SUBRC EQ 0.
          LV_NAST-DATVR = TIME_TAB-DATVR.
          LV_NAST-UHRVR = TIME_TAB-UHRVR.
        ENDIF.
      ENDIF.
    ENDIF.
    L_NAST-DATVR = LV_NAST-DATVR.
    L_NAST-UHRVR = LV_NAST-UHRVR.
  ENDIF.
************************************************************************************************************
  IF IV_NAST-ADRNR IS INITIAL.
    PERFORM GET_ADDR_KEY
      USING L_DOC-XEKKO
      CHANGING LS_ADDR_KEY.
  ELSE.
    LS_ADDR_KEY = IV_NAST-ADRNR.
  ENDIF.

* Fill up pricing condition table if calling from ME9F
  IF L_DOC-XTKOMV IS INITIAL.
    DATA : IT_PRCD_ELEMENTS TYPE TABLE OF PRCD_ELEMENTS.
    SELECT * INTO TABLE IT_PRCD_ELEMENTS FROM PRCD_ELEMENTS " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
                                     WHERE KNUMV = L_DOC-XEKKO-KNUMV.
    MOVE-CORRESPONDING IT_PRCD_ELEMENTS TO l_doc-xtkomv.
  ENDIF.
*=======================================================================================================================================
*          Below Codes Added by shanmugam
*=======================================================================================================================================
  DATA :  IT_PO   TYPE TABLE OF ZMM_PO_STRUC WITH  HEADER LINE.
  DATA :  WA_PO   TYPE  ZMM_PO_STRUC .
*==================================================
*       Added by Govind ON 03.04.2013
*===================================================
  DATA : WA_EKKO TYPE EKKO.
  DATA : WA_KONV TYPE PRCD_ELEMENTS. " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
  DATA : WA_EKBE TYPE EKBE.
  DATA : WA_RG23D TYPE J_1IRG23D.
  DATA : WA_EXCDTL TYPE J_1IEXCDTL.
*====================================================
  DATA :  IT_EKPO TYPE TABLE OF EKPO  WITH HEADER LINE.
  DATA :  WA_KONP TYPE KONP.
  DATA :  WA1_KONP TYPE KONP.
  DATA :  GKWERT  TYPE KWERT.
  DATA :  GKWERT1  TYPE KWERT.
  DATA :  DKWERT  TYPE decfloat34 .
  DATA :  DKWERT1  TYPE decfloat34 .
  DATA :  B_DUTY  TYPE KWERT.
  DATA :  B_DUTY1 TYPE KWERT.
  DATA :  B_DUTY2 TYPE KWERT.
  DATA :  B_DUTY3 TYPE KWERT.
  DATA :  B_DUTY4 TYPE KWERT.
  DATA :  B_DUTY5 TYPE KWERT.
  DATA :  B_DUTY6 TYPE KWERT.
  DATA :  BEXDUT  TYPE KWERT.
  DATA :  GTOTAL  TYPE KWERT.
  DATA :  NTOTAL TYPE KWERT.
  DATA :  GPERN   TYPE KWERT.
*  DATA :  GPERN1   TYPE KWERT. " Added By Govind
  DATA :  GCALC   TYPE KWERT.
  DATA :  VATCST  TYPE KWERT.
  DATA :  VKWERT  TYPE KWERT.
*  DATA :  VKWERT1  TYPE KWERT. " Added By Govind
  DATA :  GNETWR  TYPE NETWR.
  DATA :  GKSCHL  TYPE KSCHL.
  DATA :  WA_A363 TYPE A363.
  DATA :  WA_A969 TYPE A969.
  DATA : WA_A998 TYPE A998 .
  DATA : WA1_A998 TYPE A998 .
  DATA : WA2_A998 TYPE A998 .
  DATA :  WA_A359 TYPE A359.
  DATA :  WA1_A359 TYPE A359.
  DATA :  WA_MAKT TYPE MAKT.
  DATA :  SYDAT TYPE SY-DATUM.

  DATA :  B_DUTY11 TYPE KWERT.
  DATA :  B_DUTY12 TYPE KWERT.


  DATA :  WA_A003 TYPE A003.
  DATA :  WA1_A003 TYPE A003.

    DATA :  B_DUTY7 TYPE KWERT.           "ADDED BY MANI
    DATA :  B_DUTY8 TYPE KWERT.           "ADDED BY MANI
    DATA:S1 TYPE KWERT .                  "ADDED BY MANI





  IT_EKPO[] = L_DOC-XEKPO[].


*break-point .
  IF L_DOC-XEKKO-BSART = 'ZIM'.
    CLEAR : GTOTAL.
    LOOP AT IT_EKPO.
*      WA_PO-EBELN = IT_EKPO-EBELN.
      WA_PO-UEBPO  =  IT_EKPO-EBELP.
      WA_PO-MATNR  =  IT_EKPO-MATNR.




      CLEAR :  WA_MAKT.
*      SELECT SINGLE * FROM MAKT INTO WA_MAKT WHERE  MATNR =  IT_EKPO-MATNR.
      "Added by SPLABAP during code remediation
      SELECT * FROM MAKT INTO WA_MAKT UP TO 1 ROWS
        WHERE  MATNR =  IT_EKPO-MATNR
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
        WA_PO-MAKTX = WA_MAKT-MAKTX.
      ELSEIF WA_PO-MATNR IS INITIAL.
        WA_PO-MAKTX = IT_EKPO-TXZ01.
      ENDIF.
      WA_PO-MENGE  =  IT_EKPO-MENGE.
      WA_PO-MEINS  =  IT_EKPO-MEINS.
      WA_PO-NETPR  =  IT_EKPO-NETPR.
      WA_PO-CTOT  = IT_EKPO-NETWR.

*GTOTAL =  GTOTAL  + IT_EKPO-NETWR . Hidded By Govind For Cumulate Amount
      GTOTAL =  IT_EKPO-NETWR .
      WA_PO-TTAMT =  GTOTAL.

      APPEND WA_PO TO IT_PO.
      CLEAR : WA_PO.
    ENDLOOP.


  ELSE.   "IF L_DOC-XEKKO-BSART = 'ZSR'.
*BREAK-POINT .
    DATA :  G_EXCISE1 TYPE KWERT.
    DATA :  G_EXCISE2 TYPE KWERT.
    DATA :  G_EXCISE3 TYPE KWERT.
    DATA :  G_EXCISE4 TYPE KWERT.
    DATA :  G_EXCISE5 TYPE KWERT.
    DATA :  G_EXCISE6 TYPE KWERT.
    DATA :  G_EXCISE7 TYPE KWERT. "Added by Govind

    LOOP AT IT_EKPO.
      WA_PO-UEBPO  =  IT_EKPO-EBELP.
      WA_PO-MATNR  =  IT_EKPO-MATNR.
      CLEAR :  WA_MAKT.
*      SELECT SINGLE * FROM MAKT INTO WA_MAKT WHERE  MATNR =  IT_EKPO-MATNR.
     "Added by SPLABAP during code remediation
      SELECT  * FROM MAKT INTO WA_MAKT UP TO 1 ROWS
        WHERE  MATNR =  IT_EKPO-MATNR
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
        WA_PO-MAKTX = WA_MAKT-MAKTX.
      ELSEIF WA_PO-MATNR IS INITIAL.
        WA_PO-MAKTX = IT_EKPO-TXZ01.
      ENDIF.
      WA_PO-MENGE  =  IT_EKPO-MENGE.
      WA_PO-MEINS  =  IT_EKPO-MEINS.
      WA_PO-NETPR  =  IT_EKPO-NETPR.

*==========================================================================================================================
*           PRICE added on 02.04.2014 by Govind
*==========================================================================================================================

      CLEAR WA_KONV.
      SELECT SINGLE * FROM EKKO INTO WA_EKKO WHERE EBELN = IT_EKPO-EBELN.

      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONV FROM PRCD_ELEMENTS WHERE KNUMV = WA_EKKO-KNUMV AND KPOSN = IT_EKPO-EBELP AND KSCHL = 'P101' . " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
       "Added by SPLABAP during code remediation
        SELECT   * INTO WA_KONV UP TO 1 ROWS FROM PRCD_ELEMENTS
          WHERE KNUMV = WA_EKKO-KNUMV
          AND KPOSN = IT_EKPO-EBELP AND KSCHL = 'P101' " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
        ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          WA_PO-NETPR =  WA_KONV-KBETR .
        ENDIF.
      ENDIF.

*==========================================================================================================================
*           TOTAL PRICE  added on 03.04.2014 by Govind
*==========================================================================================================================
      WA_PO-CTOT  = IT_EKPO-NETWR.
      CLEAR WA_KONV.
      SELECT SINGLE * FROM EKKO INTO WA_EKKO WHERE EBELN = IT_EKPO-EBELN.

      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONV FROM PRCD_ELEMENTS WHERE KNUMV = WA_EKKO-KNUMV AND KPOSN = IT_EKPO-EBELP AND KSCHL = 'P101' . " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
        SELECT  * INTO WA_KONV UP TO 1 ROWS FROM PRCD_ELEMENTS WHERE KNUMV = WA_EKKO-KNUMV
          AND KPOSN = IT_EKPO-EBELP AND KSCHL = 'P101'  " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
      ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          WA_PO-CTOT =  WA_KONV-KBETR * WA_PO-MENGE .
        ENDIF.
      ENDIF.


*=======================================================================================================================================
*        EXCISE DUTY
*=======================================================================================================================================
      DATA : LV_DATE LIKE A363-DATAB.
      DATA : LF_DATE LIKE A363-DATAB.
*      BREAK-POINT.
      LV_DATE = WA_EKKO-BEDAT.

*      SELECT SINGLE  MAX( DATAB ) FROM A363 INTO LV_DATE  WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JMOP'. " Added By Govind on 28-10-2014

      CLEAR WA_KONP.
      CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT, B_DUTY, G_EXCISE1, G_EXCISE2, G_EXCISE3,   G_EXCISE4,  G_EXCISE5,  G_EXCISE6 .

      CLEAR WA_KONP.
      CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.


*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JMOP' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
     "Added by SPLABAP during code remediation
      SELECT * INTO WA_A363 UP TO 1 ROWS FROM A363
        WHERE WERKS = IT_EKPO-WERKS
        AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR
        AND KSCHL = 'JMOP' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      " Added By Govind
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH. "AND LOEVM_KO <> 'X'.
        "Added by SPLABAP during code remediation
        SELECT * INTO WA_KONP FROM KONP UP TO 1 ROWS
          WHERE KNUMH =  WA_A363-KNUMH "AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT =  WA_KONP-KBETR .
        ENDIF.
      ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
      DKWERT =  GKWERT / 10.

      G_EXCISE1 =  IT_EKPO-NETWR / 100 * DKWERT.


      CLEAR WA_KONP.
      CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JEC1'  AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
    "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363 UP TO 1 ROWS
        WHERE WERKS = IT_EKPO-WERKS
        AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR
        AND KSCHL = 'JEC1'  AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH ."AND LOEVM_KO <> 'X'.
        "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
           WHERE KNUMH =  WA_A363-KNUMH "AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT =  WA_KONP-KBETR .
        ENDIF.
      ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
      DKWERT =  GKWERT / 10.

*         G_EXCISE2 =  IT_EKPO-NETWR / 100 * DKWERT.
      G_EXCISE2 =  G_EXCISE1 / 100 * DKWERT.

      CLEAR WA_KONP.
      CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
*      SELECT SINGLE * INTO WA_A363 FROM A363  UP TO 1 ROWS
      "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363  UP TO 1 ROWS
        WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR
        AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JSEP' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
      ORDER BY PRIMARY KEY  .
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH ."AND LOEVM_KO <> 'X'.
        "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
           WHERE KNUMH =  WA_A363-KNUMH "AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT =  WA_KONP-KBETR .
        ENDIF.
      ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
      DKWERT =  GKWERT / 10.

*         G_EXCISE3 =  IT_EKPO-NETWR / 100 * DKWERT.

      G_EXCISE3 =    G_EXCISE1 / 100 * DKWERT.

*==============================================================================================================================

      CLEAR WA_KONP.
      CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JMIP'  AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      "Added by SPLABAP during code remediation
      "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363 UP TO 1 ROWS
         WHERE WERKS = IT_EKPO-WERKS
         AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR
        AND KSCHL = 'JMIP'  AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH ."AND LOEVM_KO <> 'X'.
       "Added by SPLABAP during code remediation
        SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS
          WHERE KNUMH =  WA_A363-KNUMH "AND LOEVM_KO <> 'X'
         ORDER BY PRIMARY KEY .
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT =  WA_KONP-KBETR .
        ENDIF.
      ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
      DKWERT =  GKWERT / 10.
      G_EXCISE4 =  IT_EKPO-NETWR / 100 * DKWERT.

      CLEAR WA_KONP.
      CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JEC2' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
     "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS
        AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR
        AND KSCHL = 'JEC2' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH ."AND LOEVM_KO <> 'X' .
        "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
          WHERE KNUMH =  WA_A363-KNUMH "AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT =  WA_KONP-KBETR .
        ENDIF.
      ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
      DKWERT =  GKWERT / 10.

*         G_EXCISE5 =  IT_EKPO-NETWR / 100 * DKWERT.
      G_EXCISE5 =    G_EXCISE4 / 100 * DKWERT.
      CLEAR WA_KONP.
      CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JESI' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
     "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363  UP TO 1 ROWS
        WHERE WERKS = IT_EKPO-WERKS
        AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JESI'
         AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH. "AND LOEVM_KO <> 'X'.
       "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP UP TO 1 ROWS
          FROM KONP WHERE KNUMH =  WA_A363-KNUMH "AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT =  WA_KONP-KBETR .
        ENDIF.
      ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
      DKWERT =  GKWERT / 10.

*         G_EXCISE6 =  IT_EKPO-NETWR / 100 * DKWERT.
      G_EXCISE6 =    G_EXCISE4 / 100 * DKWERT.



      B_DUTY =   G_EXCISE1 +  G_EXCISE2 +  G_EXCISE3 +  G_EXCISE4 +  G_EXCISE5 +  G_EXCISE6.

      WA_PO-KWERT = B_DUTY.

*=======================================================================================================================================
*        EXCISE DUTY added by Govind 0N 03.04.2014
*=======================================================================================================================================

*BREAK-POINT.

      CLEAR: WA_KONV,WA_EKBE,WA_RG23D,WA_EXCDTL.
      SELECT SINGLE * FROM EKKO INTO WA_EKKO WHERE EBELN = IT_EKPO-EBELN.

      IF SY-SUBRC = 0.
*        SELECT SINGLE * FROM EKBE INTO WA_EKBE WHERE EBELN = WA_EKKO-EBELN AND EBELP = IT_EKPO-EBELP AND BWART = '351'.
        "Added by SPLABAP during code remediation
        SELECT * FROM EKBE INTO WA_EKBE UP TO 1 ROWS
           WHERE EBELN = WA_EKKO-EBELN
          AND EBELP = IT_EKPO-EBELP AND BWART = '351'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          SELECT  SINGLE * INTO WA_KONV FROM PRCD_ELEMENTS WHERE KNUMV = WA_EKKO-KNUMV AND KPOSN = IT_EKPO-EBELP AND KSCHL = 'P101' . " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
          IF SY-SUBRC = 0.
*            SELECT SINGLE * FROM J_1IRG23D INTO WA_RG23D WHERE MBLNR = WA_EKBE-BELNR AND WERKS = WA_EKBE-WERKS .
           "Added by SPLABAP during code remediation
            SELECT  * FROM J_1IRG23D INTO WA_RG23D UP TO 1 ROWS
               WHERE MBLNR = WA_EKBE-BELNR AND WERKS = WA_EKBE-WERKS
              ORDER BY PRIMARY KEY.
              ENDSELECT.
            IF SY-SUBRC = 0.
*              SELECT SINGLE * FROM J_1IEXCDTL INTO WA_EXCDTL WHERE EXNUM = WA_RG23D-EXNUM AND ZEILE = WA_RG23D-ZEILE.
             "Added by SPLABAP during code remediation
              SELECT  * FROM J_1IEXCDTL INTO WA_EXCDTL UP TO 1 ROWS
                WHERE EXNUM = WA_RG23D-EXNUM AND ZEILE = WA_RG23D-ZEILE
                ORDER BY PRIMARY KEY.
                ENDSELECT.
              WA_PO-KWERT = WA_EXCDTL-EXBED.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.


*=======================================================================================================================================
*        SERVICE TAX
*=======================================================================================================================================
*BREAK-POINT .

      CLEAR : WA_KONP,GKWERT, DKWERT , B_DUTY1, B_DUTY2,  B_DUTY3,  B_DUTY4,  B_DUTY5, B_DUTY6.
      CLEAR WA_A359.
*      SELECT SINGLE * INTO WA_A359 FROM A359  WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSVD' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
    "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A359 FROM A359 UP TO 1 ROWS
         WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL
         AND KSCHL = 'JSVD' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP  FROM KONP WHERE KNUMH =  WA_A359-KNUMH ."AND LOEVM_KO <> 'X'.
       "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP  FROM KONP UP TO 1 ROWS
          WHERE KNUMH =  WA_A359-KNUMH "AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT = GKWERT + WA_KONP-KBETR.
        ENDIF.
      ENDIF.
      DKWERT =  GKWERT / 10.
      B_DUTY1 =   WA_PO-CTOT / 100  * DKWERT.

      CLEAR : WA_KONP ,  GKWERT,DKWERT.
      CLEAR WA_A359.
*      SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JEC3' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A359 FROM A359 UP TO 1 ROWS
        WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL
        AND KSCHL = 'JEC3' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH ."AND LOEVM_KO <> 'X'.
        "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
           WHERE KNUMH =  WA_A359-KNUMH"AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT = GKWERT + WA_KONP-KBETR.
        ENDIF.
      ENDIF.
      DKWERT =  GKWERT / 10.
      B_DUTY2 =  B_DUTY1 / 100  * DKWERT.

      CLEAR : WA_KONP ,  GKWERT,DKWERT ,  B_DUTY3.
      CLEAR WA_A359.
*      SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSE1' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
    "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A359 FROM A359 UP TO 1 ROWS
        WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL
        AND KSCHL = 'JSE1' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH. " AND LOEVM_KO <> 'X'.
        "Added by SPLABAP during code remediation
        SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS
           WHERE KNUMH =  WA_A359-KNUMH " AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT = GKWERT + WA_KONP-KBETR.
        ENDIF.
      ENDIF.
      DKWERT =  GKWERT / 10.
      B_DUTY3 =   B_DUTY1 / 100  * DKWERT.


      CLEAR : WA_KONP ,  GKWERT,DKWERT ,  B_DUTY4, B_DUTY5.
      CLEAR WA_A359.
*      SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSV2' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
     "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A359 FROM A359 UP TO 1 ROWS
        WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL
        AND KSCHL = 'JSV2' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
        "Commented by SPLABAP during code remediation
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH ."AND LOEVM_KO <> 'X'.
"Added by SPLABAP during code remediation
        SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS WHERE KNUMH =  WA_A359-KNUMH
          ORDER BY PRIMARY KEY."AND LOEVM_KO <> 'X'.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT = GKWERT + WA_KONP-KBETR.
        ENDIF.
      ENDIF.
      DKWERT =  GKWERT / 10.
      B_DUTY4 =   WA_PO-CTOT / 100  * DKWERT.


      CLEAR : WA_KONP ,  GKWERT,DKWERT .
      CLEAR WA_A359.
*      SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JEC4' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
     "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A359 FROM A359 UP TO 1 ROWS
         WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL
         AND KSCHL = 'JEC4' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH ."AND LOEVM_KO <> 'X'.
       "Added by SPLABAP during code remediation
        SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS
           WHERE KNUMH =  WA_A359-KNUMH "AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT = GKWERT + WA_KONP-KBETR.
        ENDIF.
      ENDIF.
      DKWERT =  GKWERT / 10.
      B_DUTY5 =   B_DUTY4 / 100  * DKWERT.


      CLEAR : WA_KONP ,  GKWERT,DKWERT,  B_DUTY6 .
      CLEAR WA_A359.
*      SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSE2' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
     "Added by SPLABAP during code remediation
      SELECT * INTO WA_A359 FROM A359 UP TO 1 ROWS
         WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL
         AND KSCHL = 'JSE2' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH ."AND LOEVM_KO <> 'X'.
     "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
          WHERE KNUMH =  WA_A359-KNUMH "AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT = GKWERT + WA_KONP-KBETR.
        ENDIF.
      ENDIF.
      DKWERT =  GKWERT / 10.
      B_DUTY6 =   B_DUTY4 / 100  * DKWERT.


      BEXDUT =   B_DUTY1 + B_DUTY2 +  B_DUTY3 +  B_DUTY4 +  B_DUTY5 +  B_DUTY6 .

      WA_PO-SERVICE = BEXDUT.
*--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

"ADDED BY MANI



 IF L_DOC-XEKKO-BSART = 'ZSR'.


     CLEAR : WA_KONP,GKWERT, DKWERT , B_DUTY1, B_DUTY2,  B_DUTY3,  B_DUTY4,  B_DUTY5, B_DUTY6.
      CLEAR : WA_A359 , WA_A969  .
*      SELECT SINGLE * INTO WA_A359 FROM A359  WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSVD' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      "Added by SPLABAP during code remediation
      SELECT * INTO WA_A359 FROM A359  UP TO 1 ROWS
        WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL
        AND KSCHL = 'JSB5' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
*  IF WA_A363 IS INITIAL .
*    SELECT SINGLE * INTO WA_A969 FROM A969 WHERE KSCHL = 'JVRD' AND MWSKZ = IT_EKPO-MWSKZ . " AND DATBI GE LV_DATE AND DATAB LE LV_DATE..
*ENDIF.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP  FROM KONP WHERE KNUMH =  WA_A359-KNUMH . " OR KNUMH = WA_A969-KNUMH ."AND LOEVM_KO <> 'X'.
       "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP  FROM KONP UP TO 1 ROWS
          WHERE KNUMH =  WA_A359-KNUMH  " OR KNUMH = WA_A969-KNUMH ."AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT = GKWERT + WA_KONP-KBETR.
        ENDIF.
      ENDIF.
      DKWERT =  GKWERT / 10.
      B_DUTY7 =   WA_PO-CTOT / 100  * DKWERT.

      CLEAR : WA_KONP ,  GKWERT,DKWERT.
      CLEAR : WA_A359 , WA_A969 .
*      SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSB6' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
     "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A359 FROM A359 UP TO 1 ROWS
         WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL
        AND KSCHL = 'JSB6' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
* IF WA_A363 IS INITIAL .
*    SELECT SINGLE * INTO WA_A969 FROM A969 WHERE KSCHL = 'JSB6' AND MWSKZ = IT_EKPO-MWSKZ . " AND DATBI GE LV_DATE AND DATAB LE LV_DATE..
*ENDIF.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH . " OR KNUMH = WA_A969-KNUMH ."AND LOEVM_KO <> 'X'.
       "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
          WHERE KNUMH =  WA_A359-KNUMH " OR KNUMH = WA_A969-KNUMH ."AND LOEVM_KO <> 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          GKWERT = GKWERT + WA_KONP-KBETR.
        ENDIF.
      ENDIF.
      DKWERT =  GKWERT / 10.
      B_DUTY8 =  WA_PO-CTOT / 100  * DKWERT.

      s1 =   s1 + BEXDUT + B_DUTY7 + B_DUTY8.

      WA_PO-SERVICE = S1 .

      ENDIF.

"ADDED BY MANI

*--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*=======================================================================================================================================
*         VAT/CST  Calculation
*=======================================================================================================================================


*      BREAK-POINT.
      CLEAR  : WA_A363, VKWERT ,WA_A969.


*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVRD' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363 UP TO 1 ROWS
        WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR
        AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVRD' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.

IF WA_A363 IS INITIAL .
*    SELECT SINGLE * INTO WA_A969 FROM A969 WHERE KSCHL = 'JVRD' AND MWSKZ = IT_EKPO-MWSKZ . " AND DATBI GE LV_DATE AND DATAB LE LV_DATE..
    SELECT * INTO WA_A969 FROM A969 UP TO 1 ROWS
      WHERE KSCHL = 'JVRD' AND MWSKZ = IT_EKPO-MWSKZ " AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      ORDER BY PRIMARY KEY.
      ENDSELECT.
ENDIF.

IF WA_A363 IS INITIAL AND WA_A969 IS INITIAL .
*    SELECT SINGLE * INTO WA_A359 FROM A359 WHERE KSCHL = 'JVRD' AND WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND DATBI GE LV_DATE AND DATAB LE LV_DATE .
   "Added by SPLABAP during code remediation
    SELECT * INTO WA_A359 FROM A359 UP TO 1 ROWS
       WHERE KSCHL = 'JVRD'
      AND WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL
      AND DATBI GE LV_DATE AND DATAB LE LV_DATE
      ORDER BY PRIMARY KEY
      .
      ENDSELECT.
 ENDIF.



 IF WA_A363 IS INITIAL AND WA_A969 IS INITIAL AND WA_A359 IS INITIAL AND L_DOC-XEKKO-BSART = 'ZCA' .

*   SELECT SINGLE * INTO WA_A998 FROM A998 WHERE KSCHL ='JVRD'  AND WERKS = IT_EKPO-WERKS AND LIFNR = L_DOC-XEKKO-LIFNR AND MATKL = IT_EKPO-MATKL .
   "Added by SPLABAP during code remediation
   SELECT  * INTO WA_A998 FROM A998 UP TO 1 ROWS
      WHERE KSCHL ='JVRD'  AND WERKS = IT_EKPO-WERKS
      AND LIFNR = L_DOC-XEKKO-LIFNR AND MATKL = IT_EKPO-MATKL
     ORDER BY PRIMARY KEY.
     ENDSELECT  .

 ENDIF.

"ADDED ON 16/2
 IF WA_A363 IS INITIAL AND WA_A969 IS INITIAL AND L_DOC-XEKKO-BSART = 'ZCA' .

*   SELECT SINGLE * INTO WA1_A998 FROM A998 WHERE KSCHL ='JVCS'  AND WERKS = IT_EKPO-WERKS AND LIFNR = L_DOC-XEKKO-LIFNR AND MATKL = IT_EKPO-MATKL .
  "Added by SPLABAP during code remediation
   SELECT  * INTO WA1_A998 FROM A998 UP TO 1 ROWS
     WHERE KSCHL ='JVCS'  AND WERKS = IT_EKPO-WERKS
     AND LIFNR = L_DOC-XEKKO-LIFNR AND MATKL = IT_EKPO-MATKL
     ORDER BY PRIMARY KEY .
     ENDSELECT.

 ENDIF.
"ADDED ON 16/2

   "   IF SY-SUBRC = 0.
      "  SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH OR KNUMH = WA_A969-KNUMH . "AND LOEVM_KO <> 'X'. " Added By Govind LOEVM_KO
 "Added by SPLABAP during code remediation
  SELECT   * INTO WA_KONP FROM KONP  UP TO 1 ROWS
    WHERE KNUMH =  WA_A363-KNUMH OR KNUMH = WA_A969-KNUMH OR
     KNUMH = WA_A998-KNUMH
    ORDER BY PRIMARY KEY  . "AND LOEVM_KO <> 'X'. " Added By Govind LOEVM_KO
    ENDSELECT.
          "ADDED BY RAM ON 7/7/2016
          IF WA_KONP-KBETR = 0 .
            CLEAR WA_KONP .
*            SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH = WA_A359-KNUMH AND LOEVM_KO NE 'X'. " Added By Govind LOEVM_KO
            "Added by SPLABAP during code remediation
            SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS
              WHERE KNUMH = WA_A359-KNUMH AND LOEVM_KO NE 'X' " Added By Govind LOEVM_KO
              ORDER BY PRIMARY KEY.
              ENDSELECT.
           "   ADDED ON 16/2
              IF WA_KONP-KBETR = 0 .
               "Added by SPLABAP during code remediation
*                 SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH = WA1_A998-KNUMH AND LOEVM_KO NE 'X'.
                 SELECT   * INTO WA_KONP FROM KONP
                   WHERE KNUMH = WA1_A998-KNUMH AND LOEVM_KO NE 'X'
                   ORDER BY  PRIMARY KEY.
                   ENDSELECT.
              ENDIF.
           ENDIF.
    " ENDED BY RAM ON 7/7/2016
        IF SY-SUBRC = 0.
          VKWERT =  VKWERT + WA_KONP-KBETR .
          WA_PO-KSCHL =  WA_KONP-KSCHL.
        ENDIF.
  "    ENDIF.

      CLEAR  : WA_A363, WA_A969.
      "Commented by SPLABAP during code remediation
*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND  MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVRN' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS
         AND  MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR
         AND KSCHL = 'JVRN' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.

 IF WA_A363 IS INITIAL .
*    SELECT SINGLE * INTO WA_A969 FROM A969 WHERE KSCHL = 'JVRN' AND MWSKZ = IT_EKPO-MWSKZ . "AND DATBI GE LV_DATE AND DATAB LE LV_DATE. .
   "Added by SPLABAP during code remediation
    SELECT  * INTO WA_A969 FROM A969 UP TO 1 ROWS
      WHERE KSCHL = 'JVRN' AND MWSKZ = IT_EKPO-MWSKZ "AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      ORDER BY PRIMARY KEY.
      ENDSELECT.
ENDIF.


"ADDED ON 21/2
     IF WA_A363 IS INITIAL AND WA_A969 IS INITIAL AND L_DOC-XEKKO-BSART = 'ZNB' .
"Commented by SPLABAP during code remediation
*       SELECT SINGLE * INTO WA2_A998 FROM A998 WHERE KSCHL ='JVRN'  AND WERKS = IT_EKPO-WERKS AND LIFNR = L_DOC-XEKKO-LIFNR AND MATKL = IT_EKPO-MATKL .
      "Added by SPLABAP during code remediation
       SELECT  * INTO WA2_A998 FROM A998
         WHERE KSCHL ='JVRN'  AND WERKS = IT_EKPO-WERKS
          AND LIFNR = L_DOC-XEKKO-LIFNR AND MATKL = IT_EKPO-MATKL
         ORDER BY PRIMARY KEY .
         ENDSELECT.


     ENDIF.

"ENDED ON 21/2

      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH  OR KNUMH = WA_A969-KNUMH OR KNUMH = WA2_A998-KNUMH AND LOEVM_KO NE 'X'.
        "Added by SPLABAP during code remediation
        SELECT * INTO WA_KONP FROM KONP UP TO 1 ROWS
           WHERE KNUMH =  WA_A363-KNUMH  OR KNUMH = WA_A969-KNUMH
           OR KNUMH = WA2_A998-KNUMH AND LOEVM_KO NE 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          VKWERT =  VKWERT + WA_KONP-KBETR .
          WA_PO-KSCHL =  WA_KONP-KSCHL.
        ENDIF.
      ENDIF.

      CLEAR  : WA_A363,WA_A969.
      "Commented by SPLABAP during code remediation
*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVCD' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
   "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363 UP TO 1 ROWS WHERE WERKS = IT_EKPO-WERKS
        AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR
        AND KSCHL = 'JVCD' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.

 IF WA_A363 IS INITIAL .
*    SELECT SINGLE * INTO WA_A969 FROM A969 WHERE KSCHL = 'JVCD' AND MWSKZ = IT_EKPO-MWSKZ . "AND DATBI GE LV_DATE AND DATAB LE LV_DATE..
    "Added by SPLABAP during code remediation
    SELECT  * INTO WA_A969 FROM A969  UP TO 1 ROWS
      WHERE KSCHL = 'JVCD' AND MWSKZ = IT_EKPO-MWSKZ "AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      ORDER BY PRIMARY KEY.
      ENDSELECT.
ENDIF.

      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH AND LOEVM_KO NE 'X'.
        "Added by SPLABAP during code remediation
        SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS
           WHERE KNUMH =  WA_A363-KNUMH AND LOEVM_KO NE 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          VKWERT =  VKWERT + WA_KONP-KBETR .
          WA_PO-KSCHL =  WA_KONP-KSCHL.
        ENDIF.
      ENDIF.
      CLEAR  : WA_A363,WA_A969.
*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVCN' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
     "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363 UP TO 1 ROWS
        WHERE WERKS = IT_EKPO-WERKS
        AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR
        AND KSCHL = 'JVCN' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
 IF WA_A363 IS INITIAL .
*    SELECT SINGLE * INTO WA_A969 FROM A969
   "Added by SPLABAP during code remediation
    SELECT  * INTO WA_A969 FROM A969 UP TO 1 ROWS
      WHERE KSCHL = 'JVCN' AND MWSKZ = IT_EKPO-MWSKZ
      ORDER BY PRIMARY KEY . " AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      ENDSELECT..
ENDIF.
      IF SY-SUBRC = 0.
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE ( KNUMH =  WA_A363-KNUMH OR KNUMH = WA_A969-KNUMH ) AND LOEVM_KO NE 'X'.
       "Added by SPLABAP during code remediation
        SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS
           WHERE ( KNUMH =  WA_A363-KNUMH OR KNUMH = WA_A969-KNUMH ) AND LOEVM_KO NE 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          VKWERT =  VKWERT + WA_KONP-KBETR .
          WA_PO-KSCHL =  WA_KONP-KSCHL.
        ENDIF.
      ENDIF.
      CLEAR  : WA_A363,WA_A969.
*      SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVCS' AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
     "Added by SPLABAP during code remediation
      SELECT  * INTO WA_A363 FROM A363 UP TO 1 ROWS
         WHERE WERKS = IT_EKPO-WERKS
        AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR
        AND KSCHL = 'JVCS' AND DATBI GE LV_DATE AND DATAB LE LV_DATE
        ORDER BY PRIMARY KEY.
        ENDSELECT.
  IF WA_A363 IS INITIAL .
*    SELECT SINGLE * INTO WA_A969 FROM A969
    "Added by SPLABAP during code remediation
    SELECT  * INTO WA_A969 FROM A969 UP TO 1 ROWS
      WHERE KSCHL = 'JVCS' AND MWSKZ = IT_EKPO-MWSKZ  " AND DATBI GE LV_DATE AND DATAB LE LV_DATE.
      ORDER BY PRIMARY KEY.
      ENDSELECT.
ENDIF.

      IF SY-SUBRC = 0.
*       "Commented by SPLABAP during code remediation
*        SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE ( KNUMH =  WA_A363-KNUMH  OR KNUMH = WA_A969-KNUMH ) AND LOEVM_KO NE 'X'.
       "Added by SPLABAP during code remediation
        SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
           WHERE ( KNUMH =  WA_A363-KNUMH  OR KNUMH = WA_A969-KNUMH )
          AND LOEVM_KO NE 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        "Added by SPLABAP during code remediation
        SELECT * INTO WA_KONP FROM KONP UP TO 1 ROWS
          WHERE ( KNUMH =  WA_A363-KNUMH  OR KNUMH = WA_A969-KNUMH ) AND LOEVM_KO NE 'X'
          ORDER BY PRIMARY KEY.
          ENDSELECT.
        IF SY-SUBRC = 0.
          VKWERT =  VKWERT + WA_KONP-KBETR .
          WA_PO-KSCHL =  WA_KONP-KSCHL.
        ENDIF.
      ENDIF.

"ADDED BY RAM ON 16/9/16

     IF L_DOC-XEKKO-BSART = 'YSNB'.
"Commented by SPLABAP during code remediation
*        SELECT SINGLE * INTO WA_A003 FROM A003 WHERE KSCHL = 'JNLN' AND MWSKZ = IT_EKPO-MWSKZ .
     "Added by SPLABAP during code remediation
        SELECT  * INTO WA_A003 FROM A003 UP TO 1 ROWS
          WHERE KSCHL = 'JNLN' AND MWSKZ = IT_EKPO-MWSKZ
          ORDER BY PRIMARY KEY.
          ENDSELECT.

          IF SY-SUBRC = 0.
            "Commented by SPLABAP during code remediation
*            SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A003-KNUMH .
            "Added by SPLABAP during code remediation
            SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
              WHERE KNUMH =  WA_A003-KNUMH
              ORDER BY PRIMARY KEY.
              ENDSELECT.

               IF SY-SUBRC = 0.
                  GKWERT = GKWERT + WA_KONP-KBETR.
                  DKWERT =  GKWERT / 10.
                   WA_PO-SERVICE =   WA_PO-CTOT / 100  * DKWERT.
  "                  VKWERT =  VKWERT + WA_KONP-KBETR .
   "                 WA_PO-KSCHL =  WA_KONP-KSCHL.

                  ENDIF.
ENDIF.
     ENDIF.

 "ENDED BY RAM ON 16/9/161

 IF L_DOC-XEKKO-BSART = 'YSNB'.

*SELECT SINGLE * INTO WA1_A003 FROM A003 WHERE KSCHL = 'JVLN' AND MWSKZ = IT_EKPO-MWSKZ .
"Added by SPLABAP during code remediation
SELECT  * INTO WA1_A003 FROM A003  UP TO 1 ROWS
  WHERE KSCHL = 'JVLN' AND MWSKZ = IT_EKPO-MWSKZ
  ORDER BY PRIMARY KEY .
  ENDSELECT.

  IF SY-SUBRC = 0.

*    SELECT  SINGLE * INTO WA1_KONP FROM KONP WHERE KNUMH =  WA1_A003-KNUMH .
    "Added by SPLABAP during code remediation
    SELECT   * INTO WA1_KONP FROM KONP UP TO 1 ROWS
       WHERE KNUMH =  WA1_A003-KNUMH
      ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF SY-SUBRC = 0.
       GKWERT1 = WA1_KONP-KBETR .
       DKWERT1  =  GKWERT1 / 10.
       WA_PO-KBETR = ( ( WA_PO-CTOT + WA_PO-SERVICE ) / 100 )  * DKWERT1 .
      ENDIF.
    ENDIF.
*      BREAK-POINT.
      CLEAR : GPERN , GCALC, GTOTAL.
      GPERN = VKWERT / 10.
*      GPERN1 = VKWERT1 / 10.
      GCALC =   ( WA_PO-CTOT + WA_PO-KWERT ) / 100.
    "  WA_PO-KBETR = GCALC * GPERN.
*      WA_PO-KBETR1 = GCALC * GPERN1.
      GTOTAL =  GTOTAL  + WA_PO-KWERT  +  WA_PO-KBETR +  IT_EKPO-NETWR + WA_PO-SERVICE.
      WA_PO-TTAMT =  GTOTAL.

*=======================================================================================================================================
*        EXCISE DUTY added by Govind 0N 03.04.2014
*=======================================================================================================================================
      CLEAR : WA_KONV,NTOTAL.
      SELECT SINGLE * FROM EKKO INTO WA_EKKO WHERE EBELN = IT_EKPO-EBELN.

      IF SY-SUBRC = 0.
        SELECT  SINGLE * INTO WA_KONV FROM PRCD_ELEMENTS WHERE KNUMV = WA_EKKO-KNUMV AND KPOSN = IT_EKPO-EBELP AND KSCHL = 'P101' . " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
        IF SY-SUBRC = 0.
          NTOTAL =  NTOTAL +  WA_PO-KWERT +  WA_PO-CTOT.
          WA_PO-TTAMT = NTOTAL.
        ENDIF.
      ENDIF.

      APPEND WA_PO TO IT_PO.
      CLEAR : WA_PO.
    ENDIF.


IF L_DOC-XEKKO-BSART <> 'YSNB'.

*      BREAK-POINT.
      CLEAR : GPERN , GCALC, GTOTAL.
      GPERN = VKWERT / 10.
*      GPERN1 = VKWERT1 / 10.
      GCALC =   ( WA_PO-CTOT + WA_PO-KWERT ) / 100.
      WA_PO-KBETR = GCALC * GPERN.
*      WA_PO-KBETR1 = GCALC * GPERN1.
      GTOTAL =  GTOTAL  + WA_PO-KWERT  +  WA_PO-KBETR +  IT_EKPO-NETWR + WA_PO-SERVICE.
      WA_PO-TTAMT =  GTOTAL.



*=======================================================================================================================================
*        EXCISE DUTY added by Govind 0N 03.04.2014
*=======================================================================================================================================
      CLEAR : WA_KONV,NTOTAL.
      SELECT SINGLE * FROM EKKO INTO WA_EKKO WHERE EBELN = IT_EKPO-EBELN.

      IF SY-SUBRC = 0.
        SELECT  SINGLE * INTO WA_KONV FROM PRCD_ELEMENTS WHERE KNUMV = WA_EKKO-KNUMV AND KPOSN = IT_EKPO-EBELP AND KSCHL = 'P101' . " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
        IF SY-SUBRC = 0.
          NTOTAL =  NTOTAL +  WA_PO-KWERT +  WA_PO-CTOT.
          WA_PO-TTAMT = NTOTAL.
        ENDIF.
      ENDIF.

      APPEND WA_PO TO IT_PO.
      CLEAR : WA_PO.
    ENDIF.

    ENDLOOP.
  ENDIF.



  IF IT_PO[] IS INITIAL.
    MESSAGE 'No line Items Found' TYPE 'I'.
  ENDIF.



*=======================================================================================================================================
*                    end  by shanmugam
*=======================================================================================================================================



*Set the print Parameters
  PERFORM SET_PRINT_PARAM USING     LS_ADDR_KEY
                          CHANGING  LS_CONTROL_PARAM
                                    LS_COMPOSER_PARAM
                                    LS_RECIPIENT
                                    LS_SENDER
                                    ENT_RETCO.

*Get the Smart Form name.
  IF NOT TNAPR-SFORM IS INITIAL.
    LF_FORMNAME = TNAPR-SFORM.
  ELSE.
    MESSAGE E001(SSFCOMPOSER).
  ENDIF.

* Determine smartform function module for purchase document
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LF_FORMNAME
    IMPORTING
      FM_NAME            = LF_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
*  error handling
    ENT_RETCO = SY-SUBRC.
    IF SY-SUBRC = 1.
      MESSAGE E001(SSFCOMPOSER).
    ENDIF.
    IF SY-SUBRC = 2.
      MESSAGE E002(SSFCOMPOSER) WITH TNAPR-SFORM.
    ENDIF.
    PERFORM PROTOCOL_UPDATE_I.
  ENDIF.


* if it is faxed, changed its title to PO number
  IF LS_CONTROL_PARAM-DEVICE = 'TELEFAX'.
    LS_COMPOSER_PARAM-TDTITLE = L_DOC-XEKKO-EBELN.
  ENDIF.

* if it is mail, changed its title to PO number
*  IF ls_control_param-device = 'MAIL'.
*    ls_composer_param-tdtitle = l_doc-xekko-ebeln.
*  ENDIF.


*>>>>> Change of Parameters <<<<<<<<<<<<<<<<<<<<<<<
  CALL FUNCTION LF_FM_NAME
    EXPORTING
      ARCHIVE_INDEX      = TOA_DARA
*     archive_parameters = arc_params
*     control_parameters = ls_control_param
      MAIL_RECIPIENT     = LS_RECIPIENT
      MAIL_SENDER        = LS_SENDER
      OUTPUT_OPTIONS     = LS_COMPOSER_PARAM
      IS_EKKO            = L_DOC-XEKKO
      USER_SETTINGS      = ' '  "Disable User Printer
      IS_PEKKO           = L_DOC-XPEKKO
      IS_NAST            = L_NAST
      IV_FROM_MEM        = L_FROM_MEMORY
      IV_DRUVO           = IV_DRUVO
      IV_XFZ             = IV_XFZ
    TABLES
      IT_EKPO            = L_DOC-XEKPO[]
      IT_EKPA            = L_DOC-XEKPA[]
      IT_PEKPO           = L_DOC-XPEKPO[]
      IT_EKET            = L_DOC-XEKET[]
      IT_TKOMV           = L_DOC-XTKOMV[]
      IT_EKKN            = L_DOC-XEKKN[]
      IT_EKEK            = L_DOC-XEKEK[]
      IT_KOMK            = L_XKOMK[]
      IT_TAB             = IT_PO[]
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.

  IF SY-SUBRC <> 0.
    ENT_RETCO = SY-SUBRC.
    PERFORM PROTOCOL_UPDATE_I.

* get SmartForm protocoll and store it in the NAST protocoll
    PERFORM ADD_SMFRM_PROT.

  ELSE.
*  required for Discrete Manufacturing scenario 230
**<<<< Start of insertion >>>>*
    IF  L_NAST-KAPPL EQ 'EL'     AND ENT_SCREEN IS INITIAL
      AND L_FROM_MEMORY IS INITIAL AND L_NAST-SNDEX IS INITIAL.
      IF SY-UCOMM NE '9ANZ' AND SY-UCOMM NE '9DPR'.
        PERFORM UPDATE_RELEASE(SAPLMEDRUCK)
                TABLES L_DOC-XEKPO L_DOC-XEKEK L_DOC-XEKEH
                USING  IV_DRUVO L_NAST-KSCHL.
      ENDIF.
    ENDIF.
*<<<< End of insertion >>>>*
    READ TABLE LS_JOB_INFO-SPOOLIDS INTO L_SPOOLID INDEX 1.
    IF SY-SUBRC IS INITIAL.
      EXPORT SPOOLID = L_SPOOLID TO MEMORY ID 'KYK_SPOOLID'.
    ENDIF.

  ENDIF.
ENDFORM.                    "PROCESSING_PO


*&---------------------------------------------------------------------*
*&      Form  set_print_param
*&---------------------------------------------------------------------*
FORM SET_PRINT_PARAM USING    IS_ADDR_KEY LIKE ADDR_KEY
                     CHANGING CS_CONTROL_PARAM TYPE SSFCTRLOP
                              CS_COMPOSER_PARAM TYPE SSFCOMPOP
                              CS_RECIPIENT TYPE  SWOTOBJID
                              CS_SENDER TYPE  SWOTOBJID
                              CF_RETCODE TYPE SY-SUBRC.

  DATA: LS_ITCPO     TYPE ITCPO.
  DATA: LF_REPID     TYPE SY-REPID.
  DATA: LF_DEVICE    TYPE TDDEVICE.
  DATA: LS_RECIPIENT TYPE SWOTOBJID.
  DATA: LS_SENDER    TYPE SWOTOBJID.

  LF_REPID = SY-REPID.

  CALL FUNCTION 'WFMC_PREPARE_SMART_FORM'
    EXPORTING
      PI_NAST       = NAST
      PI_ADDR_KEY   = IS_ADDR_KEY
      PI_REPID      = LF_REPID
    IMPORTING
      PE_RETURNCODE = CF_RETCODE
      PE_ITCPO      = LS_ITCPO
      PE_DEVICE     = LF_DEVICE
      PE_RECIPIENT  = CS_RECIPIENT
      PE_SENDER     = CS_SENDER.

  IF CF_RETCODE = 0.
    MOVE-CORRESPONDING LS_ITCPO TO CS_COMPOSER_PARAM.
*    cs_composer_param-tdnoprint = 'X'.                     "Note 591576
    CS_CONTROL_PARAM-DEVICE      = LF_DEVICE.
    CS_CONTROL_PARAM-NO_DIALOG   = 'X'.
    CS_CONTROL_PARAM-PREVIEW     = XSCREEN.
    CS_CONTROL_PARAM-GETOTF      = LS_ITCPO-TDGETOTF.
    CS_CONTROL_PARAM-LANGU       = NAST-SPRAS.
  ENDIF.
ENDFORM.                    "set_print_param
