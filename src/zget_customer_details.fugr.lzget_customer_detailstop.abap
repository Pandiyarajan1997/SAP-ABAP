FUNCTION-POOL ZGET_CUSTOMER_DETAILS        MESSAGE-ID F4.

* INCLUDE LZGET_CUSTOMER_DETAILSD...         " Local class definition



TABLES:  BAPI1010_3, BAPI1010_4, BAPI1010_5, BAPI1010_6.
TABLES:  KNKKR4.

TABLES:  BSEGP,                             " Zusatzinformationen EP
         BSID, BSAD,                        " Debitor: Offene Posten
         VF_BSID,                           " View zu BSID
         KNB1,                              " Debitor: Bukrs-Daten
         KNB4,                              " Debitor: Zahlverhalten
         KNB5,                              " Debitor: Mahndaten
         KNC1, *KNC1,                       " Debitor: Saldo
         KNC3,                              " Debitor: SHB-Saldo
         KNKK,                              " Debitor: Kreditstammsatz
         KNKKF1,                            " CM: FI-Statusdaten
         KNKKF2.                            " CM: OP's nach Verzugstagen

TABLES:  TRAS,                              " Raster
         TAKOF,                             "Abstimmkonten ohne CM-Fort.
         T000CM,                            " Mandantenabh. CM-Einstell.
         T001,                              " Buchungskreise
         T001CM,                            " Buchungskreise zum KKBER
         T009Y, *T009Y,                      " Rumpfgeschaeftsjahr
         T014,                              " Kontrollbereiche
         T047,                              " Mahndaten Buchungskreis
         T053R,                             " Strittige Posten
         T074U.                             " Sonderhauptbuchkennzeichen
TABLES:  RF035.                             " Verzugstage
TABLES:  ZBAPIRFPOS.                             " Debitor: Offene Posten
DATA:    FAEDE_EXP LIKE FAEDE.              " Fälligkeitsermittlung
DATA:    FAEDE_IMP LIKE FAEDE.              " Fälligkeitsermittlung

*------- KNB4-Daten aller Buchungskreise -------------------------------
DATA:    BEGIN OF BKNB4 OCCURS 16,
           BUKRS      LIKE KNB4-BUKRS,      " Buchungskreis
           " AFLE ENABLEMENT INCLUDE STRUCTURE IKNB4.
           WAERS  Type  WAERS,
           JAHXX  Type  GJAHR,
           MONXX  Type  MONAT,
           AGSXX  Type  AGSXX_CS,
           VZSXX  Type  VZSXX,
           AGNXX  Type  AGNXX_CS,
           VZNXX  Type  VZNXX,
           ANZXX  Type  ANZXX,
         END   OF BKNB4.

*------- Puffer für DUE_DATE_ANALYSIS ----------------------------------
TYPES:    BEGIN OF GTY_BUFFER_BSID,     "n1958095
           I_BUKRS    LIKE BSID-BUKRS,
           I_KKBER    LIKE T014-KKBER,
           I_KUNNR    LIKE BSID-KUNNR,
           I_RASID    LIKE TRAS-RASID,
           MSGNO      LIKE T100-MSGNR,
           SFAE1      LIKE RF035-SFAE1,
           SFAE2      LIKE RF035-SFAE2,
           SFAE3      LIKE RF035-SFAE3,
           SFAE4      LIKE RF035-SFAE4,
           SFAE5      LIKE RF035-SFAE5,
           SFAE6      LIKE RF035-SFAE6,
           SFAEL      LIKE RF035-SFAEL,
           SNFA1      LIKE RF035-SNFA1,
           SNFA2      LIKE RF035-SNFA2,
           SNFA3      LIKE RF035-SNFA3,
           SNFA4      LIKE RF035-SNFA4,
           SNFA5      LIKE RF035-SNFA5,
           SNFA6      LIKE RF035-SNFA6,
           SNFAE      LIKE RF035-SNFAE,
         END   OF GTY_BUFFER_BSID.

DATA: BUFFER_BSID TYPE HASHED TABLE OF GTY_BUFFER_BSID
      WITH UNIQUE KEY I_BUKRS I_KKBER I_KUNNR I_RASID WITH HEADER LINE.

*------- Puffer für CUSTOMER_BALANCE -----------------------------------
TYPES:   BEGIN OF GTY_BUFFER_KNC1,
           BUKRS        LIKE KNC1-BUKRS,
           GJAHR        LIKE KNC1-GJAHR,
           KUNNR        LIKE KNC1-KUNNR,
           SALDO        LIKE KNC1-UMSAV,
           UMP2U        LIKE RF42B-UMP2U,   " Umsatz aktuelles Jahr
           VMP2U        LIKE RF42B-UMP2U,   " Umsatz Vorjahr
           H06SA        LIKE RF035-H06SA,
           H06MO        LIKE RF035-H06MO,
           H06JA        LIKE RF035-H06JA,
           H12SA        LIKE RF035-H12SA,
           H12MO        LIKE RF035-H12MO,
           H12JA        LIKE RF035-H12JA,
           UML01        LIKE KNC1-UM01S,     " Saldo vor 01 Monaten
           UML02        LIKE KNC1-UM01S,     " Saldo vor 02 Monaten
           UML03        LIKE KNC1-UM01S,     " Saldo vor 03 Monaten
           UML04        LIKE KNC1-UM01S,     " Saldo vor 04 Monaten
           UML05        LIKE KNC1-UM01S,     " Saldo vor 05 Monaten
           UML06        LIKE KNC1-UM01S,     " Saldo vor 06 Monaten
           UML07        LIKE KNC1-UM01S,     " Saldo vor 07 Monaten
           UML08        LIKE KNC1-UM01S,     " Saldo vor 08 Monaten
           UML09        LIKE KNC1-UM01S,     " Saldo vor 09 Monaten
           UML10        LIKE KNC1-UM01S,     " Saldo vor 10 Monaten
           UML11        LIKE KNC1-UM01S,     " Saldo vor 11 Monaten
           UML12        LIKE KNC1-UM01S,     " Saldo vor 12 Monaten
         END   OF GTY_BUFFER_KNC1.

DATA: BUFFER_KNC1 TYPE HASHED TABLE OF GTY_BUFFER_KNC1
      WITH UNIQUE KEY BUKRS GJAHR KUNNR WITH HEADER LINE.

*------ Pufferung der KNKKF2-Daten für Aufruf OP_items_structure -------
DATA:    BEGIN OF BUFFER_KNKKF2 OCCURS 3,
           LOGSYS       LIKE TBDLS-LOGSYS,
           KUNNR        LIKE KNKK-KUNNR,
           KKBER        LIKE KNKK-KKBER,
           REGUL        LIKE T691F-REGUL,
         END   OF BUFFER_KNKKF2.

*------- Buchungskreise ------------------------------------------------
DATA:    BEGIN OF BUKTAB OCCURS 10,
           BUKRS        LIKE T001-BUKRS,    " Buchungskreis
           WAERS        LIKE T001-WAERS,    " Währung
         END   OF BUKTAB.

*------- Infos, ob Daten veraltet oder nicht ermittelbar (gepuffert) ---
DATA:    BEGIN OF DATAINFO OCCURS 10,
           LOGSYS       LIKE TBDLS-LOGSYS,
           KUNNR        LIKE KNKK-KUNNR,
           KKBER        LIKE KNKK-KKBER,
           REGUL        LIKE T691F-REGUL,
           ERLTA        LIKE RF035-ERLTA,
           ERLST        LIKE RF035-ERLST,
           XNDAT        LIKE BOOLE-BOOLE,
           XODAT        LIKE BOOLE-BOOLE,
           VERTA        TYPE I,
           VERST        TYPE I,
           XDUNN        LIKE BOOLE-BOOLE,
           XSTRU        LIKE BOOLE-BOOLE,
           XOLDEST      LIKE BOOLE-BOOLE,
           XKNKK        LIKE BOOLE-BOOLE,
           XVORODAT     LIKE BOOLE-BOOLE,
           XVORNDAT     LIKE BOOLE-BOOLE,


         END OF DATAINFO.

*------- Kontrollbereiche ----------------------------------------------
DATA:    BEGIN OF KKBTAB OCCURS 3,
           KKBER        LIKE T014-KKBER,    " Kontrollbereich
           WAERS        LIKE T014-WAERS,    " Währung
         END   OF KKBTAB.

*------- Puffer KNB1 ---------------------------------------------------
DATA     BEGIN OF KNB1TAB OCCURS 5.
           INCLUDE STRUCTURE KNB1.
DATA     END   OF KNB1TAB.

*------- KNKK-Daten (Daten aus Kreditvektor) ---------------------------
* begin of note 1239730
*DATA: BEGIN OF KNKKRECV OCCURS 10.
*        INCLUDE STRUCTURE KNKK.
*DATA: END OF KNKKRECV.
DATA: KNKKRECV TYPE SORTED TABLE OF knkk
      WITH UNIQUE KEY KUNNR KKBER
      WITH HEADER LINE.
* end of note 1239730

*------- KNKK-Daten (Daten direkt aus KNKK) ----------------------------
DATA: BEGIN OF IKNKK OCCURS 10.
        INCLUDE STRUCTURE KNKK.
DATA: END OF IKNKK.

*------- FI-Statusdaten (gepuffert) ------------------------------------
DATA: BEGIN OF KNKKF1TAB OCCURS 1.
        INCLUDE STRUCTURE KNKKF1.
DATA: END OF KNKKF1TAB.

*------- FI-Statusdaten (temporär) -------------------------------------
DATA: BEGIN OF IKNKKF1 OCCURS 2.
        INCLUDE STRUCTURE KNKKF1.
DATA: END OF IKNKKF1.

*------- Offene Posten nach Verzugstagen -------------------------------
DATA: BEGIN OF KNKKF2TAB OCCURS 1.
        INCLUDE STRUCTURE KNKKF2.
DATA: END OF KNKKF2TAB.

*------- Konten --------------------------------------------------------
DATA:    BEGIN OF KONTAB OCCURS 10,
           KUNNR        LIKE KNA1-KUNNR,    " Debitor
         END OF KONTAB.

* ------- Offene Posten / Felder für DUE_DATE_ANALYSIS -----------------
DATA:    BEGIN OF POSTAB OCCURS 50.
           INCLUDE STRUCTURE RFPOS.
DATA:    END   OF POSTAB.

*------- Differenzgründe -----------------------------------------------
DATA:    BEGIN OF RSTTAB OCCURS 5,
           BUKRS             LIKE T053R-BUKRS,   " Buchungskreis
           RSTGR             LIKE T053R-RSTGR,   " Differenzgrund
           XSTRP             LIKE T053R-XSTRP,   " Strittiger Posten?
         END   OF RSTTAB.

*------- Salden zum Periodenende ---------------------------------------
DATA:    BEGIN OF SALTAB OCCURS 12,
           GJAHR        LIKE KNC1-GJAHR,    " Geschäftsjahr
           MONAT        LIKE T009-ANZBP,    " Periode
           SALDO        LIKE RF42B-SALDO,   " Saldo zum Periodenende
         END   OF SALTAB.

* ------ Buchungskreise zum Kontrollbereich (aktuell)-------------------
DATA:    BEGIN OF TCMATAB OCCURS 10.
           INCLUDE STRUCTURE T001CM.
DATA:    END   OF TCMATAB.

*------- Arbeitstabelle (Richtige Konten/Richtige Buchungskreise) ------
DATA:    BEGIN OF WRKTAB OCCURS 10,
           BUKRS        LIKE T001-BUKRS,    " Buchungskreis
           KUNNR        LIKE KNB1-KUNNR,    " Debitor
           ORIKU        LIKE KNB1-KUNNR,    " Originaldebitor
         END   OF WRKTAB.

*------- Aktuelle Rechenfelder für KNB4 --------------------------------
DATA:    BEGIN OF AKNB4,
           AGSXX        LIKE IKNB4-AGSXX,   " AFLE EnAblement KNB4-AGS01,    " Ausgleich Skonto 1
           VZSXX        LIKE IKNB4-vzsxx,   " AFLE Enablement (8)     TYPE P,             " Verzugstage Skonto 1
           AGNXX        LIKE IKNB4-AGNXX,   " Ausgleich Netto
           VZNXX        LiKE IKNB4-vznxx,   " AFLE Enablement (8) TYPE P,             " Verzugstage Netto
         END   OF AKNB4.

*------- Daten für 6-Month-High-Balance --------------------------------
DATA:    BEGIN OF H06,
           VONMO        LIKE T009-ANZBP,    " Von-Monat
           VONJA        LIKE KNC1-GJAHR,    " Von-Jahr
         END   OF H06.

*------- Daten für 12-Month-High-Balance -------------------------------
DATA:    BEGIN OF H12,
           VONMO        LIKE T009-ANZBP,    " Von-Monat
           VONJA        LIKE KNC1-GJAHR,    " Von-Jahr
         END   OF H12.

*------- Daten zur höchsten Mahnstufe ----------------------------------
DATA:    BEGIN OF HIDL,
           KUNNR        LIKE KNB5-KUNNR,    " Debitor
           BUKRS        LIKE KNB5-BUKRS,    " Buchungskreis
           MABER        LIKE KNB5-MABER,    " Mahnbereich
           MAHNS        LIKE KNB5-MAHNS,    " Mahnstufe
         END   OF HIDL.
DATA:    BEGIN OF HIDLTEMP,
           KUNNR        LIKE KNB5-KUNNR,    " Debitor
           BUKRS        LIKE KNB5-BUKRS,    " Buchungskreis
           MABER        LIKE KNB5-MABER,    " Mahnbereich
           MAHNS        LIKE KNB5-MAHNS,    " Mahnstufe
         END   OF HIDLTEMP.

*------- INPUT-Daten aus Schnittstelle ---------------------------------
DATA:    BEGIN OF INPUT,
           CRCV         TYPE C,             " Kreditvektor erstellen?
           DUNNG        LIKE T691F-DUNNG,   " Mahnstufe
           ERLST        LIKE RF035-ERLST,   " erlaubte Stunden
           ERLTA        LIKE RF035-ERLTA,   " erlaubte Tage
           KKBER        LIKE KNKK-KKBER,    " Kontrollbereich
           KUNNR        LIKE KNKK-KUNNR,    " Debitor
           LOGSYS       LIKE KNKKF1-LOGSYS, " logisches System
           PDTOL        LIKE T691F-PDTOL,   " Verzugstage für OP's
           REGUL        LIKE T691F-REGUL,   " Nur Regulierer?
           TMSTMP       LIKE KNKKF1-TMSTMP, " Zeitstempel
           XCRCV        LIKE BOOLE-BOOLE,
         END   OF INPUT.

*------- Daten des Postens mit dem OLDEST DUE DATE ---------------------
DATA:    BEGIN OF OLDD,
           BELNR        LIKE BSID-BELNR,    " Belegnummer
           BUKRS        LIKE BSID-BUKRS,    " Buchungskreis
           KUNNR        LIKE BSID-KUNNR,    " Debitor
           GJAHR        LIKE BSID-GJAHR,    " Geschäftsjahr
           INFAE        LIKE BSID-INFAE,    " Invertierte Nettofälligk.
           FAEDT        LIKE RFPOS-FAEDT,   " Nettofälligkeit
           VERZN        LIKE RFPOS-VERZN,   " Verzugstage netto
           WRBTR        LIKE BSID-WRBTR,    " Betrag in Belegwährung
           WAERS        LIKE BSID-WAERS,    " Belegwährung
           XFAPO(1)     TYPE C,             " Fällige Posten gefunden?
         END   OF OLDD.

* ------- POSTAB-Key ---------------------------------------------------
DATA:    BEGIN OF PKEY,
           KUNNR        LIKE BSID-KUNNR,    " Debitor
           BUKRS        LIKE BSID-BUKRS,    " Buchungskreis
           KKBER        LIKE T014-KKBER,
         END   OF PKEY.

* ------- Rasterpunkte -------------------------------------------------
DATA:    BEGIN OF RASTER,
           TAGEM1(2)    TYPE P,             " Tage 1
           TAGEM2(2)    TYPE P,             " Tage 2
           TAGEM3(2)    TYPE P,             " Tage 3
           TAGEM4(2)    TYPE P,             " Tage 3
           TAGEM5(2)    TYPE P,             " Tage 3
         END   OF RASTER.

* ------- Kreditlimitrelevante Sonderhauptbuchkennzeichen --------------
DATA:    BEGIN OF SHBTAB OCCURS 5,
           UMSKZ        LIKE T074U-UMSKZ,   " SHB-Kennzeichen
         END   OF SHBTAB.

*------- Pufferungsinformationen ---------------------------------------
DATA:    BEGIN OF XBUFFER,
           KKBER        LIKE T014-KKBER,    " Kontrollbereich
           KUNNR        LIKE KNKK-KUNNR,    " Debitor
         END   OF XBUFFER.

* ------- Einzelfelder -------------------------------------------------
DATA:    AKONT          LIKE TAKOF-AKONT,
         BETRAG         LIKE RFPOS-DMSHB,   " Betrag zum Umrechnen
         BUKRS          LIKE T001-BUKRS,
         FNAME(10)      TYPE C,             " FBS-Name
         DATUM          LIKE SY-DATLO,
         HABEN          LIKE KNC1-UM01H,
         GD_RC          LIKE SY-SUBRC,
         h_anzxx(8)     type p,
         LIN            LIKE SY-TFILL,      " Anz. Zeilen in knkkf1tab
         MLIN           LIKE SY-TFILL VALUE '100',
         PERIOD(2)      TYPE C,             " Laufvariable
         PERIODEN(32)   TYPE C
                          VALUE '01020304050607080910111213141516',
         RATE           LIKE TCURR-UKURS,
         RCODE          LIKE SY-SUBRC,
         RCCV           LIKE SY-SUBRC,
         RCRFC          LIKE SY-SUBRC,
         RCKNKK         LIKE SY-SUBRC,
         REFE           TYPE KNC1-UMSAV, "AFLE ENABLEMENT REFE(10)       TYPE P,
         REFE1(8)       TYPE P,
         REFE2(8)       TYPE P,
         REFE3          type KNC1-UMSAV, "AFLE ENABLEMENT REFE3(8)       TYPE P,
         REFE4          type UMXXU,      "AFLE Enablement REFE4(8)       TYPE P,
         SAV_BUKRS     LIKE T001-BUKRS,
         SAV_KUNNR     LIKE KNB1-KUNNR,
         SAV_RCCV       LIKE SY-SUBRC,
         SOLL           LIKE KNC1-UM01S,
         SYDA           LIKE SY-DATUM,
         SYUZ           LIKE SY-UZEIT,
         TABIX          LIKE SY-TABIX,
         UMNNS          LIKE KNC1-UM01S,
         UMNNH          LIKE KNC1-UM01H,
         UMSATZ         LIKE KNC1-UM01U,
         UZEIT          LIKE SY-UZEIT,
         VERTA          TYPE I,
         VERST          TYPE I,
         VJAHR          LIKE KNC1-GJAHR,    " Vorjahr
         XCRCV          LIKE BOOLE-BOOLE,   " Erstellung des Vektors?
         XNDAT          LIKE BOOLE-BOOLE,   " Daten veraltet?
         XODAT          LIKE BOOLE-BOOLE,   " Daten nicht ermittelbar?
         XVORODAT       LIKE BOOLE-BOOLE,   " Daten vorher veraltet?
         XVORNDAT       LIKE BOOLE-BOOLE,
         ZWSAL          TYPE SALDO_F42B.    "AFLE ENABLEMENT ZWSAL(8)       TYPE P.             " Zwischensumme Saldo

* ------- Ranges für SELECT --------------------------------------------
RANGES:  FILKD          FOR BSID-FILKD.
RANGES:  RANGE_BUKRS    FOR KNB1-BUKRS.

* ------ Codepage-Unabhängigkeit ---------------------------------------
DATA:    VARIHOCH(2)         TYPE C.
DATA:    BEGIN OF UMSETZ,
           CHAR1             TYPE C VALUE ' ',
           HEXA1             TYPE X VALUE 'FF',
         END   OF UMSETZ.
DATA  MSG_TEXT(80) TYPE C.
DATA  VERZN.
DATA  FI LIKE BOOLE-BOOLE.
DATA  SD LIKE BOOLE-BOOLE.
DATA  RECEIVER LIKE BDBAPIDEST.
DATA  BEGIN OF FILTEROBJEKT_WERTE OCCURS 1.
        INCLUDE STRUCTURE BDI_FOBJ.
DATA  END   OF FILTEROBJEKT_WERTE.
DATA  RCRECEIVER LIKE SY-SUBRC.
DATA  FI_LOGSYS LIKE TBDLS-LOGSYS.
DATA  SD_LOGSYS LIKE TBDLS-LOGSYS.
DATA  OWNLOGSYS LIKE TBDLS-LOGSYS.
DATA  BAPIRETURN LIKE BAPIRETURN1.
DATA  LAST_KUNNR LIKE KNKK-KUNNR.
DATA  SAVE_KUNNR LIKE KNB1-KUNNR.
DATA  BEGIN OF TAKOFTAB OCCURS 1.
        INCLUDE STRUCTURE TAKOF.
DATA:   NOENTRY(1) TYPE C,
      END   OF TAKOFTAB.
DATA  GD_DUE_DATE.
DATA: GD_LASTKKBER LIKE T014-KKBER.
*------- Puffer buktab ------------------------------------------------
  DATA     BEGIN OF GT_LASTBUKTAB OCCURS 5.
          INCLUDE STRUCTURE T001CM.
  DATA     END   OF GT_LASTBUKTAB.
