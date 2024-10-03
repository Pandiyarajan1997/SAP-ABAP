REPORT ZRFKOPR10 MESSAGE-ID FR LINE-SIZE 132 NO STANDARD PAGE HEADING.

TABLES: B0SG,                                               "#EC NEEDED
        LFA1,                          "Daten auf Mandantenebene
        LFB1,                          "Daten auf Buchungskreisebene
        LFC1,                          "Verkehrszahlen
        LFC3,                          "Sonderumsätze
        BSIK,                          "Offene Posten
        ZBSEGA,
        BSAK.                          "created By prabhu on 04.04.2020

TABLES: BHDGD,
        T001,
        T001S,
        T074T,
        T074U,
        TBSL,
        TBSLT,                                              "#EC NEEDED
        TCURX,
        ADRS,
        RFPDO,
        RFPDO1,
        RFPDO3,
        RFSDO,
        FAEDE,
        T077K.
FIELD-SYMBOLS: <F1>.

DATA: LO_WRITER TYPE REF TO CL_DOPR_WRITER,
      L_TITLE_1 TYPE STRING,
      L_TITLE_PART2 TYPE STRING,
      L_HLP_TXT TYPE C LENGTH 130,
      L_HLP_TXT1 TYPE C LENGTH 15,
      L_HLP_STRING TYPE STRING,
      L_RASTER  TYPE P,
      L_HLP_LINES TYPE I,
      L_HLP_I TYPE I.

*Hilfsfelder
*---Prüfung ob mehrere Hauswährungen verarbeitet werden.
DATA:    CFAKT(3)      TYPE P,
         CHECKSALDO(8) TYPE P,
         CHECKAGOBL(8) TYPE P,
         WAERS LIKE T001-WAERS,
         WAERS2        LIKE T001-WAERS,
         WFLAG2(1)     TYPE P VALUE '0'.

*---Ermittlung aktuelles Geschäftsjahr über Funktionsbaustein.
DATA: CURRY LIKE BSIK-GJAHR.
*---Zeilenanzahl fü Adressausgabe -----------------------------------*
DATA: ZEILENANZAHL LIKE ADRS-ANZZL VALUE 7.

* Hilfsfelder
* -----------------------------------------------------------
DATA: CHAR1(1)   TYPE C.
DATA: FLAG1(1)   TYPE C.
DATA: FLAG2(1)   TYPE C.
DATA: FLTOP(1)   TYPE C.
DATA: COUNT TYPE P.
DATA: COUN1 TYPE I.

*--------------------------------------------------------------------*
*---- 'H' =   Hilfsfelder, die jederzeit fuer Berechnungen ver-  ----*
*---- wendet werden koennen. ----------------------------------------*
*--------------------------------------------------------------------*
DATA: BEGIN OF H,
        STICHTAG(8),
        OFFSET(2) TYPE P,
        OFFSE1(2) TYPE P,
        SOLL      LIKE LFC1-UM01S,
        HABEN     LIKE LFC1-UM01H,
        SALDO     LIKE LFC1-UMSAV,
        SALD2     LIKE LFC1-UMSAV,
        SHBKZ     LIKE LFC3-SHBKZ,     "Sonderhauptbuchkennzeichen
        SALDV     LIKE LFC3-SALDV,     "Sonderhauptbuch-Saldovortrag
        SHBLS     LIKE LFC3-SOLLL,     "Sonderhauptbuch-Lfd.-Saldo
        SHBSL     LIKE LFC3-SOLLL,     "Sonderhauptbuch-Lfd.-SOLL
        SHBHB     LIKE LFC3-HABNL,     "Sonderhauptbuch-Lfd.-HABEN
        TEXT(15),
        UMLOW     LIKE BSIK-UMSKZ,     "Umsatzkennzeichen
        UMHIG     LIKE BSIK-UMSKZ,     "Umsatzkennzeichen
      END   OF H.
*--------------------------------------------------------------------*
*---- 'C' =   Zwischenergebnisse, die aus Feldern des C-Segmentes ---*
*---- berechnet werden. ---------------------------------------------*
*--------------------------------------------------------------------*
DATA: BEGIN OF C,
        SALDO     TYPE P,              "Saldo
        UMKZ1     LIKE LFC3-SHBKZ,                          "SHBKZ 1
        SUMS1     TYPE P,              "Sonderumsatz 1
        UMKZ2     LIKE LFC3-SHBKZ,                          "SHBKZ 2
        SUMS2     TYPE P,              "Sonderumsatz 2
        UMKZ3     LIKE LFC3-SHBKZ,                          "SHBKZ 3
        SUMS3     TYPE P,              "Sonderumsatz 3
        UMKZ4     LIKE LFC3-SHBKZ,                          "SHBKZ 4
        SUMS4     TYPE P,              "Sonderumsatz 4
        UMKZ5     LIKE LFC3-SHBKZ,                          "SHBKZ 5
        SUMS5     TYPE P,              "Sonderumsatz 5
        UMKZ6     LIKE LFC3-SHBKZ,                          "SHBKZ 6
        SUMS6     TYPE P,              "Sonderumsatz 6
        UMKZ7     LIKE LFC3-SHBKZ,                          "SHBKZ 7
        SUMS7     TYPE P,              "Sonderumsatz 7
        UMKZ8     LIKE LFC3-SHBKZ,                          "SHBKZ 8
        SUMS8     TYPE P,              "Sonderumsatz 8
        UMKZ9     LIKE LFC3-SHBKZ,                          "SHBKZ 9
        SUMS9     TYPE P,              "Sonderumsatz 9
        UMKZ10    LIKE LFC3-SHBKZ,                          "SHBKZ 10
        SUMS10    TYPE P,              "Sonderumsatz 10
        SONOB     TYPE P,              "Sonstige Umsatz-Kz
        BABZG     TYPE P,              "Berechtigte Abzuege
        UABZG     TYPE P,              "Unberechtigte Abzuege
        KZINS     TYPE P,              "Zinszahlungen
        KUMUM     TYPE P,              "Umsatz
        KUMAG     TYPE P,              "Kum. Jahresausgleich
        AGOBLI LIKE LFC1-UMSAV,        "Gesamt-Obligo (absolut)
        LFTAGE(3) TYPE P,              "Langfristige Überzugstage
        MFTAGE(3) TYPE P,              "Mittelfristige Überzugstage
        KFTAGE(3) TYPE P,              "Kurzfristige Überzugstage
        ZVTYP(1)    TYPE C,            "Flag Skonto oder Nettozahler
        ZVPER(6)    TYPE C,            "letze Zahlungsperiode
        ZVVERZUG(8) TYPE P,            "Durchschittliche Verzugst
      END   OF C.
*--------------------------------------------------------------------*
*---- 'C2'=   Zwischenergebnisse, die aus Feldern des C-Segmentes ---*
*---- berechnet werden. ---------------------------------------------*
*--------------------------------------------------------------------*
DATA: BEGIN OF C2 OCCURS 0,
        BUKRS     LIKE LFC1-BUKRS,
        SALDO     TYPE P,              "Saldo
        UMKZ1     LIKE LFC3-SHBKZ,                          "SHBKZ 1
        SUMS1     TYPE P,              "Sonderumsatz 1
        UMKZ2     LIKE LFC3-SHBKZ,                          "SHBKZ 2
        SUMS2     TYPE P,              "Sonderumsatz 2
        UMKZ3     LIKE LFC3-SHBKZ,                          "SHBKZ 3
        SUMS3     TYPE P,              "Sonderumsatz 3
        UMKZ4     LIKE LFC3-SHBKZ,                          "SHBKZ 4
        SUMS4     TYPE P,              "Sonderumsatz 4
        UMKZ5     LIKE LFC3-SHBKZ,                          "SHBKZ 5
        SUMS5     TYPE P,              "Sonderumsatz 5
        UMKZ6     LIKE LFC3-SHBKZ,                          "SHBKZ 6
        SUMS6     TYPE P,              "Sonderumsatz 6
        UMKZ7     LIKE LFC3-SHBKZ,                          "SHBKZ 7
        SUMS7     TYPE P,              "Sonderumsatz 7
        UMKZ8     LIKE LFC3-SHBKZ,                          "SHBKZ 8
        SUMS8     TYPE P,              "Sonderumsatz 8
        UMKZ9     LIKE LFC3-SHBKZ,                          "SHBKZ 9
        SUMS9     TYPE P,              "Sonderumsatz 9
        UMKZ10    LIKE LFC3-SHBKZ,                          "SHBKZ 10
        SUMS10    TYPE P,              "Sonderumsatz 10
        SONOB     TYPE P,              "Sonstige Umsatz-Kz
        BABZG     TYPE P,              "Berechtigte Abzuege
        UABZG     TYPE P,              "Unberechtigte Abzuege
        KZINS     TYPE P,              "Zinszahlungen
        KUMUM     TYPE P,              "Umsatz
        KUMAG     TYPE P,              "Kum. Jahresausgleich
        AGOBLI LIKE LFC1-UMSAV,        "Gesamt-Obligo (absolut)
        LFTAGE(3) TYPE P,              "Langfristige Überzugstage
        MFTAGE(3) TYPE P,              "Mittelfristige Überzugstage
        KFTAGE(3) TYPE P,              "Kurzfristige Überzugstage
        ZVTYP(1)    TYPE C,            "Flag Skonto oder Nettozahler
        ZVPER(6)    TYPE C,            "letze Zahlungsperiode
        ZVVERZUG(8) TYPE P,            "Durchschittliche Verzugst
      END   OF C2.
*--------------------------------------------------------------------*
*---- 'C3'=   Zwischenergebnisse, die aus Feldern des C-Segmentes ---*
*---- berechnet werden. ---------------------------------------------*
*--------------------------------------------------------------------*
DATA: BEGIN OF C3,
        SALDO     TYPE P,              "Saldo
        UMKZ1     LIKE LFC3-SHBKZ,                          "SHBKZ 1
        SUMS1     TYPE P,              "Sonderumsatz 1
        UMKZ2     LIKE LFC3-SHBKZ,                          "SHBKZ 2
        SUMS2     TYPE P,              "Sonderumsatz 2
        UMKZ3     LIKE LFC3-SHBKZ,                          "SHBKZ 3
        SUMS3     TYPE P,              "Sonderumsatz 3
        UMKZ4     LIKE LFC3-SHBKZ,                          "SHBKZ 4
        SUMS4     TYPE P,              "Sonderumsatz 4
        UMKZ5     LIKE LFC3-SHBKZ,                          "SHBKZ 5
        SUMS5     TYPE P,              "Sonderumsatz 5
        UMKZ6     LIKE LFC3-SHBKZ,                          "SHBKZ 6
        SUMS6     TYPE P,              "Sonderumsatz 6
        UMKZ7     LIKE LFC3-SHBKZ,                          "SHBKZ 7
        SUMS7     TYPE P,              "Sonderumsatz 7
        UMKZ8     LIKE LFC3-SHBKZ,                          "SHBKZ 8
        SUMS8     TYPE P,              "Sonderumsatz 8
        UMKZ9     LIKE LFC3-SHBKZ,                          "SHBKZ 9
        SUMS9     TYPE P,              "Sonderumsatz 9
        UMKZ10    LIKE LFC3-SHBKZ,                          "SHBKZ 10
        SUMS10    TYPE P,              "Sonderumsatz 10
        SONOB     TYPE P,              "Sonstige Umsatz-Kz
        BABZG     TYPE P,              "Berechtigte Abzuege
        UABZG     TYPE P,              "Unberechtigte Abzuege
        KZINS     TYPE P,              "Zinszahlungen
        KUMUM     TYPE P,              "Umsatz
        KUMAG     TYPE P,              "Kum. Jahresausgleich
        AGOBLI LIKE LFC1-UMSAV,        "Gesamt-Obligo (absolut)
      END   OF C3.

DATA: SHBETRAG LIKE ZBSEGA-DMSHB.       "TYPE P.
*--------------------------------------------------------------------*
*---- 'RTAB' = Rastertabelle fuer offene Posten ---------------------*
*--------------------------------------------------------------------*
DATA: BEGIN OF RTAB OCCURS 30,
        SORTK(1)   TYPE C,             "0 = Summe Gesber
                                       "1 = Summe aller Gesber
                                       "2 = Umsatzdaten
        BUKRS LIKE BSIK-BUKRS,
        GSBER LIKE BSIK-GSBER,
        WAERS LIKE BSIK-WAERS,
        RAART TYPE C,                  "Rasterart
                                       "1 = Netto-Faelligkeit
                                       "2 = Skonto1-Faelligkeit
                                       "3 = Zahlungseingang
                                       "4 = Ueber-Faelligkeit
        XGUTS TYPE C,                  "Gutschrift
        KUMUM TYPE P,                  "Umsatz
        ANZAH TYPE P,                  "Anzahlungen
        OPSUM TYPE P,                  "Offene Posten Summe
        RAST1 TYPE P,                  "Rasterfeld 1
        RAST2 TYPE P,                  "Rasterfeld 2
        RAST3 TYPE P,                  "Rasterfeld 3
        RAST4 TYPE P,                  "Rasterfeld 4
        RAST5 TYPE P,                  "Rasterfeld 5
        RAST6 TYPE P,                  "Rasterfeld 6
        LIFNR LIKE LFA1-LIFNR,
        KTOKK LIKE LFA1-KTOKK,
        SORTL LIKE LFA1-SORTL,
        LAND1 LIKE LFA1-LAND1,
      END   OF RTAB.
*--------------------------------------------------------------------*
*---- 'RBUS' = Rastertabelle fuer Summen pro Sachbearbeiter ---------*
*--------------------------------------------------------------------*
DATA: BEGIN OF RBUS OCCURS 30,
        SORTK(1)   TYPE C,             "0 = Summe Gesber
                                       "1 = Summe aller Gesber
                                       "2 = Umsatzdaten
        BUKRS LIKE BSIK-BUKRS,
        GSBER LIKE BSIK-GSBER,
        WAERS LIKE BSIK-WAERS,
        RAART TYPE C,                  "Rasterart
                                       "1 = Netto-Faelligkeit
                                       "2 = Skonto1-Faelligkeit
                                       "3 = Zahlungseingang
                                       "4 = Ueber-Faelligkeit
        XGUTS TYPE C,                  "Gutschrift
        KUMUM TYPE P,                  "Umsatz
        ANZAH TYPE P,                  "Anzahlungen
        OPSUM TYPE P,                  "Offene Posten Summe
        RAST1 TYPE P,                  "Rasterfeld 1
        RAST2 TYPE P,                  "Rasterfeld 2
        RAST3 TYPE P,                  "Rasterfeld 3
        RAST4 TYPE P,                  "Rasterfeld 4
        RAST5 TYPE P,                  "Rasterfeld 5
        RAST6 TYPE P,                  "Rasterfeld 6
      END   OF RBUS.
*--------------------------------------------------------------------*
*---- 'RBUK' = Rastertabelle fuer Summen pro Buchungskreis  ---------*
*--------------------------------------------------------------------*
DATA: BEGIN OF RBUK OCCURS 30,
        SORTK(1)   TYPE C,             "0 = Summe Gesber
                                       "1 = Summe aller Gesber
                                       "2 = Umsatzdaten
        BUKRS LIKE BSIK-BUKRS,
        GSBER LIKE BSIK-GSBER,
        WAERS LIKE BSIK-WAERS,
        RAART TYPE C,                  "Rasterart
                                       "1 = Netto-Faelligkeit
                                       "2 = Skonto1-Faelligkeit
                                       "3 = Zahlungseingang
                                       "4 = Ueber-Faelligkeit
        XGUTS TYPE C,                  "Gutschrift
        KUMUM TYPE P,                  "Umsatz
        ANZAH TYPE P,                  "Anzahlungen
        OPSUM TYPE P,                  "Offene Posten Summe
        RAST1 TYPE P,                  "Rasterfeld 1
        RAST2 TYPE P,                  "Rasterfeld 2
        RAST3 TYPE P,                  "Rasterfeld 3
        RAST4 TYPE P,                  "Rasterfeld 4
        RAST5 TYPE P,                  "Rasterfeld 5
        RAST6 TYPE P,                  "Rasterfeld 6
      END   OF RBUK.
*--------------------------------------------------------------------*
*---- 'RSUM' = Rastertabelle pro Währung über alle Buchungskreise ---*
*--------------------------------------------------------------------*
DATA: BEGIN OF RSUM OCCURS 30,
        SORTK(1)   TYPE C,             "0 = Summe Gesber
                                       "1 = Summe aller Gesber
        WAERS LIKE BSIK-WAERS,
        RAART TYPE C,                  "Rasterart
                                       "1 = Netto-Faelligkeit
                                       "2 = Skonto1-Faelligkeit
                                       "3 = Zahlungseingang
                                       "4 = Ueber-Faelligkeit
        XGUTS TYPE C,                  "Gutschrift
        KUMUM TYPE P,                  "Umsatz
        ANZAH TYPE P,                  "Anzahlungen
        OPSUM TYPE P,                  "Offene Posten Summe
        RAST1 TYPE P,                  "Rasterfeld 1
        RAST2 TYPE P,                  "Rasterfeld 2
        RAST3 TYPE P,                  "Rasterfeld 3
        RAST4 TYPE P,                  "Rasterfeld 4
        RAST5 TYPE P,                  "Rasterfeld 5
        RAST6 TYPE P,                  "Rasterfeld 6
      END   OF RSUM.

*--------------------------------------------------------------------*
*---- interne Tabelle für Periodenabgrenzung-------------------------*
*--------------------------------------------------------------------*
RANGES: BMONAT FOR RFPDO-DOPRBMON.

*--------------------------------------------------------------------*
*---- In die Felder RP01 bis RP05 werden dynamisch die von aussen ---*
*---- eingegebenen Rasterpunkte uebertragen -------------------------*
*--------------------------------------------------------------------*
DATA: RP01(2)   TYPE P,                                     "   0
      RP02(2)   TYPE P,                                     "  20
      RP03(2)   TYPE P,                                     "  40
      RP04(2)   TYPE P,                                     "  80
      RP05(2)   TYPE P,                                     " 100
      RP06(3)   TYPE P,                "   1
      RP07(3)   TYPE P,                "  21
      RP08(3)   TYPE P,                "  41
      RP09(3)   TYPE P,                "  81
      RP10(3)   TYPE P.                " 101
*--------------------------------------------------------------------*
*---- In die Felder RC01 bis RC10 werden die Rasterpunkte in --------*
*---- charakterform abgestellt. (fuer REPLACE-Funktion in Variabler -*
*---- Ueberschrift) -------------------------------------------------*
*--------------------------------------------------------------------*
DATA: RC01(4)   TYPE C,                                     "  0
      RC02(4)   TYPE C,                                     "  20
      RC03(4)   TYPE C,                                     "  40
      RC04(4)   TYPE C,                                     "  80
      RC05(4)   TYPE C,                                     " 100
      RC06(4)   TYPE C,                                     "   1
      RC07(4)   TYPE C,                                     "  21
      RC08(4)   TYPE C,                                     "  41
      RC09(4)   TYPE C,                                     "  81
      RC10(4)   TYPE C.                                     " 101

*--------------------------------------------------------------------*
*---- Felder für Umsatzkennzeichen ----------------------------------*
*---- für Ausweis der Sonderumsätze----------------------------------*
*--------------------------------------------------------------------*
DATA: HUMKZ1    LIKE LFC3-SHBKZ,
      HUMKZ2    LIKE LFC3-SHBKZ,
      HUMKZ3    LIKE LFC3-SHBKZ,
      HUMKZ4    LIKE LFC3-SHBKZ,
      HUMKZ5    LIKE LFC3-SHBKZ,
      HUMKZ6    LIKE LFC3-SHBKZ,
      HUMKZ7    LIKE LFC3-SHBKZ,
      HUMKZ8    LIKE LFC3-SHBKZ,
      HUMKZ9    LIKE LFC3-SHBKZ,
      HUMKZ10   LIKE LFC3-SHBKZ.

*---- GBZAEHL - In diesem Feld wird vermerkt, fuer wieviele Ge- ------*
*----           schaeftsbereiche ein OP-Raster ausgegeben wird. ------*
*----           Wird das Raster nur fuer einen Geschaeftsbereich ge- -*
*----           druckt, so entfaellt das Summen-Raster. --------------*
DATA: GBZAEHL(3) TYPE P.

*---- TOP-FLAG '1' = bei TOP-OF-PAGE Einzelpostenueberschrift ausg. --*
*----          '2' = bei TOP-OF-PAGE Ueberschrift fuer Raster ausgeb. *
*----          '3' = bei TOP-OF-PAGE ULINE ausgeben. -----------------*
*----          '4' = bei TOP-OF-PAGE Stammsatzueberschrift ausgeben --*
DATA: TOP-FLAG(1) TYPE C.                                   "#EC *
*---- TOP-FLA2 ' ' = bei TOP-OF-PAGE Summenüberschrift ---------------*
*----          'X' = bei TOP-OF-PAGE Ueberschrift fuer Raster ausgeb. *
DATA: TOP-FLA2(1) TYPE C.                                   "#EC *

DATA: G_EX_PRINT_SEL  TYPE BOOLEAN.                         "1021583
DATA: BATCH_OP_HEADER TYPE BOOLEAN.

*---- SEL-STAMM  'J' = Stammsatz wird ausgewertet                     *
*---- SEL-POSTN  'J' = Stammsatz hat Posten gerastert                 *
*----            'N' = Stammsatz hat keine Posten gerastert           *
*---- SEL-POST2  'J' = Stammsatz hat Posten gerastert                 *
*----            'N' = Stammsatz hat keine Posten gerastert           *
DATA: BEGIN OF SEL,
        STAMM(1) TYPE C,
        POSTN(1) TYPE C,
        POST2(1) TYPE C,
      END   OF SEL.

*---- SATZART  '1' = Stammdaten --------------------------------------*
*----          '2' = Faelligkeitsraster ------------------------------*
*----          '3' = Einzelposten ------------------------------------*
DATA: SATZART(1) TYPE C.

*---- RART  =  Erste ausgewaehlte Rasterart --------------------------*
DATA: RART(1)    TYPE C.
*---- TAGE  =  Tage nach denen die Posten sortiert sind --------------*
DATA: TAGE(4)    TYPE P,
*---- NTAGE =  Tage fuer Netto-Faelligkeit ---------------------------*
      NTAGE(4)   TYPE P,
*---- STAGE =  Tage fuer Skonto1-Faelligkeit -------------------------*
      STAGE(4)   TYPE P,
*---- ZTAGE =  Tage fuer voraussichtlichen Zahlungseingang -----------*
      ZTAGE(4)   TYPE P,
*---- UTAGE =  Tage fuer Ueber-Faelligkeit ---------------------------*
      UTAGE(4)   TYPE P.

*---- RASTERUU dient zur Sortierung der Einzelposten. Die Posten -----*
*----          gemaess ihrer Rasterung die Werte '1' bis '6' ---------*
DATA: RASTERUU(1) TYPE C.

DATA: BEGIN OF GB,
        GSBER  LIKE BSIK-GSBER,
        WAERS  LIKE BSIK-WAERS,
      END   OF GB.

*---------------------------------------------------------------------*
*---- SAVE_FELDER ---------------------------------------------------*
*---------------------------------------------------------------------*
DATA: SAVE_GSBER LIKE BSIK-GSBER.
DATA: SAVE_WAERS LIKE BSIK-GSBER.

*---------------------------------------------------------------------*
*---- Variable Ueberschriften ----------------------------------------*
*---------------------------------------------------------------------*
DATA: BEGIN OF VARUEB1,
        FELD1(40)   TYPE C,
        FELD2(14)   TYPE C,
        FELD3(14)   TYPE C,
        FELD4(14)   TYPE C,
        FELD5(14)   TYPE C,
        FELD6(14)   TYPE C,
        FELD7(14)   TYPE C,
      END   OF VARUEB1.

DATA: BEGIN OF VARUEB2,
        FELD1(40)   TYPE C,
        FELD2(14)   TYPE C,
        FELD3(14)   TYPE C,
        FELD4(14)   TYPE C,
        FELD5(14)   TYPE C,
        FELD6(14)   TYPE C,
        FELD7(14)   TYPE C,
      END   OF VARUEB2.

DATA: VARUEB3(132),
      VARUEB5(132),
      VARTXT1(40).

*---------------------------------------------------------------------*
*---- Variable für Ausgabe der Sonderumsätze--------------------------*
*---------------------------------------------------------------------*
DATA: SHBBEZ LIKE T074T-LTEXT.
DATA: ASUMS  TYPE P.

*---------------------------------------------------------------------*
*---- Interne Tabelle für Bezeichnungen der SHBKZ---------------------*
*---------------------------------------------------------------------*
DATA: BEGIN OF BEZSHB OCCURS 10,
        SHBKZ LIKE T074T-SHBKZ,
        LTEXT LIKE T074T-LTEXT,
      END OF BEZSHB.

*---------------------------------------------------------------------*
*---- Interne Tabelle für Zwischenspeicherung ------------------------*
*---------------------------------------------------------------------*
DATA: BEGIN OF BLKEY,
        BUKRS LIKE BSIK-BUKRS,
        BELNR LIKE BSIK-BELNR,
        GJAHR LIKE BSIK-GJAHR,
        BUZEI LIKE BSIK-BUZEI,
      END   OF BLKEY.

DATA: BEGIN OF RTAGE,
        NTAGE LIKE NTAGE,
        STAGE LIKE STAGE,
        ZTAGE LIKE ZTAGE,
        UTAGE LIKE UTAGE,
     END   OF RTAGE.

DATA: BEGIN OF HBSIK OCCURS 10.
        INCLUDE STRUCTURE BSIK.
        INCLUDE STRUCTURE ZBSEGA.
        INCLUDE STRUCTURE RTAGE.
DATA: END   OF HBSIK.

DATA: BEGIN OF GIT_HBSIK OCCURS 10.
        INCLUDE STRUCTURE BSIK.
        INCLUDE STRUCTURE ZBSEGA.
        INCLUDE STRUCTURE RTAGE.
DATA: END   OF GIT_HBSIK.

DATA : GS_HBSIK LIKE LINE OF GIT_HBSIK.

DATA: BEGIN OF REFBL OCCURS 10.
        INCLUDE STRUCTURE BLKEY.
        INCLUDE STRUCTURE RTAGE.
DATA: END   OF REFBL.

DATA: BEGIN OF HLFB1 OCCURS 10.
        INCLUDE STRUCTURE LFB1.
DATA: END   OF HLFB1.

DATA: BEGIN OF HT001 OCCURS 10.
        INCLUDE STRUCTURE T001.
DATA: END   OF HT001.

*---Saldo der Überfälligen Posten in Hauswährung --------------------*
DATA: UEBSALDO(8) TYPE P.              "Saldo überfällige Posten

*---------------------------------------------------------------------*
*---- Interne Tabelle für Ausgabe der Obligos ------------------------*
*---------------------------------------------------------------------*
DATA: BEGIN OF AOBLIGO OCCURS 12,
        OBART TYPE C,             "Flag für Obligoart 1 = Kontokorrent
                                  "                   2 = SHBKZ
                                  "                   3 = sonstige SHB
        SHBKZ LIKE T074T-SHBKZ,        "SHB-Kennzeichen
        LTEXT LIKE T074T-LTEXT,        "Bezeichnung
        OBLIG TYPE P,                  "Obligobetrag
      END OF AOBLIGO.

*---------------------------------------------------------------------*
*---- Declarationen für Accessibility /ALV GRID ----------------------*
*---------------------------------------------------------------------*
DATA: ACC_MODE TYPE C.
DATA: UEBTEXT(22) TYPE C.
DATA: UEKTEXT(15)  TYPE C.
DATA: TITTEXT(100) TYPE C.
DATA: DATTEXT(10) TYPE C.

DATA: BEGIN OF RTAB_ALV OCCURS 30,
        BUKRS LIKE BSID-BUKRS,
        LIFNR LIKE LFA1-LIFNR,
        KTOKK LIKE LFA1-KTOKK,
        BUSAB LIKE LFB1-BUSAB,
        SORTL LIKE LFA1-SORTL,
        LAND1 LIKE LFA1-LAND1,
        GSBER LIKE BSIK-GSBER,
        WAERS LIKE BSIK-WAERS,
        RAART LIKE RF140-RAART,        "Rasterart
        SHKZG LIKE BSIK-SHKZG,
        KUMUM LIKE RF140-KUMUMHW,      "Umsatz
        ANZAH LIKE RF140-ANZBTHW,      "Anzahlungen
        OPSUM LIKE RF140-GSALDD,       "Offene Posten Summe
        RAST1 LIKE RF140-RAST1,        "Rasterfeld 1
        RAST2 LIKE RF140-RAST2,        "Rasterfeld 2
        RAST3 LIKE RF140-RAST3,        "Rasterfeld 3
        RAST4 LIKE RF140-RAST4,        "Rasterfeld 4
        RAST5 LIKE RF140-RAST5,        "Rasterfeld 5
        RAST6 LIKE RF140-RAST6,        "Rasterfeld 6
        ADRS1 LIKE ADRS-LINE0,                              "1253468
        ADRS2 LIKE ADRS-LINE0,                              "1253468
        ADRS3 LIKE ADRS-LINE0,                              "1253468
        ADRS4 LIKE ADRS-LINE0,                              "1253468
      END   OF RTAB_ALV.

DATA: GD_NO_ANRED TYPE BOOLEAN.                             "1320031

*"General Data
TYPE-POOLS: SLIS.
DATA: G_REPID LIKE SY-REPID,
      G_GRID_TITLE TYPE  LVC_TITLE.
*"Callback
DATA: G_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND',
      G_TOP_OF_PAGE  TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'. "1613289
*"Variants
DATA: GS_VARIANT LIKE DISVARIANT,
      G_SAVE.
*"ALV HEADER
DATA: GT_LISTHEADER  TYPE SLIS_T_LISTHEADER,                "1613289
      GS_LISTHEADER  TYPE SLIS_LISTHEADER.                  "1613289
* Global structure of list
* fieldcatalog
DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE. "#EC *

DATA: G_TABNAME TYPE SLIS_TABNAME VALUE 'RTAB_ALV'.
*DATA GIT_HBSIK.
*DATA GS_HBSIK.

*---------------------------------------------------------------------*
*---- FIELD-GROUPS                            ------------------------*
*---------------------------------------------------------------------*
FIELD-GROUPS:
          HEADER,
          STAMMDATEN,
          OP-RASTER,                                        "#EC *
          EINZELPOSTEN.

INSERT
  LFB1-BUKRS                           " Buchungskreis
  LFB1-BUSAB                           " Sachbearbeiter
  UEBSALDO                             " Saldo überfällige Posten
  LFA1-LIFNR                           " Kontonummer
  LFA1-KTOKK
  LFA1-SORTL
  LFA1-NAME4
  SATZART                              " Satzart
  RTAB-SORTK                           " Sortkz fuer Tabelle RTAB
                                       " '0' = normale Eintraege
                                       " '1' = Summeneintraege
  GB                                   " Geschaeftsbereich
                                       " - GB-GSBER
                                       " - GB-WAERS
  RASTERUU         " Kennzeichen fuer Detailposten bzw Raster
  RTAB-XGUTS       " Flag für Forderungen und Gutschrift
*---------------- ab hier nur fuer Einzelposten ----------------------*
  TAGE                                 " Rastertage  fuer Detailposten
  BSIK-UMSKZ                           " Umsatzkennzeichen
  BSIK-BLART                           " Belegart
  BSIK-BELNR                           " Belegnummer
  BSIK-BUZEI                           " Belegzeile
INTO HEADER.

INSERT
* Addressdaten
  ADRS-LINE0                           " 1. Zeile Adressenaufbereitung
  ADRS-LINE1                           " 2. "     "
  ADRS-LINE2                           " 3. "     "
  ADRS-LINE3                           " 4. "     "
  ADRS-LINE4                           " 5. "     "
  ADRS-LINE5                           " 6. "     "
  ADRS-LINE6                           " 7. "     "
* Umsatzdaten
  C-KUMUM                              " Umsatz
  C-BABZG                              " Berechtigt. Abzuege
  C-UABZG                              " Unberechtigt. Abzuege
  C-KZINS                              " Zinszahlungen
  C-KUMAG                              " Kum. Jahresausgleich
* Obligos
  C-SALDO                              " Saldo ohne SHB-Vorgänge
  C-UMKZ1                                                   "SHBKZ 1
  C-SUMS1                              "Sonderumsatz 1
  C-UMKZ2                                                   "SHBKZ 2
  C-SUMS2                              "Sonderumsatz 2
  C-UMKZ3                                                   "SHBKZ 3
  C-SUMS3                              "Sonderumsatz 3
  C-UMKZ4                                                   "SHBKZ 4
  C-SUMS4                              "Sonderumsatz 4
  C-UMKZ5                                                   "SHBKZ 5
  C-SUMS5                              "Sonderumsatz 5
  C-UMKZ6                                                   "SHBKZ 6
  C-SUMS6                              "Sonderumsatz 6
  C-UMKZ7                                                   "SHBKZ 7
  C-SUMS7                              "Sonderumsatz 7
  C-UMKZ8                                                   "SHBKZ 8
  C-SUMS8                              "Sonderumsatz 8
  C-UMKZ9                                                   "SHBKZ 9
  C-SUMS9                              "Sonderumsatz 9
  C-UMKZ10                                                  "SHBKZ 10
  C-SUMS10                             "Sonderumsatz 10
  C-SONOB                              " Sonst. Obligen
  C-AGOBLI                             " Absolutes Gesamtobligo
* Zahlungdaten
  C-ZVTYP                              "Flag Skonto oder Nettozahler
  C-ZVPER                              "letze Zahlungsperiode
  C-ZVVERZUG                           "Durchschittliche Verzugstage
  LFB1-ZTERM                           "Zahlungsbedingung
  LFB1-XVERR                           "Zahlungsverrechnung
INTO STAMMDATEN.

INSERT
  RTAB-RAART                           "Rasterart
  RTAB-KUMUM                           "Umsatz
  RTAB-ANZAH                           "Anzahlungen
  RTAB-OPSUM                           "Offene Posten Summe
  RTAB-RAST1                           "Rasterfeld 1
  RTAB-RAST2                           "Rasterfeld 2
  RTAB-RAST3                           "Rasterfeld 3
  RTAB-RAST4                           "Rasterfeld 4
  RTAB-RAST5                           "Rasterfeld 5
  RTAB-RAST6                           "Rasterfeld 6
  RTAB-LIFNR
  RTAB-KTOKK
  RTAB-SORTL
  RTAB-LAND1
  RTAB-GSBER                                                "1557468
INTO OP-RASTER.

INSERT
  BSIK-BUDAT                           " Buchungsdatum
  BSIK-BLDAT                           " Belegdatum
  BSIK-CPUDT                           " CPU-Datum
  BSIK-WAERS                           " Wahrungsschluessel
  ZBSEGA-NETDT                          " Nettofaelligkeitsdatum
  BSIK-ZFBDT                           " Zahlungsfristen-Basisdatum
  BSIK-BSCHL                           " Buchungsschluessel
  BSIK-ZLSCH                           " Zahlungsschluessel
  BSIK-MANST                           " Mahnstufe
  SHBETRAG                             " Hauswaehrungsbetrag
  ZBSEGA-DMSHB                          " Hauswaehrungsbetrag
  ZBSEGA-WRSHB                          " Fremwaehrungsbetrag
INTO EINZELPOSTEN.

BEGIN_OF_BLOCK 1.
SELECT-OPTIONS: UEBESAL2 FOR RFSDO-DOPRUEB2.
PARAMETERS:     XNURFORD LIKE RFPDO1-DOPRXNFO.
SELECT-OPTIONS: VERTAGE  FOR RFPDO1-DOPRVZTG.
PARAMETERS:     MONAT    LIKE RFPDO-DOPRBMON.
SELECT-OPTIONS: KKSALDO2 FOR RFSDO-KOPRSAL2,    "Saldovortrag
                AGOBLIG2 FOR RFSDO-KOPRAGO2.    "Absolutes Obligo
SELECT-OPTIONS: AKONTS   FOR LFB1-AKONT,
                AKONTP   FOR BSIK-HKONT.
SELECT-OPTIONS: DD_BUKRS FOR BSIK-BUKRS,
                DD_LIFNR FOR LFA1-LIFNR,
                DD_KTOKK    FOR LFA1-KTOKK,
                DD_SORTL   FOR LFA1-SORTL,
                DD_VTYPE   FOR LFA1-NAME4,
                BUDAT    FOR BSIK-BUDAT,
                BLDAT    FOR BSIK-BLDAT,
                NETDT    FOR BSIK-CPUDT.
PARAMETERS:     N_BELEGE LIKE RFPDO-BPETNBEL DEFAULT 'X',
                STAT_BLG LIKE RFPDO-BPETSBEL.  "Statistische Belege
END_OF_BLOCK 1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-005.
PARAMETERS DD_STIDA TYPE RFPDO-ALLGSTID DEFAULT SY-DATUM.        "created By Prabhu on 31.03.2020
SELECTION-SCREEN END OF BLOCK B3.

BEGIN_OF_BLOCK 2.
PARAMETERS: SORTART  LIKE RFPDO1-KOPRSOAR DEFAULT '1',
            VERDICHT LIKE RFPDO2-DOPRVER2 DEFAULT '0',
            RASTVERD LIKE RFPDO1-KOPRRAST DEFAULT '0',
            KONZVERS LIKE RFPDO-DOPOKONZ,   "Konzernversion
            XBUKRDAT LIKE RFPDO3-ALLGBUKD DEFAULT 0,
            KAUSGABE LIKE RFPDO3-ALLGKAOR.
PARAMETERS: RART-NET LIKE RFPDO-DOPRRNET DEFAULT ' ' NO-DISPLAY. "#EC *
PARAMETERS: RART-SKT LIKE RFPDO-DOPRRSKT DEFAULT ' ' NO-DISPLAY. "#EC *
PARAMETERS: RART-ZHL LIKE RFPDO-DOPRRZHL DEFAULT ' ' NO-DISPLAY. "#EC *
PARAMETERS: RART-UEB LIKE RFPDO-DOPRRUEB DEFAULT 'X' NO-DISPLAY. "#EC *
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(30) TEXT-026 FOR FIELD RASTBIS1.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: RASTBIS1 LIKE RFPDO1-ALLGROGR DEFAULT '000'.
PARAMETERS: RASTBIS2 LIKE RFPDO1-ALLGROGR DEFAULT '020'.
PARAMETERS: RASTBIS3 LIKE RFPDO1-ALLGROGR DEFAULT '040'.
PARAMETERS: RASTBIS4 LIKE RFPDO1-ALLGROGR DEFAULT '080'.
PARAMETERS: RASTBIS5 LIKE RFPDO1-ALLGROGR DEFAULT '100'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(30) TEXT-029 FOR FIELD FAKTOR.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: FAKTOR   LIKE RFPDO-DOPRFAKT DEFAULT '0'.
SELECTION-SCREEN COMMENT 35(1) TEXT-028 FOR FIELD STELLEN.
PARAMETERS: STELLEN  LIKE RFPDO-DOPRFAKT DEFAULT '0'.
SELECTION-SCREEN END OF LINE.
PARAMETERS: PZUOR    LIKE RFPDO2-DOPRZUOR DEFAULT 'X'.
PARAMETERS: XGETAUSW LIKE RFPDO1-DOPRXGAW.
PARAMETERS: UMSATZKZ LIKE RFPDO1-DOPRSHBO. "DEFAULT 'WBSA'.
PARAMETERS: XHITLIST LIKE RFPDO1-DOPRHITL.
PARAMETERS: TITLE    LIKE RFPDO1-ALLGLINE,
            LISTSEP  LIKE RFPDO-ALLGLSEP,
            MIKFICHE LIKE RFPDO-ALLGMIKF.
PARAMETERS: P_LVAR   LIKE GS_VARIANT-VARIANT DEFAULT SPACE MODIF ID 508.
END_OF_BLOCK 2.


AT SELECTION-SCREEN OUTPUT.
  IF ACC_MODE IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = '508'.
        SCREEN-ACTIVE    = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON AKONTS.
  LOOP AT AKONTS.
    PERFORM ALPHAFORMAT(SAPFS000)
      USING AKONTS-LOW AKONTS-LOW.
    PERFORM ALPHAFORMAT(SAPFS000)
      USING AKONTS-HIGH AKONTS-HIGH.
    MODIFY AKONTS.
  ENDLOOP.

AT SELECTION-SCREEN ON AKONTP.
  LOOP AT AKONTP.
    PERFORM ALPHAFORMAT(SAPFS000)
      USING AKONTP-LOW AKONTP-LOW.
    PERFORM ALPHAFORMAT(SAPFS000)
      USING AKONTP-HIGH AKONTP-HIGH.
    MODIFY AKONTP.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LVAR.
  GS_VARIANT-REPORT  = SY-REPID.
  GS_VARIANT-VARIANT = P_LVAR.
  PERFORM F4_FOR_S_LVAR CHANGING GS_VARIANT.
  P_LVAR = GS_VARIANT-VARIANT.

AT SELECTION-SCREEN.
  IF RASTBIS1 GT '998'
  OR RASTBIS2 GT '998'
  OR RASTBIS3 GT '998'
  OR RASTBIS4 GT '998'
  OR RASTBIS5 GT '998'.
    MESSAGE E381.
  ENDIF.

  IF NOT RASTBIS5 IS INITIAL.
    IF  RASTBIS5 GT RASTBIS4
    AND RASTBIS4 GT RASTBIS3
    AND RASTBIS3 GT RASTBIS2
    AND RASTBIS2 GT RASTBIS1.
    ELSE.
      MESSAGE E379.
    ENDIF.
  ELSE.
    IF NOT RASTBIS4 IS INITIAL.
      IF  RASTBIS4 GT RASTBIS3
      AND RASTBIS3 GT RASTBIS2
      AND RASTBIS2 GT RASTBIS1.
      ELSE.
        MESSAGE E379.
      ENDIF.
    ELSE.
      IF NOT RASTBIS3 IS INITIAL.
        IF  RASTBIS3 GT RASTBIS2
        AND RASTBIS2 GT RASTBIS1.
        ELSE.
          MESSAGE E379.
        ENDIF.
      ELSE.
        IF NOT RASTBIS2 IS INITIAL.
          IF  RASTBIS2 GT RASTBIS1.
          ELSE.
            MESSAGE E379.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR BEZSHB.
  REFRESH BEZSHB.
  CONDENSE UMSATZKZ NO-GAPS.
  IF NOT UMSATZKZ(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.
  IF NOT UMSATZKZ+1(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ+1(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.
  IF NOT UMSATZKZ+2(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ+2(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.
  IF NOT UMSATZKZ+3(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ+3(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.
  IF NOT UMSATZKZ+4(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ+4(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.
  IF NOT UMSATZKZ+5(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ+5(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.
  IF NOT UMSATZKZ+6(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ+6(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.
  IF NOT UMSATZKZ+7(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ+7(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.
  IF NOT UMSATZKZ+8(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ+8(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.
  IF NOT UMSATZKZ+9(1) IS INITIAL.
    CLEAR CHAR1.
    MOVE UMSATZKZ+9(1) TO CHAR1.
    PERFORM SHBKZ_PRUEFEN.
  ENDIF.

  IF NOT ACC_MODE IS INITIAL.
    VERDICHT = '6'.
  ENDIF.

  IF VERDICHT = '6'.
    SORTART  = '1'.
    XBUKRDAT = '2'.
  ENDIF.

  CLEAR G_EX_PRINT_SEL.                                     "1021583
  IF SY-UCOMM = 'PRIN'.                                     "1021583
    G_EX_PRINT_SEL = 'X'.                                   "1021583
  ENDIF.                                                    "1021583


INITIALIZATION.
  GET_FRAME_TITLE: 1, 2.
  MONAT = '16'.

  CALL FUNCTION 'GET_ACCESSIBILITY_MODE'
    IMPORTING
      ACCESSIBILITY     = ACC_MODE
    EXCEPTIONS
      ITS_NOT_AVAILABLE = 1
      OTHERS            = 2.

  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF NOT ACC_MODE IS INITIAL.
    G_REPID = SY-REPID.
  ENDIF.


START-OF-SELECTION.

  COMMIT WORK.
  COPY: AKONTS TO KD_AKONT, AKONTP TO KD_HKONT.

  SELECT * FROM T001 APPENDING TABLE HT001
    WHERE BUKRS IN KD_BUKRS.

*- Standardseitenkopf fuellen ---------------------------------------*
  MOVE '0'      TO BHDGD-INIFL.
  MOVE SY-LINSZ TO BHDGD-LINES.
  MOVE SY-UNAME TO BHDGD-UNAME.
  MOVE SY-REPID TO BHDGD-REPID.
  MOVE SY-TITLE TO BHDGD-LINE1.
  MOVE TITLE    TO BHDGD-LINE2.
  MOVE '    '   TO BHDGD-BUKRS.
  MOVE MIKFICHE TO BHDGD-MIFFL.
  MOVE LISTSEP  TO BHDGD-SEPAR.
  MOVE 'BUKRS'  TO BHDGD-DOMAI.
*- OP-Raster und Ueberschriften aufbereiten -------------------------*
  PERFORM RASTER_AUFBAU.
  PERFORM SHB_KENNZEICHEN.

*- Layout falls ACCESSIBILITY_MODE aktiv ----------------------------*
  MOVE:  SY-REPID TO GS_VARIANT-REPORT.                     "1554718
  MOVE:  P_LVAR   TO GS_VARIANT-VARIANT.                    "1554718

  IF N_BELEGE <> SPACE.
    N_BELEGE = 'X'.
    B0SG-XSTAN = 'X'.
  ELSE.
    B0SG-XSTAN = ' '.
  ENDIF.

  IF STAT_BLG <> SPACE.
    STAT_BLG = 'X'.
    B0SG-XSTAS = 'X'.
  ENDIF.

  IF MONAT IS INITIAL
  OR MONAT GT '16'.
    MONAT = '16'.
  ENDIF.
  BMONAT-LOW    = '1'.
  BMONAT-HIGH   = MONAT.
  BMONAT-OPTION = 'BT'.
  BMONAT-SIGN   = 'I'.
  APPEND BMONAT.

GET LFA1.
  CLEAR ADRS.
  MOVE-CORRESPONDING LFA1 TO ADRS.                          "#EC ENHOK
  IF NOT ACC_MODE IS INITIAL.                               "1320031
    CLEAR GD_NO_ANRED.                                      "1320031
    IF ADRS-ANRED IS INITIAL.                               "1320031
      GD_NO_ANRED = 'X'.                                    "1320031
    ENDIF.                                                  "1320031
    CLEAR ADRS-NAME2.                                       "1320031
    CLEAR ADRS-NAME3.                                       "1320031
    CLEAR ADRS-NAME4.                                       "1320031
    CLEAR ADRS-PFACH.                                       "1320031
    CLEAR ADRS-PSTL2.                                       "1320031
    CLEAR ADRS-PFORT.                                       "1320031
  ENDIF.
  MOVE ZEILENANZAHL TO ADRS-ANZZL.
  CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
    EXPORTING
      ADRSWA_IN  = ADRS
    IMPORTING
      ADRSWA_OUT = ADRS.
  IF NOT ACC_MODE IS INITIAL.                               "1320031
    IF GD_NO_ANRED = 'X'.                                   "1320031
      ADRS-LINE3 = ADRS-LINE2.                              "1320031
      ADRS-LINE2 = ADRS-LINE1.                              "1320031
      ADRS-LINE1 = ADRS-LINE0.                              "1320031
      CLEAR ADRS-LINE0.                                     "1320031
    ENDIF.                                                  "1320031
  ENDIF.                                                    "1320031

  IF NOT KONZVERS IS INITIAL.
    CLEAR CHECKSALDO.
    CLEAR CHECKAGOBL.
    CLEAR WAERS2.
    CLEAR WFLAG2.
    CLEAR   HBSIK.
    REFRESH HBSIK.
    CLEAR   REFBL.
    REFRESH REFBL.
    SEL-STAMM  = 'N'.
    SEL-POSTN  = 'N'.
    SEL-POST2  = 'N'.
    CLEAR   RTAB.
    REFRESH RTAB.
    CLEAR   HLFB1.
    REFRESH HLFB1.
    CLEAR   C2.
    REFRESH C2.
    CLEAR   C3.
  ENDIF.

GET LFB1.
* Lfd. Geschaeftsjahr gemaess Stichtag besorgen ---------------------*
  CHECK AKONTS.
  IF KONZVERS IS INITIAL.
    CLEAR CHECKSALDO.
    CLEAR CHECKAGOBL.
    CLEAR HBSIK.
    REFRESH HBSIK.
    CLEAR REFBL.
    REFRESH REFBL.
    SEL-STAMM = 'N'.
    SEL-POSTN = 'N'.
    CLEAR   RTAB.
    REFRESH RTAB.
  ENDIF.
  CLEAR C.
  CLEAR H-SALDO.
  CLEAR H-SALD2.
  CLEAR: GB,
         RASTERUU,
         TAGE.

* laufendes Geschäftsjahr ermitteln
* ---------------------------------
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      BUKRS = LFB1-BUKRS
      DATE  = KD_STIDA
    IMPORTING
      CURRY = CURRY.

  READ TABLE HT001 WITH KEY BUKRS = LFB1-BUKRS.
  T001 = HT001.
  IF  NOT WAERS2 IS INITIAL
  AND WAERS2 NE T001-WAERS.
    WFLAG2 = '1'.
  ENDIF.
  WAERS2 = T001-WAERS.

GET LFC1.
  CHECK: LFC1-GJAHR = CURRY.
  PERFORM CFAKTOR.
* aktuellen Saldo ermitteln (fuer CHECK auf Saldo) ------------------*
  PERFORM SALDO_AKTUELL.
  PERFORM KUM_WERTE.
  SEL-STAMM = 'J'.

GET LFC3.
  CHECK LFC3-GJAHR = CURRY.
*  Errechnen Sonderumsatz-Salden, Gesamtsaldo ------------------------*
*  Trend, Umsatz pro Gesch.Bereich -----------------------------------*
  PERFORM SONDER_UMSAETZE.
  SEL-STAMM = 'J'.

GET BSIK.
  IF KONZVERS IS INITIAL.
    CHECK CHECKSALDO IN KKSALDO2.
    CHECK CHECKAGOBL IN AGOBLIG2.
  ENDIF.
  CHECK AKONTP.
  CASE BSIK-BSTAT.
    WHEN ' '.
      CHECK N_BELEGE EQ 'X'.
    WHEN 'S'.
      CHECK STAT_BLG EQ 'X'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

* Einzelposten werden nur dann weiterverarbeitet, wenn ueberhaupt ---*
* ein OP-Raster gewuenscht wird. ------------------------------------*
  CHECK RASTVERD < '2'.

* Bei SORTART = '2' werden nur Belege verarbeitet, welche in Fremd- -*
* waehrung gebucht sind ---------------------------------------------*
  IF SORTART  = '2'.
    CHECK BSIK-WAERS NE T001-WAERS.
  ENDIF.

  CHECK BSIK-BUDAT LE KD_STIDA.



  CLEAR FAEDE.
  MOVE-CORRESPONDING BSIK TO FAEDE.                         "#EC ENHOK
  FAEDE-KOART = 'K'.

  CALL FUNCTION 'DETERMINE_DUE_DATE'                        "#EC *
    EXPORTING
      I_FAEDE = FAEDE
    IMPORTING
      E_FAEDE = FAEDE
    EXCEPTIONS
      OTHERS  = 1.


  ZBSEGA-NETDT = FAEDE-NETDT.

* TAGE gemaess Rasterart ermitteln -----------------------------------*
* Netto-Faelligkeit --------------------------------------------------*
  NTAGE = FAEDE-NETDT - KD_STIDA.
* Ueber-Faelligkeit --------------------------------------------------*
  UTAGE = KD_STIDA - FAEDE-NETDT.                      "  Commented By Prabhu on 04.04.2020
***  UTAGE =  DD_STIDA - FAEDE-NETDT.                        "  Created By Prabhu on 04.04.2020
* Skonto1-Faelligkeit ------------------------------------------------*
  STAGE = FAEDE-SK1DT - KD_STIDA.
* Voraussichtlicher Zahlungseingang-----------------------------------*
  IF NOT PZUOR    IS INITIAL
  OR NOT KONZVERS IS INITIAL.
    PERFORM EINZELPOSTEN_SAVE.
  ELSE.
* die Einzelposten werden nach den Tagen der ersten Rasterart --------*
* sortiert -----------------------------------------------------------*
    IF RART-NET = 'X'.
      TAGE = NTAGE.
    ELSE.
      IF RART-SKT = 'X'.
        TAGE = STAGE.
      ELSE.
        IF RART-ZHL = 'X'.
          TAGE = ZTAGE.
        ELSE.
          IF RART-UEB = 'X'.
            TAGE = UTAGE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


    CASE BSIK-UMSKS.
*--------------- Anzahlungen sammeln ---------------------------------*
*--------------- auch wenn nicht von aussen abgegrenzt ---------------*
      WHEN 'A'.
        CLEAR RTAB.
        IF BSIK-BSTAT NE 'S'.
          MOVE: BSIK-BUKRS TO RTAB-BUKRS,
                '0'      TO RTAB-SORTK,
                BSIK-GSBER TO RTAB-GSBER,
                RART     TO RTAB-RAART.
          IF SORTART = '2'.
            MOVE BSIK-WAERS TO RTAB-WAERS.
            MOVE ZBSEGA-WRSHB TO RTAB-ANZAH."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
          ELSE.
            IF NOT KONZVERS IS INITIAL.
              MOVE T001-WAERS TO RTAB-WAERS.
              MOVE ZBSEGA-DMSHB TO RTAB-ANZAH."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
            ELSE.
              MOVE ZBSEGA-DMSHB TO RTAB-ANZAH."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
            ENDIF.
          ENDIF.
          CLEAR RTAB-XGUTS.
          IF NOT XGETAUSW IS INITIAL.
            IF ZBSEGA-DMSHB LT 0.
              RTAB-XGUTS = 'X'.
            ENDIF.
          ENDIF.
          MOVE:
          LFA1-LIFNR TO RTAB-LIFNR,
          LFA1-SORTL TO RTAB-SORTL,
          LFA1-LAND1 TO RTAB-LAND1.
          COLLECT RTAB.
*--------------- Summieren ueber alle Geschaeftsbereiche -------------*
          MOVE: '1'      TO RTAB-SORTK,
                '**'     TO RTAB-GSBER.
          COLLECT RTAB.
        ENDIF.
    ENDCASE.

    CHECK: BUDAT,
           BLDAT,
           NETDT.
    SEL-POSTN = 'J'.

    IF SORTART = '1'.
      IF KONZVERS IS INITIAL .
        PERFORM POSTEN_RASTERN USING SPACE.
        MOVE SPACE    TO GB-WAERS.
      ELSE.
        PERFORM POSTEN_RASTERN USING T001-WAERS.
        MOVE T001-WAERS TO GB-WAERS.
      ENDIF.
    ELSE.
      PERFORM POSTEN_RASTERN USING BSIK-WAERS.
      MOVE BSIK-WAERS TO GB-WAERS.
    ENDIF.

*----- Saldoberechnung Überfälligkeitsprüfung
    IF  UTAGE GT '0'
    AND UTAGE IN VERTAGE.
      IF NOT XNURFORD IS INITIAL.
        IF ZBSEGA-DMSHB GT '0'.         "Falls keine Habenpositionen
          H-SALD2        = H-SALD2        + ZBSEGA-DMSHB.
        ENDIF.                         "bei der berechnung berück-
      ELSE.                            "sichtigt werden sollen
        H-SALD2        = H-SALD2        + ZBSEGA-DMSHB.
      ENDIF.
    ENDIF.                             "hier die Sterne entfernen

*---- nur bei Verdichtungsstufe '0' werden EINZELPOSTEN extrahiert --*
    IF VERDICHT = '0'.
      IF  UTAGE GT '0'
      AND UTAGE IN VERTAGE.
        PERFORM EINZELPOSTEN_SICHERN.
      ENDIF.
    ENDIF.
  ENDIF.

GET LFB1 LATE.
  IF KONZVERS IS INITIAL.
    CHECK CHECKSALDO IN KKSALDO2.
    CHECK CHECKAGOBL IN AGOBLIG2.
    IF NOT PZUOR IS INITIAL.
      PERFORM EINZELPOSTEN_LINK.
      PERFORM EINZELPOSTEN_PROC.
    ENDIF.
    CLEAR UEBSALDO.

* Bei SORTART = '2' werden nur dann Stammsatzdaten ausgegeben, wenn -*
* auch Einzelposten gerastert wurden. -------------------------------*
    IF SORTART = '2'.
      CHECK SEL-POSTN = 'J'.
    ENDIF.
    IF NOT KAUSGABE IS INITIAL.
      CHECK SEL-POSTN = 'J'.
    ENDIF.

*----- Modifikation für Überfälligkeitsprüfung
    PERFORM CFAKTOR.
    IF CFAKT NE 0.
      CHECKSALDO = H-SALD2 / CFAKT."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation

    ELSE.
      CHECKSALDO = H-SALD2."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
    ENDIF.
    CHECK CHECKSALDO     IN UEBESAL2.

    IF NOT XHITLIST IS INITIAL.
*----- Multiplikation um richtige Sortierung zu erreichen
      UEBSALDO = CHECKSALDO * -1.
    ENDIF.

*---- nur bei Verdichtungsstufe '0' werden EINZELPOSTEN extrahiert --*
    IF VERDICHT = '0'.
      PERFORM EINZELPOSTEN_EXTRACT.
    ENDIF.

    CLEAR: GB,
           RASTERUU,
           TAGE.
    MOVE '1' TO SATZART.
* Stammdaten extrahieren ---------------------------------------------*
    CLEAR BSIK.
    EXTRACT STAMMDATEN.
* OP-Raster extrahieren ----------------------------------------------*
    SORT RTAB ASCENDING.

    LOOP AT RTAB.
      MOVE:     '2'    TO SATZART,
            RTAB-GSBER TO GB-GSBER,
            RTAB-WAERS TO GB-WAERS,
            RTAB-RAART TO RASTERUU.
      EXTRACT OP-RASTER.
    ENDLOOP.
  ELSE.
    HLFB1 = LFB1.
    APPEND HLFB1.
    MOVE-CORRESPONDING C TO C2.
    C2-BUKRS = LFB1-BUKRS.
    APPEND C2.
  ENDIF.


  IF HBSIK IS NOT INITIAL.
    CLEAR : GIT_HBSIK,GS_HBSIK.
    LOOP AT HBSIK INTO GS_HBSIK.
      APPEND GS_HBSIK TO GIT_HBSIK.
      CLEAR GS_HBSIK.
    ENDLOOP.
  ENDIF.

  EXPORT GIT_HBSIK "#EC CI_FLDEXT_OK[2610650]
   "Added by SPLABAP during code remediation
  TO MEMORY ID 'BSIK'.

GET LFA1 LATE.
  IF NOT KONZVERS IS INITIAL.
    CLEAR H-SALD2.
    IF WFLAG2 IS INITIAL.
      CHECK CHECKSALDO IN KKSALDO2.
      CHECK CHECKAGOBL IN AGOBLIG2.
    ENDIF.
    IF NOT PZUOR IS INITIAL.
      PERFORM EINZELPOSTEN_LINK.
    ENDIF.
    CLEAR UEBSALDO.

    CLEAR SEL-POST2.
    LOOP AT HLFB1.
      LFB1 = HLFB1.
      LOOP AT C2
        WHERE BUKRS = LFB1-BUKRS.
        CLEAR C.
        MOVE-CORRESPONDING C2 TO C.
        EXIT.
      ENDLOOP.

*      PERFORM SUMM_C3.
      CLEAR SEL-POSTN.
      PERFORM EINZELPOSTEN_PROC.

* Bei SORTART = '2' werden nur dann Stammsatzdaten ausgegeben, wenn -*
* auch Einzelposten gerastert wurden. -------------------------------*
      IF SORTART = '2'.
        IF SEL-POSTN NE 'J'.
          DELETE HLFB1.
        ENDIF.
      ENDIF.
      IF NOT KAUSGABE IS INITIAL.
        IF SEL-POSTN NE 'J'.
          DELETE HLFB1.
        ENDIF.
      ENDIF.
      IF SEL-POSTN =  'J'.
        SEL-POST2 = 'J'.
      ENDIF.
    ENDLOOP.

*----- Modifikation für Überfälligkeitsprüfung
    PERFORM CFAKTOR.
    IF CFAKT NE 0.
      CHECKSALDO = H-SALD2 / CFAKT."#EC CI_FLDEXT_OK[2610650]
 "Added by SPLABAP during code remediation
    ELSE.
      CHECKSALDO = H-SALD2."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
    ENDIF.
    CHECK CHECKSALDO     IN UEBESAL2.

    IF NOT XHITLIST IS INITIAL.
*----- Multiplikation um richtige Sortierung zu erreichen
      UEBSALDO = CHECKSALDO * -1.
    ENDIF.

    IF SORTART = '2'.
      CHECK SEL-POST2 = 'J'.
    ENDIF.
    IF NOT KAUSGABE IS INITIAL.
      CHECK SEL-POST2 = 'J'.
    ENDIF.

    LOOP AT HLFB1.
      LFB1 = HLFB1.
      LOOP AT C2
        WHERE BUKRS = LFB1-BUKRS.
        CLEAR C.
        MOVE-CORRESPONDING C2 TO C.
        EXIT.
      ENDLOOP.
*---- nur bei Verdichtungsstufe '0' werden EINZELPOSTEN extrahiert --*
      IF VERDICHT = '0'.
        PERFORM EINZELPOSTEN_EXTRACT.
      ENDIF.

      CLEAR: GB,
             RASTERUU,
             TAGE.
      MOVE '1' TO SATZART.
* Stammdaten extrahieren ---------------------------------------------*
      CLEAR BSIK.
      EXTRACT STAMMDATEN.
* OP-Raster extrahieren ----------------------------------------------*
      SORT RTAB ASCENDING.

      LOOP AT RTAB
        WHERE BUKRS = LFB1-BUKRS.
        MOVE:     '2'    TO SATZART,
              RTAB-GSBER TO GB-GSBER,
              RTAB-WAERS TO GB-WAERS,
              RTAB-RAART TO RASTERUU.
        EXTRACT OP-RASTER.
        CLEAR RTAB-BUKRS.
        COLLECT RTAB.
      ENDLOOP.
    ENDLOOP.

    CLEAR LFB1.
* Bei SORTART = '2' werden nur dann Stammsatzdaten ausgegeben, wenn -*
* auch Einzelposten gerastert wurden. -------------------------------*
    CLEAR C.
    IF WFLAG2 IS INITIAL.
      MOVE-CORRESPONDING C3 TO C.
    ENDIF.

    CLEAR: GB,
           RASTERUU,
           TAGE.
    MOVE '1' TO SATZART.
* Stammdaten extrahieren ---------------------------------------------*
    CLEAR BSIK.
    EXTRACT STAMMDATEN.
* OP-Raster extrahieren ----------------------------------------------*
    SORT RTAB ASCENDING.

    LOOP AT RTAB
      WHERE BUKRS = LFB1-BUKRS.
      MOVE:     '2'    TO SATZART,
            RTAB-GSBER TO GB-GSBER,
            RTAB-WAERS TO GB-WAERS,
            RTAB-RAART TO RASTERUU.
      EXTRACT OP-RASTER.
    ENDLOOP.
  ENDIF.
  CLEAR ADRS.
*
END-OF-SELECTION.


*---------------------------------------------------------------------*
*        Aufbereitung                                                 *
*---------------------------------------------------------------------*
  CREATE OBJECT LO_WRITER.

  CLEAR   RTAB.
  REFRESH RTAB.
  IF KONZVERS = SPACE.
    SORT BY  LFB1-BUKRS
             LFB1-BUSAB
             UEBSALDO
             LFA1-LIFNR
             SATZART
             RTAB-SORTK
             GB
             RASTERUU
             RTAB-XGUTS
             TAGE
             BSIK-UMSKZ
             BSIK-BLART
             BSIK-BELNR
             BSIK-BUZEI.
  ELSE.
    SORT BY  UEBSALDO
             LFA1-LIFNR
             LFB1-BUKRS
             LFB1-BUSAB
             SATZART
             RTAB-SORTK
             GB
             RASTERUU
             RTAB-XGUTS
             TAGE
             BSIK-UMSKZ
             BSIK-BLART
             BSIK-BELNR
             BSIK-BUZEI.
  ENDIF.
*
*  LOOP.
*    AT FIRST.
*      IF KONZVERS = 'X'.
*        MOVE '0000' TO BHDGD-WERTE.
*        PERFORM NEW-SECTION(RSBTCHH0).
*      ENDIF.
*    ENDAT.
*
*    IF KONZVERS IS INITIAL.
*      AT NEW LFB1-BUKRS.
*        MOVE LFB1-BUKRS    TO BHDGD-GRPIN(4).     "<= Micro-Fiche Info
*        MOVE LFB1-BUKRS    TO BHDGD-BUKRS.
*        MOVE BHDGD-BUKRS TO BHDGD-WERTE.
*        PERFORM NEW-SECTION(RSBTCHH0).
*        CLEAR   RBUK.
*        REFRESH RBUK.
*        READ TABLE HT001 WITH KEY BUKRS = LFB1-BUKRS.
*        T001 = HT001.
*        IF WAERS EQ SPACE.
*          MOVE T001-WAERS TO WAERS.
*        ENDIF.
*
**-  Betraege in    gemaess Skalierung aufbereiten --------------------*
*        CLEAR H-TEXT.
*        IF FAKTOR(1) GT '0'.
*          MOVE '1' TO H-TEXT.
*          WHILE SY-INDEX LT 10 AND SY-INDEX LE FAKTOR(1).
*            ASSIGN H-TEXT+SY-INDEX(1) TO <F1>.
*            MOVE '0' TO <F1>.
*          ENDWHILE.
*        ENDIF.
*
*        MOVE T001-WAERS TO H-TEXT+10.
*        CONDENSE H-TEXT.
*        L_HLP_TXT = H-TEXT.                                 "1423289
*
*        DO 15 TIMES.
*          H-OFFSET = 15 - SY-INDEX.
*          ASSIGN H-TEXT+H-OFFSET(1) TO <F1>.
*          IF <F1> = SPACE.
*            MOVE  '-' TO <F1>.
*          ELSE.
*            ASSIGN <F1>+1 TO <F1>.
*            MOVE SPACE TO <F1>.
*            EXIT.
*          ENDIF.
*        ENDDO.
*
*        IF SORTART = '1'.
*          MOVE TEXT-607 TO L_TITLE_PART2.                   "1423289
*          REPLACE '$SKAL' WITH L_HLP_TXT INTO L_TITLE_PART2. "1423289
*        ELSE.
*          IF RASTVERD < '2'.
*
*          ELSE.
*            MOVE TEXT-607 TO L_TITLE_PART2.                 "1423289
*            REPLACE '$SKAL' WITH L_HLP_TXT INTO L_TITLE_PART2. "1423289
*          ENDIF.
*        ENDIF.
*        WRITE KD_STIDA TO H-STICHTAG DD/MM/YY.
*        REPLACE '$STIDA' WITH H-STICHTAG INTO L_TITLE_PART2. "1423289
*      ENDAT.
*
*      AT NEW LFB1-BUSAB.
*        MOVE LFB1-BUSAB  TO BHDGD-GRPIN+4(2).   "<= Micro-Fiche Info
*        CLEAR   RBUS.
*        REFRESH RBUS.
*        SELECT SINGLE * FROM T001S WHERE BUKRS EQ LFB1-BUKRS
*                                     AND BUSAB EQ LFB1-BUSAB.
*        MOVE TEXT-056 TO VARUEB5.
*        REPLACE '$BUK' WITH LFB1-BUKRS    INTO VARUEB5.
*        REPLACE '$SAB' WITH LFB1-BUSAB    INTO VARUEB5.
*        REPLACE '$SBZ' WITH T001S-SNAME   INTO VARUEB5.
*        FLAG2 = 'X'.
*      ENDAT.
*
*      AT NEW LFA1-LIFNR.
*        MOVE LFA1-LIFNR  TO BHDGD-GRPIN+6(10).  "<= Micro-Fiche Info
*        CLEAR GBZAEHL.
*        SAVE_GSBER = '§§§§'.                                "#EC *
*        SAVE_WAERS = '§§§§§'.                               "#EC *
**-- Nur bei Verdichtungsstufe < 2 erfolgt Seitenvorschub pro Konto ---*
*        IF VERDICHT < '2'.
*          NEW-PAGE.
**---- Es bleibt Platz fuer ein Raster --------------------------------*
*          RESERVE 5 LINES.
*        ENDIF.
*        TOP-FLAG = '0'.
*
**-- Bei Verdichtungsstufe '2' und Ausgabe von OP-Rastern muss Platz --*
**-- fuer Stamminfo inclusive Ueberschrift bleiben, weil kein Seiten- -*
**-- vorschub bei neuem Konto erfolgt. --------------------------------*
*        IF VERDICHT = '2' AND RASTVERD < '2'.
*          RESERVE 10 LINES.
*        ENDIF.
*
**-- Bei Verdichtungsstufe '2'  o h n e  Ausgabe von OP-Rastern muss --*
**-- Platz fuer Stamminfo ohne Ueberschrift bleiben, weil kein Seiten- *
**-- vorschub bei neuem Konto erfolgt. --------------------------------*
**-- Die Ueberschrift wird einmal bei TOP-OF-PAGE ausgegeben. ---------*
**-- TOP-FLAG = '4' ---------------------------------------------------*
*        IF VERDICHT = '2' AND RASTVERD = '2'.
*          RESERVE  7 LINES.
*        ENDIF.
*      ENDAT.
*    ELSE.
*      AT NEW LFA1-LIFNR.
*        FLAG2 = 'X'.
*        MOVE LFA1-LIFNR  TO BHDGD-GRPIN(10).  "<= Micro-Fiche Info
*        WRITE KD_STIDA TO H-STICHTAG DD/MM/YY.
*      ENDAT.
*
*      AT NEW LFB1-BUKRS.
*        CLEAR   RBUK.
*        REFRESH RBUK.
*        CLEAR GBZAEHL.
*        MOVE LFB1-BUKRS    TO BHDGD-GRPIN+10(4).  "<= Micro-Fiche Info
*
*        IF NOT LFB1-BUKRS IS INITIAL.
*          READ TABLE HT001 WITH KEY BUKRS = LFB1-BUKRS.
*          T001 = HT001.
*          IF WAERS EQ SPACE.
*            MOVE T001-WAERS TO WAERS.
*          ENDIF.
*        ENDIF.
*      ENDAT.
*
*      AT NEW LFB1-BUSAB.
*        CLEAR   RBUS.
*        REFRESH RBUS.
*        IF  NOT LFB1-BUKRS IS INITIAL
*        AND NOT LFB1-BUSAB IS INITIAL.
*          MOVE LFB1-BUSAB  TO BHDGD-GRPIN+14(2).   "<= Micro-Fiche Info
*          SELECT SINGLE * FROM T001S WHERE BUKRS EQ LFB1-BUKRS
*                                       AND BUSAB EQ LFB1-BUSAB.
*          MOVE TEXT-056 TO VARUEB5.
*          REPLACE '$BUK' WITH LFB1-BUKRS    INTO VARUEB5.
*          REPLACE '$SAB' WITH LFB1-BUSAB    INTO VARUEB5.
*          REPLACE '$SBZ' WITH T001S-SNAME   INTO VARUEB5.
*        ENDIF.
*      ENDAT.
*    ENDIF.
*
*    AT NEW SATZART.
*      CASE SATZART.
*        WHEN '2'.                      "Raster
*          IF ACC_MODE IS INITIAL.
*            IF RTAB-SORTK < '2'.
*              IF RASTVERD < '2'.
*                IF VERDICHT < '3'.
*                  IF  NOT KONZVERS IS INITIAL
*                  AND NOT LFB1-BUKRS IS INITIAL.
*                    CHECK XBUKRDAT NE '2'.
*                  ENDIF.
*
**-------- Wenn ein neues Raster beginnt, muessen mindestens noch -----*
**-------- 9 Zeilen Platz haben. --------------------------------------*
*                  TOP-FLAG = '3'.
*                  RESERVE 9 LINES.
*                  L_TITLE_1 = TEXT-513.
*                  REPLACE '&1' WITH LFB1-BUKRS INTO L_TITLE_1.
*                  REPLACE '&2' WITH LFB1-BUSAB INTO L_TITLE_1.
*                  REPLACE '&3' WITH LFA1-LIFNR INTO L_TITLE_1.
*                  REPLACE '&4' WITH H-STICHTAG INTO L_TITLE_1.
*                  LO_WRITER->NEW_TABLE( ID_TYPE = '2' ID_TITLE = L_TITLE_1 ).
*                  LO_WRITER->TITLE_WRITE( ).
*                  VARUEB1-FELD2 = TEXT-500.
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-520 ).
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-522 ).
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-524 ).
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-525 ).
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-526 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD2 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD3 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD4 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD5 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD6 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD7 ).
*                  LO_WRITER->ADD_HEADER_FIELD( '' ).
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-523 ).
*                  LO_WRITER->ADD_HEADER_FIELD( '' ).
*                  LO_WRITER->ADD_HEADER_FIELD( '' ).
*                  LO_WRITER->ADD_HEADER_FIELD( '' ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD2 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD3 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD4 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD5 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD6 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD7 ).
*                  LO_WRITER->RULER_WRITE( ).
*                ENDIF.
*              ENDIF.
*            ENDIF.
*            IF RTAB-SORTK < '2'.
*              IF RASTVERD < '2'.
*                IF VERDICHT = '6'
*                AND FLTOP   = SPACE.
*                  COUNT = COUNT + 1.
*                  IF COUNT GT '1'.
*                    NEW-PAGE.
*                  ENDIF.
**-------- Wenn ein neues Raster beginnt, muessen mindestens noch -----*
**-------- 9 Zeilen Platz haben. --------------------------------------*
*                  IF VERDICHT = '6'.
*                    IF RASTVERD = '0'.                      "1557468
*                      MOVE TEXT-531 TO VARUEB1-FELD1.       "1557468
*                    ELSE.                                   "1557468
*                      MOVE TEXT-702 TO VARUEB1-FELD1.       "1557468
*                    ENDIF.                                  "1557468
*                  ENDIF.
*                  TOP-FLAG = '3'.
*                  RESERVE 9 LINES.
*                  IF KONZVERS IS INITIAL.
*                    CONCATENATE VARUEB5 ',' INTO L_HLP_TXT.
*                  ELSE.
*                    CONCATENATE TEXT-220 ',' INTO L_HLP_TXT.
*                  ENDIF.
*                  CONCATENATE L_HLP_TXT L_TITLE_PART2 INTO L_TITLE_1
*                    SEPARATED BY SPACE.
*
*                  LO_WRITER->NEW_TABLE( ID_TYPE = '4' ID_TITLE = L_TITLE_1 ).
*                  LO_WRITER->TITLE_WRITE( ).
*                  VARUEB1-FELD2 = TEXT-500.
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-700 ).
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-703 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD1 ). "1557468
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-525 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD2 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD3 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD4 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD5 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD6 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD7 ).
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-701 ).
*                  LO_WRITER->ADD_HEADER_FIELD( TEXT-704 ).
*                  LO_WRITER->ADD_HEADER_FIELD( '' ).
*                  LO_WRITER->ADD_HEADER_FIELD( '' ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD2 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD3 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD4 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD5 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD6 ).
*                  LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD7 ).
*                  LO_WRITER->RULER_WRITE( ).
*                  FLTOP = 'X'.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        WHEN '3'.                      "Einzelposten
*          TOP-FLAG = '1'.                                   "1506729
*
*          CONCATENATE TEXT-547 TEXT-110 LFB1-BUKRS TEXT-111 LFB1-BUSAB TEXT-112 LFA1-LIFNR
*             TEXT-548 H-STICHTAG INTO L_TITLE_1 SEPARATED BY SPACE.
*          LO_WRITER->NEW_TABLE( ID_TYPE = '3' ID_TITLE = L_TITLE_1 ).
*          LO_WRITER->TITLE_WRITE( ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-530 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-531 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-532 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-533 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-534 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-535 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-536 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-537 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-538 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-539 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-540 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-541 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-542 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-543 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-544 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-545 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-546 ).
*          LO_WRITER->RULER_WRITE( ).
*
*      ENDCASE.
*    ENDAT.
*
*    AT STAMMDATEN.                     "Satzart '1'
*      IF VERDICHT < '3'.
*        DETAIL.
*
*        IF  NOT KONZVERS IS INITIAL
*        AND NOT LFB1-BUKRS IS INITIAL.
*          IF  XBUKRDAT = '2'
*          AND VERDICHT > '0'.
*            CHECK 1 = 2.
*          ENDIF.
*        ENDIF.
*
*        IF KONZVERS IS INITIAL.
*          CONCATENATE
*            TEXT-110 LFB1-BUKRS
*            TEXT-111 LFB1-BUSAB
*            TEXT-112 LFA1-LIFNR
*            INTO L_HLP_TXT SEPARATED BY SPACE.
*          CONCATENATE L_HLP_TXT L_TITLE_PART2 INTO L_TITLE_1
*            SEPARATED BY SPACE.
*        ELSE.
*          IF LFB1-BUKRS IS INITIAL.
*            CONCATENATE
*              TEXT-112 LFA1-LIFNR
*              INTO L_HLP_TXT SEPARATED BY SPACE.
*            CONCATENATE L_HLP_TXT L_TITLE_PART2 INTO L_TITLE_1
*              SEPARATED BY SPACE.
*          ELSE.
*            CONCATENATE
*              TEXT-112 LFA1-LIFNR
*              TEXT-110 LFB1-BUKRS
*              TEXT-111 LFB1-BUSAB
*              INTO L_HLP_TXT SEPARATED BY SPACE.
*            CONCATENATE L_HLP_TXT L_TITLE_PART2 INTO L_TITLE_1
*              SEPARATED BY SPACE.
*          ENDIF.
*        ENDIF.
*
*        CALL METHOD LO_WRITER->NEW_TABLE
*          EXPORTING
*            ID_TITLE = L_TITLE_1
*            ID_TYPE  = '1'.
*
*        TOP-FLAG = '4'.                                     "1506729
*        LO_WRITER->ADD_GROUP_HEADER_FIELD( TEXT-550 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-551 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-552 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-553 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-554 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-555 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-556 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-557 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-558 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-559 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-560 ).
*        CLEAR TOP-FLAG.
*        RESERVE 10 LINES.
*        LO_WRITER->TITLE_WRITE( ).
*        LO_WRITER->RULER_WRITE( ).
*
*        PERFORM ANSCHRIFT.
*      ENDIF.
*    ENDAT.
*
*    AT OP-RASTER.                      "Satzart '2'
*      IF VERDICHT < '3'
*      OR VERDICHT = '6'.
*        PERFORM RASTER_AUSGABE.
*      ENDIF.
**-- Summen fuer hoehere Gruppenstufen bilden --------------------------*
*      IF ACC_MODE IS INITIAL.
*        PERFORM SUM_BUSAB_BUKRS_TOTAL.
*      ENDIF.
*    ENDAT.
*
*    AT EINZELPOSTEN.                   "Satzart '3'
*      RESERVE 2 LINES.
*      PERFORM EINZELPOSTEN_AUSGABE.
*    ENDAT.
*
*    AT END OF RASTERUU.
*      IF SATZART = '3'.
*        LO_WRITER->ROW_OPEN( ).
*        LO_WRITER->FORMAT_TOTAL( ).
*        CASE RASTERUU.
*          WHEN '1'.
*            MOVE TEXT-052 TO VARTXT1.
*            REPLACE '$BIS' WITH RC01 INTO VARTXT1.
*            WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                           CURRENCY T001-WAERS
*                           ROUND FAKTOR
*                           DECIMALS STELLEN.
*            CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*            LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*            LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*            LO_WRITER->ULINE( ).
*          WHEN '2'.
*            IF NOT RC02 IS INITIAL.
*              MOVE TEXT-053 TO VARTXT1.
*              REPLACE '$VON' WITH RC06 INTO VARTXT1.
*              REPLACE '$BIS' WITH RC02 INTO VARTXT1.
*              WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                             CURRENCY T001-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*              CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*              LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*              LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*              LO_WRITER->ULINE( ).
*            ELSE.
*              MOVE TEXT-054 TO VARTXT1.
*              REPLACE '$VON' WITH RC06 INTO VARTXT1.
*              WRITE: 132 SY-VLINE.
*              WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                             CURRENCY T001-WAERS.
*              CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*              LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*              LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*              LO_WRITER->ULINE( ).
*            ENDIF.
*          WHEN '3'.
*            IF NOT RC03 IS INITIAL.
*              MOVE TEXT-053 TO VARTXT1.
*              REPLACE '$VON' WITH RC07 INTO VARTXT1.
*              REPLACE '$BIS' WITH RC03 INTO VARTXT1.
*              WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                             CURRENCY T001-WAERS.
*              CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*              LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*              LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*              LO_WRITER->ULINE( ).
*            ELSE.
*              MOVE TEXT-054 TO VARTXT1.
*              REPLACE '$VON' WITH RC07 INTO VARTXT1.
*
*              WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                             CURRENCY T001-WAERS.
*              CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*              LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*              LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*              LO_WRITER->ULINE( ).
*
*            ENDIF.
*          WHEN '4'.
*            IF NOT RC04 IS INITIAL.
*              MOVE TEXT-053 TO VARTXT1.
*              REPLACE '$VON' WITH RC08 INTO VARTXT1.
*              REPLACE '$BIS' WITH RC04 INTO VARTXT1.
*              WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                             CURRENCY T001-WAERS.
*              CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*              LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*              LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*              LO_WRITER->ULINE( ).
*            ELSE.
*              MOVE TEXT-054 TO VARTXT1.
*              REPLACE '$VON' WITH RC08 INTO VARTXT1.
*              WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                             CURRENCY T001-WAERS.
*              CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*              LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*              LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*              LO_WRITER->ULINE( ).
*            ENDIF.
*          WHEN '5'.
*            IF NOT RC05 IS INITIAL.
*              MOVE TEXT-053 TO VARTXT1.
*              REPLACE '$VON' WITH RC09 INTO VARTXT1.
*              REPLACE '$BIS' WITH RC05 INTO VARTXT1.
*              WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                             CURRENCY T001-WAERS.
*              CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*              LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*              LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*              LO_WRITER->ULINE( ).
*            ELSE.
*              MOVE TEXT-054 TO VARTXT1.
*              REPLACE '$VON' WITH RC09 INTO VARTXT1.
*              WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                             CURRENCY T001-WAERS.
*              CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*              LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*              LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*              LO_WRITER->ULINE( ).
*            ENDIF.
*          WHEN '6'.
*            MOVE TEXT-054 TO VARTXT1.
*            REPLACE '$VON' WITH RC10 INTO VARTXT1.
*            WRITE SUM(SHBETRAG) TO L_HLP_TXT1(12)
*                           CURRENCY T001-WAERS.
*            CONCATENATE VARTXT1 L_HLP_TXT1 INTO L_HLP_TXT SEPARATED BY SPACE.
*            LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*            LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*            LO_WRITER->ULINE( ).
*          WHEN OTHERS.
*            WRITE SUM(SHBETRAG) TO L_HLP_TXT(12)
*                           CURRENCY T001-WAERS.
*            LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*            LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*            LO_WRITER->ULINE( ).
*        ENDCASE.
*      ENDIF.
*    ENDAT.
*
*    AT END OF SATZART.
*      IF SATZART = '2'.
*        IF RASTVERD < '2'.
*          IF VERDICHT < '3'.
*            IF  NOT KONZVERS IS INITIAL
*            AND NOT LFB1-BUKRS IS INITIAL
*            AND XBUKRDAT = '2'
*            AND VERDICHT > '0'.
*            ELSE.
*              IF ( LO_WRITER->MD_IS_TABLE_OPEN = ABAP_TRUE ) .
*                LO_WRITER->TABLE_CLOSE( ).
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDAT.
*
*    IF ACC_MODE IS INITIAL.
*      IF KONZVERS IS INITIAL.
*        AT END OF LFB1-BUSAB.
*          CLEAR FLAG2.
*          MOVE SPACE     TO BHDGD-GRPIN+6. "<= Micro-Fiche Info
*          IF VERDICHT < '4'
*          OR VERDICHT = '6'.
*            DESCRIBE TABLE RBUS LINES COUN1.
*            IF VERDICHT = '6'.
*              IF COUN1 GT 0.
*                IF ( LO_WRITER->MD_IS_TABLE_OPEN = ABAP_TRUE ) .
*                  LO_WRITER->TABLE_CLOSE( ).
*                ENDIF.
*              ENDIF.
*            ENDIF.
*            NEW-PAGE.
*            MOVE TEXT-051 TO VARUEB3.
*            REPLACE '$BUK' WITH LFB1-BUKRS    INTO VARUEB3.
*            REPLACE '$SAB' WITH LFB1-BUSAB    INTO VARUEB3.
*            REPLACE '$SBZ' WITH T001S-SNAME   INTO VARUEB3.
*            TOP-FLAG = '2'.
*            PERFORM RASTER_AUSGABE_BUSAB.
*          ENDIF.
*          CLEAR T001S.
*          CLEAR FLTOP.
*        ENDAT.
*
*        AT END OF LFB1-BUKRS.
*          MOVE SPACE     TO BHDGD-GRPIN+4. "<= Micro-Fiche Info
*          IF VERDICHT < '5'
*          OR VERDICHT = '6'.
*            NEW-PAGE.
*            MOVE TEXT-050 TO VARUEB3.
*            REPLACE '$BUK' WITH LFB1-BUKRS    INTO VARUEB3.
*            TOP-FLAG = '2'.
*            PERFORM RASTER_AUSGABE_BUKRB.
*          ENDIF.
*          CLEAR FLTOP.
*        ENDAT.
*      ELSE.
*        AT END OF LFB1-BUSAB.
*          CLEAR T001S.
*        ENDAT.
*
*        AT END OF LFB1-BUKRS.
*          MOVE SPACE     TO BHDGD-GRPIN+14. "<= Micro-Fiche Info
*        ENDAT.
*
*        AT END OF LFA1-LIFNR.
*          MOVE SPACE       TO BHDGD-GRPIN+10.      "<= Micro-Fiche Info
*        ENDAT.
*      ENDIF.
*    ENDIF.
*
*    AT LAST.
*      CLEAR FLAG2.
*      MOVE SPACE       TO BHDGD-GRPIN. "<= Micro-Fiche Info
*      MOVE '    '      TO BHDGD-BUKRS.
*      MOVE BHDGD-BUKRS TO BHDGD-WERTE.
*      IF ( LO_WRITER->MD_IS_TABLE_OPEN = ABAP_TRUE ) .
*        LO_WRITER->TABLE_CLOSE( ).
*      ENDIF.
*      PERFORM NEW-SECTION(RSBTCHH0).
*      MOVE TEXT-055 TO VARUEB3.
*      TOP-FLAG = '2'.
*      IF ACC_MODE IS INITIAL.
*        PERFORM RASTER_AUSGABE_TOTAL.
*      ELSE.
*        PERFORM RASTER_AUSGABE_ALV_GRID.
*      ENDIF.
*    ENDAT.
*  ENDLOOP.
*
*  IF SY-PAGNO = 0.                     "Keine Liste ausgegeben
*    CALL FUNCTION 'POPUP_NO_LIST'.
*  ENDIF.
*
*TOP-OF-PAGE.
*  IF ACC_MODE IS INITIAL.
**- Standard-Seitenkopf drucken --------------------------------------*
*    PERFORM BATCH-HEADING(RSBTCHH0).
*
**-- ab der zweiten Seite pro Konto Ueberschrift fuer Einzelposten ---*
*    DETAIL.
*    CASE TOP-FLAG.
*
*      WHEN '1'.
*
** create title
*        WRITE KD_STIDA TO H-STICHTAG DD/MM/YY.
*        CONCATENATE TEXT-547 TEXT-110 LFB1-BUKRS TEXT-111 LFB1-BUSAB TEXT-112 LFA1-LIFNR
*           TEXT-548 H-STICHTAG INTO L_TITLE_1 SEPARATED BY SPACE.
*
** write table header for line items
*        LO_WRITER->NEW_TABLE( ID_TYPE = '3' ID_TITLE = L_TITLE_1 ).
*        LO_WRITER->TITLE_WRITE( ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-530 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-531 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-532 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-533 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-534 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-535 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-536 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-537 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-538 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-539 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-540 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-541 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-542 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-543 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-544 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-545 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-546 ).
*        LO_WRITER->RULER_WRITE( ).
*
**     DETAIL.
**-- Ueberschriften fuer Listenteil 2 ausgeben -----------------------*
*      WHEN '2'.
*        IF VERDICHT = '6'.
*          IF FLAG2 IS INITIAL.
*            MOVE TEXT-102 TO VARUEB1-FELD1.
*            MOVE TEXT-106 TO VARUEB2-FELD1.
*          ENDIF.
*        ENDIF.
*
** create title
*
*        IF TOP-FLA2 IS INITIAL.
*          CONCATENATE VARUEB3 ',' INTO L_HLP_STRING.
*          CONCATENATE L_HLP_STRING L_TITLE_PART2 INTO L_TITLE_1 SEPARATED BY SPACE.
*        ELSE.
*          IF KONZVERS IS INITIAL.
*            CONCATENATE VARUEB5 ',' INTO L_HLP_STRING.
*            CONCATENATE L_HLP_STRING L_TITLE_PART2 INTO L_TITLE_1 SEPARATED BY SPACE.
*          ELSE.
*            CONCATENATE TEXT-220 ',' INTO L_HLP_STRING.
*            CONCATENATE L_HLP_STRING L_TITLE_PART2 INTO L_TITLE_1 SEPARATED BY SPACE.
*          ENDIF.
*        ENDIF.
*
*        CLEAR BATCH_OP_HEADER.
*        IF ( SY-BATCH = 'X' AND VERDICHT = '6' )            "1021583
*        OR ( G_EX_PRINT_SEL = 'X' AND VERDICHT = '6' ).     "1021583
*          IF NOT TOP-FLA2 IS INITIAL.
*            BATCH_OP_HEADER = 'X'.
*          ENDIF.
*        ENDIF.
*
*        IF BATCH_OP_HEADER = 'X'.
** print title and header for BATCH OP LINES
*
*          LO_WRITER->NEW_TABLE( ID_TYPE = '4' ID_TITLE = L_TITLE_1 ).
*          LO_WRITER->TITLE_WRITE( ).
*          VARUEB1-FELD2 = TEXT-500.
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-700 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-703 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-702 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-525 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD2 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD3 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD4 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD5 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD6 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD7 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-701 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-704 ).
*          LO_WRITER->ADD_HEADER_FIELD( '' ).
*          LO_WRITER->ADD_HEADER_FIELD( '' ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD2 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD3 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD4 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD5 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD6 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD7 ).
*          LO_WRITER->RULER_WRITE( ).
*        ELSE.
** print title and header
*
*          LO_WRITER->NEW_TABLE( ID_TYPE = '2' ID_TITLE = L_TITLE_1 ).
*          LO_WRITER->TITLE_WRITE( ).
*          VARUEB1-FELD2 = TEXT-500.
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-520 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-522 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-524 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-525 ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-526 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD2 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD3 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD4 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD5 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD6 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB1-FELD7 ).
*          LO_WRITER->ADD_HEADER_FIELD( '' ).
*          LO_WRITER->ADD_HEADER_FIELD( TEXT-523 ).
*          LO_WRITER->ADD_HEADER_FIELD( '' ).
*          LO_WRITER->ADD_HEADER_FIELD( '' ).
*          LO_WRITER->ADD_HEADER_FIELD( '' ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD2 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD3 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD4 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD5 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD6 ).
*          LO_WRITER->ADD_HEADER_FIELD( VARUEB2-FELD7 ).
*          LO_WRITER->RULER_WRITE( ).
*        ENDIF.
*
*      WHEN '3'.
**-- Ueberschrift fuer Stammsatzinformationen ------------------------*
*      WHEN '4'.
** create header
*        CONCATENATE TEXT-549 TEXT-110 LFB1-BUKRS TEXT-111 LFB1-BUSAB TEXT-112 LFA1-LIFNR
*           L_TITLE_PART2 INTO L_TITLE_1 SEPARATED BY SPACE.
*
** write table header for line items
*        LO_WRITER->NEW_TABLE( ID_TYPE = '1' ID_TITLE = L_TITLE_1 ).
*        LO_WRITER->TITLE_WRITE( ).
*        LO_WRITER->ADD_GROUP_HEADER_FIELD( TEXT-550 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-551 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-552 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-553 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-554 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-555 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-556 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-557 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-558 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-559 ).
*        LO_WRITER->ADD_HEADER_FIELD( TEXT-560 ).
*        LO_WRITER->ULINE( ).
*        LO_WRITER->RULER_WRITE( ).
*
*    ENDCASE.
*    CLEAR BATCH_OP_HEADER.
*  ENDIF.

*---------------------------------------------------------------------*
*       FORM CFAKTOR                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CFAKTOR.
  IF T001-WAERS NE TCURX-CURRKEY.
    SELECT SINGLE * FROM TCURX WHERE CURRKEY = T001-WAERS.
    IF SY-SUBRC NE 0.
      TCURX-CURRKEY = T001-WAERS.
      CFAKT = 100.
    ELSE.
      CFAKT = 1.
      DO TCURX-CURRDEC TIMES.
        CFAKT = CFAKT * 10.
      ENDDO.
    ENDIF.
  ENDIF.
ENDFORM.                    "CFAKTOR

*---------------------------------------------------------------------*
*       FORM RASTER_AUFBAU                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM RASTER_AUFBAU.
* Erste ausgewaehlte Rasterarte sichern ------------------------------*
  IF RART-NET = 'X'.
    RART = '1'.
  ELSE.
    IF RART-SKT = 'X'.
      RART = '2'.
    ELSE.
      IF RART-ZHL = 'X'.
        RART = '3'.
      ELSE.
        IF RART-UEB = 'X'.
          RART = '4'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* Obergrenze Intervall -----------------------------------------------*
  RP01 = RASTBIS1.
  RP02 = RASTBIS2.
  RP03 = RASTBIS3.
  RP04 = RASTBIS4.
  RP05 = RASTBIS5.

* Untergrenze Intervall -----------------------------------------------*

  RP06 = RP01 + 1.
  IF NOT RP02 IS INITIAL.
    RP07 = RP02 + 1.
  ENDIF.
  IF NOT RP03 IS INITIAL.
    RP08 = RP03 + 1.
  ENDIF.
  IF NOT RP04 IS INITIAL.
    RP09 = RP04 + 1.
  ENDIF.
  IF NOT RP05 IS INITIAL.
    RP10 = RP05 + 1.
  ENDIF.

* Rasterpunkte in Charakterform für REPLACE.
  WRITE: RP01 TO RC01.
  IF NOT RP02 IS INITIAL.
    WRITE: RP02 TO RC02.
    MOVE TEXT-202 TO VARUEB2-FELD3.
  ENDIF.
  IF NOT RP03 IS INITIAL.
    WRITE: RP03 TO RC03.
    MOVE TEXT-203 TO VARUEB2-FELD4.
  ENDIF.
  IF NOT RP04 IS INITIAL.
    WRITE: RP04 TO RC04.
    MOVE TEXT-204 TO VARUEB2-FELD5.
  ENDIF.
  IF NOT RP05 IS INITIAL.
    WRITE: RP05 TO RC05.
    MOVE TEXT-205 TO VARUEB2-FELD6.
  ENDIF.
  IF NOT RP06 IS INITIAL.
    WRITE: RP06 TO RC06.
    MOVE TEXT-206 TO VARUEB1-FELD3.
  ENDIF.
  IF NOT RP07 IS INITIAL.
    WRITE: RP07 TO RC07.
    MOVE TEXT-207 TO VARUEB1-FELD4.
  ENDIF.
  IF NOT RP08 IS INITIAL.
    WRITE: RP08 TO RC08.
    MOVE TEXT-208 TO VARUEB1-FELD5.
  ENDIF.
  IF NOT RP09 IS INITIAL.
    WRITE: RP09 TO RC09.
    MOVE TEXT-209 TO VARUEB1-FELD6.
  ENDIF.
  IF NOT RP10 IS INITIAL.
    WRITE: RP10 TO RC10.
    MOVE TEXT-210 TO VARUEB1-FELD7.
  ENDIF.

* Variable ersetzen --------------------------------------------------*
  IF VERDICHT = '6'.
    IF RASTVERD = '0'.                                      "1557468
      MOVE TEXT-171 TO VARUEB1-FELD1.                       "1557468
      MOVE TEXT-169 TO VARUEB2-FELD1.                       "1557468
    ELSE.                                                   "1557468
      MOVE TEXT-168 TO VARUEB1-FELD1.                       "1557468
      MOVE TEXT-169 TO VARUEB2-FELD1.                       "1557468
    ENDIF.                                                  "1557468
  ELSE.
    MOVE TEXT-102 TO VARUEB1-FELD1.
    MOVE TEXT-106 TO VARUEB2-FELD1.
  ENDIF.
  MOVE TEXT-201 TO VARUEB2-FELD2.

  REPLACE 'RP01' WITH RC01 INTO VARUEB2.                    "bis   0
  REPLACE 'RP02' WITH RC02 INTO VARUEB2.                    "bis  20
  REPLACE 'RP03' WITH RC03 INTO VARUEB2.                    "bis  40
  REPLACE 'RP04' WITH RC04 INTO VARUEB2.                    "bis  80
  REPLACE 'RP05' WITH RC05 INTO VARUEB2.                    "bis 100
  REPLACE 'RP06' WITH RC06 INTO VARUEB1.                    "von   1
  REPLACE 'RP07' WITH RC07 INTO VARUEB1.                    "von  21
  REPLACE 'RP08' WITH RC08 INTO VARUEB1.                    "von  41
  REPLACE 'RP09' WITH RC09 INTO VARUEB1.                    "von  81
  REPLACE 'RP10' WITH RC10 INTO VARUEB1.                    "von 101
ENDFORM.                    "RASTER_AUFBAU

*---------------------------------------------------------------------*
*       FORM SALDO_AKTUELL                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SALDO_AKTUELL.
  ADD LFC1-UM01S THEN LFC1-UM02S UNTIL LFC1-UM16S GIVING H-SOLL
      ACCORDING TO BMONAT.
  ADD LFC1-UM01H THEN LFC1-UM02H UNTIL LFC1-UM16H GIVING H-HABEN
      ACCORDING TO BMONAT.
  H-SALDO  = H-SOLL - H-HABEN + LFC1-UMSAV.
* aktueller Saldo = Teil des Gesamtobligos --------------------------*
  C-AGOBLI = H-SALDO.
  IF CFAKT NE 0.
    CHECKSALDO = CHECKSALDO + H-SALDO / CFAKT.
    CHECKAGOBL = CHECKAGOBL + C-AGOBLI / CFAKT.
  ELSE.
    CHECKSALDO = CHECKSALDO + H-SALDO.
    CHECKAGOBL = CHECKAGOBL + C-AGOBLI.
  ENDIF.
  C-SALDO  = H-SALDO."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
ENDFORM.                    "SALDO_AKTUELL

*---------------------------------------------------------------------*
*       FORM KUM_WERTE                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM KUM_WERTE.
* Jahresumsatz -------------------------------------------------------*
  ADD LFC1-UM01U THEN LFC1-UM02U UNTIL LFC1-UM16U GIVING C-KUMUM"#EC CI_FLDEXT_OK[2610650]
   "Added by SPLABAP during code remediation
      ACCORDING TO BMONAT.
  IF SORTART = '1' . "Ausgabe kum Kum.Umsatz wenn Hauswährung gewünscht.
* Kum. Umsatz---------------------------------------------------------*
    CLEAR RTAB.
    MOVE: LFC1-BUKRS TO RTAB-BUKRS.
* Satz für Ausgabe des kummulieten Umsatzes auf Summenebene.
* (Summe pro Sachbearbeiter und Buchungskreis)
    MOVE: '2' TO RTAB-SORTK,
    '** '   TO RTAB-GSBER,
    C-KUMUM TO RTAB-KUMUM.
    MOVE:
    LFA1-LIFNR TO RTAB-LIFNR,
    LFA1-SORTL TO RTAB-SORTL,
    LFA1-LAND1 TO RTAB-LAND1.
    COLLECT RTAB.
  ENDIF.
ENDFORM.                    "KUM_WERTE

*---------------------------------------------------------------------*
*       FORM SONDER_UMSAETZE                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SONDER_UMSAETZE.
* Errechnen Sonderumsatz-Salden, Gesamtsaldo ------------------------*
*---------- Trend, Umsatz pro Gesch.Bereich -------------------------*
  H-SHBLS = LFC3-SOLLL - LFC3-HABNL.
*-- Gesamt-Obligo ----------------------------------------------------*
  C-AGOBLI = LFC3-SALDV + H-SHBLS.
  PERFORM CFAKTOR.
  IF CFAKT NE 0.
    CHECKAGOBL = CHECKAGOBL + C-AGOBLI / CFAKT.
  ELSE.
    CHECKAGOBL = CHECKAGOBL + C-AGOBLI.
  ENDIF.
*-- Sonderumsatz-Salden ----------------------------------------------*
  CASE LFC3-SHBKZ.
    WHEN HUMKZ1.
      C-UMKZ1 = LFC3-SHBKZ.
      C-SUMS1 = C-SUMS1 + LFC3-SALDV + H-SHBLS.
    WHEN HUMKZ2.
      C-UMKZ2 = LFC3-SHBKZ.
      C-SUMS2 = C-SUMS2 + LFC3-SALDV + H-SHBLS.
    WHEN HUMKZ3.
      C-UMKZ3 = LFC3-SHBKZ.
      C-SUMS3 = C-SUMS3 + LFC3-SALDV + H-SHBLS.
    WHEN HUMKZ4.
      C-UMKZ4 = LFC3-SHBKZ.
      C-SUMS4 = C-SUMS4 + LFC3-SALDV + H-SHBLS.
    WHEN HUMKZ5.
      C-UMKZ5 = LFC3-SHBKZ.
      C-SUMS5 = C-SUMS5 + LFC3-SALDV + H-SHBLS.
    WHEN HUMKZ6.
      C-UMKZ6 = LFC3-SHBKZ.
      C-SUMS6 = C-SUMS6 + LFC3-SALDV + H-SHBLS.
    WHEN HUMKZ7.
      C-UMKZ7 = LFC3-SHBKZ.
      C-SUMS7 = C-SUMS7 + LFC3-SALDV + H-SHBLS.
    WHEN HUMKZ8.
      C-UMKZ8 = LFC3-SHBKZ.
      C-SUMS8 = C-SUMS8 + LFC3-SALDV + H-SHBLS.
    WHEN HUMKZ9.
      C-UMKZ9 = LFC3-SHBKZ.
      C-SUMS9 = C-SUMS9 + LFC3-SALDV + H-SHBLS.
    WHEN HUMKZ10.
      C-UMKZ10 = LFC3-SHBKZ.
      C-SUMS10 = C-SUMS10 + LFC3-SALDV + H-SHBLS.
    WHEN OTHERS.
      C-SONOB = C-SONOB + LFC3-SALDV + H-SHBLS.
  ENDCASE.
ENDFORM.                    "SONDER_UMSAETZE

*---------------------------------------------------------------------*
*       FORM POSTEN_RASTERN                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM POSTEN_RASTERN USING  POSTEN_WAERS.
  IF RART-NET = 'X'.
    IF SORTART = '1'.
      PERFORM R USING NTAGE '1' ZBSEGA-DMSHB     POSTEN_WAERS.
    ELSE.
      PERFORM R USING NTAGE '1' ZBSEGA-WRSHB     POSTEN_WAERS.
    ENDIF.
  ENDIF.

  IF RART-SKT = 'X'.
    IF SORTART = '1'.
      PERFORM R USING STAGE '2' ZBSEGA-DMSHB     POSTEN_WAERS.
    ELSE.
      PERFORM R USING STAGE '2' ZBSEGA-WRSHB     POSTEN_WAERS.
    ENDIF.
  ENDIF.
  IF RART-ZHL = 'X'.
    IF SORTART = '1'.
      PERFORM R USING ZTAGE '3' ZBSEGA-DMSHB     POSTEN_WAERS.
    ELSE.
      PERFORM R USING ZTAGE '3' ZBSEGA-WRSHB     POSTEN_WAERS.
    ENDIF.
  ENDIF.
  IF RART-UEB = 'X'.
    IF SORTART = '1'.
      PERFORM R USING UTAGE '4' ZBSEGA-DMSHB     POSTEN_WAERS.
    ELSE.
      PERFORM R USING UTAGE '4' ZBSEGA-WRSHB     POSTEN_WAERS.
    ENDIF.
  ENDIF.
ENDFORM.                    "POSTEN_RASTERN

*---------------------------------------------------------------------*
*       FORM R                                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM R USING R_TAGE R_ART R_BETRAG R_WAERS.
  CLEAR RTAB.
  MOVE: BSIK-BUKRS TO RTAB-BUKRS,
        '0'        TO RTAB-SORTK,
        BSIK-GSBER TO RTAB-GSBER,
        R_WAERS    TO RTAB-WAERS,
        R_ART      TO RTAB-RAART,
        R_BETRAG   TO RTAB-OPSUM,"#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
        LFA1-LIFNR TO RTAB-LIFNR,
        LFA1-SORTL TO RTAB-SORTL,
        LFA1-LAND1 TO RTAB-LAND1.

  IF R_TAGE <= RP01.
    MOVE: R_BETRAG TO RTAB-RAST1."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
    IF R_ART = RART.
      MOVE  '1'    TO RASTERUU.
    ENDIF.
  ELSE.
    IF R_TAGE <= RP02
    OR RP07 IS INITIAL.
      MOVE: R_BETRAG TO RTAB-RAST2."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
      IF R_ART = RART.
        MOVE  '2'    TO RASTERUU.
      ENDIF.
    ELSE.
      IF R_TAGE <= RP03
      OR RP08 IS INITIAL.
        MOVE: R_BETRAG TO RTAB-RAST3."#EC CI_FLDEXT_OK[2610650]
         "Added by SPLABAP during code remediation
        IF R_ART = RART.
          MOVE  '3'    TO RASTERUU.
        ENDIF.
      ELSE.
        IF R_TAGE <= RP04
        OR RP09 IS INITIAL.
          MOVE: R_BETRAG TO RTAB-RAST4."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
          IF R_ART = RART.
            MOVE  '4'    TO RASTERUU.
          ENDIF.
        ELSE.
          IF R_TAGE <= RP05
          OR RP10 IS INITIAL.
            MOVE: R_BETRAG TO RTAB-RAST5."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
            IF R_ART = RART.
              MOVE  '5'    TO RASTERUU.
            ENDIF.
          ELSE.
            MOVE: R_BETRAG TO RTAB-RAST6."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
            IF R_ART = RART.
              MOVE  '6'    TO RASTERUU.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR RTAB-XGUTS.
  IF NOT XGETAUSW IS INITIAL.
    IF ZBSEGA-DMSHB LT 0.                                    "#EC *
      RTAB-XGUTS = 'X'.
    ENDIF.
  ENDIF.
  COLLECT RTAB.
* Summieren ueber alle Geschaeftsbereiche ---------------------------*
* aber nur wenn SORTART = '1' ----------------------------------------*
  MOVE: '1'      TO RTAB-SORTK,
        '**'     TO RTAB-GSBER.
  COLLECT RTAB.

ENDFORM.                    "R
*
**---------------------------------------------------------------------*
**       FORM ANSCHRIFT                                                *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM ANSCHRIFT.
*
*  DEFINE ADDRESS_LINE.
*    IF NOT ADRS-LINE&2 IS INITIAL.
*      LO_WRITER->ROW_OPEN( ).
*      CONCATENATE TEXT-113 '(&1):' INTO L_HLP_TXT.
*      LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT ).
*      LO_WRITER->SINGLE_WRITE_FIELD( ADRS-LINE&2(35) ).
*      LO_WRITER->ROW_CLOSE( ).
*    ENDIF.
*  END-OF-DEFINITION.
*
*  IF NOT LFB1-BUKRS IS INITIAL.
*    IF NOT KONZVERS IS INITIAL.
*      CHECK XBUKRDAT = 0.
*    ENDIF.
*  ENDIF.
*  PERFORM OBLIGOS.
*
*  IF KONZVERS IS INITIAL
*  OR LFB1-BUKRS IS INITIAL.
** <<<<<<<<<<<< Anschrift >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*    LO_WRITER->SECTION_WRITE( TEXT-113 ).
*    LO_WRITER->FORMAT_NORMAL( ).
*    ADDRESS_LINE: 1 0, 2 1, 3 2, 4 3, 5 4, 6 5, 7 6, 8 7, 9 8, 10 9.
*    LO_WRITER->ULINE( ).
*  ENDIF.
*
** <<<<<<<<<<<< Obligos >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*  IF NOT ( AOBLIGO IS INITIAL ) .
*    LO_WRITER->SECTION_WRITE( TEXT-116 ).
*    LO_WRITER->FORMAT_NORMAL( ).
*
*    LOOP AT AOBLIGO FROM 1 TO 12.
*      SHBBEZ = AOBLIGO-LTEXT.
*      ASUMS  = AOBLIGO-OBLIG.
*      LO_WRITER->ROW_OPEN( ).
*      CONCATENATE SHBBEZ TEXT-161 INTO L_HLP_TXT.
*      LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT ).
*      WRITE ASUMS TO L_HLP_TXT(12)
*                     CURRENCY T001-WAERS
*                     NO-ZERO
*                     ROUND FAKTOR
*                     DECIMALS STELLEN.
*      LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*      LO_WRITER->ROW_CLOSE( ).
*    ENDLOOP.
*
*    LO_WRITER->ULINE( ).
*  ENDIF.
*
** <<<<<<<<<<<<<<<<<<<< Umsatzdaten >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*  LO_WRITER->SECTION_WRITE( TEXT-115 ).
*  LO_WRITER->FORMAT_NORMAL( ).
*
*  LO_WRITER->ROW_OPEN( ).
*  LO_WRITER->SINGLE_WRITE_FIELD( TEXT-122 ).
*  WRITE C-KUMUM TO L_HLP_TXT(12)
*                 CURRENCY T001-WAERS
*                 NO-ZERO
*                 ROUND FAKTOR
*                 DECIMALS STELLEN.
*  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*  LO_WRITER->ROW_CLOSE( ).
*  LO_WRITER->TABLE_CLOSE( ).
*
*ENDFORM.                    "ANSCHRIFT
*
**---------------------------------------------------------------------*
**       FORM RASTER_AUSGABE                                           *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM RASTER_AUSGABE.
** Bei Verdichtung der Geschaeftsbereiche nur das Summenraster ausgeben*
*  IF RASTVERD = '1'.                   "AND VERDICHT > 0.
*    CHECK RTAB-SORTK = '1'.
*  ENDIF.
*
** Das Summen-Raster wird nur ausgegeben, wenn mehr als ein Geschaefts-*
** bereich vorhanden ist. ---------------------------------------------*
*  IF RTAB-SORTK = '1' AND RASTVERD NE '1'.
*    CHECK GBZAEHL > 1.
*  ENDIF.
*
*  IF  NOT KONZVERS IS INITIAL
*  AND NOT LFB1-BUKRS IS INITIAL.
*    CHECK XBUKRDAT NE '2'.
*  ENDIF.
*
*  IF RTAB-SORTK NE '2'.
*    IF VERDICHT NE '6'.
** Bei der ersten Rasterart       , Anzahlungen usw. ausgeben ---------*
*      IF RASTERUU = RART.
*        IF GB-GSBER NE '**'.
*          GBZAEHL = GBZAEHL + 1.
*        ENDIF.
*        IF GB-GSBER NE SAVE_GSBER
*        OR GB-WAERS NE SAVE_WAERS.
*          MOVE GB-GSBER TO SAVE_GSBER.
*          MOVE GB-WAERS TO SAVE_WAERS.
*        ENDIF.
*        RESERVE 5 LINES.
*
*        IF GB-GSBER NE '**'.
*          LO_WRITER->FORMAT_SUBTOTAL( ).
*        ELSE.
*          LO_WRITER->FORMAT_TOTAL( ).
*        ENDIF.
*
*        TOP-FLAG = '2'.
*
*        IF SORTART = '1' AND KONZVERS IS INITIAL.
*          LO_WRITER->ROW_OPEN( ).
*          LO_WRITER->SINGLE_WRITE_FIELD( GB-GSBER ).
*          LO_WRITER->SINGLE_WRITE_FIELD( '' ).
*          WRITE RTAB-ANZAH TO L_HLP_TXT(11)
*                           CURRENCY T001-WAERS
*                           ROUND FAKTOR
*                           DECIMALS STELLEN.
*          LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(11) ).
*          WRITE RTAB-OPSUM TO L_HLP_TXT(12)
*                           CURRENCY T001-WAERS
*                           ROUND FAKTOR
*                           DECIMALS STELLEN.
*          LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*        ELSE.
*          LO_WRITER->ROW_OPEN( ).
*          LO_WRITER->SINGLE_WRITE_FIELD( GB-GSBER ).
*          LO_WRITER->SINGLE_WRITE_FIELD( GB-WAERS ).
*          WRITE RTAB-ANZAH TO L_HLP_TXT(11)
*                           CURRENCY GB-WAERS
*                           ROUND FAKTOR
*                           DECIMALS STELLEN.
*          LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(11) ).
*          WRITE RTAB-OPSUM TO L_HLP_TXT(12)
*                           CURRENCY GB-WAERS
*                           ROUND FAKTOR
*                           DECIMALS STELLEN.
*          LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*        ENDIF.
*      ENDIF.
*
*      CASE RASTERUU.
*        WHEN '1'.                                                  " Net-Fae
*          LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-019 I_ABS_POSITION = 5 ).
*        WHEN '2'.                                                  " Skt-Fae
*          LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-020 I_ABS_POSITION = 5 ).
*        WHEN '3'.                                                  " Zhl-Ein
*          LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-021 I_ABS_POSITION = 5 ).
*        WHEN '4'.                                                  " Ueb-Fae
*          LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-022 I_ABS_POSITION = 5 ).
*      ENDCASE.
*    ELSE.
*      IF VERDICHT = '6'.
*        IF ACC_MODE IS INITIAL.
*          RESERVE 5 LINES.
*          TOP-FLAG = '2'.
*          TOP-FLA2 = 'X'.
*          LO_WRITER->ROW_OPEN( ).
*          LO_WRITER->FORMAT_NORMAL( ).
*          LO_WRITER->SINGLE_WRITE_FIELD( RTAB-LIFNR ).
*          LO_WRITER->SINGLE_WRITE_FIELD( RTAB-SORTL ).
*          IF RASTVERD = '1'.                                "1557468
*            LO_WRITER->SINGLE_WRITE_FIELD( RTAB-LAND1 ).    "1557468
*          ELSE.                                             "1557468
*            LO_WRITER->SINGLE_WRITE_FIELD( RTAB-GSBER ).    "1557468
*          ENDIF.                                            "1557468
*          IF SORTART = '1' AND KONZVERS IS INITIAL.
*            WRITE RTAB-OPSUM TO L_HLP_TXT(12)
*                             CURRENCY T001-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*          ELSE.
*            WRITE RTAB-OPSUM TO L_HLP_TXT(12)
*                             CURRENCY GB-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF ACC_MODE IS INITIAL.
*      IF SORTART = '1' AND KONZVERS IS INITIAL.
*        DO 6 TIMES VARYING L_RASTER FROM RTAB-RAST1 NEXT RTAB-RAST2.
*          WRITE L_RASTER TO L_HLP_TXT(13)
*                         CURRENCY T001-WAERS NO-ZERO
*                         ROUND FAKTOR DECIMALS STELLEN.
*          LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(13) ).
*        ENDDO.
*      ELSE.
*        DO 6 TIMES VARYING L_RASTER FROM RTAB-RAST1 NEXT RTAB-RAST2.
*          WRITE L_RASTER TO L_HLP_TXT(13)
*                         CURRENCY GB-WAERS NO-ZERO
*                         ROUND FAKTOR DECIMALS STELLEN.
*          LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(13) ).
*        ENDDO.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IF NOT ACC_MODE IS INITIAL.
*    IF RASTVERD = '0'.
*      CHECK RTAB-SORTK = '0'.
*    ENDIF.
*    CLEAR RTAB_ALV.
*    MOVE-CORRESPONDING RTAB TO RTAB_ALV.
*    RTAB_ALV-ADRS1 = ADRS-LINE0.                            "1253468
*    RTAB_ALV-ADRS2 = ADRS-LINE1.                            "1253468
*    RTAB_ALV-ADRS3 = ADRS-LINE2.                            "1253468
*    RTAB_ALV-ADRS4 = ADRS-LINE3.                            "1253468
*    IF NOT XGETAUSW IS INITIAL.
*      IF RTAB-XGUTS IS INITIAL.
*        RTAB_ALV-SHKZG = 'S'.
*      ELSE.
*        RTAB_ALV-SHKZG = 'H'.
*      ENDIF.
*    ENDIF.
*    IF RTAB-WAERS IS INITIAL.
*      RTAB_ALV-WAERS = T001-WAERS.
*    ENDIF.
*    IF RTAB-RAART = 4.
*      RTAB_ALV-RAART = 6.
*    ENDIF.
*
*    IF      KONZVERS IS INITIAL
*    AND NOT LFB1-BUKRS IS INITIAL.
*      RTAB_ALV-BUKRS = LFB1-BUKRS.
*    ENDIF.
*    IF NOT RTAB_ALV-BUKRS IS INITIAL.
*      RTAB_ALV-BUSAB = LFB1-BUSAB.
*    ENDIF.
*    MOVE C-KUMUM TO RTAB_ALV-KUMUM.
*    IF NOT RTAB_ALV IS INITIAL.
*      APPEND RTAB_ALV.
*    ENDIF.
*  ENDIF.
*  CLEAR TOP-FLA2.
*ENDFORM.                    "RASTER_AUSGABE
*
**---------------------------------------------------------------------*
**       FORM SUM_BUSAB_BUKRS_TOTAL                                    *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM SUM_BUSAB_BUKRS_TOTAL.
*  IF RTAB-SORTK = '0'.
*    IF       KONZVERS   IS INITIAL
*    OR ( NOT KONZVERS   IS INITIAL
*    AND  NOT RTAB-BUKRS IS INITIAL ) .
**-- Summen pro Sachbearbeiter ----------------------------------------*
*      MOVE-CORRESPONDING RTAB TO RBUS.
*      MOVE: LFB1-BUKRS TO  RBUS-BUKRS,
*            GB-GSBER TO  RBUS-GSBER,
*            GB-WAERS TO  RBUS-WAERS.
*      COLLECT RBUS.
**-- Gesamtsumme ueber alle Geschaeftsbereiche pro Sachbearb. ---------*
**-- ermitteln, aber nur bei SORTART = '1' ----------------------------*
*      MOVE: LFB1-BUKRS TO  RBUS-BUKRS,
*            '**'     TO  RBUS-GSBER,
*            '1'      TO  RBUS-SORTK.
*      COLLECT RBUS.
*
**-- Summen pro Buchungskreis -----------------------------------------*
*      MOVE-CORRESPONDING RTAB TO RBUK.
*      MOVE: LFB1-BUKRS TO  RBUK-BUKRS,
*            GB-GSBER   TO  RBUK-GSBER,
*            GB-WAERS   TO  RBUK-WAERS.
*      COLLECT RBUK.
**-- Gesamtsumme ueber alle Geschaeftsbereiche und Sachbearb. ---------*
**-- ermitteln, aber nur bei SORTART = '1' ----------------------------*
*      MOVE: LFB1-BUKRS TO  RBUK-BUKRS,
*            '**'       TO  RBUK-GSBER,
*            '1'        TO  RBUK-SORTK.
*      COLLECT RBUK.
*    ENDIF.
*
**-- Summen fuer Listenteil 2 ermitteln -------------------------------*
*    MOVE: LFB1-BUKRS TO  RTAB-BUKRS,
*          GB-GSBER TO  RTAB-GSBER,
*          GB-WAERS TO  RTAB-WAERS.
*    COLLECT RTAB.
*
**-- Gesamtsumme ueber alle Buchungs- und Geschaeftsbereiche ----------*
**-- ermitteln, aber nur bei SORTART = '1' ----------------------------*
*    IF       KONZVERS   IS INITIAL
*    OR ( NOT KONZVERS   IS INITIAL
*    AND      RTAB-BUKRS IS INITIAL ) .
*      MOVE-CORRESPONDING RTAB TO RSUM.
*      IF SORTART = '1' AND KONZVERS IS INITIAL.
*        MOVE: T001-WAERS TO  RSUM-WAERS.
*      ENDIF.
*      MOVE: '1'        TO  RSUM-SORTK.
*      COLLECT RSUM.
*    ENDIF.
*  ENDIF.
*
*  IF  RTAB-SORTK = '2'.
**-- Gesamtumsatz ueber alle Geschaeftsbereiche pro Sachbearb. --------*
**-- ermitteln, aber nur bei SORTART = '1' ----------------------------*
*    IF SORTART = '1'.
*      MOVE-CORRESPONDING RTAB TO RBUS.
*      MOVE: LFB1-BUKRS TO  RBUS-BUKRS,
*            GB-GSBER   TO  RBUS-GSBER,
*            GB-WAERS   TO  RBUS-WAERS,
*            '2'        TO  RBUS-SORTK.
*      COLLECT RBUS.
*    ENDIF.
*
**-- Gesamtumsatz ueber alle Geschaeftsbereiche und Sachbearb. --------*
**-- ermitteln, aber nur bei SORTART = '1' ----------------------------*
*    IF SORTART = '1'.
*      MOVE-CORRESPONDING RTAB TO RBUK.
*      MOVE: LFB1-BUKRS TO  RBUK-BUKRS,
*            GB-GSBER   TO  RBUK-GSBER,
*            GB-WAERS   TO  RBUK-WAERS,
*            '2'        TO  RBUK-SORTK.
*      COLLECT RBUK.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    "SUM_BUSAB_BUKRS_TOTAL
*
*---------------------------------------------------------------------*
*       FORM EINZELPOSTEN_AUSGABE                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM EINZELPOSTEN_AUSGABE.

  LO_WRITER->FORMAT_NORMAL( ).
  LO_WRITER->ROW_OPEN( ).
  LO_WRITER->SINGLE_WRITE_FIELD( LFB1-BUKRS ).
  LO_WRITER->SINGLE_WRITE_FIELD( GB-GSBER ).

  WRITE TAGE TO L_HLP_TXT(8).
  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(8) ).
  LO_WRITER->SINGLE_WRITE_FIELD( BSIK-UMSKZ ).
  LO_WRITER->SINGLE_WRITE_FIELD( BSIK-BLART ).
  LO_WRITER->SINGLE_WRITE_FIELD( BSIK-BELNR ).

  WRITE BSIK-BUZEI TO L_HLP_TXT(3).
  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(3) ).

  WRITE ZBSEGA-NETDT TO L_HLP_TXT(8).
  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(8) ).

  WRITE BSIK-ZFBDT TO L_HLP_TXT(8).
  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(8) ).

  WRITE BSIK-BUDAT TO L_HLP_TXT(8).
  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(8) ).

  WRITE BSIK-BLDAT TO L_HLP_TXT(8).
  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(8) ).

  LO_WRITER->SINGLE_WRITE_FIELD( BSIK-BSCHL ).
  LO_WRITER->SINGLE_WRITE_FIELD( BSIK-ZLSCH ).

  WRITE BSIK-MANST NO-ZERO TO L_HLP_TXT(1).
  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(1) ).

  WRITE ZBSEGA-DMSHB CURRENCY T001-WAERS TO L_HLP_TXT(21).
  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(21) ).
  LO_WRITER->SINGLE_WRITE_FIELD( BSIK-WAERS ).

  WRITE ZBSEGA-WRSHB CURRENCY BSIK-WAERS NO-ZERO TO L_HLP_TXT(20).
  LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(20) ).

  LO_WRITER->ROW_CLOSE( ).

ENDFORM.                    "EINZELPOSTEN_AUSGABE

**---------------------------------------------------------------------*
**       FORM RASTER_AUSGABE_BUSAB                                     *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM RASTER_AUSGABE_BUSAB.
*  IF RASTVERD < '2'.
*    DETAIL.
*    SORT RBUS.
*    CLEAR GBZAEHL.
*    DESCRIBE TABLE RBUS LINES L_HLP_LINES.
*    L_HLP_I = 0.
*    LOOP AT RBUS.
*      L_HLP_I = L_HLP_I + 1.
*
** Bei Verdichtung der Geschaeftsbereiche nur das Summenraster ausgeben*
*      IF RASTVERD = '1'.               " AND VERDICHT > 0.
*        CHECK RBUS-SORTK NE '0'.
*      ENDIF.
*
** Das Summen-Raster wird nur ausgegeben, wenn mehr als ein Geschaefts-*
** bereich vorhanden ist. ---------------------------------------------*
*      IF RBUS-SORTK = '1' AND RASTVERD NE '1'.
*        CHECK GBZAEHL GT 1.
*      ENDIF.
*
*      IF RBUS-SORTK NE '2'.
*        IF RBUS-GSBER NE '**'.
*          LO_WRITER->FORMAT_SUBTOTAL( ).
*        ELSE.
*          LO_WRITER->FORMAT_TOTAL( ).
*        ENDIF.
*
** Bei der ersten Rasterart  Anzahlungen usw. ausgeben ----------------*
*        IF RBUS-RAART = RART.
*          IF RBUS-GSBER NE '**'.
*            GBZAEHL = GBZAEHL + 1.
*          ENDIF.
*          RESERVE 5 LINES.
*
*          LO_WRITER->ROW_OPEN( ).
*          LO_WRITER->SINGLE_WRITE_FIELD( RBUS-GSBER ).
*          IF SORTART = '1' AND KONZVERS IS INITIAL.
*            LO_WRITER->SINGLE_WRITE_FIELD( '' ).
*            WRITE RBUS-ANZAH TO L_HLP_TXT(11)
*                             CURRENCY T001-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(11) ).
*            WRITE RBUS-OPSUM TO L_HLP_TXT(12)
*                             CURRENCY T001-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*          ELSE.
*            LO_WRITER->SINGLE_WRITE_FIELD( RBUS-WAERS ).
*            WRITE RBUS-ANZAH TO L_HLP_TXT(11)
*                             CURRENCY RBUS-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(11) ).
*            WRITE RBUS-OPSUM TO L_HLP_TXT(12)
*                             CURRENCY RBUS-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*          ENDIF.
*
*        ELSE.
*          LO_WRITER->ROW_OPEN( ).
*        ENDIF.
*
*        CASE RBUS-RAART.
*          WHEN '1'.                                                 " Net-Fae
*            LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-019 I_ABS_POSITION = 5 ).
*          WHEN '2'.                                                 " Skt-Fae
*            LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-020 I_ABS_POSITION = 5 ).
*          WHEN '3'.                                                 " Zhl-Fae
*            LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-021 I_ABS_POSITION = 5 ).
*          WHEN '4'.                                                 " Ueb-Fae
*            LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-022 I_ABS_POSITION = 5 ).
*        ENDCASE.
*
*        IF SORTART = '1' AND KONZVERS IS INITIAL.
*          DO 6 TIMES VARYING L_RASTER FROM RBUS-RAST1 NEXT RBUS-RAST2.
*            WRITE L_RASTER TO L_HLP_TXT(13)
*                           CURRENCY T001-WAERS NO-ZERO
*                           ROUND FAKTOR DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(13) ).
*          ENDDO.
*
*        ELSE.
*          DO 6 TIMES VARYING L_RASTER FROM RBUS-RAST1 NEXT RBUS-RAST2.
*            WRITE L_RASTER TO L_HLP_TXT(13)
*                           CURRENCY RBUS-WAERS NO-ZERO
*                           ROUND FAKTOR DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(13) ).
*          ENDDO.
*        ENDIF.
*      ELSE.
*        LO_WRITER->FORMAT_TOTAL( ).
*        LO_WRITER->ROW_OPEN( ).
*        CLEAR L_HLP_TXT.
*        WRITE: RBUS-GSBER(4) TO L_HLP_TXT,
*               TEXT-166 TO L_HLP_TXT+7,
*               RBUS-KUMUM CURRENCY T001-WAERS TO L_HLP_TXT+87(18).
*        LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*        LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*      ENDIF.
*
*      AT END OF WAERS.
*        IF L_HLP_I < L_HLP_LINES.
*          LO_WRITER->ULINE( ).
*        ELSE.
*          LO_WRITER->TABLE_CLOSE( ).
*        ENDIF.
*      ENDAT.
*
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.                    "RASTER_AUSGABE_BUSAB
*
**---------------------------------------------------------------------*
**       FORM RASTER_AUSGABE_BUKRB                                     *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM RASTER_AUSGABE_BUKRB.
*  IF RASTVERD < '2'.
*    DETAIL.
*    SORT RBUK.
*    CLEAR GBZAEHL.
*    DESCRIBE TABLE RBUK LINES L_HLP_LINES.
*    L_HLP_I = 0.
*    LOOP AT RBUK.
*      L_HLP_I = L_HLP_I + 1.
*      NEW-LINE.
*
** Bei Verdichtung der Geschaeftsbereiche nur das Summenraster ausgeben*
*      IF RASTVERD = '1'.               " AND VERDICHT > 0.
*        CHECK RBUK-SORTK NE '0' .
*      ENDIF.
*
** Das Summen-Raster wird nur ausgegeben, wenn mehr als ein Geschaefts-*
** bereich vorhanden ist. ---------------------------------------------*
*      IF RBUK-SORTK = '1' AND RASTVERD NE '1'.
*        CHECK GBZAEHL GT 1.
*      ENDIF.
*
*      IF RBUK-GSBER NE '**'.
*        LO_WRITER->FORMAT_SUBTOTAL( ).
*      ELSE.
*        LO_WRITER->FORMAT_TOTAL( ).
*      ENDIF.
*
*      IF RBUK-SORTK NE '2'.
** Bei der ersten Rasterart         Anzahlungen usw. ausgeben ---------*
*        IF RBUK-RAART = RART.
*          IF RBUK-GSBER NE '**'.
*            GBZAEHL = GBZAEHL + 1.
*          ENDIF.
*          RESERVE 5 LINES.
*          LO_WRITER->ROW_OPEN( ).
*          LO_WRITER->SINGLE_WRITE_FIELD( RBUK-GSBER ).
*          IF SORTART = '1' AND KONZVERS IS INITIAL.
*            LO_WRITER->SINGLE_WRITE_FIELD( '' ).
*            WRITE RBUK-ANZAH TO L_HLP_TXT(11)
*                             CURRENCY T001-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(11) ).
*            WRITE RBUK-OPSUM TO L_HLP_TXT(12)
*                             CURRENCY T001-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*          ELSE.
*            LO_WRITER->SINGLE_WRITE_FIELD( RBUK-WAERS ).
*            WRITE RBUK-ANZAH TO L_HLP_TXT(11)
*                             CURRENCY RBUK-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(11) ).
*            WRITE RBUK-OPSUM TO L_HLP_TXT(12)
*                             CURRENCY RBUK-WAERS
*                             ROUND FAKTOR
*                             DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*          ENDIF.
*        ELSE.
*          LO_WRITER->ROW_OPEN( ).
*        ENDIF.
*
*        CASE RBUK-RAART.
*          WHEN '1'.                                                 " Net-Fae
*            LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-019 I_ABS_POSITION = 5 ).
*          WHEN '2'.                                                 " Skt-Fae
*            LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-020 I_ABS_POSITION = 5 ).
*          WHEN '3'.                                                 " Zhl-Fae
*            LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-021 I_ABS_POSITION = 5 ).
*          WHEN '4'.                                                 " Alt-Fae
*            LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-022 I_ABS_POSITION = 5 ).
*        ENDCASE.
*
*        IF SORTART = '1' AND KONZVERS IS INITIAL.
*          DO 6 TIMES VARYING L_RASTER FROM RBUK-RAST1 NEXT RBUK-RAST2.
*            WRITE L_RASTER TO L_HLP_TXT(13)
*                           CURRENCY T001-WAERS NO-ZERO
*                           ROUND FAKTOR DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(13) ).
*          ENDDO.
*        ELSE.
*          DO 6 TIMES VARYING L_RASTER FROM RBUK-RAST1 NEXT RBUK-RAST2.
*            WRITE L_RASTER TO L_HLP_TXT(13)
*                           CURRENCY RBUK-WAERS NO-ZERO
*                           ROUND FAKTOR DECIMALS STELLEN.
*            LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(13) ).
*          ENDDO.
*        ENDIF.
*      ELSE.
** Ausgabe des Jahresumsatzes -----------------------------------------*
*        LO_WRITER->FORMAT_TOTAL( ).
*        LO_WRITER->ROW_OPEN( ).
*        CLEAR L_HLP_TXT.
*        WRITE: RBUK-GSBER(4) TO L_HLP_TXT,
*               TEXT-166 TO L_HLP_TXT+7,
*               RBUK-KUMUM CURRENCY T001-WAERS TO L_HLP_TXT+87(18).
*        LO_WRITER->WRITE_INSERTED_LINE( L_HLP_TXT ).
*        LO_WRITER->ROW_CLOSE( IB_INSERTED_LINE = ABAP_TRUE ).
*      ENDIF.
*
*      AT END OF WAERS.
*        IF L_HLP_I < L_HLP_LINES.
*          LO_WRITER->ULINE( ).
*        ELSE.
*          LO_WRITER->TABLE_CLOSE( ).
*        ENDIF.
*      ENDAT.
*
*    ENDLOOP.
*  ENDIF.
*ENDFORM.                    "RASTER_AUSGABE_BUKRB
*
**---------------------------------------------------------------------*
**       FORM RASTER_AUSGABE_TOTAL                                     *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM RASTER_AUSGABE_TOTAL.
*  IF RASTVERD < '2'.
*    DETAIL.
*    REPLACE 'RP01' WITH RC01 INTO VARUEB2.                  "bis   0
*    REPLACE 'RP02' WITH RC02 INTO VARUEB2.                  "bis  20
*    REPLACE 'RP03' WITH RC03 INTO VARUEB2.                  "bis  40
*    REPLACE 'RP04' WITH RC04 INTO VARUEB2.                  "bis  80
*    REPLACE 'RP05' WITH RC05 INTO VARUEB2.                  "bis 100
*    REPLACE 'RP06' WITH RC06 INTO VARUEB1.                  "von   1
*    REPLACE 'RP07' WITH RC07 INTO VARUEB1.                  "von  21
*    REPLACE 'RP08' WITH RC08 INTO VARUEB1.                  "von  41
*    REPLACE 'RP09' WITH RC09 INTO VARUEB1.                  "von  81
*    REPLACE 'RP10' WITH RC10 INTO VARUEB1.                  "von 101
*    SORT RSUM.
*    LOOP AT RSUM.
*
** Bei Verdichtung der Geschaeftsbereiche nur das Summenraster ausgeben*
*      IF RASTVERD = '1' AND VERDICHT > 0.
*        CHECK RSUM-SORTK = '1'.
*      ENDIF.
*
*      FORMAT COLOR COL_TOTAL INTENSIFIED.
*
** Bei der ersten Rasterart Umsatz, Anzahlungen usw. ausgeben ---------*
*      IF RSUM-RAART = RART.
*        RESERVE 5 LINES.
*        LO_WRITER->ROW_OPEN( ).
*        LO_WRITER->SINGLE_WRITE_FIELD( '**' ).
*        LO_WRITER->SINGLE_WRITE_FIELD( RSUM-WAERS ).
*        WRITE RSUM-ANZAH TO L_HLP_TXT(11)
*                         CURRENCY RSUM-WAERS
*                         ROUND FAKTOR
*                         DECIMALS STELLEN.
*        LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(11) ).
*        WRITE RSUM-OPSUM TO L_HLP_TXT(12)
*                         CURRENCY RSUM-WAERS
*                         ROUND FAKTOR
*                         DECIMALS STELLEN.
*        LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(12) ).
*      ELSE.
*        LO_WRITER->ROW_OPEN( ).
*      ENDIF.
*
*      CASE RSUM-RAART.
*        WHEN '1'.                                                 " Net-Fae
*          LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-019 I_ABS_POSITION = 5 ).
*        WHEN '2'.                                                 " Skt-Fae
*          LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-020 I_ABS_POSITION = 5 ).
*        WHEN '3'.                                                 " Zhl-Fae
*          LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-021 I_ABS_POSITION = 5 ).
*        WHEN '4'.                                                 " Ueb-Fae
*          LO_WRITER->SINGLE_WRITE_FIELD( I_FIELD = TEXT-022 I_ABS_POSITION = 5 ).
*      ENDCASE.
*
*      DO 6 TIMES VARYING L_RASTER FROM RSUM-RAST1 NEXT RSUM-RAST2.
*        WRITE L_RASTER TO L_HLP_TXT(13)
*                       CURRENCY RSUM-WAERS NO-ZERO
*                       ROUND FAKTOR DECIMALS STELLEN.
*        LO_WRITER->SINGLE_WRITE_FIELD( L_HLP_TXT(13) ).
*      ENDDO.
*
*      AT END OF WAERS.
*        LO_WRITER->ULINE( ).
*      ENDAT.
*      LO_WRITER->ROW_CLOSE( ).
*
*    ENDLOOP.
*  ENDIF.
*ENDFORM.                    "RASTER_AUSGABE_TOTAL
*
**---------------------------------------------------------------------*
**       FORM SHB_KENNZEICHEN                                          *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
FORM SHB_KENNZEICHEN.
  CLEAR HUMKZ1.
  CLEAR HUMKZ2.
  CLEAR HUMKZ3.
  CLEAR HUMKZ4.
  CLEAR HUMKZ5.
  CLEAR HUMKZ6.
  CLEAR HUMKZ7.
  CLEAR HUMKZ8.
  CLEAR HUMKZ9.
  CLEAR HUMKZ10.

  IF NOT UMSATZKZ(1) IS INITIAL.
    HUMKZ1 = UMSATZKZ(1).
  ENDIF.
  IF NOT UMSATZKZ+1(1) IS INITIAL.
    HUMKZ2 = UMSATZKZ+1(1).
  ENDIF.
  IF NOT UMSATZKZ+2(1) IS INITIAL.
    HUMKZ3 = UMSATZKZ+2(1).
  ENDIF.
  IF NOT UMSATZKZ+3(1) IS INITIAL.
    HUMKZ4 = UMSATZKZ+3(1).
  ENDIF.
  IF NOT UMSATZKZ+4(1) IS INITIAL.
    HUMKZ5 = UMSATZKZ+4(1).
  ENDIF.
  IF NOT UMSATZKZ+5(1) IS INITIAL.
    HUMKZ6 = UMSATZKZ+5(1).
  ENDIF.
  IF NOT UMSATZKZ+6(1) IS INITIAL.
    HUMKZ7 = UMSATZKZ+6(1).
  ENDIF.
  IF NOT UMSATZKZ+7(1) IS INITIAL.
    HUMKZ8 = UMSATZKZ+7(1).
  ENDIF.
  IF NOT UMSATZKZ+8(1) IS INITIAL.
    HUMKZ9 = UMSATZKZ+8(1).
  ENDIF.
  IF NOT UMSATZKZ+9(1) IS INITIAL.
    HUMKZ10 = UMSATZKZ+9(1).
  ENDIF.

ENDFORM.                    "SHB_KENNZEICHEN

*---------------------------------------------------------------------*
*       FORM SHBKZ_PRUEFEN                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SHBKZ_PRUEFEN.
  CLEAR FLAG1.
  SELECT * FROM TBSL"#EC CI_NOORDER
    WHERE KOART = 'K'.
    IF NOT TBSL-XSONU IS INITIAL.
      SELECT * FROM TBSLT
        WHERE BSCHL = TBSL-BSCHL
        AND   UMSKZ = CHAR1
        ORDER BY PRIMARY KEY.
         "Added by SPLABAP during code remediation

        FLAG1 = 'X'.
      ENDSELECT.
    ENDIF.
  ENDSELECT.
  IF NOT FLAG1 IS INITIAL.
    SELECT SINGLE * FROM T074U
      WHERE KOART = 'K'
      AND   UMSKZ = CHAR1.
    IF NOT T074U-MERKP IS INITIAL.
      IF SY-BATCH IS INITIAL.
        SET CURSOR FIELD 'UMSATZKZ'.
      ENDIF.
      MESSAGE W376 WITH CHAR1 'D'.
    ENDIF.

    SELECT SINGLE * FROM T074T
      WHERE SPRAS = SY-LANGU
      AND   KOART = 'K'
      AND   SHBKZ = CHAR1.
    IF SY-SUBRC = 0.
      BEZSHB-SHBKZ = T074T-SHBKZ.
      BEZSHB-LTEXT = T074T-LTEXT.
      APPEND BEZSHB.
    ELSE.
      CLEAR FLAG1.
    ENDIF.
  ENDIF.
  IF FLAG1 IS INITIAL.
    IF SY-BATCH IS INITIAL.
      SET CURSOR FIELD 'UMSATZKZ'.
    ENDIF.
    MESSAGE W375 WITH CHAR1 'D'.
  ENDIF.
ENDFORM.                    "SHBKZ_PRUEFEN
*
*---------------------------------------------------------------------*
*       FORM OBLIGOS                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM OBLIGOS.
  CLEAR AOBLIGO.
  REFRESH AOBLIGO.
  IF NOT C-SALDO IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '1' TO AOBLIGO-OBART.
    MOVE C-SALDO TO AOBLIGO-OBLIG.
    WRITE TEXT-117 TO AOBLIGO-LTEXT.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS1 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ1 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS1 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ1.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS2 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ2 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS2 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ2.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS3 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ3 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS3 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ3.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS4 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ4 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS4 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ4.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS5 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ5 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS5 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ5.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS6 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ6 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS6 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ6.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS7 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ7 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS7 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ7.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS8 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ8 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS8 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ8.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS9 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ9 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS9 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ9.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SUMS10 IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '2' TO AOBLIGO-OBART.
    MOVE C-UMKZ10 TO AOBLIGO-SHBKZ.
    MOVE C-SUMS10 TO AOBLIGO-OBLIG.
    LOOP AT BEZSHB
      WHERE SHBKZ = C-UMKZ10.
      MOVE BEZSHB-LTEXT TO AOBLIGO-LTEXT.
    ENDLOOP.
    APPEND AOBLIGO.
  ENDIF.
  IF NOT C-SONOB IS INITIAL.
    CLEAR AOBLIGO.
    MOVE '3' TO AOBLIGO-OBART.
    MOVE C-SONOB TO AOBLIGO-OBLIG.
    WRITE TEXT-152 TO AOBLIGO-LTEXT.
    APPEND AOBLIGO.
  ENDIF.
  SORT AOBLIGO.

ENDFORM.                    "OBLIGOS
*
*---------------------------------------------------------------------*
*       FORM EINZELPOSTEN_SICHERN                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM EINZELPOSTEN_SICHERN.
  CLEAR HBSIK.
  MOVE-CORRESPONDING BSIK  TO HBSIK.
  MOVE-CORRESPONDING ZBSEGA TO HBSIK.                        "#EC ENHOK
  APPEND  HBSIK.
ENDFORM.                    "EINZELPOSTEN_SICHERN

*---------------------------------------------------------------------*
*       FORM EINZELPOSTEN_EXTRACT                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM EINZELPOSTEN_EXTRACT.
  LOOP AT HBSIK
    WHERE BUKRS = LFB1-BUKRS.
    CLEAR BSIK.
    CLEAR ZBSEGA.
    MOVE-CORRESPONDING HBSIK TO BSIK.
    MOVE-CORRESPONDING HBSIK TO ZBSEGA.                      "#EC ENHOK
    IF NOT PZUOR IS INITIAL.
      TAGE = HBSIK-UTAGE.
    ELSE.
      TAGE = KD_STIDA - ZBSEGA-NETDT.
    ENDIF.
    IF TAGE <= RP01.
      MOVE  '1'    TO RASTERUU.
    ELSE.
      IF TAGE <= RP02
      OR RP07 IS INITIAL.
        MOVE  '2'    TO RASTERUU.
      ELSE.
        IF TAGE <= RP03
        OR RP08 IS INITIAL.
          MOVE  '3'    TO RASTERUU.
        ELSE.
          IF TAGE <= RP04
          OR RP09 IS INITIAL.
            MOVE  '4'    TO RASTERUU.
          ELSE.
            IF TAGE <= RP05.
              MOVE  '5'    TO RASTERUU.
            ELSE.
              MOVE  '6'    TO RASTERUU.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF SORTART = '1'.
      MOVE SPACE    TO GB-WAERS.
    ELSE.
      MOVE BSIK-WAERS TO GB-WAERS.
    ENDIF.
    MOVE   '3'    TO SATZART.
    MOVE   '0'    TO RTAB-SORTK.
    MOVE BSIK-GSBER TO GB-GSBER.
    MOVE ZBSEGA-DMSHB TO SHBETRAG.
*------Der Fremdwährungsbetrag soll nur Übernommen werden, wenn sich
*----- die WÄHRUNG VON DER HAUSWÄHRUNG UNTERSCHEIDET.
    IF BSIK-WAERS EQ T001-WAERS.
      MOVE SPACE TO ZBSEGA-WRSHB.
    ENDIF.
    EXTRACT EINZELPOSTEN.
  ENDLOOP.
ENDFORM.                    "EINZELPOSTEN_EXTRACT

*---------------------------------------------------------------------*
*       FORM EINZELPOSTEN_SAVE                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM EINZELPOSTEN_SAVE.
  CLEAR HBSIK.
  CLEAR REFBL.
  MOVE-CORRESPONDING BSIK  TO HBSIK.
  MOVE-CORRESPONDING ZBSEGA TO HBSIK.                        "#EC ENHOK
  MOVE NTAGE TO HBSIK-NTAGE.
  MOVE STAGE TO HBSIK-STAGE.
  MOVE ZTAGE TO HBSIK-ZTAGE.
  MOVE UTAGE TO HBSIK-UTAGE.
  APPEND HBSIK.
  MOVE-CORRESPONDING BSIK  TO REFBL.                        "#EC ENHOK
  MOVE NTAGE TO REFBL-NTAGE.
  MOVE STAGE TO REFBL-STAGE.
  MOVE ZTAGE TO REFBL-ZTAGE.
  MOVE UTAGE TO REFBL-UTAGE.
  APPEND REFBL.
ENDFORM.                    "EINZELPOSTEN_SAVE

*---------------------------------------------------------------------*
*       FORM EINZELPOSTEN_LINK                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM EINZELPOSTEN_LINK.
  SORT REFBL BY BUKRS BELNR GJAHR BUZEI.                    "1579685
  LOOP AT HBSIK
    WHERE REBZG NE SPACE.
    READ TABLE REFBL WITH KEY BUKRS = HBSIK-BUKRS           "1579685
                              BELNR = HBSIK-REBZG           "1579685
                              GJAHR = HBSIK-REBZJ           "1579685
                              BUZEI = HBSIK-REBZZ           "1579685
                              BINARY SEARCH.                "1579685
    IF SY-SUBRC = 0.                                        "1579685
      HBSIK-NTAGE = REFBL-NTAGE.
      HBSIK-STAGE = REFBL-STAGE.
      HBSIK-ZTAGE = REFBL-ZTAGE.
      HBSIK-UTAGE = REFBL-UTAGE.
      MODIFY HBSIK.
    ENDIF.                                                  "1579685
  ENDLOOP.
ENDFORM.                    "EINZELPOSTEN_LINK

*---------------------------------------------------------------------*
*       FORM EINZELPOSTEN_PROC                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM EINZELPOSTEN_PROC.
  LOOP AT HBSIK
    WHERE BUKRS = LFB1-BUKRS.
    IF T001-BUKRS NE LFB1-BUKRS.
      READ TABLE HT001 WITH KEY BUKRS = LFB1-BUKRS.
      T001 = HT001.
    ENDIF.
    CLEAR BSIK.
    CLEAR ZBSEGA.
    MOVE-CORRESPONDING HBSIK TO BSIK.
    MOVE-CORRESPONDING HBSIK TO ZBSEGA.                      "#EC ENHOK
    NTAGE =  HBSIK-NTAGE.
    STAGE =  HBSIK-STAGE.
    ZTAGE =  HBSIK-ZTAGE.
    UTAGE =  HBSIK-UTAGE.

* die Einzelposten werden nach den Tagen der ersten Rasterart --------*
* sortiert -----------------------------------------------------------*
    IF RART-NET = 'X'.
      TAGE = NTAGE.
    ELSE.
      IF RART-SKT = 'X'.
        TAGE = STAGE.
      ELSE.
        IF RART-ZHL = 'X'.
          TAGE = ZTAGE.
        ELSE.
          IF RART-UEB = 'X'.
            TAGE = UTAGE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


    CASE BSIK-UMSKS.
*--------------- Anzahlungen sammeln ---------------------------------*
*--------------- auch wenn nicht von aussen abgegrenzt ---------------*
      WHEN 'A'.
        CLEAR RTAB.
        IF BSIK-BSTAT NE 'S'.
          MOVE: BSIK-BUKRS TO RTAB-BUKRS,
                '0'      TO RTAB-SORTK,
                BSIK-GSBER TO RTAB-GSBER,
                RART     TO RTAB-RAART.
          IF SORTART = '2'.
            MOVE BSIK-WAERS TO RTAB-WAERS.
            MOVE ZBSEGA-WRSHB TO RTAB-ANZAH."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
          ELSE.
            IF NOT KONZVERS IS INITIAL.
              MOVE T001-WAERS TO RTAB-WAERS.
              MOVE ZBSEGA-DMSHB TO RTAB-ANZAH."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
            ELSE.
              MOVE ZBSEGA-DMSHB TO RTAB-ANZAH."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
            ENDIF.
          ENDIF.
          CLEAR RTAB-XGUTS.
          IF NOT XGETAUSW IS INITIAL.
            IF ZBSEGA-DMSHB LT 0.
              RTAB-XGUTS = 'X'.
            ENDIF.
          ENDIF.
          MOVE:
          LFA1-LIFNR TO RTAB-LIFNR,
          LFA1-SORTL TO RTAB-SORTL,
          LFA1-LAND1 TO RTAB-LAND1.
          COLLECT RTAB.
*--------------- Summieren ueber alle Geschaeftsbereiche -------------*
          MOVE: '1'      TO RTAB-SORTK,
                '**'     TO RTAB-GSBER.
          COLLECT RTAB.
        ENDIF.
    ENDCASE.

    CHECK: BUDAT,
           BLDAT,
           NETDT.
    SEL-POSTN = 'J'.

    IF SORTART = '1'.
      IF KONZVERS IS INITIAL.
        PERFORM POSTEN_RASTERN USING SPACE.
        MOVE SPACE    TO GB-WAERS.
      ELSE.
        PERFORM POSTEN_RASTERN USING T001-WAERS.
        MOVE T001-WAERS TO GB-WAERS.
      ENDIF.
    ELSE.
      PERFORM POSTEN_RASTERN USING BSIK-WAERS.
      MOVE BSIK-WAERS TO GB-WAERS.
    ENDIF.

*----- Saldoberechnung Überfälligkeitsprüfung
    IF  UTAGE GT '0'
    AND UTAGE IN VERTAGE.
      IF NOT XNURFORD IS INITIAL.
        IF ZBSEGA-DMSHB GT '0'.         "Falls keine Habenpositionen
          H-SALD2        = H-SALD2        + ZBSEGA-DMSHB.
        ENDIF.                         "bei der berechnung berück-
      ELSE.                            "sichtigt werden sollen
        H-SALD2        = H-SALD2        + ZBSEGA-DMSHB.
      ENDIF.
    ENDIF.                             "hier die Sterne entfernen

*---- nur bei Verdichtungsstufe '0' werden EINZELPOSTEN extrahiert --*
    IF VERDICHT = '0'.
      IF  UTAGE GT '0'
      AND UTAGE IN VERTAGE.
      ELSE.
***        DELETE HBSIK.  "commented by Prabhu on 21.08.2020
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "EINZELPOSTEN_PROC
*
**---------------------------------------------------------------------*
**       FORM SUMM_C3                                                  *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM SUMM_C3.
*  C3-SALDO     = C3-SALDO + C-SALDO.
*  C3-UMKZ1     = C-UMKZ1.
*  C3-SUMS1     = C3-SUMS1 + C-SUMS1.
*  C3-UMKZ2     = C-UMKZ2.
*  C3-SUMS2     = C3-SUMS2 + C-SUMS2.
*  C3-UMKZ3     = C-UMKZ3.
*  C3-SUMS3     = C3-SUMS3 + C-SUMS3.
*  C3-UMKZ4     = C-UMKZ4.
*  C3-SUMS4     = C3-SUMS4 + C-SUMS4.
*  C3-UMKZ5     = C-UMKZ5.
*  C3-SUMS5     = C3-SUMS5 + C-SUMS5.
*  C3-UMKZ6     = C-UMKZ6.
*  C3-SUMS6     = C3-SUMS6 + C-SUMS6.
*  C3-UMKZ7     = C-UMKZ7.
*  C3-SUMS7     = C3-SUMS7 + C-SUMS7.
*  C3-UMKZ8     = C-UMKZ8.
*  C3-SUMS8     = C3-SUMS8 + C-SUMS8.
*  C3-UMKZ9     = C-UMKZ9.
*  C3-SUMS9     = C3-SUMS9 + C-SUMS9.
*  C3-UMKZ10    = C-UMKZ10.
*  C3-SUMS10    = C3-SUMS10 + C-SUMS10.
*  C3-SONOB     = C3-SONOB  + C-SONOB.
*  C3-BABZG     = C3-BABZG  + C-BABZG.
*  C3-UABZG     = C3-UABZG  + C-UABZG.
*  C3-KZINS     = C3-KZINS  + C-KZINS.
*  C3-KUMUM     = C3-KUMUM  + C-KUMUM.
*  C3-KUMAG     = C3-KUMAG  + C-KUMAG.
*  C3-AGOBLI    = C3-AGOBLI + C-AGOBLI.
*ENDFORM.                                                    "SUMM_C3
*
**&--------------------------------------------------------------------*
**&      Form  RASTER_AUSGABE_ALV_GRID
**&--------------------------------------------------------------------*
**       ........
**---------------------------------------------------------------------*
*FORM RASTER_AUSGABE_ALV_GRID.
*
*  TITTEXT = TEXT-300.
*  WRITE KD_STIDA TO DATTEXT.
*  REPLACE '&' WITH DATTEXT INTO TITTEXT.
*  G_GRID_TITLE = TITTEXT.
*  G_REPID = SY-REPID.
*  G_SAVE = 'A'.
*  PERFORM FIELDCAT_INIT USING GT_FIELDCAT[].
*
*  IF TITLE IS INITIAL.                                      "1613289
*    CLEAR G_TOP_OF_PAGE.                                    "1613289
*  ENDIF.                                                    "1613289
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      I_CALLBACK_PROGRAM      = G_REPID
*      I_CALLBACK_USER_COMMAND = G_USER_COMMAND
*      I_CALLBACK_TOP_OF_PAGE  = G_TOP_OF_PAGE               "1613289
*      I_GRID_TITLE            = G_GRID_TITLE
*      IT_FIELDCAT             = GT_FIELDCAT[]
*      I_DEFAULT               = 'X'
*      I_SAVE                  = G_SAVE
*      IS_VARIANT              = GS_VARIANT
*    TABLES
*      T_OUTTAB                = RTAB_ALV.
*
*ENDFORM.                    "RASTER_AUSGABE_ALV_GRID
*
*
**&---------------------------------------------------------------------*
**&      Form  FIELDCAT_INIT
**&---------------------------------------------------------------------*
**       ........
**----------------------------------------------------------------------*
*FORM FIELDCAT_INIT
*       USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'BUKRS'.
*  LS_FIELDCAT-REF_FIELDNAME = 'BUKRS'.
*  LS_FIELDCAT-REF_TABNAME   = 'BSIK'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'LIFNR'.
*  LS_FIELDCAT-REF_FIELDNAME = 'LIFNR'.
*  LS_FIELDCAT-REF_TABNAME   = 'LFA1'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'BUSAB'.
*  LS_FIELDCAT-REF_FIELDNAME = 'BUSAB'.
*  LS_FIELDCAT-REF_TABNAME   = 'LFB1'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'SORTL'.
*  LS_FIELDCAT-REF_FIELDNAME = 'SORTL'.
*  LS_FIELDCAT-REF_TABNAME   = 'LFA1'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'LAND1'.
*  LS_FIELDCAT-REF_FIELDNAME = 'LAND1'.
*  LS_FIELDCAT-REF_TABNAME   = 'LFA1'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'GSBER'.
*  LS_FIELDCAT-REF_FIELDNAME = 'GSBER'.
*  LS_FIELDCAT-REF_TABNAME   = 'BSIK'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'WAERS'.
*  LS_FIELDCAT-REF_FIELDNAME = 'WAERS'.
*  LS_FIELDCAT-REF_TABNAME   = 'BSIK'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'RAART'.
*  LS_FIELDCAT-REF_FIELDNAME = 'RAART'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'SHKZG'.
*  LS_FIELDCAT-REF_FIELDNAME = 'SHKZG'.
*  LS_FIELDCAT-REF_TABNAME   = 'BSIK'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'KUMUM'.
*  LS_FIELDCAT-REF_FIELDNAME = 'KUMUMHW'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
*  LS_FIELDCAT-NO_ZERO       = 'X'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'ANZAH'.
*  LS_FIELDCAT-REF_FIELDNAME = 'ANZBTHW'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
*  LS_FIELDCAT-NO_ZERO       = 'X'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'OPSUM'.
*  LS_FIELDCAT-REF_FIELDNAME = 'GSALDD'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
*  LS_FIELDCAT-NO_ZERO       = 'X'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  WRITE: RP01 TO RC01.
*  WRITE: RP02 TO RC02.
*  WRITE: RP03 TO RC03.
*  WRITE: RP04 TO RC04.
*  WRITE: RP05 TO RC05.
*  WRITE: RP06 TO RC06.
*  WRITE: RP07 TO RC07.
*  WRITE: RP08 TO RC08.
*  WRITE: RP09 TO RC09.
*  WRITE: RP10 TO RC10.
*
*  CLEAR UEBTEXT.
*  CLEAR UEKTEXT.
*  UEBTEXT = TEXT-201.
*  UEKTEXT = TEXT-201.
*  REPLACE 'RP01' WITH RC01 INTO UEBTEXT.
*  REPLACE 'RP01' WITH RC01 INTO UEKTEXT.
*  CONDENSE UEBTEXT.
*  CONDENSE UEKTEXT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'RAST1'.
*  LS_FIELDCAT-REF_FIELDNAME = 'RAST1'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  LS_FIELDCAT-SELTEXT_S     = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_M     = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L     = UEBTEXT.
*  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
*  LS_FIELDCAT-NO_ZERO       = 'X'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR UEBTEXT.
*  CLEAR UEKTEXT.
*  IF NOT RP06 IS INITIAL.
*    IF RP02 IS INITIAL.
*      UEKTEXT = TEXT-206.
*      REPLACE 'RP06' WITH RC06 INTO UEKTEXT.
*    ELSE.
*      UEBTEXT = TEXT-206.
*      UEKTEXT      = RC06(3).
*      UEKTEXT+4(1) = '-'.
*    ENDIF.
*  ENDIF.
*  IF NOT RP02 IS INITIAL.
*    UEBTEXT+11(11) = TEXT-202.
*    IF NOT RP06 IS INITIAL.
*      UEKTEXT+6 = RC02(3).
*
*    ENDIF.
*  ENDIF.
*  REPLACE 'RP02' WITH RC02 INTO UEBTEXT.
*  REPLACE 'RP06' WITH RC06 INTO UEBTEXT.
*  CONDENSE UEBTEXT.
*  CONDENSE UEKTEXT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'RAST2'.
*  LS_FIELDCAT-REF_FIELDNAME = 'RAST2'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  LS_FIELDCAT-SELTEXT_S     = UEKTEXT.
*  LS_FIELDCAT-SELTEXT_M     = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L     = UEBTEXT.
*  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
*  LS_FIELDCAT-NO_ZERO       = 'X'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR UEBTEXT.
*  CLEAR UEKTEXT.
*  IF NOT RP07 IS INITIAL.
*    IF RP03 IS INITIAL.
*      UEKTEXT = TEXT-207.
*      REPLACE 'RP07' WITH RC07 INTO UEKTEXT.
*    ELSE.
*      UEBTEXT = TEXT-207.
*      UEKTEXT      = RC07(3).
*      UEKTEXT+4(1) = '-'.
*    ENDIF.
*  ENDIF.
*  IF NOT RP03 IS INITIAL.
*    UEBTEXT+11(11) = TEXT-203.
*    IF NOT RP07 IS INITIAL.
*      UEKTEXT+6 = RC03(3).
*    ENDIF.
*  ENDIF.
*
*  REPLACE 'RP03' WITH RC03 INTO UEBTEXT.
*  REPLACE 'RP07' WITH RC07 INTO UEBTEXT.
*  CONDENSE UEBTEXT.
*  CONDENSE UEKTEXT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'RAST3'.
*  LS_FIELDCAT-REF_FIELDNAME = 'RAST3'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  LS_FIELDCAT-SELTEXT_S     = UEKTEXT.
*  LS_FIELDCAT-SELTEXT_M     = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L     = UEBTEXT.
*  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
*  LS_FIELDCAT-NO_ZERO       = 'X'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR UEBTEXT.
*  CLEAR UEKTEXT.
*  IF NOT RP08 IS INITIAL.
*    IF RP04 IS INITIAL.
*      UEKTEXT = TEXT-208.
*      REPLACE 'RP08' WITH RC08 INTO UEKTEXT.
*    ELSE.
*      UEBTEXT = TEXT-208.
*      UEKTEXT      = RC08(3).
*      UEKTEXT+4(1) = '-'.
*    ENDIF.
*  ENDIF.
*  IF NOT RP04 IS INITIAL.
*    UEBTEXT+11(11) = TEXT-204.
*    IF NOT RP08 IS INITIAL.
*      UEKTEXT+6 = RC04(3).
*    ENDIF.
*  ENDIF.
*
*  REPLACE 'RP04' WITH RC04 INTO UEBTEXT.
*  REPLACE 'RP08' WITH RC08 INTO UEBTEXT.
*  CONDENSE UEBTEXT.
*  CONDENSE UEKTEXT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'RAST4'.
*  LS_FIELDCAT-REF_FIELDNAME = 'RAST4'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  LS_FIELDCAT-SELTEXT_S     = UEKTEXT.
*  LS_FIELDCAT-SELTEXT_M     = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L     = UEBTEXT.
*  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
*  LS_FIELDCAT-NO_ZERO       = 'X'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR UEBTEXT.
*  CLEAR UEKTEXT.
*  IF NOT RP09 IS INITIAL.
*    IF RP05 IS INITIAL.
*      UEKTEXT = TEXT-209.
*      REPLACE 'RP09' WITH RC09 INTO UEKTEXT.
*    ELSE.
*      UEBTEXT = TEXT-209.
*      UEKTEXT      = RC09(3).
*      UEKTEXT+4(1) = '-'.
*    ENDIF.
*  ENDIF.
*  IF NOT RP05 IS INITIAL.
*    UEBTEXT+11(11) = TEXT-205.
*    IF NOT RP09 IS INITIAL.
*      UEKTEXT+6 = RC05(3).
*    ENDIF.
*  ENDIF.
*
*  REPLACE 'RP05' WITH RC05 INTO UEBTEXT.
*  REPLACE 'RP09' WITH RC09 INTO UEBTEXT.
*  CONDENSE UEBTEXT.
*  CONDENSE UEKTEXT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'RAST5'.
*  LS_FIELDCAT-REF_FIELDNAME = 'RAST5'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  LS_FIELDCAT-SELTEXT_S     = UEKTEXT.
*  LS_FIELDCAT-SELTEXT_M     = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L     = UEBTEXT.
*  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
*  LS_FIELDCAT-NO_ZERO       = 'X'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR UEBTEXT.
*  CLEAR UEKTEXT.
*  IF NOT RP10 IS INITIAL.
*    UEBTEXT = TEXT-210.
*    UEKTEXT = TEXT-210.
*  ENDIF.
*  REPLACE 'RP10' WITH RC10 INTO UEBTEXT.
*  REPLACE 'RP10' WITH RC10 INTO UEKTEXT.
*  CONDENSE UEBTEXT.
*  CONDENSE UEKTEXT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME       = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME     = 'RAST6'.
*  LS_FIELDCAT-REF_FIELDNAME = 'RAST6'.
*  LS_FIELDCAT-REF_TABNAME   = 'RF140'.
*  LS_FIELDCAT-SELTEXT_S     = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_M     = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L     = UEBTEXT.
*  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
*  LS_FIELDCAT-NO_ZERO       = 'X'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
**** Address info                                               "1253468
*  UEBTEXT = TEXT-113.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME   = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME = 'ADRS1'.
*  UEBTEXT+9 = '(1)'.
*  LS_FIELDCAT-SELTEXT_S = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_M = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L = UEBTEXT.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME   = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME = 'ADRS2'.
*  UEBTEXT+9 = '(2)'.
*  LS_FIELDCAT-SELTEXT_S = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_M = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L = UEBTEXT.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME   = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME = 'ADRS3'.
*  UEBTEXT+9 = '(3)'.
*  LS_FIELDCAT-SELTEXT_S = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_M = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L = UEBTEXT.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*  CLEAR LS_FIELDCAT.
*  LS_FIELDCAT-TABNAME   = G_TABNAME.
*  LS_FIELDCAT-FIELDNAME = 'ADRS4'.
*  UEBTEXT+9 = '(4)'.
*  LS_FIELDCAT-SELTEXT_S = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_M = UEBTEXT.
*  LS_FIELDCAT-SELTEXT_L = UEBTEXT.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
**** Address info                                               "1253468
*
*ENDFORM.                    " FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       ........
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM               "#EC CALLED
                 RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LT_SELTAB TYPE STANDARD TABLE OF RSPARAMS WITH HEADER LINE.

  CASE R_UCOMM.
    WHEN '&IC1'.
      READ TABLE RTAB_ALV INTO RTAB_ALV INDEX RS_SELFIELD-TABINDEX.

      LT_SELTAB-SELNAME = 'KD_LIFNR'.
      LT_SELTAB-SIGN    = 'I'.
      LT_SELTAB-OPTION  = 'EQ'.

      LT_SELTAB-LOW     = RTAB_ALV-LIFNR.
      APPEND LT_SELTAB.
      IF NOT RTAB_ALV-BUKRS IS INITIAL.
        LT_SELTAB-SELNAME = 'KD_BUKRS'.
        LT_SELTAB-SIGN    = 'I'.
        LT_SELTAB-OPTION  = 'EQ'.
        LT_SELTAB-LOW     = RTAB_ALV-BUKRS.
        APPEND LT_SELTAB.
      ENDIF.
      LT_SELTAB-SELNAME = 'X_OPSEL'.
      LT_SELTAB-SIGN    = 'I'.
      LT_SELTAB-OPTION  = 'EQ'.
      LT_SELTAB-LOW     = 'X'.
      APPEND LT_SELTAB.
      LT_SELTAB-SELNAME = 'PA_STIDA'.

      LT_SELTAB-SIGN    = 'I'.
      LT_SELTAB-OPTION  = 'EQ'.
      LT_SELTAB-LOW     = KD_STIDA.
      APPEND LT_SELTAB.
      LT_SELTAB-SELNAME = 'X_NORM'.
      LT_SELTAB-SIGN    = 'I'.
      LT_SELTAB-OPTION  = 'EQ'.
      LT_SELTAB-LOW     = 'X'.
      APPEND LT_SELTAB.
      LT_SELTAB-SELNAME = 'X_SHBV'.
      LT_SELTAB-SIGN    = 'I'.
      LT_SELTAB-OPTION  = 'EQ'.
      LT_SELTAB-LOW     = 'X'.
      APPEND LT_SELTAB.
      LT_SELTAB-SELNAME = 'PA_GRID'.
      LT_SELTAB-SIGN    = 'I'.

      LT_SELTAB-OPTION  = 'EQ'.
      LT_SELTAB-LOW     = 'Y'.
      APPEND LT_SELTAB.

      SUBMIT RFITEMAP WITH SELECTION-TABLE LT_SELTAB
                      AND  RETURN.
  ENDCASE.
ENDFORM.                    " USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  F4_FOR_s_lvar
*&---------------------------------------------------------------------*
*       ........
*----------------------------------------------------------------------*
FORM F4_FOR_S_LVAR CHANGING  I_VARIANT LIKE DISVARIANT.
  DATA: EXIT.
  DATA: E_VARIANT LIKE DISVARIANT.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = I_VARIANT
      I_SAVE        = 'A'
    IMPORTING
      E_EXIT        = EXIT
      ES_VARIANT    = E_VARIANT
    EXCEPTIONS
      PROGRAM_ERROR = 3
      OTHERS        = 3.
  IF SY-SUBRC = 0 AND EXIT = SPACE.
    I_VARIANT-VARIANT = E_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                               " F4_FOR_s_lvar

**&---------------------------------------------------------------------*
**&      Form  TOP_OF_PAGE
**&---------------------------------------------------------------------*
**       Possible header for ALV Grid                           "1613289
**----------------------------------------------------------------------*
*FORM TOP_OF_PAGE.                                           "#EC CALLED
*
*  CLEAR GS_LISTHEADER.
*  REFRESH GT_LISTHEADER.
*  GS_LISTHEADER-TYP = 'H'.
*  GS_LISTHEADER-INFO = TITLE.
*  INSERT GS_LISTHEADER INTO TABLE GT_LISTHEADER.
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      IT_LIST_COMMENTARY = GT_LISTHEADER.
*ENDFORM.                               " TOP_OF_PAGE
