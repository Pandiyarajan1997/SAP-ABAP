************************************************************************
*   Smart Form Print Program                              *
************************************************************************
************************************************************************************************
*                 PROGRAM for C
************************************************************************************************
* Program Name          : CUSTOMER LIST
* Functional Analyst    :
* Programmer            :  SHANMUGASUNDARAM
* Start date            :
* Description           :  Program for C
* Type Of Program       :
*----------------------------------------------------------------------------------------------
PROGRAM YKP_MM_CONTRACT_NEW MESSAGE-ID ME.

type-pools:   addi, meein,
              mmpur.
tables: nast,                          "Messages
        *nast,                         "Messages
        tnapr,                         "Programs & Forms
        itcpo,                         "Communicationarea for Spool
        arc_params,                    "Archive parameters
        toa_dara,                      "Archive parameters
        addr_key.                      "Adressnumber for ADDRESS

type-pools szadr.
tables : varposr,
         rwerter,
         mtxh.

data  umbruch type i value 4.
data  headerflag.
data  begin of vartab occurs 15.
        include structure varposr.
data  end   of vartab.
data  tabn type i.
data  taba type i.
data  ebelph like ekpo-ebelp.
data  bis type i.
data  xmax type i.
data  tab like varposr-yzeile.
data  diff type i.
data  ldat_sam like eket-eindt.

data: s, v type i.
data: sampr like pekpov-netpr, varpr like pekpov-netpr.

* Struktur zur Variantenbildung

data: begin of wertetab occurs 30.
        include structure rwerter.
data: atzhl like econf_out-atzhl,
      end of wertetab.

* Interne Tabelle fuer Konditionen
data: begin of kond occurs 30.
        include structure komvd.
data: end of kond.
* Hilfsfelder
data:
      merknamex(15) type c,            "Merkmalname x-Achse
      merknamey(15) type c,            "Merkmalname y-Achse
      merknrx like rwerter-nr,         "Int. Merkmal x-Achse
      merknry like rwerter-nr,         "Int. Merkmal y-Achse
      i type i value 1,
      nr like cawn-atinn.
data: inserterror(1),sum type i,menge type i,gsumh type i, xmaxh type i.
data: gsumv type i.
* Matrixflag
data: m_flag value 'x'.

*- Tabellen -----------------------------------------------------------*
tables: cpkme,
        ekvkp,
        ekko,
        pekko,
        rm06p,
        ekpo,
        pekpo,
        pekpov,
        pekpos,
        eket,
        ekek,
        ekes,
        ekeh,
        ekkn,
        ekpa,
        ekbe,
        eine, *eine,
        lfa1,
        likp,
       *lfa1,
        kna1,
        komk,
        komp,
        komvd,
        ekomd,
        econf_out,
        thead, *thead,
        sadr,
        mdpa,
        mdpm,
        mkpf,
        tinct,
        ttxit,
        tmsi2,
        tq05,
        tq05t,
        t001,
        t001w,
        t006, *t006,
        t006a, *t006a,
        t024,
        t024e,
        t027a,
        t027b,
        t052,
        t161n,
        t163d,
        t166a,
        t165p,
        t166c,
        t166k,
        t166p,
        t166t,
        t166u,
        t165m,
        t165a,
        tmamt,
       *mara,                                               "HTN 4.0C
        mara,
        marc,
        mt06e,
        makt,
        vbak,
        vbkd,
       *vbkd,
        vbap.
tables: drad,
        drat.
tables: addr1_sel,
        addr1_val.
tables: v_htnm, rampl,tmppf.           "HTN-Abwicklung

tables: stxh.              "schnellerer Zugriff auf Texte Dienstleistung

tables: t161.              "Abgebotskennzeichen für Dienstleistung

*  DATA :  IT_PO   TYPE TABLE OF ZMM_PO_STRUC WITH  HEADER LINE.
  DATA :  IT_PO   TYPE TABLE OF ZMM_GST_PO_STRUC WITH  HEADER LINE. "" Added by Bharath
*  DATA :  WA_PO   TYPE  ZMM_PO_STRUC .
  DATA :  WA_PO   TYPE  ZMM_GST_PO_STRUC . "" Added by bharath

*- INTERNE TABELLEN ---------------------------------------------------*
*- Tabelle der Positionen ---------------------------------------------*
data: begin of xekpo occurs 10.
        include structure ekpo.
data:     bsmng like ekes-menge,
      end of xekpo.

*- Key für xekpo ------------------------------------------------------*
data: begin of xekpokey,
         mandt like ekpo-mandt,
         ebeln like ekpo-ebeln,
         ebelp like ekpo-ebelp,
      end of xekpokey.

*- Tabelle der Einteilungen -------------------------------------------*
data: begin of xeket occurs 10.
        include structure eket.
data:     fzete like pekpo-wemng,
      end of xeket.

*- Tabelle der Einteilungen temporär ----------------------------------*
data: begin of teket occurs 10.
        include structure beket.
data: end of teket.

data: begin of zeket.
        include structure eket.
data:  end of zeket.

*- Tabelle der Positionszusatzdaten -----------------------------------*
data: begin of xpekpo occurs 10.
        include structure pekpo.
data: end of xpekpo.

*- Tabelle der Positionszusatzdaten -----------------------------------*
data: begin of xpekpov occurs 10.
        include structure pekpov.
data: end of xpekpov.

*- Tabelle der Zahlungbedingungen--------------------------------------*
data: begin of zbtxt occurs 5,
         line(50),
      end of zbtxt.

*- Tabelle der Merkmalsausprägungen -----------------------------------*
data: begin of tconf_out occurs 50.
        include structure econf_out.
data: end of tconf_out.

*- Tabelle der Konditionen --------------------------------------------*
data: begin of tkomv occurs 50.
        include structure komv.
data: end of tkomv.

data: begin of tkomk occurs 1.
        include structure komk.
data: end of tkomk.

data: begin of tkomvd occurs 50.       "Belegkonditionen
        include structure komvd.
data: end of tkomvd.

data: begin of tekomd occurs 50.       "Stammkonditionen
        include structure ekomd.
data: end of tekomd.

*- Tabelle der Bestellentwicklung -------------------------------------*
data: begin of xekbe occurs 10.
        include structure ekbe.
data: end of xekbe.

*- Tabelle der Bezugsnebenkosten --------------------------------------*
data: begin of xekbz occurs 10.
        include structure ekbz.
data: end of xekbz.

*- Tabelle der WE/RE-Zuordnung ----------------------------------------*
data: begin of xekbez occurs 10.
        include structure ekbez.
data: end of xekbez.

*- Tabelle der Positionssummen der Bestellentwicklung -----------------*
data: begin of tekbes occurs 10.
        include structure ekbes.
data: end of tekbes.

*- Tabelle der Bezugsnebenkosten der Bestandsführung ------------------*
data: begin of xekbnk occurs 10.
        include structure ekbnk.
data: end of xekbnk.

*- Tabelle für Kopieren Positionstexte (hier wegen Infobestelltext) ---*
data: begin of xt165p occurs 10.
        include structure t165p.
data: end of xt165p.

*- Tabelle der Kopftexte ----------------------------------------------*
data: begin of xt166k occurs 10.
        include structure t166k.
data: end of xt166k.

*- Tabelle der Positionstexte -----------------------------------------*
data: begin of xt166p occurs 10.
        include structure t166p.
data: end of xt166p.

*- Tabelle der Anahngstexte -------------------------------------------*
data: begin of xt166a occurs 10.
        include structure t166a.
data: end of xt166a.

*- Tabelle der Textheader ---------------------------------------------*
data: begin of xthead occurs 10.
        include structure thead.
data: end of xthead.

data: begin of xtheadkey,
         tdobject like thead-tdobject,
         tdname like thead-tdname,
         tdid like thead-tdid,
      end of xtheadkey.

data: begin of qm_text_key occurs 5,
         tdobject like thead-tdobject,
         tdname like thead-tdname,
         tdid like thead-tdid,
         tdtext like ttxit-tdtext,
      end of qm_text_key.

*- Tabelle der Nachrichten alt/neu ------------------------------------*
data: begin of xnast occurs 10.
        include structure nast.
data: end of xnast.

data: begin of ynast occurs 10.
        include structure nast.
data: end of ynast.

*------ Struktur zur Übergabe der Adressdaten --------------------------
data:    begin of addr_fields.
        include structure sadrfields.
data:    end of addr_fields.

*------ Struktur zur Übergabe der Adressreferenz -----------------------
data:    begin of addr_reference.
        include structure addr_ref.
data:    end of addr_reference.

*------ Tabelle zur Übergabe der Fehler -------------------------------
data:    begin of error_table occurs 10.
        include structure addr_error.
data:    end of error_table.

*------ Tabelle zur Übergabe der Adressgruppen ------------------------
data:    begin of addr_groups occurs 3.
        include structure adagroups.
data:    end of addr_groups.

*- Tabelle der Aenderungsbescheibungen --------------------------------*
data: begin of xaend occurs 10,
         ebelp like ekpo-ebelp,
         zekkn like ekkn-zekkn,
         etenr like eket-etenr,
         ctxnr like t166c-ctxnr,
         rounr like t166c-rounr,
         insert,
         flag_adrnr,
      end of xaend.

data: begin of xaendkey,
         ebelp like ekpo-ebelp,
         zekkn like ekkn-zekkn,
         etenr like eket-etenr,
         ctxnr like t166c-ctxnr,
         rounr like t166c-rounr,
         insert,
         flag_adrnr,
      end of xaendkey.

*- Tabelle der Textänderungen -----------------------------------------*
data: begin of xaetx occurs 10,
         ebelp like ekpo-ebelp,
         textart like cdshw-textart,
         chngind like cdshw-chngind,
      end of xaetx.

*- Tabelle der geänderten Adressen ------------------------------------*
data: begin of xadrnr occurs 5,
         adrnr like sadr-adrnr,
         tname like cdshw-tabname,
         fname like cdshw-fname,
      end of xadrnr.

*- Tabelle der gerade bearbeiteten aktive Komponenten -----------------*
data begin of mdpmx occurs 10.
        include structure mdpm.
data end of mdpmx.

*- Tabelle der gerade bearbeiteten Sekundärbedarfe --------------------*
data begin of mdsbx occurs 10.
        include structure mdsb.
data end of mdsbx.

*- Struktur des Archivobjekts -----------------------------------------*
data: begin of xobjid,
        objky  like nast-objky,
        arcnr  like nast-optarcnr,
      end of xobjid.

* Struktur für zugehörigen Sammelartikel
data: begin of sekpo.
        include structure ekpo.
data:   first_varpos,
      end of sekpo.

*- Struktur für Ausgabeergebnis zB Spoolauftragsnummer ----------------*
data: begin of result.
        include structure itcpp.
data: end of result.

*- Struktur für Internet NAST -----------------------------------------*
data: begin of intnast.
        include structure snast.
data: end of intnast.

*- HTN-Abwicklung
data: begin of htnmat occurs 0.
        include structure v_htnm.
data:  revlv like rampl-revlv,
      end of htnmat.

data  htnamp like rampl  occurs 0 with header line.

*- Hilfsfelder --------------------------------------------------------*
data: hadrnr(8),                       "Key TSADR
      elementn(30),                    "Name des Elements
      save_el(30),                     "Rettfeld für Element
      retco like sy-subrc,             "Returncode Druck
      insert,                          "Kz. neue Position
      h-ind like sy-tabix,             "Hilfsfeld Index
      h-ind1 like sy-tabix,            "Hilfsfeld Index
      f1 type f,                       "Rechenfeld
      h-menge like ekpo-menge,         "Hilfsfeld Mengenumrechnung
      h-meng1 like ekpo-menge,         "Hilfsfeld Mengenumrechnung
      h-meng2 like ekpo-menge,         "Hilfsfeld Mengenumrechnung
      ab-menge like ekes-menge,        "Hilfsfeld bestätigte Menge
      kzbzg like konp-kzbzg,           "Staffeln vorhanden?
      hdatum like eket-eindt,          "Hilfsfeld Datum
      hmahnz like ekpo-mahnz,          "Hilfsfeld Mahnung
      addressnum like ekpo-adrn2,      "Hilfsfeld Adressnummer
      tablines like sy-tabix,          "Zähler Tabelleneinträge
      entries  like sy-tfill,          "Zähler Tabelleneinträge
      hstap,                           "statistische Position
      hsamm,                           "Positionen mit Sammelartikel
      hloep,                           "Gelöschte Positionen im Spiel
      hkpos,                           "Kondition zu löschen
      kopfkond,                        "Kopfkonditionen vorhanden
      no_zero_line,                    "keine Nullzeilen
      xdrflg like t166p-drflg,         "Hilfsfeld Textdruck
      xprotect,                        "Kz. protect erfolgt
      archiv_object like toa_dara-ar_object, "für opt. Archivierung
      textflag,                        "Kz. druckrel. Positionstexte
      flag,                            "allgemeines Kennzeichen
      spoolid(10),                     "Spoolidnummer
      xprogram like sy-repid,          "Programm
      lvs_recipient like swotobjid,    "Internet
      lvs_sender like swotobjid,       "Internet
      timeflag,                        "Kz. Uhrzeit bei mind. 1 Eint.
      h_vbeln like vbak-vbeln,
      h_vbelp like vbap-posnr.

*- Drucksteuerung -----------------------------------------------------*
data: aendernsrv.
data: xdruvo.                          "Druckvorgang
data: neu  value '1',                  "Neudruck
      aend value '2',                  "Änderungsdruck
      mahn value '3',                  "Mahnung
      absa value '4',                  "Absage
      lpet value '5',                  "Lieferplaneinteilung
      lpma value '6',                  "Mahnung Lieferplaneinteilung
      aufb value '7',                  "Auftragsbestätigung
      lpae value '8',                  "Änderung Lieferplaneinteilung
      lphe value '9',                  "Historisierte Einteilungen
      preisdruck,                      "Kz. Gesamtpreis drucken
      kontrakt_preis,                  "Kz. Kontraktpreise drucken
      we   value 'E'.                  "Wareneingangswert

*- Hilfsfelder Lieferplaneinteilung -----------------------------------*
data:
      xlpet,                           "Lieferplaneinteilung
      xfz,                             "Fortschrittszahlendarstellung
      xoffen,                          "offene WE-Menge
      xlmahn,                          "Lieferplaneinteilungsmahnung
      fzflag,                          "KZ. Abstimmdatum erreicht
      xnoaend,                         "keine Änderungsbelege da  LPET
      xetdrk,                        "Druckrelevante Positionen da LPET
      xetefz like eket-menge,          "Einteilungsfortschrittszahl
      xwemfz like eket-menge,          "Lieferfortschrittszahl
      xabruf like ekek-abruf,          "Alter Abruf
      p_abart like ekek-abart.         "Abrufart

*data: sum-euro-price like komk-fkwrt.                       "302203
data: sum-euro-price like komk-fkwrt_euro.                  "302203
data: euro-price like ekpo-effwr.

*- Hilfsfelder für Ausgabemedium --------------------------------------*
data: xdialog,                         "Kz. POP-UP
      xscreen,                         "Kz. Probeausgabe
      xformular like tnapr-fonam,      "Formular
      xdevice(10).                     "Ausgabemedium

*- Hilfsfelder für QM -------------------------------------------------*
data: qv_text_i like tq09t-kurztext,   "Bezeichnung Qualitätsvereinb.
      tl_text_i like tq09t-kurztext,   "Bezeichnung Technische Lieferb.
      zg_kz.                           "Zeugnis erforderlich

*- Hilfsfelder für Änderungsbeleg -------------------------------------*
data: objectid              like cdhdr-objectid,
      tcode                 like cdhdr-tcode,
      planned_change_number like cdhdr-planchngnr,
      utime                 like cdhdr-utime,
      udate                 like cdhdr-udate,
      username              like cdhdr-username,
      cdoc_planned_or_real  like cdhdr-change_ind,
      cdoc_upd_object       like cdhdr-change_ind value 'U',
      cdoc_no_change_pointers like cdhdr-change_ind.


*- Common-Part für Änderungsbeleg -------------------------------------*
*include zzfm06lccd.
data:    begin of common part fm06lccd.

*------- Tabelle der Änderunsbelegzeilen (temporär) -------------------*
data: begin of edit occurs 50.             "Änderungsbelegzeilen temp.
        include structure cdshw.
data: end of edit.

data: begin of editd occurs 50.             "Änderungsbelegzeilen temp.
        include structure cdshw.            "für Dienstleistungen
data: end of editd.


*------- Tabelle der Änderunsbelegzeilen (Ausgabeform) ----------------*
data: begin of ausg occurs 50.             "Änderungsbelegzeilen
        include structure cdshw.
data:   changenr like cdhdr-changenr,
        udate    like cdhdr-udate,
        utime    like cdhdr-utime,
      end of ausg.

*------- Tabelle der Änderunsbelegköpfe -------------------------------*
data: begin of icdhdr occurs 50.           "Änderungbelegköpfe
        include structure cdhdr.
data: end of icdhdr.

*------- Key Tabelle der Änderunsbelegköpfe --------------------------*
data: begin of hkey,                       "Key für ICDHDR
        mandt like cdhdr-mandant,
        objcl like cdhdr-objectclas,
        objid like cdhdr-objectid,
        chang like cdhdr-changenr,
      end of hkey.

*------- Key der geänderten Tabelle für Ausgabe ----------------------*
data: begin of ekkey,                    "Tabellenkeyausgabe
        ebeln like ekko-ebeln,
        ebelp like ekpo-ebelp,
        zekkn like ekkn-zekkn,
        etenr like eket-etenr,
        abruf like ekek-abruf,
        ekorg like ekpa-ekorg,           "Änderungsbelege Partner
        ltsnr like ekpa-ltsnr,           "Änderungsbelege Partner
        werks like ekpa-werks,           "Änderungsbelege Partner
        parvw like ekpa-parvw,           "Änderungsbelege Partner
        parza like ekpa-parza,           "Änderungsbelege Partner
        consnumber like adr2-consnumber, "Änderungsbelege Adressen
        comm_type  like adrt-comm_type,  "Änderungsbelege Adressen
      end of ekkey.

data:    end of common part.
*- Direktwerte --------------------------------------------------------*
************************************************************************
*          Direktwerte                                                 *
************************************************************************
*------- Werte zu Trtyp und Aktyp:
constants:  hin value 'H',             "Hinzufuegen
            ver value 'V',             "Veraendern
            anz value 'A',             "Anzeigen
            erw value 'E'.             "Bestellerweiterung

constants:
* BSTYP
  bstyp-info value 'I',
  bstyp-ordr value 'W',
  bstyp-banf value 'B',
  bstyp-best value 'F',
  bstyp-anfr value 'A',
  bstyp-kont value 'K',
  bstyp-lfpl value 'L',
  bstyp-lerf value 'Q',

* BSAKZ
  bsakz-norm value ' ',
  bsakz-tran value 'T',
  bsakz-rahm value 'R',
* BSAKZ-BEIS VALUE 'B',  "not used
* BSAKZ-KONS VALUE 'K',  "not used
* BSAKZ-LOHN VALUE 'L', "not used
* BSAKZ-STRE VALUE 'S', "not used
* BSAKZ-MENG VALUE 'M', "not used
* BSAKZ-WERT VALUE 'W', "not used
* PSTYP
  pstyp-lagm value '0',
  pstyp-blnk value '1',
  pstyp-kons value '2',
  pstyp-lohn value '3',
  pstyp-munb value '4',
  pstyp-stre value '5',
  pstyp-text value '6',
  pstyp-umlg value '7',
  pstyp-wagr value '8',
  pstyp-dien value '9',

* Kzvbr
  kzvbr-anla value 'A',
  kzvbr-unbe value 'U',
  kzvbr-verb value 'V',
  kzvbr-einz value 'E',
  kzvbr-proj value 'P',

* ESOKZ
  esokz-pipe value 'P',
  esokz-lohn value '3',
  esokz-konsi value '2',               "konsi
  esokz-charg value '1',               "sc-jp
  esokz-norm value '0'.

constants:
* Handling von Unterpositionsdaten
       sihan-nix  value ' ',           "keine eigenen Daten
       sihan-anz  value '1', "Daten aus Hauptposition kopiert, nicht änd
       sihan-kop  value '2', "Daten aus Hauptposition kopiert, aber ände
       sihan-eig  value '3'. "eigene Daten (nicht aus Hauptposition kopi

* Unterpositionstypen
constants:
  uptyp-hpo value ' ',                 "Hauptposition
  uptyp-var value '1',                 "Variante
  uptyp-nri value '2',           "Naturalrabatt Inklusive (=Dreingabe)
  uptyp-ler value '3',                 "Leergut
  uptyp-nre value '4',           "Naturalrabatt Exklusive (=Draufgabe)
  uptyp-lot value '5',                 "Lot Position
  uptyp-dis value '6',                 "Display Position
  uptyp-vks value '7',                 "VK-Set Position
  uptyp-mpn value '8',                 "Austauschposition (A&D)
  uptyp-sls value '9',           "Vorkommisionierungsposition (retail)
  uptyp-div value 'X'.           "HP hat UP's mit verschiedenen Typen

* Artikeltypen
constants:
  attyp-sam(2) value '01',             "Sammelartikel
  attyp-var(2) value '02',             "Variante
  attyp-we1(2) value '20',             "Wertartikel
  attyp-we2(2) value '21',             "Wertartikel
  attyp-we3(2) value '22',             "Wertartikel
  attyp-vks(2) value '10',             "VK-Set
  attyp-lot(2) value '11',             "Lot-Artikel
  attyp-dis(2) value '12'.             "Display

* Konfigurationsherkunft
constants:
  kzkfg-fre value ' ',                 "Konfiguration sonst woher
  kzkfg-kan value '1',                 "noch nicht konfiguriert
  kzkfg-eig value '2'.                 "Eigene Konfiguration

constants:
  c_ja   type c value 'X',
  c_nein type c value ' '.

* Vorgangsart, welche Anwendung den Fkt-Baustein aufruft
constants:
  cva_ab(1) value 'B',     "Automatische bestellung (aus banfen)
  cva_we(1) value 'C',                 "Wareneingang
  cva_bu(1) value 'D',     "Übernahme bestellungen aus fremdsystem
  cva_au(1) value 'E',                 "Aufteiler
  cva_kb(1) value 'F',                 "Kanban
  cva_fa(1) value 'G',                 "Filialauftrag
  cva_dr(1) value 'H',                                      "DRP
  cva_en(1) value '9',                 "Enjoy
  cva_ap(1) value '1',                                      "APO
  cva_ed(1) value 'T'.     "EDI-Eingang Auftragsbestätigung Update Preis

* Status des Einkaufsbeleges (EKKO-STATU)
constants:
  cks_ag(1) value 'A',                 "Angebot vorhanden für Anfrage
  cks_ab(1) value 'B',     "Automatische Bestellung (aus Banfen) ME59
  cks_we(1) value 'C',                 "Bestellung aus Wareneingang
  cks_bu(1) value 'D',                 "Bestellung aus Datenübernahme
  cks_au(1) value 'E',     "Bestellung aus Aufteiler (IS-Retail)
  cks_kb(1) value 'F',                 "Bestellung aus Kanban
  cks_fa(1) value 'G',     "Bestellung aus Filialauftrag (IS-Retail)
  cks_dr(1) value 'H',                 "Bestellung aus DRP
  cks_ba(1) value 'I',                 "Bestellung aus BAPI
  cks_al(1) value 'J',                 "Bestellung aus ALE-Szenario
  cks_sb(1) value 'S',                 "Sammelbestellung (IS-Retail)
  cks_ap(1) value '1',                                      "APO
  cks_en(1) value '9',                 "Enjoy Bestellung
  cks_fb(1) value 'X'.                 "Bestellung aus Funktionsbaustein

* Vorgang aus T160
constants:
  vorga-angb(2) value 'AG',   "Angebot zur Anfrage    ME47, ME48
  vorga-lpet(2) value 'LE',   "Lieferplaneinteilung   ME38, ME39
  vorga-frge(2) value 'EF',   "Einkaufsbelegfreigabe  ME28, ME35, ME45
  vorga-frgb(2) value 'BF',   "Banffreigabe           ME54, ME55
  vorga-bgen(2) value 'BB',            "Best. Lief.unbekannt   ME25
  vorga-anha(2) value 'FT',   "Textanhang             ME24, ME26,...
  vorga-banf(2) value 'B ',   "Banf                   ME51, ME52, ME53
  vorga-anfr(2) value 'A ',   "Anfrage                ME41, ME42, ME43
  vorga-best(2) value 'F ',   "Bestellung             ME21, ME22, ME23
  vorga-kont(2) value 'K ',   "Kontrakt               ME31, ME32, ME33
  vorga-lfpl(2) value 'L ',   "Lieferplan             ME31, ME32, ME33
  vorga-mahn(2) value 'MA',            "Liefermahnung          ME91
  vorga-aufb(2) value 'AB'.            "Bestätigungsmahnung    ME92

* Felder für Feldauswahl (früher FMMEXCOM)
data:       endmaske(210) type c,
            kmaske(140) type c,
            auswahl0 type brefn,
            auswahl1 type brefn,
            auswahl2 type brefn,
            auswahl3 type brefn,
            auswahl4 type brefn,
            auswahl5 type brefn,
            auswahl6 type brefn.

* Sonderbestandskennzeichen
constants:
  sobkz-kdein value 'E',               "Kundeneinzel
  sobkz-prein value 'Q',               "Projekteinzel
  sobkz-lohnb value 'O'.               "Lohnbearbeiterbeistell

* Min-/Maxwerte für Datenelemente
constants:
* offener Rechnungseingangswert / Feldlänge: 13 / Dezimalstellen: 2
  c_max_orewr       like rm06a-orewr   value '99999999999.99',
  c_max_orewr_f     type f             value '99999999999.99',
  c_max_orewr_x(15) type c             value '**************',

  c_max_proz_p(3)   type p decimals 2  value '999.99',      "@80545
  c_max_proz_x(6)   type c             value '******',      "@80545

  c_max_menge       like ekpo-menge  value '9999999999.999',"@83886
  c_max_menge_f     type f           value '9999999999.999',"@83886

  c_max_netwr       like ekpo-netwr  value '99999999999.99',"@83886
  c_max_netwr_f     type f           value '99999999999.99'."@83886


* Distribution Indicator Account assignment
constants:
  c_dist_ind-single   value ' ',       "no multiple = single
  c_dist_ind-quantity value '1',       "quantity distribution
  c_dist_ind-percent  value '2'.       "percentag

* Datendefinitionen für Dienstleistungen
tables: eslh,
        esll,
        ml_esll,
        rm11p.

data  begin of gliederung occurs 50.
        include structure ml_esll.
data  end   of gliederung.

data  begin of leistung occurs 50.
        include structure ml_esll.
data  end   of leistung.

data  return.

*- interne Tabelle für Abrufköpfe -------------------------------------*
data: begin of xekek          occurs 20.
        include structure iekek.
data: end of xekek.

*- interne Tabelle für Abrufköpfe alt----------------------------------*
data: begin of pekek          occurs 20.
        include structure iekek.
data: end of pekek.

*- interne Tabelle für Abrufeinteilungen ------------------------------*
data: begin of xekeh          occurs 20.
        include structure iekeh.
data: end of xekeh.

*- interne Tabelle für Abrufeinteilungen ------------------------------*
data: begin of tekeh          occurs 20.
        include structure iekeh.
data: end of tekeh.

*- Zusatztabelle Abruf nicht vorhanden XEKPO---------------------------*
data: begin of xekpoabr occurs 20,
         mandt like ekpo-mandt,
         ebeln like ekpo-ebeln,
         ebelp like ekpo-ebelp,
      end of xekpoabr.

*-- Daten Hinweis 39234 -----------------------------------------------*
*- Hilfstabelle Einteilungen ------------------------------------------*
data: begin of heket occurs 10.
        include structure eket.
data:       tflag like sy-calld,
      end of heket.

*- Key für HEKET ------------------------------------------------------*
data: begin of heketkey,
         mandt like eket-mandt,
         ebeln like eket-ebeln,
         ebelp like eket-ebelp,
         etenr like eket-etenr,
      end of heketkey.

data: h_subrc like sy-subrc,
      h_tabix like sy-tabix,
      h_field like cdshw-f_old,
      h_eindt like rvdat-extdatum.
data  z type i.

* Defintionen für Formeln

type-pools msfo.

data: variablen type msfo_tab_variablen with header line.

data: formel type msfo_formel.

* Definition für Rechnungsplan

data: tfpltdr like fpltdr occurs 0 with header line.

data: fpltdr like fpltdr.

* Definiton Defaultschema für Dienstleistung

constants: default_kalsm like t683-kalsm value 'MS0000',
           default_kalsm_stamm like t683-kalsm value 'MS0001'.

data: bstyp like ekko-bstyp,
      bsart like ekko-bsart.


data dkomk like komk.

* Defintion für Wartungsplan
tables: rmipm.

data: mpos_tab like mpos occurs 0 with header line,
      zykl_tab like mmpt occurs 0 with header line.

data: print_schedule.

data: begin of d_tkomvd occurs 50.
        include structure komvd.
data: end of d_tkomvd.
data: begin of d_tkomv occurs 50.
        include structure komv.
data: end of d_tkomv.


* Definition Drucktabellen blockweises Lesen

data: leistung_thead like stxh occurs 1 with header line.
data: gliederung_thead like stxh occurs 1 with header line. "HS

data: begin of thead_key,
        mandt    like sy-mandt,
        tdobject like stxh-tdobject,
        tdname   like stxh-tdname,
        tdid     like stxh-tdid,
        tdspras  like stxh-tdspras.
data: end of thead_key.

ranges: r1_tdname for stxh-tdname,
        r2_tdname for stxh-tdname.

data: begin of doktab occurs 0.
        include structure drad.
data  dktxt like drat-dktxt.
data: end of doktab.

*  Additionals Tabelle (CvB/4.0c)
data: l_addis_in_orders type line of addi_buying_print_itab
                                        occurs 0 with header line.
*  Die Additionals-Strukturen müssen bekannt sein
tables: wtad_buying_print_addi,"#EC CI_USAGE_OK[2371631]
   wtad_buying_print_extra_text."#EC CI_USAGE_OK[2371631]
"Added by SPLABAP during code remediation

data: ls_print_data_to_read type lbbil_print_data_to_read.
data: ls_bil_invoice type lbbil_invoice.
data: lf_fm_name            type rs38l_fnam.
data: ls_control_param      type ssfctrlop.
data: ls_composer_param     type ssfcompop.
data: ls_recipient          type swotobjid.
data: ls_sender             type swotobjid.
data: lf_formname           type tdsfname.
data: ls_addr_key           like addr_key,
      dunwitheket           type xfeld.

data: l_zekko                  like ekko,
      l_xpekko                 like pekko,
      l_xekpo                like table of ekpo,
      l_wa_xekpo             like ekpo.

data: l_xekpa like ekpa occurs 0,
      l_wa_xekpa like ekpa.
data: l_xpekpo  like pekpo occurs 0,
      l_wa_xpekpo like pekpo,
      l_xeket   like table of eket with header line,
      l_xekkn  like table of ekkn with header line,
      l_xekek  like table of ekek with header line,
      l_xekeh   like table of ekeh with header line,
      l_xkomk like table of komk with header line,
      l_xtkomv  type komv occurs 0,
      l_wa_xtkomv type komv.

data   ls_ssfcompop  type     ssfcompop.
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
FORM get_plant_address USING    p_werks LIKE t001w-werks
                       CHANGING p_adrnr
                                p_sadr  LIKE sadr.
* parameter P_ADRNR without type since there are several address
* fields with different domains

  DATA: l_ekko LIKE ekko,
        l_address LIKE addr1_val.

  CHECK NOT p_werks IS INITIAL.
  l_ekko-reswk = p_werks.
  l_ekko-bsakz = 'T'.
  CALL FUNCTION 'MM_ADDRESS_GET'
    EXPORTING
      i_ekko    = l_ekko
    IMPORTING
      e_address = l_address
      e_sadr    = p_sadr.
  p_adrnr = l_address-addrnumber.

ENDFORM.                    " GET_PLANT_ADDRESS

*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMER_ADDRESS
*&---------------------------------------------------------------------*
FORM get_customer_address USING    p_kunnr LIKE ekpo-kunnr
                          CHANGING p_adrnr.
* parameter P_ADRNR without type since there are several address
* fields with different domains

  DATA: l_adrnr LIKE kna1-adrnr.

  CHECK NOT p_kunnr IS INITIAL.
  SELECT SINGLE adrnr FROM  kna1 INTO (l_adrnr)
         WHERE  kunnr  = p_kunnr.
  IF sy-subrc EQ 0.
    p_adrnr = l_adrnr.
  ELSE.
    CLEAR p_adrnr.
  ENDIF.

ENDFORM.                    " GET_CUSTOMER_ADDRESS

*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR_ADDRESS
*&---------------------------------------------------------------------*
FORM get_vendor_address USING    p_emlif LIKE lfa1-lifnr
                        CHANGING p_adrnr.
* parameter P_ADRNR without type since there are several address
* fields with different domains

  DATA: l_lfa1 LIKE lfa1.

  CHECK NOT p_emlif IS INITIAL.
  CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_00'
    EXPORTING
      i_lfa1_lifnr     = p_emlif
      i_data           = 'X'
      i_partner        = ' '
    IMPORTING
      a_lfa1           = l_lfa1
    EXCEPTIONS
      vendor_not_found = 1.
  IF sy-subrc EQ 0.
    p_adrnr = l_lfa1-adrnr.
  ELSE.
    CLEAR p_adrnr.
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
FORM get_addr_key
     USING    l_xekko LIKE ekko
     CHANGING l_addr_key like addr_key.

  DATA: l_lfa1 LIKE lfa1.

  IF l_xekko-lifnr NE space.
    CALL FUNCTION 'MM_ADDRESS_GET'
      EXPORTING
        i_ekko = l_xekko
      IMPORTING
        e_sadr = sadr
      EXCEPTIONS
        OTHERS = 1.
    MOVE-CORRESPONDING sadr TO l_lfa1.
    IF sy-subrc = 0.
      MOVE l_lfa1-adrnr TO l_addr_key-addrnumber.
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
FORM protocol_update_i.
  CHECK xscreen = space.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = syst-msgid
      msg_nr    = syst-msgno
      msg_ty    = syst-msgty
      msg_v1    = syst-msgv1
      msg_v2    = syst-msgv2
      msg_v3    = syst-msgv3
      msg_v4    = syst-msgv4
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
FORM add_smfrm_prot.
  DATA: lt_errortab             TYPE tsferror.
  DATA: lf_msgnr                TYPE sy-msgno.
  DATA:  l_s_log          TYPE bal_s_log,
         p_loghandle      TYPE balloghndl,
       l_s_msg          TYPE bal_s_msg.
  FIELD-SYMBOLS: <fs_errortab>  TYPE LINE OF tsferror.
* get smart form protocoll
  CALL FUNCTION 'SSF_READ_ERRORS'
    IMPORTING
      errortab = lt_errortab.
  SORT lt_errortab.
* delete adjacent duplicates from lt_errortab comparing errnumber.
* add smartform protocoll to nast protocoll
  LOOP AT lt_errortab ASSIGNING <fs_errortab>.
    CLEAR lf_msgnr.
    lf_msgnr = <fs_errortab>-errnumber.
    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
      EXPORTING
        msg_arbgb = <fs_errortab>-msgid
        msg_nr    = lf_msgnr
        msg_ty    = <fs_errortab>-msgty
        msg_v1    = <fs_errortab>-msgv1
        msg_v2    = <fs_errortab>-msgv2
        msg_v3    = <fs_errortab>-msgv3
        msg_v4    = <fs_errortab>-msgv4
      EXCEPTIONS
        OTHERS    = 1.
  ENDLOOP.

* open the application log
  l_s_log-extnumber    = sy-uname.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = l_s_log
    IMPORTING
      e_log_handle = p_loghandle
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
  ENDIF.

  LOOP AT lt_errortab ASSIGNING <fs_errortab>.
    MOVE-CORRESPONDING <fs_errortab> TO l_s_msg.
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = p_loghandle
        i_s_msg      = l_s_msg
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
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
  DATA lv_debug.
  IF NOT lv_debug IS INITIAL.
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

FORM entry_neu USING ent_retco ent_screen.
*  break-point.

  DATA: l_druvo LIKE t166k-druvo.
  DATA: ls_xfz  TYPE char1.

  xscreen = ent_screen.

  CLEAR ent_retco.
  IF nast-aende EQ space.
    l_druvo = '1'.
  ELSE.
    l_druvo = '2'.
  ENDIF.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF xscreen EQ 'X' AND ( nast-tdarmod EQ '2' OR nast-tdarmod EQ '3').
    nast-tdarmod = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM processing_po USING nast ls_xfz
                     CHANGING ent_retco ent_screen l_druvo.

ENDFORM.                    "entry_neu

*&--------------------------------------------------------------------*
*&      Form  ENTRY_MAHN
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM entry_mahn USING ent_retco ent_screen.

  DATA: l_druvo LIKE t166k-druvo.
  DATA: ls_xfz  TYPE char1.

  xscreen = ent_screen.

  l_druvo = '3'.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF xscreen EQ 'X' AND ( nast-tdarmod EQ '2' OR nast-tdarmod EQ '3').
    nast-tdarmod = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM processing_po USING nast ls_xfz
                     CHANGING ent_retco ent_screen l_druvo.

ENDFORM.                    "entry_mahn
*&--------------------------------------------------------------------*
*&      Form  ENTRY_AUFB
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*

FORM entry_aufb USING ent_retco ent_screen.

  DATA: l_druvo LIKE t166k-druvo.
  DATA: ls_xfz  TYPE char1.

  xscreen = ent_screen.

  l_druvo = '7'.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF xscreen EQ 'X' AND ( nast-tdarmod EQ '2' OR nast-tdarmod EQ '3').
    nast-tdarmod = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM processing_po USING nast ls_xfz
                     CHANGING ent_retco ent_screen l_druvo.

ENDFORM.                    "entry_aufb

*&--------------------------------------------------------------------*
*&      Form  ENTRY_ABSA
*&--------------------------------------------------------------------*
* RFQ: Quotation Rejection
* Angebotsabsage
*----------------------------------------------------------------------*
FORM entry_absa USING ent_retco ent_screen.

*********************************************
  DATA: l_druvo LIKE t166k-druvo.
  DATA: ls_xfz  TYPE char1.

  xscreen = ent_screen.
  l_druvo = '4'.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF xscreen EQ 'X' AND ( nast-tdarmod EQ '2' OR nast-tdarmod EQ '3').
    nast-tdarmod = '1'.
  ENDIF.
*<<< End of Insertion <<<
  PERFORM processing_po
          USING     nast
                    ls_xfz
          CHANGING  ent_retco
                    ent_screen
                    l_druvo.

ENDFORM.                    "entry_absa

*&--------------------------------------------------------------------*
*&      Form  ENTRY_LPET
*&--------------------------------------------------------------------*
* Delivery Schedule: Traditional DLS type, without forecast or JIT
* Lieferplaneinteilung
*----------------------------------------------------------------------*
FORM entry_lpet USING ent_retco ent_screen.

  DATA: l_druvo LIKE t166k-druvo.
  DATA: ls_xfz  TYPE char1.

  CLEAR ent_retco.
  CLEAR ls_xfz.

  IF nast-aende EQ space.
    l_druvo = '5'.
  ELSE.
    l_druvo = '8'.
  ENDIF.

  xscreen = ent_screen.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF xscreen EQ 'X' AND ( nast-tdarmod EQ '2' OR nast-tdarmod EQ '3').
    nast-tdarmod = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM processing_po
          USING     nast
                    ls_xfz
          CHANGING  ent_retco
                    ent_screen
                    l_druvo.

ENDFORM.                    "entry_lpet

*&--------------------------------------------------------------------*
*&      Form  ENTRY_LPMA
*&--------------------------------------------------------------------*
* Dlivery Schedule: Urging/Reminder
* Mahnung
FORM entry_lpma USING ent_retco ent_screen.

  DATA: l_druvo LIKE t166k-druvo.
  DATA: ls_xfz  TYPE char1.

  CLEAR ent_retco.
  l_druvo = '6'.

  xscreen = ent_screen.

*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF xscreen EQ 'X' AND ( nast-tdarmod EQ '2' OR nast-tdarmod EQ '3').
    nast-tdarmod = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM processing_po
          USING     nast
                    ls_xfz
          CHANGING  ent_retco
                    ent_screen
                    l_druvo.

ENDFORM.                    "entry_lpma
*&--------------------------------------------------------------------*
*&      Form  ENTRY_LPHE_CD
*&--------------------------------------------------------------------*
* Delivery Schedule: Forecast delivery without accumnlation value
FORM entry_lphe_cd USING ent_retco ent_screen.

  DATA: l_druvo LIKE t166k-druvo.
  DATA: ls_xfz  TYPE char1.

  CLEAR ent_retco.
  l_druvo = '9'.

  xscreen = ent_screen.
*>>> Start of Insertion >>> OSS 754573 (Liu Ke)
  IF xscreen EQ 'X' AND ( nast-tdarmod EQ '2' OR nast-tdarmod EQ '3').
    nast-tdarmod = '1'.
  ENDIF.
*<<< End of Insertion <<<

  PERFORM processing_po
          USING     nast
                    ls_xfz
          CHANGING  ent_retco
                    ent_screen
                    l_druvo.


ENDFORM.                    "entry_lphe_cd

*&--------------------------------------------------------------------*
*&      Form  ENTRY_LPJE_CD
*&--------------------------------------------------------------------*
* Delivery Schedule: JIT delivery without accumnlation value
FORM entry_lpje_cd USING ent_retco ent_screen.

  DATA: l_druvo LIKE t166k-druvo.
  DATA: ls_xfz  TYPE char1.

  CLEAR ent_retco.
  l_druvo = 'A'.

** Get print preview indicator from import parameter
  xscreen = ent_screen.

** if it is print preview, Print Archiving mode should be set to
** "Print Only"
  IF xscreen EQ 'X' AND ( nast-tdarmod EQ '2' OR nast-tdarmod EQ '3').
    nast-tdarmod = '1'.
  ENDIF.

  PERFORM processing_po
          USING     nast
                    ls_xfz
          CHANGING  ent_retco
                    ent_screen
                    l_druvo.

ENDFORM.                    "entry_lpje_cd

*&--------------------------------------------------------------------*
*&      Form  PROCESSING_PO
*&--------------------------------------------------------------------*
* Purchase Order Processing Subroutine
FORM processing_po
                USING iv_nast TYPE nast
                      iv_xfz  TYPE char1
                CHANGING ent_retco
                         ent_screen
                         iv_druvo TYPE t166k-druvo.
  DATA: ls_print_data_to_read TYPE lbbil_print_data_to_read.
  DATA: ls_bil_invoice TYPE lbbil_invoice.
  DATA: lf_fm_name            TYPE rs38l_fnam.
  DATA: ls_control_param      TYPE ssfctrlop.
  DATA: ls_composer_param     TYPE ssfcompop.
  DATA: ls_recipient          TYPE swotobjid.
  DATA: ls_sender             TYPE swotobjid.
  DATA: lf_formname           TYPE tdsfname.
  DATA: ls_addr_key           LIKE addr_key.
  data: ls_job_info           type ssfcrescl.
  data: l_spoolid             type rspoid.

  DATA: l_from_memory,
        l_doc   TYPE meein_purchase_doc_print,
        l_nast  LIKE nast.



  CALL FUNCTION 'ME_READ_PO_FOR_PRINTING'
    EXPORTING
      ix_nast        = iv_nast
      ix_screen      = ent_screen
    IMPORTING
      ex_retco       = ent_retco
      ex_nast        = l_nast
      doc            = l_doc
    CHANGING
      cx_druvo       = iv_druvo
      cx_from_memory = l_from_memory.
  CHECK ent_retco EQ 0.

  l_nast-aende = iv_nast-aende.

***********************
  DATA lv_nast LIKE nast.
  IF iv_druvo EQ aend OR iv_druvo EQ lpae.
    SELECT datvr uhrvr INTO (lv_nast-datvr, lv_nast-uhrvr) FROM nast
             WHERE kappl EQ iv_nast-kappl
               AND kschl EQ iv_nast-kschl
               AND objky EQ iv_nast-objky
               AND vstat EQ '1'
       ORDER BY datvr DESCENDING uhrvr DESCENDING.
      EXIT.
    ENDSELECT.

* If no NEU-Message could be found above, we read any NEU-message of
* that purchase document and use the latest timestamp of these messages
* for the fields *nast-datvr and *nast-uhrvr.
    IF sy-subrc NE 0.                                   "Begin of 549924
      DATA: neu_messagetypes LIKE t161m OCCURS 0 WITH HEADER LINE.
      DATA: BEGIN OF time_tab OCCURS 0,
              datvr LIKE nast-datvr,
              uhrvr LIKE nast-uhrvr,
            END OF time_tab.

* Read all messagetypes that are allowed for NEU-Messages
      SELECT * FROM t161m INTO TABLE neu_messagetypes
        WHERE kvewe EQ 'B'
        AND   kappl EQ iv_nast-kappl
        AND   kschl NE iv_nast-kschl
        AND   druvo EQ neu.
      IF sy-subrc EQ 0.
        LOOP AT neu_messagetypes.
          SELECT datvr uhrvr INTO time_tab FROM nast
             WHERE kappl EQ iv_nast-kappl
               AND kschl EQ neu_messagetypes-kschl
               AND objky EQ iv_nast-objky
               AND vstat EQ '1'
             ORDER BY datvr DESCENDING uhrvr DESCENDING.
            EXIT.
          ENDSELECT.
          IF sy-subrc EQ 0.
            APPEND time_tab.
          ENDIF.
        ENDLOOP.
        SORT time_tab BY datvr DESCENDING uhrvr DESCENDING.
        READ TABLE time_tab INDEX 1.
        IF sy-subrc EQ 0.
          lv_nast-datvr = time_tab-datvr.
          lv_nast-uhrvr = time_tab-uhrvr.
        ENDIF.
      ENDIF.
    ENDIF.
    l_nast-datvr = lv_nast-datvr.
    l_nast-uhrvr = lv_nast-uhrvr.
  ENDIF.
***********************
  IF iv_nast-adrnr IS INITIAL.
    PERFORM get_addr_key
      USING l_doc-xekko
      CHANGING ls_addr_key.
  ELSE.
    ls_addr_key = iv_nast-adrnr.
  ENDIF.

* Fill up pricing condition table if calling from ME9F
  IF l_doc-xtkomv IS INITIAL.
     DATA : IT_PRCD_ELEMENTS TYPE TABLE OF PRCD_ELEMENTS.
     SELECT * INTO TABLE IT_PRCD_ELEMENTS FROM PRCD_ELEMENTS " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
                                     WHERE knumv = l_doc-xekko-knumv.
     MOVE-CORRESPONDING IT_PRCD_ELEMENTS TO l_doc-xtkomv.
  ENDIF.

*=======================================================================================================================================
*          Below Codes Added by shanmugam
*=======================================================================================================================================
*     DATA :  IT_PO   TYPE TABLE OF ZMM_PO_STRUC WITH  HEADER LINE.
*     DATA :  WA_PO   TYPE  ZMM_PO_STRUC .
     DATA :  IT_EKPO TYPE TABLE OF EKPO  WITH HEADER LINE.
     DATA :  WA_KONP TYPE KONP.
     DATA :  GKWERT  TYPE KWERT.
     DATA :  DKWERT  TYPE KWERT.
     DATA :  B_DUTY  TYPE KWERT.
     DATA :  B_DUTY1 TYPE KWERT.
     DATA :  B_DUTY2 TYPE KWERT.
     DATA :  B_DUTY3 TYPE KWERT.
     DATA :  B_DUTY4 TYPE KWERT.
     DATA :  B_DUTY5 TYPE KWERT.
     DATA :  B_DUTY6 TYPE KWERT.
     DATA :  BEXDUT  TYPE KWERT.
     DATA :  gtotal  TYPE KWERT.
     DATA :  GPERN   TYPE KWERT.
     DATA :  GCALC   TYPE KWERT.
     DATA :  VATCST  TYPE KWERT.
     DATA :  VKWERT  TYPE KWERT.
     DATA :  GNETWR  TYPE NETWR.
     DATA :  GKSCHL  TYPE KSCHL.
     DATA :  WA_A363 TYPE A363.
     DATA :  WA_A359 TYPE A359.
     DATA :  WA_MAKT TYPE MAKT.

     IT_EKPO[] = l_doc-xekpo[].


     IF L_DOC-XEKKO-BSART = 'ZIM'.
       CLEAR : GTOTAL.
        LOOP AT IT_EKPO.
         WA_PO-UEBPO  =  IT_EKPO-EBELP.
         WA_PO-MATNR  =  IT_EKPO-MATNR.
         CLEAR :  WA_MAKT.
         "Commented by SPLABAP during code remediation
*         SELECT SINGLE * FROM MAKT INTO WA_MAKT WHERE  MATNR =  IT_EKPO-MATNR.
         "Added by SPLABAP during code remediation
         SELECT * FROM MAKT  INTO WA_MAKT  UP TO 1 ROWS
           WHERE  MATNR =  IT_EKPO-MATNR
           ORDER BY PRIMARY KEY.
           ENDSELECT.
         IF SY-SUBRC = 0.
          WA_PO-MAKTX = WA_MAKT-MAKTX.
          ELSEIF WA_PO-MATNR IS INITIAL.
            WA_PO-MAKTX = IT_EKPO-TXZ01.
         ENDIF.
         WA_PO-MENGE  =  IT_EKPO-KTMNG.   "MENGE.
         WA_PO-MEINS  =  IT_EKPO-MEINS.
         WA_PO-NETPR  =  IT_EKPO-NETPR.
         WA_PO-CTOT  = IT_EKPO-ZWERT.  "NETWR.

         GTOTAL =  GTOTAL  +  IT_EKPO-NETWR.
         WA_PO-TTAMT =  GTOTAL.

          APPEND WA_PO TO IT_PO.
          CLEAR : WA_PO.
         ENDLOOP.


         ELSE.   "IF L_DOC-XEKKO-BSART = 'ZSR'.

          DATA :  G_EXCISE1 TYPE KWERT.
          DATA :  G_EXCISE2 TYPE KWERT.
          DATA :  G_EXCISE3 TYPE KWERT.
          DATA :  G_EXCISE4 TYPE KWERT.
          DATA :  G_EXCISE5 TYPE KWERT.
          DATA :  G_EXCISE6 TYPE KWERT.

         LOOP AT IT_EKPO.
         WA_PO-UEBPO  =  IT_EKPO-EBELP.
         WA_PO-MATNR  =  IT_EKPO-MATNR.
         CLEAR :  WA_MAKT.
         "Commented by SPLABAP during code remediation
*         SELECT SINGLE * FROM MAKT INTO WA_MAKT WHERE  MATNR =  IT_EKPO-MATNR.
         "Added by SPLABAP during code remediation
         SELECT * FROM MAKT INTO WA_MAKT UP TO 1 ROWS WHERE  MATNR =  IT_EKPO-MATNR
           ORDER BY PRIMARY KEY.
           ENDSELECT.
         IF SY-SUBRC = 0.
          WA_PO-MAKTX = WA_MAKT-MAKTX.
          ELSEIF WA_PO-MATNR IS INITIAL.
            WA_PO-MAKTX = IT_EKPO-TXZ01.
         ENDIF.
         WA_PO-MENGE  =  IT_EKPO-KTMNG. "MENGE.
         WA_PO-MEINS  =  IT_EKPO-MEINS.
         WA_PO-NETPR  =  IT_EKPO-NETPR.
*=======================================================================================================================================
*        EXCISE DUTY
*=======================================================================================================================================
         WA_PO-CTOT  = IT_EKPO-ZWERT.   "NETWR.
         CLEAR : IT_EKPO-NETWR.
         IT_EKPO-NETWR =  IT_EKPO-ZWERT.
         CLEAR WA_KONP.
         CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT, B_DUTY, G_EXCISE1, G_EXCISE2, G_EXCISE3,   G_EXCISE4,  G_EXCISE5,  G_EXCISE6 .

         CLEAR WA_KONP.
         CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
         "Commented by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JMOP' .
         "Added by SPLABAP during code remediation
         SELECT  * INTO WA_A363 FROM A363 up to 1 ROWS
           WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR
           AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JMOP'
           ORDER BY PRIMARY KEY.
           ENDSELECT.
         SELECT * INTO WA_A363 FROM A363 UP TO 1 ROWS
            WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JMOP'
           ORDER BY PRIMARY KEY .
           ENDSELECT.
         IF SY-SUBRC = 0.
           "commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
             WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT =  WA_KONP-KBETR .
           ENDIF.
         ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
         DKWERT =  GKWERT / 10.

         G_EXCISE1 = ( ( IT_EKPO-NETWR / 100 ) * DKWERT ) / 100.


         CLEAR WA_KONP.
         CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
         "Commened by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JEC1' .
         "Added by SPLABAP during code remediation
         SELECT  * INTO WA_A363 FROM A363 UP TO 1 ROWS
           WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JEC1'
           ORDER BY PRIMARY KEY .
           ENDSELECT.
         IF SY-SUBRC = 0.
           "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT   * INTO WA_KONP UP TO 1 ROWS FROM KONP WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT =  WA_KONP-KBETR .
           ENDIF.
         ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
         DKWERT =  GKWERT / 10.

*         G_EXCISE2 =  IT_EKPO-NETWR / 100 * DKWERT.
          G_EXCISE2 =  ( (  G_EXCISE1 / 100 ) * DKWERT ) / 100.

          CLEAR WA_KONP.
         CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
         "commenteded by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A363 FROM A363
         "Added by SPLABAP during code remediation
         SELECT * INTO WA_A363 FROM A363 UP TO 1 ROWS
            WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR
            AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JSEP'
           ORDER BY PRIMARY KEY.
           ENDSELECT.
         IF SY-SUBRC = 0.
           "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT   * INTO WA_KONP UP TO 1 ROWS FROM KONP WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT =  WA_KONP-KBETR .
           ENDIF.
         ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
         DKWERT =  GKWERT / 10.

*         G_EXCISE3 =  IT_EKPO-NETWR / 100 * DKWERT.

           G_EXCISE3 =  ( (  G_EXCISE1 / 100 ) ) * DKWERT / 100.

*==============================================================================================================================

             CLEAR WA_KONP.
         CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
         "Commented by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JMIP' .
         "Added by SPLABAP during code remediation
         SELECT  * INTO WA_A363 UP TO 1 ROWS FROM A363
            WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JMIP'
           ORDER BY PRIMARY KEY .
           ENDSELECT."Added by SPLABAP during code remediation
         IF SY-SUBRC = 0.
           "cOMMENTed by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT * INTO WA_KONP FROM KONP UP TO 1 ROWS
             WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT =  WA_KONP-KBETR .
           ENDIF.
         ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
         DKWERT =  GKWERT / 10.
         G_EXCISE4 =  ( ( IT_EKPO-NETWR / 100 ) * DKWERT ) / 100.

          CLEAR WA_KONP.
         CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
         "Commented by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JEC2' .
        "Added by SPLABAP during code remediation
         SELECT  * INTO WA_A363 FROM A363
           WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JEC2'
           ORDER BY PRIMARY KEY.
           ENDSELECT.
         IF SY-SUBRC = 0.
           "cOMMENTed by SPLABAP during code remediation
* SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
              WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT =  WA_KONP-KBETR .
           ENDIF.
         ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
         DKWERT =  GKWERT / 10.

*         G_EXCISE5 =  IT_EKPO-NETWR / 100 * DKWERT.
        G_EXCISE5 =    ( ( G_EXCISE4 / 100 ) * DKWERT ) / 100.
               CLEAR WA_KONP.
         CLEAR : WA_A363, GKWERT, DKWERT, BEXDUT.
         "Commented by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A363 FROM A363
         "Added by SPLABAP during code remediation
         SELECT * INTO WA_A363 FROM A363 UP TO 1 ROWS
           WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR
           AND LIFNR = L_DOC-XEKKO-LIFNR AND KSCHL = 'JESI'
           ORDER BY PRIMARY KEY .
           ENDSELECT.
         IF SY-SUBRC = 0.
           "cOMMENTd by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
             WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT =  WA_KONP-KBETR .
           ENDIF.
         ENDIF.
*         WA_PO-CTOT  = IT_EKPO-NETWR.
         DKWERT =  GKWERT / 10.

*         G_EXCISE6 =  IT_EKPO-NETWR / 100 * DKWERT.
           G_EXCISE6 =    ( ( G_EXCISE4 / 100 )  * DKWERT ) / 100 .



          B_DUTY =   G_EXCISE1 +  G_EXCISE2 +  G_EXCISE3 +  G_EXCISE4 +  G_EXCISE5 +  G_EXCISE6.

          WA_PO-KWERT = B_DUTY.


*=======================================================================================================================================
*        SERVICE TAX
*=======================================================================================================================================

         CLEAR : WA_KONP,GKWERT, DKWERT , B_DUTY1, B_DUTY2,  B_DUTY3,  B_DUTY4,  B_DUTY5, B_DUTY6.
         CLEAR WA_A359.
         "Commented by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A359 FROM A359  WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSVD'.
         "Added by SPLABAP during code remediation
         SELECT  * INTO WA_A359 FROM A359 UP TO 1 ROWS WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSVD'
           ORDER BY PRIMARY KEY.
           ENDSELECT.
           IF SY-SUBRC = 0.
             "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP  FROM KONP WHERE KNUMH =  WA_A359-KNUMH.
             "Added by SPLABAP during code remediation
           SELECT   * INTO WA_KONP UP TO 1 ROWS FROM KONP WHERE KNUMH =  WA_A359-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT = GKWERT + WA_KONP-KBETR.
           ENDIF.
         ENDIF.
         DKWERT =  GKWERT / 10.
         B_DUTY1 =   ( ( WA_PO-CTOT / 100  ) * DKWERT ) / 100.

         CLEAR : WA_KONP ,  GKWERT,DKWERT.
         CLEAR WA_A359.
         "Commented by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JEC3'.
         "Added by SPLABAP during code remediation
         SELECT * INTO WA_A359 FROM A359 UP TO 1 ROWS
           WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JEC3'
           ORDER BY PRIMARY KEY.
           ENDSELECT.
         IF SY-SUBRC = 0.
           "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS
             WHERE KNUMH =  WA_A359-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT = GKWERT + WA_KONP-KBETR.
           ENDIF.
         ENDIF.
         DKWERT =  GKWERT / 10.
         B_DUTY2 =  ( ( B_DUTY1 / 100 )  * DKWERT ) / 100.

         CLEAR : WA_KONP ,  GKWERT,DKWERT ,  B_DUTY3.
         CLEAR WA_A359.
         "Commented by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSE1'.
         "Added by SPLABAP during code remediation
         SELECT  * INTO WA_A359 FROM A359 up to 1 ROWS
  WHERE WERKS = IT_EKPO-WERKS
           AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSE1'
           ORDER BY PRIMARY KEY.ENDSELECT.
         IF SY-SUBRC = 0.
           "Commentedd by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS
              WHERE KNUMH =  WA_A359-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT = GKWERT + WA_KONP-KBETR.
           ENDIF.
         ENDIF.
         DKWERT =  GKWERT / 10.
         B_DUTY3 =  ( ( B_DUTY1 / 100 ) * DKWERT ) / 100.


         CLEAR : WA_KONP ,  GKWERT,DKWERT ,  B_DUTY4, B_DUTY5.
          CLEAR WA_A359.
          "Commented by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSV2'.
          "Added by SPLABAP during code remediation
         SELECT  * INTO WA_A359 FROM A359 UP TO 1 ROWS  WHERE WERKS = IT_EKPO-WERKS
            AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSV2'
           ORDER BY PRIMARY KEY.
           ENDSELECT.
         IF SY-SUBRC = 0.
           "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS WHERE KNUMH =  WA_A359-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT = GKWERT + WA_KONP-KBETR.
           ENDIF.
         ENDIF.
         DKWERT =  GKWERT / 10.
         B_DUTY4 =   ( ( WA_PO-CTOT / 100  ) * DKWERT ) / 100.


         CLEAR : WA_KONP ,  GKWERT,DKWERT .
         CLEAR WA_A359.
         "  Commeted by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JEC4'.
         "Added by SPLABAP during code remediation
         SELECT * INTO WA_A359 FROM A359 UP TO 1 ROWS
           WHERE WERKS = IT_EKPO-WERKS AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JEC4'
           ORDER BY PRIMARY KEY.
           ENDSELECT.
         IF SY-SUBRC = 0.
           "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS WHERE KNUMH =  WA_A359-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT = GKWERT + WA_KONP-KBETR.
           ENDIF.
         ENDIF.
         DKWERT =  GKWERT / 10.
         B_DUTY5 =  ( ( B_DUTY4 / 100 ) * DKWERT ) / 100.


         CLEAR : WA_KONP ,  GKWERT,DKWERT,  B_DUTY6 .
       CLEAR WA_A359.
       "Commented by SPLABAP during code remediation
*         SELECT SINGLE * INTO WA_A359 FROM A359 WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSE2'.
       "Added by SPLABAP during code remediation
         SELECT * INTO WA_A359 UP TO 1 ROWS FROM A359
           WHERE WERKS = IT_EKPO-WERKS  AND MATKL = IT_EKPO-MATKL AND KSCHL = 'JSE2'
           ORDER BY PRIMARY KEY.
           ENDSELECT.
         IF SY-SUBRC = 0.
           "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A359-KNUMH.
           "Added by SPLABAP during code remediation
           SELECT   * INTO WA_KONP UP TO 1 ROWS FROM KONP WHERE KNUMH =  WA_A359-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           GKWERT = GKWERT + WA_KONP-KBETR.
           ENDIF.
         ENDIF.
         DKWERT =  GKWERT / 10.
         B_DUTY6 =   ( ( B_DUTY4 / 100 ) * DKWERT ) / 100.


         BEXDUT =   B_DUTY1 + B_DUTY2 +  B_DUTY3 +  B_DUTY4 +  B_DUTY5 +  B_DUTY6.

         WA_PO-SERVICE = BEXDUT.


*=======================================================================================================================================
*         VAT/CST  Calculation
*=======================================================================================================================================



           CLEAR  : WA_A363, VKWERT.
"Commeted by SPLABAP during code remediation
*           SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVRD'.
           "Added by SPLABAP during code remediation
           SELECT * INTO WA_A363 FROM A363
             WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVRD'
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
             "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
             "Added by SPLABAP during code remediation
           SELECT  * INTO WA_KONP UP TO 1 ROWS FROM KONP WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           VKWERT =  VKWERT + WA_KONP-KBETR .
           WA_PO-KSCHL =  WA_KONP-KSCHL.
           ENDIF.
           ENDIF.
           CLEAR  : WA_A363.
*           SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND  MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVRN'.
           SELECT * INTO WA_A363 FROM A363 UP TO 1 ROWS
              WHERE WERKS = IT_EKPO-WERKS AND  MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVRN'
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
             "Commrnted by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
             "Added by SPLABAP during code remediation
           SELECT   * INTO WA_KONP  UP TO 1 ROWS
             FROM KONP WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           VKWERT =  VKWERT + WA_KONP-KBETR .
           WA_PO-KSCHL =  WA_KONP-KSCHL.
           ENDIF.
           ENDIF.
          CLEAR  : WA_A363.
          "Commented by SPLABAP during code remediation
*           SELECT SINGLE * INTO WA_A363 FROM A363
          "Added by SPLABAP during code remediation
           SELECT * INTO WA_A363 FROM A363
              WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR
              AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVCD'
             ORDER BY   PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
             "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
             "Added by SPLABAP during code remediation
           SELECT  * INTO WA_KONP FROM KONP UP TO 1 ROWS WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           VKWERT =  VKWERT + WA_KONP-KBETR .
           WA_PO-KSCHL =  WA_KONP-KSCHL.
           ENDIF.
           ENDIF.
         CLEAR  : WA_A363.
         "Commented by SPLABAP during code remediation
*           SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVCN'.
          "Added by SPLABAP during code remediation
           SELECT  * INTO WA_A363 FROM A363 UP TO 1 ROWS
             WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVCN'
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
             "Commented by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
             "Added by SPLABAP during code remediation
           SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           VKWERT =  VKWERT + WA_KONP-KBETR .
           WA_PO-KSCHL =  WA_KONP-KSCHL.
           ENDIF.
           ENDIF.
        CLEAR  : WA_A363.
        "Commented by SPLABAP during code remediation
*           SELECT SINGLE * INTO WA_A363 FROM A363 WHERE WERKS = IT_EKPO-WERKS AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVCS'.
          "Added by SPLABAP during code remediation
           SELECT  * INTO WA_A363 FROM A363 UP TO 1 ROWS WHERE WERKS = IT_EKPO-WERKS
             AND MATNR = IT_EKPO-MATNR AND LIFNR = L_DOC-XEKKO-LIFNR  AND KSCHL = 'JVCS'
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
             "Commentd by SPLABAP during code remediation
*           SELECT  SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH =  WA_A363-KNUMH.
           "Added by SPLABAP during code remediation
  SELECT   * INTO WA_KONP FROM KONP UP TO 1 ROWS
     WHERE KNUMH =  WA_A363-KNUMH
             ORDER BY PRIMARY KEY.
             ENDSELECT.
           IF SY-SUBRC = 0.
           VKWERT =  VKWERT + WA_KONP-KBETR .
           WA_PO-KSCHL =  WA_KONP-KSCHL.
           ENDIF.
           ENDIF.
          CLEAR : GPERN , GCALC, GTOTAL.
          GPERN = VKWERT / 10.
          GCALC =   ( ( WA_PO-CTOT + WA_PO-KWERT ) / 100 ).
          WA_PO-KBETR = ( GCALC * GPERN ) / 100.
          GTOTAL =  GTOTAL  + WA_PO-KWERT  +  WA_PO-KBETR +  IT_EKPO-NETWR + WA_PO-SERVICE.
          WA_PO-TTAMT =  GTOTAL.



          DATA: it_taxcom type taxcom,
       it_komv type komv OCCURS 20,
       wa_komv type komv,
wa_ekko1 type ekko,
it_ekpo1 type table of ekpo ,
wa_ekpo1 type ekpo.



    it_taxcom-bukrs = it_ekpo-bukrs.""'1000'."ekpo-bukrs. 1000

    it_taxcom-budat = l_doc-xekko-aedat.""'20170609'."ekko-aedat. 20170609

    it_taxcom-bldat = l_doc-xekko-bedat.""'20170609'. "" ekko-bedat. 20170609

    it_taxcom-waers = l_doc-xekko-waers.""'INR'."'ekko-waers. INR

    it_taxcom-hwaer = l_doc-xekko-waers.""'INR'."ekko-waers. INR

    it_taxcom-kposn = IT_EKPO-ebelp.""'10'.""ekpo-ebelp. 10

    it_taxcom-mwskz = it_ekpo-mwskz..""'Z2'."'Z1'.""ekpo-mwskz. Z1


     it_taxcom-wrbtr = ( it_EKPO-NETPR / it_ekpo-PEINH ) * it_EKPO-KTMNG."#EC CI_FLDEXT_OK[2610650]
     "Added by SPLABAP during code remediation
      ""'1250'.""'500'."EKPO-NETPR * EKPO-MENGE. = 50*10

    it_taxcom-xmwst = 'X'.

    it_taxcom-txjcd = it_ekpo-txjcd.""' '."ekpo-txjcd. = ?

    it_taxcom-lifnr = l_doc-xekko-lifnr.."''0010000000'."ekko-lifnr. = 0010000000

    it_taxcom-ekorg = l_doc-xekko-ekorg.""'1000'."ekko-ekorg. = 1000

    it_taxcom-matnr = it_ekpo-matnr."" '000000000010000000'.""ekpo-matnr. = 000000000010000000

    it_taxcom-werks = it_ekpo-werks.""'1000'."ekpo-werks. = 1000

    it_taxcom-matkl = it_ekpo-matkl.""'RMADD'.""ekpo-matkl. = RMADD

    it_taxcom-meins = it_ekpo-meins.""'KG'." ekpo-meins. = KG

    it_taxcom-mglme = it_ekpo-menge.""'25'."'10' ." ekpo-menge. = 10

    it_taxcom-mtart = it_ekpo-mtart.""'ROH'." ekpo-mtart. = ROH

    it_taxcom-land1 = l_doc-xekko-lands."" 'IN'.""ekko-lands. = IN

*BREAK-POINT.

CALL FUNCTION 'CALCULATE_TAX_ITEM'
  EXPORTING
*   ANZAHLUNG                 = ' '
*   DIALOG                    = ' '
*   DISPLAY_ONLY              = ' '
*   INKLUSIVE                 = ' '
*   I_ANWTYP                  = ' '
*   I_DMBTR                   = '0'
*   I_MWSTS                   = '0'
    I_TAXCOM                  = it_taxcom
*   PRUEFEN                   = ' '
*   RESET                     = ' '
*   I_KTOSL                   = ' '
* IMPORTING
*   E_NAVFW                   =
*   E_TAXCOM                  =
*   E_XSTVR                   =
*   NAV_ANTEIL                =
 TABLES
   T_XKOMV                   =  it_komv
* EXCEPTIONS
*   MWSKZ_NOT_DEFINED         = 1
*   MWSKZ_NOT_FOUND           = 2
*   MWSKZ_NOT_VALID           = 3
*   STEUERBETRAG_FALSCH       = 4
*   COUNTRY_NOT_FOUND         = 5
*   TXJCD_NOT_VALID           = 6
*   OTHERS                    = 7
          .
*endselect.
*
Data GCALC1 type KWERT.
DAta perc_CGST type kbetr.
DAta perc_sgst type kbetr.
DAta perc_IGST type kbetr.
clear GCALC1.
clear B_DUTY1.
clear G_EXCISE1.
clear: perc_CGST, perc_sgst, perc_igst.


loop at it_komv into wa_komv .
   if wa_komv-KSCHL = 'JICG'.
     G_EXCISE1 = wa_komv-KWERT.
     perc_CGST = wa_komv-KBETR / 10.
   elseif wa_komv-KSCHL = 'JISG'.
     B_DUTY1 = wa_komv-kwert.
     perc_SGST = wa_komv-KBETR / 10.
   elseif wa_komv-KSCHL = 'JIIG'.
     GCALC1 = wa_komv-kwert.
     perc_IGST = wa_komv-KBETR / 10.
  endif.
endloop.

clear B_DUTY.""*************** CGST
clear BEXDUT."""""""""""""""""SGST
clear GCALC. """""""""""""""""IGST
  if g_excise1 <> 0.   """"" for CGST
    B_DUTY =   G_EXCISE1.
    WA_PO-TAXPERS_cgst = perc_CGST.
     clear g_excise1.
     clear perc_CGST.
  endif.

if B_DUTY1 <> 0.       """"""""for SGST
  BEXDUT =   B_DUTY1.
  WA_PO-TAXPERS_sgst = perc_sGST.
     clear B_DUTY1.
     clear perc_sGST.
  endif.

if GCALC1 <> 0.       """"""""for IGST
GCALC = GCALC1.
 WA_PO-TAXPERS_igst = perc_iGST.
clear GCALC1.
clear perc_IGST.
  endif.

          APPEND WA_PO TO IT_PO.
          CLEAR : WA_PO.
        ENDLOOP.


   ENDIF.

"modify wa_po from it_po transporting perc_SGST perc_iGST perc_cGST.

IF IT_PO[] IS INITIAL.
  MESSAGE 'Items not found' TYPE 'I'.
ENDIF.

*Set the print Parameters
  PERFORM set_print_param USING     ls_addr_key
                          CHANGING  ls_control_param
                                    ls_composer_param
                                    ls_recipient
                                    ls_sender
                                    ent_retco.

*Get the Smart Form name.
  IF NOT tnapr-sform IS INITIAL.
    lf_formname = tnapr-sform.
  ELSE.
    MESSAGE e001(ssfcomposer).
  ENDIF.
*break-point.
* Determine smartform function module for purchase document
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lf_formname
    IMPORTING
      fm_name            = lf_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
*  error handling
    ent_retco = sy-subrc.
    IF sy-subrc = 1.
      MESSAGE e001(ssfcomposer).
    ENDIF.
    IF sy-subrc = 2.
      MESSAGE e002(ssfcomposer) WITH tnapr-sform.
    ENDIF.
    PERFORM protocol_update_i.
  ENDIF.


* if it is faxed, changed its title to PO number
  IF ls_control_param-device = 'TELEFAX'.
    ls_composer_param-tdtitle = l_doc-xekko-ebeln.
  ENDIF.

* if it is mail, changed its title to PO number
*  IF ls_control_param-device = 'MAIL'.
*    ls_composer_param-tdtitle = l_doc-xekko-ebeln.
*  ENDIF.


*>>>>> Change of Parameters <<<<<<<<<<<<<<<<<<<<<<<
  CALL FUNCTION lf_fm_name
    EXPORTING
      archive_index      = toa_dara
      archive_parameters = arc_params
      control_parameters = ls_control_param
      mail_recipient     = ls_recipient
      mail_sender        = ls_sender
      output_options     = ls_composer_param
      is_ekko            = l_doc-xekko
      user_settings      = ' '  "Disable User Printer
      is_pekko           = l_doc-xpekko
      is_nast            = l_nast
      iv_from_mem        = l_from_memory
      iv_druvo           = iv_druvo
      iv_xfz             = iv_xfz

    TABLES
      it_ekpo            = l_doc-xekpo[]
      it_ekpa            = l_doc-xekpa[]
      it_pekpo           = l_doc-xpekpo[]
      it_eket            = l_doc-xeket[]
      it_tkomv           = l_doc-xtkomv[]
      it_ekkn            = l_doc-xekkn[]
      it_ekek            = l_doc-xekek[]
      it_komk            = l_xkomk[]
      it_tab             = it_po[]
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    ent_retco = sy-subrc.
    PERFORM protocol_update_i.

* get SmartForm protocoll and store it in the NAST protocoll
    PERFORM add_smfrm_prot.

  else.
*  required for Discrete Manufacturing scenario 230
**<<<< Start of insertion >>>>*
    IF  l_nast-kappl eq 'EL'     AND ent_screen IS INITIAL
      AND l_from_memory IS INITIAL AND l_nast-sndex IS INITIAL.
      IF sy-ucomm NE '9ANZ' AND sy-ucomm NE '9DPR'.
        PERFORM update_release(saplmedruck)
                TABLES l_doc-xekpo l_doc-xekek l_doc-xekeh
                USING  iv_druvo l_nast-kschl.
      ENDIF.
    ENDIF.
*<<<< End of insertion >>>>*
    read table ls_job_info-spoolids into l_spoolid index 1.
    if sy-subrc is initial.
      export spoolid = l_spoolid to memory id 'KYK_SPOOLID'.
    endif.

  ENDIF.
ENDFORM.                    "PROCESSING_PO


*&---------------------------------------------------------------------*
*&      Form  set_print_param
*&---------------------------------------------------------------------*
FORM set_print_param USING    is_addr_key LIKE addr_key
                     CHANGING cs_control_param TYPE ssfctrlop
                              cs_composer_param TYPE ssfcompop
                              cs_recipient TYPE  swotobjid
                              cs_sender TYPE  swotobjid
                              cf_retcode TYPE sy-subrc.

  DATA: ls_itcpo     TYPE itcpo.
  DATA: lf_repid     TYPE sy-repid.
  DATA: lf_device    TYPE tddevice.
  DATA: ls_recipient TYPE swotobjid.
  DATA: ls_sender    TYPE swotobjid.

  lf_repid = sy-repid.

  CALL FUNCTION 'WFMC_PREPARE_SMART_FORM'
    EXPORTING
      pi_nast       = nast
      pi_addr_key   = is_addr_key
      pi_repid      = lf_repid
    IMPORTING
      pe_returncode = cf_retcode
      pe_itcpo      = ls_itcpo
      pe_device     = lf_device
      pe_recipient  = cs_recipient
      pe_sender     = cs_sender.

  IF cf_retcode = 0.
    MOVE-CORRESPONDING ls_itcpo TO cs_composer_param.
*    cs_composer_param-tdnoprint = 'X'.                     "Note 591576
    cs_control_param-device      = lf_device.
    cs_control_param-no_dialog   = 'X'.
    cs_control_param-preview     = xscreen.
    cs_control_param-getotf      = ls_itcpo-tdgetotf.
    cs_control_param-langu       = nast-spras.
  ENDIF.
ENDFORM.                    "set_print_param
