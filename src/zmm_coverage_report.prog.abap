
*& Report  ZMM_COVERAGE_REPORT
*&
*&---------------------------------------------------------------------*
*&Functional                   : Naresh Samudrala                      *
*& Developer                   : Saiprasad Deshpande                   *
*& Created On                  : 09 Aug 2016                           *
*& Title                       : Report to record                      *
*&                               consumption of a stock material       *
*& Report Name                 : ZMM_COVERAGE_REPORT                   *
*& Development Id              : SPLABAP                               *
*& Solman call No              : N/A                                   *
*& Transport Request           : DEVK913283                            *
*& Related Information         : purpose of the report is to get the   *
*&                               correct information regarding the     *
*&                               consumption of a stock material       *
*1. DEVK913283    Sheenlac:MM:01: Coverage Report
*2. DEVK913465    Sheenlac:MM:02:Coverage Report
*3.DEVK913591       SPLABAP      Sheenlac: MM: 02: Coverage Report changes
*4. DEVK913611       SPLABAP      Sheenlac: MM:04: Excess qty logic change
*& --------------------------------------------------------------------*
*& Version   Desc                         modified by         date
*& V1        coverage report changes      manohar potnuru     07/09/2016
*  V2        Excess qty logic change      manohar potnuru     08/09/2016
*  V3        Current/Next month PR's      manohar potnuru     19/09/2016
*            (For current Month PR : EBAN-BADAT < or = to 10th of current month
*             For Next Month PR : EBAN-BADAT >   10th of current month
** V4         Changing the logic for       Vamsi Ippili       07/10/2016  DEVK913995
*             displaying Current & Next
*             month Quantities and also
*             consumption plans
** V5         Changing the logic for       Vamsi Ippili       09/10/2016  DEVK914009
*             displaying Current & Next
*             month Quantities and also
*             consumption plans
* V6          Changes for cosumption plans Vamsi Ippili       19/10/2016  DEVK914088
* V7          Changes for cosumption plans Vamsi Ippili       19/10/2016  DEVK914116
* V8          New Column(Consumption Prd)  Vamsi Ippili       18/11/2016  DEVK914434
* V9          New Column(Consumption Prd)_2  Vamsi Ippili       23/11/2016  DEVK914434
* V10         Sheenlac : MM : Month Open Stock Issue  Vamsi Ippili  02/01/2017  DEVK914862
*&---------------------------------------------------------------------*

REPORT zmm_coverage_report  MESSAGE-ID mm.
TABLES:zmm_mat_coverage.
TABLES: mara, mard .                     "V1
TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*&  Structure & Internal Table Decleration
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_marc,
       matnr  TYPE matnr,
       werks  TYPE werks_d,
       eisbe  TYPE eisbe,
       END OF ty_marc.

TYPES: BEGIN OF ty_mara,
       matnr  TYPE matnr,
       mtart  TYPE mtart,
       END OF ty_mara.

TYPES: BEGIN OF ty_makt,
       matnr  TYPE matnr,
       spras  TYPE spras,
       maktx  TYPE maktx,
       END OF ty_makt.

TYPES: BEGIN OF ty_mard,
       matnr  TYPE matnr,
       werks  TYPE werks_d,
       lgort  TYPE lgort_d,
       labst  TYPE labst,
       insme  TYPE insme,
       speme  TYPE speme,
       vmuml  TYPE vmuml,
       END OF ty_mard.

TYPES: BEGIN OF ty_resb,
       rsnum TYPE rsnum,
       rspos TYPE rspos,
       rsart TYPE rsart,
       matnr  TYPE matnr,
       werks  TYPE werks_d,
       bdmng  TYPE bdmng,
       enmng  TYPE enmng,
       END OF ty_resb.

TYPES: BEGIN OF ty_mseg,
       mblnr  TYPE mblnr,
       mjahr  TYPE mjahr,
       zeile  TYPE mblpo,
       bwart  TYPE bwart,
       matnr  TYPE matnr,
       werks  TYPE werks_d,
       menge  TYPE menge_d,
       END OF ty_mseg.

TYPES: BEGIN OF ty_mseg1,
       mblnr  TYPE mblnr,
       mjahr  TYPE mjahr,
       zeile  TYPE mblpo,
       bwart  TYPE bwart,
       matnr  TYPE matnr,
       werks  TYPE werks_d,
       menge  TYPE menge_d,
       END OF ty_mseg1.

TYPES: BEGIN OF ty_eban,
       banfn  TYPE banfn,
       bnfpo  TYPE bnfpo,
       matnr  TYPE matnr,
       werks  TYPE ewerk,
       matkl  TYPE matkl,
       menge  TYPE bamng,
       END OF ty_eban.

* begin of V1
TYPES: BEGIN OF ty_ekpo,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        matnr LIKE ekpo-matnr,
        werks LIKE ekpo-werks,
        loekz LIKE ekpo-loekz,
        elikz LIKE ekpo-elikz,
      END OF ty_ekpo.

TYPES: BEGIN OF ty_eket,
        ebeln LIKE eket-ebeln,
         ebelp LIKE eket-ebelp,
         etenr  LIKE eket-etenr,
        menge LIKE eket-menge,
        wemng LIKE eket-wemng,
      END OF ty_eket.
* end of V1
*--------------------------- +V4-----------------------------*
TYPES : BEGIN OF ty_plaf,
        matnr TYPE plmat,
        plwrk TYPE plwrk,
        gsmng TYPE gsmng,
        END   OF ty_plaf.
*--------------------------- +V4-----------------------------*
**-------------------begin of +V6----------------------------*
TYPES : BEGIN OF ty_resb_con,
        rsnum TYPE rsnum,
        rspos TYPE rspos,
        rsart TYPE rsart,
*        BDART type BDART,
*        xloek TYPE xloek,
        matnr TYPE matnr,
        werks TYPE werks_d,
        erfmg TYPE erfmg,
        END   OF ty_resb_con.
**-------------------end of +V6----------------------------*
TYPES: BEGIN OF ty_final,
        werks TYPE werks_d,
        matnr TYPE matnr,
        maktx TYPE maktx,
        mtart TYPE mtart,
        zopstk  TYPE zopstk,
        eisbe TYPE eisbe,
        labst TYPE labst,
        bdmng TYPE bdmng,
        insme TYPE insme,
        vmuml TYPE vmuml,
        speme TYPE speme,
        curmnt  TYPE einme,
        cosmnt  TYPE einme,
        cosprd  TYPE einme,
        totpr TYPE  bamng,
        curmnt1 TYPE bamng,
        excstk  TYPE bamng,
        nxtmnt  TYPE bamng,
        nxtpr TYPE bamng,
        oppoqty TYPE etmen,    "V1
        curdate(10) TYPE c,    "V1
        curtime(8) TYPE c,     "V1
      END OF ty_final.

DATA: it_mara TYPE STANDARD TABLE OF ty_mara,
      wa_mara TYPE ty_mara,
      it_marc TYPE STANDARD TABLE OF ty_marc,
      wa_marc TYPE ty_marc,
      it_mard TYPE STANDARD TABLE OF ty_mard,
      wa_mard TYPE ty_mard,
      it_mard1 TYPE STANDARD TABLE OF ty_mard,
      wa_mard1 TYPE ty_mard,
      it_makt TYPE STANDARD TABLE OF ty_makt,
      wa_makt TYPE ty_makt,
      it_resb TYPE STANDARD TABLE OF ty_resb,
      wa_resb TYPE ty_resb,
      it_resb1 TYPE STANDARD TABLE OF ty_resb,
      wa_resb1 TYPE ty_resb,
      it_resb2 TYPE STANDARD TABLE OF ty_resb,
      wa_resb2 TYPE ty_resb,
      it_resb3 TYPE STANDARD TABLE OF ty_resb,
      wa_resb3 TYPE ty_resb,
      it_resb_261 TYPE STANDARD TABLE OF ty_resb,
      wa_resb_261 TYPE ty_resb,
      it_resb_1261 TYPE STANDARD TABLE OF ty_resb,
**-------------------begin of +V6----------------------------*
      it_resb_con TYPE STANDARD TABLE OF ty_resb_con,
      it_resb_con1 TYPE STANDARD TABLE OF ty_resb_con,
      it_resb_con2 TYPE STANDARD TABLE OF ty_resb_con,
      it_resb_con3 TYPE STANDARD TABLE OF ty_resb_con,
      wa_resb_con TYPE ty_resb_con,
**-------------------end of +V6----------------------------*
      it_mseg TYPE STANDARD TABLE OF ty_mseg,
      wa_mseg TYPE ty_mseg,
      it_mseg1 TYPE STANDARD TABLE OF ty_mseg,
      wa_mseg1 TYPE ty_mseg,
      it_mseg2 TYPE STANDARD TABLE OF ty_mseg,                     " +V8
*--------------------------- +V4-----------------------------*
      it_plaf1 TYPE STANDARD TABLE OF ty_plaf,
      it_plaf2 TYPE STANDARD TABLE OF ty_plaf,
      it_plaf3 TYPE STANDARD TABLE OF ty_plaf,
      it_plaf4 TYPE STANDARD TABLE OF ty_plaf,
      wa_plaf  TYPE ty_plaf,
*--------------------------- +V4-----------------------------*
      it_mseg101 TYPE STANDARD TABLE OF ty_mseg1,
      wa_mseg101 TYPE ty_mseg1,
      it_mseg1101 TYPE STANDARD TABLE OF ty_mseg1,
      wa_mseg1101 TYPE ty_mseg1,

*      it_mseg102 TYPE STANDARD TABLE OF ty_mseg,
      wa_mseg102 TYPE ty_mseg1,
*      it_mseg1102 TYPE STANDARD TABLE OF ty_mseg,
*      wa_mseg1102 TYPE ty_mseg,

*      it_mseg103 TYPE STANDARD TABLE OF ty_mseg,
      wa_mseg103 TYPE ty_mseg1,
*      it_mseg1103 TYPE STANDARD TABLE OF ty_mseg,
*      wa_mseg1103 TYPE ty_mseg,

      it_eban TYPE STANDARD TABLE OF ty_eban,
      wa_eban TYPE ty_eban,
      it_eban1 TYPE STANDARD TABLE OF ty_eban,
      wa_eban1 TYPE ty_eban,
      it_eban2 TYPE STANDARD TABLE OF ty_eban,
      wa_eban2 TYPE ty_eban,
      it_eban3 TYPE STANDARD TABLE OF ty_eban,
      wa_eban3 TYPE ty_eban,
      it_zmat TYPE STANDARD TABLE OF zmm_mat_coverage,
      wa_zmat TYPE zmm_mat_coverage,
      it_final  TYPE STANDARD TABLE OF ty_final,
      wa_final  TYPE ty_final,
      it_fcat TYPE slis_t_fieldcat_alv,
      wa_fcat TYPE slis_fieldcat_alv,
      layout TYPE slis_layout_alv,
*      it_sort TYPE slis_t_sortinfo_alv,
*      wa_sort LIKE LINE OF it_sort.

      it_ekpo TYPE STANDARD TABLE OF ty_ekpo,         "V1
      wa_ekpo TYPE ty_ekpo,                           "V1
      it_ekpo1 TYPE STANDARD TABLE OF ty_ekpo,         "V1
      wa_ekpo1 TYPE ty_ekpo,                           "V1
      it_eket TYPE STANDARD TABLE OF ty_eket,         "V1
      wa_eket TYPE ty_eket.                           "V1

DATA : lv_matnr  TYPE mara-matnr,
      lv_date TYPE sydatum,
      sdate TYPE p0001-begda,
      ldate TYPE p0001-endda,
      sdate1 TYPE p0001-begda,
      ldate1 TYPE p0001-endda,
      s_date  TYPE RANGE OF mseg-budat_mkpf,
      s_date1 TYPE RANGE OF mseg-budat_mkpf,
      wa_date LIKE LINE OF s_date,
      lv_10thofmonth TYPE sy-datum.                   "V3


************************ code added by monika
DATA : it_fld TYPE slis_t_fieldcat_alv ,
        it_evt TYPE slis_t_event     ,
        wa_fld TYPE slis_fieldcat_alv   ,
        wa_evt TYPE slis_alv_event      ,
        wa_lay TYPE slis_layout_alv     .





*--------------------------- +V4-----------------------------*
REFRESH : it_plaf1,
          it_plaf2,
          it_plaf3,
          it_plaf4,      "V6
          it_resb_con,   "V6
          it_resb_con1.  "V6
CLEAR   : wa_plaf,
          wa_resb_con.   "V6
*--------------------------- +V4-----------------------------*


*&---------------------------------------------------------------------*
*&  Selection Screen Fields
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*PARAMETERS: p_werks TYPE werks_d  OBLIGATORY.      " commented by V1
SELECT-OPTIONS : s_werks FOR mard-werks OBLIGATORY.            "V1
SELECT-OPTIONS : s_matnr FOR lv_matnr.
SELECT-OPTIONS : s_mtart FOR mara-mtart.            "V1
SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&  Start of selection
*&---------------------------------------------------------------------*

START-OF-SELECTION.
*current Month
  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = sy-datum
    IMPORTING
      ev_month_begin_date = sdate
      ev_month_end_date   = ldate.

  wa_date-low = sdate.
  wa_date-sign = 'I'.
  wa_date-high = ldate.
  wa_date-option = 'BT'.
  APPEND wa_date  TO s_date.
  CLEAR wa_date.
**Next Month
  lv_date = sy-datum.
  lv_date+4(2) = lv_date+4(2) + 1.
  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = lv_date
    IMPORTING
      ev_month_begin_date = sdate1
      ev_month_end_date   = ldate1.

  wa_date-low = sdate1.
  wa_date-sign = 'I'.
  wa_date-high = ldate1.
  wa_date-option = 'BT'.
  APPEND wa_date  TO s_date1.

  CONCATENATE sy-datum(6) '10' INTO lv_10thofmonth.

* begin of comment - V1
*  SELECT *  FROM zmm_mat_coverage INTO TABLE it_zmat     "commented by V1
*            WHERE werks = p_werks      "commented by V1
*            AND matnr  IN s_matnr.     "commented by V1

*  SELECT matnr
*         werks
*         lgort
*         labst
*         insme
*         speme
*         vmuml
*         INTO TABLE it_mard1
*               FROM mard
*               WHERE matnr IN s_matnr
*            and werks = p_werks.
*  end of commented by -- V1

* begin of V1
  DATA:  lv_menge TYPE etmen,
         lv_wemng TYPE weemg.

  SELECT  a~mandt
          a~werks
          a~matnr
          a~zopstk
          a~zdate  FROM zmm_mat_coverage AS a INNER JOIN mara AS b
              ON a~matnr = b~matnr
           INTO TABLE it_zmat
              WHERE a~werks IN s_werks
              AND a~matnr  IN s_matnr
              AND  b~mtart IN s_mtart.

  SELECT a~matnr
         a~werks
         a~lgort
         a~labst
         a~insme
         a~speme
         a~vmuml
         INTO TABLE it_mard1
               FROM mard AS a INNER JOIN mara AS b
                ON a~matnr = b~matnr
               WHERE a~matnr IN s_matnr
                 AND a~werks IN s_werks
                  AND b~mtart IN s_mtart.
* end of V1
  LOOP AT it_mard1 INTO wa_mard1.
    CLEAR wa_mard1-lgort.
    COLLECT wa_mard1  INTO it_mard.
    CLEAR: wa_mard1.
  ENDLOOP.

  IF sy-subrc EQ 0.
    SELECT matnr
           werks
           eisbe
           INTO TABLE it_marc
                 FROM marc
                 FOR ALL ENTRIES IN it_mard
                 WHERE matnr = it_mard-matnr
                 AND werks = it_mard-werks.

    SELECT matnr
           mtart
           INTO TABLE it_mara
           FROM mara
           FOR ALL ENTRIES IN it_mard
           WHERE matnr = it_mard-matnr.

    SELECT matnr
           spras
           maktx
           INTO TABLE it_makt
           FROM makt
           FOR ALL ENTRIES IN it_mard
           WHERE matnr = it_mard-matnr
           AND spras = sy-langu.

    SELECT rsnum
           rspos
           rsart
           matnr
           werks
           bdmng
           enmng
           INTO TABLE it_resb
                 FROM resb
                 FOR ALL ENTRIES IN it_mard
                 WHERE matnr = it_mard-matnr
                 AND werks = it_mard-werks
                 AND bdter IN s_date.

    SELECT rsnum
           rspos
           rsart
           matnr
           werks
           bdmng
           enmng
           INTO TABLE it_resb2
                 FROM resb
                 FOR ALL ENTRIES IN it_mard
                 WHERE matnr = it_mard-matnr
                 AND werks = it_mard-werks
                 AND bdter IN s_date1.

    SELECT rsnum
           rspos
           rsart
           matnr
           werks
           bdmng
           enmng
           INTO TABLE it_resb_1261
                 FROM resb
                 FOR ALL ENTRIES IN it_mard
                 WHERE xloek = ' '
                 AND kzear = ' '
                 AND matnr = it_mard-matnr
                 AND werks = it_mard-werks
                 AND bwart = '261'.

    SELECT mblnr
           mjahr
           zeile
           bwart                                " +V8
           matnr
           werks
           menge
           INTO TABLE it_mseg
                 FROM mseg
                 FOR ALL ENTRIES IN it_mard
                 WHERE bwart IN (261,262,201,601,641,654,551)               " +V8
                 AND  matnr = it_mard-matnr
                 AND  werks = it_mard-werks
                 AND budat_mkpf IN s_date.


    SELECT mblnr mjahr zeile bwart matnr
           werks
           menge
           INTO TABLE it_mseg101
                 FROM mseg
                 FOR ALL ENTRIES IN it_mard
                 WHERE bwart IN (101,102,122)
                 AND  matnr = it_mard-matnr
                 AND  werks = it_mard-werks
                 AND budat_mkpf IN s_date.

*-----------------------begin of -V4 ---------------------------------*
*    SELECT banfn bnfpo matnr
*           werks matkl
*           menge
*           INTO TABLE it_eban
*                 FROM eban
*                 FOR ALL ENTRIES IN it_mard
*                 WHERE matnr = it_mard-matnr
*                 AND  werks = it_mard-werks
**                 AND badat IN s_date.                          "V3
*                and badat le lv_10thofmonth.                   "V3
*
*    DELETE it_eban  WHERE ( matkl(2) NE 'RM' AND matkl(2) NE 'PM' ).
*
*
*    SELECT banfn bnfpo matnr
*           werks matkl
*           menge
*           INTO TABLE it_eban2
*                 FROM eban
*                 FOR ALL ENTRIES IN it_mard
*                 WHERE matnr = it_mard-matnr
*                 AND  werks = it_mard-werks
**                 AND badat IN s_date.                          "V3
*                and badat gt lv_10thofmonth.                   "V3
*
*    DELETE it_eban  WHERE ( matkl(2) NE 'RM' AND matkl(2) NE 'PM' ).
*---------------------end of -V4 ---------------------------------*
*---------------------Begin of  +V4 ---------------------------------*
    DATA :lv_dat  TYPE datum,
          lv_date1 TYPE datum,
          lv_date2 TYPE datum,
          lv_date3 TYPE datum,
          lv_date4 TYPE datum.

    CLEAR :lv_dat,
           lv_date1,
           lv_date2,
           lv_date3,
           lv_date4.
    IF sy-datum+6(2) LE 09.
* 10th of current month
      CALL FUNCTION 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'
        EXPORTING
          i_date_old = sy-datum
        IMPORTING
          e_date_new = lv_dat.
      lv_date1 = lv_dat.
      lv_date1+6(2) = 10.
*9th of current month
      lv_date2 = sy-datum.
      lv_date2+6(2) = 09.
*10th of next month
      lv_date3 = sy-datum.
      lv_date3+6(2) = 10.
*9th of next month
      CLEAR lv_dat.
      CALL FUNCTION 'OIL_GET_NEXT_MONTH'
        EXPORTING
          i_date = sy-datum
        IMPORTING
          e_date = lv_dat.
      lv_date4 = lv_dat.
      lv_date4+6(2) = 09.
    ELSEIF sy-datum+6(2) GE 10.
* 10th of current month
      CALL FUNCTION 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'
        EXPORTING
          i_date_old = sy-datum
        IMPORTING
          e_date_new = lv_dat.
      lv_date1 = lv_dat.
      lv_date1+6(2) = 10.
*9th of current month
      lv_date2 = sy-datum.
      lv_date2+6(2) = 09.
*10th of next month
      lv_date3 = sy-datum.
      lv_date3+6(2) = 10.
*9th of next month
      CLEAR lv_dat.
      CALL FUNCTION 'OIL_GET_NEXT_MONTH'
        EXPORTING
          i_date = sy-datum
        IMPORTING
          e_date = lv_dat.
      lv_date4 = lv_dat.
      lv_date4+6(2) = 09.
    ENDIF.
* Current PR Quantity

    SELECT banfn bnfpo matnr
           werks matkl
           menge
           INTO TABLE it_eban
                 FROM eban
                 FOR ALL ENTRIES IN it_mard
                 WHERE matnr = it_mard-matnr
                 AND  werks = it_mard-werks
                 AND  loekz NE 'X'
                 AND  badat BETWEEN lv_date1 AND lv_date2.

    IF sy-subrc EQ 0.
      DELETE it_eban  WHERE ( matkl(2) NE 'RM' AND matkl(2) NE 'PM' ).
    ENDIF.
*Next Month PR plan
    SELECT banfn bnfpo matnr
       werks matkl
       menge
       INTO TABLE it_eban2
             FROM eban
             FOR ALL ENTRIES IN it_mard
             WHERE matnr = it_mard-matnr
             AND  werks = it_mard-werks
             AND  loekz NE 'X'
             AND  badat BETWEEN  lv_date3 AND  lv_date4.
    IF sy-subrc EQ 0.
      DELETE it_eban2  WHERE ( matkl(2) NE 'RM' AND matkl(2) NE 'PM' ).
    ENDIF.
* Current Month Consumption Plan
    CLEAR :lv_date1,
           lv_date2.
    CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
      EXPORTING
        iv_date             = sy-datum
      IMPORTING
        ev_month_begin_date = lv_date1
        ev_month_end_date   = lv_date2.

    SELECT matnr
           plwrk
           gsmng
           FROM plaf
           INTO TABLE it_plaf1
           WHERE matnr IN s_matnr
             AND plwrk IN s_werks
             AND psttr BETWEEN lv_date1 AND lv_date2.
*---------------------begin of +V6--------------------*
*    IF sy-subrc NE 0.      "V7
    SELECT rsnum
           rspos
           rsart
           matnr
           werks
           erfmg
           INTO TABLE it_resb_con
           FROM resb
           FOR ALL ENTRIES IN it_mard
           WHERE bdart = 'SB'
             AND xloek = ' '
             AND matnr = it_mard-matnr
             AND werks = it_mard-werks
             AND bdter BETWEEN lv_date1 AND lv_date2.
*    ENDIF.                     "V7
*---------------------end of +V6--------------------*
* Next Month Consumption Plan
    CLEAR :lv_date1,
           lv_date2.
    CALL FUNCTION 'HR_PSD_DATES_ADD_MONTHS'
      EXPORTING
        v_date       = sy-datum
        v_months     = 1
      IMPORTING
        e_date       = lv_date2              " V5
      EXCEPTIONS
        not_positive = 1
        OTHERS       = 2.
    IF sy-subrc EQ 0.
      lv_date1 = lv_date2.                   "V5
      lv_date1+6(2) = 01.                    "V5
    ENDIF.
    SELECT matnr
               plwrk
               gsmng
               FROM plaf
               INTO TABLE it_plaf2
               WHERE matnr IN s_matnr
                 AND plwrk IN s_werks
                 AND psttr BETWEEN lv_date1 AND lv_date2.
*---------------------begin of +V6--------------------*
    SELECT rsnum
           rspos
           rsart
           matnr
           werks
           erfmg
           INTO TABLE it_resb_con1
           FROM resb
           FOR ALL ENTRIES IN it_mard
           WHERE  bdart = 'SB'
             AND xloek = ' '
             AND matnr = it_mard-matnr
             AND werks = it_mard-werks
             AND bdter BETWEEN lv_date1 AND lv_date2.
*---------------------end of +V6--------------------*
*---------------------end of +V4 ---------------------------------*
* begin of V1
    SELECT ebeln ebelp matnr werks loekz elikz  FROM ekpo
                                   INTO TABLE it_ekpo
                                   FOR ALL ENTRIES IN it_mard
                                   WHERE matnr = it_mard-matnr
                                   AND   werks = it_mard-werks
                                   AND   loekz = ' '
                                   AND   elikz = ' '.

    SELECT ebeln ebelp etenr menge wemng FROM eket
                             INTO TABLE it_eket
                             FOR ALL ENTRIES IN it_ekpo
                              WHERE ebeln = it_ekpo-ebeln
                              AND   ebelp = it_ekpo-ebelp.


* begin of V8

    CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
      IMPORTING
        et_events = it_evt.

    READ TABLE it_evt INTO wa_evt
         WITH KEY name = slis_ev_after_line_output .
    wa_evt-form = slis_ev_after_line_output .
    MODIFY it_evt FROM wa_evt INDEX sy-tabix .

    READ TABLE it_evt INTO wa_evt
         WITH KEY name = slis_ev_top_of_page .
    wa_evt-form = slis_ev_top_of_page .
    MODIFY it_evt FROM wa_evt INDEX sy-tabix .

    READ TABLE it_evt INTO wa_evt
         WITH KEY name = slis_ev_end_of_list .
    wa_evt-form = slis_ev_end_of_list .
    MODIFY it_evt FROM wa_evt INDEX sy-tabix .
* end of v8
* end of V1

  ENDIF.

  LOOP AT it_mseg INTO wa_mseg1 .
*------------------------------ +V8------------------------------------------*
    IF wa_mseg1-bwart EQ '261' OR
       wa_mseg1-bwart EQ '262'.
      CLEAR: wa_mseg1-mblnr, wa_mseg1-mjahr, wa_mseg1-zeile,wa_mseg1-bwart.
      COLLECT wa_mseg1  INTO it_mseg1.
      CLEAR wa_mseg1.
    ELSE.
      CLEAR: wa_mseg1-mblnr, wa_mseg1-mjahr, wa_mseg1-zeile,wa_mseg1-bwart.
      COLLECT wa_mseg1  INTO it_mseg2.
      CLEAR wa_mseg1.
    ENDIF.
*------------------------------ +V8------------------------------------------*
  ENDLOOP.

  LOOP AT it_mseg101 INTO wa_mseg1101.
    CLEAR: wa_mseg1101-mblnr, wa_mseg1101-mjahr, wa_mseg1101-zeile.
    COLLECT wa_mseg1101  INTO it_mseg1101.
    CLEAR wa_mseg1101.
  ENDLOOP.

  LOOP AT it_eban INTO wa_eban1.
    CLEAR: wa_eban1-banfn, wa_eban1-bnfpo, wa_eban1-matkl.
    COLLECT wa_eban1  INTO it_eban1.
    CLEAR: wa_eban1.
  ENDLOOP.

  LOOP AT it_eban2 INTO wa_eban3.
    CLEAR: wa_eban3-banfn, wa_eban3-bnfpo, wa_eban3-matkl.
    COLLECT wa_eban3  INTO it_eban3.
    CLEAR: wa_eban3.
  ENDLOOP.

  LOOP AT it_resb INTO wa_resb1.
    CLEAR: wa_resb1-rsnum,wa_resb1-rspos,wa_resb1-rsart.
    COLLECT wa_resb1  INTO it_resb1.
    CLEAR: wa_resb1.
  ENDLOOP.

  LOOP AT it_resb2 INTO wa_resb3.
    CLEAR: wa_resb3-rsnum,wa_resb3-rspos,wa_resb3-rsart.
    COLLECT wa_resb3  INTO it_resb3.
    CLEAR: wa_resb3.
  ENDLOOP.

  LOOP AT it_resb_1261 INTO wa_resb_261.
    CLEAR: wa_resb_261-rsnum,wa_resb_261-rspos,wa_resb_261-rsart.
    COLLECT wa_resb_261  INTO it_resb_261.
    CLEAR: wa_resb_261.
  ENDLOOP.
*--------------------Begin of V4 ---------------------------------*
  CLEAR wa_plaf.
  LOOP AT it_plaf1
       INTO wa_plaf.
    COLLECT wa_plaf INTO it_plaf3.
  ENDLOOP.
  CLEAR wa_plaf.
  LOOP AT it_plaf2
       INTO wa_plaf.
    COLLECT wa_plaf INTO it_plaf4.
  ENDLOOP.
*--------------------End of V4 ---------------------------------*
*------------------begin of +V6----------------------------*
  CLEAR wa_resb_con.
  LOOP AT it_resb_con
       INTO wa_resb_con.
    CLEAR : wa_resb_con-rsnum,
            wa_resb_con-rspos,
            wa_resb_con-rsart.
    COLLECT wa_resb_con INTO it_resb_con2.
  ENDLOOP.

  CLEAR wa_resb_con.
  LOOP AT it_resb_con1
       INTO wa_resb_con.
    CLEAR : wa_resb_con-rsnum,
            wa_resb_con-rspos,
            wa_resb_con-rsart.
    COLLECT wa_resb_con INTO it_resb_con3.
  ENDLOOP.

*------------------end  of +V6----------------------------*

  SORT it_makt BY matnr.
  SORT it_mara BY matnr.
  SORT it_zmat BY werks matnr.
  SORT it_marc BY matnr werks.


* begin of V1
  DATA:  lv_curdate(10) TYPE c,
         lv_curtime(8) TYPE c.

  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = sy-datum
    IMPORTING
      output = lv_curdate.

  CONCATENATE sy-uzeit+0(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) INTO lv_curtime.
* end of V1

  LOOP AT it_mard INTO wa_mard.
    wa_final-werks = wa_mard-werks.
    wa_final-matnr = wa_mard-matnr.
    READ TABLE  it_makt INTO wa_makt  WITH KEY matnr = wa_mard-matnr
                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-maktx = wa_makt-maktx.
    ENDIF.
    READ TABLE it_mara  INTO wa_mara  WITH KEY matnr = wa_mard-matnr
                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-mtart = wa_mara-mtart.
* begin of V1
      IF wa_final-mtart = 'ROH' OR wa_final-mtart = 'VERP'.
        CLEAR:  it_ekpo1, lv_menge, lv_wemng.
        REFRESH it_ekpo1[].
        it_ekpo1[]  = it_ekpo[].
        DELETE it_ekpo1 WHERE matnr NE wa_mard-matnr
                        OR werks NE wa_mard-werks.
        SORT it_ekpo1 BY ebeln ebelp.
        DELETE ADJACENT DUPLICATES FROM it_ekpo1 COMPARING ebeln ebelp.
        LOOP AT it_ekpo1 INTO wa_ekpo1.
          LOOP AT it_eket INTO wa_eket WHERE ebeln = wa_ekpo1-ebeln
                                        AND  ebelp = wa_ekpo1-ebelp.
            lv_menge = lv_menge + wa_eket-menge.
            lv_wemng = lv_wemng + wa_eket-wemng.
            CLEAR wa_eket.
          ENDLOOP.
        ENDLOOP.
        wa_final-oppoqty = lv_menge - lv_wemng.
      ENDIF.
* end of V1
    ENDIF.
    READ TABLE it_zmat  INTO wa_zmat  WITH KEY werks = wa_mard-werks
                                               matnr = wa_mard-matnr
                                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF wa_zmat-zdate+4(2) EQ sy-datum+4(2).
        wa_final-zopstk = wa_zmat-zopstk.
      ELSEIF wa_zmat-zdate+4(2) LT sy-datum+4(2).
*        CLEAR wa_zmat.
*  wa_zmat-WERKS = wa_mard-werks.
*  wa_zmat-matnr = wa_mard-matnr.
        wa_zmat-zopstk = wa_mard-labst.
        wa_final-zopstk = wa_mard-labst.
        wa_zmat-zdate = sy-datum.
        MODIFY zmm_mat_coverage FROM wa_zmat.
        COMMIT  WORK.
      ELSE.                                           " V10
        wa_zmat-zopstk = wa_mard-labst.
        wa_final-zopstk = wa_mard-labst.
        wa_zmat-zdate = sy-datum.
        MODIFY zmm_mat_coverage FROM wa_zmat.
        COMMIT  WORK.                             " V10
      ENDIF.
    ELSE.
      CLEAR wa_zmat.
      wa_final-zopstk = wa_mard-labst.
      wa_zmat-werks = wa_mard-werks.
      wa_zmat-matnr = wa_mard-matnr.
      wa_zmat-zopstk = wa_mard-labst.
      wa_zmat-zdate = sy-datum.
      INSERT zmm_mat_coverage FROM wa_zmat.
      COMMIT  WORK.
    ENDIF.

    READ TABLE  it_marc INTO wa_marc  WITH KEY matnr = wa_mard-matnr
                                      werks = wa_mard-werks
                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-eisbe = wa_marc-eisbe.
    ENDIF.
    wa_final-labst = wa_mard-labst.
    wa_final-insme = wa_mard-insme.
    wa_final-vmuml = wa_mard-vmuml.
    wa_final-speme = wa_mard-speme.

    READ TABLE  it_resb_261 INTO wa_resb_261  WITH KEY matnr = wa_mard-matnr
                                      werks = wa_mard-werks.
*                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-bdmng = wa_resb_261-bdmng - wa_resb_261-enmng.
    ENDIF.
* ----------------------------- -V4 --------------------------------------*
*    READ TABLE  it_resb1 INTO wa_resb  WITH KEY matnr = wa_mard-matnr
*                                      werks = wa_mard-werks.
**                                      BINARY SEARCH.
*    IF sy-subrc EQ 0.
**      wa_final-bdmng = wa_resb-bdmng.
*      wa_final-curmnt = wa_resb-bdmng - wa_resb-enmng.
*    ENDIF.
* ----------------------------- -V4 --------------------------------------*
    READ TABLE  it_mseg1 INTO wa_mseg  WITH KEY matnr = wa_mard-matnr
                                      werks = wa_mard-werks.
*                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-cosmnt = wa_mseg-menge.
    ENDIF.
* ----------------------------- +V8 --------------------------------------*
    CLEAR wa_mseg.
    READ TABLE it_mseg2
          INTO wa_mseg
          WITH KEY matnr = wa_mard-matnr
                   werks = wa_mard-werks.
    IF sy-subrc EQ 0.
      wa_final-cosprd = wa_mseg-menge.
    ENDIF.
* ----------------------------- +V8 --------------------------------------*
    READ TABLE  it_eban1 INTO wa_eban  WITH KEY matnr = wa_mard-matnr
                                      werks = wa_mard-werks.
*                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-totpr = wa_eban-menge.
    ENDIF.
* ----------------------------- -V4 --------------------------------------*
*    READ TABLE  it_resb3 INTO wa_resb2  WITH KEY matnr = wa_mard-matnr
*                                      werks = wa_mard-werks.
**                                      BINARY SEARCH.
*    IF sy-subrc EQ 0.
**      wa_final-bdmng = wa_resb2-bdmng.
*      wa_final-nxtmnt = wa_resb2-bdmng - wa_resb2-enmng.
*    ENDIF.
* ----------------------------- -V4 --------------------------------------*
    READ TABLE  it_eban3 INTO wa_eban2  WITH KEY matnr = wa_mard-matnr
                                      werks = wa_mard-werks.
*                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_final-nxtpr = wa_eban2-menge.
    ENDIF.

    READ TABLE  it_mseg1101 INTO wa_mseg101  WITH KEY bwart = '101'
                                      matnr = wa_mard-matnr
                                      werks = wa_mard-werks.
*                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE  it_mseg1101 INTO wa_mseg102  WITH KEY bwart = '102'
                                        matnr = wa_mard-matnr
                                        werks = wa_mard-werks.
      READ TABLE  it_mseg1101 INTO wa_mseg103  WITH KEY bwart = '122'
                                        matnr = wa_mard-matnr
                                        werks = wa_mard-werks.
      wa_final-curmnt1 = wa_mseg101-menge - ( wa_mseg102-menge + wa_mseg103-menge ).
    ENDIF.

*    wa_final-excstk = ( wa_final-labst + wa_final-bdmng + wa_final-insme )   commented by "V2
*                         - ( wa_final-curmnt - wa_final-cosmnt ).             commented by "V2
*    wa_final-excstk =  wa_final-labst - ( wa_final-curmnt - wa_final-cosmnt ). "-V8
*    wa_final-excstk = ( wa_final-labst -  wa_final-curmnt ) - wa_final-cosprd .  "+V8  -V9

* begin of V1
    wa_final-curdate = lv_curdate.
    wa_final-curtime = lv_curtime.
* end of V1

*---------------------------------------- +V4 ------------------------------------*
    CLEAR wa_plaf.
    READ TABLE it_plaf3
          INTO wa_plaf
          WITH KEY matnr = wa_mard-matnr
                   plwrk = wa_mard-werks.
    IF sy-subrc EQ 0.
      wa_final-curmnt = wa_plaf-gsmng.
*---------------------Begin of +V6 ---------------------------*
    ELSE.
      CLEAR wa_resb_con.
      READ TABLE it_resb_con2
            INTO wa_resb_con
            WITH KEY matnr = wa_mard-matnr
                     werks = wa_mard-werks.
      IF sy-subrc EQ 0.
        wa_final-curmnt = wa_resb_con-erfmg.
      ENDIF.
    ENDIF.
*---------------------end of +V6 ---------------------------*
    CLEAR wa_plaf.
    READ TABLE it_plaf4
          INTO wa_plaf
          WITH KEY matnr = wa_mard-matnr
                   plwrk = wa_mard-werks.
    IF sy-subrc EQ 0.
      wa_final-nxtmnt = wa_plaf-gsmng.
    ELSE.
      CLEAR wa_resb_con.
      READ TABLE it_resb_con3
            INTO wa_resb_con
            WITH KEY matnr = wa_mard-matnr
                     werks = wa_mard-werks.
      IF sy-subrc EQ 0.
        wa_final-nxtmnt = wa_resb_con-erfmg.
      ENDIF.
    ENDIF.
*---------------------------------------- +V4 ------------------------------------*
    wa_final-excstk = ( wa_final-labst -  wa_final-curmnt ) - wa_final-cosprd . "V9
    APPEND wa_final TO it_final.
    CLEAR: wa_mard, wa_makt, wa_zmat, wa_mara, wa_marc, wa_resb, wa_resb2,
           wa_eban, wa_eban2, wa_mseg, wa_mseg101, wa_mseg102, wa_mseg103,
           wa_final.
  ENDLOOP.

*&---------------------------------------------------------------------*
*&  ALV Layout
*&---------------------------------------------------------------------*

  PERFORM alv_layout USING 1 'Plant' 'WERKS' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 2 'Material' 'MATNR' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 3 'Material Description' 'MAKTX' 'it_final' ' ' '' '' '40'.
  PERFORM alv_layout USING 4 'Material Type' 'MTART' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 5 'Month Opening stock' 'ZOPSTK' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 6 'Safety Stock' 'EISBE' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 7 'Unrestricted stock' 'LABST' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 8 'Reserved Stock' 'BDMNG' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 9 'Stock under quality' 'INSME' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 10 'In-transit Stock' 'VMUML' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 11 'Stock under Blocked' 'SPEME' 'it_final' ' ' '' '' ''.
*  PERFORM alv_layout USING 12 'Current Month consumption' 'CURMNT' 'it_final' ' ' '' ''.       commented by     "V1
  PERFORM alv_layout USING 12 'Current Month Consumption Plan' 'CURMNT' 'it_final' ' ' '' '' ''.                     "v1
*  PERFORM alv_layout USING 13 'Consumed till date in current month' 'COSMNT' 'it_final' ' ' '' ''. commented by "V1
  PERFORM alv_layout USING 13 'Consumption' 'COSMNT' 'it_final' ' ' '' '' ''.                                        "V1
  PERFORM alv_layout USING 14 'Consumption Other Than Production' 'COSPRD' 'it_final' ' ' '' '' ''.                                        "V1
  PERFORM alv_layout USING 15 'Current month Total PRs Qty' 'TOTPR' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 16 'Current month receipt' 'CURMNT1' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 17 'Excess stock for current month' 'EXCSTK' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 18 'Next month total consumption' 'NXTMNT' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 19 'Next month Total PRs Qty' 'NXTPR' 'it_final' ' ' '' '' ''.
  PERFORM alv_layout USING 20 'Open PO Qty to be delivered' 'OPPOQTY' 'it_final' ' ' '' '' ''.                        "V1
  PERFORM alv_layout USING 21 'Run Date' 'CURDATE' 'it_final' ' ' '' '' ''.                                           "V1
  PERFORM alv_layout USING 22 'Run time' 'CURTIME' 'it_final' ' ' '' '' ''.                                          "V1

*&---------------------------------------------------------------------*
*&   ALV Display
*&---------------------------------------------------------------------*


  PERFORM alv_grid_display.

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_1326   text
*      -->P_1327   text
*      -->P_1328   text
*      -->P_1329   text
*      -->P_1330   text
*      -->P_1331   text
*----------------------------------------------------------------------*
FORM alv_layout  USING    p1 p2 p3 p4 p5 p6 p7 p8.
  CLEAR wa_fcat.
  wa_fcat-col_pos = p1.
  wa_fcat-seltext_l = p2.
  wa_fcat-fieldname = p3.
  wa_fcat-tabname = p4.
  wa_fcat-do_sum = p5.
  wa_fcat-no_zero = p6.
  wa_fcat-no_out = p7.
  wa_fcat-outputlen = p8.
  APPEND wa_fcat TO it_fcat.
ENDFORM.                    " ALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid_display .

*  layout-colwidth_optimize = 'X'.
*  layout-zebra = 'X'.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program          = sy-repid
*      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
*      is_layout                   = layout
*      it_fieldcat                 = it_fcat[]
*    TABLES
*      t_outtab                    = it_final[]
*    EXCEPTIONS
*      program_error               = 1
*      OTHERS                      = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.



  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK              = ' '
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                = ' '
     i_callback_program             = sy-repid
*   I_CALLBACK_PF_STATUS_SET       = ' '
*   I_CALLBACK_USER_COMMAND        = ' '
*   I_STRUCTURE_NAME               =
     is_layout                      = layout
     it_fieldcat                    = it_fcat[]
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
*   IT_SORT                        =
*   IT_FILTER                      =
*   IS_SEL_HIDE                    =
*   I_DEFAULT                      = 'X'
   i_save                         = 'A'
*   IS_VARIANT                     =
   it_events                      = it_evt
*   IT_EVENT_EXIT                  =
*   IS_PRINT                       =
*   IS_REPREP_ID                   =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
*   IR_SALV_LIST_ADAPTER           =
*   IT_EXCEPT_QINFO                =
*   I_SUPPRESS_EMPTY_DATA          = ABAP_FALSE
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                       = it_final[]
   EXCEPTIONS
     program_error                  = 1
     OTHERS                         = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  top_of_page_split
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_TOP      text
*----------------------------------------------------------------------*

FORM top_of_page.
  ULINE AT 98(105) .
  WRITE:/98 sy-vline , 145 'Current Values' , 202 sy-vline.
ENDFORM.                    "top_of_page


*&---------------------------------------------------------------------*
*&      Form  end_of_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM end_of_list.
*----------------------------- +V8------------------------------------------*
  WRITE:/1 sy-vline , 3 'Current and Next Month PR quantity is inclusion of  quantity converted to PO.' , 423 sy-vline.
  WRITE :/1 sy-vline , 423 sy-vline.
  ULINE AT 2(421).
*----------------------------- +V8------------------------------------------*
ENDFORM.                    "end_of_list
