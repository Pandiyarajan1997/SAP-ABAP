*&---------------------------------------------------------------------*
*& Report ZFI_POGRNINV_PENDING_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_pogrninv_pending_report.

TABLES: ekpo, ekko, ekbe,eban.
DATA repid TYPE sy-repid.
TYPE-POOLS: cxtab, slis.
DATA: control_cols TYPE cxtab_column.
DATA: gd_ucomm TYPE sy-ucomm.

TYPE-POOLS: cxtab, slis.

DATA: w_fcat                 TYPE slis_fieldcat_alv,
      i_fcat                 TYPE slis_t_fieldcat_alv,
      layout                 TYPE slis_layout_alv,
*      g_grid  TYPE REF TO cl_gui_alv_grid,
      g_top_of_page          TYPE slis_formname VALUE 'F_TOP_OF_PAGE', "for avl header.
      gt_callback_subroutine TYPE slis_formname VALUE 'USER_COMMAND',
      lv_run                 TYPE i VALUE '0'.



SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS:   d_aedat FOR ekbe-budat DEFAULT sy-datum MODIF ID 6.
  PARAMETERS : rm1 RADIOBUTTON GROUP r3 DEFAULT 'X' USER-COMMAND pc, " PO pending
               rm2 RADIOBUTTON GROUP r3,                             " GRN Pending
               rm3 RADIOBUTTON GROUP r3,                             " Invoice Pending
               rm4 RADIOBUTTON GROUP r3,                             " PR Release Status.
               rm5 RADIOBUTTON GROUP r3.                             " PO GRN FI.
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001. " Invoice pending
*        Select-OPTIONS:   D_AEDAT FOR EKBE-BUDAT.
    SELECTION-SCREEN SKIP 1.
    PARAMETERS : rb1 RADIOBUTTON GROUP r1 DEFAULT 'X' MODIF ID 1,
                 rb2 RADIOBUTTON GROUP r1 MODIF ID 1.
    SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN: END OF BLOCK b1.
  SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002. " GRN Pending
*         Select-OPTIONS:   D_BEDAT FOR EKBE-BUDAT.
    SELECTION-SCREEN SKIP 1.
    PARAMETERS : rb3 RADIOBUTTON GROUP r2 DEFAULT 'X'  MODIF ID 2, " Created Date.
                 rb4 RADIOBUTTON GROUP r2  MODIF ID 2.             " Doc. Date.
    SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN: END OF BLOCK b2.

  SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004. " PO Pending
*         Select-OPTIONS:   D_BEDAT FOR EKBE-BUDAT.
    SELECTION-SCREEN SKIP 1.
    SELECTION-SCREEN COMMENT 1(74) TEXT-005 MODIF ID 3.
*                 parameters : RB5 RADIOBUTTON GROUP R4  MODIF ID 3. " Created Date.

    SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN: END OF BLOCK b3.
  SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-006. " PR release status.
*         Select-OPTIONS:   D_BEDAT FOR EKBE-BUDAT.
    SELECTION-SCREEN SKIP 1.
    SELECTION-SCREEN COMMENT 1(74) TEXT-005 MODIF ID 4.
    SELECT-OPTIONS:   d_banfn FOR eban-banfn MODIF ID 4,
                      d_ekgrp FOR eban-ekgrp MODIF ID 4.

    SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: END OF BLOCK b5.

*d_ucomm = sy-ucomm.
SELECTION-SCREEN: BEGIN OF BLOCK bb WITH FRAME TITLE TEXT-008.
  SELECTION-SCREEN: BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-007. " PO GRN MIRO.
*         Select-OPTIONS:   D_BEDAT FOR EKBE-BUDAT.
    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS:   d_ebeln FOR ekpo-ebeln MODIF ID 5,
                      p_aedat FOR ekbe-budat MODIF ID 5.
    SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN: END OF BLOCK b6.
SELECTION-SCREEN:END OF BLOCK bb.

INITIALIZATION.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'RM4'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  IF rm1 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '1' OR screen-group1 = '2' OR screen-group1 = '4' OR screen-group1 = '5'.
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF rm2 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '3' OR screen-group1 = '1' OR screen-group1 = '4' OR screen-group1 = '5'.
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF rm3 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '3' OR screen-group1 = '2' OR screen-group1 = '4' OR screen-group1 = '5'.
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF rm4 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '2' OR screen-group1 = '3' OR screen-group1 = '1' OR screen-group1 = '5'.
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF rm5 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '2' OR screen-group1 = '3' OR screen-group1 = '1' OR screen-group1 = '4' OR screen-group1 = '6'.
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

START-OF-SELECTION.

  TYPES: BEGIN OF ty_ekbe,
           ebeln TYPE ekbe-ebeln,    " Purchasing Document
           ebelp TYPE ekbe-ebelp,    " Item
           gjahr TYPE ekbe-gjahr,    " Material Doc. Year
           belnr TYPE ekbe-belnr,    " Material Document
           buzei TYPE ekbe-buzei,    " Material Doc.Item
           bewtp TYPE ekbe-bewtp,    " PO History Category
           bwart TYPE ekbe-bwart,    " Movement Type"
           budat TYPE ekbe-budat,    " Posting date.
           cpudt TYPE ekbe-cpudt,    " Entry Date.
           menge TYPE ekbe-menge,    " Quantity
           bpmng TYPE ekbe-bpmng,    " Quantity in OPUn
           dmbtr TYPE ekbe-dmbtr,    " Amount in LC
           waers TYPE ekbe-waers,    " Currency
           lfbnr TYPE ekbe-lfbnr,    " Document No. of a Reference Document
           lfpos TYPE ekbe-lfpos,    " Ref Item
           lfgja TYPE ekbe-lfgja,    " Fisc. Year Ref. Doc.
           lifnr TYPE ekko-lifnr,    " Vendor NO.
           name1 TYPE lfa1-name1,    " Vendor Name
         END OF ty_ekbe.

  DATA: count(3)  TYPE c,
        count1(3) TYPE c,
        gr(3)     TYPE c,
        ir(3)     TYPE c,
        grnq      TYPE i,
        poq       TYPE i,
        up        TYPE i,
        count2(3) TYPE c.

  TYPES: BEGIN OF ty_ekko,
           ebeln TYPE ekko-ebeln,  " Purchasing Doc.
           bukrs TYPE ekko-bukrs,  " Company Code
           bsart TYPE ekko-bsart,  " Document Type
           aedat TYPE ekko-aedat,  " Created on
           bedat TYPE ekko-bedat,  " Document Date
           ekorg TYPE ekko-ekorg,  " Purchasing Org.
           ekgrp TYPE ekko-ekgrp,  " Purch. Group
           lifnr TYPE ekko-lifnr,  " Vendor NO
         END OF ty_ekko.

  TYPES: BEGIN OF ty_ekpo,
           aedat TYPE ekpo-aedat,  " Created Date
           ebeln TYPE ekpo-ebeln,  " Purchasing Document
           ebelp TYPE ekpo-ebelp,  " Item
           matnr TYPE ekpo-matnr,  " Material
           txz01 TYPE ekpo-txz01,  " Short Text
           lgort TYPE ekpo-lgort,  " Storage Location
           menge TYPE ekpo-menge,  " Order Quantity
           meins TYPE ekpo-meins,  " Order Unit
           netpr TYPE ekpo-netpr,  " Net Order Price
           netwr TYPE ekpo-netwr,  " Net Order Value
         END OF ty_ekpo.

  TYPES: BEGIN OF ty_finalpo,
*  EBELN TYPE EKKO-EBELN,  " Purchasing Doc.
           bukrs TYPE ekko-bukrs,  " Company Code
           bsart TYPE ekko-bsart,  " Document Type
           aedat TYPE ekko-aedat,  " Created on
           bedat TYPE ekko-bedat,  " Document Date
           ekorg TYPE ekko-ekorg,  " Purchasing Org.
           ekgrp TYPE ekko-ekgrp,  " Purch. Group
*  AEDAT TYPE EKPO-AEDAT,  " Created Date
           ebeln TYPE ekpo-ebeln,  " Purchasing Document
           ebelp TYPE ekpo-ebelp,  " Item
           matnr TYPE ekpo-matnr,  " Material
           txz01 TYPE ekpo-txz01,  " Short Text
           lgort TYPE ekpo-lgort,  " Storage Location
           menge TYPE ekpo-menge,  " Order Quantity
           bqty  TYPE ekpo-menge,   " Balance Qty
           meins TYPE ekpo-meins,  " Order Unit
           netpr TYPE ekpo-netpr,  " Net Order Price
           netwr TYPE ekpo-netwr,  " Net Order Value
           lifnr TYPE ekko-lifnr,  " Vendor NO.
           name1 TYPE lfa1-name1,  " Vendor Name

         END OF ty_finalpo.



  DATA: it_ekko     TYPE TABLE OF ty_ekko,
        wa_ekko     TYPE ty_ekko,
        it_finalpo  TYPE TABLE OF ty_finalpo,
        wa_finalpo  TYPE ty_finalpo,
        it_finalpo1 TYPE TABLE OF ty_finalpo,
        wa_finalpo1 TYPE ty_finalpo,
        it_ekpo     TYPE TABLE OF ty_ekpo,
        wa_ekpo     TYPE ty_ekpo,
        it_ekpo1    TYPE TABLE OF ty_ekpo,
        wa_ekpo1    TYPE ty_ekpo,
        it_ekbe     TYPE TABLE OF ty_ekbe,
        wa_ekbe     TYPE ty_ekbe,
        it_ekbe1    TYPE TABLE OF ty_ekbe,
        wa_ekbe1    TYPE ty_ekbe,
        it_ekbe2    TYPE TABLE OF ty_ekbe,
        wa_ekbe2    TYPE ty_ekbe,
        it_ekbe3    TYPE TABLE OF ty_ekbe,
        wa_ekbe3    TYPE ty_ekbe,
        it_mseg     TYPE TABLE OF ty_mseg,
        wa_mseg     TYPE ty_mseg.
  DATA: v_name TYPE lfa1-name1,
        v_no   TYPE ekko-lifnr.



  TYPES: BEGIN OF ty_eban,
           banfn TYPE eban-banfn,  " Purchase Requisition
           bnfpo TYPE eban-bnfpo,  " Item of Requisition
           bsart TYPE eban-bsart,  " Document Type
           ekgrp TYPE eban-ekgrp,  " Purchasing Group
           ernam TYPE eban-ernam,  " Created by
           erdat TYPE eban-erdat,  " Changed on
           afnam TYPE eban-afnam,  " Requisitioner
           txz01 TYPE eban-txz01,  " Short Text
           matnr TYPE eban-matnr,  " Material
           ematn TYPE eban-ematn,  " MPN Material
           werks TYPE eban-werks,  " Plant
           lgort TYPE eban-lgort,  " Storage Location
           bednr TYPE eban-bednr,  " Req. Tracking Number
           matkl TYPE eban-matkl,  " Material Group
           menge TYPE eban-menge,  " Quantity Requested
           meins TYPE eban-meins,  " Unit of Measure
           badat TYPE eban-badat,  " Requisition Date
           lfdat TYPE eban-lfdat,  " Delivery Date
           preis TYPE eban-preis,  " Valuation Price
           bsmng TYPE eban-bsmng,  " PO Quantity Ordered

         END OF ty_eban.

  DATA: it_eban  TYPE TABLE OF ty_eban,
        wa_eban  TYPE ty_eban,
        it_eban1 TYPE TABLE OF ty_eban,
        wa_eban1 TYPE ty_eban.
  TYPES: BEGIN OF ty_prr,
           banfn TYPE eban-banfn,  " Purchase Requisition
           bsart TYPE eban-bsart,  " Document Type
           ekgrp TYPE eban-ekgrp,  " Purchasing Group
           ernam TYPE eban-ernam,  " Created by
           erdat TYPE eban-erdat,  " Changed on
           werks TYPE eban-werks,  " Plant
           badat TYPE eban-badat,  " Requisition Date
           lfdat TYPE eban-lfdat,  " Delivery Date
           eknam TYPE t024-eknam,
           rn1   TYPE cdhdr-username,    " Release Name 1
           rn2   TYPE cdhdr-username,    " Release Name 2
           rn3   TYPE cdhdr-username,    " Release Name 3
           rn4   TYPE cdhdr-username,    " Release Name 4
           rd1   TYPE cdhdr-udate,    " Release date 1
           rd2   TYPE cdhdr-udate,    " Release date 2
           rd3   TYPE cdhdr-udate,    " Release date 3
           rd4   TYPE cdhdr-udate,    " Release date 4
           rt1   TYPE cdhdr-utime,    " Release Time 1
           rt2   TYPE cdhdr-utime,    " Release Time 2
           rt3   TYPE cdhdr-utime,    " Release Time 3
           rt4   TYPE cdhdr-utime,    " Release Time 4
         END OF ty_prr.

  TYPES: BEGIN OF ty_cdhdr,
           objectid TYPE cdhdr-objectid,
           changenr TYPE cdhdr-changenr,
           username TYPE cdhdr-username,
           udate    TYPE cdhdr-udate,
           utime    TYPE cdhdr-utime,
           tcode    TYPE cdhdr-tcode,
         END OF ty_cdhdr.

  DATA: it_prr   TYPE TABLE OF ty_prr,
        wa_prr   TYPE ty_prr,
        it_prr1  TYPE TABLE OF ty_prr,
        wa_prr1  TYPE ty_prr,
        it_cdhdr TYPE TABLE OF ty_cdhdr,
        wa_cdhdr TYPE ty_cdhdr.
  DATA: pri(3)  TYPE c,
        pri2(3) TYPE c,
        pri1(3) TYPE c,
        dat(15) TYPE c,
        cou1    TYPE i,
        cou2    TYPE i,
        s       TYPE i.

  TYPES: BEGIN OF ty_po,
           ebeln TYPE ekko-ebeln,  " Purchasing Doc.
           bedat TYPE ekko-bedat,  " Document Date
           ekgrp TYPE ekko-ekgrp,  " Purch. Group
           lifnr TYPE ekko-lifnr,  " Vendor NO
           ebelp TYPE ekpo-ebelp,  " Item
           matnr TYPE ekpo-matnr,  " Material
           txz01 TYPE ekpo-txz01,  " Short Text
*  LOEKZ TYPE EKPO-LOEKZ,  " Delete indicator
           pstyp TYPE ekpo-pstyp,  " Technical Information
           netwr TYPE ekpo-netwr,  " PO Amount
         END OF ty_po.
  TYPES: BEGIN OF ty_grn,
           mblnr TYPE  mseg-mblnr,      " Material Document
           mjahr TYPE  mseg-mjahr,      " Material Doc. Year
           zeile TYPE  mseg-zeile,      " Material Doc.Item
           bwart TYPE  mseg-bwart,      " Movement Type
           lfbja TYPE  mseg-lfbja,      " Fisc. Year Ref. Doc.
           lfbnr TYPE  mseg-lfbnr,      " Reference Document
           lfpos TYPE  mseg-lfpos,      " Reference Doc. Item
         END OF ty_grn.

  TYPES: BEGIN OF ty_ss,
           lblni TYPE  essr-lblni,   " Entry Sheet
*  LOEKZ TYPE  ESSR-LOEKZ,   " Deletion indicator
*  KZABN TYPE  ESSR-KZABN,   " Acceptance

         END OF ty_ss.

  TYPES: BEGIN OF ty_inv,
           belnr TYPE rseg-belnr,     "  Document Number
           gjahr TYPE rseg-gjahr,     "  Fiscal Year
           buzei TYPE rseg-buzei,     "  Invoice Item
           lfbnr TYPE rseg-lfbnr,     "  Reference Document
           lfgja TYPE rseg-lfgja,     "  Year current period
           lfpos TYPE rseg-lfpos,     "  Reference Doc. Item
           shkzg TYPE rseg-shkzg,     "  Debit/Credit Ind.
           wrbtr TYPE rseg-wrbtr,     " Amount
         END OF ty_inv.

  TYPES: BEGIN OF ty_fi,
           belnr TYPE rseg-belnr,     "  Document Number
           gjahr TYPE rseg-gjahr,     "  Fiscal Year
           bukrs TYPE bkpf-bukrs,     " Company code
         END OF ty_fi.

  TYPES: BEGIN OF ty_pof,
           ebeln   TYPE ekko-ebeln,  " Purchasing Doc.
           bedat   TYPE ekko-bedat,  " Document Date
           ekgrp   TYPE ekko-ekgrp,  " Purch. Group
           lifnr   TYPE ekko-lifnr,  " Vendor NO
           ebelp   TYPE ekpo-ebelp,  " Item
           matnr   TYPE ekpo-matnr,  " Material
           txz01   TYPE ekpo-txz01,  " Short Text
           netwr   TYPE ekpo-netwr,  " PO Amount
           mblnr   TYPE  mseg-mblnr,    " Material Document
           mjahr   TYPE  mseg-mjahr,    " Material Doc. Year
           zeile   TYPE  mseg-zeile,    " Material Doc.Item
           lfbnr   TYPE  mseg-lfbnr,      " Reference Document
           lfpos   TYPE  mseg-lfpos,      " Reference Doc. Item
           belnr   TYPE rseg-belnr,     "  Document Number
           gjahr   TYPE rseg-gjahr,     "  Fiscal Year
           wrbtr   TYPE rseg-wrbtr,     " Amount
***************************** 04/08.2018 Start viknesh
           belnr1  TYPE rseg-belnr,     "  Document Number
           gjahr1  TYPE rseg-gjahr,     "  Fiscal Year
           bukrs   TYPE bkpf-bukrs,     " Company code
***************************** 04/08.2018 End viknesh
           buzei   TYPE rseg-buzei,     "  Invoice Item
           pot(18) TYPE c,            "  Doc type
           lblni   TYPE  essr-lblni,    " Entry Sheet
           sno     TYPE i,              " Auto SNO
           name1   TYPE lfa1-name1,  " Vendor Name
         END OF ty_pof.

  DATA: it_po   TYPE TABLE OF ty_po,
        wa_po   TYPE ty_po,
        it_pof  TYPE TABLE OF ty_pof,
        wa_pof  TYPE ty_pof,
        it_grn  TYPE TABLE OF ty_grn,
        wa_grn  TYPE ty_grn,
        it_grn1 TYPE TABLE OF ty_grn,
        wa_grn1 TYPE ty_grn,
        it_fi   TYPE TABLE OF ty_fi,
        wa_fi   TYPE ty_fi,
        it_inv  TYPE TABLE OF ty_inv,
        wa_inv  TYPE ty_inv,
        it_inv1 TYPE TABLE OF ty_inv,
        wa_inv1 TYPE ty_inv,
        it_inv2 TYPE TABLE OF ty_inv,
        wa_inv2 TYPE ty_inv,
        it_ss   TYPE TABLE OF ty_ss,
        wa_ss   TYPE ty_ss,
        it_pof1 TYPE TABLE OF ty_pof,
        wa_pof1 TYPE ty_pof.

  IF rm2 IS NOT INITIAL.
    IF rb3 IS NOT INITIAL.
      SELECT ek~aedat ek~ekgrp ek~bedat ek~lifnr e~ebeln e~ebelp e~matnr e~txz01 e~lgort e~menge e~meins e~netpr e~netwr INTO CORRESPONDING FIELDS OF TABLE it_finalpo
      FROM ekko AS ek INNER JOIN ekpo AS e ON ek~ebeln = e~ebeln WHERE ek~aedat IN d_aedat AND ek~bsart NE 'ZSR'.
    ELSE.
      SELECT ek~aedat ek~ekgrp ek~bedat ek~lifnr e~ebeln e~ebelp e~matnr e~txz01 e~lgort e~menge e~meins e~netpr e~netwr INTO CORRESPONDING FIELDS OF TABLE it_finalpo
      FROM ekko AS ek INNER JOIN ekpo AS e ON ek~ebeln = e~ebeln WHERE ek~bedat IN d_aedat AND ek~bsart NE 'ZSR'.
    ENDIF.
    LOOP AT it_finalpo INTO wa_finalpo.

      SELECT ebeln ebelp gjahr belnr buzei bewtp bwart budat cpudt menge bpmng dmbtr waers lfbnr lfpos lfgja
      FROM ekbe  INTO TABLE it_ekbe WHERE ebeln = wa_finalpo-ebeln AND ebelp = wa_finalpo-ebelp.
      SELECT SINGLE name1 FROM lfa1 INTO  v_name  WHERE lifnr = wa_finalpo-lifnr.

      poq = wa_finalpo-menge.
      grnq = 0.
      up = 0.
      LOOP AT it_ekbe INTO wa_ekbe WHERE bewtp = 'E'.

        IF wa_ekbe-bwart = '101'.

          grnq = grnq + wa_ekbe-menge.
          up = 1.
        ELSEIF wa_ekbe-bwart = '122'.
          grnq = grnq - wa_ekbe-menge.
          up = 1.
        ENDIF.
      ENDLOOP.
      IF poq GT grnq AND ( up = 1 OR up = 0 ).
        IF  grnq = 0.
          wa_finalpo-bqty = poq.
        ELSE.
          wa_finalpo-bqty = grnq.
          wa_finalpo-netwr = wa_finalpo-netpr * wa_finalpo-bqty.
        ENDIF.
        wa_finalpo-name1 = v_name.
        APPEND wa_finalpo TO it_finalpo1.
      ENDIF.
    ENDLOOP.
    CLEAR it_finalpo.
    it_finalpo[] = it_finalpo1[].


    w_fcat-fieldname = 'EBELN'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '1'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'EBELP'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '2'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'AEDAT'.
    w_fcat-ref_tabname = 'EKKO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '3'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BEDAT'.
    w_fcat-ref_tabname = 'EKKO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '4'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'EKGRP'.
    w_fcat-ref_tabname = 'EKKO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '5'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'MATNR'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '6'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'TXZ01'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '40'.
    w_fcat-col_pos = '7'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'LGORT'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '6'.
    w_fcat-col_pos = '8'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'MENGE'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '9'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BQTY'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-seltext_m = 'Balance Qty'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '10'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'MEINS'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '11'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'NETPR'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '12'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'NETWR'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '13'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'LIFNR'.
    w_fcat-ref_tabname = 'EKKO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '14'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'Name1'.
    w_fcat-ref_tabname = 'LFA1'.
    w_fcat-seltext_m = 'Vendor Name'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '35'.
    w_fcat-col_pos = '15'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*
        i_callback_program = repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = 'HEADER'
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = 'PO BOOKED AND GRN PENDING REPORT'
*       I_GRID_SETTINGS    =
        is_layout          = layout
        it_fieldcat        = i_fcat
      TABLES
        t_outtab           = it_finalpo.


  ENDIF.


  IF rm3 IS NOT INITIAL.

    IF rb1 IS INITIAL.

      SELECT ebeln ebelp gjahr belnr buzei bewtp bwart budat cpudt menge bpmng dmbtr waers lfbnr lfpos lfgja
      FROM ekbe INTO TABLE it_ekbe WHERE cpudt IN d_aedat AND bwart = '101'.
      LOOP AT it_ekbe INTO wa_ekbe.

        SELECT ebeln ebelp gjahr belnr buzei bewtp bwart budat cpudt menge bpmng dmbtr waers lfbnr lfpos lfgja
        FROM ekbe INTO TABLE it_ekbe1 WHERE lfbnr = wa_ekbe-belnr AND lfgja = wa_ekbe-gjahr AND lfpos = wa_ekbe-buzei AND ( bwart = '102' OR bwart = '122' ).
        APPEND LINES OF it_ekbe1 TO it_ekbe2.
        CLEAR it_ekbe1.
        SELECT ebeln ebelp gjahr belnr buzei bewtp bwart budat cpudt menge bpmng dmbtr waers lfbnr lfpos lfgja
        FROM ekbe INTO TABLE it_ekbe1 WHERE lfbnr = wa_ekbe-belnr AND lfgja = wa_ekbe-gjahr AND lfpos = wa_ekbe-buzei AND bewtp = 'Q'.
        APPEND LINES OF it_ekbe1 TO it_ekbe3.
        CLEAR it_ekbe1.
      ENDLOOP.
      it_ekbe1[] = it_ekbe2[].
      CLEAR it_ekbe2[].
      it_ekbe2[] = it_ekbe3[].
      CLEAR it_ekbe3[].

*      SELECT EBELN EBELP GJAHR BELNR BUZEI BEWTP BWART BUDAT CPUDT MENGE BPMNG DMBTR WAERS lfbnr LFPOS LFGJA
*             FROM EKBE INTO TABLE IT_EKBE1 WHERE CPUDT IN D_AEDAT and bwart = '102' or bwart = '122'.
*
*      SELECT EBELN EBELP GJAHR BELNR BUZEI BEWTP BWART BUDAT CPUDT MENGE BPMNG DMBTR WAERS lfbnr LFPOS LFGJA
*             FROM EKBE INTO TABLE IT_EKBE2 WHERE CPUDT IN D_AEDAT and BEWTP = 'Q'.

    ELSE.
      SELECT ebeln ebelp gjahr belnr buzei bewtp bwart budat cpudt menge bpmng dmbtr waers lfbnr lfpos lfgja
      FROM ekbe INTO TABLE it_ekbe WHERE budat IN d_aedat AND bwart = '101'.

      LOOP AT it_ekbe INTO wa_ekbe.

        SELECT ebeln ebelp gjahr belnr buzei bewtp bwart budat cpudt menge bpmng dmbtr waers lfbnr lfpos lfgja
        FROM ekbe INTO TABLE it_ekbe1 WHERE lfbnr = wa_ekbe-belnr AND lfgja = wa_ekbe-gjahr AND lfpos = wa_ekbe-buzei AND ( bwart = '102' OR bwart = '122' ).
        APPEND LINES OF it_ekbe1 TO it_ekbe2.
        CLEAR it_ekbe1.
        SELECT ebeln ebelp gjahr belnr buzei bewtp bwart budat cpudt menge bpmng dmbtr waers lfbnr lfpos lfgja
        FROM ekbe INTO TABLE it_ekbe1 WHERE lfbnr = wa_ekbe-belnr AND lfgja = wa_ekbe-gjahr AND lfpos = wa_ekbe-buzei AND bewtp = 'Q'.
        APPEND LINES OF it_ekbe1 TO it_ekbe3.
        CLEAR it_ekbe1.
      ENDLOOP.
      it_ekbe1[] = it_ekbe2[].
      CLEAR it_ekbe2[].
      it_ekbe2[] = it_ekbe3[].
      CLEAR it_ekbe3[].
*      SELECT EBELN EBELP GJAHR BELNR BUZEI BEWTP BWART BUDAT CPUDT MENGE BPMNG DMBTR WAERS lfbnr LFPOS LFGJA
*             FROM EKBE INTO TABLE IT_EKBE1 WHERE BUDAT IN D_AEDAT and bwart = '102' or bwart = '122'.
*
*      SELECT EBELN EBELP GJAHR BELNR BUZEI BEWTP BWART BUDAT CPUDT MENGE BPMNG DMBTR WAERS lfbnr LFPOS LFGJA
*             FROM EKBE INTO TABLE IT_EKBE2 WHERE BUDAT IN D_AEDAT and BEWTP = 'Q'.

    ENDIF.

    LOOP AT  it_ekbe INTO wa_ekbe.
      SELECT SINGLE lifnr FROM ekko INTO v_no WHERE ebeln = wa_ekbe-ebeln.
      SELECT SINGLE name1 FROM lfa1 INTO v_name WHERE lifnr = v_no.
      gr = 0.
*            Count2 = 0.
      LOOP AT it_ekbe1 INTO wa_ekbe1 WHERE lfpos = wa_ekbe-buzei AND lfbnr = wa_ekbe-belnr AND lfgja = wa_ekbe-gjahr.
        gr = gr + 1.
*                  count2 = 1.
      ENDLOOP.
*                  count1 = count mod 2.
      IF gr EQ 0 ."and count2 = 1 .
        ir = 0.
        LOOP AT it_ekbe2 INTO wa_ekbe2 WHERE lfpos = wa_ekbe-buzei AND lfbnr = wa_ekbe-belnr AND lfgja = wa_ekbe-gjahr.
          ir = ir + 1.
*                         count1 = 1.
        ENDLOOP.
      ENDIF.
      count = ir MOD 2.
      IF gr EQ 0 AND count EQ 0.
        wa_ekbe-lifnr = v_no.
        wa_ekbe-name1 = v_name.
        APPEND wa_ekbe TO it_ekbe3.
      ENDIF.

*                  ENDIF.

    ENDLOOP.
    CLEAR it_ekbe.
    it_ekbe[] = it_ekbe3[].



    w_fcat-fieldname = 'EBELN'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '1'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'EBELP'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '2'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'GJAHR'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '3'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BELNR'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '4'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BUZEI'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '5'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'BUDAT'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '6'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'CPUDT'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '7'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'MENGE'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '8'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'BPMNG'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '9'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'DMBTR'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '14'.
    w_fcat-col_pos = '10'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'WAERS'.
    w_fcat-ref_tabname = 'EKBE'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '11'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'LIFNR'.
    w_fcat-ref_tabname = 'EKKO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '12'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'Name1'.
    w_fcat-ref_tabname = 'LFA1'.
    w_fcat-seltext_m = 'Vendor Name'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '35'.
    w_fcat-col_pos = '13'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*
        i_callback_program = repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = 'HEADER'
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = 'GRN BOOKED AND INVOICE PENDING REPORT'
*       I_GRID_SETTINGS    =
        is_layout          = layout
        it_fieldcat        = i_fcat
      TABLES
        t_outtab           = it_ekbe.
  ENDIF.

  IF rm1 IS NOT INITIAL.

    SELECT  banfn  bnfpo bsart ekgrp ernam erdat afnam txz01 matnr ematn werks lgort bednr matkl menge meins
    badat  lfdat preis bsmng FROM eban INTO TABLE it_eban  WHERE loekz NE 'X' AND badat IN d_aedat AND frgkz EQ 'Z'.

    LOOP AT it_eban INTO wa_eban.
      IF wa_eban-menge NE wa_eban-bsmng.
        APPEND wa_eban TO it_eban1.
      ENDIF.
    ENDLOOP.

    w_fcat-fieldname = 'BANFN'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '1'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'BNFPO'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '2'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BSART'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '3'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'EKGRP'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '4'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BADAT'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '5'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'LFDAT'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '6'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'MATNR'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '7'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'TXZ01'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '40'.
    w_fcat-col_pos = '8'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'EMATN'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '22'.
    w_fcat-col_pos = '9'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'WERKS'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '6'.
    w_fcat-col_pos = '10'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'LGORT'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '6'.
    w_fcat-col_pos = '11'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'MENGE'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '12'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'MEINS'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '13'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'PREIS'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '14'.
    w_fcat-col_pos = '14'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'BEDNR'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '15'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'MATKL'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '6'.
    w_fcat-col_pos = '16'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'AFNAM'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '20'.
    w_fcat-col_pos = '17'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'ERNAM'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '18'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'ERDAT'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '19'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*
        i_callback_program = repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = 'HEADER'
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = 'PR BOOKED AND PO PENDING REPORT'
*       I_GRID_SETTINGS    =
        is_layout          = layout
        it_fieldcat        = i_fcat
      TABLES
        t_outtab           = it_eban1.
  ENDIF.

  IF rm4 IS NOT INITIAL.
    IF d_banfn IS NOT INITIAL AND d_ekgrp IS INITIAL. " PR No given.
      SELECT  e~banfn e~bsart e~ekgrp e~ernam e~erdat e~werks e~badat e~lfdat t~eknam INTO TABLE   it_prr
        FROM eban AS e INNER JOIN t024 AS t ON e~ekgrp = t~ekgrp WHERE e~loekz NE 'X' AND e~badat IN d_aedat
      AND e~banfn IN d_banfn.

    ELSEIF d_banfn IS INITIAL AND d_ekgrp IS NOT INITIAL. " PR Purchase group given.
      SELECT  e~banfn e~bsart e~ekgrp e~ernam e~erdat e~werks e~badat e~lfdat t~eknam INTO TABLE it_prr
        FROM eban AS e INNER JOIN t024 AS t ON e~ekgrp = t~ekgrp WHERE e~loekz NE 'X' AND e~badat IN d_aedat
      AND e~ekgrp IN d_ekgrp.

    ELSEIF d_banfn IS NOT INITIAL AND d_ekgrp IS NOT INITIAL. " Both PR NO and Purchase Group given.
      SELECT  e~banfn e~bsart e~ekgrp e~ernam e~erdat e~werks e~badat e~lfdat t~eknam INTO TABLE it_prr
        FROM eban AS e INNER JOIN t024 AS t ON e~ekgrp = t~ekgrp WHERE e~loekz NE 'X' AND e~badat IN d_aedat
      AND e~ekgrp IN d_ekgrp AND e~banfn IN d_banfn.

    ELSEIF d_banfn IS INITIAL AND d_ekgrp IS INITIAL. " Both not given.
      SELECT  e~banfn e~bsart e~ekgrp e~ernam e~erdat e~werks e~badat e~lfdat t~eknam INTO TABLE it_prr
      FROM eban AS e INNER JOIN t024 AS t ON e~ekgrp = t~ekgrp WHERE e~loekz NE 'X' AND e~badat IN d_aedat.

    ENDIF.

    LOOP AT it_prr INTO wa_prr.
      COLLECT wa_prr  INTO it_prr1.
    ENDLOOP.
    CLEAR it_prr.
    it_prr[] = it_prr1[].
    CLEAR it_prr1.

    LOOP AT it_prr INTO wa_prr.
      SELECT objectid changenr username udate utime tcode FROM cdhdr INTO TABLE it_cdhdr WHERE objectid = wa_prr-banfn.
      pri1 = 0.
      pri = 0.
      DESCRIBE TABLE it_cdhdr.
      pri = sy-tfill.
      LOOP AT it_cdhdr INTO wa_cdhdr.
        pri1 = pri1 + 1 .
        IF pri1 LE pri.
          IF pri1 = 1.
            wa_prr-rn1 = wa_cdhdr-username .
            wa_prr-rd1 = wa_cdhdr-udate.
            wa_prr-rt1 = wa_cdhdr-utime.
          ELSEIF pri1 = 2.
            wa_prr-rn2 = wa_cdhdr-username.
            wa_prr-rd2 = wa_cdhdr-udate.
            wa_prr-rt2 = wa_cdhdr-utime.
          ELSEIF pri1 = 3.
            wa_prr-rn3 = wa_cdhdr-username.
            wa_prr-rd3 = wa_cdhdr-udate.
            wa_prr-rt3 = wa_cdhdr-utime.
          ELSEIF pri1 = 4.
            wa_prr-rn4 = wa_cdhdr-username.
            wa_prr-rd4 = wa_cdhdr-udate.
            wa_prr-rt4 = wa_cdhdr-utime.
          ENDIF.
        ENDIF.

      ENDLOOP.
      APPEND wa_prr TO it_prr1.
    ENDLOOP.

    w_fcat-fieldname = 'BANFN'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '1'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'BSART'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '2'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'EKGRP'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '3'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'ERNAM'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '4'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'ERDAT'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '5'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'WERKS'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '6'.
    w_fcat-col_pos = '6'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'BADAT'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '7'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'LFDAT'.
    w_fcat-ref_tabname = 'EBAN'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '8'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'EKNAM'.
    w_fcat-ref_tabname = 'T024'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '20'.
    w_fcat-col_pos = '9'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'RN1'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '1st Release Name '.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '14'.
    w_fcat-col_pos = '10'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'RD1'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '1St Release Date'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '11'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'RT1'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '1St Release Time'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '8'.
    w_fcat-col_pos = '12'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'RN2'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '2nd Release Name'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '14'.
    w_fcat-col_pos = '13'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'RD2'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '2nd Release Date'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '14'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'RT2'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '2nd Release Time'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '8'.
    w_fcat-col_pos = '15'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'RN3'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '3rd Release Name'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '14'.
    w_fcat-col_pos = '16'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'RD3'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '3rd Release Date'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '17'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'RT3'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '3rd Release Time'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '8'.
    w_fcat-col_pos = '18'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'RN4'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '4th Release Name'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '14'.
    w_fcat-col_pos = '19'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.


    w_fcat-fieldname = 'RD4'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '4th Release Date'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '20'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'RT4'.
*  W_FCAT-REF_TABNAME = 'EBAN'.
    w_fcat-seltext_m = '4th Release Time'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '8'.
    w_fcat-col_pos = '21'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*
        i_callback_program = repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = 'HEADER'
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = 'PR Release Status'
*       I_GRID_SETTINGS    =
        is_layout          = layout
        it_fieldcat        = i_fcat
      TABLES
        t_outtab           = it_prr1.
  ENDIF.

  IF rm5 IS NOT INITIAL.
    IF d_ebeln IS NOT INITIAL AND p_aedat IS NOT INITIAL .
      SELECT a~ebeln a~bedat a~ekgrp a~lifnr b~ebelp b~matnr b~txz01 b~pstyp b~netwr INTO  TABLE it_po FROM
      ekko AS a INNER JOIN ekpo AS b ON a~ebeln = b~ebeln WHERE a~aedat IN p_aedat AND a~ebeln IN d_ebeln AND b~loekz NE 'L'.
    ELSEIF d_ebeln IS INITIAL AND p_aedat IS NOT INITIAL .
      SELECT a~ebeln a~bedat a~ekgrp a~lifnr b~ebelp b~matnr b~txz01 b~pstyp b~netwr INTO TABLE it_po FROM
      ekko AS a INNER JOIN ekpo AS b ON a~ebeln = b~ebeln WHERE a~aedat IN p_aedat AND b~loekz NE 'L'.
    ELSEIF d_ebeln IS NOT INITIAL AND p_aedat IS INITIAL .
      SELECT a~ebeln a~bedat a~ekgrp a~lifnr b~ebelp b~matnr b~txz01 b~pstyp b~netwr INTO TABLE it_po FROM
      ekko AS a INNER JOIN ekpo AS b ON a~ebeln = b~ebeln WHERE a~ebeln IN d_ebeln AND b~loekz NE 'L'.
    ELSEIF d_ebeln IS INITIAL AND p_aedat IS INITIAL .
      SELECT a~ebeln a~bedat a~ekgrp a~lifnr b~ebelp b~matnr b~txz01 b~pstyp b~netwr INTO TABLE it_po FROM
      ekko AS a INNER JOIN ekpo AS b ON a~ebeln = b~ebeln WHERE b~loekz NE 'L'.
    ENDIF.

    LOOP AT it_po INTO wa_po.
      wa_pof-ebeln = wa_po-ebeln.
      wa_pof-bedat = wa_po-bedat.
      wa_pof-ekgrp = wa_po-ekgrp.
      wa_pof-lifnr = wa_po-lifnr.
      wa_pof-ebelp = wa_po-ebelp.
      wa_pof-matnr = wa_po-matnr.
      wa_pof-txz01 = wa_po-txz01.
      wa_pof-netwr = wa_po-netwr.
      s = 0.
      IF wa_po-pstyp EQ '9'.
        SELECT lblni FROM essr INTO TABLE it_ss WHERE ebeln EQ wa_po-ebeln AND ebelp EQ wa_po-ebelp AND loekz NE 'X'.
        LOOP AT it_ss INTO wa_ss.
          SELECT mblnr mjahr zeile bwart lfbja lfbnr lfpos INTO TABLE it_grn FROM mseg WHERE lfbnr EQ wa_ss-lblni.
          LOOP AT it_grn INTO wa_grn.
            s = 1.
            wa_pof-ebeln = wa_po-ebeln.
            wa_pof-bedat = wa_po-bedat.
            wa_pof-ekgrp = wa_po-ekgrp.
            wa_pof-lifnr = wa_po-lifnr.
            wa_pof-ebelp = wa_po-ebelp.
            wa_pof-matnr = wa_po-matnr.
            wa_pof-txz01 = wa_po-txz01.
            wa_pof-netwr = wa_po-netwr.
            wa_pof-mblnr = wa_grn-mblnr.
            wa_pof-mjahr = wa_grn-mjahr.
            wa_pof-zeile = wa_grn-zeile.
            wa_pof-lfbnr = wa_grn-lfbnr.
            wa_pof-lfpos = wa_grn-lfpos.
            wa_pof-pot   = 'SEN'.
            wa_pof-lblni = wa_ss-lblni.
            APPEND wa_pof TO it_pof.
            CLEAR: wa_grn,wa_pof.
          ENDLOOP.
        ENDLOOP.
        CLEAR: wa_po,wa_grn, wa_ss.
      ELSE.

        SELECT mblnr mjahr zeile bwart lfbja lfbnr lfpos INTO TABLE it_grn FROM mseg WHERE ebeln EQ wa_po-ebeln AND ebelp EQ wa_po-ebelp AND
        ( bwart EQ '101' OR bwart EQ '102' ).
        it_grn1[] = it_grn.
        LOOP AT it_grn1 INTO wa_grn1 WHERE bwart EQ '102'.
          DELETE it_grn WHERE mblnr EQ wa_grn1-lfbnr AND zeile EQ wa_grn1-lfpos.
*          DELETE IT_GRN INDEX SY-TABIX.
        ENDLOOP.
        LOOP AT it_grn INTO wa_grn WHERE bwart EQ '101'.
          s = 1.
          wa_pof-ebeln = wa_po-ebeln.
          wa_pof-bedat = wa_po-bedat.
          wa_pof-ekgrp = wa_po-ekgrp.
          wa_pof-lifnr = wa_po-lifnr.
          wa_pof-ebelp = wa_po-ebelp.
          wa_pof-matnr = wa_po-matnr.
          wa_pof-txz01 = wa_po-txz01.
          wa_pof-netwr = wa_po-netwr.
          wa_pof-mblnr = wa_grn-mblnr.
          wa_pof-mjahr = wa_grn-mjahr.
          wa_pof-zeile = wa_grn-zeile.
          wa_pof-lfbnr = wa_grn-lfbnr.
          wa_pof-lfpos = wa_grn-lfpos.
          wa_pof-pot   = ''.
          APPEND wa_pof TO it_pof.
          CLEAR: wa_grn,wa_pof.
        ENDLOOP.

      ENDIF.
      IF s NE '1'.
        APPEND wa_pof TO it_pof.
        CLEAR: wa_grn,wa_pof,wa_ss.
      ENDIF.
      CLEAR it_grn.
    ENDLOOP.
    BREAK abap.
    s = 0.
    LOOP AT it_pof INTO wa_pof.
      cou2 = 0.

      IF wa_pof-mblnr IS INITIAL.
        SELECT belnr gjahr buzei lfbnr lfgja lfpos shkzg wrbtr FROM rseg INTO TABLE it_inv WHERE ebeln EQ wa_pof-ebeln
        AND ebelp EQ wa_pof-ebelp AND shkzg EQ 'S' .
      ELSE.
        SELECT belnr gjahr buzei lfbnr lfgja lfpos shkzg wrbtr FROM rseg INTO TABLE it_inv WHERE ebeln EQ wa_pof-ebeln
        AND ebelp EQ wa_pof-ebelp AND shkzg EQ 'S' AND lfbnr EQ wa_pof-mblnr .

      ENDIF.
*    SELECT BELNR GJAHR BUZEI LFBNR LFGJA LFPOS SHKZG WRBTR FROM RSEG INTO TABLE IT_INV WHERE EBELN EQ WA_POF-EBELN
*      AND EBELP EQ WA_POF-EBELP AND SHKZG EQ 'S' and LFBNR EQ WA_POF-MBLNR .
      SELECT SINGLE name1 FROM lfa1 INTO v_name WHERE lifnr = wa_pof-lifnr.
      s = s + 1.
      wa_pof1-ebeln = wa_pof-ebeln.
      wa_pof1-bedat = wa_pof-bedat.
      wa_pof1-ekgrp = wa_pof-ekgrp.
      wa_pof1-lifnr = wa_pof-lifnr.
      wa_pof1-ebelp = wa_pof-ebelp.
      wa_pof1-matnr = wa_pof-matnr.
      wa_pof1-txz01 = wa_pof-txz01.
      wa_pof1-mblnr = wa_pof-mblnr.
      wa_pof1-mjahr = wa_pof-mjahr.
      wa_pof1-zeile = wa_pof-zeile.
      wa_pof1-lfbnr = wa_pof-lfbnr.
      wa_pof1-lfpos = wa_pof-lfpos.
      wa_pof1-pot   = wa_pof-pot.
      wa_pof1-lblni = wa_pof-lblni.
      wa_pof1-name1 = v_name.
      wa_pof1-netwr = wa_pof-netwr.

      IF wa_pof-mblnr IS  INITIAL .
        SELECT belnr gjahr buzei lfbnr lfgja lfpos shkzg wrbtr FROM rseg INTO TABLE it_inv1 WHERE ebeln EQ wa_pof-ebeln
        AND ebelp EQ wa_pof-ebelp AND shkzg EQ 'H'.

      ELSE.
        SELECT belnr gjahr buzei lfbnr lfgja lfpos shkzg wrbtr FROM rseg INTO TABLE it_inv1 WHERE ebeln EQ wa_pof-ebeln
        AND ebelp EQ wa_pof-ebelp AND shkzg EQ 'H' AND lfbnr EQ wa_pof-mblnr.

      ENDIF.
*    SELECT BELNR GJAHR BUZEI LFBNR LFGJA LFPOS SHKZG WRBTR FROM RSEG INTO TABLE IT_INV1 WHERE EBELN EQ WA_POF-EBELN
*      AND EBELP EQ WA_POF-EBELP AND SHKZG EQ 'H' and LFBNR EQ WA_POF-MBLNR.
      SORT it_inv BY belnr gjahr buzei.
      SORT it_inv1 BY belnr gjahr buzei.
*      DESCRIBE TABLE IT_INV LINES Cou1.
*      DESCRIBE TABLE IT_INV1 LINES Cou2.

      LOOP AT it_inv INTO wa_inv.
        cou1 = 0.
        LOOP AT it_inv1 INTO wa_inv1 WHERE lfbnr EQ wa_inv-lfbnr AND lfpos EQ wa_inv-lfpos.
          cou1 = 1.
          CONTINUE.
        ENDLOOP.
        IF cou1 EQ 1.
          DELETE it_inv INDEX sy-tabix.
          DELETE it_inv1 INDEX 1.
        ENDIF.

      ENDLOOP.
      DESCRIBE TABLE it_inv LINES cou2.
      LOOP AT it_inv INTO wa_inv.

        wa_pof1-belnr = wa_inv-belnr.
        wa_pof1-gjahr = wa_inv-gjahr.
        wa_pof1-buzei = wa_inv-buzei.
        wa_pof1-wrbtr = wa_inv-wrbtr.
        CONCATENATE wa_inv-belnr wa_inv-gjahr INTO dat.
        SELECT belnr gjahr bukrs FROM bkpf INTO TABLE it_fi WHERE awkey EQ dat.
        READ TABLE it_fi INTO wa_fi INDEX 1.
        wa_pof1-belnr1  = wa_fi-belnr.
        wa_pof1-gjahr1  = wa_fi-gjahr.
        wa_pof1-bukrs   = wa_fi-bukrs.


*           MODIFY IT_POF TRANSPORTING BELNR GJAHR BUZEI WHERE EBELN = WA_POF-EBELN AND EBELP EQ WA_POF-EBELP
*                  AND LFBNR EQ WA_POF-LFBNR AND LFPOS EQ WA_POF-LFPOS.
        wa_pof1-sno   = s.
        APPEND wa_pof1 TO it_pof1.
        IF cou2 GT 1.
          s = s + 1.
          cou2 = cou2 - 1.
        ENDIF.
      ENDLOOP.
      IF cou2 EQ 0.
        wa_pof1-sno   = s.
        APPEND wa_pof1 TO it_pof1.
      ENDIF.
      CLEAR: wa_pof1,wa_fi,wa_inv,wa_pof.
    ENDLOOP.

    w_fcat-fieldname = 'SNO'.
*  W_FCAT-REF_TABNAME = ''.
    w_fcat-seltext_l = 'S NO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '1'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BEDAT'.
    w_fcat-ref_tabname = 'EKKO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '10'.
    w_fcat-col_pos = '2'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'EBELN'.
    w_fcat-ref_tabname = 'EKKO'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '12'.
    w_fcat-col_pos = '3'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'EBELP'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '4'.
    w_fcat-col_pos = '4'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'NETWR'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C311'.
    w_fcat-outputlen = '15'.
    w_fcat-col_pos = '4'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'MATNR'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '8'.
    w_fcat-col_pos = '5'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'TXZ01'.
    w_fcat-ref_tabname = 'EKPO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '20'.
    w_fcat-col_pos = '6'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'LIFNR'.
    w_fcat-ref_tabname = 'EKKO'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '8'.
    w_fcat-col_pos = '7'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'NAME1'.
*  W_FCAT-REF_TABNAME = 'LFA1'.
    w_fcat-seltext_l = 'Vendor Name'.
    w_fcat-emphasize = 'C110'.
    w_fcat-outputlen = '30'.
    w_fcat-col_pos = '8'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'MBLNR'.
    w_fcat-ref_tabname = 'MSEG'.
    w_fcat-emphasize = 'C510'.
    w_fcat-outputlen = '14'.
    w_fcat-col_pos = '9'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'ZEILE'.
    w_fcat-ref_tabname = 'MSEG'.
    w_fcat-emphasize = 'C510'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '10'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'MJAHR'.
    w_fcat-ref_tabname = 'MSEG'.
    w_fcat-emphasize = 'C510'.
    w_fcat-outputlen = '6'.
    w_fcat-col_pos = '11'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BELNR'.
*  W_FCAT-REF_TABNAME = 'RSEG'.
    w_fcat-seltext_l = 'Booking Docment No'.
    w_fcat-emphasize = 'C210'.
    w_fcat-outputlen = '14'.
    w_fcat-col_pos = '12'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BUZEI'.
    w_fcat-ref_tabname = 'RSEG'.
    w_fcat-emphasize = 'C210'.
    w_fcat-outputlen = '5'.
    w_fcat-col_pos = '13'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'GJAHR'.
    w_fcat-ref_tabname = 'RSEG'.
    w_fcat-emphasize = 'C210'.
    w_fcat-outputlen = '6'.
    w_fcat-col_pos = '14'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'WRBTR'.
    w_fcat-ref_tabname = 'RSEG'.
    w_fcat-emphasize = 'C210'.
    w_fcat-outputlen = '15'.
    w_fcat-col_pos = '14'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.

    w_fcat-fieldname = 'BELNR1'.
*  W_FCAT-REF_TABNAME = 'RSEG'.
    w_fcat-seltext_l = 'FI Docment No'.
    w_fcat-emphasize = 'C210'.
    w_fcat-outputlen = '14'.
    w_fcat-col_pos = '12'.
    APPEND w_fcat TO i_fcat.
    CLEAR w_fcat.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = sy-repid
        i_callback_user_command = gt_callback_subroutine
*       i_callback_top_of_page  = g_top_of_page
        is_layout               = layout
        it_fieldcat             = i_fcat
      TABLES
        t_outtab                = it_pof1
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

FORM user_command  USING p_ucomm    LIKE sy-ucomm
                         p_selfield TYPE slis_selfield.
  "p_ucomm will hold user action like double click, clicking a button ,etc
  CASE p_ucomm.
    WHEN '&IC1'.        " SAP standard code for double-clicking
      CLEAR: wa_pof1.
      IF p_selfield-fieldname = 'EBELN'.
        CLEAR: wa_pof1.
        READ TABLE it_pof1 INTO wa_pof1 INDEX p_selfield-tabindex.
        SET PARAMETER ID 'BES' FIELD wa_pof1-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ELSEIF p_selfield-fieldname = 'BELNR1'.
        READ TABLE it_pof1 INTO wa_pof1 INDEX p_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD wa_pof1-belnr1.
        SET PARAMETER ID 'BUK' FIELD wa_pof1-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_pof1-gjahr1.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF p_selfield-fieldname = 'BELNR'.
        READ TABLE it_pof1 INTO wa_pof1 INDEX p_selfield-tabindex.
        SET PARAMETER ID 'RBN' FIELD wa_pof1-belnr.
        SET PARAMETER ID 'GJR' FIELD wa_pof1-gjahr.
*              SET PARAMETER ID 'BUK' FIELD '1080'.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      ELSEIF p_selfield-fieldname = 'MBLNR'.
        READ TABLE it_pof1 INTO wa_pof1 INDEX p_selfield-tabindex.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_pof1-mblnr
            i_mjahr             = wa_pof1-mjahr
            i_zeile             = wa_pof1-zeile.
*              I_GJAHR = WA_POF1-GJAHR.
      ENDIF.
*          MESSAGE 'OK'  TYPE 'I'.

      "It_changes hold all the change made in the ALV
  ENDCASE.
ENDFORM.
