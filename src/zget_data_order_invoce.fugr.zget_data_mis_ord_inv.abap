FUNCTION zget_data_mis_ord_inv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJID) TYPE  CHAR10
*"     VALUE(DATE_FRM) TYPE  AEDAT
*"     VALUE(DATE_TO) TYPE  AEDAT
*"     VALUE(BUKRS) TYPE  BUKRS OPTIONAL
*"  TABLES
*"      IT_INVOICE STRUCTURE  ZSTR_INVOICE_MIS
*"      IT_ORDER STRUCTURE  ZSTR_ORDER_MIS
*"      IT_CONDITION STRUCTURE  ZSTR_CONDITION
*"      DOCUMENT_NO STRUCTURE  RANGE_VBELN OPTIONAL
*"      IT_BATCH STRUCTURE  ZSTR_LIPS_BNO_MIS OPTIONAL
*"      IT_VBPA STRUCTURE  ZSTR_VBPA_MIS OPTIONAL
*"----------------------------------------------------------------------
*Changes - Removing Cancelled invoices and also the cancel invoice types

  RANGES: so_bukrs FOR vbrk-bukrs.

  IF bukrs IS NOT INITIAL.
    so_bukrs-sign   = 'I'.
    so_bukrs-option = 'EQ'.
    so_bukrs-low    = bukrs.
    APPEND so_bukrs.
  ELSE.
    so_bukrs-sign   = 'I'.
    so_bukrs-option = 'EQ'.
    so_bukrs-low    = '1000'.
    APPEND so_bukrs.
  ENDIF.

  IF objid = 'ZLVR_INV'.
    TYPES: BEGIN OF ty_invoice_fi,
             vbeln      TYPE vbrk-vbeln,
             fkdat      TYPE vbrk-fkdat,
             gjahr      TYPE char4,
             fkart      TYPE vbrk-fkart,
             werks      TYPE vbrp-werks,
             vkorg      TYPE vbrk-vkorg,
             kdgrp      TYPE vbrk-kdgrp,
             vtweg      TYPE vbrk-vtweg,
             spart      TYPE vbrk-spart,
             kunrg      TYPE vbrk-kunrg,
             aubel      TYPE vbrp-aubel,
             posnr      TYPE vbrp-posnr,
             matnr      TYPE vbrp-matnr,
             fkimg      TYPE vbrp-fkimg,
             volum      TYPE vbrp-volum,
             voleh      TYPE vbrp-voleh,
             waerk      TYPE vbrk-waerk,
             netwr      TYPE vbrp-netwr,
             mwsbp      TYPE vbrp-mwsbp,
             skfbp      TYPE vbrp-skfbp,
             dist_plant TYPE werks_d,
             bukrs      TYPE bukrs,
           END OF ty_invoice_fi.


    TYPES : BEGIN OF ty_vbrk,
              vbeln TYPE vbrk-vbeln,
              fkdat TYPE vbrk-fkdat,
              fkart TYPE vbrk-fkart,
              vkorg TYPE vbrk-vkorg,
              knumv TYPE vbrk-knumv,
              kdgrp TYPE vbrk-kdgrp,
              vtweg TYPE vbrk-vtweg,
              spart TYPE vbrk-spart,
              kunrg TYPE vbrk-kunrg,
              waerk TYPE vbrk-waerk,
              fksto TYPE vbrk-fksto,
              bukrs TYPE vbrk-bukrs,
            END OF ty_vbrk.

    TYPES : BEGIN OF ty_vbrp,
              vbeln TYPE vbrk-vbeln,
              werks TYPE vbrp-werks,
              aubel TYPE vbrp-aubel,
              posnr TYPE vbrp-posnr,
              matnr TYPE vbrp-matnr,
              fkimg TYPE vbrp-fkimg,
              volum TYPE vbrp-volum,
              voleh TYPE vbrp-voleh,
              netwr TYPE vbrp-netwr,
              mwsbp TYPE vbrp-mwsbp,
              skfbp TYPE vbrp-skfbp,
              vgbel TYPE vbrp-vgbel,
            END OF ty_vbrp.
***Added by samsudeen M
    TYPES: BEGIN OF ty_prcd,
             knumv TYPE knumv,
             kposn TYPE kposn,
             kschl TYPE kscha,
             krech TYPE krech_long,
             kwert TYPE vfprc_element_value,
             waers TYPE waers,
           END OF ty_prcd.
    RANGES: kschl FOR prcd_elements-kschl.
    DATA: it_prcd TYPE TABLE OF ty_prcd,
          wa_prcd TYPE ty_prcd.
    DATA: lt_tvarvc1 TYPE TABLE OF tvarvc,
          ls_tvarvc1 TYPE tvarvc.
    DATA: ls_condition TYPE zstr_condition. "work area
**********End of Samsudeen on 14.09.2022
    DATA: lt_matnr TYPE RANGE OF mara-matnr,
          ls_matnr LIKE LINE OF lt_matnr.

    DATA : it_invoice_fi TYPE TABLE OF ty_invoice_fi,
           wa_invoice_fi TYPE ty_invoice_fi,
           it_vbrk       TYPE TABLE OF ty_vbrk,
           wa_vbrk       TYPE ty_vbrk,
           it_vbrp       TYPE TABLE OF ty_vbrp,
           wa_vbrp       TYPE ty_vbrp,
           wa_invoice    TYPE zstr_invoice_mis.

    DATA: lt_tvarvc TYPE TABLE OF tvarvc,
          ls_tvarvc TYPE tvarvc.

    DATA: ls_batno TYPE zstr_lips_bno_mis.

*    IF objid IS NOT INITIAL AND document_no IS NOT INITIAL
*            AND date_frm IS NOT INITIAL AND date_to IS NOT INITIAL.

    SELECT   vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
             fkdat
             fkart
             vkorg
             knumv
             kdgrp
             vtweg
             spart
             kunrg
             waerk
             fksto "Cancelled document indicator
             bukrs
                   INTO TABLE it_vbrk
                   FROM vbrk
                   WHERE vbeln IN document_no
                   AND   bukrs IN so_bukrs
                   AND   fkdat GE date_frm
                   AND   fkdat LE date_to.

    IF it_vbrk IS NOT INITIAL.
      SORT it_vbrk[] BY vbeln.
      SELECT vbeln
             werks
             aubel
             posnr
             matnr
             fkimg
             volum
             voleh
             netwr
             mwsbp
             skfbp
             vgbel FROM vbrp
                   INTO TABLE it_vbrp
                   FOR ALL ENTRIES IN it_vbrk
                   WHERE vbeln = it_vbrk-vbeln.
****Added by Samsudeen on 14.09.2022
      IF sy-subrc EQ 0.
        SORT it_vbrp[] BY vbeln posnr.
      ENDIF.
****Added by Pandiarajan on 15.03.2024
**************fetch the batch details***************
      SELECT vbeln,posnr,matnr,werks,charg,lfimg,meins,vrkme,ntgew,brgew,gewei,volum,voleh,vgbel
        FROM lips INTO TABLE @DATA(it_lips)
        FOR ALL ENTRIES IN @it_vbrp
        WHERE vbeln EQ @it_vbrp-vgbel AND charg NE @abap_false.
      IF sy-subrc EQ 0.
        SORT it_lips[] BY vbeln posnr.
      ENDIF.
****Condition Type Filter values from TVARVC
      SELECT * FROM tvarvc
               INTO TABLE lt_tvarvc1
               WHERE name = 'ZBAPI_DISCOUNT_TYPE'
               AND type = 'S'.
      LOOP AT lt_tvarvc1 INTO  ls_tvarvc1.
        kschl-sign = 'I'.
        kschl-option = 'EQ'.
        kschl-low = ls_tvarvc1-low.
        APPEND kschl.
      ENDLOOP.
***Condition Record based on Billing Document Number
      SELECT knumv
             kposn
             kschl
             krech
             kwert
             waers FROM prcd_elements
                   INTO TABLE it_prcd
                   FOR ALL ENTRIES IN it_vbrk
                   WHERE knumv = it_vbrk-knumv
                   AND kschl IN kschl.
      IF sy-subrc EQ 0.
        SORT it_prcd[] BY knumv kposn.
      ENDIF.

***Condition type Text
      SELECT * FROM t685t INTO TABLE @DATA(lt_text)
               WHERE spras EQ @sy-langu
               AND kappl = 'V'.
      IF sy-subrc EQ 0.
        SORT lt_text[] BY kschl.
      ENDIF.
****End of Changes on 14.09.2022
    ENDIF.

**Material Block changes on 29.09.2022 **
    SELECT * FROM tvarvc INTO TABLE @DATA(lt_matblk)
                              WHERE name = 'ZBAPI_INVOICE_MATBLK'
                              AND type = 'S'.
*********************************************************************

*Variable to store the order types not to be selected
    SELECT * FROM tvarvc INTO TABLE lt_tvarvc WHERE name = 'ZBAPI_MIS_ORDTYPE'
    AND type = 'S'.
    IF sy-subrc EQ 0.
    ENDIF.

    IF it_vbrp IS NOT INITIAL.
      DATA: date(2)  TYPE c,
            month(2) TYPE c,
            year(4)  TYPE c.
*****************8fetch the customer details************
      SELECT kunnr,werks FROM kna1 INTO TABLE @DATA(lt_kna1)
                                   FOR ALL ENTRIES IN @it_vbrk
                                   WHERE kunnr = @it_vbrk-kunrg.
      IF sy-subrc = 0.
        SORT lt_kna1 BY kunnr.
      ENDIF.

      LOOP AT it_vbrp INTO wa_vbrp.
        CLEAR : year.
        CLEAR wa_vbrk.
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrp-vbeln.

        IF wa_vbrk-fksto IS NOT INITIAL.  "if cancelled invoice then ignore
          CONTINUE.
        ENDIF.

        IF lt_tvarvc[] IS NOT INITIAL. " if ordertype in list then ignore
          CLEAR ls_tvarvc.
          READ TABLE lt_tvarvc INTO ls_tvarvc WITH KEY low = wa_vbrk-fkart.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
        ENDIF.
***************read the distributor plant********
        READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = wa_vbrk-kunrg BINARY SEARCH.
        IF sy-subrc = 0.
          wa_invoice_fi-dist_plant = ls_kna1-werks.
        ENDIF.
        wa_invoice_fi-vbeln = wa_vbrp-vbeln.
        wa_invoice_fi-fkdat = wa_vbrk-fkdat.
        year = wa_invoice_fi-fkdat(4).
        wa_invoice_fi-gjahr = year.
        wa_invoice_fi-fkart = wa_vbrk-fkart.
        wa_invoice_fi-werks = wa_vbrp-werks.
        wa_invoice_fi-vkorg = wa_vbrk-vkorg.
        wa_invoice_fi-kdgrp = wa_vbrk-kdgrp.
        wa_invoice_fi-vtweg = wa_vbrk-vtweg.
        wa_invoice_fi-spart = wa_vbrk-spart.
        wa_invoice_fi-kunrg = wa_vbrk-kunrg.
        wa_invoice_fi-bukrs = wa_vbrk-bukrs.
        wa_invoice_fi-aubel = wa_vbrp-aubel.
        wa_invoice_fi-posnr = wa_vbrp-posnr.
        wa_invoice_fi-matnr = wa_vbrp-matnr.
*Added by Samsudeen on 29.09.2022
        IF lt_matblk[] IS NOT INITIAL.
          READ TABLE lt_matblk INTO DATA(ls_matblk) WITH KEY low = wa_vbrp-matnr.
          IF sy-subrc EQ 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        wa_invoice_fi-fkimg = wa_vbrp-fkimg.
        wa_invoice_fi-volum = wa_vbrp-volum.
        wa_invoice_fi-voleh = wa_vbrp-voleh.
        wa_invoice_fi-waerk = wa_vbrk-waerk.
        wa_invoice_fi-netwr = wa_vbrp-netwr.
        wa_invoice_fi-mwsbp = wa_vbrp-mwsbp.
        wa_invoice_fi-skfbp = wa_invoice_fi-netwr + wa_invoice_fi-mwsbp.
***Added by Samsudeen M
        CLEAR wa_vbrk.
***Read for getting condition doc Number
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_invoice_fi-vbeln BINARY SEARCH.
        IF sy-subrc EQ 0.
          LOOP AT it_prcd INTO wa_prcd WHERE knumv = wa_vbrk-knumv
                                       AND kposn = wa_vbrp-posnr.
**********Condition Type checks if not present just ignore
            READ TABLE lt_tvarvc1 INTO ls_tvarvc1 WITH KEY low = wa_prcd-kschl.
            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.
            CLEAR ls_condition.
            ls_condition-vbeln = wa_vbrp-vbeln.
            ls_condition-knumv = wa_prcd-knumv.
            ls_condition-kposn = wa_prcd-kposn.
            ls_condition-kschl = wa_prcd-kschl.
            READ TABLE lt_text INTO DATA(ls_text) WITH KEY kschl = wa_prcd-kschl BINARY SEARCH.
            IF sy-subrc EQ 0.
              ls_condition-kschl_desc = ls_text-vtext.
            ENDIF.
            ls_condition-krech = wa_prcd-krech.
            ls_condition-kbetr = wa_prcd-kwert.
            ls_condition-waers = wa_prcd-waers.
            APPEND ls_condition TO it_condition.
          ENDLOOP.
        ENDIF.
***End of Changes 0n 14.09.2022
        "WA_INVOICE_FI-SKFBP = WA_VBRP-SKFBP.
        MOVE-CORRESPONDING wa_invoice_fi TO wa_invoice.
        APPEND wa_invoice TO it_invoice.
*Based on the delivery no and material, Get all the batch no and input in the table
        LOOP AT it_lips INTO DATA(ls_lips) WHERE vbeln = wa_vbrp-vgbel AND matnr = wa_vbrp-matnr.
          CLEAR ls_batno.
          ls_batno-vbeln       = wa_vbrp-vbeln.
          ls_batno-delivery_no = ls_lips-vbeln.
          ls_batno-posnr       = wa_vbrp-posnr.
          ls_batno-matnr       = ls_lips-matnr.
          ls_batno-werks       = ls_lips-werks.
          ls_batno-charg       = ls_lips-charg.
          ls_batno-lfimg       = ls_lips-lfimg.
          ls_batno-meins       = ls_lips-meins.
          ls_batno-vrkme       = ls_lips-vrkme.
          ls_batno-ntgew       = ls_lips-ntgew.
          ls_batno-brgew       = ls_lips-brgew.
          ls_batno-gewei       = ls_lips-gewei.
          ls_batno-volum       = ls_lips-volum.
          ls_batno-voleh       = ls_lips-voleh.

          APPEND ls_batno TO it_batch.
          CLEAR : ls_lips.
        ENDLOOP.
        CLEAR : wa_invoice_fi,wa_vbrk,wa_vbrp,wa_invoice.
      ENDLOOP.

      IF it_invoice[] IS NOT INITIAL.
        SORT it_invoice[] BY vbeln posnr.
      ENDIF.
** End of Changes on 29.09.2022

*      LOOP AT it_invoice_fi INTO wa_invoice_fi.
*        MOVE-CORRESPONDING wa_invoice_fi TO wa_invoice.
*        APPEND wa_invoice TO it_invoice.
*        CLEAR:wa_invoice_fi,wa_invoice.
*      ENDLOOP.
    ENDIF.

    IF it_condition[] IS NOT INITIAL.
      SORT it_condition[] BY vbeln kposn.
    ENDIF.

  ELSEIF objid = 'ZLVR_ORD'.

    TYPES: BEGIN OF ty_order_fi,
             vbeln      TYPE vbrk-vbeln,
             fkdat      TYPE vbrk-fkdat,
             gjahr      TYPE char4,
             fkart      TYPE vbrk-fkart,
             werks      TYPE vbrp-werks,
             vkorg      TYPE vbrk-vkorg,
             kdgrp      TYPE vbrk-kdgrp,
             vtweg      TYPE vbrk-vtweg,
             spart      TYPE vbrk-spart,
             kunrg      TYPE vbrk-kunrg,
             aubel      TYPE vbrp-aubel,
             posnr      TYPE vbrp-posnr,
             matnr      TYPE vbrp-matnr,
             fklmg      TYPE vbrp-fklmg,
             volum      TYPE vbrp-volum,
             voleh      TYPE vbrp-voleh,
             cmwae      TYPE vbrk-cmwae,
             netwr      TYPE vbrp-netwr,
             mwsbp      TYPE vbrp-mwsbp,
             skfbp      TYPE vbrp-skfbp,
             dist_plant TYPE werks_d,
             bukrs      TYPE bukrs,
           END OF ty_order_fi.


    TYPES : BEGIN OF ty_vbak,
              vbeln TYPE vbrk-vbeln,
              fkdat TYPE vbrk-fkdat,
              fkart TYPE vbrk-fkart,
              vkorg TYPE vbrk-vkorg,
              "KDGRP TYPE VBRK-KDGRP,
              vtweg TYPE vbrk-vtweg,
              spart TYPE vbrk-spart,
              kunrg TYPE vbrk-kunrg,
              cmwae TYPE vbrk-cmwae,
              bukrs TYPE vbak-bukrs_vf,
            END OF ty_vbak.

    TYPES : BEGIN OF ty_vbap,
              vbeln TYPE vbrk-vbeln,
              werks TYPE vbrp-werks,
              "AUBEL TYPE VBRP-AUBEL,
              posnr TYPE vbrp-posnr,
              matnr TYPE vbrp-matnr,
              fklmg TYPE vbrp-fklmg,
              volum TYPE vbrp-volum,
              voleh TYPE vbrp-voleh,
              netwr TYPE vbrp-netwr,
              mwsbp TYPE vbrp-mwsbp,
              "SKFBP TYPE VBRP-SKFBP,
            END OF ty_vbap.

    TYPES : BEGIN OF ty_kdgrp,
              vbeln TYPE vbkd-vbeln,
              kdgrp TYPE vbkd-kdgrp,
            END OF ty_kdgrp.

    DATA : it_order_fi TYPE TABLE OF ty_order_fi,
           wa_order_fi TYPE ty_order_fi,
           it_vbak     TYPE TABLE OF ty_vbak,
           wa_vbak     TYPE ty_vbak,
           it_vbap     TYPE TABLE OF ty_vbap,
           wa_vbap     TYPE ty_vbap,
           it_vbkd     TYPE TABLE OF ty_kdgrp,
           wa_vbkd     TYPE ty_kdgrp,
           wa_order    TYPE zstr_order_mis.


    SELECT
      vbeln
      audat
      auart
      vkorg
      vtweg
      spart
      kunnr
      waerk
      bukrs_vf
   INTO TABLE it_vbak FROM vbak WHERE vbeln    IN document_no
                                AND   bukrs_vf IN so_bukrs
                                AND   audat    GE date_frm AND audat LE date_to.

    IF it_vbak IS NOT INITIAL.
      SELECT
        vbeln
        kdgrp
        INTO TABLE it_vbkd FROM vbkd FOR ALL ENTRIES IN it_vbak WHERE vbeln = it_vbak-vbeln.
      SELECT
        vbeln
        werks
        posnr
        matnr
        kwmeng
        volum
        voleh
        netwr
        mwsbp
        INTO  TABLE it_vbap FROM vbap FOR ALL ENTRIES IN it_vbak WHERE vbeln = it_vbak-vbeln.

*****************fetch the customer details************
      REFRESH lt_kna1.
      SELECT kunnr werks FROM kna1 INTO TABLE lt_kna1
                                   FOR ALL ENTRIES IN it_vbak
                                   WHERE kunnr = it_vbak-kunrg.
      IF sy-subrc = 0.
        SORT : lt_kna1 BY kunnr.
      ENDIF.

**************fetch the partner function details***************
      SELECT vbeln,
             parvw,
             kunnr,
             lifnr,
             pernr FROM vbpa INTO TABLE @it_vbpa
                   FOR ALL ENTRIES IN @it_vbak
                   WHERE vbeln EQ @it_vbak-vbeln.
      IF sy-subrc EQ 0.
        SORT it_vbpa[] BY vbeln parvw.
        DELETE ADJACENT DUPLICATES FROM it_vbpa COMPARING vbeln parvw.
      ENDIF.

    ENDIF.
    IF it_vbap IS NOT INITIAL.
*      DATA: DATE(2) TYPE C,
*              MONTH(2) TYPE C,
*              YEAR(4) TYPE C.

      LOOP AT it_vbap INTO wa_vbap.
        CLEAR : year.
        READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_vbap-vbeln.
        READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_vbap-vbeln.
        wa_order_fi-vbeln = wa_vbak-vbeln.
        wa_order_fi-fkdat = wa_vbak-fkdat.
        year = wa_order_fi-fkdat(4).
        wa_order_fi-gjahr = year.
        wa_order_fi-fkart = wa_vbak-fkart.
        wa_order_fi-werks = wa_vbap-werks.
        wa_order_fi-vkorg = wa_vbak-vkorg.
        wa_order_fi-kdgrp = wa_vbkd-kdgrp.
        wa_order_fi-vtweg = wa_vbak-vtweg.
        wa_order_fi-spart = wa_vbak-spart.
        wa_order_fi-kunrg = wa_vbak-kunrg.
        wa_order_fi-bukrs = wa_vbak-bukrs.
        wa_order_fi-aubel = wa_vbap-vbeln.
        wa_order_fi-posnr = wa_vbap-posnr.
        wa_order_fi-matnr = wa_vbap-matnr.
        wa_order_fi-fklmg = wa_vbap-fklmg.
        wa_order_fi-volum = wa_vbap-volum.
        wa_order_fi-voleh = wa_vbap-voleh.
        wa_order_fi-cmwae = wa_vbak-cmwae.
        wa_order_fi-netwr = wa_vbap-netwr.
        wa_order_fi-mwsbp = wa_vbap-mwsbp.
        wa_order_fi-skfbp = ''."WA_VBRP-SKFBP.
***************read the distributor plant********
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = wa_vbak-kunrg BINARY SEARCH.
        IF sy-subrc = 0.
          wa_invoice_fi-dist_plant = ls_kna1-werks.
        ENDIF.

        APPEND wa_order_fi TO it_order_fi.
        CLEAR : wa_order_fi,wa_vbak,wa_vbap,wa_vbkd.

      ENDLOOP.

      LOOP AT it_order_fi INTO wa_order_fi.
        MOVE-CORRESPONDING wa_order_fi TO wa_order.
        APPEND wa_order TO it_order.
        CLEAR:wa_order_fi,wa_order.
      ENDLOOP.

    ENDIF.

  ENDIF.




ENDFUNCTION.
