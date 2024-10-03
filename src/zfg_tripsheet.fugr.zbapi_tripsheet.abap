FUNCTION zbapi_tripsheet.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VENDOR) TYPE  RANGE_LIFNR OPTIONAL
*"     VALUE(FROM_DATE) TYPE  DATUM
*"     VALUE(TO_DATE) TYPE  DATUM
*"  TABLES
*"      LT_TRIPSHEET STRUCTURE  ZSTR_TRIPSHEET
*"----------------------------------------------------------------------

  TYPES:BEGIN OF ty_zfre_hed,
          trip_no        TYPE ztrip,
          vendor_code    TYPE lifnr,
          vendor_name    TYPE name1_gp,
          from_loc       TYPE name1,
          freight_charge TYPE netwr,
          loding_charge  TYPE netwr,
          unload_charge  TYPE netwr,
          halt_charge    TYPE netwr,
          lr_charge      TYPE netwr,
          vechile_number TYPE zvechile,
          crdate         TYPE zcdate,
          local_vendor   TYPE zfreight_header-local_vendor,
          lo_vendorname  TYPE zfreight_header-lo_vendorname,
          local_charge   TYPE zfreight_header-local_charge,
        END OF ty_zfre_hed.

  TYPES:BEGIN OF ty1_zfre_hed,
          trip_no          TYPE ztrip,
          vendor_code      TYPE lifnr,
          vendor_name      TYPE name1_gp,
          from_loc         TYPE name1,
          freight_charge   TYPE netwr,
          loding_charge    TYPE netwr,
          unload_charge    TYPE netwr,
          halt_charge      TYPE netwr,
          lr_charge        TYPE netwr,
          toll_charge      TYPE netwr,
          penalty_charge   TYPE netwr,
          local_charge     TYPE netwr,
          insurance_cahrge TYPE netwr,
          vechile_number   TYPE zvechile,
          crdate           TYPE zcdate,
          local_vendor     TYPE zfreight_header-local_vendor,
          lo_vendorname    TYPE zfreight_header-lo_vendorname,
          local_charge1    TYPE zfreight_header-local_charge,
        END OF ty1_zfre_hed.


  TYPES:BEGIN OF ty_zfre_itm,
          trip_no       TYPE ztrip,
          invoice_no    TYPE vbeln_vf,
          customer_name TYPE name1_gp,
          invoice_date  TYPE fkdat,
          weight        TYPE brgew_15,
          company_code  TYPE bukrs,
          location      TYPE name1_gp,
          lr_no         TYPE zlr_no,
          status        TYPE zsta,
        END OF ty_zfre_itm.

  TYPES:BEGIN OF ty_zfre_tra,
          trip_no      TYPE ztrip,
          totat_amount TYPE netwr,
        END OF ty_zfre_tra.

  TYPES:BEGIN OF ty_vbrk,
          vbeln TYPE vbeln_vf,
          netwr TYPE netwr,
        END OF ty_vbrk.

  TYPES:BEGIN OF ty_vbrp,
          vbeln TYPE vbeln_vf,
          posnr TYPE posnr_vf,
          brgew TYPE brgew_15,
          netwr TYPE netwr,
          fkimg TYPE fkimg,
          matnr TYPE matnr,
          werks TYPE werks_d,
          arktx TYPE arktx,
          vkbur TYPE vkbur,
        END OF ty_vbrp.

  TYPES:BEGIN OF ty_mseg,
          mblnr TYPE mblnr,
          matnr TYPE matnr,
          werks TYPE werks_d,
          dmbtr TYPE dmbtr,
          menge TYPE menge_d,
          erfmg TYPE erfmg,
        END OF ty_mseg.

  TYPES:BEGIN OF ty_mara,
          matnr TYPE matnr,
          brgew TYPE brgew,
        END OF ty_mara.

  TYPES:BEGIN OF ty_t001w,
          werks TYPE werks_d,
          name1 TYPE name1,
        END OF ty_t001w.

  TYPES:BEGIN OF ty_final,
          company_code   TYPE bukrs,     "Company Code
          crdate         TYPE zcdate,          "Trip sheet creation date
          trip_no        TYPE ztrip,          "Freight Entry No
          vendor_code    TYPE lifnr,      "Account Number of Vendor or Creditor
          vendor_name    TYPE name1_gp,   "Name 1
          lr_no          TYPE zlr_no,           "Lorry No
          crdate1        TYPE zcdate,         "Trip sheet creation date
          vechile_number TYPE zvechile, "Vechile No
          status(15)     TYPE c,            "Trip sheet type
          invoice_no     TYPE vbeln_vf,    "Billing Document
          invoice_date   TYPE fkdat,     "Billing date for billing index and printout
          posnr          TYPE posnr_vf,         "Billing item
          brgew1         TYPE brgew_15,        "GROSS WEIGHT IN MATERIAL
          fkimg          TYPE fkimg,            "Actual Invoiced Quantity
          matnr          TYPE matnr ,           "Material Number
          werks          TYPE werks_d,
          arktx          TYPE arktx ,           "Material Description
          customer_name  TYPE name1_gp, "Name 1
          from_loc       TYPE name1,         "Name
          location       TYPE name1_gp,      "Name 1
          netwr          TYPE netwr,            "Net Value in Document Currency
          brgew          TYPE brgew_15,         "Gross weight
          frecha         TYPE netwr,           "Net Value in FREIGHT CHARGES
          loacha         TYPE netwr,           "Net Value in LOADING CHARGES
          inlcha         TYPE netwr,           "Net Value in INLOADING CHARGES
          lrcha          TYPE netwr,            "Net Value in LR CHARGES
          halcha         TYPE netwr,           "Net Value in HALTING CHARGES
          tolcha         TYPE netwr,           "NET VALUE IN TOLL CHARGES
          inscha         TYPE netwr,           "NET VALUE IN INSURENCE CHARGE
          pencha         TYPE netwr,           "NET VALUE IN PENALTY CHARGE
          loccha         TYPE netwr,           "Net Value in LOCAL TRANSPORT
          local_vendor   TYPE zfreight_header-local_vendor,
          lo_vendorname  TYPE zfreight_header-lo_vendorname,
          local_charge   TYPE zfreight_header-local_charge,
          locnewcha      TYPE netwr,
          tot_frei       TYPE netwr,
        END OF ty_final.

  DATA: totqun   TYPE zfreight_item-weight,
        tra_tot  TYPE zfreight_item-weight,
        ofrecha  TYPE netwr,
        ohalcha  TYPE netwr,
        oloacha  TYPE netwr,
        oinlcha  TYPE netwr,
        olrcha   TYPE netwr,
        otolcha  TYPE netwr,
        oinscha  TYPE netwr,
        opencha  TYPE netwr,
        oloccha  TYPE netwr,
        ofrecha1 TYPE netwr,
        ohalcha1 TYPE netwr,
        oloacha1 TYPE netwr,
        oinlcha1 TYPE netwr,
        olrcha1  TYPE netwr,
        otolcha1 TYPE netwr,
        oinscha1 TYPE netwr,
        opencha1 TYPE netwr,
        oloccha1 TYPE netwr.

  DATA : freight1      TYPE netwr,
         halting1      TYPE netwr,
         loading1      TYPE netwr,
         inloading1    TYPE netwr,
         lrcharges1    TYPE netwr,
         tollcharge1   TYPE netwr,
         insurence1    TYPE netwr,
         penalty1      TYPE netwr,
         localtran1    TYPE netwr,
         localtrannew1 TYPE netwr,
         freight2      TYPE netwr,
         halting2      TYPE netwr,
         loading2      TYPE netwr,
         inloading2    TYPE netwr,
         lrcharges2    TYPE netwr,
         tollcharge2   TYPE netwr,
         insurence2    TYPE netwr,
         penalty2      TYPE netwr,
         localtran2    TYPE netwr,
         localtrannew2 TYPE netwr.

  DATA: fre_tot  TYPE f,
        lod_tot  TYPE f,
        inl_tot  TYPE f,
        lrc_tot  TYPE f,
        hal_tot  TYPE f,
        tol_tot  TYPE f,
        pen_tot  TYPE f,
        loc_tot  TYPE f,
        ins_tot  TYPE f,
        loc_trn  TYPE f,  "added on 28/7
        fre_tot1 TYPE f,
        lod_tot1 TYPE f,
        inl_tot1 TYPE f,
        lrc_tot1 TYPE f,
        hal_tot1 TYPE f,
        tol_tot1 TYPE f,
        pen_tot1 TYPE f,
        loc_tot1 TYPE f,
        ins_tot1 TYPE f,
        loc_trn1 TYPE f.  "added on 28/7

  DATA: it_hed    TYPE STANDARD TABLE OF ty_zfre_hed,
        wa_hed    TYPE ty_zfre_hed,
        it1_hed   TYPE STANDARD TABLE OF ty1_zfre_hed,
        wa1_hed   TYPE ty1_zfre_hed,
        it_itm    TYPE STANDARD TABLE OF ty_zfre_itm,
        wa_itm    TYPE ty_zfre_itm,
        wa1_itm   TYPE ty_zfre_itm,
        wa2_itm   TYPE ty_zfre_itm,
        it_tra    TYPE STANDARD TABLE OF zfreight_tran,
        wa_tra    TYPE zfreight_tran,
        it_vbrk   TYPE STANDARD TABLE OF ty_vbrk,
        wa_vbrk   TYPE ty_vbrk,
        it_vbrp   TYPE STANDARD TABLE OF ty_vbrp,
        wa_vbrp   TYPE ty_vbrp,
        it_mseg   TYPE STANDARD TABLE OF ty_mseg,
        wa_mseg   TYPE ty_mseg,
        it_mara   TYPE STANDARD TABLE OF ty_mara,
        wa_mara   TYPE ty_mara,
        it_t001w  TYPE STANDARD TABLE OF ty_t001w,
        wa_t001w  TYPE ty_t001w,
        it1_t001w TYPE STANDARD TABLE OF ty_t001w,
        wa1_t001w TYPE ty_t001w,
        it_fin    TYPE STANDARD TABLE OF ty_final,
        wa_fin    TYPE ty_final,
        it_final  TYPE STANDARD TABLE OF ty_final,
        wa_final  TYPE ty_final,
        it1_final TYPE STANDARD TABLE OF ty_final,
        wa1_final TYPE ty_final,
        it_makt   TYPE STANDARD TABLE OF makt,
        wa_makt   TYPE makt.

  DATA: v_repid LIKE sy-repid.

  DATA: it_fieldcat TYPE STANDARD TABLE OF slis_fieldcat_alv,
        wa_fieldcat TYPE slis_fieldcat_alv.

  DATA : layout TYPE slis_layout_alv.

  DATA: fdati     TYPE sy-datum,  "From Date
        tdati     TYPE sy-datum,  "TO Date
        fdato(10) TYPE c,
        tdato(10) TYPE c,
        fidat(30) TYPE c.     "CONCATENATE From Date and To Date

  DATA: f_trno     TYPE zfreight_header-trip_no,   "Trip Number From
        t_trno     TYPE zfreight_header-trip_no,   "Trip Number To
        c_trno(35) TYPE c.                     "CONCATENATE  Trip Number From And Trip Number To

  DATA: it_header TYPE slis_t_listheader, "lIST hEADER fOR ALV REPORT
        wa_header TYPE slis_listheader.
  "BREAK-POINT."26/6

  DATA : wa_lifnr TYPE lfa1-lifnr.

  DATA : lv_lifnr TYPE lfa1-lifnr.


  TYPES : BEGIN OF gs_lfa1,
            lifnr TYPE lfa1-lifnr,
            name1 TYPE lfa1-name1,
          END OF gs_lfa1.
  DATA : gt_lfa1 TYPE TABLE OF gs_lfa1,
         wa_lfa1 TYPE gs_lfa1.

  DATA: ls_tripsheet TYPE zstr_tripsheet.
  REFRESH: gt_lfa1.
  SELECT lifnr name1 INTO TABLE gt_lfa1 FROM lfa1.

  IF from_date IS NOT INITIAL AND to_date IS NOT INITIAL AND vendor IS INITIAL.

    REFRESH: it_hed.
    SELECT trip_no
           vendor_code
           vendor_name
           from_loc
           freight_charge
           loding_charge
           unload_charge
           halt_charge
           lr_charge
           vechile_number
           crdate
           local_vendor
           lo_vendorname
           local_charge FROM zfreight_header INTO TABLE it_hed
                        WHERE ( crdate BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0.
      SORT it_hed[] BY trip_no vendor_code.
    ENDIF.

  ELSEIF from_date IS NOT INITIAL AND to_date IS NOT INITIAL AND vendor-low IS NOT INITIAL.
    REFRESH: it_hed.
    SHIFT vendor-low LEFT DELETING LEADING '0'.
    SELECT trip_no
           vendor_code
           vendor_name
           from_loc
           freight_charge
           loding_charge
           unload_charge
           halt_charge
           lr_charge
           vechile_number
           crdate
           local_vendor
           lo_vendorname
           local_charge FROM zfreight_header INTO TABLE it_hed
                        WHERE vendor_code EQ vendor-low
                        AND ( crdate BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0.
      SORT it_hed[] BY trip_no vendor_code.
    ENDIF.

  ELSEIF from_date IS NOT INITIAL AND to_date IS NOT INITIAL AND vendor-low IS NOT INITIAL
                                                             AND vendor-high IS NOT INITIAL.
    REFRESH: it_hed.
    SHIFT vendor-low LEFT DELETING LEADING '0'.
    SHIFT vendor-high LEFT DELETING LEADING '0'.
    SELECT trip_no
           vendor_code
           vendor_name
           from_loc
           freight_charge
           loding_charge
           unload_charge
           halt_charge
           lr_charge
           vechile_number
           crdate
           local_vendor
           lo_vendorname
           local_charge FROM zfreight_header INTO TABLE it_hed
                        WHERE ( vendor_code BETWEEN vendor-low AND vendor-high )
                        AND ( crdate BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0.
      SORT it_hed[] BY trip_no vendor_code.
    ENDIF.

  ELSEIF from_date IS NOT INITIAL AND to_date IS INITIAL AND vendor IS INITIAL.

    REFRESH: it_hed.
    SELECT trip_no
           vendor_code
           vendor_name
           from_loc
           freight_charge
           loding_charge
           unload_charge
           halt_charge
           lr_charge
           vechile_number
           crdate
           local_vendor
           lo_vendorname
           local_charge FROM zfreight_header INTO TABLE it_hed
                        WHERE ( crdate BETWEEN from_date AND sy-datum ).
    IF sy-subrc EQ 0.
      SORT it_hed[] BY trip_no vendor_code.
    ENDIF.
  ENDIF.




  IF NOT it_hed[] IS INITIAL.
    REFRESH: it_itm,it_tra,it_vbrk,it_vbrp,it_mseg,it_mara,it_makt,it_t001w.

    SELECT  trip_no
            invoice_no
            customer_name
            invoice_date
            weight
            company_code
            location
            lr_no
            status FROM zfreight_item
                   INTO TABLE it_itm
                   FOR ALL ENTRIES IN it_hed
                   WHERE trip_no EQ it_hed-trip_no .

    SELECT * FROM zfreight_tran
             INTO TABLE it_tra.

    SELECT vbeln
           netwr FROM vbrk
                 INTO TABLE it_vbrk
                 FOR ALL ENTRIES IN it_itm
                 WHERE vbeln EQ it_itm-invoice_no.


    SELECT vbeln
           posnr
           brgew
           netwr
           fkimg
           matnr
           werks
           arktx
           vkbur FROM vbrp
                 INTO TABLE it_vbrp
                 FOR ALL ENTRIES IN it_itm
                 WHERE vbeln EQ it_itm-invoice_no AND fklmg <> 0 .

    SELECT mblnr
           matnr
           werks
           dmbtr
           menge
           erfmg FROM mseg
                 INTO TABLE it_mseg
                 FOR ALL ENTRIES IN it_itm
                 WHERE mblnr EQ it_itm-invoice_no
                 AND shkzg EQ 'H' AND bwart EQ '351'.

    SELECT matnr
           brgew FROM mara
                 INTO TABLE it_mara
                 FOR ALL ENTRIES IN it_mseg
                 WHERE matnr EQ it_mseg-matnr.

    SELECT * FROM makt
             INTO TABLE it_makt
             FOR ALL ENTRIES IN it_mseg
             WHERE matnr EQ it_mseg-matnr.

    IF it_vbrp[] IS NOT INITIAL.

      SELECT werks name1 FROM t001w
                         INTO TABLE it_t001w
                         FOR ALL ENTRIES IN it_vbrp
                         WHERE werks EQ it_vbrp-vkbur.

    ENDIF.

    IF it_mseg[] IS NOT INITIAL.
      SELECT werks
             name1 FROM t001w
                   INTO TABLE it1_t001w
                   FOR ALL ENTRIES IN it_mseg
                   WHERE werks EQ it_mseg-werks.
    ENDIF.
    APPEND LINES OF it1_t001w TO it_t001w.

  ENDIF.

  CLEAR wa_hed.
  LOOP AT it_hed INTO wa_hed.
    CLEAR wa_itm.
    LOOP AT it_itm INTO wa_itm WHERE trip_no EQ wa_hed-trip_no.
      CLEAR wa_vbrp.
      LOOP AT it_vbrp INTO wa_vbrp WHERE vbeln EQ wa_itm-invoice_no.
        LOOP AT it_itm INTO wa_itm WHERE trip_no EQ wa_hed-trip_no.
          totqun = totqun + wa_itm-weight.
        ENDLOOP.
        READ TABLE it_itm INTO wa_itm WITH KEY invoice_no = wa_vbrp-vbeln.
        READ TABLE it_hed INTO wa_hed WITH KEY trip_no = wa_itm-trip_no.
        READ TABLE it_tra INTO wa_tra WITH KEY trip_no = wa_itm-trip_no.
        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_itm-invoice_no.
        READ TABLE it_mseg INTO wa_mseg WITH KEY mblnr = wa_itm-invoice_no.
        READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_vbrp-vkbur.
        MOVE:
        wa_itm-company_code TO wa_final-company_code,
        wa_hed-crdate  TO wa_final-crdate,
        wa_hed-trip_no  TO wa_final-trip_no,
        wa_hed-vendor_code TO wa_final-vendor_code,
        wa_hed-vendor_name TO wa_final-vendor_name,
        wa_itm-lr_no TO wa_final-lr_no,
        wa_hed-crdate TO wa_final-crdate1,
        wa_hed-vechile_number TO wa_final-vechile_number,
        wa_hed-local_vendor TO wa_final-local_vendor ,
        wa_hed-lo_vendorname TO wa_final-lo_vendorname .
        IF wa_itm-status = 'ZINV' .
          wa_final-status = 'Sales Invoice'.
        ELSEIF wa_itm-status = 'ZSTO'.
          wa_final-status = 'Stock Transfer'.
        ELSE.
          MOVE:wa_itm-status TO wa_final-status.
        ENDIF.
        MOVE:
        wa_itm-invoice_no TO wa_final-invoice_no,
        wa_vbrp-matnr TO wa_final-matnr,
        "WA_VBRP-WERKS TO WA_FINAL-WERKS,
        wa_vbrp-vkbur TO wa_final-werks,
        wa_vbrp-arktx TO wa_final-arktx,
        wa_vbrp-fkimg TO wa_final-fkimg,
        wa_itm-invoice_date TO wa_final-invoice_date,
        wa_itm-customer_name TO wa_final-customer_name,
        "WA_HED-FROM_LOC TO WA_FINAL-FROM_LOC,
        wa_t001w-name1 TO wa_final-from_loc,
        wa_itm-location TO wa_final-location.
        IF wa_vbrp-vbeln = wa_itm-invoice_no..
          MOVE:wa_vbrp-netwr TO wa_final-netwr.
          MOVE:wa_vbrp-brgew TO wa_final-brgew .
        ELSE.
          wa_mseg-mblnr = wa_itm-invoice_no.
          MOVE:wa_mseg-dmbtr TO wa_final-netwr.
          wa_final-brgew = wa_mseg-menge * wa_mara-brgew.
        ENDIF.

        wa_final-locnewcha = wa_final-local_charge / wa_final-brgew .

        CLEAR wa_tra.
        LOOP AT it_tra INTO wa_tra WHERE trip_no = wa_hed-trip_no.
          IF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'FREIGHT CHARGES'.
            ofrecha = ofrecha + wa_tra-totat_amount.

          ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LOADING CHARGES'.
            oloacha = oloacha + wa_tra-totat_amount.

          ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'UNLOADING CHARGES'.
            oinlcha = oinlcha + wa_tra-totat_amount.

          ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LR CHARGES'.
            olrcha = olrcha + wa_tra-totat_amount.

          ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'HALTING CHARGES'.
            ohalcha = ohalcha + wa_tra-totat_amount.

          ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'TOLL CHARGES'.
            otolcha = otolcha + wa_tra-totat_amount.

          ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'INSURANCE'.
            oinscha = oinscha + wa_tra-totat_amount.

          ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'PENALTY'.
            opencha = opencha + wa_tra-totat_amount.

          ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LOCAL TRANSPORT'.
            oloccha = oloccha + wa_tra-totat_amount.
          ENDIF.
        ENDLOOP.


        IF totqun <> 0.
          fre_tot = ( wa_hed-freight_charge + ofrecha ) / totqun  .
          freight1 = fre_tot * wa_final-brgew .

          hal_tot = ( wa_hed-halt_charge + ohalcha ) / totqun.
          halting1 = hal_tot * wa_final-brgew.

          lod_tot = ( wa_hed-loding_charge + oloacha ) / totqun.
          loading1 = lod_tot * wa_final-brgew.

          inl_tot = ( wa_hed-unload_charge + oinlcha ) / totqun.
          inloading1 = inl_tot * wa_final-brgew.

          lrc_tot = ( wa_hed-lr_charge  + olrcha ) / totqun.
          lrcharges1 = lrc_tot * wa_final-brgew.

          loc_trn =  wa_hed-loding_charge / totqun .
          localtrannew1 = loc_trn * wa_final-brgew .

          tol_tot = otolcha / totqun.
          tollcharge1 = tol_tot * wa_final-brgew.

          ins_tot = oinscha / totqun.
          insurence1 = ins_tot * wa_final-brgew.

          pen_tot = opencha / totqun.
          penalty1 = pen_tot * wa_final-brgew.

          loc_tot = oloccha / totqun.
          localtran1 = loc_tot * wa_final-brgew.

        ENDIF.
        LOOP AT it_itm INTO wa_itm WHERE trip_no EQ wa_final-trip_no AND location EQ wa_final-location.
          tra_tot = tra_tot +  wa_itm-weight.
        ENDLOOP.

        LOOP AT it_tra INTO wa_tra WHERE trip_no EQ wa_final-trip_no.

          IF wa_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'FREIGHT CHARGES'.
            ofrecha1 = ofrecha1 + wa_tra-totat_amount.

          ELSEIF wa_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LOADING CHARGES'.
            oloacha1 = oloacha1 + wa_tra-totat_amount.

          ELSEIF wa_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'UNLOADING CHARGES'.
            oinlcha1 = oinlcha1 + wa_tra-totat_amount.

          ELSEIF wa_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LR CHARGES'.
            olrcha1 = olrcha1 + wa_tra-totat_amount.

          ELSEIF wa_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'HALTING CHARGES'.
            ohalcha1 = ohalcha1 + wa_tra-totat_amount.

          ELSEIF wa_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'TOLL CHARGES'.
            otolcha1 = otolcha1 + wa_tra-totat_amount.

          ELSEIF wa_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'INSURANCE'.
            oinscha1 = oinscha1 + wa_tra-totat_amount.

          ELSEIF wa_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'PENALTY'.
            opencha1 = opencha1 + wa_tra-totat_amount.

          ELSEIF wa_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LOCAL TRANSPORT'.
            oloccha1 = oloccha1 + wa_tra-totat_amount.

          ENDIF.
        ENDLOOP.

        IF  tra_tot <> 0.
          IF wa_final-brgew <> 0.
            fre_tot1 =  ofrecha1  / tra_tot.
            freight2 =  fre_tot1  * wa1_final-brgew.

            lod_tot1 = oloacha1 / tra_tot.
            loading2 = lod_tot1 * wa1_final-brgew .

            inl_tot1 = oinlcha1  / tra_tot.
            inloading2 =  inl_tot1 * wa1_final-brgew.

            lrc_tot1 =  olrcha1 / tra_tot.
            lrcharges2 =  lrc_tot1 * wa1_final-brgew.

            hal_tot1 =  ohalcha1 / tra_tot.
            halting2 =   hal_tot1 * wa1_final-brgew.

            tol_tot1 = otolcha1   / tra_tot.
            tollcharge2 =  tol_tot1 * wa1_final-brgew.

            ins_tot1 =  oinscha1   / tra_tot.
            insurence2 =  ins_tot1 * wa1_final-brgew.

            pen_tot1 = opencha1 / tra_tot.
            penalty2 =  pen_tot1 * wa1_final-brgew.

            loc_tot1 = oloccha1  / tra_tot.
            localtran2 =  loc_tot1 * wa1_final-brgew.

            wa_final-frecha = freight1 + freight2.
            wa_final-halcha = halting1 + halting2.
            wa_final-inlcha = inloading1 + inloading2.
            wa_final-loacha = loading1 + loading2.
            wa_final-lrcha = lrcharges1 + lrcharges2.
            wa_final-tolcha = tollcharge1 + tollcharge2.
            wa_final-inscha = insurence1 + insurence2.
            wa_final-pencha = penalty1 + penalty2.
            wa_final-loccha = localtran1 + localtran2.

            wa_final-tot_frei = wa_final-frecha + wa_final-halcha + wa_final-inlcha +
                                   wa_final-loacha + wa_final-lrcha +  wa_final-tolcha  +
                                   wa_final-inscha + wa_final-pencha + wa_final-loccha +
                                   wa_final-locnewcha .

          ENDIF.
        ENDIF.
        APPEND: wa_final TO it_final.
        CLEAR: wa_final , tra_tot , totqun,
               ofrecha, oloacha , oinlcha , olrcha , ohalcha , otolcha , oinscha , opencha , oloccha,
               ofrecha1, oloacha1 , oinlcha1 , olrcha1 , ohalcha1 , otolcha1 , oinscha1 , opencha1 , oloccha1,        " ZFREIGHT_TRAN TABLE FIELDS VALUE SUM               " ZFREIGHT_TRAN TABLE FIELDS VALUE SUM
               freight1, halting1, loading1, inloading1, lrcharges1, tollcharge1, insurence1, penalty1, localtran1,   " FINAL MOVE VALUES AND FREIGHT_ITEM TABLE TOTAL WEIGHT
               freight2, halting2, loading2, inloading2, lrcharges2, tollcharge2, insurence2, penalty2, localtran2,   " LOCAL VARIABLE DECLARE FOR CALCULATING FUNCTION
               fre_tot ,lod_tot , inl_tot , lrc_tot , hal_tot , tol_tot ,pen_tot , loc_tot ,ins_tot ,
               fre_tot1,lod_tot1, inl_tot1, lrc_tot1, hal_tot1, tol_tot1, pen_tot1, loc_tot1, ins_tot1.
      ENDLOOP.
      IF wa_vbrp-vbeln <> wa_itm-invoice_no .
        CLEAR wa_mseg.
        LOOP AT it_mseg INTO wa_mseg WHERE mblnr EQ wa_itm-invoice_no.
          CLEAR wa_itm.
          LOOP AT it_itm INTO wa_itm WHERE trip_no EQ wa_hed-trip_no.
            totqun = totqun + wa_itm-weight.
          ENDLOOP.
          READ TABLE it_itm INTO wa_itm WITH KEY invoice_no = wa_mseg-mblnr.
          READ TABLE it_hed INTO wa_hed WITH KEY trip_no = wa_itm-trip_no.
          READ TABLE it_tra INTO wa_tra WITH KEY trip_no = wa_itm-trip_no.
          READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mseg-matnr.
          READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_mseg-matnr.
          READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_mseg-werks.
          MOVE:
          wa_itm-company_code TO wa1_final-company_code,
          wa_hed-crdate  TO wa1_final-crdate,
          wa_hed-trip_no  TO wa1_final-trip_no,
          wa_hed-vendor_code TO wa1_final-vendor_code,
          wa_hed-vendor_name TO wa1_final-vendor_name,
          wa_itm-lr_no TO wa1_final-lr_no,
          wa_hed-crdate TO wa1_final-crdate1,
          wa_hed-vechile_number TO wa1_final-vechile_number.
          IF wa_itm-status = 'ZINV' .
            wa1_final-status = 'Sales Invoice'.
          ELSEIF wa_itm-status = 'ZSTO'.
            wa1_final-status = 'Stock Transfer'.
          ELSE.
            MOVE:wa_itm-status TO wa1_final-status.
          ENDIF.
          MOVE:
          wa_itm-invoice_no TO wa1_final-invoice_no,
          wa_mseg-matnr TO wa1_final-matnr,
          wa_mseg-werks TO wa1_final-werks,
          wa_makt-maktx TO wa1_final-arktx,
          wa_mseg-menge TO wa1_final-fkimg,
          wa_itm-invoice_date TO wa1_final-invoice_date,
          wa_itm-customer_name TO wa1_final-customer_name,
          "WA_HED-FROM_LOC TO WA1_FINAL-FROM_LOC,
          wa_t001w-name1 TO wa1_final-from_loc,
          wa_itm-location TO wa1_final-location.
          IF wa_mseg-mblnr = wa_itm-invoice_no.
            MOVE: wa_mseg-dmbtr TO wa1_final-netwr. "#EC CI_FLDEXT_OK[2610650]    "Added by SPLABAP during code remediation
            wa1_final-brgew = wa_mseg-menge * wa_mara-brgew.
          ENDIF.

          CLEAR wa_tra.
          LOOP AT it_tra INTO wa_tra WHERE trip_no = wa_hed-trip_no.
            IF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'FREIGHT CHARGES'.
              ofrecha = ofrecha + wa_tra-totat_amount.

            ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LOADING CHARGES'.
              oloacha = oloacha + wa_tra-totat_amount.

            ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'UNLOADING CHARGES'.
              oinlcha = oinlcha + wa_tra-totat_amount.

            ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LR CHARGES'.
              olrcha = olrcha + wa_tra-totat_amount.

            ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'HALTING CHARGES'.
              ohalcha = ohalcha + wa_tra-totat_amount.

            ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'TOLL CHARGES'.
              otolcha = otolcha + wa_tra-totat_amount.

            ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'INSURANCE'.
              oinscha = oinscha + wa_tra-totat_amount.

            ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'PENALTY'.
              opencha = opencha + wa_tra-totat_amount.

            ELSEIF wa_hed-from_loc = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LOCAL TRANSPORT'.
              oloccha = oloccha + wa_tra-totat_amount.
            ENDIF.
          ENDLOOP.


          IF totqun <> 0.
            fre_tot = ( wa_hed-freight_charge + ofrecha ) / totqun  .
            freight1 = fre_tot * wa1_final-brgew .

            hal_tot = ( wa_hed-halt_charge + ohalcha ) / totqun.
            halting1 = hal_tot * wa1_final-brgew.

            lod_tot = ( wa_hed-loding_charge + oloacha ) / totqun.
            loading1 = lod_tot * wa1_final-brgew.

            inl_tot = ( wa_hed-unload_charge + oinlcha ) / totqun.
            inloading1 = inl_tot * wa1_final-brgew.

            lrc_tot = ( wa_hed-lr_charge  + olrcha ) / totqun.
            lrcharges1 = lrc_tot * wa1_final-brgew.

            tol_tot = otolcha / totqun.
            tollcharge1 = tol_tot * wa1_final-brgew.

            ins_tot = oinscha / totqun.
            insurence1 = ins_tot * wa1_final-brgew.

            pen_tot = opencha / totqun.
            penalty1 = pen_tot * wa1_final-brgew.

            loc_tot = oloccha / totqun.
            localtran1 = loc_tot * wa1_final-brgew.

            loc_trn  = wa_hed-local_charge / totqun . "added on 28/7
            localtrannew1 = loc_trn * wa1_final-brgew .


          ENDIF.
          LOOP AT it_itm INTO wa_itm WHERE trip_no EQ wa1_final-trip_no AND location EQ wa1_final-location.
            tra_tot = tra_tot +  wa_itm-weight.
          ENDLOOP.

          LOOP AT it_tra INTO wa_tra WHERE trip_no EQ wa1_final-trip_no.

            IF wa1_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'FREIGHT CHARGES'.
              ofrecha1 = ofrecha1 + wa_tra-totat_amount.

            ELSEIF wa1_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LOADING CHARGES'.
              oloacha1 = oloacha1 + wa_tra-totat_amount.

            ELSEIF wa1_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'UNLOADING CHARGES'.
              oinlcha1 = oinlcha1 + wa_tra-totat_amount.

            ELSEIF wa1_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LR CHARGES'.
              olrcha1 = olrcha1 + wa_tra-totat_amount.

            ELSEIF wa1_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'HALTING CHARGES'.
              ohalcha1 = ohalcha1 + wa_tra-totat_amount.

            ELSEIF wa1_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'TOLL CHARGES'.
              otolcha1 = otolcha1 + wa_tra-totat_amount.

            ELSEIF wa1_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'INSURANCE'.
              oinscha1 = oinscha1 + wa_tra-totat_amount.

            ELSEIF wa1_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'PENALTY'.
              opencha1 = opencha1 + wa_tra-totat_amount.

            ELSEIF wa1_final-location = wa_tra-charge_loc AND wa_tra-charge_type EQ 'LOCAL TRANSPORT'.
              oloccha1 = oloccha1 + wa_tra-totat_amount.

            ENDIF.
          ENDLOOP.

          IF  tra_tot <> 0.
            IF wa1_final-brgew <> 0.
              fre_tot1 =  ofrecha1  / tra_tot.
              freight2 =  fre_tot1  * wa1_final-brgew.

              lod_tot1 = oloacha1 / tra_tot.
              loading2 = lod_tot1 * wa1_final-brgew .

              inl_tot1 = oinlcha1  / tra_tot.
              inloading2 =  inl_tot1 * wa1_final-brgew.

              lrc_tot1 =  olrcha1 / tra_tot.
              lrcharges2 =  lrc_tot1 * wa1_final-brgew.

              hal_tot1 =  ohalcha1 / tra_tot.
              halting2 =   hal_tot1 * wa1_final-brgew.

              tol_tot1 = otolcha1   / tra_tot.
              tollcharge2 =  tol_tot1 * wa1_final-brgew.

              ins_tot1 =  oinscha1   / tra_tot.
              insurence2 =  ins_tot1 * wa1_final-brgew.

              pen_tot1 = opencha1 / tra_tot.
              penalty2 =  pen_tot1 * wa1_final-brgew.

              loc_tot1 = oloccha1  / tra_tot.
              localtran2 =  loc_tot1 * wa1_final-brgew.

              wa1_final-frecha = freight1 + freight2.
              wa1_final-halcha = halting1 + halting2.
              wa1_final-inlcha = inloading1 + inloading2.
              wa1_final-loacha = loading1 + loading2.
              wa1_final-lrcha = lrcharges1 + lrcharges2.
              wa1_final-tolcha = tollcharge1 + tollcharge2.
              wa1_final-inscha = insurence1 + insurence2.
              wa1_final-pencha = penalty1 + penalty2.
              wa1_final-loccha = localtran1 + localtran2.

              wa1_final-locnewcha = localtrannew1 + localtrannew2 .

              wa1_final-tot_frei = wa1_final-frecha + wa1_final-halcha + wa1_final-inlcha +
                                   wa1_final-loacha + wa1_final-lrcha +  wa1_final-tolcha  +
                                   wa1_final-inscha + wa1_final-pencha + wa1_final-loccha +
                                   wa1_final-locnewcha .

              " WA1_FINAL-LOCNEWCHA = LOCALTRANNEW1 + LOCALTRANNEW2.
            ENDIF.
          ENDIF.
          APPEND: wa1_final TO it1_final.
          CLEAR: wa1_final , tra_tot , totqun,
                 ofrecha, oloacha , oinlcha , olrcha , ohalcha , otolcha , oinscha , opencha , oloccha,
                 ofrecha1, oloacha1 , oinlcha1 , olrcha1 , ohalcha1 , otolcha1 , oinscha1 , opencha1 , oloccha1,               " ZFREIGHT_TRAN TABLE FIELDS VALUE SUM               " ZFREIGHT_TRAN TABLE FIELDS VALUE SUM
                 freight1, halting1, loading1, inloading1, lrcharges1, tollcharge1, insurence1, penalty1, localtran1,                         " FINAL MOVE VALUES AND FREIGHT_ITEM TABLE TOTAL WEIGHT
                 freight2, halting2, loading2, inloading2, lrcharges2, tollcharge2, insurence2, penalty2, localtran2,
                 localtrannew1,localtrannew2," LOCAL VARIABLE DECLARE FOR CALCULATING FUNCTION
                 fre_tot ,lod_tot , inl_tot , lrc_tot , hal_tot , tol_tot ,pen_tot , loc_tot ,ins_tot ,
                 fre_tot1,lod_tot1, inl_tot1, lrc_tot1, hal_tot1, tol_tot1, pen_tot1, loc_tot1, ins_tot1.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  APPEND LINES OF it1_final TO it_final.
  IF it_final[] IS NOT INITIAL.
    SORT it_final[] BY trip_no vendor_code invoice_no.
  ENDIF.

  REFRESH: lt_tripsheet.

  LOOP AT it_final INTO wa_final.
    CLEAR ls_tripsheet.
    MOVE-CORRESPONDING wa_final TO ls_tripsheet.
    APPEND ls_tripsheet TO lt_tripsheet.
    CLEAR wa_final.
  ENDLOOP.


ENDFUNCTION.
