*&---------------------------------------------------------------------*
*& Report ZMM_DMS_STOCK_TRANSFER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_dms_stock_transfer.
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 11.06.2024
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                :
*
*  Business Logic            : DMS stock transfer plant to plant
*
*  Released on Date          :
*
*=======================================================================
*****************From Plant Details**************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : frm_dist TYPE kna1-kunnr MODIF ID a,  "from distributor
               frm_plnt TYPE mard-werks MODIF ID a,  "from plant
               frm_strg TYPE lgort_d MODIF ID a,      "from strg loc.
               frm_sorg TYPE vkorg MODIF ID a,       "from sales org
               frm_divi TYPE vtweg MODIF ID a.        "from division
SELECTION-SCREEN END OF BLOCK b1.

*****************To Plant Details**************

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : to_dist TYPE kna1-kunnr MODIF ID a,   "To distributor
               to_plnt TYPE mard-werks MODIF ID a,   "To plant
               to_strg TYPE lgort_d MODIF ID a,      "To strg loc.
               to_sorg TYPE vkorg MODIF ID a,        "To sales org
               to_divi TYPE vtweg MODIF ID a.        "To division
SELECTION-SCREEN END OF BLOCK b2.

DATA :    lv_matnr TYPE mara-matnr.
SELECT-OPTIONS : so_matnr FOR lv_matnr.

SELECTION-SCREEN : SKIP 1.

PARAMETERS : r1 RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND u1,   "mat ext & batch creation
             r2 RADIOBUTTON GROUP g1,               "price update
             r3 RADIOBUTTON GROUP g1,               "stock update
             r4 RADIOBUTTON GROUP g1.               "stock zero


CLASS lcl_stktrnsfr DEFINITION FINAL.

  PUBLIC SECTION.

    DATA: wa_headerdata           TYPE bapimathead,
          wa_plantdata            TYPE bapi_marc,
          wa_plantdatax           TYPE bapi_marcx,
          wa_storagelocationdata  TYPE bapi_mard,
          wa_storagelocationdatax TYPE bapi_mardx,
          wa_valuationdata        TYPE bapi_mbew,
          wa_valuationdatax       TYPE bapi_mbewx,
          wa_salesdata            TYPE bapi_mvke,
          wa_salesdatax           TYPE bapi_mvkex,
          frm_lgort               TYPE lgort_d VALUE 'D1',
          to_lgort                TYPE lgort_d VALUE 'D1',
          frm_vkorg               TYPE vkorg VALUE 'SDMS',
          to_vkorg                TYPE vkorg VALUE 'SDMS',
          frm_vtweg               TYPE vtweg VALUE '20',
          to_vtweg                TYPE vtweg VALUE '20',
          lv_vprsv                TYPE vprsv VALUE 'S'.

    METHODS : mat_ext,
      price_update,
      stk_transfer,
      zero_stock,
      marc   IMPORTING matnr     TYPE matnr          "plant data
                       from_plnt TYPE werks_d
                       to_plnt   TYPE werks_d,
      marcx  IMPORTING to_plnt   TYPE werks_d,
      mard   IMPORTING matnr     TYPE matnr          "storage loc data
                       from_plnt TYPE werks_d
                       to_plnt   TYPE werks_d
                       from_sloc TYPE lgort_d,
      mardx  IMPORTING to_plnt   TYPE werks_d,
      mbew   IMPORTING matnr     TYPE matnr          "valuation data
                       from_plnt TYPE werks_d
                       to_plnt   TYPE werks_d,
      mbewx  IMPORTING to_plnt   TYPE werks_d,
      mvke   IMPORTING matnr      TYPE matnr         "sales data
                       from_vkorg TYPE vkorg
                       from_vtweg TYPE vtweg,
      mvkex,
      header IMPORTING matnr  TYPE matnr.

ENDCLASS.


CLASS lcl_stktrnsfr IMPLEMENTATION.

  METHOD mat_ext.
*****************structure************
    TYPES : BEGIN OF ty_mchb,
              frm_dist TYPE zdist,
              frm_plnt TYPE werks_d,
              to_dist  TYPE zdist,
              to_plnt  TYPE werks_d,
              matnr    TYPE matnr,
              charg    TYPE charg_d,
              type     TYPE bapi_mtype,
              msg      TYPE zremark,
            END OF ty_mchb.
    DATA : wa_return TYPE bapiret2,
           lt_alv    TYPE TABLE OF ty_mchb,
           lv_matnr  TYPE matnr18,
           lv_type   TYPE bapi_mtype,
           lv_msg    TYPE string.

*************batch creation class***********
    DATA : lo_batch TYPE REF TO zcl_api_dms_grn_inv.
    CREATE OBJECT lo_batch.

**********************get the stock details**************8
    SELECT matnr,werks,lgort,charg FROM mchb INTO TABLE @DATA(lt_mchb)
                                             WHERE matnr IN @so_matnr
                                             AND   werks IN ( @frm_plnt , @to_plnt ).
    IF sy-subrc = 0.

      SORT : lt_mchb BY werks lgort matnr charg.

*******************get the new plant material details*******************
      SELECT matnr,werks,lgort FROM mard INTO TABLE @DATA(lt_mard)
                                         WHERE werks = @to_plnt
                                         AND   lgort = @to_lgort.
      IF sy-subrc = 0.
        SORT : lt_mard BY werks lgort matnr.
      ENDIF.
********************data fill & call bapi process( material save )************
      LOOP AT lt_mchb ASSIGNING FIELD-SYMBOL(<fs_mchb>) WHERE werks = frm_plnt
                                                        AND   lgort = frm_lgort.

        AT NEW matnr.

          CLEAR : lv_type,lv_msg.
***************check material already extended or not********
          READ TABLE lt_mard TRANSPORTING NO FIELDS WITH KEY werks = to_plnt
                                                             lgort = to_lgort
                                                             matnr = <fs_mchb>-matnr BINARY SEARCH.
          IF sy-subrc NE 0.
*****************fill the header data****************
            CALL METHOD header( matnr = <fs_mchb>-matnr ).
*****************fill the plant data****************
            CALL METHOD marc(
              EXPORTING
                matnr     = <fs_mchb>-matnr
                from_plnt = frm_plnt
                to_plnt   = to_plnt ).
*****************fill the plant data flag****************
            CALL METHOD marcx( to_plnt = to_plnt ).
*****************fill the storage location data****************
            CALL METHOD mard(
              EXPORTING
                matnr     = <fs_mchb>-matnr
                from_plnt = frm_plnt
                to_plnt   = to_plnt
                from_sloc = frm_lgort ).
*****************fill the storage location data flag****************
            CALL METHOD mardx( to_plnt = to_plnt ).
*****************fill the sales data****************
            CALL METHOD mvke(
              EXPORTING
                matnr      = <fs_mchb>-matnr
                from_vkorg = frm_vkorg
                from_vtweg = frm_vtweg ).
*****************fill the sales data flag****************
            CALL METHOD mvkex( ).
*****************fill the valuation data****************
            CALL METHOD mbew(
              EXPORTING
                matnr     = <fs_mchb>-matnr
                from_plnt = frm_plnt
                to_plnt   = to_plnt ).
*****************fill the valuation data flag****************
            CALL METHOD mbewx( to_plnt = to_plnt ).
*******************************BAPI Process**********************
            CLEAR : wa_return.
            CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
              EXPORTING
                headdata             = wa_headerdata
                plantdata            = wa_plantdata
                plantdatax           = wa_plantdatax
                storagelocationdata  = wa_storagelocationdata
                storagelocationdatax = wa_storagelocationdatax
                valuationdata        = wa_valuationdata
                valuationdatax       = wa_valuationdatax
                salesdata            = wa_salesdata
                salesdatax           = wa_salesdatax
              IMPORTING
                return               = wa_return.
            IF wa_return-type = 'S'.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.
            ELSE.
              lv_type = 'E'.
              lv_msg  = wa_return-message.
            ENDIF.
          ELSE.
            lv_msg  = |Already Material extended|.
            lv_type = 'S'.
          ENDIF.
        ENDAT.

        IF lv_type = 'S'.
*************check batch already created or not************
          READ TABLE lt_mchb TRANSPORTING NO FIELDS WITH KEY werks = to_plnt
                                                             lgort = to_lgort
                                                             matnr = <fs_mchb>-matnr
                                                             charg = <fs_mchb>-charg BINARY SEARCH.
          IF sy-subrc NE 0.
**************create new batch**************
            lv_matnr = <fs_mchb>-matnr.
            lo_batch->batch_create(
              EXPORTING
                material   = lv_matnr
                plant      = to_plnt
                batch      = <fs_mchb>-charg
                storageloc = to_lgort
              IMPORTING
                type       = lv_type
                message    = lv_msg ).
          ELSE.
            lv_msg  = | { lv_msg } , Already batch created |.
            lv_type = 'S'.
          ENDIF.
        ENDIF.

        APPEND VALUE #( frm_dist = frm_dist
                        frm_plnt = frm_plnt
                        to_dist  = to_dist
                        to_plnt  = to_plnt
                        matnr    = <fs_mchb>-matnr
                        charg    = <fs_mchb>-charg
                        type     = lv_type
                        msg      = lv_msg ) TO lt_alv.

      ENDLOOP.

***************Display the ALV
      CALL FUNCTION 'Z_POPUP_ALV'
        TABLES
          it_alv = lt_alv.

    ELSE.

      MESSAGE : 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

  ENDMETHOD.

  METHOD stk_transfer.
*****************structure************
    TYPES : BEGIN OF ty_alv,
              frm_dist TYPE zdist,
              frm_plnt TYPE werks_d,
              to_dist  TYPE zdist,
              to_plnt  TYPE werks_d,
              matnr    TYPE matnr,
              charg    TYPE charg_d,
              clabs    TYPE labst,
              belnr    TYPE belnr_d,
              type     TYPE bapi_mtype,
              msg      TYPE zremark,
            END OF ty_alv.

    DATA : lt_alv   TYPE TABLE OF ty_alv,
           lv_belnr TYPE belnr_d,
           lv_type  TYPE bapi_mtype,
           lv_msg   TYPE string.

    DATA: lo_inv_grn TYPE REF TO zcl_api_dms_grn_inv.     "data dec for physical inventory api class
    CREATE OBJECT lo_inv_grn.
    DATA : lt_create TYPE STANDARD TABLE OF  bapi_physinv_create_items,   "for physical inventory bapi
           lt_count  TYPE TABLE OF bapi_physinv_count_items,
           lv_item   TYPE posnr,
           lv_matnr  TYPE char50,
           lv_price  TYPE wrbtr.

****************fetch the standard price for the material*************
    SELECT matnr,bwkey,vprsv,verpr,stprs FROM mbew INTO TABLE @DATA(lt_mbew)
                                         WHERE matnr IN @so_matnr
                                         AND   bwkey EQ @to_plnt.
    IF sy-subrc = 0.
      SORT : lt_mbew BY matnr bwkey.
    ENDIF.
**********************get the stock details**************8
    SELECT matnr,werks,lgort,charg,clabs FROM mchb INTO TABLE @DATA(lt_mchb)
                                         WHERE matnr IN @so_matnr
                                         AND   werks IN ( @frm_plnt , @to_plnt )
                                         AND   clabs NE @abap_false.
    IF sy-subrc = 0.

      SORT : lt_mchb BY werks lgort matnr charg.

**************get the material details**************
      SELECT matnr,meins FROM mara INTO TABLE @DATA(lt_mara) FOR ALL ENTRIES IN @lt_mchb
                                                             WHERE matnr = @lt_mchb-matnr.
      IF sy-subrc = 0.
        SORT : lt_mara BY matnr.
      ENDIF.

********************data fill & call bapi process( material save )************
      LOOP AT lt_mchb ASSIGNING FIELD-SYMBOL(<fs_mchb>) WHERE werks = frm_plnt
                                                        AND   lgort = frm_lgort.

        CLEAR : lv_price.

        READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY matnr = <fs_mchb>-matnr
                                                       bwkey = to_plnt BINARY SEARCH.
        IF sy-subrc = 0.
          IF to_plnt(1) = 'D'.
            lv_price = ls_mbew-stprs.
          ELSE.
            lv_price = ls_mbew-verpr.
          ENDIF.
          CLEAR : ls_mbew.
          IF lv_price = 0.
*****************fill the DATA from alv****************
            APPEND VALUE #( frm_dist = frm_dist
                            frm_plnt = frm_plnt
                            to_dist  = to_dist
                            to_plnt  = to_plnt
                            matnr    = <fs_mchb>-matnr
                            charg    = <fs_mchb>-charg
                            clabs    = <fs_mchb>-clabs
                            type     = 'E'
                            msg      = 'Material Price not updated' ) TO lt_alv.
            CONTINUE.
          ENDIF.
        ELSE.
*****************fill the DATA from alv****************
          APPEND VALUE #( frm_dist = frm_dist
                          frm_plnt = frm_plnt
                          to_dist  = to_dist
                          to_plnt  = to_plnt
                          matnr    = <fs_mchb>-matnr
                          charg    = <fs_mchb>-charg
                          clabs    = <fs_mchb>-clabs
                          type     = 'E'
                          msg      = 'Material not Extended' ) TO lt_alv.
          CONTINUE.

        ENDIF.


        READ TABLE lt_mchb TRANSPORTING NO FIELDS WITH KEY werks = to_plnt
                                                           lgort = to_lgort
                                                           matnr = <fs_mchb>-matnr
                                                           charg = <fs_mchb>-charg BINARY SEARCH.
        IF sy-subrc NE 0 .

          READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = <fs_mchb>-matnr BINARY SEARCH.

*item data filling
          APPEND VALUE bapi_physinv_create_items( material     = <fs_mchb>-matnr
                                                  batch        = <fs_mchb>-charg ) TO lt_create.
          lv_item = 1 .
*item count data filling
          APPEND VALUE bapi_physinv_count_items( material      = <fs_mchb>-matnr
                                                 batch         = <fs_mchb>-charg
                                                 entry_qnt     = <fs_mchb>-clabs
                                                 item          = lv_item
                                                 entry_uom     = ls_mara-meins ) TO lt_count.
***************physical inventory ref***************
          lv_matnr = |{ frm_plnt }-{ <fs_mchb>-matnr }-{ sy-datum }|.

          DATA(ls_header) = VALUE bapi_physinv_create_head( plant        = to_plnt
                                                            stge_loc     = to_lgort
                                                            phys_inv_ref = lv_matnr  ).
**********call the physical inventory bapi**********************
          lo_inv_grn->physical_bapi(
            EXPORTING
              ls_header     = ls_header
              lt_itemcount  = lt_count
              lt_itemcreate = lt_create
            IMPORTING
              message       = lv_msg
              type          = lv_type
              belnr         = lv_belnr ).
        ELSE.
          lv_type = 'E'.
          lv_msg  = 'Already batch stock available'.
        ENDIF.
*****************fill the data from alv****************
        APPEND VALUE #( frm_dist = frm_dist
                        frm_plnt = frm_plnt
                        to_dist  = to_dist
                        to_plnt  = to_plnt
                        matnr    = <fs_mchb>-matnr
                        charg    = <fs_mchb>-charg
                        clabs    = <fs_mchb>-clabs
                        belnr    = lv_belnr
                        type     = lv_type
                        msg      = lv_msg ) TO lt_alv.

        CLEAR : lv_type,lv_msg,lv_belnr.
      ENDLOOP.
***************Display the ALV
      CALL FUNCTION 'Z_POPUP_ALV'
        TABLES
          it_alv = lt_alv.

    ELSE.

      MESSAGE : 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.
  ENDMETHOD.

  METHOD zero_stock.
*****************structure************
    TYPES : BEGIN OF ty_alv,
              frm_dist TYPE zdist,
              frm_plnt TYPE werks_d,
              matnr    TYPE matnr,
              charg    TYPE charg_d,
              old_stk  TYPE labst,
              new_stk  TYPE labst,
              belnr    TYPE belnr_d,
              type     TYPE bapi_mtype,
              msg      TYPE zremark,
            END OF ty_alv.

    DATA : lt_alv   TYPE TABLE OF ty_alv,
           lv_belnr TYPE belnr_d,
           lv_type  TYPE bapi_mtype,
           lv_msg   TYPE string.

    DATA: lo_inv_grn TYPE REF TO zcl_api_dms_grn_inv.     "data dec for physical inventory api class
    CREATE OBJECT lo_inv_grn.
    DATA : lt_create TYPE STANDARD TABLE OF  bapi_physinv_create_items,   "for physical inventory bapi
           lt_count  TYPE TABLE OF bapi_physinv_count_items,
           lv_item   TYPE posnr,
           lv_matnr  TYPE char50.

**********************get the stock details**************
    SELECT matnr,werks,lgort,charg,clabs FROM mchb INTO TABLE @DATA(lt_mchb)
                                         WHERE matnr IN @so_matnr
                                         AND   werks =  @frm_plnt
                                         AND   lgort =  @frm_lgort
                                         AND   clabs NE @abap_false.
    IF sy-subrc = 0.

      SORT : lt_mchb BY werks lgort matnr charg.

**************get the material details**************
      SELECT matnr,meins FROM mara INTO TABLE @DATA(lt_mara) FOR ALL ENTRIES IN @lt_mchb
                                                             WHERE matnr = @lt_mchb-matnr.
      IF sy-subrc = 0.
        SORT : lt_mara BY matnr.
      ENDIF.

********************data fill & call bapi process( material save )************
      LOOP AT lt_mchb ASSIGNING FIELD-SYMBOL(<fs_mchb>).

        READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = <fs_mchb>-matnr BINARY SEARCH.

*item data filling
        APPEND VALUE bapi_physinv_create_items( material     = <fs_mchb>-matnr
                                                batch        = <fs_mchb>-charg ) TO lt_create.
        lv_item = 1 + lv_item.
*item count data filling
        APPEND VALUE bapi_physinv_count_items( material      = <fs_mchb>-matnr
                                               batch         = <fs_mchb>-charg
*                                               entry_qnt     = 0
                                               zero_count    = abap_true
                                               item          = lv_item
                                               entry_uom     = ls_mara-meins ) TO lt_count.
***************physical inventory ref***************
        lv_matnr = |{ frm_plnt }-{ <fs_mchb>-matnr }-{ sy-datum }|.

        DATA(ls_header) = VALUE bapi_physinv_create_head( plant        = frm_plnt
                                                          stge_loc     = frm_lgort
                                                          phys_inv_ref = lv_matnr  ).
**********call the physical inventory bapi**********************
        lo_inv_grn->physical_bapi(
          EXPORTING
            ls_header     = ls_header
            lt_itemcount  = lt_count
            lt_itemcreate = lt_create
          IMPORTING
            message       = lv_msg
            type          = lv_type
            belnr         = lv_belnr ).
*****************fill the data from alv****************
        APPEND VALUE #( frm_dist = frm_dist
                        frm_plnt = frm_plnt
                        matnr    = <fs_mchb>-matnr
                        charg    = <fs_mchb>-charg
                        old_stk  = <fs_mchb>-clabs
                        new_stk  = 0
                        belnr    = lv_belnr
                        type     = lv_type
                        msg      = lv_msg ) TO lt_alv.

        CLEAR : lv_type,lv_msg,lv_belnr.
      ENDLOOP.
***************Display the ALV
      CALL FUNCTION 'Z_POPUP_ALV'
        TABLES
          it_alv = lt_alv.

    ELSE.

      MESSAGE : 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.
  ENDMETHOD.

  METHOD price_update.
*****************structure************
    TYPES : BEGIN OF ty_alv,
              frm_dist  TYPE zdist,
              frm_plnt  TYPE werks_d,
              matnr     TYPE matnr,
              frm_price TYPE stprs,
              to_dist   TYPE zdist,
              to_plnt   TYPE werks_d,
              old_price TYPE stprs,
              new_price TYPE stprs,
              type      TYPE bapi_mtype,
              msg       TYPE zremark,
            END OF ty_alv.
    DATA : lt_alv    TYPE TABLE OF ty_alv,
           lv_type   TYPE bapi_mtype,
           lv_msg    TYPE string,
           lv_newmat TYPE matnr.
    DATA : lv_matnr TYPE matnr18,
           amount   TYPE bapi_price.
    DATA: lo_inv_grn TYPE REF TO zcl_api_dms_grn_inv.     "data dec for physical inventory api class
    CREATE OBJECT lo_inv_grn.
****************fetch the standard price for the material*************
    SELECT matnr,bwkey,vprsv,verpr,stprs FROM mbew INTO TABLE @DATA(lt_mbew)
                                         WHERE matnr IN @so_matnr
                                         AND   bwkey IN ( @frm_plnt , @to_plnt ).
    IF sy-subrc = 0.
      SORT : lt_mbew BY bwkey matnr.

      LOOP AT lt_mbew ASSIGNING FIELD-SYMBOL(<fs_mbew>) WHERE bwkey = frm_plnt.
*******************get material old price******************

*        READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY matnr = <fs_mbew>-matnr
*                                                       bwkey = frm_plnt BINARY SEARCH.
*        IF sy-subrc EQ 0.

        CLEAR : lv_newmat.
        IF <fs_mbew>-matnr(1) NE 'Z'.
          lv_newmat = |Z{ <fs_mbew>-matnr }|.
        ENDIF.

        SELECT SINGLE matnr,stprs FROM mbew INTO @DATA(ls_new) WHERE matnr = @lv_newmat
                                                         AND   bwkey = @to_plnt.
        IF sy-subrc EQ 0.
**********check material price & new material price same or not***********
          IF <fs_mbew>-stprs NE ls_new-stprs.

            lv_matnr = lv_newmat.
            amount   = <fs_mbew>-stprs.
**********call the price change method*********
            lo_inv_grn->price_change(
                          EXPORTING
                            material = lv_matnr  " Material Number (18 Characters)
                            plant    = to_plnt   " Valuation area
                            date     = sy-datum  " Date for Pricing and Exchange Rate
                            amount   = amount     " Price in BAPI currency format
                          IMPORTING
                            message  = lv_msg
                            type     = lv_type
            ).

          ELSE.
            lv_type = 'S'.
            lv_msg  = 'Already same price updated'.
          ENDIF.
          CLEAR : ls_new.
        ELSE.
          lv_type = 'E'.
          lv_msg  = |material { <fs_mbew>-matnr } not maintained in plant - { to_plnt }|.
        ENDIF.
*        ELSE.
*          lv_type = 'E'.
*          lv_msg  = |material { <fs_mbew>-matnr } not maintained in plant - { to_plnt }|.
*        ENDIF.
*****************fill the data from alv****************
        APPEND VALUE #( frm_dist  = frm_dist
                        frm_plnt  = frm_plnt
                        to_dist   = to_dist
                        to_plnt   = to_plnt
                        matnr     = <fs_mbew>-matnr
                        frm_price = <fs_mbew>-stprs
                        new_price = <fs_mbew>-stprs
                        old_price = ls_new-stprs
                        type      = lv_type
                        msg       = lv_msg ) TO lt_alv.

        CLEAR : lv_type,lv_msg,lv_matnr,amount,ls_new.
      ENDLOOP.
***************Display the ALV
      CALL FUNCTION 'Z_POPUP_ALV'
        TABLES
          it_alv = lt_alv.

    ELSE.

      MESSAGE : 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.
  ENDMETHOD.

  METHOD marc.
    DATA: lv_dispo TYPE t024d-dispo,
          lv_fevor TYPE t024f-fevor.

***********get the from plant details**************
    SELECT SINGLE * FROM marc INTO @DATA(wa_marc) WHERE matnr = @matnr
                                                  AND   werks = @from_plnt.
    IF sy-subrc = 0.
      CLEAR wa_plantdata.
      wa_plantdata-plant      = to_plnt. "wa_marc-werks.
      wa_plantdata-pur_status = wa_marc-mmsta.
      wa_plantdata-pvalidfrom = wa_marc-mmstd.
      wa_plantdata-abc_id     = wa_marc-maabc.
      wa_plantdata-crit_part  = wa_marc-kzkri.
      wa_plantdata-pur_group  = wa_marc-ekgrp.
      wa_plantdata-issue_unit = wa_marc-ausme.
      wa_plantdata-mrpprofile = wa_marc-dispr.
      wa_plantdata-mrp_type   = wa_marc-dismm.

*    IF wa_final-dispo IS NOT INITIAL.
*      wa_plantdata-mrp_ctrler = wa_final-dispo.
*    ELSE.
      wa_plantdata-mrp_ctrler = wa_marc-dispo.
*    ENDIF.
      wa_plantdata-plnd_delry = wa_marc-plifz.
      wa_plantdata-gr_pr_time = wa_marc-webaz.
      wa_plantdata-period_ind = wa_marc-perkz.
      wa_plantdata-assy_scrap = wa_marc-ausss.
      wa_plantdata-lotsizekey = wa_marc-disls.
*      IF wa_final-beskz IS NOT INITIAL.
*        wa_plantdata-proc_type = wa_final-beskz.
*      ELSE.
      wa_plantdata-proc_type = wa_marc-beskz.
*      ENDIF.
*      IF wa_final-sobsl IS NOT INITIAL.
*        wa_plantdata-spproctype = wa_final-sobsl.
*      ELSE.
      wa_plantdata-spproctype = wa_marc-sobsl.
*      ENDIF.
      wa_plantdata-reorder_pt = wa_marc-minbe.
      wa_plantdata-safety_stk = wa_marc-eisbe.
      wa_plantdata-minlotsize = wa_marc-bstmi.
      wa_plantdata-maxlotsize = wa_marc-bstma.
      wa_plantdata-fixed_lot = wa_marc-bstfe.
      wa_plantdata-round_val = wa_marc-bstrf.
      wa_plantdata-max_stock = wa_marc-mabst.
      wa_plantdata-ord_costs = wa_marc-losfx.
      wa_plantdata-dep_req_id = wa_marc-sbdkz.
      wa_plantdata-stor_costs = wa_marc-lagpr.
      wa_plantdata-alt_bom_id = wa_marc-altsl.
      wa_plantdata-discontinu = wa_marc-kzaus.
      wa_plantdata-eff_o_day = wa_marc-ausdt.
      wa_plantdata-follow_up = wa_marc-nfmat.
      wa_plantdata-grp_reqmts = wa_marc-kzbed.
      wa_plantdata-mixed_mrp = wa_marc-miskz.
      wa_plantdata-sm_key = wa_marc-fhori.
      wa_plantdata-backflush = wa_marc-rgekz.
*      IF wa_final-fevor IS NOT INITIAL.
*        wa_plantdata-production_scheduler = wa_final-fevor.
*      ELSE.
      wa_plantdata-production_scheduler = wa_marc-fevor.
*      ENDIF.
      wa_plantdata-proc_time = wa_marc-bearz.
      wa_plantdata-setuptime = wa_marc-ruezt.
      wa_plantdata-interop = wa_marc-tranz.
      wa_plantdata-base_qty = wa_marc-basmg.
      wa_plantdata-inhseprodt = wa_marc-dzeit.
      wa_plantdata-stgeperiod = wa_marc-maxlz.
      wa_plantdata-stge_pd_un = wa_marc-lzeih.
      wa_plantdata-over_tol = wa_marc-ueeto.
      wa_plantdata-unlimited = wa_marc-ueetk.
      wa_plantdata-under_tol = wa_marc-uneto.
      wa_plantdata-replentime = wa_marc-wzeit.
      wa_plantdata-replace_pt = wa_marc-atpkz.
      wa_plantdata-ind_post_to_insp_stock   = wa_marc-insmk.
      wa_plantdata-ctrl_key   = wa_marc-ssqss.
      wa_plantdata-doc_reqd   = wa_marc-kzdkz.
      wa_plantdata-insp_int   = wa_marc-prfrq.
      wa_plantdata-loadinggrp   = wa_marc-ladgr.
      wa_plantdata-batch_mgmt   = wa_marc-xchpf.
      wa_plantdata-quotausage   = wa_marc-usequ.
      wa_plantdata-serv_level   = wa_marc-lgrad.
      wa_plantdata-split_ind   = wa_marc-auftl.
      wa_plantdata-availcheck   = wa_marc-mtvfp.
      wa_plantdata-fy_variant   = wa_marc-periv.
      wa_plantdata-corr_fact   = wa_marc-kzkfk.
      wa_plantdata-setup_time   = wa_marc-vrvez.
      wa_plantdata-base_qty_plan   = wa_marc-vbamg.
      wa_plantdata-ship_proc_time   = wa_marc-vbeaz.
      wa_plantdata-sup_source   = wa_marc-bwscl.
      wa_plantdata-auto_p_ord   = wa_marc-kautb.
      wa_plantdata-sourcelist   = abap_false.
      wa_plantdata-comm_code   = wa_marc-stawn.
*  WA_PLANTDATA-COUNTRYORI   = WA_MARC-HERKL.
*  WA_PLANTDATA-REGIONORIG   = WA_MARC-HERKR.
      wa_plantdata-comm_co_un   = wa_marc-expme.
*  WA_PLANTDATA-EXPIMPGRP   = WA_MARC-MTVER.
*      IF wa_final-prctr IS NOT INITIAL.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = wa_final-prctr
*          IMPORTING
*            output = wa_final-prctr.
      wa_plantdata-profit_ctr   = wa_marc-prctr.
*      ELSE.
*        wa_plantdata-profit_ctr   = wa_marc-prctr.
*      ENDIF.
      wa_plantdata-ppc_pl_cal   = wa_marc-mrppp.
      wa_plantdata-rep_manuf   = wa_marc-sauft.
      wa_plantdata-pl_ti_fnce   = wa_marc-fxhor.
      wa_plantdata-consummode   = wa_marc-vrmod.
      wa_plantdata-bwd_cons   = wa_marc-vint1.
      wa_plantdata-fwd_cons   = wa_marc-vint2.
      wa_plantdata-alternative_bom  = wa_marc-stlal.
      wa_plantdata-bom_usage  = wa_marc-stlan.
      wa_plantdata-planlistgrp  = wa_marc-plnnr.
      wa_plantdata-planlistcnt  = wa_marc-aplal.
      wa_plantdata-lot_size  = wa_marc-losgr.
      wa_plantdata-specprocty  = wa_marc-sobsk.
      wa_plantdata-prod_unit  = wa_marc-frtme.
*      IF wa_final-lgpro IS NOT INITIAL.
*        wa_plantdata-iss_st_loc  = wa_final-lgpro.
*      ELSE.
      wa_plantdata-iss_st_loc  = wa_marc-lgpro.
*      ENDIF.
*  WA_PLANTDATA-ISS_ST_LOC  = ''.
      wa_plantdata-comp_scrap  = wa_marc-kausf.
      wa_plantdata-cert_type  = wa_marc-qzgtp.
      wa_plantdata-cycle_time  = wa_marc-takzt.
      wa_plantdata-covprofile  = wa_marc-rwpro.
      wa_plantdata-cc_ph_inv   = wa_marc-abcin.
      wa_plantdata-variance_key  = wa_marc-awsls.
      wa_plantdata-serno_prof  = wa_marc-sernp.
      wa_plantdata-repmanprof  = wa_marc-sfepr.
      wa_plantdata-neg_stocks  = wa_marc-xmcng.
      wa_plantdata-qm_rgmts    = wa_marc-qssys.
      wa_plantdata-plng_cycle  = wa_marc-lfrhy.
      wa_plantdata-round_prof  = wa_marc-rdprf.
      wa_plantdata-refmatcons  = wa_marc-vrbmt.
      wa_plantdata-d_to_ref_m  = wa_marc-vrbdt.
      wa_plantdata-mult_ref_m  = wa_marc-vrbfk.
      wa_plantdata-auto_reset  = wa_marc-autru.
      wa_plantdata-countryori  = wa_marc-prefe.
      wa_plantdata-ex_cert_id  = wa_marc-prenc.
      wa_plantdata-ex_cert_no_new  = wa_marc-preno.
      wa_plantdata-ex_cert_dt  = wa_marc-prend.
      wa_plantdata-milit_id  = wa_marc-itark.
      wa_plantdata-co_product  = wa_marc-kzkup.
      wa_plantdata-plan_strgp  = wa_marc-strgr.
      wa_plantdata-sloc_exprc  = wa_marc-lgfsb.
      wa_plantdata-bulk_mat  = wa_marc-schgt.
      wa_plantdata-cc_fixed  = wa_marc-ccfix.
      wa_plantdata-determ_grp  = wa_marc-eprio.
      wa_plantdata-qm_authgrp  = wa_marc-qmata.
      wa_plantdata-task_list_type   = wa_marc-plnty.
*      IF wa_final-sfcpf IS NOT INITIAL.
*        wa_plantdata-prodprof   = wa_final-sfcpf.
*      ELSE.
      wa_plantdata-prodprof   = wa_marc-sfcpf.
*      ENDIF.
      wa_plantdata-safty_t_id   = wa_marc-shflg.
      wa_plantdata-safetytime   = wa_marc-shzet.
      wa_plantdata-plord_ctrl   = wa_marc-mdach.
*      IF wa_final-kzech IS NOT INITIAL.
*        wa_plantdata-batchentry   = wa_final-kzech.
*      ELSE.
      wa_plantdata-batchentry   = wa_marc-kzech.
*      ENDIF.
      wa_plantdata-unit_group   = wa_marc-megru.
      wa_plantdata-matfrgtgrp   = wa_marc-mfrgr.
      wa_plantdata-prodverscs   = wa_marc-fvidk.
      wa_plantdata-fxd_price   = wa_marc-fxpru.
      wa_plantdata-handlg_grp   = wa_marc-loggr.
      wa_plantdata-distr_prof   = wa_marc-fprfm.
      wa_plantdata-eu_list_no   = wa_marc-mownr.
      wa_plantdata-eu_mat_grp   = wa_marc-mogru.
*  WA_PLANTDATA-CAS_NO   = WA_MARC-CASNR.
*  WA_PLANTDATA-PRODCOM_NO   = WA_MARC-GPNUM.
      wa_plantdata-ctrl_code   = wa_marc-steuc.
      wa_plantdata-jit_relvt   = wa_marc-fabkz.
      wa_plantdata-mat_grp_trans   = wa_marc-matgr.
      wa_plantdata-supply_area   = wa_marc-vspvb.
      wa_plantdata-fair_share_rule   = wa_marc-dplfs.
      wa_plantdata-push_distrib   = wa_marc-dplpu.
      wa_plantdata-deploy_horiz   = wa_marc-dplho.
      wa_plantdata-min_lot_size   = wa_marc-minls.
      wa_plantdata-max_lot_size   = wa_marc-maxls.
      wa_plantdata-fix_lot_size   = wa_marc-fixls.
      wa_plantdata-lot_increment  = wa_marc-ltinc.
      wa_plantdata-prod_conv_type  = wa_marc-convt.
      wa_plantdata-period_profile_safety_time   = wa_marc-shpro.
      wa_plantdata-mrp_relevancy_dep_requirements   = wa_marc-ahdis.
      wa_plantdata-avail_check_all_proj_segments    = wa_marc-kzpsp.
      wa_plantdata-overallprf    = wa_marc-ocmpf.
      wa_plantdata-min_safety_stk  = wa_marc-eislo.
      wa_plantdata-no_costing  = wa_marc-ncost.
      wa_plantdata-rotation_date  = wa_marc-rotation_date.
      wa_plantdata-original_batch_flag   = wa_marc-uchkz.
      wa_plantdata-original_batch_ref_material = wa_marc-ucmat.
      wa_plantdata-segmentation_strategy  = wa_marc-sgt_covs.
      wa_plantdata-segmentation_status  = wa_marc-sgt_statc.
      wa_plantdata-consumption_priority  = wa_marc-sgt_prcm.
      wa_plantdata-discrete_batch_flag  = wa_marc-sgt_chint.
      wa_plantdata-stock_protection_ind  = wa_marc-sgt_stk_prt.
      wa_plantdata-default_stock_segment = wa_marc-sgt_defsc.
      wa_plantdata-pvalidfrom = wa_marc-sgt_mmstd.
    ENDIF.

  ENDMETHOD.
  METHOD mard.
* get sloc data
    SELECT SINGLE * FROM mard INTO @DATA(wa_mard) WHERE matnr = @matnr
                                                  AND   werks = @from_plnt
                                                  AND   lgort = @from_sloc.
    IF sy-subrc = 0.
      CLEAR : wa_storagelocationdata.
      wa_storagelocationdata-plant        = to_plnt.
      wa_storagelocationdata-stge_loc     = to_lgort."WA_MARD-LGORT.
      wa_storagelocationdata-mrp_ind      = wa_mard-diskz.
      wa_storagelocationdata-spec_proc    = wa_mard-lsobs.
      wa_storagelocationdata-reorder_pt   = wa_mard-lminb.
      wa_storagelocationdata-repl_qty     = wa_mard-lbstf.
      wa_storagelocationdata-stge_bin     = wa_mard-lgpbe.
      wa_storagelocationdata-pickg_area   = wa_mard-lwmkb.
      wa_storagelocationdata-inv_corr_fac = wa_mard-bskrf.
    ENDIF.
  ENDMETHOD.
  METHOD mbew.
* Get Valuation Data
    SELECT SINGLE * FROM mbew INTO @DATA(wa_mbew) WHERE matnr = @matnr
                                                  AND   bwkey = @from_plnt.
    IF sy-subrc = 0.
      DATA: lv_kosgr TYPE tck14-kosgr.
      CLEAR : wa_valuationdata.
      wa_valuationdata-val_area = to_plnt.
      wa_valuationdata-overhead_grp  = wa_mbew-kosgr.
      wa_valuationdata-val_type = wa_mbew-bwtar.
*    IF wa_final-vprsv IS NOT INITIAL.
      wa_valuationdata-price_ctrl = lv_vprsv.
*    ELSE.
*      wa_valuationdata-price_ctrl = wa_mbew-vprsv.
*    ENDIF.
      wa_valuationdata-moving_pr = wa_mbew-verpr. "wa_final-verpr.
      wa_valuationdata-std_price = wa_mbew-stprs.
      wa_valuationdata-price_unit = wa_mbew-peinh.
      wa_valuationdata-val_class = wa_mbew-bklas.
      wa_valuationdata-pr_ctrl_pp = wa_mbew-vmvpr.
      wa_valuationdata-mov_pr_pp = wa_mbew-vmver.
      wa_valuationdata-std_pr_pp = wa_mbew-vmstp.
      wa_valuationdata-pr_unit_pp = wa_mbew-vmpei.
      wa_valuationdata-vclass_pp = wa_mbew-vmbkl.
      wa_valuationdata-pr_ctrl_py = wa_mbew-vjvpr.
      wa_valuationdata-mov_pr_py = wa_mbew-vjver.
      wa_valuationdata-std_pr_py = wa_mbew-vjstp.
      wa_valuationdata-pr_unit_py = wa_mbew-vjpei.
      wa_valuationdata-vclass_py = wa_mbew-vjbkl.
      wa_valuationdata-val_cat = wa_mbew-bwtty.
      wa_valuationdata-valid_from = wa_mbew-zkdat.
      wa_valuationdata-taxprice_1 = wa_mbew-bwprs.
      wa_valuationdata-commprice1 = wa_mbew-bwprh.
      wa_valuationdata-taxprice_3 = wa_mbew-vjbws.
      wa_valuationdata-commprice3 = wa_mbew-vjbwh.
      wa_valuationdata-plnd_price = wa_mbew-zplpr.
      wa_valuationdata-plndprice1 = wa_mbew-zplp1.
      wa_valuationdata-plndprice2 = wa_mbew-zplp2.
      wa_valuationdata-plndprice3 = wa_mbew-zplp3.
      wa_valuationdata-plndprdate1 = wa_mbew-zpld1.
      wa_valuationdata-plndprdate2 = wa_mbew-zpld2.
      wa_valuationdata-plndprdate3 = wa_mbew-zpld3.
      wa_valuationdata-lifo_fifo = wa_mbew-xlifo.
      wa_valuationdata-poolnumber = wa_mbew-mypol.
      wa_valuationdata-commprice2 = wa_mbew-bwph1.
      wa_valuationdata-taxprice_2 = wa_mbew-bwps1.
      wa_valuationdata-deval_ind = wa_mbew-abwkz.
*  wa_valuationdata-orig_group = wa_mbew-hrkft.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_mbew-kosgr
        IMPORTING
          output = wa_mbew-kosgr.
      CLEAR lv_kosgr.
      SELECT SINGLE kosgr
               FROM tck14
               INTO lv_kosgr
              WHERE bwkey = to_plnt.
      wa_valuationdata-qty_struct = wa_mbew-ekalr.
      wa_valuationdata-ml_active = wa_mbew-mlmaa.
      wa_valuationdata-ml_settle = wa_mbew-mlast.
      wa_valuationdata-orig_mat = wa_mbew-hkmat.
      wa_valuationdata-orig_mat = wa_mbew-hkmat.
    ENDIF.
  ENDMETHOD.
  METHOD mvkex.

    IF wa_salesdata IS NOT INITIAL.
      CLEAR : wa_salesdatax.
      wa_salesdatax-sales_org  = to_vkorg.
      wa_salesdatax-distr_chan = to_vtweg.
      wa_salesdatax-matl_stats = 'X'.
      wa_salesdatax-rebate_grp = 'X'.
      wa_salesdatax-comm_group = 'X'.
      wa_salesdatax-cash_disc = 'X'.
      wa_salesdatax-sal_status = 'X'.
      wa_salesdatax-valid_from = 'X'.
      wa_salesdatax-min_order = 'X'.
      wa_salesdatax-min_dely = 'X'.
      wa_salesdatax-min_mto = 'X'.
      wa_salesdatax-dely_unit = 'X'.
      wa_salesdatax-dely_uom = 'X'.
      wa_salesdatax-sales_unit = 'X'.
      wa_salesdatax-item_cat = 'X'.
      wa_salesdatax-delyg_plnt = 'X'.
      wa_salesdatax-prod_hier = 'X'.
      wa_salesdatax-pr_ref_mat = 'X'.
      wa_salesdatax-mat_pr_grp = 'X'.
      wa_salesdatax-acct_assgt = 'X'.
      wa_salesdatax-matl_grp_1 = 'X'.
      wa_salesdatax-matl_grp_2 = 'X'.
      wa_salesdatax-matl_grp_3 = 'X'.
      wa_salesdatax-matl_grp_4 = 'X'.
      wa_salesdatax-matl_grp_5 = 'X'.
      wa_salesdatax-prod_att_1 = 'X'.
      wa_salesdatax-prod_att_2 = 'X'.
      wa_salesdatax-prod_att_3 = 'X'.
      wa_salesdatax-prod_att_4 = 'X'.
      wa_salesdatax-prod_att_5 = 'X'.
      wa_salesdatax-prod_att_6 = 'X'.
      wa_salesdatax-prod_att_7 = 'X'.
      wa_salesdatax-prod_att_8 = 'X'.
      wa_salesdatax-prod_att_9 = 'X'.
      wa_salesdatax-prod_att10 = 'X'.
      wa_salesdatax-round_prof = 'X'.
      wa_salesdatax-var_sales_un = 'X'.
      wa_salesdatax-unit_group = 'X'.
    ENDIF.

  ENDMETHOD.
  METHOD mvke.
* Sales Data
    SELECT SINGLE * FROM mvke INTO @DATA(wa_mvke) WHERE matnr = @matnr
                                                  AND   vkorg = @from_vkorg
                                                  AND   vtweg = @from_vtweg.
    IF sy-subrc = 0.
      CLEAR : wa_salesdata.
      wa_salesdata-sales_org  = to_vkorg.
      wa_salesdata-distr_chan = to_vtweg.
      wa_salesdata-matl_stats = wa_mvke-versg.
      wa_salesdata-rebate_grp = wa_mvke-bonus.
      wa_salesdata-comm_group = wa_mvke-provg.
      wa_salesdata-cash_disc = wa_mvke-sktof.
      wa_salesdata-sal_status = wa_mvke-vmsta.
      wa_salesdata-valid_from = wa_mvke-vmstd.
      wa_salesdata-min_order = wa_mvke-aumng.
      wa_salesdata-min_dely = wa_mvke-lfmng.
      wa_salesdata-min_mto = wa_mvke-efmng.
      wa_salesdata-dely_unit = wa_mvke-scmng.
      wa_salesdata-dely_uom = wa_mvke-schme.
      wa_salesdata-sales_unit = wa_mvke-vrkme.
      wa_salesdata-item_cat = wa_mvke-mtpos.
      wa_salesdata-delyg_plnt = wa_mvke-dwerk. "WA_FINAL-TPLANT."WA_MVKE-DWERK.
      wa_salesdata-prod_hier = wa_mvke-prodh.
      wa_salesdata-pr_ref_mat = wa_mvke-pmatn.
      wa_salesdata-mat_pr_grp = wa_mvke-kondm.
      wa_salesdata-acct_assgt = wa_mvke-ktgrm.
      wa_salesdata-matl_grp_1 = wa_mvke-mvgr1.
      wa_salesdata-matl_grp_2 = wa_mvke-mvgr2.
      wa_salesdata-matl_grp_3 = wa_mvke-mvgr3.
      wa_salesdata-matl_grp_4 = wa_mvke-mvgr4.
      wa_salesdata-matl_grp_5 = wa_mvke-mvgr5.
      wa_salesdata-prod_att_1 = wa_mvke-prat1.
      wa_salesdata-prod_att_2 = wa_mvke-prat2.
      wa_salesdata-prod_att_3 = wa_mvke-prat3.
      wa_salesdata-prod_att_4 = wa_mvke-prat4.
      wa_salesdata-prod_att_5 = wa_mvke-prat5.
      wa_salesdata-prod_att_6 = wa_mvke-prat6.
      wa_salesdata-prod_att_7 = wa_mvke-prat7.
      wa_salesdata-prod_att_8 = wa_mvke-prat8.
      wa_salesdata-prod_att_9 = wa_mvke-prat9.
      wa_salesdata-prod_att10 = wa_mvke-prata.
      wa_salesdata-round_prof = wa_mvke-rdprf.
      wa_salesdata-var_sales_un = wa_mvke-vavme.
      wa_salesdata-unit_group = wa_mvke-megru.
    ENDIF.
  ENDMETHOD.
  METHOD marcx.
    IF wa_plantdata IS NOT INITIAL.
      CLEAR : wa_plantdatax.
      wa_plantdatax-plant      = to_plnt.
      wa_plantdatax-pur_status = 'X'.
      wa_plantdatax-pvalidfrom = 'X'.
      wa_plantdatax-abc_id     = 'X'.
      wa_plantdatax-crit_part = 'X'.
      wa_plantdatax-pur_group = 'X'.
      wa_plantdatax-issue_unit = 'X'.
*  wa_plantdatax-mrpprofile = 'X'.
      wa_plantdatax-mrp_type = 'X'.
*  IF wa_final-dispo IS NOT INITIAL.
      wa_plantdatax-mrp_ctrler = 'X'.
*  ENDIF.
      wa_plantdatax-plnd_delry = 'X'.
      wa_plantdatax-gr_pr_time = 'X'.
      wa_plantdatax-period_ind = 'X'.
      wa_plantdatax-assy_scrap = 'X'.
      wa_plantdatax-lotsizekey = 'X'.
      wa_plantdatax-proc_type = 'X'.
      wa_plantdatax-spproctype = 'X'.
      wa_plantdatax-reorder_pt = 'X'.
      wa_plantdatax-safety_stk = 'X'.
      wa_plantdatax-minlotsize = 'X'.
      wa_plantdatax-maxlotsize = 'X'.
      wa_plantdatax-fixed_lot = 'X'.
      wa_plantdatax-round_val = 'X'.
      wa_plantdatax-max_stock = 'X'.
      wa_plantdatax-ord_costs = 'X'.
      wa_plantdatax-dep_req_id = 'X'.
      wa_plantdatax-stor_costs = 'X'.
      wa_plantdatax-alt_bom_id = 'X'.
      wa_plantdatax-discontinu = 'X'.
      wa_plantdatax-eff_o_day = 'X'.
      wa_plantdatax-follow_up = 'X'.
      wa_plantdatax-grp_reqmts = 'X'.
      wa_plantdatax-mixed_mrp = 'X'.
      wa_plantdatax-sm_key = 'X'.
      wa_plantdatax-backflush = 'X'.
      wa_plantdatax-production_scheduler = 'X'.
      wa_plantdatax-proc_time = 'X'.
      wa_plantdatax-setuptime = 'X'.
      wa_plantdatax-interop = 'X'.
      wa_plantdatax-base_qty = 'X'.
      wa_plantdatax-inhseprodt = 'X'.
      wa_plantdatax-stgeperiod = 'X'.
      wa_plantdatax-stge_pd_un = 'X'.
      wa_plantdatax-over_tol = 'X'.
      wa_plantdatax-unlimited = 'X'.
      wa_plantdatax-under_tol = 'X'.
      wa_plantdatax-replentime = 'X'.
      wa_plantdatax-replace_pt = 'X'.
      wa_plantdatax-ind_post_to_insp_stock = 'X'.
      wa_plantdatax-ctrl_key = 'X'.
      wa_plantdatax-doc_reqd = 'X'.
      wa_plantdatax-insp_int = 'X'.
      wa_plantdatax-loadinggrp = 'X'.
      wa_plantdatax-batch_mgmt = 'X'.
      wa_plantdatax-quotausage = 'X'.
      wa_plantdatax-serv_level = 'X'.
      wa_plantdatax-split_ind = 'X'.
      wa_plantdatax-availcheck = 'X'.
      wa_plantdatax-fy_variant = 'X'.
      wa_plantdatax-corr_fact = 'X'.
      wa_plantdatax-setup_time = 'X'.
      wa_plantdatax-base_qty_plan = 'X'.
      wa_plantdatax-ship_proc_time = 'X'.
      wa_plantdatax-sup_source = 'X'.
      wa_plantdatax-auto_p_ord = 'X'.
      wa_plantdatax-sourcelist = 'X'.
      wa_plantdatax-comm_code = 'X'.
      wa_plantdatax-countryori = 'X'.
      wa_plantdatax-regionorig = 'X'.
      wa_plantdatax-comm_co_un = 'X'.
      wa_plantdatax-expimpgrp = 'X'.
      wa_plantdatax-profit_ctr = 'X'.
      wa_plantdatax-ppc_pl_cal = 'X'.
      wa_plantdatax-rep_manuf = 'X'.
      wa_plantdatax-pl_ti_fnce = 'X'.
      wa_plantdatax-consummode = 'X'.
      wa_plantdatax-bwd_cons = 'X'.
      wa_plantdatax-fwd_cons = 'X'.
      wa_plantdatax-alternative_bom = 'X'.
      wa_plantdatax-bom_usage = 'X'.
      wa_plantdatax-planlistgrp = 'X'.
      wa_plantdatax-planlistcnt = 'X'.
      wa_plantdatax-lot_size = 'X'.
      wa_plantdatax-specprocty = 'X'.
      wa_plantdatax-prod_unit = 'X'.
      wa_plantdatax-iss_st_loc = 'X'.
      wa_plantdatax-mrp_group = 'X'.
*  wa_plantdatax-comp_scrap = 'X'.
      wa_plantdatax-cert_type = 'X'.
      wa_plantdatax-cycle_time = 'X'.
      wa_plantdatax-covprofile = 'X'.
      wa_plantdatax-cc_ph_inv = 'X'.
      wa_plantdatax-variance_key = 'X'.
      wa_plantdatax-serno_prof = 'X'.
      wa_plantdatax-repmanprof = 'X'.
      wa_plantdatax-neg_stocks = 'X'.
      wa_plantdatax-qm_rgmts = 'X'.
      wa_plantdatax-plng_cycle = 'X'.
      wa_plantdatax-round_prof = 'X'.
      wa_plantdatax-refmatcons = 'X'.
      wa_plantdatax-d_to_ref_m = 'X'.
      wa_plantdatax-mult_ref_m = 'X'.
      wa_plantdatax-auto_reset = 'X'.
      wa_plantdatax-countryori = 'X'.
      wa_plantdatax-ex_cert_id = 'X'.
      wa_plantdatax-ex_cert_no_new = 'X'.
      wa_plantdatax-ex_cert_dt = 'X'.
      wa_plantdatax-milit_id = 'X'.
      wa_plantdatax-co_product = 'X'.
      wa_plantdatax-plan_strgp = 'X'.
      wa_plantdatax-sloc_exprc = 'X'.
      wa_plantdatax-bulk_mat = 'X'.
      wa_plantdatax-cc_fixed = 'X'.
      wa_plantdatax-determ_grp = 'X'.
      wa_plantdatax-qm_authgrp = 'X'.
      wa_plantdatax-task_list_type = 'X'.
      wa_plantdatax-prodprof = 'X'.
      wa_plantdatax-safty_t_id = 'X'.
      wa_plantdatax-safetytime = 'X'.
      wa_plantdatax-plord_ctrl = 'X'.
      wa_plantdatax-batchentry = 'X'.
      wa_plantdatax-unit_group = 'X'.
      wa_plantdatax-matfrgtgrp = 'X'.
      wa_plantdatax-mrpprofile = 'X'.
      wa_plantdatax-prodverscs = 'X'.
      wa_plantdatax-fxd_price = 'X'.
      wa_plantdatax-handlg_grp = 'X'.
      wa_plantdatax-distr_prof = 'X'.
      wa_plantdatax-eu_list_no = 'X'.
      wa_plantdatax-eu_mat_grp = 'X'.
      wa_plantdatax-cas_no = 'X'.
      wa_plantdatax-prodcom_no = 'X'.
      wa_plantdatax-ctrl_code = 'X'.
      wa_plantdatax-jit_relvt = 'X'.
      wa_plantdatax-mat_grp_trans = 'X'.
      wa_plantdatax-supply_area = 'X'.
      wa_plantdatax-fair_share_rule = 'X'.
      wa_plantdatax-push_distrib = 'X'.
      wa_plantdatax-deploy_horiz = 'X'.
      wa_plantdatax-min_lot_size = 'X'.
      wa_plantdatax-max_lot_size = 'X'.
      wa_plantdatax-fix_lot_size = 'X'.
      wa_plantdatax-lot_increment = 'X'.
      wa_plantdatax-prod_conv_type = 'X'.
      wa_plantdatax-period_profile_safety_time = 'X'.
      wa_plantdatax-mrp_relevancy_dep_requirements = 'X'.
      wa_plantdatax-avail_check_all_proj_segments = 'X'.
      wa_plantdatax-overallprf = 'X'.
      wa_plantdatax-min_safety_stk = 'X'.
      wa_plantdatax-no_costing = 'X'.
      wa_plantdatax-rotation_date = 'X'.
      wa_plantdatax-original_batch_flag = 'X'.
      wa_plantdatax-original_batch_ref_material = 'X'.
      wa_plantdatax-segmentation_strategy = 'X'.
      wa_plantdatax-segmentation_status = 'X'.
      wa_plantdatax-consumption_priority = 'X'.
      wa_plantdatax-discrete_batch_flag = 'X'.
      wa_plantdatax-stock_protection_ind = 'X'.
      wa_plantdatax-default_stock_segment = 'X'.
      wa_plantdatax-pvalidfrom = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD mbewx.
    IF wa_valuationdata IS NOT INITIAL.
      CLEAR : wa_valuationdatax.
      wa_valuationdatax-val_area = to_plnt.
      wa_valuationdatax-val_type = wa_valuationdata-val_type.
      wa_valuationdatax-price_ctrl = 'X'.
      wa_valuationdatax-moving_pr = 'X'.
      wa_valuationdatax-std_price = 'X'.
      wa_valuationdatax-price_unit = 'X'.
      wa_valuationdatax-val_class = 'X'.
      wa_valuationdatax-pr_ctrl_pp = 'X'.
      wa_valuationdatax-mov_pr_pp = 'X'.
      wa_valuationdatax-std_pr_pp = 'X'.
      wa_valuationdatax-pr_unit_pp = 'X'.
      wa_valuationdatax-vclass_pp = 'X'.
      wa_valuationdatax-pr_ctrl_py = 'X'.
      wa_valuationdatax-mov_pr_py = 'X'.
      wa_valuationdatax-std_pr_py = 'X'.
      wa_valuationdatax-pr_unit_py = 'X'.
      wa_valuationdatax-vclass_py = 'X'.
      wa_valuationdatax-val_cat = 'X'.
      wa_valuationdatax-valid_from = 'X'.
      wa_valuationdatax-taxprice_1 = 'X'.
      wa_valuationdatax-commprice1 = 'X'.
      wa_valuationdatax-taxprice_3 = 'X'.
      wa_valuationdatax-commprice3 = 'X'.
      wa_valuationdatax-plnd_price = 'X'.
      wa_valuationdatax-plndprice1 = 'X'.
      wa_valuationdatax-plndprice2 = 'X'.
      wa_valuationdatax-plndprice3 = 'X'.
      wa_valuationdatax-plndprdate1 = 'X'.
      wa_valuationdatax-plndprdate2 = 'X'.
      wa_valuationdatax-plndprdate3 = 'X'.
      wa_valuationdatax-lifo_fifo = 'X'.
      wa_valuationdatax-poolnumber = 'X'.
      wa_valuationdatax-commprice2 = 'X'.
      wa_valuationdatax-taxprice_2 = 'X'.
      wa_valuationdatax-deval_ind = 'X'.
      wa_valuationdatax-orig_group = 'X'.
      wa_valuationdatax-overhead_grp = 'X'.
      wa_valuationdatax-qty_struct = 'X'.
      wa_valuationdatax-ml_active = 'X'.
      wa_valuationdatax-ml_settle = 'X'.
      wa_valuationdatax-orig_mat = 'X'.
      wa_valuationdatax-orig_mat = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD header.
    CLEAR : wa_headerdata.
    wa_headerdata-basic_view      = abap_true.
    wa_headerdata-purchase_view   = abap_true.
*  WA_HEADERDATA-MRP_VIEW        = WA_FINAL-MRP_VIEW.
*  WA_HEADERDATA-WORK_SCHED_VIEW = WA_FINAL-WORK_SCHED_VIEW.
*  WA_HEADERDATA-COST_VIEW       = WA_FINAL-COST_VIEW.
    wa_headerdata-account_view    = abap_true.
    wa_headerdata-sales_view      = abap_true.
    wa_headerdata-storage_view    = abap_true.
*  WA_HEADERDATA-QUALITY_VIEW    = WA_FINAL-QUALITY_VIEW.
    wa_headerdata-material = matnr.
  ENDMETHOD.
  METHOD mardx.
    IF wa_storagelocationdata IS NOT INITIAL.
      CLEAR : wa_storagelocationdatax.
      wa_storagelocationdatax-plant = to_plnt.
      wa_storagelocationdatax-stge_loc = to_lgort."WA_MARD-LGORT.
      wa_storagelocationdatax-mrp_ind = 'X'.
      wa_storagelocationdatax-spec_proc = 'X'.
      wa_storagelocationdatax-reorder_pt = 'X'.
      wa_storagelocationdatax-repl_qty = 'X'.
      wa_storagelocationdatax-stge_bin = 'X'.
      wa_storagelocationdatax-pickg_area = 'X'.
      wa_storagelocationdatax-inv_corr_fac = 'X'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.



START-OF-SELECTION.

  DATA : lobj_stktrnsfr TYPE REF TO lcl_stktrnsfr.
  CREATE OBJECT lobj_stktrnsfr.

  lobj_stktrnsfr->frm_lgort = frm_strg.
  lobj_stktrnsfr->to_lgort  = to_strg.
  lobj_stktrnsfr->frm_vkorg = frm_sorg.
  lobj_stktrnsfr->to_vkorg  = to_sorg.
  lobj_stktrnsfr->frm_vtweg = frm_divi.
  lobj_stktrnsfr->to_vtweg  = to_divi.

*****************set the pricing control***************
  IF to_sorg = 'SDMS'.
    lobj_stktrnsfr->lv_vprsv = 'S'.
  ELSE.
    lobj_stktrnsfr->lv_vprsv = 'V'.
  ENDIF.

  IF r1 = abap_true.             "material extension part

    SELECT SINGLE werks FROM t001l INTO @DATA(ls_frmard) WHERE werks = @frm_plnt
                                                        AND   lgort = @frm_strg.
    IF sy-subrc = 0.

      SELECT SINGLE werks FROM t001l INTO @DATA(ls_tomard) WHERE werks = @to_plnt
                                                          AND   lgort = @to_strg.
      IF sy-subrc = 0.
        lobj_stktrnsfr->mat_ext( ).
      ELSE.
        MESSAGE : 'Invalid To Plant Details' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ELSE.
      MESSAGE : 'Invalid From Plant Details' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ELSEIF r2 = abap_true.         "Price update part
******************validate the from plant***********
    SELECT SINGLE kunnr FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @frm_dist
                                                      AND   werks = @frm_plnt.
*    IF sy-subrc = 0.
******************validate the To plant***********
*      SELECT SINGLE kunnr FROM kna1 INTO @ls_kna1 WHERE kunnr = @to_dist
*                                                  AND   werks = @to_plnt.
*      IF sy-subrc = 0.
    SELECT SINGLE bwkey, bukrs FROM t001k INTO @DATA(ls_t001k) WHERE bwkey = @to_plnt.
    IF sy-subrc = 0.
      lobj_stktrnsfr->price_update( ).
    ELSE.
      MESSAGE : 'Invalid To Plant Details' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
*      ELSE.
*        MESSAGE : 'To Plant & Distributor Mismatch' TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.
*    ELSE.
*      MESSAGE : 'From Plant & Distributor Mismatch' TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.

  ELSEIF r3 = abap_true.          "Transfer the stock part

*****************validate the from plant***********
    SELECT SINGLE kunnr FROM kna1 INTO @ls_kna1 WHERE kunnr = @frm_dist
                                                AND   werks = @frm_plnt.
    IF sy-subrc = 0.
*****************validate the To plant***********
      SELECT SINGLE kunnr FROM kna1 INTO @ls_kna1 WHERE kunnr = @to_dist
                                                  AND   werks = @to_plnt.
      IF sy-subrc = 0.
        SELECT SINGLE bwkey, bukrs FROM t001k INTO @ls_t001k WHERE bwkey = @to_plnt.
        IF sy-subrc = 0.
          lobj_stktrnsfr->stk_transfer( ).
        ELSE.
          MESSAGE : 'Invalid To Plant Details' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        MESSAGE : 'To Plant & Distributor Mismatch' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE : 'From Plant & Distributor Mismatch' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

*****************zero stock the from plant*********************

  ELSEIF r4 = abap_true.

*****************validate the from plant & Distributor***********
    SELECT SINGLE kunnr FROM kna1 INTO @ls_kna1 WHERE kunnr = @frm_dist
                                                AND   werks = @frm_plnt.
    IF sy-subrc = 0.
      lobj_stktrnsfr->zero_stock( ).
    ELSE.
      MESSAGE : 'Distributor & Plant Mismatch' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF r4 = 'X' AND ( screen-name CS 'TO_PLNT' OR screen-name CS 'TO_DIST' OR screen-name CS 'TO_STRG' OR screen-name CS 'TO_SORG' OR screen-name CS 'TO_DIVI' ).
      screen-active = 0.
    ENDIF.

    IF r2 = 'X' AND ( screen-name CS 'FRM_STRG' OR screen-name CS 'TO_STRG' ).
      screen-active = 0.
    ENDIF.

    IF r1 = 'X' AND ( screen-name CS 'FRM_DIST' OR screen-name CS 'TO_DIST' ).
      screen-active = 0.
    ENDIF.

    IF r1 = abap_false AND ( screen-name CS 'FRM_SORG' OR screen-name CS 'FRM_DIVI' OR screen-name CS 'TO_SORG' OR screen-name CS 'TO_DIVI' ).
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
