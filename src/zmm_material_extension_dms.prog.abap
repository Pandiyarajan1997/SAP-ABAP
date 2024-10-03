*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 23.12.2023
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                : DEVK934848
*
*  Business Logic            : Report for material extension based on distributor invoice
*
*  Released on Date          :
*
* Hardcoded                  : price_ctrl  - 'S'
*                              strg loc    - D1
*                              sales org   - SDMS
*                              dist_chnl   - 20
*TVRC - name = 'MAT_EXT_PROFIT_CENTER'
*       type = 'S'.
*=======================================================================
REPORT zmm_material_extension_dms.

DATA : gv_kunnr TYPE vbrk-kunag,
       gv_vbeln TYPE vbrk-vbeln.

SELECT-OPTIONS : so_kunnr FOR gv_kunnr,
                 so_date  FOR sy-datum,
                 so_vbeln FOR gv_vbeln.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

  PARAMETERS : p_chk AS CHECKBOX USER-COMMAND u1,
               plant TYPE werks.

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& Include          ZMM_MAT_MASTER_C01
*&---------------------------------------------------------------------*
CLASS lcl_material DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_final,        "for alv changes
              to_plnt   TYPE werks_d,
              matnr     TYPE matnr,
              vbeln     TYPE vbeln,
              kunag     TYPE kunag,
              vkorg     TYPE vkorg,
              vtweg     TYPE vtweg,
              posnr     TYPE posnr,
              from_plnt TYPE werks_d,
              vgbel     TYPE vbeln_vl,
              lgort     TYPE lgort_d,
              charg     TYPE charg_d,
              type      TYPE bapi_mtype,
              msg       TYPE string,
            END OF ty_final.
    TYPES : BEGIN OF ty_kna1,        "for alv changes
              kunnr TYPE kunnr,
              werks TYPE werks,
            END OF ty_kna1.

    DATA : gt_final  TYPE TABLE OF ty_final,     "final it
           lt_return TYPE TABLE OF bapiret2,
           gt_kna1   TYPE TABLE OF ty_kna1.

    DATA: wa_headerdata           TYPE bapimathead,
          wa_plantdata            TYPE bapi_marc,
          wa_plantdatax           TYPE bapi_marcx,
          wa_storagelocationdata  TYPE bapi_mard,
          wa_storagelocationdatax TYPE bapi_mardx,
          wa_valuationdata        TYPE bapi_mbew,
          wa_valuationdatax       TYPE bapi_mbewx,
          wa_salesdata            TYPE bapi_mvke,
          wa_salesdatax           TYPE bapi_mvkex.

    DATA : gv_alv    TYPE REF TO cl_gui_alv_grid.
*methods dec
    METHODS :
      fetch,                                       "fetching
      alv,                                         "mat master alv
      bapi,                                        "call bapi
      marc IMPORTING matnr     TYPE matnr          "plant data
                     from_plnt TYPE werks_d
                     to_plnt   TYPE werks_d,
      marcx IMPORTING to_plnt   TYPE werks_d,
      mard IMPORTING matnr     TYPE matnr          "storage loc data
                     from_plnt TYPE werks_d
                     to_plnt   TYPE werks_d
                     from_sloc TYPE lgort_d,
      mardx IMPORTING to_plnt   TYPE werks_d,
      mbew IMPORTING matnr     TYPE matnr          "valuation data
                     from_plnt TYPE werks_d
                     to_plnt   TYPE werks_d,
      mbewx IMPORTING to_plnt   TYPE werks_d,
      mvke IMPORTING matnr      TYPE matnr         "sales data
                     from_vkorg TYPE vkorg
                     from_vtweg TYPE vtweg,
      mvkex,
      header IMPORTING matnr  TYPE matnr.

ENDCLASS.


CLASS lcl_material IMPLEMENTATION.
  METHOD fetch.
***************fetch only DMS distributor***************
    SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
    IF sy-subrc = 0.
      SELECT c~werks AS to_plnt,
             b~matnr,
             a~vbeln,
             a~kunag,
             a~vkorg,
             a~vtweg,
             b~posnr,
             b~werks AS from_plnt,
             b~vgbel FROM vbrk AS a
                     INNER JOIN vbrp AS b ON a~vbeln EQ b~vbeln
                     INNER JOIN kna1 AS c ON a~kunag EQ c~kunnr
                     INTO TABLE @gt_final
                     FOR ALL ENTRIES IN @lt_t001k
                     WHERE a~vbeln IN @so_vbeln
                     AND   fkart   EQ 'YBBR'
                     AND   kunag   IN @so_kunnr
                     AND   fkart   NE 'S1'
                     AND   fkdat   IN @so_date
                     AND   fksto   NE @abap_true
                     AND   c~werks EQ @lt_t001k-bwkey.
    ENDIF.
    IF sy-subrc = 0.
****************test run for particular plant****************
      IF p_chk = abap_true AND plant IS NOT INITIAL.
        DATA : ls_final LIKE LINE OF gt_final.
        ls_final-to_plnt = plant.
        MODIFY gt_final FROM ls_final TRANSPORTING to_plnt WHERE to_plnt NE ' '.
      ENDIF.
******************call the bapi process************
      bapi( ).
      alv( ).
      CALL SCREEN 9000.
    ELSE.
      MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD alv.

    DATA lt_fcat TYPE lvc_t_fcat.   "data dec for fieldcat

    DATA(ls_layo) = VALUE lvc_s_layo( cwidth_opt = abap_true
                                      zebra = abap_true ).   "data dec for layout design
    IF gv_alv IS INITIAL.
**create the object for alv
      CREATE OBJECT gv_alv
        EXPORTING
          i_parent = cl_gui_container=>default_screen.

    ENDIF.

    lt_fcat = VALUE #(
    ( col_pos = 1  fieldname = 'VBELN'      ref_field = 'VBELN'      ref_table = 'VBRP')
    ( col_pos = 2  fieldname = 'MATNR'      ref_field = 'MATNR'      ref_table = 'VBRP')
    ( col_pos = 3  fieldname = 'KUNAG'      ref_field = 'KUNNR'      ref_table = 'KNA1')
    ( col_pos = 4  fieldname = 'VKORG'      ref_field = 'VKORG_AUFT' ref_table = 'VBRP')
    ( col_pos = 5  fieldname = 'VTWEG'      ref_field = 'VTWEG_AUFT' ref_table = 'VBRP')
    ( col_pos = 6  fieldname = 'FROM_PLNT'  scrtext_m = 'From Plant'                   )
    ( col_pos = 7  fieldname = 'LGORT'      ref_field = 'LGORT'      ref_table = 'VBRP')
    ( col_pos = 8  fieldname = 'CHARG'      ref_field = 'CHARG'      ref_table = 'VBRP')
    ( col_pos = 9  fieldname = 'TO_PLNT'    scrtext_m = 'To Plant'                     )
    ( col_pos = 10 fieldname = 'TYPE'       scrtext_m = 'Type'                         )
    ( col_pos = 11 fieldname = 'MSG'        scrtext_m = 'Message'                      ) ).
*display the alv
    gv_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layo    "Layout
      CHANGING
        it_outtab                     = gt_final    "Output Table
        it_fieldcatalog               = lt_fcat     "Field Catalog
    ).
    REFRESH lt_fcat.        "refresh fieldcat

  ENDMETHOD.
  METHOD bapi.

    DATA : wa_return TYPE bapiret2,
           lv_matnr  TYPE matnr18,
           lv_type   TYPE bapi_mtype,
           lv_msg    TYPE string,
           lv_lgort  TYPE lgort_d.
*************batch creation class***********
    DATA : lo_batch TYPE REF TO zcl_api_dms_grn_inv.
    CREATE OBJECT lo_batch.
*************customer block check***********
    DATA : lo_block TYPE REF TO zcl_common_check.
    CREATE OBJECT lo_block.
    DATA : lt_block TYPE TABLE OF zsd_st_cust_block.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.
    lv_lgort = 'D1'.        "dms sloc.
******************fetch the storage loc & batch details of invoice************
    SELECT vbeln,matnr,werks,lgort,charg FROM lips INTO TABLE @DATA(lt_lips)
                                                   FOR ALL ENTRIES IN @gt_final
                                                   WHERE vbeln =  @gt_final-vgbel
                                                   AND   charg NE @abap_false.
    IF sy-subrc = 0.
      SORT lt_lips BY vbeln matnr.
    ENDIF.
******************fetch the plant & material details************
    SELECT matnr,werks,lgort FROM mard INTO TABLE @DATA(lt_mard)
                                       FOR ALL ENTRIES IN @gt_final
                                       WHERE "matnr = @gt_final-matnr
                                              werks = @gt_final-to_plnt
                                       AND   lgort = @lv_lgort.
    IF sy-subrc = 0.
      SORT lt_mard BY matnr werks.
    ENDIF.
    SELECT matnr,werks,lgort,charg,ersda FROM mchb INTO TABLE @DATA(lt_mchb)
                                                   FOR ALL ENTRIES IN @gt_final
                                                   WHERE "matnr = @gt_final-matnr.
                                                         werks = @gt_final-to_plnt.
    IF sy-subrc = 0.
      SORT lt_mchb BY matnr werks lgort charg.
    ENDIF.
**************call customer check*********
    REFRESH : lt_block.
    lo_block->customer_block_check(
      EXPORTING
        bukrs                     = '1000'      " Company Code
        vkorg                     = '1000'      " Sales Organization
      CHANGING
        cust_tab                  = lt_block    " Table Type for Customer Block Check
        ).
    IF lt_block IS NOT INITIAL.
      SORT : lt_block BY kunnr block.
    ENDIF.
***********sorting the final table.**************
    SORT : gt_final BY to_plnt matnr from_plnt.
    DELETE ADJACENT DUPLICATES FROM gt_final COMPARING to_plnt matnr.
********************data fill & call bapi process( material save )************
    LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final>).

      CLEAR : lv_type,lv_msg.
      READ TABLE lt_lips INTO DATA(ls_lips) WITH KEY vbeln = <fs_final>-vgbel
                                                     matnr = <fs_final>-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_final>-lgort = ls_lips-lgort.
        <fs_final>-charg = ls_lips-charg.
      ELSE.
        lv_msg = | Cancelled invoice or delivery data not found |.
      ENDIF.
      IF <fs_final>-to_plnt IS INITIAL.
        lv_msg = | Distributor plant not maintained , { lv_msg } |.
      ENDIF.
*****************check distributor block or not*******************
      IF lt_block IS NOT INITIAL.
        READ TABLE lt_block TRANSPORTING NO FIELDS WITH KEY kunnr = <fs_final>-kunag
                                                            block = abap_true BINARY SEARCH.
        IF sy-subrc = 0.
          lv_msg = | Distributor is blocked , { lv_msg } |.
        ENDIF.
      ENDIF.

***********************Old material code conversion************
      IF <fs_final>-matnr(1) NE 'Z'.
        SELECT SINGLE matnr,bismt,mtart FROM mara INTO @DATA(ls_old)
                                        WHERE matnr = @<fs_final>-matnr.
        IF ls_old-mtart NE 'HAWA'.
          IF ls_old-bismt IS INITIAL.
            lv_msg = | New material - { <fs_final>-matnr } is not maintained in old material code, { lv_msg } |.
          ELSE.
            CONDENSE ls_old-bismt.
            <fs_final>-matnr = ls_old-bismt.
          ENDIF.
        ENDIF.
        CLEAR : ls_old.
      ENDIF.

      IF lv_msg IS NOT INITIAL.
        <fs_final>-msg = lv_msg.
        <fs_final>-type = 'E'.
*************error log store************
        lo_errorlog_dms->log_entry_store(
          EXPORTING
            type                = 22
            status              = 10
            plant               = <fs_final>-to_plnt
            material            = <fs_final>-matnr
            distributor         = <fs_final>-kunag
            msg                 = <fs_final>-msg ).
        CONTINUE.
      ENDIF.

      AT NEW matnr.

        READ TABLE lt_mard TRANSPORTING NO FIELDS WITH KEY matnr = <fs_final>-matnr
                                                           werks = <fs_final>-to_plnt BINARY SEARCH.
        IF sy-subrc NE 0.
*****************fill the header data****************
          CALL METHOD header( matnr = <fs_final>-matnr ).
*****************fill the plant data****************
          CALL METHOD marc(
            EXPORTING
              matnr     = <fs_final>-matnr
              from_plnt = <fs_final>-from_plnt
              to_plnt   = <fs_final>-to_plnt ).
*****************fill the plant data flag****************
          CALL METHOD marcx( to_plnt = <fs_final>-to_plnt ).
*****************fill the storage location data****************
          CALL METHOD mard(
            EXPORTING
              matnr     = <fs_final>-matnr
              from_plnt = <fs_final>-from_plnt
              to_plnt   = <fs_final>-to_plnt
              from_sloc = <fs_final>-lgort ).
*****************fill the storage location data flag****************
          CALL METHOD mardx( to_plnt = <fs_final>-to_plnt ).
*****************fill the sales data****************
          CALL METHOD mvke(
            EXPORTING
              matnr      = <fs_final>-matnr
              from_vkorg = <fs_final>-vkorg
              from_vtweg = <fs_final>-vtweg ).
*****************fill the sales data flag****************
          CALL METHOD mvkex( ).
*****************fill the valuation data****************
          CALL METHOD mbew(
            EXPORTING
              matnr     = <fs_final>-matnr
              from_plnt = <fs_final>-from_plnt
              to_plnt   = <fs_final>-to_plnt ).
*****************fill the valuation data flag****************
          CALL METHOD mbewx( to_plnt = <fs_final>-to_plnt ).
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
            <fs_final>-msg  = wa_return-message.
            <fs_final>-type = wa_return-type.
          ELSE.
            <fs_final>-msg  = wa_return-message.
            <fs_final>-type = wa_return-type.
**************store error logs to the dms table*************
            lv_msg = wa_return-message.
            lo_errorlog_dms->log_entry_store(
              EXPORTING
                type                = 22
                status              = 10
                plant               = <fs_final>-to_plnt
                material            = <fs_final>-matnr
                distributor         = <fs_final>-kunag
                msg                 = lv_msg ).
            CLEAR : lv_msg.
          ENDIF.
        ELSE.
          wa_return-message = |Already Material extended|.
          wa_return-type    = 'S'.
        ENDIF.
      ENDAT.

      IF wa_return-type = 'S'.
*************check batch already created or not************
        READ TABLE lt_mchb TRANSPORTING NO FIELDS WITH KEY matnr = <fs_final>-matnr
                                                           werks = <fs_final>-to_plnt
                                                           lgort = lv_lgort
                                                           charg = <fs_final>-charg BINARY SEARCH.
        IF sy-subrc NE 0.
**************create new batch**************
          lv_matnr = <fs_final>-matnr.
          lo_batch->batch_create(
            EXPORTING
              material   = lv_matnr
              plant      = <fs_final>-to_plnt
              batch      = <fs_final>-charg
              storageloc = lv_lgort
            IMPORTING
              type       = lv_type
              message    = lv_msg ).
          IF lv_type = 'S'.
            <fs_final>-msg  = | { wa_return-message } , { lv_msg } |.
            <fs_final>-type = lv_type.
          ELSE.
            <fs_final>-msg  = | { wa_return-message } , { lv_msg } |.
            TRANSLATE lv_msg TO UPPER CASE.
            SEARCH lv_msg FOR 'ALREADY'.
            IF sy-subrc = 0.
              <fs_final>-type = 'S'.
              <fs_final>-msg  = | { wa_return-message } , Batch { <fs_final>-charg } of Material { <fs_final>-matnr } is Already Exists |.
              CLEAR : ls_lips ,lv_type,lv_msg.
              CONTINUE.
            ENDIF.
            <fs_final>-type = lv_type.
**************store error logs to the dms table*************
            lo_errorlog_dms->log_entry_store(
              EXPORTING
                type                = 22
                status              = 10
                plant               = <fs_final>-to_plnt
                material            = <fs_final>-matnr
                distributor         = <fs_final>-kunag
                msg                 = lv_msg ).
          ENDIF.
        ELSE.
          <fs_final>-msg = | { wa_return-message } , Already batch created |.
          <fs_final>-type = 'S'.
        ENDIF.
      ENDIF.
      CLEAR : ls_lips ,lv_type,lv_msg.
    ENDLOOP.

  ENDMETHOD.
  METHOD marc.
    DATA: lv_dispo TYPE t024d-dispo,
          lv_fevor TYPE t024f-fevor.

*Varibale for get profit center
    SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc) WHERE name = 'MAT_EXT_PROFIT_CENTER'
                                                      AND   type = 'S'.
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
      wa_plantdata-profit_ctr   = ls_tvarvc-low.
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
      wa_storagelocationdata-stge_loc     = 'D1'."WA_MARD-LGORT.
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
      wa_valuationdata-price_ctrl = 'S'.
*    ELSE.
*      wa_valuationdata-price_ctrl = wa_mbew-vprsv.
*    ENDIF.
*      wa_valuationdata-moving_pr = wa_mbew-verpr. "wa_final-verpr.
      wa_valuationdata-std_price = wa_mbew-stprs.
      wa_valuationdata-price_unit = wa_mbew-peinh.
      wa_valuationdata-val_class = wa_mbew-bklas.
*      wa_valuationdata-pr_ctrl_pp = wa_mbew-vmvpr.
*      wa_valuationdata-mov_pr_pp = wa_mbew-vmver.
      wa_valuationdata-std_pr_pp = wa_mbew-vmstp.
      wa_valuationdata-pr_unit_pp = wa_mbew-vmpei.
      wa_valuationdata-vclass_pp = wa_mbew-vmbkl.
      wa_valuationdata-pr_ctrl_py = wa_mbew-vjvpr.
      wa_valuationdata-mov_pr_py = wa_mbew-vjver.
      wa_valuationdata-std_pr_py = wa_mbew-vjstp.
      wa_valuationdata-pr_unit_py = wa_mbew-vjpei.
      wa_valuationdata-vclass_py = wa_mbew-vjbkl.
      wa_valuationdata-val_cat    = abap_false.
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
      wa_salesdatax-sales_org = 'SDMS'.
      wa_salesdatax-distr_chan = '20'.
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
      wa_salesdata-sales_org = 'SDMS'.
      wa_salesdata-distr_chan = '20'.
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
      wa_storagelocationdatax-stge_loc = 'D1'."WA_MARD-LGORT.
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

INITIALIZATION.
  DATA lobj_mat TYPE REF TO lcl_material.
  CREATE OBJECT lobj_mat. "create the object for local class

START-OF-SELECTION.
  lobj_mat->fetch( ).

MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR lobj_mat.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF p_chk = 'X' AND screen-name = 'PLANT'.          "disable excel file path
      screen-input = 1.
    ELSEIF p_chk = ' ' AND screen-name = 'PLANT'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
