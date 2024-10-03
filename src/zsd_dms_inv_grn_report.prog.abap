*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 30.11.2023
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                : DEVK934718
*
*  Business Logic            : Report for physical inventory to the material batch for dms invoice based & apr 1 stock
*
*  Released on Date          :
*
* Hardcoded                  : comcode  - 1000
*                              strg loc - D1
*                              lv_date = '20230401'.
*=======================================================================
REPORT zsd_dms_inv_grn_report.

**********global data dec***************
*data dec for invoice based physical inventory process
DATA : BEGIN OF gt_final OCCURS 0,
         vbeln    TYPE vbeln,
         fkart    TYPE fkart,
         vkorg    TYPE vkorg,
         fkdat    TYPE fkdat,
         kunag    TYPE kunag,
         posnr    TYPE posnr,
         meins    TYPE meins,
         matnr    TYPE matnr,
         fklmg    TYPE fklmg,
         werks    TYPE werks_d,
         name1    TYPE name1,
         werks2   TYPE kna1-werks,
         price    TYPE kna1-werks,
         checkbox TYPE checkbox,
         type     TYPE bapi_mtype,
         message  TYPE string,
         celltab  TYPE lvc_t_styl,
       END OF gt_final.
DATA : gv_kunag TYPE vbrk-kunag,
       gv_fkdat TYPE vbrk-fkdat,
       gv_vbeln TYPE vbrk-vbeln,
       gv_matnr TYPE mara-matnr.

************selection screen design****************
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : so_kunag FOR gv_kunag,     "customer no.
                   so_fkdat FOR gv_fkdat,     "billing date.
                   so_vbeln FOR gv_vbeln,     "billing date.
                   so_matnr FOR gv_matnr.     "Material

  PARAMETERS : p_date TYPE sy-datum,          "stock date
               p_days TYPE num4.              "invoice date

SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS : r1 RADIOBUTTON GROUP g1 USER-COMMAND u1 DEFAULT 'X',
               r2 RADIOBUTTON GROUP g1.

SELECTION-SCREEN : END OF BLOCK b2.

*&---------------------------------------------------------------------*
*class definition
*&---------------------------------------------------------------------*
CLASS lcl_invoice DEFINITION FINAL.

  PUBLIC SECTION.
*structure for apr1 stock based physical inventory process
    TYPES : BEGIN OF ty_final2,
              kunnr    TYPE kunnr,
              matnr    TYPE matnr,
              erdat    TYPE erdat,
              meins    TYPE meins,
              tot_stk  TYPE zlabst,
              vwerk    TYPE vwerk,
              name1    TYPE name1,
              checkbox TYPE checkbox,
              type     TYPE bapi_mtype,
              message  TYPE string,
            END OF ty_final2.
*data dec for apr1 stock based physical inventory process
    DATA : gt_final2 TYPE TABLE OF ty_final2.
*data dec alv class
    DATA : gv_alv TYPE REF TO cl_gui_alv_grid.
*methods for event handlers
    METHODS: change FOR EVENT data_changed
                    OF cl_gui_alv_grid IMPORTING er_data_changed.
*methods dec
    METHODS : fetch,           "fetching from vbrk,vbrp,kna1
      alv CHANGING lt_final TYPE ANY TABLE,             "master alv screen process
      data_process,     "processing the data to display in alv
      pop_up,           "pop to confirm update
      alv_change,       "changes on alv screen for invoice
      alv_change2,      "changes on alv screen for apr1 stk
      fetch2,           "fetch the data for apr1 stock
      select_all,       "select all checkbox
      deselect_all.     "deselect all checkbox

ENDCLASS.

*&---------------------------------------------------------------------*
*class IMPLEMENTATION
*&---------------------------------------------------------------------*

CLASS lcl_invoice IMPLEMENTATION.
  METHOD fetch.
***********Fetching data from vbrk,vbrp,kna1**************
    REFRESH : gt_final[],gt_final2[].
***************fetch only DMS distributor***************
    SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
    IF sy-subrc = 0.
      SELECT
              a~vbeln,
              a~fkart,
              a~vkorg,
              a~fkdat,
              a~kunag,
              b~posnr,
              b~meins,
              b~matnr,
              b~fklmg,
              b~werks,
              c~name1,
              c~werks AS werks2
              INTO CORRESPONDING FIELDS OF TABLE @gt_final[]
              FROM vbrk AS a
              INNER JOIN vbrp AS b ON
              a~vbeln = b~vbeln
              INNER JOIN kna1 AS c ON
              a~kunag = c~kunnr
              FOR ALL ENTRIES IN @lt_t001k
              WHERE a~vbeln IN @so_vbeln
              AND   a~kunag IN @so_kunag
              AND   a~fkdat IN @so_fkdat
              AND   fksto = @abap_false
              AND   bukrs = '1000'
              AND   c~werks = @lt_t001k-bwkey.
    ENDIF.
    IF sy-subrc = 0.
*********call method for background job************
      IF sy-batch = abap_true.
***********select all checkbox***********
        select_all( ).
***********physical inventory process***********
        alv_change( ).
***********Display ALV***********
        alv(
          CHANGING
            lt_final = gt_final[] ).
*        CALL SCREEN 9001.
      ELSE.                 "foreground direct display alv
        alv(
          CHANGING
            lt_final = gt_final[] ).
        CALL SCREEN 9001.
      ENDIF.
    ELSE.
      MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD alv.
    DATA lt_fcat TYPE lvc_t_fcat.   "data dec for fieldcat
    DATA : lt_exclude TYPE TABLE OF ui_func.      "exclude the toolbar it
    DATA(ls_layo) = VALUE lvc_s_layo( cwidth_opt = abap_true
                                      zebra = abap_true
                                      stylefname = 'CELLTAB' ).   "data dec for layout design
******create the object for alv********
    IF gv_alv IS INITIAL.
      CREATE OBJECT gv_alv
        EXPORTING
          i_parent = cl_gui_container=>default_screen.
    ENDIF.
*************"data append to the fieldcat***********
    REFRESH : lt_fcat.
    IF r1 = abap_true.
      lt_fcat = VALUE #(
      ( col_pos = 1  fieldname = 'CHECKBOX' scrtext_m = 'Checkbox' checkbox = abap_true edit = abap_true )
      ( col_pos = 2  fieldname = 'KUNAG'    ref_field = 'KUNAG' ref_table = 'VBRK')
      ( col_pos = 3  fieldname = 'NAME1'    ref_field = 'NAME1' ref_table = 'KNA1')
      ( col_pos = 4  fieldname = 'WERKS2'   scrtext_m = 'Delivery Plant')
      ( col_pos = 5  fieldname = 'VBELN'    ref_field = 'VBELN' ref_table = 'VBRK')
      ( col_pos = 6  fieldname = 'POSNR'    ref_field = 'POSNR' ref_table = 'VBRP')
      ( col_pos = 7  fieldname = 'FKDAT'    ref_field = 'FKDAT' ref_table = 'VBRK')
      ( col_pos = 8  fieldname = 'WERKS'    scrtext_m = 'Sales Plant')
      ( col_pos = 9  fieldname = 'MATNR'    ref_field = 'MATNR' ref_table = 'VBRP')
      ( col_pos = 10 fieldname = 'FKLMG'    ref_field = 'FKLMG' ref_table = 'VBRP')
      ( col_pos = 11 fieldname = 'MEINS'    ref_field = 'MEINS' ref_table = 'VBRP')
      ( col_pos = 12 fieldname = 'TYPE'     scrtext_m = 'TYPE'    )
      ( col_pos = 13 fieldname = 'MESSAGE'  scrtext_m = 'Message' ) ).
******call method for data processing for disable checkbox**********
      data_process( ).

    ELSEIF r2 = abap_true.
      lt_fcat = VALUE #(
      ( col_pos = 1   fieldname = 'CHECKBOX' scrtext_m = 'Checkbox' checkbox = abap_true edit = abap_true )
      ( col_pos = 2   fieldname = 'KUNNR'    ref_field = 'KUNNR'    ref_table = 'YMARD_NSAP')
      ( col_pos = 3   fieldname = 'NAME1'    ref_field = 'NAME1'    ref_table = 'KNA1')
      ( col_pos = 4   fieldname = 'VWERK'    scrtext_m = 'Plant')
      ( col_pos = 5   fieldname = 'ERDAT'    ref_field = 'ERDAT'    ref_table = 'YMARD_NSAP')
      ( col_pos = 6   fieldname = 'MATNR'    ref_field = 'MATNR'    ref_table = 'VBRP')
      ( col_pos = 7   fieldname = 'MEINS'    ref_field = 'MEINS'    ref_table = 'VBRP')
      ( col_pos = 8   fieldname = 'TOT_STK'  ref_field = 'TOT_STK'  ref_table = 'YMARD_NSAP')
      ( col_pos = 9   fieldname = 'TYPE'     scrtext_m = 'Type'    )
      ( col_pos = 10  fieldname = 'MESSAGE'  scrtext_m = 'Message' ) ).
    ENDIF.
***********exclude the standard toolbar***************
    lt_exclude = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_undo )
                          ( cl_gui_alv_grid=>mc_fc_refresh )
                          ( cl_gui_alv_grid=>mc_fc_loc_cut )
                          ( cl_gui_alv_grid=>mc_fc_loc_paste )
                          ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_copy  ) ).
* set the event handler for alv
    SET HANDLER change FOR gv_alv.
*******display the alv*********
    gv_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layo    "Layout
        it_toolbar_excluding          = lt_exclude
      CHANGING
        it_outtab                     = lt_final[]    "Output Table
        it_fieldcatalog               = lt_fcat ).     "Field Catalog ).
    REFRESH : lt_fcat.        "refresh fieldcat

  ENDMETHOD.

  METHOD alv_change.
    DATA: lo_inv_grn TYPE REF TO zcl_api_dms_grn_inv.     "data dec for physical inventory api class
    CREATE OBJECT lo_inv_grn.
*************customer block check***********
    DATA : lo_block TYPE REF TO zcl_common_check.
    CREATE OBJECT lo_block.
    DATA : lt_block TYPE TABLE OF zsd_st_cust_block.
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
*******process for selected invoice to update**********
    LOOP AT gt_final[] ASSIGNING FIELD-SYMBOL(<fs_final>) WHERE checkbox = 'X'.
      IF sy-subrc = 0.
        AT NEW vbeln.
*****************check distributor block or not*******************
          IF lt_block IS NOT INITIAL.
            READ TABLE lt_block TRANSPORTING NO FIELDS WITH KEY kunnr = <fs_final>-kunag
                                                                block = abap_true BINARY SEARCH.
            IF sy-subrc = 0.
              <fs_final>-message = | Distributor is blocked |.
              <fs_final>-type = 'E'.
              MODIFY gt_final[] FROM <fs_final> TRANSPORTING message WHERE vbeln = <fs_final>-vbeln.
              CONTINUE.
            ENDIF.
          ENDIF.
          DATA(ls_input) = VALUE zstr_dms_grn_inv_input( comcode     = '1000'
                                                         invoiceno   = <fs_final>-vbeln
                                                         invoicedate = <fs_final>-fkdat
                                                         distributor = <fs_final>-kunag ).
*******call method to update physical inventory**********
          lo_inv_grn->validation_process(
            EXPORTING
              ls_input = ls_input  " structure for direct physical inventory for dms - input
              double_click_check = abap_false
            IMPORTING
              lv_msg   = <fs_final>-message
              lv_type  = <fs_final>-type ).
          IF <fs_final>-type IS INITIAL.
            <fs_final>-type = 'E'.
          ENDIF.
          MODIFY gt_final[] FROM <fs_final> TRANSPORTING type message WHERE vbeln = <fs_final>-vbeln.
        ENDAT.
      ENDIF.
    ENDLOOP.
    CLEAR : lo_inv_grn.

    IF sy-batch NE abap_true.
***********refresh the alv display*************
      gv_alv->refresh_table_display( ).
    ENDIF.

  ENDMETHOD.

  METHOD data_process.
    DATA: ls_edit TYPE lvc_s_styl,
          lt_edit TYPE lvc_t_styl.
    DATA: ls_outtab LIKE LINE OF gt_final,
          lv_check  TYPE char1,
          lv_index  TYPE sy-tabix.
***********process for hide the line item checkbox*****************
    IF gt_final[] IS NOT INITIAL.
      SORT : gt_final[] BY kunag vbeln posnr.
      LOOP AT gt_final[] ASSIGNING FIELD-SYMBOL(<fs_final>).
        lv_index = sy-tabix.
        AT NEW vbeln.
          lv_check = 'X'.
        ENDAT.
        IF lv_check IS INITIAL.
          ls_edit-fieldname = 'CHECKBOX'.
          ls_edit-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT ls_edit INTO TABLE lt_edit.
          ls_outtab-celltab[] = lt_edit[].
          MODIFY gt_final[] INDEX lv_index FROM ls_outtab TRANSPORTING celltab.
          REFRESH : lt_edit.
          CLEAR : ls_outtab.
        ENDIF.
        CLEAR lv_check.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD change.
*    lt_row[] =  er_data_changed->mt_good_cells[].      "get changed data value row index
  ENDMETHOD.

  METHOD pop_up.
    "confirmation for material changes
    DATA : lv_ans TYPE char4,
           w_x    TYPE char1 VALUE abap_true.
***********get changed values to it********
    gv_alv->check_changed_data(
                        CHANGING
                           c_refresh = w_x ).
***********pop to confirm the update*************
    READ TABLE gt_final[] TRANSPORTING NO FIELDS WITH KEY checkbox = 'X'.
    IF sy-subrc NE 0.
      READ TABLE gt_final2[] TRANSPORTING NO FIELDS WITH KEY checkbox = 'X'.
    ENDIF.
    IF sy-subrc = 0.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar      = TEXT-006
          text_question = TEXT-003
          text_button_1 = TEXT-004
          text_button_2 = TEXT-005
        IMPORTING
          answer        = lv_ans.
      IF lv_ans = '1' AND r1 = abap_true.      "if we click yes for r1 invoice based
        alv_change( ).
      ELSEIF lv_ans = '1' AND r2 = abap_true.   "if we click yes for r2 apr1 date based
        alv_change2( ).
      ELSE.
        LEAVE SCREEN.
      ENDIF.
    ELSE.
      MESSAGE : 'Please select any data' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD fetch2..
**********fetch the datas from ymard_nsap & kna1*********
    REFRESH : gt_final2[],gt_final[].
    DATA : lv_index TYPE sy-tabix.
*************customer block check***********
    DATA : lo_block TYPE REF TO zcl_common_check.
    CREATE OBJECT lo_block.
    DATA : lt_block TYPE TABLE OF zsd_st_cust_block.
***************fetch only DMS distributor***************
    SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
    IF sy-subrc = 0.

      SELECT a~kunnr,
             a~matnr,
             a~erdat,
             a~meins,
             a~tot_stk,
             a~werks,
             b~name1
             FROM ymard_nsap AS a
             INNER JOIN kna1 AS b ON
             a~kunnr = b~kunnr
             INTO TABLE @gt_final2
             FOR ALL ENTRIES IN @lt_t001k
             WHERE tot_stk GT 0
             AND   a~kunnr IN @so_kunag
             AND   a~matnr IN @so_matnr
             AND   a~erdat EQ @p_date
             AND   a~werks EQ @lt_t001k-bwkey.
    ENDIF.
    IF sy-subrc = 0.
***************fetch the stock inward LOG table**********
      SELECT distributor,dist_plant,matnr,type FROM zdms_op_stk_inw INTO TABLE @DATA(lt_log).
      IF sy-subrc = 0.
        SORT : lt_log BY distributor dist_plant matnr type.
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
* fetch the material wise plant & strg loc.
      SELECT matnr,werks,lgort FROM mard INTO TABLE @DATA(lt_mard) FOR ALL ENTRIES IN @gt_final2
                                         WHERE matnr = @gt_final2-matnr
                                         AND   werks = @gt_final2-vwerk.
      IF sy-subrc = 0.
        SORT : lt_mard BY matnr werks.
      ENDIF.

******************Delete the success entries************
      LOOP AT gt_final2 ASSIGNING FIELD-SYMBOL(<ls_final>).
        CLEAR : lv_index.
        lv_index = sy-tabix.
**********check the material stock GRN completed or not**********
        READ TABLE lt_log TRANSPORTING NO FIELDS WITH KEY distributor = <ls_final>-kunnr
                                                          dist_plant  = <ls_final>-vwerk
                                                          matnr       = <ls_final>-matnr
                                                          type        = 'S' BINARY SEARCH.
        IF sy-subrc = 0.
          DELETE gt_final2 INDEX lv_index.
          CONTINUE.
        ENDIF.
*****************check distributor block or not*******************
        IF lt_block IS NOT INITIAL.
          READ TABLE lt_block TRANSPORTING NO FIELDS WITH KEY kunnr = <ls_final>-kunnr
                                                              block = abap_true BINARY SEARCH.
          IF sy-subrc = 0.
            <ls_final>-message = | Distributor is blocked |.
            <ls_final>-type = 'E'.
            CONTINUE.
          ENDIF.
        ENDIF.
*********************Plant check****************
        IF <ls_final>-vwerk IS INITIAL.
          <ls_final>-message = | Material - { <ls_final>-matnr } plant is missing - { <ls_final>-vwerk }|.
          <ls_final>-type    = 'E'.
          CONTINUE.
        ENDIF.
*****************material ext check*************
        READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY matnr = <ls_final>-matnr
                                                       werks = <ls_final>-vwerk BINARY SEARCH.
        IF sy-subrc NE 0.
          <ls_final>-message = | Material - { <ls_final>-matnr } not maintained in plant - { <ls_final>-vwerk }|.
          <ls_final>-type    = 'E'.
          CONTINUE.
        ENDIF.
      ENDLOOP.
*********call METHOD for background job************
      IF sy-batch = abap_true.
***********select all checkbox***********
        select_all( ).
***********physical inventory process***********
        alv_change2( ).
***********Display ALV***********
        alv(
          CHANGING
            lt_final = gt_final2[] ).
*        CALL SCREEN 9001.
      ELSE.
*********call method for for display main alv screen************
        alv(
          CHANGING
            lt_final = gt_final2 ).
        CALL SCREEN 9001.
      ENDIF.
    ELSE.
      MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD alv_change2.
    DATA: lo_inv_grn TYPE REF TO zcl_api_dms_grn_inv.     "data dec for physical inventory api class
    CREATE OBJECT lo_inv_grn.
    DATA : lt_create TYPE STANDARD TABLE OF  bapi_physinv_create_items,   "for physical inventory bapi
           lt_count  TYPE TABLE OF bapi_physinv_count_items.
    DATA : lt_stock TYPE TABLE OF	zdms_grn_stock. "for data insert custom log table dms
    DATA : lv_matnr  TYPE matnr18, "for local variable dec
           lv_item   TYPE posnr,
           lv_amt    TYPE bapi_price,
           lv_oldamt TYPE bapi_price,
           lv_belnr  TYPE belnr_d,
           lv_date   TYPE sy-datum,
           lv_lgort  TYPE lgort_d VALUE 'D1'.
*           lv_new    type LABST,

* fetch the stock details
    SELECT matnr,werks,lgort,charg,clabs FROM mchb INTO TABLE @DATA(lt_mchb) FOR ALL ENTRIES IN @gt_final2
                                                   WHERE matnr = @gt_final2-matnr
                                                   AND   werks = @gt_final2-vwerk.
**********get the 30 days before date***************
    IF p_days IS INITIAL.
      lv_date = p_date - 30.
    ELSE.
      lv_date = p_date - p_days.
    ENDIF.
****fetch the invoice details for price update
    SELECT vbeln,
           posnr,
           kunag_ana,
           fkdat_ana,
           matnr,
           fkimg,
           netwr,
           vgbel
           INTO TABLE @DATA(lt_vbrp) FROM vbrp
           FOR ALL ENTRIES IN @gt_final2
           WHERE kunag_ana  = @gt_final2-kunnr
           AND   matnr      = @gt_final2-matnr
           AND   fkdat_ana LE @p_date
           AND   fkdat_ana GE @lv_date
           AND   fkart_ana = 'YBBR'
           AND   vf_status_ana NE 'C'.
    IF sy-subrc = 0.
      SELECT vbeln,posnr,matnr,werks,lgort,charg FROM lips INTO TABLE @DATA(lt_lips)
                                                 FOR ALL ENTRIES IN @lt_vbrp
                                                 WHERE vbeln = @lt_vbrp-vgbel
                                                 AND   charg NE @abap_false.
      IF sy-subrc = 0.
        SORT : lt_lips BY vbeln matnr.
      ENDIF.
    ENDIF.
****************fetch the standard price for the material*************
    SELECT matnr,bwkey,vprsv,verpr,stprs FROM mbew INTO TABLE @DATA(lt_mbew)
                                         FOR ALL ENTRIES IN @gt_final2
                                         WHERE matnr = @gt_final2-matnr
                                         AND   bwkey = @gt_final2-vwerk.
    SORT : lt_mchb   BY matnr werks lgort charg,
           lt_vbrp   BY kunag_ana ASCENDING matnr ASCENDING fkdat_ana DESCENDING,
           lt_mbew   BY matnr bwkey.

    SORT : gt_final2 BY kunnr vwerk matnr.
*******process for selected customer to update**********
    LOOP AT gt_final2[] ASSIGNING FIELD-SYMBOL(<fs_final2>) WHERE checkbox = abap_true
                                                            AND   type     EQ abap_false.
***********check the material stock GRN completed or not**********
*      READ TABLE lt_log TRANSPORTING NO FIELDS WITH KEY distributor = <fs_final2>-kunnr
*                                                        matnr       = <fs_final2>-matnr
*                                                        type        = 'S' BINARY SEARCH.
*      IF sy-subrc = 0.
*        <fs_final2>-message = | Already Material stock inwarded |.
*        <fs_final2>-type = 'S'.
*        CONTINUE.
*      ENDIF.
*      IF <fs_final2>-type NE 'E'.
*******************get material old price******************
      READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY matnr     = <fs_final2>-matnr
                                                     bwkey     = <fs_final2>-vwerk BINARY SEARCH.
*******************get sales order price******************
      READ TABLE lt_vbrp INTO DATA(ls_vbrp) WITH KEY kunag_ana = <fs_final2>-kunnr
                                                     matnr     = <fs_final2>-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
**********check invoice material price & new material price same or not***********
        ls_vbrp-netwr = ls_vbrp-netwr / ls_vbrp-fkimg.
        lv_amt        = ls_vbrp-netwr.
        lv_matnr      = <fs_final2>-matnr.
*******check old price std or avrg price.
        IF ls_mbew-vprsv = 'S'.
          lv_oldamt = ls_mbew-stprs.
        ENDIF.
**********call the price change method*********
        IF lv_amt NE lv_oldamt.
          lo_inv_grn->price_change(
                        EXPORTING
                          material = lv_matnr             " Material Number (18 Characters)
                          plant    = <fs_final2>-vwerk             " Valuation area
                          date     = sy-datum             " Date for Pricing and Exchange Rate
                          amount   = lv_amt               " Price in BAPI currency format
                        IMPORTING
                          message  = <fs_final2>-message
                          type     = <fs_final2>-type               " Message type: S Success, E Error, W Warning, I Info, A Abort
          ).
        ENDIF.
      ELSE.
        <fs_final2>-message = | No more invoices for Material - { <fs_final2>-matnr } past one month - { <fs_final2>-kunnr } |.
        <fs_final2>-type = 'E'.
      ENDIF.
*      ENDIF.
**********check the batch details***********
      IF <fs_final2>-type NE 'E'.
        READ TABLE lt_lips INTO DATA(ls_lips) WITH KEY vbeln = ls_vbrp-vgbel
                                                       matnr = ls_vbrp-matnr BINARY SEARCH.
        IF ls_lips-charg IS INITIAL.
          <fs_final2>-message = | Material batch is not found - { <fs_final2>-matnr } |.
          <fs_final2>-type = 'E'.
        ELSE.
          READ TABLE lt_mchb INTO DATA(ls_mchb) WITH KEY matnr = <fs_final2>-matnr
                                                         werks = <fs_final2>-vwerk
                                                         lgort = lv_lgort
                                                         charg = ls_lips-charg BINARY SEARCH.
          IF sy-subrc NE 0.
**********call the batch create**********************
            lo_inv_grn->batch_create(
              EXPORTING
                material   = lv_matnr
                plant      = <fs_final2>-vwerk
                batch      = ls_lips-charg
                storageloc = lv_lgort
              IMPORTING
                message    = <fs_final2>-message
                type       = <fs_final2>-type
            ).
          ENDIF.
        ENDIF.
      ENDIF.

      IF <fs_final2>-type NE 'E'.
*item data filling
        APPEND VALUE bapi_physinv_create_items( material     = <fs_final2>-matnr
                                                batch        = ls_lips-charg ) TO lt_create.
        lv_item = 1 + lv_item.
*item count data filling
        APPEND VALUE bapi_physinv_count_items( material      = <fs_final2>-matnr
                                               batch         = ls_lips-charg
                                               entry_qnt     = ls_mchb-clabs + <fs_final2>-tot_stk
                                               item          = lv_item
                                               entry_uom     = <fs_final2>-meins ) TO lt_count.
        CONDENSE : lv_matnr.
        DATA(ls_header) = VALUE bapi_physinv_create_head( plant        = <fs_final2>-vwerk
                                                          stge_loc     = lv_lgort
                                                          phys_inv_ref = |{ lv_matnr }|  ).
**********call the physical inventory bapi**********************
        lo_inv_grn->physical_bapi(
          EXPORTING
            ls_header     = ls_header        " BAPI Communication Structure: Create Phys. Inv. Doc., Header
            lt_itemcount  = lt_count         " BAPI Communication Structure: Count Data for Item
            lt_itemcreate = lt_create        " BAPI Communication Structure: Create Phys. Inv. Doc., Items
          IMPORTING
            message       = <fs_final2>-message
            type          = <fs_final2>-type
            belnr         = lv_belnr ).    " Physical Inventory Document
      ENDIF.
*store the item logs into zdms_grn_stock.
      DATA(ls_stock) =  VALUE zdms_op_stk_inw( distributor  = <fs_final2>-kunnr
                                               dist_plant   = <fs_final2>-vwerk
                                               vbeln        = ls_vbrp-vbeln
                                               invoice_date = ls_vbrp-fkdat_ana
                                               matnr        = <fs_final2>-matnr
                                               charg        = ls_lips-charg
                                               old_price    = lv_oldamt
                                               new_price    = lv_amt
                                               erdat        = sy-datum
                                               erzet        = sy-uzeit
                                               ernam        = sy-uname
                                               ext_qty      = ls_mchb-clabs
                                               total_qty    = <fs_final2>-tot_stk + ls_mchb-clabs
                                               new_qty      = <fs_final2>-tot_stk
                                               belnr        = lv_belnr
                                               type         = <fs_final2>-type
                                               message      = <fs_final2>-message ).
      MODIFY zdms_op_stk_inw FROM ls_stock.
      CLEAR : lv_item,ls_header,lv_item,ls_stock,lv_oldamt,ls_mchb,
              lv_amt,ls_vbrp,ls_mbew,lv_matnr,lv_belnr,ls_lips.
      REFRESH : lt_count,lt_create.
    ENDLOOP.
    CLEAR : lo_inv_grn.
    IF sy-batch NE abap_true.
***********refresh the alv display*************
      gv_alv->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.

  METHOD select_all.

    DATA : ls_final  LIKE LINE OF gt_final,
           ls_final2 LIKE LINE OF gt_final2.

    IF r1 = abap_true.        "for invoice method

      ls_final-checkbox = abap_true.
      MODIFY gt_final  FROM ls_final TRANSPORTING checkbox WHERE checkbox = abap_false.

    ELSEIF r2 = abap_true.      "for opening stock

      ls_final2-checkbox = abap_true.
      MODIFY gt_final2  FROM ls_final2 TRANSPORTING checkbox WHERE checkbox = abap_false.

    ENDIF.

    IF sy-subrc = 0 AND sy-batch NE abap_true.
      gv_alv->refresh_table_display( ).
    ENDIF.

  ENDMETHOD.

  METHOD deselect_all.

    DATA : ls_final  LIKE LINE OF gt_final,
           ls_final2 LIKE LINE OF gt_final2.

    IF r1 = abap_true.

      ls_final-checkbox = abap_false.
      MODIFY gt_final  FROM ls_final TRANSPORTING checkbox WHERE checkbox = abap_true.

    ELSEIF r2 = abap_true.

      ls_final2-checkbox = abap_false.
      MODIFY gt_final2  FROM ls_final2 TRANSPORTING checkbox WHERE checkbox = abap_true.

    ENDIF.

    IF sy-subrc = 0.
      gv_alv->refresh_table_display( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*******before screen process***********
INITIALIZATION.
*********create the object for local class*********
  DATA lobj_inv TYPE REF TO lcl_invoice.
  CREATE OBJECT lobj_inv.
  p_days = 30.
********begining of the program**********
START-OF-SELECTION.
***********call method for fetching the data********
  IF lobj_inv IS NOT INITIAL.
***********invoice***********
    IF r1 = abap_true.
      lobj_inv->fetch( ).
**********Apr 1 stock********
    ELSEIF r2 = abap_true.
      lobj_inv->fetch2( ).
    ENDIF.
  ENDIF.
*modify the screen based on radiobutton
AT SELECTION-SCREEN OUTPUT.
*********disable invoice date********
  LOOP AT SCREEN.
    IF r2 = 'X' AND ( screen-name CS 'SO_FKDAT' OR screen-name CS 'SO_VBELN' ).
      screen-active = 0.
      MODIFY SCREEN.
    ELSEIF r1 = 'X' AND ( screen-name CS 'P_DATE' OR screen-name CS 'P_DAYS' OR screen-name CS 'SO_MATNR' ).
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZINV'.
  SET TITLEBAR 'ZINV'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR lobj_inv.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'UPDATE'.        "intiate the pop up screen.
      lobj_inv->pop_up( ).
    WHEN 'SELECTALL'.
      lobj_inv->select_all( ).
    WHEN 'DEALL'.
      lobj_inv->deselect_all( ).
  ENDCASE.

ENDMODULE.
