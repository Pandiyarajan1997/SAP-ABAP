class ZCL_API_DMS_GRN_INV definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  data GS_INPUT type ZSTR_DMS_GRN_INV_INPUT .
  data GV_MSG type STRING .
  data GV_DATA type STRING .
  data GV_TYPE type BAPI_MTYPE .

  methods VALIDATION_PROCESS
    importing
      value(LS_INPUT) type ZSTR_DMS_GRN_INV_INPUT optional
      !DOUBLE_CLICK_CHECK type CHAR1 default ABAP_TRUE
    exporting
      !LV_MSG type STRING
      !LV_TYPE type BAPI_MTYPE .
  methods SEND_RESPONSE .
  methods PHYSICAL_BAPI
    importing
      value(LS_HEADER) type BAPI_PHYSINV_CREATE_HEAD
      value(LT_ITEMCOUNT) type ZBAPI_TT_PHYSINV_COUNT_ITEMS
      value(LT_ITEMCREATE) type ZBAPI_TT_PHYSINV_CREATE_ITEMS
    exporting
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING
      !BELNR type IBLNR .
  methods BATCH_CREATE
    importing
      value(MATERIAL) type MATNR18
      value(PLANT) type WERKS_D
      value(BATCH) type CHARG_D
      value(STORAGELOC) type LGORT_D
    exporting
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE .
  methods BATCH_CHANGE
    importing
      !MATERIAL type MATNR18
      !PLANT type WERKS_D
      !BATCH type CHARG_D
      !STORAGELOC type LGORT_D
      !CREATED_ON type ERSDA
    exporting
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE .
  methods PRICE_CHANGE
    importing
      value(MATERIAL) type BAPI_MATVAL_KEY-MATERIAL
      value(PLANT) type BAPI_MATVAL_KEY-VAL_AREA
      value(DATE) type PRSDT
      value(AMOUNT) type BAPI_PRICE
    exporting
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE .
protected section.
private section.

  data GV_BODY type STRING .
ENDCLASS.



CLASS ZCL_API_DMS_GRN_INV IMPLEMENTATION.


  METHOD batch_change.

    DATA: lt_class      TYPE STANDARD TABLE OF  sclass,
          lt_objectdata TYPE STANDARD TABLE OF  clobjdat.
    DATA: lt_objectkeytable  TYPE  TABLE OF bapi1003_object_keys.
    DATA: lw_objectkeytable LIKE LINE OF lt_objectkeytable.
    DATA: lv_objectkeys TYPE bapi1003_key-object.
* holds data for charcateristics with type CHAR/DATE
    DATA: lt_val_char  TYPE TABLE OF bapi1003_alloc_values_char,
          lw_val_char  TYPE bapi1003_alloc_values_char,
          lt_return    TYPE TABLE OF bapiret2,
          lv_objkey    TYPE ausp-objek.
    DATA: lt_allocnum  TYPE STANDARD TABLE OF  bapi1003_alloc_values_num,
          lt_alloccurr TYPE STANDARD TABLE OF  bapi1003_alloc_values_curr.
* update batch classification of charcateristics type created on
    lv_objkey = material.
    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
        classtype    = '023'
        object       = lv_objkey
        objecttable  = 'MARA'
      TABLES
        t_class      = lt_class
        t_objectdata = lt_objectdata.
    IF sy-subrc <> 0.
*       Implement suitable error handling here
    ELSE.
      DATA(l_class) = VALUE #( lt_class[ 1 ]-class OPTIONAL ).
      IF l_class = 'ZBATCH_CREATION'.

        lw_objectkeytable-key_field = 'MATNR'.
        lw_objectkeytable-value_int = material.
        APPEND lw_objectkeytable TO lt_objectkeytable.
        CLEAR  lw_objectkeytable.
        lw_objectkeytable-key_field = 'CHARG'.
        lw_objectkeytable-value_int = batch.
        APPEND lw_objectkeytable TO lt_objectkeytable.
        CLEAR  lw_objectkeytable.
        lw_objectkeytable-key_field = 'WERKS'.
        lw_objectkeytable-value_int = plant.
        APPEND lw_objectkeytable TO lt_objectkeytable.
        CLEAR  lw_objectkeytable.
        lw_objectkeytable-key_field = 'LGORT'.
        lw_objectkeytable-value_int = storageloc.
        APPEND lw_objectkeytable TO lt_objectkeytable.
        CLEAR  lw_objectkeytable.
*Classification BAPI: Generate Concatenated Key
        REFRESH : lt_return.
        CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
          EXPORTING
            objecttable    = 'MCH1'
          IMPORTING
            objectkey_conc = lv_objectkeys
          TABLES
            objectkeytable = lt_objectkeytable
            return         = lt_return.
*Classification BAPI: Change Assignment
        REFRESH : lt_return.
        lw_val_char-charact = 'BATCH_DATE'.
        lw_val_char-value_neutral = created_on.
        APPEND lw_val_char TO lt_val_char.
        CALL FUNCTION 'BAPI_OBJCL_CHANGE'
          EXPORTING
            objectkey          = lv_objectkeys
            objecttable        = 'MCH1'
            classnum           = 'ZBATCH_CREATION'
            classtype          = '023'
          TABLES
            return             = lt_return
            allocvaluesnumnew  = lt_allocnum[]
            allocvaluescharnew = lt_val_char[]
            allocvaluescurrnew = lt_alloccurr[].
        READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
        IF sy-subrc EQ 0.
          message = ls_return-message.
          type    = ls_return-type.
        ELSE.
          message = |Created on date changed successfully for batch - { batch }|.
          type = 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD batch_create.
    DATA lt_return TYPE TABLE OF bapiret2.
    CALL FUNCTION 'BAPI_BATCH_CREATE'
      EXPORTING
        material             = material
        plant                = plant
        batch                = batch
        batchstoragelocation = storageloc
      TABLES
        return               = lt_return.
    READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      message = ls_return-message.
      type    = 'E'.
    ELSE.
      type    = 'S'.
      message = | { batch } - batch created successfully |.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.

*&Created by: Pandiarajan
*&Created On: 15.11.2023
*&Purpose   : direct physical inventory for distributor & store datas into log table - DMS
*&Reference : Ramakrishnan J
*hardcoded  : strg loc = D1
*-------------------------------------------------------------------------------------
*    "Input of API Request
    CLEAR : gv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = gv_data.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
      EXPORTING
        json         = gv_data
        pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data         = gs_input ).
*method contains validation & process
    CALL METHOD validation_process
      EXPORTING
        ls_input = gs_input                  " structure for direct physical inventory for dms - input
      IMPORTING
        lv_msg   = gv_msg
        lv_type  = gv_type.                 " Message type: S Success, E Error, W Warning, I Info, A Abort
*method contains convert the response
    CALL METHOD send_response.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = gv_body ).

  ENDMETHOD.


  METHOD physical_bapi.
    DATA : lt_return TYPE TABLE OF bapiret2,
           lv_iblnr  TYPE ikpf-iblnr.
    DATA : lv_gjahr TYPE gjahr.
****************get fiscal year*******************
    CALL FUNCTION 'GET_CURRENT_YEAR'
      EXPORTING
        bukrs = 'DMS1'
        date  = sy-datum
      IMPORTING
        curry = lv_gjahr.
    REFRESH lt_return.
*Create Physical Inventory Document
    CALL FUNCTION 'BAPI_MATPHYSINV_CREATE_MULT'
      EXPORTING
        head   = ls_header
      TABLES
        items  = lt_itemcreate
        return = lt_return.
    READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      IF ls_return-number = '700' AND ls_return-id = 'M7'.
        IF ls_header-phys_inv_ref IS NOT INITIAL.
          SELECT * FROM ikpf INTO TABLE @DATA(lt_ikpf) WHERE werks = @ls_header-plant
                                                       AND   gjahr = @lv_gjahr
                                                       AND   xblni = @ls_header-phys_inv_ref.
        ELSE.
          SELECT * FROM ikpf INTO TABLE @lt_ikpf WHERE werks = @ls_header-plant
                                                 AND   gjahr = @lv_gjahr
                                                 AND   vgart = 'IB'
                                                 AND   dstat = @abap_false.
        ENDIF.
        IF sy-subrc = 0.
          SORT lt_ikpf BY bldat DESCENDING.
          READ TABLE lt_ikpf INTO DATA(ls_ikpf) INDEX 1.
          lv_iblnr = ls_ikpf-iblnr.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE lt_return INTO ls_return WITH KEY type = 'S'.
      lv_iblnr = ls_return-message_v1.
    ENDIF.
    IF lv_iblnr IS NOT INITIAL.
      lv_iblnr = |{ lv_iblnr ALPHA = IN }|.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      REFRESH lt_return.
*Enter Count for Certain Items of a Physical Inventory Document
      CALL FUNCTION 'BAPI_MATPHYSINV_COUNT'
        EXPORTING
          physinventory = lv_iblnr
          fiscalyear    = lv_gjahr
        TABLES
          items         = lt_itemcount
          return        = lt_return.
      READ TABLE lt_return INTO ls_return WITH KEY type = 'S'.
      IF sy-subrc = 0 OR ls_ikpf-zstat = abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        REFRESH lt_return.
*Post differences for certain items in a phys. inv. document
        CALL FUNCTION 'BAPI_MATPHYSINV_POSTDIFF'
          EXPORTING
            physinventory = lv_iblnr
            fiscalyear    = lv_gjahr
          TABLES
            return        = lt_return.
        READ TABLE lt_return INTO ls_return WITH KEY type = 'S'.
        IF sy-subrc NE 0.
          READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
        ELSE.""""success doc posted
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          belnr   = ls_return-message_v2.
        ENDIF.
      ELSE.
        READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
      ENDIF.
    ENDIF.
    message = ls_return-message.
    type    = ls_return-type.
  ENDMETHOD.


  METHOD price_change.
    DATA : lt_prices     TYPE TABLE OF  bapi_matval_prices,
           lt_return     TYPE TABLE OF bapiret2,
           ls_prices     TYPE  bapi_matval_prices,
           valuationtype TYPE  bapi_matval_key-val_type,
           pricedate     TYPE  bapi_matval_pricedate,
           lv_period     TYPE  bkpf-monat,
           lv_bukrs      TYPE  bukrs.
    CLEAR : ls_prices,pricedate.
    lv_bukrs = 'DMS1'.
    CALL FUNCTION 'FI_PERIOD_DETERMINE'
      EXPORTING
        i_budat = date
        i_bukrs = lv_bukrs
      IMPORTING
        e_gjahr = pricedate-fisc_year
        e_monat = lv_period.
**********fill header data***********
    pricedate-fisc_period = lv_period.
    pricedate-price_date  = date.
********fill price table***********
    ls_prices-currency       = 'INR'.
    ls_prices-price          = amount.
    ls_prices-curr_type      = '10'.
    ls_prices-valuation_view = '0'.
    ls_prices-price_unit     = '1'.
    ls_prices-currency_iso   = 'INR'.
    APPEND ls_prices TO lt_prices.
**********call the bapi to change price***********
    CALL FUNCTION 'BAPI_MATVAL_PRICE_CHANGE'
      EXPORTING
        material      = material
        valuationarea = plant
        valuationtype = valuationtype
        pricedate     = pricedate
      TABLES
        prices        = lt_prices
        return        = lt_return.
*********check any error*************
    READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
    IF sy-subrc = 0.
      message = ls_return-message.
      type = 'E'.
    ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
      message = |{ material } - price changed successfully| .
      type    = 'S'.

    ENDIF.
  ENDMETHOD.


  METHOD send_response.
    TYPES : BEGIN OF ty_output,
              comcode     TYPE bukrs,
              distributor TYPE kunag,
              invoiceno	  TYPE vbeln_vf,
              invoicedate TYPE fkdat,
              status      TYPE bapi_mtype,
              message	    TYPE string,
            END OF ty_output.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.     "data dec for store api log table
    CREATE OBJECT lo_log_upd.
    DATA : lt_response TYPE TABLE OF ty_output.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.

***********fill the response**************
    IF gv_msg IS NOT INITIAL.
      IF gv_type IS INITIAL.
        gv_type = 'E'.
      ENDIF.
      APPEND VALUE #( comcode     = gs_input-comcode
                      invoiceno   = gs_input-invoiceno
                      invoicedate = gs_input-invoicedate
                      distributor = gs_input-distributor
                      status      = gv_type
                      message     = gv_msg ) TO lt_response.
    ENDIF.
**********send the data to mis & store logs***********
    IF lt_response IS NOT INITIAL.
*********************error log***********
      DATA : lv_orderid TYPE zorder_id.
      lv_orderid = gs_input-invoiceno.
*for dms plant fetching
      gs_input-distributor = |{ gs_input-distributor ALPHA = IN }|.
      SELECT SINGLE werks FROM kna1 INTO @DATA(lv_werks)
                                    WHERE kunnr = @gs_input-distributor.
      READ TABLE lt_response INTO DATA(ls_output) WITH KEY status = 'E'.
      IF sy-subrc = 0.
        lo_errorlog_dms->log_entry_store(
          EXPORTING
            type                = 17
            status              = 10
            dms_orderid         = lv_orderid
            distributor         = gs_input-distributor
            plant               = lv_werks
            msg                 = ls_output-message ).
      ENDIF.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  lt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = gv_body ).
*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMS_GRN_INV'
          ijson           = gv_data
          ojson           = gv_body
          distributor     = gs_input-distributor
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      gs_input-invoiceno   = |{ gs_input-invoiceno ALPHA = IN }|.
      DELETE FROM zsd_invoice_dms WHERE invoice_no = gs_input-invoiceno.
    ENDIF.

  ENDMETHOD.


  METHOD validation_process.

    DATA : lt_create TYPE STANDARD TABLE OF  bapi_physinv_create_items,   "for physical inventory bapi
           lt_count  TYPE TABLE OF bapi_physinv_count_items.
    DATA : lt_stock  TYPE TABLE OF  zdms_grn_stock. "for data insert custom log table dms
    DATA : lv_total  TYPE fkimg,                    "for local variable dec
           lv_new    TYPE fkimg,
           lv_matnr  TYPE matnr18,
           lv_item   TYPE posnr,
           lv_amt    TYPE bapi_price,
           lv_oldamt TYPE bapi_price.
    DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
    CREATE OBJECT lo_common_check.
    CLEAR : gv_msg,gv_type,lv_msg,lv_type.
    IF ls_input IS INITIAL.
      lv_msg  = | Please fill the data |.
      EXIT.
    ENDIF.
*input conversion for vbeln and inv date
    ls_input-invoiceno   = |{ ls_input-invoiceno ALPHA = IN }|.
    ls_input-distributor = |{ ls_input-distributor ALPHA = IN }|.
*Multiple click on front end so avoiding duplicates
    IF double_click_check = abap_true.
      SELECT SINGLE * FROM zsd_invoice_dms INTO @DATA(l_dupl_ordid)
                                           WHERE invoice_no = @ls_input-invoiceno.
      IF sy-subrc = 0.
        lv_msg = | Already Invoice In Processing { ls_input-invoiceno }; { lv_msg }|.
        EXIT.
      ELSE.
        DATA(l_orderid_chk) = VALUE zsd_invoice_dms( mandt      = sy-mandt
                                                     invoice_no = ls_input-invoiceno ).
        INSERT zsd_invoice_dms FROM l_orderid_chk.
      ENDIF.
    ENDIF.
*fetch the invoice & stocks datas from vbrk, vbrp,lips
    SELECT SINGLE vbeln,fkart,kunag,fksto FROM  vbrk INTO @DATA(ls_vbrk)
                                          WHERE vbeln = @ls_input-invoiceno
                                          AND   fkdat = @ls_input-invoicedate
                                          AND   bukrs = @ls_input-comcode
                                          AND   kunag = @ls_input-distributor.
    IF sy-subrc NE 0.
      lv_msg = | invoice not created for { ls_input-invoiceno } and this date { ls_input-invoicedate } for Distributor - { ls_input-distributor }; { lv_msg }|.
      EXIT.
    ELSEIF sy-subrc = 0 AND ls_vbrk-fksto = abap_false AND ls_input-cancel_flag IS NOT INITIAL.
      lv_msg = | Invoice is not cancelled in Sheenlac |.
      EXIT.
    ELSEIF sy-subrc = 0 AND ls_vbrk-fksto = abap_true AND ls_input-cancel_flag IS INITIAL.
      lv_msg = | GRN not Possible - invoice already cancelled in Sheenlac |.
      EXIT.
    ENDIF.
*for dms plant fetching
    SELECT SINGLE werks FROM kna1 INTO @DATA(lv_werks)
                                  WHERE kunnr = @ls_vbrk-kunag.
    IF lv_werks IS INITIAL.
      lv_msg = | Distributor { ls_vbrk-kunag } plant not maintained in Customer Master ; { lv_msg }|.
      EXIT.
    ENDIF.
***********check plant & business area********
    SELECT werks,spart FROM t134g INTO TABLE @DATA(lt_area) WHERE werks = @lv_werks
                                                            AND   gsber = @lv_werks.
    IF sy-subrc NE 0.
      lv_msg = | Plant - { lv_werks } & business area Mismatch; { lv_msg }|.
      EXIT.
    ELSE.
      SORT lt_area BY werks spart.
    ENDIF.
***********check plant & valuation area********
    SELECT SINGLE bwkey FROM t134h INTO @DATA(ls_area) WHERE bwkey = @lv_werks
                                                       AND   gsber = @lv_werks.
    IF sy-subrc NE 0.
      lv_msg = | Plant - { lv_werks } & Valuation area Mismatch; { lv_msg }|.
      EXIT.
    ENDIF.
** checking plant & company code
    SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(t001k) WHERE bwkey = @lv_werks
                                                          AND   bukrs = 'DMS1'.
    IF sy-subrc NE 0.
      lv_msg = | Plant { lv_werks } not maintained in - DMS1  ; { lv_msg }|.
      EXIT.
    ENDIF.
***************change the order type based on the cancel GRN
    IF ls_vbrk-fkart = 'YBBR' AND ls_input-cancel_flag IS NOT INITIAL.
      ls_vbrk-fkart = 'YBBC'.  "cancel the invoice GRN
    ELSEIF ls_vbrk-fkart = 'YBRE' AND ls_input-cancel_flag IS NOT INITIAL.
      ls_vbrk-fkart = 'YBRC'.  "cancel the Return invoice GRN
    ENDIF.
**fetch the invoice & customer already stock inward - ZDMS_GRN_HEADER
    SELECT SINGLE vbeln,invoice_type FROM zdms_grn_header INTO @DATA(ls_vbeln)
                                                          WHERE distributor  = @ls_vbrk-kunag
                                                          AND   vbeln        = @ls_input-invoiceno
                                                          AND   invoice_date = @ls_input-invoicedate
                                                          AND   invoice_type = @ls_vbrk-fkart.
    IF sy-subrc EQ 0.
      lv_msg = | Already GRN completed for this invoice { ls_input-invoiceno }; { lv_msg }|.
      EXIT.
    ENDIF.
**fetch the invoice & customer already stock inward - ZDMS_GRN_HEADER
    SELECT SINGLE vbeln FROM zdms_grn_header INTO ls_vbeln
                                             WHERE distributor  = ls_vbrk-kunag
                                             AND   vbeln        = ls_input-invoiceno
                                             AND   invoice_date = ls_input-invoicedate.
    IF sy-subrc NE 0 AND ls_input-cancel_flag IS NOT INITIAL.
      lv_msg = | GRN is not completed for this invoice { ls_input-invoiceno } - cancellation is not possible; { lv_msg }|.
      EXIT.
    ENDIF.
***************************FI period check*******************
    lo_common_check->fi_period_check(
      EXPORTING
        posting_date =  sy-datum          " Field of type DATS
        acct_type    =  'D'               " Account type
        com_code     =  'DMS1'   " Company Code
      IMPORTING
        type         =  DATA(lv_msgtype)                  " Single-Character Flag
      EXCEPTIONS
        mandatory    = 1                " Fill POSTING_DATE , ACCT_TYPE , COM_CODE
        OTHERS       = 2
).
    IF lv_msgtype = 'E'.
      lv_msg = |FI Posting Period Not Opened|.
      EXIT.
    ENDIF.
***************************MM period check*******************
    CLEAR : lv_msgtype.
    lo_common_check->mm_period_check(
      EXPORTING
        com_code  = 'DMS1'               " Company Code
        date      = sy-datum                      " Posting Date in the Document
      IMPORTING
        type      = lv_msgtype
      EXCEPTIONS
        mandatory = 1                " Fill Com_code & Date
        OTHERS    = 2 ).
    IF lv_msgtype = 'E'.
      lv_msg = |MM Posting Period Not Opened|.
      EXIT.
    ENDIF.

*****************************Block the process for distributor or customer*****************
    DATA : lobj_check TYPE REF TO zcl_common_check.
    CREATE OBJECT lobj_check.
********************for distributor checking****************
    CLEAR : lv_msgtype.
    lobj_check->dms_process_stop(
      EXPORTING
        kunnr        = gs_input-distributor
        process_type = 'GR'
      IMPORTING
        message      = lv_msg
        type         = lv_msgtype ).
    IF lv_msgtype = 'E'.
      EXIT.
    ENDIF.

*fetching the delivery no to fetch the material & qty list
    SELECT vgbel,matnr,fkimg,netwr FROM vbrp INTO TABLE @DATA(lt_vbrp)
                                             WHERE vbeln = @ls_vbrk-vbeln.
    IF sy-subrc = 0.
      READ TABLE lt_vbrp INTO DATA(ls_vbrp) INDEX 1.
      SELECT vbeln,matnr,werks,lgort,charg,lfimg,meins FROM lips INTO TABLE @DATA(lt_lips)
                                                                 WHERE vbeln = @ls_vbrp-vgbel
                                                                 AND   charg NE ' '.
    ENDIF.
    IF sy-subrc = 0.
* fetch the stock details
      SELECT matnr,werks,lgort,charg,clabs,ersda FROM mchb
                                                 INTO TABLE @DATA(lt_mchb)
*                                                 FOR ALL ENTRIES IN @lt_lips
                                                 WHERE werks = @lv_werks. "matnr = @lt_lips-matnr.
* fetch the material details
      SELECT a~matnr,a~werks,a~lgort,b~spart FROM mard AS a
                                             INNER JOIN mara AS b ON a~matnr = b~matnr
                                             INTO TABLE @DATA(lt_mard)
*                                             FOR ALL ENTRIES IN @lt_lips
                                             WHERE "a~matnr = @lt_lips-matnr
                                                    werks = @lv_werks.
****************fetch the standard price for the material*************
      SELECT matnr,bwkey,vprsv,verpr,stprs FROM mbew INTO TABLE @DATA(lt_mbew)
*                                           FOR ALL ENTRIES IN @lt_lips
                                           WHERE "matnr = @lt_lips-matnr
                                                 bwkey = @lv_werks.
    ENDIF.
******************
*sort the it for binary search.
    SORT  : lt_mchb BY matnr werks lgort charg,
            lt_mard BY matnr werks,
            lt_lips BY matnr charg,
            lt_mbew BY matnr bwkey,
            lt_vbrp BY matnr.
    CLEAR: lv_total,lv_new,lv_item,ls_vbeln.
*local variable dec.
    DATA(lt_lips2) = lt_lips.
***********add the qty based duplicate material & batch ************
    DELETE ADJACENT DUPLICATES FROM lt_lips COMPARING matnr charg.
    IF sy-subrc = 0.
      LOOP AT lt_lips2 INTO DATA(ls_lips).
        AT NEW matnr.
          CONTINUE.
        ENDAT.
        READ TABLE lt_lips ASSIGNING FIELD-SYMBOL(<fs_lips2>) WITH KEY matnr = ls_lips-matnr
                                                                       charg = ls_lips-charg BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_lips2>-lfimg = <fs_lips2>-lfimg + ls_lips-lfimg.
        ENDIF.
      ENDLOOP.
    ENDIF.
    REFRESH lt_lips2.

*****************old material to new material code conversion***************
    LOOP AT lt_lips ASSIGNING FIELD-SYMBOL(<fs_lips>).
***********************Old material code conversion************
      IF <fs_lips>-matnr(1) NE 'Z'.
***********old material code get*************
        SELECT SINGLE matnr,bismt,mtart FROM mara INTO @DATA(ls_old)
                                        WHERE matnr = @<fs_lips>-matnr.
        IF ls_old-mtart NE 'HAWA'.
          IF ls_old-bismt IS INITIAL.
            lv_msg = | New material is not maintained in old material code - { <fs_lips>-matnr }, { lv_msg } |.
            CONTINUE.
          ELSE.
            CONDENSE ls_old-bismt.
            <fs_lips>-matnr = ls_old-bismt.
          ENDIF.
        ENDIF.
        CLEAR : ls_old.
      ENDIF.
    ENDLOOP.

    IF lv_msg IS NOT INITIAL.
      EXIT.
    ENDIF.

    SORT : lt_lips BY matnr charg.
**********loop process & bapi data filling******************
    LOOP AT lt_lips ASSIGNING <fs_lips>.
************check qty zero**********
      IF <fs_lips>-lfimg IS INITIAL.
        lv_msg = | Material - { <fs_lips>-matnr } qty is zero ; { lv_msg }|.
        CONTINUE.
      ENDIF.

      AT NEW matnr.
*******************get sales order price******************
        CLEAR : ls_vbrp.
        READ TABLE lt_vbrp INTO ls_vbrp WITH KEY matnr = <fs_lips>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
**********check invoice material price & new material price same or not***********
          CLEAR : lv_matnr,lv_amt,lv_oldamt.
          ls_vbrp-netwr = ls_vbrp-netwr / ls_vbrp-fkimg.
          lv_amt        = ls_vbrp-netwr.
        ELSE.
          CLEAR : ls_old,ls_vbrp.
          SELECT SINGLE matnr,bismt,mtart FROM mara INTO @ls_old
                                          WHERE bismt = @<fs_lips>-matnr.
          READ TABLE lt_vbrp INTO ls_vbrp WITH KEY matnr = ls_old-matnr BINARY SEARCH.
          IF sy-subrc = 0.
**********check invoice material price & new material price same or not***********
            CLEAR : lv_matnr,lv_amt,lv_oldamt.
            ls_vbrp-netwr = ls_vbrp-netwr / ls_vbrp-fkimg.
            lv_amt        = ls_vbrp-netwr.
          ENDIF.
        ENDIF.

        READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY matnr = <fs_lips>-matnr
                                                       werks = lv_werks BINARY SEARCH.
        IF sy-subrc NE 0.
          lv_msg = | Material - { <fs_lips>-matnr } not maintained in plant - { lv_werks } ; { lv_msg }|.
          CONTINUE.
        ENDIF.
***********check plant , b.area & division material level**********
        READ TABLE lt_area TRANSPORTING NO FIELDS WITH KEY werks = lv_werks
                                                           spart = ls_mard-spart BINARY SEARCH.
        IF sy-subrc NE 0.
          lv_msg = | Material - { <fs_lips>-matnr } Division not assigned to business area - { lv_werks } ; { lv_msg }|.
          CONTINUE.
        ENDIF.
*******************get material old price******************
        READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY matnr     = <fs_lips>-matnr
                                                       bwkey     = lv_werks BINARY SEARCH.
*******check old price std or avrg price.
        IF ls_mbew-vprsv = 'S'.
          lv_oldamt = ls_mbew-stprs.
*        ELSEIF ls_mbew-vprsv = 'V'.
*          lv_oldamt = ls_mbew-verpr.
        ENDIF.

        lv_matnr      = <fs_lips>-matnr.
**********call the price change method*********
        IF lv_amt NE lv_oldamt.
          price_change(
                        EXPORTING
                          material = lv_matnr             " Material Number (18 Characters)
                          plant    = lv_werks             " Valuation area
                          date     = sy-datum             " Date for Pricing and Exchange Rate
                          amount   = lv_amt               " Price in BAPI currency format
                        IMPORTING
                          message  = DATA(lv_message)
                          type     = lv_type               " Message type: S Success, E Error, W Warning, I Info, A Abort
          ).
          IF lv_type = 'E'.
            lv_msg = | Material - { <fs_lips>-matnr } { lv_message }; { lv_msg }|.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDAT.

*******read the already exist batch qty**********
      READ TABLE lt_mchb INTO DATA(ls_mchb) WITH KEY matnr = <fs_lips>-matnr
                                                     werks = lv_werks
                                                     lgort = ls_mard-lgort
                                                     charg = <fs_lips>-charg BINARY SEARCH.
      IF ls_mchb IS INITIAL.
*if batch not found means create new batch
        batch_create(
          EXPORTING
            material   = lv_matnr           " Material Number
            plant      = lv_werks           " Plant Table for National (Centrally Agreed) Contracts
            batch      = <fs_lips>-charg    " Transfer Table to SAPLMBIN for Batch/Stock Selection
            storageloc = ls_mard-lgort      " Material Master View on Storage Location and Batch Selection
          IMPORTING
            message    = lv_message
            type       = lv_type ).         " Message type: S Success, E Error, W Warning, I Info, A Abort
        IF lv_type = 'E'.
          lv_msg = | Material - { <fs_lips>-matnr } { lv_message }; { lv_msg }|.
          CONTINUE.
*        ELSEIF lv_type = 'S'.
***************read the old batch date*************
*          READ TABLE lt_mchb INTO DATA(ls_mchb2) WITH KEY matnr = <fs_lips>-matnr
*                                                          werks = <fs_lips>-werks
*                                                          lgort = <fs_lips>-lgort
*                                                          charg = <fs_lips>-charg BINARY SEARCH.
*          IF sy-subrc = 0.
*            batch_change(
*              EXPORTING
*                material   = lv_matnr          " Material Number (18 Characters)
*                plant      = lv_werks          " Plant
*                batch      = <fs_lips>-charg   " Batch Number
*                storageloc = ls_mard-lgort     " Storage location
*                created_on = ls_mchb2-ersda    " Created On
*              IMPORTING
*                message    = lv_message
*                type       = lv_type   " Message type: S Success, E Error, W Warning, I Info, A Abort
*            ).
*            CLEAR ls_mchb2.
*            IF lv_type = 'E'.
*              lv_msg = | Material - { <fs_lips>-matnr } { lv_message }; { lv_msg }|.
*              CONTINUE.
*            ENDIF.
*          ENDIF.
        ENDIF.
      ENDIF.
*item data filling ( material )
      APPEND VALUE bapi_physinv_create_items( material = <fs_lips>-matnr
                                              batch    = <fs_lips>-charg ) TO lt_create.
      lv_item = 1 + lv_item.
*for sales order  and cancelled invoice
      IF ls_vbrk-fkart = 'YBBR' OR ls_vbrk-fkart = 'YBRC'.
*calculation for total qty
        lv_total = <fs_lips>-lfimg + ls_mchb-clabs.
        lv_new   = <fs_lips>-lfimg.
        APPEND VALUE bapi_physinv_count_items( material  = <fs_lips>-matnr
                                               batch     = <fs_lips>-charg
                                               entry_qnt = ls_mchb-clabs + <fs_lips>-lfimg
                                               item      = lv_item
                                               entry_uom = <fs_lips>-meins ) TO lt_count.
*for sales return and cancelled invoice
      ELSEIF ls_vbrk-fkart = 'YBRE' OR ls_vbrk-fkart = 'YBBC'.
        IF ls_mchb-clabs LT <fs_lips>-lfimg.
          lv_msg = | Return Material - { <fs_lips>-matnr } quantity is greater than the available stock in batch - { <fs_lips>-charg } ; { lv_msg }|.
          CONTINUE.
        ENDIF.
*    check zero qty
        IF ls_mchb-clabs - <fs_lips>-lfimg = 0.
          DATA(lv_zero) = abap_true.
        ENDIF.
*calculation for total qty
        lv_total = ls_mchb-clabs - <fs_lips>-lfimg.
        lv_new   = - <fs_lips>-lfimg.
        APPEND VALUE bapi_physinv_count_items( material  = <fs_lips>-matnr
                                               batch     = <fs_lips>-charg
                                               entry_qnt = ls_mchb-clabs - <fs_lips>-lfimg
                                               item      = lv_item
                                               entry_uom = <fs_lips>-meins
                                               zero_count = lv_zero ) TO lt_count.
      ENDIF.
*store the item logs into zdms_grn_stock.
      APPEND VALUE #( distributor  = ls_vbrk-kunag
                      source_plant = <fs_lips>-werks
                      dist_plant   = lv_werks
                      vbeln        = ls_input-invoiceno
                      invoice_date = ls_input-invoicedate
                      invoice_type = ls_vbrk-fkart
                      matnr        = <fs_lips>-matnr
                      charg        = <fs_lips>-charg
                      fkimg        = <fs_lips>-lfimg
                      meins        = <fs_lips>-meins
                      old_price    = lv_oldamt
                      new_price    = lv_amt
                      erdat        = sy-datum
                      ext_qty      = ls_mchb-clabs
                      total_qty    = lv_total
                      new_qty      = lv_new ) TO lt_stock.
      DATA(lv_source) = <fs_lips>-werks.
      CLEAR : ls_mchb,lv_zero,lv_type,ls_mbew,ls_vbrp,lv_new,lv_total,lv_message.
    ENDLOOP.
    CLEAR : lv_type.
**********check if there any error message***********
    IF lv_msg IS INITIAL.
      DATA(ls_header) = VALUE bapi_physinv_create_head( plant        = lv_werks
                                                        stge_loc     = ls_mard-lgort
                                                        phys_inv_ref = ls_input-invoiceno ).
**********call the physical inventory bapi**********************
      physical_bapi(
        EXPORTING
          ls_header     = ls_header        " BAPI Communication Structure: Create Phys. Inv. Doc., Header
          lt_itemcount  = lt_count         " BAPI Communication Structure: Count Data for Item
          lt_itemcreate = lt_create        " BAPI Communication Structure: Create Phys. Inv. Doc., Items
        IMPORTING
          type          = lv_type          " Message type: S Success, E Error, W Warning, I Info, A Abort
          message       = lv_msg
          belnr         = DATA(lv_belnr) )." Physical Inventory Document
**************store the logs into zdms_grn_header.*****************
      IF lv_type = 'S'.
        DATA(ls_head) = VALUE zdms_grn_header( distributor  = ls_input-distributor
                                               dist_plant   = lv_werks
                                               source_plant = lv_source
                                               vbeln        = ls_input-invoiceno
                                               invoice_date = ls_input-invoicedate
                                               invoice_type = ls_vbrk-fkart
                                               belnr        = lv_belnr
                                               erdat        = sy-datum
                                               time         = sy-uzeit
                                               ernam        = sy-uname
                                               remarks      = lv_msg ).
        lv_msg = 'GRN Completed'.
        MODIFY zdms_grn_header FROM ls_head.
        MODIFY zdms_grn_stock  FROM TABLE lt_stock.
        CLEAR : ls_vbrk ,lv_belnr,ls_head.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
