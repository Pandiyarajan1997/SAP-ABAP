CLASS zcl_api_alpn_grn_phys_inven DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    TYPES:
**********item structure************
      BEGIN OF ty_item,
        material      TYPE matnr18,
        quantity      TYPE menge_d,
        uom           TYPE meins,
        batch         TYPE charg_d,
        per_unitprice TYPE netpr,
      END OF ty_item .
    TYPES:
      tt_item TYPE STANDARD TABLE OF ty_item WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
******************input json*************
      BEGIN OF ty_input,
        mis_id         TYPE  zorder_id,
        comcode        TYPE  bukrs,
        franchise_code TYPE  kunag,
        plant          TYPE  werks_d,
        storage_loc    TYPE  lgort_d,
        item           TYPE  tt_item,
      END OF ty_input .

    METHODS validation_process
      IMPORTING
        VALUE(ls_input)     TYPE ty_input OPTIONAL
        !double_click_check TYPE char1 DEFAULT abap_true
      EXPORTING
        !lv_msg             TYPE string
        !lv_type            TYPE bapi_mtype .
    METHODS physical_bapi
      IMPORTING
        !comp_code           TYPE bukrs
        VALUE(ls_header)     TYPE bapi_physinv_create_head
        VALUE(lt_itemcount)  TYPE zbapi_tt_physinv_count_items
        VALUE(lt_itemcreate) TYPE zbapi_tt_physinv_create_items
      EXPORTING
        !type                TYPE bapi_mtype
        !message             TYPE string
        !belnr               TYPE iblnr
        !year                TYPE mjahr .
    METHODS batch_create
      IMPORTING
        VALUE(material)   TYPE matnr18
        VALUE(plant)      TYPE werks_d
        VALUE(batch)      TYPE charg_d
        VALUE(storageloc) TYPE lgort_d
      EXPORTING
        !message          TYPE string
        !type             TYPE bapi_mtype .
    METHODS price_change
      IMPORTING
        !bukrs          TYPE bukrs
        VALUE(material) TYPE bapi_matval_key-material
        VALUE(plant)    TYPE bapi_matval_key-val_area
        VALUE(date)     TYPE prsdt
        VALUE(amount)   TYPE bapi_price
      EXPORTING
        !message        TYPE string
        !type           TYPE bapi_mtype .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_ALPN_GRN_PHYS_INVEN IMPLEMENTATION.


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
*&Created On: 09.07.2024
*&Purpose   : direct physical inventory for franchise
*&Reference : Ramakrishnan J
*hardcoded  : strg loc = D1
*-------------------------------------------------------------------------------------
******************output json*************
    TYPES : BEGIN OF ty_output,
              comcode        TYPE bukrs,
              franchise_code TYPE kunag,
              mis_id         TYPE zorder_id,
              status         TYPE bapi_mtype,
              message	       TYPE string,
            END OF ty_output.

******************output json - Farm*************
    TYPES : BEGIN OF ty_farmout,
              comcode TYPE bukrs,
              plant   TYPE werks_d,
              mis_id  TYPE zorder_id,
              status  TYPE bapi_mtype,
              message	TYPE string,
            END OF ty_farmout.

    DATA : lv_data  TYPE string,
           lv_body  TYPE string,
           lv_msg   TYPE string,
           lv_type  TYPE bapi_mtype,
           ls_input TYPE ty_input.
*    CALL METHOD send_response.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.     "data dec for store api log table
    CREATE OBJECT lo_log_upd.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.
*    "Input of API Request
    CLEAR : lv_data,lv_body,lv_msg,lv_body.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
      EXPORTING
        json         = lv_data
        pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data         = ls_input ).

    IF ls_input-franchise_code IS NOT INITIAL.
      ls_input-franchise_code = |{ ls_input-franchise_code ALPHA = IN }|.
    ENDIF.
*method contains validation & process
    CLEAR : lv_msg,lv_type.

    CALL METHOD validation_process
      EXPORTING
        ls_input = ls_input
      IMPORTING
        lv_msg   = lv_msg
        lv_type  = lv_type.

***********fill the response**************
    IF lv_type IS INITIAL.
      lv_type = 'E'.
    ENDIF.

**********send the data to mis & store logs***********
*for plant fetching
    IF ls_input-plant IS INITIAL.
      SELECT SINGLE werks FROM kna1 INTO @ls_input-plant
                                    WHERE kunnr = @ls_input-franchise_code.
    ENDIF.
*********************error log***********
    IF lv_type = 'E'.
      lo_errorlog_dms->log_entry_store(
        EXPORTING
          type                = 27
          status              = 10
          dms_orderid         = ls_input-mis_id
          distributor         = ls_input-franchise_code
          plant               = ls_input-plant
          msg                 = lv_msg ).
    ENDIF.
** serialize the output for response based on the company code***
    IF ls_input-comcode <> '3500'.

      DATA(ls_output) =  VALUE ty_output( comcode        = ls_input-comcode
                                          mis_id         = ls_input-mis_id
                                          franchise_code = ls_input-franchise_code
                                          status         = lv_type
                                          message        = lv_msg ).
      CLEAR : lv_body.
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  ls_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

    ELSE.

      DATA(ls_farmout) =  VALUE ty_farmout( comcode        = ls_input-comcode
                                            mis_id         = ls_input-mis_id
                                            plant          = ls_input-plant
                                            status         = lv_type
                                            message        = lv_msg ).
      CLEAR : lv_body.
      /ui2/cl_json=>serialize(
      EXPORTING
       data         = ls_farmout
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

    ENDIF.

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'ALPN_GRN_PHYS'
        ijson           = lv_data
        ojson           = lv_body
        distributor     = ls_input-franchise_code
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.

    DELETE FROM zsd_invoice_dms WHERE invoice_no = ls_input-mis_id.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = lv_body ).

  ENDMETHOD.


  METHOD physical_bapi.
    DATA : lt_return TYPE TABLE OF bapiret2,
           lv_iblnr  TYPE ikpf-iblnr.
    DATA : lv_gjahr TYPE gjahr.
****************get fiscal year*******************
    CALL FUNCTION 'GET_CURRENT_YEAR'
      EXPORTING
        bukrs = comp_code
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
*          READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
          LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
            message = | { message } ; { ls_return-message }|.
          ENDLOOP.
          ls_return-message = message.
        ELSE.""""success doc posted
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          belnr   = ls_return-message_v2.
          year    = lv_gjahr.
        ENDIF.
      ELSE.
        READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
      ENDIF.
    ENDIF.
    message = ls_return-message.
    type    = ls_return-type.
    IF type = 'A'.
      type = 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD price_change.
    DATA : lt_prices     TYPE TABLE OF  bapi_matval_prices,
           lt_return     TYPE TABLE OF bapiret2,
           ls_prices     TYPE  bapi_matval_prices,
           valuationtype TYPE  bapi_matval_key-val_type,
           pricedate     TYPE  bapi_matval_pricedate,
           lv_period     TYPE  bkpf-monat.

    CLEAR : ls_prices,pricedate.

    CALL FUNCTION 'FI_PERIOD_DETERMINE'
      EXPORTING
        i_budat = date
        i_bukrs = bukrs
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


  METHOD validation_process.

    DATA : lt_create TYPE STANDARD TABLE OF  bapi_physinv_create_items,   "for physical inventory bapi
           lt_count  TYPE TABLE OF bapi_physinv_count_items.
    DATA : lt_stock  TYPE TABLE OF  zalpn_grn_phys. "for data insert custom log table dms
    DATA : lv_matnr     TYPE matnr18,
           lv_item      TYPE posnr,
           lv_matnr_chk TYPE matnr,
           lv_batch     TYPE charg_d,
           lv_totqty    TYPE labst,
           lv_extqty    TYPE labst,
           lv_amnt      TYPE bapi_price.

    CONSTANTS : batch_no TYPE num06 VALUE '000001'.

    DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
    CREATE OBJECT lo_common_check.
    CLEAR : lv_msg,lv_type.
    IF ls_input IS INITIAL.
      lv_msg  = | Please fill the data |.
      EXIT.
    ENDIF.
*Multiple click on front end so avoiding duplicates
    IF double_click_check = abap_true.
      SELECT SINGLE * FROM zsd_invoice_dms INTO @DATA(l_dupl_ordid)
                                           WHERE invoice_no = @ls_input-mis_id.
      IF sy-subrc = 0.
        lv_msg = | Already In Processing { ls_input-mis_id }; { lv_msg }|.
        EXIT.
      ELSE.
        DATA(l_orderid_chk) = VALUE zsd_invoice_dms( mandt      = sy-mandt
                                                     invoice_no = ls_input-mis_id ).
        INSERT zsd_invoice_dms FROM l_orderid_chk.
      ENDIF.
    ENDIF.

*****************check the grn completed or not*************
    SELECT SINGLE * FROM zalpn_grn_phys INTO @DATA(ls_grn)
                                        WHERE mis_id = @ls_input-mis_id
                                        AND   type   = 'S'.
    IF sy-subrc = 0.
      lv_msg  = 'GRN Already Completed'.
      lv_type = 'S'.
      EXIT.
    ENDIF.

    IF ls_input-comcode <> '3500'.          "not for gibbon farm
*for franchise plant fetching
      SELECT SINGLE werks FROM kna1 INTO @ls_input-plant
                                    WHERE kunnr = @ls_input-franchise_code.
      IF ls_input-plant IS INITIAL.
        lv_msg = | Franchise code { ls_input-franchise_code } plant not maintained in Customer Master ; { lv_msg }|.
        EXIT.
      ENDIF.
    ENDIF.

***********************check the storage location*************
    IF ls_input-storage_loc IS INITIAL.
      lv_msg = |Storage location is missing|.
      EXIT.
    ENDIF.

************check plant & business area********
*    SELECT werks,spart FROM t134g INTO TABLE @DATA(lt_area) WHERE werks = @ls_input-plant
*                                                            AND   gsber = @ls_input-plant.
*    IF sy-subrc NE 0.
*      lv_msg = | Plant - { ls_input-plant } & business area Mismatch; { lv_msg }|.
*      EXIT.
*    ELSE.
*      SORT lt_area BY werks spart.
*    ENDIF.
************check plant & valuation area********
*    SELECT SINGLE bwkey FROM t134h INTO @DATA(ls_area) WHERE bwkey = @ls_input-plant
*                                                       AND   gsber = @ls_input-plant.
*    IF sy-subrc NE 0.
*      lv_msg = | Plant - { ls_input-plant } & Valuation area Mismatch; { lv_msg }|.
*      EXIT.
*    ENDIF.
** checking plant & company code
    SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(t001k) WHERE bwkey = @ls_input-plant
                                                          AND   bukrs = @ls_input-comcode.
    IF sy-subrc NE 0.
      lv_msg = | Plant { ls_input-plant } not maintained in - { ls_input-comcode }  ; { lv_msg }|.
      EXIT.
    ENDIF.
***************************FI period check*******************
    lo_common_check->fi_period_check(
      EXPORTING
        posting_date =  sy-datum          " Field of type DATS
        acct_type    =  'D'               " Account type
        com_code     =  ls_input-comcode   " Company Code
      IMPORTING
        type         =  DATA(lv_msgtype)
      EXCEPTIONS
        mandatory    = 1
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
        com_code  = ls_input-comcode               " Company Code
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

** fetch the stock details
    SELECT matnr,werks,lgort,charg,clabs,ersda FROM mchb
                                               INTO TABLE @DATA(lt_mchb)
                                               WHERE werks = @ls_input-plant.
* fetch the material details
    SELECT a~matnr,a~werks,a~lgort,a~labst,b~spart,c~xchpf FROM mard AS a
                                           INNER JOIN mara AS b ON a~matnr = b~matnr
                                           INNER JOIN marc AS c ON a~matnr = c~matnr
                                           AND  a~werks = c~werks
                                           INTO TABLE @DATA(lt_mard)
                                           WHERE a~werks = @ls_input-plant.
****************fetch the standard price for the material*************
    SELECT matnr,bwkey,vprsv,verpr,stprs FROM mbew
                                         INTO TABLE @DATA(lt_mbew)
                                         WHERE  bwkey = @ls_input-plant.

******************batch number series only for ALPN**********
    CLEAR : lv_batch.

    lv_batch = |{ ls_input-plant }{ batch_no }|.

*********************material conversion**************
    LOOP AT ls_input-item ASSIGNING FIELD-SYMBOL(<fs_item>).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fs_item>-material
        IMPORTING
          output = <fs_item>-material.

    ENDLOOP.

*******************
*sort the internal table for binary search.
    SORT  : lt_mard BY werks lgort matnr,
            lt_mbew BY matnr bwkey,
            lt_mchb BY matnr werks lgort charg,
            ls_input-item BY material.
**********loop process & bapi data filling******************
    LOOP AT ls_input-item ASSIGNING <fs_item>.

      CLEAR : lv_matnr_chk,lv_totqty,lv_extqty.
************check qty zero**********
      IF <fs_item>-quantity IS INITIAL.
        lv_msg = | Material - { <fs_item>-material } qty is zero ; { lv_msg }|.
        CONTINUE.
      ENDIF.

************check UOM**********
      IF <fs_item>-uom IS INITIAL.
        lv_msg = |Material - { <fs_item>-material } UOM is Missing ; { lv_msg }|.
        CONTINUE.
      ENDIF.

************check UOM**********
      IF <fs_item>-per_unitprice IS INITIAL.
        lv_msg = |Material - { <fs_item>-material } Per Unit price is Missing ; { lv_msg }|.
        CONTINUE.
      ENDIF.

      AT NEW material.
        lv_matnr_chk = <fs_item>-material.

*******************get material price******************
        READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY matnr     = <fs_item>-material
                                                       bwkey     = ls_input-plant BINARY SEARCH.
        IF ls_mbew-stprs <> <fs_item>-per_unitprice.
          CLEAR : lv_amnt.
          lv_amnt = <fs_item>-per_unitprice.
*****************update the VPRS price material wise***************
          CALL METHOD price_change
            EXPORTING
              bukrs    = ls_input-comcode         " Company Code
              material = <fs_item>-material       " Material Number (18 Characters)
              plant    = ls_input-plant           " Valuation area
              date     = sy-datum                 " Date for Pricing and Exchange Rate
              amount   = lv_amnt                  " Price in BAPI currency format
            IMPORTING
              message  = lv_msg
              type     = lv_type.
          IF lv_type <> 'S'.
            EXIT.
          ELSE.
            CLEAR : lv_msg.
          ENDIF.
        ENDIF.
      ENDAT.

************check dublicate material***********
      IF <fs_item>-material NE lv_matnr_chk.
        lv_msg  = |{ lv_msg } ,Given Material - { <fs_item>-material } repeated more than once|.
        CONTINUE.
      ENDIF.

**************************ONLY for franchise and ALPN*****************
      IF ls_input-comcode = 'ALPN' OR ls_input-comcode = 'F001'
        OR ls_input-comcode = 'F002'.
        <fs_item>-batch = lv_batch.
      ENDIF.

      READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY werks = ls_input-plant
                                                     lgort = ls_input-storage_loc
                                                     matnr = <fs_item>-material BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = | Material - { <fs_item>-material } not maintained in plant - { ls_input-plant } - s.loc { ls_input-storage_loc } ; { lv_msg }|.
        CONTINUE.
      ELSEIF sy-subrc = 0 AND ls_mard-xchpf = abap_false.          "check the material is batch management config or not
        CLEAR : <fs_item>-batch.
        lv_totqty = <fs_item>-quantity + ls_mard-labst.
        lv_extqty = ls_mard-labst.
      ENDIF.
*******read the already exist batch qty**********
      READ TABLE lt_mchb INTO DATA(ls_mchb) WITH KEY matnr = <fs_item>-material
                                                     werks = ls_input-plant
                                                     lgort = ls_input-storage_loc
                                                     charg = <fs_item>-batch BINARY SEARCH.

      IF ls_mchb IS INITIAL AND ls_mard-xchpf = abap_true.

        IF <fs_item>-batch IS INITIAL.
          lv_msg = | Material - { <fs_item>-material } - Batch is Missing ; { lv_msg }|.
          CONTINUE.
        ENDIF.

        CLEAR : lv_matnr.
        lv_matnr = <fs_item>-material.
*if batch not found means create new batch
        batch_create(
          EXPORTING
            material   = lv_matnr           " Material Number
            plant      = ls_input-plant           " Plant Table for National (Centrally Agreed) Contracts
            batch      = <fs_item>-batch           " Transfer Table to SAPLMBIN for Batch/Stock Selection
            storageloc = ls_input-storage_loc      " Material Master View on Storage Location and Batch Selection
          IMPORTING
            message    = lv_msg
            type       = lv_type ).         " Message type: S Success, E Error, W Warning, I Info, A Abort

        IF lv_type = 'S'.
          CLEAR : lv_msg.
          lv_totqty = <fs_item>-quantity.
        ELSE.
          lv_msg = | Material - { <fs_item>-material } { lv_msg }|.
          CONTINUE.
        ENDIF.

      ELSEIF ls_mchb IS NOT INITIAL.
        lv_totqty = <fs_item>-quantity + ls_mchb-clabs.
        lv_extqty = ls_mchb-clabs.
      ENDIF.
*item data filling ( material )
      APPEND VALUE bapi_physinv_create_items( material = <fs_item>-material
                                              batch    = <fs_item>-batch ) TO lt_create.
      lv_item = 1 + lv_item.
*calculation for total qty
      APPEND VALUE bapi_physinv_count_items( material  = <fs_item>-material
                                             batch     = <fs_item>-batch
                                             entry_qnt = lv_totqty
                                             item      = lv_item
                                             entry_uom = <fs_item>-uom ) TO lt_count.
*store the item logs into ZALPN_GRN_PHYS.
      APPEND VALUE #( mis_id       = ls_input-mis_id
                      comp_code    = ls_input-comcode
                      franchise_no = ls_input-franchise_code
                      plant        = ls_input-plant
                      storage_loc  = ls_input-storage_loc
                      material     = <fs_item>-material
                      batch        = <fs_item>-batch
                      quantity     = <fs_item>-quantity
                      ext_qty      = lv_extqty
                      total_qty    = lv_totqty
                      uom          = <fs_item>-uom
                      erdat        = sy-datum
                      ernam        = sy-uname
                      time         = sy-uzeit ) TO lt_stock.

      CLEAR : ls_mchb,ls_mard,ls_mbew.
    ENDLOOP.
**********check if there any error message***********
    IF lv_msg IS NOT INITIAL.
      lv_type = 'E'.
      EXIT.
    ENDIF.

    DATA(ls_header) = VALUE bapi_physinv_create_head( plant        = ls_input-plant
                                                      stge_loc     = ls_input-storage_loc
                                                      phys_inv_ref = ls_input-mis_id ).
**********call the physical inventory bapi**********************
    physical_bapi(
      EXPORTING
        comp_code     = ls_input-comcode
        ls_header     = ls_header        " BAPI Communication Structure: Create Phys. Inv. Doc., Header
        lt_itemcount  = lt_count         " BAPI Communication Structure: Count Data for Item
        lt_itemcreate = lt_create        " BAPI Communication Structure: Create Phys. Inv. Doc., Items
      IMPORTING
        type          = lv_type          " Message type: S Success, E Error, W Warning, I Info, A Abort
        message       = lv_msg
        belnr         = DATA(lv_belnr) " Physical Inventory Document
        year          = DATA(lv_year) ).
**************store the logs into zdms_grn_header.*****************
    IF lv_type = 'S'.

      DATA(ls_stock) = VALUE zalpn_grn_phys( type = 'S'        remarks = lv_msg
                                             mblnr = lv_belnr  mjahr   = lv_year ).

      MODIFY lt_stock FROM ls_stock TRANSPORTING type remarks mblnr mjahr
                                    WHERE mis_id = ls_input-mis_id.
      MODIFY zalpn_grn_phys FROM TABLE lt_stock.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      lv_msg = 'GRN Completed'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
