class ZCL_API_FARM_GI_COSTCENTER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_FARM_GI_COSTCENTER IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Pandiarajan
*&Created On: 22.03.2024
*&Purpose : API for farm Goods issue for cost center
*&Reference: Ramakrishnan J & gopal
*-------------------------------------------------------------------------------------
**********item structure************
    TYPES: BEGIN OF ty_item,
             material TYPE matnr18,
             qty      TYPE menge_d,
           END OF ty_item.
    DATA : lt_item TYPE TABLE OF ty_item.
**********input response structure************
    TYPES: BEGIN OF ty_input,
             ref_no       TYPE zorder_id,
             compcode     TYPE bukrs,
             work_center  TYPE arbpl,
             posting_date TYPE budat,
             item         LIKE lt_item,
           END OF ty_input.
**********output response structure************
    TYPES: BEGIN OF ty_output,
             ref_no       TYPE zorder_id,
             compcode     TYPE bukrs,
             work_center  TYPE arbpl,
             posting_date TYPE budat,
             doc_no       TYPE mblnr,
             fisc_year    TYPE char4,
             type         TYPE bapi_mtype,
             message      TYPE string,
           END OF ty_output.
    DATA: gs_output TYPE ty_output. "response
    DATA: gs_input TYPE ty_input.     "input
    DATA: lv_msg       TYPE string,
          lv_posnr     TYPE posnr,
          lv_type      TYPE bapi_mtype,
          lv_index     TYPE sy-tabix,
          lv_matnr     TYPE matnr18,
          lv_matnr_chk TYPE matnr18,
          lv_check     TYPE char1.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    DATA : lt_log TYPE TABLE OF zfi_farm_gi_cost,
           ls_log TYPE zfi_farm_gi_cost.

    DATA: lw_goodsmvt_header TYPE bapi2017_gm_head_01,
          lt_goodsmvt_item   TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          ls_goodsmvt_item   TYPE bapi2017_gm_item_create,
          lt_return          TYPE STANDARD TABLE OF bapiret2.
    DATA: lw_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          goodsmvt_code_tmp   TYPE bapi2017_gm_code.

    "Input of API Request
    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gs_input ).

    IF gs_input IS NOT INITIAL.
      CLEAR : lv_msg,lv_type,ls_goodsmvt_item,ls_log.
*******************date check*****************
      IF gs_input-posting_date IS INITIAL.
        lv_msg = |Posting date is missing|.
      ENDIF.
************dublicate check***********
      SELECT SINGLE mblnr mjahr FROM zfi_farm_gi_cost INTO lw_goodsmvt_headret
                WHERE ref_no = gs_input-ref_no
                AND   bukrs  = gs_input-compcode
                AND   type = 'S'.
      IF sy-subrc EQ 0.
        lv_msg = |Document already Created based on same reference - { gs_input-ref_no }|.
      ENDIF.
      SELECT SINGLE mblnr mjahr FROM mkpf INTO lw_goodsmvt_headret
                                WHERE xblnr = gs_input-ref_no
                                AND   bktxt = gs_input-ref_no.
      IF sy-subrc EQ 0.
        lv_msg = |Document already Created based on same reference - { gs_input-ref_no } |.
      ENDIF.
*Reference Document Number
      IF gs_input-ref_no IS INITIAL.
        lv_msg = |{ lv_msg },Reference Number is Mandatory|.
      ELSE.
      ENDIF.
*Company Code
      SELECT SINGLE bukrs FROM t001 INTO @DATA(lv_bukrs)
        WHERE bukrs = @gs_input-compcode.
      IF sy-subrc NE 0.
        lv_msg = |{ lv_msg },Incorrect Company Code|.
      ENDIF.
*Deriving the Costcenter Based on Work Center in Input JSON
      SELECT SINGLE * FROM crhd INTO @DATA(l_crhd)
        WHERE objty = 'A' AND arbpl = @gs_input-work_center.
      IF sy-subrc = 0.
        DATA(lv_plant) = l_crhd-werks.
        SELECT SINGLE kostl FROM crco INTO @DATA(lv_costcenter)
          WHERE objty = 'A' AND objid = @l_crhd-objid.
      ELSE.
        lv_msg  = |{ lv_msg },Work Center is Incorrect|.
      ENDIF.
***********fill for log table*************
      ls_log-mandt    = sy-mandt.
      ls_log-bukrs    = gs_input-compcode.
      ls_log-ref_no   = gs_input-ref_no.
      ls_log-werks    = lv_plant.
      ls_log-kostl    = lv_costcenter.
      ls_log-arbpl    = gs_input-work_center.
      ls_log-erdat    = sy-datum.
      ls_log-ernam    = sy-uname.
      ls_log-erzet    = sy-uzeit.
*Error Message Maintainence
      IF lv_msg IS NOT INITIAL.
        lv_type = 'E'.
      ELSE.
****************fetch the material wise stock details*************
        SELECT matnr,lgort,labst FROM mard INTO TABLE @DATA(lt_mard) WHERE werks = @lv_plant.
        IF sy-subrc = 0.
          SORT : lt_mard BY matnr labst DESCENDING.
        ENDIF.
****************fetch the material batch wise stock details*************
        SELECT matnr,werks,lgort,charg,clabs,ersda FROM mchb INTO TABLE @DATA(lt_mchb) WHERE werks = @lv_plant
                                                                                       AND   clabs NE @abap_false.
        IF sy-subrc = 0.
          SORT : lt_mchb BY matnr werks lgort ersda clabs DESCENDING.
        ENDIF.

        SORT : gs_input-item[] BY material.

        LOOP AT gs_input-item[] ASSIGNING FIELD-SYMBOL(<fs_item>).
          CLEAR : lv_matnr,lv_matnr_chk.
************alpha INPUT conversion*****************
          lv_matnr = |{ <fs_item>-material ALPHA = IN }|.
          AT NEW material.
            lv_matnr_chk = lv_matnr.
*****************fill the item details for LOG table**********
            ls_log-material = lv_matnr.
          ENDAT.
************check dublicate material***********
          IF lv_matnr NE lv_matnr_chk.
            lv_msg  = |{ lv_msg } ,Given Material - { <fs_item>-material } repeated more than once|.
            CONTINUE.
          ENDIF.
*********************get the material availability**************
          READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY matnr = lv_matnr BINARY SEARCH.
          IF sy-subrc NE 0.
            lv_msg  = |{ lv_msg } , Material - { <fs_item>-material } not maintained in plant - { lv_plant } |.
            CONTINUE.
          ELSE.
*********************check the material availability**************
            IF ls_mard-labst LT <fs_item>-qty.
              lv_msg  = |{ lv_msg },Given Material - { <fs_item>-material } quantity exceed the current stock|.
              CONTINUE.
            ENDIF.
*****************fill the item details for bapi - batch stock**********
            ls_goodsmvt_item-move_type  = '201'.
            ls_goodsmvt_item-plant      = lv_plant.
            ls_goodsmvt_item-costcenter = lv_costcenter.
            ls_goodsmvt_item-stge_loc   = ls_mard-lgort.
            ls_goodsmvt_item-material   = lv_matnr.
            ls_goodsmvt_item-item_text  = gs_input-work_center.
****************Batch split based on material qty*************
            DO.
              READ TABLE lt_mchb INTO DATA(ls_mchb) WITH KEY matnr = lv_matnr
                                                             werks = lv_plant
                                                             lgort = ls_mard-lgort BINARY SEARCH.
*****************Get the internal table index no************
              lv_index = sy-tabix.
              IF ls_mchb-clabs LT <fs_item>-qty AND ls_mchb IS NOT INITIAL.
                <fs_item>-qty = <fs_item>-qty - ls_mchb-clabs.
                DELETE lt_mchb INDEX lv_index.
*****************fill the item details for bapi - batch stock**********
                ls_goodsmvt_item-batch      = ls_mchb-charg.
                ls_goodsmvt_item-entry_qnt  = ls_mchb-clabs.
                APPEND ls_goodsmvt_item TO lt_goodsmvt_item.
*****************fill the item details for log table**********
                ls_log-posnr    = ls_log-posnr + 1.
                ls_log-batch    = ls_mchb-charg.
                ls_log-qty      = ls_mchb-clabs.
                APPEND ls_log TO lt_log.
              ELSE.
*****************fill the item details for bapi - given quantity**********
                ls_goodsmvt_item-batch      = ls_mchb-charg.
                ls_goodsmvt_item-entry_qnt  = <fs_item>-qty.
                APPEND ls_goodsmvt_item TO lt_goodsmvt_item.
*****************fill the item details for log table**********
                ls_log-posnr = ls_log-posnr + 1.
                ls_log-batch = ls_mchb-charg.
                ls_log-qty   = <fs_item>-qty.
                APPEND ls_log TO lt_log.
                EXIT.
              ENDIF.
              CLEAR : ls_mchb,lv_index.
            ENDDO.
            CLEAR : ls_mard,ls_mchb,lv_index.
          ENDIF.
        ENDLOOP.

        IF lv_msg IS NOT INITIAL.
          lv_type = 'E'.
        ELSE.
*************fill header details**************
          lw_goodsmvt_header-pstng_date = gs_input-posting_date.
          lw_goodsmvt_header-doc_date   = gs_input-posting_date.
          lw_goodsmvt_header-header_txt = gs_input-ref_no.
          lw_goodsmvt_header-ref_doc_no = gs_input-ref_no.
          goodsmvt_code_tmp-gm_code     = '03'.

************Call the goods issue BAPI*************
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
            EXPORTING
              goodsmvt_header  = lw_goodsmvt_header
              goodsmvt_code    = goodsmvt_code_tmp
            IMPORTING
              goodsmvt_headret = lw_goodsmvt_headret
            TABLES
              goodsmvt_item    = lt_goodsmvt_item
              return           = lt_return.

**********check the bapi return table***********
          IF lw_goodsmvt_headret IS NOT INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            lv_type = 'S'.
            lv_msg  = |Goods issued successfully - { lw_goodsmvt_headret-mat_doc } |.
          ELSE.
            LOOP AT lt_return INTO DATA(ls_return).
              lv_msg = |{ ls_return-message },{ lv_msg }|.
            ENDLOOP.
            lv_type = 'E'.
          ENDIF.
**************pass the LOG table data**************
          CLEAR: ls_log.
          ls_log-type    = lv_type.
          ls_log-message = lv_msg.
          ls_log-mblnr   = lw_goodsmvt_headret-mat_doc.
          ls_log-mjahr   = lw_goodsmvt_headret-doc_year.
          MODIFY lt_log FROM ls_log TRANSPORTING type message mblnr mjahr WHERE ref_no = gs_input-ref_no.
          MODIFY zfi_farm_gi_cost FROM TABLE lt_log.
        ENDIF.
      ENDIF.

**********fill the output response*************
      gs_output-compcode    = gs_input-compcode.
      gs_output-work_center = gs_input-work_center.
      gs_output-ref_no      = gs_input-ref_no.
      gs_output-posting_date = gs_input-posting_date.
      gs_output-type        = lv_type.
      gs_output-message     = lv_msg.
      gs_output-doc_no      = lw_goodsmvt_headret-mat_doc.
      gs_output-fisc_year   = lw_goodsmvt_headret-doc_year.

      IF gs_output IS NOT INITIAL.
        CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
        /ui2/cl_json=>serialize(
        EXPORTING
         data         =  gs_output
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        RECEIVING
         r_json         = lv_body ).
**************update the API logs******************
        CALL METHOD lo_log_upd->log_entry_store
          EXPORTING
            apiname         = 'FARM_GI_COSTCNTR'
            ijson           = lv_data
            ojson           = lv_body
          EXCEPTIONS
            apiname_missing = 1
            json_missing    = 2
            OTHERS          = 3.
*Set JSON Content-Type
        CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        CALL METHOD server->response->set_cdata( data = lv_body ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
