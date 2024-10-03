class ZCL_API_CUS_FIN_INV definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_CUS_FIN_INV IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*-------------------------------------------------------------------------------*
    "Created by   : Pandiarajan
    "Created on   : 18.10.2023
    "Purpose      : Customer invoice details for Sundaram Finance
    "Reference by : Suresh BV and Ramakrishnan J
*-------------------------------------------------------------------------------*

    TYPES: BEGIN OF ty_output,

             customer  TYPE kunnr,
             cusname   TYPE name1_gp,
             inv_date  TYPE fkdat,
             inv_no    TYPE vbeln_vf,
             inv_item  TYPE posnr_vf,
             matnr     TYPE matnr,
             matdes    TYPE txz01,
             inv_qty   TYPE fkimg,
             netqty_kg TYPE ntgew_15,
             grsqty_kg TYPE brgew_15,

           END OF ty_output.

    DATA gt_open_inv TYPE TABLE OF ty_output.
    DATA : lv_body TYPE string.
*Get Body data
    DATA v_jsonload TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.       "*Output Entry in Log Table
    CREATE OBJECT lo_log_upd.


    REFRESH : gt_open_inv.


**-------- Fetching Invoice Details from zsd_sf_cust_inv ----------------------------------------*

    SELECT invoicekey,
           bukrs,
           status,
           invoiceno FROM zsd_sf_cust_inv INTO TABLE @DATA(lt_sfcust) WHERE status IN ( 12 , 13 , 14 , 15 ).

    IF lt_sfcust IS NOT INITIAL.

**-------- Fetching Invoice Details from vbrk, vbrp , kna1 ----------------------------------------*

      SELECT a~vbeln AS inv_no
             a~kunag AS customer
             a~fkdat AS inv_date
             b~posnr AS inv_item
             b~fkimg AS inv_qty
             b~ntgew AS netqty_kg
             b~brgew AS grsqty_kg
             b~matnr
             b~arktx AS matdes
             c~name1 AS cusname INTO CORRESPONDING FIELDS OF TABLE gt_open_inv
                                FROM vbrk AS a
                                INNER JOIN vbrp AS b ON a~vbeln = b~vbeln
                                INNER JOIN kna1 AS c ON a~kunag = c~kunnr
                                FOR ALL ENTRIES IN lt_sfcust
                                WHERE a~vbeln = lt_sfcust-invoiceno.

    ENDIF.

    IF gt_open_inv IS NOT INITIAL.

      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_open_inv
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'SF_CUS_INV'
          ijson           = 'NO INPUT JSON'
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.


      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).

    ELSE.
      CLEAR lv_body.
      lv_body = |No data found|.
      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
