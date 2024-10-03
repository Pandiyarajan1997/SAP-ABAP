class ZCL_API_SF_PENINV definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SF_PENINV IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*&Created by: Pandiarajan
*&Created On: 20.11.2023
*&Purpose : For fetch sf customer wise pending invoice from zsd_sf_cust_inv to mis
*&Reference: Ramakrishnan J
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_output,
             customer   TYPE kunnr,
             comcode    TYPE bukrs,
             fintype    TYPE zfincode,
             dueamount  TYPE netwr,
             credit_amt TYPE netwr,
             type       TYPE c1,
             message    TYPE string,
           END OF ty_output.

    DATA: gt_output TYPE TABLE OF ty_output,
          lv_amt    TYPE netwr.
    DATA: lv_data TYPE string.          "for input json string

    DATA: lv_body    TYPE string,       "for output json string
          v_jsonload TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.     "data dec for store api log table
    CREATE OBJECT lo_log_upd.
*******************get the finance customer list**************
    SELECT kunnr,zcustype FROM kna1 INTO TABLE @DATA(lt_kna1)
                                    WHERE zcustype NE @abap_false.
*******customer wise dueamt process***********
*****fetching from zsd_sf_cust_inv ******
    lv_data = 'No input'.
    IF sy-subrc = 0.
      SELECT custno,bukrs,fintype,invoiceamount FROM zsd_sf_cust_inv
                                            INTO TABLE @DATA(lt_inv)
                                            FOR ALL ENTRIES IN @lt_kna1
                                            WHERE status IN ( 10,12,13,14,15 )
                                            AND   custno EQ @lt_kna1-kunnr.

      DATA : lt_invdue LIKE lt_inv.
      REFRESH : lt_invdue.
      SORT lt_inv BY custno.

      LOOP AT lt_inv ASSIGNING FIELD-SYMBOL(<fs_out>).
        lv_amt = <fs_out>-invoiceamount + lv_amt.
        AT END OF custno.
******************get the dueamount******************

          APPEND VALUE #( custno         = <fs_out>-custno
                          bukrs          = <fs_out>-bukrs
                          invoiceamount  = lv_amt ) TO lt_invdue.
          CLEAR : lv_amt.
        ENDAT.
      ENDLOOP.

***************************get the open item for the customer******************
      SELECT kunnr,bukrs,belnr,budat,blart,shkzg,dmbtr
             FROM bsid
             INTO TABLE @DATA(lt_bsid)
             FOR ALL ENTRIES IN @lt_kna1
             WHERE bukrs = '1000'
             AND   kunnr = @lt_kna1-kunnr
             AND   umskz <> 'H'
             AND   shkzg = 'H'.

      DATA : l_total_amt TYPE wrbtr,
             lt_crbal    LIKE lt_bsid.

      SORT : lt_bsid BY kunnr.

      REFRESH : lt_crbal.
*******customer wise invoice calculation process***********
      LOOP AT lt_bsid ASSIGNING FIELD-SYMBOL(<fs_bsid>).
        l_total_amt = l_total_amt + <fs_bsid>-dmbtr.

        AT END OF kunnr.

          APPEND VALUE #( kunnr = <fs_bsid>-kunnr
                          dmbtr = l_total_amt
                          bukrs = <fs_bsid>-bukrs ) TO lt_crbal.
          CLEAR : l_total_amt.
        ENDAT.
      ENDLOOP.

***************sorting
      SORT : lt_crbal BY kunnr,
             lt_invdue  BY custno.

      LOOP AT lt_kna1 INTO DATA(ls_kna1).
****************get the INV Amount**************
        READ TABLE lt_invdue INTO DATA(ls_inv) WITH KEY custno = ls_kna1-kunnr BINARY SEARCH.
****************get the credit amount**************
        READ TABLE lt_crbal INTO DATA(ls_crbal) WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.

        IF ls_inv-bukrs IS INITIAL.
          ls_inv-bukrs = ls_crbal-bukrs.
        ENDIF.
********************fill the final table*******************
        APPEND VALUE #( customer   = ls_kna1-kunnr
                        comcode    = ls_inv-bukrs
                        fintype    = ls_kna1-zcustype
                        dueamount  = ls_inv-invoiceamount
                        credit_amt = ls_crbal-dmbtr
                        type       = 'S'
                        message    = 'Success' ) TO gt_output.

        CLEAR : ls_crbal , ls_kna1 , ls_inv.

      ENDLOOP.
    ENDIF.

    IF gt_output IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'SF_PENINV'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      v_jsonload = |{ lv_body }|.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ELSE.
      APPEND VALUE #( type = 'E'
                      message = 'No pending invoice for sf customer' ) TO gt_output.
*      ** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
      v_jsonload = |{ lv_body }|.

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'SF_PENINV'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
