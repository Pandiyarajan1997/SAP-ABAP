class ZCL_API_CR_DR_NOTE_LIST_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_CR_DR_NOTE_LIST_DMS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*Created By: Pandiarajan
*Created On: 07.03.2024
*Purpose : Sending the pdf string for retailer ledger account statement - DMS
*---------------------------------------------------------------*
    TYPES : BEGIN OF ty_kunnr,
              distributor TYPE kunnr,
            END OF ty_kunnr.
    DATA : dist TYPE TABLE OF ty_kunnr.
    TYPES: BEGIN OF input,
             distributor LIKE dist,
             fromdate    TYPE budat,
             todate      TYPE budat,
           END OF input.
    TYPES: BEGIN OF output,
             distributor  TYPE kunnr,
             dist_name    TYPE name1,
             retailer     TYPE kunnr,
             retlr_name   TYPE name1,
             doc_no       TYPE belnr_d,
             doc_type     TYPE string,
             posting_date TYPE budat,
             ref_no       TYPE xblnr,
             sgtxt        TYPE sgtxt,
             amount       TYPE dmbtr,
             glacnt       TYPE saknr,
             gl_desc      TYPE txt50_skat,
             note_type    TYPE char20,
             new_ref      TYPE char20,
           END OF output.

    DATA: gs_input  TYPE input.
    DATA: gt_output TYPE TABLE OF output.

    DATA: lv_msg    TYPE string,
          lv_type   TYPE string,
          lv_gltype TYPE char20.

    DATA: lr_request  TYPE REF TO if_http_request,
          lr_response TYPE REF TO if_http_response.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.

    lr_request  = server->request.
    lr_response = server->response.
* Check the Calling Method
    IF lr_request->get_method( ) EQ 'GET'.
      CALL METHOD lr_request->get_cdata RECEIVING data = DATA(lv_data).
      DATA(lv_data_tmp) = |{ lv_data }|.
** Deserialize the input our required input ***
      /ui2/cl_json=>deserialize(
      EXPORTING
       json         = lv_data_tmp
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
       assoc_arrays = abap_true
      CHANGING
       data         = gs_input ).

**************check the from & to date*********
      IF gs_input-fromdate IS INITIAL.
        gs_input-fromdate = |{ sy-datum(6) }01|.
      ENDIF.
      IF gs_input-todate IS INITIAL.
        gs_input-todate = sy-datum.
      ENDIF.
***************fetch only dms distributor***************
      IF gs_input-distributor IS NOT INITIAL.
        SELECT werks,kunnr,name1 FROM kna1 INTO TABLE @DATA(lt_dist)
                           FOR ALL ENTRIES IN @gs_input-distributor[]
                           WHERE kunnr = @gs_input-distributor-distributor.
      ELSE.
        SELECT a~bwkey,b~kunnr,b~name1 FROM t001k AS a
                               INNER JOIN kna1 AS b
                               ON a~bwkey = b~werks
                               INTO TABLE @lt_dist WHERE bukrs = 'DMS1'.
      ENDIF.
      SORT : lt_dist BY werks.
**********************Excess payment removal process*************
      SELECT * FROM zdms_dz_removal INTO TABLE @DATA(lt_delete)
                                    FOR ALL ENTRIES IN @lt_dist
                                    WHERE distributor = @lt_dist-kunnr.
      IF sy-subrc = 0.
        SORT : lt_delete BY belnr gjahr.
      ENDIF.
****************fetch the GL ACNT details************
      SELECT saknr,ktopl,txt50 FROM skat INTO TABLE @DATA(lt_skat) WHERE ktopl = 'SDMS'.
      IF sy-subrc = 0.
        SORT lt_skat BY saknr.
      ENDIF.
*******************get the bktxt(new number) from bkpf***********
      SELECT bukrs,belnr,gjahr,bktxt FROM bkpf INTO TABLE @DATA(lt_bkpf)
                                         WHERE bukrs = 'DMS1'
                                         AND   blart IN ( 'DG' , 'DR' ).
      IF sy-subrc = 0.
        SORT : lt_bkpf BY bukrs belnr gjahr.
      ENDIF.
**********************BSAD*******************
      SELECT a~belnr,a~gjahr,a~kunnr,a~budat,a~blart,a~xblnr,a~sgtxt,a~dmbtr,
             b~rbusa,b~racct,c~name1
             FROM bsad AS a INNER JOIN faglflexa AS b
             ON  a~gjahr = b~ryear
             AND a~belnr = b~docnr
             INNER JOIN kna1 AS c
             ON  a~kunnr = c~kunnr
             INTO TABLE @DATA(lt_bsad)
             FOR ALL ENTRIES IN @lt_dist
             WHERE a~budat <= @gs_input-todate
             AND   a~budat >= @gs_input-fromdate
             AND   a~blart IN ('DR','DG')
             AND   a~bukrs EQ 'DMS1'
             AND   a~umskz <> 'H'
             AND   b~rldnr  = '0L'
             AND   b~docln  = '000002'
             AND   b~rbusa  = @lt_dist-werks.
      IF sy-subrc = 0.

        LOOP AT lt_bsad ASSIGNING FIELD-SYMBOL(<fs_bsad>).
**********************Excess payment removal process*************
          READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = <fs_bsad>-belnr
                                                               gjahr = <fs_bsad>-gjahr BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
*****************check the document type***********
          IF <fs_bsad>-blart = 'DR'.
            lv_type = 'DEBIT NOTE'.
          ELSEIF <fs_bsad>-blart = 'DG'.
            lv_type = 'CREDIT NOTE'.
          ENDIF.

******************check the gl type*************
          CLEAR : lv_gltype.
          IF <fs_bsad>-racct = '0042002013' OR <fs_bsad>-racct = '0042002012'.
            lv_gltype = 'AUTO CD'.
          ELSE.
            lv_gltype = 'MANUAL'.
          ENDIF.
*      "GL Account existence checks
          READ TABLE lt_skat INTO DATA(ls_skat) WITH KEY saknr = <fs_bsad>-racct BINARY SEARCH.
********************get the distributor name*************
          READ TABLE lt_dist INTO DATA(ls_dist) WITH KEY werks = <fs_bsad>-rbusa BINARY SEARCH.

******************read the new number series****************
          READ TABLE lt_bkpf INTO DATA(ls_bkpf) WITH KEY bukrs = 'DMS1'
                                                         belnr = <fs_bsad>-belnr
                                                         gjahr = <fs_bsad>-gjahr BINARY SEARCH.

**************append the final output*******************
          APPEND VALUE #( distributor  = ls_dist-kunnr
                          dist_name    = ls_dist-name1
                          retailer     = <fs_bsad>-kunnr
                          retlr_name   = <fs_bsad>-name1
                          doc_no       = <fs_bsad>-belnr
                          doc_type     = lv_type
                          posting_date = <fs_bsad>-budat
                          ref_no       = <fs_bsad>-xblnr
                          sgtxt        = <fs_bsad>-sgtxt
                          amount       = <fs_bsad>-dmbtr
                          glacnt       = <fs_bsad>-racct
                          gl_desc      = ls_skat-txt50
                          note_type    = lv_gltype
                          new_ref      = ls_bkpf-bktxt ) TO gt_output.
          CLEAR : lv_type,ls_dist,ls_skat,ls_bkpf.
        ENDLOOP.
      ENDIF.
**********************BSID*******************
      SELECT a~belnr,a~gjahr,a~kunnr,a~budat,a~blart,a~xblnr,
             a~sgtxt,a~dmbtr,b~rbusa,b~racct,c~name1
             FROM bsid AS a INNER JOIN faglflexa AS b
             ON  a~gjahr = b~ryear
             AND a~belnr = b~docnr
             INNER JOIN kna1 AS c
             ON  a~kunnr = c~kunnr
             INTO TABLE @DATA(lt_bsid)
             FOR ALL ENTRIES IN @lt_dist
             WHERE a~budat <= @gs_input-todate
             AND   a~budat >= @gs_input-fromdate
             AND   a~blart IN ('DR','DG')
             AND   a~bukrs EQ 'DMS1'
             AND   a~umskz <> 'H'
             AND   b~rldnr  = '0L'
             AND   b~docln  = '000002'
             AND   b~rbusa  = @lt_dist-werks.
      IF sy-subrc = 0.
        LOOP AT lt_bsid ASSIGNING FIELD-SYMBOL(<fs_bsid>).
**********************Excess payment removal process*************
          READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = <fs_bsid>-belnr
                                                               gjahr = <fs_bsid>-gjahr BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
*****************check the document type***********
          IF <fs_bsid>-blart = 'DR'.
            lv_type = 'DEBIT NOTE'.
          ELSEIF <fs_bsid>-blart = 'DG'.
            lv_type = 'CREDIT NOTE'.
          ENDIF.

******************check the gl type*************
          CLEAR : lv_gltype.
          IF <fs_bsid>-racct = '0042002013' OR <fs_bsid>-racct = '0042002012'.
            lv_gltype = 'AUTO CD'.
          ELSE.
            lv_gltype = 'MANUAL'.
          ENDIF.
*      "GL Account existence checks
          CLEAR : ls_skat.
          READ TABLE lt_skat INTO ls_skat WITH KEY saknr = <fs_bsid>-racct BINARY SEARCH.
********************get the distributor name*************
          READ TABLE lt_dist INTO ls_dist WITH KEY werks = <fs_bsid>-rbusa BINARY SEARCH.
******************read the new number series****************
          READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = 'DMS1'
                                                   belnr = <fs_bsid>-belnr
                                                   gjahr = <fs_bsid>-gjahr BINARY SEARCH.

**************append the final output*******************
          APPEND VALUE #( distributor  = ls_dist-kunnr
                          dist_name    = ls_dist-name1
                          retailer     = <fs_bsid>-kunnr
                          retlr_name   = <fs_bsid>-name1
                          doc_no       = <fs_bsid>-belnr
                          doc_type     = lv_type
                          posting_date = <fs_bsid>-budat
                          ref_no       = <fs_bsid>-xblnr
                          sgtxt        = <fs_bsid>-sgtxt
                          amount       = <fs_bsid>-dmbtr
                          glacnt       = <fs_bsid>-racct
                          gl_desc      = ls_skat-txt50
                          note_type    = lv_gltype
                          new_ref      = ls_bkpf-bktxt ) TO gt_output.
          CLEAR : lv_type,ls_dist,ls_bkpf.
        ENDLOOP.
      ENDIF.

      SORT : gt_output BY distributor posting_date new_ref.
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
          apiname         = 'DMS_CRDR_LOG'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
*Set JSON Content-Type
      CALL METHOD lr_response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD lr_response->set_cdata( data = lv_body ).
    ELSE.
      v_jsonload = |No Data is Captured in Input|.
*Set JSON Content-Type
      CALL METHOD lr_response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD lr_response->set_cdata( data = v_jsonload ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
