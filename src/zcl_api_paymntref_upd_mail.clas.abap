class ZCL_API_PAYMNTREF_UPD_MAIL definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_PAYMNTREF_UPD_MAIL IMPLEMENTATION.


 METHOD if_http_extension~handle_request.
*Created By: Samsudeen M
*Created On: 08.05.2023
*Purpose: API for Updating the Payment Reference and Sending Mail to the Respective Team
*Reference: Ramakrishnan J & Balamurugan Cinthamani (Testing)
*------------------------------------------------------------------------------------------*
   TYPES: BEGIN OF ty_input,
            orginv   TYPE belnr_d,
            pymtref  TYPE kidno,
            compcode TYPE bukrs,
            vendor   TYPE lifnr,
          END OF ty_input.
   DATA: gs_input TYPE ty_input.

   TYPES: BEGIN OF ty_output,
            orginv   TYPE belnr_d,
            pymtref  TYPE kidno,
            compcode TYPE bukrs,
            vendor   TYPE lifnr,
            docno    TYPE belnr_d,
            fisyear  TYPE gjahr,
            status   TYPE char1,
            msg      TYPE string,
          END OF ty_output.
   DATA: gs_output TYPE ty_output.

   DATA: gt_bseg   TYPE STANDARD TABLE OF bseg,
         gt_accchg TYPE TABLE OF accchg.
   DATA: gv_utr_field TYPE    char50 VALUE 'KIDNO'.
   DATA: lt_mail_list  TYPE /cfg/t_email.

   DATA: v_jsonload TYPE string.

   DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
   CREATE OBJECT lo_log_upd.

   CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

** Deserialize the input our required input ***
   /ui2/cl_json=>deserialize(
   EXPORTING
    json         = lv_data
    pretty_name  = /ui2/cl_json=>pretty_mode-user
   CHANGING
    data         = gs_input ).

   IF gs_input IS NOT INITIAL.
     IF gs_input-orginv IS INITIAL.
       DATA(lv_msg) = |Vendor Original Invoice Missing|.
     ELSEIF gs_input-pymtref IS INITIAL.
       lv_msg = |Payment Reference is Missing|.
     ELSEIF gs_input-vendor IS INITIAL.
       lv_msg = |Vendor code is Missing|.
     ELSE.
*Mail ID Fetching from Log table
       DATA(lv_part_lifnr) = |{ gs_input-Vendor ALPHA = IN }|.
       SELECT SINGLE * FROM zdebit_ref_mis
         INTO @DATA(l_vendordtls)
         WHERE bukrs = @gs_input-compcode
           AND doc_no = @gs_input-orginv
           AND account_type = 'K'
           AND account = @gs_input-Vendor. "actual_doc1 = @gs_input-orginv.
       IF sy-subrc = 0.
       ELSE.
         lv_msg = |No Clearing Document Present Against Original Invoice and Vendor|.
       ENDIF.
     ENDIF.

*Check if already updated
     IF l_vendordtls IS NOT INITIAL.
       REFRESH: gt_bseg.
       CALL FUNCTION 'FI_DOCUMENT_READ'
         EXPORTING
           i_bukrs     = l_vendordtls-bukrs
           i_belnr     = l_vendordtls-doc_no
           i_gjahr     = l_vendordtls-gjahr
         TABLES
           t_bseg      = gt_bseg[]
         EXCEPTIONS
           wrong_input = 1
           not_found   = 2
           OTHERS      = 3.
       IF sy-subrc = 0.
         LOOP AT gt_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg1>).
           IF <fs_bseg1>-kidno IS NOT INITIAL.
             lv_msg = |UTR No already updated|.
           ENDIF.
         ENDLOOP.
       ELSE.
         CASE sy-subrc.
           WHEN '1'.
             lv_msg = |wrong_input|.
           WHEN '2'.
             lv_msg = |not_found|.
           WHEN OTHERS.
             lv_msg = |Error in Document read|.
         ENDCASE.
       ENDIF.
     ENDIF.
   ENDIF.

   IF lv_msg IS NOT INITIAL.

     CLEAR gs_output.
     gs_output-orginv   = gs_input-orginv.
     gs_output-pymtref  = gs_input-pymtref.
     gs_output-compcode = gs_input-compcode.
     gs_output-vendor   = gs_input-vendor.
     gs_output-status   = 'E'.
     gs_output-msg      = lv_msg.

*     DATA(lv_body) = | { '{' }Error: { '"' }  { lv_msg } { '"' } { '}' } |.
     CLEAR v_jsonload.
** serialize the output for response ***
     /ui2/cl_json=>serialize(
     EXPORTING
      data         =  gs_output
      pretty_name  = /ui2/cl_json=>pretty_mode-user
     RECEIVING
      r_json         = v_jsonload ).

*      SET json content-type
     CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
     CALL METHOD server->response->set_cdata( data = v_jsonload ).
   ELSE.
*Financial Document Read Based on Input
     REFRESH: gt_bseg.
     CALL FUNCTION 'FI_DOCUMENT_READ'
       EXPORTING
         i_bukrs     = l_vendordtls-bukrs
         i_belnr     = l_vendordtls-doc_no
         i_gjahr     = l_vendordtls-gjahr
       TABLES
         t_bseg      = gt_bseg[]
       EXCEPTIONS
         wrong_input = 1
         not_found   = 2
         OTHERS      = 3.
     IF sy-subrc = 0.
       DATA(lv_awref) = l_vendordtls-doc_no.
       DATA(lv_aworg) = CONV aworg( |{ l_vendordtls-bukrs }{ l_vendordtls-gjahr }| ).
       LOOP AT gt_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg>).
         IF <fs_bseg>-kidno IS NOT INITIAL.

         ENDIF.
         REFRESH: gt_accchg.
         DATA(lw_accchg) = VALUE accchg( fdname = gv_utr_field
                                         oldval = <fs_bseg>-kidno
                                         newval = gs_input-pymtref ).
         APPEND lw_accchg TO gt_accchg.
         "Function Module to update the Payment reference
         CALL FUNCTION 'FI_DOCUMENT_CHANGE'
           EXPORTING
             i_awtyp              = 'BKPF'
             i_awref              = lv_awref
             i_aworg              = lv_aworg
             i_buzei              = <fs_bseg>-buzei
           TABLES
             t_accchg             = gt_accchg
           EXCEPTIONS
             no_reference         = 1
             no_document          = 2
             many_documents       = 3
             wrong_input          = 4
             overwrite_creditcard = 5
             OTHERS               = 6.
         IF sy-subrc NE 0.
           CASE sy-subrc.
             WHEN '1'.
               lv_msg = 'no_reference'.
             WHEN '2'.
               lv_msg = 'no_document'.
             WHEN '3'.
               lv_msg = 'many_documents'.
             WHEN '4'.
               lv_msg = 'wrong_input'.
             WHEN '5'.
               lv_msg = 'overwrite_creditcard'.
             WHEN '6'.
               lv_msg = 'Others'.
           ENDCASE.
         ENDIF.
       ENDLOOP.
       IF lv_msg IS INITIAL.
         CLEAR: gs_output.
         gs_output-orginv   = gs_input-orginv.
         gs_output-pymtref  = gs_input-pymtref.
         gs_output-compcode = gs_input-compcode.
         gs_output-vendor   = gs_input-vendor.
         gs_output-docno    = l_vendordtls-doc_no.
         gs_output-fisyear  = l_vendordtls-gjahr.
         gs_output-status   = 'S'.
         gs_output-msg = |Payment Reference Updated|.
       ENDIF.
     ELSE.
       CASE sy-subrc.
         WHEN '1'.
           lv_msg = |wrong_input|.
         WHEN '2'.
           lv_msg = |not_found|.
         WHEN OTHERS.
           lv_msg = |Error in Document read|.
       ENDCASE.
     ENDIF.
   ENDIF.

*Final Response and Mail Triggering
   IF lv_msg IS INITIAL.
     l_vendordtls-pay_reference = gs_input-pymtref.
     MODIFY zdebit_ref_mis FROM l_vendordtls.
** serialize the output for response ***
     CLEAR v_jsonload.

     /ui2/cl_json=>serialize(
     EXPORTING
      data         =  gs_output
      pretty_name  = /ui2/cl_json=>pretty_mode-user
     RECEIVING
      r_json         = v_jsonload ).

*      set json content-type
     CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
     CALL METHOD server->response->set_cdata( data = v_jsonload ).
   ELSE.
*     lv_body = | { '{' }Error: { '"' }  { lv_msg } { '"' } { '}' } |.

     CLEAR gs_output.
     gs_output-orginv   = gs_input-orginv.
     gs_output-pymtref  = gs_input-pymtref.
     gs_output-compcode = gs_input-compcode.
     gs_output-vendor   = gs_input-vendor.
     gs_output-status   = 'E'.
     gs_output-msg      = lv_msg.

** serialize the output for response ***
     clear v_jsonload.
     /ui2/cl_json=>serialize(
     EXPORTING
      data         =  gs_output
      pretty_name  = /ui2/cl_json=>pretty_mode-user
     RECEIVING
      r_json         = v_jsonload ).

*      SET json content-type
     CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
     CALL METHOD server->response->set_cdata( data = v_jsonload ).
   ENDIF.

   CALL METHOD lo_log_upd->log_entry_store
     EXPORTING
       apiname         = 'PYMTREFUPD_IN'
       ijson           = lv_data
       ojson           = v_jsonload
     EXCEPTIONS
       apiname_missing = 1
       json_missing    = 2
       OTHERS          = 3.
   IF sy-subrc <> 0.

   ENDIF.
**Business Partner Fetching Based on Vendor Number
*      SELECT SINGLE businesspartner FROM ibpsupplier
*        INTO @DATA(l_businesspartner)
*        WHERE supplier = @l_vendordtls-account.
*      IF sy-subrc = 0.
**Group Feature which means Vendor Type Fetching Based on Business Partner
*        SELECT SINGLE group_feature FROM bp001
*          INTO @DATA(l_groupfeature)
*          WHERE partner = @l_businesspartner.
*        IF l_groupfeature IS NOT INITIAL.
**Mail ID list Fetching Based On Vendor Type
*          SELECT SINGLE * FROM zfi_pymtclr_mail
*            INTO @DATA(l_maidid)
*            WHERE vendortyp = @l_groupfeature.
*          IF sy-subrc = 0.
*            REFRESH lt_mail_list.
*            IF l_maidid-mailid1 IS NOT INITIAL.
*              APPEND VALUE #( sign   = 'I'
*                              option = 'EQ'
*                              low    = l_maidid-mailid1 ) TO lt_mail_list.
*            ENDIF.
*            IF l_maidid-mailid2 IS NOT INITIAL.
*              APPEND VALUE #( sign   = 'I'
*                              option = 'EQ'
*                              low    = l_maidid-mailid2 ) TO lt_mail_list.
*            ENDIF.
*            IF l_maidid-mailid3 IS NOT INITIAL.
*              APPEND VALUE #( sign   = 'I'
*                              option = 'EQ'
*                              low    = l_maidid-mailid3 ) TO lt_mail_list.
*            ENDIF.
*          ELSE.
*            v_jsonload = |[ Mail ID for respective grouping Characterstics is not maintained ]|.
**      set json content-type
*            CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
**Set Response Data
*            CALL METHOD server->response->set_cdata( data = v_jsonload ).
*          ENDIF.
*    ELSE.
*      v_jsonload = |[ Grouping Characterstics is not Maintained for Vendor { l_vendordtls-account } ]|.
**      set json content-type
*      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
**Set Response Data
*      CALL METHOD server->response->set_cdata( data = v_jsonload ).
*    ENDIF.
**MAIL TRIGGERING FUNCTION MODULE
*  CALL FUNCTION 'ZSEND_PYMTCLEARENCE_MAILS'
*    EXPORTING
*      orginv         = gs_input-orginv
*    IMPORTING
*      message        = lv_msg
*    TABLES
*      it_mailid      = lt_mail_list
*    EXCEPTIONS
*      mailid_missing = 1
*      OTHERS         = 2.
*  IF sy-subrc = 0.
*** serialize the output for response ***
*    /ui2/cl_json=>serialize(
*    EXPORTING
*     data         =  gs_output
*     pretty_name  = /ui2/cl_json=>pretty_mode-user
*    RECEIVING
*     r_json         = v_jsonload ).
*  ENDIF.
**      SET json content-type
*  CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
**Set Response Data
*  CALL METHOD server->response->set_cdata( data = v_jsonload ).
*ENDIF.
*ELSE.
*lv_body = | { '{' }Error: { '"' }  { lv_msg } { '"' } { '}' } |.
*** serialize the output for response ***
*/ui2/cl_json=>serialize(
*EXPORTING
* data         =  lv_body
* pretty_name  = /ui2/cl_json=>pretty_mode-user
*RECEIVING
* r_json         = v_jsonload ).
*
**      SET json content-type
*CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
**Set Response Data
*CALL METHOD server->response->set_cdata( data = v_jsonload ).
*ENDIF.

 ENDMETHOD.
ENDCLASS.
