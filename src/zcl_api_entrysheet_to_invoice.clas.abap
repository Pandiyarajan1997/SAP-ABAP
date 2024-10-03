class ZCL_API_ENTRYSHEET_TO_INVOICE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_ENTRYSHEET_TO_INVOICE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 03.05.2023
*&Purpose : For Creating Entrysheet to Invoice Based on Purchase order
*&Reference: Ramakrishnan J & Gopal raja
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             ebeln      TYPE ebeln,
             ebelp      TYPE ebelp,
             doc_date   TYPE bldat,
             post_date  TYPE budat,
             ref_doc_no TYPE xblnr,
             attach1    TYPE xstring,
             attach2    TYPE xstring,
             attach3    TYPE xstring,
           END OF ty_input.

    TYPES: BEGIN OF ty_output,
             ebeln        TYPE ebeln,
             ebelp        TYPE ebelp,
             addn_charges TYPE flag,
             enstno       TYPE lblni,
             invoiceno    TYPE belnr_d,
           END OF ty_output.
    TYPES: BEGIN OF ty_str,
             ebeln      TYPE ebeln,
             ebelp      TYPE ebelp,
             ent_sno    TYPE lblni,
             belnr      TYPE belnr_d,
             acc_doc_no TYPE belnr_d,
             type       TYPE bapi_mtype,
             msg        TYPE msg,
           END OF ty_str.
    DATA: ls_final_json TYPE ty_str.
    DATA: gt_input TYPE TABLE OF ty_input,
          gs_input TYPE ty_input.
    DATA: gt_output TYPE TABLE OF ty_output.

    DATA: lv_data TYPE string,
          lv_msg  TYPE string,
          l_msg   TYPE string.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA: lo_main TYPE REF TO zcl_scm_servicepo_to_invoice.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    DATA l_awkey TYPE awkey .

    CREATE OBJECT lo_log_upd.

    "Input of API Request
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    DATA(lv_data_tmp) = |[ { lv_data } ]|.
    CONDENSE lv_data_tmp NO-GAPS.
    SHIFT lv_data_tmp LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data_tmp
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    CREATE OBJECT lo_main.

    IF gt_input IS NOT INITIAL.
      DATA(l_podtls) = gt_input[ 1 ].
      IF l_podtls-ebeln IS INITIAL.
        lv_msg = |Purchase Order Number is Missing|.
      ELSEIF l_podtls-ebelp IS INITIAL.
        lv_msg = |Purchase Order Item Number is Missing|.
      ELSEIF l_podtls-doc_date IS INITIAL.
        lv_msg = |Purchase Order invoice Date is Missing|.
      ELSEIF l_podtls-post_date IS INITIAL.
        lv_msg = |Purchase Order posting Date is Missing|.
      ELSEIF l_podtls-ref_doc_no IS INITIAL.
        lv_msg = |Purchase Order vendor invoice no is Missing|.
      ELSE.
        SELECT SINGLE * FROM ekpo
           INTO @DATA(l_polineitem)
           WHERE ebeln = @l_podtls-ebeln
           AND ebelp = @l_podtls-ebelp.
        IF sy-subrc NE 0.
          lv_msg = |Kindly Check Purchase Order Details in Input|.
        ELSE.
          SELECT SINGLE frgke FROM ekko
                INTO @DATA(l_release_flag) WHERE ebeln = @l_podtls-ebeln AND frgzu = 'XX'.
          IF l_release_flag NE '3'.
            lv_msg = 'The purchase order has not been released'.
          ELSE.
            SELECT SINGLE * FROM zmm_esno_invoice
              INTO @DATA(l_logdtls)
              WHERE ebeln = @l_polineitem-ebeln
              AND ebelp = @l_polineitem-ebelp.
            IF sy-subrc = 0.
              IF l_logdtls-type NE 'S'.
                IF l_logdtls-ent_sno IS INITIAL.
*Create Entrysheet if already entry present in log table
                  CALL METHOD lo_main->create_entrysheet
                    EXPORTING
                      ebeln      = l_podtls-ebeln
                      ebelp      = l_podtls-ebelp
                      doc_date   = l_podtls-doc_date
                      post_date  = l_podtls-post_date
                    IMPORTING
                      entrysheet = DATA(lv_entrysheet)
                      return     = DATA(lt_return).
                  IF lv_entrysheet IS NOT INITIAL.
                    l_logdtls-ent_sno = lv_entrysheet.
                    WAIT UP TO 2 SECONDS.
*Create Invoice Number while any errors
                    REFRESH lt_return.
                    CALL METHOD lo_main->create_miro_invoice
                      EXPORTING
                        ebeln         = l_podtls-ebeln
                        ebelp         = l_podtls-ebelp
                        entrysheet_no = lv_entrysheet
                        doc_date      = l_podtls-doc_date
                        post_date     = l_podtls-post_date
                        ref_doc_no    = l_podtls-ref_doc_no
                        attach1       = l_podtls-attach1
                        attach2       = l_podtls-attach2
                        attach3       = l_podtls-attach3
                      IMPORTING
                        invocie_no    = DATA(l_invno)
                        gjahr         = DATA(l_year)
                        return        = lt_return.
                    IF ( l_invno IS NOT INITIAL AND l_year IS NOT INITIAL ).
                      l_logdtls-belnr = l_invno.
                      l_logdtls-gjahr = l_year.
                      l_awkey = |{ l_invno }{ l_year }|.
                      SELECT SINGLE belnr FROM bkpf
                          INTO l_logdtls-acc_doc_no
                          WHERE awkey = l_awkey.

                      l_logdtls-aedat = sy-datum.
                      l_logdtls-chg_time = sy-uzeit.
                      l_logdtls-type = 'S'.
                      l_logdtls-msg = |Entrysheet and Invoice Created|.
                    ELSE.
                      CLEAR lv_msg.
                      LOOP AT lt_return INTO DATA(lw_ret) WHERE ( type = 'E' OR type = 'A').
                        CLEAR: l_msg.
                        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                          EXPORTING
                            msgid               = lw_ret-id
                            msgnr               = lw_ret-number
                            msgv1               = lw_ret-message_v1
                            msgv2               = lw_ret-message_v2
                            msgv3               = lw_ret-message_v3
                            msgv4               = lw_ret-message_v4
                          IMPORTING
                            message_text_output = l_msg.
                        lv_msg = |{ lv_msg },{ l_msg }|.
                      ENDLOOP.
                      l_logdtls-aedat = sy-datum.
                      l_logdtls-chg_time = sy-uzeit.
                      l_logdtls-type = 'E'.
                      l_logdtls-msg = lv_msg.
                    ENDIF.
                  ELSE.
                    CLEAR lv_msg.
                    LOOP AT lt_return INTO lw_ret WHERE ( type = 'E' OR type = 'A').
                      CLEAR: l_msg.
                      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                        EXPORTING
                          msgid               = lw_ret-id
                          msgnr               = lw_ret-number
                          msgv1               = lw_ret-message_v1
                          msgv2               = lw_ret-message_v2
                          msgv3               = lw_ret-message_v3
                          msgv4               = lw_ret-message_v4
                        IMPORTING
                          message_text_output = l_msg.
                      lv_msg = |{ lv_msg },{ l_msg }|.
                    ENDLOOP.
                    l_logdtls-aedat = sy-datum.
                    l_logdtls-chg_time = sy-uzeit.
                    l_logdtls-type = 'E'.
                    l_logdtls-msg = lv_msg.
                  ENDIF.
                  MODIFY zmm_esno_invoice FROM l_logdtls.
                ENDIF.
*Scenario where entrysheet is created but Invoice Number Not Generated
                IF l_logdtls-ent_sno IS NOT INITIAL AND l_logdtls-belnr IS INITIAL.
                  CALL METHOD lo_main->create_miro_invoice
                    EXPORTING
                      ebeln         = l_podtls-ebeln
                      ebelp         = l_podtls-ebelp
                      entrysheet_no = l_logdtls-ent_sno
                      doc_date      = l_podtls-doc_date
                      post_date     = l_podtls-post_date
                      ref_doc_no    = l_podtls-ref_doc_no
                      attach1       = l_podtls-attach1
                      attach2       = l_podtls-attach2
                      attach3       = l_podtls-attach3
                    IMPORTING
                      invocie_no    = l_invno
                      gjahr         = l_year
                      return        = lt_return.
                  IF ( l_invno IS NOT INITIAL AND l_year IS NOT INITIAL ).
                    l_logdtls-belnr = l_invno.
                    l_logdtls-gjahr = l_year.
                    l_logdtls-aedat = sy-datum.
                    l_logdtls-chg_time = sy-uzeit.
                    l_logdtls-type = 'S'.
                    l_awkey = |{ l_invno }{ l_year }|.
                    SELECT SINGLE belnr FROM bkpf
                        INTO l_logdtls-acc_doc_no
                        WHERE awkey = l_awkey.
                    l_logdtls-msg = |Entrysheet and Invoice Created|.
                  ELSE.
                    CLEAR lv_msg.
                    LOOP AT lt_return INTO lw_ret WHERE ( type = 'E' OR type = 'A').
                      CLEAR: l_msg.
                      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                        EXPORTING
                          msgid               = lw_ret-id
                          msgnr               = lw_ret-number
                          msgv1               = lw_ret-message_v1
                          msgv2               = lw_ret-message_v2
                          msgv3               = lw_ret-message_v3
                          msgv4               = lw_ret-message_v4
                        IMPORTING
                          message_text_output = l_msg.
                      lv_msg = |{ lv_msg },{ l_msg }|.
                    ENDLOOP.
                    l_logdtls-aedat = sy-datum.
                    l_logdtls-chg_time = sy-uzeit.
                    l_logdtls-type = 'E'.
                    l_logdtls-msg = lv_msg.
                  ENDIF.
                  MODIFY zmm_esno_invoice FROM l_logdtls.
                ENDIF.
              ENDIF.
            ELSE.
*If the Record is New log
*Create Entrysheet if already entry present in log table
              l_logdtls-ebeln = l_podtls-ebeln.
              l_logdtls-ebelp = l_podtls-ebelp.
              l_logdtls-erdat = sy-datum.
              l_logdtls-erzet = sy-uzeit.
              l_logdtls-ernam = sy-uname.

              IF l_logdtls-ent_sno IS INITIAL.
                CALL METHOD lo_main->create_entrysheet
                  EXPORTING
                    ebeln      = l_podtls-ebeln
                    ebelp      = l_podtls-ebelp
                    doc_date   = l_podtls-doc_date
                    post_date  = l_podtls-post_date
                  IMPORTING
                    entrysheet = lv_entrysheet
                    return     = lt_return.
                IF lv_entrysheet IS NOT INITIAL.
                  l_logdtls-ent_sno = lv_entrysheet.
                  WAIT UP TO 2 SECONDS.
                ELSE.
                  CLEAR lv_msg.
                  LOOP AT lt_return INTO lw_ret WHERE ( type = 'E' OR type = 'A').
                    CLEAR: l_msg.
                    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                      EXPORTING
                        msgid               = lw_ret-id
                        msgnr               = lw_ret-number
                        msgv1               = lw_ret-message_v1
                        msgv2               = lw_ret-message_v2
                        msgv3               = lw_ret-message_v3
                        msgv4               = lw_ret-message_v4
                      IMPORTING
                        message_text_output = l_msg.
                    lv_msg = |{ lv_msg },{ l_msg }|.
                  ENDLOOP.
                  l_logdtls-type = 'E'.
                  l_logdtls-msg = lv_msg.
                ENDIF.
                MODIFY zmm_esno_invoice FROM l_logdtls.
              ENDIF.
              IF l_logdtls-belnr IS INITIAL.
*Create Invoice Number while any errors
                REFRESH lt_return.
                CALL METHOD lo_main->create_miro_invoice
                  EXPORTING
                    ebeln         = l_podtls-ebeln
                    ebelp         = l_podtls-ebelp
                    entrysheet_no = lv_entrysheet
                    doc_date      = l_podtls-doc_date
                    post_date     = l_podtls-post_date
                    ref_doc_no    = l_podtls-ref_doc_no
                    attach1       = l_podtls-attach1
                    attach2       = l_podtls-attach2
                    attach3       = l_podtls-attach3
                  IMPORTING
                    invocie_no    = l_invno
                    gjahr         = l_year
                    return        = lt_return.
                IF ( l_invno IS NOT INITIAL AND l_year IS NOT INITIAL ).
                  l_logdtls-belnr = l_invno.
                  l_logdtls-gjahr = l_year.
                  l_logdtls-type = 'S'.
                  l_awkey = |{ l_invno }{ l_year }|.
                  SELECT SINGLE belnr FROM bkpf
                      INTO l_logdtls-acc_doc_no
                      WHERE awkey = l_awkey.
                  l_logdtls-msg = |Entrysheet and Invoice Created|.
                  MODIFY zmm_esno_invoice FROM l_logdtls.
                ELSE.
                  CLEAR lv_msg.
                  LOOP AT lt_return INTO lw_ret WHERE ( type = 'E' OR type = 'A').
                    CLEAR: l_msg.
                    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                      EXPORTING
                        msgid               = lw_ret-id
                        msgnr               = lw_ret-number
                        msgv1               = lw_ret-message_v1
                        msgv2               = lw_ret-message_v2
                        msgv3               = lw_ret-message_v3
                        msgv4               = lw_ret-message_v4
                      IMPORTING
                        message_text_output = l_msg.
                    lv_msg = |{ lv_msg },{ l_msg }|.
                  ENDLOOP.
                  l_logdtls-type = 'E'.
                  l_logdtls-msg = lv_msg.
                  MODIFY zmm_esno_invoice FROM l_logdtls.
                ENDIF.
              ENDIF.
*Invocie Completed
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING l_logdtls TO ls_final_json.
    IF ls_final_json IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  ls_final_json
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

      v_jsonload = | { '[' } { lv_body } { ']' } |.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'ESHEET_MIRO'
          ijson           = lv_data
          ojson           = v_jsonload
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.

      ENDIF.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ELSE.

      ls_final_json-ebeln   = l_podtls-ebeln.
      ls_final_json-ebelp   = l_podtls-ebelp.
      ls_final_json-type    = 'E'.
      ls_final_json-msg     = lv_msg.

** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  ls_final_json
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

      v_jsonload = | { '[' } { lv_body } { ']' } |.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'ESHEET_MIRO'
          ijson           = lv_data
          ojson           = v_jsonload
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.

      ENDIF.

*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
