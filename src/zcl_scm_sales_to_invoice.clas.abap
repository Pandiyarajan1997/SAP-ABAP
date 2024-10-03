class ZCL_SCM_SALES_TO_INVOICE definition
  public
  final
  create public .

public section.

  methods CREATE_OB_DELIVERY
    importing
      !SALES_DOCUMENT type VBELN_VA
      !LGORT type LGORT_D optional
    exporting
      !DELIVERY_NO type VBELN
      !RETURN type BAPIRET2_T
      !MSG type STRING .
  methods POST_GOODS_ISSUE
    importing
      !DELIVERY_NO type VBELN
      !LGORT type LGORT_D optional
    exporting
      !PGI_NO type MBLNR
      !RETURN type BAPIRET2_T
      !MSG type STRING .
  methods CREATE_INVOICE
    importing
      !DELIVERY_NO type VBELN_VL
    exporting
      !BILL_INVOICE type VBELN_VF
      !ACCO_INVOICE type BELNR_D
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !RETURN type BAPIRET2_T .
  methods MAIL_SENT
    importing
      !VBELN type VBELN_VF optional
      value(GJAHR) type GJAHR optional
    exporting
      !MAIL_SENT type FLAG .
  methods FINANCING
    importing
      !LS_VBRK type VBRK
      !BOOSTER type FLAG optional .
  methods DIGITAL_SIGN
    importing
      !VBELN type VBELN
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !PDF type FPCONTENT
    exporting
      !SIGN_PDF type FPCONTENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SCM_SALES_TO_INVOICE IMPLEMENTATION.


  METHOD create_invoice.
    DATA: vbsk_i TYPE vbsk.
    DATA: d_success TYPE c.
    DATA: xkomfk   TYPE STANDARD TABLE OF komfk,
          w_xkomfk TYPE komfk,
          xkomv    TYPE STANDARD TABLE OF komv,
          xthead   TYPE STANDARD TABLE OF theadvb,
          xvbfs    TYPE STANDARD TABLE OF vbfs,
          xvbpa    TYPE STANDARD TABLE OF vbpavb,
          xvbrk    TYPE STANDARD TABLE OF vbrkvb,
          xvbrp    TYPE STANDARD TABLE OF vbrpvb,
          xvbss    TYPE STANDARD TABLE OF vbss,
          xkomfkgn TYPE STANDARD TABLE OF komfkgn.

    REFRESH: xkomfk, xkomv,
    xthead, xvbfs,
    xvbpa, xvbrk,
    xvbrp, xvbss.

    CLEAR : xkomfk, xkomv,
    xthead, xvbfs,
    xvbpa, xvbrk,
    xvbrp, xvbss,
    vbsk_i.

    vbsk_i-smart = 'F'.
    w_xkomfk-vbeln = delivery_no.
    w_xkomfk-vbtyp = 'J'.

    APPEND w_xkomfk TO xkomfk.

    CALL FUNCTION 'RV_INVOICE_CREATE'
      EXPORTING
        vbsk_i       = vbsk_i
        with_posting = 'C'
      TABLES
        xkomfk       = xkomfk
        xkomv        = xkomv
        xthead       = xthead
        xvbfs        = xvbfs
        xvbpa        = xvbpa
        xvbrk        = xvbrk
        xvbrp        = xvbrp
        xvbss        = xvbss.

    IF sy-subrc EQ 0.
      COMMIT WORK.
      SELECT SINGLE vbeln FROM vbfa
        INTO bill_invoice
        WHERE vbelv = delivery_no AND vbtyp_n = 'M'.

      SELECT SINGLE belnr
                    bukrs
                    gjahr FROM vbrk
        INTO ( acco_invoice,
               bukrs,
               gjahr )
        WHERE vbeln = bill_invoice.
    ENDIF.
  ENDMETHOD.


  METHOD create_ob_delivery.

    DATA: lt_salesitem TYPE TABLE OF bapidlvreftosalesorder,
          l_delivery   TYPE bapishpdelivnumb-deliv_numb,
          lt_return    TYPE TABLE OF bapiret2,
          lw_return    TYPE bapiret2.
    DATA: lv_qty TYPE menge_d.
    DATA: lt_ret TYPE bapireturn.
    DATA: lt_table1 TYPE TABLE OF bapiwmdvs,
          lt_table2 TYPE TABLE OF bapiwmdve.

    IF sales_document IS NOT INITIAL.

      SELECT vbeln,
           posnr,
           matnr,
           meins,
           kwmeng,
           werks  FROM vbap INTO TABLE @DATA(lt_items)
                  WHERE vbeln = @sales_document.
      IF sy-subrc = 0.
        REFRESH: lt_salesitem.
        SORT lt_items[] BY matnr.
        DATA(lt_item_tmp) = lt_items[].
        LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<fs_items>).
          "Check for Auto batch Determination for picking
          SELECT SINGLE * FROM koth001 INTO @DATA(l_autobatch) WHERE kappl = 'V'
                                       AND kschl = 'ZSD' AND matnr = @<fs_items>-matnr
                                       AND datbi GE @sy-datum AND datab LE @sy-datum.
          IF sy-subrc NE 0.
            DATA(l_msg) = |Kindly check auto batch Determination for Material { <fs_items>-matnr } - Check with R&D Team|.
            EXIT.
          ELSE.
            SELECT SINGLE * FROM kondh INTO @DATA(l_batchsrch) WHERE knumh = @l_autobatch-knumh
                                                               AND chvsk   = 'O'.
            IF sy-subrc NE 0.
              l_msg = |Kindly check auto batch selection strategy for Material { <fs_items>-matnr } - Check with R&D Team|.
              EXIT.
            ENDIF.
          ENDIF.
          APPEND VALUE #( ref_doc = <fs_items>-vbeln
                          ref_item = <fs_items>-posnr
                          dlv_qty  = <fs_items>-kwmeng
                          sales_unit = <fs_items>-meins ) TO lt_salesitem.

          LOOP AT lt_item_tmp ASSIGNING FIELD-SYMBOL(<fs_item_tmp>) WHERE matnr = <fs_items>-matnr.
            lv_qty = lv_qty + <fs_item_tmp>-kwmeng.
          ENDLOOP.
          DATA(l_stock) = CONV bapicm61v-wkbst( 0 ).
          DATA(matnr) = CONV matnr18( <fs_items>-matnr ).
          "Stock Availabity Check for Material
          CLEAR: lt_ret,lt_table1,lt_table2.
          CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
            EXPORTING
              plant      = <fs_items>-werks
              material   = matnr
              unit       = <fs_items>-meins
            IMPORTING
              av_qty_plt = l_stock
              return     = lt_ret
            TABLES
              wmdvsx     = lt_table1
              wmdvex     = lt_table2.
          IF lt_table2 IS NOT INITIAL.
            l_stock = VALUE #( lt_table2[ 1 ]-com_qty OPTIONAL ).
          ENDIF.
          IF l_stock LT lv_qty.
            l_msg = |{ l_msg } Kindly check the stock of Material { <fs_items>-matnr } |.
            EXIT.
          ENDIF.
          CLEAR lv_qty.
        ENDLOOP.
        IF l_msg IS INITIAL.
          "Bapi to create Delivery alone
          CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
            IMPORTING
              delivery          = l_delivery
            TABLES
              sales_order_items = lt_salesitem
              return            = lt_return.
          IF l_delivery IS NOT INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            delivery_no = l_delivery.
          ELSE.
            REFRESH: return.
            LOOP AT lt_return INTO lw_return WHERE ( type = 'E' OR type = 'A' ).
              CLEAR l_msg.
              CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                EXPORTING
                  msgid               = lw_return-id
                  msgnr               = lw_return-number
                  msgv1               = lw_return-message_v1
                  msgv2               = lw_return-message_v2
                  msgv3               = lw_return-message_v3
                  msgv4               = lw_return-message_v4
                IMPORTING
                  message_text_output = l_msg.
              msg = |{ msg },{ l_msg }|.
            ENDLOOP.
          ENDIF.
        ELSE.
          msg = l_msg.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD digital_sign.
    TYPES: BEGIN OF ty_output,
             pdf    TYPE fpcontent,
             status TYPE string,
             error  TYPE string,
           END OF ty_output.
    DATA: lo_http_client TYPE REF TO if_http_client.
    DATA: lv_url TYPE string.
    DATA: lv_response   TYPE string, "API Response
          lv_codes      TYPE i,      "STATUS Code
          lv_http_error TYPE string. "STATUS Description
    DATA: lv_response1 TYPE string.
    DATA: lv_date(10) TYPE c.
    DATA: lv_pdf TYPE fpcontent.
    DATA: lt_output TYPE STANDARD TABLE OF ty_output.
    IF lv_pdf  IS INITIAL.

      CLEAR lv_url.

      SELECT SINGLE low FROM tvarvc
        INTO @DATA(l_url)
        WHERE name = 'ZANS_MIGO_URL'
        AND   type = 'P'.

      DATA : lv_bearer_token TYPE string.
      DATA : lv_msg TYPE string.
      lv_url = 'http://103.181.108.169:81/SignPDF_Base64String'.
*        lv_url = l_url.

*  lv_url = 'https://webdevqas.sheenlac.com:44300/sap/zapi_service/ZMM_ASN_TEST?sap-client=500'. " SAP System
      cl_http_client=>create_by_url(
        EXPORTING
        url = lv_url
        IMPORTING
        client = lo_http_client
        EXCEPTIONS
        argument_not_found = 1
        plugin_not_active = 2
        internal_error = 3
        OTHERS = 4 ).

      CHECK lo_http_client IS BOUND.
      lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.


      lo_http_client->request->set_method(
       EXPORTING
       method = if_http_entity=>co_request_method_post ).


      lo_http_client->request->set_content_type(
       EXPORTING
       content_type = if_rest_media_type=>gc_appl_json ).

      lo_http_client->request->set_cdata(
        EXPORTING
        data = '{ JSON_Payload }' ).

*        lv_bearer_token = |Bearer { lv_bearer_token }|.
      lv_bearer_token = 'Bearer UnNjI0AxIWVSMDk0NDUkQHN2YjpzY0VSTDBAIUdAY3ZydGN4Ug=='.

      lo_http_client->request->set_header_field(  EXPORTING  name  = 'Authorization'  value = CONV string( lv_bearer_token ) ).

      DATA : v_jsonload TYPE string.
      DATA lv_string TYPE string.
      lv_string = pdf.
      CONCATENATE '{'
                  '"pdfByte1":"' lv_string '",'
                  '"AuthorizedSignatory":"Webtel",'
                  '"SignerName":"Sanjana",'
                  '"TopLeft":"0",'
                  '"BottomLeft":"0",'
                  '"TopRight":"0",'
                  '"BottomRight":"0",'
                  '"FindAuth":"For SHEENLAC PAINTS LIMITED",'
                  '"FindAuthLocation":"1"'
                  '}' INTO v_jsonload.

      CONDENSE v_jsonload NO-GAPS.

      lo_http_client->request->set_cdata(
      EXPORTING
       data = v_jsonload ).

      lo_http_client->send(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2 ).


      CHECK sy-subrc = 0.
      lo_http_client->receive(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2
       http_processing_failed = 3 ).


      lo_http_client->response->get_status(
      IMPORTING
        code = lv_codes ).

      lo_http_client->response->get_status(
      IMPORTING
        reason = lv_http_error ).

      CLEAR lv_response.
      IF lv_codes = 200.
        lv_response = lo_http_client->response->get_cdata( ).
        REPLACE ALL OCCURRENCES OF 'u0022'   IN lv_response WITH '"'.
        REPLACE ALL OCCURRENCES OF '\'  IN lv_response WITH ''.
        REPLACE ALL OCCURRENCES OF 'rn'  IN lv_response WITH ''.
*REPLACE ALL OCCURRENCES OF '""'  IN lv_response WITH ''.
        REPLACE ALL OCCURRENCES OF '/'  IN lv_response WITH ''.
        REPLACE ALL OCCURRENCES OF '[]'  IN lv_response WITH ''.



        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        CHANGING
         data         = lt_output ).

        LOOP AT lt_output ASSIGNING FIELD-SYMBOL(<fs_out>).
          sign_pdf = <fs_out>-pdf.
        ENDLOOP.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD financing.
* Created By: Samsudeen M
* Created On: 31.08.2023
* Purpose : To Store the Invoice in the Financing table if it is allowed for
*           financing
*Reference: Ramakrishnan J

    DATA: lv_amt TYPE netwr,
          lv_utc TYPE string.

    IF ls_vbrk IS NOT INITIAL.
      SELECT SINGLE * FROM kna1 INTO @DATA(l_kna1)
        WHERE kunnr = @ls_vbrk-kunag
        AND zcustype NE @space.
      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM t005u WHERE  land1 = 'IN'
                                   AND    bland = @l_kna1-regio INTO @DATA(ls_regio).
      ENDIF.

*to get pan no from a different table
      SELECT SINGLE * FROM zsd_cus_pan INTO @DATA(ls_sdpan) WHERE cus_no = @ls_vbrk-kunag.
      IF sy-subrc = 0.

      ENDIF.
**      " Check Custmer Active in CF Table
      SELECT SINGLE * FROM zsd_cf_activate INTO @DATA(lw_cf) WHERE kunnr = @ls_vbrk-kunag.
      IF lw_cf-active IS NOT INITIAL .
        IF lw_cf-apprl = abap_true.
          DATA(l_status)  = '99'. " Pre Approval
        ELSE.
          l_status  = '10'.
        ENDIF.
        IF ls_vbrk IS NOT INITIAL AND ls_vbrk-fkart EQ 'YBBR' AND l_kna1 IS NOT INITIAL.
          DATA(lv_key) = CONV char18( |{ ls_vbrk-vbeln }{ ls_vbrk-bukrs }{ ls_vbrk-fkdat+0(4) }| ).
          SHIFT lv_key LEFT DELETING LEADING '0'.
          CLEAR lv_amt.
          lv_amt = ls_vbrk-netwr + ls_vbrk-mwsbk.

***************UTC conversion*****************
          CLEAR : lv_utc.
          lv_utc = |{ ls_vbrk-fkdat(4) }-{ ls_vbrk-fkdat+4(2) }-{ ls_vbrk-fkdat+6(2) }T{ sy-uzeit(2) }:{ sy-uzeit+2(2) }:{ sy-uzeit+4(2) }.000Z|.

          IF booster = abap_false.
            DATA(l_fintable) = VALUE zsd_sf_cust_inv( mandt         = sy-mandt
                                                      invoicekey    = lv_key
                                                      bukrs         = ls_vbrk-bukrs
                                                      status        = l_status
                                                      fintype       = l_kna1-zcustype
                                                      custno        = l_kna1-kunnr
                                                      custname      = l_kna1-name1
                                                      invoiceno     = ls_vbrk-vbeln
                                                      invoicedate   = ls_vbrk-fkdat
                                                      gjahr         = ls_vbrk-gjahr
                                                      invoicetype   = ls_vbrk-fkart
                                                      createddate   = sy-datum
                                                      createdtime   = sy-uzeit
                                                      createdby     = sy-uname
                                                      basicamount   = lv_amt
                                                      invoiceamount = lv_amt
                                                      duedate       = ls_vbrk-fkdat + 30
                                                      dueamount     = lv_amt
                                                      gstn          = l_kna1-stcd3
                                                      pan           = ls_sdpan-pan_no
                                                      partycode     = l_kna1-zfincode
                                                      invstatus     = 'INV'
                                                      amt_bef_tax   = ls_vbrk-netwr
                                                      disbstatus    = '10'
                                                      street        = l_kna1-stras
                                                      city1         = l_kna1-ort01
                                                      region        = l_kna1-regio
                                                      bezei         = ls_regio-bezei
                                                      landmark      = l_kna1-name1
                                                      post_code1    = l_kna1-pstlz
                                                      country       = l_kna1-land1
                                                      inv_date_tstp = lv_utc ).
            MODIFY zsd_sf_cust_inv FROM l_fintable.
          ELSE.
            DATA(l_booster) = VALUE zsd_sf_cus_inv_b( mandt         = sy-mandt
                                            invoicekey    = lv_key
                                            bukrs         = ls_vbrk-bukrs
                                            status        = l_status
                                            fintype       = l_kna1-zcustype
                                            custno        = l_kna1-kunnr
                                            custname      = l_kna1-name1
                                            invoiceno     = ls_vbrk-vbeln
                                            invoicedate   = ls_vbrk-fkdat
                                            gjahr         = ls_vbrk-gjahr
                                            invoicetype   = ls_vbrk-fkart
                                            createddate   = sy-datum
                                            createdtime   = sy-uzeit
                                            createdby     = sy-uname
                                            basicamount   = lv_amt
                                            invoiceamount = lv_amt
                                            duedate       = ls_vbrk-fkdat + 30
                                            dueamount     = lv_amt
                                            gstn          = l_kna1-stcd3
                                            pan           = ls_sdpan-pan_no
                                            partycode     = l_kna1-zfincode
                                            invstatus     = 'INV'
                                            amt_bef_tax   = ls_vbrk-netwr
                                            disbstatus    = '10'
                                            street        = l_kna1-stras
                                            city1         = l_kna1-ort01
                                            region        = l_kna1-regio
                                            bezei         = ls_regio-bezei
                                            landmark      = l_kna1-name1
                                            post_code1    = l_kna1-pstlz
                                            country       = l_kna1-land1
                                            inv_date_tstp = lv_utc ).
            MODIFY zsd_sf_cus_inv_b FROM l_booster.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD mail_sent.

    DATA: lv_ind TYPE char10.
    DATA : gt_soli_tab      TYPE soli_tab, " Email Body
           gs_soli_tab_line TYPE LINE OF soli_tab.

*Header Details
    SELECT SINGLE * FROM vbrk INTO @DATA(ls_vbrk)
      WHERE vbeln = @vbeln
      AND gjahr = @gjahr.
    IF sy-subrc = 0.
      DATA(lv_vbeln) = ls_vbrk-vbeln .
      DATA(lv_netwr) = ls_vbrk-netwr.
      DATA(lv_mwsbk) = ls_vbrk-mwsbk.
      WRITE ls_vbrk-fkdat TO lv_ind DD/MM/YYYY.
      DATA(lv_netwr1) = CONV vbrk-netwr( lv_netwr + lv_mwsbk ).
      "If dev or Qas for Internal Mail ID
      IF sy-sysid = 'DEV' OR sy-sysid = 'QAS'.
        SELECT SINGLE low FROM tvarvc INTO @DATA(ls_tvarvc)
                                      WHERE name = 'ZRLB_INVOICE'
                                      AND type = 'P'.
        DATA(lv_mail) = CONV ad_smtpadr( ls_tvarvc ).
      ELSE.
        SELECT SINGLE adrnr INTO @DATA(lv_adr) FROM kna1 WHERE kunnr = @ls_vbrk-kunag.
        SELECT SINGLE smtp_addr INTO lv_mail FROM adr6 WHERE addrnumber = lv_adr .
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM vbrp INTO @DATA(ls_vbrp)
      WHERE vbeln = @vbeln.
    IF sy-subrc = 0.
      DATA(lv_plant) = ls_vbrp-werks.
      SELECT SINGLE name1 FROM t001w INTO @DATA(lv_pname) WHERE werks = @lv_plant .
    ENDIF.

    CLEAR: gt_soli_tab,gs_soli_tab_line.
    CASE ls_vbrk-vkorg.
*Sales Organisation '4000'
      WHEN '4000'.

        DATA(lv_sender) = CONV ad_smtpadr( 'CCC@JNPL.IN' ).

        IF ls_vbrk-fkart EQ 'YBBR' .
          DATA(lv_tot) = |JENSON & NICHOLSON Invoice No :{ lv_vbeln } on Date:{ lv_ind } for Value { lv_netwr1 }|.

          gs_soli_tab_line = 'Dear Customer,' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = '           ' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = '          With reference to your order, the above invoice has been generated at our end.'.
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = '                                  ' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = 'If you have any queries / doubts, kindly contact us on  8300030505 (or) send us the mail to CCC@jnpl.in'.
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = '                                                                                          ' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = '                                                                                          ' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = 'Thanks & Best Regards,' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = 'Customer Care (Jenson and Nicholson)' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

        ENDIF.
*Sales Organization '1000'
      WHEN '1000'.

        lv_sender = 'customerfeedback@sheenlac.in'.

        IF ls_vbrk-fkart EQ 'YBBR' OR ls_vbrk-fkart EQ 'YRF2'
           OR ls_vbrk-fkart EQ 'YBTE' OR ls_vbrk-fkart EQ 'YBFS' .
          lv_tot = |Sheenlac Paints Limited Invoice No :{ lv_vbeln }|.
          gs_soli_tab_line = 'Dear Customer,' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = '           ' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = CONV string( |With reference to your order, the above invoice has been generated at our end On Date:{ lv_ind } for Value:{ lv_netwr1 }| ).
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = '                                  ' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = 'If you have any queries / doubts, kindly contact us on 8300030404 (or) send us the mail to ccchennai@sheenlac.in'.
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = '                                                                                          ' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = '                                                                                          ' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = 'Thanks & Best Regards,' .
          APPEND gs_soli_tab_line TO gt_soli_tab.

          gs_soli_tab_line = 'Customer Care (Sheenlac Paints Ltd)' .
          APPEND gs_soli_tab_line TO gt_soli_tab.
        ENDIF.
    ENDCASE.
*Functio Module to send Mail
    IF ls_vbrk-fkart NE 'YBDP'.
      CALL FUNCTION 'EFG_GEN_SEND_EMAIL'
        EXPORTING
          i_title       = lv_tot
          i_sender      = lv_sender
          i_recipient   = lv_mail
        TABLES
          i_tab_lines   = gt_soli_tab
        EXCEPTIONS
          not_qualified = 1
          failed        = 2
          OTHERS        = 3.
      IF sy-subrc = 0.
        mail_sent = abap_true.
      ENDIF.
    ELSE.
**only for YBDP Sales *
*      CALL FUNCTION 'ZINVOICE_MAIL_WITH_PDF'
*        EXPORTING
*          vbeln     = vbeln
*          gjahr     = ls_vbrk-gjahr
*        IMPORTING
*          mail_sent = mail_sent.
    ENDIF.

  ENDMETHOD.


  METHOD post_goods_issue.

    DATA: ls_komph   TYPE komph,
          ls_bdcom   TYPE bdcom,
          lt_bdbatch TYPE STANDARD TABLE OF bdbatch INITIAL SIZE 0.
    DATA: i_vbkok   TYPE vbkok,
          lt_prot   TYPE STANDARD TABLE OF prott,
          vbpok_tab TYPE vbpok,
          lt_vbpok  TYPE STANDARD TABLE OF vbpok,
          lw_return	TYPE bapiret2.
    DATA: l_msg TYPE string.

    SELECT  vbeln,
            posnr,
            matnr,
            werks,
            lfimg,
            meins,
            arktx,
            vgbel,
            vgpos,
            bwart
            INTO TABLE @DATA(gt_deliv_item)
            FROM lips
            WHERE vbeln = @delivery_no AND
                  bwart = '601' AND "Movement type
                  lgort NE @space AND
                  charg NE @space AND
                  lfimg NE 0.
    IF sy-subrc NE 0.
      lw_return-type         = 'E'.
      lw_return-id           = 'ZSD'.
      lw_return-number       = '006'.
      lw_return-message_v1 = delivery_no.
      APPEND lw_return TO return.
    ELSE.
      LOOP AT gt_deliv_item INTO DATA(itab).

        i_vbkok-vbeln_vl = itab-vbeln.
        i_vbkok-vbeln = itab-vbeln.
        i_vbkok-wabuc    = 'X'.       "<- automatic pgi


        vbpok_tab-vbeln_vl = itab-vbeln.
        vbpok_tab-posnr_vl = itab-posnr.
        vbpok_tab-vbeln = itab-vbeln.
        vbpok_tab-posnn = itab-posnr.
        vbpok_tab-matnr = itab-matnr.
        vbpok_tab-werks = itab-werks.
* VBPOK_TAB-LIANP = 'X'.
        vbpok_tab-pikmg = itab-lfimg.
        vbpok_tab-ndifm = itab-lfimg.
        vbpok_tab-lgort = lgort.
        APPEND vbpok_tab TO lt_vbpok.


      ENDLOOP.
      IF sy-subrc = 0.
        CALL FUNCTION 'WS_DELIVERY_UPDATE'
          EXPORTING
            vbkok_wa                 = i_vbkok
            synchron                 = 'X'
            no_messages_update       = ' '
            commit                   = 'X'
            delivery                 = delivery_no
            update_picking           = 'X'
            nicht_sperren            = 'X'
            if_database_update       = '1'
            if_error_messages_send_0 = 'X'
          TABLES
            vbpok_tab                = lt_vbpok
            prot                     = lt_prot.
        LOOP AT lt_prot INTO DATA(lw_prot) WHERE msgty = 'A' OR msgty = 'E'.
          CLEAR l_msg.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = lw_prot-msgid
              msgnr               = lw_prot-msgno
              msgv1               = lw_prot-msgv1
              msgv2               = lw_prot-msgv2
              msgv3               = lw_prot-msgv3
              msgv4               = lw_prot-msgv3
            IMPORTING
              message_text_output = l_msg.
          msg = |{ msg },{ l_msg }|.
        ENDLOOP.
        IF sy-subrc <> 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.
      ENDIF.

      SELECT SINGLE mblnr FROM mseg
        INTO pgi_no
        WHERE vbeln_im = delivery_no.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
