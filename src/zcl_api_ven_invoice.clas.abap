class ZCL_API_VEN_INVOICE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_INVOICE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: date_frm TYPE string,
          date_to  TYPE string.
    DATA: lv_path_info      TYPE string,
          lv_request_method TYPE string,
          lv_response_data  TYPE string.
    DATA: it_inputparams TYPE tihttpnvp,
          ls_inputparams TYPE LINE OF tihttpnvp.

    TYPES:BEGIN OF ty_rbkp,
            lifnr TYPE rbkp-lifnr,
            xblnr TYPE rbkp-xblnr,
            bldat TYPE rbkp-bldat,
            budat TYPE rbkp-budat,
            cpudt TYPE rbkp-cpudt,
            belnr TYPE rbkp-belnr,
            bukrs TYPE rbkp-bukrs,
            gjahr TYPE rbkp-gjahr,
          END OF ty_rbkp.
    TYPES:BEGIN OF ty_final,
            lifnr TYPE rbkp-lifnr,
            xblnr TYPE rbkp-xblnr,
            bldat TYPE rbkp-bldat,
            budat TYPE rbkp-budat,
            cpudt TYPE rbkp-cpudt,
            ebeln TYPE rseg-ebeln,
            belnr TYPE rbkp-belnr,
          END OF ty_final.

    TYPES:BEGIN OF ty_rseg,
            ebeln TYPE rseg-ebeln,
            belnr TYPE rbkp-belnr,
            bukrs TYPE rbkp-bukrs,
            gjahr TYPE rbkp-gjahr,
          END OF ty_rseg.
    TYPES:BEGIN OF ty_miro,
            belnr TYPE rbkp-belnr,
            xblnr TYPE rbkp-xblnr,
          END OF ty_miro.

    CONSTANTS : c_comma                TYPE c VALUE ',',
                c_curly_brackets_open  TYPE char01 VALUE '{',
                c_curly_brackets_close TYPE char01 VALUE '}',
                c_quatation_mark       TYPE char01 VALUE '"',
                c_colon                TYPE char01 VALUE':'.

    DATA: it_rbkp  TYPE TABLE OF ty_rbkp,
          wa_rbkp  TYPE ty_rbkp,
          ls_rseg  TYPE TABLE OF ty_rseg,
          lw_rseg  TYPE ty_rseg,
          it_miro  TYPE TABLE OF ty_miro,
          wa_miro  TYPE ty_miro,
          it_final TYPE TABLE OF ty_final,
          wa_final TYPE ty_final,
          lt_list  TYPE TABLE OF bapiorders,
          ls_list  TYPE bapiorders,
          xml_out  TYPE string.
    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    TYPES : BEGIN OF t_body,
              invoiceitem TYPE string,
            END OF t_body.
    DATA: it_inv_body TYPE STANDARD TABLE OF t_body,
          wa_inv_body TYPE t_body.

* get the request attributes
    lv_path_info = server->request->get_header_field( name = '~path_info' ).
    SHIFT lv_path_info LEFT BY 1 PLACES.

    FIELD-SYMBOLS <fs_param> TYPE LINE OF tihttpnvp.

*Get GET parameters
    CALL METHOD server->request->get_form_fields
      CHANGING
        fields = it_inputparams.
    UNASSIGN <fs_param>.
    LOOP AT it_inputparams ASSIGNING <fs_param>.
      TRANSLATE <fs_param>-name TO UPPER CASE.
    ENDLOOP.

    CLEAR: date_frm,ls_inputparams.

    READ TABLE it_inputparams INTO ls_inputparams WITH KEY name = 'DATE_FRM'.
    date_frm = ls_inputparams-value.
    CLEAR ls_inputparams.
    READ TABLE it_inputparams INTO ls_inputparams WITH KEY name = 'DATE_TO'.
    date_to = ls_inputparams-value.
    CLEAR ls_inputparams.
*    IF gt_cdhdr IS NOT INITIAL.
    SELECT lifnr
           xblnr
           bldat
           budat
           cpudt
           belnr
           bukrs
           gjahr
      INTO TABLE it_rbkp FROM rbkp WHERE cpudt GE date_frm AND cpudt LE date_to AND rbstat = '5'.
*    ENDIF.
* Get the Material details
    IF it_rbkp IS NOT INITIAL.
      SELECT ebeln
             belnr
             bukrs
             gjahr
         INTO TABLE ls_rseg FROM rseg FOR ALL ENTRIES IN it_rbkp WHERE  belnr = it_rbkp-belnr AND bukrs = it_rbkp-bukrs AND gjahr = it_rbkp-gjahr.
      SELECT belnr
             xblnr
              INTO TABLE it_miro FROM rbkp FOR ALL ENTRIES IN it_rbkp WHERE  stblg = ' ' AND bukrs = it_rbkp-bukrs AND gjahr = it_rbkp-gjahr..
    ENDIF.


    LOOP AT it_rbkp INTO wa_rbkp.
      READ TABLE it_miro INTO wa_miro WITH KEY xblnr = wa_rbkp-xblnr.
      READ TABLE ls_rseg INTO lw_rseg WITH KEY belnr = wa_rbkp-belnr bukrs = wa_rbkp-bukrs gjahr = wa_rbkp-gjahr.
      wa_final-lifnr = wa_rbkp-lifnr.
      wa_final-xblnr = wa_rbkp-xblnr.
      wa_final-bldat = wa_rbkp-bldat.
      wa_final-budat = wa_rbkp-budat.
      wa_final-cpudt = wa_rbkp-cpudt.
      wa_final-ebeln = lw_rseg-ebeln.
      wa_final-belnr = wa_miro-belnr.

      APPEND wa_final TO it_final.
      CLEAR: wa_final,wa_rbkp,lw_rseg,wa_miro.
    ENDLOOP.

    LOOP AT it_final INTO wa_final.
      CONCATENATE
      '{'
      '"vendor_code":'          c_quatation_mark wa_final-lifnr c_quatation_mark c_comma
      '"invoice_no":'           c_quatation_mark wa_final-xblnr c_quatation_mark c_comma
      '"invoice_date" :'        c_quatation_mark wa_final-bldat c_quatation_mark c_comma
      '"posting_date":'         c_quatation_mark wa_final-budat c_quatation_mark c_comma
      '"entry_date":'           c_quatation_mark wa_final-cpudt c_quatation_mark c_comma
      '"po_number":'            c_quatation_mark wa_final-ebeln c_quatation_mark c_comma
      '"miro_no":'              c_quatation_mark wa_final-belnr c_quatation_mark c_comma
      '"cancellation_indicator":' c_quatation_mark  '-' c_quatation_mark "c_comma
      '}'
      INTO wa_inv_body-invoiceitem.
      APPEND wa_inv_body TO it_inv_body.
      CLEAR : wa_inv_body.
    ENDLOOP.

    DATA: lv_tabix  TYPE sy-tabix,
          lv_int(6) TYPE n.
    lv_int = 0.
    lv_int = lines( it_inv_body ).

    LOOP AT it_inv_body INTO wa_inv_body.
      lv_tabix = sy-tabix.
      IF lv_tabix < lv_int.
        CONCATENATE ls_json wa_inv_body-invoiceitem ',' INTO ls_json.
      ELSE.
        CONCATENATE ls_json wa_inv_body-invoiceitem INTO ls_json.
      ENDIF.
    ENDLOOP.
    CLEAR: lv_int,lv_tabix.

    CONCATENATE '[' ls_json ']'  INTO v_jsonload.

*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = v_jsonload ).

  ENDMETHOD.
ENDCLASS.
