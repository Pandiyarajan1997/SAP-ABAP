class ZCL_API_VEN_GRN definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_GRN IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: date_frm TYPE string,
          date_to  TYPE string.
    DATA: lv_path_info      TYPE string,
          lv_request_method TYPE string,
          lv_response_data  TYPE string.
    DATA: it_inputparams TYPE tihttpnvp,
          ls_inputparams TYPE LINE OF tihttpnvp.
    CONSTANTS : c_comma                TYPE c VALUE ',',
                c_curly_brackets_open  TYPE char01 VALUE '{',
                c_curly_brackets_close TYPE char01 VALUE '}',
                c_quatation_mark       TYPE char01 VALUE '"',
                c_colon                TYPE char01 VALUE':'.

    TYPES : BEGIN OF ty_ekbe,
              ebeln TYPE ekbe-ebeln,
              ebelp TYPE ekbe-ebelp,
              matnr TYPE ekbe-matnr,
              lsmng TYPE ekbe-lsmng,
              charg TYPE ekbe-charg,
              lsmeh TYPE ekbe-lsmeh,
              budat TYPE ekbe-budat,
              cpudt TYPE ekbe-cpudt,
              xblnr TYPE ekbe-xblnr,
              belnr TYPE ekbe-belnr,
              gjahr TYPE ekbe-gjahr,
              buzei TYPE ekbe-buzei,
              shkzg TYPE ekbe-shkzg,
            END OF ty_ekbe.
    TYPES : BEGIN OF ty_final,
              belnr TYPE ekbe-belnr,
              lifnr TYPE mseg-lifnr,
              ebeln TYPE ekbe-ebeln,
              ebelp TYPE ekbe-ebelp,
              matnr TYPE ekbe-matnr,
              maktx TYPE makt-maktx,
              lsmng TYPE string,
              charg TYPE ekbe-charg,
              hsdat TYPE mseg-hsdat,
              lsmeh TYPE ekbe-lsmeh,
              budat TYPE ekbe-budat,
              cpudt TYPE ekbe-cpudt,
              xblnr TYPE ekbe-xblnr,
              shkzg TYPE ekbe-shkzg,
            END OF ty_final.
    TYPES : BEGIN OF ty_makt,
              matnr TYPE makt-matnr,
              maktx TYPE makt-maktx,
            END OF ty_makt.
    TYPES : BEGIN OF ty_mseg,
              lifnr TYPE mseg-lifnr,
              hsdat TYPE mseg-hsdat,
              belnr TYPE mseg-belnr,
              gjahr TYPE mseg-gjahr,
              buzei TYPE ekbe-buzei,
            END OF ty_mseg.
    TYPES :BEGIN OF ty_ekpo,
             ebeln TYPE ekbe-ebeln,
             ebelp TYPE ekbe-ebelp,
             matnr TYPE ekbe-matnr,
             txz01 TYPE ekpo-txz01,
           END OF ty_ekpo.
    TYPES:BEGIN OF ty_ekko,
            ebeln TYPE ekko-ebeln,
            lifnr TYPE ekko-lifnr,
          END OF ty_ekko.
    DATA: it_ekbe  TYPE TABLE OF ty_ekbe,
          wa_ekbe  TYPE ty_ekbe,
          it_makt  TYPE TABLE OF ty_makt,
          wa_makt  TYPE ty_makt,
          it_mseg  TYPE TABLE OF ty_mseg,
          wa_mseg  TYPE ty_mseg,
          it_ekpo  TYPE TABLE OF ty_ekpo,
          wa_ekpo  TYPE ty_ekpo,
          it_ekko  TYPE TABLE OF ty_ekko,
          wa_ekko  TYPE ty_ekko,
          it_final TYPE TABLE OF ty_final,
          wa_final TYPE ty_final,
          lt_list  TYPE TABLE OF bapiorders,
          ls_list  TYPE bapiorders,
          xml_out  TYPE string.
    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    TYPES : BEGIN OF t_body,
              grnitem TYPE string,
            END OF t_body.
    DATA: it_grn_body TYPE STANDARD TABLE OF t_body,
          wa_grn_body TYPE t_body.

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

    SELECT
      ebeln
      ebelp
      matnr
      lsmng
      charg
      lsmeh
      budat
      cpudt
      xblnr
      belnr
      gjahr
      buzei
      shkzg
      INTO TABLE  it_ekbe FROM ekbe WHERE budat GE date_frm AND budat LE date_to AND bwart = '101'.

    IF it_ekbe IS NOT INITIAL.
      SELECT ebeln ebelp matnr txz01 INTO TABLE it_ekpo FROM ekpo FOR ALL ENTRIES IN it_ekbe WHERE matnr = it_ekbe-matnr AND ebeln = it_ekbe-ebeln AND ebelp = it_ekbe-ebelp .
      SELECT ebeln lifnr INTO TABLE it_ekko FROM ekko FOR ALL ENTRIES IN it_ekbe WHERE ebeln = it_ekbe-ebeln ."AND bukrs IN ('1000','1400') AND bsart NOT IN ( 'ZUB','UB' ) .
*      SELECT matnr maktx INTO TABLE it_makt FROM makt FOR ALL ENTRIES IN it_ekbe WHERE matnr = it_ekbe-matnr.
      SELECT lifnr hsdat belnr gjahr buzei INTO TABLE it_mseg FROM mseg FOR ALL ENTRIES IN it_ekbe WHERE belnr = it_ekbe-belnr AND gjahr = it_ekbe-gjahr AND  buzei = it_ekbe-buzei+0(3).
    ENDIF.

    SORT it_ekbe ASCENDING BY matnr belnr gjahr buzei.
*    SORT it_makt ASCENDING BY matnr.
    SORT it_mseg ASCENDING BY belnr gjahr buzei.
    SORT it_ekko ASCENDING BY ebeln.
    SORT it_ekpo ASCENDING BY ebeln ebelp matnr.

    LOOP AT it_ekbe INTO wa_ekbe.
      READ TABLE it_mseg INTO wa_mseg WITH KEY belnr = WA_ekbe-belnr  gjahr = WA_ekbe-gjahr buzei = wa_ekbe-buzei.
      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ekbe-matnr.
      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_ekbe-ebeln.
      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekbe-ebeln ebelp = wa_ekbe-ebelp matnr = wa_ekbe-matnr.
      wa_final-belnr = wa_ekbe-belnr.
      wa_final-lifnr = wa_ekko-lifnr.
      wa_final-ebeln = wa_ekbe-ebeln.
      wa_final-ebelp = wa_ekbe-ebelp.
      wa_final-matnr = wa_ekbe-matnr.
      wa_final-maktx = wa_ekpo-txz01.
      wa_final-lsmng = wa_ekbe-lsmng.
      wa_final-charg = wa_ekbe-charg.
      wa_final-hsdat = wa_mseg-hsdat.
      wa_final-lsmeh = wa_ekbe-lsmeh.
      wa_final-budat = wa_ekbe-budat.
      wa_final-cpudt = wa_ekbe-cpudt.
      wa_final-xblnr = wa_ekbe-xblnr.
      wa_final-shkzg = wa_ekbe-shkzg.
      APPEND wa_final TO it_final.
      CLEAR: wa_final,wa_ekbe,wa_mseg,wa_makt.
    ENDLOOP.

    DELETE it_final WHERE lsmng = 0.
    LOOP AT it_final INTO wa_final.
      CONCATENATE
      '{'
      '"grpo_number":'          c_quatation_mark wa_final-belnr c_quatation_mark c_comma
      '"vendor_code":'           c_quatation_mark wa_final-lifnr c_quatation_mark c_comma
      '"purchase_order_number" :'      c_quatation_mark wa_final-ebeln c_quatation_mark c_comma
      '"base_po_line_number":'         c_quatation_mark wa_final-ebelp c_quatation_mark c_comma
      '"Item_code":'           c_quatation_mark wa_final-matnr c_quatation_mark c_comma
*      '"item_description":'            c_quatation_mark wa_final-maktx c_quatation_mark c_comma
      '"grn_quantity":'              c_quatation_mark wa_final-lsmng c_quatation_mark c_comma
      '"batch_no":'              c_quatation_mark wa_final-charg c_quatation_mark c_comma
      '"mfg_date":'              c_quatation_mark wa_final-hsdat c_quatation_mark c_comma
      '"uom":'              c_quatation_mark wa_final-lsmeh c_quatation_mark c_comma
      '"posting_date":'              c_quatation_mark wa_final-budat c_quatation_mark c_comma
      '"created_date":'              c_quatation_mark wa_final-cpudt c_quatation_mark c_comma
      '"vendor_invoice_number":' c_quatation_mark  wa_final-xblnr c_quatation_mark" c_comma
*      '"Debit/Credit Indicator":' c_quatation_mark  wa_final-shkzg c_quatation_mark
      '}'
      INTO wa_grn_body-grnitem.
      APPEND wa_grn_body TO it_grn_body.
      CLEAR : wa_grn_body.
    ENDLOOP.

    DATA: lv_tabix  TYPE sy-tabix,
          lv_int(6) TYPE n.
    lv_int = 0.
    lv_int = lines( it_grn_body ).

    LOOP AT it_grn_body INTO wa_grn_body.
      lv_tabix = sy-tabix.
      IF lv_tabix < lv_int.
        CONCATENATE ls_json wa_grn_body-grnitem ',' INTO ls_json.
      ELSE.
        CONCATENATE ls_json wa_grn_body-grnitem INTO ls_json.
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
