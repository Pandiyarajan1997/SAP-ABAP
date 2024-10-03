class ZCL_API_VEN_POITEM definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_POITEM IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: date_frm TYPE string,
          date_to  TYPE string.
    DATA: lv_path_info      TYPE string,
          lv_request_method TYPE string,
          lv_response_data  TYPE string.
    DATA: it_inputparams TYPE tihttpnvp,
          ls_inputparams TYPE LINE OF tihttpnvp.

    TYPES:BEGIN OF ty_ekpo,
            ebeln TYPE ekpo-ebeln,
            ebelp TYPE ekpo-ebelp,
            matnr TYPE ekpo-matnr,
            menge TYPE ekpo-menge,
            meins TYPE ekpo-meins,
            netwr TYPE ekpo-netwr,
            netpr TYPE ekpo-netpr,
            elikz TYPE ekpo-elikz,
            loekz TYPE ekpo-loekz,
*            eeind TYPE eket-EINDT,
            retpo TYPE ekpo-retpo,
          END OF ty_ekpo.
    TYPES:BEGIN OF ty_final,
            ebeln   TYPE ekpo-ebeln,
            ebelp   TYPE ekpo-ebelp,
            matnr   TYPE ekpo-matnr,
            m_meins TYPE mara-meins,
            menge   TYPE string,
            meins   TYPE ekpo-meins,
            netwr   TYPE string,
            netpr   TYPE string,
            elikz   TYPE ekpo-elikz,
            loekz   TYPE ekpo-loekz,
            eeind   TYPE eket-eindt,
            retpo   TYPE ekpo-retpo,
          END OF ty_final.

    TYPES:BEGIN OF ty_mara,
            matnr TYPE ekpo-matnr,
            meins TYPE mara-meins,
          END OF ty_mara.

    TYPES:BEGIN OF ty_eket,
            ebeln TYPE eket-ebeln,
            ebelp TYPE eket-ebelp,
            eindt TYPE eket-eindt,
          END OF ty_eket.

    CONSTANTS : c_comma                TYPE c VALUE ',',
                c_curly_brackets_open  TYPE char01 VALUE '{',
                c_curly_brackets_close TYPE char01 VALUE '}',
                c_quatation_mark       TYPE char01 VALUE '"',
                c_colon                TYPE char01 VALUE':'.

    DATA: ls_ekpo  TYPE TABLE OF ty_ekpo,
          lw_ekpo  TYPE ty_ekpo,
          ls_eket  TYPE TABLE OF ty_eket,
          lw_eket  TYPE ty_eket,
          ls_mara  TYPE TABLE OF ty_mara,
          lw_mara  TYPE ty_mara,
          it_final TYPE TABLE OF ty_final,
          wa_final TYPE ty_final,
          lt_list  TYPE TABLE OF bapiorders,
          ls_list  TYPE bapiorders,
          xml_out  TYPE string.
    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    TYPES : BEGIN OF t_body,
              poitem TYPE string,
            END OF t_body.
    DATA: it_po_body TYPE STANDARD TABLE OF t_body,
          wa_po_body TYPE t_body.

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
      menge
      meins
      netwr
      netpr
      elikz
      loekz
*      eeind
      retpo
      INTO TABLE ls_ekpo FROM ekpo WHERE aedat GE date_frm AND aedat LE date_to AND pstyp IN ('0','3') AND elikz <> 'X'.

* Get the Material details
    IF ls_ekpo IS NOT INITIAL.
      SELECT matnr
             meins
         INTO TABLE ls_mara FROM mara FOR ALL ENTRIES IN ls_ekpo WHERE  matnr = ls_ekpo-matnr.
      SELECT ebeln ebelp eindt INTO TABLE ls_eket FROM eket FOR ALL ENTRIES IN ls_ekpo WHERE ebeln = ls_ekpo-ebeln AND ebelp = ls_ekpo-ebelp.
    ENDIF.


    LOOP AT ls_ekpo INTO lw_ekpo.
      READ TABLE ls_mara INTO lw_mara WITH KEY matnr = lw_ekpo-matnr.
      READ TABLE ls_eket INTO lw_eket WITH KEY ebeln = lw_ekpo-ebeln ebelp = lw_ekpo-ebelp.
      wa_final-ebeln = lw_ekpo-ebeln.
      wa_final-ebelp = lw_ekpo-ebelp.
      wa_final-matnr = lw_ekpo-matnr.
      wa_final-menge = lw_ekpo-menge.
      wa_final-meins = lw_ekpo-meins.
      wa_final-netwr = lw_ekpo-netwr.
      wa_final-netpr = lw_ekpo-netpr.
      wa_final-elikz = lw_ekpo-elikz.
      wa_final-loekz = lw_ekpo-loekz.
      wa_final-eeind = lw_eket-eindt.
      wa_final-retpo = lw_ekpo-retpo.
      wa_final-m_meins = lw_mara-meins.
      APPEND wa_final TO it_final.
      CLEAR: wa_final,lw_ekpo,lw_mara.
    ENDLOOP.

    LOOP AT it_final INTO wa_final.
      CONCATENATE
      '{'
      '"po_number":'              c_quatation_mark wa_final-ebeln c_quatation_mark c_comma
      '"sap_item_id":'          c_quatation_mark wa_final-ebelp c_quatation_mark c_comma
      '"material_code" :'          c_quatation_mark wa_final-matnr c_quatation_mark c_comma
      '"uom":'            c_quatation_mark wa_final-m_meins c_quatation_mark c_comma
      '"quantity":'       c_quatation_mark wa_final-menge c_quatation_mark c_comma
      '"purchasing_uom":'              c_quatation_mark wa_final-meins c_quatation_mark c_comma
      '"net_price":' c_quatation_mark wa_final-netwr c_quatation_mark c_comma
      '"price_unit":' c_quatation_mark wa_final-netpr c_quatation_mark c_comma
      '"delivery_indicator":' c_quatation_mark wa_final-elikz c_quatation_mark c_comma
      '"deletion":' c_quatation_mark wa_final-loekz c_quatation_mark c_comma
      '"delivery_date":' c_quatation_mark wa_final-eeind c_quatation_mark c_comma
      '"return_parameter":' c_quatation_mark wa_final-retpo c_quatation_mark c_comma
      '"po_line_number":'                   c_quatation_mark  '-' c_quatation_mark "c_comma
      '}'
      INTO wa_po_body-poitem.
      APPEND wa_po_body TO it_po_body.
      CLEAR : wa_po_body.
    ENDLOOP.

    DATA: lv_tabix  TYPE sy-tabix,
          lv_int(6) TYPE n.
    lv_int = 0.
    lv_int = lines( it_po_body ).

    LOOP AT it_po_body INTO wa_po_body.
      lv_tabix = sy-tabix.
      IF lv_tabix < lv_int.
        CONCATENATE ls_json wa_po_body-poitem ',' INTO ls_json.
      ELSE.
        CONCATENATE ls_json wa_po_body-poitem INTO ls_json.
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
