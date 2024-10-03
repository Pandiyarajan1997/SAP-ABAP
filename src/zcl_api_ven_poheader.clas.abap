class ZCL_API_VEN_POHEADER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_POHEADER IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: date_frm TYPE string,
          date_to  TYPE string.
    DATA: lv_path_info      TYPE string,
          lv_request_method TYPE string,
          lv_response_data  TYPE string.
    DATA: it_inputparams TYPE tihttpnvp,
          ls_inputparams TYPE LINE OF tihttpnvp.

    TYPES:BEGIN OF ty_ekko,
            ebeln TYPE ekko-ebeln,
            bedat TYPE ekko-bedat,
            aedat TYPE ekko-aedat,
            lifnr TYPE ekko-lifnr,
            zterm TYPE ekko-zterm,
          END OF ty_ekko.
    TYPES:BEGIN OF ty_final,
            ebeln      TYPE ekko-ebeln,
            bedat      TYPE ekko-bedat,
            aedat      TYPE ekko-aedat,
            lifnr      TYPE ekko-lifnr,
            zterm      TYPE ekko-zterm,
            werks      TYPE ekpo-werks,
            change_ind TYPE cdhdr-change_ind,
          END OF ty_final.

    TYPES:BEGIN OF ty_ekpo,
            ebeln TYPE ekko-ebeln,
            werks TYPE ekpo-werks,
          END OF ty_ekpo.
    TYPES:BEGIN OF ty_cdhdr,
            objectclas TYPE cdhdr-objectclas,
            objectid   TYPE cdhdr-objectid,
            change_ind TYPE cdhdr-change_ind,
          END OF ty_cdhdr.

    CONSTANTS : c_comma                TYPE c VALUE ',',
                c_curly_brackets_open  TYPE char01 VALUE '{',
                c_curly_brackets_close TYPE char01 VALUE '}',
                c_quatation_mark       TYPE char01 VALUE '"',
                c_colon                TYPE char01 VALUE':'.

    DATA: ls_ekko  TYPE TABLE OF ty_ekko,
          lw_ekko  TYPE ty_ekko,
          ls_ekpo  TYPE TABLE OF ty_ekpo,
          lw_ekpo  TYPE ty_ekpo,
          ls_cdhdr TYPE TABLE OF ty_cdhdr,
          lw_cdhdr TYPE ty_cdhdr,
          it_final TYPE TABLE OF ty_final,
          wa_final TYPE ty_final,
          lt_list  TYPE TABLE OF bapiorders,
          ls_list  TYPE bapiorders,
          xml_out  TYPE string.
    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    TYPES : BEGIN OF t_body,
              POheader TYPE string,
            END OF t_body.
    DATA: it_po_body TYPE STANDARD TABLE OF t_body,
          wa_po_body TYPE t_body.

    TYPES : BEGIN OF t_chg_body,
              obj_id TYPE cdhdr-objectid,
            END OF t_chg_body.
    DATA: it_chg_body TYPE STANDARD TABLE OF t_chg_body,
          wa_chg_body TYPE t_chg_body.

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
      bedat
      aedat
      lifnr
      zterm
      INTO TABLE ls_ekko FROM ekko WHERE aedat GE date_frm AND aedat LE date_to AND bukrs IN ('1000','1400') AND bsart NOT IN ( 'ZUB','UB' ) .

    LOOP AT ls_ekko INTO lw_ekko.
      wa_chg_body-obj_id = lw_ekko-ebeln.
      APPEND wa_chg_body TO it_chg_body.
      CLEAR : wa_chg_body.
    ENDLOOP.
* Get the Material details
    IF ls_ekko IS NOT INITIAL.
      SELECT ebeln
             werks
         INTO TABLE ls_ekpo FROM ekpo FOR ALL ENTRIES IN ls_ekko WHERE  ebeln = ls_ekko-ebeln.
      SELECT
        objectclas
        objectid
        change_ind
              INTO TABLE ls_cdhdr FROM cdhdr FOR ALL ENTRIES IN it_chg_body WHERE  objectclas = 'EINKBELEG' AND objectid  = it_chg_body-obj_id.
    ENDIF.


    LOOP AT ls_ekko INTO lw_ekko.
      READ TABLE ls_ekpo INTO lw_ekpo WITH KEY ebeln = lw_ekko-ebeln.
      READ TABLE ls_cdhdr INTO lw_cdhdr WITH KEY objectid = lw_ekko-ebeln.
      wa_final-ebeln = lw_ekko-ebeln.
      wa_final-bedat = lw_ekko-bedat.
      wa_final-aedat = lw_ekko-aedat.
      wa_final-lifnr = lw_ekko-lifnr.
      wa_final-zterm = lw_ekko-zterm.
      wa_final-werks = lw_ekpo-werks.
      wa_final-change_ind = lw_cdhdr-change_ind.

      APPEND wa_final TO it_final.
      CLEAR: wa_final,lw_ekko,lw_ekpo,lw_cdhdr.
    ENDLOOP.

    LOOP AT it_final INTO wa_final.
      CONCATENATE
      '{'
      '"po_number":'              c_quatation_mark wa_final-ebeln c_quatation_mark c_comma
      '"document_date":'          c_quatation_mark wa_final-bedat c_quatation_mark c_comma
      '"created_date" :'          c_quatation_mark wa_final-aedat c_quatation_mark c_comma
      '"vendor_code":'            c_quatation_mark wa_final-lifnr c_quatation_mark c_comma
      '"terms_of_payment":'       c_quatation_mark wa_final-zterm c_quatation_mark c_comma
      '"indicator":'              c_quatation_mark wa_final-change_ind c_quatation_mark c_comma
      '"delivery_location_code":' c_quatation_mark wa_final-werks c_quatation_mark c_comma
      '"text":'                   c_quatation_mark  '-' c_quatation_mark "c_comma
      '}'
      INTO wa_po_body-poheader.
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
        CONCATENATE ls_json wa_po_body-poheader ',' INTO ls_json.
      ELSE.
        CONCATENATE ls_json wa_po_body-poheader INTO ls_json.
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
