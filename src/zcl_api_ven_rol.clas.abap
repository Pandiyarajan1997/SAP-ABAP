class ZCL_API_VEN_ROL definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_ROL IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: date_frm TYPE string,
          date_to  TYPE string,
          plant    TYPE string.
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
    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    TYPES : BEGIN OF t_body,
              Rolitem TYPE string,
            END OF t_body.
    DATA: it_rol_body TYPE STANDARD TABLE OF t_body,
          wa_rol_body TYPE t_body.

    TYPES:BEGIN OF ty_marc,
            matnr TYPE marc-matnr,
            werks TYPE marc-werks,
            dismm TYPE marc-dismm,
            eisbe TYPE marc-eisbe,
            minbe TYPE marc-minbe,
            webaz TYPE marc-webaz,
          END OF ty_marc.
    TYPES:BEGIN OF ty_makt,
            matnr TYPE makt-matnr,
            maktx TYPE makt-maktx,
          END OF ty_makt.
    TYPES:BEGIN OF ty_LFA1,
            lifnr TYPE lfa1-lifnr,
            ktokk TYPE lfa1-ktokk,
          END OF ty_LFA1.
    TYPES:BEGIN OF ty_eina,
            lifnr TYPE lfa1-lifnr,
            matnr TYPE makt-matnr,
          END OF ty_eina.

    TYPES:BEGIN OF ty_mbew,
            matnr TYPE mbew-matnr,
            bwkey TYPE mbew-bwkey,
            lbkum TYPE mbew-lbkum,
          END OF ty_mbew.
    TYPES:BEGIN OF ty_final,
            matnr TYPE marc-matnr,
            werks TYPE marc-werks,
            dismm TYPE marc-dismm,
            eisbe TYPE string,
            minbe TYPE marc-minbe,
            webaz TYPE string,
            lbkum TYPE string,
          END OF ty_final.

    DATA:it_marc  TYPE TABLE OF ty_marc,
         wa_marc  TYPE ty_marc,
         it_mbew  TYPE TABLE OF ty_mbew,
         wa_mbew  TYPE ty_mbew,
         it_lfa1  TYPE TABLE OF ty_lfa1,
         wa_lfa1  TYPE ty_lfa1,
         it_eina  TYPE TABLE OF ty_eina,
         wa_eina  TYPE ty_eina,
         it_final TYPE TABLE OF ty_final,
         wa_final TYPE ty_final,
         lt_list  TYPE TABLE OF bapiorders,
         ls_list  TYPE bapiorders,
         xml_out  TYPE string.

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

    READ TABLE it_inputparams INTO ls_inputparams WITH KEY name = 'PLANT'.
    plant = ls_inputparams-value.
    CLEAR ls_inputparams.

    SELECT lifnr ktokk INTO TABLE it_lfa1 FROM lfa1 WHERE ktokk = 'YB01'.
    IF it_lfa1 IS NOT INITIAL.
      SELECT lifnr matnr INTO TABLE it_eina FROM eina FOR ALL ENTRIES IN it_lfa1 WHERE lifnr = it_lfa1-lifnr.
    ENDIF.

    IF it_eina IS NOT INITIAL.
      SELECT matnr werks dismm eisbe minbe webaz INTO TABLE it_marc FROM marc FOR ALL ENTRIES IN it_eina WHERE matnr = it_eina-matnr AND werks = plant .
    ENDIF.

    IF it_marc IS NOT INITIAL.
      SELECT matnr
       bwkey
       lbkum INTO TABLE it_mbew FROM mbew FOR ALL ENTRIES IN it_marc WHERE matnr = it_marc-matnr AND bwkey = it_marc-werks.
    ENDIF.
    SORT it_marc ASCENDING BY matnr werks.
    SORT it_mbew ASCENDING BY matnr bwkey.
    LOOP AT it_marc INTO wa_marc.
      READ TABLE it_mbew INTO wa_mbew WITH KEY matnr = wa_marc-matnr bwkey = wa_marc-werks.
      wa_final-matnr = wa_marc-matnr.
      wa_final-werks = wa_marc-werks.
      wa_final-webaz = wa_marc-webaz.
      IF wa_marc-dismm = 'ND'.
        wa_final-eisbe = ''.
      ELSEIF wa_marc-dismm = 'PD'.
        wa_final-eisbe = wa_marc-eisbe.
      ELSEIF wa_marc-dismm = 'V1' OR wa_marc-dismm = 'VB'.
        wa_final-eisbe = wa_marc-minbe.
      ENDIF.
      wa_final-lbkum = wa_mbew-lbkum.
      APPEND wa_final TO it_final.
      clear:wa_final,wa_mbew,wa_marc.
    ENDLOOP.

     LOOP AT it_final INTO wa_final.
      CONCATENATE
      '{'
      '"material_code":'     c_quatation_mark wa_final-matnr c_quatation_mark c_comma
      '"storage_location":'  c_quatation_mark wa_final-werks c_quatation_mark c_comma
      '"reorder_level" :'    c_quatation_mark wa_final-eisbe c_quatation_mark c_comma
      '"average_lead_time":' c_quatation_mark wa_final-webaz c_quatation_mark c_comma
      '"current_stock":'     c_quatation_mark  wa_final-lbkum c_quatation_mark "c_comma
      '}'
      INTO wa_rol_body-rolitem.
      APPEND wa_rol_body TO it_rol_body.
      CLEAR : wa_rol_body.
    ENDLOOP.

    DATA: lv_tabix  TYPE sy-tabix,
          lv_int(6) TYPE n.
    lv_int = 0.
    lv_int = lines( it_rol_body ).

    LOOP AT it_rol_body INTO wa_rol_body.
      lv_tabix = sy-tabix.
      IF lv_tabix < lv_int.
        CONCATENATE ls_json wa_rol_body-rolitem ',' INTO ls_json.
      ELSE.
        CONCATENATE ls_json wa_rol_body-rolitem INTO ls_json.
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
