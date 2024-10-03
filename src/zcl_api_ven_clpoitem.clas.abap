class ZCL_API_VEN_CLPOITEM definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_CLPOITEM IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: date_frm TYPE string,
          date_to  TYPE string.
    DATA: lv_path_info      TYPE string,
          lv_request_method TYPE string,
          lv_response_data  TYPE string.
    DATA: it_inputparams TYPE tihttpnvp,
          ls_inputparams TYPE LINE OF tihttpnvp.

    TYPES:BEGIN OF ty_ekpo_key,
            mandt TYPE ekpr-mandt,
            ebeln TYPE ekpo-ebeln,
            ebelp TYPE ekpo-ebelp,
          END OF ty_ekpo_key.

    DATA: lt_ekpo_key TYPE TABLE OF ty_ekpo_key,
          ls_ekpo_key TYPE ty_ekpo_key.

    TYPES:BEGIN OF ty_ekpo,
            ebeln TYPE ekpo-ebeln,
            ebelp TYPE ekpo-ebelp,
            matnr TYPE ekpo-matnr,
            elikz TYPE ekpo-elikz,
          END OF ty_ekpo.
    TYPES:BEGIN OF ty_final,
            ebeln TYPE ekpo-ebeln,
            ebelp TYPE ekpo-ebelp,
            matnr TYPE ekpo-matnr,
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


    DATA: lt_cdhdr TYPE TABLE OF cdhdr,
          ls_cdhdr TYPE cdhdr.

    DATA: lt_cdpos TYPE TABLE OF cdpos,
          ls_cdpos TYPE cdpos.


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

    SELECT * FROM cdhdr INTO TABLE lt_cdhdr
      WHERE objectclas = 'EINKBELEG'
        AND udate BETWEEN date_frm AND date_to
        AND tcode IN ('ME22N' , 'ME23N', 'MASS', 'MEMASSPO', 'ME29N').
    IF sy-subrc = 0.
      SELECT * FROM cdpos INTO TABLE lt_cdpos
        FOR ALL ENTRIES IN lt_cdhdr
        WHERE objectclas = lt_cdhdr-objectclas
          AND objectid   = lt_cdhdr-objectid
          AND changenr   = lt_cdhdr-changenr
          AND Tabname    = 'EKPO'
          AND fname      = 'ELIKZ'
          AND value_new  = 'X'.
      IF sy-subrc = 0.
        SORT lt_cdpos BY objectid.
        LOOP AT lt_cdpos INTO ls_cdpos.
          CLEAR ls_ekpo_key.
          MOVE ls_cdpos-tabkey TO ls_ekpo_key.
          APPEND ls_ekpo_key TO lt_ekpo_key.
        ENDLOOP.


      ENDIF.
    ENDIF.

    IF lt_cdpos[] IS NOT INITIAL.
      SELECT
        ebeln
        ebelp
        matnr
        INTO TABLE ls_ekpo FROM ekpo
        FOR ALL ENTRIES IN lt_ekpo_key
        WHERE ebeln = lt_ekpo_key-ebeln
          AND ebelp = lt_ekpo_key-ebelp.

      LOOP AT ls_ekpo INTO lw_ekpo.
        wa_final-ebeln = lw_ekpo-ebeln.
        wa_final-ebelp = lw_ekpo-ebelp.
        wa_final-matnr = lw_ekpo-matnr.
        APPEND wa_final TO it_final.
        CLEAR: wa_final,lw_ekpo.
      ENDLOOP.
    ENDIF.



    LOOP AT it_final INTO wa_final.
      CONCATENATE
      '{'
      '"po_number":'              c_quatation_mark wa_final-ebeln c_quatation_mark c_comma
      '"sap_item_id":'          c_quatation_mark wa_final-ebelp c_quatation_mark c_comma
      '"material_code" :'          c_quatation_mark wa_final-matnr c_quatation_mark
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

    IF ls_json IS INITIAL.
      ls_json = '{}'.
    ENDIF.

    CONCATENATE '[' ls_json ']'  INTO v_jsonload.

*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = v_jsonload ).

  ENDMETHOD.
ENDCLASS.
