class ZCL_API_VEN_VENDOR definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_VENDOR IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: date_frm TYPE string,
          date_to  TYPE string.
    DATA: lv_path_info      TYPE string,
          lv_request_method TYPE string,
          lv_response_data  TYPE string.
    DATA: it_inputparams TYPE tihttpnvp,
          ls_inputparams TYPE LINE OF tihttpnvp.
    TYPES : BEGIN OF gs_cdhdr,
              objectclas TYPE cdhdr-objectclas,
              objectid   TYPE cdhdr-objectid,
              changenr   TYPE cdhdr-changenr,
              username   TYPE cdhdr-username,
              udate      TYPE cdhdr-udate,
              utime      TYPE cdhdr-utime,
              tcode      TYPE cdhdr-tcode,
            END OF gs_cdhdr.
    TYPES:BEGIN OF ty_ADR6,
            addrnumber TYPE adr6-addrnumber,
            smtp_addr  TYPE adr6-smtp_addr,
          END OF ty_ADR6.
    TYPES:BEGIN OF ty_LFA1,
            ktokk TYPE lfa1-ktokk,
            lifnr TYPE lfa1-lifnr,
            name1 TYPE lfa1-name1,
            stras TYPE lfa1-stras,
            ort02 TYPE lfa1-ort02,
            mcod3 TYPE lfa1-mcod3,
            land1 TYPE lfa1-land1,
            adrnr TYPE lfa1-adrnr,
            telf1 TYPE lfa1-telf1,
            telf2 TYPE lfa1-telf2,
            regio TYPE lfa1-regio,
            pstlz TYPE lfa1-pstlz,
            stcd3 TYPE lfa1-stcd3,
          END OF ty_LFA1.
    TYPES:BEGIN OF ty_final,
            ktokk TYPE string,
            lifnr TYPE string,
            name1 TYPE string,
            stras TYPE string,
            land1 TYPE string,
            ort02 TYPE string,
            mcod3 TYPE string,
            adrnr TYPE string,
            telf1 TYPE string,
            telf2 TYPE string,
            regio TYPE string,
            pstlz TYPE string,
            stcd3 TYPE string,
          END OF ty_final.

    CONSTANTS : c_comma                TYPE c VALUE ',',
                c_curly_brackets_open  TYPE char01 VALUE '{',
                c_curly_brackets_close TYPE char01 VALUE '}',
                c_quatation_mark       TYPE char01 VALUE '"',
                c_colon                TYPE char01 VALUE':'.
    DATA : gt_cdhdr TYPE TABLE OF gs_cdhdr,
           wa_cdhdr TYPE gs_cdhdr,
           it_lfa1  TYPE TABLE OF ty_lfa1,
           wa_lfa1  TYPE ty_lfa1,
           it_final TYPE TABLE OF ty_final,
           wa_final TYPE ty_final,
           it_adr6  TYPE TABLE OF ty_adr6,
           wa_adr6  TYPE ty_adr6.

    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.
    TYPES : BEGIN OF t_body,
              Vendoritem TYPE string,
            END OF t_body.
    DATA: it_ven_body TYPE STANDARD TABLE OF t_body,
          wa_ven_body TYPE t_body.



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
    IF date_frm IS NOT INITIAL AND date_to IS NOT INITIAL.
      SELECT  objectclas objectid changenr username udate utime tcode FROM  cdhdr
      INTO TABLE gt_cdhdr
      WHERE objectclas = 'KRED'
      AND udate GE date_frm AND udate LE date_to.

    ELSE.
      SELECT  objectclas objectid changenr username udate utime tcode FROM  cdhdr
      INTO TABLE gt_cdhdr
      WHERE objectclas = 'KRED'.
    ENDIF.
    SORT gt_cdhdr ASCENDING BY objectid.

* Get the Change data details
    IF gt_cdhdr IS NOT INITIAL.
      SELECT ktokk
          lifnr
          name1
          stras
          ort02
          mcod3
          land1
          adrnr
          telf1
          telf2
          regio
          pstlz
          stcd3
          FROM lfa1 INTO TABLE it_lfa1 FOR ALL ENTRIES IN gt_cdhdr WHERE lifnr = gt_cdhdr-objectid+0(10) AND loevm <> 'X' AND ktokk = 'YB01'.
      SELECT addrnumber
             smtp_addr  FROM adr6 INTO TABLE it_adr6 FOR ALL ENTRIES IN it_lfa1 WHERE addrnumber = it_lfa1-adrnr.
    ENDIF.

    SORT it_lfa1 ASCENDING BY lifnr.
    SORT it_adr6 ASCENDING BY addrnumber.
    LOOP AT it_lfa1 INTO wa_lfa1.
      wa_final-ktokk = wa_lfa1-ktokk.
      wa_final-lifnr = wa_lfa1-lifnr.
      wa_final-name1 = wa_lfa1-name1.
      wa_final-stras = wa_lfa1-stras.
      wa_final-ort02 = wa_lfa1-ort02.
      wa_final-mcod3 = wa_lfa1-mcod3.
      wa_final-land1 = wa_lfa1-land1.
      READ TABLE it_adr6 INTO wa_adr6 WITH KEY addrnumber = wa_lfa1-adrnr.
      wa_final-adrnr = wa_adr6-smtp_addr." wa_lfa1-adrnr.
      wa_final-telf1 = wa_lfa1-telf1.
      wa_final-telf2 = wa_lfa1-telf2.
      wa_final-regio = wa_lfa1-regio.
      wa_final-pstlz = wa_lfa1-pstlz.
      wa_final-stcd3 = wa_lfa1-stcd3.
      APPEND wa_final TO it_final.
      CLEAR wa_final.
    ENDLOOP.

    LOOP AT it_final INTO wa_final.
      REPLACE '"' WITH '' INTO wa_final-stras.
      REPLACE '"' WITH '' INTO wa_final-mcod3.
      REPLACE '"' WITH '' INTO wa_final-ort02.
      CONCATENATE
          '{'
          '"vendor_type":'  c_quatation_mark wa_final-ktokk c_quatation_mark c_comma
          '"vendor_code":'  c_quatation_mark wa_final-lifnr c_quatation_mark c_comma   "SNLC001",
          '"vendor_name":'  c_quatation_mark wa_final-name1 c_quatation_mark c_comma   "Vendor Pvt Ltd - 1",
          '"address":'      c_quatation_mark wa_final-stras ',' wa_final-ort02 ',' wa_final-mcod3 c_quatation_mark c_comma   "NA",
          '"country":'      c_quatation_mark wa_final-land1 c_quatation_mark c_comma   "IN",
          '"email_Id":'     c_quatation_mark wa_final-adrnr c_quatation_mark c_comma   "ner_vendor1@test.com",
          '"phone_no":'     c_quatation_mark wa_final-telf1 c_quatation_mark c_comma   "NA",
          '"mobile":'       c_quatation_mark wa_final-telf2 c_quatation_mark c_comma   "NA",
          '"region":'       c_quatation_mark wa_final-regio c_quatation_mark c_comma   "NA",
          '"Pincode":'      c_quatation_mark wa_final-pstlz c_quatation_mark c_comma   "I",
          '"GSTIN":'        c_quatation_mark wa_final-stcd3 c_quatation_mark "c_comma
          '}'
          INTO wa_ven_body-vendoritem.
      APPEND wa_ven_body TO it_ven_body.
      CLEAR wa_ven_body.
    ENDLOOP.

    DATA: lv_tabix  TYPE sy-tabix,
          lv_int(6) TYPE n.
    lv_int = 0.
    lv_int = lines( it_ven_body ).

    LOOP AT it_ven_body INTO wa_ven_body.
      lv_tabix = sy-tabix.
      IF lv_tabix < lv_int.
        CONCATENATE ls_json wa_ven_body-vendoritem ',' INTO ls_json.
      ELSE.
        CONCATENATE ls_json wa_ven_body-vendoritem INTO ls_json.
      ENDIF.
    ENDLOOP.
    CLEAR: lv_int,lv_tabix.

    CONCATENATE '[' ls_json ']'  INTO v_jsonload.

*Set json Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = v_jsonload ).

  ENDMETHOD.
ENDCLASS.
