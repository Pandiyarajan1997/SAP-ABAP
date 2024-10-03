class ZCL_API_VEN_BUYERLOCATIONS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_BUYERLOCATIONS IMPLEMENTATION.


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
    TYPES : BEGIN OF ty_t001w,
              werks TYPE t001w-werks,
              adrnr TYPE t001w-adrnr,
            END OF ty_t001w.
    TYPES : BEGIN OF ty_adrc,
              addrnumber TYPE adrc-addrnumber,
              name1      TYPE adrc-name1,
              house_num1 TYPE adrc-house_num1,
              street     TYPE adrc-street,
              city1      TYPE adrc-city1,
              region     TYPE adrc-region,
              country    TYPE adrc-country,
              post_code1 TYPE adrc-post_code1,
              tel_number TYPE adrc-tel_number,
            END OF ty_adrc.
    TYPES : BEGIN OF ty_final,
              werks      TYPE t001w-werks,
              adrnr      TYPE t001w-adrnr,
              name1      TYPE adrc-name1,
              house_num1 TYPE adrc-house_num1,
              street     TYPE adrc-street,
              city1      TYPE adrc-city1,
              region     TYPE adrc-region,
              country    TYPE adrc-country,
              post_code1 TYPE adrc-post_code1,
              tel_number TYPE adrc-tel_number,
            END OF ty_final.
    DATA: ls_t001w TYPE TABLE OF ty_t001w,
          lw_t001w TYPE ty_t001w,
          ls_adrc  TYPE TABLE OF ty_adrc,
          lw_adrc  TYPE ty_adrc,
          it_final TYPE TABLE OF ty_final,
          wa_final TYPE ty_final,
          lt_list  TYPE TABLE OF bapiorders,
          ls_list  TYPE bapiorders,
          xml_out  TYPE string.
    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    TYPES : BEGIN OF t_body,
              locitem TYPE string,
            END OF t_body.
    DATA: it_loc_body TYPE STANDARD TABLE OF t_body,
          wa_loc_body TYPE t_body.

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
      werks
      adrnr
  INTO TABLE ls_T001W FROM t001w WHERE ekorg IN ('1000','1400').

* Get the Material details
    IF ls_t001w IS NOT INITIAL.
      SELECT
        addrnumber
        name1
        house_num1
        street
        city1
        region
        country
        post_code1
        tel_number
         INTO TABLE ls_adrc FROM adrc FOR ALL ENTRIES IN ls_t001w WHERE addrnumber = ls_t001w-adrnr.
    ENDIF.


    LOOP AT ls_t001w INTO lw_t001w.
      READ TABLE ls_adrc INTO lw_adrc WITH KEY addrnumber = lw_t001w-adrnr.
      wa_final-werks = lw_t001w-werks.
      wa_final-adrnr = lw_t001w-adrnr.
      wa_final-name1 = lw_adrc-name1.
      wa_final-house_num1 = lw_adrc-house_num1.
      wa_final-street = lw_adrc-street.
      wa_final-city1 = lw_adrc-city1.
      wa_final-region = lw_adrc-region.
      wa_final-country = lw_adrc-country.
      wa_final-post_code1 = lw_adrc-post_code1.
      wa_final-tel_number = lw_adrc-tel_number.
      APPEND wa_final TO it_final.
      CLEAR: wa_final,lw_adrc,lw_t001w.
    ENDLOOP.

    LOOP AT it_final INTO wa_final.
      CONCATENATE
      '{'
      '"code":'       c_quatation_mark wa_final-werks c_quatation_mark c_comma
      '"name":'       c_quatation_mark wa_final-name1 c_quatation_mark c_comma
      '"address":'    c_quatation_mark wa_final-house_num1 c_comma wa_final-street c_comma wa_final-city1 c_comma wa_final-region c_comma wa_final-country c_comma wa_final-post_code1 c_quatation_mark c_comma
      '"email":'      c_quatation_mark '-' c_quatation_mark c_comma
      '"phone":'      c_quatation_mark wa_final-tel_number c_quatation_mark c_comma
      '"mobile":'     c_quatation_mark wa_final-tel_number c_quatation_mark c_comma
      '"gstin":'      c_quatation_mark '-' c_quatation_mark c_comma
      '"state_code":' c_quatation_mark wa_final-region c_quatation_mark c_comma
      '"status":'     c_quatation_mark '-' c_quatation_mark" c_comma
      '}'
      INTO wa_loc_body-locitem.
      APPEND wa_loc_body TO it_loc_body.
      CLEAR : wa_loc_body.
    ENDLOOP.

    DATA: lv_tabix  TYPE sy-tabix,
          lv_int(6) TYPE n.
    lv_int = 0.
    lv_int = lines( it_loc_body ).

    LOOP AT it_loc_body INTO wa_loc_body.
      lv_tabix = sy-tabix.
      IF lv_tabix < lv_int.
        CONCATENATE ls_json wa_loc_body-locitem ',' INTO ls_json.
      ELSE.
        CONCATENATE ls_json wa_loc_body-locitem  INTO ls_json.
      ENDIF.
    ENDLOOP.
    CLEAR: lv_int,lv_tabix.

    CONCATENATE '[' ls_json ']'  INTO v_jsonload.

*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata(
        data = v_jsonload ).
  ENDMETHOD.
ENDCLASS.
