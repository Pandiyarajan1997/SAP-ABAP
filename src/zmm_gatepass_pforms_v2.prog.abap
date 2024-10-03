
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_LIFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_lifnr .
  DATA: ls_lfa1 TYPE lfa1 .
  DATA: lv_lifnr TYPE lfa1-lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = zmm_ge_asn-lifnr
    IMPORTING
      output = zmm_ge_asn-lifnr.

  SELECT SINGLE * FROM lfa1 INTO ls_lfa1 WHERE lifnr = zmm_ge_asn-lifnr  .
  IF sy-subrc = 0 .
    zmm_ge_asn-name1  = ls_lfa1-name1 .
    zmm_ge_asn-ort01  = ls_lfa1-ort01 .
    CLEAR ls_lfa1.
  ELSE.
    MESSAGE e001(zge) WITH zmm_ge_asn-lifnr.
  ENDIF .

  SELECT SINGLE lifnr
    FROM ekko
    INNER JOIN ekpo ON ( ekko~ebeln = ekpo~ebeln )
    INTO lv_lifnr
    WHERE ekko~lifnr = zmm_ge_asn-lifnr
    AND   ekko~frgke IN ('3', '')
    AND   ekpo~werks = p_werks.
  IF sy-subrc <> 0.
    MESSAGE e003(zge) WITH zmm_ge_asn-lifnr.
  ELSE.
    CLEAR lv_lifnr.
  ENDIF.
ENDFORM.                    " VALIDATE_LIFNR
FORM validate_asnno.
  SELECT SINGLE asnno
    FROM zmm_ge_asn INTO @DATA(l_asno)
    WHERE lifnr = @zmm_ge_asn-lifnr AND
          asnno = @zmm_ge_asn-asnno AND
          mis_update = 'X'.
  IF sy-subrc = 0.
    DATA(l_text) = | ASN No: { zmm_ge_asn-asnno } has been processed in MIS Portal and Only New ASN is allowed|.
    MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_VBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_vbeln.
  DATA: lv_mblnr TYPE mblnr,
        lv_vbeln TYPE zasninv.

  SHIFT  zmm_ge_asn-vbeln LEFT DELETING LEADING '' .
  SHIFT  zmm_ge_asn-vbeln LEFT DELETING LEADING '0' .

  SELECT mblnr vbeln UP TO 1 ROWS FROM zmm_ge_asn
    INTO (lv_mblnr,lv_vbeln) WHERE lifnr = zmm_ge_asn-lifnr AND
                                   vbeln = zmm_ge_asn-vbeln .
  ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
  IF sy-subrc = 0 .
    DATA(l_text) = |DC/INVOICE IS ALREADY USED in Table ZMM_GE_ASN (GRN No: { lv_mblnr })|.
    MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
    gv_inv_err = 'X'.
  ENDIF.
  CLEAR: lv_mblnr, lv_vbeln.
ENDFORM.                    " VALIDATE_VBELN
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_ddate .
  IF zmm_ge_asn-ddate > sy-datum .
    MESSAGE e002(zge).
  ENDIF .
ENDFORM.                    " VALIDATE_DDATE
*&---------------------------------------------------------------------*
*&      Form  GATEPASS_BACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gatepass_back .
  LEAVE TO SCREEN 100.
ENDFORM.                    " GATEPASS_BACK
*&---------------------------------------------------------------------*
*&      Form  GATEPASS_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gatepass_exit .
  IF sy-ucomm EQ 'BACK' OR sy-ucomm EQ 'EXIT' .
    LEAVE PROGRAM.
*    CALL SCREEN 100."LEAVE PROGRAM .
  ENDIF.

ENDFORM.                    " GATEPASS_EXIT


*&---------------------------------------------------------------------*
*& Form f_get_po_detail_from_mis_api
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_po_detail_from_mis_api .
  DATA: lo_http_client TYPE REF TO if_http_client.
  DATA: lv_url TYPE string.
  DATA: lv_response   TYPE string, "API Response
        lv_codes      TYPE i,      "STATUS Code
        lv_http_error TYPE string. "STATUS Description
  DATA: lv_response1 TYPE string.
  DATA: lv_date(10) TYPE c.
  DATA: lt_output TYPE STANDARD TABLE OF zmm_st_ge_asn.

  IF gt_output IS INITIAL.

    CLEAR lv_url.

    SELECT SINGLE low FROM tvarvc
      INTO @DATA(l_url)
      WHERE name = 'ZANS_MIGO_URL'
      AND   type = 'P'.

    DATA : lv_bearer_token TYPE string.
    DATA : lv_msg TYPE string.
    DATA : l_uname TYPE  syst_uname.
    DATA : l_password TYPE  char50.

    SELECT SINGLE low FROM tvarvc
      INTO l_password
      WHERE name = 'ZANS_UPD_PWORD'
      AND   type = 'P'.
    SELECT SINGLE low FROM tvarvc
      INTO l_uname
      WHERE name = 'ZANS_UPD_UNAME'
      AND   type = 'P'.

    CALL FUNCTION 'ZGET_BEARER_TOKEN_MIS'
      EXPORTING
        username          = l_uname
        password          = l_password
      IMPORTING
        bearer_token      = lv_bearer_token
        msg               = lv_msg
      EXCEPTIONS
        maintain_url_link = 1
        input_error       = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1. lv_msg = 'ULR Link Error'.
        WHEN 2. lv_msg = 'Invalid Input'.
        WHEN OTHERS. lv_msg = 'Unknown Error'.
      ENDCASE.
* Implement suitable error handling here
    ENDIF.
    IF lv_msg IS NOT INITIAL.
      DATA(l_txt) = |Get MIS ASN Error: { lv_msg }|.
      MESSAGE l_txt TYPE 'I' DISPLAY LIKE 'S'.
    ELSE.
*    lv_url = 'https://misportal.sheenlac.com:97/api/DistributorStock/GETASNDATA'.
*    lv_url = 'http://103.154.252.203:97/api/DistributorStock/GETASNDATA'. " MIS IP
      lv_url = l_url.

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

*    lo_http_client->authenticate(
*        EXPORTING
*         username = 'KPABAP'
*         password = 'Sheenlac@12' ).

      lo_http_client->request->set_method(
       EXPORTING
       method = if_http_entity=>co_request_method_post ).


      lo_http_client->request->set_content_type(
       EXPORTING
       content_type = if_rest_media_type=>gc_appl_json ).

      lo_http_client->request->set_cdata(
        EXPORTING
        data = '{ JSON_Payload }' ).

      lv_bearer_token = |Bearer { lv_bearer_token }|.
      lo_http_client->request->set_header_field(  EXPORTING  name  = 'Authorization'  value = CONV string( lv_bearer_token ) ).

      DATA : v_jsonload TYPE string.

*    CONCATENATE '{'
*                '"vendor":"'zmm_ge_asn-lifnr'"'
*                '}' INTO v_jsonload. " SAP System

      CONCATENATE '{'
                  '"filtervalue1":"'zmm_ge_asn-lifnr'"'
                  '}' INTO v_jsonload.
*    v_jsonload = | { '{' } { '"filtervalue1" :"' } { '0010000341"' }  { '}' }|. "0010002141"

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


*    CLEAR lv_response1.
*    CALL METHOD /ui2/cl_json=>serialize
*      EXPORTING
*        data        = lv_response
*        pretty_name = /ui2/cl_json=>pretty_mode-user
*      RECEIVING
*        r_json      = lv_response1.
*    .
*
*    REPLACE ALL OCCURRENCES OF '\'  IN lv_response1 WITH ''.
*    REPLACE ALL OCCURRENCES OF '/'  IN lv_response1 WITH ''.
*    SHIFT lv_response1 LEFT DELETING LEADING '"'.

        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        CHANGING
         data         = lt_output ).

*  ENDIF.
        DATA: lr_str  TYPE RANGE OF string,
              lwa_str LIKE LINE OF lr_str.
        CLEAR lr_str.

        DELETE lt_output WHERE nasnqty = 0.
**    DELETE lt_output WHERE asnno IN lr_str.
*    DATA: lt_mseg TYPE STANDARD TABLE OF gty_mseg.
        DATA l_matnr TYPE matnr18.
        LOOP AT lt_output ASSIGNING FIELD-SYMBOL(<fs_out>).
*          lwa_str-sign = 'I'.
*          lwa_str-option = 'CP'.
*          lwa_str-low = |{ <fs_out>-asnno }*|.
          SELECT SINGLE asnno FROM zmm_ge_asn
                              INTO @DATA(l_asn)
                              WHERE lifnr = @zmm_ge_asn-lifnr AND
                                    asnno = @<fs_out>-asnno.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
*          APPEND lwa_str TO lr_str.
          l_matnr = <fs_out>-cmaterialno.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_matnr
            IMPORTING
              output = l_matnr.
          <fs_out>-cmaterialno = l_matnr.
          MOVE-CORRESPONDING <fs_out> TO gs_output.
          APPEND gs_output TO gt_output.
        ENDLOOP.
*    IF lr_str IS NOT INITIAL.
*      SELECT * FROM zmm_ge_asn
*        INTO TABLE @DATA(lt_asn)
*        WHERE asnno IN @lr_str.
*      CLEAR lr_str.
*      SORT lt_asn BY asnno.
*      DELETE ADJACENT DUPLICATES FROM lt_asn COMPARING asnno.
*      LOOP AT lt_asn ASSIGNING FIELD-SYMBOL(<fs_asn>).
*        lwa_str-sign = 'I'.
*        lwa_str-option = 'CP'.
*        lwa_str-low = |{ <fs_asn>-asnno }*|.
*        APPEND lwa_str TO lr_str.
*      ENDLOOP.
*    ENDIF.
**    " Delete ASN's already created.
*    IF lr_str IS NOT INITIAL.
*      DELETE lt_output WHERE asnno IN lr_str.
*    ENDIF.
      ELSE.
        l_txt = |MIS Update Error Code: { lv_codes }: { lv_http_error }|.
        MESSAGE l_txt TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ASNNO_F4HELP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE asnno_f4help INPUT.
  PERFORM f_get_asn_sap_data.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_get_asn_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_asn_sap_data .
**** Purpose of this API is for Customer incoming Payment entry Posting ***
  SORT gt_output BY asnno.
  TYPES: BEGIN OF ty_asnno,
           asnno      TYPE zmm_st_ge_asn-asnno,
           asndesc    TYPE zmm_st_ge_asn-asndesc,
*           invoiceno   TYPE zmm_st_ge_asn-invoiceno,
*           invoicedate TYPE zmm_st_ge_asn-invoicedate,
           cvehicleno TYPE zmm_st_ge_asn-cvehicleno,
*           cmaterialno TYPE zmm_st_ge_asn-cmaterialno,
*           nasnqty     TYPE zmm_st_ge_asn-nasnqty,
*           cbatchno    TYPE zmm_st_ge_asn-cbatchno,
         END OF ty_asnno.
  DATA lt_asn TYPE STANDARD TABLE OF ty_asnno.
  DATA ls_asn TYPE ty_asnno.
  DATA : return_tab TYPE TABLE OF ddshretval WITH HEADER LINE .
  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
    MOVE-CORRESPONDING <fs_output> TO ls_asn.
    APPEND ls_asn TO lt_asn.
  ENDLOOP.
  SORT lt_asn BY asnno.
  DELETE ADJACENT DUPLICATES FROM lt_asn COMPARING ALL FIELDS.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ASNNO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'ZMM_GE_ASN-ASNNO'
      value_org   = 'S'
    TABLES
      value_tab   = lt_asn
      return_tab  = return_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form Shelf_life_validation
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM Shelf_life_validation  .


*      SELECT SINGLE mhdhb FROM mara
*        INTO @DATA(l_tot_shelf_life)
*        WHERE matnr = @iv_data-matnr.
*      IF l_tot_shelf_life GT 0 and iv_data-date_of_manufacture is NOT INITIAL.
*        iv_gw_goodsmvt_item-prod_date = iv_data-date_of_manufacture.
*        ELSEIF l_tot_shelf_life GT 0 and iv_data-date_of_manufacture is INITIAL.
*          MESSAGE 'Enter the Date of Manufacture' type 'E' DISPLAY LIKE 'I'.
*      ENDIF.
ENDFORM.
