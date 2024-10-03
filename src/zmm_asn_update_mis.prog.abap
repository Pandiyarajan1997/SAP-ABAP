*&---------------------------------------------------------------------*
*& Report ZMM_ASN_UPDATE_MIS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_asn_update_mis.
TABLES: zmm_ge_asn.
TYPES: BEGIN OF ty_msg,
         vendor  TYPE string,
         asnno   TYPE string,
         migo_no TYPE string,
         po_no   TYPE string,
         status  TYPE string,
       END OF ty_msg.

TYPES: BEGIN OF ty_status,
         vendor TYPE string,
         asnno  TYPE string,
         status TYPE string,
       END OF ty_status.

DATA: lo_http_client TYPE REF TO if_http_client.

DATA: lv_response   TYPE string, "API Response
      lv_codes      TYPE i,      "STATUS Code
      lv_http_error TYPE string. "STATUS Description
DATA: lv_response1 TYPE string.
DATA: lv_date(10) TYPE c.

DATA: gt_response TYPE TABLE OF ty_msg.
DATA: gw_response TYPE ty_msg.
DATA: gt_status TYPE TABLE OF ty_status.
DATA: lt_ge_asn TYPE STANDARD TABLE OF zmm_ge_asn.
DATA : v_jsonload TYPE string.

DATA : lv_bearer_token TYPE string.
DATA : lv_msg TYPE string.
DATA : l_uname TYPE  syst_uname.
DATA : l_password TYPE  char50.

SELECT-OPTIONS: so_asnno FOR zmm_ge_asn-asnno MATCHCODE OBJECT zmm_vendor_asn,
                so_lifnr FOR zmm_ge_asn-lifnr.

INITIALIZATION.


START-OF-SELECTION.

  SELECT *
     FROM zmm_ge_asn
    INTO TABLE lt_ge_asn
  WHERE asnno IN so_asnno AND
        lifnr IN so_lifnr AND
        mis_update = ''.
  IF lt_ge_asn IS NOT INITIAL.

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
      DATA(l_txt) = |Reacord's not updated in MIS Usename/Password { lv_msg }|.
      MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      SORT lt_ge_asn BY  lifnr asnno.
      DATA(lt_ge_asn_t) = lt_ge_asn.
      DELETE ADJACENT DUPLICATES FROM lt_ge_asn_t COMPARING lifnr asnno.
      SELECT mblnr,
             asnno,
             lifnr,
             ebeln
         FROM zmm_ge_asn_migo
        INTO TABLE @DATA(lt_ge_asn_mogo)
        FOR ALL ENTRIES IN @lt_ge_asn_t
        WHERE lifnr = @lt_ge_asn_t-lifnr AND
              asnno = @lt_ge_asn_t-asnno.

      LOOP AT lt_ge_asn_t ASSIGNING FIELD-SYMBOL(<fs_output1>).
        LOOP AT lt_ge_asn_mogo ASSIGNING FIELD-SYMBOL(<fs_output>)
                              WHERE lifnr = <fs_output1>-lifnr AND
                                    asnno = <fs_output1>-asnno.
          gw_response-vendor = <fs_output>-lifnr.
          gw_response-asnno = <fs_output>-asnno.
          gw_response-migo_no = <fs_output>-mblnr.
          IF gw_response-po_no IS INITIAL.
            gw_response-po_no = |-{ <fs_output>-ebeln }|.
          ELSE.
            gw_response-po_no = |{ gw_response-po_no }-{ <fs_output>-ebeln }|.
          ENDIF.
        ENDLOOP.
        APPEND gw_response TO gt_response.
        CLEAR gw_response.
      ENDLOOP.
      DATA(lv_lines) = lines( gt_response ).
      LOOP AT gt_response INTO gw_response.
        CASE sy-tabix.
          WHEN 1.
            CASE sy-tabix.
              WHEN lv_lines.
                CONCATENATE '[{'
                      '"vendor": "' gw_response-vendor '",'
                      '"asnno": "' gw_response-asnno '",'
                      '"migO_NO": "' gw_response-migo_no '",'
                      '"pO_NO": "' gw_response-po_no '",'
                      '"status": "" '
                      '}]' INTO v_jsonload.
              WHEN OTHERS.
                CONCATENATE '[{'
                      '"vendor": "' gw_response-vendor '",'
                      '"asnno": "' gw_response-asnno '",'
                      '"migO_NO": "' gw_response-migo_no '",'
                      '"pO_NO": "' gw_response-po_no '",'
                      '"status": "" '
                      '}' INTO v_jsonload.
            ENDCASE.
          WHEN OTHERS.
            CASE sy-tabix.
              WHEN lv_lines.
                CONCATENATE v_jsonload
                     ',{'
                    '"vendor": "' gw_response-vendor '",'
                    '"asnno": "' gw_response-asnno '",'
                    '"migO_NO": "' gw_response-migo_no '",'
                    '"pO_NO": "' gw_response-po_no '",'
                    '"status": "" '
                      '}]' INTO v_jsonload.
              WHEN OTHERS.
                CONCATENATE v_jsonload
                     ',{'
                    '"vendor": "' gw_response-vendor '",'
                    '"asnno": "' gw_response-asnno '",'
                    '"migO_NO": "' gw_response-migo_no '",'
                    '"pO_NO": "' gw_response-po_no '",'
                    '"status": "" '
                      '}' INTO v_jsonload.
            ENDCASE.
        ENDCASE.
      ENDLOOP.
      CLEAR: gt_response[],gw_response.
      SELECT SINGLE low FROM tvarvc
        INTO @DATA(l_url)
        WHERE name = 'ZANS_UPD_URL'
      AND   type = 'P'.

      DATA(create_url) = CONV string( l_url ).
      cl_http_client=>create_by_url(
        EXPORTING
        url = create_url
        IMPORTING
        client = lo_http_client
        EXCEPTIONS
        argument_not_found = 1
        plugin_not_active = 2
        internal_error = 3
        OTHERS = 4 ).

      CHECK lo_http_client IS BOUND.

      lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.
      lo_http_client->request->set_method(  EXPORTING method = if_http_entity=>co_request_method_post ).

      lo_http_client->request->set_content_type( EXPORTING content_type = if_rest_media_type=>gc_appl_json ).

      "Header Data Fields for API
      lv_bearer_token = |Bearer { lv_bearer_token }|.
      lo_http_client->request->set_header_field(  EXPORTING  name  = 'Authorization'  value = CONV string( lv_bearer_token ) ).

      lo_http_client->request->set_cdata(
        EXPORTING
        data = '{ JSON_Payload }' ).

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
         data         = gt_status ).

*  ENDIF.
        DATA: lr_asn  TYPE RANGE OF text30,
              lwa_asn LIKE LINE OF lr_asn.
        DATA: lr_lifnr  TYPE RANGE OF lifnr,
              lwa_lifnr LIKE LINE OF lr_lifnr.
        LOOP AT gt_status ASSIGNING FIELD-SYMBOL(<fs_out>) WHERE status = 'OK'.
          lwa_asn-sign = 'I'.
          lwa_asn-option = 'EQ'.
          lwa_asn-low = <fs_out>-asnno.
          APPEND lwa_asn TO lr_asn.
          lwa_lifnr-sign = 'I'.
          lwa_lifnr-option = 'EQ'.
          lwa_lifnr-low = <fs_out>-vendor.
          APPEND lwa_lifnr TO lr_lifnr.
        ENDLOOP.
*        IF lr_lifnr IS NOT INITIAL.
*          DELETE lt_ge_asn WHERE lifnr IN lr_lifnr.
*        ENDIF.
*        IF lr_asn IS NOT INITIAL.
*          DELETE lt_ge_asn WHERE asnno IN lr_asn.
*        ENDIF.
        LOOP AT lt_ge_asn ASSIGNING FIELD-SYMBOL(<fs>) WHERE lifnr IN lr_lifnr
                                                         AND asnno IN lr_asn.
          <fs>-mis_update = 'X'.
        ENDLOOP.
        IF sy-subrc = 0.
          MODIFY zmm_ge_asn FROM TABLE lt_ge_asn.
        ENDIF.
      ELSE.
        l_txt = |MIS Update Error Code: { lv_codes }: { lv_http_error }|.
        MESSAGE l_txt TYPE 'I' DISPLAY LIKE 'S'.
      ENDIF.

      DATA: lo_gr_alv TYPE REF TO cl_salv_table. " Variables for ALV properties
* Create the ALV object
      TRY.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = lo_gr_alv
            CHANGING
              t_table      = lt_ge_asn.
        CATCH cx_salv_msg.
      ENDTRY.
      lo_gr_alv->display( ).
    ENDIF.
  ELSE.
    MESSAGE 'No records found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
