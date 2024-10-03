*&---------------------------------------------------------------------*
*& Include zhr_greythr_empoth_update
*&---------------------------------------------------------------------*
*******Getting Access Token To process Further API***********
FORM get_access_token.

  CALL FUNCTION 'ZHR_GET_ACCESS_TOKEN'
    IMPORTING
      access_token          = token
    EXCEPTIONS
      communication_failure = 1
      status_failure        = 2
      OTHERS                = 3.

  CLEAR str1.
  str1 = token.

ENDFORM.
***********Greytip ID stores in Infotype 0105 **************************
FORM call_employee_lookup_api.

  DATA: lc_pernr(8) TYPE c.

  CLEAR lv_url.

  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMP_LOOKUP' AND type = 'S'.


  REFRESH:gt_employee.

  LOOP AT gt_pernr INTO gs_pernr.

    CLEAR create_url.
    create_url = ls_tvarvc-low.

    CLEAR lc_pernr.
    lc_pernr = gs_pernr-pernr.

    SHIFT lc_pernr LEFT DELETING LEADING '0'.
    CONDENSE lc_pernr NO-GAPS.

    CONCATENATE create_url lc_pernr INTO create_url.

    CONDENSE create_url NO-GAPS.

*****Actual API call*******
    CLEAR lv_response.
    PERFORM api_call.


    IF lv_response IS INITIAL.
      CLEAR gs_log_report.
      gs_log_report-pernr = gs_pernr-pernr.
      gs_log_report-subty = '0002'.
      gs_log_report-infty = '0105'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Error in Communication for the Employee'.
      APPEND gs_log_report TO gt_log_report.

    ELSE.
      CLEAR gs_employee.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = gs_employee ).

      APPEND gs_employee TO gt_employee.
    ENDIF.


    PERFORM f_update_infty_0105 USING '0002' . "0105 Infotype Operation

*    PERFORM f_update_infty_0105 USING '0010' . "0105 Infotype Operation

    CLEAR gs_pernr.

  ENDLOOP.

ENDFORM.
**********Employee ID Details stores in Infotype 0185(PAN ADHAAR PASSPORT) **************************
FORM call_employee_identity_details.

*  DATA: create_url1 TYPE string,
*        create_url2 TYPE string,
*        create_url3 TYPE string.


  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMP_ID_DETAILS' AND type = 'S'.

  REFRESH:gt_id_details.

  CLEAR lv_url.
  lv_url = ls_tvarvc-low.

  CLEAR gs_id_get.
  LOOP AT gt_id_get INTO gs_id_get.
    CLEAR emp_id.
    emp_id = gs_id_get-usrid.

    CLEAR create_url.
    CONCATENATE lv_url emp_id '/identities' INTO create_url.
    CONDENSE create_url NO-GAPS.

*****Actual API call*******
    CLEAR lv_response.
    PERFORM api_call.


    IF lv_response IS INITIAL.
      CLEAR gs_log_report.
      gs_log_report-pernr = gs_id_get-pernr.
      gs_log_report-subty = '02'.
      gs_log_report-infty = '0185'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Error in Communication for the Employee'.
      APPEND gs_log_report TO gt_log_report.

    ELSE.
      CLEAR gs_id_details.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = gt_id_details ).

    ENDIF.

    LOOP AT gt_id_details INTO gs_id_details WHERE employeeid = gs_id_get-usrid..
      REPLACE ALL OCCURRENCES OF '-' IN gs_id_details-expirydate WITH ''.
      IF gs_id_details-idcode = 'PAN'.
        PERFORM f_update_infty_0185 USING '02' . "0185 Infotype Operation

      ELSEIF gs_id_details-idcode = 'AADHAR'.
*      PERFORM f_update_infty_0185 USING '06' . "0185 Infotype Operation

      ELSEIF gs_id_details-idcode = 'PASSPORT'.
        PERFORM f_update_infty_0185 USING '09' . "0185 Infotype Operation

      ENDIF.
      CLEAR gs_id_details.
    ENDLOOP.

    CLEAR gs_id_get.
  ENDLOOP.

ENDFORM.

**********Employee Family Details stores in Infotype 0021**************************
FORM call_employee_family_details.

  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY' AND type = 'P'.

  REFRESH gt_family.

  LOOP AT gt_id_get INTO gs_id_get.

    CLEAR emp_id.
    emp_id = gs_id_get-usrid.

    CLEAR lv_url.
    lv_url = ls_tvarvc-low.

    CLEAR create_url.
    CONCATENATE lv_url emp_id '/families' INTO create_url.

*****Actual API call*******
    CLEAR lv_response.
    PERFORM api_call.


    IF lv_response IS INITIAL.
      CLEAR gs_log_report.
      gs_log_report-pernr = gs_id_get-pernr.
      gs_log_report-subty = '02'.
      gs_log_report-infty = '0021'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Error in Communication for the Employee'.
      APPEND gs_log_report TO gt_log_report.

    ELSE.
      CLEAR gs_family.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = gt_family ).

      IF gt_family[] IS NOT INITIAL .
        SORT gt_family[] BY dob.
      ENDIF.
    ENDIF.

    LOOP AT gt_family INTO gs_family.
      REPLACE ALL OCCURRENCES OF '-' IN gs_family-dob WITH ''.

      IF gs_family-relationid = '1'.
        PERFORM f_update_infty_0021 USING '11'. "0021 Infotype Operation

      ELSEIF gs_family-relationid = '2'.
        PERFORM f_update_infty_0021 USING '12' . "0021 Infotype Operation

      ELSEIF gs_family-relationid = '3'.
        PERFORM f_update_infty_0021 USING '1' .  "0021 Infotype Operation

      ELSEIF gs_family-relationid = '4'.
        PERFORM f_update_infty_0021 USING '1' .  "0021 Infotype Operation

      ELSEIF gs_family-relationid = '5'.
        PERFORM f_update_infty_0021 USING '2' . "0021 Infotype Operation

      ELSEIF gs_family-relationid = '6'.
        PERFORM f_update_infty_0021 USING '2' . "0021 Infotype Operation

      ELSEIF gs_family-relationid = '7'.
        PERFORM f_update_infty_0021 USING '96' ."0021 Infotype Operation

      ELSEIF gs_family-relationid = '8'.
        PERFORM f_update_infty_0021 USING '97' ."0021 Infotype Operation

      ENDIF.
      CLEAR gs_family.
    ENDLOOP.
    REFRESH gt_family.
    CLEAR gs_id_get.
  ENDLOOP.

ENDFORM.

**********Employee Address Details stores in Infotype 0006**************************
FORM call_employee_address_api.

  DATA: create_url1 TYPE string,
        create_url2 TYPE string,
        create_url3 TYPE string.

  CLEAR: create_url1,create_url2,create_url3.
  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY'
                                             AND type = 'P'.

  CLEAR lv_url.
  lv_url = ls_tvarvc-low.

  REFRESH gt_address.

  LOOP AT gt_id_get INTO gs_id_get.

    CLEAR emp_id.
    emp_id = gs_id_get-usrid.

    CLEAR: create_url1,create_url2 .
    CONCATENATE lv_url emp_id '/addresses/permanentaddress' INTO create_url1.
    CONDENSE create_url1 NO-GAPS.

    CLEAR ls_url.
    ls_url-url1 = create_url1.
    APPEND ls_url TO lt_url.

    CONCATENATE lv_url emp_id '/addresses/presentaddress' INTO create_url2.
    CONDENSE create_url2 NO-GAPS.

    CLEAR ls_url.
    ls_url-url1 = create_url2.
    APPEND ls_url TO lt_url.

    CONCATENATE lv_url emp_id '/addresses/emergencyaddress' INTO create_url3.
    CONDENSE create_url3 NO-GAPS.

    CLEAR ls_url.
    ls_url-url1 = create_url3.
    APPEND ls_url TO lt_url.

  ENDLOOP.

  LOOP AT lt_url INTO ls_url.

    CLEAR create_url.
    create_url = ls_url-url1.

*****Actual API call*******
    CLEAR lv_response.
    PERFORM api_call.


    IF lv_response IS INITIAL.
      CLEAR gs_log_report.
      gs_log_report-pernr = gs_id_get-pernr.
      gs_log_report-subty = '1'.
      gs_log_report-infty = '0003'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Error in Communication for the Employee'.
      APPEND gs_log_report TO gt_log_report.

    ELSE.
      CLEAR gs_address.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = gs_address ).

      APPEND gs_address TO gt_address.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_id_get INTO gs_id_get.
    LOOP AT gt_address INTO gs_address WHERE employeeid = gs_id_get-usrid.

      TRANSLATE gs_address-addresstype TO UPPER CASE.
      IF gs_address-addresstype = 'PERMANENTADDRESS'.
        PERFORM f_update_infty_0006 USING '1' ."0006 Infotype Operation

      ELSEIF gs_address-addresstype = 'PRESENTADDRESS'.
        PERFORM f_update_infty_0006 USING '3' ."0006 Infotype Operation
      ELSEIF gs_address-addresstype = 'EMERGENCYADDRESS'.
        PERFORM f_update_infty_0006 USING '4' ."0006 Infotype Operation
      ENDIF.
      CLEAR gs_address.
    ENDLOOP.
    CLEAR gs_id_get.
  ENDLOOP.


ENDFORM.

**********Employee Bank Details stores in Infotype 0009**************************
FORM call_employee_bank_api.

  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY'
                                             AND type = 'P'.

  CLEAR lv_url.
  lv_url = ls_tvarvc-low.

  REFRESH gt_bank.
  LOOP AT gt_id_get INTO gs_id_get.

    CLEAR emp_id.
    emp_id = gs_id_get-usrid.

    CLEAR create_url.
    CONCATENATE lv_url emp_id '/bank' INTO create_url.
    CONDENSE create_url NO-GAPS.

*****Actual API call*******
    CLEAR lv_response.
    PERFORM api_call.


    IF lv_response IS INITIAL.
      CLEAR gs_log_report.
      gs_log_report-pernr = gs_id_get-pernr.
      gs_log_report-subty = '1'.
      gs_log_report-infty = '0009'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Error in Communication for the Employee'.
      APPEND gs_log_report TO gt_log_report.

    ELSE.
      CLEAR gs_bank.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = gs_bank ).

      APPEND gs_bank TO gt_bank.
    ENDIF.

    PERFORM f_update_infty_0009 USING '0' ."0009 Infotype Operation
    CLEAR gs_id_get.
  ENDLOOP.

ENDFORM.

**********Employee Qualification Details stores in Infotype 0024**************************
FORM call_employee_qualification.

  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY'
                                             AND type = 'P'.

  CLEAR lv_url.
  lv_url = ls_tvarvc-low.

  REFRESH gt_education.

  LOOP AT gt_id_get INTO gs_id_get.

    CLEAR emp_id.
    emp_id = gs_id_get-usrid.

    CLEAR create_url.
    CONCATENATE lv_url emp_id '/qualifications' INTO create_url.
    CONDENSE create_url NO-GAPS.

*****Actual API call*******
    CLEAR lv_response.
    PERFORM api_call.


    IF lv_response IS INITIAL.
      CLEAR gs_log_report.
      gs_log_report-pernr = gs_id_get-pernr.
      gs_log_report-subty = '1'.
      gs_log_report-infty = '0024'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Error in Communication for the Employee'.
      APPEND gs_log_report TO gt_log_report.

    ELSE.
      CLEAR gs_education.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = gt_education ).
    ENDIF.
  ENDLOOP.

  LOOP AT gt_education INTO gs_education.

  ENDLOOP.
ENDFORM.

*********Actual API call******************
FORM api_call.

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


  lo_http_client->request->set_method(
   EXPORTING
   method = if_http_entity=>co_request_method_get ).


  lo_http_client->request->set_content_type(
   EXPORTING
   content_type = if_rest_media_type=>gc_appl_json ).

  PERFORM get_access_token. "Getting access token Based on auth API

  CLEAR ls_tvarvc1.
  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc1 WHERE name = 'URL_LOV_API'
                                              AND type = 'P'.

  CLEAR str2.
  str2 = ls_tvarvc1-low.

  lo_http_client->request->set_header_field( EXPORTING name  = 'ACCESS-TOKEN' value = str1 ).
  lo_http_client->request->set_header_field( EXPORTING name = 'x-greythr-domain' value = str2 ).


*  lo_http_client->request->set_cdata(
*   EXPORTING
*   data = '{ JSON_Payload }' ).


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

  CLEAR lv_codes.
  lo_http_client->response->get_status(
  IMPORTING
    code = lv_codes ).

  CLEAR lv_http_error.
  lo_http_client->response->get_status(
  IMPORTING
    reason = lv_http_error ).

  CLEAR lv_response.
  IF lv_codes = 200.
    lv_response = lo_http_client->response->get_cdata( ).
    WAIT UP TO 2 SECONDS.
  ENDIF.
*  WRITE: lv_response.

ENDFORM.

***Infotype 0105 Updation based on GET_EMPLOYEE_LOOKUP API***
FORM f_update_infty_0105 USING p_subty TYPE subty .


***********Data declaration for infotype 0105*********
  DATA: lv_pernr TYPE pernr_d,       "Employee Number"
        ls_ret   TYPE  bapireturn1,  "Structure for Messages"
        ls_0105  TYPE p0105.         "Structure for Infotype 0105"
*******************************************************
  DATA: ls_p0105 TYPE pa0105.

  DATA: lv_create TYPE boolean,
        lv_delete TYPE boolean.

  DATA: ls_lookup TYPE tvarvc.

  CLEAR lv_pernr.
  lv_pernr = gs_pernr-pernr.

  CLEAR ls_lookup.
  SELECT SINGLE * FROM tvarvc INTO ls_lookup WHERE name = 'ZHR_GREYTHR_LOOKUP'
                                             AND type = 'P'.
  IF ls_lookup-low EQ 'X'.
*first check is the value in infotype is present in SAP
    SELECT SINGLE *
      FROM pa0105
      INTO ls_p0105
     WHERE pernr = lv_pernr
       AND subty = p_subty
       AND begda <= sy-datum
       AND endda >= sy-datum.
    IF sy-subrc EQ 0.
      lv_delete = abap_true.
      lv_create = abap_true. " For updating Without check
    ELSE.
      lv_create = abap_true.
    ENDIF.
  ELSE.
*first check is the value in infotype is same as the one from API
*  IF p_subty = '0002'.
    SELECT SINGLE *
      FROM pa0105
      INTO ls_p0105
     WHERE pernr = lv_pernr
       AND subty = p_subty
       AND begda <= sy-datum
       AND endda >= sy-datum.
    IF sy-subrc NE 0.
      lv_create = abap_true.
    ELSE.
      IF p_subty = '0002'.   "for the employeeid field
        IF gs_employee-employeeid = ls_p0105-usrid.
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '0002'.
          gs_log_report-infty = '0105'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee id Value in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ELSEIF p_subty = '0010'. "for Personal email
        IF gs_employee-email = ls_p0105-usrid_long.
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '0010'.
          gs_log_report-infty = '0105'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Personal Email Value in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.



  CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

  IF lv_delete = abap_true.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      MOVE-CORRESPONDING ls_p0105 TO ls_0105.
      ls_0105-infty = '0105'.

      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '0105'
          number        = ls_0105-pernr
          subtype       = ls_0105-subty
          validityend   = ls_0105-endda
          validitybegin = ls_0105-begda
          record        = ls_0105
          operation     = 'DEL'
        IMPORTING
          return        = ls_ret.

      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0105-pernr.
        gs_log_report-subty = ls_0105-subty.
        gs_log_report-infty = ls_0105-infty.
        gs_log_report-status = 'S'.
        gs_log_report-text   = 'Data deleted successfully'.
        APPEND gs_log_report TO gt_log_report.
      ELSE.
        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0105-pernr.
        gs_log_report-subty = ls_0105-subty.
        gs_log_report-infty = ls_0105-infty.
        gs_log_report-status = 'E'.
        gs_log_report-text   = ls_ret-message.
        APPEND gs_log_report TO gt_log_report.
      ENDIF.

      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr
        IMPORTING
          return = ls_ret.

    ELSE.
      CLEAR gs_log_report.
      gs_log_report-pernr = ls_0105-pernr.
      gs_log_report-subty = ls_0105-subty.
      gs_log_report-infty = ls_0105-infty.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Employee id already locked in SAP'.
      APPEND gs_log_report TO gt_log_report.
      EXIT.
    ENDIF.
  ENDIF.


  IF lv_create = abap_true.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CLEAR:ls_0105, ls_ret.
      ls_0105-pernr = lv_pernr.



      READ TABLE gt_pernr_hire INTO gs_pernr_hire WITH KEY pernr = lv_pernr.
      IF sy-subrc = 0.
        ls_0105-begda = gs_pernr_hire-dat01.
      ELSE.
        ls_0105-begda = '19000101'.
      ENDIF.

      ls_0105-endda = '99991231'.
      IF p_subty = '0002'.
        ls_0105-subty  = '0002'.
        ls_0105-usrty  = '0002'.
        ls_0105-usrid =  gs_employee-employeeid .
      ELSEIF p_subty = '0010'.
        ls_0105-subty  = '0010'.
        ls_0105-usrty  = '0010'.
        ls_0105-usrid_long =  gs_employee-email .
      ENDIF.



      ls_0105-infty = '0105'.

      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '0105'
          number        = ls_0105-pernr
          subtype       = ls_0105-subty
          validityend   = ls_0105-endda
          validitybegin = ls_0105-begda
          record        = ls_0105
          operation     = 'INS'
        IMPORTING
          return        = ls_ret.


      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

        CLEAR gs_log_report.
        gs_log_report-pernr = lv_pernr.
        gs_log_report-subty = ls_0105-subty.
        gs_log_report-infty = ls_0105-infty.
        gs_log_report-status = 'S'.
        gs_log_report-text   = 'Data update successfully'.
        APPEND gs_log_report TO gt_log_report.

      ELSE.
        CLEAR gs_log_report.
        gs_log_report-pernr = lv_pernr.
        gs_log_report-subty = ls_0105-subty.
        gs_log_report-infty = ls_0105-infty.
        gs_log_report-status = 'E'.
        gs_log_report-text   = ls_ret-message.
        APPEND gs_log_report TO gt_log_report.
      ENDIF.
      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr
        IMPORTING
          return = ls_ret.
    ELSE.
      CLEAR gs_log_report.
      gs_log_report-pernr = lv_pernr.
      gs_log_report-subty = ls_0105-subty.
      gs_log_report-infty = '0105'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Employee id already locked in SAP'.
      APPEND gs_log_report TO gt_log_report.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.

***Infotype 0185 Updation based on GET_EMPLOYEE_IDENTITY_DETAILS API***
FORM f_update_infty_0185 USING p_subty TYPE subty .

**********Data declaration for infotype 0105*********
  DATA: lv_pernr TYPE pernr_d,       "Employee Number"
        ls_ret   TYPE  bapireturn1,  "Structure for Messages"
        ls_0185  TYPE p0185.         "Structure for Infotype 0185"
*******************************************************
  DATA: ls_p0185 TYPE pa0185.

  DATA: lv_create TYPE boolean,
        lv_delete TYPE boolean.

  DATA: ls_identity TYPE tvarvc.

  CLEAR lv_pernr.
  lv_pernr = gs_id_get-pernr.

  IF p_subty = '02' AND gs_id_details-documentno IS INITIAL.
    CLEAR gs_log_report.
    gs_log_report-pernr = lv_pernr.
    gs_log_report-subty = p_subty.
    gs_log_report-infty = '0185'.
    gs_log_report-status = 'S'.
    gs_log_report-text   = 'Employee PAN Number is not available in GreytHR'.
    APPEND gs_log_report TO gt_log_report.
    EXIT.
  ELSEIF  p_subty = '09' AND gs_id_details-documentno IS INITIAL.
    CLEAR gs_log_report.
    gs_log_report-pernr = lv_pernr.
    gs_log_report-subty = p_subty.
    gs_log_report-infty = '0185'.
    gs_log_report-status = 'S'.
    gs_log_report-text   = 'Employee Passport Number is not available in GreytHR'.
    APPEND gs_log_report TO gt_log_report.
    EXIT.
  ELSEIF  p_subty = '06' AND gs_id_details-documentno IS INITIAL.
    CLEAR gs_log_report.
    gs_log_report-pernr = lv_pernr.
    gs_log_report-subty = p_subty.
    gs_log_report-infty = '0185'.
    gs_log_report-status = 'S'.
    gs_log_report-text   = 'Employee Aadhar is not available in GreytHR'.
    APPEND gs_log_report TO gt_log_report.
    EXIT.
  ENDIF.

  CLEAR ls_identity.
  SELECT SINGLE * FROM tvarvc INTO ls_identity WHERE name = 'ZHR_GREYTHR_ID_DETAILS'
                                               AND type = 'P'.
  IF ls_identity-low EQ 'X'.
*first check is the value in infotype is same as the one from API
    CLEAR ls_p0185.
    SELECT SINGLE *
      FROM pa0185
      INTO ls_p0185
      WHERE pernr = lv_pernr
      AND subty = p_subty
      AND begda <= sy-datum
      AND endda >= sy-datum.
    IF sy-subrc EQ 0.
      lv_delete = abap_true.
      lv_create = abap_true.  " For update without checks
    ELSE.
      lv_create = abap_true.
    ENDIF.
  ELSE.
*first check is the value in infotype is same as the one from API
*  IF p_subty = '02'.
    SELECT SINGLE *
      FROM pa0185
      INTO ls_p0185
      WHERE pernr = lv_pernr
      AND subty = p_subty
      AND begda <= sy-datum
      AND endda >= sy-datum.
    IF sy-subrc NE 0.
      lv_create = abap_true.
    ELSE.
      IF p_subty = '02'.   "for the Employee PAN
        IF gs_id_details-documentno = ls_p0185-icnum.
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '02'.
          gs_log_report-infty = '0185'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee PAN Number in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ELSEIF p_subty = '06'. "for Employee AADHAR
        IF gs_id_details-documentno = ls_p0185-icnum.
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '06'.
          gs_log_report-infty = '0185'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee AADHAR Number in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ELSEIF p_subty = '09'. "for Employee PASSPORT
        IF gs_id_details-documentno = ls_p0185-icnum.
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '09'.
          gs_log_report-infty = '0185'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee PASSPORT Number in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

  IF lv_delete = abap_true.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      MOVE-CORRESPONDING ls_p0185 TO ls_0185.
      ls_0185-infty = '0185'.

      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '0185'
          number        = ls_0185-pernr
          subtype       = ls_0185-subty
          validityend   = ls_0185-endda
          validitybegin = ls_0185-begda
          record        = ls_0185
          operation     = 'DEL'
        IMPORTING
          return        = ls_ret.

      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0185-pernr.
        gs_log_report-subty = ls_0185-subty.
        gs_log_report-infty = ls_0185-infty.
        gs_log_report-status = 'S'.
        gs_log_report-text   = 'Data deleted successfully'.
        APPEND gs_log_report TO gt_log_report.
      ELSE.
        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0185-pernr.
        gs_log_report-subty = ls_0185-subty.
        gs_log_report-infty = ls_0185-infty.
        gs_log_report-status = 'E'.
        gs_log_report-text   = ls_ret-message.
        APPEND gs_log_report TO gt_log_report.
      ENDIF.

      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr
        IMPORTING
          return = ls_ret.

    ELSE.
      CLEAR gs_log_report.
      gs_log_report-pernr = ls_0185-pernr.
      gs_log_report-subty = ls_0185-subty.
      gs_log_report-infty = ls_0185-infty.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Employee id already locked in SAP'.
      APPEND gs_log_report TO gt_log_report.
      EXIT.
    ENDIF.
  ENDIF.

  IF lv_create = abap_true.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CLEAR:ls_0185, ls_ret.
      ls_0185-pernr = lv_pernr.


      READ TABLE gt_pernr_hire INTO gs_pernr_hire WITH KEY pernr = lv_pernr.
      IF sy-subrc = 0.
        ls_0185-begda = gs_pernr_hire-dat01.
      ELSE.
        ls_0185-begda = '19000101'.
      ENDIF.

      ls_0185-endda = '99991231'. " End Date
      IF gs_id_details-idcode = 'PAN'.  "PAN
        ls_0185-subty = '02'.
        ls_0185-ictyp = '02'.
      ELSEIF gs_id_details-idcode = 'AADHAR'. "AADHAR
        ls_0185-subty = '06'.
        ls_0185-ictyp = '06'.
      ELSEIF gs_id_details-idcode = 'PASSPORT'. "PASSPORT
        ls_0185-subty = '09'.
        ls_0185-ictyp = '09'.
      ENDIF.

      IF p_subty = '02' . "PAN
        IF gs_id_details-idcode = 'PAN' .
          ls_0185-icnum = gs_id_details-documentno.
          ls_0185-ename = gs_id_details-nameasperdoc.
        ENDIF.
      ELSEIF p_subty = '06' . "AADHAR
        IF ( gs_id_details-idcode = 'AADHAR' AND gs_id_details-documentno IS NOT INITIAL )..
          ls_0185-icnum = gs_id_details-documentno.
          ls_0185-ename = gs_id_details-nameasperdoc.
        ELSEIF ( gs_id_details-idcode = 'AADHAR' AND gs_id_details-documentno IS INITIAL ).
          ls_0185-icold = gs_id_details-aadharappno.
          ls_0185-ename = gs_id_details-nameasperdoc.
        ENDIF.
      ELSEIF p_subty = '09' . "PASSPORT
        IF gs_id_details-idcode = 'PASSPORT' .
          ls_0185-icnum = gs_id_details-documentno.
          ls_0185-ename = gs_id_details-nameasperdoc.
          ls_0185-expid =  gs_id_details-expirydate.
        ENDIF.
      ENDIF.

      ls_0185-infty = '0185'.

      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '0185'
          number        = ls_0185-pernr
          subtype       = ls_0185-subty
          validityend   = ls_0185-endda
          validitybegin = ls_0185-begda
          record        = ls_0185
          operation     = 'INS'
        IMPORTING
          return        = ls_ret.


      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0185-pernr.
        gs_log_report-subty = ls_0185-subty.
        gs_log_report-infty = ls_0185-infty.
        gs_log_report-status = 'S'.
        gs_log_report-text   = 'Data update successfully'.
        APPEND gs_log_report TO gt_log_report.

      ELSE.
        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0185-pernr.
        gs_log_report-subty = ls_0185-subty.
        gs_log_report-infty = ls_0185-infty.
        gs_log_report-status = 'E'.
        gs_log_report-text   = ls_ret-message.
        APPEND gs_log_report TO gt_log_report.
      ENDIF.
      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr
        IMPORTING
          return = ls_ret.
    ELSE.
      CLEAR gs_log_report.
      gs_log_report-pernr = lv_pernr.
      gs_log_report-subty = ls_0185-subty.
      gs_log_report-infty = '0185'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Employee id already locked in SAP'.
      APPEND gs_log_report TO gt_log_report.
      EXIT.
    ENDIF.
  ENDIF.
*  ENDIF.
ENDFORM.

***Infotype 0021 Updation based on GET_EMPLOYEE_FAMILY_DETAILS API***
FORM f_update_infty_0021 USING p_subty TYPE subty .

**********Data declaration for infotype 0105*********
  DATA: lv_pernr TYPE pernr_d,       "Employee Number"
        ls_ret   TYPE  bapireturn1,  "Structure for Messages"
        ls_0021  TYPE p0021.         "Structure for Infotype 0021"
*******************************************************
  DATA: ls_p0021 TYPE pa0021.

  DATA: lv_create TYPE boolean,
        lv_delete TYPE boolean.
  DATA: ls_family TYPE tvarvc.
  CLEAR lv_pernr.
  lv_pernr = gs_id_get-pernr.

  CLEAR ls_family.
  SELECT SINGLE * FROM tvarvc INTO ls_family WHERE name = 'ZHR_GREYTHR_FAMILY'
                                             AND type = 'P'.
  IF ls_family-low EQ 'X'.
*first check is the value in infotype is present in SAP
    CLEAR ls_p0021.
    SELECT SINGLE *
      FROM pa0021
      INTO ls_p0021
      WHERE pernr = lv_pernr
      AND subty = p_subty
      AND begda <= sy-datum
      AND endda >= sy-datum.
    IF sy-subrc EQ 0.
      lv_delete = abap_true.
      lv_create = abap_true.   " For update without checks
    ELSE.
      lv_create = abap_true.
    ENDIF.
  ELSE.
*first check is the value in infotype is same as the one from API
    CLEAR ls_p0021.
    SELECT SINGLE *
      FROM pa0021
      INTO ls_p0021
      WHERE pernr = lv_pernr
      AND subty = p_subty
      AND begda <= sy-datum
      AND endda >= sy-datum.
    IF sy-subrc NE 0.
      lv_create = abap_true.
    ELSE.
      IF p_subty = '1'."for the Employee SPOUSE
        IF ( gs_family-name = ls_p0021-favor AND gs_family-relationid = '3').
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '1'.
          gs_log_report-infty = '0021'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee Husband name in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
        ELSEIF ( gs_family-name = ls_p0021-favor AND gs_family-relationid = '4' ).
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '1'.
          gs_log_report-infty = '0021'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee Wife name in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ELSEIF p_subty = '11'. "for Employee FATHER
        IF ( gs_family-name = ls_p0021-favor AND gs_family-relationid = '1' ).

          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '11'.
          gs_log_report-infty = '0021'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee Father Name in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ELSEIF p_subty = '12'. "for Employee MOTHER
        IF ( gs_family-name = ls_p0021-favor AND gs_family-relationid = '2' ) .
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '12'.
          gs_log_report-infty = '0021'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee Mother Name in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ELSEIF p_subty = '2'. "for Employee CHILDREN
        IF ( gs_family-name = ls_p0021-favor AND gs_family-relationid = '5'
                                             OR gs_family-relationid = '6' ) .
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '2'.
          gs_log_report-infty = '0021'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee Child Name in GreytHR and SAP Same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

  IF lv_delete = abap_true.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      MOVE-CORRESPONDING ls_p0021 TO ls_0021.
      ls_0021-infty = '0021'.

      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '0021'
          number        = ls_0021-pernr
          subtype       = ls_0021-subty
          validityend   = ls_0021-endda
          validitybegin = ls_0021-begda
          record        = ls_0021
          operation     = 'DEL'
        IMPORTING
          return        = ls_ret.

      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0021-pernr.
        gs_log_report-subty = ls_0021-subty.
        gs_log_report-infty = ls_0021-infty.
        gs_log_report-status = 'S'.
        gs_log_report-text   = 'Data deleted successfully'.
        APPEND gs_log_report TO gt_log_report.
      ELSE.
        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0021-pernr.
        gs_log_report-subty = ls_0021-subty.
        gs_log_report-infty = ls_0021-infty.
        gs_log_report-status = 'E'.
        gs_log_report-text   = ls_ret-message.
        APPEND gs_log_report TO gt_log_report.
      ENDIF.

      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr
        IMPORTING
          return = ls_ret.

    ELSE.
      CLEAR gs_log_report.
      gs_log_report-pernr = ls_0021-pernr.
      gs_log_report-subty = ls_0021-subty.
      gs_log_report-infty = ls_0021-infty.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Employee id already locked in SAP'.
      APPEND gs_log_report TO gt_log_report.
      EXIT.
    ENDIF.
  ENDIF.

  IF lv_create = abap_true.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CLEAR:ls_0021, ls_ret.
      ls_0021-pernr = lv_pernr.

      READ TABLE lt_greythr INTO ls_greythr WITH KEY code = gs_family-relationid.
      IF sy-subrc EQ 0.
        ls_0021-subty = ls_greythr-subty_0021.
        ls_0021-famsa = ls_greythr-subty_0021.
      ENDIF.

      READ TABLE gt_pernr_hire INTO gs_pernr_hire WITH KEY pernr = lv_pernr.
      IF sy-subrc = 0.
        ls_0021-begda = gs_pernr_hire-dat01.
      ELSE.
        ls_0021-begda = '19000101'.
      ENDIF.

      ls_0021-endda = '99991231'.

      READ TABLE lt_greythr INTO ls_greythr WITH KEY code = gs_family-bloodgroup.
      IF sy-subrc EQ 0.
        ls_0021-zbgrup = ls_greythr-bloodgrp_des.
      ENDIF.

      ls_0021-zprofession = gs_family-profession.

      IF p_subty = '1'.
        IF gs_family-relationid = '3'.  " EMPLOYEE HUSBAND-SPOUSE
          ls_0021-favor  = gs_family-name.
          ls_0021-fanam = '.'.
          ls_0021-fgbdt = gs_family-dob.
          ls_0021-fasex = '1'.
        ELSEIF gs_family-relationid = '4'." EMPLOYEE WIFE-SPOUSE
          ls_0021-favor  = gs_family-name.
          ls_0021-fanam = '.'.
          ls_0021-fgbdt = gs_family-dob.
          ls_0021-fasex = '2'.
        ENDIF.
      ELSEIF p_subty = '11'.
        IF gs_family-relationid = '1'."EMPLOYEE FATHER
          ls_0021-favor  = gs_family-name.
          ls_0021-fanam = '.'.
          ls_0021-fgbdt = gs_family-dob.
          ls_0021-fasex = '1'.
        ENDIF.
      ELSEIF p_subty = '12'.
        IF gs_family-relationid = '2' ."EMPLOYEE-MOTHER
          ls_0021-favor  = gs_family-name.
          ls_0021-fanam = '.'.
          ls_0021-fgbdt = gs_family-dob.
          ls_0021-fasex = '2'.
        ENDIF.
      ELSEIF p_subty = '2'.
        IF gs_family-relationid = '5'."EMPLOYEE-CHILDREN
          ls_0021-favor  = gs_family-name.
          ls_0021-fanam = '.'.
          ls_0021-fgbdt = gs_family-dob.
          ls_0021-fasex = '1'.
        ELSEIF gs_family-relationid = '6'. "EMPLOYEE-CHILDREN
          ls_0021-favor  = gs_family-name.
          ls_0021-fanam = '.'.
          ls_0021-fgbdt = gs_family-dob.
          ls_0021-fasex = '2'.
        ENDIF.
      ELSEIF p_subty = '96'.
        IF gs_family-relationid = '7' ."EMPLOYEE-BROTHER
          ls_0021-favor  = gs_family-name.
          ls_0021-fanam = '.'.
          ls_0021-fgbdt = gs_family-dob.
          ls_0021-fasex = '1'.
        ENDIF.
      ELSEIF p_subty = '97'.
        IF gs_family-relationid = '8' ."EMPLOYEE-SISTER
          ls_0021-favor  = gs_family-name.
          ls_0021-fanam = '.'.
          ls_0021-fgbdt = gs_family-dob.
          ls_0021-fasex = '2'.
        ENDIF.
      ENDIF.

      ls_0021-infty = '0021'.

      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '0021'
          number        = ls_0021-pernr
          subtype       = ls_0021-subty
          validityend   = ls_0021-endda
          validitybegin = ls_0021-begda
          record        = ls_0021
          operation     = 'INS'
        IMPORTING
          return        = ls_ret.

      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0021-pernr.
        gs_log_report-subty = ls_0021-subty.
        gs_log_report-infty = ls_0021-infty.
        gs_log_report-status = 'S'.
        gs_log_report-text   = 'Data update successfully'.
        APPEND gs_log_report TO gt_log_report.

      ELSE.
        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0021-pernr.
        gs_log_report-subty = ls_0021-subty.
        gs_log_report-infty = ls_0021-infty.
        gs_log_report-status = 'E'.
        gs_log_report-text   = ls_ret-message.
        APPEND gs_log_report TO gt_log_report.
      ENDIF.
      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr
        IMPORTING
          return = ls_ret.
    ELSE.
      CLEAR gs_log_report.
      gs_log_report-pernr = lv_pernr.
      gs_log_report-subty = ls_0021-subty.
      gs_log_report-infty = '0021'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Employee id already locked in SAP'.
      APPEND gs_log_report TO gt_log_report.
      EXIT.
    ENDIF.
  ENDIF.
*  ENDIF.
ENDFORM.


***Infotype 0006 Updation based on GET_EMPLOYEE_ADDRESS_DETAILS API***
FORM f_update_infty_0006 USING p_subty TYPE subty .

**********Data declaration for infotype 0105*********
  DATA: lv_pernr TYPE pernr_d,       "Employee Number"
        ls_ret   TYPE  bapireturn1,  "Structure for Messages"
        ls_0006  TYPE p0006.         "Structure for Infotype 0006"
*******************************************************
  DATA: ls_p0006 TYPE pa0006.

  DATA: lv_create TYPE boolean,
        lv_delete TYPE boolean.

  DATA: ls_address TYPE tvarvc.

  CLEAR lv_pernr.
  lv_pernr = gs_id_get-pernr.

  IF p_subty = '1' AND gs_address-address1 IS INITIAL AND gs_address-address2 IS INITIAL .
    CLEAR gs_log_report.
    gs_log_report-pernr = lv_pernr.
    gs_log_report-subty = p_subty.
    gs_log_report-infty = '0006'.
    gs_log_report-status = 'S'.
    gs_log_report-text   = 'Employee Permanent Address is not available in Greythr'.
    APPEND gs_log_report TO gt_log_report.
    EXIT.
  ELSEIF p_subty = '3' AND gs_address-address1 IS INITIAL AND gs_address-address2 IS INITIAL .
    CLEAR gs_log_report.
    gs_log_report-pernr = lv_pernr.
    gs_log_report-subty = p_subty.
    gs_log_report-infty = '0006'.
    gs_log_report-status = 'S'.
    gs_log_report-text   = 'Employee Present Address is not available in Greythr'.
    APPEND gs_log_report TO gt_log_report.
    EXIT.
  ELSEIF p_subty = '4' AND gs_address-name IS INITIAL AND gs_address-mobile IS INITIAL.
    CLEAR gs_log_report.
    gs_log_report-pernr = lv_pernr.
    gs_log_report-subty = p_subty.
    gs_log_report-infty = '0006'.
    gs_log_report-status = 'S'.
    gs_log_report-text   = 'Employee Emergency address is not available in Greythr'.
    APPEND gs_log_report TO gt_log_report.
    EXIT.
  ENDIF.

  CLEAR ls_address.
  SELECT SINGLE * FROM tvarvc INTO ls_address WHERE name = 'ZHR_GREYTHR_ADDRESS'
                                              AND type = 'P'.
  IF ls_address-low EQ 'X'.
*first check is the value in infotype is same as the one from API
    SELECT SINGLE *
       FROM pa0006
       INTO ls_p0006
       WHERE pernr = lv_pernr
       AND subty = p_subty
       AND begda <= sy-datum
       AND endda >= sy-datum.
    IF sy-subrc EQ 0.
      lv_delete = abap_true.
      lv_create = abap_true.   " For update without checks
    ELSE.
      lv_create = abap_true.
    ENDIF.
  ELSE.
*first check is the value in infotype is same as the one from API
    SELECT SINGLE *
       FROM pa0006
       INTO ls_p0006
       WHERE pernr = lv_pernr
       AND subty = p_subty
       AND begda <= sy-datum
       AND endda >= sy-datum.
    IF sy-subrc NE 0.
      lv_create = abap_true.
    ELSE.
      IF p_subty = '1'.
        IF ( gs_address-address1 = ls_p0006-stras AND gs_address-addresstype = 'PERMANENTADDRESS'
                                                  AND gs_address-address2 = ls_p0006-locat ).
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '1'.
          gs_log_report-infty = '0006'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee Permanent Address in Greythr and SAP same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_create = abap_true.
        ENDIF.
      ELSEIF p_subty = '3'.
        IF ( gs_address-address1 = ls_p0006-stras AND gs_address-addresstype = 'PRESENTADDRESS'
                                                  AND gs_address-address2 = ls_p0006-locat ).
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '3'.
          gs_log_report-infty = '0006'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee Present Address in Greythr and SAP same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ELSEIF p_subty = '4'.
        IF ( gs_address-name = ls_p0006-name2 AND gs_address-addresstype = 'EMERGENCYADDRESS'
                                              AND gs_address-mobile = ls_p0006-telnr ).
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = '3'.
          gs_log_report-infty = '0006'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee Emergency Address in Greythr and SAP same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_delete = abap_true.
          lv_create = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

  IF lv_delete = abap_true.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      MOVE-CORRESPONDING ls_p0006 TO ls_0006.
      ls_0006-infty = '0006'.

      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '0006'
          number        = ls_0006-pernr
          subtype       = ls_0006-subty
          validityend   = ls_0006-endda
          validitybegin = ls_0006-begda
          record        = ls_0006
          operation     = 'DEL'
        IMPORTING
          return        = ls_ret.

      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0006-pernr.
        gs_log_report-subty = ls_0006-subty.
        gs_log_report-infty = ls_0006-infty.
        gs_log_report-status = 'S'.
        gs_log_report-text   = 'Data deleted successfully'.
        APPEND gs_log_report TO gt_log_report.
      ELSE.
        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0006-pernr.
        gs_log_report-subty = ls_0006-subty.
        gs_log_report-infty = ls_0006-infty.
        gs_log_report-status = 'E'.
        gs_log_report-text   = ls_ret-message.
        APPEND gs_log_report TO gt_log_report.
      ENDIF.

      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr
        IMPORTING
          return = ls_ret.

    ELSE.
      CLEAR gs_log_report.
      gs_log_report-pernr = lv_pernr.
      gs_log_report-subty = ls_0006-subty.
      gs_log_report-infty = ls_0006-infty.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Employee id already locked in SAP'.
      APPEND gs_log_report TO gt_log_report.
      EXIT.
    ENDIF.
  ENDIF.

  IF lv_create = abap_true.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CLEAR:ls_0006, ls_ret.
      ls_0006-pernr = lv_pernr.

      READ TABLE gt_pernr_hire INTO gs_pernr_hire WITH KEY pernr = lv_pernr.
      IF sy-subrc = 0.
        ls_0006-begda = gs_pernr_hire-dat01.
      ELSE.
        ls_0006-begda = '19000101'.
      ENDIF.

      IF ( gs_address-address1 NE ls_p0006-stras AND gs_address-addresstype = 'PERMANENTADDRESS'
                                                 AND gs_address-address2 NE ls_p0006-locat ).
        ls_0006-begda = sy-datum.
      ENDIF.

      ls_0006-endda = '99991231'.

      READ TABLE lt_greythr INTO ls_greythr WITH KEY address_type = gs_address-addresstype.
      IF sy-subrc EQ 0.
        ls_0006-subty  = ls_greythr-subty_0006.
      ENDIF.

      READ TABLE lt_greythr INTO ls_greythr WITH KEY code = gs_address-country.
      IF sy-subrc EQ 0.
        ls_0006-land1 = ls_greythr-country_desc.
      ENDIF.
      DATA(ls_desc) = VALUE #( lt_lov[ field_code = '17' id = gs_address-state ]-value OPTIONAL ).
      SELECT SINGLE bland FROM t005u INTO ls_0006-state WHERE spras = sy-langu
                                                         AND bezei = ls_desc
                                                         AND land1 = 'IN'.

*      READ TABLE lt_greythr INTO ls_greythr WITH KEY code = gs_address-state.
*      IF sy-subrc EQ 0.
*        ls_0006-state = ls_greythr-state_desc.
*      ENDIF.

      IF p_subty = '1'.
        IF gs_address-addresstype = 'PERMANENTADDRESS'.
          ls_0006-name2 = gs_address-name.
          ls_0006-stras = gs_address-address1.
          ls_0006-locat = gs_address-address2.
          ls_0006-hsnmr = gs_address-extnno.
          ls_0006-pstlz = gs_address-pin.
          ls_0006-ort01 = gs_address-city.
          ls_0006-telnr = gs_address-phone1.
          ls_0006-com01 = 'CELL'.
          ls_0006-num01 = gs_address-phone2.
          ls_0006-com02 = 'CELL'.
          ls_0006-num02 = gs_address-mobile.
        ENDIF.
      ELSEIF p_subty = '3'.
        IF gs_address-addresstype = 'PRESENTADDRESS'.
          ls_0006-name2 = gs_address-name.
          ls_0006-stras = gs_address-address1.
          ls_0006-locat = gs_address-address2.
          ls_0006-hsnmr = gs_address-extnno.
          ls_0006-pstlz = gs_address-pin.
          ls_0006-ort01 = gs_address-city.
          ls_0006-telnr = gs_address-phone1.
          ls_0006-com01 = 'CELL'.
          ls_0006-num01 = gs_address-phone2.
          ls_0006-com02 = 'CELL'.
          ls_0006-num02 = gs_address-mobile.
        ENDIF.
      ELSEIF p_subty = '4'.
        IF gs_address-addresstype = 'EMERGENCYADDRESS'.
          ls_0006-subty = '4'.
          ls_0006-name2 = gs_address-name.
          ls_0006-ort01 = 'DEFAULT'.
          ls_0006-telnr = gs_address-phone1.
          ls_0006-land1 = 'IN'.
        ENDIF.
      ENDIF.

      ls_0006-infty = '0006'.

      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '0006'
          number        = ls_0006-pernr
          subtype       = ls_0006-subty
          validityend   = ls_0006-endda
          validitybegin = ls_0006-begda
          record        = ls_0006
          operation     = 'INS'
        IMPORTING
          return        = ls_ret.

      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0006-pernr.
        gs_log_report-subty = ls_0006-subty.
        gs_log_report-infty = ls_0006-infty.
        gs_log_report-status = 'S'.
        gs_log_report-text   = 'Data update successfully'.
        APPEND gs_log_report TO gt_log_report.

      ELSE.
        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0006-pernr.
        gs_log_report-subty = ls_0006-subty.
        gs_log_report-infty = ls_0006-infty.
        gs_log_report-status = 'E'.
        gs_log_report-text   = ls_ret-message.
        APPEND gs_log_report TO gt_log_report.
      ENDIF.
      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr
        IMPORTING
          return = ls_ret.
    ELSE.
      CLEAR gs_log_report.
      gs_log_report-pernr = lv_pernr.
      gs_log_report-subty = ls_0006-subty.
      gs_log_report-infty = '0006'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Employee id already locked in SAP'.
      APPEND gs_log_report TO gt_log_report.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR ls_desc.
ENDFORM.

DATA: lt_add TYPE  bapi1011_address,
      ls_add TYPE bapi1011_address.

DATA: bank_name   TYPE banka,
      bank_branch TYPE brnch.
DATA: bankcountry TYPE bapi1011_key-bank_ctry,
      bankkey     TYPE bapi1011_key-bank_key.

FORM f_update_infty_0009 USING p_subty TYPE subty .

**********Data declaration for infotype 0105*********
  DATA: lv_pernr TYPE pernr_d,       "Employee Number"
        ls_ret   TYPE  bapireturn1,  "Structure for Messages"
        ls_0009  TYPE p0009.         "Structure for Infotype 0009"
  DATA: lv_msg TYPE string.
*******************************************************
  DATA: lt_p0009 TYPE TABLE OF pa0009.
  DATA: ls_p0009 TYPE pa0009.

  DATA: lv_create TYPE boolean,
        lv_delete TYPE boolean.

  DATA: ls_banks TYPE tvarvc.
  DATA: default_key TYPE tvarvc.

  CLEAR lv_pernr.
  lv_pernr = gs_id_get-pernr.

  IF p_subty = '0' AND gs_bank-bankaccountnumber IS INITIAL.
    CLEAR gs_log_report.
    gs_log_report-pernr = lv_pernr.
    gs_log_report-subty = p_subty.
    gs_log_report-infty = '0009'.
    gs_log_report-status = 'S'.
    gs_log_report-text   = 'Employee Bank Accountnumber is not available in Greythr'.
    APPEND gs_log_report TO gt_log_report.
    EXIT.
  ENDIF.


  CLEAR ls_banks.
  SELECT SINGLE * FROM tvarvc INTO ls_banks WHERE name = 'ZHR_GREYTHR_BANK'
                                            AND type = 'P'.
  IF ls_banks-low EQ 'X'.
*first check is the value in infotype is present SAP
    SELECT SINGLE *
       FROM pa0009
       INTO ls_p0009
       WHERE pernr = lv_pernr
       AND subty = p_subty
       AND begda <= sy-datum
       AND endda >= sy-datum.
    IF sy-subrc EQ 0.
      lv_delete = abap_true.
      lv_create = abap_true. " For update without checks
    ELSE.
      lv_create = abap_true.
    ENDIF.
  ELSE.
*first check is the value in infotype is same as the one from API
    SELECT SINGLE *
       FROM pa0009
       INTO ls_p0009
       WHERE pernr = lv_pernr
       AND subty = p_subty
       AND begda <= sy-datum
       AND endda >= sy-datum.
    IF sy-subrc NE 0.
      lv_create = abap_true.
    ELSE.
      IF p_subty = '0'.
        IF ( gs_bank-bankaccountnumber = ls_p0009-bankn AND gs_bank-branchcode = ls_p0009-bankl ).
          CLEAR gs_log_report.
          gs_log_report-pernr = lv_pernr.
          gs_log_report-subty = p_subty.
          gs_log_report-infty = '0009'.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Employee Bank Details in Greythr and SAP same'.
          APPEND gs_log_report TO gt_log_report.
          EXIT.
        ELSE.
          lv_create = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
  IF lv_create = abap_true.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CLEAR:ls_0009, ls_ret.
      ls_0009-pernr = lv_pernr.

      READ TABLE gt_pernr_hire INTO gs_pernr_hire WITH KEY pernr = lv_pernr.
      IF sy-subrc = 0.
        ls_0009-begda = gs_pernr_hire-dat01.
      ELSE.
        ls_0009-begda = '19000101'.
      ENDIF.

* to avoid issues of the Error in Payroll past
      IF ls_0009-begda LT '20220101'.
        ls_0009-begda = '20220101'.
      ENDIF.

      IF ( gs_bank-bankaccountnumber NE ls_p0009-bankn AND gs_bank-branchcode NE ls_p0009-bankl ).
        ls_0009-begda = sy-datum.
      ENDIF.

      ls_0009-endda = '99991231'.

      ls_0009-subty = '0'.
      ls_0009-bankn = gs_bank-bankaccountnumber.
      ls_0009-emftx = gs_bank-nameasperbank.
      ls_0009-zlsch = 'T'.
      ls_0009-bkont = '11'.

      READ TABLE lt_lov INTO ls_lov WITH KEY id = gs_bank-bankname.
      IF sy-subrc EQ 0.
        CLEAR bank_name.
        bank_name = ls_lov-value.
      ENDIF.

      READ TABLE lt_lov INTO ls_lov WITH KEY id = gs_bank-bankbranch
                                              field_code = '16'
                                              bank_code = gs_bank-bankname.
      IF sy-subrc EQ 0.
        CLEAR bank_branch.
        bank_branch = ls_lov-value.
      ENDIF.

      CLEAR ls_add.
      ls_add-bank_name = bank_name.
      ls_add-bank_branch = bank_branch.
** If Bank key Not Present in Greythr itself  **
      IF gs_bank-branchcode IS INITIAL.
*        ls_0009-bankl = '913020026408306'.
*        ls_0009-banks = 'IN'.
        CLEAR gs_log_report.
        gs_log_report-pernr = ls_0009-pernr.
        gs_log_report-subty = ls_0009-subty.
        gs_log_report-infty = ls_0009-infty.
        gs_log_report-status = 'E'.
        gs_log_report-text   = |Ifsc Code is Missing|.
        APPEND gs_log_report TO gt_log_report.
      ELSE.
** If present in greythr and also in SAP   **
        READ TABLE lt_bank INTO ls_bank WITH KEY bankl = gs_bank-branchcode.
        IF sy-subrc EQ 0.
          ls_0009-banks = ls_bank-banks.
          ls_0009-bankl = ls_bank-bankl.
        ELSE.
**** If present in greythr but not in SAP Creating bank key***
          CLEAR lv_msg.
          PERFORM create_bank CHANGING lv_msg.
          IF lv_msg IS NOT INITIAL.
            CLEAR gs_log_report.
            gs_log_report-pernr = ls_0009-pernr.
            gs_log_report-subty = ls_0009-subty.
            gs_log_report-infty = ls_0009-infty.
            gs_log_report-status = 'E'.
            gs_log_report-text   = lv_msg.
            APPEND gs_log_report TO gt_log_report.
          ELSE.
            ls_0009-bankl = bankkey.
            ls_0009-banks = bankcountry.
            ls_0009-infty = '0009'.
          ENDIF.
        ENDIF.
        CLEAR: ls_ret.
        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
          EXPORTING
            infty         = '0009'
            number        = ls_0009-pernr
            subtype       = ls_0009-subty
            validityend   = ls_0009-endda
            validitybegin = ls_0009-begda
            record        = ls_0009
            operation     = 'INS'
          IMPORTING
            return        = ls_ret.

        IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

          CLEAR gs_log_report.
          gs_log_report-pernr = ls_0009-pernr.
          gs_log_report-subty = ls_0009-subty.
          gs_log_report-infty = ls_0009-infty.
          gs_log_report-status = 'S'.
          gs_log_report-text   = 'Data Inserted successfully'.
          APPEND gs_log_report TO gt_log_report.

        ELSE.
          CLEAR gs_log_report.
          gs_log_report-pernr = ls_0009-pernr.
          gs_log_report-subty = ls_0009-subty.
          gs_log_report-infty = ls_0009-infty.
          gs_log_report-status = 'E'.
          gs_log_report-text   = ls_ret-message.
          APPEND gs_log_report TO gt_log_report.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr
        IMPORTING
          return = ls_ret.
    ELSE.
      CLEAR gs_log_report.
      gs_log_report-pernr = lv_pernr.
      gs_log_report-subty = p_subty.
      gs_log_report-infty = '0009'.
      gs_log_report-status = 'E'.
      gs_log_report-text   = 'Employee id already locked in SAP'.
      APPEND gs_log_report TO gt_log_report.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.

FORM alv_display.
  REFRESH gt_fcat.
  CLEAR gs_fcat.
  PERFORM f_fieldcat USING  'PERNR' 'Personnel Number' 1  space.
  PERFORM f_fieldcat USING  'INFTY' 'Infotype' 2  space.
  PERFORM f_fieldcat USING  'SUBTY' 'Subtype' 3  space.
  PERFORM f_fieldcat USING  'STATUS' 'Status' 4  space.
  PERFORM f_fieldcat USING  'TEXT' 'Message' 5  space.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

*****ALV Display********************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_top_of_page = 'TOP-OF-PAGE '
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
    TABLES
      t_outtab           = gt_log_report.
ENDFORM.
FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4.
  gs_fcat-fieldname = f_var1.
  gs_fcat-seltext_m = f_var2.
  gs_fcat-col_pos = f_var3.
  gs_fcat-edit = f_var4.
  APPEND gs_fcat TO gt_fcat.
  CLEAR gs_fcat.
ENDFORM.
FORM top_of_page.
  REFRESH lt_header.
  ls_header-typ = 'H'.
  ls_header-info = 'INFOTYPE 0105 UPDATE DETAILS'.
  APPEND ls_header TO lt_header.
  CLEAR ls_header.

  WRITE sy-datum TO today_date.
  ls_header-typ = 'S'.
  ls_header-key = 'REPORT RUN DATE'.
  ls_header-info = today_date.
  APPEND ls_header TO lt_header.
  CLEAR ls_header.

  WRITE sy-uzeit TO today_time.
  ls_header-typ = 'S'.
  ls_header-key = 'REPORT RUN TIME'.
  ls_header-info = today_time.
  APPEND ls_header TO lt_header.
  CLEAR ls_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.
ENDFORM.

FORM f_get_hiredate .

*Fetching Employee Begin date from Infotype Table PA0041 based on selection screen input*
  SELECT pernr
         dat01
    FROM pa0041
    INTO TABLE gt_pernr_hire
    FOR ALL ENTRIES IN gt_emp_data
   WHERE pernr = gt_emp_data-pernr
     AND dar01 = 'S1'.

  IF sy-subrc = 0.
    SORT gt_pernr_hire BY pernr.
  ENDIF.

ENDFORM.

FORM f_greytipid_get .
*Fetching Greytip ID from Infotype Table PA0105 based on Empoth_update data*
  SELECT pernr
           usrid
      FROM pa0105
      INTO TABLE gt_id_get
      FOR ALL ENTRIES IN gt_emp_data
     WHERE pernr = gt_emp_data-pernr
       AND subty = '0002'.

  IF sy-subrc = 0.
    SORT gt_id_get BY pernr.
  ENDIF.

ENDFORM.

FORM create_bank CHANGING p_msg.
  DATA: bank_ctry TYPE banks,
        bank_key  TYPE bankk.
  CLEAR: bank_ctry,bank_key.
  DATA: default_key TYPE tvarvc.
*** changes ON 23.09.2022 **
*  CLEAR default_key.
*  SELECT SINGLE * FROM tvarvc
*                  INTO default_key
*                  WHERE name = 'ZHR_GREYTHR_BANKKEY_FIX'
*                  AND type = 'P'.

  bank_ctry = 'IN'.
  bank_key = gs_bank-branchcode.

  "Creating bank key
  CALL FUNCTION 'BAPI_BANK_CREATE'
    EXPORTING
      bank_ctry    = bank_ctry
      bank_key     = bank_key
      bank_address = ls_add
      i_xupdate    = 'X'
    IMPORTING
      return       = ls_ret
      bankcountry  = bankcountry
      bankkey      = bankkey.
  IF ls_ret IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    p_msg = ls_ret-message.
  ENDIF.
  IF sy-subrc EQ 0.
    MESSAGE 'Bank Key Created in SAP' TYPE 'S'.
  ENDIF.

ENDFORM.
*-------- Added By Samsudeen M on 03.03.2023 --------*
FORM update_costcenter_to_greyhr.
  DATA: lv_costcenter TYPE kostl,
        lv_ccid       TYPE string,
        lv_msg        TYPE string.
  DATA: lv_flag TYPE flag.
  TYPES: BEGIN OF costcenter,
           kostl TYPE kostl,
           kokrs TYPE kokrs,
         END OF costcenter.
  DATA: ls_costcenter TYPE costcenter.

  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY' AND type = 'P'.
  IF sy-subrc = 0.

  ENDIF.

  REFRESH: gt_log_report.
  LOOP AT gt_pernr ASSIGNING FIELD-SYMBOL(<fs_pernr>).
    CLEAR: lv_response,lv_costcenter,lv_ccid.
    "Greytip ID of Employee in Greyhr system
    DATA(lv_hire_date) = VALUE #( gt_pernr_hire[ pernr = <fs_pernr>-pernr ]-dat01 OPTIONAL ).
    DATA(lv_empid) = VALUE #( gt_id_get[ pernr = <fs_pernr>-pernr ]-usrid OPTIONAL ).
    IF lv_empid IS INITIAL.
      SHIFT <fs_pernr>-pernr LEFT DELETING LEADING '0'.
      CLEAR create_url.
      create_url = |{ ls_tvarvc-low }{ <fs_pernr>-pernr }|.
      "API Call
      PERFORM api_call.
      IF lv_response IS NOT INITIAL.
        CLEAR gs_employee.
        /ui2/cl_json=>deserialize(
       EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
       CHANGING
         data         = gs_employee ).

        IF gs_employee IS NOT INITIAL.
          lv_empid = gs_employee-employeeid.
          "Getting Costcenter from Greythr
          PERFORM f_get_costcenter_greyhr USING lv_empid
                                          CHANGING lv_ccid lv_costcenter.
        ENDIF.
      ELSE.
        APPEND VALUE #(  pernr = <fs_pernr>-pernr
                         infty = '0185'
                         subty = '0002'
                         status = 'E'
                         text = |Employee Greytip ID is not there| ) TO gt_log_report.
        CONTINUE.
      ENDIF.
    ELSE.
      "Getting Costcenter from Greythr
      PERFORM f_get_costcenter_greyhr USING lv_empid
                                      CHANGING lv_ccid lv_costcenter.
    ENDIF.

    CLEAR lv_flag.
    CLEAR create_url.
    create_url = |{ ls_tvarvc-low }{ lv_empid }/categories|.

    SELECT SINGLE * FROM pa0001
      INTO @DATA(ls_pa0001)
      WHERE pernr = @<fs_pernr>-pernr
        AND begda LE @sy-datum
        AND endda GE @sy-datum.
*    SELECT SINGLE * FROM hrp1001
*                    INTO @DATA(ls_hrp1001)
*                    WHERE otype = 'S'
*                    AND plvar = '01'
*                    AND rsign = 'A'
*                    AND relat = '008'
*                    AND sclas = 'P'
*                    AND sobid = @<fs_pernr>-pernr
*                    AND begda LE @sy-datum
*                    AND endda GE @sy-datum.
    IF sy-subrc = 0.
      SELECT SINGLE sobid FROM hrp1001
                      INTO @DATA(l_costcenter)
                      WHERE otype = 'S'
                      AND objid = @ls_pa0001-plans
                      AND plvar = '01'
                      AND rsign = 'A'
                      AND relat = '011'
                      AND sclas = 'K'
                      AND begda LE @sy-datum
                      AND endda GE @sy-datum.
      IF sy-subrc = 0.
        CLEAR ls_costcenter.
        ls_costcenter =  l_costcenter.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM csks
                    INTO @DATA(ls_csks)
                    WHERE kostl = @ls_costcenter-kostl.
    IF sy-subrc = 0.
      IF lv_costcenter = ls_csks-kostl.
        APPEND VALUE #(  pernr = <fs_pernr>-pernr
                         infty = 'COST'
                         subty = 'SAME'
                         status = 'S'
                         text = |Costcenter for Employee{ <fs_pernr>-pernr } is Same| ) TO gt_log_report.
        CONTINUE.
      ELSE.
        PERFORM api_update_costcenter USING create_url
                                            ls_csks-kostl
                                            lv_hire_date
                                      CHANGING lv_flag lv_msg.
        IF lv_flag = abap_false.
          APPEND VALUE #(  pernr = <fs_pernr>-pernr
                           infty = 'COST'
                           subty = 'UPD'
                           status = 'E'
                           text = |{ lv_msg }| ) TO gt_log_report.
          CONTINUE.
        ELSE.
          APPEND VALUE #(  pernr = <fs_pernr>-pernr
           infty = 'COST'
           subty = 'UPD'
           status = 'S'
           text = |Costcenter for Employee{ <fs_pernr>-pernr } is Updated| ) TO gt_log_report.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM f_get_costcenter_greyhr USING p_empid CHANGING p_ccid p_costcenter.
  TYPES: BEGIN OF position,
           id            TYPE string,
           category      TYPE string,
           value         TYPE string,
           effectivefrom TYPE string,
           effectiveto   TYPE string,
         END OF position.
  DATA: lt_position TYPE TABLE OF position.

  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY' AND type = 'P'.
  IF sy-subrc = 0.
    CLEAR create_url.
    create_url = |{ ls_tvarvc-low }{ p_empid }/categories|.
    "API Call
    PERFORM api_call.
    IF lv_response IS NOT INITIAL.
      REFRESH: lt_position.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = lt_position ).
      IF lt_position IS NOT INITIAL.
        DATA(lv_ccid) = VALUE #( lt_position[ category = '5' ]-value OPTIONAL ).
        SELECT SINGLE costcenter
                      FROM zhr_greyhr_cstcr INTO @DATA(lv_cstcr) WHERE cc_id = @lv_ccid.
        IF sy-subrc = 0.
          p_costcenter = lv_cstcr.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
FORM api_update_costcenter USING p_url p_kostl p_date CHANGING p_flag p_msg.
  DATA: lv_date(10) TYPE c.
  DATA: lv_first_date TYPE syst_datum.
  DATA: lv_last_date TYPE syst_datum.

  CLEAR lv_date.
  lv_date = p_date.
  CLEAR: lv_first_date,lv_last_date.
  CALL FUNCTION 'OIL_MONTH_GET_FIRST_LAST'
    EXPORTING
      i_month     = sy-datum+4(2)
      i_year      = sy-datum+0(4)
      i_date      = sy-datum
    IMPORTING
      e_first_day = lv_first_date
      e_last_day  = lv_last_date
    EXCEPTIONS
      wrong_date  = 1
      OTHERS      = 2.
  IF sy-subrc = 0.
    IF lv_date GT lv_first_date .
      DATA(lv_datum) = |{ p_date+0(4) }-{ p_date+4(2) }-{ p_date+6(2) }|.
    ELSE.
      lv_datum = |{ lv_first_date+0(4) }-{ lv_first_date+4(2) }-{ lv_first_date+6(2) }|.
    ENDIF.
  ENDIF.

  cl_http_client=>create_by_url(
     EXPORTING
     url = p_url
     IMPORTING
     client = lo_http_client
     EXCEPTIONS
     argument_not_found = 1
     plugin_not_active = 2
     internal_error = 3
     OTHERS = 4 ).

  CHECK lo_http_client IS BOUND.
  lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

  lo_http_client->request->set_method( 'POST' ).

  lo_http_client->request->set_content_type(
   EXPORTING
   content_type = if_rest_media_type=>gc_appl_json ).

  PERFORM get_access_token. "Getting access token Based on auth API

  SELECT SINGLE *  FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_LOV_API'
                                                     AND type = 'P'.
  IF sy-subrc = 0.
    DATA(lv_domain) = CONV string( ls_tvarvc-low ).
  ENDIF.

  lo_http_client->request->set_header_field( EXPORTING name  = 'ACCESS-TOKEN' value = str1 ).
  lo_http_client->request->set_header_field( EXPORTING name = 'x-greythr-domain' value = lv_domain ).

  DATA :ls_json    TYPE string,
        v_jsonload TYPE string.

  SELECT SINGLE cc_id FROM zhr_greyhr_cstcr INTO @DATA(lv_ccid) WHERE costcenter = @p_kostl.
  IF sy-subrc = 0.

  ENDIF.
  CLEAR v_jsonload.
  CONCATENATE '{'
     '"list":['
        '{'
           '"category":5,'
           '"value":' lv_ccid','
           '"effectiveDate":"' lv_datum'"'
       ' }'
     ']'
  '}' INTO v_jsonload.
*  CONDENSE v_jsonload NO-GAPS.
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
  IF lv_codes = 201.
    lv_response = lo_http_client->response->get_cdata( ).
    WAIT UP TO 2 SECONDS.
    p_flag = abap_true.
  ELSE.
    p_msg = lv_http_error.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_manager_details
*&---------------------------------------------------------------------*
FORM f_get_manager_details CHANGING  lt_emp_data LIKE gt_emp_data ..
*  REFRESH gt_log_report.
  LOOP AT lt_emp_data ASSIGNING FIELD-SYMBOL(<fs_pernr>) WHERE greyt_manager_upd IS INITIAL.
    DATA(lv_empid) = VALUE #( gt_id_get[ pernr = <fs_pernr>-pernr ]-usrid OPTIONAL ).
    IF lv_empid IS INITIAL.
      SHIFT <fs_pernr>-pernr LEFT DELETING LEADING '0'.
      CLEAR create_url.
      create_url = |{ ls_tvarvc-low }{ <fs_pernr>-pernr }|.
      "API Call
      PERFORM api_call.
      IF lv_response IS NOT INITIAL.
        CLEAR gs_employee.
        /ui2/cl_json=>deserialize(
       EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
       CHANGING
         data         = gs_employee ).

        IF gs_employee IS NOT INITIAL.
          lv_empid = gs_employee-employeeid.
          "Get Manager Details of employee from greythr
          PERFORM f_getapi_managerdtls USING lv_empid  <fs_pernr>-pernr <fs_pernr>-greyt_manager_upd.
        ENDIF.
      ELSE.
        APPEND VALUE #(  pernr = <fs_pernr>-pernr
                         infty = '9021'
                         subty = '0000'
                         status = 'E'
                         text = |Employee Greytip ID is not there| ) TO gt_log_report.
        <fs_pernr>-greyt_manager_upd = 'E'.
        CONTINUE.
      ENDIF.
    ELSE.
      "Get Manager Details of employee from greythr
      PERFORM f_getapi_managerdtls USING lv_empid <fs_pernr>-pernr <fs_pernr>-greyt_manager_upd.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_getapi_managerdtls
*&---------------------------------------------------------------------*
FORM f_getapi_managerdtls  USING    p_lv_empid
                                    p_lv_empno TYPE pernr_d
                                    p_mngr_upd TYPE xfeld.
  DATA: lt_mgrdtls TYPE TABLE OF zhr_manager_dtls_st,
        lv_flag    TYPE flag,
        lv_msg     TYPE string.

  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY' AND type = 'P'.
  IF sy-subrc = 0.
    CLEAR create_url.
    create_url = |{ ls_tvarvc-low }{  p_lv_empid }/org-tree|.
    "API Call
    PERFORM api_call.
    IF lv_response IS NOT INITIAL.
      REFRESH: lt_mgrdtls.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = lt_mgrdtls ).

      DATA(ls_mgrdtls) = VALUE #( lt_mgrdtls[ 1 ] OPTIONAL ).
      IF ls_mgrdtls IS NOT INITIAL.
        DATA(l_empdtls) = VALUE #( gt_pernr_hire[ pernr = p_lv_empno ] OPTIONAL ).
        "Fetching existing data for data is same or not
        SELECT SINGLE * FROM pa9021 INTO @DATA(l_mgrdtls) WHERE pernr = @p_lv_empno
                                                          AND begda LE @sy-datum
                                                          AND endda GE @sy-datum.
        DATA(l_greyid) = |{ ls_mgrdtls-manager-name }-{ ls_mgrdtls-manager-employeeid }|.
        TRANSLATE l_greyid TO UPPER CASE.
        SHIFT l_mgrdtls-greytmngid LEFT DELETING LEADING '0'.
        IF l_mgrdtls-greytid = l_greyid AND l_mgrdtls-greytmngid = ls_mgrdtls-manager-employeeno.
          APPEND VALUE #(  pernr = l_empdtls-pernr
                           infty = '9021'
                           subty = '0000'
                           status = 'S'
                           text = |Data is same in Greythr and SAP| ) TO gt_log_report.
          p_mngr_upd = 'X'.
        ELSE.
          DATA(l_p9021) = VALUE p9021( pernr    = l_empdtls-pernr
                                       infty    = '9021'
                                       begda    = sy-datum
                                       endda    = '99991231'
                                       greytid  = |{ ls_mgrdtls-manager-name }-{ ls_mgrdtls-manager-employeeid }|
                                       greytmngid = ls_mgrdtls-manager-employeeno ).
          "Infotype 9021 Updation
          PERFORM update_infotype_9021 USING l_p9021 CHANGING lv_flag lv_msg.
          IF lv_flag IS INITIAL.
            APPEND VALUE #(  pernr = l_empdtls-pernr
                             infty = '9021'
                             subty = '0000'
                             status = 'S'
                             text = |Record Inserted Successfully| ) TO gt_log_report.
********update_manager_details from greytHR *********
            p_mngr_upd = 'X'. "9022
          ELSE.
            APPEND VALUE #(  pernr = l_empdtls-pernr
                             infty = '9021'
                             subty = '0000'
                             status = 'E'
                             text = lv_msg ) TO gt_log_report.
            p_mngr_upd = 'E'.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #(  pernr = l_empdtls-pernr
                       infty = '9021'
                       subty = '0000'
                       status = 'E'
                       text = |No Manager ID details available in Greythr| ) TO gt_log_report.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_manager_details
*&---------------------------------------------------------------------*
FORM f_sync_bupa_emp_single CHANGING It_emp_data LIKE gt_emp_data.
  DATA lt_pernr_tab       TYPE /shcm/t_pernr_change.
  DATA lt_pernr_tab_a     TYPE pernr_tab.
  DATA ls_pernr           TYPE /shcm/s_pernr_change.
  DATA lo_bupa_sync       TYPE REF TO /shcm/if_sync_empl_to_bupa_op.
  DATA lv_is_ok           TYPE boole_d.
  DATA lv_taskname        TYPE string.
  DATA lv_task_done       TYPE char1.
  DATA lo_message_handler TYPE REF TO cl_hrpa_message_list.

  CREATE OBJECT lo_message_handler.
  /shcm/cl_sync_empl_to_bupa_op=>gv_migration_is_active = abap_true.

  CLEAR lv_is_ok.
  IF It_emp_data[] IS NOT INITIAL.

    LOOP AT It_emp_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE vend_code_upd IS INITIAL.

      refresh lt_pernr_tab.
      CLEAR ls_pernr.

      ls_pernr-pernr = <fs_data>-pernr.
      APPEND ls_pernr TO lt_pernr_tab.

      "Lock the employee
      CALL METHOD cl_hrpa_masterdata_enq_deq=>enqueue_by_pernr
        EXPORTING
          tclas           = 'A'
          pernr           = <fs_data>-pernr
          message_handler = lo_message_handler
        IMPORTING
          is_ok           = lv_is_ok.

      IF lv_is_ok = abap_true.

        "Synchronous call: Calling the wrapper function module
        CALL FUNCTION '/SHCM/TRIGGER_BUPA_SYNC' DESTINATION 'NONE'
          EXPORTING
            it_pernr_tab       = lt_pernr_tab
            iv_suppress_log    = abap_false
            iv_migration_check = abap_true.

        "Release the employee
        CALL METHOD cl_hrpa_masterdata_enq_deq=>dequeue_by_pernr
          EXPORTING
            tclas = 'A'
            pernr = <fs_data>-pernr.

        APPEND VALUE #(  pernr = <fs_data>-pernr
                       infty = '0000'
                       subty = '0000'
                       status = 'S'
                       text = |Personnel Number is processed and Synced to BP| ) TO gt_log_report.
********update vendor code for pernr flag *********
        <fs_data>-vend_code_upd = 'X'. "9022
      ELSE.
        APPEND VALUE #(  pernr = <fs_data>-pernr
                       infty = '0000'
                       subty = '0000'
                       status = 'S'
                       text = |Personnel Number is locked. Skipped -| ) TO gt_log_report.
        <fs_data>-vend_code_upd = 'E'.
      ENDIF.

    ENDLOOP.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_infotype_9021
*&---------------------------------------------------------------------*
FORM update_infotype_9021  USING    p_l_p9021 TYPE p9021
                           CHANGING p_lv_flag p_lv_msg.
  DATA: ls_ret TYPE  bapireturn1.  "Structure for Messages"

  IF p_l_p9021 IS NOT INITIAL.

    DATA(l_pernr) =  p_l_p9021-pernr.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = l_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '9021'
          number        = p_l_p9021-pernr
          subtype       = p_l_p9021-subty
          validityend   = p_l_p9021-endda
          validitybegin = p_l_p9021-begda
          record        = p_l_p9021
          operation     = 'INS'
        IMPORTING
          return        = ls_ret.
      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        CLEAR ls_ret.
      ELSE.
        p_lv_flag = abap_true.
        p_lv_msg = ls_ret-message.
      ENDIF.
    ELSE.
      p_lv_flag = abap_true.
      p_lv_msg  = |Pernr { l_pernr } is already locked in SAP|.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = l_pernr
      IMPORTING
        return = ls_ret.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_employee_work_details
*&---------------------------------------------------------------------*
FORM f_employee_work_details CHANGING  lt_emp_data LIKE gt_emp_data.
*  REFRESH gt_log_report.
  LOOP AT lt_emp_data ASSIGNING FIELD-SYMBOL(<fs_pernr>) WHERE greyt_data_upd IS INITIAL.
    DATA(lv_empid) = VALUE #( gt_id_get[ pernr = <fs_pernr>-pernr ]-usrid OPTIONAL ).
    IF lv_empid IS INITIAL.
      SHIFT <fs_pernr>-pernr LEFT DELETING LEADING '0'.
      CLEAR create_url.
      create_url = |{ ls_tvarvc-low }{ <fs_pernr>-pernr }|.
      "API Call
      PERFORM api_call.
      IF lv_response IS NOT INITIAL.
        CLEAR gs_employee.
        /ui2/cl_json=>deserialize(
       EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
       CHANGING
         data         = gs_employee ).

        IF gs_employee IS NOT INITIAL.
          lv_empid = gs_employee-employeeid.
          "Get all Category Details of position
          PERFORM f_get_position_dtls USING lv_empid <fs_pernr>-pernr <fs_pernr>-greyt_data_upd.
        ENDIF.
      ELSE.
        APPEND VALUE #(  pernr = <fs_pernr>-pernr
                         infty = '9021'
                         subty = '0000'
                         status = 'E'
                         text = |Employee Greytip ID is not there| ) TO gt_log_report.
        <fs_pernr>-greyt_data_upd = 'E'.
        CONTINUE.
      ENDIF.
    ELSE.
      PERFORM f_get_position_dtls USING lv_empid <fs_pernr>-pernr <fs_pernr>-greyt_data_upd.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_position_dtls
*&---------------------------------------------------------------------*
FORM f_get_position_dtls  USING    p_lv_empid
                                   p_pernr TYPE pernr_d
                                   p_emp_upd TYPE flag.
  TYPES: BEGIN OF ty_posdtls,
           id            TYPE string,
           category      TYPE string,
           value         TYPE string,
           effectivefrom TYPE string,
           effectiveto   TYPE string,
         END OF ty_posdtls.
  DATA: lt_posdtls TYPE TABLE OF ty_posdtls.
  DATA: l_p9020 TYPE p9020.
  DATA: l_p9020_tmp TYPE p9020.
  DATA: lv_flag TYPE flag,
        lv_msg  TYPE string.


  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_EMPLOYEE_FAMILY' AND type = 'P'.
  IF sy-subrc = 0.
    CLEAR create_url.
    create_url = |{ ls_tvarvc-low }{  p_lv_empid }/categories|.
    "API Call
    PERFORM api_call.
    IF lv_response IS NOT INITIAL.
      REFRESH: lt_posdtls.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = lt_posdtls ).
      SORT lt_posdtls[] BY category.

      LOOP AT lt_posdtls ASSIGNING FIELD-SYMBOL(<fs_posdtls>).
        CASE <fs_posdtls>-category.
            "Location
          WHEN '1'.
            DATA(l_fcode) = CONV num02( '20' ).
            DATA(l_value) = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @DATA(l_data) WHERE field_code = @l_fcode
                                                                     AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-location = l_data-value.
            ENDIF.
            "Department
          WHEN '2'.
            l_fcode = CONV num02( '22' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                             AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-department = l_data-value.
            ENDIF.
            "Division
          WHEN '3'.
            l_fcode = CONV num02( '28' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                             AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-division = l_data-value.
            ENDIF.
            "Grade
          WHEN '4'.
            l_fcode = CONV num02( '29' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                             AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-grade = l_data-value.
            ENDIF.
            "Cost Center
          WHEN '5'.
            l_fcode = CONV num02( '18' ).
            l_value = CONV zcc_id( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greyhr_cstcr INTO @DATA(l_costcenter) WHERE field_code = @l_fcode
                                                                           AND cc_id = @l_value.
            IF sy-subrc = 0.
              l_p9020-kostl = l_costcenter-costcenter.
            ENDIF.
            "Designation
          WHEN '6'.
            l_fcode = CONV num02( '21' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                             AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-design = l_data-value.
            ENDIF.
            "State
          WHEN '7'.
            l_fcode = CONV num02( '17' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                             AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-state = l_data-value.
            ENDIF.
            "Company
          WHEN '8'.
            l_fcode = CONV num02( '23' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                             AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-company = l_data-value.
            ENDIF.
            "Employee Shift Details
          WHEN '9'.
            l_fcode = CONV num02( '30' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                             AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-empshift = l_data-value.
            ENDIF.
            "Work Location
          WHEN '10'.
            l_fcode = CONV num02( '25' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                              AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-worklocation = l_data-value.
            ENDIF.
            "Pillers
          WHEN '11'.
            l_fcode = CONV num02( '26' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                             AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-pillers = l_data-value.
            ENDIF.
            "Temporary Employee
          WHEN '12'.
            l_fcode = CONV num02( '33' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                             AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-tempemployee = l_data-value.
            ENDIF.
            "Base Location
          WHEN '13'.
            l_fcode = CONV num02( '24' ).
            l_value = CONV char10( <fs_posdtls>-value ).
            SELECT SINGLE * FROM zhr_greythr_lov INTO @l_data WHERE field_code = @l_fcode
                                                              AND id = @l_value.
            IF sy-subrc = 0.
              l_p9020-baselocation = l_data-value.
            ENDIF.
        ENDCASE.
      ENDLOOP.
      DATA(l_empdtls) = VALUE #( gt_pernr_hire[ pernr = p_pernr ] OPTIONAL ).
      IF l_empdtls IS NOT INITIAL.
        l_p9020-pernr = l_empdtls-pernr.
        l_p9020-begda = sy-datum.
        l_p9020-endda = '99991231'.
      ENDIF.
* Fetching infotype 9020 data for employee
      SELECT SINGLE * FROM pa9020 INTO @DATA(l_9020) WHERE pernr = @p_pernr
                                                     AND begda LE @sy-datum
                                                     AND endda GE @sy-datum.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING l_9020 TO l_p9020_tmp.
      ENDIF.
      IF l_p9020_tmp-kostl        = l_p9020-kostl        AND l_p9020_tmp-baselocation = l_p9020-baselocation  AND
         l_p9020_tmp-location     = l_p9020-location     AND l_p9020_tmp-department   = l_p9020-department    AND
         l_p9020_tmp-division     = l_p9020-division     AND l_p9020_tmp-grade        = l_p9020-grade         AND
         l_p9020_tmp-state        = l_p9020-state        AND l_p9020_tmp-design       = l_p9020-design        AND
         l_p9020_tmp-company      = l_p9020-company      AND l_p9020_tmp-empshift     = l_9020-empshift       AND
         l_p9020_tmp-worklocation = l_p9020-worklocation AND l_p9020_tmp-pillers      = l_9020-pillers.
        APPEND VALUE #(  pernr = p_pernr
                         infty = '9020'
                         subty = '0000'
                         status = 'S'
                         text = |Data is same in Greythr and SAP| ) TO gt_log_report.
        p_emp_upd = 'X'.
      ELSE.
        PERFORM update_infotype_9020 USING l_p9020 CHANGING lv_flag lv_msg.
        IF lv_flag IS INITIAL.
          APPEND VALUE #(  pernr = l_empdtls-pernr
                           infty = '9020'
                           subty = '0000'
                           status = 'S'
                           text = |Record Inserted Successfully| ) TO gt_log_report.
********update_infotype_9021 in *********
          p_emp_upd = 'X'. "9021
        ELSE.
          APPEND VALUE #(  pernr = l_empdtls-pernr
                           infty = '9020'
                           subty = '0000'
                           status = 'E'
                           text = lv_msg ) TO gt_log_report.
          p_emp_upd = 'E'.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #(  pernr = p_pernr
                       infty = '9020'
                       subty = '0000'
                       status = 'E'
                       text = |No Position details available in Greythr| ) TO gt_log_report.
      p_emp_upd = 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_infotype_9020
*&---------------------------------------------------------------------*
FORM update_infotype_9020  USING    p_l_p9020 TYPE p9020
                           CHANGING p_lv_flag
                                    p_lv_msg.
  DATA: ls_ret TYPE  bapireturn1.
  IF p_l_p9020 IS NOT INITIAL.
    DATA(l_pernr) = p_l_p9020-pernr.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = l_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '9020'
          number        = p_l_p9020-pernr
          subtype       = p_l_p9020-subty
          validityend   = p_l_p9020-endda
          validitybegin = p_l_p9020-begda
          record        = p_l_p9020
          operation     = 'INS'
        IMPORTING
          return        = ls_ret.
      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        CLEAR ls_ret.
      ELSE.
        p_lv_flag = abap_true.
        p_lv_msg = ls_ret-message.
      ENDIF.
    ELSE.
      p_lv_flag = abap_true.
      p_lv_msg  = |Pernr { l_pernr } is already locked in SAP|.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = l_pernr
      IMPORTING
        return = ls_ret.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_costcenter_9001
*&---------------------------------------------------------------------*
FORM f_update_costcenter_9001 CHANGING  lt_emp_data LIKE gt_emp_data .
  DATA: lv_costcenter TYPE kostl,
        lv_ccid       TYPE string,
        lv_msg        TYPE string.
  DATA: lv_flag TYPE flag,
        l_p9001 TYPE p9001.

  REFRESH gt_log_report.

  LOOP AT lt_emp_data ASSIGNING FIELD-SYMBOL(<fs_pernr>) WHERE kostl_upd IS INITIAL.
    DATA(lv_empid) = VALUE #( gt_id_get[ pernr = <fs_pernr>-pernr ]-usrid OPTIONAL ).
    SELECT SINGLE kostl FROM pa9001 INTO @DATA(l_costcenter) WHERE pernr = @<fs_pernr>-pernr
                                                         AND begda LE @sy-datum
                                                         AND endda GE @sy-datum.
    IF sy-subrc = 0.

    ENDIF.
    "If greytip ID not there then get it from greythr
    IF lv_empid IS INITIAL.
      CLEAR create_url.
      create_url = |{ ls_tvarvc-low }{ <fs_pernr>-pernr }|.
      "API Call
      PERFORM api_call.
      IF lv_response IS NOT INITIAL.
        CLEAR gs_employee.
        /ui2/cl_json=>deserialize(
       EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
       CHANGING
         data         = gs_employee ).

        IF gs_employee IS NOT INITIAL.
          lv_empid = gs_employee-employeeid.
          "Getting Costcenter from Greythr
          PERFORM f_get_costcenter_greyhr USING lv_empid
                                          CHANGING lv_ccid lv_costcenter.
        ENDIF.
      ELSE.
        APPEND VALUE #(  pernr =  <fs_pernr>-pernr
                         infty = '0185'
                         subty = '0002'
                         status = 'E'
                         text = |Employee Greytip ID is not there| ) TO gt_log_report.
        <fs_pernr>-kostl_upd = 'E'.
        CONTINUE.
      ENDIF.
      "If greythr ID is present then directly Getting costcenter and Updating
    ELSE.
      "Getting Costcenter from Greythr
      PERFORM f_get_costcenter_greyhr USING lv_empid
                                      CHANGING lv_ccid lv_costcenter.
    ENDIF.

    IF lv_costcenter IS NOT INITIAL.
      IF l_costcenter IS NOT INITIAL.
        IF l_costcenter = lv_costcenter.
          APPEND VALUE #(  pernr = <fs_pernr>-pernr
                           infty = '9001'
                           subty = '0000'
                           status = 'S'
                           text = |Costcenter is Same in Greythr and SAP| ) TO gt_log_report.
********Update Cost Centre update field Once Cost Centre from GreytHR*********
          <fs_pernr>-kostl_upd = 'X'.
          CONTINUE.
        ENDIF.
      ENDIF.
      DATA(l_empdtls) = VALUE #( gt_pernr_hire[ pernr = <fs_pernr>-pernr ] OPTIONAL ).
      IF l_empdtls IS NOT INITIAL.
        l_p9001-pernr = l_empdtls-pernr.
        l_p9001-begda = sy-datum.
        l_p9001-endda = '99991231'.
      ENDIF.
      l_p9001-kostl = lv_costcenter.
      l_p9001-infty = '9001'.
      PERFORM f_update_9001 USING l_p9001 CHANGING lv_flag lv_msg.
      IF lv_flag IS NOT INITIAL.
        APPEND VALUE #(  pernr = <fs_pernr>-pernr
                         infty = '9001'
                         subty = '0000'
                         status = 'E'
                         text = lv_msg ) TO gt_log_report.
        <fs_pernr>-kostl_upd = 'E'.
        CONTINUE.
      ELSE.
        APPEND VALUE #(  pernr = <fs_pernr>-pernr
                         infty = '9001'
                         subty = '0000'
                         status = 'S'
                         text = |Costcenter Updated Successfully| ) TO gt_log_report.
********Update Cost Centre update field Once Cost Centre from GreytHR*********
        <fs_pernr>-kostl_upd = 'X'.
      ENDIF.
    ELSE. "If costcenter doesnot exists
      APPEND VALUE #(  pernr = <fs_pernr>-pernr
                       infty = '0185'
                       subty = '0002'
                       status = 'E'
                       text = |Costcenter Not available in Greythr| ) TO gt_log_report.
      <fs_pernr>-kostl_upd = 'E'.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_9001
*&---------------------------------------------------------------------*
FORM f_update_9001  USING    p_l_p9001 TYPE p9001
                    CHANGING p_lv_flag
                             p_lv_msg.
  DATA: ls_ret TYPE  bapireturn1.
  IF p_l_p9001 IS NOT INITIAL.
    DATA(l_pernr) = p_l_p9001-pernr.
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = l_pernr
      IMPORTING
        return = ls_ret.
    IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
      CLEAR: ls_ret.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '9001'
          number        = p_l_p9001-pernr
          subtype       = p_l_p9001-subty
          validityend   = p_l_p9001-endda
          validitybegin = p_l_p9001-begda
          record        = p_l_p9001
          operation     = 'INS'
        IMPORTING
          return        = ls_ret.
      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        p_lv_flag = abap_true.
        p_lv_msg = ls_ret-message.
      ENDIF.
    ELSE.
      p_lv_flag = abap_true.
      p_lv_msg  = |Pernr { l_pernr } is already locked in SAP|.
    ENDIF.

*Deque employee always
    CLEAR ls_ret.
    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = l_pernr
      IMPORTING
        return = ls_ret.
  ENDIF.
ENDFORM.
