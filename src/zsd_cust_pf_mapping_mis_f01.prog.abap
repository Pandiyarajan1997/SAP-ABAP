*----------------------------------------------------------------------*
***INCLUDE ZSD_CUST_PF_MAPPING_MIS_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_get_bearer_token
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GV_TOKEN
*&      <-- GV_MSG
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_get_bearer_token
*&---------------------------------------------------------------------*
FORM f_get_bearer_token CHANGING p_token p_msg.
  DATA : l_uname TYPE  syst_uname.
  DATA : l_password TYPE  char50.
  SELECT SINGLE low FROM tvarvc
    INTO l_password
    WHERE name = 'ZSD_MIS_BEARER_PWD'
  AND   type = 'P'.
  SELECT SINGLE low FROM tvarvc
    INTO l_uname
    WHERE name = 'ZSD_MIS_BEARER_UID'
  AND   type = 'P'.

  CALL FUNCTION 'ZGET_BEARER_TOKEN_MIS'
    EXPORTING
      username          = l_uname
      password          = l_password
    IMPORTING
      bearer_token      = p_token
      msg               = p_msg
    EXCEPTIONS
      maintain_url_link = 1
      input_error       = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1. p_msg = 'ULR Link Error'.
      WHEN 2. p_msg = 'Invalid Input'.
      WHEN OTHERS. p_msg = 'Unknown Error'.
    ENDCASE.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form api_call
*&---------------------------------------------------------------------*
FORM f_dms_apicall .
** getting api Link from tvarvc table **
  DATA: lo_http_client TYPE REF TO if_http_client.
  DATA: lv_url TYPE string.
  DATA: lv_response   TYPE string, "API Response
        lv_codes      TYPE i,      "STATUS Code
        lv_http_error TYPE string. "STATUS Description
  DATA: lv_response1 TYPE string.

  DATA : lv_bearer_token TYPE string.

  CLEAR lv_url.
  SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc)
                  WHERE name = 'ZSD_CUST_PF_MAPCHK' AND type = 'P'.
  IF sy-subrc EQ 0.
    "Actual API Link
    CLEAR lv_url.
    lv_url = ls_tvarvc-low.
    WRITE: / 'Connecting to URL ', lv_url.
  ENDIF.

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


  lo_http_client->request->set_method(
   EXPORTING
   method = if_http_entity=>co_request_method_get ).


  lo_http_client->request->set_content_type(
   EXPORTING
   content_type = if_rest_media_type=>gc_appl_json ).

  "Header Data Fields for API
  lv_bearer_token = |Bearer { gv_token }|.
  lo_http_client->request->set_header_field(  EXPORTING  name  = 'Authorization'  value = lv_bearer_token ).

  lo_http_client->request->set_cdata(
    EXPORTING
    data = '{ JSON_Payload }' ).

  DATA :ls_json    TYPE string,
        v_jsonload TYPE string.


  lo_http_client->send(
   EXCEPTIONS
   http_communication_failure = 1
   http_invalid_state = 2 ).

  WRITE: / 'Requesting Data ', Sy-datum, sy-uzeit.

  CHECK sy-subrc = 0.
  lo_http_client->receive(
   EXCEPTIONS
   http_communication_failure = 1
   http_invalid_state = 2
   http_processing_failed = 3 ).

** API response Code **
  lo_http_client->response->get_status(
  IMPORTING
    code = lv_codes ).
** API response error Description **
  lo_http_client->response->get_status(
  IMPORTING
    reason = lv_http_error ).

  CLEAR lv_response.
  IF lv_codes = 200.
** Actual API Response **
    lv_response = lo_http_client->response->get_cdata( ).
  ELSE.
    WRITE: / 'Response Error ', lv_codes, Sy-datum, sy-uzeit.
  ENDIF.

  REPLACE ALL OCCURRENCES OF 'u0022'   IN lv_response WITH '"'.
  REPLACE ALL OCCURRENCES OF '\'  IN lv_response WITH ''.
  REPLACE ALL OCCURRENCES OF 'rn'  IN lv_response WITH ''.
  REPLACE ALL OCCURRENCES OF '/'  IN lv_response WITH ''.
  REPLACE ALL OCCURRENCES OF '[]'  IN lv_response WITH ''.

** Serialize API Response to JSON Format **
  CLEAR lv_response1.
  CALL METHOD /ui2/cl_json=>serialize
    EXPORTING
      data        = lv_response
      pretty_name = /ui2/cl_json=>pretty_mode-user
    RECEIVING
      r_json      = lv_response1.


  REPLACE ALL OCCURRENCES OF '\'  IN lv_response1 WITH ''.
  REPLACE ALL OCCURRENCES OF '/'  IN lv_response1 WITH ''.
  SHIFT lv_response1 LEFT DELETING LEADING '"'.

**Deserialize Converted JSON to Internal Table **
  /ui2/cl_json=>deserialize(
  EXPORTING
   json         = lv_response1
   pretty_name  = /ui2/cl_json=>pretty_mode-user
  CHANGING
   data         = gt_cust ). "Stock API Table

  WRITE: / 'Data De-Seraialized ', Sy-datum, sy-uzeit.

  SORT gt_cust BY org Channel customercode position_code.
  DESCRIBE TABLE gt_cust LINES DATA(lv_lines). "Count of Internal Table
  WRITE: / 'Count of Data Received ', lv_lines, Sy-datum, sy-uzeit.

** Run For Specific Customer **
  IF s_kunnr IS NOT INITIAL.
    DELETE gt_cust WHERE customercode NOT IN s_kunnr.
  ENDIF.

  SELECT kunnr FROM knvv INTO TABLE @DATA(lt_kunnr_sdms) WHERE vkorg = 'SDMS' AND vtweg = '20' AND spart = '10'.
  IF sy-subrc = 0.
    SORT lt_kunnr_sdms BY kunnr.
  ENDIF.

*get all the position codes and the respective orgunits
  SELECT * FROM hrp1001
    INTO TABLE @DATA(lt_hrp1001)
    WHERE otype = 'S'
      AND objid BETWEEN '20000000' AND '29999999'
      AND plvar = '01'
      AND rsign = 'A'
      AND relat = '003'
      AND begda <= @sy-datum
      AND endda >= @sy-datum
      AND sclas = 'O'.
  IF sy-subrc = 0.
    SORT lt_hrp1001 BY objid.
  ENDIF.

  DATA: lv_orgid TYPE hrobjid,
        lv_name1 TYPE name1_gp,
        lv_stext TYPE stext.


  LOOP AT gt_cust INTO gs_cust.

    CLEAR gs_cusposm.
    gs_cusposm-mandt         = sy-mandt.
    gs_cusposm-org           = gs_cust-org.
    gs_cusposm-Channel       = gs_cust-Channel.
    gs_cusposm-customercode  = |{ gs_cust-customercode ALPHA = IN }|.
    gs_cusposm-position_code = gs_cust-position_code.
    gs_cusposm-entdate       = sy-datum.

    CLEAR lv_orgid.
    READ TABLE lt_hrp1001 INTO DATA(ls_hrp1001) WITH KEY objid = gs_cusposm-position_code BINARY SEARCH.
    IF sy-subrc = 0.
      gs_cusposm-orgunit = ls_hrp1001-sobid+0(8).
      lv_orgid = gs_cusposm-orgunit.
    ENDIF.

    CLEAR: gs_custdet, lv_name1 .
    READ TABLE gt_custdet INTO gs_custdet WITH KEY kunnr = gs_cusposm-customercode BINARY SEARCH.
    IF sy-subrc = 0.
      gs_cusposm-name1 = gs_custdet-name1.
      lv_name1 = gs_custdet-name1.
    ENDIF.

    CLEAR: gs_hrp1000, lv_stext.
    READ TABLE gt_hrp1000 INTO gs_hrp1000 WITH KEY objid = gs_cusposm-position_code BINARY SEARCH.
    IF sy-subrc = 0.
      gs_cusposm-stext = gs_hrp1000-stext.
      lv_stext = gs_hrp1000-stext.
    ENDIF.

    APPEND gs_cusposm TO gt_cusposm.

    IF gs_cust-org = '6000'.
      READ TABLE lt_kunnr_sdms WITH KEY kunnr = gs_cusposm-customercode TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc = 0.
        gv_addtnl = gv_addtnl + 1.
        CLEAR gs_cusposm.
        gs_cusposm-mandt         = sy-mandt.
        gs_cusposm-org           = 'SDMS'.
        gs_cusposm-Channel       = gs_cust-Channel.
        gs_cusposm-customercode  = |{ gs_cust-customercode ALPHA = IN }|.
        gs_cusposm-position_code = gs_cust-position_code.
        gs_cusposm-entdate       = sy-datum.
        gs_cusposm-orgunit       = lv_orgid.
        gs_cusposm-name1         = lv_name1.
        gs_cusposm-stext         = lv_stext.
        APPEND gs_cusposm TO gt_cusposm.
      ENDIF.
    ENDIF.
  ENDLOOP.

  WRITE: / 'Count of Data added for SDMS ', gv_addtnl, Sy-datum, sy-uzeit.

*first select and check if already data for the date is there
  SELECT customercode FROM zsd_custpf_posm INTO TABLE @DATA(lt_temp) WHERE entdate = @sy-datum.
  IF sy-subrc = 0.

    CLEAR lv_lines.
    DESCRIBE TABLE lt_temp LINES lv_lines. "Count of Internal Table
    WRITE: / 'Count of existing data ', lv_lines, Sy-datum, sy-uzeit.

    DELETE FROM zsd_custpf_posm WHERE entdate = sy-datum.
    IF sy-subrc = 0.
      WRITE: / 'Data Deleted from Table ', lv_lines, Sy-datum, sy-uzeit.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

*now insert for todays date
  IF gt_cusposm[] IS NOT INITIAL.

    CLEAR lv_lines.
    DESCRIBE TABLE gt_cusposm LINES lv_lines. "Count of Internal Table
    WRITE: / 'Count of New Data ', lv_lines, Sy-datum, sy-uzeit.

    SORT gt_cusposm BY customercode org.
    DELETE ADJACENT DUPLICATES FROM gt_cusposm COMPARING customercode org.
    CLEAR lv_lines.
    DESCRIBE TABLE gt_cusposm LINES lv_lines. "Count of Internal Table
    WRITE: / 'Count of New Data after deleting duplicates', lv_lines, Sy-datum, sy-uzeit.


    MODIFY zsd_custpf_posm FROM TABLE gt_cusposm.
    IF sy-subrc = 0.
      WRITE: / 'Data inserted into Table', sy-dbcnt, Sy-datum, sy-uzeit.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_VKORG_LOW
*&---------------------------------------------------------------------*
FORM f_process  USING    p_vkorg TYPE vkorg.

  DATA: ltt_knvp  TYPE TABLE OF knvp,
        ls_postab TYPE zhr_so_to_top_ts.

  DATA : lo_check TYPE REF TO zcl_common_check.
  CREATE OBJECT lo_check.

  DATA : lt_cusblk TYPE zsd_tt_cust_block,
         ls_cusblk TYPE zsd_st_cust_block.

  DATA: lt_tvarvc TYPE tvarvc_t.
  REFRESH gt_cusposm.
*select all entries from zsd_custpf_posm based on todays date.
  SELECT * FROM zsd_custpf_posm INTO TABLE gt_cusposm WHERE org = p_vkorg AND entdate = sy-datum AND customercode IN s_kunnr.
  IF sy-subrc NE 0.
    WRITE: / 'No Record in zsd_custpf_posm for Sales Org and Entry Date', p_vkorg, Sy-datum.
    EXIT.
  ENDIF.

*store Additional orgunit ID with no additional ZCGM in a TVARVC Variable
  SELECT * FROM tvarvc INTO TABLE lt_tvarvc WHERE name = 'ZSOTOPPOSDIFFORG' AND type = 'S'.
  IF sy-subrc = 0.
  ENDIF.

*******customer block check********
  SELECT SINGLE bukrs FROM tvko INTO @DATA(ls_bukrs) WHERE vkorg = @p_vkorg.
  lo_check->customer_block_check(
    EXPORTING
      bukrs                     = ls_bukrs      " Company Code
      vkorg                     = p_vkorg         " Sales Organization
    CHANGING
      cust_tab                  = lt_cusblk   ).     " Table Type for Customer Block Check

  SORT : lt_cusblk BY kunnr.

*call the Positing Hierarchy fm
  CALL FUNCTION 'ZHR_SO_TO_TOP_POSITION_2'
*   EXPORTING
*     SO_POSITION        =
*   IMPORTING
*     MESSAGE            =
    TABLES
      it_positions = gt_positions
      it_postab    = gt_postab.

  SELECT kunnr,vkorg,vtweg,spart FROM knvv
    INTO TABLE @DATA(lt_knvv)
    FOR ALL ENTRIES IN @gt_cusposm
    WHERE kunnr = @gt_cusposm-customercode AND vkorg = @gt_cusposm-org AND vtweg = '20' AND loevm NE 'X'.
  IF sy-subrc = 0.
    SELECT * FROM knvp INTO TABLE @DATA(lt_knvp)
      FOR ALL ENTRIES IN @lt_knvv WHERE kunnr = @lt_knvv-kunnr AND vkorg = @lt_knvv-vkorg AND vtweg = @lt_knvv-vtweg AND spart = @lt_knvv-spart.

    IF sy-subrc = 0.
      DELETE lt_knvp WHERE parvw NOT IN gr_parvw.
    ENDIF.

  ENDIF.

  CLEAR: gv_total, gv_errors, gv_success, gv_ignore.

  IF lt_knvp[] IS NOT INITIAL.

    LOOP AT gt_cusposm ASSIGNING FIELD-SYMBOL(<fs_cusposm>).

      gv_total = gv_total + 1.


      IF <fs_cusposm>-org IS INITIAL.
        gv_errors = gv_errors + 1.
        CLEAR gs_cuselog.
        MOVE-CORRESPONDING <fs_cusposm> TO gs_cuselog.
        gs_cuselog-log_type = 'SO'.
        gs_cuselog-status = 'E'.
        gs_cuselog-message = 'Sales Org Missing'.
        APPEND gs_cuselog TO gt_cuselog.
        CONTINUE.
      ENDIF.

      IF <fs_cusposm>-position_code IS INITIAL.
        gv_errors = gv_errors + 1.
        CLEAR gs_cuselog.
        MOVE-CORRESPONDING <fs_cusposm> TO gs_cuselog.
        gs_cuselog-log_type = 'SO'.
        gs_cuselog-status = 'E'.
        gs_cuselog-message = 'position_code is Missing'.
        APPEND gs_cuselog TO gt_cuselog.
        CONTINUE.
      ENDIF.

      CLEAR ls_cusblk.
      READ TABLE lt_cusblk INTO ls_cusblk WITH KEY kunnr = <fs_cusposm>-customercode BINARY SEARCH.
      IF sy-subrc = 0.
        IF ls_cusblk-block = 'X'.
          gv_errors = gv_errors + 1.
          CLEAR gs_cuselog.
          MOVE-CORRESPONDING <fs_cusposm> TO gs_cuselog.
          gs_cuselog-log_type = 'SO'.
          gs_cuselog-status = 'E'.
          gs_cuselog-message = 'Customer Code is Blocked in SAP'.
          APPEND gs_cuselog TO gt_cuselog.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE lt_knvp INTO DATA(ls_knvp) WITH KEY kunnr = <fs_cusposm>-customercode vkorg = <fs_cusposm>-org vtweg = '20' spart = '10'.
      IF sy-subrc = 0.

        REFRESH ltt_knvp.
*        ltt_knvp[] = lt_knvp[].
*        DELETE ltt_knvp WHERE kunnr NE <fs_cusposm>-customercode.

        IF <fs_cusposm>-org = '1000'.
          SELECT * FROM knvp INTO TABLE ltt_knvp
            WHERE kunnr = <fs_cusposm>-customercode
              AND vkorg = <fs_cusposm>-org
              AND vtweg = '20'
              AND parvw IN gr_parvw.
        ELSE.
          SELECT * FROM knvp INTO TABLE ltt_knvp
            WHERE kunnr = <fs_cusposm>-customercode
              AND vkorg = <fs_cusposm>-org
              AND vtweg = '20'
              AND spart = '10'
              AND parvw IN gr_parvw.
        ENDIF.


        CLEAR ls_postab.
        READ TABLE gt_postab INTO ls_postab WITH KEY plans_m = <fs_cusposm>-position_code.
        IF sy-subrc = 0.

          IF ls_postab-pos_xn IS INITIAL OR ls_postab-pos_xl IS INITIAL OR ls_postab-pos_xk IS INITIAL OR ls_postab-pos_yd IS INITIAL OR
             ls_postab-pos_xj IS INITIAL OR ls_postab-pernr_l5 IS INITIAL OR ls_postab-pernr_l3 IS INITIAL OR ls_postab-pernr_l2 IS INITIAL OR
             ls_postab-pernr_zs IS INITIAL OR ls_postab-pernr_l1 IS INITIAL OR ls_postab-bp_xn IS INITIAL OR ls_postab-bp_xl IS INITIAL OR
             ls_postab-bp_xk IS INITIAL  OR ls_postab-bp_yd IS INITIAL  OR ls_postab-bp_xj IS INITIAL.

            gv_errors = gv_errors + 1.

            CLEAR gs_cuselog.
            MOVE-CORRESPONDING <fs_cusposm> TO gs_cuselog.
            gs_cuselog-log_type = 'SO'.
            gs_cuselog-status = 'E'.
            gs_cuselog-message = 'Position Hierarchy Error'.
            APPEND gs_cuselog TO gt_cuselog.
            CONTINUE.
          ENDIF.
        ELSE.
          gv_errors = gv_errors + 1.
          CLEAR gs_cuselog.
          MOVE-CORRESPONDING <fs_cusposm> TO gs_cuselog.
          gs_cuselog-log_type = 'SO'.
          gs_cuselog-status = 'E'.
          gs_cuselog-message = 'Position Hierarchy Missing'.
          APPEND gs_cuselog TO gt_cuselog.
          CONTINUE.
        ENDIF.

*check and update
        PERFORM f_check_process USING ltt_knvp <fs_cusposm> ls_postab lt_tvarvc.
      ELSE.
*error handling
        gv_errors = gv_errors + 1.
        CLEAR gs_cuselog.
        MOVE-CORRESPONDING <fs_cusposm> TO gs_cuselog.
        gs_cuselog-log_type = 'SO'.
        gs_cuselog-status = 'E'.
        gs_cuselog-message = 'Customer not Extented to Division 10'.
        APPEND gs_cuselog TO gt_cuselog.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    WRITE: / 'Total Data Processed ', gv_total, Sy-datum, sy-uzeit.
    WRITE: / 'Total Success Processed ', gv_success, Sy-datum, sy-uzeit.
    WRITE: / 'Total Error Processed ', gv_errors, Sy-datum, sy-uzeit.
    WRITE: / 'Total Unchanged Processed ', gv_ignore, Sy-datum, sy-uzeit.

    IF gt_cuselog[] IS NOT INITIAL.
      IF P_test IS INITIAL.
        DESCRIBE TABLE gt_cuselog LINES DATA(lv_lines). "Count of Internal Table
        WRITE: / 'Count of Data in error Log ', lv_lines, Sy-datum, sy-uzeit.
        MODIFY zsd_cpf_pos_elog FROM TABLE gt_cuselog.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

    ENDIF.

  ELSE.
    WRITE: / 'No Record in KNVP'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LTT_KNVP
*&      --> <FS_CUSPOSM>_KUNNR
*&      --> <FS_CUSPOSM>_POSITION_CODE
*&      --> LS_POSTAB
*&---------------------------------------------------------------------*
FORM f_check_process  USING    im_knvp_tab TYPE gtt_knvp
                               im_CUSPOSM  TYPE ts_cusposm
                               ls_postab TYPE zhr_so_to_top_ts
                               im_TVARVC TYPE tvarvc_t.




  DATA: lt_knvp_tab TYPE gtt_knvp,
        lt_knvp_com TYPE gtt_knvp,
        lt_knvp_upd TYPE gtt_knvp.

  DATA: lsv_knvp TYPE knvp,
        ls_knvp  TYPE knvp.

  DATA: lt_yknvp_u TYPE TABLE OF fknvp,
        lt_Xknvp_u TYPE TABLE OF fknvp,
        lt_yknvp_I TYPE TABLE OF fknvp,
        lt_Xknvp_I TYPE TABLE OF fknvp,
        ls_xknvp   TYPE fknvp,
        ls_yknvp   TYPE fknvp.

  DATA: lv_OBJECTID TYPE cdhdr-objectid.
  DATA: ls_tvarvc TYPE tvarvc.
*  DATA(lv_kunnr) = |{ IM_kunnr ALPHA = IN }|.

  lt_knvp_tab[] = im_knvp_tab[].
  SORT lt_knvp_tab BY kunnr vkorg vtweg spart.
  DELETE ADJACENT DUPLICATES FROM lt_knvp_tab COMPARING kunnr vkorg vtweg spart.

*Prepare the new Table based on position hierarchy
  LOOP AT lt_knvp_tab INTO ls_knvp.

    CLEAR lsv_knvp.
    lsv_knvp-mandt = sy-mandt.
    lsv_knvp-kunnr = ls_knvp-kunnr.
    lsv_knvp-vkorg = ls_knvp-vkorg.
    lsv_knvp-vtweg = ls_knvp-vtweg.
    lsv_knvp-spart = ls_knvp-spart.

*position - lifnr partner function
    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-lifnr = ls_postab-bp_xn.
    lsv_knvp-parvw = 'XN'.
    APPEND lsv_knvp TO lt_knvp_com.

    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-lifnr = ls_postab-bp_xl.
    lsv_knvp-parvw = 'XL'.
    APPEND lsv_knvp TO lt_knvp_com.

    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-lifnr = ls_postab-bp_xk.
    lsv_knvp-parvw = 'XK'.
    APPEND lsv_knvp TO lt_knvp_com.

    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-lifnr = ls_postab-bp_yd.
    lsv_knvp-parvw = 'YD'.
    APPEND lsv_knvp TO lt_knvp_com.

    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-lifnr = ls_postab-bp_xj.
    lsv_knvp-parvw = 'XJ'.
    APPEND lsv_knvp TO lt_knvp_com.

* This partner function is only applicable for some orgunits like Gold and Silver
    CLEAR ls_tvarvc.
    READ TABLE im_TVARVC INTO ls_tvarvc WITH KEY low = ls_postab-orgid.
    IF sy-subrc = 0.
      CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
      lsv_knvp-lifnr = ls_postab-bp_xS.
      lsv_knvp-parvw = 'XS'.
      APPEND lsv_knvp TO lt_knvp_com.
    ENDIF.

*pernr partner funtion
    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-pernr = ls_postab-pernr_l5.
    lsv_knvp-parvw = 'L5'.
    APPEND lsv_knvp TO lt_knvp_com.

    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-pernr = ls_postab-pernr_l3.
    lsv_knvp-parvw = 'L3'.
    APPEND lsv_knvp TO lt_knvp_com.

    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-pernr = ls_postab-pernr_l2.
    lsv_knvp-parvw = 'L2'.
    APPEND lsv_knvp TO lt_knvp_com.

    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-pernr = ls_postab-pernr_zs.
    lsv_knvp-parvw = 'ZS'.
    APPEND lsv_knvp TO lt_knvp_com.

    CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
    lsv_knvp-pernr = ls_postab-pernr_l1.
    lsv_knvp-parvw = 'L1'.
    APPEND lsv_knvp TO lt_knvp_com.

* This partner function is only applicable for some orgunits like Gold and Silver
    CLEAR ls_tvarvc.
    READ TABLE im_TVARVC INTO ls_tvarvc WITH KEY low = ls_postab-orgid.
    IF sy-subrc = 0.
      CLEAR: lsv_knvp-lifnr, lsv_knvp-pernr, lsv_knvp-parvw.
      lsv_knvp-pernr = ls_postab-pernr_pw.
      lsv_knvp-parvw = 'PW'.
      APPEND lsv_knvp TO lt_knvp_com.
    ENDIF.

  ENDLOOP.



*compare with the original KNVP Table
  LOOP AT lt_knvp_com INTO ls_knvp.

    CLEAR lsv_knvp.
    READ TABLE im_knvp_tab INTO lsv_knvp WITH KEY kunnr = ls_knvp-kunnr vkorg = ls_knvp-vkorg
                                                  vtweg = ls_knvp-vtweg spart = ls_knvp-spart
                                                  parvw = ls_knvp-parvw.
    IF sy-subrc = 0.

      CASE ls_knvp-parvw.
        WHEN 'XN' OR 'XL' OR 'XK' OR 'YD' OR 'XJ' OR 'XS'.
          IF lsv_knvp-lifnr = ls_knvp-lifnr.
            CONTINUE.
          ENDIF.
        WHEN 'L5' OR 'L3' OR 'L2' OR 'ZS' OR 'L1' OR 'PW'.
          IF lsv_knvp-pernr = ls_knvp-pernr.
            CONTINUE.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      CLEAR: ls_yknvp.
      MOVE-CORRESPONDING lsv_knvp TO ls_yknvp.
      ls_yknvp-mandt = sy-mandt.
      ls_yknvp-kz = 'U'.
      APPEND ls_yknvp TO lt_yknvp_u.

*apeend to the update table
      APPEND ls_knvp TO lt_knvp_upd.

*fill the after structure
      CLEAR: ls_xknvp.
      MOVE-CORRESPONDING ls_knvp TO ls_xknvp.
      ls_xknvp-mandt = sy-mandt.
      ls_xknvp-kz = 'U'.
      APPEND ls_Xknvp TO lt_Xknvp_u.

    ELSE.
* THis is a case of new insert for New partner record inserting
      CLEAR lsv_knvp.
      lsv_knvp-mandt = sy-mandt.
      lsv_knvp-kunnr = ls_knvp-kunnr.
      lsv_knvp-vkorg = ls_knvp-vkorg.
      lsv_knvp-vtweg = ls_knvp-vtweg.
      lsv_knvp-spart = ls_knvp-spart.
      lsv_knvp-parvw = ls_knvp-parvw.

* fill the before structure
      CLEAR: ls_yknvp.
      MOVE-CORRESPONDING lsv_knvp TO ls_yknvp.
      ls_yknvp-mandt = sy-mandt.
      ls_yknvp-kz = 'I'.
      APPEND ls_yknvp TO lt_yknvp_I.

      CASE ls_knvp-parvw.
        WHEN 'XN' OR 'XL' OR 'XK' OR 'YD' OR 'XJ' OR 'XS'.
          lsv_knvp-lifnr = ls_knvp-lifnr.
        WHEN 'L5' OR 'L3' OR 'L2' OR 'ZS' OR 'L1' OR 'PW'.
          lsv_knvp-pernr = ls_knvp-pernr.
        WHEN OTHERS.
      ENDCASE.

*apeend to the update table
      APPEND lsv_knvp TO lt_knvp_upd.

*fill the after structure
      CLEAR: ls_xknvp.
      MOVE-CORRESPONDING ls_knvp TO ls_xknvp.
      ls_xknvp-mandt = sy-mandt.
      ls_xknvp-kz = 'I'.
      APPEND ls_Xknvp TO lt_Xknvp_I.

    ENDIF.

  ENDLOOP.

  IF lt_knvp_upd[] IS INITIAL.
*No Partner Function Data to Update all the compariosn are same do no
*update or message is required
    gv_ignore = gv_ignore + 1.
  ELSE.
    IF P_test = abap_true.
      APPEND LINES OF lt_knvp_upd TO GT_knvp_tst.
      CLEAR gs_cuselog.
      MOVE-CORRESPONDING im_cusposm TO gs_cuselog.
      gs_cuselog-log_type = 'SO'.
      gs_cuselog-status = 'S'.
      gs_cuselog-message = 'To Update due to Difference'.
      APPEND gs_cuselog TO gt_cuselog.
    ELSE.
      MODIFY knvp FROM TABLE lt_knvp_upd.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.

        CLEAR gs_cuselog.
        MOVE-CORRESPONDING im_cusposm TO gs_cuselog.
        gs_cuselog-log_type = 'SO'.
        gs_cuselog-status = 'S'.
        gs_cuselog-message = 'Updated Successfully'.
        APPEND gs_cuselog TO gt_cuselog.

        gv_success = gv_success + 1.

        IF lt_Xknvp_I[] IS NOT INITIAL.
* Call the FM to write entry to CDHDR and CDPOS for the respective Insert
          CLEAR lv_objectid.
          lv_objectid = lv_kunnr.

          CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
            EXPORTING
              objectid = lv_objectid
              tcode    = 'XD02'
              utime    = sy-uzeit
              udate    = sy-datum
              username = sy-uname
              upd_knvp = 'I'
            TABLES
              xknvp    = lt_xknvp_i
              yknvp    = lt_yknvp_i.
        ENDIF.

        IF lt_Xknvp_u[] IS NOT INITIAL.
* Call the FM to write entry to CDHDR and CDPOS for the respective Insert
          CLEAR lv_objectid.
          lv_objectid = lv_kunnr.

          CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
            EXPORTING
              objectid = lv_objectid
              tcode    = 'XD02'
              utime    = sy-uzeit
              udate    = sy-datum
              username = sy-uname
              upd_knvp = 'U'
            TABLES
              xknvp    = lt_xknvp_u
              yknvp    = lt_yknvp_u.
        ENDIF.

        COMMIT WORK AND WAIT.

      ELSE.

        gv_errors = gv_errors + 1.
        CLEAR gs_cuselog.
        MOVE-CORRESPONDING im_CUSPOSM TO gs_cuselog.
        gs_cuselog-log_type = 'SO'.
        gs_cuselog-status = 'E'.
        gs_cuselog-message = 'Error during update to KNVP'.
        APPEND gs_cuselog TO gt_cuselog.

      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fill_parvw
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_fill_parvw .

  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name = 'ZSOTOPPOSPARVW'.
  IF sy-subrc = 0.
    LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).
      CLEAR gs_parvw.
      gs_parvw-sign = 'I'.
      gs_parvw-option = 'EQ'.
      gs_parvw-low = ls_tvarvc-low.
      APPEND gs_parvw TO gr_parvw.
    ENDLOOP.
  ENDIF.

* select all the positio description
  SELECT *
    FROM hrp1000
    INTO TABLE gt_hrp1000
   WHERE plvar = '01'
     AND otype = 'S'
     AND objid BETWEEN '20000000' AND '29999999'
     AND begda <= sy-datum
     AND endda >= sy-datum.
  IF sy-subrc = 0.
    SORT gt_hrp1000 BY objid.
  ENDIF.

* customer number and name
  SELECT kunnr name1 werks
    FROM kna1
    INTO TABLE gt_custdet.
  IF sy-subrc = 0.
    SORT gt_custdet BY kunnr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DELETE_RECORDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_delete_records .

  DATA: lv_date TYPE sy-datum.

  lv_date = sy-datum - 15.

  WRITE: / 'Preparing to delete data from table zsd_custpf_posm before ', lv_date, Sy-datum, sy-uzeit.

  SELECT customercode FROM zsd_custpf_posm INTO TABLE @DATA(lt_custmp) WHERE entdate < @lv_date.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_custmp LINES DATA(lv_lines). "Count of Internal Table
    WRITE: / 'Count of Data to be deleted', lv_lines, Sy-datum, sy-uzeit.

    DELETE FROM zsd_custpf_posm WHERE entdate < lv_date.
    IF sy-subrc = 0.
      WRITE: / 'Count of Data Deleted ', sy-dbcnt, Sy-datum, sy-uzeit.
    ENDIF.
    WRITE:/.

    WRITE: / 'Preparing to delete data from table ZSD_CPF_POS_ELOG before ', lv_date, Sy-datum, sy-uzeit.
    DELETE FROM zsd_cpf_pos_elog WHERE entdate < lv_date.
    IF sy-subrc = 0.
      WRITE: / 'Count of Data Deleted ', sy-dbcnt, Sy-datum, sy-uzeit.
    ENDIF.

  ELSE.
    WRITE: / 'No Data to Delete ', Sy-datum, sy-uzeit.
  ENDIF.
  WRITE: / 'No Data to Delete ', Sy-datum, sy-uzeit.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_delete_date
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_delete_date .


  WRITE: / 'Preparing to delete data from table zsd_custpf_posm before ', p_date, Sy-datum, sy-uzeit.

  SELECT customercode FROM zsd_custpf_posm INTO TABLE @DATA(lt_custmp) WHERE entdate = @p_date.
  IF sy-subrc = 0.

    DESCRIBE TABLE lt_custmp LINES DATA(lv_lines). "Count of Internal Table
    WRITE: / 'Count of Data to be deleted by date ', lv_lines, Sy-datum, sy-uzeit.

    DELETE FROM zsd_custpf_posm WHERE entdate = p_date.
    IF sy-subrc = 0.
      WRITE: / 'Count of Data Deleted by date ', sy-dbcnt, Sy-datum, sy-uzeit.
    ENDIF.
    WRITE:/.

    WRITE: / 'Preparing to delete data from table ZSD_CPF_POS_ELOG before ', p_date, Sy-datum, sy-uzeit.
    DELETE FROM zsd_cpf_pos_elog WHERE entdate = p_date.
    IF sy-subrc = 0.
      WRITE: / 'Count of Data Deleted ', sy-dbcnt, Sy-datum, sy-uzeit.
    ENDIF.
  ELSE.
    WRITE: / 'No Data to Delete by date ', Sy-datum, sy-uzeit.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_SKSDMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_update_SKSDMS .

  DATA: lt_cusposm_6000 TYPE TABLE OF zsd_custpf_posm,
        ls_cusposm_6000 TYPE zsd_custpf_posm,
        lt_cusposm_SDMS TYPE TABLE OF zsd_custpf_posm,
        ls_cusposm_SDMS TYPE zsd_custpf_posm.

  DATA: lv_lines TYPE sy-tfill.

  DATA : lo_check TYPE REF TO zcl_common_check.
  CREATE OBJECT lo_check.

  DATA : lt_cusblk_6000 TYPE zsd_tt_cust_block,
         lt_cusblk_1000 TYPE zsd_tt_cust_block,
         lt_cusblk_SDMS TYPE zsd_tt_cust_block.

  DATA : ls_cusblk_6000 TYPE zsd_st_cust_block,
         ls_cusblk_1000 TYPE zsd_st_cust_block,
         ls_cusblk_SDMS TYPE zsd_st_cust_block.

  DATA : lt_knvp_6000 TYPE TABLE OF knvp,
         lt_knvp_SDMS TYPE TABLE OF knvp.

  DATA : ls_knvp_6000 TYPE knvp,
         ls_knvp_SDMS TYPE knvp.

  CLEAR: gv_total, gv_errors, gv_success, gv_ignore, gv_addtnl.

  REFRESH: lt_cusposm_6000, lt_cusposm_SDMS .
*select all entries from zsd_custpf_posm based on todays date for 6000 org.
  SELECT * FROM zsd_custpf_posm INTO TABLE lt_cusposm_6000
    WHERE org = '6000' AND entdate = sy-datum AND customercode IN s_kunnr.
  IF sy-subrc = 0.
    SORT lt_cusposm_6000 BY customercode.
    REFRESH lt_knvp_6000.
    SELECT * FROM knvp
      INTO TABLE lt_knvp_6000
      FOR ALL ENTRIES IN lt_cusposm_6000
      WHERE kunnr = lt_cusposm_6000-customercode
        AND vkorg = '6000' AND vtweg = '20' AND spart = '10' AND parvw = 'SK'.
    IF sy-subrc = 0.
      SORT lt_knvp_6000 BY kunnr.
      CLEAR lv_lines.
      DESCRIBE TABLE lt_knvp_6000 LINES lv_lines.
      WRITE: / 'Count of Data Sales Org 6000', lv_lines, Sy-datum, sy-uzeit.
    ENDIF.
  ENDIF.

*select all entries from zsd_custpf_posm based on todays date for 6000 org.
  SELECT * FROM zsd_custpf_posm INTO TABLE lt_cusposm_SDMS
    WHERE org = 'SDMS' AND entdate = sy-datum AND customercode IN s_kunnr.
  IF sy-subrc = 0.
    SORT lt_cusposm_SDMS BY customercode.
    REFRESH lt_knvp_sdms.
    SELECT * FROM knvp
      INTO TABLE lt_knvp_sdms
      FOR ALL ENTRIES IN lt_cusposm_sdms
      WHERE kunnr = lt_cusposm_sdms-customercode
        AND vkorg = 'SDMS' AND vtweg = '20' AND spart = '10' AND parvw = 'SK'.
    IF sy-subrc = 0.
      SORT lt_knvp_SDMS BY kunnr.
      CLEAR lv_lines.
      DESCRIBE TABLE lt_knvp_sdms LINES lv_lines.
      WRITE: / 'Count of Data Sales Org SDMS', lv_lines, Sy-datum, sy-uzeit.
    ENDIF.
  ENDIF.

*******customer block check for 6000********
  SELECT SINGLE bukrs FROM tvko INTO @DATA(ls_bukrs) WHERE vkorg = '6000'.
  lo_check->customer_block_check(
    EXPORTING
      bukrs                     = ls_bukrs      " Company Code
      vkorg                     = '6000'         " Sales Organization
    CHANGING
      cust_tab                  = lt_cusblk_6000   ).     " Table Type for Customer Block Check

  SORT : lt_cusblk_6000 BY kunnr.

******customer block check for SDMS********
  CLEAR ls_bukrs.
  SELECT SINGLE bukrs FROM tvko INTO ls_bukrs WHERE vkorg = 'SDMS'.
  lo_check->customer_block_check(
    EXPORTING
      bukrs                     = ls_bukrs      " Company Code
      vkorg                     = 'SDMS'         " Sales Organization
    CHANGING
      cust_tab                  = lt_cusblk_SDMS   ).     " Table Type for Customer Block Check

  SORT : lt_cusblk_sdms BY kunnr.

******customer block check for Sheenlac for SK PF (Attached Distributor)********
  CLEAR ls_bukrs.
  SELECT SINGLE bukrs FROM tvko INTO ls_bukrs WHERE vkorg = '1000'.
  lo_check->customer_block_check(
    EXPORTING
      bukrs                     = ls_bukrs      " Company Code
      vkorg                     = '1000'         " Sales Organization
    CHANGING
      cust_tab                  = lt_cusblk_1000   ).     " Table Type for Customer Block Check

  SORT : lt_cusblk_1000 BY kunnr.

  LOOP AT lt_cusposm_6000 INTO ls_cusposm_6000.
*  check if the 6000 customer is not Blocked
    CLEAR ls_cusblk_6000.
    READ TABLE lt_cusblk_6000 INTO ls_cusblk_6000 WITH KEY kunnr = ls_cusposm_6000-customercode block = 'X' BINARY SEARCH.
    IF sy-subrc = 0.
      PERFORM F_update_elog USING ls_cusposm_6000 'SK' 'E' TEXT-e01 space.
      gv_errors = gv_errors + 1.
      CONTINUE.
    ENDIF.

* Check if customer PF data present in KNVP - SK
    CLEAR ls_knvp_6000.
    READ TABLE lt_knvp_6000 INTO ls_knvp_6000 WITH KEY kunnr = ls_cusposm_6000-customercode BINARY SEARCH.
    IF sy-subrc = 0 AND ls_knvp_6000-kunn2 IS NOT INITIAL.
      CLEAR ls_cusblk_1000.
*check if SK Distributor customer code in 1000 is blocked
      READ TABLE lt_cusblk_1000 INTO ls_cusblk_1000 WITH KEY kunnr = ls_knvp_6000-kunn2 block = 'X' BINARY SEARCH.
      IF sy-subrc = 0.
        PERFORM F_update_elog USING ls_cusposm_6000 'SK' 'E' TEXT-e02 ls_knvp_6000-kunn2.
        gv_errors = gv_errors + 1.
        CONTINUE.
      ENDIF.
    ELSE.
*     customer not extended to 10 division or SK partner function not present
      PERFORM F_update_elog USING ls_cusposm_6000 'SK' 'E' TEXT-e04 space.
      gv_errors = gv_errors + 1.
      CONTINUE.
    ENDIF.


*check if customer is extended to SDMS
    CLEAR ls_knvp_sdms.
    READ TABLE lt_cusposm_SDMS INTO ls_cusposm_sdms WITH KEY customercode = ls_cusposm_6000-customercode BINARY SEARCH.
    IF sy-subrc NE 0.
*       customer not extended to SDMS
      PERFORM F_update_elog USING ls_cusposm_6000 'SK' 'E' TEXT-e03 space.
      gv_errors = gv_errors + 1.
      CONTINUE.
    ENDIF.

*check if the SDMS code is also blocked
    CLEAR ls_cusblk_sdms.
    READ TABLE lt_cusblk_sdms INTO ls_cusblk_sdms WITH KEY kunnr = ls_cusposm_sdms-customercode block = 'X' BINARY SEARCH.
    IF sy-subrc = 0.
      PERFORM F_update_elog USING ls_cusposm_sdms 'SK' 'E' TEXT-e05 space.
      gv_errors = gv_errors + 1.
      CONTINUE.
    ENDIF.

* check for the SDMS SK PF Value to compare with 6000 SK PF value
    CLEAR ls_knvp_sdms.
    READ TABLE lt_knvp_sdms INTO ls_knvp_sdms WITH KEY kunnr = ls_cusposm_sdms-customercode BINARY SEARCH.
    IF sy-subrc = 0.
      IF ls_knvp_sdms-kunn2 = ls_knvp_6000-kunn2.
*            Since Distributor value is same we can ignore
        gv_ignore = gv_ignore + 1.
      ELSE.
        IF P_test = abap_true. "if test mode only
          PERFORM F_update_elog USING ls_cusposm_SDMS 'SK' 'S' TEXT-s01 ls_knvp_6000-kunn2.
          gv_success = gv_success + 1.
        ELSE.
* For Change doc Updation fill before structure
          PERFORM F_Update_SK_KNVP USING ls_knvp_sdms ls_knvp_6000-kunn2 ls_cusposm_sdms.
          gv_success = gv_success + 1.
        ENDIF.

      ENDIF.

    ELSE.
*insert scenario
      IF P_test = abap_true.
        PERFORM F_update_elog USING ls_cusposm_SDMS 'SK' 'S' TEXT-s03 ls_knvp_6000-kunn2.
        gv_success = gv_success + 1.
      ELSE.
        PERFORM f_insert_sk_knvp USING ls_knvp_6000 ls_knvp_6000-kunn2 ls_cusposm_sdms.
        gv_success = gv_success + 1.
      ENDIF.

    ENDIF.
  ENDLOOP.


  IF gt_cuselog[] IS NOT INITIAL.
    IF P_test IS INITIAL.
      CLEAR lv_lines.
      DESCRIBE TABLE gt_cuselog LINES lv_lines. "Count of Internal Table
      WRITE: / 'Count of Data in Log ', lv_lines, Sy-datum, sy-uzeit.
      MODIFY zsd_cpf_pos_elog FROM TABLE gt_cuselog.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_update_elog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_CUSPOSM_6000
*&      --> P_
*&      --> P_
*&      --> SPACE
*&      --> SPACE
*&---------------------------------------------------------------------*
FORM F_update_elog  USING p_ls_cusposm_6000 TYPE zsd_custpf_posm
                          p_logtype TYPE zlogtype
                          p_mtype   TYPE bapi_mtype
                          p_msg     TYPE zmessage
                          p_kunnr   TYPE kunnr.


  CLEAR gs_cuselog.
  MOVE-CORRESPONDING p_ls_cusposm_6000 TO gs_cuselog.
  gs_cuselog-log_type = p_logtype.
  gs_cuselog-status   = p_mtype.
  gs_cuselog-message  = p_msg.

  IF p_kunnr IS NOT INITIAL.
    gs_cuselog-kunnr_sk = p_kunnr.
    READ TABLE gt_custdet INTO gs_custdet WITH KEY kunnr = gs_cuselog-kunnr_sk BINARY SEARCH.
    IF sy-subrc = 0.
      gs_cuselog-kunnr_sk_name = gs_custdet-name1.
    ENDIF.
  ENDIF.

  APPEND gs_cuselog TO gt_cuselog.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_Update_SK_KNVP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_KNVP_SDMS
*&      --> P_
*&      --> LS_KNVP_6000_KUNNR
*&      --> REFRESH
*&      --> LT_KNVP_UPD
*&---------------------------------------------------------------------*
FORM F_Update_SK_KNVP  USING    ls_knvp_sdms TYPE knvp
                                ls_knvp_6000_kunnr TYPE kunnr
                                ls_cusposm_SDMS TYPE zsd_custpf_posm.

  DATA: lsv_knvp TYPE knvp.

  DATA: lt_knvp_upd TYPE TABLE OF knvp.

  DATA: lt_yknvp_u TYPE TABLE OF fknvp,
        lt_Xknvp_u TYPE TABLE OF fknvp,
        lt_yknvp_I TYPE TABLE OF fknvp,
        lt_Xknvp_I TYPE TABLE OF fknvp,
        ls_xknvp   TYPE fknvp,
        ls_yknvp   TYPE fknvp.

  DATA: lv_OBJECTID TYPE cdhdr-objectid.


  REFRESH: lt_knvp_upd, lt_yknvp_u, lt_Xknvp_u.
  CLEAR: ls_yknvp.
  MOVE-CORRESPONDING ls_knvp_sdms TO ls_yknvp.
  ls_yknvp-mandt = sy-mandt.
  ls_yknvp-kz = 'U'.
  APPEND ls_yknvp TO lt_yknvp_u.

*apeend to the update table
  ls_knvp_sdms-kunn2 = ls_knvp_6000_kunnr .
  APPEND ls_knvp_sdms TO lt_knvp_upd.

*fill the after structure
  CLEAR: ls_xknvp.
  MOVE-CORRESPONDING ls_knvp_sdms TO ls_xknvp.
  ls_xknvp-mandt = sy-mandt.
  ls_xknvp-kz = 'U'.
  APPEND ls_Xknvp TO lt_Xknvp_u.

*update the table
  MODIFY knvp FROM TABLE lt_knvp_upd.
  IF sy-subrc = 0 .
    COMMIT WORK AND WAIT.
    PERFORM F_update_elog USING ls_cusposm_SDMS 'SK' 'S' TEXT-s02 ls_knvp_6000_kunnr.

    IF lt_Xknvp_u[] IS NOT INITIAL.
* Call the FM to write entry to CDHDR and CDPOS for the respective Insert
      CLEAR lv_objectid.
      lv_objectid = ls_knvp_sdms-kunnr.

      CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
        EXPORTING
          objectid = lv_objectid
          tcode    = 'XD02'
          utime    = sy-uzeit
          udate    = sy-datum
          username = sy-uname
          upd_knvp = 'U'
        TABLES
          xknvp    = lt_xknvp_u
          yknvp    = lt_yknvp_u.
    ENDIF.

    COMMIT WORK AND WAIT.

  ELSE.
    PERFORM F_update_elog USING ls_cusposm_SDMS 'SK' 'E' TEXT-e06 ls_knvp_6000_kunnr.
    gv_errors = gv_errors + 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insert_sk_knvp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_KNVP_6000
*&      --> LS_KNVP_6000_KUNN2
*&      --> LS_CUSPOSM_SDMS
*&---------------------------------------------------------------------*
FORM f_insert_sk_knvp  USING    ls_knvp_sdms TYPE knvp
                                ls_knvp_6000_kunnr TYPE kunnr
                                ls_cusposm_SDMS TYPE zsd_custpf_posm.

  DATA: lsv_knvp TYPE knvp.

  DATA: lt_knvp_upd TYPE TABLE OF knvp.

  DATA: lt_yknvp_u TYPE TABLE OF fknvp,
        lt_Xknvp_u TYPE TABLE OF fknvp,
        lt_yknvp_I TYPE TABLE OF fknvp,
        lt_Xknvp_I TYPE TABLE OF fknvp,
        ls_xknvp   TYPE fknvp,
        ls_yknvp   TYPE fknvp.

  DATA: lv_OBJECTID TYPE cdhdr-objectid.

* THis is a case of new insert for New partner record inserting
  CLEAR lsv_knvp.
  lsv_knvp-mandt = sy-mandt.
  lsv_knvp-kunnr = ls_knvp_sdms-kunnr.
  lsv_knvp-vkorg = 'SDMS'.
  lsv_knvp-vtweg = ls_knvp_sdms-vtweg.
  lsv_knvp-spart = ls_knvp_sdms-spart.
  lsv_knvp-parvw = ls_knvp_sdms-parvw.

* fill the before structure
  CLEAR: ls_yknvp.
  MOVE-CORRESPONDING lsv_knvp TO ls_yknvp.
  ls_yknvp-mandt = sy-mandt.
  ls_yknvp-kz = 'I'.
  APPEND ls_yknvp TO lt_yknvp_I.

  lsv_knvp-kunn2 = ls_knvp_6000_kunnr.

*apeend to the update table
  APPEND lsv_knvp TO lt_knvp_upd.

*fill the after structure
  CLEAR: ls_xknvp.
  MOVE-CORRESPONDING lsv_knvp TO ls_xknvp.
  ls_xknvp-mandt = sy-mandt.
  ls_xknvp-kz = 'I'.
  APPEND ls_Xknvp TO lt_Xknvp_I.

  MODIFY knvp FROM TABLE lt_knvp_upd.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    PERFORM F_update_elog USING ls_cusposm_SDMS 'SK' 'S' TEXT-s04 ls_knvp_6000_kunnr.

    IF lt_Xknvp_I[] IS NOT INITIAL.
* Call the FM to write entry to CDHDR and CDPOS for the respective Insert
      CLEAR lv_objectid.
      lv_objectid = ls_knvp_sdms-kunnr.

      CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
        EXPORTING
          objectid = lv_objectid
          tcode    = 'XD02'
          utime    = sy-uzeit
          udate    = sy-datum
          username = sy-uname
          upd_knvp = 'I'
        TABLES
          xknvp    = lt_xknvp_i
          yknvp    = lt_yknvp_i.

      COMMIT WORK AND WAIT.

    ENDIF.
  ELSE.
    PERFORM F_update_elog USING ls_cusposm_SDMS 'SK' 'E' TEXT-e07 ls_knvp_6000_kunnr.
    gv_errors = gv_errors + 1.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_update_SKSDMS_New
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_update_SKSDMS_New .

  DATA: lt_cusposm_6000 TYPE TABLE OF zcust_blk_chk,
        ls_cusposm_6000 TYPE zcust_blk_chk,

        lt_cusposm_SDMS TYPE TABLE OF zcust_blk_chk,
        ls_cusposm_SDMS TYPE zcust_blk_chk,

        lt_cusposm_1000 TYPE TABLE OF zcust_blk_chk,
        ls_cusposm_1000 TYPE zcust_blk_chk.

  DATA : lt_knvp_6000 TYPE TABLE OF knvp,
         lt_knvp_SDMS TYPE TABLE OF knvp.

  DATA : ls_knvp_6000 TYPE knvp,
         ls_knvp_SDMS TYPE knvp.

  DATA: lv_lines TYPE sy-tfill.


  REFRESH: lt_cusposm_6000, lt_cusposm_SDMS .
*select all entries from zsd_custpf_posm based on todays date for 6000 org.
  SELECT * FROM zcust_blk_chk INTO TABLE lt_cusposm_6000
    WHERE bukrs = '6000' AND vkorg = '6000' AND kunnr IN s_kunnr AND block NE 'X'.
  IF sy-subrc = 0.
    SORT lt_cusposm_6000 BY kunnr. REFRESH lt_knvp_6000.
    SELECT * FROM knvp INTO TABLE lt_knvp_6000 FOR ALL ENTRIES IN lt_cusposm_6000
      WHERE kunnr = lt_cusposm_6000-kunnr AND vkorg = '6000' AND vtweg = '20' AND spart = '10' AND parvw = 'SK'.
    IF sy-subrc = 0.
      SORT lt_knvp_6000 BY kunnr.
      CLEAR lv_lines.
      DESCRIBE TABLE lt_knvp_6000 LINES lv_lines.
      WRITE: / 'Count of Data Sales Org 6000', lv_lines, Sy-datum, sy-uzeit.
    ENDIF.
  ENDIF.

*select all entries from zsd_custpf_posm based on todays date for SDMS org.
  SELECT * FROM zcust_blk_chk INTO TABLE lt_cusposm_sdms
    WHERE bukrs = 'DMS1' AND vkorg = 'SDMS' AND kunnr IN s_kunnr AND block NE 'X'.
  IF sy-subrc = 0.
    SORT lt_cusposm_SDMS BY kunnr. REFRESH lt_knvp_SDMS.
    SELECT * FROM knvp INTO TABLE lt_knvp_sdms FOR ALL ENTRIES IN lt_cusposm_SDMS
      WHERE kunnr = lt_cusposm_sdms-kunnr AND vkorg = 'SDMS' AND vtweg = '20' AND spart = '10' AND parvw = 'SK'.
    IF sy-subrc = 0.
      SORT lt_knvp_sdms BY kunnr.
      CLEAR lv_lines.
      DESCRIBE TABLE lt_knvp_sdms LINES lv_lines.
      WRITE: / 'Count of Data Sales Org SDMS', lv_lines, Sy-datum, sy-uzeit.
    ENDIF.
  ENDIF.

*start loop based on the 6000 org
  LOOP AT lt_cusposm_6000 INTO ls_cusposm_6000.

    IF ls_cusposm_6000-dist IS INITIAL.
*     customer not extended to 10 division or SK partner function not present
      PERFORM F_update_elog_new USING ls_cusposm_6000 'KS' 'E' TEXT-e04 space space.
      gv_errors = gv_errors + 1.
      CONTINUE.
    ENDIF.

    IF ls_cusposm_6000-dist_block = 'X'.
*check if SK Distributor customer code in 1000 is blocked
      PERFORM F_update_elog_new USING ls_cusposm_6000 'KS' 'E' TEXT-e02 ls_cusposm_6000-dist space.
      gv_errors = gv_errors + 1.
      CONTINUE.
    ENDIF.

*check if customer is extended to SDMS
    CLEAR ls_knvp_sdms.
    READ TABLE lt_cusposm_SDMS INTO ls_cusposm_sdms WITH KEY kunnr = ls_cusposm_6000-kunnr BINARY SEARCH.
    IF sy-subrc NE 0.
*       customer not extended to SDMS
      PERFORM F_update_elog_new USING ls_cusposm_6000 'KS' 'E' TEXT-e03 ls_cusposm_6000-dist space.
      gv_errors = gv_errors + 1.
      CONTINUE.
    ELSE.

    ENDIF.


*if both distributor values are same
    IF ls_cusposm_6000-dist = ls_cusposm_sdms-dist.
*   Since Distributor value is same we can ignore
      gv_ignore = gv_ignore + 1.
      CONTINUE.
    ENDIF.

* check for the SDMS SK PF Value to compare with 6000 SK PF value
    CLEAR ls_knvp_sdms.
    READ TABLE lt_knvp_sdms INTO ls_knvp_sdms WITH KEY kunnr = ls_cusposm_sdms-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      IF ls_knvp_sdms-kunn2 = ls_cusposm_6000-dist.
*            Since Distributor value is same we can ignore
*        gv_ignore = gv_ignore + 1.
      ELSE.
        IF P_test = abap_true. "if test mode only
          PERFORM F_update_elog_new USING ls_cusposm_SDMS 'KS' 'S' TEXT-s01 ls_cusposm_6000-dist ls_knvp_sdms-kunn2.
          gv_success = gv_success + 1.
        ELSE.
* For Change doc Updation fill before structure
          PERFORM F_Update_SK_KNVP_new USING ls_knvp_sdms ls_cusposm_6000-dist ls_cusposm_sdms.
          gv_success = gv_success + 1.
        ENDIF.

      ENDIF.

    ELSE.
*insert scenario
      IF P_test = abap_true.
        PERFORM F_update_elog_new USING ls_cusposm_SDMS 'KS' 'S' TEXT-s03 ls_cusposm_6000-dist ls_cusposm_6000-dist.
        gv_success = gv_success + 1.
      ELSE.
        CLEAR ls_knvp_6000.
        READ TABLE lt_knvp_6000 INTO ls_knvp_6000 WITH KEY kunnr = ls_cusposm_sdms-kunnr BINARY SEARCH.
        IF sy-subrc = 0.
          PERFORM f_insert_sk_knvp_new USING ls_knvp_6000 ls_cusposm_6000-dist ls_cusposm_sdms.
          gv_success = gv_success + 1.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.


  IF gt_cuselog[] IS NOT INITIAL.
    IF P_test IS INITIAL.
      CLEAR lv_lines.
      DESCRIBE TABLE gt_cuselog LINES lv_lines. "Count of Internal Table
      WRITE: / 'Count of Data in Log ', lv_lines, Sy-datum, sy-uzeit.
      MODIFY zsd_cpf_pos_elog FROM TABLE gt_cuselog.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_update_elog_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_CUSPOSM_6000
*&      --> P_
*&      --> P_
*&      --> SPACE
*&      --> SPACE
*&---------------------------------------------------------------------*
FORM F_update_elog_new  USING p_ls_cusposm_6000 TYPE zcust_blk_chk
                              p_logtype TYPE zlogtype
                              p_mtype   TYPE bapi_mtype
                              p_msg     TYPE zmessage
                              p_kunnr   TYPE kunnr
                              p_kunnr2  TYPE kunnr .


  CLEAR gs_cuselog.
  MOVE-CORRESPONDING p_ls_cusposm_6000 TO gs_cuselog.
  gs_cuselog-customercode = p_ls_cusposm_6000-kunnr.
  gs_cuselog-org          = p_ls_cusposm_6000-vkorg.
  gs_cuselog-entdate      = sy-datum.
  gs_cuselog-log_type     = p_logtype.
  gs_cuselog-status       = p_mtype.
  gs_cuselog-message      = p_msg.
  gs_cuselog-Channel      = p_ls_cusposm_6000-vtext.
  gs_cuselog-kunnr_sk     = p_kunnr.
*  gs_cuselog-kunnr_sk_name = p_ls_cusposm_6000-dist_name.

  IF p_kunnr IS NOT INITIAL.
    gs_cuselog-kunnr_sk = p_kunnr.
    READ TABLE gt_custdet INTO gs_custdet WITH KEY kunnr = gs_cuselog-kunnr_sk BINARY SEARCH.
    IF sy-subrc = 0.
      gs_cuselog-kunnr_sk_name = gs_custdet-name1.
    ENDIF.
  ENDIF.

  IF p_kunnr2 IS NOT INITIAL.
    gs_cuselog-old_kun_sk = p_kunnr2.
    READ TABLE gt_custdet INTO gs_custdet WITH KEY kunnr = gs_cuselog-old_kun_sk BINARY SEARCH.
    IF sy-subrc = 0.
      gs_cuselog-old_kun_sk_name = gs_custdet-name1.
    ENDIF.
  ENDIF.

  APPEND gs_cuselog TO gt_cuselog.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_Update_SK_KNVP_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_KNVP_SDMS
*&      --> P_
*&      --> LS_KNVP_6000_KUNNR
*&      --> REFRESH
*&      --> LT_KNVP_UPD
*&---------------------------------------------------------------------*
FORM F_Update_SK_KNVP_new  USING ls_knvp_sdms TYPE knvp
                                 ls_knvp_6000_kunnr TYPE kunnr
                                 ls_cusposm_SDMS TYPE zcust_blk_chk.

  DATA: lsv_knvp TYPE knvp.

  DATA: lt_knvp_upd TYPE TABLE OF knvp.

  DATA: lt_yknvp_u TYPE TABLE OF fknvp,
        lt_Xknvp_u TYPE TABLE OF fknvp,
        lt_yknvp_I TYPE TABLE OF fknvp,
        lt_Xknvp_I TYPE TABLE OF fknvp,
        ls_xknvp   TYPE fknvp,
        ls_yknvp   TYPE fknvp.

  DATA: lv_sdms_kunn2 TYPE kunnr.

  DATA: lv_OBJECTID TYPE cdhdr-objectid.


  REFRESH: lt_knvp_upd, lt_yknvp_u, lt_Xknvp_u.
  CLEAR: ls_yknvp.
  MOVE-CORRESPONDING ls_knvp_sdms TO ls_yknvp.
  ls_yknvp-mandt = sy-mandt.
  ls_yknvp-kz = 'U'.
  APPEND ls_yknvp TO lt_yknvp_u.

*apeend to the update table
  lv_sdms_kunn2 = ls_knvp_sdms-kunn2.
  ls_knvp_sdms-kunn2 = ls_knvp_6000_kunnr .
  APPEND ls_knvp_sdms TO lt_knvp_upd.

*fill the after structure
  CLEAR: ls_xknvp.
  MOVE-CORRESPONDING ls_knvp_sdms TO ls_xknvp.
  ls_xknvp-mandt = sy-mandt.
  ls_xknvp-kz = 'U'.
  APPEND ls_Xknvp TO lt_Xknvp_u.

*update the table
  MODIFY knvp FROM TABLE lt_knvp_upd.
  IF sy-subrc = 0 .
    COMMIT WORK AND WAIT.
    PERFORM F_update_elog_new USING ls_cusposm_SDMS 'KS' 'S' TEXT-s02 ls_knvp_6000_kunnr lv_sdms_kunn2.

    IF lt_Xknvp_u[] IS NOT INITIAL.
* Call the FM to write entry to CDHDR and CDPOS for the respective Insert
      CLEAR lv_objectid.
      lv_objectid = ls_knvp_sdms-kunnr.

      CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
        EXPORTING
          objectid = lv_objectid
          tcode    = 'XD02'
          utime    = sy-uzeit
          udate    = sy-datum
          username = sy-uname
          upd_knvp = 'U'
        TABLES
          xknvp    = lt_xknvp_u
          yknvp    = lt_yknvp_u.
    ENDIF.

    COMMIT WORK AND WAIT.

  ELSE.
    PERFORM F_update_elog_new USING ls_cusposm_SDMS 'KS' 'E' TEXT-e06 ls_knvp_6000_kunnr lv_sdms_kunn2.
    gv_errors = gv_errors + 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insert_sk_knvp_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_KNVP_6000
*&      --> LS_KNVP_6000_KUNN2
*&      --> LS_CUSPOSM_SDMS
*&---------------------------------------------------------------------*
FORM f_insert_sk_knvp_new  USING ls_knvp_sdms TYPE knvp
                                 ls_knvp_6000_kunnr TYPE kunnr
                                 ls_cusposm_SDMS TYPE zcust_blk_chk.

  DATA: lsv_knvp TYPE knvp.

  DATA: lt_knvp_upd TYPE TABLE OF knvp.

  DATA: lt_yknvp_u TYPE TABLE OF fknvp,
        lt_Xknvp_u TYPE TABLE OF fknvp,
        lt_yknvp_I TYPE TABLE OF fknvp,
        lt_Xknvp_I TYPE TABLE OF fknvp,
        ls_xknvp   TYPE fknvp,
        ls_yknvp   TYPE fknvp.

  DATA: lv_OBJECTID TYPE cdhdr-objectid.

* THis is a case of new insert for New partner record inserting
  CLEAR lsv_knvp.
  lsv_knvp-mandt = sy-mandt.
  lsv_knvp-kunnr = ls_knvp_sdms-kunnr.
  lsv_knvp-vkorg = 'SDMS'.
  lsv_knvp-vtweg = ls_knvp_sdms-vtweg.
  lsv_knvp-spart = ls_knvp_sdms-spart.
  lsv_knvp-parvw = ls_knvp_sdms-parvw.

* fill the before structure
  CLEAR: ls_yknvp.
  MOVE-CORRESPONDING lsv_knvp TO ls_yknvp.
  ls_yknvp-mandt = sy-mandt.
  ls_yknvp-kz = 'I'.
  APPEND ls_yknvp TO lt_yknvp_I.

  lsv_knvp-kunn2 = ls_knvp_6000_kunnr.

*apeend to the update table
  APPEND lsv_knvp TO lt_knvp_upd.

*fill the after structure
  CLEAR: ls_xknvp.
  MOVE-CORRESPONDING lsv_knvp TO ls_xknvp.
  ls_xknvp-mandt = sy-mandt.
  ls_xknvp-kz = 'I'.
  APPEND ls_Xknvp TO lt_Xknvp_I.

  MODIFY knvp FROM TABLE lt_knvp_upd.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    PERFORM F_update_elog_new USING ls_cusposm_SDMS 'KS' 'S' TEXT-s04 ls_knvp_6000_kunnr ls_knvp_6000_kunnr.

    IF lt_Xknvp_I[] IS NOT INITIAL.
* Call the FM to write entry to CDHDR and CDPOS for the respective Insert
      CLEAR lv_objectid.
      lv_objectid = ls_knvp_sdms-kunnr.

      CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
        EXPORTING
          objectid = lv_objectid
          tcode    = 'XD02'
          utime    = sy-uzeit
          udate    = sy-datum
          username = sy-uname
          upd_knvp = 'I'
        TABLES
          xknvp    = lt_xknvp_i
          yknvp    = lt_yknvp_i.

      COMMIT WORK AND WAIT.

    ENDIF.
  ELSE.
    PERFORM F_update_elog_new USING ls_cusposm_SDMS 'KS' 'E' TEXT-e07 ls_knvp_6000_kunnr ls_knvp_6000_kunnr.
    gv_errors = gv_errors + 1.
  ENDIF.
ENDFORM.
