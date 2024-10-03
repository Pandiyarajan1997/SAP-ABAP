*----------------------------------------------------------------------*
***INCLUDE LZFG_EWAYBILLF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EWBDTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IM_EWB  text
*----------------------------------------------------------------------*
FORM ewbdtls  USING     p_im_ewb TYPE zst_ewaybill.

  IF gv_expdtls IS NOT INITIAL.

    IF p_im_ewb-distance IS INITIAL.

      IF lv_sel_pin1 = lv_buy_pin.    "Export Seller GSTIN = Buyer GSTIN
        CONCATENATE '"' '25"' INTO lv_distance.
      ELSE.
        CONCATENATE '"' '0"'  INTO lv_distance.
      ENDIF.
    ELSE.
      MOVE p_im_ewb-distance TO lv_distance.      "Manual Distance Input
    ENDIF.

    DATA: lv_ownerid TYPE zownerid,
          ls_tvarvc  TYPE tvarvc.

    CLEAR ls_tvarvc.
    SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'ZTECH_EWAY_CDKEY' AND type = 'P'.
    IF sy-subrc = 0.
      lv_ownerid = ls_tvarvc-low.
    ELSE.
      lv_ownerid = '1552069'.
    ENDIF.


    SELECT SINGLE * FROM yeinv_url INTO wa_url WHERE gstin = gv_gstin AND Ownerid = lv_ownerid .
    "ENDIF.
    SELECT SINGLE * FROM yzeinv_url INTO wa_yzurl WHERE gstin = gv_gstin AND Ownerid = lv_ownerid .

    IF wa_url IS NOT INITIAL.
      gv_gstin    = gv_gstin.
      gv_ownerid  = wa_url-ownerid.
      "gv_auth     = wa_url-authtoken.
      gv_url      = wa_url-zeway_cr.
      "  GV_GETURL   = WA_URL-ZIRN_GETURL.
    ENDIF.

    IF wa_yzurl IS NOT INITIAL.
      gv_efusername = wa_yzurl-efusername .
      gv_efpassword = wa_yzurl-efpassword.
      gv_einvusername = wa_yzurl-einvusername.
      gv_einvpassword = wa_yzurl-einvpassword.
    ENDIF.



    IF syst-sysid = 'DEV' OR syst-sysid = 'QAS'.          "Development & Quality Sandbox
      CONCATENATE
        ' {"Push_Data_List": '
        '['     "Dev & QAS Sandbox JSON
        '{'
          '"Irn":'        p_im_ewb-irn c_comma
          '"Distance":'   lv_distance c_comma
          '"TransMode":'  p_im_ewb-transmode c_comma
          '"TransId":'    p_im_ewb-transid c_comma
          '"TransName":'  p_im_ewb-transname c_comma
          '"TransDocDt":' p_im_ewb-transdt c_comma
          '"TransDocNo":' p_im_ewb-transdoc c_comma
          '"VehNo":'      p_im_ewb-vehno c_comma
          '"VehType":'    p_im_ewb-vehtyp c_comma gv_expdtls
          ' "GSTIN" : 'c_quatation_mark gv_gstin c_quatation_mark c_comma
          ' "CDKey": 'c_quatation_mark gv_ownerid c_quatation_mark c_comma
       ' "EWBUSERNAME" : 'c_quatation_mark gv_einvusername c_quatation_mark c_comma
' "EWBPASSWORD": 'c_quatation_mark gv_einvpassword c_quatation_mark c_comma
' "EFUserName" : 'c_quatation_mark gv_efusername c_quatation_mark c_comma
' "EFPassword" : 'c_quatation_mark gv_efpassword c_quatation_mark

   '}'
          ']'
         ' } '"Dev & QAS Sandbox JSON

          INTO lv_ewbdtls.
    ELSE.
      CONCATENATE
        ' {"Push_Data_List": '
      '['     " Commented While Moving to PRD
     '{'
       '"Irn":'        p_im_ewb-irn c_comma
       '"Distance":'   lv_distance c_comma
       '"TransMode":'  p_im_ewb-transmode c_comma
       '"TransId":'    p_im_ewb-transid c_comma
       '"TransName":'  p_im_ewb-transname c_comma
       '"TransDocDt":' p_im_ewb-transdt c_comma
       '"TransDocNo":' p_im_ewb-transdoc c_comma
       '"VehNo":'      p_im_ewb-vehno c_comma
       '"VehType":'    p_im_ewb-vehtyp  c_comma gv_expdtls
       ' "GSTIN" : 'c_quatation_mark gv_gstin c_quatation_mark c_comma
       ' "CDKey": 'c_quatation_mark gv_ownerid c_quatation_mark c_comma
    ' "EWBUSERNAME" : 'c_quatation_mark gv_einvusername c_quatation_mark c_comma
' "EWBPASSWORD": 'c_quatation_mark gv_einvpassword c_quatation_mark c_comma
' "EFUserName" : 'c_quatation_mark gv_efusername c_quatation_mark c_comma
' "EFPassword" : 'c_quatation_mark gv_efpassword c_quatation_mark

'}'
        ']'    "Commented While Moving to PRD
        '}'
       INTO lv_ewbdtls.
    ENDIF.

  ELSE.

    IF p_im_ewb-distance IS INITIAL.

      IF lv_sel_pin1 = lv_buy_pin.    "Export Seller GSTIN = Buyer GSTIN
        CONCATENATE '"' '25"' INTO lv_distance.
      ELSE.
        CONCATENATE '"' '0"'  INTO lv_distance.
      ENDIF.

    ELSE.
      MOVE p_im_ewb-distance TO lv_distance.        "Manual Distance Input
    ENDIF.

    IF syst-sysid = 'DEV' OR syst-sysid = 'QAS'.    "Development & Quality Sandbox

      CONCATENATE
         ' {"Push_Data_List": '
      '['         " Dev & QAS Sandbox JSON
      '{'
        '"Irn":'        p_im_ewb-irn c_comma
        '"Distance":'   lv_distance c_comma
        '"TransMode":'  p_im_ewb-transmode c_comma
        '"TransId":'    p_im_ewb-transid c_comma
        '"TransName":'  p_im_ewb-transname c_comma
        '"TransDocDt":' p_im_ewb-transdt c_comma
        '"TransDocNo":' p_im_ewb-transdoc c_comma
        '"VehNo":'      p_im_ewb-vehno c_comma
        '"VehType":'    p_im_ewb-vehtyp c_comma gv_expdtls
        ' "GSTIN" : 'c_quatation_mark gv_gstin c_quatation_mark c_comma
        ' "CDKey": 'c_quatation_mark gv_ownerid c_quatation_mark c_comma
    ' "EWBUSERNAME" : 'c_quatation_mark gv_einvusername c_quatation_mark c_comma
' "EWBPASSWORD": 'c_quatation_mark gv_einvpassword c_quatation_mark c_comma
' "EFUserName" : 'c_quatation_mark gv_efusername c_quatation_mark c_comma
' "EFPassword" : 'c_quatation_mark gv_efpassword c_quatation_mark

 '}'
       " '}'
        ']'
        '}'    " Dev & QAS Sandbox JSON
        INTO lv_ewbdtls.
    ELSE.                                "PRD Credentials

      CONCATENATE
        ' {"Push_Data_List": '
        '['
   '{'
     '"Irn":'        p_im_ewb-irn c_comma
     '"Distance":'   lv_distance c_comma
     '"TransMode":'  p_im_ewb-transmode c_comma
     '"TransId":'    p_im_ewb-transid c_comma
     '"TransName":'  p_im_ewb-transname c_comma
     '"TransDocDt":' p_im_ewb-transdt c_comma
     '"TransDocNo":' p_im_ewb-transdoc c_comma
     '"VehNo":'      p_im_ewb-vehno c_comma
     '"VehType":'    p_im_ewb-vehtyp c_comma gv_expdtls
      ' "GSTIN" : 'c_quatation_mark gv_gstin c_quatation_mark c_comma
     ' "CDKey": 'c_quatation_mark gv_ownerid c_quatation_mark c_comma
    ' "EWBUSERNAME" : 'c_quatation_mark gv_einvusername c_quatation_mark c_comma
' "EWBPASSWORD": 'c_quatation_mark gv_einvpassword c_quatation_mark c_comma
' "EFUserName" : 'c_quatation_mark gv_efusername c_quatation_mark c_comma
' "EFPassword" : 'c_quatation_mark gv_efpassword c_quatation_mark

'}'
 ']'
    '}'
     INTO lv_ewbdtls.

    ENDIF.
  ENDIF.

ENDFORM.                    " EWBDTLS

*&---------------------------------------------------------------------*
*&      Form  API_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_EWBDTLS  text
*      -->P_IM_EWB  text
*      <--P_EWAY_OUT  text
*      <--P_RESPONSE  text
*      <--P_GV_MSG  text
*      <--P_EX_ERROR  text
*----------------------------------------------------------------------*
FORM api_call  USING    p_v_payload TYPE string
                        p_im_ewb TYPE zst_ewaybill
               CHANGING p_eway_out TYPE j_1ig_ewaybill
                        p_response TYPE string
                        p_msg TYPE c
                        p_ex_error TYPE string.
  DATA: lc_msg TYPE REF TO cx_salv_msg.

  CLEAR: gv_url,gv_ownerid,gv_auth,lv_branch,lv_gstin,lv_werks.

*******Owner id's against GSTIN *******************

  SELECT werks UP TO 1 ROWS FROM vbrp INTO lv_werks WHERE vbeln = p_im_ewb-vbeln ORDER BY PRIMARY KEY.
  ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

  "Get the Seller Gstin--->>
  IF lv_werks IS NOT INITIAL.
    SELECT SINGLE j_1bbranch FROM t001w
*           INTO @DATA(lv_branch)
           INTO lv_branch
*           WHERE werks EQ @lv_werks.
           WHERE werks EQ lv_werks.
  ENDIF.

  IF lv_branch IS NOT INITIAL.
    SELECT  gstin    "Seller GSTIN
      UP TO 1 ROWS FROM j_1bbranch
*      INTO @DATA(lv_gstin)
      INTO lv_gstin
      WHERE branch = lv_branch ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
  ENDIF.

*  lv_gstin  = gv_gstin.      "For Sandbox GSTIN 29   Comment while moving PRD
  gv_gstin = lv_gstin.      "For LIve PRD GSTIN    Uncomment while moving PRD

*  IF GV_OWNERID IS INITIAL.        "GSTIN
*    SELECT SINGLE * FROM YEINV_URL INTO WA_URL WHERE GSTIN = '29AAACW3775F000'.
*  ENDIF.

  IF wa_url IS NOT INITIAL.
    gv_ownerid  = wa_url-ownerid.         "Owner id
    gv_auth     = wa_url-authtoken.       "Auth Token
    gv_url      = wa_url-zeway_cr.        "Eway Create by IRN URL
    CONDENSE gv_url NO-GAPS.
  ENDIF.

  " IF GV_OWNERID IS INITIAL.        "GSTIN
  SELECT SINGLE * FROM yeinv_url INTO wa_url WHERE gstin = gv_gstin AND Ownerid = '1550241' .
  "ENDIF.
  SELECT SINGLE * FROM yzeinv_url INTO wa_yzurl WHERE gstin = gv_gstin AND Ownerid = '1550241' .

  IF wa_url IS NOT INITIAL.
    gv_gstin    = gv_gstin.
    gv_ownerid  = wa_url-ownerid.
    "gv_auth     = wa_url-authtoken.
    gv_url      = wa_url-zeway_cr.
    "  GV_GETURL   = WA_URL-ZIRN_GETURL.
  ENDIF.

  IF wa_yzurl IS NOT INITIAL.
    gv_efusername = wa_yzurl-efusername .
    gv_efpassword = wa_yzurl-efpassword.
    gv_einvusername = wa_yzurl-einvusername.
    gv_einvpassword = wa_yzurl-einvpassword.
  ENDIF.




  IF gv_url IS NOT INITIAL.

*PASS the Token GSTIN Owner id from SAP Customized Table to Cancel the IRN
    TRY.

        CONDENSE p_v_payload NO-GAPS.
        cl_http_client=>create_by_url( EXPORTING url = gv_url IMPORTING client = http_client ).

*****POST SANDBOX & LIVE API Method********
        http_client->request->set_method( if_http_request=>co_request_method_post ).
        http_client->request->set_content_type( 'application/json; charset=utf-8' ).

******Header Details JSON **********
        "http_client->request->set_header_field( EXPORTING  name  = 'x-cleartax-auth-token' value = gv_auth ).
        "http_client->request->set_header_field( EXPORTING  name  = 'x-cleartax-product​' value = | EInvoice | ).
        " http_client->request->set_header_field( EXPORTING  name  = 'gstin'           value = gv_gstin ).
        " http_client->request->set_header_field( EXPORTING  name  = 'owner_id'        value = gv_ownerid ).
        http_client->request->set_cdata( p_v_payload ).
        http_client->send( ).
      CATCH cx_salv_msg INTO lc_msg .                   "#EC NO_HANDLER
*      DATA(lv_string) = lc_msg->get_text( ).   "
*      lv_string = lc_msg->get_text( ).   "
        p_ex_error = lv_string.
        EXIT.
    ENDTRY.
  ENDIF.
* Disable SAP's pop-up for user id and password:
  http_client->propertytype_logon_popup = http_client->co_disabled.    "  POPUP for user id and pwd

  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3.
  http_client->response->get_status( IMPORTING code   = lv_http_return_code ).
  http_client->response->get_status( IMPORTING reason = lv_http_error_descr ).

**Calling API
  p_response = http_client->response->get_cdata( ).

  SPLIT p_response AT '"Success":"' INTO lv_srt1  v_msg.

*************Converting JSON Format to ABAP Structure ********
  " IF v_msg = 'Y'.
  PERFORM json_abap_structure  USING p_im_ewb CHANGING p_eway_out p_response.
  " ENDIF.

ENDFORM.                    " API_CALL
*&---------------------------------------------------------------------*
*&      Form  JSON_ABAP_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IM_EWB  text
*      <--P_P_EWAY_OUT  text
*      <--P_P_RESPONSE  text
*----------------------------------------------------------------------*
FORM json_abap_structure USING     p_eway        TYPE yst_ewaybill
                          CHANGING p_eway_out1  TYPE j_1ig_ewaybill
                                   p_response1  TYPE string .

  DATA: v_ewbdt  TYPE char10,
        v_ewbdt1 TYPE char21.

  DATA : v_type    TYPE string,
         v_type1   TYPE string,
         v_veh     TYPE char12,
         v_ewb     TYPE char12,
         msg       TYPE char6,
         v_tempinv TYPE char10,
         v_status  TYPE char03.

  SPLIT p_response1 AT '{"Success":' INTO v_type msg .


  IF msg IS INITIAL.
    SPLIT p_response1 AT '{"IsSuccess":' INTO v_type msg .
    "  SPLIT P_RESPONSE1 AT '"Success":' INTO V_TYPE MSG .
  ENDIF.

******JSON Format to ABAP Structure **************
  IF lv_http_return_code = '200' AND lv_http_error_descr = 'OK' AND ( msg = '"Y"' OR msg = '"True"' ).


**********************Eway Bill Details****************
******Output Structure*********
    CLEAR: v_type,v_type1,v_status,v_ewbdt,v_ewbdt1,v_veh,v_ewb.

    SPLIT p_response1 AT '"EwbNo":' INTO v_type p_eway_out1-ebillno.
    MOVE p_eway_out1-ebillno TO v_ewb.

    IF v_ewb IS NOT INITIAL.

      MOVE sy-mandt     TO p_eway_out1-mandt.
      MOVE p_eway-bukrs  TO p_eway_out1-bukrs.
      MOVE p_eway-vbeln  TO p_eway_out1-docno.
      MOVE p_eway-gjahr  TO p_eway_out1-gjahr.
      MOVE p_eway-doctyp TO p_eway_out1-doctyp.
      MOVE sy-uname     TO p_eway_out1-ernam.
      MOVE sy-datum     TO p_eway_out1-erdat.
      MOVE sy-datum     TO p_eway_out1-egen_dat.
      MOVE sy-uzeit     TO p_eway_out1-egen_time.

      "Vehicle No.
      MOVE p_eway-vehno TO v_veh.
      REPLACE ALL OCCURRENCES OF '"' IN v_veh WITH space.
      CONDENSE v_veh NO-GAPS.
      MOVE v_veh          TO p_eway_out1-vehno.

******Eway Bill Status**********
      MOVE 'A' TO p_eway_out1-status.    " A-Active , C-Cancelled

****EWB From Date & Time*********
      CLEAR: v_type.
      SPLIT p_response1 AT '"EwbDt":"' INTO v_type v_ewbdt1.

****Valid From date
      REPLACE ALL OCCURRENCES OF '-' IN v_ewbdt1 WITH space.
      CONDENSE v_ewbdt1 NO-GAPS.
      MOVE v_ewbdt1(8) TO p_eway_out1-vdfmdate.

****Valid From Time
      REPLACE ALL OCCURRENCES OF ':' IN v_ewbdt1 WITH space.
      CONDENSE v_ewbdt1 NO-GAPS.
      MOVE v_ewbdt1+8(6) TO p_eway_out1-vdfmtime.

**********Valid TO Date**********
*****EWB To date****************
      CLEAR: v_type,v_ewbdt1.
      SPLIT p_response1 AT '"EwbValidTill":"' INTO v_type v_ewbdt1.

****Valid TO date
      REPLACE ALL OCCURRENCES OF '-' IN v_ewbdt1 WITH space.
      CONDENSE v_ewbdt1 NO-GAPS.
      MOVE v_ewbdt1(8) TO p_eway_out1-vdtodate.

****Valid To Time
      REPLACE ALL OCCURRENCES OF ':' IN v_ewbdt1 WITH space.
      CONDENSE v_ewbdt1 NO-GAPS.
      MOVE v_ewbdt1+8(6) TO p_eway_out1-vdtotime.

    ENDIF.
  ENDIF.
ENDFORM.                    " JSON_ABAP_STRUCTURE
*&---------------------------------------------------------------------*
*&      Form  ERROR_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ERROR_MSG  text
*      -->P_RESPONSE  text
*----------------------------------------------------------------------*
FORM error_msg  TABLES p_error_msg TYPE bapiret2_t
                  USING p_response   TYPE string.
  SPLIT p_response AT '"errors":' INTO v_str v_errormsg.
  REPLACE ALL OCCURRENCES OF '}' IN v_errormsg WITH space.

  IF v_errormsg IS INITIAL.
    SPLIT p_response AT '},"ewb_status":"' INTO v_str v_errormsg.
    SPLIT v_str AT '"ErrorDetails":' INTO v_str1 v_errormsg.
  ENDIF.


  CLEAR:v_str.
  SPLIT v_errormsg AT '"error_code":"'   INTO v_str wa_error-error_code.
  CLEAR: v_str.
  SPLIT v_errormsg AT '"error_message":"' INTO v_str1 wa_error-error_message.
  CLEAR: v_str.
  SPLIT v_errormsg AT '"error_source":"' INTO v_str1 wa_error-error_source.

*  /ui2/cl_json=>deserialize( EXPORTING json = v_errormsg
*                               pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*                      CHANGING data = it_error ).
*  LOOP AT it_error INTO wa_error.
  wa_bapiret-id         = wa_error-error_code.
  wa_bapiret-message    = wa_error-error_message.
  wa_bapiret-message_v1 = wa_error-error_source.
  APPEND wa_bapiret TO p_error_msg.
  CLEAR wa_bapiret.
*  ENDLOOP.

ENDFORM.                    " ERROR_MSG
*&---------------------------------------------------------------------*
*&      Form  RESPONSE_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EWAY_OUT  text
*----------------------------------------------------------------------*
FORM response_move  USING    p_eway_out TYPE j_1ig_ewaybill.

***Ewaybill details--->>>
  wa_j_1ig_ewaybill-bukrs           = p_eway_out-bukrs.
  wa_j_1ig_ewaybill-doctyp          = p_eway_out-doctyp.
*  SHIFT p_eway_out-docno LEFT DELETING LEADING '0'.
  wa_j_1ig_ewaybill-docno           = p_eway_out-docno.
  wa_j_1ig_ewaybill-gjahr           = p_eway_out-gjahr.
  wa_j_1ig_ewaybill-ebillno         = p_eway_out-ebillno.
  wa_j_1ig_ewaybill-vehno           = p_eway_out-vehno.
  wa_j_1ig_ewaybill-egen_dat        = p_eway_out-egen_dat.
  wa_j_1ig_ewaybill-egen_time       = p_eway_out-egen_time.
  wa_j_1ig_ewaybill-vdfmdate        = p_eway_out-vdfmdate.
  wa_j_1ig_ewaybill-vdfmtime        = p_eway_out-vdfmtime.
  wa_j_1ig_ewaybill-vdtodate        = p_eway_out-vdtodate.
  wa_j_1ig_ewaybill-vdtotime        = p_eway_out-vdtotime.
  wa_j_1ig_ewaybill-status          = p_eway_out-status.
  wa_j_1ig_ewaybill-ernam           = p_eway_out-ernam.
  wa_j_1ig_ewaybill-erdat           = p_eway_out-erdat.
  wa_j_1ig_ewaybill-aenam           = p_eway_out-aenam.
  wa_j_1ig_ewaybill-aedat           = p_eway_out-aedat.

ENDFORM.                    " RESPONSE_MOVE
*&---------------------------------------------------------------------*
*&      Form  SELLER_BUYER_DTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seller_buyer_dtls  USING p_ewb TYPE zst_ewaybill.

  "Get Plant address details for Seller Addr1
  SELECT a~werks
         a~pstlz
         a~land1
         a~regio
         b~bezei
         c~addrnumber      "Seller Legal Name
         c~name1           "Despatch from Name\
         c~house_num1
         c~street          "Street     &"Despatch from add1
         c~str_suppl3      "
         c~location        "Despatch from add1
         c~post_code1
         c~region
         c~mc_city1
                FROM ( ( t001w AS a
                  INNER JOIN t005u AS b
                  ON  b~spras = sy-langu
                  AND a~land1 = b~land1
                  AND a~regio = b~bland )
                  INNER JOIN adrc AS c
                  ON  a~adrnr = c~addrnumber )
                 INTO TABLE l_tab_t005u
*                FOR ALL ENTRIES IN l_tab_vbrp
            WHERE a~werks = lv_werks.

  "Get BUYER details
  SELECT a~vbeln
         a~kunnr
         a~parvw       "Partner Functn
         b~name1       "Buyer Legal Name & "Ship to Legal Name
         b~stcd3       "Buyer Gstin    & "Ship to SGTIN
         b~land1
         b~regio       "Buyer State Code
         c~addrnumber
         c~post_code1  "Buyer Pin Code
         c~str_suppl1
         c~house_num1   "Ship to LEFT
         c~str_suppl3   "Buyer Address1
         c~city1        "Buyer Address1
         c~location     "Despatch from LEFT
         c~region       "Ship to State Code
         d~bezei
          FROM ( ( ( vbpa AS a
                INNER JOIN kna1 AS b
               ON a~kunnr = b~kunnr
               INNER JOIN adrc AS c
               ON b~adrnr = c~addrnumber )
               LEFT OUTER JOIN t005u AS d
               ON d~spras = sy-langu
               AND b~land1 = d~land1
               AND b~regio = d~bland ) )
              INTO TABLE l_tab_vbpa_adrc
*                    FOR ALL ENTRIES IN l_tab_vbrp
             WHERE a~vbeln = p_ewb-vbeln AND a~parvw = 'AG'.



ENDFORM.                    " SELLER_BUYER_DTLS
*&---------------------------------------------------------------------*
*&      Form  EXP_DTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exp_dtls .

  CONCATENATE

      '"ShipTo_Addr1":"' wa_ship_port-plant_name '",'
      '"ShipTo_Addr2": null,'
      '"ShipTo_Loc":' '"'  wa_ship_port-sh_loc '",'       "lv_sel_loc
      '"ShipTo_Pin":'   '"'    wa_ship_port-sh_pin '",'            "lv_sel_pin
      '"ShipTo_Stcd":' '"' wa_ship_port-sh_region '",'     "lv_region
      INTO gv_expdtls.


ENDFORM.                    " EXP_DTLS
