class ZCL_ADOBE_ESIGN_PROCESS definition
  public
  final
  create public .

public section.

  types:
    zsd_dms_sale_itm_ty TYPE STANDARD TABLE OF zsd_dms_sale_itm .

  methods GET_DOCUMENTID
    importing
      !HR_SCREEN_STRUCT type ZHR_ST_APPT_LETTER optional
      !PERNR type PERNR_D
      !LTYPE type ZHR_LETTER_TYP
      !LDATE type BEGDA
      !SIGN_LINK type C optional
    exporting
      !TRANSIENTID type STRING .
  methods POST_DCUMENTID
    importing
      !PERNR type PERNR_D
      !LTYPE type ZHR_LETTER_TYP
      !LDATE type BEGDA
      !SIGN_EMAIL type ZASSIGN_EMAIL
      !EMP_EMAIL type ZASSIGN_EMAIL
      !TRANSIENTID type STRING
    exporting
      !AGREEMENTID type STRING .
  methods GET_STATUS
    importing
      !PERNR type PERNR_D
      !LTYPE type ZHR_LETTER_TYP
      !LDATE type BEGDA
      !TRANSIENTID type STRING
      !AGREEMENTID type STRING
    exporting
      !STATUS type TEXT50 .
  methods GET_SIGNED_DOCUMENT
    importing
      !PERNR type PERNR_D
      !LTYPE type ZHR_LETTER_TYP
      !LDATE type BEGDA
      !TRANSIENTID type STRING
      !AGREEMENTID type STRING
    exporting
      !PDF type FPCONTENT .
  methods GENERATE_LETTER
    importing
      !HR_SCREEN_STRUCT type ZHR_ST_APPT_LETTER optional
      !PERNR type PERNR_D
      !LTYPE type ZHR_LETTER_TYP
      !LDATE type BEGDA
      !SIGN_LINK type C optional
      !SIGN_EMAIL type ZASSIGN_EMAIL optional
      !EMP_EMAIL type ZASSIGN_EMAIL optional
    exporting
      !OUT_PDF type FPCONTENT
    exceptions
      NOT_FOUND .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ADOBE_ESIGN_PROCESS IMPLEMENTATION.


  METHOD generate_letter.

    DATA ls_docpara TYPE sfpdocparams.
    DATA ls_outpara TYPE sfpoutputparams.
    DATA ls_output  TYPE fpformoutput.
    DATA  ls_frmname TYPE fpname.
    DATA: lv_fm        TYPE rs38l_fnam,
          l_count      TYPE numc5,
*          l_emp_letter TYPE zhr_emp_letters,
          l_app_letter TYPE  zhr_appointment_letter.

    MOVE-CORRESPONDING hr_screen_struct TO l_app_letter.

    SELECT SINGLE bukrs FROM pa0001
      INTO @DATA(l_ccode)
      WHERE pernr = @pernr.

*    l_emp_letter = gw_emp_letter.
    SELECT  MAX( lcount )
    FROM zhr_letters_sign INTO l_count
      WHERE bukrs = l_ccode AND
            ltype = ltype AND
            lsrno = 1.
    SELECT SINGLE *
    FROM zhr_letters_sign INTO @DATA(l_letter_sign)
      WHERE bukrs = @l_ccode AND ltype = @ltype AND lsrno = 1.
    IF sy-subrc = 0.
      IF sign_link IS INITIAL.
        l_count = l_count + 1.
        l_letter_sign-lcount = l_count.
        l_app_letter-singnatory = l_letter_sign-emnam.
        l_app_letter-singnatory_design = l_letter_sign-designation.
        MODIFY zhr_letters_sign FROM l_letter_sign.
        l_app_letter-letterdate = sy-datum.
      ELSE.
        l_app_letter-singnatory = l_letter_sign-emnam.
        l_app_letter-singnatory_design = l_letter_sign-designation.
        l_app_letter-letterdate = ldate.
      ENDIF.
    ELSE.
      RAISE not_found.
*      l_txt = 'Letter type is not found in Table ZHR_LETTERS_SIGN'.
*      MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
    ENDIF.

    CASE ltype.
      WHEN '11'. " Appointment
        CASE l_ccode.
          WHEN '1000'.        ls_frmname = 'ZHR_APPMT_LETTER'. DATA(l_txt) = 'SPL'.
          WHEN '1800'.        ls_frmname = 'ZHR_APPMT_LETTER_1800'. l_txt = 'ITW'.
          WHEN '1400'.        ls_frmname = 'ZHR_APPMT_LETTER_1400'. l_txt = 'TECH'.
        ENDCASE.
        l_app_letter-letterref = |{ l_txt }{ sy-datum(4) }/APP/{ l_count }|.
      WHEN '12'. " Confirmation
      WHEN OTHERS.
    ENDCASE.

    l_app_letter-w_sal_month-basic = hr_screen_struct-grosspay * '0.40'. "40 % of Gross
    l_app_letter-w_sal_month-hra = hr_screen_struct-grosspay * '0.20'. "20 % of Gross
    l_app_letter-w_sal_month-conveyance = '1600'.

    IF hr_screen_struct-grosspay GE 21000.
      l_app_letter-w_sal_month-medical = '1250'.
    ELSE.
      l_app_letter-w_sal_month-medical = 0.
      DATA l_number TYPE i.
      l_number = hr_screen_struct-grosspay * '0.0075'.
      l_app_letter-w_sal_month-esiemployee = l_number.
      l_number = hr_screen_struct-grosspay * '0.0325'.
      l_app_letter-w_sal_month-esiemployer = l_number.
    ENDIF.

    IF hr_screen_struct-grosspay LE 11000.
      l_app_letter-w_sal_month-food = 0.
    ELSEIF hr_screen_struct-grosspay BETWEEN 11001 AND 21000.
      l_app_letter-w_sal_month-food = '2500'.
    ELSEIF hr_screen_struct-grosspay GT 21000.
      l_app_letter-w_sal_month-food = '3500'.
    ENDIF.

    l_app_letter-w_sal_month-grosspay = hr_screen_struct-grosspay.

    l_app_letter-w_sal_month-special = l_app_letter-w_sal_month-grosspay -
                           ( l_app_letter-w_sal_month-basic +
                           l_app_letter-w_sal_month-hra +
                           l_app_letter-w_sal_month-conveyance +
                           l_app_letter-w_sal_month-medical +
                           l_app_letter-w_sal_month-food ).

    DATA l_string TYPE string.

    IF ( hr_screen_struct-grosspay - l_app_letter-w_sal_month-hra ) GE 15000.
      l_app_letter-w_sal_month-pfemployee = 1800.
    ELSE.
      l_app_letter-w_sal_month-pfemployee = ( l_app_letter-w_sal_month-grosspay - l_app_letter-w_sal_month-hra ) * '0.12'.
      CLEAR l_string.

      l_string = l_app_letter-w_sal_month-pfemployee.
      SPLIT l_string AT '.' INTO DATA(l_va11) DATA(l_val2). "= l_app_letter-w_sal_month-pfemployee
      IF l_val2(1) GE 5.
        l_app_letter-w_sal_month-pfemployee = l_va11 + 1.
      ELSE.
        l_app_letter-w_sal_month-pfemployee = l_va11.
      ENDIF.
      CLEAR: l_string, l_va11, l_val2.
    ENDIF.


    l_app_letter-w_sal_month-netpay = l_app_letter-w_sal_month-grosspay - l_app_letter-w_sal_month-pfemployee - l_app_letter-w_sal_month-esiemployee.

    IF ( hr_screen_struct-grosspay - l_app_letter-w_sal_month-hra ) GE 15000.
      l_app_letter-w_sal_month-pfemployer = 1800.
    ELSE.
      l_app_letter-w_sal_month-pfemployer = ( l_app_letter-w_sal_month-grosspay - l_app_letter-w_sal_month-hra ) * '0.12'.

      CLEAR: l_string, l_va11, l_val2.

      l_string = l_app_letter-w_sal_month-pfemployer.
      SPLIT l_string AT '.' INTO l_va11 l_val2. "= l_app_letter-w_sal_month-pfemployer
      IF l_val2(1) GE 5.
        l_app_letter-w_sal_month-pfemployer = l_va11 + 1.
      ELSE.
        l_app_letter-w_sal_month-pfemployer = l_va11.
      ENDIF.
      CLEAR: l_string, l_va11, l_val2.
    ENDIF.
    l_app_letter-w_sal_month-variablepay = hr_screen_struct-variablepay.

    l_app_letter-w_sal_annual-basic = l_app_letter-w_sal_month-basic * 12.
    l_app_letter-w_sal_annual-hra = l_app_letter-w_sal_month-hra * 12.
    l_app_letter-w_sal_annual-conveyance = l_app_letter-w_sal_month-conveyance * 12.
    l_app_letter-w_sal_annual-medical = l_app_letter-w_sal_month-medical * 12.
    l_app_letter-w_sal_annual-food = l_app_letter-w_sal_month-food * 12.
    l_app_letter-w_sal_annual-special = l_app_letter-w_sal_month-special * 12.
    l_app_letter-w_sal_annual-grosspay = l_app_letter-w_sal_month-grosspay * 12.
    l_app_letter-w_sal_annual-pfemployee = l_app_letter-w_sal_month-pfemployee * 12.
    l_app_letter-w_sal_annual-esiemployee = l_app_letter-w_sal_month-esiemployee * 12.
    l_app_letter-w_sal_annual-netpay = l_app_letter-w_sal_annual-grosspay - l_app_letter-w_sal_annual-pfemployee.
    l_app_letter-w_sal_annual-pfemployer = l_app_letter-w_sal_month-pfemployer * 12.
    l_app_letter-w_sal_annual-esiemployer = l_app_letter-w_sal_month-esiemployer * 12.
    l_app_letter-w_sal_annual-gratuity = l_app_letter-w_sal_annual-basic * '0.048'.

    CLEAR: l_va11, l_val2, l_string.
    l_string = l_app_letter-w_sal_annual-gratuity.

    SPLIT l_string AT '.' INTO l_va11 l_val2. "= l_app_letter-w_sal_annual-gratuity
    IF l_val2(1) GE 5.
      l_app_letter-w_sal_annual-gratuity = l_va11 + 1.
    ELSE.
      l_app_letter-w_sal_annual-gratuity = l_va11.
    ENDIF.
    CLEAR: l_va11, l_val2.
    l_app_letter-w_sal_annual-bonus = 8400.
    l_app_letter-w_sal_month-gratuity = l_app_letter-w_sal_annual-gratuity / 12.
    l_string = l_app_letter-w_sal_month-gratuity.
    SPLIT l_string AT '.' INTO l_va11 l_val2. "= l_app_letter-w_sal_annual-gratuity
    IF l_val2(1) GE 5.
      l_app_letter-w_sal_month-gratuity = l_va11 + 1.
    ELSE.
      l_app_letter-w_sal_month-gratuity = l_va11.
    ENDIF.
    l_app_letter-w_sal_month-bonus = l_app_letter-w_sal_annual-bonus / 12.
    l_app_letter-w_sal_month-fixedctc =  l_app_letter-w_sal_month-grosspay + l_app_letter-w_sal_month-pfemployer + l_app_letter-w_sal_month-esiemployer +
                                         l_app_letter-w_sal_month-gratuity + l_app_letter-w_sal_month-bonus.
    l_app_letter-w_sal_annual-fixedctc = l_app_letter-w_sal_month-fixedctc * 12.
    l_app_letter-w_sal_annual-variablepay = l_app_letter-w_sal_month-variablepay * 12.
    l_app_letter-w_sal_month-totalctc =  l_app_letter-w_sal_month-fixedctc +  l_app_letter-w_sal_month-variablepay.
    l_app_letter-w_sal_annual-totalctc = l_app_letter-w_sal_annual-fixedctc + l_app_letter-w_sal_annual-variablepay.
    IF sign_link = abap_true.
      l_app_letter-sign_link1 = '{{BigSig_es_:signer1:signature:dimension(width=75mm,height=15mm)}}'.
      l_app_letter-sign_link2 = '{{BigSig_es_:signer2:signature:dimension(width=75mm,height=15mm)}}'.
    ENDIF.


    ls_outpara-getpdf = abap_true.
*ls_outpara-nodialog  = 'X'.
*ls_outpara-preview = abap_true.

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outpara
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = ls_frmname
          IMPORTING
            e_funcname = lv_fm.
      CATCH cx_root.
        RETURN.
    ENDTRY.

* 3. Generate the PDF data.
    CALL FUNCTION lv_fm
      EXPORTING
        /1bcdwb/docparams  = ls_docpara
        appt_letter        = l_app_letter
      IMPORTING
        /1bcdwb/formoutput = ls_output
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CALL FUNCTION 'FP_JOB_CLOSE'
*   IMPORTING
*     E_RESULT      =
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    out_pdf = ls_output-pdf.

  ENDMETHOD.


  METHOD get_documentid.

*    TYPES:t_xline(2048) TYPE x. "BINARY FILES
*    DATA: lt_data_tab TYPE STANDARD TABLE OF t_xline.
*    DATA: l_filename    TYPE string.
*    DATA: it_lines    TYPE TABLE OF tline,
*          lt_otf_data TYPE TABLE OF itcoo,
*          w_otf_data  LIKE LINE OF lt_otf_data.
*    DATA: lv_size      TYPE i,
*          lv_bin_value TYPE xstring,
*          bin_filesize TYPE i,
*          lv_dummy     TYPE c,
*          lv_clen      TYPE i,
*          lv_len       TYPE i,
*          lv_hex(144)  TYPE x,
*          lv_line(72)  TYPE c.
*    FIELD-SYMBOLS: <l_fs> TYPE c.
*    DATA : lv_bearer_token TYPE string.
*
*

    DATA: p_http_client TYPE REF TO if_http_client.
    DATA  part          TYPE REF TO if_http_entity.
    DATA: lw_data TYPE fpcontent,
          lv_url  TYPE string.
    DATA  lv_code      TYPE i.      "STATUS Code
    DATA: lv_response TYPE string,
          lv_status   TYPE string. "API Response
    DATA: l_date TYPE sy-datum.
*    SELECT SINGLE low FROM tvarvc
*      INTO @DATA(l_url)
*      WHERE name = 'ZHR_L_ADOBE_TRANS_URL'
*      AND   type = 'P'.
*    lv_url = l_url.
*    CASE ltype.
*      WHEN '1'.
*        CONCATENATE 'C:\HR Letters\Appt Letter\' "/C:/HR Letters/Appt Letter/
*                    pernr
*                    '.pdf' INTO l_filename.
*      WHEN '2'.
*        CONCATENATE 'C:\HR Letters\Confirm Letter\'
*                    pernr
*                    '.pdf' INTO l_filename.
*      WHEN OTHERS.
*    ENDCASE.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = 'https://api.in1.adobesign.com/api/rest/v6/transientDocuments'
      IMPORTING
        client             = p_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    p_http_client->request->set_method( 'POST' ).

    SELECT SINGLE token FROM zhr_auth_token
      INTO @DATA(l_token)
      WHERE sysid = @sy-sysid.

* header field - Authorization
    CALL METHOD p_http_client->request->set_header_field(
      EXPORTING
        name  = 'Authorization'
        value = |Bearer { l_token }| )."3AAABLBLQZHBAVMOYFAYWXQRJQRJJQBMX8ETHYDROLUII0AKKSM77Y_NTHLMQLI0BQJB8EGI5FUIYWDCDEN9WNGWPUUM0HKCV
*        value = 'Bearer 3AAABLblqZhBB8_50AGdVwF14hLEo3TAM-07chYlT9W_TPMjEC0k4VZs6Klbqm1MomSZOp8P4VWU98zxx1EJwFwkJTCnRRkg6' ).


* header field - Content-Type
    CALL METHOD p_http_client->request->set_header_field
      EXPORTING
        name  = 'content-type'
        value = 'multipart/form-data'.
*    p_http_client->request->set_content_type( content_type = 'multipart/form-data'  ).

* header field - Accept

    CALL METHOD p_http_client->request->set_header_field
      EXPORTING
        name  = 'Accept'
        value = 'application/json'.

    CALL METHOD p_http_client->request->if_http_entity~set_formfield_encoding
      EXPORTING
        formfield_encoding = cl_http_request=>if_http_entity~co_encoding_raw.
*    p_http_client->request->set_header_field( name = 'Accept' value = 'application/json' ).

    DATA l_string TYPE string.
    p_http_client->propertytype_logon_popup = 0.
*    p_http_client->propertytype_logon_popup = p_http_client->co_disabled.

    part = p_http_client->request->add_multipart( ).
*    DATA:part TYPE REF TO if_http_entity.
*    part = p_http_client->request->add_multipart( ).

    part->set_header_field( name  = if_http_header_fields=>content_disposition
                            value = 'form-data;name="File-Name"' ).
    l_string = |{ pernr }.pdf|.
    part->append_cdata( data = l_string ).

    part->set_header_field( name  = if_http_header_fields=>content_disposition
                            value = 'form-data;name="Mime-Type"' ).
    part->append_cdata( data = 'application/pdf' ).

    l_string = |form-data;name="File";filename="{ pernr }"|.
    part->set_header_field( name  = if_http_header_fields=>content_disposition
*                            value = 'form-data;name="File";filename="MyFile.pdf"' ).
                            value = l_string ).

    CALL METHOD part->set_content_type
      EXPORTING
        content_type = 'application/pdf'.

    CALL METHOD me->generate_letter
      EXPORTING
        hr_screen_struct = hr_screen_struct
        pernr            = pernr
        ltype            = ltype
        ldate            = ldate
        sign_link        = sign_link
*       sign_email       =
*       emp_email        =
      IMPORTING
        out_pdf          = lw_data
      EXCEPTIONS
        not_found        = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
*  TRY.
**
*    CALL METHOD cl_gui_frontend_services=>gui_upload
*      EXPORTING
*        filename   = l_filename
*        filetype   = 'BIN'
*      IMPORTING
*        filelength = lv_size
*      CHANGING
*        data_tab   = lt_data_tab.
**    APPEND lw_data TO lt_data_tab.
**    lv_size = xstrlen( lw_data ).
*    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*      EXPORTING
*        input_length = lv_size
*      IMPORTING
*        buffer       = lv_bin_value
*      TABLES
*        binary_tab   = lt_data_tab.

*    DATA(pdf64) = cl_http_utility=>encode_x_base64( unencoded = lv_bin_value ).

*      LOOP AT it_lines INTO DATA(w_data).


*
*    DATA:filename TYPE string.
*
*    filename = '/sapmnt/vportal/4000026659.pdf'.
*
*    DATA:lv_raw TYPE xstring.
    DATA len TYPE i.
*
*    OPEN DATASET filename FOR INPUT  IN LEGACY BINARY MODE CODE PAGE '1100'.
*    READ DATASET filename INTO lv_raw.

    len = xstrlen( lw_data ).

    CALL METHOD part->set_data
      EXPORTING
        data   = lw_data
        offset = 0
        length = len.
*    CLOSE DATASET filename .

    CALL METHOD p_http_client->send
      EXPORTING
        timeout                    = 200
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    IF sy-subrc = 0.

      CALL METHOD p_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 5.

    ENDIF.

    CALL METHOD p_http_client->response->get_status
      IMPORTING
        code   = lv_code
        reason = lv_status.
* Response
    lv_response = p_http_client->response->get_cdata( ).

    IF lv_response IS NOT INITIAL AND lv_code = '200' OR lv_code = '201'.
      REPLACE ALL OCCURRENCES OF '{'   IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '}'  IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '"'  IN lv_response WITH ''.
      SPLIT lv_response AT ':' INTO TABLE DATA(lt_response).
      transientid = lt_response[ 2 ].
    ENDIF.
  ENDMETHOD.


  METHOD get_status.

    DATA : lv_bearer_token TYPE string.
    DATA : l_url TYPE string.
    DATA: p_http_client TYPE REF TO if_http_client.
    DATA: lv_url        TYPE string.
    DATA  lv_code      TYPE i.      "STATUS Code
    DATA: lv_response TYPE string,
          lv_status   TYPE string. "API Response

    l_url = |https://api.in1.adobesign.com:443/api/rest/v6/agreements/{ agreementid }|.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = l_url
      IMPORTING
        client             = p_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    p_http_client->request->set_method( 'GET' ).

    SELECT SINGLE token FROM zhr_auth_token
      INTO @DATA(l_token)
      WHERE sysid = @sy-sysid.

* header field - Authorization
    CALL METHOD p_http_client->request->set_header_field(
      EXPORTING
        name  = 'Authorization'
        value = |Bearer { l_token }| )."3AAABLblqZhBaVmoYfayWXqRJQRJjqbMX8EthyDrOLuiI0AKkSM77Y_nThLmQLi0BQJB8eGi5fuiYWdcdeN9WNGWpuuM0HKcv

    p_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).

*    p_http_client->request->set_cdata(
*      EXPORTING
*      data = '{ JSON_Payload }' ).
*    DATA v_jsonload TYPE string.
*
*    CONCATENATE
*                '{'
*                '"fileInfos": ['
*                  '{'
*                  '"transientDocumentId": "' transientid '"'
*                  '}],'
*                '"name": "PraveenLetter",'
*                '"participantSetsInfo": ['
*                  '{'
*                    '"order": 1,'
*                    '"role": "SIGNER",'
*                    '"memberInfos": ['
*                      '{'
*                        '"email": "' sign_email '"'
*                      '}'
*                    ']'
*                  '},'
*                  '{'
*                    '"order": 2,'
*                    '"role": "SIGNER",'
*                    '"memberInfos": ['
*                      '{'
*                        '"email": "' emp_email '"'
*                      '}'
*                    ']'
*                  '}'
*                '],'
*                '"signatureType": "ESIGN",'
*                '"state": "IN_PROCESS"'
*              '}' INTO v_jsonload.
*
*    CONDENSE v_jsonload NO-GAPS.
*
*    p_http_client->request->set_cdata(
*    EXPORTING
*     data = v_jsonload ).

    CALL METHOD p_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    IF sy-subrc = 0.
      CALL METHOD p_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 5.
    ENDIF.

    CALL METHOD p_http_client->response->get_status
      IMPORTING
        code   = lv_code
        reason = lv_status.
* Response
    lv_response = p_http_client->response->get_cdata( ).

    IF lv_response IS NOT INITIAL AND lv_code = '200' OR lv_code = '201'.
      REPLACE ALL OCCURRENCES OF '{'   IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '}'  IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '"'  IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '['  IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF ']'  IN lv_response WITH ''.
      SPLIT lv_response AT ',' INTO TABLE DATA(lt_response).
      LOOP AT lt_response INTO DATA(lw_res).
        SPLIT lw_res AT ':' INTO DATA(l_field) DATA(l_value).
        IF l_field = 'status'.
          status = l_value.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD post_dcumentid.

    DATA: p_http_client TYPE REF TO if_http_client.
    DATA : lv_bearer_token TYPE string.

    DATA: lv_url        TYPE string.
    DATA  lv_code      TYPE i.      "STATUS Code
    DATA: lv_response TYPE string,
          lv_status   TYPE string. "API Response


*    SELECT SINGLE low FROM tvarvc
*      INTO @DATA(l_url)
*      WHERE name = 'ZHR_L_ADOBE_TRANS_URL'
*      AND   type = 'P'.
*    lv_url = l_url.


    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = 'https://api.in1.adobesign.com:443/api/rest/v6/agreements'
      IMPORTING
        client             = p_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    p_http_client->request->set_method( 'POST' ).

    SELECT SINGLE token FROM zhr_auth_token
      INTO @DATA(l_token)
      WHERE sysid = @sy-sysid.

* header field - Authorization
    CALL METHOD p_http_client->request->set_header_field(
      EXPORTING
        name  = 'Authorization'
        value = |Bearer { l_token }| )."3AAABLblqZhBaVmoYfayWXqRJQRJjqbMX8EthyDrOLuiI0AKkSM77Y_nThLmQLi0BQJB8eGi5fuiYWdcdeN9WNGWpuuM0HKcv

    p_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).

    p_http_client->request->set_cdata(
      EXPORTING
      data = '{ JSON_Payload }' ).
    DATA v_jsonload TYPE string.
    DATA l_pernr TYPE char10.
    l_pernr = pernr.
    SHIFT l_pernr LEFT DELETING LEADING '0'.
    CONCATENATE
                '{'
                '"fileInfos": ['
                  '{'
                  '"transientDocumentId": "' transientid '"'
                  '}],'
                '"name": "Appoinment Letter(' l_pernr ')",'
                '"participantSetsInfo": ['
                  '{'
                    '"order": 1,'
                    '"role": "SIGNER",'
                    '"memberInfos": ['
                      '{'
                        '"email": "' sign_email '"'
                      '}'
                    ']'
                  '},'
                  '{'
                    '"order": 2,'
                    '"role": "SIGNER",'
                    '"memberInfos": ['
                      '{'
                        '"email": "' emp_email '"'
                      '}'
                    ']'
                  '}'
                '],'
                '"emailOption": {'
                '"sendOptions": {'
                '"completionEmails": "NONE",'
                '"inFlightEmails": "NONE",'
                '"initEmails": "ALL"'
                '}'
                '},'
                '"signatureType": "ESIGN",'
                '"state": "IN_PROCESS"'
              '}' INTO v_jsonload.

    CONDENSE v_jsonload NO-GAPS.

    p_http_client->request->set_cdata(
    EXPORTING
     data = v_jsonload ).


    CALL METHOD p_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    IF sy-subrc = 0.
      CALL METHOD p_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 5.
    ENDIF.

    CALL METHOD p_http_client->response->get_status
      IMPORTING
        code   = lv_code
        reason = lv_status.
* Response
    lv_response = p_http_client->response->get_cdata( ).

    IF lv_response IS NOT INITIAL AND lv_code = '200' OR lv_code = '201'.
      REPLACE ALL OCCURRENCES OF '{'   IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '}'  IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '"'  IN lv_response WITH ''.
      SPLIT lv_response AT ':' INTO TABLE DATA(lt_response).
      agreementid = lt_response[ 2 ].
    ENDIF.


*Output Entry in Log Table
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'APPT_LETTER'
          ijson           = v_jsonload
          ojson           = lv_response
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.



  ENDMETHOD.


  METHOD get_signed_document.

    DATA : lv_bearer_token TYPE string.
    DATA : l_url TYPE string.
    DATA: p_http_client TYPE REF TO if_http_client.
    DATA: lv_url        TYPE string.
    DATA  lv_code      TYPE i.      "STATUS Code
    DATA: lv_response TYPE string,
          lv_status   TYPE string. "API Response

    l_url = |https://api.in1.adobesign.com:443/api/rest/v6/agreements/{ agreementid }/documents|.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = l_url
      IMPORTING
        client             = p_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    p_http_client->request->set_method( 'GET' ).

    SELECT SINGLE token FROM zhr_auth_token
      INTO @DATA(l_token)
      WHERE sysid = @sy-sysid.

* header field - Authorization
    CALL METHOD p_http_client->request->set_header_field(
      EXPORTING
        name  = 'Authorization'
        value = |Bearer { l_token }| ). "3AAABLblqZhBaVmoYfayWXqRJQRJjqbMX8EthyDrOLuiI0AKkSM77Y_nThLmQLi0BQJB8eGi5fuiYWdcdeN9WNGWpuuM0HKcv

    p_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).


    CALL METHOD p_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    IF sy-subrc = 0.
      CALL METHOD p_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 5.
    ENDIF.

    CALL METHOD p_http_client->response->get_status
      IMPORTING
        code   = lv_code
        reason = lv_status.
* Response
    lv_response = p_http_client->response->get_cdata( ).

    IF lv_response IS NOT INITIAL AND lv_code = '200' OR lv_code = '201'.
      REPLACE ALL OCCURRENCES OF '{'   IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '}'  IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '"'  IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF '['  IN lv_response WITH ''.
      REPLACE ALL OCCURRENCES OF ']'  IN lv_response WITH ''.
      SPLIT lv_response AT ',' INTO TABLE DATA(lt_response).
      LOOP AT lt_response INTO DATA(lw_res).
        REPLACE ALL OCCURRENCES OF 'documents:id' IN lw_res WITH 'documentsid'.
        SPLIT lw_res AT ':' INTO DATA(l_field) DATA(l_value).
        IF l_field = 'documentsid'.
          DATA(documentsid) = l_value.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    FREE: p_http_client.

    l_url = |https://api.in1.adobesign.com:443/api/rest/v6/agreements/{ agreementid }/documents/{ documentsid }|.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = l_url
      IMPORTING
        client             = p_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    p_http_client->request->set_method( 'GET' ).

    SELECT SINGLE token FROM zhr_auth_token
      INTO @l_token
      WHERE sysid = @sy-sysid.

* header field - Authorization
    CALL METHOD p_http_client->request->set_header_field(
      EXPORTING
        name  = 'Authorization'
        value = |Bearer { l_token }| ). "3AAABLblqZhBaVmoYfayWXqRJQRJjqbMX8EthyDrOLuiI0AKkSM77Y_nThLmQLi0BQJB8eGi5fuiYWdcdeN9WNGWpuuM0HKcv

    p_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).


    CALL METHOD p_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    IF sy-subrc = 0.
      CALL METHOD p_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 5.
    ENDIF.

    CLEAR: lv_code,lv_status, lv_response.

    CALL METHOD p_http_client->response->get_status
      IMPORTING
        code   = lv_code
        reason = lv_status.

* Response
*    lv_response = p_http_client->response->get_cdata( ).
    pdf = p_http_client->response->get_data( ).


*    IF lv_response IS NOT INITIAL AND lv_code = '200' OR lv_code = '201'.
*      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*        EXPORTING
*          text   = lv_response     " variable type string
*        IMPORTING
*          buffer = pdf. " variable type xstring
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
