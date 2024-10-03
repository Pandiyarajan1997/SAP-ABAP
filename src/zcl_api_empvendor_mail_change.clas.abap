class ZCL_API_EMPVENDOR_MAIL_CHANGE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_EMPVENDOR_MAIL_CHANGE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 18.04.2023
*&Purpose : API for changing employee Vendor Mail Address after the Resignation
*&Reference: Ramakrishnan J
*-------------------------------------------------------------------------------------


    TYPES: BEGIN OF input,
             employeeno TYPE pernr_d,
           END OF input.

    DATA: gt_input TYPE TABLE OF input.

    TYPES: BEGIN OF output,
             employee_no TYPE pernr_d,
             type        TYPE bapi_mtype,
             msg         TYPE string,
           END OF output.
    DATA: gt_output TYPE TABLE OF output.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA: lv_msg TYPE string.
    DATA: ls_master_data_main TYPE vmds_ei_main.
    DATA: lt_master_data TYPE vmds_ei_extern_t,
          ls_master_data TYPE vmds_ei_extern.
    DATA: lt_mailid TYPE cvis_ei_smtp_t,
          ls_mailid TYPE cvis_ei_smtp_str.
    DATA: ls_address TYPE bapibus1006_address.

    DATA: it_maildata  TYPE STANDARD TABLE OF bapiadsmtp,
          it_maildatax TYPE STANDARD TABLE OF bapiadsmtx,  " BAPI Structure for E-Mail Addresses (Bus. Address Services)
          lt_return    TYPE TABLE OF bapiret2.

    DATA es_master_data_correct   TYPE vmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_message_correct       TYPE cvis_message. " Error Indicator and System Messages
    DATA es_master_data_defective TYPE vmds_ei_main. " Ext. Interface: Total Customer Data
    DATA es_message_defective     TYPE cvis_message. " Error Indicator and System Messages

    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
    CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        SELECT SINGLE pernr FROM pa0001 INTO @DATA(l_pernr) WHERE pernr = @<fs_input>-employeeno
                                                                 AND begda LE @sy-datum
                                                                 AND endda GE @sy-datum.
        IF sy-subrc <> 0.
          APPEND VALUE #( employee_no = <fs_input>-employeeno
                          type        = 'E'
                          msg         = |Incorrect Employee Number| ) TO gt_output.
        ELSE.
          SELECT SINGLE objid, sobid FROM hrp1001 INTO @DATA(l_hrp1001_cp)
                                     WHERE otype = 'P' AND objid = @l_pernr
                                     AND plvar = '01' AND rsign = 'A'
                                     AND relat = '209' AND istat = '1'
                                     AND begda LE @sy-datum
                                     AND endda GE @sy-datum
                                     AND sclas = 'CP'.
          IF sy-subrc = 0.
            SELECT SINGLE objid, sobid FROM hrp1001 INTO @DATA(l_hrp1001_bp)
                                       WHERE otype = 'CP' AND objid = @l_hrp1001_cp-sobid
                                       AND plvar = '01' AND rsign = 'B'
                                       AND relat = '207' AND istat = '1'
                                       AND begda LE @sy-datum
                                       AND endda GE @sy-datum
                                       AND sclas = 'BP'.
            IF sy-subrc = 0.
              DATA(l_vendor_bp) = CONV bapibus1006_head-bpartner( l_hrp1001_bp-sobid ).
              IF l_vendor_bp IS NOT INITIAL.
                SELECT SINGLE low FROM tvarvc
                    INTO @DATA(l_reponsible)
                    WHERE name = 'F&F_EMAIL_ID'
                    AND type = 'P'.

                SELECT SINGLE loevm FROM lfa1 INTO @DATA(l_flag) WHERE loevm EQ @space.
                IF sy-subrc EQ 0.
                  SELECT SINGLE * FROM but020 INTO @DATA(ls_but020) WHERE partner = @l_vendor_bp.
                  IF sy-subrc = 0.
                    SELECT SINGLE * FROM adr6 INTO @DATA(ls_adr6) WHERE addrnumber = @ls_but020-addrnumber.
                    IF sy-subrc = 0.
                      ls_adr6-smtp_addr = l_reponsible.
                      DATA(l_reponsible_cp) = l_reponsible.
                      TRANSLATE l_reponsible_cp TO UPPER CASE.
                      ls_adr6-smtp_srch = l_reponsible_cp.
                      MODIFY adr6 FROM ls_adr6.
                    ENDIF.
                  ENDIF.
                ENDIF.

                REFRESH: lt_master_data,lt_mailid. CLEAR: ls_master_data_main,ls_master_data.
                ls_master_data-header-object_instance-lifnr = l_vendor_bp.
                ls_master_data-header-object_task = 'U'.
                CLEAR: ls_master_data-central_data,ls_mailid.
                ls_mailid-contact-task = 'U'.
                ls_mailid-contact-data-std_no = abap_true.
                ls_mailid-contact-data-e_mail = l_reponsible.
                ls_mailid-contact-data-std_recip = abap_true.
                ls_mailid-contact-data-home_flag = abap_true.
                ls_mailid-contact-data-consnumber = '001'.
                ls_mailid-contact-datax-std_no = abap_true.
                ls_mailid-contact-datax-e_mail = abap_true.
                ls_mailid-contact-datax-std_recip = abap_true.
                ls_mailid-contact-datax-home_flag = abap_true.
                ls_mailid-contact-datax-consnumber = abap_true.
                ls_mailid-contact-datax-updateflag = abap_true.
                APPEND ls_mailid TO lt_mailid.
                ls_master_data-central_data-address-task = 'U'.
                ls_master_data-central_data-address-communication-smtp-current_state = abap_true.
                ls_master_data-central_data-address-communication-smtp-smtp = lt_mailid[].
                APPEND ls_master_data TO lt_master_data.
                ls_master_data_main-vendors = lt_master_data.
                CALL METHOD vmd_ei_api=>maintain_bapi
                  EXPORTING
                    iv_collect_messages      = 'X'
                    is_master_data           = ls_master_data_main
                  IMPORTING
                    es_master_data_correct   = es_master_data_correct
                    es_message_correct       = es_message_correct
                    es_master_data_defective = es_master_data_defective
                    es_message_defective     = es_message_defective.
                IF es_message_defective-is_error IS INITIAL.
                  COMMIT WORK.
                  APPEND VALUE #( employee_no = l_pernr
                                  type        = 'S'
                                  msg         = |Email Updated Successfully| ) TO gt_output.
                ELSE.
                  LOOP AT es_message_defective-messages INTO DATA(wa).
                    lv_msg = | { lv_msg } { wa-message } |.
                  ENDLOOP.
                  IF lv_msg IS NOT INITIAL.
                    APPEND VALUE #( employee_no = l_pernr
                                    type        = 'E'
                                    msg         = lv_msg ) TO gt_output.
                  ENDIF.
                ENDIF.
              ELSE.
                APPEND VALUE #( employee_no = l_pernr
                                type        = 'E'
                                msg         = |Kindly Check the BP Relationship for Employee| ) TO gt_output.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF gt_output[] IS NOT INITIAL.
      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
