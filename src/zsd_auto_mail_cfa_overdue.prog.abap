*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 18.06.2024
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                :
*
*  Business Logic            : Finance customer overdue Automail to channel manager
*
*  Released on Date          :
*
*=======================================================================
REPORT zsd_auto_mail_cfa_overdue.


PARAMETERS : p_fname TYPE rlgrap-filename.     "for excel file upload

SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN : PUSHBUTTON 5(15) TEXT-001 USER-COMMAND send,
PUSHBUTTON 35(15) TEXT-002 USER-COMMAND maint.

FIELD-SYMBOLS : <gt_data> TYPE STANDARD TABLE .

************************data definition****************
CLASS lcl_cust_bal DEFINITION.
  PUBLIC SECTION.
*****************Customer wise Data structure- Excel************
    TYPES : BEGIN OF ty_custexc,
              customer_code     TYPE string,
              customer_name     TYPE string,
              limit_type        TYPE string,
              sanctioned_amount TYPE string,
              principal         TYPE string,
              available_limit   TYPE string,
              max_dpd           TYPE string,
              overdue_amount    TYPE string,
              expiry_date       TYPE string,
              anchor_code	      TYPE string,
              anchor_name       TYPE string,
            END OF ty_custexc.
*****************Customer wise Data structure - mail - overdue DPD*************
    TYPES : BEGIN OF ty_custmail,
              channel_manager   TYPE name1,
              customer_code     TYPE kunnr,
              customer_name     TYPE name1,
              limit_type        TYPE char20,
              sanctioned_amount TYPE char30,
              principal         TYPE char30,
              available_limit   TYPE char30,
              max_dpd           TYPE char10,
              overdue_amount    TYPE char30,
            END OF ty_custmail.

*****************Contract wise Data structure - Excel *************
    TYPES : BEGIN OF ty_contexc,
              customer_code    TYPE string,
              customer_name    TYPE string,
              contract_number  TYPE string,
              limit_type       TYPE string,
              disbursed_amount TYPE string,
              principal        TYPE string,
              created_date     TYPE string,
              expiry_date      TYPE string,
              days_exp         TYPE string,
              dpd              TYPE string,
              overdue_amount   TYPE string,
              anchor_code	     TYPE string,
              anchor_name      TYPE string,
            END OF ty_contexc.

*****************Contract wise Data structure - Mail - Upcomming overdue*************
    TYPES : BEGIN OF ty_contmail,
              channel_manager  TYPE name1,
              customer_code    TYPE kunnr,
              customer_name    TYPE name1,
              limit_type       TYPE char20,
              disbursed_amount TYPE char30,
              principal        TYPE char30,
              created_date     TYPE char20,
              expiry_date      TYPE char20,
              days_exp         TYPE char10,
            END OF ty_contmail.
*********************data dec for raw data of excel**************
    DATA : gt_customer TYPE TABLE OF ty_custexc,
           gt_contract TYPE TABLE OF ty_contexc.
*********************data dec mail**************
    DATA : gt_custmail TYPE TABLE OF ty_custmail.
    DATA : gt_contmail TYPE TABLE OF ty_contmail.

    METHODS : read_file,
      process,
      send_mail.

ENDCLASS.

CLASS lcl_cust_bal IMPLEMENTATION.

  METHOD read_file.

    DATA : lv_filename      TYPE string,
           lt_records       TYPE solix_tab,
           lv_headerxstring TYPE xstring,
           lv_filelength    TYPE i.

    lv_filename = p_fname.
****************************************************************************************************

    DATA: l_t_data  TYPE w3mimetabtype,
          l_t_files TYPE filetable,
          l_rc      TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename = lv_filename
        filetype = 'BIN'
      IMPORTING
        filelength = DATA(l_length)
      CHANGING
        data_tab = l_t_data
    ).

    TRY.
        DATA(l_r_xls) = NEW cl_fdt_xl_spreadsheet(
          document_name = lv_filename
          xdocument = cl_fxs_converter=>w3mimetab_to_xstring( iv_w3mimetab = l_t_data iv_length = l_length )
        ).
      CATCH cx_fdt_excel_core.
        ASSERT 1 = 2.
    ENDTRY.

    l_r_xls->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(lt_worksheets) ).

    IF lt_worksheets IS NOT INITIAL.

      REFRESH : gt_contmail,gt_customer,gt_contract,gt_custmail.

      LOOP AT lt_worksheets INTO DATA(lv_woksheetname).

        DATA(lo_data_ref) = l_r_xls->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                                 lv_woksheetname ).
        "now you have excel work sheet data in dyanmic internal table
        ASSIGN lo_data_ref->* TO <gt_data>.
        IF <gt_data> IS ASSIGNED.
          DELETE <gt_data> INDEX 1.
        ENDIF.

        IF gt_customer IS INITIAL.

          gt_customer[] = <gt_data>.

        ELSEIF gt_contract IS INITIAL.

          gt_contract[] = <gt_data>.

        ENDIF.
      ENDLOOP.

    ENDIF.

**********************call the process method********
    IF gt_customer IS NOT INITIAL AND gt_contract IS NOT INITIAL.

      CALL METHOD process.

    ELSE.

      MESSAGE : 'Error in excel processing' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.


  ENDMETHOD.
  METHOD process.

********************get the customer code from kna1**************
    SELECT a~kunnr,a~ch_pernr_name,b~name1,b~zfincode FROM zcust_blk_chk AS a
                          INNER JOIN kna1 AS b
                          ON a~kunnr = b~kunnr
                          INTO TABLE @DATA(lt_kna1)
                          WHERE a~vkorg = '1000'.
    IF sy-subrc = 0.
      SORT : lt_kna1 BY zfincode.
    ENDIF.

**********************Get the total number of records - for highlight last record*************
    IF gt_customer IS NOT INITIAL.
      DATA(lv_lines) = lines( gt_customer ).
    ENDIF.
************OVERDUE (DPD) process*********

    LOOP AT gt_customer INTO DATA(ls_cust).
      IF lv_lines = sy-tabix.
        CONTINUE.
      ENDIF.
      IF ls_cust-overdue_amount NE 0.
*****************read the customer code & name by customer fincode**************
        READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY
                           zfincode = ls_cust-customer_code BINARY SEARCH.

        IF ls_kna1-name1 IS INITIAL.
          ls_kna1-name1 = ls_cust-customer_name.
        ENDIF.

        APPEND VALUE #( channel_manager    = ls_kna1-ch_pernr_name
                        customer_code      = ls_kna1-kunnr
                        customer_name      = ls_kna1-name1
                        limit_type         = ls_cust-limit_type
                        sanctioned_amount  = ls_cust-sanctioned_amount
                        principal          = ls_cust-principal
                        available_limit    = ls_cust-available_limit
                        max_dpd            = ls_cust-max_dpd
                        overdue_amount     = ls_cust-overdue_amount ) TO gt_custmail.
        CLEAR : ls_kna1 , ls_cust.
      ENDIF.

    ENDLOOP.

*****************UPCOMING OVERDUE ( pending Days to expire ) process****************

    LOOP AT gt_contract INTO DATA(ls_cont).

*      IF ls_cont-overdue_amount = 0 AND ls_cont-days_exp LT 8.
      IF ls_cont-days_exp GE 1 AND ls_cont-days_exp LE 7.

*****************read the customer code & name by customer fincode**************
        CLEAR : ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY
                           zfincode = ls_cont-customer_code BINARY SEARCH.

        IF ls_kna1-name1 IS INITIAL.
          ls_kna1-name1 = ls_cont-customer_name.
        ENDIF.

        APPEND VALUE #( channel_manager   = ls_kna1-ch_pernr_name
                        customer_code     = ls_kna1-kunnr
                        customer_name     = ls_kna1-name1
                        limit_type        = ls_cont-limit_type
                        disbursed_amount  = ls_cont-disbursed_amount
                        principal         = ls_cont-principal
                        created_date      = ls_cont-created_date
                        expiry_date       = ls_cont-expiry_date
                        days_exp          = ls_cont-days_exp ) TO gt_contmail.
        CLEAR : ls_kna1 , ls_cont.
      ENDIF.

    ENDLOOP.

****************sort the internal table by channel manager.
    SORT : gt_custmail BY channel_manager.
    SORT : gt_contmail BY channel_manager.
********************finally add the grandtotal in customer list***********
    IF gt_customer IS NOT INITIAL.
      CLEAR ls_cust.
      READ TABLE gt_customer INTO ls_cust INDEX lv_lines.
      APPEND VALUE #( max_dpd            = ls_cust-max_dpd
                      overdue_amount     = ls_cust-overdue_amount ) TO gt_custmail.
    ENDIF.
****************call method to send the mail*****************
    CALL METHOD send_mail.

  ENDMETHOD.

  METHOD send_mail.

    DATA : lv_filename        TYPE string,
           lv_amount          TYPE char20,
           lv_totalinv        TYPE wrbtr,
           lv_channel_manager TYPE name1,
           lv_channel_partner TYPE char30,
           lv_date            TYPE char20.

************************convert the excel file into xstring******************8
*    lv_filename = p_fname.
*
*    TRY.
*        DATA(lv_xstring)  = cl_openxml_helper=>load_local_file( lv_filename ).
*      CATCH cx_openxml_not_found.
*    ENDTRY.

    TRY.
**********************get the email address***************
        SELECT * FROM zsd_automail_cfa INTO TABLE @DATA(lt_mail).
        IF sy-subrc NE 0.
          MESSAGE : 'Email address not maintained in - ZSD_AUTOMAIL_CFA' TYPE 'E'.
        ENDIF.
        "Create send request
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

        "Create mail body
        DATA(lt_body) = VALUE bcsy_text(
                          ( line = '<h2 style="margin:0; padding:0"><font size="3" >Dear Sir,</h2>' )
                        ).
****************************overdue (DPD) Table design*********
        IF gt_custmail IS NOT INITIAL.
          DATA : ls_body LIKE LINE OF lt_body.
********header
          ls_body-line = '<h3><font size="4" >Please find the overdue report for today;</h3>'.
          APPEND ls_body TO lt_body.
* table display
          ls_body-line = '<table style="MARGIN: 10px" bordercolor="NavajoWhite" '.
          APPEND ls_body TO lt_body.
          ls_body-line = 'cellspacing="0" cellpadding="1" width="1000"'.
          APPEND ls_body TO lt_body.
          ls_body-line = 'border="1"><tbody><tr>'.
          APPEND ls_body TO lt_body.
* table header
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Channel Manager</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Customer Code</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Customer Name</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Limit Type</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Sanctioned Amount</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Principal O/S</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Available Limit</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Max DPD</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Overdue Amount</font></th></tr>'.
          APPEND ls_body TO lt_body.
* table Contents
**********************Get the total number of records - for highlight last record*************
          IF gt_custmail IS NOT INITIAL.
            DATA(lv_lines) = lines( gt_custmail ).
          ENDIF.

          DATA : lv_check TYPE sy-tabix.

          LOOP AT gt_custmail INTO DATA(ls_cust).

            lv_check = sy-tabix.
            ls_body-line = '<tr>'.
            APPEND ls_body TO lt_body.

            IF lv_lines = lv_check.
              CLEAR : ls_cust-channel_manager,ls_cust-customer_code,ls_cust-customer_name,
                      ls_cust-limit_type,ls_cust-sanctioned_amount,ls_cust-principal,ls_cust-available_limit.
            ENDIF.

            CONCATENATE '<td><center>' ls_cust-channel_manager '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_cust-customer_code '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_cust-customer_name '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_cust-limit_type '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td align="right">' ls_cust-sanctioned_amount '</td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td align="right">' ls_cust-principal '</td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td align="right">' ls_cust-available_limit '</td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.

*****************highlight the grandtotal atlast red - colour****************
            IF lv_lines = lv_check.
              CONCATENATE '<td><center><font color="RED">' ls_cust-max_dpd '</center></td>' INTO ls_body-line.
              APPEND ls_body TO lt_body.
              CONCATENATE '<td align="right"><font color="RED">' ls_cust-overdue_amount '</td></tr>' INTO ls_body-line.
              APPEND ls_body TO lt_body.
              CONTINUE.
            ENDIF.

            CONCATENATE '<td><center>' ls_cust-max_dpd '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td align="right">' ls_cust-overdue_amount '</td></tr>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CLEAR : ls_cust.

          ENDLOOP.
* table close
          ls_body-line = '</tbody> </table>'.
          APPEND ls_body TO lt_body.
        ENDIF.


        IF gt_contmail IS NOT INITIAL.
****************************UPCOMING OVERDUE ( pending Days to expire ) Table design*********
* header<font color="gray">
          ls_body-line = '<h3><font size="4" >The below invoice will be expiring within the mentioned days and the same are locked for billing;</h3>'.
          APPEND ls_body TO lt_body.
* table display
          ls_body-line = '<table style="MARGIN: 10px" bordercolor="NavajoWhite" '.
          APPEND ls_body TO lt_body.
          ls_body-line = 'cellspacing="0" cellpadding="1" width="1000"'.
          APPEND ls_body TO lt_body.
          ls_body-line = 'border="1"><tbody><tr>'.
          APPEND ls_body TO lt_body.
* table header
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Channel Manager</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Customer Code</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Customer Name</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Limit Type</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Disbursed Amount</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Principal O/S</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Contract Creation Date</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Contract Expiry Date</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Days to Expire</font></th></tr>'.
          APPEND ls_body TO lt_body.
* table Contents
          LOOP AT gt_contmail INTO DATA(ls_cont).

            ls_body-line = '<tr>'.
            APPEND ls_body TO lt_body.

            CONCATENATE '<td><center>' ls_cont-channel_manager '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_cont-customer_code '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_cont-customer_name '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_cont-limit_type '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td align="right">' ls_cont-disbursed_amount '</td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td align="right">' ls_cont-principal '</td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_cont-created_date '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_cont-expiry_date '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center><font color="RED">' ls_cont-days_exp '</center></td></tr>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CLEAR : ls_cont.
          ENDLOOP.
* table close
          ls_body-line = '</tbody> </table>'.
          APPEND ls_body TO lt_body.
        ENDIF.

********************get the 14 status invoices from finance customer table**************

        SELECT a~invoicekey,a~status,a~fintype,a~custno,a~custname,a~invoiceno,
               a~invoicedate,a~dueamount,b~ch_pernr_name
               FROM zsd_sf_cust_inv AS a
               INNER JOIN zcust_blk_chk AS b
               ON a~custno = b~kunnr
               INTO TABLE @DATA(lt_fininv)
               WHERE a~status = '14'.
        IF lt_fininv IS NOT INITIAL.

* header
          ls_body-line = '<h3><font size="4" >The below list refers to the invoices, which are still not accepted by the Distributors and the payment from <br>Sundaram Finance & Rupifi to SPL is on hold.</h3>'.
          APPEND ls_body TO lt_body.
* table display
          ls_body-line = '<table style="MARGIN: 10px" bordercolor="NavajoWhite" '.
          APPEND ls_body TO lt_body.
          ls_body-line = 'cellspacing="0" cellpadding="1" width="1000"'.
          APPEND ls_body TO lt_body.
          ls_body-line = 'border="1"><tbody><tr>'.
          APPEND ls_body TO lt_body.
* table header
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Channel Manager</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Status</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Party Code</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Counterparty Name</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Financing Requested<br>(SF TO SPL)</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Inv Date</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Inv no</font></th>'.
          APPEND ls_body TO lt_body.
          ls_body-line = '<th style="background-color:lightgray;"><font color="Black">Channel Partner</font></th>'.
          APPEND ls_body TO lt_body.
* table Contents

          SORT : lt_fininv BY ch_pernr_name custno.

          LOOP AT lt_fininv INTO DATA(ls_inv).

***************calculate the total inv amount**************
            lv_totalinv = lv_totalinv + ls_inv-dueamount.

            CLEAR : lv_amount.
            lv_amount = ls_inv-dueamount.
*******************type of finance partner************
            CLEAR : lv_channel_partner.
            IF ls_inv-fintype = 'SF'.
              lv_channel_partner = 'Sundaram Finance'.
            ELSEIF ls_inv-fintype = 'RF'.
              lv_channel_partner = 'Rupifi'.
            ENDIF.
******************get the date format**********
            CLEAR : lv_date.
            WRITE ls_inv-invoicedate TO lv_date.

            ls_body-line = '<tr>'.
            APPEND ls_body TO lt_body.

            CONCATENATE '<td><center>' ls_inv-ch_pernr_name '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            ls_body = '<td><center>GRN Pending - SKU End</center></td>'.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_inv-custno'</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_inv-custname '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td align="right">' lv_amount '</td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' lv_date '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' ls_inv-invoiceno '</center></td>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CONCATENATE '<td><center>' lv_channel_partner '</center></td></tr>' INTO ls_body-line.
            APPEND ls_body TO lt_body.
            CLEAR : ls_inv.

          ENDLOOP.
********************for inv grandtotal****************
          CLEAR : lv_amount.
          lv_amount = lv_totalinv.

          ls_body-line = '<tr>'.
          APPEND ls_body TO lt_body.

          CONCATENATE '<td><center>' '<font color= "RED">' 'Total Amount' '</center></td>' INTO ls_body-line.
          APPEND ls_body TO lt_body.
          ls_body = '<td><center> </center></td>'.
          APPEND ls_body TO lt_body.
          CONCATENATE '<td><center>' '</center></td>' INTO ls_body-line.
          APPEND ls_body TO lt_body.
          CONCATENATE '<td><center>'  '</center></td>' INTO ls_body-line.
          APPEND ls_body TO lt_body.
          CONCATENATE '<td align="right"><font color="RED">' lv_amount '</td>' INTO ls_body-line.
          APPEND ls_body TO lt_body.
          CONCATENATE '<td><center>' '</center></td>' INTO ls_body-line.
          APPEND ls_body TO lt_body.
          CONCATENATE '<td><center>' '</center></td>' INTO ls_body-line.
          APPEND ls_body TO lt_body.
          CONCATENATE '<td><center>'  '</center></td></tr>' INTO ls_body-line.
          APPEND ls_body TO lt_body.
          CLEAR : ls_inv.
* table close
          ls_body-line = '</tbody> </table>'.
          APPEND ls_body TO lt_body.
        ENDIF.
        ls_body-line = '<h3><font size="4" >Note : Please let me know if there are any invoices, which have been adjusted internally and also,<br>'.
        ls_body-line = |{ ls_body-line }<mark>Please ignore this in case the materials are not delivered to the respective distributors yet.</mark></h3>|.
        APPEND ls_body TO lt_body.
**************end signature process************
        ls_body-line = '<P><b>Thanks & regards</b> <br><b>S. Ramshid Mon <br><b>Ph: 9445864596</P>'.
        APPEND ls_body TO lt_body.
*************change date format************
        WRITE : sy-datum TO lv_date.
        "Set up document object
        DATA(lo_document) = cl_document_bcs=>create_document(
                              i_type = 'HTM'
                              i_text = lt_body
                              i_subject = |SF OVERDUE REPORT - { lv_date }| ).

*        "Add attachment
*        lo_document->add_attachment(
*            i_attachment_type    = 'xls'
*            i_attachment_size    = CONV #( xstrlen( lv_xstring ) )
*            i_attachment_subject = |Re : Auto mail For CFA Overdue As On { lv_date } |
*            i_attachment_header  = VALUE #( ( line = |Customer_Contract_Dtls_Auto_Mail - { lv_date }.xlsx' | ) )
*            i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( lv_xstring ) ).

        "Add document to send request
        lo_document->set_importance( i_importance = '1' ).
        lo_send_request->set_document( lo_document ).

***********************add the mail recipient*************
        LOOP AT lt_mail INTO DATA(ls_mail).

          IF ls_mail-com_type = 'TO'.     "To recipient

            lo_send_request->add_recipient(
                       EXPORTING
                         i_recipient  = cl_cam_address_bcs=>create_internet_address(
                                          i_address_string = CONV #( ls_mail-email )
                                          i_address_name   = CONV #( ls_mail-name )
                                        )
                         i_express    = abap_true ).

          ELSEIF ls_mail-com_type = 'CC'.  "CC recipient

            lo_send_request->add_recipient(
                       EXPORTING
                         i_recipient  = cl_cam_address_bcs=>create_internet_address(
                                          i_address_string = CONV #( ls_mail-email )
                                          i_address_name   = CONV #( ls_mail-name )
                                        )
                         i_express    = abap_true
                         i_copy = abap_true ).

          ELSEIF ls_mail-com_type = 'FR'.  "From recipient
            "Set sender
            lo_send_request->set_sender(
              cl_cam_address_bcs=>create_internet_address(
                i_address_string = CONV #( ls_mail-email )
                i_address_name = CONV #( ls_mail-name )
              )
            ).
          ENDIF.

          CLEAR : ls_mail.

        ENDLOOP.

        "Send Email
        lo_send_request->set_priority( i_priority = '1' ).
        DATA(lv_sent_to_all) = lo_send_request->send( ).
        COMMIT WORK.

      CATCH cx_send_req_bcs INTO DATA(lx_req_bsc).
        "Error handling
      CATCH cx_document_bcs INTO DATA(lx_doc_bcs).
        "Error handling
      CATCH cx_address_bcs  INTO DATA(lx_add_bcs).
        "Error handling
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


INITIALIZATION.

  DATA : lobj_cust_bal TYPE REF TO lcl_cust_bal.
  CREATE OBJECT lobj_cust_bal.

******************call email maintance screen******************
AT SELECTION-SCREEN.

  IF sy-ucomm = 'MAINT'.
    CALL TRANSACTION 'ZSF_EMAIL'.
  ENDIF.

****************send the automail************
  IF sy-ucomm = 'SEND'.
    IF p_fname IS NOT INITIAL.
      lobj_cust_bal->read_file( ).
      MESSAGE : 'Auto Mail Send Successfully' TYPE 'S'.
    ELSE.
      MESSAGE : 'Please select the filepath' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

  "f4 functionality to file path

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.


  DATA: lv_rc TYPE i.
  DATA: lt_file_table TYPE filetable,
        ls_file_table TYPE file_table.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title = 'Select a file'
    CHANGING
      file_table   = lt_file_table
      rc           = lv_rc.

  IF lt_file_table IS NOT INITIAL.
    READ TABLE lt_file_table INTO ls_file_table INDEX 1.
    p_fname = ls_file_table-filename.
  ENDIF.
