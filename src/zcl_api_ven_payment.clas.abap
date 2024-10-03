class ZCL_API_VEN_PAYMENT definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_PAYMENT IMPLEMENTATION.


  method if_http_extension~handle_request.
    data: date_frm type string,
          date_to  type string,
          vendor   type string.
    data: lv_path_info      type string,
          lv_request_method type string,
          lv_response_data  type string.
    data: it_inputparams type tihttpnvp,
          ls_inputparams type line of tihttpnvp.
    types : begin of t_body,
              Accitem type string,
            end of t_body.
    data: it_acc_body type standard table of t_body,
          wa_acc_body type t_body.

    constants : c_comma                type c value ',',
                c_curly_brackets_open  type char01 value '{',
                c_curly_brackets_close type char01 value '}',
                c_quatation_mark       type char01 value '"',
                c_colon                type char01 value':'.
    data :ls_json    type string,
          v_jsonload type string.
    data:so_lifnr type lfa1-lifnr,
         so_bukrs type lfb1-bukrs.
    types: begin of tem_lfa1,
             lifnr type lfa1-lifnr,
             name1 type lfa1-name1,
             adrnr type lfa1-adrnr,
           end of tem_lfa1.
    data: z_lfa1 type table of tem_lfa1,
          y_lfa1 type tem_lfa1.

    types: begin of tem_lfb1,
             lifnr type lfb1-lifnr,
             bukrs type lfb1-bukrs,
           end of tem_lfb1.
    types: begin of gs_bsak,
             prctr type bsak-prctr,                  " Profit Center
             lifnr type bsak-lifnr,                  " Customer Number
             augbl type bsak-augbl,
             budat type bsak-budat,                  " Posting Date in the Document
             bldat type bsak-bldat,
             blart type bsak-blart,
             zfbdt type bsak-zfbdt,                  " Baseline Date for Due Date Calculation
             dmbtr type bsak-dmbtr,                  " Amount in Local Currency
             shkzg type bsak-shkzg,                  " Debit/Credit Indicator
             xblnr type bsak-xblnr,                  " Reference Document Number
             belnr type bsak-belnr,                  " Bill Number
             gsber type bsak-gsber,                  " Business Area
             sgtxt type bsak-sgtxt,                  " Text OR Remarks
             ebeln type bsak-ebeln,                  " Billing Doc.No
             umskz type bsak-umskz,                  " Spl.Gl.Ind
             bukrs type bsak-bukrs,
             gjahr type bsak-gjahr,
             sknto type bsak-sknto,
             zterm type bsak-zterm,
             zbd1t type bsak-zbd1t,
             umsks type bsak-umsks,
             augdt type bsak-augdt,
             zuonr type bsak-zuonr,
             buzei type bsak-buzei,
           end of gs_bsak.
    types: begin of gs_final,
             lifnr    type bsak-lifnr,                  " Customer Number
             umskz    type bsik-umskz,                  " Spl.GL Ind
             belnr    type bsak-belnr,                  " Bill Number
             ebeln    type bsak-ebeln,                  " Billing Document Number
             xblnr    type bsik-xblnr,                  " Reference Document Number
             bldat    type bsak-bldat,                  " Posting Date in the Document
             zfbdt    type bsak-zfbdt,
             cr_amt   type String,                 " Cr Amount
             dr_amt   type string,                 " Dr Amount
             budat    type bsik-budat,                 " Posting Date
             name1    type lfa1-name1,                 " Customer Name
             blart    type bsik-blart,                  " Bill Doc. Type
             bschl    type bsid-bschl,
             sgtxt    type bsik-sgtxt,                  " Text OR Remarks
             gsber    type bsik-gsber,                  " Business Area
             shkzg    type bsik-shkzg,                  " Dbit/Crdit ind.
             bal_amt  type bsak-dmbtr,                " Balance Amount
             ltext    type t003t-ltext,                  " Bill Doc .Type Descrption
             bukrs    type bsik-bukrs,
             gjahr    type bsik-gjahr,
             sknto    type bsak-sknto,
             lv_count type int1,
             sta      type int1,
             t_tttext type char20,
             zterm    type bsak-zterm,
             zbd1t    type string,
           end of gs_final.
    data: z_lfb1   type table of tem_lfb1,
          y_lfb1   type tem_lfb1,
          lv_buk   type t001-bukrs,
          gt_bsak  type table of gs_bsak,
          wa_bsak  type gs_bsak,
          gt_final type table of gs_final,
          wa_final type gs_final.

* get the request attributes
    lv_path_info = server->request->get_header_field( name = '~path_info' ).
    shift lv_path_info left by 1 places.

    field-symbols <fs_param> type line of tihttpnvp.

*Get GET parameters
    call method server->request->get_form_fields
      changing
        fields = it_inputparams.
    unassign <fs_param>.
    loop at it_inputparams assigning <fs_param>.
      translate <fs_param>-name to upper case.
    endloop.

    clear: date_frm,ls_inputparams.

    read table it_inputparams into ls_inputparams with key name = 'DATE_FRM'.
    date_frm = ls_inputparams-value.
    clear ls_inputparams.
    read table it_inputparams into ls_inputparams with key name = 'DATE_TO'.
    date_to = ls_inputparams-value.
    clear ls_inputparams.
    read table it_inputparams into ls_inputparams with key name = 'VENDOR'.
    vendor = ls_inputparams-value.
    clear ls_inputparams.

    so_bukrs = 1000 .
    so_lifnr = vendor.
    if so_lifnr is not initial .
      select lifnr bukrs from lfb1 into table z_lfb1 where bukrs eq so_bukrs and loevm <> 'X' and lifnr = so_lifnr .
    else.
      select lifnr bukrs from lfb1 into table z_lfb1 where bukrs eq so_bukrs and loevm <> 'X'.
    endif.

    if  z_lfb1 is not initial.
      sort z_lfb1 by lifnr.
      clear: lv_buk.
      loop at z_lfb1 into y_lfb1.
        select bukrs  from t001 into lv_buk where bukrs eq y_lfb1-bukrs.
          clear: gt_bsak.
          select
             prctr
             lifnr
             umskz
             augbl
             budat
             bldat
             blart
             zfbdt
             dmbtr
             shkzg
             xblnr
             belnr
             gsber
             sgtxt
             ebeln
             bukrs
             gjahr
             sknto
             zterm
             zbd1t
             umsks
             augdt
             zuonr
             buzei
             from bsak into corresponding fields of table gt_bsak where budat ge date_frm and budat le date_to and bukrs eq y_lfb1-bukrs
                                                            and lifnr eq y_lfb1-lifnr and umskz <> 'H'  and umskz <> 'F' .
        endselect.
        if gt_bsak[] is not initial.
          sort gt_bsak ascending by belnr lifnr xblnr.
          loop at gt_bsak into wa_bsak.
            wa_final-belnr = wa_bsak-belnr.
            wa_final-xblnr = wa_bsak-xblnr.
            wa_final-lifnr = wa_bsak-lifnr.
            APPEND wa_final to gt_final.
          endloop.
          clear : wa_bsak.
        endif.
        if gt_final is not initial.
          loop at gt_final into wa_final.
            concatenate
            '{'
            '"payment_doc_no":'      c_quatation_mark wa_final-belnr c_quatation_mark c_comma
            '"invoice_no" :'    c_quatation_mark wa_final-xblnr c_quatation_mark c_comma
            '"vendor_code":'      c_quatation_mark  wa_final-lifnr c_quatation_mark "c_comma
            '}'
            into wa_acc_body-accitem.
            append wa_acc_body to it_acc_body.
            clear : wa_acc_body.
          endloop.
        endif.
        data: lv_tabix  type sy-tabix,
              lv_int(6) type n.
        lv_int = 0.
        lv_int = lines( it_acc_body ).

        loop at it_acc_body into wa_acc_body.
          lv_tabix = sy-tabix.
          if lv_tabix < lv_int.
            concatenate ls_json wa_acc_body-accitem ',' into ls_json.
          else.
            concatenate ls_json wa_acc_body-accitem into ls_json.
          endif.
        endloop.
        clear: lv_int,lv_tabix.

        concatenate '[' ls_json ']'  into v_jsonload.

*Set JSON Content-Type
        call method server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        call method server->response->set_cdata( data = v_jsonload ).
      endloop.
    endif.

  endmethod.
ENDCLASS.
