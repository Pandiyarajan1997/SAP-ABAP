class ZCL_API_EMPVENDOR_INVOICES definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_EMPVENDOR_INVOICES IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 16.04.2023
*&Purpose : For creating invoices for employee vendor for travels allowance and F&F deposits
*&Reference: Ramakrishnan J
*-------------------------------------------------------------------------------------
*Change by Ramakrishnan. Get the Position from Infotype 0001 and the costcenter for that position

    TYPES: BEGIN OF input,
             employee_no TYPE pernr_d,
             amount      TYPE wrbtr,
             doc_date    TYPE bldat,
             post_date   TYPE budat,
             overallkms  TYPE string,
             pymttype    TYPE char01,
           END OF input.

    DATA: gt_input TYPE TABLE OF input.

    TYPES: BEGIN OF output,
             employee_no TYPE pernr_d,
             docno       TYPE belnr_d,
             type        TYPE bapi_mtype,
             msg         TYPE string,
           END OF output.
    DATA: gt_output TYPE TABLE OF output.

    TYPES: BEGIN OF costcenter,
             kostl TYPE kostl,
             kokrs TYPE kokrs,
           END OF costcenter.
    DATA: ls_costcenter TYPE costcenter.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA: lv_msg TYPE string.

    DATA: lt_payable   TYPE TABLE OF bapiacap09,
          lt_accountgl TYPE TABLE OF bapiacgl09,
          lt_currency  TYPE TABLE OF bapiaccr09.
    DATA: return TYPE bapiret2_t.

    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1.
    DATA: lv_fund TYPE char10.

    DATA: lv_objtyp TYPE bapiache09-obj_type.
    DATA: lv_objkey TYPE bapiache09-obj_key.
    DATA: lv_objsys TYPE bapiache09-obj_sys.

    DATA: lv_pern(8) TYPE c.

    DATA: lo_logupd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_logupd.
    "Input Of the API from Request Parameters
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
      REFRESH: gt_output.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).

        IF <fs_input>-employee_no IS INITIAL.
          APPEND VALUE #( employee_no = <fs_input>-employee_no type = 'E' msg = |Employee Number Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF <fs_input>-amount IS INITIAL.
          APPEND VALUE #( employee_no = <fs_input>-employee_no type = 'E' msg = |Amount Missing in input| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF <fs_input>-doc_date IS INITIAL.
          APPEND VALUE #( employee_no = <fs_input>-employee_no type = 'E' msg = |Doc_date Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF <fs_input>-post_date IS INITIAL.
          APPEND VALUE #( employee_no = <fs_input>-employee_no type = 'E' msg = |Post_date Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF <fs_input>-pymttype IS INITIAL.
          APPEND VALUE #( employee_no = <fs_input>-employee_no type = 'E' msg = |Payment type Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF not ( <fs_input>-pymttype = 'T' or <fs_input>-pymttype = 'I' ).
          APPEND VALUE #( employee_no = <fs_input>-employee_no type = 'E' msg = |Incorrect Payment type| ) TO gt_output.
          CONTINUE.
        ENDIF.


        SELECT SINGLE pernr, bukrs, plans, ename FROM pa0001 INTO @DATA(ls_pernr)
          WHERE pernr = @<fs_input>-employee_no AND begda LE @sy-datum AND endda GE @sy-datum.
        IF sy-subrc <> 0.
          APPEND VALUE #( employee_no = <fs_input>-employee_no type = 'E' msg = |Incorrect Employee Number| ) TO gt_output.
          CONTINUE.
        ELSE.
          IF ls_pernr-plans = '99999999'.
            APPEND VALUE #( employee_no = <fs_input>-employee_no type = 'E' msg = |Employee Already Exit| ) TO gt_output.
            CONTINUE.
          ENDIF.
        ENDIF.

*for Posting payment block
        SELECT SINGLE * FROM zta_emp_pymtblk INTO @DATA(l_pernr_blk)
          WHERE pernr = @<fs_input>-employee_no.
        IF sy-subrc EQ 0.
          IF l_pernr_blk-flag EQ abap_true.
            DATA(lv_pymtblk) = CONV dzlspr( 'A' ).
          ENDIF.
        ENDIF.

* get the P to CP relationship
        SELECT SINGLE objid, sobid FROM hrp1001 INTO @DATA(l_hrp1001_cp)
           WHERE otype = 'P' AND objid = @ls_pernr-pernr AND plvar = '01' AND rsign = 'A'
             AND relat = '209' AND istat = '1' AND begda LE @sy-datum AND endda GE @sy-datum AND sclas = 'CP'.
        IF sy-subrc = 0.
          SELECT SINGLE objid, sobid FROM hrp1001 INTO @DATA(l_hrp1001_bp)
             WHERE otype = 'CP' AND objid = @l_hrp1001_cp-sobid AND plvar = '01' AND rsign = 'B'
               AND relat = '207' AND istat = '1' AND begda LE @sy-datum AND endda GE @sy-datum AND sclas = 'BP'.
          IF sy-subrc = 0.
            DATA(l_vendor_bp) = CONV bapibus1006_head-bpartner( l_hrp1001_bp-sobid ).
            IF l_vendor_bp IS NOT INITIAL.
              DATA(lv_vendor) = l_vendor_bp.
              SHIFT lv_vendor LEFT DELETING LEADING '0'.
              SELECT SINGLE name1 FROM lfa1 INTO @DATA(lv_venname) WHERE lifnr = @l_vendor_bp.
              IF sy-subrc EQ 0.
                CONDENSE lv_venname.
              ENDIF.
              IF <fs_input>-pymttype = 'T'.
                "G/L Account Fetching Variables in TVARVC
                SELECT SINGLE low FROM tvarvc
                       INTO @DATA(l_glaccount)
                       WHERE name = 'TRA_GLACCOUNT'
                       AND type = 'P'.
                DATA(l_txt)       = CONV sgtxt( |TA-{ <fs_input>-overallkms }Kms-{ lv_vendor }-{ lv_venname } | ). "Item Text
                DATA(lv_refrence) = |TA-{ <fs_input>-doc_date }|. "Reference Document Number
                DATA(lv_invtyp) = <fs_input>-pymttype.

              ELSEIF <fs_input>-pymttype = 'I'.
                "G/L Account Fetching Variables in TVARVC
                SELECT SINGLE low FROM tvarvc
                       INTO @l_glaccount
                       WHERE name = 'INC_GLACCOUNT'
                       AND type = 'P'.

                lv_pern =  ls_pernr-pernr.
                SHIFT lv_pern LEFT DELETING LEADING '0'.
*                  l_txt       = |Incentives on { <fs_input>-post_date } |. "Item Text
                l_txt      = |{ lv_pern }-{ ls_pernr-ename }|. "Item text
                lv_refrence = |INC-{ <fs_input>-doc_date }|. "Reference Document Number
                lv_invtyp   = <fs_input>-pymttype.
              ENDIF.
*** FUNCTION module to GET fiscal year ***
              CLEAR: lv_fisyear,lv_month,l_return.
              CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
                EXPORTING
                  companycodeid = ls_pernr-bukrs
                  posting_date  = <fs_input>-post_date
                IMPORTING
                  fiscal_year   = lv_fisyear
                  fiscal_period = lv_month
                  return        = l_return.
**Added by Samsudeen M on 08.06.2023
              SELECT SINGLE * FROM zfi_travel_log INTO @DATA(l_duplicate_chk)
                WHERE bukrs = @ls_pernr-bukrs
                AND emp_vendor = @l_vendor_bp
                AND gjahr = @lv_fisyear
                AND xblnr = @lv_refrence
                AND type NE 'E'.
              IF sy-subrc EQ 0.
                APPEND VALUE #( employee_no = ls_pernr-pernr
                                type        = 'E'
                                msg         = |Reference { lv_refrence } already available| ) TO gt_output.
                CONTINUE.
              ENDIF.
              "Already Reference Checks
              SELECT SINGLE * FROM bsip INTO @DATA(l_bsik) WHERE bukrs = @ls_pernr-bukrs
                                                           AND lifnr = @l_vendor_bp
                                                           AND gjahr = @lv_fisyear
                                                           AND xblnr = @lv_refrence.
              IF sy-subrc = 0.
                APPEND VALUE #( employee_no = ls_pernr-pernr
                                type        = 'E'
                                msg         = |Reference { lv_refrence } already available| ) TO gt_output.
                CONTINUE.
              ELSE.
* Get the cost center of the Position directly based on infotype 0001 position
                SELECT SINGLE sobid FROM hrp1001
                                INTO @DATA(l_costcenter)
                                WHERE otype = 'S'
                                AND objid = @ls_pernr-plans  "@ls_hrp1001-objid
                                AND plvar = '01'
                                AND rsign = 'A'
                                AND relat = '011'
                                AND sclas = 'K'
                                AND begda LE @sy-datum
                                AND endda GE @sy-datum.
                IF sy-subrc = 0.
                  CLEAR ls_costcenter.
                  ls_costcenter =  l_costcenter.

                  REFRESH: lt_payable,lt_accountgl,lt_currency.
*** Actual Process starts Here for Posting Invoice ****
                  DATA(ls_header) = VALUE bapiache09( bus_act = 'RFBU'
                                                      username = sy-uname
                                                      comp_code = ls_pernr-bukrs
                                                      doc_date = <fs_input>-doc_date
                                                      pstng_date = <fs_input>-post_date
                                                      fisc_year = lv_fisyear
                                                      doc_type = 'KR'
                                                      ref_doc_no = lv_refrence ).
**** Vendor Line items to be passed ***
                  lt_payable = VALUE #(
                               ( itemno_acc = '1'
                                 vendor_no = l_vendor_bp
                                 bline_date = <fs_input>-doc_date
                                 item_text  = l_txt
                                 pmnt_block = lv_pymtblk ) ).

                  APPEND VALUE #( itemno_acc = '1'
                                  currency = 'INR'
                                  amt_doccur = <fs_input>-amount * -1
                                  amt_base = <fs_input>-amount * -1 ) TO lt_currency."Vendor Line item

                  CLEAR lv_fund.
                  CALL FUNCTION 'ZFUND_GET'
                    EXPORTING
                      date       = <fs_input>-post_date
                    IMPORTING
                      lv_fund    = lv_fund
                    EXCEPTIONS
                      enter_date = 1
                      OTHERS     = 2.
                  IF sy-subrc <> 0.

                  ENDIF.
                  DATA(l_account) = CONV hkont( |{ l_glaccount ALPHA = IN }| ).
                  APPEND VALUE #( itemno_acc = '2'
                                  gl_account = l_account
                                  fund = lv_fund
                                  item_text = l_txt
                                  costcenter = ls_costcenter-kostl ) TO lt_accountgl.
                  APPEND VALUE #( itemno_acc = '2'
                                  currency = 'INR'
                                  amt_doccur = <fs_input>-amount
                                  amt_base =  <fs_input>-amount
                                ) TO lt_currency . "G/L line Items
                  REFRESH: return.
*** Document Check Before posting ***
                  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
                    EXPORTING
                      documentheader = ls_header
                    TABLES
                      accountgl      = lt_accountgl
                      accountpayable = lt_payable
                      currencyamount = lt_currency
                      return         = return.
                  CLEAR lv_msg.
                  LOOP AT return INTO DATA(l_ret) WHERE type = 'E' OR type = 'A'.
                    lv_msg = |{ lv_msg } { l_ret-message }|.
                  ENDLOOP.
                  IF lv_msg IS NOT INITIAL.
                    APPEND VALUE #( employee_no = <fs_input>-employee_no
                                    type        = 'E'
                                    msg         = lv_msg ) TO gt_output.
                    "error updation in log table
                    DATA(l_update) = VALUE zfi_travel_log(  mandt        = sy-mandt
                                                            employee_no  = <fs_input>-employee_no
                                                            emp_vendor   = l_vendor_bp
                                                            xblnr        = lv_refrence
                                                            amount       = <fs_input>-amount
                                                            bldat        = <fs_input>-doc_date
                                                            budat        = <fs_input>-post_date
                                                            overall_kms   = <fs_input>-overallkms
                                                            erdat        = sy-datum
                                                            er_time      = sy-uzeit
                                                            type         = 'E'
                                                            invtype      = lv_invtyp
                                                            msg          = lv_msg
                                                            kostl        = ls_costcenter-kostl
                                                            saknr        = l_glaccount ).
                    MODIFY zfi_travel_log FROM l_update.
                  ELSE.
*** Function Module to create Debit note ***
                    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
                      EXPORTING
                        documentheader = ls_header
                      IMPORTING
                        obj_type       = lv_objtyp
                        obj_key        = lv_objkey
                        obj_sys        = lv_objsys
                      TABLES
                        accountgl      = lt_accountgl
                        accountpayable = lt_payable
                        currencyamount = lt_currency
                        return         = return.
                    IF sy-subrc = 0.
                      COMMIT WORK AND WAIT.
                      CLEAR lv_msg.
                      IF lv_pymtblk IS NOT INITIAL.
                        lv_msg = |Document No:{ lv_objkey(10) } Posted Successfully but Blocked for Payment|.
                      ELSE.
                        lv_msg = |Document No:{ lv_objkey(10) } Posted Successfully |.
                      ENDIF.
                      APPEND VALUE #( employee_no = ls_pernr-pernr
                                      docno       = lv_objkey(10)
                                      type        = 'S'
                                      msg         = lv_msg ) TO gt_output.
                      "Success Scenerios updating the log
                      l_update = VALUE zfi_travel_log(  mandt        = sy-mandt
                                                        employee_no  = <fs_input>-employee_no
                                                        emp_vendor   = l_vendor_bp
                                                        xblnr        = lv_refrence
                                                        amount       = <fs_input>-amount
                                                        bldat        = <fs_input>-doc_date
                                                        budat        = <fs_input>-post_date
                                                        overall_kms   = <fs_input>-overallkms
                                                        bukrs        = ls_pernr-bukrs
                                                        gjahr        = lv_fisyear
                                                        belnr        = lv_objkey(10)
                                                        erdat        = sy-datum
                                                        er_time      = sy-uzeit
                                                        type         = 'S'
                                                        invtype      = lv_invtyp
                                                        msg          = lv_msg
                                                        kostl        = ls_costcenter-kostl
                                                        saknr        = l_glaccount ).
                      MODIFY zfi_travel_log FROM l_update.
                    ENDIF.
                  ENDIF.
                ELSE.
                  CLEAR: lv_msg.
                  lv_msg = |Costcenter not available for Employee|.
                  APPEND VALUE #( employee_no = ls_pernr-pernr
                                  type        = 'E'
                                  msg         = lv_msg ) TO gt_output.
                  "error updation in log table
                  l_update = VALUE zfi_travel_log(  mandt        = sy-mandt
                                                    employee_no  = <fs_input>-employee_no
                                                    emp_vendor   = l_vendor_bp
                                                    xblnr        = lv_refrence
                                                    amount       = <fs_input>-amount
                                                    bldat        = <fs_input>-doc_date
                                                    budat        = <fs_input>-post_date
                                                    overall_kms   = <fs_input>-overallkms
                                                    erdat        = sy-datum
                                                    er_time      = sy-uzeit
                                                    type         = 'E'
                                                    invtype      = lv_invtyp
                                                    msg          = lv_msg
                                                    kostl        = ls_costcenter-kostl
                                                    saknr        = l_glaccount ).
                  MODIFY zfi_travel_log FROM l_update.
                ENDIF.
*                  ENDIF.
              ENDIF.
            ELSE.
              CLEAR: lv_msg.
              lv_msg = |Kindly check Employee Vendor is extended for employee or not|.
              APPEND VALUE #( employee_no = ls_pernr-pernr
                              type        = 'E'
                              msg         = lv_msg ) TO gt_output.
              "error updation in log table
              l_update = VALUE zfi_travel_log(  mandt        = sy-mandt
                                                employee_no  = <fs_input>-employee_no
                                                emp_vendor   = l_vendor_bp
                                                xblnr        = lv_refrence
                                                amount       = <fs_input>-amount
                                                bldat        = <fs_input>-doc_date
                                                budat        = <fs_input>-post_date
                                                overall_kms   = <fs_input>-overallkms
                                                type         = 'E'
                                                invtype      = lv_invtyp
                                                erdat        = sy-datum
                                                er_time      = sy-uzeit
                                                msg          = lv_msg
                                                kostl        = ls_costcenter-kostl
                                                saknr        = l_glaccount ).
              MODIFY zfi_travel_log FROM l_update.
            ENDIF.
          ENDIF.
        ENDIF.
*        ENDIF.
      ENDLOOP.
    ELSE.
      v_jsonload = | { '[' }Error: Kindly check all the input filled or not { ']' } |.
**set json content-type
*      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
**Set Response Data
*      CALL METHOD server->response->set_cdata( data = v_jsonload ).
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
    ENDIF.

    CALL METHOD lo_logupd->log_entry_store
      EXPORTING
        apiname         = 'TRAVEL'
        ijson           = lv_data
        ojson           = v_jsonload
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.

    ENDIF.

*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = v_jsonload ).

  ENDMETHOD.
ENDCLASS.
