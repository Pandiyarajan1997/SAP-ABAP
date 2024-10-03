class ZCL_API_TRAVEL_EXP_POSTING definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  methods ATTACHMENT
    importing
      !FILE type STRING
      !OBJECT_KEY type BAPIACHE09-OBJ_KEY
      !COUNT type I .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_TRAVEL_EXP_POSTING IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*-------------Declaration for travel category----------*
    TYPES : BEGIN OF lv_travel,
              trav_type    TYPE ztrtype,
              tr_amount    TYPE wrbtr,
              text         TYPE sgtxt,
              Attachment_1 TYPE string,
            END OF lv_travel,
            tt_travel TYPE TABLE OF lv_travel.
*-------------Declaration for accomedation category------------*
    TYPES : BEGIN OF lv_acco,
              accom_type   TYPE ztrtype,
              acc_amount   TYPE wrbtr,
              text         TYPE sgtxt,
              Attachment_2 TYPE string,
            END OF lv_acco,
            tt_accom TYPE TABLE OF lv_acco.
*-----------------Declaration for food category---------------*
    TYPES : BEGIN OF lv_food,
              food_type    TYPE ztrtype,
              amount       TYPE wrbtr,
              text         TYPE sgtxt,
              Attachment_3 TYPE string,
            END OF lv_food,
            tt_food TYPE TABLE OF lv_food.

*            -----------------------

    DATA : lt_travel TYPE TABLE OF lv_travel.
    DATA : lt_acco TYPE TABLE OF lv_acco.
    DATA : lt_food TYPE TABLE OF lv_food.

    DATA : ls_travel TYPE lv_travel.
    DATA : ls_acco TYPE lv_acco.
    DATA : ls_food TYPE lv_food.

    DATA : lv_file TYPE Xstring.
    DATA : lv_objkey TYPE sbdst_object_key.
    DATA : lv_new_string TYPE bapiache09-obj_key.
    DATA : lv_count TYPE i.


*____________________________________________*
    TYPES : BEGIN OF lv_input,
              comp_code    TYPE bukrs,
              ref_no       TYPE xblnr,
              booking_type TYPE zbtype,
              vendor       TYPE lifnr,
              emp_code     TYPE persno,
              doc_date     TYPE datum,
              pos_date     TYPE datum,
              travel       LIKE lt_travel,
              acco         LIKE lt_acco,
              food         LIKE lt_food,
            END OF lv_input.
*----------------------------------------------*
    TYPES : BEGIN OF lv_output,
              comp_code   TYPE bukrs,
              ref_no      TYPE xblnr,
              emp_code    TYPE persno,
              doc_no      TYPE belnr_d,
              fiscal_year TYPE gjahr,
              type        TYPE bapi_mtype,
              msg         TYPE string,
            END OF lv_output.
*           -----------------------
    DATA : lt_input TYPE TABLE OF lv_input.
    DATA : lt_output TYPE TABLE OF lv_output.

    DATA : ls_input TYPE lv_input.
    DATA : ls_output TYPE lv_output.

    DATA : lv_header TYPE bapiache09.
    DATA : lv_obtype TYPE bapiache09-obj_type.
    DATA : lv_obkey TYPE bapiache09-obj_key.
    DATA : lv_OBJSYS TYPE bapiache09-obj_sys.
    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year.
    DATA : lv_accountgl TYPE TABLE OF bapiacgl09.
    DATA : lv_accountpayble TYPE TABLE OF bapiacap09.
    DATA : lv_curramount TYPE TABLE OF bapiaccr09.
    DATA : lv_return TYPE TABLE OF bapiret2.

    DATA : lv_body TYPE string.
    DATA : v_jsonload TYPE string.
    DATA : lv_msg TYPE string.
    DATA : lv_data TYPE string.
    DATA : lv_part1 TYPE bukrs.
    DATA : lv_part2 TYPE belnr_d.
    DATA : lv_part3 TYPE gjahr.


    DATA: lo_logupd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_logupd.
    "Input Of the API from Request Parameters
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.


*---------Deserialize the required input---------*

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             =    lv_data                                          " JSON string
        pretty_name      =    /ui2/cl_json=>pretty_mode-camel_case             " Pretty Print property names

      CHANGING
        data             =      ls_input                                       " Data to serialize
    ).
*                  ---------------------------
*    ls_input-pos_date = |{ ls_input-pos_date+0(4) }{ ls_input-pos_date+5(2) }{ ls_input-pos_date+8(2) }|.
*    ls_input-doc_date = |{ ls_input-doc_date+0(4) }{ ls_input-doc_date+5(2) }{ ls_input-doc_date+8(2) }|.
    IF ls_input IS NOT INITIAL.
      REFRESH : lt_output.
      CLEAR : ls_output-msg, lv_msg.

*-----------to check document type is present or not ------------*
      IF ls_input-doc_date IS INITIAL.
        ls_output-emp_code = ls_input-emp_code.
        ls_output-type = 'E'.
        lv_msg = | { lv_msg }, Doc Date Is Missing |.
      ENDIF.



*-------------to check posting type is present or not--------*
      IF ls_input-pos_date IS INITIAL.
        ls_output-emp_code = ls_input-emp_code.
        ls_output-type = 'E'.
        lv_msg = | { lv_msg }, Posting Date Is Missing |.
      ENDIF.




*---------------to check reference number is present or not---------*
      IF ls_input-ref_no IS INITIAL.
        ls_output-emp_code = ls_input-emp_code.
        ls_output-type = 'E'.
        lv_msg = | { lv_msg }, Reference number Is Missing |.

      ELSE.
        SELECT SINGLE comp_code, ref_no, emp_code, doc_no, fiscal_year, type, msg FROM zfi_trvl_exp_log INTO @DATA(ls_log)
          WHERE ref_no = @ls_input-ref_no.

        IF sy-subrc = 0 AND ls_log-doc_no IS NOT INITIAL.

          lv_msg = |{ lv_msg }, 'reference number already exist'|.

        ENDIF.
      ENDIF.

*------------to check employee code is present or not and exists in sap or not-----------*
      IF ls_input-emp_code IS INITIAL.
        ls_output-emp_code = ls_input-emp_code.
        ls_output-type = 'E'.
        lv_msg = | { lv_msg }, Employee Code Is Missing |.

      ELSE.
        DATA(lv_pernr) = |{ ls_input-emp_code ALPHA = IN }|.
        SELECT SINGLE pernr, kostl, ename FROM pa0001 INTO @DATA(ls_pa0001)
          WHERE pernr = @lv_pernr.


        IF sy-subrc <> 0.

          lv_msg = | { lv_msg }, employee number { ls_input-emp_code } is invalid |.

        ENDIF.
      ENDIF.


*------------to check company code is present or not and exists in sap or not-----------*
      IF ls_input-comp_code IS INITIAL.
        ls_output-emp_code = ls_input-emp_code.
        ls_output-type = 'E'.
        lv_msg = | { lv_msg }, Company Code Is Missing |.

      ELSE.

        SELECT SINGLE bukrs FROM t001 INTO @DATA(ls_t001)
          WHERE bukrs = @ls_input-comp_code.


        IF sy-subrc <> 0.
          lv_msg =  | { lv_msg } , Company code { ls_input-comp_code }is invalid |.
        ENDIF.
      ENDIF.

*-------------------to check vendor code is present or not and exists in sap or not-----------*
      IF ls_input-vendor IS INITIAL.
        ls_output-emp_code = ls_input-emp_code.
        ls_output-type = 'E'.
        lv_msg = | { lv_msg }, Vendor number is missing |.

      ELSE.
        DATA(lv_lifnr) = |{ ls_input-vendor ALPHA = IN }|.
        SELECT SINGLE lifnr
          FROM lfa1
          INTO @DATA(ls_lfa1)
          WHERE lifnr = @lv_lifnr.


        IF sy-subrc <> 0.
          lv_msg = | { lv_msg }, vendor code { ls_input-vendor } is invalid |.
        ENDIF.
      ENDIF.

*---------------Gathering all expences into VEND_AMNT ----------*
      IF lv_msg IS INITIAL.

        DATA(iv_cnt) = VALUE i(  ).
        ADD 1 TO iv_cnt.
        DATA(vend_amnt) = VALUE netprice(  ).
*-----------------Populating Vendor Line and currency----------------*

        lv_accountpayble = VALUE #(
            ( itemno_acc         = iv_cnt
            vendor_no          = lv_lifnr
            bline_date         = ls_input-doc_date
                                                     ) ).

        lv_curramount = VALUE #(
               ( itemno_acc      = iv_cnt
                currency        = 'INR'
                amt_doccur      =  vend_amnt  "* -1 + ( 10 / 100 * vend_amnt )
                 ) ) .


*------------checking whether LS_INPUT-TRAVEL has data or not -------*
        IF ls_input-travel IS NOT INITIAL.

          SELECT company_code, booking_type, trvl_type, gl_acnt
            FROM ztb_booking_auto
            INTO TABLE @DATA(lt_ztb)
            FOR ALL ENTRIES IN @ls_input-travel
            WHERE trvl_type = @ls_input-travel-trav_type.

          SELECT bukrs, saknr
            FROM skb1 INTO TABLE @DATA(lt_skb1)
            WHERE bukrs = @ls_input-comp_code.


          LOOP AT ls_input-travel INTO DATA(lss_trav).
            ADD lss_trav-tr_amount TO vend_amnt.
            ADD 1 TO iv_cnt.

            READ TABLE lt_ztb INTO DATA(ls_ztb) WITH KEY
            company_code = ls_input-comp_code
            booking_type = ls_input-booking_type
             trvl_type = lss_trav-trav_type.

            IF sy-subrc <> 0.
              lv_msg = | { lv_msg }, travel type { lss_trav-trav_type } is invalid |.
            ENDIF.

            READ TABLE lt_skb1 INTO DATA(ls_skb1) WITH KEY
            bukrs = ls_input-comp_code
            saknr = ls_ztb-gl_acnt.


            IF sy-subrc <> 0.
              lv_msg = | { lv_msg },  gl account { ls_ztb-gl_acnt } is invalid |.
            ENDIF.
*-----------------Populating Travel GL Line and currency----------------*
            APPEND VALUE #( itemno_acc                = iv_cnt
                           gl_account                = ls_skb1-saknr
                           item_text                 = lss_trav-text
                           costcenter                = ls_pa0001-kostl )  TO lv_accountgl.

            APPEND VALUE #(
               itemno_acc      = iv_cnt
               currency        = 'INR'
               amt_doccur      = lss_trav-tr_amount
                                            ) TO lv_curramount.

          ENDLOOP.
        ENDIF.

*-------------checking whether LS_INPUT-ACCO has data or not--------*
        IF ls_input-acco IS NOT INITIAL.

          SELECT company_code, booking_type, trvl_type, gl_acnt
            FROM ztb_booking_auto
            INTO TABLE @lt_ztb
            FOR ALL ENTRIES IN @ls_input-acco
            WHERE trvl_type = @ls_input-acco-accom_type.



          LOOP AT ls_input-acco INTO DATA(lss_acco).
            ADD lss_acco-acc_amount TO vend_amnt.
            ADD 1 TO iv_cnt.

            CLEAR : ls_ztb.
            READ TABLE lt_ztb INTO ls_ztb WITH KEY
            company_code = ls_input-comp_code
            booking_type = ls_input-booking_type
            trvl_type = lss_acco-accom_type.


            IF sy-subrc <> 0.
              lv_msg = | { lv_msg }, Accomodation type { lss_acco-accom_type } is invalid |.
            ENDIF.

            CLEAR : ls_skb1.
            READ TABLE lt_skb1 INTO ls_skb1 WITH KEY
       bukrs = ls_input-comp_code
       saknr = ls_ztb-gl_acnt.


            IF sy-subrc <> 0.
              lv_msg = | { lv_msg }, gl account { ls_ztb-gl_acnt } is invalid |.
            ENDIF.
*-----------------Populating Accomodation GL Line and currency----------------*
            APPEND VALUE #(
                                  itemno_acc                = iv_cnt
                                  gl_account                = ls_skb1-saknr
                                  item_text                 = lss_acco-text
                                  costcenter                = ls_pa0001-kostl )  TO lv_accountgl.

            APPEND VALUE #(
              itemno_acc      = iv_cnt
              currency        = 'INR'
              amt_doccur      = lss_acco-acc_amount
                                            )  TO lv_curramount.


          ENDLOOP.
        ENDIF.
*--------------checking whether LS_INPUT-FOOD has data or not--------*
        IF ls_input-food IS NOT INITIAL.

          SELECT company_code, booking_type, trvl_type, gl_acnt
            FROM ztb_booking_auto
            INTO TABLE @lt_ztb
            FOR ALL ENTRIES IN @ls_input-food
            WHERE trvl_type = @ls_input-food-food_type.


          LOOP AT ls_input-food INTO DATA(lss_food).
            ADD lss_food-amount TO vend_amnt.
            ADD 1 TO iv_cnt.

            CLEAR : ls_ztb.
            READ TABLE lt_ztb INTO ls_ztb WITH KEY
            company_code = ls_input-comp_code
            booking_type = ls_input-booking_type
            trvl_type = lss_food-food_type.

            IF sy-subrc <> 0.
              lv_msg = | { lv_msg }, food type { lss_food-food_type } is invalid|.
            ENDIF.

            CLEAR : ls_skb1.
            READ TABLE lt_skb1 INTO ls_skb1 WITH KEY
                 bukrs = ls_input-comp_code
                 saknr = ls_ztb-gl_acnt.


            IF sy-subrc <> 0.
              lv_msg = | { lv_msg }, gl account { ls_ztb-gl_acnt } is invalid |.
            ENDIF.


*-----------------Populating Food GL Line and currency----------------*
            APPEND VALUE #(
                                      itemno_acc                = iv_cnt
                                      gl_account                = ls_skb1-saknr
                                      item_text                 = lss_food-text
                                      costcenter                = ls_pa0001-kostl )  TO lv_accountgl.

            APPEND VALUE #(
              itemno_acc      = iv_cnt
              currency        = 'INR'
              amt_doccur      = lss_food-amount
                                            ) TO lv_curramount.


          ENDLOOP.
        ENDIF.

*--------------------Applying TDS tax logic------------------------------*
        IF ls_input-booking_type = 'VE' AND vend_amnt GE '30000'.

          SELECT SINGLE company_code, booking_type, trvl_type, gl_acnt
            FROM ztb_booking_auto
            INTO @DATA(ls_ztax)
            WHERE  company_code = @ls_input-comp_code
            AND booking_type = @ls_input-booking_type
            AND trvl_type = 'TD'.

          ADD 1 TO iv_cnt.
          CLEAR : ls_skb1.
          READ TABLE lt_skb1 INTO ls_skb1 WITH KEY  bukrs = ls_input-comp_code
                                                    saknr = ls_ztax-gl_acnt.


          IF sy-subrc <> 0.
            lv_msg = | { lv_msg }, gl account { ls_ztb-gl_acnt } is invalid |.
          ENDIF.

          APPEND VALUE #(
                               itemno_acc                = iv_cnt
                               gl_account                = ls_skb1-saknr
                               item_text                 = 'TDS tax'
                               costcenter                = ls_pa0001-kostl )  TO lv_accountgl.

          APPEND VALUE #(
            itemno_acc      = iv_cnt
            currency        = 'INR'
            amt_doccur      = vend_amnt * ( -10 / 100 )
                                          ) TO lv_curramount.


*---------------------if type is vendor and t.amount more than 30000---------------------------*

          ASSIGN lv_curramount[ itemno_acc = '1' ] TO FIELD-SYMBOL(<fs_vend>).
          IF <fs_vend> IS ASSIGNED.
            <fs_vend>-amt_doccur = vend_amnt * -1 + ( 10 / 100 * vend_amnt ).
          ENDIF.

          ASSIGN lv_curramount[ itemno_acc = '5' ] TO FIELD-SYMBOL(<fs_vend1>).
          IF <fs_vend1> IS ASSIGNED.
            <fs_vend1>-amt_doccur = vend_amnt * ( -10 / 100 ).
          ENDIF.


        ELSE.

*--------------Assigning Aggregated Amount to vendor amount--------------*

          ASSIGN lv_curramount[ itemno_acc = '1' ] TO <fs_vend>.
          IF <fs_vend> IS ASSIGNED.
            <fs_vend>-amt_doccur = vend_amnt * -1.
          ENDIF.
*------------------------------------------------------------------------*
        ENDIF.
      ENDIF.





      IF lv_msg IS NOT INITIAL.
        IF ls_input-ref_no EQ ls_log-ref_no.

          ls_output-comp_code = ls_input-comp_code.
          ls_output-ref_no = ls_input-ref_no.
          ls_output-emp_code = ls_input-emp_code.
          ls_output-doc_no = ls_log-doc_no.
          ls_output-fiscal_year = ls_log-fiscal_year.
          ls_output-type = 'S'.
          ls_output-msg = lv_msg.
          APPEND ls_output TO lt_output.


        ELSE.
          ls_output-emp_code = ls_input-emp_code.
          ls_output-type = 'E'.
          ls_output-msg = lv_msg.
          APPEND ls_output TO lt_output.

        ENDIF.
      ELSE.

        lv_header = VALUE #(
             bus_act          = 'RFBU'
             username         = sy-uname
             comp_code        = ls_input-comp_code
             doc_date         = ls_input-doc_date
             pstng_date       = ls_input-pos_date
             fisc_year        = lv_fisyear
             doc_type         = 'KR'
             ref_doc_no       = ls_input-ref_no ).


        CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
          EXPORTING
            documentheader = lv_header
          IMPORTING
            obj_type       = lv_obtype
            obj_key        = lv_obkey
            obj_sys        = lv_OBJSYS
          TABLES
            accountgl      = lv_accountgl
            accountpayable = lv_accountpayble
            currencyamount = lv_curramount
            return         = lv_return.

        CLEAR : iv_cnt.
        ADD 1 TO iv_cnt.


        IF line_exists( lv_return[ type = 'E' ] ) OR line_exists(  lv_return[ type = 'A' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          SORT lv_return BY type.
          CLEAR : lv_msg.
          LOOP AT lv_return INTO DATA(l_ret) WHERE type = 'E' OR type = 'A'.
            lv_msg = | { lv_msg } { l_ret-message }|.
            ADD 1 TO iv_cnt.

            AT END OF type.

              APPEND VALUE #( comp_code  = ls_input-comp_code
                              ref_no     = ls_input-ref_no
                              emp_code   = ls_input-emp_code
                              doc_no     = ls_output-doc_no
                              fiscal_year = ls_output-fiscal_year
                              type       = 'E'
                              msg        = lv_msg

              ) TO lt_output.

              DATA(l_update) = VALUE zfi_trvl_exp_log(
                  mandt        = sy-mandt
                  comp_code    = ls_input-comp_code
                  ref_no       = ls_input-ref_no
                  posnr        = iv_cnt
                  vendor       = ls_input-vendor
                  booking_type = ls_input-booking_type
*                gl_type      =
                gl_acnt      = ls_skb1-saknr
*                amount       =
                  emp_code     = ls_input-emp_code
                  doc_date     = ls_input-doc_date
                  pos_date     = ls_input-pos_date
*                doc_no       = ls_output-doc_no
                  fiscal_year  = ls_output-fiscal_year
                  creation_DATE = sy-datum
                  type         = 'E'
                  msg          = lv_msg
              ).
              MODIFY zfi_trvl_exp_log FROM l_update.
            ENDAT.
          ENDLOOP.

        ELSE.
          LOOP AT lv_return INTO l_ret WHERE type = 'S'.
            lv_msg = | { lv_msg } { l_ret-message }|.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            APPEND VALUE #( comp_code  = ls_input-comp_code
                           ref_no     = ls_input-ref_no
                           emp_code   = ls_input-emp_code
                           doc_no     = lv_obkey+0(10)
                           fiscal_year = lv_obkey+13(5)
                           type       = 'S'
                           msg        = lv_msg

           ) TO lt_output.

            l_update = VALUE zfi_trvl_exp_log(
            mandt        = sy-mandt
            comp_code    = ls_input-comp_code
            ref_no       = ls_input-ref_no
            posnr        = iv_cnt
            vendor       = ls_input-vendor
            booking_type = ls_input-booking_type
*                gl_type      =
                  gl_acnt      = ls_skb1-saknr
*                amount       =
            emp_code     = ls_input-emp_code
            doc_date     = ls_input-doc_date
            pos_date     = ls_input-pos_date
            doc_no       = lv_obkey+0(10)
            fiscal_year  = ls_output-fiscal_year
            creation_DATE = sy-datum
            type         = 'S'
            msg          = lv_msg
        ).
            MODIFY zfi_trvl_exp_log FROM l_update.
          ENDLOOP.
        ENDIF.


*--------------------Document attachment--------------------------------*


        " Extract parts of the string
        lv_part1 = lv_obkey+10(4).  " Extract '67890' (6th to 10th character)
        lv_part2 = lv_obkey(10). " Extract 'ABCDE' (11th to 15th character)
        lv_part3 = lv_obkey+14(4).    " Extract '12345' (1st to 5th character)

        " Concatenate the parts in the desired order
        lv_new_string = lv_part1 && lv_part2 && lv_part3.

        CLEAR : lv_count.
        IF ls_input-travel IS NOT INITIAL.

          LOOP AT ls_input-travel INTO lss_trav.
            IF lss_trav-attachment_1 IS NOT INITIAL.

              .
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.

              ADD 1 TO lv_count.

              CALL METHOD attachment
                EXPORTING
                  file       = lss_trav-attachment_1
                  object_key = lv_new_string
                  count      = lv_count.


            ENDIF.
          ENDLOOP.
        ENDIF.

        IF ls_input-acco IS NOT INITIAL.


          LOOP AT ls_input-acco INTO lss_acco.
            IF lss_acco-attachment_2 IS NOT INITIAL.

*                lv_file = lss_acco-attachment_2.
*                lv_objkey = lv_obkey.
              ADD 1 TO lv_count.

              CALL METHOD attachment
                EXPORTING
                  file       = lss_acco-attachment_2
                  object_key = lv_new_string
                  count      = lv_count.


            ENDIF.
          ENDLOOP.
        ENDIF.

        IF ls_input-food IS NOT INITIAL.


          LOOP AT ls_input-food INTO lss_food.
            IF lss_food-attachment_3 IS NOT INITIAL.

*
*                lv_file = lss_food-attachment_3.
*                lv_objkey = lv_obkey.
              ADD 1 TO lv_count.

              CALL METHOD attachment
                EXPORTING
                  file       = lss_food-attachment_3
                  object_key = lv_new_string
                  count      = lv_count.

            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
*------------------------------------------------------*
    ENDIF.



    IF lt_output[] IS NOT INITIAL.

      /ui2/cl_json=>serialize(
        EXPORTING
          data             =   lt_output                                 " Data to serialize
          pretty_name      = /ui2/cl_json=>pretty_mode-user              " Pretty Print property names
        RECEIVING
          r_json           =  lv_body                                    " JSON string
      ).


    ENDIF.


    CALL METHOD lo_logupd->log_entry_store
      EXPORTING
        apiname         = 'TRAVEL_EXPENSE'
        ijson           = lv_data
        ojson           = lv_body
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.

    ENDIF.


*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = lv_body ).

  ENDMETHOD.


  METHOD attachment.

    DATA: i_component TYPE sbdst_components.
    DATA: w_component LIKE LINE OF i_component.
    DATA: objkey TYPE sbdst_object_key.
    DATA: i_signature TYPE sbdst_signature.
    DATA: w_signature LIKE LINE OF i_signature.
    DATA : l_classname TYPE sbdst_classname.
    DATA : gt_content TYPE TABLE OF bapiconten.
    DATA : lv_out TYPE i.

    DATA : lv_xstring TYPE xstring.
    DATA : lv_string TYPE string.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input  = file
*       UNESCAPE       = 'X'
      IMPORTING
        output = lv_xstring
* EXCEPTIONS
*       FAILED = 1
*       OTHERS = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*
*    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*      EXPORTING
*        text   = lv_string
**       MIMETYPE       = ' '
**       ENCODING       =
*      IMPORTING
*        buffer = lv_xstring.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_out
      TABLES
        binary_tab    = gt_content.

    w_component-doc_count  = count.
    w_component-comp_count = count.
    w_component-comp_id    = object_key.
    w_component-comp_size  = lines( gt_content ) * 1022.

    SELECT SINGLE mimetype
      FROM toadd
      INTO w_component-mimetype
    WHERE doc_type = 'PDF'.

    APPEND w_component TO i_component.

    w_signature-doc_count  = count.
    w_signature-doc_id     = 'MESSAGE' .
    w_signature-doc_ver_no = 2.
    w_signature-comp_count = count.
    w_signature-prop_name  = 'DESCRIPTION'.
    w_signature-prop_value = object_key.
    objkey = object_key.

    APPEND w_signature TO i_signature.



    l_classname = 'BKPF'.
    CALL METHOD cl_bds_document_set=>create_with_table
      EXPORTING
        classname       = l_classname
        classtype       = 'BO'
        client          = sy-mandt
        components      = i_component
        content         = gt_content
      CHANGING
        object_key      = objkey
        signature       = i_signature
      EXCEPTIONS
        internal_error  = 1
        error_kpro      = 2
        parameter_error = 3
        not_authorized  = 4
        not_allowed     = 5
        nothing_found   = 6
        OTHERS          = 7.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
