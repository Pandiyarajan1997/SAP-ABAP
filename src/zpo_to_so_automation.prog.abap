*&---------------------------------------------------------------------*
*& Report ZPO_TO_SO_AUTOMATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpo_to_so_automation.
DATA : wa_order_header_in      TYPE bapisdhd1,
       wa_order_header_inx     TYPE bapisdhd1x,
       wa_order_items_in       TYPE bapisditm,
       it_order_items_in       TYPE TABLE OF bapisditm,
       wa_order_items_inx      TYPE bapisditmx,
       it_order_items_inx      TYPE TABLE OF bapisditmx,
       wa_order_partners       TYPE bapiparnr,
       it_order_partners       TYPE TABLE OF bapiparnr,

       wa_ORDER_SCHEDULES_IN   TYPE bapischdl,
       it_ORDER_SCHEDULES_IN   TYPE TABLE OF bapischdl,
       wa_ORDER_SCHEDULES_INX  TYPE bapischdlx,
       it_ORDER_SCHEDULES_INX  TYPE TABLE OF bapischdlx,


       wa_order_conditions_in  TYPE bapicond,
       it_order_conditions_in  TYPE TABLE OF bapicond,
       wa_order_conditions_inx TYPE bapicondx,
       it_order_conditions_inx TYPE TABLE OF bapischdlx,
       wa_order_cfgs_ref       TYPE bapicucfg,
       it_order_cfgs_ref       TYPE TABLE OF bapicucfg,
       wa_order_text           TYPE bapisdtext,
       it_order_text           TYPE TABLE OF bapisdtext,
       salesdocument           LIKE bapivbeln-vbeln,
       it_return               TYPE TABLE OF bapiret2.

DATA : wa_zpo_to_so TYPE zpo_to_so,
       it_zpo_to_so TYPE TABLE OF zpo_to_so.



START-OF-SELECTION.

  SELECT ek~ebeln,ek~bsart,ek~lifnr,ep~ebelp,ep~uniqueid, ep~werks, ep~matnr,ma~spart,ep~menge, sc~kunnr,ek~aedat FROM ekko AS ek
  INNER JOIN ekpo AS ep ON ek~ebeln = ep~ebeln
  INNER JOIN mara AS ma ON ep~matnr = ma~matnr
  INNER JOIN zpo_to_so_config AS sc ON ep~werks = sc~werks
  WHERE ek~bsart = 'ZNB' AND ek~lifnr = '0010001546' AND ek~aedat > '20240825' AND ep~uniqueid NOT IN ( SELECT  uniqueid FROM zpo_to_so )
  INTO TABLE @DATA(it_tab).

  IF it_tab IS NOT INITIAL.
    SELECT ebeln,spart,aedat FROM @it_TAB AS t1
      GROUP BY ebeln, spart ,aedat INTO TABLE @DATA(it_tab01).
    LOOP AT   it_tab01 INTO DATA(wa_tab01).
      SELECT * FROM @it_tab AS t2 WHERE ebeln = @wa_tab01-ebeln AND spart = @wa_tab01-spart INTO TABLE @DATA(it_tab02).
      IF it_tab02 IS NOT INITIAL.

        LOOP AT it_tab02 INTO DATA(wa_tab02).
          wa_order_items_in-itm_number = wa_tab02-ebelp.
          wa_order_items_in-material = wa_tab02-matnr.
          wa_order_items_in-target_qty = wa_tab02-menge.
          wa_order_items_in-salqtynum = wa_tab02-menge.
          wa_order_items_in-salqtyden = wa_tab02-menge.
          APPEND wa_order_items_in TO it_order_items_in.
          CLEAR  : wa_order_items_in .
          wa_order_items_inx-itm_number = wa_tab02-ebelp.
          wa_order_items_inx-updateflag = 'X'.
          wa_order_items_inx-material = 'X'.
          wa_order_items_inx-target_qty = 'X'.
          wa_order_items_inx-trg_qty_no = 'X'.
          wa_order_items_inx-salqtynum = 'X'.
          wa_order_items_inx-salqtyden = 'X'.
          APPEND wa_order_items_inx TO it_order_items_inx.
          CLEAR : wa_order_items_inx.

          wa_order_schedules_in-itm_number =  wa_tab02-ebelp.
          wa_order_schedules_in-req_qty =  wa_tab02-menge.
          APPEND wa_order_schedules_in TO it_order_schedules_in.
          CLEAR : wa_order_schedules_in.

          wa_order_schedules_inx-itm_number =  wa_tab02-ebelp..
          wa_order_schedules_inx-req_qty = 'X'.
          APPEND wa_order_schedules_inx TO it_order_schedules_inx.
          CLEAR : wa_order_schedules_inx.

          wa_order_text-itm_number = wa_tab02-ebelp.
          wa_order_text-text_id = '0001'.
          wa_order_text-langu = 'E'.
          wa_order_text-text_line = wa_tab02-uniqueid.
          wa_order_text-function = '001'.
          append wa_order_text to it_order_text.
          clear : wa_order_text.


        ENDLOOP.

        wa_order_header_in-doc_type = 'YBFS'.
        wa_order_header_in-sales_org = '1400'.
        wa_order_header_in-distr_chan = '40'.
        wa_order_header_in-division = it_tab02[ 1 ]-spart.
        wa_order_header_in-sales_off = '1401'.
        wa_order_header_in-ref_1 = it_tab02[ 1 ]-ebeln.
        wa_order_header_in-purch_no_c =  it_tab02[ 1 ]-ebeln.
        wa_order_header_in-purch_date =  it_tab02[ 1 ]-aedat .

        wa_order_header_inx-doc_type = 'X'.
        wa_order_header_inx-sales_org = 'X'.
        wa_order_header_inx-distr_chan = 'X'.
        wa_order_header_inx-division = 'X'.
        wa_order_header_inx-sales_off = 'X'.
        wa_order_header_inx-ref_1 = 'X'.
        wa_order_header_inx-purch_no_c = 'X'.
        wa_order_header_inx-purch_date = 'X'.

        wa_order_partners-partn_role = 'AG'.
        wa_order_partners-partn_numb = it_tab02[ 1 ]-kunnr.
        APPEND wa_order_partners TO it_order_partners.
        CLEAR : wa_order_partners.
        CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
          EXPORTING
            order_header_in      = wa_order_header_in
            order_header_inx     = wa_order_header_inx
          IMPORTING
            salesdocument        = salesdocument
          TABLES
            return               = it_return
            order_items_in       = it_order_items_in
            order_items_inx      = it_order_items_inx
            order_partners       = it_order_partners
            order_schedules_in   = it_order_schedules_in
            order_schedules_inx  = it_order_schedules_inx
            order_conditions_in  = it_order_conditions_in
            order_conditions_inx = it_order_conditions_inx
            order_cfgs_ref       = it_order_cfgs_ref
            order_text           = it_order_text.

        DATA : lv_timestamp TYPE string.
        CONCATENATE sy-datum sy-uzeit INTO lv_timestamp .
        IF salesdocument IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          WAIT UP TO 1 SECONDS.
          LOOP AT it_tab02 INTO DATA(wa_tab03).
            wa_zpo_to_so-ebeln = wa_tab03-ebeln.
            wa_zpo_to_so-ebelp = wa_tab03-ebelp.
            wa_zpo_to_so-uniqueid = wa_tab03-uniqueid.
            wa_zpo_to_so-vbeln = salesdocument.
            wa_zpo_to_so-matnr = wa_tab03-matnr.
            wa_zpo_to_so-spart = wa_tab03-spart.
            wa_zpo_to_so-status = 'SUCCESS'.
            wa_zpo_to_so-timestamp =  lv_timestamp.
            APPEND wa_zpo_to_so TO it_zpo_to_so.



            WRITE :/ wa_zpo_to_so-uniqueid , wa_zpo_to_so-status.
           CLEAR : wa_zpo_to_so.
          ENDLOOP.
          clear : wa_tab03.

       ELSE.
          LOOP AT it_tab02 INTO DATA(wa_tab04).
            wa_zpo_to_so-ebeln = wa_tab04-ebeln.
            wa_zpo_to_so-ebelp = wa_tab04-ebelp.
            wa_zpo_to_so-uniqueid = wa_tab04-uniqueid.
            wa_zpo_to_so-matnr = wa_tab04-matnr.
            wa_zpo_to_so-spart = wa_tab04-spart.
            wa_zpo_to_so-status = 'FAILED'.
            wa_zpo_to_so-timestamp =  lv_timestamp.
            APPEND wa_zpo_to_so TO it_zpo_to_so.
            WRITE :/ wa_zpo_to_so-uniqueid , wa_zpo_to_so-status.

            CLEAR : wa_zpo_to_so.
          ENDLOOP.
          clear : wa_tab04.
        ENDIF.
        IF it_zpo_to_so IS NOT INITIAL.
          MODIFY zpo_to_so FROM table it_zpo_to_so.
        ENDIF.





      ENDIF.
      CLEAR : wa_order_header_in,wa_order_header_inx,wa_tab02,lv_timestamp.
      REFRESH : it_return, it_order_text, it_zpo_to_so,it_tab02,it_order_items_in,it_order_items_inx,it_order_schedules_in,it_order_schedules_inx,it_order_conditions_in,it_order_conditions_inx,it_order_partners.
    ENDLOOP.

  ENDIF.
