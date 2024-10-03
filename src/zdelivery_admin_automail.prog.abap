*&---------------------------------------------------------------------*
*& Report  ZDELIVERY_DETAILS_ADMIN_AUTOMAIL
*&
*&---------------------------------------------------------------------*
*& Changed by: Ramakrishnan J (704322)
*& Change Requested for - SCM Team
*& Change Desc - To Send email for all the e-mail ids mapped in the
*& zemail_salesoff against the sales organization
*& Change Date - 16.11.2022
*&---------------------------------------------------------------------*

REPORT zdelivery_admin_automail.


DATA: s_repid LIKE sy-repid.
DATA: in_mailid TYPE ad_smtpadr.
DATA: s_email TYPE  adr6-smtp_addr.

TYPES : BEGIN OF ty_zemail_salesoff,
          em_vkbur      TYPE zemail_salesoff-vkbur,
          em_department TYPE zemail_salesoff-zdepartment,
          em_emailid    TYPE zemail_salesoff-emailid,
          em_status     TYPE zemail_salesoff-status,
        END OF ty_zemail_salesoff.

DATA : it_zemail_salesoff  TYPE TABLE OF ty_zemail_salesoff,
       wa_zemail_salesoff  TYPE ty_zemail_salesoff,

       it_sendmail         TYPE TABLE OF ty_zemail_salesoff,
       wa_sendmail         TYPE ty_zemail_salesoff,

       temp_sendmail       TYPE TABLE OF ty_zemail_salesoff,
       temp_sendmail_plant TYPE TABLE OF ty_zemail_salesoff,
       tewa_sendmail       TYPE ty_zemail_salesoff,

       itfinal_sendmail    TYPE TABLE OF ty_zemail_salesoff,
       wafinal_sendmail    TYPE ty_zemail_salesoff.

DATA : it_send TYPE TABLE OF zemail_salesoff.

TYPES :BEGIN OF ty_zsal_zone_reg,
         sa_vkbur TYPE zsal_zone_reg-vkbur,
         sa_name1 TYPE zsal_zone_reg-name1,
         sa_zone  TYPE zsal_zone_reg-zone1,
         sa_regio TYPE zsal_zone_reg-regio,
       END OF ty_zsal_zone_reg.

TYPES : BEGIN OF ty_ymard_nsap,
          kunnr    TYPE ymard_nsap-kunnr,
          matnr    TYPE ymard_nsap-matnr,
          erdat    TYPE ymard_nsap-erdat,
          erzet    TYPE ymard_nsap-erzet,
          meins    TYPE ymard_nsap-meins,
          labst    TYPE ymard_nsap-labst,
          trame    TYPE ymard_nsap-trame,
          sku_stk  TYPE ymard_nsap-tot_stk,
          open_ord TYPE ymard_nsap-pen_ord,
        END OF ty_ymard_nsap.

DATA : it_ymard_nsap TYPE TABLE OF ty_ymard_nsap,
       wa_ymard_nsap TYPE ty_ymard_nsap.



TYPES : BEGIN OF ty_vbap,
          vbeln  TYPE vbap-vbeln,
          posnr  TYPE vbap-posnr,
          matnr  TYPE vbrp-matnr,
          arktx  TYPE vbrp-arktx,
          netwr  TYPE vbrp-netwr,
          kwmeng TYPE vbap-kwmeng,
          werks  TYPE vbap-werks,
        END OF ty_vbap.

DATA  : it_vbap TYPE TABLE OF ty_vbap,
        wa_vbap TYPE ty_vbap.

TYPES : BEGIN OF ty_vbak,
          vbeln TYPE vbakuk-vbeln, "#EC CI_USAGE_OK[2198647] " Sale Order NoAdded by <IT-CAR Tool> during Code Remediation
          audat TYPE vbakuk-audat, "#EC CI_USAGE_OK[2198647] " DateAdded by <IT-CAR Tool> during Code Remediation
          auart TYPE vbakuk-auart, "#EC CI_USAGE_OK[2198647] " Sales Order TypeAdded by <IT-CAR Tool> during Code Remediation
          netwr TYPE vbakuk-netwr, "#EC CI_USAGE_OK[2198647] " NetValueAdded by <IT-CAR Tool> during Code Remediation
          vkbur TYPE vbakuk-vkbur, "#EC CI_USAGE_OK[2198647] " Sales OfficeAdded by <IT-CAR Tool> during Code Remediation
          bstnk TYPE vbakuk-bstnk, "#EC CI_USAGE_OK[2198647] " PO numberAdded by <IT-CAR Tool> during Code Remediation
          kunnr TYPE vbakuk-kunnr, "#EC CI_USAGE_OK[2198647] " Customer NumberAdded by <IT-CAR Tool> during Code Remediation
        END OF ty_vbak.

DATA  : it_vbak TYPE TABLE OF ty_vbak,
        wa_vbak TYPE ty_vbak.

DATA  : it_vbak1 TYPE TABLE OF ty_vbak,
        wa_vbak1 TYPE ty_vbak.

TYPES : BEGIN OF ty_lips,
          vbeln TYPE lips-vbeln,
          posnr TYPE lips-posnr,
          erdat TYPE lips-erdat,
          vgbel TYPE lips-vgbel,
          vgpos TYPE lips-vgpos,
        END OF ty_lips.

DATA  : it_lips TYPE TABLE OF ty_lips,
        wa_lips TYPE ty_lips.

TYPES : BEGIN OF ty_vbrp,
          vbeln TYPE vbrp-vbeln,
          posnr TYPE vbrp-posnr,
          fkimg TYPE vbrp-fkimg,
          netwr TYPE vbrp-netwr,
          aubel TYPE vbrp-aubel,
          aupos TYPE vbrp-aupos,
        END OF ty_vbrp.

DATA  : it_vbrp TYPE TABLE OF ty_vbrp,
        wa_vbrp TYPE ty_vbrp.

TYPES : BEGIN OF ty_vbfa,
          vbelv   TYPE vbfa-vbelv,
          posnv   TYPE vbfa-posnv,
          vbeln   TYPE vbfa-vbeln,
          posnn   TYPE vbfa-posnn,
          vbtyp_n TYPE vbfa-vbtyp_n,
        END OF ty_vbfa.

DATA  : it_vbfa TYPE TABLE OF ty_vbfa,
        wa_vbfa TYPE ty_vbfa.

TYPES : BEGIN OF ty_vbrk,
          vbeln            TYPE vbrk-vbeln,
          fkart            TYPE vbrk-fkart,
          fkdat            TYPE vbrk-fkdat,
          fksto            TYPE vbrk-fksto,
          date_of_delivery TYPE vbrk-date_of_delivery,
          remarks          TYPE vbrk-remarks,
        END OF ty_vbrk.

DATA  : it_vbrk TYPE TABLE OF ty_vbrk,
        wa_vbrk TYPE ty_vbrk.

TYPES : BEGIN OF ty_kna1,
          kunnr TYPE kna1-kunnr,
          land1 TYPE kna1-land1,
          name1 TYPE kna1-name1,
          regio TYPE kna1-regio,
        END OF ty_kna1.

DATA  : it_kna1 TYPE TABLE OF ty_kna1,
        wa_kna1 TYPE ty_kna1.

TYPES : BEGIN OF ty_t005u,
          spras TYPE t005u-spras,
          land1 TYPE t005u-land1,
          bland TYPE t005u-bland,
          bezei TYPE t005u-bezei,
        END OF ty_t005u.

DATA  : it_t005u TYPE TABLE OF ty_t005u,
        wa_t005u TYPE ty_t005u.

TYPES : BEGIN OF ty_t001w,
          werks TYPE t001w-werks,
          name1 TYPE t001w-name1,
        END OF ty_t001w.

DATA  : it_t001w TYPE TABLE OF ty_t001w,
        wa_t001w TYPE ty_t001w.

TYPES : BEGIN OF ty_final,
          vbeln    TYPE vbakuk-vbeln, "#EC CI_USAGE_OK[2198647] " SALES ORDER NUMBERAdded by <IT-CAR Tool> during Code Remediation
          auart    TYPE auart,       " Order type
          item     TYPE vbap-posnr, " SALE ORDER ITEM NO
          audat    TYPE vbakuk-audat, "#EC CI_USAGE_OK[2198647] " ORDER DATEAdded by <IT-CAR Tool> during Code Remediation
          netwr    TYPE vbakuk-netwr, "#EC CI_USAGE_OK[2198647] " NetValueAdded by <IT-CAR Tool> during Code Remediation
          bstnk    TYPE vbakuk-bstnk, "#EC CI_USAGE_OK[2198647] " PO numberAdded by <IT-CAR Tool> during Code Remediation
          vbeln1   TYPE vbrk-vbeln, " INVOICE NUMBER
          fkdat    TYPE vbrk-fkdat, " INVOICE DATE
          kunnr    TYPE vbakuk-kunnr, "#EC CI_USAGE_OK[2198647] " CUSTOMER CODEAdded by <IT-CAR Tool> during Code Remediation
          kdgrp    TYPE knvv-kdgrp,
          kdtxt    TYPE vtxtk,
          name1    TYPE kna1-name1, " CUSTOMER NAME
          bezei    TYPE t005u-bezei, " REGION
          werks    TYPE vbap-werks, " SALES OFFICE
          matnr    TYPE vbap-matnr, " MATERIAL CODE
          arktx    TYPE vbap-arktx, " MATERIAL DESCRIPTION
          kwmeng   TYPE vbap-kwmeng, " SALE ORDER QUANTITY
          fkimg    TYPE vbrp-fkimg,   " BILLED QUANTITY
          aubel    TYPE vbrp-aubel, "Sales Document ref
          aupos    TYPE vbrp-aupos, "Sales Document item no ref
          bal_q    TYPE string,   " BALANCE QUANTITY
          bal_a    TYPE string,   " BALANCE AMOUNT
          inv_q    TYPE string,   " INVOICE QUANTITY
          inv_a    TYPE string,   " INVOICE AMOUNT
          sal_q    TYPE string,   " SALE ORDER QUANTITY
          sal_a    TYPE string,   " SALE ORDER AMOUNT
          d_o_d    TYPE vbrk-date_of_delivery, " DATE OF DELIVERY
          remarks  TYPE vbrk-remarks, " REMARKS
          days     TYPE char5, " NO OF DAYS
          btdd     TYPE char5, " Billed TO Delivery Date.
          otdd     TYPE char5, " Order To Delivery Date
          new_val  TYPE string, "New_ROL.
          sku_stk  TYPE string, "SKUSTOCK
          open_ord TYPE string, "SKUOPENORDER
        END OF ty_final.

DATA  : it_final TYPE TABLE OF ty_final,
        wa_final TYPE ty_final.

DATA  : it_final1 TYPE TABLE OF ty_final,
        wa_final1 TYPE ty_final.
TYPES : BEGIN OF ty_rol,
          kunnr   TYPE zcustmat_rol-kunnr,
          matnr   TYPE zcustmat_rol-matnr,
          new_val TYPE zcustmat_rol-new_val,
          new_dat TYPE zcustmat_rol-new_dat,
        END OF ty_rol.

TYPES: BEGIN OF ty_knvv,
         kunnr TYPE kunnr,
         vkorg TYPE vkorg,
         kdgrp TYPE kdgrp,
       END OF ty_knvv.

DATA: it_knvv TYPE TABLE OF ty_knvv,
      wa_knvv TYPE ty_knvv.

DATA: it_T151T TYPE TABLE OF t151t,
      wa_T151T TYPE t151t.

DATA  : it_rol TYPE TABLE OF ty_rol,
        wa_rol TYPE ty_rol.

DATA  : sal_ofc TYPE char40.

DATA  : g_tab_lines  TYPE i.

DATA  : tem_qun TYPE vbrp-fkimg.
DATA  : tem_qun1 TYPE vbrp-fkimg.
DATA  : g_sent_to_all TYPE sonv-flag.

DATA  : days TYPE i.
DATA  : btdd TYPE i.
DATA : otdd TYPE i .

DATA  : i TYPE char5,
        j TYPE char5.

DATA  : bal_qun TYPE vbrp-fkimg,
        bal_amt TYPE vbap-netwr.

DATA  : ndni  TYPE char100,
        dni   TYPE char50,
        count TYPE char250.

DATA  : con_werks TYPE char50,
        con_audat TYPE char50,
        con_kunnr TYPE char50.

DATA  : low_dt TYPE char10,
        hig_dt TYPE char10.

DATA  : pos TYPE i VALUE 0 .

DATA  : flag TYPE c.

DATA  : it_header TYPE slis_t_listheader,
        wa_header TYPE slis_listheader.

DATA  : wa_layout TYPE slis_layout_alv.

DATA  : audat_low TYPE vbak-audat,
        audat_hig TYPE vbak-audat.


DATA : to_date TYPE vbak-audat.
to_date = sy-datum.
DATA  : mon TYPE i.


DATA : lv_string      TYPE string, "declare string
       lv_data_string TYPE string. "declare string

DATA: lit_binary_content TYPE solix_tab.
DATA: l_attsubject   TYPE sood-objdes.

DATA  : it_attachment TYPE STANDARD TABLE OF solisti1,
        wa_attachment LIKE LINE OF it_attachment.

DATA: lv_date TYPE char10.

START-OF-SELECTION.

  PERFORM get_dat.



END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  GET_DAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dat .

  DATA: temp_emailid TYPE zemail_salesoff-emailid.

  SELECT vkbur
       zdepartment
       emailid
       status FROM zemail_salesoff INTO TABLE it_zemail_salesoff WHERE zdepartment = 'ADMIN'.

  SELECT * FROM t151t INTO TABLE it_T151T WHERE spras = 'E'.

*  CLEAR : it_sendmail ,temp_emailid.
*
*  LOOP AT it_zemail_salesoff INTO wa_zemail_salesoff.
*    APPEND wa_zemail_salesoff TO  it_sendmail.
*  ENDLOOP.
*
*  SORT it_sendmail ASCENDING BY em_emailid.
*
*  LOOP AT it_sendmail INTO wa_sendmail.
*    IF temp_emailid = wa_sendmail-em_emailid.
*      tewa_sendmail-em_emailid = wa_sendmail-em_emailid.
*      tewa_sendmail-em_vkbur = wa_sendmail-em_vkbur.
*    ELSE.
*      IF tewa_sendmail IS NOT INITIAL.
*        APPEND tewa_sendmail TO temp_sendmail.
*        CLEAR tewa_sendmail.
*      ENDIF.
*      tewa_sendmail = wa_sendmail.
*      temp_emailid = wa_sendmail-em_emailid.
*    ENDIF.
*  ENDLOOP.
*  APPEND tewa_sendmail TO temp_sendmail.
*  CLEAR :tewa_sendmail ,temp_emailid.
*
*
*  SORT temp_sendmail ASCENDING BY em_emailid.

  IF it_zemail_salesoff[] IS NOT INITIAL.
    temp_sendmail_plant[] = it_zemail_salesoff[].
    SORT temp_sendmail_plant BY em_vkbur.
    DELETE ADJACENT DUPLICATES FROM temp_sendmail_plant COMPARING em_vkbur.
  ENDIF.

  LOOP AT temp_sendmail_plant INTO wa_sendmail .

*    SELECT vkbur
*         zdepartment
*         emailid
*         status FROM zemail_salesoff INTO TABLE itfinal_sendmail WHERE zdepartment = 'ADMIN' AND emailid = wa_sendmail-em_emailid.

*    SELECT vbeln audat auart netwr vkbur bstnk  kunnr
*      FROM vbakuk
*      INTO TABLE it_vbak
*      FOR ALL ENTRIES IN itfinal_sendmail
*      WHERE vkbur = itfinal_sendmail-em_vkbur AND vkorg = '1000'
*          AND ( auart = 'YBBR' OR auart = 'YBDP' ) AND bestk <> ' ' AND gbstk <> 'C' AND erdat >= '20200501'.

*Get all data from the sales order table VBAK based on the salses office
    SELECT vbeln audat auart netwr vkbur bstnk  kunnr
      FROM vbakuk
      INTO TABLE it_vbak
*      FOR ALL ENTRIES IN itfinal_sendmail_plant
      WHERE vkbur = wa_sendmail-em_vkbur AND vkorg = '1000'
          AND ( auart = 'YBBR' OR auart = 'YBDP' ) AND bestk <> ' ' AND gbstk <> 'C' AND erdat >= '20200501'.

    IF it_vbak[] IS NOT INITIAL.

* Select customer details from KNA1
      SELECT kunnr land1 name1 regio
        FROM kna1 INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_vbak
        WHERE kunnr = it_vbak-kunnr.

* Select region texts from t005u
      SELECT spras land1 bland bezei
        FROM t005u
        INTO TABLE it_t005u
        FOR ALL ENTRIES IN it_kna1
        WHERE spras = sy-langu
        AND land1 = it_kna1-land1
        AND bland = it_kna1-regio.

*Selecting data from DMS stock table for todays date
      SELECT kunnr matnr erdat erzet meins labst trame tot_stk pen_ord
        FROM ymard_nsap
        INTO TABLE it_ymard_nsap
        FOR ALL ENTRIES IN it_vbak
        WHERE kunnr = it_vbak-kunnr
          AND erdat = to_date.

*Selecting data from Customer ROL table
      SELECT kunnr matnr new_val new_dat
        FROM zcustmat_rol
        INTO TABLE it_rol
        FOR ALL ENTRIES IN it_vbak WHERE kunnr = it_vbak-kunnr. "and New_dat = SY-DATUM.

*for getting customer group
      SELECT kunnr vkorg kdgrp
        FROM knvv INTO TABLE it_knvv FOR ALL ENTRIES IN it_vbak WHERE kunnr = it_vbak-kunnr AND vkorg = '1000'.
      IF sy-subrc = 0.
        SORT it_knvv BY kunnr vkorg.
        DELETE ADJACENT DUPLICATES FROM it_knvv COMPARING kunnr vkorg.
      ENDIF.

*Select data from sales order items details table
      SELECT vbeln posnr matnr arktx netwr kwmeng werks
        FROM vbap
        INTO TABLE it_vbap
        FOR ALL ENTRIES IN it_vbak
        WHERE vbeln = it_vbak-vbeln.

      IF it_vbap[] IS NOT INITIAL.
* select data from VBFA table
        SELECT vbelv posnv vbeln posnn vbtyp_n
          FROM vbfa
          INTO TABLE it_vbfa
          FOR ALL ENTRIES IN it_vbap
          WHERE vbelv = it_vbap-vbeln
            AND posnv = it_vbap-posnr AND vbtyp_n = 'M' .

*Select data from VBRK (Invocice Header table)
        SELECT vbeln fkart fkdat fksto date_of_delivery remarks
          FROM vbrk
          INTO TABLE it_vbrk
          FOR ALL ENTRIES IN it_vbfa
          WHERE vbeln = it_vbfa-vbeln
            AND fksto <> 'X'.

* Select data from vbrp (Invoice details table)
        SELECT vbeln posnr fkimg netwr aubel aupos
          FROM vbrp
          INTO TABLE it_vbrp
          FOR ALL ENTRIES IN it_vbrk
          WHERE vbeln = it_vbrk-vbeln.



      ELSE.
*        MESSAGE 'No Data Found' TYPE 'I' DISPLAY LIKE 'E'.
**      SUBMIT ZDELIVERY_DETAILS VIA SELECTION-SCREEN .
        CONTINUE.
      ENDIF.
    ELSE.
*      MESSAGE 'No Data Found' TYPE 'I' DISPLAY LIKE 'E'.
**    SUBMIT ZDELIVERY_DETAILS VIA SELECTION-SCREEN .
      CONTINUE.
    ENDIF.

**********************READ_DATA****************
    CLEAR wa_vbap.

    LOOP AT it_vbap  INTO wa_vbap.
      CLEAR: wa_vbak, wa_vbfa, wa_kna1, wa_t005u, wa_ymard_nsap, wa_knvv, wa_T151T.
      READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_vbap-vbeln.
      READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbelv = wa_vbap-vbeln posnv = wa_vbap-posnr .
      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr.
      READ TABLE it_t005u INTO wa_t005u WITH KEY land1 = wa_kna1-land1 bland = wa_kna1-regio .
      READ TABLE it_ymard_nsap INTO wa_ymard_nsap WITH KEY kunnr = wa_vbak-kunnr matnr = wa_vbap-matnr.
      READ TABLE it_knvv INTO wa_knvv WITH KEY kunnr = wa_vbak-kunnr vkorg = '1000'.
      READ TABLE it_T151T INTO wa_t151t WITH KEY kdgrp = wa_knvv-kdgrp.

      wa_final-name1 = wa_kna1-name1.
      wa_final-bezei = wa_t005u-bezei.

      wa_final-vbeln = wa_vbak-vbeln.
      wa_final-audat = wa_vbak-audat.
      wa_final-auart = wa_vbak-auart.
      wa_final-bstnk = wa_vbak-bstnk.
      wa_final-werks = wa_vbak-vkbur.
      wa_final-kunnr = wa_vbak-kunnr.

      wa_final-kdgrp = wa_knvv-kdgrp.
      wa_final-kdtxt = wa_T151T-ktext.

      wa_final-netwr = wa_vbap-netwr .
      wa_final-kwmeng = wa_vbap-kwmeng. "Sale Order Quantity.
      wa_final-sal_q = wa_vbap-kwmeng .
      wa_final-sal_a = wa_vbap-netwr .
      wa_final-item = wa_vbap-posnr .
      wa_final-matnr = wa_vbap-matnr.
      wa_final-arktx = wa_vbap-arktx.

      wa_final-bal_q = wa_vbap-kwmeng.
      wa_final-bal_a = wa_vbap-netwr.

      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from = wa_vbak-audat
*         I_KEY_DAY_FROM =
          i_date_to   = sy-datum
*         I_KEY_DAY_TO   =
*         I_FLG_SEPARATE = ' '
        IMPORTING
          e_days      = otdd
*         E_MONTHS    =
*         E_YEARS     =
        .

      wa_final-otdd =  otdd .

      LOOP AT it_vbrp INTO wa_vbrp WHERE aubel EQ wa_vbap-vbeln AND aupos EQ wa_vbap-posnr .

        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbrp-vbeln .
        wa_final-vbeln1 = wa_vbrk-vbeln.
        wa_final-fkdat = wa_vbrk-fkdat.
        wa_final-d_o_d = wa_vbrk-date_of_delivery.
        wa_final-remarks = wa_vbrk-remarks.

        wa_final-inv_q = wa_vbrp-fkimg + wa_final-inv_q .
        wa_final-inv_a = wa_vbrp-netwr + wa_final-inv_a .
        wa_final-matnr = wa_vbap-matnr.
        wa_final-arktx = wa_vbap-arktx.

      ENDLOOP.

      IF wa_final-vbeln1 IS NOT INITIAL.
        bal_qun = wa_final-sal_q - wa_final-inv_q .
*            BAL_AMT = WA_FINAL-SAL_A - WA_FINAL-INV_A .
        bal_amt = wa_final-netwr / wa_final-kwmeng .
        wa_final-bal_q = bal_qun .

        IF bal_amt <> 0.
          wa_final-bal_a = bal_amt * bal_qun.
        ELSE .
          wa_final-bal_a = '-' .
        ENDIF.

      ENDIF.

      READ TABLE it_rol INTO wa_rol WITH KEY kunnr = wa_vbak-kunnr matnr = wa_vbap-matnr .
      IF wa_vbap-matnr = wa_rol-matnr.
        wa_final-new_val = wa_rol-new_val.
      ELSE.
        wa_final-new_val = 0.
      ENDIF.

      IF wa_vbap-matnr = wa_ymard_nsap-matnr.
        wa_final-sku_stk = wa_ymard_nsap-sku_stk.
        wa_final-open_ord = wa_ymard_nsap-open_ord.
      ELSE.
        wa_final-sku_stk = 0.
        wa_final-open_ord = 0.
      ENDIF.


      APPEND: wa_final TO it_final .
      CLEAR: wa_vbfa  , wa_vbak , wa_final , wa_kna1 , wa_t005u , flag  , wa_vbrp , wa_vbrk , wa_rol , wa_ymard_nsap.
    ENDLOOP.

    SORT it_final BY vbeln item .


***************************END_READ_DATA****************
*    s_email = wa_sendmail-em_emailid.
*    in_mailid = s_email.

***************** BUILD_XLS_DATA_TABLE_DETAIL************


    DATA: str1 TYPE char50,
          str2 TYPE char8,
          str3 TYPE string,
          str4 TYPE string,
          dat1 TYPE char2,
          dat2 TYPE char2,
          dat3 TYPE char4.
    CLEAR:  str1,str2,str3,str4,
              dat1,dat2,dat3.
    str1 = 'Pending Order For Dispatch_'.
    str2 = sy-datum.
    str4 = '/'.
    dat3 = str2+0(4).
    dat2 = str2+4(2).
    dat1 = str2+6(2).

    CONCATENATE str1 dat1 str4 dat2 str4 dat3 '_(' wa_sendmail-em_vkbur ')' INTO str3.


    DATA:  lo_send_request TYPE REF TO cl_bcs VALUE IS INITIAL.
    CLASS cl_bcs DEFINITION LOAD.
    lo_send_request = cl_bcs=>create_persistent( ).

* Message body and subject
    DATA: lo_document TYPE REF TO cl_document_bcs VALUE IS INITIAL. "document object
    DATA : i_text TYPE bcsy_text. "Table for body
    DATA : w_text LIKE LINE OF i_text. "work area for message body
    CLEAR:lo_document,i_text,w_text.
*Set body
    w_text-line = 'Dear Sir/Madam,'.
    APPEND w_text TO i_text.
    CLEAR w_text.
*    W_TEXT-LINE = ''.
*    APPEND W_TEXT TO I_TEXT.
*    CLEAR W_TEXT.
    DATA: tem_bdy TYPE string.
    CONCATENATE '    Please find the attachment of Open Sales order details at excel.'  '' INTO tem_bdy SEPARATED BY space.
    w_text-line = tem_bdy.
    APPEND w_text TO i_text.
    CLEAR w_text.
    w_text-line = 'Regards&Thank.'.
    APPEND w_text TO i_text.
    CLEAR w_text.
*Create Email document
    DATA: subject(50) TYPE c .
    CONCATENATE '' str3 INTO subject SEPARATED BY space.
    lo_document = cl_document_bcs=>create_document( "create document
    i_type = 'TXT' "Type of document HTM, TXT etc
    i_text =  i_text "email body internal table
    i_subject = subject ). "email subject here p_sub input parameter

* Pass the document to send request
    lo_send_request->set_document( lo_document ).

    CLEAR : lv_string.
    CLASS cl_abap_char_utilities DEFINITION LOAD.

    CONCATENATE   'Sales Office'
                  'Order type'
                  'Doc Date'
                  'Customer Name'
                  'Region'
                  'Material Desc'
                  'Sale Order Qty'
                  'Sale Order Val'
                  'Order vs POD'
                  'Sales Doc'
                  'Customer No'
                  'Customer Group'
                  'Item'
                  'Material No'
                  'Order From'
*                'Inv Doc'
*                'Inv Date'
                  'Inv Qty'
*                'Inv Value'
                  'SKU ROL'
                  'DMS STOCK'
                  'DMS Open Order'
                  'Remaining Qty'
                  'Remaining Value'
*                'Date Of Delivery'
*                'Remarks'
*                'Invoice vs POD'

 cl_abap_char_utilities=>newline INTO lv_string SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    LOOP AT it_final INTO  wa_final.

      DATA: dstr1    TYPE char8,
            dstr2    TYPE char2,
            dstr3    TYPE char2,
            dstr4    TYPE char4,
            dstr5    TYPE char1,
            datestr6 TYPE char10.

      dstr1 = wa_final-audat.
      dstr4 = dstr1+0(4).
      dstr3 = dstr1+4(2).
      dstr2 = dstr1+6(2).
      dstr5 = '/'.



      CONCATENATE dstr2 dstr5 dstr3 dstr5 dstr4  INTO datestr6.

      CONCATENATE lv_string
                       wa_final-werks cl_abap_char_utilities=>horizontal_tab
                       wa_final-auart cl_abap_char_utilities=>horizontal_tab
                       datestr6 cl_abap_char_utilities=>horizontal_tab
                       wa_final-name1 cl_abap_char_utilities=>horizontal_tab
                       wa_final-bezei cl_abap_char_utilities=>horizontal_tab
                       wa_final-arktx cl_abap_char_utilities=>horizontal_tab
                       wa_final-sal_q cl_abap_char_utilities=>horizontal_tab
                       wa_final-sal_a cl_abap_char_utilities=>horizontal_tab
                       wa_final-otdd cl_abap_char_utilities=>horizontal_tab
                       wa_final-vbeln cl_abap_char_utilities=>horizontal_tab
                       wa_final-kunnr cl_abap_char_utilities=>horizontal_tab
                       wa_final-kdtxt cl_abap_char_utilities=>horizontal_tab
                       wa_final-item cl_abap_char_utilities=>horizontal_tab
                       wa_final-matnr cl_abap_char_utilities=>horizontal_tab
                       wa_final-bstnk cl_abap_char_utilities=>horizontal_tab
*                 WA_FINAL-VBELN1 CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
*                 WA_FINAL-FKDAT CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                       wa_final-inv_q cl_abap_char_utilities=>horizontal_tab
*                 WA_FINAL-INV_A CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
                       wa_final-new_val cl_abap_char_utilities=>horizontal_tab
                       wa_final-sku_stk cl_abap_char_utilities=>horizontal_tab
                       wa_final-open_ord cl_abap_char_utilities=>horizontal_tab
                       wa_final-bal_q cl_abap_char_utilities=>horizontal_tab
                       wa_final-bal_a cl_abap_char_utilities=>horizontal_tab
*                 WA_FINAL-D_O_D
*                 WA_FINAL-REMARKS
*                 WA_FINAL-BTDD
         cl_abap_char_utilities=>newline INTO lv_string.

    ENDLOOP.

    DATA lv_xstring TYPE xstring .
    CLEAR lv_xstring.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_string
*       mimetype =                   ld_mimetype
*       encoding =                   ld_encoding
      IMPORTING
        buffer = lv_xstring
      EXCEPTIONS
        failed = 1
      .  "  SCMS_STRING_TO_XSTRING

    IF sy-subrc EQ 0.
      "All OK
    ELSEIF sy-subrc EQ 1. "Exception
      "Add code for exception here
    ENDIF.

    DATA: l_zipper TYPE REF TO cl_abap_zip. " Zip class declerration
    DATA : l_data TYPE string.

    CLEAR: l_zipper,l_data.
    WRITE sy-datum TO lv_date.
    CONCATENATE 'Pending Order For Dispatch_' lv_date '_' wa_sendmail-em_vkbur '.xls' INTO l_data.
***Xstring to binary
    CREATE OBJECT l_zipper.
    "add file to zip
    CALL METHOD l_zipper->add
      EXPORTING
        name    = l_data "'Pending Order For Dispatch.xls' "filename
        content = lv_xstring.
    "save zip
    CALL METHOD l_zipper->save
      RECEIVING
        zip = lv_xstring.

*  * Convert Xstring into Binary
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = lv_xstring
      TABLES
        binary_tab = lit_binary_content.
*        EXCEPTIONS
*          program_error                     = 1
*          OTHERS                            = 2.


    WRITE sy-datum TO lv_date.
    CLEAR: l_attsubject, l_data.
    CONCATENATE 'Pending Order For Dispatch_' lv_date '_' wa_sendmail-em_vkbur INTO l_data.
    l_attsubject = l_data.
    CLEAR l_data.
* Create Attachment
    TRY.
        lo_document->add_attachment( EXPORTING
                                        i_attachment_type = 'ZIP'
                                        i_attachment_subject = l_attsubject
                                        i_att_content_hex = lit_binary_content  ).
    ENDTRY.

*   *Set Sender
    DATA: lo_sender TYPE REF TO if_sender_bcs VALUE IS INITIAL.
    TRY.
        lo_sender = cl_sapuser_bcs=>create( sy-uname ). "sender is the logged in user
* Set sender to send request
        lo_send_request->set_sender(
        EXPORTING
        i_sender = lo_sender ).
*    CATCH CX_ADDRESS_BCS.
****Catch exception here
    ENDTRY.

* *Set recipient
    DATA: lo_recipient TYPE REF TO if_recipient_bcs VALUE IS INITIAL.
    LOOP AT it_zemail_salesoff INTO wa_zemail_salesoff WHERE em_vkbur = wa_sendmail-em_vkbur.

      IF wa_zemail_salesoff-em_emailid IS INITIAL.
        CONTINUE.
      ELSE.
        CLEAR in_mailid.
        in_mailid = wa_zemail_salesoff-em_emailid.
      ENDIF.

      lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid )." 'santhoshkumar@sphinaxinfosystems.com' ). "Here Recipient is email input p_email
      TRY.
          lo_send_request->add_recipient(
              EXPORTING
              i_recipient = lo_recipient
              i_express = 'X').
*        I_COPY = ' ').
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
      ENDTRY.
    ENDLOOP.

*    DATA: lo_recipient TYPE REF TO if_recipient_bcs VALUE IS INITIAL.
*    lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid )." 'santhoshkumar@sphinaxinfosystems.com' ). "Here Recipient is email input p_email
*    TRY.
*        lo_send_request->add_recipient(
*            EXPORTING
*            i_recipient = lo_recipient
*            i_express = 'X').
**        I_COPY = ' ').
**  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
***Catch exception here
*    ENDTRY.


*Set immediate sending
    TRY.
        CALL METHOD lo_send_request->set_send_immediately
          EXPORTING
            i_send_immediately = 'X'.
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
    ENDTRY.

    TRY.
** Send email
        lo_send_request->send(
        EXPORTING
        i_with_error_screen = 'X' ).
        COMMIT WORK.
        IF sy-subrc = 0.
          WRITE :/ 'Mail sent successfully'.
        ENDIF.
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
*catch exception here
    ENDTRY.
    CLEAR:wa_final,wa_final1,it_final1,it_final,it_attachment,in_mailid,w_text,wa_header,wa_kna1,wa_layout,wa_lips,wa_sendmail,wa_t001w,wa_t005u,wa_vbak,wa_vbak1,wa_vbap,wa_vbfa,wa_vbrk,wa_vbrp,wa_zemail_salesoff,wafinal_sendmail.

***************** END_BUILD_XLS_DATA_TABLE_DETAIL************
  ENDLOOP.

ENDFORM.                    "GET_DAT
