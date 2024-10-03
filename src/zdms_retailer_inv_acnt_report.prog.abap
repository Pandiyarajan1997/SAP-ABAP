
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 23.01.2024
*
*  Requester Name            : Ramakrishnan
*
*  Business Logic            : Reatiler Invoice accounting document posting report
*
*  Released on Date          :
* Hardcoded                  : gv_bukrs = 'DMS1'.
*                              gv_date = '20230401'.
*=======================================================================
REPORT zdms_retailer_inv_acnt_report.
**************Alv sctructure*********
TYPES: BEGIN OF alv,
         checkbox    TYPE char1,
         distributor TYPE zdist,
         dist_name   TYPE zdist_name,
         gsber       TYPE gsber,
         account(10) TYPE c,        "dealer
         reta_name   TYPE zret_name,
         acctyp      TYPE koart,
         blart       TYPE blart,
         refdoc      TYPE xblnr1,   "ref dealer
         fisyear     TYPE gjahr,
         period      TYPE monat,
         docdate     TYPE bldat,
         pdate       TYPE budat,
         gl_account  TYPE saknr,
         glacc_txt   TYPE txt50,
         total       TYPE zdms_retail_acnt-total_amt,
         net         TYPE zdms_retail_acnt-net_amt,
         igst        TYPE zdms_retail_acnt-igst,
         cgst        TYPE zdms_retail_acnt-cgst,
         sgst        TYPE zdms_retail_acnt-sgst,
         igst_gl     TYPE saknr,
         cgst_gl     TYPE saknr,
         sgst_gl     TYPE saknr,
         item_txt    TYPE sgtxt,
         docno       TYPE belnr_d,
         msgtyp      TYPE bapi_mtype,
         message     TYPE string,
       END OF alv.
****************Global data dec*******************
DATA : gt_alv  TYPE TABLE OF alv.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
DATA : gv_date  TYPE budat.
DATA : gv_bukrs TYPE bukrs.
DATA : gv_kunnr TYPE kna1-kunnr.
DATA : gv_doc   TYPE bkpf-blart.
DATA : gv_statu TYPE zdms_retail_acnt-status.
DATA : ls_alv LIKE LINE OF gt_alv.
*******************Selection screen design******************

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS : r3 RADIOBUTTON GROUP g1 USER-COMMAND u1, "upload excel past inv
               r6 RADIOBUTTON GROUP g1,          "past inv data manipulate
               r5 RADIOBUTTON GROUP g1,          "upload excel dr/cr note
               r7 RADIOBUTTON GROUP g1,          "upload excel payment
               r1 RADIOBUTTON GROUP g1,          "customer posting
               r2 RADIOBUTTON GROUP g1,          "business area update
               r4 RADIOBUTTON GROUP g1.          "zdms_deal_opbal table display
SELECTION-SCREEN : END OF BLOCK b1.
SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS : so_kunnr FOR gv_kunnr MODIF ID a,        "distributor
                   so_retai FOR gv_kunnr MODIF ID a,        "retailer
                   so_pdate FOR sy-datum MODIF ID a,        "posting date
                   so_docty FOR gv_doc   MODIF ID a,        "doc type
                   so_statu FOR gv_statu MODIF ID a NO INTERVALS.        "posting date
  PARAMETERS : p_fname TYPE rlgrap-filename MODIF ID a.     "for excel file upload
  PARAMETERS : p_chk3 AS CHECKBOX MODIF ID bl3.
SELECTION-SCREEN : END OF BLOCK b2.

**********Events**********
START-OF-SELECTION.
  IF r1 = abap_true.                    "Display the posting data
********************background job running check****************
    IF sy-batch = abap_true.
************call the posting process**************
      PERFORM : process.
*****************select all check box*********
      CLEAR : ls_alv.
      ls_alv-checkbox = abap_true.
      MODIFY gt_alv FROM ls_alv TRANSPORTING checkbox WHERE checkbox = abap_false.
************call the posting process**************
      SORT gt_alv BY distributor account pdate.
      PERFORM : f_posting.
**************display the result*****************
      PERFORM : alv_display.
    ELSE.
      PERFORM : process.
      PERFORM : alv_display.
    ENDIF.
  ELSEIF r2 = abap_true.                ""business area update
    PERFORM : barea_update.
  ELSEIF r3 = abap_true.                "upload excel past inv
    PERFORM : inv_excel_upload.
  ELSEIF r4 = abap_true.                "zdms_deal_opbal table display
    PERFORM : display.
  ELSEIF r5 = abap_true.                "upload the past cr/dr note excel file
    PERFORM : credit_debit_upload.
  ELSEIF r6 = abap_true.                "invoice manipulate & store in zdms_retailer_acnt
    PERFORM : data_manipulate.
  ELSEIF r7 = abap_true.                "incomming payment excel upload
    PERFORM : payment_upload.
  ENDIF.
  "f4 functionality to file path

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = 'P_FNAME'
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_fname.
***********set the default values*******
INITIALIZATION.
  gv_bukrs = 'DMS1'.
  gv_date  = '20230401'.

  IF sy-tcode = 'ZDMS_ACNT_POST'.
    r1 = 'X'.
  ELSE.
    r3 = 'X'.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  PERFORM screen_adj.
**********Make the file path as mandatory********
AT SELECTION-SCREEN.

  IF ( r3 = 'X' OR r5 = 'X' OR r7 = 'X' ) AND p_fname IS INITIAL AND sy-ucomm = 'ONLI'.

    MESSAGE : 'Please Fill the File Path' TYPE 'E'.

  ENDIF.

END-OF-SELECTION.
*----------------------------------------------------------------------*
FORM inv_excel_upload.

  TYPES : BEGIN OF ty_excel,
            distributor TYPE zdist,
            dist_name   TYPE string,
            dis_gstno   TYPE zdis_gst,
            retailer    TYPE kunnr,
            reta_name   TYPE string,
            re_gstno    TYPE zre_gst,
            inv_type    TYPE zinv_type,
            invoice     TYPE zinvoice_no,
            zdate       TYPE sy-datum,
            mis_ordid   TYPE zorder_id,
            material    TYPE matnr,
            qty         TYPE fkimg,
            net_amt     TYPE netwr,
            tax_amt     TYPE mwsbp,
            total_amt   TYPE ztotal,
          END OF ty_excel.

  DATA : lt_excel TYPE TABLE OF ty_excel,     "final it
         lt_raw   TYPE truxs_t_text_data.
  DATA : ls_dealer TYPE zdms_re_acnt_raw.
****************excel to it**************
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'       "excel to it
    EXPORTING
      i_tab_raw_data       = lt_raw
      i_filename           = p_fname
      i_line_header        = abap_true
    TABLES
      i_tab_converted_data = lt_excel.

  IF lt_excel IS INITIAL.
    MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    SORT lt_excel BY distributor invoice.
***********fetch the datas from zdms_retail_acnt for duplicate entry check***********
    SELECT distributor,retailer,invoice,material FROM zdms_re_acnt_raw INTO TABLE @DATA(lt_check).
    IF sy-subrc = 0.
      SORT lt_check BY distributor retailer invoice material.
    ENDIF.
**************data process**************
    LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fs_exc>).
****************check the duplicate entry****************
      IF lt_check IS NOT INITIAL.
        READ TABLE lt_check TRANSPORTING NO FIELDS WITH KEY distributor = <fs_exc>-distributor
                                                            retailer    = <fs_exc>-retailer
                                                            invoice     = <fs_exc>-invoice
                                                            material    = <fs_exc>-material BINARY SEARCH.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.
      ENDIF.
****************alpha input conversion****************
      <fs_exc>-distributor  = |{ <fs_exc>-distributor ALPHA = IN }|.
      <fs_exc>-retailer     = |{ <fs_exc>-retailer ALPHA = IN }|.
      ls_dealer-distributor = <fs_exc>-distributor.
      ls_dealer-retailer    = <fs_exc>-retailer.
      ls_dealer-invoice     = <fs_exc>-invoice.
      ls_dealer-material    = <fs_exc>-material.
      ls_dealer-dis_gstno   = <fs_exc>-dis_gstno.
      ls_dealer-inv_type    = <fs_exc>-inv_type.
      ls_dealer-mis_ordid   = <fs_exc>-mis_ordid.
      ls_dealer-qty         = <fs_exc>-qty.
      ls_dealer-net_amt     = <fs_exc>-net_amt.
      ls_dealer-total_amt   = <fs_exc>-total_amt.
      ls_dealer-tax_amt     = <fs_exc>-tax_amt.
      ls_dealer-re_gstno    = <fs_exc>-re_gstno.
      ls_dealer-dis_gstno   = <fs_exc>-dis_gstno.
      CONDENSE ls_dealer-re_gstno.
      CONDENSE ls_dealer-dis_gstno.
      ls_dealer-inv_date    = <fs_exc>-zdate.
      ls_dealer-dist_name   = <fs_exc>-dist_name.
      ls_dealer-reta_name   = <fs_exc>-reta_name.
****************data insert***********
      IF ls_dealer-distributor IS NOT INITIAL AND ls_dealer-retailer IS NOT INITIAL AND ls_dealer-invoice IS NOT INITIAL.
        MODIFY zdms_re_acnt_raw FROM ls_dealer.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
      CLEAR : ls_dealer.
    ENDLOOP.
    IF sy-subrc = 0.
      MESSAGE : 'Data uploaded successfully' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.

FORM data_manipulate.

  DATA : ls_dealer TYPE zdms_retail_acnt.
  DATA : lv_fisyear TYPE bapi0002_4-fiscal_year,
         lv_month   TYPE bapi0002_4-fiscal_period,
         l_return   TYPE bapireturn1.

  DATA : lv_msg   TYPE string,
         lv_gl    TYPE  hkont,
         lv_total TYPE  zdms_retail_acnt-total_amt,
         lv_net   TYPE  zdms_retail_acnt-net_amt,
         lv_tax   TYPE  zdms_retail_acnt-igst.

  DATA : lv_refno TYPE zref_doc.

  SELECT * FROM zdms_re_acnt_raw INTO TABLE @DATA(lt_raw) WHERE status IN ( ' ' , 'E' ).
  IF sy-subrc NE 0.
    MESSAGE : 'No data Found' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    SORT lt_raw BY distributor invoice status.
***********fetch the datas from zdms_retail_acnt for duplicate entry check***********
    SELECT distributor,retailer,invoice FROM zdms_retail_acnt INTO TABLE @DATA(lt_check).
    IF sy-subrc = 0.
      SORT lt_check BY distributor retailer invoice.
    ENDIF.
****************fetch the distributor & dealer details************
    SELECT kunnr,name1,werks FROM kna1 INTO TABLE @DATA(lt_kna1).
    IF sy-subrc = 0.
      SORT lt_kna1 BY kunnr.
    ENDIF.
****************fetch the incomming GL details************
    SELECT kunnr,werks,incgl FROM zdms_distb_gl INTO TABLE @DATA(lt_glact).
    IF sy-subrc = 0.
      SORT lt_glact BY kunnr.
    ENDIF.
****************fetch the GL ACNT details************
    SELECT saknr,ktopl,txt50 FROM skat INTO TABLE @DATA(lt_skat).
    IF sy-subrc = 0.
      SORT lt_skat BY saknr ktopl.
    ENDIF.
**************data process**************
    LOOP AT lt_raw ASSIGNING FIELD-SYMBOL(<fs_raw>).

      lv_total = lv_total + <fs_raw>-total_amt.
      lv_net   = lv_net   + <fs_raw>-net_amt.
      lv_tax   = lv_tax   + <fs_raw>-tax_amt.

      AT END OF invoice.
****************check the duplicate entry****************
        IF lt_check IS NOT INITIAL.
          READ TABLE lt_check TRANSPORTING NO FIELDS WITH KEY distributor = <fs_raw>-distributor
                                                              retailer    = <fs_raw>-retailer
                                                              invoice     = <fs_raw>-invoice BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
        ENDIF.
************get the next no for ref_doc**************
        CLEAR lv_refno.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZREF_DOC'
          IMPORTING
            number                  = lv_refno
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
        ls_dealer-ref_doc     = lv_refno.
****************get the distributor plant code****************
        READ TABLE lt_kna1 INTO DATA(ls_kna2) WITH KEY kunnr = <fs_raw>-distributor BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_dealer-b_area = ls_kna2-werks.
        ENDIF.
        IF <fs_raw>-inv_type = 'Sales Invoice'.
          ls_dealer-doc_type = 'RV'.
        ELSEIF <fs_raw>-inv_type = 'Return Invoice'.
          ls_dealer-doc_type = 'VG'.
        ENDIF.
        ls_dealer-distributor = <fs_raw>-distributor.
        ls_dealer-retailer    = <fs_raw>-retailer.
        ls_dealer-invoice     = <fs_raw>-invoice.
        ls_dealer-doc_date    = <fs_raw>-inv_date.
        ls_dealer-pos_date    = <fs_raw>-inv_date.
        ls_dealer-mis_ordid   = <fs_raw>-mis_ordid.
        ls_dealer-net_amt     = lv_net.
        ls_dealer-total_amt   = lv_total.
        ls_dealer-dist_name   = <fs_raw>-dist_name.
        ls_dealer-reta_name   = <fs_raw>-reta_name.
        ls_dealer-erdat       = sy-datum.
        ls_dealer-ernam       = sy-uname.
        ls_dealer-uzeit       = sy-uzeit.
        CONDENSE <fs_raw>-re_gstno.
        CONDENSE <fs_raw>-dis_gstno.
        IF <fs_raw>-re_gstno+(2) = <fs_raw>-dis_gstno+(2).
          lv_tax = lv_tax / 2.
          ls_dealer-cgst = lv_tax.
          ls_dealer-sgst = lv_tax.
        ELSEIF <fs_raw>-re_gstno IS INITIAL.
          SELECT SINGLE regio FROM kna1 INTO @DATA(dist_regio) WHERE kunnr = @<fs_raw>-distributor.
          SELECT SINGLE regio FROM kna1 INTO @DATA(ret_regio)  WHERE kunnr = @<fs_raw>-retailer.
          IF dist_regio = ret_regio.
            lv_tax = lv_tax / 2.
            ls_dealer-cgst = lv_tax.
            ls_dealer-sgst = lv_tax.
          ELSE.
            ls_dealer-igst = lv_tax.
          ENDIF.
        ELSEIF <fs_raw>-re_gstno+(2) NE <fs_raw>-dis_gstno+(2).
          ls_dealer-igst = lv_tax.
        ENDIF.
***********update the error status in table*************
        MODIFY zdms_retail_acnt FROM ls_dealer.
        IF sy-subrc = 0.
          UPDATE zdms_re_acnt_raw SET   status = 'S'
                                  WHERE distributor = <fs_raw>-distributor
                                  AND   retailer    = <fs_raw>-retailer
                                  AND   invoice     = <fs_raw>-invoice.
          COMMIT WORK.
        ELSE.
          UPDATE zdms_re_acnt_raw SET   status = 'E'
                                  WHERE distributor = <fs_raw>-distributor
                                  AND   retailer    = <fs_raw>-retailer
                                  AND   invoice     = <fs_raw>-invoice.
          ROLLBACK WORK.
        ENDIF.
        CLEAR : ls_dealer,lv_tax,lv_total,lv_net.
      ENDAT.
    ENDLOOP.
    IF sy-subrc = 0.
      MESSAGE : 'Data Updated successfully' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.

FORM process.
  DATA : ls_dealer TYPE zdms_retail_acnt.
  DATA : lv_fisyear TYPE bapi0002_4-fiscal_year,
         lv_month   TYPE bapi0002_4-fiscal_period,
         l_return   TYPE bapireturn1.
************loc variable dec**********
  DATA : lv_msg    TYPE  string,
         lv_gl     TYPE  hkont,
         lv_text   TYPE  string,
         lv_sgstgl TYPE  hkont,
         lv_igstgl TYPE  hkont,
         lv_cgstgl TYPE  hkont,
         lv_status TYPE bapi_mtype.

  lv_status = abap_false.
*************customer block check***********
  DATA : lo_block TYPE REF TO zcl_common_check.
  CREATE OBJECT lo_block.
  DATA : lt_block      TYPE TABLE OF zsd_st_cust_block,
         lt_retail_blk TYPE TABLE OF zsd_st_cust_block.

  REFRESH gt_alv.
  SELECT * FROM zdms_retail_acnt INTO TABLE @DATA(lt_dealer) WHERE distributor IN @so_kunnr
                                                             AND   retailer    IN @so_retai
                                                             AND   pos_date    IN @so_pdate
                                                             AND   doc_type    IN @so_docty
                                                             AND   status      EQ @lv_status.
*                                                             AND   status      IN ( ' ' , 'E' ).
  IF sy-subrc NE 0.
    MESSAGE : 'No data Found' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    SORT lt_dealer BY distributor invoice status.
****************fetch the retailer sales details************
    SELECT a~kunnr,a~name1,a~werks,b~loevm,b~vkorg,b~vtweg,b~spart FROM kna1 AS a
                                           INNER JOIN knvv AS b ON a~kunnr = b~kunnr
                                           INTO TABLE @DATA(lt_kna2)
                                           FOR ALL ENTRIES IN @lt_dealer
                                           WHERE a~kunnr = @lt_dealer-retailer.
    IF sy-subrc = 0.
      SORT lt_kna2 BY kunnr vkorg vtweg spart.
    ENDIF.
****************fetch the incomming GL details************
    SELECT kunnr,werks,incgl FROM zdms_distb_gl INTO TABLE @DATA(lt_glact).
    IF sy-subrc = 0.
      SORT lt_glact BY kunnr werks.
    ENDIF.
****************fetch the GL ACNT details************
    SELECT saknr,ktopl,txt50 FROM skat INTO TABLE @DATA(lt_skat).
    IF sy-subrc = 0.
      SORT lt_skat BY saknr ktopl.
    ENDIF.
**************call distributor check*********
    REFRESH : lt_block.
    lo_block->customer_block_check(
      EXPORTING
        bukrs                     = '1000'      " Company Code
        vkorg                     = '1000'      " Sales Organization
      CHANGING
        cust_tab                  = lt_block    " Table Type for Customer Block Check
        ).
    IF lt_block IS NOT INITIAL.
      SORT : lt_block BY kunnr block.
    ENDIF.
**************call customer check*********
    REFRESH : lt_retail_blk.
    lo_block->customer_block_check(
      EXPORTING
        bukrs                     = 'DMS1'      " Company Code
        vkorg                     = 'SDMS'      " Sales Organization
      CHANGING
        cust_tab                  = lt_retail_blk    " Table Type for Customer Block Check
        ).
    IF lt_retail_blk IS NOT INITIAL.
      SORT : lt_retail_blk BY kunnr block.
    ENDIF.
**************data process**************
    LOOP AT lt_dealer ASSIGNING FIELD-SYMBOL(<fs_exc>).
****************check the dealer code****************
      READ TABLE lt_kna2 INTO DATA(ls_kna1) WITH KEY kunnr = <fs_exc>-retailer BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Retailer code is incorrect , { lv_msg }|.
      ENDIF.
****************check the dealer code****************
      READ TABLE lt_kna2 INTO ls_kna1 WITH KEY kunnr = <fs_exc>-retailer
                                               vkorg = 'SDMS'
                                               vtweg = '20'
                                               spart = '10' BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Retailer code is not maintained in SDMS , { lv_msg }|.
      ELSE.
*        IF ls_kna1-loevm = abap_true.
*          lv_msg = | Retailer code is blocked , { lv_msg } |.
*        ENDIF.
      ENDIF.
*****************check retailer block or not*******************
      IF lt_retail_blk IS NOT INITIAL.
        READ TABLE lt_retail_blk TRANSPORTING NO FIELDS WITH KEY kunnr = <fs_exc>-retailer
                                                                 block = abap_true BINARY SEARCH.
        IF sy-subrc = 0.
          lv_msg = | Retailer is blocked , { lv_msg } |.
        ENDIF.
      ENDIF.
***********check the amount***********
      IF <fs_exc>-total_amt IS INITIAL.
        lv_msg = |Amount is zero , { lv_msg }|.
      ENDIF.
*****************check distributor block or not*******************
      IF lt_block IS NOT INITIAL.
        READ TABLE lt_block TRANSPORTING NO FIELDS WITH KEY kunnr = <fs_exc>-distributor
                                                            block = abap_true BINARY SEARCH.
        IF sy-subrc = 0.
          lv_msg = | Distributor is blocked , { lv_msg } |.
        ENDIF.
      ENDIF.
****************check the distributor code****************
      READ TABLE lt_block TRANSPORTING NO FIELDS WITH KEY kunnr = <fs_exc>-distributor BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Distributor code is incorrect , { lv_msg }|.
      ENDIF.
      "Business Area checks
      IF <fs_exc>-b_area IS INITIAL.
        lv_msg = |Business area is missing , { lv_msg }|.
      ENDIF.
      IF <fs_exc>-doc_type IS INITIAL.
        lv_msg = |Doc type is Incorrect , { lv_msg }|.
      ENDIF.
*****************check the GL account***************
      IF <fs_exc>-doc_type = 'VG'.
        lv_gl = '31100001'.
      ELSEIF <fs_exc>-doc_type = 'RV'.
        lv_gl = '31100001'.
      ELSEIF <fs_exc>-doc_type = 'DG'.
**************get the discount & repate gl accnts*********
        IF <fs_exc>-discount = '0'.         "for others
          lv_gl = '42002011'.
        ELSEIF <fs_exc>-discount = '1.5'.   "for 1.5% discount
          lv_gl = '42002013'.
        ELSEIF <fs_exc>-discount = '2.5'.   "for 2.5% discount
          lv_gl = '42002012'.
        ENDIF.

      ELSEIF <fs_exc>-doc_type = 'DR'.
        lv_gl = '31130002'.
      ELSEIF <fs_exc>-doc_type = 'DZ'.
        READ TABLE lt_glact INTO DATA(ls_glact) WITH KEY kunnr = <fs_exc>-distributor
                                                         werks = <fs_exc>-b_area BINARY SEARCH.
        IF sy-subrc = 0.
          lv_gl = ls_glact-incgl.
        ELSE.
          lv_msg = |GL Account not maintained in zdms_distb_gl , { lv_msg }|.
        ENDIF.
      ENDIF.
      lv_gl = |{ lv_gl ALPHA = IN }|.
*      "GL Account existence checks
      READ TABLE lt_skat INTO DATA(ls_skat) WITH KEY saknr = lv_gl
                                                     ktopl = 'SDMS' BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Incorrect GL Account Number , { lv_msg }|.
      ENDIF.
***********check inv posting or cr/dr note posting*************
      CLEAR : lv_sgstgl,lv_cgstgl,lv_igstgl,lv_text.
      IF <fs_exc>-net_amt IS INITIAL.

        IF <fs_exc>-doc_type = 'DZ'.
          lv_text   = |{ <fs_exc>-distributor } - { <fs_exc>-retailer } - { <fs_exc>-invoice } |.
        ELSE.
          lv_text = <fs_exc>-descr.
        ENDIF.

      ELSE.
        lv_sgstgl = '10010001'.
        lv_igstgl = '10010003'.
        lv_cgstgl = '10010002'.
        lv_sgstgl = |{ lv_sgstgl ALPHA = IN }|.
        lv_igstgl = |{ lv_igstgl ALPHA = IN }|.
        lv_cgstgl = |{ lv_cgstgl ALPHA = IN }|.
        lv_text   = |{ <fs_exc>-distributor } - { <fs_exc>-retailer } - { <fs_exc>-invoice } |.
      ENDIF.
*** function module to get fiscal year ***
      CLEAR: lv_fisyear,lv_month,l_return.
      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
        EXPORTING
          companycodeid = gv_bukrs
          posting_date  = <fs_exc>-pos_date
        IMPORTING
          fiscal_year   = lv_fisyear
          fiscal_period = lv_month
          return        = l_return.
      "Reference Document Checks
      IF <fs_exc>-doc_no IS NOT INITIAL AND <fs_exc>-status = 'S'.
        lv_msg = |Already Document with same reference Available , { lv_msg }|.
      ENDIF.

      IF lv_msg IS INITIAL.
        "Adding the Success Msg
        APPEND VALUE #( distributor = <fs_exc>-distributor
                        account     = <fs_exc>-retailer
                        dist_name   = <fs_exc>-dist_name
                        reta_name   = <fs_exc>-reta_name
                        acctyp      = 'D'
                        blart       = <fs_exc>-doc_type
                        refdoc      = <fs_exc>-ref_doc
                        fisyear     = lv_fisyear
                        period      = lv_month
                        docdate     = <fs_exc>-doc_date
                        pdate       = <fs_exc>-pos_date
                        gl_account  = lv_gl
                        glacc_txt   = ls_skat-txt50
                        total       = <fs_exc>-total_amt
                        net         = <fs_exc>-net_amt
                        igst        = <fs_exc>-igst
                        cgst        = <fs_exc>-cgst
                        sgst        = <fs_exc>-sgst
                        sgst_gl     = lv_sgstgl
                        igst_gl     = lv_igstgl
                        cgst_gl     = lv_cgstgl
                        item_txt    = lv_text
                        gsber       = <fs_exc>-b_area
                        msgtyp      = 'S'
                        message     = |No errror in lineitems| ) TO gt_alv.
      ELSE.
        "Adding the Error Msg
        APPEND VALUE #( distributor = <fs_exc>-distributor
                        account     = <fs_exc>-retailer
                        dist_name   = <fs_exc>-dist_name
                        reta_name   = <fs_exc>-reta_name
                        acctyp      = 'D'
                        blart       = <fs_exc>-doc_type
                        refdoc      = <fs_exc>-ref_doc
                        fisyear     = lv_fisyear
                        docdate     = <fs_exc>-pos_date
                        pdate       = <fs_exc>-doc_date
                        gl_account  = lv_gl
                        glacc_txt   = ls_skat-txt50
                        total       = <fs_exc>-total_amt
                        net         = <fs_exc>-net_amt
                        igst        = <fs_exc>-igst
                        cgst        = <fs_exc>-cgst
                        sgst        = <fs_exc>-sgst
                        sgst_gl     = lv_sgstgl
                        igst_gl     = lv_igstgl
                        cgst_gl     = lv_cgstgl
                        item_txt    = lv_text
                        gsber       = <fs_exc>-b_area
                        msgtyp      = 'E'
                        message     = lv_msg ) TO gt_alv.
***********update the error status in table*************
        IF <fs_exc>-doc_no IS NOT INITIAL OR <fs_exc>-total_amt = 0.
          UPDATE zdms_retail_acnt SET   status = 'S' message = lv_msg
                                  WHERE distributor = <fs_exc>-distributor
                                  AND   retailer    = <fs_exc>-retailer
                                  AND   invoice     = <fs_exc>-invoice.
        ELSE.
          UPDATE zdms_retail_acnt SET   status = 'E' message = lv_msg
                                  WHERE distributor = <fs_exc>-distributor
                                  AND   retailer    = <fs_exc>-retailer
                                  AND   invoice     = <fs_exc>-invoice.
        ENDIF.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
      CLEAR : ls_dealer,ls_kna1,ls_skat,lv_msg,lv_gl.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Include          ZDMS_OPENING_BALANCE_POST_CLS
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION. " Variables for events
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function, "Event for User command
      on_link_click FOR EVENT link_click  OF cl_salv_events_table
        IMPORTING
          row,
      on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column.
ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.
  METHOD on_double_click.

    IF column = 'DOCNO'.
      READ TABLE gt_alv INTO DATA(ls_alv) INDEX row.
      IF ls_alv-docno IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_alv-docno.
        SET PARAMETER ID 'BUK' FIELD gv_bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

  ENDMETHOD.
* Implementation
  METHOD on_link_click.
    READ TABLE gt_alv INTO DATA(ls_alv) INDEX row.
    IF ls_alv-checkbox = abap_true.
      ls_alv-checkbox = abap_false.
    ELSE.
      ls_alv-checkbox = abap_true.
    ENDIF.
    MODIFY gt_alv FROM ls_alv INDEX row TRANSPORTING checkbox.
* refresh ALV
    CALL METHOD lo_gr_alv->refresh.
  ENDMETHOD.
ENDCLASS.

FORM alv_display.
  IF gt_alv IS NOT INITIAL.

    SORT gt_alv BY distributor account pdate.

    DATA: lo_gr_functions  TYPE REF TO cl_salv_functions_list.
    DATA: lo_event_handler TYPE REF TO lcl_handle_events, " Variables for events
          lo_events        TYPE REF TO cl_salv_events_table.
    DATA: lo_display       TYPE REF TO cl_salv_display_settings. " Variable for layout settings
    DATA: lo_columns TYPE REF TO cl_salv_columns,
          lo_column  TYPE REF TO cl_salv_column_table.
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.

    lo_gr_alv->set_screen_status(
        pfstatus      =  'POSTING'
        report        =  sy-repid
        set_functions = lo_gr_alv->c_functions_all ).

* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).


* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Register events
    lo_events = lo_gr_alv->get_event( ).
    CREATE OBJECT lo_event_handler.
    SET HANDLER lo_event_handler->on_user_command FOR lo_events.
    SET HANDLER lo_event_handler->on_link_click FOR lo_events.
    SET HANDLER lo_event_handler->on_double_click FOR lo_events.

    TRY.
        lo_column ?= lo_columns->get_column( 'CHECKBOX' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Checkbox' ).
        lo_column->set_medium_text( 'Checkbox' ).
        lo_column->set_short_text( 'Checkbox' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>checkbox_hotspot ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ACCOUNT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Account code' ).
        lo_column->set_medium_text( 'Account' ).
        lo_column->set_short_text( 'Account' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'NAME1' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Name' ).
        lo_column->set_medium_text( 'Name' ).
        lo_column->set_short_text( 'Name' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ACCTYP' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Account type' ).
        lo_column->set_medium_text( 'Account type' ).
        lo_column->set_short_text( 'Acctyp' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'REFDOC' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Reference Doc' ).
        lo_column->set_medium_text( 'Reference' ).
        lo_column->set_short_text( 'Reference' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'DOCDATE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Document date' ).
        lo_column->set_medium_text( 'Document date' ).
        lo_column->set_short_text( 'Docdate' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'PDATE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Posting Date' ).
        lo_column->set_medium_text( 'Posting Date' ).
        lo_column->set_short_text( 'Postdate' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'BLART' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Document Type' ).
        lo_column->set_medium_text( 'Document Type' ).
        lo_column->set_short_text( 'Doctype' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'FISYEAR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Fiscal Year' ).
        lo_column->set_medium_text( 'Fiscal Year' ).
        lo_column->set_short_text( 'Fisyear' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'GL_ACCOUNT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'GL Account' ).
        lo_column->set_medium_text( 'GL Account' ).
        lo_column->set_short_text( 'GL Account' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'GLACC_TXT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'GL Acctext' ).
        lo_column->set_medium_text( 'GL Acctext' ).
        lo_column->set_short_text( 'GL Acctext' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.


    TRY.
        lo_column ?= lo_columns->get_column( 'AMT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Amount' ).
        lo_column->set_medium_text( 'Amount' ).
        lo_column->set_short_text( 'Amount' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MSGTYP' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Msgtyp' ).
        lo_column->set_medium_text( 'Msgtyp' ).
        lo_column->set_short_text( 'Msgtyp' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Message' ).
        lo_column->set_medium_text( 'Message' ).
        lo_column->set_short_text( 'Message' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'IGST_GL' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'IGST_GL' ).
        lo_column->set_medium_text( 'IGST_GL' ).
        lo_column->set_short_text( 'IGST_GL' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'SGST_GL' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'SGST_GL' ).
        lo_column->set_medium_text( 'SGST_GL' ).
        lo_column->set_short_text( 'SGST_GL' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'CGST_GL' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'CGST_GL' ).
        lo_column->set_medium_text( 'CGST_GL' ).
        lo_column->set_short_text( 'CGST_GL' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
***********Call the alv screen************
    lo_gr_alv->display( ).

  ENDIF.
ENDFORM.


FORM handle_user_command USING i_ucomm TYPE salv_de_function.
  DATA : ls_alv TYPE alv.
  CASE i_ucomm.
    WHEN '&INV_POST'.
      PERFORM f_posting.
    WHEN 'ALL'.
      ls_alv-checkbox = abap_true.
      MODIFY gt_alv FROM ls_alv TRANSPORTING checkbox WHERE checkbox = abap_false.
    WHEN 'DALL'.
      ls_alv-checkbox = abap_false.
      MODIFY gt_alv FROM ls_alv TRANSPORTING checkbox WHERE checkbox = abap_true.
  ENDCASE.
* refresh alv
  CALL METHOD lo_gr_alv->refresh.
ENDFORM.
FORM f_posting.
  DATA: lv_msg_text TYPE string.
  DATA: lv_objtyp TYPE bapiache09-obj_type.
  DATA: lv_objkey TYPE bapiache09-obj_key.
  DATA: lv_objsys TYPE bapiache09-obj_sys.
  DATA: lt_glaccount  TYPE TABLE OF bapiacgl09,
        lt_payable    TYPE TABLE OF bapiacap09,
        lt_recievable TYPE TABLE OF bapiacar09,
        lt_curramnt   TYPE TABLE OF bapiaccr09,
        lt_return     TYPE TABLE OF bapiret2.
  DATA: lv_belnr TYPE belnr_d.

  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE msgtyp = 'S' AND checkbox = abap_true.
    DATA(l_headers) = VALUE bapiache09( bus_act    = 'RFBU'
                                        username   = sy-uname
                                        comp_code  = gv_bukrs
                                        doc_date   = <fs_alv>-docdate
                                        pstng_date = <fs_alv>-pdate
                                        ref_doc_no = <fs_alv>-refdoc
                                        fisc_year  = <fs_alv>-fisyear
                                        doc_type   = <fs_alv>-blart ).

    REFRESH: lt_glaccount,lt_curramnt,lt_return,lt_recievable.

    lt_recievable = VALUE #( ( itemno_acc = '1'
                               customer   = <fs_alv>-account
                               item_text  = <fs_alv>-item_txt
                               bus_area   = <fs_alv>-gsber  ) ).
    IF <fs_alv>-igst IS INITIAL AND <fs_alv>-net IS NOT INITIAL.

      lt_glaccount = VALUE #( ( itemno_acc = '2'
                                gl_account = <fs_alv>-gl_account
                                item_text  = <fs_alv>-item_txt
                                bus_area   = <fs_alv>-gsber )
                              ( itemno_acc = '3'
                                gl_account = <fs_alv>-cgst_gl
                                item_text  = <fs_alv>-item_txt
                                bus_area   = <fs_alv>-gsber )
                              ( itemno_acc = '4'
                                gl_account = <fs_alv>-sgst_gl
                                item_text  = <fs_alv>-item_txt
                                bus_area   = <fs_alv>-gsber ) ).

      lt_curramnt = VALUE #( ( itemno_acc = '1'
                               currency   = 'INR'
                               amt_doccur = <fs_alv>-total )
                             ( itemno_acc = '2'
                               currency   = 'INR'
                               amt_doccur = ( <fs_alv>-net ) * -1 )
                             ( itemno_acc = '3'
                               currency   = 'INR'
                               amt_doccur = ( <fs_alv>-cgst ) * -1 )
                             ( itemno_acc = '4'
                               currency   = 'INR'
                               amt_doccur = ( <fs_alv>-sgst ) * -1 ) ).
    ELSEIF <fs_alv>-cgst IS INITIAL AND <fs_alv>-net IS NOT INITIAL.

      lt_glaccount = VALUE #( ( itemno_acc = '2'
                                gl_account = <fs_alv>-gl_account
                                item_text  = <fs_alv>-item_txt
                                bus_area   = <fs_alv>-gsber )
                              ( itemno_acc = '3'
                                gl_account = <fs_alv>-igst_gl
                                item_text  = <fs_alv>-item_txt
                                bus_area   = <fs_alv>-gsber ) ).

      lt_curramnt = VALUE #( ( itemno_acc = '1'
                               currency   = 'INR'
                               amt_doccur = <fs_alv>-total )
                             ( itemno_acc = '2'
                               currency   = 'INR'
                               amt_doccur = ( <fs_alv>-net ) * -1 )
                             ( itemno_acc = '3'
                               currency   = 'INR'
                               amt_doccur = ( <fs_alv>-igst ) * -1 ) ).

    ELSEIF <fs_alv>-cgst IS INITIAL AND <fs_alv>-igst IS INITIAL.

      lt_glaccount = VALUE #( ( itemno_acc = '2'
                                gl_account = <fs_alv>-gl_account
                                item_text  = <fs_alv>-item_txt
                                bus_area   = <fs_alv>-gsber ) ).

      IF <fs_alv>-blart = 'DG' OR <fs_alv>-blart = 'DZ'.            "credit note or incomming payment

        lt_curramnt = VALUE #( ( itemno_acc = '1'
                                 currency   = 'INR'
                                 amt_doccur = ( <fs_alv>-total ) * -1 )
                               ( itemno_acc = '2'
                                 currency   = 'INR'
                                 amt_doccur = <fs_alv>-total  ) ).

      ELSEIF <fs_alv>-blart = 'DR'.          "debit note

        lt_curramnt = VALUE #( ( itemno_acc = '1'
                                 currency   = 'INR'
                                 amt_doccur = <fs_alv>-total )
                               ( itemno_acc = '2'
                                 currency   = 'INR'
                                 amt_doccur = ( <fs_alv>-total ) * -1 ) ).

      ENDIF.
    ENDIF.
    IF p_chk3 EQ abap_true.
      REFRESH : lt_return.
      CLEAR   : lv_objkey,lv_objsys,lv_objtyp.
*** Document Check Before posting ***
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader    = l_headers
        TABLES
          accountgl         = lt_glaccount
          accountreceivable = lt_recievable
          accountpayable    = lt_payable
          currencyamount    = lt_curramnt
          return            = lt_return.
      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        CLEAR lv_msg_text.
        LOOP AT lt_return INTO DATA(l_ret) WHERE ( type = 'E' OR type = 'A' ).
          lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
        ENDLOOP.
        <fs_alv>-msgtyp  = 'E'.
        <fs_alv>-message = lv_msg_text.
        UPDATE zdms_retail_acnt SET   status      = 'E' message = <fs_alv>-message
                                WHERE distributor = <fs_alv>-distributor
                                AND   retailer    = <fs_alv>-account
                                AND   ref_doc     = <fs_alv>-refdoc.
      ELSE.
        <fs_alv>-msgtyp  = 'P'.
        <fs_alv>-message = |Document Ready to Post|.
      ENDIF.
*** process to create Debit note ***
    ELSEIF p_chk3 NE abap_true.

*****************code adding only for DZ****************
*
*      IF <fs_alv>-blart = 'DZ'.
*        cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false metadata = abap_false   data = abap_true ).
*        SUBMIT zfi_dms_incoming_payment_new WITH bldat = <fs_alv>-pdate
*                                            WITH budat = <fs_alv>-pdate
*                                            WITH blart = <fs_alv>-blart
*                                            WITH xblnr = <fs_alv>-refdoc
*                                            WITH newko = <fs_alv>-gl_account
*                                            WITH xtext = <fs_alv>-item_txt
*                                            WITH bukrs = gv_bukrs
*                                            WITH waers = 'INR'
*                                            WITH gjahr = <fs_alv>-fisyear
*                                            WITH monat = <fs_alv>-period
*                                            WITH kunnr = <fs_alv>-account
*                                            WITH wrbtr = <fs_alv>-total
*                                            WITH distb = <fs_alv>-distributor
*                                            WITH gsber = <fs_alv>-gsber AND RETURN.
*        SELECT SINGLE belnr FROM bkpf INTO lv_belnr WHERE xblnr = <fs_alv>-refdoc.
*        IF sy-subrc = 0.
*          <fs_alv>-msgtyp  = 'P'.
*          <fs_alv>-message = 'Document posted successfully'.
*          <fs_alv>-docno = lv_belnr.
*          UPDATE zdms_retail_acnt SET   status      = 'S' message = <fs_alv>-message doc_no = lv_belnr
*                                  WHERE distributor = <fs_alv>-distributor
*                                  AND   retailer    = <fs_alv>-account
*                                  AND   ref_doc     = <fs_alv>-refdoc.
*        ELSE.
*          <fs_alv>-msgtyp  = 'R'.
*          <fs_alv>-message = 'Document not posted'.
*          UPDATE zdms_retail_acnt SET   status      = 'R' message = <fs_alv>-message doc_no = lv_belnr
*                                  WHERE distributor = <fs_alv>-distributor
*                                  AND   retailer    = <fs_alv>-account
*                                  AND   ref_doc     = <fs_alv>-refdoc.
*        ENDIF.
*        CLEAR lv_belnr.
*
*      ELSE.
      REFRESH : lt_return.
      CLEAR   : lv_objkey,lv_objsys,lv_objtyp.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = l_headers
        IMPORTING
          obj_type          = lv_objtyp
          obj_key           = lv_objkey
          obj_sys           = lv_objsys
        TABLES
          accountgl         = lt_glaccount
          accountreceivable = lt_recievable
          accountpayable    = lt_payable
          currencyamount    = lt_curramnt
          return            = lt_return.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        CLEAR lv_msg_text.
        LOOP AT lt_return INTO l_ret WHERE ( type = 'E' OR type = 'A' ).
          lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
        ENDLOOP.
        <fs_alv>-msgtyp  = 'E'.
        <fs_alv>-message = lv_msg_text.
        UPDATE zdms_retail_acnt SET   status      = 'E' message = <fs_alv>-message
                                WHERE distributor = <fs_alv>-distributor
                                AND   retailer    = <fs_alv>-account
                                AND   ref_doc     = <fs_alv>-refdoc.
      ELSE.
        COMMIT WORK AND WAIT.
        CLEAR lv_belnr.
        lv_belnr         = lv_objkey+0(10).
        <fs_alv>-docno   = lv_belnr.
        <fs_alv>-msgtyp  = 'P'.
        <fs_alv>-message = |Document { lv_belnr } Posted successfully|.
        UPDATE zdms_retail_acnt SET   status      = 'S' message = <fs_alv>-message doc_no = lv_belnr
                                WHERE distributor = <fs_alv>-distributor
                                AND   retailer    = <fs_alv>-account
                                AND   ref_doc     = <fs_alv>-refdoc.
      ENDIF.
*      ENDIF.
    ENDIF.
  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE : 'Please select any one of without errror lineitem' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM barea_update.

  SELECT bwkey, bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
  IF sy-subrc = 0.

    SELECT kunnr,werks FROM kna1 INTO TABLE @DATA(lt_kna1)
                       FOR ALL ENTRIES IN @lt_t001k
                       WHERE werks =  @lt_t001k-bwkey
                       AND   kunnr IN @so_kunnr.

    LOOP AT lt_kna1 INTO DATA(ls_kna2).
      UPDATE zdms_retail_acnt SET b_area = ls_kna2-werks WHERE distributor = ls_kna2-kunnr.
    ENDLOOP.
    IF sy-subrc = 0.
      MESSAGE : 'Business Area Updated successfully' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.
FORM display .

  SELECT * FROM zdms_retail_acnt INTO TABLE @DATA(lt_retail) WHERE distributor IN @so_kunnr
                                                             AND   retailer    IN @so_retai
                                                             AND   pos_date    IN @so_pdate
                                                             AND   doc_type    IN @so_docty
                                                             AND   status      IN @so_statu.
  IF sy-subrc = 0.

    DATA : ls_layo TYPE slis_layout_alv.
    ls_layo-colwidth_optimize = abap_true.
***********call the alv screen************
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_structure_name = 'ZDMS_RETAIL_ACNT'
        is_layout        = ls_layo
      TABLES
        t_outtab         = lt_retail.
  ELSE.
    MESSAGE : 'No data Found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.


FORM credit_debit_upload.

  TYPES : BEGIN OF ty_excel,
            zdate       TYPE sy-datum,
            invoice     TYPE char50,
            distributor TYPE zdist,
            dist_name   TYPE string,
            retailer    TYPE kunnr,
            reta_name   TYPE string,
            inv_type    TYPE zinv_type,
            dis_type    TYPE char20,
            total_amt   TYPE ztotal,
            desc        TYPE string,
            status      TYPE string,
          END OF ty_excel.

  DATA : lt_excel TYPE TABLE OF ty_excel,     "final it
         lt_raw   TYPE truxs_t_text_data.
  DATA : ls_dealer TYPE zdms_retail_acnt.
  DATA : lv_refno TYPE zref_doc.
  DATA : lv_string TYPE string.
****************excel to it**************
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'       "excel to it
    EXPORTING
      i_tab_raw_data       = lt_raw
      i_filename           = p_fname
      i_line_header        = abap_true
    TABLES
      i_tab_converted_data = lt_excel.

  IF lt_excel IS INITIAL.
    MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    SORT lt_excel BY distributor invoice.
***********fetch the datas from zdms_retail_acnt for dublicate entry check***********
    SELECT distributor,retailer,invoice FROM zdms_retail_acnt INTO TABLE @DATA(lt_check).
    IF sy-subrc = 0.
      SORT lt_check BY distributor retailer invoice.
    ENDIF.
****************fetch the distributor & dealer details************
    SELECT kunnr,name1,werks FROM kna1 INTO TABLE @DATA(lt_kna1) WHERE werks NE ' '.
    IF sy-subrc = 0.
      SORT lt_kna1 BY kunnr.
    ENDIF.
**************data process**************
    LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fs_exc>).
****************alpha input conversion****************
      <fs_exc>-distributor  = |{ <fs_exc>-distributor ALPHA = IN }|.
      <fs_exc>-retailer     = |{ <fs_exc>-retailer ALPHA = IN }|.
      ls_dealer-distributor = <fs_exc>-distributor.
      ls_dealer-retailer    = <fs_exc>-retailer.
      ls_dealer-invoice     = <fs_exc>-invoice+14(20).
      CONDENSE ls_dealer-invoice NO-GAPS.
****************check the dublicate entry****************
      READ TABLE lt_check TRANSPORTING NO FIELDS WITH KEY distributor = ls_dealer-distributor
                                                          retailer    = ls_dealer-retailer
                                                          invoice     = ls_dealer-invoice BINARY SEARCH.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
************get the next no for ref_doc**************
      CLEAR lv_refno.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZREF_DOC'
        IMPORTING
          number                  = lv_refno
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF <fs_exc>-status IS NOT INITIAL.
        ls_dealer-status  = 'C'.
        ls_dealer-message = 'Doc. is cancelled'.
      ENDIF.
      ls_dealer-ref_doc     = lv_refno.
      ls_dealer-total_amt   = <fs_exc>-total_amt.
      ls_dealer-pos_date    = <fs_exc>-zdate.
      ls_dealer-doc_date    = <fs_exc>-zdate.
      ls_dealer-descr       = <fs_exc>-desc.
      ls_dealer-reta_name   = <fs_exc>-reta_name.
      ls_dealer-dist_name   = <fs_exc>-dist_name.
      ls_dealer-erdat       = sy-datum.
      ls_dealer-ernam       = sy-uname.
      ls_dealer-uzeit       = sy-uzeit.
****************check rebate percentage************
      IF <fs_exc>-dis_type = 'Others'.
        ls_dealer-discount = '0'.
      ELSEIF <fs_exc>-dis_type = 'Discount & Rebate'.
        lv_string = ls_dealer-descr.
        TRANSLATE lv_string TO UPPER CASE.
        SEARCH lv_string FOR '1.5%'.
        IF sy-subrc = 0.
          ls_dealer-discount = '1.5'.
        ELSE.
          SEARCH lv_string FOR '2.5%'.
          IF sy-subrc = 0.
            ls_dealer-discount = '2.5'.
          ENDIF.
        ENDIF.
      ENDIF.
      ls_dealer-mis_ordid   = <fs_exc>-invoice.
**********check the doc type********
      IF <fs_exc>-inv_type = 'Debit'.
        ls_dealer-doc_type    = 'DR'.
      ELSEIF <fs_exc>-inv_type = 'Credit'.
        ls_dealer-doc_type    = 'DG'.
      ENDIF.
****************distributor code business area update****************
      READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_dealer-distributor BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_dealer-b_area = ls_kna1-werks.
      ENDIF.
****************data insert***********
      IF ls_dealer-distributor IS NOT INITIAL AND ls_dealer-retailer IS NOT INITIAL AND ls_dealer-invoice IS NOT INITIAL.
        MODIFY zdms_retail_acnt FROM ls_dealer.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
      CLEAR : ls_dealer.
    ENDLOOP.
    IF sy-subrc = 0.
      MESSAGE : 'Data uploaded successfully' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.

FORM payment_upload.

  TYPES : BEGIN OF ty_excel,
            zdate       TYPE sy-datum,
            distributor TYPE zdist,
            dist_name   TYPE string,
            retailer    TYPE kunnr,
            reta_name   TYPE string,
            invoice     TYPE char50,
            total_amt   TYPE ztotal,
            status      TYPE char20,
          END OF ty_excel.

  DATA : lt_excel TYPE TABLE OF ty_excel,     "final it
         lt_raw   TYPE truxs_t_text_data.
  DATA : ls_dealer TYPE zdms_retail_acnt.
  DATA : lv_refno TYPE zref_doc.
****************excel to it**************
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'       "excel to it
    EXPORTING
      i_tab_raw_data       = lt_raw
      i_filename           = p_fname
      i_line_header        = abap_true
    TABLES
      i_tab_converted_data = lt_excel.

  IF lt_excel IS INITIAL.
    MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    SORT lt_excel BY distributor invoice.
***********fetch the datas from zdms_retail_acnt for dublicate entry check***********
    SELECT distributor,retailer,invoice FROM zdms_retail_acnt INTO TABLE @DATA(lt_check).
    IF sy-subrc = 0.
      SORT lt_check BY distributor retailer invoice.
    ENDIF.
****************fetch the distributor & dealer details************
    SELECT kunnr,name1,werks FROM kna1 INTO TABLE @DATA(lt_kna1) WHERE werks NE ' '.
    IF sy-subrc = 0.
      SORT lt_kna1 BY kunnr.
    ENDIF.
**************data process**************
    LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fs_exc>).
      IF <fs_exc>-status  = 'Cancelled'.
        ls_dealer-status  = 'C'.
        ls_dealer-message = 'Doc. is cancelled'.
      ENDIF.
****************alpha input conversion****************
      <fs_exc>-distributor  = |{ <fs_exc>-distributor ALPHA = IN }|.
      <fs_exc>-retailer     = |{ <fs_exc>-retailer ALPHA = IN }|.
      ls_dealer-distributor = <fs_exc>-distributor.
      ls_dealer-retailer    = <fs_exc>-retailer.
      ls_dealer-invoice     = <fs_exc>-invoice+15(20).
      CONDENSE ls_dealer-invoice NO-GAPS.
****************check the dublicate entry****************
      READ TABLE lt_check TRANSPORTING NO FIELDS WITH KEY distributor = ls_dealer-distributor
                                                          retailer    = ls_dealer-retailer
                                                          invoice     = ls_dealer-invoice BINARY SEARCH.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
************get the next no for ref_doc**************
      CLEAR lv_refno.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZREF_DOC'
        IMPORTING
          number                  = lv_refno
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      ls_dealer-ref_doc     = lv_refno.
      ls_dealer-total_amt   = <fs_exc>-total_amt.
      ls_dealer-pos_date    = <fs_exc>-zdate.
      ls_dealer-doc_date    = <fs_exc>-zdate.
      ls_dealer-mis_ordid   = <fs_exc>-invoice.
      ls_dealer-reta_name   = <fs_exc>-reta_name.
      ls_dealer-dist_name   = <fs_exc>-dist_name.
      ls_dealer-erdat       = sy-datum.
      ls_dealer-ernam       = sy-uname.
      ls_dealer-uzeit       = sy-uzeit.
**********check the doc type********
      ls_dealer-doc_type    = 'DZ'.
****************distributor code business area update****************
      READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_dealer-distributor BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_dealer-b_area = ls_kna1-werks.
      ENDIF.
****************data insert***********
      IF ls_dealer-distributor IS NOT INITIAL AND ls_dealer-retailer IS NOT INITIAL AND ls_dealer-invoice IS NOT INITIAL.
        MODIFY zdms_retail_acnt FROM ls_dealer.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
      CLEAR : ls_dealer.
    ENDLOOP.
    IF sy-subrc = 0.
      MESSAGE : 'Data uploaded successfully' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.

FORM screen_adj.
**********screen adjustment**************
  LOOP AT SCREEN.

    IF sy-tcode = 'ZDMS_ACNT_POST'.
      IF screen-name CS 'R2' OR screen-name CS 'R3' OR screen-name CS 'R5'
                             OR screen-name CS 'R6' OR screen-name CS 'R7'.
        screen-active = 0.
      ENDIF.

    ELSEIF sy-tcode = 'ZDMS_ACNT_UPL'.
      IF screen-name CS 'R1'.
        screen-active = 0.
      ENDIF.
    ENDIF.

    IF r1 = abap_false AND r4 = abap_false AND ( screen-name CS 'SO_RETAI' OR screen-name CS 'SO_PDATE' OR screen-name CS 'SO_DOCTY' ).
      screen-active = 0.
    ENDIF.
    IF r1 = abap_false AND ( screen-name CS 'P_CHK3' ).
      screen-active = 0.
    ENDIF.
    IF ( r6 = 'X' OR r2 = 'X' OR r4 = 'X' OR r1 = 'X' ) AND screen-name CS 'P_FNAME'.
      screen-active = 0.
    ENDIF.
    IF ( r4 = 'X' OR r2 = 'X' OR r1 = 'X' )  AND screen-name CS 'SO_KUNNR'. "enable distributor fields
      screen-active = 1.
    ELSEIF r4 = abap_false AND screen-name CS 'SO_KUNNR' AND r2 = abap_false.  "disable distributor fields
      screen-active = 0.
    ENDIF.
    IF r4 = abap_false AND ( screen-name CS 'SO_STATU' ).
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
