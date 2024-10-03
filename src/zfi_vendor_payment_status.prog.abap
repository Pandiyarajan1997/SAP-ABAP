*&---------------------------------------------------------------------*
*& Report ZFI_VENDOR_PAYMENT_STATUS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_vendor_payment_status.
*Tables Decleration
TABLES: lfa1,
        zca_invoice_park.

*Types Decleration
TYPES: BEGIN OF ty_status,
         status TYPE char01,
         desc   TYPE char40,
       END OF ty_status.

TYPES: BEGIN OF ty_output,
         bukrs       TYPE  bukrs,
         belnr       TYPE  belnr_d,
         budat       TYPE  budat,
         version     TYPE  zversion1,
         zbelnr      TYPE  zca_de_belnr1,
         zbelnr1     TYPE  zca_de_belnr,
         lifnr       TYPE  lifnr_def,
         refdoc      TYPE  belnr_d,
         refaut      TYPE  zcs_de_ref,
         shkzg       TYPE  shkzg,
         dmbtr       TYPE  dmbtr,
         payment     TYPE  zinvpay,
         reference   TYPE  zreference,
         referdate   TYPE  augdt,
         REFETIME    TYPE  ZCA_DE_TIMS2,
         REFEUNAME   TYPE  ZCA_DE_UNAME2,
         procstype   TYPE  zprocstype,
         payoption   TYPE  zpayopt,
         processed   TYPE  zprocessed,
         zdate       TYPE  zca_de_dats,
         ztime       TYPE  zca_de_tims,
         uname       TYPE  zca_de_uname,
         zextract    TYPE  zextract1,
         zdate1      TYPE  zca_de_dats1,
         ztime1      TYPE  zca_de_tims1,
         uname1      TYPE  zca_de_uname1,
         hbkid       TYPE  hbkid,
         hktid       TYPE  hktid,
         prctr       TYPE  prctr,
         dept        TYPE  zsetvalmin_dept,
         refer_key   TYPE  zsetva_ref_key,
         gsber       TYPE  gsber,
         uniq_id     TYPE  zfi_uniqid,
         name1       TYPE  name1,
         debitacc    TYPE  zfi_debitacc,
         creditacc   TYPE  zfi_creditacc,
         isfc        TYPE  zfi_ifsc,
         amount      TYPE  zel_amount,
         clear_doc   TYPE  belnr_d,
         status      TYPE  zfi_status,
         status_d    TYPE  char25,
         gjahr       TYPE  gjahr,
         erdat       TYPE  erdat,
         uzeit       TYPE  uzeit,
         reqid       TYPE  zfi_reqid,
         urnnumber   TYPE  zfi_urnno,
         status_txt  TYPE  zfi_sttxt,
         transa_txt  TYPE  zfi_trntxt,
         utrno       TYPE  zfi_utrn0,
         tstatus     TYPE  zfi_el_stat,
         remarks     TYPE  zfi_remark,
         utr_upd_flg TYPE  flag,
       END OF ty_output.

DATA: gt_status TYPE TABLE OF ty_status,
      gs_status TYPE ty_status.

DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.

DATA: gt_bank_post TYPE TABLE OF zfi_ven_bnk_post,
      gs_bank_post TYPE zfi_ven_bnk_post.

DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.

DATA: gv_domain TYPE dd07l-domname.
DATA: gt_tab    TYPE TABLE OF dd07v,
      gs_tab    TYPE dd07v.

DATA: gt_variant TYPE disvariant.

*Selection Screen
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr MATCHCODE OBJECT kred_c." OBLIGATORY .
  SELECT-OPTIONS  S_bukrs FOR zca_invoice_park-bukrs." DEFAULT '1000' OBLIGATORY.        "Co. Code
  SELECT-OPTIONS: s_belnr FOR zca_invoice_park-belnr.
  PARAMETERS: p_dcstat TYPE char01 OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_layo LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.

* Get Domain Values for Status field
gv_domain = 'ZFI_DOC_STATUS'.
CALL FUNCTION 'GET_DOMAIN_VALUES'
  EXPORTING
    domname               = gv_domain
*   TEXT                  = 'X'
*   FILL_DD07L_TAB        = ' '
 TABLES
   VALUES_TAB            = gt_tab
*   VALUES_DD07L          =
 EXCEPTIONS
   NO_VALUES_FOUND       = 1
   OTHERS                = 2
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.


  PERFORM get_default_layout CHANGING p_layo.

  IF gt_status[] IS INITIAL.
    CLEAR gs_status.
    gs_status-status = 'A'.
    gs_status-desc = 'Parked for Payment'.
    APPEND gs_status TO gt_status.

    CLEAR gs_status.
    gs_status-status = 'B'.
    gs_status-desc = 'First Approval Completed'.
    APPEND gs_status TO gt_status.

    CLEAR gs_status.
    gs_status-status = 'C'.
    gs_status-desc = 'FInal Approval Completed'.
    APPEND gs_status TO gt_status.

    CLEAR gs_status.
    gs_status-status = 'D'.
    gs_status-desc = 'Payment Pending from Bank'.
    APPEND gs_status TO gt_status.

    CLEAR gs_status.
    gs_status-status = 'E'.
    gs_status-desc = 'Payment Successful'.
    APPEND gs_status TO gt_status.

    CLEAR gs_status.
    gs_status-status = 'F'.
    gs_status-desc = 'Payment Error'.
    APPEND gs_status TO gt_status.

    CLEAR gs_status.
    gs_status-status = 'G'.
    gs_status-desc = 'Error Sending Payment to Bank'.
    APPEND gs_status TO gt_status.

  ENDIF.

AT SELECTION-SCREEN.
  DATA: gx_variant TYPE disvariant.

  IF NOT p_layo IS INITIAL.
    MOVE gt_variant TO gx_variant.
    MOVE: p_layo         TO gx_variant-variant,
          sy-repid       TO gx_variant-report.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = gx_variant.
    gt_variant = gx_variant.
  ELSE.
    CLEAR gt_variant.
    gt_variant-report = sy-repid.
    gt_variant-username = sy-uname.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layo.
  PERFORM select_alv_variant CHANGING p_layo.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dcstat.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = 'STATUS'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'P_DCSTAT'
      value_org       = 'S'
    TABLES
      value_tab       = gt_status
*     FIELD_TAB       =
*     RETURN_TAB      =
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.




START-OF-SELECTION.

  DATA lv_where TYPE string.

  IF p_dcstat = 'A'.
    CLEAR lv_where.
    lv_where = 'uname1 = space'.
  ELSEIF p_dcstat = 'B'.
    CLEAR lv_where.
    lv_where = 'uname1 <> space and refeuname = space and reference = space' .
  ELSEIF p_dcstat = 'C' OR p_dcstat = 'D' OR p_dcstat = 'E' OR p_dcstat = 'F' or p_dcstat = 'G'.
    CLEAR lv_where.
    lv_where = 'REFerence <> space'.
  ENDIF.

  SELECT * FROM zca_invoice_park INTO CORRESPONDING FIELDS OF TABLE gt_output
    WHERE bukrs in s_bukrs AND lifnr IN s_lifnr AND belnr IN s_belnr AND (lv_where).

  IF gt_output[] IS NOT INITIAL.
    SELECT * FROM zfi_ven_bnk_post
      INTO TABLE gt_bank_post
      FOR ALL ENTRIES IN gt_output
     WHERE clear_doc = gt_output-reference.
    IF sy-subrc = 0.
      IF p_dcstat = 'C'.
        DELETE gt_bank_post WHERE status NE '01'.
      ELSEIF p_dcstat = 'D'.
        DELETE gt_bank_post WHERE status NE '02'.
      ELSEIF p_dcstat = 'E'.
        DELETE gt_bank_post WHERE status NE '03'.
      ELSEIF p_dcstat = 'F'.
        DELETE gt_bank_post WHERE status NE '04'.
      ELSEIF p_dcstat = 'G'.
        DELETE gt_bank_post WHERE status NE '06'.
      ENDIF.
    ENDIF.
  ENDIF.

DATA: lv_tabix TYPE sy-tabix.

  loop AT gt_output INTO gs_output.
    clear lv_tabix.
    lv_tabix = sy-tabix.
    READ TABLE GT_BANK_POST INTO GS_BANK_POST with key clear_doc = gs_output-reference.
    IF sy-subrc = 0.
      gs_output-uniq_id = GS_BANK_POST-uniq_id.
      gs_output-name1 = GS_BANK_POST-name1.
      gs_output-debitacc = GS_BANK_POST-debitacc.
      gs_output-CREDITACC = gs_bank_post-CREDITACC.
      gs_output-isfc = GS_BANK_POST-isfc.
      gs_output-amount = GS_BANK_POST-amount.
      gs_output-clear_doc = GS_BANK_POST-clear_doc.
      gs_output-status = GS_BANK_POST-status.
      gs_output-gjahr = GS_BANK_POST-gjahr.
      gs_output-erdat = GS_BANK_POST-erdat.
      gs_output-uzeit = GS_BANK_POST-uzeit.
      gs_output-reqid = GS_BANK_POST-reqid.
      gs_output-urnnumber = GS_BANK_POST-urnnumber.
      gs_output-status_txt = GS_BANK_POST-status_txt.
      gs_output-transa_txt = GS_BANK_POST-transa_txt.
      gs_output-utrno = GS_BANK_POST-utrno.
      gs_output-tstatus = GS_BANK_POST-tstatus.
      gs_output-remarks = GS_BANK_POST-remarks.
      gs_output-utr_upd_flg = GS_BANK_POST-utr_upd_flg.

      clear gs_tab.
      READ TABLE gt_tab INTO gs_tab with key domvalue_l = gs_output-status.
      IF sy-subrc = 0.
        gs_output-status_d = gs_tab-ddtext.
      ENDIF.

      MODIFY gt_output FROM gs_output INDEX LV_TABIX.
    ELSE.
      IF p_dcstat = 'C' or p_dcstat = 'D' or p_dcstat = 'E' or p_dcstat = 'F' or p_dcstat = 'G'.
        DELETE gt_output INDEX lv_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  SELECT a~bukrs a~belnr a~budat a~version a~zbelnr a~zbelnr1 a~lifnr a~refdoc a~refaut a~shkzg a~dmbtr
*         a~payment a~reference a~referdate a~procstype a~payoption a~processed a~zdate a~ztime a~uname
*         a~zextract a~zdate1 a~ztime1 a~uname1 a~hbkid a~hktid a~prctr a~dept a~refer_key a~gsber
*         b~uniq_id b~name1 b~debitacc b~creditacc b~isfc b~amount b~clear_doc b~status b~gjahr b~erdat b~uzeit
*         b~reqid b~urnnumber b~status_txt b~transa_txt b~utrno b~tstatus b~remarks b~utr_upd_flg
*    INTO CORRESPONDING FIELDS OF TABLE gt_output
*    FROM zca_invoice_park AS a LEFT OUTER JOIN zfi_ven_bnk_post AS b ON a~reference = b~clear_doc
*    WHERE a~bukrs = p_bukrs AND a~lifnr IN s_lifnr AND belnr IN s_belnr AND (lv_where).



END-OF-SELECTION.

  REFRESH gt_fcat.

  gs_layout-colwidth_optimize = 'X'.

  PERFORM f_fieldcat USING 'BUKRS'         'Company code'       1 space space space.
  PERFORM f_fieldcat USING 'BELNR'         'Acc Doc Number'     2 space space space.
  PERFORM f_fieldcat USING 'BUDAT'         'Posting Date'       3 space space space.
  PERFORM f_fieldcat USING 'LIFNR'         'Vendor number'      4 space space space.
  PERFORM f_fieldcat USING 'NAME1'         'Vendor name'        5 space space space.
  PERFORM f_fieldcat USING 'REFERENCE'     'Posting Doc No'     6 space space space.
  PERFORM f_fieldcat USING 'ZBELNR'        'Park doc number'    7 space 'X' space.
  PERFORM f_fieldcat USING 'ZBELNR1'       'Post doc number'    8 space 'X' space.
  PERFORM f_fieldcat USING 'SHKZG'         'D/C Indicator'      9 space space space.
  PERFORM f_fieldcat USING 'PAYMENT'       'Invoice Payment'   10 space space space.
  PERFORM f_fieldcat USING 'REFERDATE'     'Reference Date'    11 space space space.
  PERFORM f_fieldcat USING 'PAYOPTION'     'Payment Options'   12 space 'X' space.
  PERFORM f_fieldcat USING 'ZDATE'         'Parked Date'       13 space space space.
  PERFORM f_fieldcat USING 'ZTIME'         'Parked Time'       14 space space space.
  PERFORM f_fieldcat USING 'UNAME'         'Parked By'         15 space space space.
  PERFORM f_fieldcat USING 'ZDATE1'        'Posted Date'       16 space space space.
  PERFORM f_fieldcat USING 'ZTIME1'        'Posted time'       17 space space space.
  PERFORM f_fieldcat USING 'UNAME1'        'Posted By'         18 space space space.
  PERFORM f_fieldcat USING 'HBKID'         'House Bank ID'     19 space space space.
  PERFORM f_fieldcat USING 'UNIQ_ID'       'Uniq id'           20 space space space.
  PERFORM f_fieldcat USING 'DEBITACC'      'Debit Account'     21 space space space.
  PERFORM f_fieldcat USING 'CREDITACC'     'Credit Account'    22 space space space.
  PERFORM f_fieldcat USING 'ISFC'          'IFSC code'         23 space space space.
  PERFORM f_fieldcat USING 'AMOUNT'        'Amount'            24 space space space.
  PERFORM f_fieldcat USING 'STATUS'        'Status'            25 space 'X' space.
  PERFORM f_fieldcat USING 'STATUS_D'      'Status'            26 space space space.
  PERFORM f_fieldcat USING 'UTRNO'         'UTR Number'        27 space space space.
  PERFORM f_fieldcat USING 'GJAHR'         'Fiscal Year'       28 space space space.
  PERFORM f_fieldcat USING 'ERDAT'         'Created Date'      29 space space space.
  PERFORM f_fieldcat USING 'UZEIT'         'Time'              30 space space space.
  PERFORM f_fieldcat USING 'REQID'         'Req ID'            31 space space space.
  PERFORM f_fieldcat USING 'STATUS_TXT'    'Status Des'        32 space space space.
  PERFORM f_fieldcat USING 'TRANSA_TXT'    'Trans status'      33 space space space.
  PERFORM f_fieldcat USING 'REMARKS'       'Remarks'           34 space space space.
  PERFORM f_fieldcat USING 'UTR_UPD_FLG'   'UTR upd Flag'      35 space space 'X'.

***********ALV DISPLAY  *******************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
      i_save             = 'X'
      is_variant         = gt_variant
    TABLES
      t_outtab           = gt_output.



FORM get_default_layout CHANGING cv_layout TYPE disvariant-variant.
  DATA:
    ls_variant TYPE disvariant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = ls_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.
  IF sy-subrc = 0.
    cv_layout = ls_variant-variant.
  ENDIF.
ENDFORM.

FORM select_alv_variant CHANGING cv_layout TYPE disvariant-variant.
  DATA:
    ls_variant TYPE disvariant.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = ls_variant
      i_save        = 'A'
    IMPORTING
      es_variant    = ls_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    cv_layout = ls_variant-variant.
  ENDIF.
ENDFORM.

FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 f_var5 f_var6.
  gs_fcat-fieldname = f_var1.
  gs_fcat-seltext_m = f_var2.
  gs_fcat-col_pos = f_var3.
  gs_fcat-edit = f_var4.
  gs_fcat-no_out = f_var5.
  gs_fcat-tech = f_var6.
  APPEND gs_fcat TO gt_fcat.
  CLEAR gs_fcat.
ENDFORM.
