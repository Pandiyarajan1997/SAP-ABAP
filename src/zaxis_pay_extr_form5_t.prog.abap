*----------------------------------------------------------------------*
***INCLUDE ZAXIS_PAY_EXTR_FORM5 .
*======================================================================*
*       text        APPEND RECORDS AND DISPLAY RELATED FORMS
*======================================================================*



*&---------------------------------------------------------------------*
*&      Form  APPEND_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_E01  text
*      -->P_TEXT_E12  text
*      -->P_C_RED_COLOUR  text
*----------------------------------------------------------------------*
FORM append_display  USING    status
                              remarks
                              colour.
  CLEAR : wa_bkpf,wa_bsis.
  READ TABLE g_tab_bkpf INTO wa_bkpf WITH KEY bukrs = bsak1-bukrs
                                              belnr = bsak1-augbl
                                              gjahr = bsak1-gjahr.
  READ TABLE g_tab_bsis INTO wa_bsis WITH KEY bukrs = bsak1-bukrs
                                              belnr = bsak1-augbl
                                              gjahr = bsak1-gjahr.

  READ TABLE g_tab_t012k INTO t012k WITH KEY hbkid = s_hbkid-low.

  MOVE-CORRESPONDING bsak1 TO wa_display.

  wa_display-belnr        = bsak1-augbl.        "PAYMENT DOCUMENT NUMBER
  wa_display-budat        = wa_bkpf-budat.      "PAYMENT POSTING DATE
  wa_display-posted_user  = wa_bkpf-usnam.      "DOC Posted User
  wa_display-hbkid        = t012k-hbkid.        "HOUSE BANK
  wa_display-hktid        = t012k-hktid.        "ACCOUNT ID

  IF p_auto = abap_true. "'X'.
    wa_display-laufd      = reguh-laufd.        "RUN DATE FOR AUTOMATIC PAYMENTS
    wa_display-laufi      = reguh-laufi.        "RUN ID FOR AUTOMATIC PAYMENTS
  ENDIF.

  wa_display-lifnr        = lfa1-lifnr.         "VENDOR CODE
  wa_display-vendor_name  = gv_vendor_name.     "VENDOR NAME
  wa_display-bankn        = gv_acc_no.          "VENDOR ACCOUNT NUMBER
  wa_display-ifsc_code    = gv_ifsc_code.       "VENDOR IFSC CODE
  wa_display-product_code = gv_product_code .   "PAYMENT METHOD
  wa_display-no_of_inv    = gv_no_inv .         "NO OF INVOICES FOR THIS PAYMENT

  IF wa_display-prctr IS INITIAL.
    wa_display-prctr = wa_bsis-prctr.         "profit center
  ENDIF.

  IF wa_display-gsber IS INITIAL.
    wa_display-gsber = wa_bsis-gsber.         "BUSSENESS AREA
  ENDIF.

  wa_display-status       = status .
  wa_display-extr_date    = sy-datum.           "EXTRACTION DATE
  wa_display-extr_time    = sy-uzeit.           "EXTRACTION TIME
  wa_display-extr_user    = sy-uname.           "EXTRACTED USER
  wa_display-remarks      = remarks .
  wa_display-colour       = colour .

  CONDENSE wa_display-remarks.
  APPEND wa_display TO git_display.

ENDFORM.                    " APPEND_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  APPEND_EXCEPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_E01  text
*      -->P_GV_TEXT1  text
*      -->P_C_RED_COLOUR  text
*----------------------------------------------------------------------*
FORM append_exception  USING    status
                                remarks
                                colour.
  CLEAR : wa_bkpf,wa_bsis.
  READ TABLE g_tab_bkpf INTO wa_bkpf WITH KEY bukrs = bsak1-bukrs
                                              belnr = bsak1-augbl
                                              gjahr = bsak1-gjahr.
  READ TABLE g_tab_bsis INTO wa_bsis WITH KEY bukrs = bsak1-bukrs
                                            belnr = bsak1-augbl
                                            gjahr = bsak1-gjahr.
  MOVE-CORRESPONDING bsak1 TO wa_exception.

  wa_exception-belnr        = bsak1-augbl.        "PAYMENT DOCUMENT NUMBER
  wa_exception-budat        = wa_bkpf-budat.      "PAYMENT POSTING DATE
  wa_exception-posted_user  = wa_bkpf-usnam.      "DOC Posted User
  wa_exception-hbkid        = t012k-hbkid.        "HOUSE BANK
  wa_exception-hktid        = t012k-hktid.        "ACCOUNT ID

  IF p_auto = abap_true. "'X'.
    wa_exception-laufd      = reguh-laufd.        "RUN DATE FOR AUTOMATIC PAYMENTS
    wa_exception-laufi      = reguh-laufi.        "RUN ID FOR AUTOMATIC PAYMENTS
  ENDIF.

  wa_exception-lifnr        = lfa1-lifnr.         "VENDOR CODE
  wa_exception-vendor_name  = gv_vendor_name.     "VENDOR NAME
  wa_exception-bankn        = gv_acc_no.          "VENDOR ACCOUNT NUMBER
  wa_exception-ifsc_code    = gv_ifsc_code.       "VENDOR IFSC CODE
  wa_exception-product_code = gv_product_code .   "PAYMENT METHOD
  wa_exception-no_of_inv    = gv_no_inv .         "NO OF INVOICES FOR THIS PAYMENT

  IF wa_exception-prctr IS INITIAL.
    wa_exception-prctr = wa_bsis-prctr.           "profit center
  ENDIF.

  IF wa_exception-gsber IS INITIAL.
    wa_exception-gsber = wa_bsis-gsber.            "BUSSENESS AREA
  ENDIF.

  wa_exception-status       = status .
  wa_exception-extr_date    = sy-datum.           "EXTRACTION DATE
  wa_exception-extr_time    = sy-uzeit.           "EXTRACTION TIME
  wa_exception-extr_user    = sy-uname.           "EXTRACTED USER
  wa_exception-remarks      = remarks .
  wa_exception-colour       = colour .

  CONDENSE wa_exception-remarks.
  APPEND wa_exception TO git_exception.

ENDFORM.                    " APPEND_EXCEPTION
*&---------------------------------------------------------------------*
*&      Form  APPEND_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_log .

  MOVE-CORRESPONDING bsak1 TO wa_log.
  IF bvor-belnr IS NOT INITIAL.
    CLEAR : wa_bkpf.
    READ TABLE g_tab_bkpf INTO wa_bkpf WITH KEY bvorg = bvor-bvorg
                                                bukrs = p_bukrs.
    IF sy-subrc = 0.
      wa_log-belnr        = wa_bkpf-belnr.        "Cross company PAYMENT DOCUMENT NUMBER
      wa_log-bukrs        = wa_bkpf-bukrs.        "Cross company code
    ELSE.
      wa_log-belnr        = bsak1-augbl.        "PAYMENT DOCUMENT NUMBER
    ENDIF. "SY-SUBRC = 0.
    IF bsis IS INITIAL.
      READ TABLE g_tab_bsis INTO bsis WITH KEY belnr = wa_bkpf-belnr
                                               bukrs = wa_bkpf-bukrs
                                               gjahr = wa_bkpf-gjahr.
    ENDIF.
  ELSE.
    wa_log-belnr        = bsak1-augbl.        "PAYMENT DOCUMENT NUMBER
  ENDIF.
  wa_log-budat        = bkpf-budat.         "PAYMENT POSTING DATE
  wa_log-hbkid        = t012k-hbkid.        "HOUSE BANK
  wa_log-hktid        = t012k-hktid.        "ACCOUNT ID
  wa_log-posted_user  = bkpf-usnam.         "DOC Posted User
  wa_log-tcode        = bkpf-tcode.         "DOC Posted Tcode

  IF p_auto = abap_true. "'X'.
    wa_log-laufd      = reguh-laufd.        "RUN DATE FOR AUTOMATIC PAYMENTS
    wa_log-laufi      = reguh-laufi.        "RUN ID FOR AUTOMATIC PAYMENTS
  ENDIF.

  wa_log-lifnr        = lfa1-lifnr.         "VENDOR CODE
  wa_log-vendor_name  = gv_vendor_name.     "VENDOR NAME
  wa_log-bankn        = gv_acc_no.          "VENDOR ACCOUNT NUMBER
  wa_log-ifsc_code    = gv_ifsc_code.       "VENDOR IFSC CODE
  wa_log-product_code = gv_product_code .   "PAYMENT METHOD
  wa_log-no_of_inv    = gv_no_inv .         "NO OF INVOICES FOR THIS PAYMENT

  IF wa_log-prctr IS INITIAL.
    wa_log-prctr = bsis-prctr.              "profit center
  ENDIF.

  IF wa_log-gsber IS INITIAL.
    wa_log-gsber = bsis-gsber.              "BUSSENESS AREA
  ENDIF.


  IF sy-tcode = c_tcode_extr.
    wa_log-seqnr      = 1.                  "NO OF EXTRACTIONS OCCURED FOR THIS PAYMENT
    wa_log-extr_date  = sy-datum.           "EXTRACTION DATE
    wa_log-extr_time  = sy-uzeit.           "EXTRACTION TIME
    wa_log-extr_user  = sy-uname.           "EXTRACTED USER

  ELSEIF sy-tcode = c_tcode_rextr.
    wa_log-rex_date   = sy-datum.           "RE-EXTRACTION DATE
    wa_log-rex_time   = sy-uzeit.           "RE-EXTRACTION DATE
    wa_log-rex_user   = sy-uname.           "RE-EXTRACTED USER

    READ TABLE g_tab_zaxis_tab_ctrltab INTO wa_log_tmp WITH KEY
                                            bukrs = wa_log-bukrs
                                            belnr = wa_log-belnr
                                            gjahr = wa_log-gjahr.

    wa_log-seqnr      = wa_log_tmp-seqnr + 1.  " increment the old sequence no to know the payment was re extracted these many times.
    wa_log-extr_date  = wa_log_tmp-extr_date.
    wa_log-extr_time  = wa_log_tmp-extr_time.
    wa_log-extr_user  = wa_log_tmp-extr_user.
  ENDIF.
  APPEND wa_log TO git_log_table.
  CLEAR : wa_bkpf.
ENDFORM.                    " APPEND_LOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
  CLEAR : git_fcat[].
  SORT git_display BY status.
  PERFORM auto_get_fieldcat  TABLES git_display
                                    git_fcat
                             USING 'GIT_DISPLAY'.
  PERFORM fieldcat_modify.
  PERFORM fill_layout USING  'DISPLAY'.
  PERFORM fill_events USING 'DISPLAY'.
  PERFORM alv TABLES git_display
               USING  'DISPLAY'.
ENDFORM.                    " DISPLAY_ALV

*&---------------------------------------------------------------------*
*&      Form  SELECT_TRANSACTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_transactions .
  CLEAR : git_fcat[].
  PERFORM sel_vendor_trans.
  PERFORM auto_get_fieldcat_sel  TABLES git_sel_display
                                    git_fcat_lvc
                             USING 'GIT_SEL_DISPLAY'.
  PERFORM fieldcat_modify_sel.
  PERFORM fill_layout USING  'SELECT'.

DATA: GV_SPL(20) TYPE C VALUE '()-[]_.,'';:'.  " Store all special characters
DATA: COUNTER TYPE I.


  IF git_sel_display IS NOT INITIAL.
    SELECT lifnr,
           smtp_addr AS email
           FROM lfa1 AS a
           INNER JOIN adr6 AS b ON a~adrnr = b~addrnumber
           INTO TABLE @DATA(lt_lifnr)
           FOR ALL ENTRIES IN @git_sel_display
            WHERE lifnr = @git_sel_display-bene_code.
      DATA(LV_TIMES) = STRLEN( GV_SPL ).
      LOOP AT git_sel_display ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-email = VALUE #( lt_lifnr[ lifnr = <fs>-bene_code ]-email OPTIONAL ).
        COUNTER = 0.
        WHILE COUNTER LT LV_TIMES ."SY-SUBRC EQ 0.
          REPLACE ALL OCCURENCES OF GV_SPL+COUNTER(1) IN <fs>-BENE_NAME WITH ''.
          ADD 1 TO COUNTER.
        ENDWHILE.

      ENDLOOP.
*    ENDIF.
  ENDIF.

  PERFORM alv TABLES git_sel_display
               USING  'SELECT'.
ENDFORM.                    " SELECT_TRANSACTIONS

*&---------------------------------------------------------------------*
*&      Form  AUTO_GET_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_DISPLAY  text
*      -->P_GIT_FACT  text
*      -->P_0677   text
*----------------------------------------------------------------------*
FORM auto_get_fieldcat  TABLES tt_data
                               gt_fieldcat
                        USING pv_alv_tab_name TYPE string.


  DATA : lo_tab_desc   TYPE REF TO cl_abap_structdescr,
         lt_struc_comp TYPE abap_component_tab,
         ls_struc_comp TYPE LINE OF abap_component_tab,
         gs_fieldcat   TYPE slis_fieldcat_alv,
         ls_ddic_dield TYPE dfies,
         lo_elem_descr TYPE REF TO cl_abap_elemdescr.

  CLEAR gt_fieldcat.

  lo_tab_desc ?= cl_abap_tabledescr=>describe_by_data( tt_data ).
  lt_struc_comp = lo_tab_desc->get_components( ).

  LOOP AT lt_struc_comp INTO ls_struc_comp.
    " to check if the field is: (do not display)
    " box field
    " style field
    IF ls_struc_comp-name = wa_layout-box_fieldname .
      CONTINUE.
    ENDIF.

    gs_fieldcat-fieldname = ls_struc_comp-name.     " get field name and tab name.
    gs_fieldcat-tabname   = pv_alv_tab_name.

    " get element descr
    TRY .
        lo_elem_descr ?= ls_struc_comp-type.
      CATCH cx_root.
        CONTINUE.
    ENDTRY.

    lo_elem_descr->get_ddic_field(
                    EXPORTING
                      p_langu = sy-langu
                      RECEIVING
                      p_flddescr = ls_ddic_dield
                    EXCEPTIONS
                      not_found = 1
                      no_ddic_type = 2
                      ).
    gs_fieldcat-seltext_l = ls_ddic_dield-scrtext_l.


    APPEND gs_fieldcat TO gt_fieldcat.
    CLEAR: gs_fieldcat, ls_ddic_dield.
  ENDLOOP.

ENDFORM.                    " AUTO_GET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_modify .
  FIELD-SYMBOLS : <fs_fcat> TYPE slis_fieldcat_alv.
  LOOP AT git_fcat ASSIGNING <fs_fcat>.
    CASE <fs_fcat>-fieldname.
      WHEN 'BELNR'.
        <fs_fcat>-emphasize = abap_true. "'X' .
        <fs_fcat>-hotspot   = abap_true. "'X' .
        <fs_fcat>-key       = abap_true. "'X' .

      WHEN 'COLOUR'.
        <fs_fcat>-no_out = abap_true. "'X' .

      WHEN 'BUKRS'.
        <fs_fcat>-no_out = abap_true. "'X' .
      WHEN 'HBKID'.
        <fs_fcat>-no_out = abap_true. "'X' .
      WHEN 'HKTID'.
        <fs_fcat>-no_out = abap_true. "'X' .


      WHEN 'LAUFD'.
        IF p_auto NE abap_true. " 'X' .
          <fs_fcat>-no_out = abap_true. " 'X' .
        ENDIF.
      WHEN 'LAUFI'.
        IF p_auto NE abap_true. " 'X' .
          <fs_fcat>-no_out = abap_true. " 'X' .
        ENDIF.


      WHEN 'EXTR_DATE'.
        IF p_test EQ abap_true. "'X' .
          <fs_fcat>-no_out = abap_true. "'X' .
        ENDIF.

      WHEN 'EXTR_TIME'.
        IF p_test EQ abap_true. "'X' .
          <fs_fcat>-no_out = abap_true. "'X' .
        ENDIF.

      WHEN 'FILENAME'.
        IF p_test EQ abap_true. "'X' .
          <fs_fcat>-no_out = abap_true. "'X' .
        ENDIF.

      WHEN 'DMBTR'.
        <fs_fcat>-do_sum = abap_true. "'X' .

      WHEN 'LIFNR'.
        <fs_fcat>-emphasize = abap_true. " 'X' .
        <fs_fcat>-hotspot   = abap_true. " 'X' .

      WHEN 'BLART'.
        <fs_fcat>-seltext_l   = 'Doc Type'.

      WHEN 'TCODE'.
        <fs_fcat>-seltext_l   = 'Tcode'.

      WHEN 'NO_OF_INV'.
        <fs_fcat>-seltext_l   = 'Invoices'.

    ENDCASE.
  ENDLOOP.
ENDFORM.                    " FIELDCAT_MODIFY

*&---------------------------------------------------------------------*
*&      Form  AUTO_GET_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_DISPLAY  text
*      -->P_GIT_FACT  text
*      -->P_0677   text
*----------------------------------------------------------------------*
FORM auto_get_fieldcat_sel  TABLES tt_data
                               gt_fieldcat
                        USING pv_alv_tab_name TYPE string.

  DATA : lo_tab_desc      TYPE REF TO cl_abap_structdescr,
         lt_struc_comp    TYPE abap_component_tab,
         lt_struc_comp1   TYPE abap_component_tab,
         ls_struc_comp    TYPE LINE OF abap_component_tab,
         gs_fieldcat      TYPE lvc_s_fcat,
         ls_ddic_dield    TYPE dfies,
         lo_elem_descr    TYPE REF TO cl_abap_elemdescr,
         lt_included_view TYPE abap_component_view_tab,
         ls_included_view TYPE LINE OF abap_component_view_tab,
         lv_tabix(4)      TYPE n.

  CLEAR gt_fieldcat.

  lo_tab_desc ?= cl_abap_tabledescr=>describe_by_data( tt_data ).
  lt_struc_comp = lo_tab_desc->get_components( ).

  lt_struc_comp1[] = lt_struc_comp[]. " take one dummy table and check any includes.

*&---------------------------------------------------------------------*
*       " if any include structures are present or not, IF YES THEN ADD.
*----------------------------------------------------------------------
  lv_tabix = 1.
  LOOP AT lt_struc_comp1 INTO ls_struc_comp WHERE as_include = 'X'.

    ADD 1 TO lv_tabix.

    lo_tab_desc ?= ls_struc_comp-type.
    CALL METHOD lo_tab_desc->get_included_view
      EXPORTING
        p_level  = '1'
      RECEIVING
        p_result = lt_included_view.

    LOOP AT lt_included_view INTO ls_included_view.
      CLEAR : ls_struc_comp.
      MOVE-CORRESPONDING ls_included_view TO ls_struc_comp.
      APPEND ls_struc_comp TO lt_struc_comp.
      ADD 1 TO lv_tabix.
      CLEAR : ls_struc_comp.
    ENDLOOP.
  ENDLOOP.

*&---------------------------------------------------------------------*
*  ADD ALL THE FIELDS DESCRIPTION TO FIELD CAT LOG TABLE.
*----------------------------------------------------------------------*
  LOOP AT lt_struc_comp INTO ls_struc_comp.
    " to check if the field is: (do not display)
    " box field or style field
    IF ls_struc_comp-name = wa_layout_lvc-box_fname .
      CONTINUE.
    ENDIF.

    gs_fieldcat-fieldname = ls_struc_comp-name. " get field name and tab name.
    gs_fieldcat-tabname   = pv_alv_tab_name.
    " get element descr
    TRY .
        lo_elem_descr ?= ls_struc_comp-type.
      CATCH cx_root.
        CONTINUE.
    ENDTRY.

    CALL METHOD lo_elem_descr->get_ddic_field
      EXPORTING
        p_langu      = sy-langu
      RECEIVING
        p_flddescr   = ls_ddic_dield
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3.
    gs_fieldcat-scrtext_l = ls_ddic_dield-scrtext_l.

    APPEND gs_fieldcat TO gt_fieldcat.
    CLEAR: gs_fieldcat, ls_ddic_dield.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM gt_fieldcat COMPARING ALL FIELDS.
ENDFORM.                    " AUTO_GET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_modify_sel.

  DATA: it_dropdown TYPE lvc_t_drop,
        ty_dropdown TYPE lvc_s_drop.
*To create drop down for the field 'Pay method'
* with our own f4 help
  LOOP AT git_pay_mtd INTO wa_pay_mtd.
    ty_dropdown-handle = '1'.
    ty_dropdown-value  = wa_pay_mtd-payment_type.
    APPEND ty_dropdown TO it_dropdown.
    CLEAR : ty_dropdown, wa_pay_mtd.
  ENDLOOP.
  SORT it_dropdown BY value.
  DELETE ADJACENT DUPLICATES FROM it_dropdown COMPARING ALL FIELDS.

  IF ref_grid IS NOT INITIAL.
    CALL METHOD ref_grid->set_drop_down_table
      EXPORTING
        it_drop_down = it_dropdown.
  ENDIF.

  FIELD-SYMBOLS : <fs_fcat> TYPE lvc_s_fcat.
  LOOP AT git_fcat_lvc ASSIGNING <fs_fcat>.
    CASE <fs_fcat>-fieldname.
      WHEN 'BELNR'.
        <fs_fcat>-emphasize = abap_true. " 'X' .
        <fs_fcat>-hotspot   = abap_true. " 'X' .
        <fs_fcat>-key       = abap_true. " 'X' .

      WHEN 'COLOUR'.
        <fs_fcat>-no_out = abap_true. " 'X' .

      WHEN 'BUKRS'.
*        <FS_FCAT>-NO_OUT = 'X' .
      WHEN 'HBKID'.
        <fs_fcat>-no_out = abap_true. " 'X' .
      WHEN 'HKTID'.
        <fs_fcat>-no_out = abap_true. " 'X' .

      WHEN 'LAUFD'.
        IF p_auto NE abap_true. " 'X' .
          <fs_fcat>-no_out = abap_true. " 'X' .
        ENDIF.
      WHEN 'LAUFI'.
        IF p_auto NE abap_true. " 'X' .
          <fs_fcat>-no_out = abap_true. " 'X' .
        ENDIF.

      WHEN 'EXTR_DATE'.
        IF p_test EQ abap_true. " 'X' .
          <fs_fcat>-no_out = abap_true. " 'X' .
        ENDIF.


      WHEN 'EXTR_TIME'.
        IF p_test EQ abap_true. " 'X' .
          <fs_fcat>-no_out = abap_true. " 'X' .
        ENDIF.

      WHEN 'FILENAME'.
        IF p_test EQ abap_true. " 'X' .
          <fs_fcat>-no_out = abap_true. " 'X' .
        ENDIF.

      WHEN 'DMBTR'.
        <fs_fcat>-do_sum = abap_true. " 'X' .

      WHEN 'LIFNR'.
        <fs_fcat>-emphasize = abap_true. " 'X' .
        <fs_fcat>-hotspot   = abap_true. " 'X' .

      WHEN 'AUGBL'.
        <fs_fcat>-emphasize = 'X'.
        <fs_fcat>-key       = 'X'.

      WHEN 'BENE_CODE'.
        <fs_fcat>-scrtext_l   = 'Bene Code'.

      WHEN 'BENE_NAME'.
        <fs_fcat>-scrtext_l   = 'Bene Name'.

      WHEN 'BLART'.
        <fs_fcat>-scrtext_l   = 'Doc Type'.

      WHEN 'TCODE'.
        <fs_fcat>-scrtext_l   = 'Tcode'.

      WHEN 'ACC_NO'.
        <fs_fcat>-scrtext_l   = 'Account No'.
        <fs_fcat>-emphasize   = 'X'.
        <fs_fcat>-outputlen   = 18.
        <fs_fcat>-key         = 'X'.

      WHEN 'IFSC'.
        <fs_fcat>-scrtext_l    = 'IFSC Code' .
        <fs_fcat>-outputlen    = 11 .
        <fs_fcat>-emphasize    = 'X'.
        <fs_fcat>-key          = 'X'.

      WHEN 'PAYMENT_TYPE'.
        <fs_fcat>-scrtext_l   = 'Pay Method' .
        <fs_fcat>-emphasize   = 'X'.
        <fs_fcat>-key         = 'X'.
        IF f_edit_pay_mtd = 'Y'.
          <fs_fcat>-drdn_hndl   = '1'.
        ENDIF.
      WHEN 'CHK_BOX'.
        <fs_fcat>-edit        = 'X'.
        <fs_fcat>-checkbox    = 'X'.
        <fs_fcat>-hotspot     = 'X'.
        <fs_fcat>-scrtext_l   = 'CHK_BOX'.
        <fs_fcat>-key         = 'X'.
      WHEN 'EMAIL'.
        <fs_fcat>-scrtext_l   = 'E-mail Address'.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " FIELDCAT_MODIFY
*&---------------------------------------------------------------------*
*&      Form  fill_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_layout USING flag TYPE string.

  CLEAR : wa_layout,wa_layout_lvc,git_exclude[].
  IF flag = 'SELECT'.
    wa_layout_lvc-cwidth_opt        = abap_true.  ""'X'.
    wa_layout_lvc-info_fname        = 'COLOUR'.
    wa_layout_lvc-stylefname        = 'FIELD_STYLE'.
  ELSE.
    wa_layout-no_input          = abap_true. "'X' .
    wa_layout-colwidth_optimize = abap_true. "'X' .
    wa_layout-info_fieldname    = 'COLOUR'.
  ENDIF.
*  WA_LAYOUT-TOTALS_BEF        = 'Totals'(101).


*--Code added to exclude Menu icons like Mail recipient,ABC Analysis,Graphic
  wa_exclude-fcode = '%SL'.
  APPEND wa_exclude TO git_exclude.
  CLEAR wa_exclude.

  wa_exclude-fcode = '&ABC'.
  APPEND wa_exclude TO git_exclude.
  CLEAR wa_exclude.

  wa_exclude-fcode = '&GRAPH'.
  APPEND wa_exclude TO git_exclude.
  CLEAR wa_exclude.

ENDFORM.                    " LAYOUUT

*&---------------------------------------------------------------------*
*&      Form  FILL_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_events USING flag TYPE string..

  IF flag = 'DISPLAY'.
    CLEAR:wa_events.
    wa_events-name = 'END_OF_LIST'.
    wa_events-form = 'END_OF_LIST1'.
    APPEND wa_events TO git_events.
    CLEAR:wa_events.

  ELSEIF flag = 'EXCEPTION'.

    wa_events-name = 'TOP_OF_PAGE'.
    wa_events-form = 'TOP_OF_PAGE1'.
    APPEND wa_events TO git_events.
    CLEAR:wa_events.

  ENDIF.

ENDFORM.                    " FILL_EVENTS
*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv TABLES tt_display
         USING flag TYPE string..

  IF flag = 'DISPLAY'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program          = sy-repid
        i_callback_html_top_of_page = 'TOP_OF_PAGE1'
        i_callback_user_command     = 'USER_COMMAND'
        is_layout                   = wa_layout
        it_fieldcat                 = git_fcat[]
        it_events                   = git_events[]
        i_default                   = abap_true      "'X'
        i_save                      = abap_true     "'X'
        is_variant                  = ls_variant
        it_excluding                = git_exclude
        i_html_height_top           = 18
        i_html_height_end           = 9
      TABLES
        t_outtab                    = tt_display[]
      EXCEPTIONS
        program_error               = 1
        OTHERS                      = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF flag = 'SELECT'.

    CLEAR : w_total.
    LOOP AT git_sel_display INTO wa_sel_display WHERE chk_box = 'X'.
      w_total = w_total + wa_sel_display-dmbtr.
    ENDLOOP.
    w_total_c = w_total.
    CONDENSE w_total_c NO-GAPS.
    CONCATENATE 'Total Selected Amount =' w_total_c INTO w_title SEPARATED BY space. " Total Selected Amount =

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_bypassing_buffer          = 'X'
        i_buffer_active             = 'X'
        i_callback_program          = sy-repid
        i_callback_pf_status_set    = 'PFSTAT_CHECKER'
        i_callback_user_command     = 'USER_COMMAND_SEL'
        i_callback_top_of_page      = ' '
        i_callback_html_top_of_page = 'TOP_OF_PAGE_SEL'
        i_callback_html_end_of_list = ' '
*       I_STRUCTURE_NAME            =
        i_background_id             = ' '
        i_grid_title                = w_title
*       I_GRID_SETTINGS             =
        is_layout_lvc               = wa_layout_lvc
        it_fieldcat_lvc             = git_fcat_lvc[]
        it_excluding                = git_exclude
        i_html_height_top           = 20
        i_html_height_end           = 14
*       IT_SPECIAL_GROUPS_LVC       =
*       IT_SORT_LVC                 =
*       IT_FILTER_LVC               =
*       IT_HYPERLINK                =
*       IS_SEL_HIDE                 =
        i_default                   = 'X'
        i_save                      = 'X'
        is_variant                  = ls_variant
        it_events                   = git_events[]
      TABLES
        t_outtab                    = tt_display[]
      EXCEPTIONS
        program_error               = 1
        OTHERS                      = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*&---------------------------------------------------------------------*
*    For Back Button
*----------------------------------------------------------------------*
    IF sy-ucomm EQ '&F03'.
      LOOP AT git_sel_display INTO wa_sel_display.
        wa_sel_display-chk_box = ' '.
        MODIFY git_sel_display FROM wa_sel_display.
      ENDLOOP.
    ELSEIF sy-ucomm EQ '&F12'.
      LEAVE PROGRAM.
    ELSEIF sy-ucomm EQ '&F15'.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.

    LOOP AT g_tab_bsak_pymnt INTO wa_bsak.
      READ TABLE git_sel_display INTO wa_sel_display WITH KEY augbl = wa_bsak-augbl
                                                              gjahr = wa_bsak-gjahr
                                                              bukrs = wa_bsak-bukrs
                                                              chk_box = 'X'.
      IF sy-subrc NE 0.
        DELETE g_tab_bsak_pymnt.
      ENDIF.
    ENDLOOP.

    IF g_tab_bsak_pymnt[] IS INITIAL.
      PERFORM pop_up_error USING TEXT-125.    " NO PAYMENTS SELECTED FOR THE EXTRACTION.
    ENDIF.

  ENDIF.
ENDFORM.                    " ALV

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND for Display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM        text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  IF ucomm EQ '&IC1'.

    CASE rs_selfield-fieldname.
      WHEN 'BELNR'.
        READ TABLE git_display INTO wa_display INDEX  rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD wa_display-belnr.
        SET PARAMETER ID 'BUK' FIELD wa_display-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_display-gjahr.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      WHEN 'LIFNR'.
        READ TABLE git_display INTO wa_display INDEX  rs_selfield-tabindex.
        SET PARAMETER ID 'LIF' FIELD wa_display-lifnr.
        SET PARAMETER ID 'BUK' FIELD wa_display-bukrs.

        CALL TRANSACTION 'FK03' . "#EC CI_USAGE_OK[2265093] " Added by <IT-CAR Tool> during Code Remediation

    ENDCASE.
  ENDIF.
ENDFORM.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  PFSTAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXTAB    text
*----------------------------------------------------------------------*
FORM pfstat_checker USING p_extab TYPE slis_t_extab.
  SET PF-STATUS 'SELECT_REC'.
ENDFORM.                    "PFSTAT

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM        text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command_sel USING ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.

  IF ucomm EQ '&IC1'.

    CASE rs_selfield-fieldname.
*&---------------------------------------------------------------------*
*       to select the each check box
*----------------------------------------------------------------------*

      WHEN 'CHK_BOX'.

        PERFORM fieldcat_modify_sel.

        READ TABLE git_sel_display INTO wa_sel_display INDEX  rs_selfield-tabindex.
        IF sy-subrc = 0.
          CASE wa_sel_display-chk_box.
            WHEN 'X'.
              CLEAR : wa_sel_display-chk_box.
              wa_sel_display-colour       = c_grn_colour.
              PERFORM style_change USING abap_false.  " to disable the edit fileds
            WHEN ' '.
              wa_sel_display-chk_box      = 'X'.
              wa_sel_display-colour       = c_grn1_colour.
              PERFORM style_change USING abap_true.  " to enable the edit fileds
          ENDCASE.
          MODIFY git_sel_display FROM wa_sel_display INDEX rs_selfield-tabindex.
        ENDIF.
        PERFORM get_change_data.
        rs_selfield-refresh = 'X'.

    ENDCASE.

*&---------------------------------------------------------------------*
*       for select all
*----------------------------------------------------------------------*
  ELSEIF ucomm EQ 'SELECT'.

    PERFORM fieldcat_modify_sel.
    LOOP AT git_sel_display INTO wa_sel_display.

      wa_sel_display-chk_box      = abap_true.
      wa_sel_display-colour       = c_grn1_colour.

      PERFORM style_change USING abap_true.           " to enable the edit fileds
      MODIFY git_sel_display FROM wa_sel_display.
    ENDLOOP.

    PERFORM get_change_data.
    rs_selfield-refresh = 'X'.

*&---------------------------------------------------------------------*
*       for deselect all
*----------------------------------------------------------------------*
  ELSEIF ucomm EQ 'DESELECT'.

    PERFORM fieldcat_modify_sel.

    LOOP AT git_sel_display INTO wa_sel_display.
      wa_sel_display-chk_box = ' '.
      wa_sel_display-colour       = c_grn_colour.

      PERFORM style_change USING abap_false.      " to disable the edit fileds
      MODIFY git_sel_display FROM wa_sel_display.
    ENDLOOP.

    PERFORM get_change_data.
    rs_selfield-refresh = 'X'.

*&---------------------------------------------------------------------*
*       for Back
*----------------------------------------------------------------------*
  ELSEIF ucomm EQ 'BACK_1' OR ucomm EQ '&F03'.
    LOOP AT git_sel_display INTO wa_sel_display.
      wa_sel_display-chk_box = ' '.
      MODIFY git_sel_display FROM wa_sel_display.
    ENDLOOP.
    rs_selfield-refresh = 'X'.
    LEAVE TO SCREEN 0.

*&---------------------------------------------------------------------*
*       For approve
*----------------------------------------------------------------------*
  ELSEIF ucomm EQ '&DATA_SAVE' OR ucomm EQ 'APPROVE'.

    PERFORM get_change_data.

    LOOP AT g_tab_bsak_pymnt INTO wa_bsak.
      READ TABLE git_sel_display INTO wa_sel_display WITH KEY augbl = wa_bsak-augbl
                                                              gjahr = wa_bsak-gjahr
                                                              bukrs = wa_bsak-bukrs
                                                              chk_box = 'X'.
      IF sy-subrc NE 0.
        DELETE g_tab_bsak_pymnt.
      ENDIF.
    ENDLOOP.

    rs_selfield-refresh = 'X'.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Submit'
*       DIAGNOSE_OBJECT       = ' '
        text_question         = TEXT-532     "'Submit selected records. Are you sure?'
        text_button_1         = 'Yes'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'No'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = ''
*       USERDEFINED_F1_HELP   = ' '
*       START_COLUMN          = 25
*       START_ROW             = 6
*       POPUP_TYPE            =
*       IV_QUICKINFO_BUTTON_1 = ' '
*       IV_QUICKINFO_BUTTON_2 = ' '
      IMPORTING
        answer                = w_answer
*       TABLES
*       PARAMETER             =
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    LEAVE TO SCREEN 0.
  ELSEIF ucomm EQ 'DOWNLOAD'.

    TYPES: BEGIN OF ty_download,
             txn         TYPE string,
             bene_code   TYPE string,
             bene_acno   TYPE string,
             amount      TYPE string,
             bene_name   TYPE string,
             blank1      TYPE string,
             blank2      TYPE string,
             blank3      TYPE string,
             blank4      TYPE string,
             blank5      TYPE string,
             blank6      TYPE string,
             blank7      TYPE string,
             blank8      TYPE string,
             cust_refno  TYPE string,
             payment1    TYPE string,
             payment2    TYPE string,
             payment3    TYPE string,
             payment4    TYPE string,
             payment5    TYPE string,
             payment6    TYPE string,
             payment7    TYPE string,
             blank9      TYPE string,
             post_date   TYPE string,
             blank10     TYPE string,
             ifsc_code   TYPE string,
             bank_name   TYPE string,
             bank_branch TYPE string,
             email       TYPE string,
           END OF ty_download.
    DATA lt_down_file TYPE STANDARD TABLE OF ty_download.
    DATA lw_down_file TYPE ty_download.

    TYPES : BEGIN OF ty_header,
              line(50) TYPE c,
            END OF ty_header.

    DATA : lt_header TYPE STANDARD TABLE OF ty_header,
           lw_header TYPE ty_header.
    lw_header-line        = 'Txn type'.
    APPEND lw_header TO lt_header.
    lw_header-line  = 'Beneficiary Code'.
    APPEND lw_header TO lt_header.
    lw_header-line  = 'Bene A/c No'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'Amount'.
    APPEND lw_header TO lt_header.
    lw_header-line  = 'Beneficiary Name'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line = 'Customer Ref No'.
    APPEND lw_header TO lt_header.
    lw_header-line   = 'Payment Detail 1'.
    APPEND lw_header TO lt_header.
    lw_header-line   = 'Payment Detail 2'.
    APPEND lw_header TO lt_header.
    lw_header-line   = 'Payment Detail 3'.
    APPEND lw_header TO lt_header.
    lw_header-line   = 'Payment Detail 4'.
    APPEND lw_header TO lt_header.
    lw_header-line   = 'Payment Detail 5'.
    APPEND lw_header TO lt_header.
    lw_header-line   = 'Payment Detail 6'.
    APPEND lw_header TO lt_header.
    lw_header-line   = 'Payment Detail 7'.
    APPEND lw_header TO lt_header.
    lw_header-line     = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line  = 'Transaction value date'.
    APPEND lw_header TO lt_header.
    lw_header-line    = 'BLANK'.
    APPEND lw_header TO lt_header.
    lw_header-line  = 'IFSC code'.
    APPEND lw_header TO lt_header.
    lw_header-line  = 'Bene Bank Name'.
    APPEND lw_header TO lt_header.
    lw_header-line = 'Bene Bank Branch Name'.
    APPEND lw_header TO lt_header.
    lw_header-line      = 'Bene Email ID'.
    APPEND lw_header TO lt_header.

    LOOP AT  git_sel_display INTO DATA(lw_data) WHERE chk_box = abap_true.
      IF lw_data-ifsc(4)          = 'HDFC'.
        lw_down_file-txn          = 'I'.
      ELSE.
        lw_down_file-txn          = 'N'.
      ENDIF.

      SHIFT lw_data-bene_code LEFT DELETING LEADING '0'.
      lw_down_file-bene_code      = lw_data-bene_code.
      CONDENSE lw_down_file-bene_code NO-GAPS.
      lw_down_file-bene_acno      = lw_data-acc_no.
      CONDENSE lw_down_file-bene_acno NO-GAPS.
      lw_down_file-amount         = lw_data-dmbtr.
      CONDENSE lw_down_file-amount NO-GAPS.
      lw_down_file-bene_name      = lw_data-bene_name(35).
      CONDENSE lw_down_file-bene_name.
      lw_down_file-blank1         = ''.
      lw_down_file-blank2         = ''.
      lw_down_file-blank3         = ''.
      lw_down_file-blank4         = ''.
      lw_down_file-blank5         = ''.
      lw_down_file-blank6         = ''.
      lw_down_file-blank7         = ''.
      lw_down_file-blank8         = ''.
      lw_down_file-cust_refno     = lw_data-bene_name(20).
      CONDENSE lw_down_file-cust_refno.
      lw_down_file-payment1       = ''.
      lw_down_file-payment2       = ''.
      lw_down_file-payment3       = ''.
      lw_down_file-payment4       = ''.
      lw_down_file-payment5       = ''.
      lw_down_file-payment6       = ''.
      lw_down_file-payment7       = ''.
      lw_down_file-blank9         = ''.

      CONCATENATE lw_data-budat+6(2) '/'
                  lw_data-budat+4(2) '/'
                  lw_data-budat(4)
             INTO lw_down_file-post_date.
      CONDENSE lw_down_file-post_date.

      lw_down_file-blank10    = ''.
      lw_down_file-ifsc_code  = lw_data-ifsc.
      CONDENSE lw_down_file-ifsc_code NO-GAPS.
      lw_down_file-bank_name  = ''.
      lw_down_file-bank_branch = ''.
      lw_down_file-email      = lw_data-email.
      CONDENSE lw_down_file-email NO-GAPS.
      APPEND lw_down_file TO lt_down_file.
    ENDLOOP.
    DATA: lv_filename TYPE string,
          lv_path     TYPE string,
          lv_length   TYPE i,
          lv_fullpath TYPE string.

    lv_filename = | { 'VENDOR PAYMENT TEMPLATE' } |.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title      = 'ENTER FILE NAME'
        default_extension = 'XLSX'
        default_file_name = lv_filename
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath.

    IF lv_fullpath IS NOT INITIAL.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
*         BIN_FILESIZE            = LV_LENGTH
          filename                = lv_fullpath
          filetype                = 'ASC'
          write_field_separator   = 'X'
        TABLES
          data_tab                = lt_down_file
          fieldnames              = lt_header
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.
      IF sy-subrc <> 0.
      ELSE.
        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            document               = lv_fullpath
*           APPLICATION            =
*           PARAMETER              =
*           DEFAULT_DIRECTORY      =
*           MAXIMIZED              =
*           MINIMIZED              =
*           SYNCHRONOUS            =
*           OPERATION              = 'OPEN'
          EXCEPTIONS
            cntl_error             = 1
            error_no_gui           = 2
            bad_parameter          = 3
            file_not_found         = 4
            path_not_found         = 5
            file_extension_unknown = 6
            error_execute_failed   = 7
            synchronous_failed     = 8
            not_supported_by_gui   = 9
            OTHERS                 = 10.
        IF sy-subrc <> 0.
*       IMPLEMENT SUITABLE ERROR HANDLING HERE
        ENDIF.

      ENDIF.
    ENDIF.

  ELSEIF ucomm EQ 'DOWNLOAD1'.
*    TYPES: BEGIN OF ty_text_file,
*             l_string TYPE string,
*           END OF ty_text_file.
    DATA: l_string  TYPE string,
          lt_string TYPE STANDARD TABLE OF string.

    LOOP AT  git_sel_display INTO lw_data  WHERE chk_box = abap_true.
      IF lw_data-ifsc(4)          = 'HDFC'.
        lw_down_file-txn          = 'I'.
      ELSE.
        lw_down_file-txn          = 'N'.
      ENDIF.
      SHIFT lw_data-bene_code LEFT DELETING LEADING '0'.
      lw_down_file-bene_code      = lw_data-bene_code.
      CONDENSE lw_down_file-bene_code NO-GAPS.
      lw_down_file-bene_acno      = lw_data-acc_no.
      CONDENSE lw_down_file-bene_acno NO-GAPS.
      lw_down_file-amount         = lw_data-dmbtr.
      CONDENSE lw_down_file-amount NO-GAPS.
      lw_down_file-bene_name      = lw_data-bene_name(35).
      CONDENSE lw_down_file-bene_name.
      lw_down_file-blank1         = ''.
      lw_down_file-blank2         = ''.
      lw_down_file-blank3         = ''.
      lw_down_file-blank4         = ''.
      lw_down_file-blank5         = ''.
      lw_down_file-blank6         = ''.
      lw_down_file-blank7         = ''.
      lw_down_file-blank8         = ''.
      lw_down_file-cust_refno     = lw_data-bene_name(20).
      CONDENSE lw_down_file-cust_refno.
      lw_down_file-payment1       = ''.
      lw_down_file-payment2       = ''.
      lw_down_file-payment3       = ''.
      lw_down_file-payment4       = ''.
      lw_down_file-payment5       = ''.
      lw_down_file-payment6       = ''.
      lw_down_file-payment7       = ''.
      lw_down_file-blank9         = ''.

      CONCATENATE lw_data-budat+6(2) '/'
                  lw_data-budat+4(2) '/'
                  lw_data-budat(4)
             INTO lw_down_file-post_date.
      CONDENSE lw_down_file-post_date.

      lw_down_file-blank10    = ''.
      lw_down_file-ifsc_code  = lw_data-ifsc.
      CONDENSE lw_down_file-ifsc_code NO-GAPS.
      lw_down_file-bank_name  = ''.
      lw_down_file-bank_branch = ''.
      lw_down_file-email      = lw_data-email.
      CONDENSE lw_down_file-email NO-GAPS.
      CONCATENATE lw_down_file-txn
                  lw_down_file-bene_code
                  lw_down_file-bene_acno
                  lw_down_file-amount
                  lw_down_file-bene_name
                  lw_down_file-blank1
                  lw_down_file-blank2
                  lw_down_file-blank3
                  lw_down_file-blank4
                  lw_down_file-blank5
                  lw_down_file-blank6
                  lw_down_file-blank7
                  lw_down_file-blank8
                  lw_down_file-cust_refno
                  lw_down_file-payment1
                  lw_down_file-payment2
                  lw_down_file-payment3
                  lw_down_file-payment4
                  lw_down_file-payment5
                  lw_down_file-payment6
                  lw_down_file-payment7
                  lw_down_file-blank9
                  lw_down_file-post_date
                  lw_down_file-blank10
                  lw_down_file-ifsc_code
                  lw_down_file-bank_name
                  lw_down_file-bank_branch
                  lw_down_file-email  INTO
                  l_string SEPARATED BY ','.

      APPEND l_string TO lt_string.
    ENDLOOP.

    lv_filename = | { 'EEN72RBI' }{ sy-datum+6 }{ sy-datum+4(2) }|.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title      = 'ENTER FILE NAME'
        default_extension = 'TXT'
        default_file_name = lv_filename
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath.

    IF lv_fullpath IS NOT INITIAL.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
*         BIN_FILESIZE            = LV_LENGTH
          filename                = lv_fullpath
          filetype                = 'ASC'
          write_field_separator   = 'X'
        TABLES
          data_tab                = lt_string
*         fieldnames              = lt_header
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.
      IF sy-subrc <> 0.
      ELSE.
        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            document               = lv_fullpath
*           APPLICATION            =
*           PARAMETER              =
*           DEFAULT_DIRECTORY      =
*           MAXIMIZED              =
*           MINIMIZED              =
*           SYNCHRONOUS            =
*           OPERATION              = 'OPEN'
          EXCEPTIONS
            cntl_error             = 1
            error_no_gui           = 2
            bad_parameter          = 3
            file_not_found         = 4
            path_not_found         = 5
            file_extension_unknown = 6
            error_execute_failed   = 7
            synchronous_failed     = 8
            not_supported_by_gui   = 9
            OTHERS                 = 10.
        IF sy-subrc <> 0.
*       IMPLEMENT SUITABLE ERROR HANDLING HERE
        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.                    "USER_COMMAND FOR SELECT

*&---------------------------------------------------------------------*
*&      Form  top_of_page FOR DISPLAY
*&---------------------------------------------------------------------*
FORM top_of_page1 USING document TYPE REF TO cl_dd_document.

  DATA:
    l_text TYPE sdydo_text_element,
    l_ltx  TYPE t247-ltx,
    l_date TYPE sdydo_text_element.

  DATA: lv_time TYPE char10,
        lv_hr   TYPE string,
        lv_min  TYPE string,
        lv_sec  TYPE string.


  DATA: lv_s_count TYPE string,
        lv_a_count TYPE string,
        lv_e_count TYPE string.

  DATA : c_area TYPE REF TO cl_dd_area.


  CALL METHOD document->initialize_document.
  CALL METHOD document->vertical_split
    EXPORTING
      split_area  = document
      split_width = '75%'
    IMPORTING
      right_area  = c_area.


  IF   p_test = 'X'.

    l_text = 'THIS IS TEST RUN.FILE WILL NOT DOWNLOAD.'.
    PERFORM list_positive USING document
                                l_text
                                cl_dd_area=>large
                                cl_dd_area=>strong
                                cl_dd_area=>list_positive_inv.

    IF git_exception[] IS NOT INITIAL.

      CALL METHOD document->new_line.
      CALL METHOD document->add_icon
        EXPORTING
          sap_icon = 'ICON_LED_RED'.
      CLEAR : l_text.

      l_text = 'Some Documents having the errors please rectify the error or remove the error Documents from this batch.'.
      PERFORM list_positive USING document
                                  l_text
                                  cl_dd_area=>large
                                  ' '
                                  cl_dd_area=>list_negative_inv.
    ELSE.
      CALL METHOD document->new_line.
      CALL METHOD document->add_icon
        EXPORTING
          sap_icon = 'ICON_LED_GREEN'.

      IF g_it_output[] IS NOT INITIAL.
        CLEAR : l_text.
        l_text = 'No Errors in this batch'.
        PERFORM list_positive USING document
                                    l_text
                                    cl_dd_area=>large
                                    ' '
                                    cl_dd_area=>list_positive_inv.

      ELSE.   "G_IT_OUTPUT[] IS NOT INITIAL.
        CLEAR : l_text.
        l_text = 'NO PAYMENTS AVAILABLE TO DOWNLOAD'.

        PERFORM list_positive USING document
                                    l_text
                                    cl_dd_area=>medium
                                    cl_dd_area=>strong
                                    cl_dd_area=>list_positive_inv.
      ENDIF.     "G_IT_OUTPUT[] IS NOT INITIAL.
    ENDIF.      "IF GIT_EXCEPTION[] IS NOT INITIAL.

  ELSE.         " P_TEST = 'X'.

    IF git_exception[] IS INITIAL.
      CLEAR : l_text.
      wa_heading-typ  = 'H'.
      IF sy-tcode = c_tcode_extr.
        l_text = 'EXTRACTED PAYMENTS'.
      ELSE.
        l_text = 'RE-EXTRACTED PAYMENTS'.
      ENDIF.
      PERFORM list_positive USING document
                                  l_text
                                  cl_dd_area=>large
                                  cl_dd_area=>strong
                                  cl_dd_area=>list_positive_inv.

      CALL METHOD document->new_line.
      CALL METHOD document->add_icon
        EXPORTING
          sap_icon = 'ICON_LED_GREEN'.

      IF g_it_output[] IS NOT INITIAL.

        IF sp_download_error NE 'X' AND ftp_download_error NE 'X' AND shared_download_error NE 'X'.

          CLEAR : l_text.
          CONCATENATE 'DOWNLODED FILE:' gv_filename INTO l_text SEPARATED BY space.
          PERFORM list_positive USING document
                                      l_text
                                      cl_dd_area=>medium
                                      cl_dd_area=>strong
                                      cl_dd_area=>list_positive_inv.
        ELSE.
          CLEAR : l_text.
          l_text = 'File not downloded in mentioned location kindly check'.
          PERFORM list_positive USING document
                                      l_text
                                      cl_dd_area=>medium
                                      cl_dd_area=>strong
                                      cl_dd_area=>list_negative_inv.
        ENDIF.

      ELSE.   "G_IT_OUTPUT[] IS NOT INITIAL.

        CLEAR : l_text.
        l_text = 'NO PAYMENTS AVAILABLE TO DOWNLOAD'.
        PERFORM list_positive USING document
                                    l_text
                                    cl_dd_area=>medium
                                    cl_dd_area=>strong
                                    cl_dd_area=>list_positive_inv.

      ENDIF.     "G_IT_OUTPUT[] IS NOT INITIAL.

    ELSE.      "GIT_EXCEPTION[] IS INITIAL.
      l_text = 'FILE NOT DOWNLOADED DUE TO ERROR DOCUMENTS.'.
      PERFORM list_positive USING document
                                  l_text
                                  cl_dd_area=>large
                                  cl_dd_area=>strong
                                  cl_dd_area=>list_negative_inv.

      "Adding new line
      CALL METHOD document->new_line.
      CALL METHOD document->add_icon
        EXPORTING
          sap_icon = 'ICON_LED_RED'.

      CLEAR : l_text.
      l_text = 'Some Documents having the errors please rectify the error or remove the error Documents from this batch.'.
      PERFORM list_positive USING document
                                  l_text
                                  cl_dd_area=>large
                                  ' '
                                  cl_dd_area=>list_negative_inv.

    ENDIF.   "GIT_EXCEPTION[] IS INITIAL.
  ENDIF.    " P_TEST = 'X'.

*&---------------------------------------------------------------------*
*&     SUCCESS AND ERROR
*&---------------------------------------------------------------------*
  "Adding new line
*  CALL METHOD DOCUMENT->NEW_LINE.

  CALL METHOD document->add_gap
    EXPORTING
      width = 50.

  git_success_display[] = git_display[].
  git_error_display[]   = git_display[].
  git_avoid_display[]   = git_display[].


  DELETE git_success_display WHERE status NE TEXT-s01.
  DELETE git_error_display   WHERE status NE TEXT-e01.
  DELETE git_avoid_display   WHERE status NE TEXT-e23.

  DESCRIBE TABLE git_success_display LINES lv_s_count.
  DESCRIBE TABLE git_error_display   LINES lv_e_count.
  DESCRIBE TABLE git_avoid_display   LINES lv_a_count.

  CLEAR : l_text.
  CONCATENATE 'Success:'  lv_s_count ','
              'Error:'  lv_e_count ','
              'Avoided Documents:'  lv_a_count INTO
              l_text SEPARATED BY space.

  "Displaying text
  CALL METHOD document->add_text
    EXPORTING
      text         = l_text
      sap_fontsize = cl_dd_area=>large
      sap_emphasis = cl_dd_area=>emphasis
      sap_color    = cl_dd_area=>list_positive. "LIST_BACKGROUND_INT. "LIST_KEY_INV. "LIST_KEY. "



*&---------------------------------------------------------------------*
*&     create RIGHT SIDE table.
*&---------------------------------------------------------------------*
  DATA : r_dd_table TYPE REF TO cl_dd_table_area.

  CALL METHOD c_area->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '90%'
    IMPORTING
      tablearea     = r_dd_table.


*&---------------------------------------------------------------------*
*&     fill table
*&---------------------------------------------------------------------*
  CALL METHOD r_dd_table->add_text
    EXPORTING
      text = ' '.
  CALL METHOD r_dd_table->new_row.
  CALL METHOD r_dd_table->add_text
    EXPORTING
      text = ' '.
  CALL METHOD r_dd_table->new_row.
  CALL METHOD r_dd_table->add_text
    EXPORTING
      text = ' '.
  CALL METHOD r_dd_table->new_row.
*&---------------------------------------------------------------------*
*&     USER NAME
*&---------------------------------------------------------------------*
  PERFORM right_table USING r_dd_table
                            'USER NAME'
                             sy-uname.

*&---------------------------------------------------------------------*
*&     DATE
*&---------------------------------------------------------------------*
  " Getting Date format.
  SELECT SINGLE ltx
    FROM t247
    INTO l_ltx
   WHERE spras  = sy-langu
     AND mnr    = sy-datum+4(2).
  CONCATENATE sy-datum+6(2) l_ltx sy-datum+0(4) INTO l_date SEPARATED BY '-'.

  PERFORM right_table USING r_dd_table
                            'DATE'
                            l_date.
*&---------------------------------------------------------------------*
*&     TIME
*&---------------------------------------------------------------------*

  lv_time = sy-uzeit .
  lv_hr   = lv_time+0(2) .
  lv_min  = lv_time+2(2) .
  lv_sec  = lv_time+4(2) .
  CONCATENATE lv_hr ':' lv_min ':' lv_sec INTO lv_time .

  PERFORM right_table USING r_dd_table
                            'TIME'
                            lv_time.

*&---------------------------------------------------------------------*
*&     create LEFT SIDE table.
*&---------------------------------------------------------------------*

  DATA : l_dd_table TYPE REF TO cl_dd_table_area.

  CALL METHOD document->add_table
    EXPORTING
      no_of_columns = 2
      border        = '1'
      width         = '20%'
    IMPORTING
      tablearea     = l_dd_table.

*&---------------------------------------------------------------------*
*&     FILL LEFT SIDE table.
*&---------------------------------------------------------------------*

  PERFORM left_table USING l_dd_table
                           'Corporate Code'
                            gv_corp_code.

  PERFORM left_table USING l_dd_table
                           'House Bank'
                            s_hbkid-low.

  PERFORM left_table USING l_dd_table
                           'Acc ID'
                           s_hktid-low.

*  PERFORM LEFT_TABLE USING L_DD_TABLE
*                           'Fiscal Year'
*                           P_GJAHR.

ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  end_of_list FOR DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM end_of_list1 .
  DATA: lv_amt     TYPE string,
        lv_tot_amt TYPE bsak-dmbtr.

  FORMAT COLOR COL_GROUP.


  lv_amt = 'Total Payment Amount'.
  LOOP AT git_display INTO wa_display WHERE status = TEXT-s01.
    lv_tot_amt = lv_tot_amt + wa_display-dmbtr.
  ENDLOOP.


  REFRESH:git_heading[].

  FORMAT COLOR COL_GROUP.
  wa_heading-typ = 'S'.
  wa_heading-info =  lv_amt.
  APPEND wa_heading TO git_heading.
  CLEAR wa_heading.


  FORMAT COLOR COL_GROUP.
  wa_heading-typ = 'S'.
  wa_heading-info = lv_tot_amt.
  APPEND wa_heading TO git_heading.
  CLEAR wa_heading.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = git_heading[].

  REFRESH:git_heading[].
ENDFORM.                    " end_of_list1

*&---------------------------------------------------------------------*
*&      Form  top_of_page FOR SELECT
*&---------------------------------------------------------------------*
FORM top_of_page_sel USING document TYPE REF TO cl_dd_document.

  DATA: l_text TYPE sdydo_text_element,
        l_ltx  TYPE t247-ltx,
        l_date TYPE sdydo_text_element.

  DATA: lv_time TYPE char10,
        lv_hr   TYPE string,
        lv_min  TYPE string,
        lv_sec  TYPE string.

  DATA : c_area TYPE REF TO cl_dd_area.


  CALL METHOD document->initialize_document.
  CALL METHOD document->vertical_split
    EXPORTING
      split_area  = document
      split_width = '75%'
    IMPORTING
      right_area  = c_area.

*&---------------------------------------------------------------------*
*&     create RIGHT SIDE table.
*&---------------------------------------------------------------------*
  DATA : r_dd_table TYPE REF TO cl_dd_table_area.

  CALL METHOD c_area->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '90%'
    IMPORTING
      tablearea     = r_dd_table.

*&---------------------------------------------------------------------*
*&     fill table
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&     USER NAME
*&---------------------------------------------------------------------*
  PERFORM right_table USING r_dd_table
                            'USER NAME'
                             sy-uname.

*&---------------------------------------------------------------------*
*&     DATE
*&---------------------------------------------------------------------*
  " Getting Date format.
  SELECT SINGLE ltx
    FROM t247
    INTO l_ltx
   WHERE spras  = sy-langu
     AND mnr    = sy-datum+4(2).
  CONCATENATE sy-datum+6(2) l_ltx sy-datum+0(4) INTO l_date SEPARATED BY '-'.

  PERFORM right_table USING r_dd_table
                            'DATE'
                            l_date.
*&---------------------------------------------------------------------*
*&     TIME
*&---------------------------------------------------------------------*

  lv_time = sy-uzeit .
  lv_hr  = lv_time+0(2) .
  lv_min = lv_time+2(2) .
  lv_sec = lv_time+4(2) .
  CONCATENATE lv_hr ':' lv_min ':' lv_sec INTO lv_time .

  PERFORM right_table USING r_dd_table
                            'TIME'
                            lv_time.
*&---------------------------------------------------------------------*
*&     create LEFT SIDE table.
*&---------------------------------------------------------------------*
  DATA : l_dd_table TYPE REF TO cl_dd_table_area.

  CALL METHOD document->add_table
    EXPORTING
      no_of_columns = 2
      border        = '1'
      width         = '20%'
    IMPORTING
      tablearea     = l_dd_table.

*&---------------------------------------------------------------------*
*&     FILL LEFT SIDE table.
*&---------------------------------------------------------------------*

  PERFORM left_table USING l_dd_table
                           'Company Code'
                            p_bukrs.

  PERFORM left_table USING l_dd_table
                           'House Bank'
                            s_hbkid-low.

  PERFORM left_table USING l_dd_table
                           'Acc ID'
                           s_hktid-low.
  PERFORM left_table USING l_dd_table
                           'Fiscal Year'
                           p_gjahr.

  PERFORM get_change_data.

ENDFORM.                    "TOP_OF_PAGE_SEL

*&---------------------------------------------------------------------*
*&      Form  RIGHT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_DD_TABLE  text
*      -->P_2863   text
*      -->P_SY_UNAME  text
*----------------------------------------------------------------------*
FORM right_table  USING    r_dd_table TYPE REF TO cl_dd_table_area
                           p_text
                           p_value.

  DATA :  lv_text   TYPE sdydo_text_element.
  DATA :  lv_value  TYPE sdydo_text_element.
  CLEAR : lv_text , lv_value.

  lv_text  = p_text.
  lv_value = p_value.

  CALL METHOD r_dd_table->add_text
    EXPORTING
      text         = lv_text                                "TEXT-H03
      sap_fontsize = cl_dd_area=>medium
      sap_emphasis = cl_dd_area=>strong
      sap_color    = cl_dd_area=>list_group.

  CALL METHOD r_dd_table->add_text
    EXPORTING
      text         = lv_value
      sap_fontsize = cl_dd_area=>medium
*     SAP_EMPHASIS = CL_DD_AREA=>STRONG
      sap_color    = cl_dd_area=>list_group_inv.

  CALL METHOD r_dd_table->new_row.

ENDFORM.                    " RIGHT_TABLE
*&---------------------------------------------------------------------*
*&      Form  LEFT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DD_TABLE  text
*      -->P_2948   text
*      -->P_GV_TIME  text
*----------------------------------------------------------------------*
FORM left_table  USING    l_dd_table TYPE REF TO cl_dd_table_area
                          p_text
                          p_value.

  DATA :  lv_text   TYPE sdydo_text_element.
  DATA :  lv_value  TYPE sdydo_text_element.
  CLEAR : lv_text , lv_value.

  lv_text  = p_text.
  lv_value = p_value.

  CALL METHOD l_dd_table->add_text
    EXPORTING
      text         = lv_text
      sap_fontsize = cl_dd_area=>medium
      sap_color    = cl_dd_area=>list_background_int.

  CALL METHOD l_dd_table->add_text
    EXPORTING
      text         = lv_value
      sap_fontsize = cl_dd_area=>medium
      sap_color    = cl_dd_area=>list_background_int.

  CALL METHOD l_dd_table->new_row.

ENDFORM.                    " LEFT_TABLE
*&---------------------------------------------------------------------*
*&      Form  LIST_POSITIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TEXT  text
*----------------------------------------------------------------------*
FORM list_positive  USING    document TYPE REF TO cl_dd_document
                             p_l_text
                             font_size
                             emphasis
                             color.

  CALL METHOD document->add_text
    EXPORTING
      text         = p_l_text
      sap_fontsize = font_size
      sap_emphasis = emphasis
      sap_color    = color.

ENDFORM.                    " LIST_POSITIVE

*&---------------------------------------------------------------------*
*&      Form  SEL_VENDOR_TRANS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_vendor_trans .
  CLEAR bsak1.
  LOOP AT g_tab_bsak_pymnt INTO bsak1.

    CLEAR :  wa_sel_display, gv_payment, gv_acc_no,gv_ifsc_code,
             bsec,bkpf,lfa1,lfb1,lfbk,bnka.

    MOVE-CORRESPONDING bsak1 TO wa_sel_display.
    IF bsak1-shkzg = 'S' AND bsak1-augbl NE bsak1-belnr.
      wa_sel_display-dmbtr = bsak1-dmbtr * -1.
    ENDIF.

    PERFORM read_header_tables USING bsak1.

    PERFORM read_acc_details.

    PERFORM find_payment_method.

    wa_sel_display-usnam            = bkpf-usnam.
    wa_sel_display-tcode            = bkpf-tcode.
    wa_sel_display-budat            = bkpf-budat.
    wa_sel_display-bene_code        = lfa1-lifnr.
    wa_sel_display-bene_name        = lfa1-name1.
    wa_sel_display-acc_no           = gv_acc_no.
    wa_sel_display-ifsc             = gv_ifsc_code.
    wa_sel_display-payment_type     = gv_payment.
    wa_sel_display-colour           = c_grn_colour.

    APPEND wa_sel_display TO git_sel_display.
    CLEAR : bsak1,bsec,bkpf,lfa1,lfb1,lfbk,bnka, wa_sel_display,gv_acc_no,
            gv_ifsc_code,gv_payment,gv_product_code.
  ENDLOOP.

*&---------------------------------------------------------------------*
*    To disable the Edit option for fields
*----------------------------------------------------------------------*
  LOOP AT git_sel_display INTO wa_sel_display.

    CLEAR : ls_stylerow,wa_sel_display-field_style.
    PERFORM style_change USING abap_false.  " to disable the edit fileds

    MODIFY git_sel_display FROM wa_sel_display.
  ENDLOOP.

*&---------------------------------------------------------------------*
*    Fetch the editable fields
*----------------------------------------------------------------------*
  CLEAR : wa_convers.
  READ TABLE git_convers INTO wa_convers WITH KEY bank_code   = c_bank_code
                                                 record_type = 'H'
                                                 fieldname   = 'VEND'
                                                 oldvalue    = 'EDIT_BANK'.
  IF wa_convers-newvalue IS NOT INITIAL.
    f_edit_bank = wa_convers-newvalue.
  ENDIF.

  CLEAR : wa_convers.
  READ TABLE git_convers INTO wa_convers WITH KEY bank_code   = c_bank_code
                                                 record_type = 'H'
                                                 fieldname   = 'VEND'
                                                 oldvalue    = 'EDIT_PAY_MTD'.
  IF  wa_convers-newvalue IS NOT INITIAL.
    f_edit_pay_mtd = wa_convers-newvalue.
  ENDIF.

ENDFORM.                    " SEL_VENDOR_TRANS

*&---------------------------------------------------------------------*
*&      Form  GET_CHANGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_change_data .

  CLEAR : w_total.
  LOOP AT git_sel_display INTO wa_sel_display WHERE chk_box = 'X'.
    w_total = w_total + wa_sel_display-dmbtr.
  ENDLOOP.
  w_total_c = w_total.
  CONDENSE w_total_c NO-GAPS.
  CONCATENATE 'Total Selected Amount =' w_total_c INTO w_title SEPARATED BY space. " Total Selected Amount =

  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
  ENDIF.
  IF ref_grid IS NOT INITIAL.
    CALL METHOD ref_grid->check_changed_data .

    CALL METHOD ref_grid->set_gridtitle
      EXPORTING
        i_gridtitle = w_title.
  ENDIF.
ENDFORM.                    " GET_CHANGE_DATA

*&---------------------------------------------------------------------*
*&      Form  STYLE_CHANGE
*&---------------------------------------------------------------------*
*    To disable the Edit option for fields
*----------------------------------------------------------------------*
*      -->P_1875   text
*----------------------------------------------------------------------*
FORM style_change  USING    p_value.

  IF p_value = abap_false.

    CLEAR : ls_stylerow,wa_sel_display-field_style.
    ls_stylerow-fieldname = 'ACC_NO' .
    ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
    "set field to disabled
    APPEND ls_stylerow  TO wa_sel_display-field_style.


    CLEAR : ls_stylerow.
    ls_stylerow-fieldname = 'IFSC' .
    ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
    APPEND ls_stylerow  TO wa_sel_display-field_style.


    CLEAR : ls_stylerow.
    ls_stylerow-fieldname = 'PRODUCT_CODE' .
    ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
    APPEND ls_stylerow  TO wa_sel_display-field_style.
  ELSE.
    CLEAR : ls_stylerow,wa_sel_display-field_style.
    ls_stylerow-fieldname = 'ACC_NO' .
    IF f_edit_bank EQ 'Y' AND p_test EQ ' '.
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    APPEND ls_stylerow TO wa_sel_display-field_style.

    CLEAR : ls_stylerow.
    ls_stylerow-fieldname = 'IFSC' .
    IF f_edit_bank EQ 'Y' AND p_test EQ ' '.
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    APPEND ls_stylerow TO wa_sel_display-field_style.

    CLEAR : ls_stylerow.
    ls_stylerow-fieldname = 'PAYMENT_TYPE' .
    IF f_edit_pay_mtd EQ 'Y' AND p_test EQ ' '.
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    APPEND ls_stylerow TO wa_sel_display-field_style.

  ENDIF.
ENDFORM.                    " STYLE_CHANGE
