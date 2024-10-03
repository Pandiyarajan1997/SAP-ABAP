*&---------------------------------------------------------------------*
*& Include          ZSD_SF_CUST_INV_C01
*&---------------------------------------------------------------------*
CLASS lcl_cust DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_final,       "final structure
              select(1)             TYPE  c,
              invoicekey            TYPE  zinvkey,
              bukrs                 TYPE  bukrs,
              status                TYPE  z_sf_status,
              fintype               TYPE  zfincode,
              custno                TYPE  kunnr,
              custname              TYPE  name1_gp,
              invoiceno             TYPE  vbeln,
              invoicedate	          TYPE  fadat,
              invoicetype	          TYPE  fkart,
              gjahr	                TYPE  gjahr,
              pono                  TYPE  ebeln,
              createddate           TYPE  erdat,
              createdtime           TYPE  erzzt,
              createdby             TYPE  ernam,
              basicamount           TYPE  netwr,
              invoiceamount         TYPE  netwr,
              duedate               TYPE  drdat,
              dueamount             TYPE  netwr,
              gstn                  TYPE  jv_gstno,
              pan                   TYPE  j_1ipanno,
              partycode             TYPE  zparty_code,
              inv_file_identifier   TYPE  text25,
              fin_inv_id            TYPE  text20,
              fin_inv_type          TYPE  text10,
              inv_approvedby        TYPE  text100,
              inv_approvedon        TYPE  p25m_aprdt,
              inv_approvedat        TYPE  erzzt,
              cancelledinv          TYPE  vbeln,
              invstatus	            TYPE  zinv_stat,
              filename              TYPE  /sapdmc/mg_filname,
              filesent_date         TYPE  erdat,
              filesent_time         TYPE  erzzt,
              can_filename          TYPE  /sapdmc/mg_filname,
              can_filesent_date     TYPE  erdat,
              can_filesent_time     TYPE  erzzt,
              sheenlacremarks       TYPE  zci_remarks,
              revfile_name          TYPE  /sapdmc/mg_filname,
              revfile_recd_date     TYPE  erdat,
              revfile_recd_time     TYPE  erzzt,
              revfile_remarks       TYPE  zci_remarks,
              can_revfile_name      TYPE  /sapdmc/mg_filname,
              can_revfile_recd_date TYPE  erdat,
              can_revfile_recd_time TYPE  erzzt,
              can_revfile_remarks   TYPE  zci_remarks,
              buyerpartyid          TYPE  text20,
              borrowerid            TYPE  text20,
              desc                  TYPE  string,

            END OF ty_final.


    DATA : lt_final  TYPE TABLE OF ty_final.     "final it
    DATA : lv_alv    TYPE REF TO cl_gui_alv_grid,
           lv_custom TYPE REF TO cl_gui_custom_container.


*methods dec
    METHODS : fetch,     "fetching
      update_10,
      update_20,
      alv,       "display Sf Customer alv
      select_alv_variant CHANGING cv_layout TYPE disvariant-variant.
ENDCLASS.


CLASS lcl_cust IMPLEMENTATION.
  METHOD fetch.

***********fetch the datas from zsd_sf_cust_inv********************
    DATA so_stat TYPE RANGE OF zsd_sf_cust_inv-status.
    IF p_appr = abap_true.
      APPEND VALUE #(  sign = 'I'
                       option = 'EQ'
                       low = '14' ) TO so_stat. " Sent to Finance Status

      APPEND VALUE #(  sign = 'I'
                       option = 'EQ'
                       low = '15' ) TO so_stat. " Sent to Finance Status
*      APPEND VALUE #(  sign = 'I'
*                       option = 'EQ'
*                       low = '10' ) TO so_stat. " Initial Status

    ELSEIF p_status IS NOT INITIAL.
      APPEND VALUE #(  sign = 'I'
                       option = 'EQ'
                       low = p_status ) TO so_stat.
    ENDIF.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_final
      FROM zsd_sf_cust_inv
      WHERE custno IN so_cno
      AND   invoiceno   IN so_invno
      AND   createddate IN so_date
      AND   status      IN so_stat.

    SORT lt_final BY custno status.

    DATA lo_elemdescr TYPE REF TO cl_abap_elemdescr.

    lo_elemdescr ?= cl_abap_elemdescr=>describe_by_name( 'Z_SF_STATUS' ).

    cl_fpm_list_utils=>get_ddic_fixed_values(
      EXPORTING
        io_elem_descr = lo_elemdescr
      IMPORTING
        et_value_set  = DATA(lt_value_set)
    ).
    LOOP AT lt_final ASSIGNING FIELD-SYMBOL(<fs_final>).
      <fs_final>-desc = lt_value_set[ value = <fs_final>-status ]-text .
    ENDLOOP.
*
*    IF sy-subrc = 0.
*
**      MESSAGE TEXT-002 TYPE 'S' .
*
*      CALL METHOD alv.
*
*    ELSE.
*
*      MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
*
*    ENDIF.

  ENDMETHOD.
  METHOD update_10.

    DATA : lt_upd_tab TYPE TABLE OF zsd_sf_cust_inv.

    lv_alv->check_changed_data( ).
    me->alv( ).
    DATA(lt_tab) = lt_final.
    DELETE lt_tab WHERE select = ''.
    IF lt_tab IS NOT INITIAL.
      SELECT * INTO TABLE lt_upd_tab
        FROM zsd_sf_cust_inv
        FOR ALL ENTRIES IN lt_tab
        WHERE invoicekey = lt_tab-invoicekey.
      IF sy-subrc = 0.
        LOOP AT lt_upd_tab ASSIGNING FIELD-SYMBOL(<fs>).
          <fs>-status = '10'.
        ENDLOOP.
        MODIFY zsd_sf_cust_inv FROM TABLE lt_upd_tab.
      ENDIF.
    ELSE.
      MESSAGE 'Please select the records' TYPE 'E' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD update_20.
    DATA : lt_upd_tab TYPE TABLE OF zsd_sf_cust_inv.
    lv_alv->check_changed_data( ).
    me->alv( ).
    DATA(lt_tab) = lt_final.
    DELETE lt_tab WHERE select = ''.
    IF lt_tab IS NOT INITIAL.
      SELECT * INTO TABLE lt_upd_tab
        FROM zsd_sf_cust_inv
        FOR ALL ENTRIES IN lt_tab
        WHERE invoicekey = lt_tab-invoicekey.
      IF sy-subrc = 0.
        LOOP AT lt_upd_tab ASSIGNING FIELD-SYMBOL(<fs>).
          <fs>-inv_approvedon = sy-datum.
          <fs>-inv_approvedat = sy-uzeit.
          <fs>-sheenlacremarks = |Manually changed to Pre-Approval status by { sy-uname } on { sy-datum } |.
          <fs>-status = '20'.
        ENDLOOP.
        MODIFY zsd_sf_cust_inv FROM TABLE lt_upd_tab.
      ENDIF.
    ELSE.
      MESSAGE 'Please select the records' TYPE 'E' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD alv.


    DATA lt_fcat TYPE lvc_t_fcat.   "data dec for fieldcat

    DATA(ls_layo) = VALUE lvc_s_layo( cwidth_opt = abap_true
                                      zebra = abap_true
                                      no_rowmark = abap_true " no_toolbar = 'X'
                                    ).   "data dec for layout design

    DATA(gs_variant) = VALUE disvariant( report   = sy-repid
                                         username = sy-uname
                                         variant  = p_layo ).       "layout variant


    IF lv_custom IS INITIAL.

*    **create object for custom container

      CREATE OBJECT lv_custom
        EXPORTING
          container_name = 'CUSTOM'.  "Name of the Screen CustCtrl Name to Link Container To alv

      IF lv_custom IS NOT INITIAL.

**create the object for alv
        CREATE OBJECT lv_alv
          EXPORTING
            i_parent = lv_custom.

      ENDIF.



      "data append to the fieldcat

      lt_fcat = VALUE #(
      ( col_pos = 1 fieldname = 'SELECT' scrtext_m = 'Select'
                    no_out = abap_true
*                  outputlen = 20
                    checkbox = abap_true
                    edit = abap_true )
      ( col_pos = 2 key = abap_true fieldname = 'INVOICEKEY' scrtext_m = TEXT-004 )
      ( col_pos = 3 fieldname = 'BUKRS' scrtext_m = TEXT-005 )
      ( col_pos = 4 fieldname = 'STATUS' scrtext_m = TEXT-006 )
      ( col_pos = 5 fieldname = 'DESC' scrtext_m = TEXT-050 )
      ( col_pos = 6 fieldname = 'FINTYPE' scrtext_m = TEXT-007 )
      ( col_pos = 7 fieldname = 'CUSTNO' scrtext_m = TEXT-008 )
      ( col_pos = 8 fieldname = 'CUSTNAME'   scrtext_m = TEXT-009 )
      ( col_pos = 9 fieldname = 'INVOICENO'   scrtext_m = TEXT-010 )
      ( col_pos = 10 fieldname = 'INVOICEDATE'   scrtext_m = TEXT-011 )
      ( col_pos = 11 fieldname = 'INVOICETYPE'   scrtext_m = TEXT-012 )
      ( col_pos = 12 fieldname = 'GJAHR'   scrtext_m = TEXT-013 )
      ( col_pos = 13 fieldname = 'PONO'   scrtext_m = TEXT-014 )
      ( col_pos = 14 fieldname = 'CREATEDDATE'   scrtext_m = TEXT-015 )
      ( col_pos = 15 fieldname = 'CREATEDTIME'   scrtext_m = TEXT-016 )
      ( col_pos = 16 fieldname = 'CREATEDBY'   scrtext_m = TEXT-017 )
      ( col_pos = 17 fieldname = 'BASICAMOUNT'   scrtext_m = TEXT-018 )
      ( col_pos = 18 fieldname = 'INVOICEAMOUNT'   scrtext_m = TEXT-019 )
      ( col_pos = 19 fieldname = 'DUEDATE'   scrtext_m = TEXT-020 )
      ( col_pos = 20 fieldname = 'DUEAMOUNT'   scrtext_m = TEXT-021 )
      ( col_pos = 21 fieldname = 'GSTN'   scrtext_m = TEXT-022 )
      ( col_pos = 22 fieldname = 'PAN'   scrtext_m = TEXT-023 )
      ( col_pos = 23 fieldname = 'PARTYCODE'   scrtext_m = TEXT-024 )
      ( col_pos = 24 fieldname = 'INV_FILE_IDENTIFIER'   scrtext_m = TEXT-025 )
      ( col_pos = 25 fieldname = 'FIN_INV_ID'   scrtext_m = TEXT-026 )
      ( col_pos = 26 fieldname = 'FIN_INV_TYPE'   scrtext_m = TEXT-027 )
      ( col_pos = 27 fieldname = 'INV_APPROVEDBY'   scrtext_m = TEXT-028 )
      ( col_pos = 28 fieldname = 'INV_APPROVEDON'   scrtext_m = TEXT-029 )
      ( col_pos = 29 fieldname = 'INV_APPROVEDAT'   scrtext_m = TEXT-030 )
      ( col_pos = 30 fieldname = 'CANCELLEDINV'   scrtext_m = TEXT-031 )
      ( col_pos = 31 fieldname = 'INVSTATUS'   scrtext_m = TEXT-032 )
      ( col_pos = 32 fieldname = 'FILENAME'   scrtext_m = TEXT-033 )
      ( col_pos = 33 fieldname = 'FILESENT_DATE'   scrtext_m = TEXT-034 )
      ( col_pos = 34 fieldname = 'FILESENT_TIME'   scrtext_m = TEXT-035 )
      ( col_pos = 35 fieldname = 'CAN_FILENAME'   scrtext_m = TEXT-036 )
      ( col_pos = 36 fieldname = 'CAN_FILESENT_DATE'   scrtext_m = TEXT-037 )
      ( col_pos = 37 fieldname = 'CAN_FILESENT_TIME'   scrtext_m = TEXT-038 )
      ( col_pos = 38 fieldname = 'SHEENLACREMARKS'   scrtext_m = TEXT-039 )
      ( col_pos = 39 fieldname = 'REVFILE_NAME'   scrtext_m = TEXT-040 )
      ( col_pos = 40 fieldname = 'REVFILE_RECD_DATE'   scrtext_m = TEXT-041 )
      ( col_pos = 41 fieldname = 'REVFILE_RECD_TIME'   scrtext_m = TEXT-042 )
      ( col_pos = 42 fieldname = 'REVFILE_REMARKS'   scrtext_m = TEXT-043 )
      ( col_pos = 43 fieldname = 'CAN_REVFILE_NAME'   scrtext_m = TEXT-044 )
      ( col_pos = 44 fieldname = 'CAN_REVFILE_RECD_DATE'   scrtext_m = TEXT-045 )
      ( col_pos = 45 fieldname = 'CAN_REVFILE_RECD_TIME'   scrtext_m = TEXT-046 )
      ( col_pos = 46 fieldname = 'CAN_REVFILE_REMARKS'   scrtext_m = TEXT-047 )
      ( col_pos = 47 fieldname = 'BUYERPARTYID'   scrtext_m = TEXT-048 )
      ( col_pos = 48 fieldname = 'BORROWERID'   scrtext_m = TEXT-049 ) ).

      READ TABLE lt_fcat ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY fieldname = 'SELECT'.
      IF sy-subrc = 0.
        IF p_appr = abap_true.
          <fs>-no_out = abap_false.
        ENDIF.
      ENDIF.
      DATA: gr_exclude TYPE ui_functions.
      gr_exclude = VALUE #( ( cl_gui_alv_grid=>mc_fc_refresh )
                            ( cl_gui_alv_grid=>mc_fc_loc_cut )
                            ( cl_gui_alv_grid=>mc_fc_loc_copy  )
                            ( cl_gui_alv_grid=>mc_fc_loc_paste )
                            ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_undo )
                            ( cl_gui_alv_grid=>mc_fc_check )
                            ( cl_gui_alv_grid=>mc_fc_graph )
                            ( cl_gui_alv_grid=>mc_fc_info )
                            ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_delete_row  )
                            ( cl_gui_alv_grid=>mc_fc_loc_copy_row  )
                              ).

*display the alv
      lv_alv->set_table_for_first_display(
        EXPORTING
          is_layout                     = ls_layo    "Layout
          is_variant                    = gs_variant
          i_default = 'X'
          i_save                        = 'A'
          it_toolbar_excluding = gr_exclude[]
        CHANGING
          it_outtab                     = lt_final    "Output Table
          it_fieldcatalog               = lt_fcat     "Field Catalog
      ).
      REFRESH lt_fcat.        "refresh fieldcat
    ELSE.
      CALL METHOD lv_alv->refresh_table_display.
    ENDIF.
  ENDMETHOD.

  METHOD select_alv_variant.
    DATA: ls_variant TYPE disvariant.

    ls_variant-report   = sy-repid.
    ls_variant-username = sy-uname.

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
  ENDMETHOD.

ENDCLASS.
