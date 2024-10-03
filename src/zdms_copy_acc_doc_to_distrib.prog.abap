*&---------------------------------------------------------------------*
*& Report ZDMS_COPY_ACC_DOC_TO_DISTRIB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdms_copy_acc_doc_to_distrib.
TYPE-POOLS: icon.

INCLUDE zdms_copy_acc_doc_to_distribt1.
INCLUDE zdms_copy_acc_doc_to_distribcl.
INCLUDE zdms_copy_acc_doc_to_distriof1.


INITIALIZATION.
  so_docdt-low = sy-datum - 1.
  so_docdt-high = sy-datum.
  APPEND so_docdt.

  CREATE OBJECT gr_main_cls.

  " Document Types
  so_docty[] = VALUE #(
                      ( sign   = 'I' option = 'EQ' low = 'VG')
                      ( sign   = 'I' option = 'EQ' low = 'DR')
                      ( sign   = 'I' option = 'EQ' low = 'RV')
                      ( sign   = 'I' option = 'EQ' low = 'DZ')
                      ( sign   = 'I' option = 'EQ' low = 'DG')
                      ( sign   = 'I' option = 'EQ' low = 'DA')
                    ).


  SELECT bwkey,
         bukrs
    FROM t001k
    INTO TABLE @DATA(lt_t001k)
    WHERE bukrs = 'DMS1'.
  IF sy-subrc = 0.
    SELECT kunnr, werks FROM kna1
      FOR ALL ENTRIES IN @lt_t001k
      WHERE werks = @lt_t001k-bwkey
      INTO TABLE @DATA(lt_kna1).
    IF sy-subrc = 0.
*************customer block check***********
      DATA : lo_block TYPE REF TO zcl_common_check.
      CREATE OBJECT lo_block.
      DATA : lt_block TYPE TABLE OF zsd_st_cust_block.
      DATA : lw_block TYPE zsd_st_cust_block.
      LOOP AT lt_kna1 INTO DATA(lw_kna).
        lw_block-bukrs = '1000'.
        lw_block-vkorg = '1000'.
        lw_block-kunnr = lw_kna-kunnr.
        APPEND lw_block TO lt_block.
      ENDLOOP.
**************call customer check*********
      lo_block->customer_block_check(
        EXPORTING
          bukrs                     = '1000'      " Company Code
          vkorg                     = '1000'      " Sales Organization
        CHANGING
          cust_tab                  = lt_block    " Table Type for Customer Block Check
          ).
      IF lt_block IS NOT INITIAL.
        SORT : lt_block BY kunnr block.
        DELETE lt_block WHERE block = 'X'.
      ENDIF.

      gr_kunnr = VALUE t_kunnr( FOR ls_kna11 IN lt_block
                     LET s = 'I'
                         o = 'EQ'
                     IN sign   = s
                        option = o
                   ( low = |{ ls_kna11-kunnr ALPHA = IN }| ) ).
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  IF so_kunnr IS NOT INITIAL.
    CLEAR gr_kunnr[].
    gr_kunnr = VALUE t_kunnr( FOR wa_kunnr IN so_kunnr
                   LET s = 'I'
                       o = 'EQ'
                   IN sign   = s
                      option = o
                 ( low = |{ wa_kunnr-low ALPHA = IN }| ) ).
  ENDIF.
  gr_main_cls->get_data( ).

  IF sy-batch = abap_true.
    LOOP AT gr_main_cls->gt_final_tab ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE type NE 'P'.
      gr_main_cls->gw_final_tab = <fs_data>.
      " Post Method
      gr_main_cls->post_documents( ).
      " Get Doc No after Posting
      <fs_data>-belnr2 = gr_main_cls->gw_final_tab-belnr2.
      <fs_data>-belnr3 = gr_main_cls->gw_final_tab-belnr3.
      <fs_data>-type = gr_main_cls->gw_final_tab-type.
      <fs_data>-remarks = gr_main_cls->gw_final_tab-remarks.
    ENDLOOP.
  ENDIF.
  gr_main_cls->build_alv( ).
