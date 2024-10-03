*&---------------------------------------------------------------------*
*& Include          ZFI_DOCUMENTS_ADOBE_PFORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_logo_xstring
*&---------------------------------------------------------------------*
FORM get_logo_xstring  CHANGING p_gv_logo.
  CLEAR: gv_logo.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object       = 'GRAPHICS'
      p_name         = 'ZSHEENLAC'
      p_id           = 'BMAP'
      p_btype        = 'BCOL'
    RECEIVING
      p_bmp          = gv_logo
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form data_selection
*&---------------------------------------------------------------------*
FORM data_selection .
  REFRESH: gt_bkpf,gt_bseg,gt_bset.
  SELECT * FROM bkpf INTO TABLE gt_bkpf
           WHERE belnr IN s_belnr
           AND bukrs EQ p_bukrs
           AND gjahr EQ p_gjahr.
  IF sy-subrc = 0.
    SORT gt_bkpf[] BY bukrs belnr gjahr.
  ELSE.
    DATA(l_msg) = |No Document Details to display|.
    MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form data_population
*&---------------------------------------------------------------------*
FORM data_population .
  LOOP AT gt_bkpf ASSIGNING FIELD-SYMBOL(<fs_bkpf>).

    DATA(l_header) = VALUE header( belnr = <fs_bkpf>-belnr
                                   bukrs = <fs_bkpf>-bukrs
                                   gjahr = <fs_bkpf>-gjahr
                                   blart = <fs_bkpf>-blart
                                   bldat = <fs_bkpf>-bldat
                                   budat = <fs_bkpf>-budat
                                   xblnr = <fs_bkpf>-xblnr ).
    "Getting Header text
    CLEAR gv_heading.
    PERFORM get_header_text USING l_header-blart CHANGING gv_heading.
    DATA(lt_bseg) = gt_bseg[].
    DELETE lt_bseg[] WHERE belnr NE <fs_bkpf>-belnr AND bukrs NE <fs_bkpf>-bukrs
                                                  AND gjahr NE <fs_bkpf>-gjahr.
    DATA(lt_tax) = lt_bseg[].
    DELETE lt_tax[] WHERE buzid NE 'T'.
    REFRESH gt_final.
    LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fls_bseg>) WHERE buzid NE 'T'.

      IF <fls_bseg>-shkzg = 'H'.
        DATA(l_amount_dt) = <fls_bseg>-dmbtr * -1.
      ELSE.
        DATA(l_amount_ct) = <fls_bseg>-dmbtr.
      ENDIF.

      IF <fls_bseg>-koart = 'K'. "Vendor Details
        SELECT SINGLE * FROM lfa1 INTO @DATA(l_lfa1) WHERE lifnr = @<fls_bseg>-lifnr.
        IF sy-subrc = 0.
          DATA(l_account) = VALUE account( account = l_lfa1-lifnr
                                           name1 = l_lfa1-name1
                                           stras = l_lfa1-stras
                                           ort01 = l_lfa1-ort01
                                           land1 = l_lfa1-land1
                                           telf1 = l_lfa1-telf1 ).
          SELECT SINGLE smtp_addr FROM adr6 INTO l_account-email WHERE addrnumber = l_lfa1-adrnr.
        ENDIF.
        APPEND VALUE #( sno = <fls_bseg>-buzei
                        account = <fls_bseg>-lifnr
                        item_txt = <fls_bseg>-sgtxt
                        debit = l_amount_dt
                        credit = l_amount_ct ) TO gt_final.

      ELSEIF <fls_bseg>-koart = 'D'. "Customer Details

        SELECT SINGLE * FROM kna1 INTO @DATA(l_kna1) WHERE kunnr = @<fls_bseg>-kunnr.
        IF sy-subrc = 0.
          l_account = VALUE account( account = l_lfa1-lifnr
                                     name1   = l_lfa1-name1
                                     stras   = l_lfa1-stras
                                     ort01   = l_lfa1-ort01
                                     land1   = l_lfa1-land1
                                     telf1   = l_lfa1-telf1 ).
          SELECT SINGLE smtp_addr FROM adr6 INTO l_account-email WHERE addrnumber = l_kna1-adrnr.
        ENDIF.
        APPEND VALUE #( sno = <fls_bseg>-buzei
                        account = <fls_bseg>-lifnr
                        item_txt = <fls_bseg>-sgtxt
                        debit = l_amount_dt
                        credit = l_amount_ct ) TO gt_final.

      ELSEIF <fls_bseg>-koart = 'S'.
        APPEND VALUE #( sno = <fls_bseg>-buzei
                        account = <fls_bseg>-hkont
                        item_txt = <fls_bseg>-sgtxt
                        debit = l_amount_dt
                        credit = l_amount_ct ) TO gt_final.
      ENDIF.
    ENDLOOP.

    DATA(l_cgst) = VALUE #( lt_tax[ ktosl = 'JOC' ] OPTIONAL ).
    IF l_cgst IS NOT INITIAL.
      APPEND VALUE #( sno = l_cgst-buzei
                      account = l_cgst-hkont
                      item_txt = l_cgst-sgtxt
                      debit = l_cgst-hwbas
                      ctax = l_cgst-dmbtr ) TO gt_final.
    ENDIF.
    DATA(l_sgst) = VALUE #( lt_tax[ ktosl = 'JOS' ] OPTIONAL ).
    IF l_sgst IS NOT INITIAL.
      APPEND VALUE #( sno = <fls_bseg>-buzei
                      account = <fls_bseg>-hkont
                      item_txt = <fls_bseg>-sgtxt
                      debit = l_sgst-hwbas
                      stax = l_sgst-dmbtr ) TO gt_final.
    ENDIF.
    DATA(l_igst) = VALUE #( lt_tax[ ktosl = 'JOI' ] OPTIONAL ).
    IF l_igst IS NOT INITIAL.
      APPEND VALUE #( sno = <fls_bseg>-buzei
                      account = <fls_bseg>-hkont
                      item_txt = <fls_bseg>-sgtxt
                      debit = l_igst-hwbas
                      itax  = l_igst-dmbtr ) TO gt_final.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_header_text
*&---------------------------------------------------------------------*
FORM get_header_text  USING    p_l_header_blart
                      CHANGING p_gv_heading.

  IF p_l_header_blart IS NOT INITIAL.
    SELECT SINGLE ltext FROM t003t
                        INTO p_gv_heading
                        WHERE blart = p_l_header_blart
                        AND spras = sy-langu.
  ENDIF.
ENDFORM.
