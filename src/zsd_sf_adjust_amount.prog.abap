*&---------------------------------------------------------------------*
*& Report ZSD_SF_ADJUST_AMOUNT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_sf_adjust_amount.

TABLES: zsd_sf_cust_inv.

DATA: gs_bapireturn  TYPE bapireturn,
      gt_lineitems   TYPE STANDARD TABLE OF bapi3007_2,       " line items for customer
      gt_lineitems_m TYPE STANDARD TABLE OF bapi3007_2,       " line items for customer
      gs_lineitems   TYPE bapi3007_2.

DATA: lt_cust_inv TYPE TABLE OF zsd_sf_cust_inv,
      ls_cust_inv TYPE zsd_sf_cust_inv.

*Selection Options
SELECT-OPTIONS: so_invno FOR zsd_sf_cust_inv-invoiceno," MATCHCODE OBJECT f4_vbrk,
                so_kunnr FOR zsd_sf_cust_inv-custno.


START-OF-SELECTION.

  SELECT *
     FROM zsd_sf_cust_inv
    INTO TABLE lt_cust_inv
  WHERE  invoiceno IN so_invno AND
         custno IN so_kunnr AND
*        invoicedate BETWEEN @so_date-low AND @so_date-high AND
         status = '99'.

  SORT lt_cust_inv BY invoiceno.

  IF lt_cust_inv[] IS NOT INITIAL.

    LOOP AT lt_cust_inv INTO ls_cust_inv.

      REFRESH: gt_lineitems, gt_lineitems_m.
      CLEAR: gs_bapireturn.

      PERFORM bapi_ar_acc_getopenitems USING    ls_cust_inv-bukrs ls_cust_inv-custno
                                       CHANGING gs_bapireturn gt_lineitems .

      IF gt_lineitems[] IS NOT INITIAL.
* Consider only Credit Line Items
        LOOP AT gt_lineitems INTO gs_lineitems WHERE doc_type = 'H'.

          SELECT SINGLE * FROM zfi_sf_cr_notes INTO @DATA(ls_ZFI_SF_CR_NOTES)
            WHERE kunnr = @gs_lineitems-customer
              AND bukrs = @gs_lineitems-comp_code
              AND belnr = @gs_lineitems-doc_no
              AND doc_type = @gs_lineitems-doc_type
              AND gjahr = @gs_lineitems-fisc_year
              AND status = '10'.
          IF sy-subrc = 0.

          ENDIF.
        ENDLOOP.


      ENDIF.

    ENDLOOP.


  ENDIF.




*&---------------------------------------------------------------------*
*& Form bapi_ar_acc_getopenitems
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_HEADER_BUKRS
*&      --> GS_TAB_KUNNR
*&      --> GS_TAB_BELNR
*&      <-- GS_BAPIRETURN
*&      <-- GT_LINEITEMS
*&---------------------------------------------------------------------*
FORM bapi_ar_acc_getopenitems  USING  p_bukrs TYPE bukrs
                                      p_kunnr TYPE kunnr
                            CHANGING  ps_bapireturn LIKE gs_bapireturn
                                      pt_lineitems LIKE gt_lineitems.


  CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS' "#EC CI_USAGE_OK[2628704]
    EXPORTING
      companycode = p_bukrs
      customer    = p_kunnr
      keydate     = sy-datum
*     NOTEDITEMS  = ' '
*     SECINDEX    = ' '
    IMPORTING
      return      = ps_bapireturn
    TABLES
      lineitems   = pt_lineitems.

*  SELECT  rbukrs AS comp_code,
*          gjahr AS fisc_year,
*          belnr AS doc_no
*            FROM acdoca INTO TABLE @DATA(lt_cust_openitem)
*                 FOR ALL ENTRIES IN @pt_lineitems
*                 WHERE  rbukrs = @pt_lineitems-comp_code
*                    AND gjahr = @pt_lineitems-fisc_year
*                    AND belnr = @pt_lineitems-doc_no
*                    AND kunnr = @kunnr
*                    AND rbusa = @gsber.
*  IF sy-subrc = 0.
*    DATA(lt_lineitems) = pt_lineitems.
*    CLEAR lt_lineitems[].
*    LOOP AT pt_lineitems INTO DATA(lw_data).
*      READ TABLE lt_cust_openitem TRANSPORTING NO FIELDS WITH KEY  comp_code = lw_data-comp_code
*                                                                   fisc_year = lw_data-fisc_year
*                                                                   doc_no    = lw_data-doc_no.
*      IF sy-subrc = 0.
*        APPEND lw_data TO lt_lineitems.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*  CLEAR pt_lineitems[].
*  APPEND LINES OF lt_lineitems TO pt_lineitems.

**  IF sy-subrc IS NOT INITIAL.                              "#EC FB_NORC
**    gs_message-mess_type = gs_bapireturn-type.
**    gs_message-message = gs_bapireturn-message.
**    MOVE-CORRESPONDING gs_tab TO gs_message.
**    APPEND gs_message TO gt_message.
**    CLEAR gs_bapireturn.
**  ELSE.
**
**  ENDIF.


*  LOOP AT pt_lineitems INTO gs_lineitems WHERE sp_gl_ind NE ''.
*    IF gs_lineitems-sp_gl_ind NE 'A'.
*      DELETE pt_lineitems INDEX sy-tabix..
*    ENDIF.
*    CLEAR gs_lineitems.
*  ENDLOOP.

ENDFORM.
