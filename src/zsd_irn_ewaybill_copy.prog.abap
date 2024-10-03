*&---------------------------------------------------------------------*
*& Report ZSD_IRN_EWAYBILL_COPY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_irn_ewaybill_copy.
PARAMETERS p_bukrs TYPE vbrk-bukrs.
PARAMETERS p_gjahr TYPE vbrk-gjahr.
PARAMETERS p_vbeln TYPE vbrk-vbeln.
PARAMETERS p_vbeln2 TYPE vbrk-vbeln.

SELECT SINGLE * FROM j_1ig_invrefnum
  INTO @DATA(lw_irn)
  WHERE bukrs = @p_bukrs
    AND docno = @p_vbeln
    AND doc_year = @p_gjahr
    AND irn_status = 'ACT'.

SELECT SINGLE * FROM j_1ig_ewaybill
  INTO @DATA(lw_ewaybill)
  WHERE bukrs = @p_bukrs
    AND docno = @p_vbeln
    AND gjahr = @p_gjahr
    AND status = 'A'.
  IF sy-datum+4(4) BETWEEN 0401 AND 1231.
    DATA(l_year) = sy-datum(4).
  ELSE.
*  ENDIF.
*  IF sy-datum+4(4) BETWEEN 0101 AND 0331.
    l_year  = sy-datum(4) - 1.
  ENDIF.
IF lw_irn IS NOT INITIAL.
  lw_irn-docno = p_vbeln2.
  lw_irn-doc_year = l_year.
  MODIFY j_1ig_invrefnum FROM lw_irn.
ENDIF.
IF lw_ewaybill IS NOT INITIAL.
  lw_ewaybill-docno = p_vbeln2.
  lw_ewaybill-gjahr = l_year.
  MODIFY j_1ig_ewaybill FROM lw_ewaybill.
ENDIF.
IF lw_irn IS INITIAL AND lw_ewaybill IS INITIAL.
  MESSAGE 'No Data FOund' TYPE 'S' DISPLAY LIKE 'E'.
ELSE.
  MESSAGE 'Update Successfully' TYPE 'S' DISPLAY LIKE 'S'.

ENDIF.
