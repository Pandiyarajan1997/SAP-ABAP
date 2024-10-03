*&---------------------------------------------------------------------*
*& Report ZSD_CUSTFIN_INV_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_custfin_inv_update.

PARAMETERS : p_vbeln TYPE vbrk-vbeln.
DATA: lo_object_cls TYPE REF TO zcl_scm_sales_to_invoice.
CREATE OBJECT lo_object_cls.

START-OF-SELECTION.

  SELECT SINGLE * FROM vbrk INTO @DATA(l_vbrk) WHERE vbeln = @p_vbeln.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM zsd_sf_cust_inv INTO @DATA(ls_custinv) WHERE invoiceno = @p_vbeln.
    IF sy-subrc = 0.
      MESSAGE : 'Already Invoice found' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
**************call the method to add the invoice*************
      CALL METHOD lo_object_cls->financing
        EXPORTING
          ls_vbrk = l_vbrk.
    ENDIF.
  ELSE.
    MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
