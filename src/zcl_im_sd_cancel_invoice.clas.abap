class ZCL_IM_SD_CANCEL_INVOICE definition
  public
  final
  create public .

public section.

  interfaces IF_EX_SD_CIN_LV60AU02 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SD_CANCEL_INVOICE IMPLEMENTATION.


  METHOD if_ex_sd_cin_lv60au02~excise_invoice_create.
*Created By: Samsudeen M
*Created On: 05.09.2023
*Purpose: To Capture the Cancelled Invoices and change the status in
*         customer finance custom table
*Reference: Ramakrishnan J
    IF sy-tcode EQ 'VF11'.

      data : lt_sf_invtab TYPE TABLE of zsd_sf_cust_inv.
      "Actual SAP Invoices
      DATA(lt_actual_inv) = xvbrk[].
      DELETE lt_actual_inv WHERE fkart EQ 'S1'.
      "Cancelled Documents against original Invoices
      DATA(lt_cancel_inv) = xvbrk[].
      DELETE lt_cancel_inv WHERE fkart NE 'S1'.

      IF lt_actual_inv[] IS NOT INITIAL.
        SELECT * FROM zsd_sf_cust_inv
          INTO TABLE lt_sf_invtab
          FOR ALL ENTRIES IN lt_actual_inv
          WHERE invoiceno = lt_actual_inv-vbeln.
        IF sy-subrc EQ 0.
          SORT lt_sf_invtab[] BY invoiceno.
          SORT lt_cancel_inv[] BY xblnr.
          LOOP AT lt_cancel_inv ASSIGNING FIELD-SYMBOL(<fs_canc_inv>).
            LOOP AT lt_sf_invtab ASSIGNING FIELD-SYMBOL(<fs_sf_invtab>) WHERE invoiceno = <fs_canc_inv>-xblnr.
              "E-waybill table Comaprison for E-waybill Created or not before Cancellation
              SELECT SINGLE * FROM j_1ig_ewaybill INTO @DATA(l_ewaybill)
                WHERE bukrs = @<fs_sf_invtab>-bukrs
                AND doctyp = @<fs_sf_invtab>-invoicetype
                AND docno = @<fs_sf_invtab>-invoiceno
                AND gjahr = @<fs_sf_invtab>-gjahr.
              IF sy-subrc EQ 0.
                "E-waybill Created and Cancelled the Invoice
                <fs_sf_invtab>-status = '17'.
              ELSE.
                "Before E-waybill creation itself cancelled the invoice
                <fs_sf_invtab>-status = '11'.
              ENDIF.
              <fs_sf_invtab>-cancelledinv = <fs_canc_inv>-vbeln.
              <fs_sf_invtab>-invstatus    = 'CAN'.
            ENDLOOP.
          ENDLOOP.
          MODIFY zsd_sf_cust_inv FROM TABLE lt_sf_invtab.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
