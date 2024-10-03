FUNCTION zfi_customer_stat_pdf_dms.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DISTRIBUTOR) TYPE  KUNNR
*"     VALUE(RETAILER) TYPE  KUNNR
*"     VALUE(FROM_DATE) TYPE  BLDAT
*"     VALUE(TO_DATE) TYPE  BLDAT
*"  EXPORTING
*"     VALUE(EP_STRING) TYPE  STRING
*"----------------------------------------------------------------------
*&Created By: Pandiaraja
*&Created On: 07.03.2024
*&Purpose: To send the Customer Balance Sheet PDF - DMS
*&Reference: Ramakrishnan J
*-----------------------------------------------------------------------*
  DATA: lv_str_pdf TYPE string.
  DATA: lt_seltab TYPE TABLE OF rsparams.

  REFRESH: lt_seltab.
  lt_seltab = VALUE #(  ( selname = 'P_DIST'
                          kind    = 'P'
                          sign    = 'I'
                          option  = 'EQ'
                          low     = distributor )
                        ( selname = 'P_RETA'
                          kind    = 'P'
                          sign    = 'I'
                          option  = 'EQ'
                          low     = retailer )

                        ( selname = 'SO_BUDAT'
                          kind    = 'S'
                          sign    = 'I'
                          option  = 'BT'
                          low     = from_date
                          high    = to_date )

                        ( selname = 'R1'
                          kind    = 'P'
                          sign    = 'I'
                          option  = 'EQ'
                          low     = abap_true )

                        ( selname = 'P_C1'
                          kind    = 'P'
                          sign    = 'I'
                          option  = 'EQ'
                          low     = abap_true )
                         ).

  "Pdf Get Program.
*  SUBMIT zcust_bal_ledger_print_dms
*          WITH SELECTION-TABLE lt_seltab
*          AND RETURN.

  SUBMIT zcust_bal_ledger_print_dms_cop
        WITH SELECTION-TABLE lt_seltab
        AND RETURN.

  CLEAR: lv_str_pdf.
  IMPORT lv_string TO lv_str_pdf FROM MEMORY ID 'ZCUST_DMS'.
  FREE MEMORY ID 'ZCUST_DMS'.

  IF lv_str_pdf IS NOT INITIAL.
    ep_string = lv_str_pdf.
  ENDIF.
ENDFUNCTION.
