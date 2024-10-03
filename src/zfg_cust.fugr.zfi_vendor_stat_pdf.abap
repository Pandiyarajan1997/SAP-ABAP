FUNCTION ZFI_VENDOR_STAT_PDF.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LIFNR) TYPE  LIFNR
*"     VALUE(FROM_DATE) TYPE  BLDAT
*"     VALUE(TO_DATE) TYPE  BLDAT
*"     VALUE(BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     VALUE(EP_STRING) TYPE  STRING
*"----------------------------------------------------------------------
*&Created By: Samsudeen M
*&Created On: 23.08.2023
*&Purpose: To send the Customer Balance Sheet PDF
*&Reference: Ramakrishnan J
*-----------------------------------------------------------------------*
  DATA: lv_str_pdf TYPE string.
  DATA: lt_seltab TYPE TABLE OF rsparams.

  REFRESH: lt_seltab.
  lt_seltab = VALUE #(  ( selname = 'SO_LIFNR'
                          kind    = 'P'
                          sign    = 'I'
                          option  = 'EQ'
                          low     = LIFNR )

                        ( selname = 'SO_BUKRS'
                          kind    = 'P'
                          sign    = 'I'
                          option  = 'EQ'
                          low     = BUKRS )

                        ( selname = 'SO_BUDAT'
                          kind    = 'S'
                          sign    = 'I'
                          option  = 'BT'
                          low     = from_date
                          high    = to_date )

*                        ( selname = 'R1'
*                          kind    = 'P'
*                          sign    = 'I'
*                          option  = 'EQ'
*                          low     = abap_true )

                        ( selname = 'P_C1'
                          kind    = 'P'
                          sign    = 'I'
                          option  = 'EQ'
                          low     = abap_true )
                         ).

  "Pdf Get Program.
  SUBMIT ZVEND_BAL_LEDGER_PRINT
          WITH SELECTION-TABLE lt_seltab
          AND RETURN.

  CLEAR: lv_str_pdf.
  IMPORT lv_string TO lv_str_pdf FROM MEMORY ID 'ZVEND'.
  FREE MEMORY ID 'ZVEND'.

  IF lv_str_pdf IS NOT INITIAL.
    ep_string = lv_str_pdf.
  ENDIF.
ENDFUNCTION.
