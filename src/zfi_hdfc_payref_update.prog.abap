*&---------------------------------------------------------------------*
*& Report ZFI_HDFC_PAYREF_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_hdfc_payref_update.

TYPES: BEGIN OF alv,
         doc_no TYPE belnr_d,
         gjahr  TYPE gjahr,
         type   TYPE bapi_mtype,
         msg    TYPE message-msgtx,
       END OF alv.
DATA: gt_alv TYPE TABLE OF alv.
DATA: gt_bseg   TYPE STANDARD TABLE OF bseg,
      gt_accchg TYPE TABLE OF accchg.
DATA: gv_utr_field TYPE    char50 VALUE 'KIDNO'.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
"Selection Screen Design
DATA: lv_belnr TYPE belnr_d,
      lv_blart TYPE blart.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_belnr FOR lv_belnr.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  "Data fetching based on input
  SELECT * FROM zdebit_ref_mis
           INTO TABLE @DATA(lt_log)
           WHERE doc_no IN @s_belnr.
  IF sy-subrc = 0.
    REFRESH: gt_alv.
    LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).
      REFRESH: gt_bseg.
      DATA(lv_awref) = CONV awref( <fs_log>-doc_no ).
      DATA(lv_aworg) = CONV aworg( |{ <fs_log>-bukrs }{ <fs_log>-gjahr }| ).
      CALL FUNCTION 'FI_DOCUMENT_READ'       "#EC CI_USAGE_OK[2628706] " Added by <IT-CAR Tool> during Code Remediation
        EXPORTING
          i_bukrs     = <fs_log>-bukrs
          i_belnr     = <fs_log>-doc_no
          i_gjahr     = <fs_log>-gjahr
        TABLES
          t_bseg      = gt_bseg
        EXCEPTIONS
          wrong_input = 1
          not_found   = 2
          OTHERS      = 3.
      IF sy-subrc = 0.
        LOOP AT gt_bseg INTO DATA(lw_bseg).
          REFRESH: gt_accchg.
          DATA(lw_accchg) = VALUE accchg( fdname = gv_utr_field
                                          oldval = lw_bseg-kidno
                                          newval = <fs_log>-pay_reference ).
          APPEND lw_accchg TO gt_accchg.
          "Function Module to update the Payment reference
          CALL FUNCTION 'FI_DOCUMENT_CHANGE'
            EXPORTING
              i_awtyp              = 'BKPF'
              i_awref              = lv_awref
              i_aworg              = lv_aworg
              i_buzei              = lw_bseg-buzei
            TABLES
              t_accchg             = gt_accchg
            EXCEPTIONS
              no_reference         = 1
              no_document          = 2
              many_documents       = 3
              wrong_input          = 4
              overwrite_creditcard = 5
              OTHERS               = 6.
          IF sy-subrc NE 0.
            CASE sy-subrc.
              WHEN '1'.
                DATA(lv_msg) = 'no_reference'.
                EXIT.
              WHEN '2'.
                lv_msg = 'no_document'.
                EXIT.
              WHEN '3'.
                lv_msg = 'many_documents'.
                EXIT.
              WHEN '4'.
                lv_msg = 'wrong_input'.
                EXIT.
              WHEN '5'.
                lv_msg = 'overwrite_creditcard'.
                EXIT.
              WHEN '6'.
                lv_msg = 'Others'.
                EXIT.
            ENDCASE.
          ENDIF.
        ENDLOOP.
        IF lv_msg IS INITIAL.
          APPEND VALUE #( doc_no = <fs_log>-doc_no
                          gjahr = <fs_log>-gjahr
                          type = 'S'
                          msg = |Payment Reference updated| ) TO gt_alv.
        ELSE.
          APPEND VALUE #( doc_no = <fs_log>-doc_no
                          gjahr = <fs_log>-gjahr
                          type = 'E'
                          msg = lv_msg ) TO gt_alv.
        ENDIF.
      ENDIF.
      CLEAR: lv_msg,lv_awref,lv_aworg.
    ENDLOOP.
  ENDIF.

  IF gt_alv IS NOT INITIAL.
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.
    lo_gr_alv->display( ).
  ENDIF.
