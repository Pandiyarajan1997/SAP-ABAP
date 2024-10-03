*&---------------------------------------------------------------------*
*& Report ZPAYMENT_ADJUST_LOG_DISPLAY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpayment_adjust_log_display.

TYPES: BEGIN OF ty_alv,
         acctyp_des(25) TYPE c,
         venname        TYPE name1_gp,
         cusname        TYPE name1_gp,
         gldesc         TYPE text50.
         INCLUDE STRUCTURE zdebit_ref_mis.
TYPES: END OF ty_alv.

DATA: gt_final TYPE TABLE OF ty_alv,
      gs_final TYPE ty_alv.
*** Slection screen content ****
DATA: lv_bukrs   TYPE zdebit_ref_mis-bukrs,
      lv_docno   TYPE zdebit_ref_mis-doc_no,
      lv_year    TYPE zdebit_ref_mis-gjahr,
      lv_actyp   TYPE zdebit_ref_mis-account_type,
      lv_docty   TYPE zdebit_ref_mis-doc_type,
      lv_account TYPE zdebit_ref_mis-account,
      lv_date    TYPE zdebit_ref_mis-erdat.
DATA: gt_variant TYPE disvariant.

INCLUDE zpayment_adjust_log_disp_sel.

INCLUDE zpayment_adjust_log_disp_forms.

INITIALIZATION.
  PERFORM get_default_layout CHANGING p_layo.

AT SELECTION-SCREEN.
  DATA: gx_variant TYPE disvariant.

  IF NOT p_layo IS INITIAL.
    MOVE gt_variant TO gx_variant.
    MOVE: p_layo         TO gx_variant-variant,
          sy-repid       TO gx_variant-report.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = gx_variant.
    gt_variant = gx_variant.
  ELSE.
    CLEAR gt_variant.
    gt_variant-report = sy-repid.
    gt_variant-username = sy-uname.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layo.
  PERFORM select_alv_variant CHANGING p_layo.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_belnr-low.
  PERFORM f4_valuehelp USING 'S_BELNR-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_belnr-high.
  PERFORM f4_valuehelp USING 'S_BELNR-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_docty-low.
  PERFORM f4_valuehelp USING 'S_DOCTY-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_docty-high.
  PERFORM f4_valuehelp USING 'S_DOCTY-HIGH'.


START-OF-SELECTION.
** Actual Process ***
  PERFORM f_prepare_data.
*** ALV Display ***
  PERFORM f_alv_display.
