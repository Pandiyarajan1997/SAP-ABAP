*&---------------------------------------------------------------------*
*& Report ZSD_CUST_PF_MAPPING_MIS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_cust_pf_mapping_mis.

TABLES: knvv.
TYPES: BEGIN OF ty_cust,
         org           TYPE string,
         Channel       TYPE string,
         customercode  TYPE string,
         position_code TYPE string,
       END OF ty_cust.
DATA: gt_cust TYPE TABLE OF ty_cust,
      gs_cust TYPE ty_cust.

TYPES: BEGIN OF ty_custdet,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
         werks TYPE werks_d,
       END OF ty_custdet.

DATA: gt_custdet TYPE TABLE OF ty_custdet,
      gs_custdet TYPE ty_custdet.

TYPES: gtt_knvp TYPE TABLE OF knvp .

TYPES: ts_cusposm TYPE zsd_custpf_posm.

DATA: GT_knvp_tst TYPE TABLE OF knvp.

DATA: lv_kunnr TYPE kna1-kunnr. "Selection Screen Input

DATA: gv_token TYPE string,
      gv_msg   TYPE string.

DATA: gt_cusposm TYPE TABLE OF zsd_custpf_posm,
      gs_cusposm TYPE zsd_custpf_posm.

DATA: gt_cuselog TYPE TABLE OF zsd_cpf_pos_elog,
      gs_cuselog TYPE zsd_cpf_pos_elog.

DATA: gt_positions TYPE TABLE OF zhr_so_to_top_new,
      gt_postab    TYPE TABLE OF zhr_so_to_top_ts.

DATA: gr_parvw TYPE RANGE OF parvw,
      gs_parvw LIKE LINE OF gr_parvw.

DATA: gt_hrp1000 TYPE TABLE OF hrp1000,
      gs_hrp1000 TYPE hrp1000.

DATA: gv_total   TYPE i,
      gv_errors  TYPE i,
      gv_success TYPE i,
      gv_ignore  TYPE i,
      gv_addtnl  TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_kunnr FOR lv_kunnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_cuspos RADIOBUTTON GROUP r1.
    SELECTION-SCREEN COMMENT 2(16) TEXT-006 FOR FIELD p_cuspos.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_Proce RADIOBUTTON GROUP r1.
    SELECTION-SCREEN COMMENT 2(16) TEXT-004 FOR FIELD p_Proce.
    SELECTION-SCREEN POSITION 20.
    SELECT-OPTIONS: s_vkorg FOR knvv-vkorg NO INTERVALS.
    SELECTION-SCREEN COMMENT 30(6) TEXT-005 FOR FIELD S_vkorg.
*    SELECTION-SCREEN POSITION 50.
*    PARAMETERS: p_pdate TYPE sy-datum .
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.

    PARAMETERS: p_Delold RADIOBUTTON GROUP r1.
    SELECTION-SCREEN COMMENT 2(25) TEXT-007 FOR FIELD p_delold.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_delpar RADIOBUTTON GROUP r1.
    SELECTION-SCREEN COMMENT 2(16) TEXT-008 FOR FIELD p_delpar.
    SELECTION-SCREEN POSITION 20.
    PARAMETERS: p_date TYPE sy-datum.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_updsk RADIOBUTTON GROUP r1.
    SELECTION-SCREEN COMMENT 2(25) TEXT-009 FOR FIELD p_updsk.
*    SELECTION-SCREEN POSITION 20.
*    PARAMETERS: p_date TYPE sy-datum.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_updsof RADIOBUTTON GROUP r1.
    SELECTION-SCREEN COMMENT 2(25) TEXT-010 FOR FIELD p_updsof.
*    SELECTION-SCREEN POSITION 20.
*    PARAMETERS: p_date TYPE sy-datum.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: P_test AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b3.

INCLUDE zsd_cust_pf_mapping_mis_f01.

INITIALIZATION.

  PERFORM f_fill_parvw.

START-OF-SELECTION.


  IF p_cuspos = abap_true.  "to get records from MIS
    "get bearer token from MIS
    WRITE: / 'Extraction Intiated', sy-datum, Sy-uzeit.
    CLEAR: gv_token,gv_msg.
    PERFORM f_get_bearer_token CHANGING gv_token gv_msg.


    IF gv_token IS NOT INITIAL.
      WRITE: / 'Bearer token received'.
** API Call **
      CLEAR gv_addtnl.
      PERFORM f_dms_apicall.
    ELSE.
      WRITE: / 'Error Occurs as', gv_msg.
    ENDIF.
  ELSEIF p_Proce = abap_true.

    IF s_vkorg[] IS INITIAL.
      WRITE: / 'Sales org not Provided for processing records'.
    ELSE.
      REFRESH GT_knvp_tst.
      LOOP AT s_vkorg.
        WRITE: / 'Processing Started for Sorg', s_vkorg-low.
        PERFORM f_process USING S_vkorg-low.
        WRITE: / 'Processing Completed for Sorg', s_vkorg-low.
      ENDLOOP.
    ENDIF.
  ELSEIF p_Delold = abap_true.
    PERFORM f_delete_records.
  ELSEIF p_delpar = abap_true.
    IF p_date IS INITIAL.
      WRITE: / 'Date not provided for Deletion'.
    ELSE.
      PERFORM f_delete_date.
    ENDIF.
  ELSEIF p_updsk = abap_true.
    PERFORM f_update_SKSDMS.
  ELSEIF p_updsof = abap_true.
    Refresh gt_cuselog.
    PERFORM f_update_SKSDMS_New.
  ENDIF.


end-of-SELECTION.

  IF P_test = abap_true.
    IF gt_cuselog[] IS NOT INITIAL.
      CALL FUNCTION 'Z_POPUP_ALV'
        EXPORTING
          i_start_column = 25
          i_start_line   = 6
          i_end_column   = 100
          i_end_line     = 20
          i_title        = 'Customer Differences'
*         i_status_field_name = 'KUNNR'
*         i_popup        = 'X'
        TABLES
          it_alv         = gt_cuselog.
    ENDIF.
  ENDIF.
