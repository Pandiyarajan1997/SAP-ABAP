*&---------------------------------------------------------------------*
*& Report zcust_block_unblock_rep
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcust_block_unblock_rep.

*TYPES: tt_data TYPE STANDARD TABLE OF zst_cust_inp_bl_ubl.
TYPES: BEGIN OF ty_data,
         customer   TYPE  kunnr,
  customer_type  type zde_ctype,
  block_indicator type  char10,
  END OF ty_data.
 TYPES:
      BEGIN OF ty_ret_data,
        Customer        TYPE kunnr,
        Customer_type   TYPE char50,
        Block_Indicator TYPE char02,
        Message         TYPE string,
        Type            TYPE bapi_mtype,
      END OF ty_ret_data .
    DATA: GT_response TYPE TABLE OF ty_ret_data,
          Ls_err_log type ty_ret_data,
          con_txt type char50.

DATA:  t_data type STANDARD TABLE OF ty_data,
       iv_payload_json type string,
       gs_input type zst_cust_inp_bl_ubl.
DATA: gt_kna1    TYPE table of kna1,
          gt_knb1    TYPE TABLE OF knb1,
          gt_knvv    TYPE TABLE OF knvv,
         lv_message TYPE string,
          lv_type    TYPE bapi_mtype.

INITIALIZATION.
  DATA: lo_uploader TYPE REF TO zcl_excel_uploader_new.
  CREATE OBJECT lo_uploader.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_fname TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
lo_uploader->f4_help(
  IMPORTING
    p_fname = p_fname
).
**********************************************************************
START-OF-SELECTION.
*  lo_uploader->max_rows = 9999.
  lo_uploader->filename =  p_fname .
  lo_uploader->header_rows_count = 1.
  lo_uploader->upload( CHANGING ct_data = t_data ).
  If T_data is initial.
   Message 'No Data in the upload file' type 'E' DISPLAY LIKE 'I'.
  Endif.
  data(lo_cust_bl_ubl) = NEW zcl_api_cust_block_unblock(  ).
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Manipulate Data
  LOOP at t_data ASSIGNING FIELD-SYMBOL(<fs_data>).
  data(lv_index) = sy-tabix.
  <fs_data>-customer = |{ <fs_data>-customer ALPHA = IN }|.
  <fs_data>-block_indicator = to_lower( <fs_data>-block_indicator ).
   case <fs_data>-block_indicator.
   when 'block'.
   <fs_data>-block_indicator = 'X'.
   When 'unblock'.
   clear <fs_data>-block_indicator.
   When OTHERS.
   Message 'Incorrect Block Type' TYPE 'E'.
   endcase.
   Read TABLE t_data into gs_input index lv_index.
    lo_cust_bl_ubl->gs_input = gs_input.
     lo_cust_bl_ubl->validate_customer(
       EXPORTING
         ls_input = gs_input
       IMPORTING
         gt_kna1  = gt_kna1
         gt_knb1  = gt_knb1
         gt_knvv  = gt_knvv
         message  = lv_message
         type     = lv_type
     ).
         IF lv_message IS INITIAL .
        lo_cust_bl_ubl->process_data(
          EXPORTING
            ls_input    = gs_input
            lt_kna1     =  gt_kna1
            lt_knb1     =  gt_knb1
            lt_knvv     =  gt_knvv
          IMPORTING
            et_response = gt_response
        ).
        Else.
          MOVE-CORRESPONDING gs_input TO Ls_err_log.
          DATA(ls_val) = SWITCH #(  gs_input-customer_type WHEN 'DT' THEN '01' WHEN 'DD' THEN '02' WHEN 'SB' THEN '03' WHEN 'OT' THEN '04'  ).
          SELECT SINGLE FROM dd07v   "#EC CI_NOORDER
          FIELDS ddtext
          WHERE domname = 'ZDOM_CTYPE'
          AND  valpos = @ls_val
          INTO @DATA(ls_text).
          IF sy-subrc = 0.
            con_txt = | { gs_input-customer_type } : { ls_text } |.
            ls_err_log-customer_type = con_txt.
          ENDIF.

          Ls_err_log-message = lv_message.
          Ls_err_log-type = lv_type.
          APPEND Ls_err_log TO gt_response.
        ENDIF.
        CLEAR Ls_err_log.
  ENDLOOP.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Build ALV
 lo_uploader->build_alv_cb_ub(
   CHANGING
     iv_data = gt_response
 ).
