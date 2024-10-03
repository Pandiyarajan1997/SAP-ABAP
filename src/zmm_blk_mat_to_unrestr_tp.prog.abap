*&---------------------------------------------------------------------*
*& Report zmm_blk_mat_to_unrestr_tp
*&---------------------------------------------------------------------*
*&Created by: Zakir hussain
*&Changed by: Zakir hussain
*&Purpose : Material SLED extension and Transfer Post
*&Reference: Ramakrishnan J
*&Date : 14.08.2024
*&---------------------------------------------------------------------*
REPORT zmm_blk_mat_to_unrestr_tp.
INCLUDE <icon>.
"Data Declaration
INCLUDE zmm_blk_mat_to_unrestr_top.
"Local events
*INCLUDE zmm_service_pr_creation_cls.
INCLUDE zmm_sled_update_event.
"Selection Screen
INCLUDE zmm_blk_mat_to_unrestr_sel.
"Subroutines
INCLUDE zmm_blk_mat_to_unrestr_forms.

INITIALIZATION.
  DATA(lo_upload) = NEW zcl_excel_uploader_new( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM Upload_request USING p_fname.

AT SELECTION-SCREEN OUTPUT.
SET PF-STATUS 'ZSEL_STAT1'.

AT SELECTION-SCREEN.
IF sy-ucomm <> 'FC01' and p_budat is INITIAL .
MESSAGE 'Enter Posting Date' TYPE 'E'.
ELSEIF sy-ucomm <> 'FC01' and p_fname is INITIAL.
MESSAGE 'Please Upload a Excel File' TYPE 'E'.
ENDIF.
  CASE sy-ucomm.
    WHEN 'FC01'.
      Perform Excel_download.
  ENDCASE.

START-OF-SELECTION.
  lo_upload->filename = p_fname.
  lo_upload->header_rows_count = 1.
  lo_upload->upload( CHANGING ct_data = lt_upload ).
  IF lt_upload IS INITIAL.
    MESSAGE 'No Data in the upload file' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING.
  ENDIF.

  "validations for excel data
  PERFORM validations USING lt_upload gt_trpost.
  PERFORM build_alv USING gt_trpost.
