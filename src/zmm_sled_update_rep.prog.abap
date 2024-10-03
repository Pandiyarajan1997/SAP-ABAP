*&---------------------------------------------------------------------*
*& Report zmm_sled_update_rep
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_sled_update_rep.
INCLUDE <icon>.
"Data Declarations..
INCLUDE zmm_blk_mat_to_unrestr_top.
"Local events
INCLUDE zmm_sled_update_event.
"Selection Screen
INCLUDE zmm_sled_update_sel.
"Subroutines
INCLUDE zmm_sled_update_forms.

INITIALIZATION.
  DATA(lo_upload) = NEW zcl_excel_uploader_new( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM Upload_requests USING p_fname.

AT SELECTION-SCREEN OUTPUT.
  SET PF-STATUS 'ZSEL_STAT2'.

AT SELECTION-SCREEN.
  IF sy-ucomm <> 'FC01' AND p_fname IS INITIAL.
    MESSAGE 'Please Upload a Excel File' TYPE 'E'.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM Excel_download.
  ENDCASE.

START-OF-SELECTION.
  lo_upload->filename = p_fname.
  lo_upload->header_rows_count = 1.
  lo_upload->upload( CHANGING ct_data = gt_upload ).
  IF gt_upload IS INITIAL.
    MESSAGE 'No Data in the upload file' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING.
  ENDIF.

  "validations for excel data
  PERFORM SLED_validation USING gt_upload CHANGING gt_sled_logs.
  "Build SALV
  PERFORM build_alv_sled USING gt_sled_logs.
