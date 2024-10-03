*&---------------------------------------------------------------------*
*& Report ZSD_POSVENDOR_PF_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_posvendor_pf_upload.
*----------------------------------------------------------------------*
"Created By: Samsudeen M
"Created on: 28.01.2023
"Reference: Ramakrishnan J
"Purpose: For Uploading Partner function for customers only position vendors
*----------------------------------------------------------------------------*
*---Data Declaration and selection screen  --------*
INCLUDE zsd_posvendor_pf_top.
*--- Subroutines for Actual Process ---------------*
INCLUDE zsd_posvendor_pf_upforms.
*-----Input Help in selction screen ---------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM f_get_f4_help USING p_fname.
*--- Start of Selection ---------------------------*
START-OF-SELECTION.
  "Convert excel data to internal table SAP
  PERFORM f_convert_excel_data USING p_fname.
  "Validate Excel file and Process the excel File
  PERFORM f_validate_process_data.
  " ALV Display
  PERFORM f_display_alv.
*----------------------------------------------------*
