*&---------------------------------------------------------------------*
*& Report ZMM_PO_FORM_DOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
** Program Automatic Email for Vendor as on daywise **
"created by: Samsudeen M
"Created on: 03.11.2022
"Reference by: Ramakrishnan J
" Description: Program Automatic Email for Vendor as on daywise
************************************************************************

REPORT zmm_po_form_email_vend.
** Data Declarations **
INCLUDE zmm_po_form_email_top.
** Slection Screen **
INCLUDE zmm_po_form_email_sel.
*** Subroutines For Email **
INCLUDE zmm_po_form_email_forms.
** Initialization **
INITIALIZATION.

START-OF-SELECTION.
**Actual Process**
  PERFORM actual_process.
** Message Display **
  IF gt_alv[] IS NOT INITIAL.
    PERFORM alv_display.
  ENDIF.
