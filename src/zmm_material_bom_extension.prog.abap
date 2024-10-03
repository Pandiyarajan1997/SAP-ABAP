*&---------------------------------------------------------------------*
*& Report ZMM_MATERIAL_BOM_EXTENSION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_material_bom_extension.
"Data Declarations
INCLUDE zmm_mat_bom_extn_top.

"Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_fname TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

"Subroutines
INCLUDE zmm_mat_bom_extn_forms.

DATA: lo_bom_extn TYPE REF TO lcl_bom_process.

INITIALIZATION.
  CREATE OBJECT lo_bom_extn.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  "F4 Help for Filename
  lo_bom_extn->f4_value_help( ).

START-OF-SELECTION.
  "Convert excel
  lo_bom_extn->convert_excel( ).
  "Actual checks and Process
  lo_bom_extn->f_create_extend_bom( ).
