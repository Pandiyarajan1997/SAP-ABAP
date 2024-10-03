*&---------------------------------------------------------------------*
*& Report ZMM_GET_PRICE_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_get_price_report.
********global data dec*********
DATA : gv_matnr TYPE mara-matnr,
       gt_price	TYPE TABLE OF	zstr_price_rec,
       gs_layo  TYPE slis_layout_alv.
********selection screen design*********
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : so_matnr FOR gv_matnr.     "material no.
  PARAMETERS : p_date TYPE datum OBLIGATORY.     "date.
  PARAMETERS : p_type TYPE t685-kschl.

SELECTION-SCREEN : END OF BLOCK b1.
********begin of the process*********
START-OF-SELECTION.
********call fm for get price*********
  CALL FUNCTION 'ZBAPI_GET_PRICE'
    EXPORTING
      iv_date  = p_date
    TABLES
      et_price = gt_price
      lt_matnr = so_matnr.
********alv layout design*********
  gs_layo-colwidth_optimize = abap_true.
  gs_layo-zebra = abap_true.
  IF p_type IS NOT INITIAL.
    DELETE gt_price WHERE kschl NE p_type.
  ENDIF.
  IF gt_price IS NOT INITIAL.
********diplay the alv report*********
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_structure_name = 'ZSTR_PRICE_REC'
        is_layout        = gs_layo
      TABLES
        t_outtab         = gt_price.
  ELSE.
    MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
