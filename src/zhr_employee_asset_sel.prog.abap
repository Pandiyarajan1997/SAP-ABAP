*&---------------------------------------------------------------------*
*& Include          ZHR_EMPLOYEE_ASSET_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_rad1 RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND grp1 MODIF ID bl1,
              p_rad2 RADIOBUTTON GROUP rad  MODIF ID bl1.

  SELECTION-SCREEN SKIP.

  PARAMETERS : p_fname  TYPE rlgrap-filename MODIF ID bl2.
  SELECT-OPTIONS: s_pernr FOR lv_pernr MODIF ID bl3 MATCHCODE OBJECT prem,
                  s_bukrs FOR lv_bukrs MODIF ID bl3.

SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY 1.
