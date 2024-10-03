*&---------------------------------------------------------------------*
*& Include          ZKP_SD_SALES_REGISTER_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs LIKE vbrk-bukrs OBLIGATORY . " Company code
  SELECT-OPTIONS: g_vbeln FOR vbrk-vbeln.
  SELECT-OPTIONS :s_werks FOR vbrp-werks . " Plant
  SELECT-OPTIONS: s_fkdat FOR vbrk-fkdat.    " Billing Date
  SELECT-OPTIONS:  s_vtweg FOR vbrk-vtweg,    " Distribution channel
                  s_spart FOR vbrk-spart,    " Division
                  s_vkbur FOR vbrp-vkbur.
  PARAMETERS: p_cb AS CHECKBOX .
  PARAMETERS : p_sr AS CHECKBOX.
  PARAMETERS : p_ic AS CHECKBOX .
  PARAMETERS : p_st AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK 1.
