*&---------------------------------------------------------------------*
*& Include          ZSD_REP_INVOICECOUNT_COGS_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t01.

  PARAMETERS : r1 RADIOBUTTON GROUP g1 USER-COMMAND uc1 MODIF ID tb2 ,          " SELECTION SCREEN FOR INVOICE

               r2 RADIOBUTTON GROUP g1 MODIF ID tb2 DEFAULT 'X'.

  SELECTION-SCREEN SKIP.

  SELECT-OPTIONS: so_bukrs FOR lv_bukrs,
                  so_fkdat FOR w_aux_fkdat MODIF ID tb2 DEFAULT sy-datum OBLIGATORY,        " SELECTION SCREEN ELEMENTS FOR INVOICE
                  so_fkart  FOR w_fkart MODIF ID tb2,
                  so_vbeln FOR w_vbeln MODIF ID tb2,
                  so_kunag FOR w_kunag MODIF ID tb2,
                  so_matnr FOR w_matnr MODIF ID tb2,
                  so_charg FOR w_charg MODIF ID tb5,
                  so_vkbu2 FOR w_inv_vkbur MODIF ID lb2 OBLIGATORY,                                    " SELECT OPTIONS FOR SALES OFFICE IN invoice
                  so_spart FOR w_spart MODIF ID tb2 .

  SELECTION-SCREEN SKIP.

  PARAMETERS: p_cb AS CHECKBOX USER-COMMAND cbc MODIF ID ab2.
  PARAMETERS: p_sr AS CHECKBOX USER-COMMAND cbc MODIF ID ab3.
  PARAMETERS: p_fs AS CHECKBOX USER-COMMAND cbc MODIF ID ab4.
  PARAMETERS: p_ic AS CHECKBOX USER-COMMAND cbc MODIF ID ib2.
  PARAMETERS: p_bp AS CHECKBOX USER-COMMAND cbc MODIF ID bp1.  "Added by Samsudeen on (23.06.2022)
SELECTION-SCREEN: END OF BLOCK b2.
