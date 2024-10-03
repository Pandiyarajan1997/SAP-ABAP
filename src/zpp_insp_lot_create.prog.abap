*&---------------------------------------------------------------------*
*& Report ZPP_INSP_LOT_CREATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&                                                                     *
*&---------------------------------------------------------------------*
*& Functional                  : Ragu M                                *
*& Developer                   : Himansu Patnaik                       *
*& Created On                  : 29.08.2022                            *
*& Description                 : Create  Manual Inspection Lots for    *
*                                Samples.                              *
*                                view.                                 *
*& Report Name                 : ZPP_INSP_LOT_CREATE                   *
*& Report Name                 :                                       *
*& Transport Request           : DEVK932137                            *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*                          Modification Log                            *
*----------------------------------------------------------------------*
* Date         |     Name          |   Request No.  |   Description    *
*              |                   |                |                  *
*              |                   |                |                  *
*              |                   |                |                  *
*----------------------------------------------------------------------*
REPORT zpp_insp_lot_create.

*----------------------------------------------------------------------*
*-Data Types Declaration                                               *
*----------------------------------------------------------------------*

DATA: gt_bdcdata TYPE STANDARD TABLE OF bdcdata,
      gs_bdcdata TYPE bdcdata.

DATA : gt_msgcoll TYPE STANDARD TABLE OF bdcmsgcoll,
       gv_mode    TYPE c VALUE 'N',
       gv_flag    TYPE c,
       gv_count   TYPE char2.

TABLES : qprs.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS : p_prart TYPE qprs-prart OBLIGATORY,
               p_phynr TYPE qprs-phynr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

IF p_prart IS NOT INITIAL AND
   p_phynr IS NOT INITIAL .

  PERFORM bdc_dynpro      USING 'RQPRLS10'          '1000'.

  PERFORM bdc_field       USING 'BDC_CURSOR'        'S_PHYNR-LOW'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=ONLI'.
  PERFORM bdc_field       USING 'S_PRART-LOW'       p_prart.
  PERFORM bdc_field       USING 'S_PHYNR-LOW'       p_phynr.

  PERFORM bdc_dynpro      USING 'SAPMSSY0'          '0120'.

  PERFORM bdc_field       USING 'BDC_CURSOR'        '04/03'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=&ALL'.

  PERFORM bdc_dynpro      USING 'SAPMSSY0'          '0120'.

  PERFORM bdc_field       USING 'BDC_CURSOR'        '04/03'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=XLOS'.

  PERFORM bdc_dynpro      USING 'SAPMSSY0'          '0120'.

  PERFORM bdc_field       USING 'BDC_CURSOR'        '04/03'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=XSAV'.

  CALL TRANSACTION 'QPR5' USING gt_bdcdata
                      MODE gv_mode
                      UPDATE 'S'
                      MESSAGES  INTO gt_msgcoll.

  READ TABLE gt_msgcoll ASSIGNING FIELD-SYMBOL(<fs_msgcoll>)
                        WITH KEY msgtyp = 'E'.
  IF sy-subrc <> 0.
    CLEAR gv_flag.
    gv_flag = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
  CLEAR : gt_bdcdata,gt_msgcoll.
ENDIF.
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.

  gs_bdcdata-program  = program.
  gs_bdcdata-dynpro   = dynpro.
  gs_bdcdata-dynbegin = 'X'.
  APPEND gs_bdcdata TO gt_bdcdata.
  CLEAR gs_bdcdata.
ENDFORM.                    "bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.

  gs_bdcdata-fnam = fnam.
  gs_bdcdata-fval = fval.
  APPEND gs_bdcdata TO gt_bdcdata.
  CLEAR gs_bdcdata .
ENDFORM.                    "bdc_field
