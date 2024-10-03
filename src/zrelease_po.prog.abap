*&---------------------------------------------------------------------------&*
* Object Name     : ZRELEASE_PO                                               *
* Trasaction Code :                                                           *
* Tech Consultant : Raviram                                                   *
* Module Name     : MM                                                        *
* Program Type    : Report                                                    *
* SAP Release     : 6.0                                                       *
* Description     : PO Release                                                *
*******************************************************************************
* Transport No    :                                                           *
* Create Date     : 29-Jan-2020                                               *
*******************************************************************************
*-------------------- CHANGE HISTORY -----------------------------------------*
* Name    Date    Request No       Description                                *
*                                                                             *
*                                                                             *
*-----------------------------------------------------------------------------*

REPORT ZRELEASE_PO.

TABLES:ekko,t16fs.

TYPES: BEGIN OF ty_rel,
        frggr TYPE ekko-frggr,    "Release Group
        frgco TYPE ekko-frgsx,    "Release Code
        ebeln TYPE ekko-ebeln,    "Purch Doc No.
       END OF ty_rel.

TYPES: BEGIN OF t_ekko,
       ebeln TYPE ebeln,          "Purchase Document
       frggr TYPE ekko-frggr,    "Release Group
       frgsx TYPE ekko-frgsx,    "Release Code
       END OF t_ekko.

TYPES:  BEGIN OF i_final,
       ebeln   TYPE ebeln,          " PO
       frgco   TYPE ekko-frgsx,     " Release Code
       msg     LIKE t100-text,      " Message
       status  TYPE icon-id,        " Status
      END OF i_final.

DATA: lt_rel           TYPE TABLE OF ty_rel,
      ls_rel           TYPE ty_rel,
      it_return        TYPE TABLE OF bapireturn,
      wa_return        TYPE bapireturn,
      it_final         TYPE TABLE OF i_final,
      wa_final         TYPE i_final,
      ls_status        TYPE bapimmpara-rel_status,
      ls_rel_ind       TYPE bapimmpara-po_rel_ind,
      ls_subrc         TYPE sy-subrc,
      lt_ekko          TYPE TABLE OF t_ekko,
      ls_ekko          TYPE t_ekko,
      it_fieldcatalog  TYPE slis_t_fieldcat_alv ,
      wa_fieldcatalog  TYPE slis_fieldcat_alv,
      ws_layout        TYPE slis_layout_alv,
      ls_t16fs         TYPE t16fs.

CONSTANTS:
c_red           TYPE icon-id VALUE '@0A@',
c_green         TYPE icon-id VALUE '@08@'.


*PARAMETERS: p_date TYPE sy-datum .
PARAMETERS: p_po TYPE ebeln .
*INITIALIZATION.
*p_date = sy-datum.

*&---------------------------------------------------------------------*
*&  START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
****Release Strategy for PO
if sy-batch EQ 'X'.
  PERFORM get_data.
  PERFORM po_release.
  PERFORM alv_display.
ENDIF.

**** Release Code and Release Strategy Wise
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  CLEAR: ls_t16fs,lt_rel,ls_rel,it_return,ls_ekko,lt_ekko.

  SELECT ebeln frggr frgsx FROM ekko INTO TABLE lt_ekko     "WHERE frgzu = ' '
*                                          AND statu = 'B'     "Auto PO Release
                                           WHERE ebeln = p_po  AND FRGZU = ' '.
*                                          AND aedat = p_date AND FRGRL = 'X'.

  IF lt_ekko IS NOT INITIAL.
    LOOP AT lt_ekko INTO ls_ekko.

      IF ls_ekko IS NOT INITIAL.
        SELECT SINGLE * FROM t16fs INTO ls_t16fs WHERE frggr = ls_ekko-frggr
                                                  AND frgsx = ls_ekko-frgsx.


        IF ls_t16fs IS NOT INITIAL.

          IF ls_t16fs-frgc1 IS NOT INITIAL.
            ls_rel-frggr = ls_t16fs-frggr.
            ls_rel-frgco = ls_t16fs-frgc1.
            ls_rel-ebeln = ls_ekko-ebeln.
            APPEND ls_rel TO lt_rel.
            CLEAR:ls_rel.
          ENDIF.
*
          IF ls_t16fs-frgc2 IS NOT INITIAL.
            ls_rel-frggr = ls_t16fs-frggr.
            ls_rel-frgco = ls_t16fs-frgc2.
            ls_rel-ebeln = ls_ekko-ebeln.
            APPEND ls_rel TO lt_rel.
            CLEAR:ls_rel.
          ENDIF.

          IF ls_t16fs-frgc3 IS NOT INITIAL.
            ls_rel-frggr = ls_t16fs-frggr.
            ls_rel-frgco = ls_t16fs-frgc3.
            ls_rel-ebeln = ls_ekko-ebeln.
            APPEND ls_rel TO lt_rel.
            CLEAR:ls_rel.
          ENDIF.

          IF ls_t16fs-frgc4 IS NOT INITIAL.
            ls_rel-frggr = ls_t16fs-frggr.
            ls_rel-frgco = ls_t16fs-frgc4.
            ls_rel-ebeln = ls_ekko-ebeln.
            APPEND ls_rel TO lt_rel.
            CLEAR:ls_rel.
          ENDIF.

          IF ls_t16fs-frgc5 IS NOT INITIAL.
            ls_rel-frggr = ls_t16fs-frggr.
            ls_rel-frgco = ls_t16fs-frgc5.
            ls_rel-ebeln = ls_ekko-ebeln.
            APPEND ls_rel TO lt_rel.
            CLEAR:ls_rel.
          ENDIF.

          IF ls_t16fs-frgc6 IS NOT INITIAL.
            ls_rel-frggr = ls_t16fs-frggr.
            ls_rel-frgco = ls_t16fs-frgc6.
            ls_rel-ebeln = ls_ekko-ebeln.
            APPEND ls_rel TO lt_rel.
            CLEAR:ls_rel.
          ENDIF.

          IF ls_t16fs-frgc7 IS NOT INITIAL.
            ls_rel-frggr = ls_t16fs-frggr.
            ls_rel-frgco = ls_t16fs-frgc7.
            ls_rel-ebeln = ls_ekko-ebeln.
            APPEND ls_rel TO lt_rel.
            CLEAR:ls_rel.
          ENDIF.

          IF ls_t16fs-frgc8 IS NOT INITIAL.
            ls_rel-frggr = ls_t16fs-frggr.
            ls_rel-frgco = ls_t16fs-frgc8.
            ls_rel-ebeln = ls_ekko-ebeln.
            APPEND ls_rel TO lt_rel.
            CLEAR:ls_rel.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: LS_EKKO,LS_T16FS,LS_REL.
    ENDLOOP.
  ELSE.
    MOVE 'No Records to Release' TO wa_final-msg.
    MOVE c_green                 TO wa_final-status.
    APPEND wa_final TO it_final.
    CLEAR: wa_final.

  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PO_RELEASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM po_release .

  IF lt_rel IS NOT INITIAL.

    LOOP AT lt_rel INTO ls_rel.

      CALL FUNCTION 'BAPI_PO_RELEASE'
        EXPORTING
          purchaseorder     = ls_rel-ebeln
          po_rel_code       = ls_rel-frgco
          use_exceptions    = 'X'
          no_commit         = 'X'
        IMPORTING
          rel_status_new    = ls_status
          rel_indicator_new = ls_rel_ind
          ret_code          = ls_subrc
        TABLES
          return            = it_return.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        MOVE ls_rel-frgco   TO wa_final-frgco.
        MOVE ls_REL-ebeln   TO wa_final-ebeln.
        MOVE 'Successful'   TO wa_final-msg.
        MOVE  c_green       TO wa_final-status.
        APPEND wa_final TO it_final.
        CLEAR: wa_final.
      ELSE.
        READ TABLE it_return INTO wa_return WITH KEY type  = 'E'.
        IF sy-subrc IS INITIAL.
          MOVE ls_rel-frgco         TO wa_final-frgco.
          MOVE ls_REL-ebeln         TO wa_final-ebeln.
          MOVE wa_return-message    TO wa_final-msg.
          MOVE  c_red               TO wa_final-status.
          APPEND wa_final TO it_final.
          CLEAR: wa_final.
        ENDIF.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      REFRESH : it_return.
      CLEAR: ls_rel,ls_status,ls_rel_ind,ls_subrc,wa_return.

    ENDLOOP.
    REFRESH: lt_rel.

  ENDIF.

ENDFORM.                    " PO_RELEASE
**&---------------------------------------------------------------------*
**&      Form  ALV_DISPLAY
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
FORM alv_display .

  DATA: lv  TYPE i.

  lv = 1.


  wa_fieldcatalog-fieldname = 'EBELN'.
  wa_fieldcatalog-col_pos   = lv + 1.
  wa_fieldcatalog-outputlen = 10.
  wa_fieldcatalog-seltext_m = 'Purchase Document'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR wa_fieldcatalog.

  wa_fieldcatalog-fieldname = 'FRGCO'.
  wa_fieldcatalog-col_pos   = lv + 1.
  wa_fieldcatalog-outputlen = 10.
  wa_fieldcatalog-seltext_m = 'Release Code'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR wa_fieldcatalog.

  wa_fieldcatalog-fieldname = 'MSG'.
  wa_fieldcatalog-col_pos   = lv + 1.
  wa_fieldcatalog-outputlen = 80.
  wa_fieldcatalog-no_zero   = 'X'.
  wa_fieldcatalog-seltext_m = 'Error Message'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR wa_fieldcatalog.

  wa_fieldcatalog-fieldname = 'STATUS'.
  wa_fieldcatalog-col_pos   = lv + 1.
  wa_fieldcatalog-outputlen = 10.
  wa_fieldcatalog-seltext_m = 'Status'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR wa_fieldcatalog.


  ws_layout-colwidth_optimize = 'X'.
  ws_layout-zebra             = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-cprog
      is_layout          = ws_layout
      it_fieldcat        = it_fieldcatalog
      i_save             = 'X'
    TABLES
      t_outtab           = it_final.


REFRESH : it_final,lt_rel,lt_ekko.
ENDFORM.                    " ALV_DISPLAY
