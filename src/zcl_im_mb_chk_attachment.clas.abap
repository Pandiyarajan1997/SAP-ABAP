class ZCL_IM_MB_CHK_ATTACHMENT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_MIGO_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MB_CHK_ATTACHMENT IMPLEMENTATION.


  method IF_EX_MB_MIGO_BADI~CHECK_HEADER.
  endmethod.


  METHOD if_ex_mb_migo_badi~check_item.
*
*    DATA: ls_zmigoattachment TYPE zmigoattachment.
*    DATA: ls_sood LIKE ls_zmigoattachment,
*          lt_sood TYPE TABLE OF zmigoattachment.
*
*    IF sy-tcode = 'MIGO'.
*      SELECT SINGLE * FROM zmigoattachment INTO ls_zmigoattachment WHERE cronam = sy-uname.
*      IF sy-subrc = 0.
*        SELECT * FROM sood INTO CORRESPONDING FIELDS OF TABLE lt_sood WHERE cronam = sy-uname.
*        SORT lt_sood BY crdat DESCENDING crtim DESCENDING.
*        READ TABLE lt_sood INDEX 1 INTO ls_sood.
*        IF sy-subrc = 0 AND ls_sood = ls_zmigoattachment.
*          MESSAGE 'Upload an attachment!' TYPE 'E'.
*        ENDIF.
*      ENDIF.
*      CLEAR: ls_zmigoattachment, ls_sood, lt_sood.
*    ENDIF.

  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_DELETE.
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_LOAD.
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_SAVE.
  endmethod.


  METHOD if_ex_mb_migo_badi~init.

*    DATA: ls_sood TYPE zmigoattachment,
*          lt_sood TYPE TABLE OF zmigoattachment.
*
*    SELECT * FROM sood INTO CORRESPONDING FIELDS OF TABLE lt_sood WHERE cronam = sy-uname.
*    IF sy-subrc = 0.
*      SORT lt_sood BY crdat DESCENDING crtim DESCENDING.
*      READ TABLE lt_sood INDEX 1 INTO ls_sood.
*      IF ls_sood IS NOT INITIAL.
*        MODIFY zmigoattachment FROM ls_sood.
*      ENDIF.
*      CLEAR: ls_sood, lt_sood.
*    ENDIF.
  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~LINE_DELETE.
  endmethod.


  method IF_EX_MB_MIGO_BADI~LINE_MODIFY.
  endmethod.


  method IF_EX_MB_MIGO_BADI~MAA_LINE_ID_ADJUST.
  endmethod.


  method IF_EX_MB_MIGO_BADI~MODE_SET.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_DETAIL.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_HEADER.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_DETAIL.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_HEADER.
  endmethod.


  METHOD if_ex_mb_migo_badi~post_document.
*    DATA: ls_sood TYPE zmigoattachment,
*          lt_sood TYPE TABLE OF zmigoattachment.
*
*
*    DATA : wa_mseg TYPE  mseg .
*
*    DATA: lv_mtart TYPE mtart.
*
*    IF sy-tcode = 'MIGO'.
*      IF it_mseg[] IS NOT INITIAL.
** check for specific plant code and movement type as 101 - goods receipt
*        READ TABLE it_mseg INTO wa_mseg INDEX 1.
*        IF ( wa_mseg-werks = '1002' OR
*           wa_mseg-werks = '1003' OR
*           wa_mseg-werks = '1005' OR
*           wa_mseg-werks = '1401' ) AND wa_mseg-bwart = '101'.
*
*          LOOP AT it_mseg INTO wa_mseg.
*            CLEAR lv_mtart.
*            SELECT SINGLE mtart INTO lv_mtart FROM mara WHERE matnr = wa_mseg-matnr.
*            IF sy-subrc = 0 AND lv_mtart = 'ROH' .
*              SELECT * FROM sood INTO CORRESPONDING FIELDS OF TABLE lt_sood WHERE cronam = sy-uname.
*              IF sy-subrc = 0.
*                SORT lt_sood BY crdat DESCENDING crtim DESCENDING.
*                READ TABLE lt_sood INDEX 1 INTO ls_sood.
*                IF ls_sood IS NOT INITIAL.
*                  MODIFY zmigoattachment FROM ls_sood.
*                  exit.
*                ENDIF.
*                CLEAR: ls_sood, lt_sood.
*              ENDIF.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~PROPOSE_SERIALNUMBERS.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PUBLISH_MATERIAL_ITEM.
  endmethod.


  method IF_EX_MB_MIGO_BADI~RESET.
  endmethod.


  method IF_EX_MB_MIGO_BADI~STATUS_AND_HEADER.
  endmethod.
ENDCLASS.
