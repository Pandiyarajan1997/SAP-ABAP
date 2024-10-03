FUNCTION zget_data_for_app1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(COMP_CODE) TYPE  BUKRS
*"  TABLES
*"      IT_MARM STRUCTURE  MARM
*"----------------------------------------------------------------------

  TYPES : BEGIN OF lv_ekpo,
            bukrs TYPE bukrs,
           werks    TYPE werks,
            meins    TYPE meins,
            matnr    TYPE matnr,
          END OF lv_ekpo.

  DATA : lt_ekpo TYPE TABLE OF lv_ekpo.
  DATA : ls_ekpo TYPE lv_ekpo.

  SELECT bukrs werks meins matnr FROM ekpo INTO TABLE lt_ekpo WHERE bukrs = comp_code.

  IF lt_ekpo IS NOT INITIAL.

    SELECT * FROM marm INTO TABLE it_marm
      FOR ALL ENTRIES IN lt_ekpo
      WHERE matnr = lt_ekpo-matnr.
  ENDIF.




ENDFUNCTION.
