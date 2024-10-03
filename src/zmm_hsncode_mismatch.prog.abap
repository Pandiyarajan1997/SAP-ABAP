*&---------------------------------------------------------------------*
*& Report ZDUPLICATE_CUSTGROUP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_hsncode_mismatch.

SELECT a~matnr,
a~werks,
a~steuc,
b~maktx FROM marc AS a INNER JOIN makt AS b ON a~matnr = b~matnr
                       INTO TABLE @DATA(it_marc)
                       WHERE werks IN ( '1100' , '1122' , '1144' , '1005' , '1401' )
                       AND   spras = 'E'.

IF sy-subrc = 0.

  DATA : lt_final LIKE it_marc,
         ls_marc  LIKE LINE OF it_marc,
         lt_fcat  TYPE slis_t_fieldcat_alv.   "data dec for fieldcat

  SORT it_marc BY matnr werks.

  LOOP AT it_marc ASSIGNING FIELD-SYMBOL(<fs_marc>).

    AT NEW matnr.
      CLEAR : ls_marc.
      READ TABLE it_marc INTO ls_marc WITH KEY <fs_marc>-matnr BINARY SEARCH.
    ENDAT.

    IF <fs_marc>-steuc NE ls_marc-steuc.
      APPEND <fs_marc> TO lt_final.
      AT END OF matnr.
        APPEND ls_marc TO lt_final.
      ENDAT.
    ENDIF.

  ENDLOOP.

ENDIF.

IF lt_final IS NOT INITIAL.

  SORT lt_final BY matnr werks.
  DELETE ADJACENT DUPLICATES FROM lt_final COMPARING matnr.

  SELECT a~matnr,
  a~werks,
  a~steuc,
  b~maktx FROM marc AS a INNER JOIN makt AS b ON a~matnr = b~matnr
                         INTO TABLE @DATA(lt_final2)
                         FOR ALL ENTRIES IN @lt_final
                         WHERE a~matnr = @lt_final-matnr
                         AND   werks IN ( '1100' , '1122' , '1144' , '1005' , '1401' )
                         AND   spras = 'E'.

  SORT lt_final2 BY matnr werks.

  lt_fcat = VALUE #(
  ( col_pos = 1 fieldname = 'MATNR' seltext_m = 'Material No' )
  ( col_pos = 2 fieldname = 'MAKTX' seltext_m = 'Description' )
  ( col_pos = 3 fieldname = 'WERKS' seltext_m = 'Plant' )
  ( col_pos = 4 fieldname = 'STEUC' seltext_m = 'HSN Code' ) ).



  DATA(ls_layo) = VALUE slis_layout_alv( colwidth_optimize = abap_true
                                         zebra = abap_true ).   "data dec for layout design

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      is_layout   = ls_layo
      it_fieldcat = lt_fcat
    TABLES
      t_outtab    = lt_final2.

ENDIF.
