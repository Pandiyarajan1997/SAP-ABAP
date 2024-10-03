*&---------------------------------------------------------------------*
*& Report ZPF_MAP_DELETE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpf_map_delete.
TYPES: BEGIN OF ty_msg,
         kunnr TYPE kunnr,
         vkorg TYPE vkorg,
         vtweg TYPE vtweg,
         spart TYPE spart,
         msg   TYPE char50,
       END OF ty_msg.

DATA: lt_disp TYPE TABLE OF ty_msg,
      ls_disp TYPE ty_msg.

********** Internal Table for ALV **********************
DATA: lt_fcat   TYPE lvc_t_fcat,
      ls_fcat   TYPE lvc_s_fcat,
      ls_layout TYPE lvc_s_layo.

DATA: lo_grid TYPE REF TO cl_salv_table.

DATA: lv_cust TYPE kna1-kunnr.
SELECT-OPTIONS: s_cust FOR  lv_cust.

START-OF-SELECTION.
  SELECT kunnr,vkorg,vtweg,spart FROM knvv INTO TABLE @DATA(lt_knvv)
                                 WHERE kunnr IN @s_cust.
  IF sy-subrc = 0.
    SORT lt_knvv BY kunnr vkorg vtweg spart.
    SELECT kunnr,vkorg,vtweg,spart FROM knvp INTO TABLE @DATA(lt_knvp)
                                   FOR ALL ENTRIES IN @lt_knvv
                                   WHERE kunnr EQ @lt_knvv-kunnr.
  ENDIF.

  LOOP AT lt_knvp INTO DATA(ls_knvp).
    READ TABLE lt_knvv INTO DATA(ls_knvv) WITH KEY kunnr = ls_knvp-kunnr
                                                   vkorg = ls_knvp-vkorg
                                                   vtweg = ls_knvp-vtweg
                                                   spart = ls_knvp-spart BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE FROM knvp WHERE kunnr = ls_knvp-kunnr AND vkorg = ls_knvp-vkorg AND vtweg = ls_knvp-vtweg AND spart = ls_knvp-spart.
      CLEAR: ls_disp.
      ls_disp-kunnr = ls_knvp-kunnr.
      ls_disp-vkorg = ls_knvp-vkorg.
      ls_disp-vtweg = ls_knvp-vtweg.
      ls_disp-spart = ls_knvp-spart.
      ls_disp-msg = 'Deleted'.
      APPEND ls_disp TO lt_disp.
    ENDIF.
  ENDLOOP.

  REFRESH: lt_fcat.
  PERFORM f_fieldcat USING 'KUNNR'   'Customer No'          1 space.
  PERFORM f_fieldcat USING 'VKORG'   'Sales Organisation'   2 space.
  PERFORM f_fieldcat USING 'VTWEG'   'Distribution Channel' 3 space.
  PERFORM f_fieldcat USING 'SPART'   'Division'             4 space.
  PERFORM f_fieldcat USING 'MSG'     'Messages'             5 space.
  IF lt_disp[] IS NOT INITIAL.
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_grid
          CHANGING
            t_table      = lt_disp.
      CATCH cx_salv_msg.

    ENDTRY.

    CALL METHOD lo_grid->display.

  ENDIF.


FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 .
  ls_fcat-fieldname = f_var1.
  ls_fcat-coltext = f_var2.
  ls_fcat-col_pos = f_var3.
  ls_fcat-edit = f_var4.
  APPEND ls_fcat TO lt_fcat.
ENDFORM.
