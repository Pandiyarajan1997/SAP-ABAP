"Created by: Pandiarajan
"Created on: 24.07.2024
"Reference by: Ramakrishnan J
"Purpose : Report for plant wise report with expiry days
*-------------------------------------------------------------*

REPORT zmm_blocked_stock.


TYPES : BEGIN OF ty_alv,

          status    TYPE zaxis1_dte_status,
          kunnr     TYPE zdist,
          werks     TYPE werks_d,
          name1     TYPE name1,
          lgort     TYPE lgort_d,
          mtart     TYPE mtart,
          matkl     TYPE matkl,
          wgbez     TYPE wgbez,
          spart     TYPE spart,
          vtext     TYPE vtext,
          matnr     TYPE matnr,
          bismt     TYPE bismt,
          maktx     TYPE maktx,
          charg     TYPE charg_d,
          stk_type  TYPE char15,
          stock     TYPE ztotal_qty, "speme,
          mhdrz     TYPE mhdrz,
          mhdhb     TYPE mhdhb,
          hsdat     TYPE hsdat,
          vfdat     TYPE vfdat,
          days      TYPE i,
          new_lwedt TYPE lwedt,
          old_lwedt TYPE lwedt,

        END OF ty_alv.

DATA : gv_matnr TYPE mara-matnr,
       gv_charg TYPE mchb-charg,
       gv_werks TYPE mard-werks,
       gv_lgort TYPE mard-lgort,
       gv_mtart TYPE mara-mtart,
       gv_title TYPE lvc_title.

DATA : gt_alv TYPE TABLE OF ty_alv.


SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : so_matnr FOR gv_matnr,
                   so_charg FOR gv_charg,
                   so_werks FOR gv_werks,
                   so_lgort FOR gv_lgort,
                   so_mtart FOR gv_mtart.

SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN : SKIP 1.

PARAMETERS : r1 RADIOBUTTON GROUP g1 DEFAULT 'X',
             r2 RADIOBUTTON GROUP g1,
             r3 RADIOBUTTON GROUP g1,
             r4 RADIOBUTTON GROUP g1.


START-OF-SELECTION.

  PERFORM data_process.

  IF gt_alv IS NOT INITIAL.
    PERFORM alv_display.
  ELSE.
    MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

END-OF-SELECTION.

FORM data_process.

  DATA : ls_alv TYPE ty_alv.

  IF r1 = 'X'.

    gv_title = 'Blocked Stock Report'.

    DATA : lr_cspem TYPE RANGE OF speme.

    APPEND VALUE #( sign   = 'I'
                    option = 'NE'
                    low    = abap_false ) TO lr_cspem.

  ELSEIF r2 = 'X'.

    gv_title = 'Unrestricted Stock Report'.

    DATA : lr_labst TYPE RANGE OF labst.

    APPEND VALUE #( sign   = 'I'
                    option = 'NE'
                    low    = abap_false ) TO lr_labst.

  ELSEIF r3 = 'X'.

    gv_title = 'Quality Inspection Stock Report'.

    DATA : lr_insme TYPE RANGE OF insme.

    APPEND VALUE #( sign   = 'I'
                    option = 'NE'
                    low    = abap_false ) TO lr_insme.

  ELSEIF r4 = 'X'.

    gv_title = 'Overall Stock Report'.

  ENDIF.

*************************fetch the datas from mchb*********************

  SELECT a~werks,c~name1,a~lgort,a~matnr,a~charg,a~clabs AS unrestricted,a~cspem AS blocked,
         a~cinsm AS quality,b~hsdat,b~vfdat,b~lwedt
         FROM mchb AS a
         INNER JOIN mch1  AS b ON  a~matnr = b~matnr
         AND a~charg = b~charg
         INNER JOIN t001w AS c ON  a~werks = c~werks
         INNER JOIN mara  AS d ON  a~matnr = d~matnr
         INTO TABLE @DATA(lt_stock) WHERE a~matnr IN @so_matnr
                                    AND   a~werks IN @so_werks
                                    AND   a~lgort IN @so_lgort
                                    AND   a~charg IN @so_charg
                                    AND   a~cspem IN @lr_cspem
                                    AND   a~clabs IN @lr_labst
                                    AND   a~cinsm IN @lr_insme
                                    AND   d~mtart IN @so_mtart.

  IF lt_stock IS NOT INITIAL.


    SELECT a~matnr,a~mtart,a~bismt,a~matkl,a~spart,
           a~lvorm,a~status,a~mhdrz,a~mhdhb,b~maktx,c~wgbez,d~vtext
                       FROM mara AS a
                       LEFT OUTER JOIN makt  AS b ON a~matnr = b~matnr
                       LEFT OUTER JOIN t023t AS c ON a~matkl = c~matkl
                       LEFT OUTER JOIN tspat AS d ON a~spart = d~spart
                       INTO TABLE @DATA(lt_mara)
                       FOR ALL ENTRIES IN @lt_stock
                       WHERE a~matnr = @lt_stock-matnr
                       AND   b~spras = @sy-langu.
*                       AND   c~spras = @sy-langu
*                       AND   d~spras = @sy-langu.
    IF sy-subrc = 0.
      SORT : lt_mara BY matnr.
    ENDIF.

    SELECT kunnr,werks FROM kna1 INTO TABLE @DATA(lt_kna1) WHERE werks <> @abap_false.
    IF sy-subrc = 0.
      SORT : lt_kna1 BY werks.
    ENDIF.

*    SELECT * FROM t023 INTO TABLE @DATA(lt_t023).
*      IF sy-subrc = 0.
*        SORT : lt_T023 by matkl.
*      ENDIF.
*    SELECT * FROM TSPA INTO TABLE @DATA(lt_TSPA).
*      IF sy-subrc = 0.
*        SORT : lt_tspa by m.
*      ENDIF.

*******************get the old material code batch details****************
    SELECT matnr,charg,lwedt FROM mch1 INTO TABLE @DATA(lt_mch1)
                             FOR ALL ENTRIES IN @lt_mara
                             WHERE matnr = @lt_mara-bismt.
    IF sy-subrc = 0.
      SORT : lt_mch1 BY matnr charg.
    ENDIF.

    LOOP AT lt_stock ASSIGNING FIELD-SYMBOL(<fs_stock>).

**************read the data from mara*************
      READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = <fs_stock>-matnr BINARY SEARCH.

**************read the data from kna1*************
      READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY werks = <fs_stock>-werks BINARY SEARCH.

******************fill the data for output display***********
      CLEAR : ls_alv.
      ls_alv-kunnr = ls_kna1-kunnr.
      ls_alv-werks = <fs_stock>-werks.
      ls_alv-name1 = <fs_stock>-name1.
      ls_alv-lgort = <fs_stock>-lgort.
      ls_alv-matkl = ls_mara-matkl.
      ls_alv-wgbez = ls_mara-wgbez.
      ls_alv-spart = ls_mara-spart.
      ls_alv-vtext = ls_mara-vtext.
      ls_alv-mtart = ls_mara-mtart.
      ls_alv-bismt = ls_mara-bismt.
      ls_alv-mhdhb = ls_mara-mhdhb.
      ls_alv-mhdrz = ls_mara-mhdrz.
      ls_alv-matnr = <fs_stock>-matnr.
      ls_alv-maktx = ls_mara-maktx.
      ls_alv-charg = <fs_stock>-charg.
      ls_alv-hsdat = <fs_stock>-hsdat.
      ls_alv-vfdat = <fs_stock>-vfdat.
      ls_alv-new_lwedt = <fs_stock>-lwedt.

************Check the material status*********
      IF ls_mara-lvorm = 'X' OR ls_mara-status = 'X'.
        ls_alv-status = 'INACTIVE'.
      ELSE.
        ls_alv-status = 'ACTIVE'.
      ENDIF.

*********************calculation exp days*********
      IF ls_alv-vfdat IS NOT INITIAL.
        ls_alv-days = ls_alv-vfdat - sy-datum.
      ENDIF.

*****************read the old material code batch details**************
      READ TABLE lt_mch1 INTO DATA(ls_mch1) WITH KEY matnr = ls_alv-bismt
                                                     charg = <fs_stock>-charg BINARY SEARCH.
      IF sy-subrc = 0.
        ls_alv-old_lwedt = ls_mch1-lwedt.
      ENDIF.

*******************stock moving process***********
      IF r1 = abap_true.                          "Blocked stock

        ls_alv-stock    = <fs_stock>-blocked.
        ls_alv-stk_type = 'BLOCKED'.

      ELSEIF r2 = abap_true.                      "Unrestricted stock

        ls_alv-stock    = <fs_stock>-unrestricted.
        ls_alv-stk_type = 'UNRESTRICTED'.

      ELSEIF r3 = abap_true.                      "Quality stock

        ls_alv-stock    = <fs_stock>-quality.
        ls_alv-stk_type = 'QUALITY'.

      ELSEIF r4 = abap_true.                      "overall stock

        IF <fs_stock>-blocked IS NOT INITIAL.
          ls_alv-stock    = <fs_stock>-blocked.
          ls_alv-stk_type = 'BLOCKED'.
          APPEND : ls_alv TO gt_alv.
        ENDIF.

        IF <fs_stock>-unrestricted IS NOT INITIAL.
          ls_alv-stock    = <fs_stock>-unrestricted.
          ls_alv-stk_type = 'UNRESTRICTED'.
          APPEND : ls_alv TO gt_alv.
        ENDIF.

        IF <fs_stock>-quality IS NOT INITIAL.
          ls_alv-stock    = <fs_stock>-quality.
          ls_alv-stk_type = 'QUALITY'.
          APPEND : ls_alv TO gt_alv.
        ENDIF.

        CLEAR : ls_mara,ls_kna1,ls_mch1.
        CONTINUE.

      ENDIF.
*************append to the final internal table***********
      APPEND : ls_alv TO gt_alv.
      CLEAR : ls_mara,ls_kna1,ls_mch1.

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM alv_display.

  DATA : lo_gr_functions  TYPE REF TO cl_salv_functions_list.
  DATA : lo_display       TYPE REF TO cl_salv_display_settings. " Variable for layout settings
  DATA : lo_columns TYPE REF TO cl_salv_columns,
         lo_column  TYPE REF TO cl_salv_column_table.

  DATA : lo_gr_alv TYPE REF TO cl_salv_table. " Variables for ALV properties

  SORT : gt_alv BY werks lgort matnr charg.

* create the alv object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_alv.
    CATCH cx_salv_msg.
  ENDTRY.

* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

********change the field name**************
  TRY.
      lo_column ?= lo_columns->get_column( 'STK_TYPE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'STK Type' ).
      lo_column->set_medium_text( 'Stock Type' ).
      lo_column->set_short_text( 'Stock Type' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'DAYS' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Days Exp.' ).
      lo_column->set_medium_text( 'Days Exp.' ).
      lo_column->set_short_text( 'Days Exp.' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'STOCK' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Stock' ).
      lo_column->set_medium_text( 'Stock' ).
      lo_column->set_short_text( 'Stock' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'BISMT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_short_text( 'Old Mat.' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'NEW_LWEDT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_short_text( 'New LastGR' ).
      lo_column->set_medium_text( 'New mat. last GR' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'OLD_LWEDT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_short_text( 'Old LastGR' ).
      lo_column->set_medium_text( 'Old mat. last GR' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  " Set Title
  IF gv_title IS NOT INITIAL.
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_list_header( gv_title ).
  ENDIF.

*********Display the ALv screen********
  lo_gr_alv->display( ).

ENDFORM.
