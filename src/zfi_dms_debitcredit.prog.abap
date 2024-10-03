
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 28.02.2024
*
*  Requester Name            : Ramakrishnan
*
*  Business Logic            : Distributor debit / credit note report
*
*  Released on Date          :
* Hardcoded                  : gv_bukrs = 'DMS1'.
*=======================================================================
REPORT zfi_dms_debitcredit.
**************Alv sctructure*********
TYPES: BEGIN OF ty_alv,
         distributor TYPE zdist,
         dist_name   TYPE zdist_name,
         plant       TYPE werks_d,
         ret_code    TYPE kunnr,        "dealer
         reta_name   TYPE zret_name,
         type        TYPE char20,
         gl_account  TYPE saknr,
         glacc_txt   TYPE txt50,
         blart       TYPE blart,
         fisyear     TYPE gjahr,
         docno       TYPE belnr_d,
         docdate     TYPE bldat,
         pdate       TYPE budat,
         refdoc      TYPE xblnr1,   "ref dealer
         amount      TYPE wrbtr,
         item_txt    TYPE sgtxt,
         rev_doc     TYPE stblg,
         rev_yr      TYPE stjah,
         rev_type    TYPE blart,
         rev_date    TYPE budat,
       END OF ty_alv.
****************Global data dec*******************
DATA : gt_alv  TYPE TABLE OF ty_alv.
DATA : lo_gr_alv TYPE REF TO cl_salv_table. " Variables for ALV properties
DATA : gv_bukrs TYPE bukrs.
DATA : gv_kunnr TYPE bsad-kunnr,
       lv_type  TYPE char20.
*******************Selection screen design******************
SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS : so_kunnr FOR gv_kunnr NO INTERVALS,      "distributor
                   so_retai FOR gv_kunnr NO INTERVALS,      "retailer
                   so_pdate FOR sy-datum MODIF ID a.        "posting date
SELECTION-SCREEN : END OF BLOCK b2.

**********Events**********
START-OF-SELECTION.
  PERFORM : process.
  IF gt_alv IS NOT INITIAL.
    PERFORM : alv_display.
  ELSE.
    MESSAGE : 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

***********set the default values*******
INITIALIZATION.
  gv_bukrs = 'DMS1'.

END-OF-SELECTION.

FORM process.

***************fetch only dms distributor***************
  SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = @gv_bukrs.
  IF sy-subrc = 0.
    SELECT kunnr,name1,werks FROM kna1 INTO TABLE @DATA(lt_dist) FOR ALL ENTRIES IN @lt_t001k
                                                                 WHERE kunnr IN @so_kunnr
                                                                 AND   werks EQ @lt_t001k-bwkey.
  ENDIF.
  IF sy-subrc = 0.
**********sort the distributor details************
    SORT : lt_dist BY werks.
****************fetch the cleared items**************
    SELECT a~kunnr,a~belnr,a~buzei,a~budat,a~bldat,a~gjahr,a~bukrs,
           a~xblnr,a~gsber,a~blart,a~wrbtr,a~sgtxt,b~racct,c~name1
       FROM bsad AS a
       INNER JOIN faglflexa AS b
       ON  a~gjahr = b~ryear
       AND a~belnr = b~docnr
       INNER JOIN kna1 AS c
       ON  a~kunnr = c~kunnr
       APPENDING TABLE @DATA(lt_acnt)
       FOR ALL ENTRIES IN @lt_dist
       WHERE a~bukrs EQ @gv_bukrs
       AND a~kunnr IN @so_retai
       AND a~umskz <> 'H'
       AND a~budat IN @so_pdate
       AND a~blart IN ( 'DG' , 'DR' , 'DA' )
       AND b~rldnr  = '0L'
       AND b~rbukrs = @gv_bukrs
       AND b~docln  = '000002'
       AND b~rbusa  = @lt_dist-werks.
****************fetch the open items**************
    SELECT a~kunnr,a~belnr,a~buzei,a~budat,a~bldat,a~gjahr,a~bukrs,"a~stblg AS rev_doc,a~stjah AS rev_yr,
           a~xblnr,a~gsber,a~blart,a~wrbtr,a~sgtxt,b~racct,c~name1
       FROM bsid AS a
       INNER JOIN faglflexa AS b
       ON  a~gjahr = b~ryear
       AND a~belnr = b~docnr
       INNER JOIN kna1 AS c
       ON  a~kunnr = c~kunnr
       APPENDING TABLE @lt_acnt
       FOR ALL ENTRIES IN @lt_dist
       WHERE a~bukrs EQ @gv_bukrs
       AND a~kunnr IN @so_retai
       AND a~umskz <> 'H'
       AND a~budat IN @so_pdate
       AND a~blart IN ( 'DG' , 'DR' , 'DA' )
       AND b~rldnr  = '0L'
       AND b~rbukrs = @gv_bukrs
       AND b~docln  = '000002'
       AND b~rbusa  = @lt_dist-werks.
  ENDIF.
***********moving to final alv display table***********
  IF lt_acnt IS NOT INITIAL.
**********************Excess payment removal process*************
    SELECT * FROM zdms_dz_removal INTO TABLE @DATA(lt_delete).
    IF sy-subrc = 0.
      SORT lt_delete BY belnr gjahr.
    ENDIF.
****************fetch the GL ACNT details************
    SELECT saknr,ktopl,txt50 FROM skat INTO TABLE @DATA(lt_skat) WHERE ktopl = 'SDMS'.
    IF sy-subrc = 0.
      SORT lt_skat BY saknr.
    ENDIF.

    SORT : lt_acnt BY bukrs belnr gjahr.

    LOOP AT lt_acnt ASSIGNING FIELD-SYMBOL(<fs_acnt>).

***********************Excess payment removal process*************
*      IF lt_delete IS NOT INITIAL AND <fs_acnt>-blart <> 'DA'.
*        READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = <fs_acnt>-belnr
*                                                             gjahr = <fs_acnt>-gjahr BINARY SEARCH.
*        IF sy-subrc = 0.
*          CONTINUE.
*        ENDIF.
*      ENDIF.
******************document type check**********
      CLEAR : lv_type.
      IF <fs_acnt>-racct = '0042002013' OR <fs_acnt>-racct = '0042002012'.
        lv_type = 'AUTO CD'.
      ELSE.
        lv_type = 'MANUAL'.
      ENDIF.

***************only for DA document ***************
      IF <fs_acnt>-blart = 'DA'.
        SELECT SINGLE belnr,gjahr,bukrs,stblg,stjah FROM bkpf
                                                    INTO @DATA(ls_bkpf)
                                                    WHERE bukrs = @<fs_acnt>-bukrs
                                                    AND   belnr = @<fs_acnt>-belnr
                                                    AND   gjahr = @<fs_acnt>-gjahr.
        IF sy-subrc = 0.
          READ TABLE lt_acnt INTO DATA(ls_acnt) WITH KEY bukrs = ls_bkpf-bukrs
                                                         belnr = ls_bkpf-stblg
                                                         gjahr = ls_bkpf-stjah BINARY SEARCH.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        CLEAR : ls_bkpf.

      ENDIF.

**************only for negative value DA & DR***************
      IF <fs_acnt>-blart = 'DA' OR <fs_acnt>-blart = 'DR'.
        <fs_acnt>-wrbtr = <fs_acnt>-wrbtr * -1.
      ENDIF.

*      "GL Account existence checks
      READ TABLE lt_skat INTO DATA(ls_skat) WITH KEY saknr = <fs_acnt>-racct BINARY SEARCH.
****************get the distributor details**************
      READ TABLE lt_dist INTO DATA(ls_dist) WITH KEY werks = <fs_acnt>-gsber BINARY SEARCH.

      APPEND VALUE #( distributor = ls_dist-kunnr
                      dist_name   = ls_dist-name1
                      plant       = <fs_acnt>-gsber
                      ret_code    = <fs_acnt>-kunnr
                      reta_name   = <fs_acnt>-name1
                      blart       = <fs_acnt>-blart
                      gl_account  = <fs_acnt>-racct
                      glacc_txt   = ls_skat-txt50
                      type        = lv_type
                      fisyear     = <fs_acnt>-gjahr
                      docno       = <fs_acnt>-belnr
                      docdate     = <fs_acnt>-bldat
                      pdate       = <fs_acnt>-bldat
                      refdoc      = <fs_acnt>-xblnr
                      amount      = <fs_acnt>-wrbtr
                      item_txt    = <fs_acnt>-sgtxt
                      rev_doc     = ls_acnt-belnr
                      rev_yr      = ls_acnt-gjahr
                      rev_type    = ls_acnt-blart
                      rev_date    = ls_acnt-budat ) TO gt_alv.

      CLEAR : ls_dist,ls_skat,ls_acnt.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Include          ZDMS_OPENING_BALANCE_POST_CLS
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION. " Variables for events
  PUBLIC SECTION.
    METHODS : on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
*****double click on the doc no & call the FB03************
  METHOD on_double_click.

    IF column = 'DOCNO'.
      READ TABLE gt_alv INTO DATA(ls_alv) INDEX row.
      IF ls_alv-docno IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_alv-docno.
        SET PARAMETER ID 'BUK' FIELD gv_bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_alv-fisyear.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        CLEAR : ls_alv.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

FORM alv_display.
  IF gt_alv IS NOT INITIAL.

    SORT gt_alv BY distributor ret_code pdate.

    DATA: lo_gr_functions  TYPE REF TO cl_salv_functions_list.
    DATA: lo_event_handler TYPE REF TO lcl_handle_events, " Variables for events
          lo_events        TYPE REF TO cl_salv_events_table.
    DATA: lo_display       TYPE REF TO cl_salv_display_settings. " Variable for layout settings
    DATA: lo_columns TYPE REF TO cl_salv_columns,
          lo_column  TYPE REF TO cl_salv_column_table.
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.
* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).
* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).
    TRY.
        lo_column ?= lo_columns->get_column( 'TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Type' ).
        lo_column->set_medium_text( 'Type' ).
        lo_column->set_short_text( 'Type' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Register events
    lo_events = lo_gr_alv->get_event( ).
    CREATE OBJECT lo_event_handler.
    SET HANDLER lo_event_handler->on_double_click FOR lo_events.
***********Call the alv screen************
    lo_gr_alv->display( ).

  ENDIF.
ENDFORM.
