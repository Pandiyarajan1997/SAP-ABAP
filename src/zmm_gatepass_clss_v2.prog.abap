*&--------------------------------------------------------------------------------------------*
*&    Title       :
*&    Author      : Puratchiveeran
*&    Created On  : 29-FEB-2023

*&--------------------------------------------------------------------------------------------*
*&----------------------------------    Change History   -------------------------------------*
*&--------------------------------------------------------------------------------------------*
*&    S.No  | Changed By        |   Changed on    | Request(s)
*&      1   | xxxxxxxxxxxxxxxxx |   XX-XX-XXXX    |
*&    Reason      :
*&--------------------------------------------------------------------------------------------*

REPORT zmm_gatepass_clss_v2.


TYPE-POOLS:ftis.
TABLES sscrfields.
TABLES : zmm_ge_asn, nriv,  t001w.

DATA: gt_output    TYPE STANDARD TABLE OF zmm_st_asn,
      gt_asn_split TYPE STANDARD TABLE OF zmm_st_asn,
      g_nasnqty(5) TYPE c,
      gs_output    TYPE zmm_st_asn,
      gv_inv_err   TYPE c,
      gv_confirm   TYPE c,
      gv_newasn    TYPE c.
TYPES:
  BEGIN OF ty_migo,
    ebeln      TYPE ekpo-ebeln,
    ebelp      TYPE ekpo-ebelp,
    bedat      TYPE ekko-bedat,
    matnr      TYPE ekpo-matnr,
    txz01      TYPE ekpo-txz01,
    werks      TYPE ekpo-werks,
    lgort      TYPE lips-lgort,
    charg      TYPE lips-charg,
    po_qty     TYPE eket-menge,
    del_qty    TYPE eket-wemng,
    open_qty   TYPE eket-menge,
    migo_qty   TYPE eket-wemng,
    gravity    TYPE eket-wemng,
    temprature TYPE text10,
    meins      TYPE ekpo-meins,
    asn_qty    TYPE eket-wemng,
    sku_qty    TYPE eket-wemng,
    remark     TYPE sgtxt,
    netpr      TYPE ekpo-netpr,
    total      TYPE ekpo-netpr,
    flag       TYPE flag,
    Date_of_manufacture type ekko-aedat,
  END OF ty_migo.

DATA: "gt_out_1   TYPE STANDARD TABLE OF zmm_st_ge_asn,
      "gw_out_1   TYPE zmm_st_ge_asn,
      "wa_data    TYPE ty_migo,
      "gt_out_2   TYPE STANDARD TABLE OF ty_migo,
      "gt_migo    TYPE STANDARD TABLE OF zmm_st_ge_asn,
      "gw_migo    TYPE zmm_st_ge_asn,
      gv_scr_101 TYPE flag,
      gv_scr_100 TYPE flag.
DATA:
  p_werks   TYPE werks_d,
  p_frbnr   TYPE frbnr,
  p_d_note  TYPE lfsnr1,
  p_asnno   TYPE zasnno,
  p_htext   TYPE bktxt,
  p_test    TYPE flag VALUE 'X',
  p_intime  TYPE sy-uzeit,
  p_outtime TYPE sy-uzeit,
  gv_gjahr  TYPE gjahr.

INCLUDE zmm_ge_auto_migo_prg_cls.

INCLUDE zmm_gatepass_pforms_v2.

INITIALIZATION.

  CALL FUNCTION 'FTI_FISCAL_YEAR_MONTH_GET'
    EXPORTING
      i_bukrs = '1000'
      i_budat = sy-datum
    IMPORTING
      e_gjahr = gv_gjahr.

  CREATE OBJECT go_main.

  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1100 OUTPUT.
  SET PF-STATUS 'ZNEW_1100'.
  SET TITLEBAR 'INWARD_GATE_PASS'.
  IF zmm_ge_asn-gdate IS INITIAL.
    zmm_ge_asn-werks = p_werks .
    zmm_ge_asn-gdate = sy-datum .
    zmm_ge_asn-gtime = sy-uzeit .
    zmm_ge_asn-usnam = sy-uname .
  ENDIF.
  IF p_d_note IS INITIAL.
    p_d_note = zmm_ge_asn-asnno(16).
  ENDIF.
  p_asnno = zmm_ge_asn-asnno.
*  LOOP AT SCREEN.
*    IF screen-name = 'P_D_NOTE' OR
*       screen-name = 'ZMM_GE_ASN-ASNNO'.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
  IF zmm_ge_asn-lifnr IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'ZMM_GE_ASN-LIFNR'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF gv_scr_101 = abap_true.
    LOOP AT SCREEN.
      IF screen-name = 'CMD_ASN'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.
ENDMODULE.                 " STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1100 INPUT.
  DATA : gv_flag(1) TYPE c .
  CLEAR : gv_flag .
  DATA : lv_lifnr_p(10) TYPE n,
         lv_ebeln_p(10) TYPE n.
  DATA : wa_city TYPE  lfa1 .
  DATA : lv_lifn(10) TYPE c .
  DATA : lv_found(10) TYPE c .

  CLEAR: lv_lifn, lv_found.
  lv_lifn = zmm_ge_asn-lifnr .

  CASE sy-ucomm.

    WHEN 'ASN'.

      PERFORM f_get_po_detail_from_mis_api.

      IF gt_output IS INITIAL.
        SELECT * FROM tvarvc
                 INTO TABLE @DATA(lt_tvarvc)
                 WHERE name = 'ZASN_ALLOWED_VENDOR' AND
                       type = 'S'.
        SHIFT lv_lifn LEFT DELETING LEADING '0'.
        lv_found = VALUE #( lt_tvarvc[ low = lv_lifn ]-low OPTIONAL  ).
        IF lv_found IS INITIAL.
          DATA(l_vendtxt) = |No ASN is found for Vendor Code - { zmm_ge_asn-lifnr }|.
          MESSAGE l_vendtxt TYPE 'S' DISPLAY LIKE 'E'.
          EXIT .
        ENDIF.
      ENDIF.

      CLEAR: go_main->gv_editable.

      IF gv_scr_101 IS INITIAL.
        CLEAR gv_newasn.
      ENDIF.
      IF gt_output IS INITIAL.
        gv_newasn = abap_true.
      ENDIF.

      LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fg_out>).
        <fg_out>-mark = ''.
      ENDLOOP.
      CALL SCREEN 0101
                       STARTING AT 10 5
                       ENDING AT 150 20.
*      PERFORM f_display_popup_alv.
*
*      CALL FUNCTION 'Z_POPUP_ALV'
*        EXPORTING
*          i_popup = 'X'
*        TABLES
*          it_alv  = gt_output.
      PERFORM f_get_open_op.
    WHEN 'OPEN_PO'.
      PERFORM f_get_open_op.
    WHEN 'BACK'.
      go_main->flush( ).
      PERFORM gatepass_back.
    WHEN 'EXIT'.
      go_main->flush( ).
      PERFORM gatepass_exit.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.


  DATA : it_cre TYPE TABLE OF zgeplantv,
         wa_cre TYPE zgeplantv.

  CLEAR zmm_ge_asn.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'DISP'.
      CLEAR: zmm_ge_asn,
        gt_output[],
        p_frbnr  ,
        p_d_note ,
        p_asnno ,
        p_htext  ,
        p_intime ,
        p_outtime.
      gv_scr_100 = abap_true.
      CLEAR: gv_confirm,  gv_scr_101.
      CLEAR gt_asn_split[].
*      p_werks = t001w-werks.

      CALL SCREEN 1100.
    WHEN 'STA' .

      AUTHORITY-CHECK OBJECT 'ZGATPASS'
               ID 'WERKS' FIELD p_werks.

      IF sy-subrc = 0.
        AUTHORITY-CHECK OBJECT 'ZGATPASS'
                   ID 'ACTVT' FIELD '16'.
        IF sy-subrc = 0.
          SET PARAMETER ID 'SBJ' FIELD p_werks.
          CALL TRANSACTION 'ZASN_MIGO_STATUS' .
        ELSE.
          MESSAGE 'Authorization Required' TYPE 'I'.

        ENDIF.
      ELSE.
        MESSAGE 'Authorization Required' TYPE 'I'.

      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZNEW_1100F'.
  SET TITLEBAR 'ZGPS1'.
*  PERFORM image_on_screen.

ENDMODULE.                                                  "STATUS_010

*
**&---------------------------------------------------------------------*
**&      Module  EXIT  INPUT200
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE exit INPUT.
*  CASE sy-ucomm .
*
*    WHEN 'EXIT' .
**    LEAVE PROGRAM .
*      LEAVE TO TRANSACTION 'ZGE_NEW' .
*  ENDCASE .
*ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  LIFNR_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lifnr_validation INPUT.
  PERFORM validate_lifnr.

ENDMODULE.                 " LIFNR_VALIDATION  INPUT
MODULE asnno_validation INPUT.
  PERFORM validate_asnno.

ENDMODULE.
MODULE validate_vbeln INPUT.
  PERFORM validate_vbeln.

ENDMODULE." VBELN_VALIDATION  INPUT

*&---------------------------------------------------------------------*
*& Module PREPARE_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM prepare_screen .

  go_main->fetch_invoice_data( ).

  go_main->fetch_purchase_data( ).

  go_main->build_alv( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  DATA l_asn_err TYPE c.
  CLEAR l_asn_err.
  CASE sy-ucomm.
    WHEN 'CMDSAVE'.

      DELETE gt_output WHERE cmaterialno = space.
      DATA(lv_asnno) = VALUE #( gt_output[ mark = 'X' ]-asnno OPTIONAL ).
      READ TABLE gt_output INTO gs_output WITH KEY asnno = lv_asnno
                                                   maktx = ''.
      IF sy-subrc = 0.
        DATA(l_txt) = |{ gs_output-cmaterialno } Material is not found|.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_asn_err = abap_true.
      ENDIF.
      READ TABLE gt_output INTO gs_output WITH KEY asnno = lv_asnno
                                                   nasnqty = ''.
      IF sy-subrc = 0.
        l_txt = |Remove the Material { gs_output-cmaterialno } - Quantity not found|.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_asn_err = abap_true.
      ENDIF.
      READ TABLE gt_output INTO gs_output WITH KEY asnno = lv_asnno
                                                   cbatchno = ''.
      IF sy-subrc = 0.
        l_txt = |Remove the Material { gs_output-cmaterialno } - Vendor Batch not found|.
        MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
        l_asn_err = abap_true.
      ENDIF.

*      LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<wa>)
*                        GROUP BY ( cbatchno = <wa>-cbatchno  count = GROUP SIZE )
*                        ASCENDING  ASSIGNING FIELD-SYMBOL(<group_key>).
*        IF <group_key>-count GT 1.
*          l_txt = |Vendor Batch { <group_key>-cbatchno } is repeated more than once|.
*          MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
*          l_asn_err = abap_true.
*        ENDIF.
*      ENDLOOP.
      CLEAR lv_asnno.
      IF gv_newasn IS NOT INITIAL.
        IF l_asn_err IS INITIAL.
          LEAVE TO SCREEN 0.
        ENDIF.
      ELSE.
        IF l_asn_err IS INITIAL.
          READ TABLE gt_output INTO gs_output WITH KEY mark = abap_true.
          IF sy-subrc = 0.
            zmm_ge_asn-asnno = gs_output-asnno.
            LOOP AT gt_output INTO gs_output WHERE asnno = zmm_ge_asn-asnno AND
                                                   maktx IS INITIAL.
              l_asn_err = abap_true.
              DATA(l_mattext) = |{ gs_output-cmaterialno } Material is not found|.
              MESSAGE l_mattext TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
            ENDLOOP.
            IF l_asn_err IS INITIAL.
              LEAVE TO SCREEN 0.
              gv_confirm = abap_true.
            ENDIF.
          ELSE.
            MESSAGE 'Please select ASN No before Confirm' TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'CMDEXIT' OR
         'EXIT'.
      IF gv_confirm = abap_true.
        DELETE gt_output WHERE asnno = zmm_ge_asn-asnno AND
                               maktx IS INITIAL.
      ELSE.
        IF gv_newasn = abap_true.
          CLEAR: zmm_ge_asn-asnno,
                 p_asnno,
                 gt_output[].
        ENDIF.
      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  DATA l_qty TYPE zasnqty.
  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      LOOP AT gt_asn_split ASSIGNING FIELD-SYMBOL(<fs_wdata>).
        l_qty = l_qty +  <fs_wdata>-nasnqty.
      ENDLOOP.
      IF g_nasnqty EQ l_qty.

        DELETE go_main->gt_out_1 WHERE mark = 'X'.
        DELETE gt_asn_split WHERE nasnqty = 0.
        APPEND LINES OF gt_asn_split TO go_main->gt_out_1.
        CLEAR l_qty.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Total quantity and entered quantity is not matched' TYPE 'E' DISPLAY LIKE 'I'.
        CLEAR l_qty.
      ENDIF.
    WHEN 'BACK' OR
         'EXIT'.
*      IF gv_confirm = abap_true.
*        DELETE gt_output WHERE asnno = zmm_ge_asn-asnno AND
*                               maktx IS INITIAL.
*      ELSE.
*        IF gv_newasn = abap_true.
*          CLEAR: zmm_ge_asn-asnno,
*                 p_asnno,
*                 gt_output[].
*        ENDIF.
*      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0102 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0102 OUTPUT.

  SET PF-STATUS 'ZNEW_0101'.
  SET TITLEBAR 'ASN Material Details'.

  LOOP AT gt_asn_split ASSIGNING FIELD-SYMBOL(<fs1>).
    <fs1>-cmaterialno = CONV matnr18( |{ <fs1>-cmaterialno ALPHA = IN }| ).
    SELECT SINGLE maktx FROM makt
      INTO <fs1>-maktx
      WHERE matnr = <fs1>-cmaterialno AND
            spras = sy-langu.
    IF sy-subrc <> 0.
      CLEAR <fs1>-maktx.
    ENDIF.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0101 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  SET PF-STATUS 'ZNEW_0101'.
  SET TITLEBAR 'ASN Material Details'.
  gv_scr_101 = abap_true.

***********Fetch Current Number
  DATA l_snrono(10) TYPE n.
  IF zmm_ge_asn-asnno IS INITIAL AND
     gv_newasn IS NOT INITIAL.
    SELECT SINGLE * FROM nriv  WHERE object    = 'ZASN_SNRO'
                                 AND subobject = p_werks
                                 AND nrrangenr = '01'
                                 AND toyear  = gv_gjahr.
    IF sy-subrc = 0.
*      IF nriv-nrlevel IS INITIAL.
*        l_snrono  = nriv-fromnumber.
*      ELSE.
*        l_snrono = nriv-nrlevel + 1.
*      ENDIF.
      CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
        EXPORTING
          object           = 'ZASN_SNRO'   "Create with SNUM
        EXCEPTIONS
          foreign_lock     = 1
          object_not_found = 2
          system_failure   = 3
          OTHERS           = 4.
      IF sy-subrc NE 0.
        MESSAGE  'Lock error: NUMBER_RANGE_ENQUEUE(ZASN_SNRO)' TYPE 'S' DISPLAY LIKE 'W'.
      ELSE.
        DATA wdocno(10).
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01' "(or pass the Variable)
            object      = 'ZASN_SNRO'
            subobject   = p_werks
            toyear      = gv_gjahr
          IMPORTING
            number      = wdocno.
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object = 'ZASN_SNRO'.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          zmm_ge_asn-asnno = |{ gv_gjahr+2(2) }{ p_werks }{ wdocno }|.
*          CLEAR gs_output.
*          gs_output-asnno = zmm_ge_asn-asnno.
*          INSERT gs_output INTO gt_output INDEX 1.
        ENDIF.
      ENDIF.
      CLEAR nriv.
    ELSE.
      DATA(l_text) = |Please Maintain Number Range ZASN_SNRO for Plant-{ p_werks } and FiscalYr- { gv_gjahr }|.
      MESSAGE l_text TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.
  ENDIF.
  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs>).
    <fs>-cmaterialno = CONV matnr18( |{ <fs>-cmaterialno ALPHA = IN }| ).
    SELECT SINGLE maktx FROM makt
      INTO <fs>-maktx
      WHERE matnr = <fs>-cmaterialno AND
            spras = sy-langu.
    IF sy-subrc <> 0.
      CLEAR <fs>-maktx.
    ENDIF.
  ENDLOOP.

*  go_main->display_popup_alv( ).


ENDMODULE.

" DECLARATION OF TABLECONTROL 'TC_ASN' ITSELF
CONTROLS: tc_asn TYPE TABLEVIEW USING SCREEN 0101.

" LINES OF TABLECONTROL 'TC_ASN'
DATA:     g_tc_asn_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

" OUTPUT MODULE FOR TC 'TC_ASN'. DO NOT CHANGE THIS LINE!
" UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_asn_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_output LINES tc_asn-lines.
ENDMODULE.

" OUTPUT MODULE FOR TC 'TC_ASN'. DO NOT CHANGE THIS LINE!
" GET LINES OF TABLECONTROL
MODULE tc_asn_get_lines OUTPUT.
  g_tc_asn_lines = sy-loopc.
ENDMODULE.

" INPUT MODULE FOR TC 'TC_ASN'. DO NOT CHANGE THIS LINE!
" MODIFY TABLE
MODULE tc_asn_modify INPUT.
  IF gv_newasn IS NOT INITIAL AND
     gs_output-asnno IS INITIAL.
    gs_output-asnno = zmm_ge_asn-asnno.
  ENDIF.
  MODIFY gt_output FROM gs_output INDEX tc_asn-current_line.
  IF sy-subrc <> 0.
    INSERT gs_output INTO gt_output INDEX tc_asn-current_line.
  ENDIF.
ENDMODULE.

" INPUT MODULE FOR TC 'TC_ASN'. DO NOT CHANGE THIS LINE!
" PROCESS USER COMMAND
MODULE tc_asn_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_ASN'
                              'GT_OUTPUT'
                              ' '
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

  " BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
  " END OF LOCAL DATA------------------------------------------*

  " Table control specific operations                          *
  " evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
  " execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row

      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      CHECK p_tc_name	= 'TC_ASN'.
      READ TABLE gt_output INTO gs_output WITH KEY mark = abap_true.
      IF sy-subrc = 0.
        PERFORM fcode_delete_row USING    p_tc_name
                                          p_table_name
                                          p_mark_name.
        CLEAR p_ok.
      ELSE.
        MESSAGE 'Please select the Material' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
*    WHEN 'MARK'.                      "mark all filled lines
*      PERFORM fcode_tc_mark_lines USING p_tc_name
*                                        p_table_name
*                                        p_mark_name   .
*      CLEAR p_ok.

*    WHEN 'DMRK'.                      "demark all filled lines
*      PERFORM fcode_tc_demark_lines USING p_tc_name
*                                          p_table_name
*                                          p_mark_name .
*      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.
  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

  " BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
  " END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

  " get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

  " get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

  " get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
    " set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
  " set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

  " insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
  " set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

  " BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
  " END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

  " get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

  " delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

    " access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT 'MARK' OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
  " BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
  " END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
  " get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


  " is no line filled?                                         *
  IF <tc>-lines = 0.
    " yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
    " no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

  " get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
      " et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

  " set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
  " EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
  " END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

  " get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

  " mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

    " access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
  " BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
  " END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

  " get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

  " demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

    " access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  GET_MAKTX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_maktx INPUT.

  gs_output-cmaterialno = CONV matnr18( |{ gs_output-cmaterialno ALPHA = IN }| ).
  SELECT SINGLE maktx FROM makt
    INTO gs_output-maktx
    WHERE matnr = gs_output-cmaterialno AND
          spras = sy-langu.
  IF sy-subrc <> 0.
    CLEAR gs_output-maktx.
*    l_txt = |{ gs_output-cmaterialno } Material is not found|.
*    MESSAGE l_txt TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDMODULE.
MODULE check_batchno.
  DATA l_count TYPE i.
  LOOP AT gt_asn_split TRANSPORTING NO FIELDS WHERE  cbatchno = gs_output-cbatchno..
    l_count = l_count + 1.
  ENDLOOP.
  IF l_count GT 1.
    CLEAR l_count.
    MESSAGE 'Duplicate Batch No is found'  TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
  CLEAR l_count.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  IF gv_newasn IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'GS_OUTPUT-CMATERIALNO' OR
         screen-name EQ 'GS_OUTPUT-NASNQTY' OR
         screen-name EQ 'GS_OUTPUT-CBATCHNO'.        .
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_SPLIT_BATCH' ITSELF
CONTROLS: tc_split_batch TYPE TABLEVIEW USING SCREEN 0102.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_SPLIT_BATCH'
DATA:     g_tc_split_batch_lines  LIKE sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_SPLIT_BATCH'. DO NOT CHANGE THIS LI
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_split_batch_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_output LINES tc_split_batch-lines.
ENDMODULE.
MODULE tc_split_change_tc_attr_102 OUTPUT.
  DESCRIBE TABLE gt_asn_split LINES tc_split_batch-lines.
ENDMODULE.


*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_SPLIT_BATCH'. DO NOT CHANGE THIS LI
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_split_batch_get_lines OUTPUT.
  g_tc_split_batch_lines = sy-loopc.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form f_get_open_op
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_open_op .

  CLEAR gv_inv_err.
  PERFORM validate_vbeln.
  IF gv_inv_err IS INITIAL.
    PERFORM prepare_screen.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  TC_SPLIT_BATCH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_split_batch INPUT.

*  IF gv_newasn IS NOT INITIAL AND
*     gs_output-asnno IS INITIAL.
*    gs_output-asnno = zmm_ge_asn-asnno.
*  ENDIF.
  MODIFY gt_asn_split FROM gs_output INDEX tc_split_batch-current_line.
  IF sy-subrc <> 0.
    INSERT gs_output INTO gt_asn_split INDEX tc_split_batch-current_line.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  COPY_DATA  INPUT
*&---------------------------------------------------------------------*
MODULE copy_data INPUT.

  CASE sy-ucomm.
    WHEN 'COPY'.
*      sort gt_asn_split by flag ascending.
      DESCRIBE TABLE gt_asn_split LINES DATA(l_lines).
      DATA(wa_split) =  gt_asn_split[ l_lines ].
      CLEAR: wa_split-cbatchno,wa_split-nasnqty.
      APPEND wa_split TO gt_asn_split.
      CLEAR sy-ucomm.
    WHEN 'DELE'.
*      READ TABLE gt_asn_split INTO wa_split INDEX tc_split_batch-current_line.
      DELETE gt_asn_split WHERE mark = 'X'.
      CLEAR sy-ucomm.
  ENDCASE.
ENDMODULE.                 " COPY_DATA  INPUT
