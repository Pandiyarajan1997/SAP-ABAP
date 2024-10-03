*&---------------------------------------------------------------------*
*& Report ZMM_MATERIAL_DETAILS_PO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_material_details_po.

TYPES: BEGIN OF ty_marc,       " Structure For MARC
         matnr TYPE matnr,
         werks TYPE werks_d,
         ekgrp TYPE ekgrp,
         dismm TYPE dismm,
         dispo TYPE dispo,
         disls TYPE disls,
         beskz TYPE beskz,
         sobsl TYPE sobsl,
         eisbe TYPE eisbe,
         kautb TYPE kautb,
         kordb TYPE kordb,
         disgr TYPE disgr,
         trame TYPE trame,
       END OF ty_marc.

TYPES: BEGIN OF ty_mard,   "Structure For MARD Table
         matnr TYPE matnr,
         werks TYPE werks_d,
         labst TYPE labst,
         lfgja TYPE lfgja,
         lfmon TYPE lfmon,
       END OF ty_mard.

TYPES: BEGIN OF ty_eord, "structure for Eord Table
         matnr TYPE matnr,
         werks TYPE werks_d,
         vdatu TYPE ordab,
         bdatu TYPE ordbi,
         lifnr TYPE elifn,
       END OF ty_eord.

TYPES: BEGIN OF ty_a017,       "Structure For Info Record valid Date
         kschl TYPE kschl,
         lifnr TYPE elifn,
         matnr TYPE matnr,
         ekorg TYPE ekorg,
         werks TYPE werks_d,
         datab TYPE kodatab,
         datbi TYPE kodatbi,
         knumh TYPE knumh,
       END OF ty_a017.

TYPES:BEGIN OF ty_konp,       "Structure For Pricing Condition
        knumh TYPE knumh,
        kopos TYPE kopos,
        kbetr TYPE kbetr_kond,
        kschl TYPE kschl,
      END OF ty_konp.

TYPES: BEGIN OF ty_eina,     "structure For Info Record General Data
         infnr TYPE infnr,
         matnr TYPE matnr,
         lifnr TYPE elifn,
       END OF ty_eina.

TYPES: BEGIN OF ty_mara,      "Structure for Mara table
         matnr TYPE matnr,
         mtart TYPE mtart,
       END OF ty_mara.

TYPES: BEGIN OF ty_lfa1,    " Structure for LFA1 table
         lifnr TYPE elifn,
         name1 TYPE name1_gp,
       END OF ty_lfa1.

TYPES: BEGIN OF ty_display,          " Structure For ALV Display
         matnr      TYPE matnr,
         maktx      TYPE maktx,
         mtart      TYPE mtart,
         werks      TYPE werks_d,
         ekgrp      TYPE ekgrp,
         ekorg      TYPE ekorg,
         lifnr      TYPE elifn,
         name1      TYPE name1_gp,
         eisbe      TYPE eisbe,
         labst      TYPE labst,
         trame      TYPE trame,
         vdatu      TYPE ordab,
         bdatu      TYPE ordbi,
         infnr      TYPE infnr,
         kschl      TYPE kschl,
         dismm      TYPE dismm,
         dispo      TYPE dispo,
         disls      TYPE disls,
         beskz      TYPE beskz,
         sobsl      TYPE sobsl,
         kautb      TYPE kautb,
         kordb      TYPE kordb,
         disgr      TYPE disgr,
         knumh      TYPE knumh,
         kbter      TYPE kbetr_kond,
         freight    TYPE kbetr_kond,
         added_cost TYPE kbetr_kond,
         datab      TYPE kodatab,
         datbi      TYPE kodatbi,
       END OF ty_display.


***********Internal Table and Work Area Declaration******************
DATA: gt_marc  TYPE TABLE OF ty_marc,
      gs_marc  TYPE ty_marc,
      gt_mara  TYPE TABLE OF ty_mara,
      gs_mara  TYPE ty_mara,
      gt_mard  TYPE TABLE OF ty_mard,
      gt_mard1 TYPE TABLE OF ty_mard,
      gt_mard2 TYPE TABLE OF ty_mard,
      gs_mard  TYPE ty_mard,
      gs_mard1 TYPE ty_mard,
      gs_mard2 TYPE ty_mard,
      gt_eord  TYPE TABLE OF ty_eord,
      gs_eord  TYPE ty_eord,
      gt_a017  TYPE TABLE OF ty_a017,
      gs_a017  TYPE ty_a017,
      gt_konp  TYPE TABLE OF ty_konp,
      gs_konp  TYPE ty_konp,
      gt_eina  TYPE TABLE OF ty_eina,
      gs_eina  TYPE ty_eina,
      gt_lfa1  TYPE TABLE OF ty_lfa1,
      gs_lfa1  TYPE ty_lfa1.

DATA: gt_makt TYPE STANDARD TABLE OF makt,
      gs_makt TYPE makt.

***************Internal Table for ALV Display ***************************
DATA: gt_display TYPE TABLE OF ty_display,
      gs_display TYPE ty_display.

DATA: lv_unrestrict TYPE labst.
DATA: lv_add_cost TYPE kbetr.

***********Selection Screen Design****************************************
DATA: gv_werks TYPE marc-werks.
DATA: gv_matnr TYPE marc-matnr.

DATA :gt_fcat    TYPE lvc_t_fcat,
      gs_fcat    TYPE lvc_s_fcat,
      gt_exclude TYPE ui_functions,
      gs_excl    TYPE ui_func,
      gs_layout  TYPE lvc_s_layo.

DATA: lo_grid  TYPE REF TO cl_gui_alv_grid.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_werks FOR gv_werks,
                  s_matnr FOR gv_matnr.
  PARAMETERS: p_from TYPE a017-datab,
              p_to   TYPE a017-datbi.
SELECTION-SCREEN END OF BLOCK b1.

***********Actual Data fetching Starts ********************************
START-OF-SELECTION.

  REFRESH: gt_marc,gt_mard,gt_mard1,gt_mard2,gt_eord,gt_a017,gt_konp,gt_eina,gt_mara,gt_makt,gt_lfa1,gt_display.

  CLEAR: gs_marc,gs_mard,gs_eord,gs_a017,gs_konp,gs_eina,gs_display,gs_mard1,gs_mard2,gs_mara,gs_makt,gs_lfa1.

************ Data Retrieval **************************************
  PERFORM data_retrieval.

END-OF-SELECTION.

  PERFORM alv_display.

FORM data_retrieval.
***********Fetching Latest Pricing Condition  ****************
  SELECT kschl
         lifnr
         matnr
         ekorg
         werks
         datab
         datbi
         knumh FROM a017
               INTO TABLE gt_a017
               WHERE matnr IN s_matnr
               AND werks IN s_werks
               AND ( datab LE p_to )
               AND ( datbi GE p_from ).
  IF sy-subrc EQ 0.
    SORT gt_a017[] BY kschl matnr werks lifnr datab DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_a017 COMPARING kschl matnr werks lifnr.
  ENDIF.
********Fetching All material based on plant input  **********************
  IF gt_a017[] IS NOT INITIAL.
    SELECT matnr
           werks
           ekgrp
           dismm
           dispo
           disls
           beskz
           sobsl
           eisbe
           kautb
           kordb
           disgr
           trame FROM marc
                 INTO TABLE gt_marc
                 FOR ALL ENTRIES IN gt_a017
                 WHERE matnr EQ gt_a017-matnr
                 AND werks EQ gt_a017-werks.
    IF sy-subrc EQ 0.
      SORT gt_marc[] BY matnr werks.
      DELETE ADJACENT DUPLICATES FROM gt_marc COMPARING matnr werks.
    ENDIF.

***********Based on MARC fetching MARD for stock ***************
    SELECT matnr
           werks
           labst
           lfgja
           lfmon FROM mard
                 INTO TABLE gt_mard
                 FOR ALL ENTRIES IN gt_a017
                 WHERE matnr EQ gt_a017-matnr
                 AND werks EQ gt_a017-werks.
    IF sy-subrc EQ 0.
      SORT gt_mard[] BY matnr werks.
    ENDIF.

    IF gt_mard[] IS NOT INITIAL.
      REFRESH: gt_mard1.
      gt_mard1[] = gt_mard[].
      SORT gt_mard1[] BY matnr werks.

      CLEAR gs_mard1.
      LOOP AT gt_mard1 INTO gs_mard1.
        CLEAR: lv_unrestrict, gs_mard.
        LOOP AT gt_mard INTO gs_mard WHERE matnr = gs_mard1-matnr AND
                                              werks = gs_mard1-werks.
          lv_unrestrict = lv_unrestrict + gs_mard-labst.
        ENDLOOP.
        CLEAR gs_mard1-labst.
        gs_mard1-labst = lv_unrestrict.
        APPEND gs_mard1 TO gt_mard2.
      ENDLOOP.

***********  Final added stock internal table  ***********************************
      IF gt_mard2[] IS NOT INITIAL.
        SORT gt_mard2[] BY matnr werks.
        DELETE ADJACENT DUPLICATES FROM gt_mard2 COMPARING matnr werks.
      ENDIF.
    ENDIF.

*********** Fetching material Valid from and valid to from EORD table**************
    SELECT matnr
           werks
           vdatu
           bdatu
           lifnr FROM eord
                 INTO TABLE gt_eord
                 FOR ALL ENTRIES IN gt_a017
                 WHERE matnr EQ gt_a017-matnr
                 AND werks EQ gt_a017-werks.
    IF sy-subrc EQ 0.
      SORT gt_eord[] BY matnr werks.
    ENDIF.
****************Fetching Info Record Number Based on material and Supplier  ****************
    SELECT infnr
           matnr
           lifnr FROM eina
                 INTO TABLE gt_eina
                 FOR ALL ENTRIES IN gt_a017
                 WHERE matnr EQ gt_a017-matnr
                 AND lifnr EQ gt_a017-lifnr.
    IF sy-subrc EQ 0.
      SORT gt_eina[] BY matnr.
    ENDIF.

*************Fetching Price from KONP Table   ***************************************************
    SELECT knumh
           kopos
           kbetr
           kschl FROM konp
                 INTO TABLE gt_konp
                 FOR ALL ENTRIES IN gt_a017
                 WHERE knumh EQ gt_a017-knumh.
    IF sy-subrc EQ 0.
      SORT gt_konp[] BY kschl.
    ENDIF.

************* Fetching Material type from mara **************************
    SELECT matnr
           mtart FROM mara
                 INTO TABLE gt_mara
                 FOR ALL ENTRIES IN gt_a017
                 WHERE matnr EQ gt_a017-matnr.
    IF sy-subrc EQ 0.
      SORT gt_mara[] BY matnr.
    ENDIF.
************** Fetching material description ftom MAKT table **************
    SELECT * FROM makt
             INTO TABLE gt_makt
             FOR ALL ENTRIES IN gt_a017
             WHERE matnr EQ gt_a017-matnr
             AND spras EQ sy-langu.
    IF sy-subrc EQ 0.
      SORT gt_makt[] BY matnr.
    ENDIF.

************ Fetching Vendor Name from LFA1 *****************************
    SELECT lifnr
           name1 FROM lfa1
                 INTO TABLE gt_lfa1
                 FOR ALL ENTRIES IN gt_a017
                 WHERE lifnr EQ gt_a017-lifnr.
    IF sy-subrc EQ 0.
      SORT gt_lfa1[] BY lifnr.
    ENDIF.
  ENDIF.

************* Final output Preparation ************************************
  LOOP AT gt_a017 INTO gs_a017.

    CLEAR gs_display.
    gs_display-matnr = gs_a017-matnr.
    gs_display-knumh = gs_a017-knumh.
    gs_display-werks = gs_a017-werks.
    gs_display-lifnr = gs_a017-lifnr.
    gs_display-ekorg = gs_a017-ekorg.
    gs_display-datab = gs_a017-datab.
    gs_display-datbi = gs_a017-datbi.

    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_a017-lifnr.
    IF sy-subrc EQ 0.

      gs_display-name1 =  gs_lfa1-name1.

    ENDIF.

    READ TABLE gt_marc INTO gs_marc WITH KEY matnr = gs_a017-matnr
                                             werks = gs_a017-werks.
    IF sy-subrc EQ 0.

      gs_display-ekgrp = gs_marc-ekgrp.
      gs_display-dismm = gs_marc-dismm.
      gs_display-dispo = gs_marc-dispo.
      gs_display-disls = gs_marc-disls.
      gs_display-beskz = gs_marc-beskz.
      gs_display-sobsl = gs_marc-sobsl.
      gs_display-eisbe = gs_marc-eisbe.
      gs_display-kautb = gs_marc-kautb.
      gs_display-kordb = gs_marc-kordb.
      gs_display-disgr = gs_marc-disgr.
      gs_display-trame = gs_marc-trame.

    ENDIF.

    READ TABLE gt_mard2 INTO gs_mard2 WITH KEY matnr = gs_a017-matnr
                                               werks = gs_a017-werks.
    IF sy-subrc EQ 0.

      gs_display-labst = gs_mard2-labst.

    ENDIF.

    READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_a017-matnr.
    IF sy-subrc EQ 0.

      gs_display-mtart = gs_mara-mtart.

    ENDIF.

    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_a017-matnr.
    IF sy-subrc EQ 0.

      gs_display-maktx =  gs_makt-maktx.

    ENDIF.

    READ TABLE gt_eord INTO gs_eord WITH KEY matnr = gs_a017-matnr
                                             werks = gs_a017-werks.
    IF sy-subrc EQ 0.

      gs_display-vdatu = gs_eord-vdatu.
      gs_display-bdatu = gs_eord-bdatu.

    ENDIF.

    READ TABLE gt_eina INTO gs_eina WITH KEY matnr = gs_a017-matnr.
    IF sy-subrc EQ 0.

      gs_display-infnr = gs_eina-infnr.

    ENDIF.
************ Price Calculation for final ALV table    ***********************
    READ TABLE gt_konp INTO gs_konp WITH KEY knumh = gs_a017-knumh.
    IF sy-subrc EQ 0 AND gs_konp-kschl EQ 'P000'.
      gs_display-kschl = gs_konp-kschl.
      gs_display-kbter = gs_konp-kbetr.
    ELSEIF gs_konp-kschl EQ 'FRB1'.
      gs_display-freight = gs_konp-kbetr.
    ENDIF.

    CLEAR lv_add_cost.
    lv_add_cost = gs_display-kbter + gs_display-freight.

    gs_display-added_cost = lv_add_cost.

    APPEND gs_display TO gt_display.

  ENDLOOP.

ENDFORM.
********** ALV Display *****************
FORM alv_display.

  REFRESH gt_fcat.

  PERFORM f_fieldcat USING 'MATNR'        'Material No'        1 space.
  PERFORM f_fieldcat USING 'MAKTX'        'Material Desc'      2 space.
  PERFORM f_fieldcat USING 'MTART'        'Material Type'      3 space.
  PERFORM f_fieldcat USING 'WERKS'        'Plant'              4 space.
  PERFORM f_fieldcat USING 'EKORG'        'Purchase Org'       5 space.
  PERFORM f_fieldcat USING 'EKGRP'        'Purchase Grp'       6 space.
  PERFORM f_fieldcat USING 'LIFNR'        'Supplier'           7 space.
  PERFORM f_fieldcat USING 'NAME1'        'Supplier_Name'      8 space.
  PERFORM f_fieldcat USING 'EISBE'        'Safety Stock'       9 space.
  PERFORM f_fieldcat USING 'LABST'        'Unrestricted stk'  10 space.
  PERFORM f_fieldcat USING 'TRAME'        'Transit Stk'       11 space.
  PERFORM f_fieldcat USING 'VDATU'        'Valid_from'        12 space.
  PERFORM f_fieldcat USING 'BDATU'        'Valid_to'          13 space.
  PERFORM f_fieldcat USING 'INFNR'        'Info record No'    14 space.
  PERFORM f_fieldcat USING 'KSCHL'        'Condition type'    15 space.
  PERFORM f_fieldcat USING 'KNUMH'        'Condition Rec No'  16 space.
  PERFORM f_fieldcat USING 'KBTER'        'Amount in Rec'     17 space.
  PERFORM f_fieldcat USING 'FRIEGHT'      'Freight_cost'      18 space.
  PERFORM f_fieldcat USING 'ADDED_COST'   'Added_cost'        19 space.
  PERFORM f_fieldcat USING 'DATAB'        'Valid_From'        20 space.
  PERFORM f_fieldcat USING 'DATBI'        'Valid_to'          21 space.
  PERFORM f_fieldcat USING 'DISMM'        'MRP Type'          22 space.
  PERFORM f_fieldcat USING 'DISPO'        'MRP Controller'    23 space.
  PERFORM f_fieldcat USING 'DISGR'        'MRP Group'        24 space.
  PERFORM f_fieldcat USING 'DISLS'        'LotSiz Pro for MP' 25 space.
  PERFORM f_fieldcat USING 'BESKZ'        'Procurement Type'  26 space.
  PERFORM f_fieldcat USING 'SOBSL'        'Spl procure type'  27 space.
  PERFORM f_fieldcat USING 'KAUTB'        'Indicator AUTO PO' 28 space.
  PERFORM f_fieldcat USING 'KORDB'        'Indicator:Slist'   29 space.

  CALL SCREEN 100.

ENDFORM.

*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZALV'.
  SET TITLEBAR 'ZTITLE'.
  IF lo_grid IS INITIAL .

    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = cl_gui_container=>default_screen.

    gs_layout-cwidth_opt = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = gt_exclude
      CHANGING
        it_outtab            = gt_display
        it_fieldcatalog      = gt_fcat.

  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.

*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4.
  CLEAR gs_fcat.
  gs_fcat-fieldname = f_var1.
  gs_fcat-coltext = f_var2.
  gs_fcat-col_pos = f_var3.
  gs_fcat-edit = f_var4.
  APPEND gs_fcat TO gt_fcat.
ENDFORM.
