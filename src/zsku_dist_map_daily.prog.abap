*&---------------------------------------------------------------------*
*& Report ZSKU_DIST_MAP_DAILY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsku_dist_map_daily.

TABLES: kna1 ,knvv.

TYPES: BEGIN OF lty_cdhdr,           "Structure for Change Document
         objectclas TYPE cdobjectcl,
         objectid   TYPE cdobjectv,
         changenr   TYPE cdchangenr,
         username   TYPE cdusername,
         udate      TYPE cddatum,
         utime      TYPE cduzeit,
         tcode      TYPE cdtcode,
       END OF lty_cdhdr.

TYPES: BEGIN OF lty_knvv,
         kunnr TYPE kunnr,
         vkorg TYPE vkorg,
         kdgrp TYPE kdgrp,
       END OF lty_knvv.

TYPES: BEGIN OF lty_knvp,  "For Split Table key from CDPOS
         mandt TYPE mandt,
         kunnr TYPE kunnr,
         vkorg TYPE vkorg,
         vtweg TYPE vtweg,
         spart TYPE spart,
         parvw TYPE parvw,
         parza TYPE parza,
       END OF lty_knvp.

TYPES: BEGIN OF lty_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
       END OF lty_kna1.
TYPES: BEGIN OF lty_disp,     "Alv Display Structure
         kunnr        TYPE kunnr,
         name1        TYPE name1_gp,
         primary      TYPE kunn2,
         primary_name TYPE name1_gp,
         secondary1   TYPE kunn2,
         sec_name1    TYPE name1_gp,
         secondary2   TYPE kunn2,
         sec_name2    TYPE name1_gp,
         message(100) TYPE c,
       END OF lty_disp.
****** Internal Table Declaration ****************
DATA: lt_cdhdr TYPE STANDARD TABLE OF lty_cdhdr,
      ls_cdhdr TYPE lty_cdhdr.

DATA: lt_cdpos TYPE STANDARD TABLE OF cdpos,
      ls_cdpos TYPE cdpos.

DATA: lt_knvv TYPE STANDARD TABLE OF lty_knvv,
      ls_knvv TYPE lty_knvv.

DATA: lt_knvp TYPE STANDARD TABLE OF lty_knvp,
      ls_knvp TYPE lty_knvp.

DATA: lt_knvp1 TYPE STANDARD TABLE OF knvp,
      ls_knvp1 TYPE knvp,
      ls_knvp2 TYPE knvp.

DATA: lt_kna1 TYPE TABLE OF lty_kna1,
      ls_kna1 TYPE lty_kna1.

DATA: ls_tvarvc TYPE tvarvc.
DATA: lv_key TYPE string.

DATA: lt_sku_map    TYPE STANDARD TABLE OF zsku_table,
      ls_sku_map    TYPE zsku_table,
      ls_sku_map_mn TYPE zsku_table.

DATA: lt_disp TYPE TABLE OF lty_disp,
      ls_disp TYPE lty_disp.


DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.


DATA: lv_msg TYPE char100.

*********Selection Screen parameters *************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_kunnr FOR kna1-kunnr,
                  s_vkorg FOR knvv-vkorg.
  PARAMETERS: p_date TYPE datum.

SELECTION-SCREEN END OF BLOCK b1.
****************************************************************

INITIALIZATION.

  IF p_date IS INITIAL.

    CLEAR ls_tvarvc.
    SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'ZSKU_DIST_MAP'
                                               AND type = 'P'.
    p_date = ls_tvarvc-low.

  ENDIF.


START-OF-SELECTION.

** Select all change document created in CDHDR **
  REFRESH lt_cdhdr.

  SELECT objectclas
         objectid
         changenr
         username
         udate
         utime
         tcode FROM cdhdr
               INTO TABLE lt_cdhdr
               WHERE objectclas EQ 'DEBI'
               AND ( udate BETWEEN p_date AND sy-datum )
               AND tcode EQ 'XD02'.

  IF sy-subrc EQ 0 AND lt_cdhdr[] IS NOT INITIAL.
    SORT lt_cdhdr[] BY objectid.

    "Getting all change document values from CDPOS"
    REFRESH lt_cdpos.

    SELECT * FROM cdpos
             INTO TABLE lt_cdpos
             FOR ALL ENTRIES IN lt_cdhdr
             WHERE objectclas EQ lt_cdhdr-objectclas
             AND objectid EQ lt_cdhdr-objectid
             AND changenr EQ lt_cdhdr-changenr
             AND tabname EQ 'KNVP'.
    IF sy-subrc EQ 0.
      SORT lt_cdpos[] BY objectid.
    ENDIF.
  ENDIF.

*Data fetching from distributor Mapping Table*
  SELECT * FROM zsku_table
           INTO TABLE lt_sku_map.
  IF sy-subrc EQ 0.
    SORT lt_sku_map[] BY customer_no.
  ENDIF.

** select knvp data based on change document *
  SELECT * FROM knvp
           INTO TABLE lt_knvp1
           WHERE parvw IN ( 'SK' , 'S2' , 'S3' ).
  IF sy-subrc EQ 0.
    SORT lt_knvp1[] BY kunnr.
  ENDIF.

** Selecting cust number and cust name from KNA1**
  REFRESH lt_kna1.
  SELECT kunnr
         name1 FROM kna1
                     INTO TABLE lt_kna1.


  CLEAR ls_cdpos.
  LOOP AT lt_cdpos INTO ls_cdpos.

    CLEAR ls_knvp.
    MOVE ls_cdpos-tabkey TO ls_knvp.

    IF ls_knvp-parvw EQ 'SK'.
      APPEND ls_knvp TO lt_knvp.
    ELSEIF ls_knvp-parvw EQ 'S2'.
      APPEND ls_knvp TO lt_knvp.
    ELSEIF ls_knvp-parvw EQ 'S3'.
      APPEND ls_knvp TO lt_knvp.
    ELSE.
      CLEAR ls_knvp.
    ENDIF.
  ENDLOOP.

  IF lt_knvp[] IS NOT INITIAL.
    SORT lt_knvp[] BY kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_knvp COMPARING kunnr.
  ENDIF.

* Select all Customers from KNVV when
  SELECT kunnr
         vkorg
         kdgrp FROM knvv
               INTO TABLE lt_knvv
               FOR ALL ENTRIES IN lt_knvp
               WHERE kunnr EQ lt_knvp-kunnr
               AND vkorg IN s_vkorg
               AND kdgrp = '10'.
  IF sy-subrc EQ 0.
    SORT lt_knvv[] BY kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_knvv COMPARING kunnr.
  ENDIF.

  CLEAR ls_knvp.
  LOOP AT lt_knvp INTO ls_knvp.

    CLEAR ls_knvv.
    READ TABLE lt_knvv INTO ls_knvv WITH KEY kunnr = ls_knvp-kunnr.  " Main Dealer checks
    IF sy-subrc EQ 0.

      CLEAR ls_knvp1.
      READ TABLE lt_knvp1 INTO ls_knvp1 WITH KEY kunnr = ls_knvp-kunnr
                                                 parvw = 'SK'.
      IF sy-subrc EQ 0.
        CLEAR ls_sku_map.
        ls_sku_map-mandt = sy-mandt.
        ls_sku_map-customer_no = ls_knvp-kunnr.

        "Distributor1  and "Distributor Nme1
        ls_sku_map-distributor_1 = ls_knvp1-kunn2.

        IF ls_sku_map-distributor_1 IS NOT INITIAL.
          CLEAR ls_kna1.
          READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-distributor_1.
          IF sy-subrc EQ 0.
            ls_sku_map-distributor_nme1 = ls_kna1-name1.
          ENDIF.

          " Distributor2 AND Distributor Nme2
          CLEAR ls_knvp2.
          READ TABLE lt_knvp1 INTO ls_knvp2 WITH KEY kunnr = ls_knvp1-kunn2
                                                     parvw = 'S2'.
          IF sy-subrc = 0.
            ls_sku_map-distributor_2 = ls_knvp2-kunn2.

            CLEAR ls_kna1.
            READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-distributor_2.
            IF sy-subrc EQ 0.
              ls_sku_map-distributor_nme2 = ls_kna1-name1.
            ENDIF.
          ENDIF.
        ENDIF.

        " Distributor3 AND Distributor Nme3
        CLEAR ls_knvp2.
        READ TABLE lt_knvp1 INTO ls_knvp2 WITH KEY kunnr = ls_knvp1-kunn2
                                                   parvw = 'S3'.
        IF sy-subrc = 0.
          ls_sku_map-distributor_3 = ls_knvp2-kunn2.

          CLEAR ls_kna1.
          READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-distributor_3.
          IF sy-subrc EQ 0.
            ls_sku_map-distributor_nme3 = ls_kna1-name1.
          ENDIF.
        ENDIF.

        " Checking Mapping Table
        READ TABLE lt_sku_map INTO ls_sku_map_mn WITH KEY customer_no = ls_sku_map-customer_no.
        IF sy-subrc = 0.
          IF ls_sku_map_mn = ls_sku_map.

            " Same Data available in mapping and knvp table
            CLEAR ls_disp.
            ls_disp-kunnr = ls_sku_map_mn-customer_no.
            CLEAR ls_kna1.

            READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map_mn-customer_no.
            IF sy-subrc EQ 0.
              ls_disp-name1 = ls_kna1-name1.
            ENDIF.

            ls_disp-primary = ls_sku_map_mn-distributor_1.
            ls_disp-primary_name = ls_sku_map_mn-distributor_nme1.
            ls_disp-secondary1 = ls_sku_map_mn-distributor_2.
            ls_disp-sec_name1 = ls_sku_map_mn-distributor_nme2.
            ls_disp-secondary2 = ls_sku_map_mn-distributor_3.
            ls_disp-sec_name2 = ls_sku_map_mn-distributor_nme3.
            ls_disp-message = 'Data is already Same'.
            APPEND ls_disp TO lt_disp.
            CONTINUE.
          ELSE.

            "Different Data means updating mapping table
            ls_sku_map-create_on = ls_sku_map_mn-create_on.
            ls_sku_map-created_by = ls_sku_map_mn-created_by.
            ls_sku_map-changed_on = sy-datum.
            ls_sku_map-changed_by = sy-uname.
            ls_sku_map-status = 'Updated'.
            MODIFY zsku_table FROM ls_sku_map.
            COMMIT WORK AND WAIT.

            CLEAR ls_disp.
            ls_disp-kunnr = ls_sku_map-customer_no.
            CLEAR ls_kna1.

            READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-customer_no.
            IF sy-subrc EQ 0.
              ls_disp-name1 = ls_kna1-name1.
            ENDIF.

            ls_disp-primary = ls_sku_map-distributor_1.
            ls_disp-primary_name = ls_sku_map-distributor_nme1.
            ls_disp-secondary1 = ls_sku_map-distributor_2.
            ls_disp-sec_name1 = ls_sku_map-distributor_nme2.
            ls_disp-secondary2 = ls_sku_map-distributor_3.
            ls_disp-sec_name2 = ls_sku_map-distributor_nme3.
            ls_disp-message = 'Data is Updated in Table'.
            APPEND ls_disp TO lt_disp.
            CONTINUE.
          ENDIF.
        ELSE.
          ls_sku_map-created_by = sy-uname.
          ls_sku_map-create_on = sy-datum.
          ls_sku_map-changed_on = sy-datum.
          ls_sku_map-changed_by = sy-uname.
          ls_sku_map-status = 'New'.
          MODIFY zsku_table FROM ls_sku_map.
          COMMIT WORK AND WAIT.

          CLEAR ls_disp.
          ls_disp-kunnr = ls_sku_map-customer_no.
          CLEAR ls_kna1.

          READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-customer_no.
          IF sy-subrc EQ 0.
            ls_disp-name1 = ls_kna1-name1.
          ENDIF.

          ls_disp-primary = ls_sku_map-distributor_1.
          ls_disp-primary_name = ls_sku_map-distributor_nme1.
          ls_disp-secondary1 = ls_sku_map-distributor_2.
          ls_disp-sec_name1 = ls_sku_map-distributor_nme2.
          ls_disp-secondary2 = ls_sku_map-distributor_3.
          ls_disp-sec_name2 = ls_sku_map-distributor_nme3.
          ls_disp-message = 'Data is Created in Table'.
          APPEND ls_disp TO lt_disp.
        ENDIF.
      ELSE.
        CLEAR ls_disp.
        ls_disp-kunnr = ls_knvp-kunnr.
        CLEAR lv_msg.
        CONCATENATE 'For' ls_knvp-kunnr 'No SK and S2 Mapping Available' INTO lv_msg SEPARATED BY space.
        ls_disp-message = lv_msg.
        APPEND ls_disp TO lt_disp.
      ENDIF.

    ELSE. "Apart from SUB Dealer.

      CLEAR ls_knvp1.
      LOOP AT lt_knvp1 INTO ls_knvp1 WHERE kunn2 = ls_knvp-kunnr
                                           AND parvw = 'SK'.
        IF sy-subrc EQ 0.
          CLEAR ls_sku_map.
          ls_sku_map-mandt = sy-mandt.
          ls_sku_map-customer_no = ls_knvp1-kunnr.

          "Distributor1  and "Distributor Nme1
          ls_sku_map-distributor_1 = ls_knvp1-kunn2.

          IF ls_sku_map-distributor_1 IS NOT INITIAL.
            CLEAR ls_kna1.
            READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-distributor_1.
            IF sy-subrc EQ 0.
              ls_sku_map-distributor_nme1 = ls_kna1-name1.
            ENDIF.

            " Distributor2 AND Distributor Nme2
            CLEAR ls_knvp2.
            READ TABLE lt_knvp1 INTO ls_knvp2 WITH KEY kunnr = ls_knvp1-kunn2
                                                       parvw = 'S2'.
            IF sy-subrc = 0.
              ls_sku_map-distributor_2 = ls_knvp2-kunn2.

              CLEAR ls_kna1.
              READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-distributor_2.
              IF sy-subrc EQ 0.
                ls_sku_map-distributor_nme2 = ls_kna1-name1.
              ENDIF.
            ENDIF.
          ENDIF.

          " Distributor3 AND Distributor Nme3
          CLEAR ls_knvp2.
          READ TABLE lt_knvp1 INTO ls_knvp2 WITH KEY kunnr = ls_knvp1-kunn2
                                                     parvw = 'S3'.
          IF sy-subrc = 0.
            ls_sku_map-distributor_3 = ls_knvp2-kunn2.

            CLEAR ls_kna1.
            READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-distributor_3.
            IF sy-subrc EQ 0.
              ls_sku_map-distributor_nme3 = ls_kna1-name1.
            ENDIF.
          ENDIF.

          " Checking Mapping Table
          READ TABLE lt_sku_map INTO ls_sku_map_mn WITH KEY customer_no = ls_sku_map-customer_no.
          IF sy-subrc = 0.
            IF ls_sku_map_mn = ls_sku_map.

              " Same Data available in mapping and knvp table
              CLEAR ls_disp.
              ls_disp-kunnr = ls_sku_map_mn-customer_no.
              CLEAR ls_kna1.

              READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map_mn-customer_no.
              IF sy-subrc EQ 0.
                ls_disp-name1 = ls_kna1-name1.
              ENDIF.

              ls_disp-primary = ls_sku_map_mn-distributor_1.
              ls_disp-primary_name = ls_sku_map_mn-distributor_nme1.
              ls_disp-secondary1 = ls_sku_map_mn-distributor_2.
              ls_disp-sec_name1 = ls_sku_map_mn-distributor_nme2.
              ls_disp-secondary2 = ls_sku_map_mn-distributor_3.
              ls_disp-sec_name2 = ls_sku_map_mn-distributor_nme3.
              ls_disp-message = 'Data is already Same'.
              APPEND ls_disp TO lt_disp.
              CONTINUE.
            ELSE.

              "Different Data means updating mapping table
              ls_sku_map-create_on = ls_sku_map_mn-create_on.
              ls_sku_map-created_by = ls_sku_map_mn-created_by.
              ls_sku_map-changed_on = sy-datum.
              ls_sku_map-changed_by = sy-uname.
              ls_sku_map-status = 'Updated'.
              MODIFY zsku_table FROM ls_sku_map.
              COMMIT WORK AND WAIT.

              CLEAR ls_disp.
              ls_disp-kunnr = ls_sku_map-customer_no.
              CLEAR ls_kna1.

              READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-customer_no.
              IF sy-subrc EQ 0.
                ls_disp-name1 = ls_kna1-name1.
              ENDIF.

              ls_disp-primary = ls_sku_map-distributor_1.
              ls_disp-primary_name = ls_sku_map-distributor_nme1.
              ls_disp-secondary1 = ls_sku_map-distributor_2.
              ls_disp-sec_name1 = ls_sku_map-distributor_nme2.
              ls_disp-secondary2 = ls_sku_map-distributor_3.
              ls_disp-sec_name2 = ls_sku_map-distributor_nme3.
              ls_disp-message = 'Data is Updated in Table'.
              APPEND ls_disp TO lt_disp.
              CONTINUE.
            ENDIF.
          ELSE.
            ls_sku_map-created_by = sy-uname.
            ls_sku_map-create_on = sy-datum.
            ls_sku_map-changed_on = sy-datum.
            ls_sku_map-changed_by = sy-uname.
            ls_sku_map-status = 'New'.
            MODIFY zsku_table FROM ls_sku_map.
            COMMIT WORK AND WAIT.

            CLEAR ls_disp.
            ls_disp-kunnr = ls_sku_map-customer_no.
            CLEAR ls_kna1.

            READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_sku_map-customer_no.
            IF sy-subrc EQ 0.
              ls_disp-name1 = ls_kna1-name1.
            ENDIF.

            ls_disp-primary = ls_sku_map-distributor_1.
            ls_disp-primary_name = ls_sku_map-distributor_nme1.
            ls_disp-secondary1 = ls_sku_map-distributor_2.
            ls_disp-sec_name1 = ls_sku_map-distributor_nme2.
            ls_disp-secondary2 = ls_sku_map-distributor_3.
            ls_disp-sec_name2 = ls_sku_map-distributor_nme3.
            ls_disp-message = 'Data is Created in Table'.
            APPEND ls_disp TO lt_disp.
          ENDIF.
        ELSE.
          CLEAR ls_disp.
          ls_disp-kunnr = ls_knvp-kunnr.
          CLEAR lv_msg.
          CONCATENATE 'For' ls_knvp-kunnr 'No SK and S2 Mapping Available' INTO lv_msg SEPARATED BY space.
          ls_disp-message = lv_msg.
          APPEND ls_disp TO lt_disp.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  REFRESH gt_fcat.
  PERFORM f_fieldcat USING 'KUNNR'        'Customer_no'      1 space.
  PERFORM f_fieldcat USING 'NAME1'        'Customer_Name'    2 space.
  PERFORM f_fieldcat USING 'PRIMARY'      'Distributor_1'    3 space.
  PERFORM f_fieldcat USING 'PRIMARY_NAME' 'Distributor_nme1' 4 space.
  PERFORM f_fieldcat USING 'SECONDARY1'   'Distributor_2'    5 space.
  PERFORM f_fieldcat USING 'SEC_NAME1'    'Distributor_nme2' 6 space.
  PERFORM f_fieldcat USING 'SECONDARY2'   'Distributor_3'    7 space.
  PERFORM f_fieldcat USING 'SEC_NAME2'    'Distributor_nme3' 8 space.
  PERFORM f_fieldcat USING 'MESSAGE'      'Message'          9 space.


  gs_layout-colwidth_optimize = 'X'.

***********ALV DISPLAY  *******************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
    TABLES
      t_outtab           = lt_disp.


FORM f_fieldcat USING f_var1 f_var2 f_var3 f_var4.
  CLEAR gs_fcat.
  gs_fcat-fieldname = f_var1.
  gs_fcat-seltext_m = f_var2.
  gs_fcat-col_pos = f_var3.
  gs_fcat-edit = f_var4.
  APPEND gs_fcat TO gt_fcat.
ENDFORM.
