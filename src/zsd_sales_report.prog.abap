*&---------------------------------------------------------------------*
*& Report ZSD_SALES_REPORT
*&---------------------------------------------------------------------*
*& Date Created - 29.03.2022
*& Created By   - KPABAP (Shamsudeen)
*& Description  - Program to set the sales potential for a customer
*  TR No        - DEVK931523, DEVK931565
*&---------------------------------------------------------------------*
* Change Date   - 21.04.2022
* TR NO         - DEVK931614
* Changed By    - Ramkrishnan (701322)
* Reference     - Discussion with Malathi
* Description   - Add KATR6 (Custom Region/Zone and Customer no to
*                 selection screen and fetch data based on it
*&----------------------------------------------------------------------
REPORT zsd_sales_report.

TABLES kna1.

TYPES: BEGIN OF lty_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
         regio TYPE regio,
         land1 TYPE land1_gp,
         katr6 TYPE katr6,
       END OF lty_kna1,

       BEGIN OF lty_vbrk,
         vbeln TYPE vbeln_vf,
         fkart TYPE fkart,
         kunag TYPE kunnr,
         fksto TYPE fksto,
         fkdat TYPE fkdat,
       END OF lty_vbrk,

       BEGIN OF lty_vbrp,
         vbeln TYPE vbeln_vf,
         volum TYPE volum_15,
         matnr TYPE matnr,
       END OF lty_vbrp,

       BEGIN OF lty_mara,
         matnr TYPE matnr,
         matkl TYPE matkl,
         zeinr TYPE dzeinr,
       END OF lty_mara,

       BEGIN OF lty_disp,
         kunag   TYPE kunnr,
         name1   TYPE name1_gp,
         matkl   TYPE matkl,
         wgbez60 TYPE wgbez60,
         volum   TYPE volum_15,
         volum1  TYPE volum_15,
         regio   TYPE regio,
         katr6   TYPE katr6,
         zeinr   TYPE dzeinr,
       END OF lty_disp,

       BEGIN OF lty_fields,
         fieldname TYPE fieldname,
         rollname  TYPE rollname,
         position  TYPE tabfdpos,
         datatype  TYPE datatype_d,
         leng      TYPE ddleng,
       END OF lty_fields.

DATA: lt_t023t   TYPE STANDARD TABLE OF t023t,
      ls_t023t   TYPE t023t,
      lt_disp    TYPE STANDARD TABLE OF lty_disp,
      lt_disp1   TYPE STANDARD TABLE OF lty_disp,
      lt_disp2   TYPE STANDARD TABLE OF lty_disp,
      lt_disp3   TYPE STANDARD TABLE OF lty_disp,
      ls_disp    TYPE lty_disp,
      ls_disp1   TYPE lty_disp,
      lt_kna1    TYPE STANDARD TABLE OF lty_kna1,
      ls_kna1    TYPE lty_kna1,
      lt_vbrk    TYPE STANDARD TABLE OF lty_vbrk,
      ls_vbrk    TYPE lty_vbrk,
      lt_vbrp    TYPE STANDARD TABLE OF lty_vbrp,
      ls_vbrp    TYPE lty_vbrp,
      lt_mara    TYPE STANDARD TABLE OF lty_mara,
      ls_mara    TYPE lty_mara,
      lt_t005u    TYPE STANDARD TABLE OF t005u,
      ls_t005u    TYPE t005u,
      lt_tvk6t    TYPE STANDARD TABLE OF tvk6t,
      ls_tvk6t    TYPE tvk6t,
      lt_fields  TYPE STANDARD TABLE OF lty_fields,
      ls_fields  TYPE lty_fields,
      lt_fcat    TYPE lvc_t_fcat,
      ls_fcat    TYPE lvc_s_fcat,
      lt_exclude TYPE ui_functions,
      ls_excl    TYPE ui_func,
      ls_layout  TYPE lvc_s_layo.

DATA: lv_container TYPE scrfname VALUE 'ALV_GRID_CONT',
      lv_date      TYPE datum,
      lv_line      TYPE i.

CLASS lcl_handle_events DEFINITION DEFERRED.

DATA: gv_tabix LIKE sy-tabix.

DATA: lo_grid      TYPE REF TO cl_gui_alv_grid,
      lo_events    TYPE REF TO lcl_handle_events,
      lo_container TYPE REF TO cl_gui_custom_container.


*---------------------------------------------------------------------
* Select Options
*---------------------------------------------------------------------
SELECT-OPTIONS: s_kunnr FOR kna1-kunnr ,  "Customer No
                s_regio FOR  kna1-regio,  "Customer Region
                s_adlrg FOR kna1-katr6.   "Additonal Customer region

*---------------------------------------------------------------------
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------
* Define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      data_changed  FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD data_changed.
    DATA: ls_row   TYPE lvc_s_modi,
          lv_index TYPE i.
    CLEAR:ls_disp, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'VOLUM1'.
    IF sy-subrc = 0 AND ls_row-value IS NOT INITIAL.
      READ  TABLE lt_disp3 INTO ls_disp INDEX ls_row-row_id.
      ls_disp-volum1 = ls_row-value.
      MODIFY lt_disp3 FROM ls_disp INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  REFRESH: lt_t023t,lt_disp,lt_kna1,lt_vbrp,lt_vbrk,lt_mara.

  SELECT * FROM t023t INTO TABLE lt_t023t WHERE spras = sy-langu.

*get region from attribute 6 help table from KNA1 for field katr6
  SELECT * FROM tvk6t INTO TABLE lt_tvk6t WHERE spras = sy-langu.

  SELECT kunnr
         name1
         regio
         land1
         katr6
    FROM kna1
    INTO TABLE lt_kna1
   WHERE kunnr IN s_kunnr
     AND regio IN s_regio
     AND loevm NE 'X'
     AND katr6 IN s_adlrg.

  IF lt_kna1[] IS NOT INITIAL.

  SORT lt_kna1 BY kunnr.
  SELECT * FROM t005u INTO TABLE lt_t005u FOR ALL ENTRIES IN lt_kna1
                                           WHERE spras EQ sy-langu AND
                                                 land1 EQ lt_kna1-land1.





    CLEAR lv_date.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = 0
        months    = 36
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_date.

    SORT lt_kna1 BY kunnr.
    SELECT  vbeln  fkart kunag fksto fkdat FROM vbrk INTO TABLE lt_vbrk
                            FOR ALL ENTRIES IN lt_kna1
                                     WHERE kunag = lt_kna1-kunnr AND
                                           fkart = 'YBBR' AND
                                           fksto NE 'X' AND
                                           fkdat BETWEEN lv_date AND sy-datum.

    IF lt_vbrk[] IS NOT   INITIAL.
      SORT lt_vbrk BY vbeln.
      SELECT vbeln volum matnr FROM vbrp INTO TABLE lt_vbrp
                               FOR ALL ENTRIES IN lt_vbrk
                                       WHERE vbeln = lt_vbrk-vbeln.
      IF lt_vbrp[] IS NOT INITIAL.
        SORT lt_vbrp BY matnr.
        SELECT matnr matkl zeinr FROM mara INTO TABLE lt_mara
                           FOR ALL ENTRIES IN lt_vbrp
                                           WHERE matnr = lt_vbrp-matnr.
      ENDIF.



      SORT lt_kna1 BY kunnr.
      SORT lt_vbrp BY vbeln.

      LOOP AT lt_vbrk INTO ls_vbrk.
        CLEAR ls_disp.
        ls_disp-kunag = ls_vbrk-kunag.


* Getting the Name and Region
        CLEAR ls_kna1.
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_vbrk-kunag BINARY SEARCH.
        IF sy-subrc = 0.
          ls_disp-name1 = ls_kna1-name1.
          ls_disp-regio = ls_kna1-regio.
          ls_disp-katr6 = ls_kna1-KATR6.
        ENDIF.


        CLEAR ls_vbrp.
        READ TABLE lt_vbrp WITH KEY vbeln = ls_vbrk-vbeln BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          CLEAR gv_tabix.
          gv_tabix = sy-tabix.
        ENDIF.

        LOOP AT lt_vbrp INTO ls_vbrp FROM gv_tabix.

          IF ls_vbrk-vbeln NE ls_vbrp-vbeln.
            EXIT.
          ENDIF.

*          ls_disp-volum  =   ls_vbrp-volum  .

          CLEAR ls_mara.
          READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_vbrp-matnr BINARY SEARCH.
          IF sy-subrc = 0.

            READ TABLE lt_disp WITH KEY kunag = ls_disp matkl = ls_mara-matkl TRANSPORTING NO FIELDS.
            IF sy-subrc NE 0.
              ls_disp-matkl = ls_mara-matkl.
              ls_disp-zeinr = ls_mara-zeinr.

              CLEAR ls_t023t.
              READ TABLE lt_t023t INTO ls_t023t WITH KEY matkl = ls_disp-matkl.
              IF sy-subrc = 0.
                ls_disp-wgbez60 = ls_t023t-wgbez60.
              ENDIF.
            ENDIF.



          ENDIF.


          .


          APPEND ls_disp TO lt_disp.

        ENDLOOP.




      ENDLOOP.
    ENDIF.
  ENDIF.

  REFRESH: lt_disp1[],lt_disp2[],lt_disp3[].
  lt_disp1[] = lt_disp[].

  SORT lt_disp1 BY kunag matkl.
  DELETE ADJACENT DUPLICATES FROM lt_disp1 COMPARING kunag matkl.

  LOOP AT lt_disp1 INTO ls_disp.
    REFRESH lt_disp2[].
    lt_disp2[] = lt_disp[].
    SORT lt_disp2 BY kunag matkl.
    DELETE lt_disp2[] WHERE kunag NE ls_disp-kunag.
    DELETE lt_disp2[] WHERE matkl NE ls_disp-matkl.
    SORT lt_disp2 DESCENDING BY volum.
    CLEAR lv_line.
    DESCRIBE TABLE lt_disp2 LINES lv_line.
    IF lv_line = 1.
      CLEAR ls_disp1.
      READ TABLE lt_disp2 INTO ls_disp INDEX 1.
    ELSE.
      READ TABLE lt_disp2 INTO ls_disp INDEX 2.
    ENDIF.
    APPEND ls_disp TO lt_disp3.
  ENDLOOP.

END-OF-SELECTION.

  REFRESH lt_fcat.

  PERFORM f_fieldcat USING  'KUNAG' 'Customer No' 1 space.
  PERFORM f_fieldcat USING  'NAME1' 'Customer Nme' 2 space.
  PERFORM f_fieldcat USING  'MATKL' 'Material Group' 3 space.
  PERFORM f_fieldcat USING  'WGBEZ60' 'Material Group Description' 4 space.
  PERFORM f_fieldcat USING  'VOLUM' 'Potential' 5 space.
  PERFORM f_fieldcat USING  'VOLUM1' 'Comitment' 6 'X'.
  PERFORM f_fieldcat USING 'REGIO' 'region' 7 space.
  PERFORM f_fieldcat USING 'ZEINR' 'Document No' 8 space.

  CALL SCREEN 100.

FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 .
  ls_fcat-fieldname = f_var1.
  ls_fcat-coltext = f_var2.
  ls_fcat-col_pos = f_var3.
  ls_fcat-edit = f_var4.
  IF ls_fcat-fieldname = 'VOLUM1'.
    ls_fcat-domname = 'MENG15'.
    ls_fcat-ref_table = 'VBRP'.
    ls_fcat-ref_field = 'VOLUM'.
  ENDIF.
  APPEND ls_fcat TO lt_fcat.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS'.
  SET TITLEBAR 'ZSALE'.

  IF lo_container IS INITIAL.
    CREATE OBJECT lo_container
      EXPORTING
        container_name = lv_container.
    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = lo_container.

    CREATE OBJECT lo_events.

    SET HANDLER: lo_events->data_changed FOR lo_grid.

    ls_layout-cwidth_opt = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = ls_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = lt_disp3
        it_fieldcatalog      = lt_Fcat.

** Register for EDIT Fields and Events
    CALL METHOD lo_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD lo_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.


MODULE user_command_0100 INPUT.
  DATA: Lt_SALES TYPE STANDARD TABLE OF zsales_table,
        ls_sales TYPE zsales_table.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      REFRESH lt_sales.
      LOOP AT lt_disp3 INTO ls_disp WHERE volum1 IS NOT INITIAL.
        CLEAR: ls_sales.
        ls_sales-kunnr = ls_disp-kunag.
*        ls_sales-name1  = ls_disp-name1.
        ls_sales-matkl =  ls_disp-matkl.
*        ls_sales-matkx  = ls_disp-wgbez60.
        ls_sales-potential  = ls_disp-volum.
        ls_sales-commitment  = ls_disp-volum1.
        ls_sales-regio = ls_disp-regio.
        ls_sales-zeinr =  ls_disp-zeinr.
        ls_sales-ernam  = sy-uname.
        ls_sales-erdat  = sy-datum.
        APPEND ls_sales TO lt_sales.
      ENDLOOP.
      IF lt_sales[] IS NOT INITIAL.
        MODIFY zsales_table FROM TABLE lt_sales.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          MESSAGE TEXT-001 TYPE 'S'.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMODULE.
