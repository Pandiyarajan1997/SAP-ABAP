*&---------------------------------------------------------------------*
*& Include          ZMM_MAT_MASTER_C01
*&---------------------------------------------------------------------*
CLASS lcl_material DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_final,        "for alv changes

              matnr TYPE matnr,
              werks TYPE werks,
              eisbe TYPE eisbe,
              maktx TYPE maktx,
              name1 TYPE name1,
              new   TYPE char10,
              msg   TYPE char100,

            END OF ty_final.


    TYPES : BEGIN OF ty_excel,     "for excel upload

              matnr TYPE matnr,
              werks TYPE werks,
              new   TYPE char10,
              msg   TYPE char100,

            END OF ty_excel.


    DATA : lt_final  TYPE TABLE OF ty_final,     "final it
           lt_return TYPE TABLE OF bapiret2.


    DATA : lv_alv    TYPE REF TO cl_gui_alv_grid,
           lv_custom TYPE REF TO cl_gui_custom_container,
           lt_row    TYPE TABLE OF lvc_s_modi.   "modified row checking

*methods for event handlers
    METHODS: change FOR EVENT data_changed
                    OF cl_gui_alv_grid IMPORTING er_data_changed.
*methods dec
    METHODS : fetch,     "fetching
      alv IMPORTING edit TYPE any OPTIONAL,       "mat master alv
      validation,    "input screen validation
      pop_up,        "save changes
      alv_change,    "changes on alv
      excel,         "excel file to it
      bapi IMPORTING matnr    TYPE matnr      "call bapi
                     werks    TYPE werks
                     row_id   TYPE any OPTIONAL
           EXPORTING
                     msg_type TYPE any
                     msg      TYPE any
           CHANGING  new      TYPE char10.

ENDCLASS.


CLASS lcl_material IMPLEMENTATION.
  METHOD fetch.
    REFRESH lt_final.
    SELECT                "Fetching data from marc , t001w & makt
            a~matnr,
            a~werks,
            a~eisbe,
            b~maktx,
            c~name1
            INTO TABLE @lt_final
            FROM marc AS a
            INNER JOIN makt AS b ON
            a~matnr = b~matnr
            INNER JOIN t001w AS c ON
            a~werks = c~werks
            WHERE a~matnr IN @so_matnr AND
            a~werks IN @so_werks AND
            b~spras = 'E'.

    IF sy-subrc = 0.

      MESSAGE s001.

    ELSE.

      MESSAGE e002.

    ENDIF.

  ENDMETHOD.

  METHOD alv.


    DATA lt_fcat TYPE lvc_t_fcat.   "data dec for fieldcat

    DATA(ls_layo) = VALUE lvc_s_layo( cwidth_opt = abap_true
                                      zebra = abap_true ).   "data dec for layout design


    IF lv_custom IS INITIAL.

*    **create object for custom container

      CREATE OBJECT lv_custom
        EXPORTING
          container_name = 'CUSTOM'.  "Name of the Screen CustCtrl Name to Link Container To alv

      IF lv_custom IS NOT INITIAL.

**create the object for alv
        CREATE OBJECT lv_alv
          EXPORTING
            i_parent = lv_custom.

      ENDIF.

    ENDIF.

    IF r1 = 'X'.                                    "data append to the fieldcat

      lt_fcat = VALUE #(
      ( col_pos = 1 fieldname = 'MATNR' scrtext_m = TEXT-002 )
      ( col_pos = 2 fieldname = 'MAKTX' scrtext_m = TEXT-003 )
      ( col_pos = 3 fieldname = 'WERKS' scrtext_m = TEXT-004 )
      ( col_pos = 4 fieldname = 'NAME1' scrtext_m = TEXT-005 )
      ( col_pos = 5 fieldname = 'EISBE' scrtext_m = TEXT-006 )
      ( col_pos = 6 fieldname = 'NEW'   scrtext_m = TEXT-007 edit = edit ) ).

    ELSEIF r2 = 'X'.

      lt_fcat = VALUE #(
    ( col_pos = 1 fieldname = 'MATNR' scrtext_m = TEXT-002 )
    ( col_pos = 2 fieldname = 'WERKS' scrtext_m = TEXT-004 )
    ( col_pos = 3 fieldname = 'NEW'   scrtext_m = TEXT-007 edit = edit )
    ( col_pos = 4 fieldname = 'MSG'   scrtext_m = TEXT-013 ) ).



    ENDIF.

*   name1 set the event handler for alv
    SET HANDLER change FOR lv_alv.
*display the alv
    lv_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layo    "Layout
      CHANGING
        it_outtab                     = lt_final    "Output Table
        it_fieldcatalog               = lt_fcat     "Field Catalog
    ).
    REFRESH lt_fcat.        "refresh fieldcat

  ENDMETHOD.

  METHOD alv_change.

* get the modified datas from alv and upload to db table

    LOOP AT lt_row INTO DATA(ls_row).

      DATA(ls_final) = lt_final[ ls_row-row_id ].

      IF sy-subrc = 0.

        bapi(
          EXPORTING
            matnr = ls_final-matnr
            werks = ls_final-werks
            row_id = ls_row-row_id
          CHANGING
            new   = ls_final-new
        ).

      ENDIF.

    ENDLOOP.


    IF lt_return IS INITIAL.

      MESSAGE : s003.       "success

    ELSE.

      REFRESH lt_return.

      MESSAGE : e007.       "error

    ENDIF.


  ENDMETHOD.

  METHOD validation.


    IF so_matnr IS INITIAL AND so_werks IS INITIAL AND r1 = 'X'.

      MESSAGE e004.

    ENDIF.

    IF p_fname IS INITIAL AND r2 = 'X'.

      MESSAGE e009.

    ENDIF.

    IF so_matnr-high IS NOT INITIAL.
      SELECT SINGLE matnr FROM marc INTO @DATA(lv_matnr)
      WHERE matnr = @so_matnr-high.
      IF sy-subrc NE 0.
        MESSAGE e005.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD change.

    APPEND LINES OF er_data_changed->mt_good_cells[] TO lt_row[].      "get changed data value row index

  ENDMETHOD.

  METHOD pop_up.
    "confirmation for material changes
    DATA : lv_ans TYPE char4,
           w_x    TYPE char1 VALUE abap_true.

    lv_alv->check_changed_data(                 "get changed values to it
                        CHANGING
                           c_refresh = w_x ).


    IF lt_row IS NOT INITIAL.    "if any changes on material master

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar      = TEXT-008
          text_question = TEXT-009
          text_button_1 = TEXT-010
          text_button_2 = TEXT-011
        IMPORTING
          answer        = lv_ans.

      IF lv_ans = '1'.      "if we click yes call bapi

        alv_change( ).

      ELSE.

        LEAVE SCREEN.

      ENDIF.

    ELSE.

      MESSAGE e006.

    ENDIF.

  ENDMETHOD.


  METHOD excel.

    DATA : lt_excel TYPE TABLE OF ty_excel,     "final it
           lt_raw   TYPE truxs_t_text_data,
           msg_type TYPE c.

    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'       "excel to it
      EXPORTING
        i_tab_raw_data       = lt_raw
        i_filename           = p_fname
      TABLES
        i_tab_converted_data = lt_excel.
    IF sy-subrc <> 0.

      MESSAGE e010.

    ENDIF.

    LOOP AT lt_excel INTO DATA(ls_final).

      bapi(                               "call bapi for update
        EXPORTING
          matnr = ls_final-matnr
          werks = ls_final-werks
        IMPORTING
          msg_type = msg_type
          msg      = ls_final-msg
        CHANGING
          new   = ls_final-new
      ).

      IF ls_final-msg IS NOT INITIAL.

        MODIFY lt_excel FROM ls_final TRANSPORTING msg.
        CLEAR ls_final.

      ENDIF.

      IF msg_type = 'S'.

        DELETE lt_excel WHERE matnr = ls_final-matnr AND werks = ls_final-werks.      "delete success records from excel it
        CLEAR msg_type.

      ENDIF.

    ENDLOOP.

    IF lt_return IS INITIAL.

      MESSAGE : s003.       "success

    ELSE.

      IF lt_excel IS NOT INITIAL.         "display the error records in alv

        REFRESH lt_return.

        MOVE-CORRESPONDING lt_excel TO lt_final.

        alv( ).
        CALL SCREEN 9001.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD bapi.

    DATA : bapimathead TYPE bapimathead,
           bapi_marc   TYPE bapi_marc,
           bapi_marcx  TYPE bapi_marcx,
           ls_return   TYPE bapiret2,
           lv_matnr    TYPE matnr18.


    IF new  CO ' 1234567890.' AND new GE 0 OR new = ' '.

      new = CONV eisbe( new ).

      lv_matnr = |{ matnr ALPHA = IN }|.
      bapimathead-material  = lv_matnr.
      bapimathead-mrp_view  = abap_true.
      bapi_marc-plant       = werks.
      bapi_marc-safety_stk  = new.
      bapi_marcx-plant      = werks.
      bapi_marcx-safety_stk = abap_true.


      IF bapimathead IS NOT INITIAL.        "update the database using bapi

        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata   = bapimathead
            plantdata  = bapi_marc
            plantdatax = bapi_marcx
          IMPORTING
            return     = ls_return.

        IF ls_return-type = 'S'.

          msg_type = ls_return-type.

          IF lt_row IS NOT INITIAL.

            DELETE lt_row WHERE row_id = row_id .         "delete the repeated update same value

          ENDIF.

          CLEAR : bapimathead,
                  bapi_marc,
                  bapi_marcx.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

        ELSE.

          APPEND ls_return TO lt_return.
          msg = ls_return-message.
          CLEAR ls_return.

        ENDIF.

      ENDIF.

    ELSE.

      ls_return-type = 'E'.
      msg = TEXT-015.
      APPEND ls_return TO lt_return.
      CLEAR ls_return.

    ENDIF.


  ENDMETHOD.


ENDCLASS.
