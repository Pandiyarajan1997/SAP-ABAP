*&---------------------------------------------------------------------*
*& Include          ZSD_UPDATE_CUSPOS_C01
*&---------------------------------------------------------------------*

CLASS lcl_cust DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_excel,        "excel str

              kunnr   TYPE kunnr,
              hrobjid TYPE hrobjid,

            END OF ty_excel.

    TYPES : BEGIN OF ty_final,         "final alv str

              kunnr  TYPE kunnr,
              vkorg  TYPE vkorg,
              vtweg  TYPE vtweg,
              spart  TYPE spart,
              parvw  TYPE parvw,
              kunnr2 TYPE kunnr,
              lifnr  TYPE lifnr,
              pernr  TYPE p_pernr,

            END OF ty_final.


    DATA : gt_final  TYPE TABLE OF ty_final.     "final it dec

*methods dec
    METHODS : exc_convert,     "excel convert
      alv.             "display alv

ENDCLASS.


CLASS lcl_cust IMPLEMENTATION.
  METHOD exc_convert.

************************data dec*************************

    DATA : lt_excel    TYPE TABLE OF ty_excel,
           lt_raw      TYPE truxs_t_text_data,
           lt_position TYPE TABLE OF zhr_so_to_top,
           gs_final    TYPE ty_final.

***************************excel to it*******************

    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_tab_raw_data       = lt_raw
        i_filename           = p_fname
      TABLES
        i_tab_converted_data = lt_excel.

    IF lt_excel IS NOT INITIAL.

      LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fs_excel2>).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_excel2>-kunnr
          IMPORTING
            output = <fs_excel2>-kunnr.

      ENDLOOP.

      SORT lt_excel BY kunnr.

      SELECT kunnr,vkorg,vtweg,spart FROM knvv INTO TABLE @DATA(lt_knvv)
                                     FOR ALL ENTRIES IN @lt_excel      "get data from knvv
                                     WHERE kunnr = @lt_excel-kunnr.
      IF sy-subrc = 0.

        SORT lt_knvv BY kunnr vkorg vtweg spart.

        LOOP AT lt_knvv ASSIGNING FIELD-SYMBOL(<fs_knvv>).

          AT NEW kunnr.

            READ TABLE lt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>) WITH KEY kunnr = <fs_knvv>-kunnr BINARY SEARCH.

            IF <fs_excel> IS ASSIGNED.

              REFRESH lt_position.

              CALL FUNCTION 'ZHR_SO_TO_TOP_POSITION'        "get data from postion fm
                EXPORTING
                  so_position  = <fs_excel>-hrobjid
                TABLES
                  it_positions = lt_position.

            ENDIF.

          ENDAT.

          LOOP AT lt_position ASSIGNING FIELD-SYMBOL(<fs_position>) WHERE parvw_new IN so_parvw OR parvw_unc IN so_parvw.

            gs_final-kunnr = <fs_knvv>-kunnr.
            gs_final-vkorg = <fs_knvv>-vkorg.
            gs_final-vtweg = <fs_knvv>-vtweg.
            gs_final-spart = <fs_knvv>-spart.

*          IF <fs_position>-parvw_new = 'XN'.

            gs_final-lifnr = <fs_position>-buspartner.
            gs_final-parvw = <fs_position>-parvw_new.
            IF gs_final-parvw IN so_parvw.
              APPEND gs_final TO gt_final.
            ENDIF.

            CLEAR gs_final-lifnr.

*            gs_final-pernr = <fs_position>-pernr.
*            gs_final-parvw = <fs_position>-parvw_unc.
*            APPEND gs_final TO gt_final.
*            CLEAR gs_final.

*          ELSE.



            gs_final-pernr = <fs_position>-pernr.
            gs_final-parvw = <fs_position>-parvw_unc.

            IF gs_final-parvw IN so_parvw.
              APPEND gs_final TO gt_final.

            ENDIF.
            CLEAR gs_final.
*          ENDIF.

          ENDLOOP.

        ENDLOOP.

      ENDIF.

*    ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD alv.

    DATA : lv_alv    TYPE REF TO cl_gui_alv_grid,
           lv_custom TYPE REF TO cl_gui_custom_container.

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
    "data append to the fieldcat

    lt_fcat = VALUE #(
    ( col_pos = 1 fieldname = 'KUNNR'  scrtext_m = TEXT-002 )
    ( col_pos = 2 fieldname = 'VKORG'  scrtext_m = TEXT-003 )
    ( col_pos = 3 fieldname = 'VTWEG'  scrtext_m = TEXT-004 )
    ( col_pos = 4 fieldname = 'SPART'  scrtext_m = TEXT-005 )
    ( col_pos = 5 fieldname = 'PARVW'  scrtext_m = TEXT-006 )
    ( col_pos = 6 fieldname = 'KUNNR2' scrtext_m = TEXT-007 )
    ( col_pos = 7 fieldname = 'LIFNR'  scrtext_m = TEXT-008 )
    ( col_pos = 8 fieldname = 'PERNR'  scrtext_m = TEXT-009 ) ).


*display the alv
    lv_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layo    "Layout
      CHANGING
        it_outtab                     = gt_final    "Output Table
        it_fieldcatalog               = lt_fcat     "Field Catalog
    ).
    REFRESH lt_fcat.        "refresh fieldcat

  ENDMETHOD.

ENDCLASS.
