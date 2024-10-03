REPORT zrbdmatcc_r LINE-SIZE 200 NO STANDARD PAGE HEADING.

INCLUDE mmmgxd01. "DATA definitions for GUI
INCLUDE mmmgxd02. "DATA definitions for IDoc

"Screen 1000: Selection screen
PARAMETERS: p_matnr TYPE mara-matnr OBLIGATORY "material number
                    MATCHCODE OBJECT mat1
                    MEMORY ID mat.

"count of how many materials should be created
PARAMETERS: p_nummat TYPE matns DEFAULT 1 OBLIGATORY.

"test run only; not productive mode
PARAMETERS: p_test TYPE w_aktest DEFAULT 'X'.

"only try to create. If the material already exists, do not change it
PARAMETERS: p_nochan TYPE md_nochange DEFAULT 'X'.

"provide the selected organizations from the customizing table
PARAMETERS: p_table TYPE md_userset DEFAULT ' '.

"program was executed by submit
PARAMETERS: p_submit TYPE abap_bool DEFAULT abap_false NO-DISPLAY.

SELECTION-SCREEN SKIP 1.

"The views to be copied
SELECTION-SCREEN BEGIN OF BLOCK sichten WITH FRAME TITLE TEXT-090.
  "begin of note 1956786
  "basic data
  PARAMETERS: p_mara1 TYPE md_mara AS CHECKBOX MODIF ID p1 DEFAULT 'X'.

  "plant data
  PARAMETERS: p_marc1 TYPE md_marc AS CHECKBOX MODIF ID p2 DEFAULT 'X'.

  "storage location data
  PARAMETERS: p_mard1 TYPE md_mard AS CHECKBOX MODIF ID p3 DEFAULT 'X'.

  "distribution chain data
  PARAMETERS: p_mvke1 TYPE md_mvke AS CHECKBOX MODIF ID p5 DEFAULT 'X'.

  "warehouse number data
  PARAMETERS: p_mlgn1 TYPE md_mlgn AS CHECKBOX MODIF ID p6 DEFAULT 'X'.

  "storage type data
  PARAMETERS: p_mlgt1 TYPE md_mlgt AS CHECKBOX MODIF ID p7 DEFAULT 'X'.

  "valuation data
  PARAMETERS: p_mbew1 TYPE md_mbew AS CHECKBOX MODIF ID p4 DEFAULT 'X'.
  "end of note 1956786
SELECTION-SCREEN END OF BLOCK sichten.

"Filter options
SELECTION-SCREEN BEGIN OF BLOCK filter WITH FRAME TITLE TEXT-094.
  SELECT-OPTIONS: so_werks FOR lv_werks, "plants
                  so_lgort FOR lv_lgort, "storage locations
                  so_vkorg FOR lv_vkorg, "sales organizations
                  so_vtweg FOR lv_vtweg, "distribution channels
                  so_lgnum FOR lv_lgnum, "warehouse numbers
                  so_lgtyp FOR lv_lgtyp, "storage types
                  so_bwkey FOR lv_bwkey. "valuation areas
SELECTION-SCREEN END OF BLOCK filter.

"Screen 0400: Table screen
SELECTION-SCREEN BEGIN OF SCREEN 400.
  SELECTION-SCREEN BEGIN OF TABBED BLOCK tabblock1 FOR 25 LINES.
    "Tab materials
    SELECTION-SCREEN TAB (16) TEXT-001 USER-COMMAND comm0 DEFAULT SCREEN 301
      MODIF ID t0.
    "Tab plants
    SELECTION-SCREEN TAB (16) TEXT-002 USER-COMMAND comm1 DEFAULT SCREEN 501
      MODIF ID t1.
    "Tab storage locations
    SELECTION-SCREEN TAB (16) TEXT-003 USER-COMMAND comm2 DEFAULT SCREEN 502
      MODIF ID t2.
    "Tab distribution chains
    SELECTION-SCREEN TAB (16) TEXT-004 USER-COMMAND comm3 DEFAULT SCREEN 503
      MODIF ID t3.
    "Tab warehouse numbers
    SELECTION-SCREEN TAB (16) TEXT-005 USER-COMMAND comm4 DEFAULT SCREEN 504
      MODIF ID t4.
    "Tab storage types
    SELECTION-SCREEN TAB (16) TEXT-006 USER-COMMAND comm5 DEFAULT SCREEN 505
      MODIF ID t5.
    "Tab valuation areas
    SELECTION-SCREEN TAB (16) TEXT-007 USER-COMMAND comm6 DEFAULT SCREEN 506
      MODIF ID t6.
  SELECTION-SCREEN END OF BLOCK tabblock1.
SELECTION-SCREEN END OF SCREEN 400.

"Buffer the data which is entered by the user
DATA: lv_buf_matnr    TYPE mara-matnr.
DATA: lv_buf_nummat   TYPE numc3.
DATA: lv_buf_table(1) TYPE c.
DATA: lv_buf_mara(1)  TYPE c.
DATA: lv_buf_marc(1)  TYPE c.
DATA: lv_buf_mard(1)  TYPE c.
DATA: lv_buf_mbew(1)  TYPE c.
DATA: lv_buf_mvke(1)  TYPE c.
DATA: lv_buf_mlgn(1)  TYPE c.
DATA: lv_buf_mlgt(1)  TYPE c.
DATA: lv_buf_werks LIKE STANDARD TABLE OF so_werks.
DATA: lv_buf_lgort LIKE STANDARD TABLE OF so_lgort.
DATA: lv_buf_vkorg LIKE STANDARD TABLE OF so_vkorg.
DATA: lv_buf_vtweg LIKE STANDARD TABLE OF so_vtweg.
DATA: lv_buf_lgnum LIKE STANDARD TABLE OF so_lgnum.
DATA: lv_buf_lgtyp LIKE STANDARD TABLE OF so_lgtyp.
DATA: lv_buf_bwkey LIKE STANDARD TABLE OF so_bwkey.

"Internal tables and structures with client, material short text,
"material number with material short text, plant, storage location,
"distribution chain, warehouse number, storage type, valuation area,
"buffered selection parameters, refresh structure and customizing table
DATA: ls_mand    TYPE t_mand,
      ls_makt    TYPE t_makt,
      ls_mate    TYPE t_mate,
      lt_mate    TYPE TABLE OF t_mate,
      ls_marc    TYPE t_marc,
      lt_marc    TYPE TABLE OF t_marc,
      ls_mard    TYPE t_mard,
      lt_mard    TYPE TABLE OF t_mard,
      ls_mvke    TYPE t_mvke,
      lt_mvke    TYPE TABLE OF t_mvke,
      ls_mlgn    TYPE t_mlgn,
      lt_mlgn    TYPE TABLE OF t_mlgn,
      ls_mlgt    TYPE t_mlgt,
      lt_mlgt    TYPE TABLE OF t_mlgt,
      ls_mbew    TYPE t_mbew,
      lt_mbew    TYPE TABLE OF t_mbew,
      ls_refresh TYPE t_refresh.

"Contains the internal material numbers
DATA: lt_nrold TYPE TABLE OF t_intn.
DATA: lt_nrnew TYPE TABLE OF t_intn.

INITIALIZATION.
  CALL FUNCTION 'GET_FLE_TOPIC_MODE'
    EXPORTING
      iv_extension_topic = 'MFLE'
    IMPORTING
      ev_mode            = lv_mfle_mode.

  "Set which ALV toolbar buttons are not shown
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_maximum.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_minimum.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_sum.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_filter.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_detail.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_average.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_print.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_views.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_current_variant.
  APPEND ls_alv_buttons TO lt_alv_buttons.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_alv_buttons TO lt_alv_buttons.

  APPEND LINES OF lt_alv_buttons TO lt_alv_buttons_m.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_alv_buttons TO lt_alv_buttons_m.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_alv_buttons TO lt_alv_buttons_m.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_alv_buttons TO lt_alv_buttons_m.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_alv_buttons TO lt_alv_buttons_m.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_alv_buttons TO lt_alv_buttons_m.
  ls_alv_buttons = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_alv_buttons TO lt_alv_buttons_m.

  "note 2154866: Set GUI statuses
  APPEND 'PRIN' TO lt_exclude_1000.
  APPEND 'SJOB' TO lt_exclude_1000.

  APPEND 'SCRH' TO lt_exclude_0400.
  APPEND 'GET'  TO lt_exclude_0400.
  APPEND 'VSHO' TO lt_exclude_0400.
  APPEND 'VDEL' TO lt_exclude_0400.
  APPEND 'SPOS' TO lt_exclude_0400.

  "Do not allow inserts in ALV for material data
  ls_layout_m-no_rowins = 'X'.

  "Set optimal column width for ALV
  ls_layout-cwidth_opt = 'X'.
  ls_layout_m-cwidth_opt = 'X'.

  "Instantiate BAdI if available
  TRY.
      ctx = cl_badi_report_context=>get_instance( repid = sy-repid ).
      GET BADI r_badi
        CONTEXT
            ctx.
    CATCH cx_badi_not_implemented.                      "#EC NO_HANDLER
  ENDTRY.

AT SELECTION-SCREEN OUTPUT. "PBO
  IF sy-dynnr = '1000'. "Selection screen
    "note 2154866
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = '%_00'
      TABLES
        p_exclude = lt_exclude_1000.

    "Read the cached reference material number
    CLEAR lv_buf_matnr.                                   "note 2427932
    IMPORT lv_buf_matnr FROM MEMORY ID 'MMCCmatnr'.       "note 2427932

    "Free the memory of unneeded data
    FREE MEMORY ID 'MMCCmatnr'.                           "note 2427932

    lv_buf_nummat = p_nummat.
    lv_buf_table = p_table.
    lv_buf_mara = p_mara1.
    lv_buf_marc = p_marc1.
    lv_buf_mard = p_mard1.
    lv_buf_mbew = p_mbew1.
    lv_buf_mvke = p_mvke1.
    lv_buf_mlgn = p_mlgn1.
    lv_buf_mlgt = p_mlgt1.

    "build buffered select options
    IF p_submit IS NOT INITIAL.
      lv_buf_werks[] = so_werks[].
      lv_buf_lgort[] = so_lgort[].
      lv_buf_vkorg[] = so_vkorg[].
      lv_buf_vtweg[] = so_vtweg[].
      lv_buf_lgnum[] = so_lgnum[].
      lv_buf_lgtyp[] = so_lgtyp[].
      lv_buf_bwkey[] = so_bwkey[].

      p_submit = ' '.
    ENDIF.

  ELSEIF sy-dynnr = '0400'. "Tables screen
    "note 2154866
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = '%_CS'
      TABLES
        p_exclude = lt_exclude_0400.

    "Read the cached data only if the selection did not change
    IF ( sy-ucomm EQ 'ONLI' OR sy-ucomm EQ 'CRET' ).      "note 2427932
      IF ls_refresh-marap IS INITIAL.
        IMPORT lt_mate FROM MEMORY ID 'MMCCTables'.
      ENDIF.
      IF ls_refresh-marcp IS INITIAL AND ls_refresh-marcf IS INITIAL.
        IMPORT lt_marc FROM MEMORY ID 'MMCCTables'.
      ENDIF.
      IF ls_refresh-mardp IS INITIAL AND ls_refresh-mardf IS INITIAL
        AND ls_refresh-marcf IS INITIAL.
        IMPORT lt_mard FROM MEMORY ID 'MMCCTables'.
      ENDIF.
      IF ls_refresh-mbewp IS INITIAL AND ls_refresh-mbewf IS INITIAL.
        IMPORT lt_mbew FROM MEMORY ID 'MMCCTables'.
      ENDIF.
      IF ls_refresh-mvkep IS INITIAL AND ls_refresh-mvkef IS INITIAL.
        IMPORT lt_mvke FROM MEMORY ID 'MMCCTables'.
      ENDIF.
      IF ls_refresh-mlgnp IS INITIAL AND ls_refresh-mlgnf IS INITIAL.
        IMPORT lt_mlgn FROM MEMORY ID 'MMCCTables'.
      ENDIF.
      IF ls_refresh-mlgtp IS INITIAL AND ls_refresh-mlgtf IS INITIAL
        AND ls_refresh-mlgnf IS INITIAL.
        IMPORT lt_mlgt FROM MEMORY ID 'MMCCTables'.
      ENDIF.

      "Free the memory of unneeded data
      FREE MEMORY ID 'MMCCTables'.
    ENDIF.

    "Refresh the table with material numbers and material short texts if
    "needed
    IF ls_refresh-marap = 'X'.
      PERFORM build_mate_table.
    ENDIF.

    "Refresh the organisational tables if needed
    PERFORM build_organization_tables.

    "Hide unselected tabs or tabs with no data
    LOOP AT SCREEN INTO screen_wa.
      IF p_marc1 = ' ' AND screen_wa-group1 = 'T1'.
        screen_wa-active = '0'.
      ELSEIF p_mard1 = ' ' AND screen_wa-group1 = 'T2'.
        screen_wa-active = '0'.
      ELSEIF p_mvke1 = ' ' AND screen_wa-group1 = 'T3'.
        screen_wa-active = '0'.
      ELSEIF p_mlgn1 = ' ' AND screen_wa-group1 = 'T4'.
        screen_wa-active = '0'.
      ELSEIF p_mlgt1 = ' ' AND screen_wa-group1 = 'T5'.
        screen_wa-active = '0'.
      ELSEIF p_mbew1 = ' ' AND screen_wa-group1 = 'T6'.
        screen_wa-active = '0'.
      ENDIF.

      MODIFY SCREEN FROM screen_wa.
    ENDLOOP.
  ENDIF.



AT SELECTION-SCREEN ON EXIT-COMMAND.
  IF sy-dynnr = '1000'. "Selection screen
    CASE sy-ucomm.
      WHEN 'CBAC'.
        LEAVE PROGRAM.
      WHEN 'CEND'.
        LEAVE PROGRAM.
      WHEN 'CCAN'.
        LEAVE PROGRAM.
    ENDCASE.
  ELSEIF sy-dynnr = '0400'. "Table screen
    CASE sy-ucomm.
      WHEN 'CBAC'.
        "Check if the user changed data in the tables
        IF g_ref_grid0 IS NOT INITIAL.
          CALL METHOD g_ref_grid0->check_changed_data.
        ENDIF.
        IF g_ref_grid1 IS NOT INITIAL.
          CALL METHOD g_ref_grid1->check_changed_data.
        ENDIF.
        IF g_ref_grid2 IS NOT INITIAL.
          CALL METHOD g_ref_grid2->check_changed_data.
        ENDIF.
        IF g_ref_grid3 IS NOT INITIAL.
          CALL METHOD g_ref_grid3->check_changed_data.
        ENDIF.
        IF g_ref_grid4 IS NOT INITIAL.
          CALL METHOD g_ref_grid4->check_changed_data.
        ENDIF.
        IF g_ref_grid5 IS NOT INITIAL.
          CALL METHOD g_ref_grid5->check_changed_data.
        ENDIF.
        IF g_ref_grid6 IS NOT INITIAL.
          CALL METHOD g_ref_grid6->check_changed_data.
        ENDIF.

        EXPORT lt_mate lt_marc lt_mard lt_mvke
               lt_mlgn lt_mlgt lt_mbew
               TO MEMORY ID 'MMCCTables'.

        "Cache the reference material number
        EXPORT lv_buf_matnr TO MEMORY ID 'MMCCmatnr'.     "note 2427932

        "Prepare the submit table which contains the values of all
        "selection fields
        PERFORM prepare_submit_table.

        "Execute this report from scratch and fill the selection fields
        "with the provided data from the user
        SUBMIT rbdmatcc WITH SELECTION-TABLE lt_submit
        VIA SELECTION-SCREEN.
      WHEN 'CEND'.
        LEAVE PROGRAM.
      WHEN 'CCAN'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDIF.



AT SELECTION-SCREEN. "PAI
  IF lv_error IS NOT INITIAL.
    "Free the memory of unneeded data
    FREE MEMORY ID 'MMCCTables'.
    FREE MEMORY ID 'MMCCIntnum'.

    "Cache the data contained in the tables
    EXPORT lt_mate lt_marc lt_mard lt_mvke
           lt_mlgn lt_mlgt lt_mbew
           TO MEMORY ID 'MMCCTables'.

    "Cache the reference material number
    EXPORT lv_buf_matnr TO MEMORY ID 'MMCCmatnr'.         "note 2427932

    "Cache the internal material numbers
    EXPORT lt_nrnew TO MEMORY ID 'MMCCIntnum'.

    "Prepare the submit table which contains the values of all
    "selection fields
    PERFORM prepare_submit_table.

    "Execute this report from scratch and fill the selection fields
    "with the provided data from the user
    SUBMIT rbdmatcc WITH SELECTION-TABLE lt_submit
    VIA SELECTION-SCREEN.
  ENDIF.

  "Selection screen
  IF sy-dynnr = '1000'.
    IF ( sy-ucomm = 'ONLI' OR sy-ucomm = 'CRET' ).
      "Check if the material exists
      CALL FUNCTION 'MARA_SINGLE_READ'
        EXPORTING
          matnr             = p_matnr
        EXCEPTIONS
          lock_on_material  = 01
          lock_system_error = 02
          not_found         = 03
          OTHERS            = 04.
      CASE sy-subrc.
        WHEN 03.
          "NOT_FOUND
          MESSAGE e305(m3) WITH p_matnr.
        WHEN 01.
          "MESSAGE E022(M3) RAISING LOCK_ON_MATERIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        WHEN 02.
          "LOCK_SYSTEM_ERROR
          MESSAGE e021(m3).
        WHEN 04.
          "MESSAGE E035(MG) RAISING WRONG_CALL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDCASE.

      "Check that the user creates/extends at least 1 material
      IF p_nummat <= 0.
        MESSAGE e473(mg).
      ENDIF.

      "Check if the selection parameters changed
      PERFORM check_change_param.
      PERFORM check_change_soptions.

      "Call table screen
      CALL SELECTION-SCREEN '0400'.
    ELSE.
      "Check if the selection parameters changed
      PERFORM check_change_param.
      PERFORM check_change_soptions.
    ENDIF.

  ELSEIF sy-dynnr = '0400'. "Table screen
    IF sy-ucomm = 'CRET' OR sy-ucomm = 'ONLI'.
      "Check if the data entered in the tables is correct
      PERFORM check_mate_data.
      PERFORM check_werks_data.
      PERFORM check_lgort_data.
      PERFORM check_vtweg_data.
      PERFORM check_lgnum_data.
      PERFORM check_lgtyp_data.
      PERFORM check_bwkey_data.

      "Create or change the materials
      PERFORM create_materials.

      "Free the memory of unneeded data
      FREE MEMORY ID 'MMCCTables'.
      FREE MEMORY ID 'MMCCIntnum'.

      LEAVE TO SCREEN 0.

    ENDIF.
  ENDIF.

  INCLUDE mmmgxo01. "PBOs
  INCLUDE mmmgxi01. "PAIs
  INCLUDE mmmgxu01. "Forms for GUI
  INCLUDE mmmgxu02. "Forms for IDoc
