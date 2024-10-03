*&---------------------------------------------------------------------*
*& Report ZMM_ANS_MIGO_STATUS_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_infty_change_log_report.
TABLES: pa0000.

TYPES: BEGIN OF ty_output,
         pernr     TYPE    persno,
*           begda     TYPE    begda,
         massn     TYPE    massn,
         massn_new TYPE    massn,
*           mntxt     TYPE    mntxt,
*           massg     TYPE    massg,
         plans     TYPE    plans,
         stext     TYPE    stext,
         plans_new TYPE    plans,
         stext_new TYPE    stext,
         msg       TYPE    zremark,
       END OF ty_output.
DATA gt_output TYPE STANDARD TABLE OF ty_output.
DATA gw_output TYPE ty_output.

SELECT-OPTIONS s_pernr FOR pa0000-pernr.
SELECT-OPTIONS s_date FOR pa0000-begda OBLIGATORY.

INITIALIZATION.
  s_date-low = sy-datum.
  s_date-high = sy-datum.
  APPEND s_date.


START-OF-SELECTION.
  PERFORM f_process_data.
  PERFORM p_display_alv.
*&---------------------------------------------------------------------*
*& Form p_display_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM p_display_alv .
  DATA: lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties

  DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
        lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
        lo_content     TYPE REF TO cl_salv_form_element,

        lv_title       TYPE string,
        lv_rows        TYPE string.

  DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

  DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
        lo_columns    TYPE REF TO cl_salv_columns,
        lo_column     TYPE REF TO cl_salv_column_table.
  DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
  DATA: lr_groups TYPE REF TO cl_salv_sorts .
  DATA: toolbar TYPE REF TO cl_salv_functions_list .

  IF gt_output IS INITIAL.
    MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT .
  ELSE.
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_output.


*      lo_gr_alv->set_screen_status(
*        pfstatus      =  'SAL_STATUS'
*        report        =  sy-repid
*        set_functions = lo_gr_alv->c_functions_all ).

* Let's show all default buttons of ALV
        lo_gr_functions = lo_gr_alv->get_functions( ).
        lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
        lo_columns = lo_gr_alv->get_columns( ).
        lo_columns->set_optimize( 'X' ).

* Apply zebra style to lv_rows
        lo_display = lo_gr_alv->get_display_settings( ).
        lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

        TRY.
            lo_column ?= lo_columns->get_column( 'MASSN_NEW' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'New Action Type' ).
            lo_column->set_medium_text( 'New Action Type' ).
*          lo_column->set_short_text( 'Action Type' ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
*      TRY.
*          lo_column ?= lo_columns->get_column( 'STEXT_NEW' ).
*          lo_column->set_visible( if_salv_c_bool_sap=>true ).
*          lo_column->set_long_text( 'New Reason for Action' ).
*          lo_column->set_medium_text( 'New Reason/Action' ).
**          lo_column->set_short_text( 'Reason for Action' ).
*        CATCH cx_salv_not_found.
*        CATCH cx_salv_existing.
*        CATCH cx_salv_data_error.
*      ENDTRY.
        TRY.
            lo_column ?= lo_columns->get_column( 'PLANS_NEW' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'New Position' ).
            lo_column->set_medium_text( 'New Position' ).
*          lo_column->set_short_text( 'Position' ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
        lo_gr_alv->display( ).
      CATCH cx_salv_msg.
    ENDTRY.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_process_data .
  TYPES: BEGIN OF ty_pos_knvp,
           postion   TYPE plans,
           pf_code   TYPE parvw,
           x_pf_code TYPE parvw,
           pernr     TYPE persno,
         END OF ty_pos_knvp.
  DATA lt_knvp_update TYPE STANDARD TABLE OF ty_pos_knvp.
  DATA  pernr_tab            TYPE persno_range_tab.
  DATA  infty_tab            TYPE infty_range_tab.
  DATA  infty_tab_before     TYPE TABLE OF prelp.
  DATA  infty_tab_after      TYPE TABLE OF prelp.
  DATA  doc_key_tab          TYPE TABLE OF pldoc_key.
  DATA  event_opera          TYPE ecm_opera.
  DATA  lv_subrc TYPE sy-subrc.
  DATA lw_0000 TYPE p0000.
  DATA lw_0001 TYPE p0001.

  DATA: lt_yknvp TYPE TABLE OF fknvp,
        lt_xknvp TYPE TABLE OF fknvp,
        ls_xknvp TYPE fknvp,
        ls_yknvp TYPE fknvp.

  DATA: lv_objectid TYPE cdhdr-objectid.
* 1. Fill range tables
  LOOP AT s_pernr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = s_pernr-low ) TO pernr_tab.
  ENDLOOP.
  APPEND VALUE #( sign   = 'I' option = 'EQ' low  = '0000' ) TO infty_tab.
  APPEND VALUE #( sign   = 'I' option = 'EQ' low  = '0001' ) TO infty_tab.
* 1. Read documents
  CALL FUNCTION 'HR_INFOTYPE_LOG_GET_LIST'
    EXPORTING
      begda              = s_date-low
      endda              = s_date-high
    IMPORTING
      subrc              = lv_subrc
    TABLES
      pernr_tab          = pernr_tab
      infty_tab          = infty_tab
      infty_logg_key_tab = doc_key_tab.
* 2. Process documents and fill event table
  SORT doc_key_tab BY pernr infty.
  LOOP AT doc_key_tab INTO DATA(doc_key).
    CLEAR event_opera.
*
*   2.1. Get details for document
    CALL FUNCTION 'HR_INFOTYPE_LOG_GET_DETAIL'
      EXPORTING
        logged_infotype  = doc_key
      IMPORTING
        subrc            = lv_subrc
      TABLES
        infty_tab_before = infty_tab_before
        infty_tab_after  = infty_tab_after.
*

*   2.2. Do not take locked infotype records
    DELETE  infty_tab_before WHERE sprps = abap_true.
    DELETE  infty_tab_after  WHERE sprps = abap_true.
    READ TABLE infty_tab_before INTO DATA(w_prelp) WITH KEY endda = '99991231'.
    READ TABLE infty_tab_after INTO DATA(w_prelp_new) WITH KEY endda = '99991231'.
    gw_output-pernr    =  doc_key-pernr.
*    gw_output-begda = lw_0000-begda.
    CASE w_prelp-infty.
      WHEN '0000'.
        CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_pnnnn
          EXPORTING
            prelp = w_prelp
          IMPORTING
            pnnnn = lw_0000.
        gw_output-massn = lw_0000-massn.
*          SELECT SINGLE mntxt FROM  t529t
*            INTO @DATA(l_mntxt)
*            WHERE sprsl = @sy-langu
*                 AND  massn = @gw_output-massn .
*          SELECT SINGLE mgtxt FROM  t530t
*            INTO @DATA(l_mgtxt)
*            WHERE sprsl = @sy-langu
*                 AND  massn = @gw_output-massn
*                 AND massg = @gw_output-massg .
      WHEN '0001'.
        CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_pnnnn
          EXPORTING
            prelp = w_prelp
          IMPORTING
            pnnnn = lw_0001.
        gw_output-plans = lw_0001-plans.
        SELECT SINGLE stext FROM hrp1000
          INTO gw_output-stext
          WHERE plvar = '01' AND
                otype = 'S' AND
                objid = lw_0001-plans.
      WHEN OTHERS.
    ENDCASE.
    CASE w_prelp_new-infty.
      WHEN '0000'.
        CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_pnnnn
          EXPORTING
            prelp = w_prelp_new
          IMPORTING
            pnnnn = lw_0000.
        gw_output-massn_new = lw_0000-massn.
*          SELECT SINGLE mntxt FROM  t529t
*            INTO @DATA(l_mntxt)
*            WHERE sprsl = @sy-langu
*                 AND  massn = @gw_output-massn .
*          SELECT SINGLE mgtxt FROM  t530t
*            INTO @DATA(l_mgtxt)
*            WHERE sprsl = @sy-langu
*                 AND  massn = @gw_output-massn
*                 AND massg = @gw_output-massg .
      WHEN '0001'.
        CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_pnnnn
          EXPORTING
            prelp = w_prelp_new
          IMPORTING
            pnnnn = lw_0001.
        gw_output-plans_new = lw_0001-plans.
        SELECT SINGLE stext FROM hrp1000
          INTO gw_output-stext_new
          WHERE plvar = '01' AND
                otype = 'S' AND
                objid = lw_0001-plans.
      WHEN OTHERS.
    ENDCASE.
    AT END OF pernr.
      APPEND gw_output TO gt_output.
      CLEAR gw_output.
    ENDAT.
  ENDLOOP.
  IF gt_output IS  NOT INITIAL.
    " Job Code for New positions
    SELECT a~objid AS plans,
           a~sobid AS stell,
           b~parvw AS pfcode,
           b~parvw2 AS parvw2, " Partner Function X Series
           b~pernr AS pernr " Default Pernr
           INTO TABLE @DATA(lt_positions_new)
           FROM hrp1001 AS a INNER JOIN zhr_jobcode AS b ON b~stell = a~sobid
      FOR ALL ENTRIES IN @gt_output
      WHERE plvar = '01'
        AND otype = 'S'
        AND objid = @gt_output-plans_new
        AND rsign = 'B'
        AND relat = '007'
        AND SCLAS = 'C'.

    " Job Code for Old positions
    SELECT a~objid AS plans,
           a~sobid AS stell,
           b~parvw AS pfcode,
           b~parvw2 AS parvw2, " Partner Function X Series
           b~pernr AS pernr " Default Pernr
           INTO TABLE @DATA(lt_positions_old)
           FROM hrp1001 AS a INNER JOIN zhr_jobcode AS b ON b~stell = a~sobid
      FOR ALL ENTRIES IN @gt_output
      WHERE plvar = '01'
        AND otype = 'S'
        AND objid = @gt_output-plans
        AND rsign = 'B'
        AND relat = '007'
        AND SCLAS = 'C'.

*    SELECT * FROM zhr_bp_set_pernr INTO TABLE @DATA(lt_default_pernr).
    DELETE lt_positions_new WHERE stell = '60000192' AND pfcode = 'L5'.
    DELETE lt_positions_old WHERE stell = '60000192' AND pfcode = 'L5'.
  ENDIF.

  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
    <fs_output>-msg = 'Not Eligible'.

*    SELECT SINGLE parvw2 INTO @DATA(lv_pf_code)
*          FROM zhr_bp_set_pernr
*    parvw
    IF <fs_output>-plans_new = '99999999'.
      READ TABLE lt_positions_old INTO DATA(lw_pos) WITH KEY plans = <fs_output>-plans.
      IF sy-subrc = 0.
        DATA(lv_postion) = <fs_output>-plans.
        DATA(lv_pf_code) = lw_pos-parvw2.
        DATA(lv_pernr) = lw_pos-pernr. " Default Pernr
        APPEND VALUE #( postion = lv_postion
                        pf_code = lw_pos-pfcode
                        x_pf_code = lw_pos-parvw2
                        pernr     = lv_pernr
                      ) TO lt_knvp_update.
      ENDIF.
    ELSE.
      READ TABLE lt_positions_new INTO lw_pos WITH KEY plans = <fs_output>-plans_new.
      IF sy-subrc = 0.
        lv_postion  = <fs_output>-plans_new.
        lv_pf_code = lw_pos-parvw2.
        lv_pernr = <fs_output>-pernr.
        APPEND VALUE #( postion = lv_postion
                        pf_code = lw_pos-pfcode
                        x_pf_code = lw_pos-parvw2
                        pernr     = lv_pernr
                      ) TO lt_knvp_update.
      ENDIF.
      READ TABLE lt_positions_old INTO lw_pos WITH KEY plans = <fs_output>-plans.
      IF sy-subrc = 0.
        lv_postion  = <fs_output>-plans.
        lv_pf_code = lw_pos-parvw2.
        SELECT SINGLE sobid INTO @DATA(lv_old_pernr)
          FROM hrp1001
          WHERE plvar = '01'
            AND otype = 'S'
            AND objid = @lv_postion
            AND rsign = 'A'
            AND relat = '008'
            AND sclas = 'P'
            AND begda LE @sy-datum
            AND endda GE @sy-datum.
        IF sy-subrc = 0.
          lv_pernr = lv_old_pernr.
        ELSE.
          lv_pernr = lw_pos-pernr. " Default Pernr
        ENDIF.
        APPEND VALUE #( postion = lv_postion
                        pf_code = lw_pos-pfcode
                        x_pf_code = lw_pos-parvw2
                        pernr     = lv_pernr
                      ) TO lt_knvp_update.
      ENDIF.
    ENDIF.
    LOOP AT lt_knvp_update INTO DATA(lw_data).

      " Select Customer from Patner Function X? Postion Vendors in mapping table ZHR_BP_SET_PERNR
      SELECT SINGLE sobid INTO @DATA(lv_pos_vendor)
        FROM hrp1001
        WHERE plvar = '01'
          AND otype = 'S'
          AND objid = @lw_data-postion
          AND rsign = 'A'
          AND relat = '008'
          AND sclas = 'BP'.
      IF sy-subrc = 0.
        DATA(lv_lifnr) = CONV num10( lv_pos_vendor ).
        SELECT kunnr FROM knvp
              INTO TABLE @DATA(lt_cust)
              WHERE parvw  = @lw_data-x_pf_code
                AND lifnr = @lv_lifnr.
        DELETE ADJACENT DUPLICATES FROM lt_cust COMPARING ALL FIELDS.
      ENDIF.

      IF lt_cust IS NOT INITIAL.
        SELECT * FROM knvp
              INTO TABLE @DATA(lt_knvp)
              FOR ALL ENTRIES IN @lt_cust
              WHERE kunnr = @lt_cust-kunnr
                AND parvw  = @lw_data-pf_code.
*              AND pernr = @<fs_output>-pernr.
        IF sy-subrc = 0.
          DATA(lt_knvp_new) = lt_knvp[].

          CLEAR : lt_knvp_new[].
          LOOP AT lt_knvp ASSIGNING FIELD-SYMBOL(<fs_knvp>).
            <fs_knvp>-pernr = lw_data-pernr.
            APPEND <fs_knvp> TO lt_knvp_new.
          ENDLOOP.

          MODIFY knvp FROM TABLE lt_knvp_new.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

      LOOP AT lt_knvp ASSIGNING <fs_knvp>.
        CLEAR: ls_yknvp,lt_yknvp.
        CLEAR: ls_xknvp,lt_xknvp.
* Call the FM to write entry to CDHDR and CDPOS for therespective update
        CLEAR lv_objectid.
        lv_objectid = <fs_knvp>-kunnr.

* fill the before structure
        MOVE-CORRESPONDING <fs_knvp> TO ls_yknvp.
        ls_yknvp-mandt = sy-mandt.
        ls_yknvp-kz = 'I'.
        APPEND ls_yknvp TO lt_yknvp.
*fill the after structure
        READ TABLE lt_knvp_new INTO DATA(lw_knv) WITH KEY kunnr = <fs_knvp>-kunnr
                                               vkorg = <fs_knvp>-vkorg
                                               vtweg = <fs_knvp>-vtweg
                                               spart = <fs_knvp>-spart
                                               parvw = <fs_knvp>-parvw.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <fs_knvp> TO ls_xknvp.
          ls_xknvp-mandt = sy-mandt.
          ls_xknvp-kz = 'I'.
          APPEND ls_xknvp TO lt_xknvp.
        ENDIF.

        CALL FUNCTION 'DEBI_WRITE_DOCUMENT'
          EXPORTING
            objectid = lv_objectid
            tcode    = 'XD02'
            utime    = sy-uzeit
            udate    = sy-datum
            username = sy-uname
            upd_knvp = 'U'
          TABLES
            xknvp    = lt_xknvp
            yknvp    = lt_yknvp.
      ENDLOOP.
      CLEAR: lt_cust,
             lt_knvp,
             lt_knvp_new.
    ENDLOOP.
    CLEAR: lt_knvp_update.
  ENDLOOP.
ENDFORM.
