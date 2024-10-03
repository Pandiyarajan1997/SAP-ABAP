*&---------------------------------------------------------------------*
*& Report ZHR_EMP_BP_SYNCH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_emp_bp_synch.

TYPES: BEGIN OF Ty_pernr,
         pernr TYPE persno,
       END OF ty_pernr.

TYPES: BEGIN OF Ty_objid,
         objid TYPE hrobjid,
       END OF ty_objid.

TYPES: BEGIN OF Ty_partner,
         partner TYPE bu_partner,
       END OF ty_partner.

TYPES: BEGIN OF ty_bpbank,
        PARTNER TYPE BU_PARTNER,
        BKVID TYPE BU_BKVID,
        BANKS TYPE BU_BANKS,
        BANKL TYPE BU_BANKK,
        BANKN TYPE BU_BANKN,
        BKONT TYPE BU_BKONT,
       END OF ty_bpbank.

TYPES: BEGIN OF ty_output,  "Structure for infotype creation status"
         pernr     TYPE pernr_d,
         infty     TYPE infty,
         subty     TYPE sbttx,
         status(1) TYPE c,
         message   TYPE char100,
       END OF ty_output.

DATA: Gv_pernr TYPE pa0001-pernr.

DATA: gt_pernr TYPE TABLE OF ty_pernr,
      gs_pernr TYPE ty_pernr.

DATA: gt_objid TYPE TABLE OF ty_objid,
      gs_objid TYPE ty_objid.

DATA: gt_hiring TYPE STANDARD TABLE OF zhr_hiring_tab,
      gs_hiring TYPE zhr_hiring_tab.

DATA: gt_partner TYPE TABLE OF ty_partner,
      gs_partner TYPE ty_partner.

*****Internal Table For Error Display in infotype 0000 Creation*******
DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.

DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_pernr FOR Gv_pernr MATCHCODE OBJECT prem.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_rad1 RADIOBUTTON GROUP rad1,
              p_rad2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b4.


START-OF-SELECTION.
  REFRESH gt_pernr.

  IF p_rad1 EQ abap_true.

    SELECT * FROM zhr_hiring_tab
      INTO TABLE gt_hiring
      WHERE status = '04'
        AND pernr IN s_pernr
        AND pernr_vendor NE 'X'.
    IF sy-subrc = 0.
      LOOP AT gt_hiring INTO gs_hiring.
        CLEAR gs_pernr-pernr.
        gs_pernr-pernr = gs_hiring-pernr.
        APPEND gs_pernr TO gt_pernr.
      ENDLOOP.
    ENDIF.

  ELSE.

    SELECT
      pa0001~pernr
      INTO TABLE gt_pernr
      FROM pa0000 INNER JOIN pa0001 ON pa0000~pernr = pa0001~pernr
      WHERE pa0000~pernr IN s_pernr
        AND pa0000~stat2 EQ '3'
        AND pa0001~begda LE sy-datum
        AND pa0000~endda GE sy-datum.
    IF sy-subrc = 0.

    ENDIF.
  ENDIF.

  IF gt_pernr[] IS NOT INITIAL.
    PERFORM f_pernr_bp_creation.
  ENDIF.


end-of-SELECTION.

  IF gt_output[] IS NOT INITIAL.
    PERFORM f_display_output.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form f_pernr_bp_creation
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pernr_bp_creation .

  DATA: lt_pernr TYPE TABLE OF ty_pernr,
        ls_pernr TYPE ty_pernr.

  DATA: lt_seltab TYPE TABLE OF rsparams,
        ls_seltab LIKE LINE OF lt_seltab.

  DATA: lt_hrp1001_cp TYPE STANDARD TABLE OF hrp1001,
        lt_hrp1001_bp TYPE STANDARD TABLE OF hrp1001,
        ls_hrp1001    TYPE hrp1001,
        gs_hrp1001    TYPE hrp1001.


  DATA: lt_PA0009 TYPE TABLE OF pa0009,
        ls_pa0009 TYPE pa0009.

  DATA: lt_pa0006 TYPE TABLE OF pa0006,
        ls_pa0006 TYPE TABLE OF pa0006.


  IF gt_pernr[] IS NOT INITIAL.
    SELECT *
      FROM pa0009
      INTO TABLE lt_pa0009
      FOR ALL ENTRIES IN gt_pernr
     WHERE pernr = gt_pernr-pernr
       AND subty = '0'
       AND begda <= sy-datum
       AND endda >= sy-datum.
    IF sy-subrc = 0.

    ENDIF.


    SELECT *
      FROM pa0006
      INTO TABLE lt_pa0006
      FOR ALL ENTRIES IN gt_pernr
     WHERE pernr = gt_pernr-pernr
       AND subty = '1'
       AND begda <= sy-datum
       AND endda >= sy-datum.
    IF sy-subrc = 0.

    ENDIF.

    SELECT * FROM hrp1001
      INTO TABLE lt_hrp1001_cp
      WHERE otype = 'P'
      AND objid EQ gs_hiring-pernr
      AND plvar = '01'
      AND rsign = 'A'
      AND relat = '209'
      AND begda LE '99991231'
      AND endda GE gs_hiring-joining_date
      AND sclas EQ 'CP'.
    IF sy-subrc = 0.



      LOOP AT lt_hrp1001_cp  INTO ls_hrp1001 .
        CLEAR gs_objid.
        gs_objid-objid = ls_hrp1001-objid.
        APPEND gs_objid TO gt_objid.
      ENDLOOP.

      IF gt_objid[] IS NOT INITIAL.
        SELECT * FROM hrp1001
          INTO TABLE lt_hrp1001_bp
          FOR ALL ENTRIES IN gt_objid
         WHERE otype EQ 'CP'
           AND objid EQ gt_objid-objid
           AND plvar = '01'
           AND rsign = 'B'
           AND relat = '207'
           AND sclas EQ 'BP'.
        IF sy-subrc = 0.
          SORT lt_hrp1001_bp BY sobid.
          LOOP AT lt_hrp1001_bp  INTO ls_hrp1001 .
            CLEAR gs_partner.
            gs_partner-partner = ls_hrp1001-sobid+0(10).
            APPEND gs_partner TO gt_partner.
          ENDLOOP.



        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


  SORT lt_hrp1001_cp BY objid.
  SORT lt_hrp1001_bp BY objid.

  LOOP AT gt_pernr INTO gs_pernr.

*get BP object
    REFRESH lt_pernr.
    CLEAR: ls_hrp1001,gs_hrp1001.
    READ TABLE lt_hrp1001_cp INTO ls_hrp1001 WITH KEY objid = gs_pernr-pernr BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_hrp1001_bp INTO gs_hrp1001 WITH KEY objid = ls_hrp1001-objid BINARY SEARCH.
      IF sy-subrc = 0.
      ELSE.
        CLEAR ls_pernr.
        ls_pernr-pernr = gs_pernr-pernr.
        APPEND ls_pernr TO lt_pernr.
      ENDIF.
    ENDIF.

  ENDLOOP.



  REFRESH: lt_seltab.
  CLEAR ls_seltab.
  ls_seltab-selname = 'PNPTIMR1'.          " Name of parameter on submitted program
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = 'X'.
  APPEND ls_seltab TO lt_seltab.

  LOOP AT gt_hiring INTO gs_hiring.
    CLEAR ls_seltab.
    ls_seltab-selname = 'PNPPERNR'.          " Name of parameter on submitted program
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = gs_hiring-pernr.
    APPEND ls_seltab TO lt_seltab.
  ENDLOOP.

  SUBMIT /shcm/rh_sync_bupa_empl_single
       WITH SELECTION-TABLE lt_seltab
       EXPORTING LIST TO MEMORY
       AND RETURN.

  WAIT UP TO 10 SECONDS.

*  REFRESH lt_hrp1001.


  LOOP AT gt_hiring INTO gs_hiring.

    CLEAR: ls_hrp1001,gs_hrp1001.
    SELECT SINGLE * FROM hrp1001
               INTO  ls_hrp1001
               WHERE otype = 'P'
                 AND objid EQ gs_hiring-pernr
                 AND plvar = '01'
                 AND rsign = 'A'
                 AND relat = '209'
                 AND begda LE '99991231'
                 AND endda GE gs_hiring-joining_date
                 AND sclas EQ 'CP'.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM hrp1001
                      INTO gs_hrp1001
                      WHERE otype EQ 'CP'
                        AND objid EQ ls_hrp1001-sobid
                        AND plvar = '01'
                        AND rsign = 'B'
                        AND relat = '207'
                        AND sclas EQ 'BP'.
      IF sy-subrc = 0.
        CLEAR gs_output.
        gs_output-pernr = gs_hiring-pernr.
        gs_output-subty = 'BP'.
        gs_output-infty = 'HRP1001'.
        gs_output-status = 'S'.
        CONCATENATE 'Employee linked to BP' gs_hrp1001-sobid 'for CP' ls_hrp1001-sobid INTO gs_output-message SEPARATED BY space.
        APPEND gs_output TO gt_output.

        " Lock Table for update
        CALL FUNCTION 'ENQUEUE_EZZHRHIRING'
          EXPORTING
            mode_zhr_hiring_tab = 'E'
            mandt               = sy-mandt
            temp_empno          = gs_hiring-temp_empno
            temp_empid          = gs_hiring-temp_empid
          EXCEPTIONS
            foreign_lock        = 1
            system_failure      = 2
            OTHERS              = 3.

        IF sy-subrc = 0.
          UPDATE zhr_hiring_tab SET pernr_vendor = 'X'
                                    centralperson = ls_hrp1001-sobid
                                    bpid = gs_hrp1001-sobid
                               WHERE temp_empno = gs_hiring-temp_empno
                               AND temp_empid = gs_hiring-temp_empid
                               AND pernr = gs_hiring-pernr.
        ELSE.
          CLEAR gs_output.
          gs_output-pernr = gs_hiring-pernr.
          gs_output-subty = 'BP'.
          gs_output-infty = 'HRP1001'.
          gs_output-status = 'E'.
          gs_output-message   = 'Update for Table Failed due to lock'.
          APPEND gs_output TO gt_output.
        ENDIF.

        " Release Lock for  Table
        CALL FUNCTION 'DEQUEUE_EZZHRHIRING'
          EXPORTING
            mode_zhr_hiring_tab = 'E'
            mandt               = sy-mandt
            temp_empno          = gs_hiring-temp_empno
            temp_empid          = gs_hiring-temp_empid.

      ELSE.
        CLEAR gs_output.
        gs_output-pernr = gs_hiring-pernr.
        gs_output-subty = 'BP'.
        gs_output-infty = 'HRP1001'.
        gs_output-status = 'E'.
        gs_output-message   = 'Employee BP linking Failed'.
        APPEND gs_output TO gt_output.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_output .

*TYPES: BEGIN OF ty_output,  "Structure for infotype creation status"
*         pernr     TYPE pernr_d,
*         infty     TYPE infty,
*         subty     TYPE sbttx,
*         status(1) TYPE c,
*         message   TYPE char100,
*       END OF ty_output.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '1'.
  gs_fcat-fieldname = 'PERNR'.
  gs_fcat-tabname  = 'GT_OUTPUT'.
  gs_fcat-seltext_m  = 'Employee Number'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '2'.
  gs_fcat-fieldname = 'INFTY'.
  gs_fcat-tabname  = 'GT_OUTPUT'.
  gs_fcat-seltext_m  = 'Infotype'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '3'.
  gs_fcat-fieldname = 'SUBTY'.
  gs_fcat-tabname  = 'GT_OUTPUT'.
  gs_fcat-seltext_m  = 'Subtype'.
  gs_fcat-no_out = 'X'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '4'.
  gs_fcat-fieldname = 'STATUS'.
  gs_fcat-tabname  = 'GT_OUTPUT'.
  gs_fcat-seltext_m  = 'STATUS'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '5'.
  gs_fcat-fieldname = 'MESSAGE'.
  gs_fcat-tabname  = 'GT_OUTPUT'.
  gs_fcat-seltext_m  = 'MESSAGE'.
  APPEND gs_fcat TO gt_fcat.

  gs_layout-colwidth_optimize = 'X'.

***********ALV DISPLAY  *******************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = gt_fcat
    TABLES
      t_outtab           = GT_output.

ENDFORM.
