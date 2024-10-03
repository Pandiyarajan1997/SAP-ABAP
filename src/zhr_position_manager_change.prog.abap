*&---------------------------------------------------------------------*
*& Report ZHR_POSITION_MANAGER_CHANGE
*&---------------------------------------------------------------------*
*&Created By: Samsudeen M
*&Created On: 26.05.2023
*&Purpose: Changing Reporting Manager for employee based on position
*&Reference: Ramakrishnan J
*&---------------------------------------------------------------------*
REPORT zhr_position_manager_change.

TYPES: BEGIN OF ty_posmgrchg,
         sel        TYPE loevm,
         position   TYPE hrp1000-objid,
         posdesc    TYPE hrp1000-stext,
         posempno   TYPE pernr_d,
         posempname TYPE sname,
         oldrepmgr  TYPE pernr_d,
         oldmgrname TYPE sname,
         oldmgrpos  TYPE hrp1001-sobid,
         oldmgrpdes TYPE hrp1000-stext,
         begda      TYPE begda,
         newmgr     TYPE pernr_d,
         newmgrname TYPE sname,
         newmgrpos  TYPE hrp1000-objid,
         newmgrpdes TYPE hrp1000-stext,
         type       TYPE bapi_mtype,
         msg        TYPE string,
       END OF ty_posmgrchg.
DATA: gt_posdtls TYPE TABLE OF ty_posmgrchg,
      gs_posdtls TYPE ty_posmgrchg.

FIELD-SYMBOLS <fs_posdtls> TYPE ty_posmgrchg.
DATA: lv_position TYPE hrp1000-objid.
DATA: gv_editable TYPE abap_bool.
DATA: lo_gr_alv TYPE REF TO cl_salv_table. " Variables for ALV properties
DATA :gt_fcat    TYPE lvc_t_fcat,
      gs_fcat    TYPE lvc_s_fcat,
      gt_exclude TYPE ui_functions,
      gs_excl    TYPE ui_func,
      gs_layout  TYPE lvc_s_layo.
CONSTANTS: gc_otype_o TYPE hrp1000-otype VALUE 'O',
           gc_otype_s TYPE hrp1000-otype VALUE 'S',
           gc_otype_c TYPE hrp1000-otype VALUE 'C',
           gc_otype_k TYPE hrp1000-otype VALUE 'K'.

CONSTANTS: gc_plan_ver TYPE hrp1000-plvar VALUE '01'.

CONSTANTS: gc_rsign_a TYPE hrp1001-rsign VALUE 'A',
           gc_rsign_b TYPE hrp1001-rsign VALUE 'B'.

CONSTANTS: gc_relat_002 TYPE hrp1001-relat VALUE '002',
           gc_relat_003 TYPE hrp1001-relat VALUE '003',
           gc_relat_007 TYPE hrp1001-relat VALUE '007',
           gc_relat_011 TYPE hrp1001-relat VALUE '011'.

CONSTANTS: gc_high_date TYPE hrp1000-endda VALUE '99991231'.

CLASS lcl_handle_events DEFINITION DEFERRED.

** ALV Containers and Events **
DATA: lo_grid      TYPE REF TO cl_gui_alv_grid,
      lo_events    TYPE REF TO lcl_handle_events,
      lo_container TYPE REF TO cl_gui_container.

INCLUDE   zhr_position_manager_chg_forms.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_reppos  FOR lv_position MODIF ID bl1.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.
***Position ID selection based on Input
  SELECT objid,
         stext FROM hrp1000
         INTO TABLE @DATA(lt_posdtls)
         WHERE plvar = '01'
         AND otype = 'S'
         AND objid IN @s_reppos
         AND begda LE @sy-datum
         AND endda GE @sy-datum.
  IF sy-subrc EQ 0.
    SORT lt_posdtls[] BY objid.
  ENDIF.

  REFRESH: gt_posdtls.
  LOOP AT lt_posdtls ASSIGNING FIELD-SYMBOL(<lfs_posdtls>).
    gs_posdtls-position = <lfs_posdtls>-objid. "Position Number
    gs_posdtls-posdesc  = <lfs_posdtls>-stext. "Position Description
*Selecting Position Pernr Number
    SELECT SINGLE sobid FROM hrp1001
      INTO @DATA(l_pos_pernr)
      WHERE otype = 'S'
      AND objid = @<lfs_posdtls>-objid
      AND plvar = '01'
      AND rsign = 'A'
      AND relat = '008'
      AND begda LE @sy-datum
      AND endda GE @sy-datum
      AND sclas = 'P'.
    IF sy-subrc EQ 0.
      gs_posdtls-posempno = l_pos_pernr. "position Pernr Number
*Position Employee Name
      SELECT SINGLE sname FROM pa0001
        INTO @gs_posdtls-posempname
        WHERE pernr = @l_pos_pernr
        AND begda LE @sy-datum
        AND endda GE @sy-datum.
*Position to existing Manager Position
      SELECT SINGLE sobid FROM hrp1001
        INTO @gs_posdtls-oldmgrpos
        WHERE otype = 'S'
        AND objid = @gs_posdtls-position
        AND plvar = '01'
        AND rsign = 'A'
        AND relat = '002'
        AND begda LE @sy-datum
        AND endda GE @sy-datum
        AND sclas = 'S'.
      IF sy-subrc = 0.
        SELECT SINGLE mc_stext FROM hrp1000
          INTO @gs_posdtls-oldmgrpdes
          WHERE objid = @gs_posdtls-oldmgrpos.
*Oldmgr Position Pernr Number
        SELECT SINGLE sobid FROM hrp1001
          INTO @DATA(l_oldmgr_pernr)
          WHERE otype = 'S'
          AND objid = @gs_posdtls-oldmgrpos
          AND plvar = '01'
          AND rsign = 'A'
          AND relat = '008'
          AND begda LE @sy-datum
          AND endda GE @sy-datum
          AND sclas = 'P'.
        IF sy-subrc = 0.
          gs_posdtls-oldrepmgr = l_oldmgr_pernr.
*Old Manager Name
          SELECT SINGLE sname FROM pa0001
            INTO @gs_posdtls-oldmgrname
            WHERE pernr = @l_oldmgr_pernr
            AND begda LE @sy-datum
            AND endda GE @sy-datum.
        ENDIF.
      ENDIF.
    ELSE.
      gs_posdtls-type = 'E'.
      gs_posdtls-msg = TEXT-002.
      APPEND gs_posdtls TO gt_posdtls.
      CONTINUE.
    ENDIF.
    APPEND gs_posdtls TO gt_posdtls.
    CLEAR: gs_posdtls.
  ENDLOOP.

END-OF-SELECTION.

  PERFORM alv_display.
