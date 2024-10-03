*&---------------------------------------------------------------------*
*& Include          ZSD_COPACHARACTERSTIC_UP_CLS
*&---------------------------------------------------------------------*
DATA: lv_pernr TYPE pernr_d.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_pernr FOR lv_pernr MATCHCODE OBJECT h_prem.
SELECTION-SCREEN END OF BLOCK b1.
* Class Definition*
CLASS lcl_copachar_updation DEFINITION.
  PUBLIC SECTION.
    METHODS: existing_pf_data,  get_existing_values,
      data_manipulation, data_updation, build_alv.
  PRIVATE SECTION.
    DATA: gt_knvp          TYPE STANDARD TABLE OF knvp WITH NON-UNIQUE SORTED KEY par COMPONENTS parvw,
          gt_charvalues    TYPE TABLE OF bapi1161_charact_values,
          gt_getvalues_rsm TYPE TABLE OF bapi1161_charact_values,
          gt_getvalues_asm TYPE TABLE OF bapi1161_charact_values,
          gt_getvalues_slo TYPE TABLE OF bapi1161_charact_values,
          gt_addvalues_rsm TYPE TABLE OF bapi1161_charact_values,
          gt_addvalues_asm TYPE TABLE OF bapi1161_charact_values,
          gt_addvalues_slo TYPE TABLE OF bapi1161_charact_values,
          gs_ret           TYPE bapiret2.
    DATA: lr_pfunc TYPE RANGE OF knvp-parvw.
    DATA: lv_characterstics TYPE rke_characteristic.
    DATA: lv_seqno_rsm TYPE bapi1161_charact_values-sequence_no,
          lv_seqno_asm TYPE bapi1161_charact_values-sequence_no,
          lv_seqno_slo TYPE bapi1161_charact_values-sequence_no.
ENDCLASS.

CLASS lcl_copachar_updation IMPLEMENTATION.
  METHOD existing_pf_data.
* Variavle table for partner Function filter
    SELECT low FROM tvarvc INTO TABLE @DATA(lt_tvarvc)
      WHERE name = 'COPA_CHARUPD_PF'
      AND type = 'S'.
    IF sy-subrc = 0.
      lr_pfunc = VALUE #(
                   FOR ls_tvarvc IN lt_tvarvc
                          ( sign = 'I'
                            option = 'EQ'
                            low = ls_tvarvc-low ) ).
    ELSE.
    ENDIF.
    IF lr_pfunc IS NOT INITIAL.
      REFRESH: gt_knvp.
      SELECT * FROM knvp
        INTO TABLE gt_knvp
        WHERE parvw IN lr_pfunc
        AND pernr IN s_pernr.
      IF sy-subrc = 0.
        SORT gt_knvp[] BY pernr.
        DELETE ADJACENT DUPLICATES FROM gt_knvp[] COMPARING pernr.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_existing_values.
    LOOP AT lr_pfunc ASSIGNING FIELD-SYMBOL(<fls_pfunc>).
      CLEAR: lv_characterstics.
      CASE <fls_pfunc>-low.
*Regional Sales Manager
        WHEN 'L2'.
          lv_characterstics = 'WWRSM'.
          REFRESH: gt_getvalues_rsm.
          CALL FUNCTION 'BAPI_COPACHARACT_GETVALUES'
            EXPORTING
              copacharacteristic    = lv_characterstics
            IMPORTING
              return                = gs_ret
            TABLES
              characteristic_values = gt_getvalues_rsm.
          SORT gt_getvalues_rsm[] BY sequence_no DESCENDING.
*Area Sales Manager
        WHEN 'L3'.
          lv_characterstics = 'WWASM'.
          REFRESH: gt_getvalues_asm.
          CALL FUNCTION 'BAPI_COPACHARACT_GETVALUES'
            EXPORTING
              copacharacteristic    = lv_characterstics
            IMPORTING
              return                = gs_ret
            TABLES
              characteristic_values = gt_getvalues_asm.
          SORT gt_getvalues_asm[] BY sequence_no DESCENDING.
*Sales Officer
        WHEN 'L5'.
          lv_characterstics = 'WWSOF'.
          REFRESH: gt_getvalues_slo.
          CALL FUNCTION 'BAPI_COPACHARACT_GETVALUES'
            EXPORTING
              copacharacteristic    = lv_characterstics
            IMPORTING
              return                = gs_ret
            TABLES
              characteristic_values = gt_getvalues_slo.
          SORT gt_getvalues_slo[] BY sequence_no DESCENDING.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD data_manipulation.
    "Data Filteration and Updation
    "Overall Regional Sales Manager in Partner Function Table
    DATA(lt_knvp_rsm) = FILTER #( gt_knvp USING KEY par WHERE parvw = 'L2' ).
    "Overall Area Sales Manager in Partner Function Table
    DATA(lt_knvp_asm) = FILTER #( gt_knvp USING KEY par WHERE parvw = 'L3' ).
    "Overall  Sales Officer in Partner Function Table
    DATA(lt_knvp_so)  = FILTER #( gt_knvp USING KEY par WHERE parvw = 'L5' ).

*Regional Sales Manager
    CLEAR: lv_seqno_rsm,gt_addvalues_rsm.
    lv_seqno_rsm = VALUE #( gt_getvalues_rsm[ 1 ]-sequence_no OPTIONAL ).
    LOOP AT lt_knvp_rsm ASSIGNING FIELD-SYMBOL(<fs_knvp_rsm>).
* Selecting Existing RSM From table
      SELECT SINGLE wwrsm FROM t2507
        INTO @DATA(l_rsm)
        WHERE wwrsm = @<fs_knvp_rsm>-pernr.
      IF sy-subrc NE 0.
        SELECT SINGLE sname FROM pa0001
          INTO @DATA(l_rsm_name)
          WHERE pernr = @<fs_knvp_rsm>-pernr
          AND begda LE @sy-datum
          AND endda GE @sy-datum.
        lv_seqno_rsm = lv_seqno_rsm + 1.
        APPEND VALUE #( value           = <fs_knvp_rsm>-pernr
                        name            = l_rsm_name
                        sequence_no     = lv_seqno_rsm
                        sequence_sub_no = '0001'
                        characteristic  = 'WWRSM' ) TO gt_addvalues_rsm.

      ENDIF.
    ENDLOOP.
*Area Sales Manager
    CLEAR: lv_seqno_asm,gt_addvalues_asm.
    lv_seqno_asm = VALUE #( gt_getvalues_asm[ 1 ]-sequence_no OPTIONAL ).
    LOOP AT lt_knvp_asm ASSIGNING FIELD-SYMBOL(<fs_knvp_asm>).
* Selecting Existing ASM From table
      SELECT SINGLE wwasm FROM t2503
        INTO @DATA(l_asm)
        WHERE wwasm = @<fs_knvp_asm>-pernr.
      IF sy-subrc NE 0.
        SELECT SINGLE sname FROM pa0001
          INTO @DATA(l_asm_name)
          WHERE pernr = @<fs_knvp_asm>-pernr
          AND begda LE @sy-datum
          AND endda GE @sy-datum.
        lv_seqno_asm = lv_seqno_asm + 1.
        APPEND VALUE #( value           = <fs_knvp_asm>-pernr
                        name            = l_asm_name
                        sequence_no     = lv_seqno_asm
                        sequence_sub_no = '0001'
                        characteristic  = 'WWASM' ) TO gt_addvalues_asm.
      ENDIF.
    ENDLOOP.
*Sales Officer
    CLEAR: lv_seqno_slo,gt_addvalues_slo.
    lv_seqno_slo = VALUE #( gt_getvalues_slo[ 1 ]-sequence_no OPTIONAL ).
    LOOP AT lt_knvp_so ASSIGNING FIELD-SYMBOL(<fs_knvp_so>).
* Selecting Existing RSM From table
      SELECT SINGLE wwsof FROM t2506
        INTO @DATA(l_sso)
        WHERE wwsof = @<fs_knvp_so>-pernr.
      IF sy-subrc NE 0.
        SELECT SINGLE sname FROM pa0001
          INTO @DATA(l_so_name)
          WHERE pernr = @<fs_knvp_so>-pernr
          AND begda LE @sy-datum
          AND endda GE @sy-datum.
        lv_seqno_slo = lv_seqno_slo + 1.
        APPEND VALUE #( value           = <fs_knvp_so>-pernr
                        name            = l_so_name
                        sequence_no     = lv_seqno_slo
                        sequence_sub_no = '0001'
                        characteristic  = 'WWSOF' ) TO gt_addvalues_slo.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD data_updation.
    IF gt_addvalues_rsm IS NOT INITIAL.
      CLEAR: gs_ret.
      CALL FUNCTION 'BAPI_COPACHARUDEF_ADDVALUES'
        EXPORTING
          copacharacteristic    = 'WWRSM'
        IMPORTING
          return                = gs_ret
        TABLES
          characteristic_values = gt_addvalues_rsm.
      IF gs_ret IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        APPEND LINES OF gt_addvalues_rsm[] TO gt_charvalues.
      ENDIF.
    ENDIF.
    IF gt_addvalues_asm IS NOT INITIAL.
      CLEAR: gs_ret.
      CALL FUNCTION 'BAPI_COPACHARUDEF_ADDVALUES'
        EXPORTING
          copacharacteristic    = 'WWASM'
        IMPORTING
          return                = gs_ret
        TABLES
          characteristic_values = gt_addvalues_asm.
      IF gs_ret IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        APPEND LINES OF gt_addvalues_asm[] TO gt_charvalues.
      ENDIF.
    ENDIF.
    IF gt_addvalues_slo IS NOT INITIAL.
      CLEAR: gs_ret.
      CALL FUNCTION 'BAPI_COPACHARUDEF_ADDVALUES'
        EXPORTING
          copacharacteristic    = 'WWSOF'
        IMPORTING
          return                = gs_ret
        TABLES
          characteristic_values = gt_addvalues_slo.
      IF gs_ret IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        APPEND LINES OF gt_addvalues_slo[] TO gt_charvalues.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD build_alv.
    DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_charvalues.
      CATCH cx_salv_msg.
    ENDTRY.
    lo_gr_alv->display( ).
  ENDMETHOD.

ENDCLASS.
