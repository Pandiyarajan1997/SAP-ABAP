FUNCTION zbapi_mat_gst.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MATNR) TYPE  RANGE_T_MATNR OPTIONAL
*"     VALUE(WKREG) TYPE  WKREG OPTIONAL
*"     VALUE(REGIO) TYPE  REGIO OPTIONAL
*"  TABLES
*"      IT_OUTPUT STRUCTURE  ZBAPI_MAT_TAX
*"----------------------------------------------------------------------

  TYPES : BEGIN OF ty_where_clause,
            line TYPE char72,
          END OF ty_where_clause.

  TYPES:BEGIN OF ty_konp,
          knumh    TYPE konp-knumh,
          kbetr    TYPE konp-kbetr,
          loevm_ko TYPE   konp-loevm_ko,
        END OF ty_konp.

  TYPES: BEGIN OF ty_hsn,
           matnr TYPE matnr,
           steuc TYPE steuc,
         END OF ty_hsn.

  DATA: lt_hsn TYPE TABLE OF ty_hsn,
        ls_hsn TYPE ty_hsn.

  DATA: ls_output TYPE zbapi_mat_tax.

  DATA:lt_konp TYPE TABLE OF ty_konp,
       ls_konp TYPE ty_konp.

  DATA : lv_where TYPE string.
  DATA : ls_condtab TYPE hrcond.

  DATA : lt_condtab      TYPE STANDARD TABLE OF hrcond,
         lt_where_clause TYPE STANDARD TABLE OF ty_where_clause.

  DATA: Lt_a709 TYPE TABLE OF a709,
        Lt_a709_t TYPE TABLE OF a709,
        ls_a709 TYPE a709.

  DATA: lt_kschl TYPE farr_tt_cond_type_range.

  IF wkreg IS NOT INITIAL.
    ls_condtab-field = 'WKREG'.
    ls_condtab-opera = 'EQ'.
    ls_condtab-low   = wkreg.
    APPEND ls_condtab TO lt_condtab.
  ENDIF.

  IF regio IS NOT INITIAL.
    ls_condtab-field = 'REGIO'.
    ls_condtab-opera = 'EQ'.
    ls_condtab-low   = regio.
    APPEND ls_condtab TO lt_condtab.
  ENDIF.


  CALL FUNCTION 'RH_DYNAMIC_WHERE_BUILD'
    EXPORTING
      dbtable         = 'A709'
    TABLES
      condtab         = lt_condtab
      where_clause    = lt_where_clause
    EXCEPTIONS
      empty_condtab   = 1
      no_db_field     = 2
      unknown_db      = 3
      wrong_condition = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  IF lt_condtab[] IS INITIAL.
    SELECT *
      FROM a709
      INTO TABLE lt_a709
      WHERE kschl IN ('JOCG','JOSG','JOIG')
        AND taxk1 EQ '0'
        AND matnr IN matnr
        AND datab LE sy-datum
        AND datbi GE sy-datum.
  ELSE.
    SELECT *
      FROM a709
      INTO TABLE lt_a709
     WHERE kschl IN ('JOCG','JOSG','JOIG')
      AND taxk1 EQ '0'
      AND matnr IN matnr
      AND (lt_where_clause)
      AND datab LE sy-datum
      AND datbi GE sy-datum .
  ENDIF.


  IF lt_a709[] IS NOT INITIAL.

    SELECT knumh kbetr loevm_ko
      FROM konp
      INTO TABLE Lt_konp
      FOR ALL ENTRIES IN Lt_a709
      WHERE knumh EQ lt_a709-knumh
        AND kschl IN ('JOCG','JOSG','JOIG').

    IF sy-subrc = 0.
      SORT lt_konp ASCENDING BY knumh.

      refresh lt_a709_t.
      lt_a709_t[] = Lt_a709[].

      SORT lt_a709_t by matnr.
      delete ADJACENT DUPLICATES FROM lt_a709_t COMPARING matnr.

      SELECT matnr STEUC FROM marc INTO TABLE lt_hsn FOR ALL ENTRIES IN lt_a709_t WHERE matnr = lt_a709_t-matnr.
      IF sy-subrc = 0.
        SORT lt_hsn by matnr steuc.
        delete lt_hsn WHERE steuc is INITIAL.
        delete ADJACENT DUPLICATES FROM lt_hsn COMPARING matnr.
      ENDIF.

      LOOP AT lt_a709 INTO ls_a709.

        ls_output-kschl = ls_a709-kschl.
        ls_output-wkreg = ls_a709-wkreg.
        ls_output-regio = ls_a709-regio.
        ls_output-taxk1 = ls_a709-taxk1.
        ls_output-matnr = ls_a709-matnr.
        ls_output-kfrst = ls_a709-kfrst.
        ls_output-datbi = ls_a709-datbi.
        ls_output-datab = ls_a709-datab.
        ls_output-kbstat = ls_a709-kbstat.
        ls_output-knumh = ls_a709-knumh.

        READ TABLE lt_konp INTO ls_konp with key knumh = ls_a709-knumh BINARY SEARCH.
        IF sy-subrc = 0.
          IF ls_konp-loevm_ko = 'X'.
            CONTINUE.
          ENDIF.
          IF ls_konp-kbetr is NOT INITIAL.
            ls_output-kbetr = ls_konp-kbetr / 10.
          ELSE.
            CONTINUE.
          ENDIF.

        ENDIF.

        READ TABLE lt_hsn INTO ls_hsn with key matnr = ls_a709-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          ls_output-STEUC = ls_hsn-steuc.
        ENDIF.

        APPEND ls_output to it_output.
      ENDLOOP.
    ENDIF.
  ENDIF.


ENDFUNCTION.
