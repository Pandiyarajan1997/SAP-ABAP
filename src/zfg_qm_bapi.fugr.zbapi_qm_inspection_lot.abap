FUNCTION zbapi_qm_inspection_lot.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FROM_DATE) TYPE  DATUM OPTIONAL
*"     VALUE(TO_DATE) TYPE  DATUM OPTIONAL
*"  TABLES
*"      LT_INSPECTION STRUCTURE  ZSTR_QM_INS_LOT
*"----------------------------------------------------------------------
*&&&& Created_on : 23.06.2022
*     Created_by : Samsudeen M
*     Reference  : praveen kumar (SD)
*     Description : Inspection lot Detaiils From QALS Table
*     TR NO:  DEVK931814

**-----------------------------------------------------
  "Changed_on: 14.07.2022
  "Changed_by: Samsudeen M
  "TR_NO: DEVK931864
  "Description: Added three fields unit 0f measure, time_taken,Days_taken.
*---------------------------------------------------------------------------
  TYPES: BEGIN OF ty_qals,  "Structure For QALS table
           werk       TYPE werks_d,
           objnr      TYPE j_objnr,
           charg      TYPE charg_d,
           matnr      TYPE matnr,
           enstehdat  TYPE qentst,
           entstezeit TYPE qentstzeit,
           bwart      TYPE bwart,
           mblnr      TYPE mblnr,
           ktextmat   TYPE qktextobj,
           losmenge   TYPE qlosmenge,
           stat34     TYPE qstat34,
           stat35     TYPE qstat35,
         END OF ty_qals.

  TYPES: BEGIN OF ty_mseg,
           matnr      TYPE matnr,
           werks      TYPE werks_d,
           charg      TYPE charg_d,
           bwart      TYPE bwart,
           xauto      TYPE mb_xauto,
           shkzg      TYPE shkzg,
           cpudt_mkpf TYPE cpudt,
           cputm_mkpf TYPE cputm,
         END OF ty_mseg.

  TYPES: BEGIN OF ty_mara,    "Structure for MARA table
           matnr TYPE matnr,
           mtart TYPE mtart,
           meins TYPE meins,
         END OF ty_mara.

*******Internal table and Work Area Declaration  ***************
  DATA: gt_inspection TYPE TABLE OF ty_qals,
        gs_inspection TYPE ty_qals.

  DATA: gt_mara TYPE TABLE OF ty_mara,
        gs_mara TYPE ty_mara.

  DATA: gt_mseg  TYPE TABLE OF ty_mseg,
        gs_mseg  TYPE ty_mseg,
        gt_mseg1 TYPE TABLE OF ty_mseg,
        gs_mseg1 TYPE ty_mseg,
        gt_mseg2 TYPE TABLE OF ty_mseg,
        gs_mseg2 TYPE ty_mseg.

***********Work Area Declaration  For Final Internal table***************
  DATA: ls_inspection TYPE  zstr_qm_ins_lot.

  DATA: line      TYPE bsvx-sttxt,
        user_line TYPE bsvx-sttxt.

  DATA: gt_qm_stat TYPE STANDARD TABLE OF zqm_status,
        gs_qm_stat TYPE zqm_status.

  DATA: it_inspection TYPE STANDARD TABLE OF zstr_qm_ins_lot.
  REFRESH:      gt_inspection,gt_mara,lt_inspection,gt_qm_stat.
  CLEAR: gs_inspection,gs_mara,gs_qm_stat.

  IF ( from_date IS NOT INITIAL
     AND to_date IS NOT INITIAL ).
********Fetching Data from QALS table  ************
    SELECT werk
           objnr
           charg
           matnr
           enstehdat
           entstezeit
           bwart
           mblnr
           ktextmat
           losmenge
           stat34
           stat35 FROM qals
           INTO TABLE gt_inspection
           WHERE werk IN ( '1005' , '1401' )
           AND bwart EQ '101'
           AND ( enstehdat BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0.
      SORT gt_inspection[] BY werk matnr.
    ENDIF.

  ELSEIF ( from_date IS INITIAL
     AND to_date IS INITIAL ).
*********Fetching Data from QALS without input ***********
    SELECT werk
           objnr
           charg
           matnr
           enstehdat
           entstezeit
           bwart
           mblnr
           ktextmat
           losmenge
           stat34
           stat35 FROM qals
           INTO TABLE gt_inspection
           WHERE  werk IN ( '1005' , '1401' )
           AND bwart EQ '101'.
    IF sy-subrc EQ 0.
      SORT gt_inspection[] BY werk matnr.
    ENDIF.

  ENDIF.

  IF gt_inspection[] IS NOT INITIAL.
******* Fetching  Material type from MARA ************
    SELECT matnr
           mtart
           meins  FROM mara
                 INTO TABLE gt_mara
                 FOR ALL ENTRIES IN gt_inspection
                 WHERE matnr = gt_inspection-matnr.
    IF sy-subrc EQ 0.
      SORT gt_mara[] BY  matnr.
    ENDIF.

**********Fetching time and date from MSEG for Movement type 321 122 **********
    SELECT matnr
           werks
           charg
           bwart
           xauto
           shkzg
           cpudt_mkpf
           cputm_mkpf FROM mseg
                      INTO TABLE gt_mseg
                      FOR ALL ENTRIES IN gt_inspection
                      WHERE matnr = gt_inspection-matnr
                      AND werks = gt_inspection-werk
                      AND charg = gt_inspection-charg
                      AND bwart IN ( '122' , '321' )
                      AND xauto NE 'X'
                      AND ( cpudt_mkpf BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0.
      SORT gt_mseg[] BY matnr.
      REFRESH: gt_mseg1[],gt_mseg2[].
      gt_mseg1[] = gt_mseg[].
      SORT gt_mseg1[] BY matnr.
      DELETE gt_mseg WHERE bwart EQ '321'.
      DELETE gt_mseg1 WHERE  bwart EQ '122'.
    ENDIF.
**** Fetching status based on QA32 status from table(ZQM_STATUS) ************
    SELECT * FROM zqm_status
             INTO TABLE gt_qm_stat.

  ENDIF.

  DATA: lv_date TYPE p0347-scrdd.
  DATA: lv_days TYPE char20.
  DATA: lv_date1(06) TYPE c.
  DATA: lv_difference TYPE tims,
        lv_diff1(08)  TYPE c.
  DATA: lv_diff      TYPE i,
        lv_min(03)   TYPE c,
        lv_hours(02) TYPE c.

  LOOP AT gt_inspection  INTO gs_inspection.
    CLEAR ls_inspection.
    ls_inspection-matnr = gs_inspection-matnr.
    ls_inspection-material_desc = gs_inspection-ktextmat.
    ls_inspection-plant = gs_inspection-werk.
    ls_inspection-batch_no = gs_inspection-charg.
    ls_inspection-usage_decision = gs_inspection-stat35.
    ls_inspection-stock_post_com = gs_inspection-stat34.
    ls_inspection-lot_created = gs_inspection-enstehdat.
    ls_inspection-quantity = gs_inspection-losmenge.

    READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_inspection-matnr.
    IF sy-subrc EQ 0.
      ls_inspection-material_type = gs_mara-mtart.
      ls_inspection-unit_measure = gs_mara-meins.
    ENDIF.

    CLEAR: line,user_line.
************* Function Module for status Text ********************************
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        objnr            = gs_inspection-objnr
        spras            = sy-langu
      IMPORTING
        line             = line
        user_line        = user_line
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc EQ 0.
      ls_inspection-qm_status = line.
    ENDIF.

    READ TABLE gt_qm_stat INTO gs_qm_stat WITH KEY qm_status = ls_inspection-qm_status.
    IF sy-subrc EQ 0.
      ls_inspection-status = gs_qm_stat-status.
    ENDIF.

    READ TABLE gt_mseg1 INTO gs_mseg1 WITH KEY matnr = gs_inspection-matnr       "For Movement Type '321'
                                               werks = gs_inspection-werk
                                               charg = gs_inspection-charg.
    IF sy-subrc EQ 0.
      IF gs_mseg1-cpudt_mkpf EQ gs_inspection-enstehdat.
        ls_inspection-days_taken = '0 Days'.
      ELSE.
        CLEAR lv_date.
        CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
          EXPORTING
            date1                       = gs_mseg1-cpudt_mkpf
            date2                       = gs_inspection-enstehdat
            output_format               = '03'
          IMPORTING
            days                        = lv_date
          EXCEPTIONS
            overflow_long_years_between = 1
            invalid_dates_specified     = 2
            OTHERS                      = 3.
        IF sy-subrc <> 0.

        ENDIF.

        CLEAR: lv_days,lv_date1.
        WRITE lv_date TO lv_date1.
        CONCATENATE lv_date1 'Days' INTO lv_days SEPARATED BY space.
        CONDENSE lv_days.
        ls_inspection-days_taken = lv_days.
      ENDIF.

      CLEAR lv_difference.

      lv_difference = gs_mseg1-cputm_mkpf - gs_inspection-entstezeit .
      CLEAR lv_diff1.
      WRITE lv_difference TO lv_diff1.

      SPLIT lv_diff1 AT ':' INTO DATA(str1) DATA(str2) DATA(str3).
      CONCATENATE str1 'Hours' str2 'Minutes' str3 'Seconds' INTO ls_inspection-time_taken SEPARATED BY space.
      CONDENSE ls_inspection-time_taken.
    ENDIF.

    READ TABLE gt_mseg INTO gs_mseg WITH KEY matnr = gs_inspection-matnr    " For Movement Type '122'
                                             werks = gs_inspection-werk
                                             charg = gs_inspection-charg.
    IF sy-subrc EQ 0.
      IF gs_mseg1-cpudt_mkpf EQ gs_inspection-enstehdat.
        ls_inspection-days_taken = '0 Days'.
      ELSE.
        CLEAR lv_date.
        CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
          EXPORTING
            date1                       = gs_mseg-cpudt_mkpf
            date2                       = gs_inspection-enstehdat
            output_format               = '03'
          IMPORTING
            days                        = lv_date
          EXCEPTIONS
            overflow_long_years_between = 1
            invalid_dates_specified     = 2
            OTHERS                      = 3.

        CLEAR: lv_days,lv_date1.
        WRITE lv_date TO lv_date1.
        CONCATENATE lv_date1 'Days' INTO lv_days SEPARATED BY space.
        CONDENSE lv_days.
        ls_inspection-days_taken = lv_days.
      ENDIF.

      CLEAR lv_difference.

      lv_difference = gs_mseg1-cputm_mkpf - gs_inspection-entstezeit .
      CLEAR lv_diff1.
      WRITE lv_difference TO lv_diff1.

      SPLIT lv_diff1 AT ':' INTO DATA(str4) DATA(str5) DATA(str6).
      CONCATENATE str4 'Hours' str5 'Minutes' str6 'Seconds' INTO ls_inspection-time_taken SEPARATED BY space.
      CONDENSE ls_inspection-time_taken.
    ENDIF.

    APPEND ls_inspection TO lt_inspection.
  ENDLOOP.

ENDFUNCTION.
