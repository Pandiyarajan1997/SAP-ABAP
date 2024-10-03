FUNCTION zbapi_customer_open_item.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUKRS) LIKE  BSID-BUKRS
*"     VALUE(KUNNR) LIKE  BSID-KUNNR OPTIONAL
*"  TABLES
*"      LT_OPEN_ITEMS STRUCTURE  ZSTR_OPEN_ITEMS
*"  EXCEPTIONS
*"      NO_OPEN_ITEMS
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
*" Created by: Samsudeen M
  " Created_on: 15.07.2022
  " Reference By : Suresh B V
  "                Praveen Kumar
  "                Sathish
  " Description: Customer Open Items From FBL5N
*------------------------------------------------------------------------

  TYPES : BEGIN OF ty_bsid, " Structure For BSID
            bukrs TYPE bukrs,
            kunnr TYPE kunnr,
          END OF ty_bsid.

  TYPES: BEGIN OF ty_kna1,  " Structure for KNA1
           kunnr TYPE kunnr,
           name1 TYPE name1_gp,
         END OF ty_kna1.

  DATA: gt_kna1 TYPE TABLE OF ty_kna1,
        gs_kna1 TYPE ty_kna1.

  DATA: gt_bsid TYPE TABLE OF ty_bsid,
        gs_bsid TYPE ty_bsid.

  DATA: gt_open_items  TYPE STANDARD TABLE OF zstr_open_items,
        gt_open_items1 TYPE STANDARD TABLE OF zstr_open_items,
        gs_open_items  TYPE zstr_open_items.                       " Structure for final output

  DATA: lt_rfpos TYPE STANDARD TABLE OF rfpos,
        ls_rfpos TYPE rfpos.

  REFRESH: gt_bsid,gt_kna1.

  IF bukrs IS NOT INITIAL AND kunnr IS INITIAL.
*********Fetching Customer Based on Company Code **********
    SELECT bukrs
           kunnr FROM bsid
                 INTO TABLE gt_bsid
                 WHERE bukrs EQ bukrs.

  ELSEIF bukrs IS NOT INITIAL AND kunnr IS NOT INITIAL.
*********Fetching Customer Based on Company Code and kunnr **********
    SELECT bukrs
           kunnr FROM bsid
                 INTO TABLE gt_bsid
                 WHERE bukrs EQ bukrs
                 AND kunnr EQ kunnr.

  ENDIF.

  IF gt_bsid[] IS NOT INITIAL.

    SORT gt_bsid[] BY bukrs kunnr.
    DELETE ADJACENT DUPLICATES FROM gt_bsid[] COMPARING kunnr.

*********Fetching Name from customer master *************************
    SELECT kunnr
           name1 FROM kna1
                 INTO TABLE gt_kna1
                 FOR ALL ENTRIES IN gt_bsid
                 WHERE kunnr EQ gt_bsid-kunnr.

    IF sy-subrc EQ 0.
      SORT gt_kna1[] BY kunnr.
    ENDIF.

  ENDIF.

  CLEAR: gs_bsid.

  LOOP AT gt_bsid INTO gs_bsid.

    REFRESH: lt_rfpos, gt_open_items.
**********Function Module For Customer open Items ************

    CALL FUNCTION 'CUSTOMER_OPEN_ITEMS'
      EXPORTING
        bukrs    = gs_bsid-bukrs
        kunnr    = gs_bsid-kunnr
      TABLES
        t_postab = lt_rfpos.

    IF sy-subrc <> 0.
      RAISE no_open_items.
    ENDIF.


    CLEAR ls_rfpos.
    LOOP AT lt_rfpos INTO ls_rfpos.

      CLEAR: gs_open_items.
      MOVE-CORRESPONDING ls_rfpos TO gs_open_items.

      CLEAR gs_kna1.
      READ TABLE gt_kna1 INTO gs_kna1 WITH KEY kunnr = gs_open_items-kunnr. "Customer name

      IF sy-subrc EQ 0.
        gs_open_items-name1 = gs_kna1-name1.
      ENDIF.

      APPEND gs_open_items TO gt_open_items.

    ENDLOOP.

    APPEND LINES OF gt_open_items TO lt_open_items.

  ENDLOOP.

*  DELETE ADJACENT DUPLICATES FROM lt_open_items[] COMPARING kunnr belnr.
  SORT lt_open_items[] BY bldat.
ENDFUNCTION.
