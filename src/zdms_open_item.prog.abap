*&---------------------------------------------------------------------*
*& Report ZDMS_OPEN_ITEM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdms_open_item.

TYPES : BEGIN OF ty_final,
          bukrs       TYPE  bukrs,
          distributor TYPE  zdist,
          dist_name   TYPE  name1_gp,
          dist_plant  TYPE  werks_d,
          dealer      TYPE  kunnr,
          dealer_name TYPE  name1_gp,
          belnr       TYPE  belnr_d,
          blart       TYPE  blart,
          budat       TYPE  budat,
          bwwrt       TYPE  bwwrt,
          payable     TYPE  bwwrt,
          hwaer       TYPE  hwaer,
          gjahr       TYPE  gjahr,
          sgtxt       TYPE  sgtxt,
          vbeln       TYPE  vbeln_vf,
          xblnr       TYPE  xblnr1,
          faedt       TYPE  faedt_fpos,
          credit_bal  TYPE  bapidmbtr,
        END OF ty_final.

DATA : gv_customer TYPE kunnr,
       lt_dist     TYPE TABLE OF kunnr_sty,
       lt_cust     TYPE TABLE OF kunnr_sty,
       lt_open     TYPE TABLE OF zstr_open_items_bsid_dms,
       lt_open2    TYPE TABLE OF zstr_open_items_dms,
       lt_crbal    TYPE TABLE OF zstr_crbalance_dms,
       ls_final    TYPE ty_final,
       lt_final    TYPE TABLE OF ty_final.

SELECT-OPTIONS : so_dist FOR gv_customer,
                 so_cust FOR gv_customer.

PARAMETERS : r1 RADIOBUTTON GROUP g1 DEFAULT 'X',
             r2 RADIOBUTTON GROUP g1,
             r3 RADIOBUTTON GROUP g1.

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE TEXT-006.
  PARAMETERS p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK a2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_layouts USING cl_salv_layout=>restrict_none CHANGING p_layout.

START-OF-SELECTION.

  IF so_dist IS NOT INITIAL.
    LOOP AT so_dist.
      APPEND VALUE #( kunnr = so_dist-low ) TO lt_dist.
    ENDLOOP.
  ENDIF.

  IF so_cust IS NOT INITIAL.
    LOOP AT so_cust.
      APPEND VALUE #( kunnr = so_cust-low ) TO lt_cust.
    ENDLOOP.
  ENDIF.

  IF r1 = 'X'.
    CALL FUNCTION 'ZBAPI_CUSTOMER_OPEN_ITEM_DMS'
      EXPORTING
        lt_distributor = lt_dist
      TABLES
        lt_open_items  = lt_open2
        lt_customer    = lt_cust
        lt_crbalance   = lt_crbal.

  ELSEIF r2 = 'X'.
    CALL FUNCTION 'ZBAPI_CUST_OPEN_ITEM_BSID_DMS'
      EXPORTING
        lt_distributor = lt_dist
      TABLES
        lt_open_items  = lt_open
        lt_customer    = lt_cust
        lt_crbalance   = lt_crbal.

  ELSEIF r3 = 'X'.
    CALL FUNCTION 'ZBAPI_CUST_OPEN_ITEM_DMS_NSD'
      EXPORTING
        lt_distributor = lt_dist
      TABLES
        lt_open_items  = lt_open
        lt_customer    = lt_cust
        lt_crbalance   = lt_crbal
        .
  ENDIF.


  SORT : lt_crbal BY distributor dealer.
  SELECT kunnr,name1 FROM kna1 INTO TABLE @DATA(lt_kna1).

  SORT : lt_kna1 BY kunnr.

  LOOP AT lt_crbal ASSIGNING FIELD-SYMBOL(<fs_crbal>).
    READ TABLE lt_kna1 INTO DATA(ls_dist) WITH KEY kunnr = <fs_crbal>-distributor BINARY SEARCH.
    READ TABLE lt_kna1 INTO DATA(ls_dealer) WITH KEY kunnr = <fs_crbal>-dealer BINARY SEARCH.
    ls_final-distributor = <fs_crbal>-distributor.
    ls_final-dealer      = <fs_crbal>-dealer.
    ls_final-credit_bal  = <fs_crbal>-credit_bal.
    ls_final-dist_name   = ls_dist-name1.
    ls_final-dealer_name = ls_dealer-name1.
    APPEND ls_final TO lt_final.
    CLEAR : ls_final,ls_dist,ls_dealer.
  ENDLOOP.
**********************for test bapi*************
  LOOP AT lt_open ASSIGNING FIELD-SYMBOL(<fs_open>).
***********************append the final tables****************
    APPEND VALUE #( bukrs         = <fs_open>-bukrs
                    distributor   = <fs_open>-distributor
                    dist_name     = <fs_open>-dist_name
                    dist_plant    = <fs_open>-dist_plant
                    dealer        = <fs_open>-dealer
                    dealer_name   = <fs_open>-dealer_name
                    belnr         = <fs_open>-belnr
                    blart         = <fs_open>-blart
                    budat         = <fs_open>-budat
                    bwwrt         = <fs_open>-bwwrt
                    payable       = <fs_open>-payable
                    hwaer         = <fs_open>-hwaer
                    gjahr         = <fs_open>-gjahr
                    sgtxt         = <fs_open>-sgtxt
                    vbeln         = <fs_open>-vbeln
                    xblnr         = <fs_open>-xblnr
                    faedt         = <fs_open>-faedt ) TO lt_final.

  ENDLOOP.
**********************for original bapi*************
  LOOP AT lt_open2 ASSIGNING FIELD-SYMBOL(<fs_open2>).
***********************append the final tables****************
    APPEND VALUE #( bukrs         = <fs_open2>-bukrs
                    distributor   = <fs_open2>-distributor
                    dist_name     = <fs_open2>-dist_name
                    dist_plant    = <fs_open2>-dist_plant
                    dealer        = <fs_open2>-dealer
                    dealer_name   = <fs_open2>-dealer_name
                    belnr         = <fs_open2>-belnr
                    blart         = <fs_open2>-blart
                    budat         = <fs_open2>-budat
                    bwwrt         = <fs_open2>-bwwrt
                    payable       = <fs_open2>-payable
                    hwaer         = <fs_open2>-hwaer
                    gjahr         = <fs_open2>-gjahr
                    sgtxt         = <fs_open2>-sgtxt
                    vbeln         = <fs_open2>-vbeln
                    xblnr         = <fs_open2>-xblnr
                    faedt         = <fs_open2>-faedt ) TO lt_final.

  ENDLOOP.

  IF lt_final IS NOT INITIAL.
    SORT : lt_final BY distributor dealer budat belnr.
    CALL FUNCTION 'Z_POPUP_ALV'
      EXPORTING
        i_repid  = sy-repid
        i_layout = p_layout
      TABLES
        it_alv   = lt_final.
  ENDIF.



FORM f4_layouts USING i_restrict TYPE salv_de_layout_restriction
CHANGING c_layout TYPE disvariant-variant.

  DATA: ls_layout TYPE salv_s_layout_info,
        ls_key    TYPE salv_s_layout_key.

  ls_key-report = sy-repid.

  ls_layout = cl_salv_layout_service=>f4_layouts(
  s_key    = ls_key
  restrict = i_restrict ).

  c_layout = ls_layout-layout.

ENDFORM.                    " F4_LAYOUTS
