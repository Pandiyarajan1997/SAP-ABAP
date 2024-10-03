FUNCTION zmm_get_process_ord_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  MARC-WERKS
*"     VALUE(DATE_FROM) TYPE  SY-DATUM
*"     VALUE(DATE_TO) TYPE  SY-DATUM
*"     VALUE(ORDER_NO) TYPE  AUFNR OPTIONAL
*"  TABLES
*"      IT_HEADER STRUCTURE  ZPP_ST_ORDER_DETAILS
*"      IT_COMP STRUCTURE  ZPP_ST_ORDER_ITEM
*"  EXCEPTIONS
*"      INVALID_PLANT
*"      DIVIDE_BY_ZERO
*"----------------------------------------------------------------------
**************************************************************************
*Developed By : Ramakrishnan ( 705322 ).
*Reference By : Praveen
*Requirement By: Purchase Team
*Description : Check stock for all RM/PM used for Material and compare
*             available qty vs required quantity
**************************************************************************

*  DATA : lt_order_objects TYPE STANDARD TABLE OF bapi_pi_order_objects.
  DATA: lw_order_objects TYPE bapi_pi_order_objects,
        component        TYPE TABLE OF  bapi_order_component,
        header           TYPE TABLE OF bapi_order_header1,
        lw_header        TYPE zpp_st_order_details,
        lw_comp          TYPE zpp_st_order_item,
        lv_matnr         TYPE matnr.
  DATA oref TYPE REF TO cx_root.
  IF date_from IS INITIAL OR
     date_to IS INITIAL.
    EXIT .
  ENDIF.

  DATA l_auart TYPE aufart.
  DATA l_auart_sfg TYPE aufart.

  CASE plant.
    WHEN '1003'.
      l_auart = 'PK03'.
      l_auart_sfg = 'PR03'.
    WHEN '1401'.
      l_auart = 'PK41'.
      l_auart_sfg = 'PR40'.
    WHEN OTHERS.
      RAISE invalid_plant.
  ENDCASE.
  IF order_no IS NOT INITIAL.
    SELECT a~aufnr,
           a~idat2,
           b~charg
           FROM aufk AS a
           INNER JOIN afpo AS b ON a~aufnr = b~aufnr
      INTO TABLE @DATA(lt_aufk)
      WHERE a~werks = @plant AND
            a~aufnr = @order_no AND
            a~auart = @l_auart AND
            a~idat2 BETWEEN @date_from AND @date_to.
  ELSE.
    SELECT a~aufnr,
           a~idat2,
           b~charg
           FROM aufk AS a
           INNER JOIN afpo AS b ON a~aufnr = b~aufnr
      INTO TABLE  @lt_aufk
      WHERE  a~werks = @plant AND
             a~auart = @l_auart AND
             a~idat2 BETWEEN @date_from AND @date_to.
  ENDIF.

  CHECK lt_aufk IS NOT INITIAL.
  LOOP AT lt_aufk ASSIGNING FIELD-SYMBOL(<fs_aufk>).
    IF <fs_aufk>-charg(1) NE '0'.
      <fs_aufk>-charg = |0{ <fs_aufk>-charg }|.
    ENDIF.
  ENDLOOP.

  SELECT b~aufnr,
         b~idat2,
         a~charg
         FROM afpo AS a
         INNER JOIN aufk AS b ON a~aufnr = b~aufnr
    FOR ALL ENTRIES IN @lt_aufk
    WHERE  b~werks = @plant  AND
           b~auart = @l_auart_sfg AND
           a~charg = @lt_aufk-charg
    APPENDING TABLE   @lt_aufk.

  SORT lt_aufk BY aufnr.
  DELETE ADJACENT DUPLICATES FROM lt_aufk COMPARING aufnr.

  SELECT aufnr,
         matnr,
         charg,
         dmbtr,
         rsnum,
         rspos
   INTO TABLE @DATA(lt_mseg)
   FROM mseg
   FOR ALL ENTRIES IN @lt_aufk
   WHERE bwart = '261' AND
         werks = @plant AND
         aufnr = @lt_aufk-aufnr.


  SORT lt_mseg BY aufnr matnr.

  lw_order_objects-header              = 'X'.
*  lw_order_objects-positions           = 'X'.
*  lw_order_objects-sequences           = 'X'.
*  lw_order_objects-phases              = 'X'.
  lw_order_objects-components          = 'X'.
*  lw_order_objects-prod_rel_tools      = 'X'.
*  lw_order_objects-trigger_points      = 'X'.
*  lw_order_objects-secondary_resources = 'X'.

  LOOP AT lt_aufk INTO DATA(lw_aufk).
    CLEAR: header[], component[].
    CALL FUNCTION 'BAPI_PROCORD_GET_DETAIL'  "#EC CI_USAGE_OK[2438131] added by SPLABAP during code remedation
      EXPORTING
        number        = lw_aufk-aufnr
        order_objects = lw_order_objects
      TABLES
        header        = header
        component     = component.


    LOOP AT header INTO DATA(lw_hd).
      MOVE-CORRESPONDING lw_hd TO lw_header.
      lw_header-tech_comp_date = lw_aufk-idat2.
      APPEND lw_header TO it_header.
    ENDLOOP.
    DELETE component WHERE batch IS INITIAL.
    IF component IS INITIAL.
      CONTINUE.
    ENDIF.
    TYPES: BEGIN OF ty_comp,
             material TYPE matnr,
             batch    TYPE charg_d,
           END OF ty_comp.
    DATA lt_comp_t TYPE STANDARD TABLE OF ty_comp.
    DATA lw_comp_t TYPE ty_comp.
    LOOP AT component INTO DATA(lw_component).
      MOVE-CORRESPONDING lw_component TO lw_comp_t.
      APPEND lw_comp_t TO lt_comp_t.
    ENDLOOP.
    SELECT matnr,
           charg,
           menge,
           meins,
           dmbtr
*           rsnum,
*           rspos
     INTO TABLE @DATA(lt_mseg_101)
     FROM mseg
     FOR ALL ENTRIES IN @lt_comp_t
     WHERE bwart = '101' AND
           werks = @plant AND
           matnr = @lt_comp_t-material AND
           charg = @lt_comp_t-batch.

    LOOP AT component INTO lw_component.
      MOVE-CORRESPONDING lw_component TO lw_comp.


      lw_comp-base_uom_price = VALUE #( lt_mseg[ aufnr = lw_aufk-aufnr
                                        matnr = lw_component-material
                                        charg = lw_component-batch
                                        rsnum = lw_component-reservation_number
                                        rspos = lw_component-reservation_item ]-dmbtr OPTIONAL ).
      IF lw_comp-req_quan > 0.
        lw_comp-base_perunit_price = lw_comp-base_uom_price / lw_comp-req_quan.
      ELSE.
        lw_comp-base_perunit_price = 0.
      ENDIF.

      IF lw_comp-base_uom = lw_comp-entry_uom.
        lw_comp-comp_uom_price = lw_comp-base_uom_price.
      ELSE.
        lv_matnr = lw_component-material.
        SELECT SINGLE
                umrez,
                umren FROM marm
                INTO @DATA(lw_marm)
                WHERE matnr = @lv_matnr AND
                      meinh = @lw_component-entry_uom.
        IF sy-subrc = 0.
*          lw_comp-comp_uom_price = ( lw_comp-base_uom_price / lw_comp-withdrawn_quantity ) * lw_comp-entry_quantity.
          lw_comp-comp_uom_price = ( lw_marm-umrez / lw_marm-umren ) * lw_comp-base_uom_price.
        ENDIF.
      ENDIF.
      IF lw_comp-base_perunit_price > 0.
        lw_comp-comp_perunit_price = lw_comp-comp_uom_price / lw_comp-entry_quantity.
      ELSE.
        lw_comp-comp_perunit_price = 0.
      ENDIF.
      TRY.
          lw_comp-gr_base_price = ( VALUE #( lt_mseg_101[ matnr = lw_component-material
                                                          charg = lw_component-batch ]-dmbtr OPTIONAL ) /
                                    VALUE #( lt_mseg_101[ matnr = lw_component-material
                                                          charg = lw_component-batch ]-menge OPTIONAL ) ) * lw_comp-req_quan.
        CATCH cx_sy_zerodivide INTO oref.
          RAISE divide_by_zero.
        CLEANUP.
      ENDTRY.
      IF lw_comp-req_quan > 0.
        lw_comp-gr_base_perunit = lw_comp-gr_base_price / lw_comp-req_quan.
      ELSE.
        lw_comp-gr_base_perunit = 0.
      ENDIF.
      IF lw_comp-base_uom = lw_comp-entry_uom.
       lw_comp-gr_comp_perunit  = lw_comp-gr_base_perunit.
      ELSE.
        lw_comp-gr_comp_perunit = ( lw_marm-umrez / lw_marm-umren )  * lw_comp-gr_base_perunit .
      ENDIF.
      IF lw_comp-entry_quantity > 0.
        lw_comp-gr_comp_price  = lw_comp-gr_comp_perunit * lw_comp-entry_quantity.
      ELSE.
        lw_comp-gr_comp_price  = 0.
      ENDIF.
      APPEND lw_comp TO it_comp.
      CLEAR lw_comp.
    ENDLOOP.
  ENDLOOP.
  SORT it_comp BY order_number material batch.

ENDFUNCTION.
