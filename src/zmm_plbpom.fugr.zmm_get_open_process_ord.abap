FUNCTION zmm_get_open_process_ord.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  MARC-WERKS OPTIONAL
*"     VALUE(ORDERNO) TYPE  AUFNR OPTIONAL
*"     VALUE(MATNR) TYPE  MATNR OPTIONAL
*"  TABLES
*"      IT_HEADER STRUCTURE  ZPP_ST_ORDER_DETAILS_INS
*"      IT_COMP STRUCTURE  ZPP_ST_ORDER_ITEM_INS
*"  EXCEPTIONS
*"      INVALID_PLANT
*"----------------------------------------------------------------------
*Requirement By: Purchase Team
*Description : Open Process Order with Instructions
**************************************************************************

*  DATA : lt_order_objects TYPE STANDARD TABLE OF bapi_pi_order_objects.
  DATA: l_plant          TYPE werks_d,
        lw_order_objects TYPE bapi_pi_order_objects,
        component        TYPE TABLE OF  bapi_order_component,
        header           TYPE TABLE OF bapi_order_header1,
        lw_header        TYPE zpp_st_order_details_ins,
        lw_comp          TYPE zpp_st_order_item_ins,
        lv_matnr         TYPE matnr.
  DATA oref TYPE REF TO cx_root.
  DATA lv_date TYPE sy-datum.

  IF plant IS INITIAL.
    l_plant = '1401'.
  ENDIF.

  SELECT SINGLE low FROM tvarvc INTO @DATA(l_low)
                    WHERE name = 'PROCESSORD_API_DATE'
                    AND   type = 'P'.
  IF sy-subrc = 0.
    lv_date = l_low.
  ELSE.
    EXIT.
  ENDIF.
  DATA: lr_orderno TYPE RANGE OF aufnr.
  DATA: lr_matnr TYPE RANGE OF matnr.
  IF orderno IS NOT INITIAL.
    APPEND  VALUE #( sign   = 'I' option = 'EQ' low = orderno ) TO lr_orderno.
  ENDIF.
  IF matnr IS NOT INITIAL.
    APPEND  VALUE #( sign   = 'I' option = 'EQ' low = matnr ) TO lr_matnr.
  ENDIF.
  SELECT a~aufnr,
         a~idat2,
         c~plnbez AS matnr,
         c~stlal
         FROM aufk AS a
         INNER JOIN afko AS c ON c~aufnr = a~aufnr
    INTO TABLE @DATA(lt_aufk)
    WHERE  a~aufnr IN @lr_orderno AND
           a~werks = @plant AND
           a~erdat GE @lv_date AND
           a~idat2 = '00000000' AND
           c~plnbez IN @lr_matnr.

  DATA lr_stlnr TYPE RANGE OF zsl_no.
  DATA l_stlnr TYPE text10.

  lw_order_objects-header              = 'X'.
  lw_order_objects-components          = 'X'.

  LOOP AT lt_aufk INTO DATA(lw_aufk).
    CLEAR: header[], component[].
    CALL FUNCTION 'BAPI_PROCORD_GET_DETAIL'
      EXPORTING
        number        = lw_aufk-aufnr
        order_objects = lw_order_objects
      TABLES
        header        = header
        component     = component.


    DELETE component WHERE req_quan = 0.
    IF component IS INITIAL.
      CONTINUE.
    ENDIF.
    LOOP AT header INTO DATA(lw_hd).
      MOVE-CORRESPONDING lw_hd TO lw_header.
      APPEND lw_header TO it_header.
    ENDLOOP.


    SELECT  sl_no,
            plant,
            matnr,
            matnr_alt,
            alternative,
            matnr_pro,
            matnr_pro1,
            matnr_pro2,
            matnr_pro3,
            alternative AS flag
        FROM
            zwatbase_table
        INTO TABLE @DATA(lt_waterbase)
        WHERE matnr = @lw_aufk-matnr AND
              alternative = @lw_aufk-stlal AND
              plant = @plant.
    LOOP AT lt_waterbase ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-flag = 'X'.
    ENDLOOP.


    LOOP AT component INTO DATA(lw_component).
      MOVE-CORRESPONDING lw_component TO lw_comp.
      SHIFT lw_aufk-matnr LEFT  DELETING LEADING  '0'.
      SHIFT lw_component-material LEFT  DELETING LEADING  '0'.
      READ TABLE lt_waterbase
          ASSIGNING <fs>
            WITH KEY matnr = lw_aufk-matnr
                     matnr_alt = lw_component-material
                     flag = 'X'.
      IF sy-subrc = 0.
        lw_comp-matnr_pro = <fs>-matnr_pro.
        lw_comp-matnr_pro1 = <fs>-matnr_pro1.
        lw_comp-matnr_pro2 = <fs>-matnr_pro2.
        lw_comp-matnr_pro3 = <fs>-matnr_pro3.
        <fs>-flag = ''.
      ENDIF.
      lw_comp-material = lw_component-material.
      APPEND lw_comp TO it_comp.
      CLEAR lw_comp.
    ENDLOOP.
    CLEAR lt_waterbase[].
  ENDLOOP.

ENDFUNCTION.
