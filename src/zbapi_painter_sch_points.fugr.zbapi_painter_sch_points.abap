FUNCTION zbapi_painter_sch_points.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(REGIO) TYPE  REGIO OPTIONAL
*"     VALUE(DATE_FRM) TYPE  DATUM OPTIONAL
*"     VALUE(DATE_TO) TYPE  DATUM OPTIONAL
*"  TABLES
*"      IT_PAINTER_POINT STRUCTURE  ZPAINTER_SCH_POINTS_DETAIL OPTIONAL
*"      IT_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------
  ""Changed by: Samsudeen M
* Referece by : RaguBalakrishnan
*  Changes: Added one field from Mara "LVORM"
*            Which Means Blocked Material filter
* Date: 25.08.2022
****************************************************************************
*** New changes for Date Parameters and code remedition ***
  "Changed on: 16.12.2022
  "Changed By: Samsudeen M
*----------------------------------------------------------------------------
*** New changes for Date Parameters and code remedition ***
  "Changed on: 26.12.2022
  "Changed By: Ramakrishnan
*----------------------------------------------------------------------------

  TYPES : BEGIN OF ty_zpainter_sch_log,
            bukrs         TYPE zpainter_sch_log-bukrs,
            regio         TYPE zpainter_sch_log-regio,
            matkl         TYPE zpainter_sch_log-matkl,
            zpoints       TYPE zpainter_sch_log-zpoints,
            ztotal_points TYPE zpainter_sch_log-ztotal_points,
            from_per      TYPE zpainter_sch_log-from_per,
            to_per        TYPE zpainter_sch_log-to_per,
            matnr         TYPE mara-matnr,
            maktx         TYPE makt-maktx,
            volum         TYPE mara-volum,
          END OF ty_zpainter_sch_log.

  DATA : it_zpainter_sch_log TYPE TABLE OF ty_zpainter_sch_log,
         wa_zpainter_sch_log TYPE ty_zpainter_sch_log.

  DATA:ztotal_points TYPE i.
  DATA: lv_meins_it TYPE mara-meins,
        lv_meins_ot TYPE mara-meins.

  TYPES : BEGIN OF ty_mara,
            matnr TYPE mara-matnr,
*            zeinr         TYPE mara-zeinr,
            volum TYPE mara-volum,
            lvorm TYPE mara-lvorm,
            matkl TYPE mara-matkl,
            meins TYPE mara-meins,
          END OF ty_mara.

  DATA: it_mara TYPE TABLE OF ty_mara,
        wa_mara TYPE ty_mara.

  TYPES: BEGIN OF ty_makt,
           matnr TYPE makt-matnr,
           maktx TYPE makt-maktx,
         END OF ty_makt.

  DATA: it_makt TYPE TABLE OF ty_makt,
        wa_makt TYPE ty_makt.


  IF bukrs IS NOT INITIAL.

    SELECT  bukrs
            regio
            matkl
            zpoints
            ztotal_points
            from_per
            to_per   INTO TABLE it_zpainter_sch_log
                     FROM zpainter_sch_log
                     WHERE bukrs EQ bukrs.

  ELSEIF regio IS NOT INITIAL.

    SELECT  bukrs
            regio
            matkl
            zpoints
            ztotal_points
            from_per
            to_per   INTO TABLE it_zpainter_sch_log
                     FROM zpainter_sch_log
                     WHERE regio EQ regio.
  ELSEIF date_frm IS NOT INITIAL AND date_to IS NOT INITIAL.

    SELECT bukrs
           regio
           matkl
           zpoints
           ztotal_points
           from_per
           to_per   INTO TABLE it_zpainter_sch_log
                    FROM zpainter_sch_log
                    WHERE from_per <= date_to AND to_per >= date_frm.

  ELSEIF date_frm IS NOT INITIAL AND date_to IS INITIAL.

    SELECT bukrs
           regio
           matkl
           zpoints
           ztotal_points
           from_per
           to_per  INTO TABLE it_zpainter_sch_log
                   FROM zpainter_sch_log
                   WHERE from_per = date_frm.
  ELSE.

    SELECT bukrs
           regio
           matkl
           zpoints
           ztotal_points
           from_per
           to_per   INTO TABLE it_zpainter_sch_log
                    FROM zpainter_sch_log
           WHERE from_per <= sy-datum
             AND to_per >= sy-datum.

  ENDIF.

  IF it_zpainter_sch_log IS NOT INITIAL.
    SELECT matnr
           volum
           lvorm
           matkl
           meins INTO TABLE it_mara
                 FROM mara
                 FOR ALL ENTRIES IN it_zpainter_sch_log
                 WHERE matkl EQ it_zpainter_sch_log-matkl
                 AND lvorm NE 'X'.
  ENDIF.

  IF it_mara IS NOT INITIAL.
    SELECT matnr maktx INTO TABLE it_makt FROM makt FOR ALL ENTRIES IN  it_mara WHERE matnr EQ it_mara-matnr
      AND spras = sy-langu.
  ENDIF.

  SORT it_zpainter_sch_log BY matkl.
  SORT it_mara BY matnr volum matkl.
  SORT it_makt BY matnr.

  CLEAR: lv_meins_it,lv_meins_ot.
  lv_meins_it = 'L'.
  lv_meins_ot = 'BOT'.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_meins_it
    IMPORTING
      output = lv_meins_it.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_meins_ot
    IMPORTING
      output = lv_meins_ot.

  REFRESH: it_painter_point.
  LOOP AT it_mara INTO wa_mara.
    DATA: lv_volum  TYPE ekpo-menge,
          lv_volum1 TYPE ekpo-menge.
    CLEAR: lv_volum,lv_volum1.
    lv_volum = wa_mara-volum.
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = wa_mara-matnr
        i_in_me              = lv_meins_it
        i_out_me             = lv_meins_ot
        i_menge              = lv_volum
      IMPORTING
        e_menge              = lv_volum1
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.
    DATA(lv_tot) = VALUE #( it_zpainter_sch_log[ matkl = wa_mara-matkl ]-zpoints OPTIONAL ) * wa_mara-volum.
    IF lv_volum1 NE 0.
      DATA(lv_tot_bt) = lv_tot / lv_volum1.
    ENDIF.
    APPEND VALUE #( matnr = wa_mara-matnr
                    maktx = VALUE #( it_makt[ matnr = wa_mara-matnr ]-maktx OPTIONAL )
                    bukrs = VALUE #( it_zpainter_sch_log[ matkl = wa_mara-matkl ]-bukrs OPTIONAL )
                    regio = VALUE #( it_zpainter_sch_log[ matkl = wa_mara-matkl ]-regio OPTIONAL )
                    zpoints = VALUE #( it_zpainter_sch_log[ matkl = wa_mara-matkl ]-zpoints OPTIONAL )
                    zeinr = wa_mara-matkl
                    volum = wa_mara-volum
                    ztotal_points =  lv_tot
                    ztotal_in_bt = lv_tot_bt
                    from_per = VALUE #( it_zpainter_sch_log[ matkl = wa_mara-matkl ]-from_per OPTIONAL )
                    to_per = VALUE #( it_zpainter_sch_log[ matkl = wa_mara-matkl ]-to_per OPTIONAL )
                   ) TO it_painter_point.
    CLEAR: lv_tot,lv_tot_bt.
  ENDLOOP.
ENDFUNCTION.
