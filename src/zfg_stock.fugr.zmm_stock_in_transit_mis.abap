FUNCTION zmm_stock_in_transit_mis.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D
*"     VALUE(MATNR) TYPE  MATNR OPTIONAL
*"  TABLES
*"      E_STOCK_INTRANS TYPE  ZMM_STOCK_INTRANS_TT
*"----------------------------------------------------------------------

  DATA lw_stock_intrans TYPE zmm_stock_intrans_st.

* Interface structure for function module
  DATA: BEGIN OF xtab6 OCCURS 0,
          werks LIKE ekpo-werks,
          matnr LIKE ekpo-matnr,
          menge LIKE ekbe-menge,
          meins LIKE ekpo-meins,
          dmbtr LIKE ekbe-dmbtr,
          waers LIKE ekbe-waers,
          netwr LIKE ekpo-netwr,
          bwaer LIKE ekko-waers,
          ebeln LIKE ekbe-ebeln,
          ebelp LIKE ekbe-ebelp,
          sobkz LIKE mdbs-sobkz,
          pstyp LIKE mdbs-pstyp,
          bstmg LIKE ekbe-menge,
          bstme LIKE ekpo-meins,
          reswk LIKE ekko-reswk,
          bsakz LIKE ekko-bsakz,                             "sdp462
          lgort LIKE ekpo-lgort,                             "DFPS
          reslo LIKE ekpo-reslo,                             "DFPS
        END OF xtab6.
  DATA: BEGIN OF bestand OCCURS 0.
          INCLUDE STRUCTURE xtab6.
  DATA:   maktx   LIKE makt-maktx,
          name1   LIKE t001w-name1,
          color   TYPE slis_t_specialcol_alv,
          longnum TYPE /sappspro/longnum,
          kasit   TYPE ekbe-menge,                                    "SIT
          kasitb  TYPE ekbe-menge.                                   "SIT
* add CW-fields                                        CWM SO7K113663 AP
  DATA: /cwm/menge TYPE /cwm/menge,                    " CWM SO7K113663 AP
        /cwm/meins TYPE /cwm/meins.                  " CWM SO7K113663 AP
  DATA: /cwm/kasit TYPE /cwm/menge.
*ENHANCEMENT-POINT rm07mtrb_01 SPOTS es_rm07mtrb STATIC.
  DATA: END OF bestand.
  DATA lt_seltab TYPE TABLE OF rsparams.

  REFRESH lt_seltab.
  lt_seltab = VALUE #(
                     ( selname = 'WERKS'    kind = 'S' sign = 'I' option = 'EQ'  low = plant )
                     ( selname = 'XSPER'    kind = 'P' sign = 'I' option = 'EQ'  low = 'X' )
                     ( selname = 'XLOEK'    kind = 'P' sign = 'I' option = 'EQ'  low = 'X' )
                     ( selname = 'XELIK'    kind = 'P' sign = 'I' option = 'EQ'  low = 'X' )
                     ( selname = 'XKSIT'    kind = 'P' sign = 'I' option = 'EQ'  low = 'X' )
                     ( selname = 'XNLCC'    kind = 'P' sign = 'I' option = 'EQ'  low = 'X' )
                     ( selname = 'XKSIT'    kind = 'P' sign = 'I' option = 'EQ'  low = 'X' )
                       ).
  IF matnr IS NOT INITIAL.
    APPEND VALUE #( selname = 'MATNR'    kind = 'S' sign = 'I' option = 'EQ'  low = matnr ) TO lt_seltab.
  ENDIF.
  DATA fm_call TYPE c VALUE 'X'.
  EXPORT fm_call TO MEMORY ID 'ZMM_STOCK'.
  SUBMIT rm07mtrb WITH SELECTION-TABLE lt_seltab
         AND RETURN.
  IMPORT bestand FROM MEMORY ID 'ZMM_STOCK_INTRANSIT'.
  IF sy-subrc EQ 0.
    FREE MEMORY ID 'ZMM_STOCK_INTRANSIT'.
  ENDIF.

  SELECT b~ebeln,
         b~ebelp,
         a~bsart, " Doc Type UB and ZUB
         a~knumv, " Doc Contition No
         a~bedat,
         b~netwr
*         c~kbetr  " Amount
    FROM ekko AS a
    INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
*    INNER JOIN prcd_elements AS c ON a~knumv = c~knumv
*                                 AND b~ebelp = c~kposn
     FOR ALL ENTRIES IN @bestand
     WHERE a~ebeln = @bestand-ebeln
       AND b~ebeln = @bestand-ebeln
       AND b~ebelp = @bestand-ebelp
**       AND c~kposn = @bestand-ebelp
*       AND c~kschl = 'P101'
    INTO TABLE @DATA(lt_ekpo).
  LOOP AT bestand.
    MOVE-CORRESPONDING bestand TO lw_stock_intrans.
    lw_stock_intrans-menge_qnty = bestand-menge.
    lw_stock_intrans-meins_qnty = bestand-meins.
    lw_stock_intrans-dmbtr_qnty = bestand-dmbtr.
    lw_stock_intrans-waers_qnty = bestand-waers.
    lw_stock_intrans-menge_ordqnty = bestand-bstmg.
    lw_stock_intrans-meins_ordqnty = bestand-bstme.
    lw_stock_intrans-kasit_bum = bestand-kasit.
    lw_stock_intrans-meins_bum = bestand-meins.
    lw_stock_intrans-kasit_oum = bestand-kasit.
    lw_stock_intrans-meins_oum = bestand-meins.
    lw_stock_intrans-netwr_ordqnty = VALUE #( lt_ekpo[ ebeln = bestand-ebeln ebelp = bestand-ebelp ]-netwr OPTIONAL ).
    lw_stock_intrans-bedat = VALUE #( lt_ekpo[ ebeln = bestand-ebeln ebelp = bestand-ebelp ]-bedat OPTIONAL ).
    DATA(lv_doc_type) = VALUE #( lt_ekpo[ ebeln = bestand-ebeln ebelp = bestand-ebelp ]-bsart OPTIONAL ).
    IF lv_doc_type = 'ZUB' OR
       lv_doc_type = 'UB'.
      DATA(l_knumv) = VALUE #( lt_ekpo[ ebeln = bestand-ebeln ebelp = bestand-ebelp ]-knumv OPTIONAL ).
      SELECT SINGLE kbetr
        INTO @DATA(l_kbetr) FROM prcd_elements
          WHERE knumv = @l_knumv
          AND kposn = @bestand-ebelp
          AND kschl = 'P101'.
      lw_stock_intrans-netwr_ordqnty =  l_kbetr * lw_stock_intrans-menge_qnty.
    ENDIF.
    APPEND lw_stock_intrans TO e_stock_intrans.
  ENDLOOP.
ENDFUNCTION.
