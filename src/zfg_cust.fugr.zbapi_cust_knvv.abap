FUNCTION zbapi_cust_knvv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_KUNNR) TYPE  KUNNR_TTY OPTIONAL
*"     VALUE(STA_DAT) TYPE  ERDAT
*"     VALUE(END_DAT) TYPE  ERDAT
*"  TABLES
*"      IT_CUSTOMER STRUCTURE  ZSD_ST_CUST_KUNNV
*"----------------------------------------------------------------------
*** Created By : Puratchiveeran

*-----------------------------------------------------------------------
  TYPES:BEGIN OF ty_konp,
          knumh    TYPE konp-knumh,
          kopos    TYPE konp-kopos,
          kschl    TYPE konp-kschl,
          kbetr    TYPE konp-kbetr,
          konwa    TYPE konp-konwa,
          kmein    TYPE konp-kmein,
          meins    TYPE konp-kmein,
          loevm_ko TYPE   konp-loevm_ko,
        END OF ty_konp.

  DATA:it_konp TYPE TABLE OF ty_konp,
       wa_konp TYPE ty_konp.

  TYPES : BEGIN OF ty_cdhdr,
            mandant    TYPE cdhdr-mandant,
            objectclas TYPE cdhdr-objectclas,
            objectid   TYPE cdhdr-objectid,
            changenr   TYPE cdhdr-changenr,
            udate      TYPE cdhdr-udate,
            utime      TYPE cdhdr-utime,
            tcode      TYPE cdhdr-tcode,
          END OF ty_cdhdr.

  TYPES : BEGIN OF ty_cdpos,
            mandant      TYPE cdpos-mandant,
            objectclas   TYPE cdpos-objectclas,
            objectid(10) ,
            changenr     TYPE cdpos-changenr,
            tabname      TYPE cdpos-tabname,
            tabkey       TYPE cdpos-tabkey,
            fname        TYPE cdpos-fname,
            value_new    TYPE cdpos-value_new,
            value_old    TYPE cdpos-value_old,
          END OF ty_cdpos.


  DATA : it_cdhdr TYPE TABLE OF ty_cdhdr,
         wa_cdhdr TYPE ty_cdhdr.

  DATA : it_cdpos TYPE TABLE OF ty_cdpos,
         wa_cdpos TYPE ty_cdpos.

  DATA: lt_kunnr TYPE STANDARD TABLE OF kunnr_sty.
  DATA: lr_kunnr TYPE RANGE OF kunnr,
        ls_kunnr LIKE LINE OF lr_kunnr.
  DATA: lt_customer TYPE STANDARD TABLE OF  zsd_st_cust_kunnv.
  SELECT
        mandant
        objectclas
        objectid
        changenr
        udate
        utime
        tcode
         INTO TABLE it_cdhdr FROM cdhdr
    WHERE udate GE sta_dat AND
          udate LE end_dat AND
          ( tcode EQ 'XD01' OR
            tcode EQ 'XD02' OR
            tcode EQ 'VD01' OR
            tcode EQ 'VD02' OR
            tcode EQ 'MASS' OR
            tcode EQ 'XD05' OR
            tcode EQ 'XD06' OR
            tcode EQ 'XD99' ) .

  IF it_cdhdr IS NOT INITIAL.
    SELECT
             mandant
             objectclas
             objectid
             changenr
             tabname
             tabkey
             fname
             value_new
             value_old FROM cdpos INTO TABLE it_cdpos
      FOR ALL ENTRIES IN it_cdhdr WHERE
             objectclas = it_cdhdr-objectclas AND
             objectid = it_cdhdr-objectid AND
             changenr = it_cdhdr-changenr AND
             tabname = 'KNVV' AND
             ( fname EQ 'KVGR1' OR fname EQ 'KVGR2' OR fname EQ 'KVGR3' OR fname EQ 'KVGR4' OR fname EQ 'KVGR5').
  ENDIF.
  IF iv_kunnr IS NOT INITIAL.
    LOOP AT iv_kunnr INTO DATA(lw_kunnr).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_kunnr-kunnr
        IMPORTING
          output = lw_kunnr-kunnr.
      ls_kunnr-sign = 'I'.
      ls_kunnr-option = 'EQ'.
      ls_kunnr-low = lw_kunnr-kunnr.
*      APPEND lw_kunnr TO lt_kunnr.
      APPEND ls_kunnr TO lr_kunnr.
    ENDLOOP.
  ENDIF.
  IF lr_kunnr IS NOT INITIAL.
    DELETE it_cdpos WHERE objectid NOT IN lr_kunnr.
  ENDIF.
  SELECT
        kunnr
        vkorg
        vtweg
        spart
        kdgrp
        kvgr1
        kvgr2
        kvgr3
        kvgr4
        kvgr5 FROM knvv INTO CORRESPONDING FIELDS OF TABLE lt_customer
      FOR ALL ENTRIES IN it_cdpos
        WHERE kunnr = it_cdpos-objectid
          and loevm ne 'X'.
*                erdat BETWEEN sta_dat AND end_dat.
  IF sy-subrc = 0.
    SELECT * FROM  tvv1t
      INTO TABLE @DATA(lt_tvv1t)
      FOR ALL ENTRIES IN @lt_customer
      WHERE spras = @sy-langu AND
            kvgr1 = @lt_customer-kvgr1.
    SELECT * FROM  tvv2t
      INTO TABLE @DATA(lt_tvv2t)
      FOR ALL ENTRIES IN @lt_customer
      WHERE spras = @sy-langu AND
            kvgr2 = @lt_customer-kvgr2.
    SELECT * FROM  tvv3t
      INTO TABLE @DATA(lt_tvv3t)
      FOR ALL ENTRIES IN @lt_customer
      WHERE spras = @sy-langu AND
            kvgr3 = @lt_customer-kvgr3.
    SELECT * FROM  tvv4t
      INTO TABLE @DATA(lt_tvv4t)
      FOR ALL ENTRIES IN @lt_customer
      WHERE spras = @sy-langu AND
            kvgr4 = @lt_customer-kvgr4.
    SELECT * FROM  tvv5t
      INTO TABLE @DATA(lt_tvv5t)
      FOR ALL ENTRIES IN @lt_customer
      WHERE spras = @sy-langu AND
            kvgr5 = @lt_customer-kvgr5.

    SELECT * FROM t151t
      INTO TABLE @DATA(lt_t151t)
      FOR ALL ENTRIES IN @lt_customer
      WHERE spras = @sy-langu AND
            kdgrp = @lt_customer-kdgrp.
  ENDIF.
  LOOP AT lt_customer ASSIGNING FIELD-SYMBOL(<fs_cust>).
    DATA flag TYPE c.
    <fs_cust>-kdgrp_txt = VALUE #( lt_t151t[ kdgrp = <fs_cust>-kdgrp ]-ktext OPTIONAL ).

    <fs_cust>-kvgr1_txt = VALUE #( lt_tvv1t[ kvgr1 = <fs_cust>-kvgr1 ]-bezei OPTIONAL ).
    IF <fs_cust>-kvgr1_txt IS NOT INITIAL.
      flag = abap_true.
    ENDIF.
    <fs_cust>-kvgr2_txt = VALUE #( lt_tvv2t[ kvgr2 = <fs_cust>-kvgr2 ]-bezei OPTIONAL ).
    IF <fs_cust>-kvgr2_txt IS NOT INITIAL.
      flag = abap_true.
    ENDIF.
    <fs_cust>-kvgr3_txt = VALUE #( lt_tvv3t[ kvgr3 = <fs_cust>-kvgr3 ]-bezei OPTIONAL ).
    IF <fs_cust>-kvgr3_txt IS NOT INITIAL.
      flag = abap_true.
    ENDIF.
    <fs_cust>-kvgr4_txt = VALUE #( lt_tvv4t[ kvgr4 = <fs_cust>-kvgr4 ]-bezei OPTIONAL ).
    IF <fs_cust>-kvgr4_txt IS NOT INITIAL.
      flag = abap_true.
    ENDIF.
    <fs_cust>-kvgr5_txt = VALUE #( lt_tvv5t[ kvgr5 = <fs_cust>-kvgr5 ]-bezei OPTIONAL ).
    IF <fs_cust>-kvgr5_txt IS NOT INITIAL.
      flag = abap_true.
    ENDIF.
    IF flag IS NOT INITIAL .
      APPEND <fs_cust> TO  it_customer.
      CLEAR flag.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
