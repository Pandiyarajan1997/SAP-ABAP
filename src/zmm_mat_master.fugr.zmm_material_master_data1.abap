FUNCTION zmm_material_master_data1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMP_CODE) TYPE  BUKRS
*"  TABLES
*"      IT_MATERIAL STRUCTURE  ZSTR_MATERIAL_MASTER
*"      LT_MATNR STRUCTURE  RANGE_S_MATNR
*"----------------------------------------------------------------------

  DATA: lr_matnr TYPE RANGE OF mara-matnr.
  DATA: lr_mtart TYPE RANGE OF mara-mtart.


  DATA: gs_struc TYPE zstr_material_master.

*----------fetching company code and plant from T001K table---------*

  SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k)
    WHERE bukrs = @comp_code.

  IF sy-subrc = 0.

*----------------fatching data from MARC and MARA-------------------*

    SELECT
           b~matnr,
           b~ersda,
           b~ernam,
           b~laeda,
           b~aenam,
           b~mtart,
           b~mbrsh,
           b~matkl,
           b~bismt,
           b~meins,
           b~zeinr,
           b~groes,
           b~brgew,
           b~ntgew,
           b~gewei,
           b~volum,
           b~voleh,
           b~is_foc,
           b~reference_material,
           b~status,
           b~ecom_mat_name,
           b~ecom_name,
           b~ecom_category,
           b~zmrp_percen,
           b~zterm,
           b~lvorm,
           a~werks
             FROM marc AS a INNER JOIN mara AS b
      ON a~matnr = b~matnr INTO TABLE @DATA(gt_mara)
                FOR ALL ENTRIES IN @lt_t001k
                WHERE werks = @lt_t001k-bwkey.

    IF gt_mara IS NOT INITIAL.

*----------------fetching data from MARC-----------------*

      SORT gt_mara[] BY werks matnr mtart matkl.
      SELECT * FROM makt INTO TABLE @DATA(lt_makt)
               FOR ALL ENTRIES IN @gt_mara
               WHERE matnr = @gt_mara-matnr
               AND spras = @sy-langu.
      IF sy-subrc = 0.
        SORT lt_makt[] BY matnr.
      ENDIF.
    ENDIF.
    SELECT shcode, shtext FROM ztbl_sh INTO TABLE @DATA(lt_shades).
    SELECT pccode, pctext FROM ztbl_pc INTO TABLE @DATA(lt_procat).
    LOOP AT gt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>).
***************check the client level active or not*********
      IF <fs_mara>-lvorm = 'X'.
        <fs_mara>-status = 'X'.
      ENDIF.

*----------------appending all the fetched data to IT_material---------------*

      APPEND VALUE #( company = comp_code
                      plant = <fs_mara>-werks
                      matnr = <fs_mara>-matnr
                      maktx = VALUE #( lt_makt[ matnr = <fs_mara>-matnr ]-maktx OPTIONAL )
                      ersda = <fs_mara>-ersda
                      ernam = <fs_mara>-ernam
                      laeda = <fs_mara>-laeda
                      aenam = <fs_mara>-aenam
                      mtart = <fs_mara>-mtart
                      mbrsh = <fs_mara>-mbrsh
                      matkl = <fs_mara>-matkl
                      bismt = <fs_mara>-bismt
                      meins = <fs_mara>-meins
                      zeinr = <fs_mara>-zeinr
                      groes = <fs_mara>-groes
                      volum = <fs_mara>-volum
                      voleh = <fs_mara>-voleh
                      is_foc = <fs_mara>-is_foc
                      reference_material = <fs_mara>-reference_material
                      status = <fs_mara>-status
                      ecom_mat_name = <fs_mara>-ecom_mat_name
                      ecom_name = <fs_mara>-ecom_name
                      ecom_des = VALUE #( lt_shades[ shcode = <fs_mara>-ecom_name ]-shtext OPTIONAL )
                      ecom_category = <fs_mara>-ecom_category
                      cat_des = VALUE #( lt_procat[ pccode = <fs_mara>-ecom_category ]-pctext OPTIONAL )
                      zmrp_percen = <fs_mara>-zmrp_percen
                      zterm = <fs_mara>-zterm
                      brgew = <fs_mara>-brgew
                      ntgew = <fs_mara>-ntgew
                      gewei = <fs_mara>-gewei ) TO it_material.
    ENDLOOP.
  ENDIF.


ENDFUNCTION.
