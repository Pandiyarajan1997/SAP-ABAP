FUNCTION zmm_material_master_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_MATERIAL STRUCTURE  ZSTR_MATERIAL_MASTER
*"      LT_MATNR STRUCTURE  RANGE_S_MATNR OPTIONAL
*"----------------------------------------------------------------------

  DATA: lr_matnr TYPE RANGE OF mara-matnr.
  DATA: lr_mtart TYPE RANGE OF mara-mtart.

  DATA: gs_struc TYPE zstr_material_master.

  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc)
                       WHERE name = 'MATERIAL_TYPE_BAPI'
                       AND type = 'S'.
  LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).
    APPEND VALUE #( sign = 'I'
                    option = 'EQ'
                    low = ls_tvarvc-low ) TO lr_mtart.
  ENDLOOP.
  REFRESH: lr_matnr.
  IF lt_matnr IS NOT INITIAL.
    LOOP AT lt_matnr INTO DATA(lw_matnr).
      APPEND VALUE #( sign = 'I'
                      option = 'EQ'
                      low = lw_matnr-low ) TO lr_matnr.
    ENDLOOP.
  ENDIF.

  SELECT matnr,
         ersda,
         ernam,
         laeda,
         aenam,
         mtart,
         mbrsh,
         matkl,
         bismt,
         meins,
         zeinr,
         groes,
         brgew,
         ntgew,
         gewei,
         volum,
         voleh,
         is_foc,
         reference_material,
         status,
         ecom_mat_name,
         ecom_name,
         ecom_category,
*         scheme_points,
         zmrp_percen,
         zterm,
         lvorm  FROM mara INTO TABLE @DATA(gt_mara)
                WHERE matnr IN @lr_matnr
                AND mtart IN @lr_mtart.
*                AND status NE 'X'.

  IF gt_mara IS NOT INITIAL.
    SORT gt_mara[] BY matnr mtart matkl.
    SELECT * FROM makt INTO TABLE @DATA(lt_makt)
             FOR ALL ENTRIES IN @gt_mara
             WHERE matnr = @gt_mara-matnr
             AND spras = @sy-langu.
    IF sy-subrc = 0.
      SORT lt_makt[] BY matnr.
    ENDIF.
    SELECT shcode, shtext FROM ztbl_sh INTO TABLE @DATA(lt_shades).
    SELECT pccode, pctext FROM ztbl_pc INTO TABLE @DATA(lt_procat).
*    SELECT scheme, description FROM zsch_points INTO TABLE @DATA(lt_schemes).
    LOOP AT gt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>).
***************check the client level active or not*********
      IF <fs_mara>-lvorm = 'X'.
        <fs_mara>-status = 'X'.
      ENDIF.

      APPEND VALUE #( matnr = <fs_mara>-matnr
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
*                      scheme_points = <fs_mara>-scheme_points
*                      sch_des = VALUE #( lt_schemes[ scheme = <fs_mara>-scheme_points ]-description OPTIONAL )
                      zmrp_percen = <fs_mara>-zmrp_percen
                      zterm = <fs_mara>-zterm
                      brgew = <fs_mara>-brgew
                      ntgew = <fs_mara>-ntgew
                      gewei = <fs_mara>-gewei ) TO it_material.
    ENDLOOP.
  ENDIF.


ENDFUNCTION.
