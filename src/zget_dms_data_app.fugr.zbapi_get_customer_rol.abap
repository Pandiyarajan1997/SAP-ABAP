FUNCTION zbapi_get_customer_rol.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_KDGRP) TYPE  KDGRP
*"  TABLES
*"      IT_ROL STRUCTURE  ZSTR_PEPUP_ROL
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------


  TYPES: BEGIN OF gs_knvv,
           kunnr TYPE knvv-kunnr,
           vkorg TYPE knvv-vkorg,
           kdgrp TYPE knvv-kdgrp,
         END OF gs_knvv.

  TYPES : BEGIN OF gs_zcustmat_rol,
            kunnr   TYPE zcustmat_rol-kunnr,
            matnr   TYPE zcustmat_rol-matnr,
            new_val TYPE zcustmat_rol-new_val,
            new_dat TYPE zcustmat_rol-new_dat,
            new_cre TYPE zcustmat_rol-new_cre,
            old_val TYPE zcustmat_rol-old_val,
            old_dat TYPE zcustmat_rol-old_dat,
            old_cre TYPE zcustmat_rol-old_cre,
          END OF gs_zcustmat_rol.

  DATA : it_knvv TYPE TABLE OF gs_knvv,
         wa_knvv TYPE gs_knvv.

  DATA: it_zcustmat_rol TYPE STANDARD TABLE OF gs_zcustmat_rol,
        wa_zcustmat_rol TYPE gs_zcustmat_rol.

  DATA : wa_rol TYPE zstr_pepup_rol.

  SELECT
         kunnr
         vkorg
         kdgrp FROM knvv INTO TABLE it_knvv WHERE kdgrp EQ p_kdgrp .
  SORT : it_knvv BY kunnr.
  DELETE ADJACENT DUPLICATES FROM it_knvv COMPARING kunnr.

  SELECT
         kunnr
         matnr
         new_val
         new_dat
         new_cre
         old_val
         old_dat
         old_cre
                 FROM zcustmat_rol
                 INTO TABLE it_zcustmat_rol FOR ALL ENTRIES IN
                 it_knvv WHERE kunnr = it_knvv-kunnr.
*                                AND   b~lvorm  NE 'X'
*                                AND   b~status NE 'X'.
  IF sy-subrc = 0.
    SELECT matnr,lvorm,status FROM mara INTO TABLE @DATA(lt_mara).
*                                        FOR ALL ENTRIES IN @it_zcustmat_rol
*                                        WHERE matnr = @it_zcustmat_rol-matnr.
    IF sy-subrc = 0.
      SORT : lt_mara BY matnr.
    ENDIF.
  ENDIF.
  ."WHERE kunnr eq p_kunnr.

  IF NOT it_zcustmat_rol IS INITIAL.
    LOOP AT it_zcustmat_rol INTO wa_zcustmat_rol.

      CONDENSE : wa_zcustmat_rol-matnr.
      READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = wa_zcustmat_rol-matnr BINARY SEARCH.
      IF ls_mara-lvorm = 'X' OR ls_mara-status = 'X'.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING wa_zcustmat_rol TO wa_rol.
      APPEND wa_rol TO it_rol.
      CLEAR : wa_rol,ls_mara.
    ENDLOOP.
  ENDIF.
*&------------------------------------------ Data logic - End -------------------------------------------------

*&--------------------------------------------- BAPI Return Messages--------------------------------------------------------------------
  IF it_rol[] IS NOT INITIAL.
    PERFORM raise_message TABLES  return   USING   'S'   'SUCCESS!!! - Customer Material Rol Data Taken !!!'.
  ELSE.
    PERFORM raise_message TABLES  return   USING   'E'   'Error!!! - Customer Material Rol Data Not Taken !!!'.
  ENDIF.






ENDFUNCTION.
