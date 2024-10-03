FUNCTION ZGET_CUSTOMER_ROL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_KDGRP) TYPE  KDGRP
*"  TABLES
*"      IT_ROL STRUCTURE  ZSTR_PEPUP_ROL
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------


TYPES: BEGIN OF gs_KNVV,
        KUNNR TYPE KNVV-KUNNR,
        VKORG TYPE KNVV-VKORG,
        KDGRP TYPE KNVV-KDGRP,
      END OF gs_KNVV.

TYPES : BEGIN OF gs_ZCUSTMAT_ROL,
        kUNNR TYPE ZCUSTMAT_ROL-KUNNR,
        MATNR TYPE ZCUSTMAT_ROL-MATNR,
        NEW_VAL TYPE ZCUSTMAT_ROL-NEW_VAL,
        NEW_DAT TYPE ZCUSTMAT_ROL-NEW_DAT,
        NEW_CRE TYPE ZCUSTMAT_ROL-NEW_CRE,
        OLD_VAL TYPE ZCUSTMAT_ROL-OLD_VAL,
        OLD_DAT TYPE ZCUSTMAT_ROL-OLD_DAT,
        OLD_CRE TYPE ZCUSTMAT_ROL-OLD_CRE,
      END OF gs_ZCUSTMAT_ROL.

 DATA : it_knvv TYPE TABLE OF gs_knvv,
        wa_knvv TYPE gs_knvv.

   DATA: IT_ZCUSTMAT_ROL TYPE STANDARD TABLE OF gs_ZCUSTMAT_ROL,
        WA_ZCUSTMAT_ROL TYPE gs_ZCUSTMAT_ROL.

 DATA : wa_rol TYPE ZSTR_PEPUP_ROL.

 SELECT
        KUNNR
        VKORG
        KDGRP FROM knvv INTO TABLE it_knvv WHERE kdgrp eq P_KDGRP .
sort : it_knvv BY kunnr.
   delete ADJACENT DUPLICATES FROM it_knvv COMPARING kunnr.

 SELECT
        kUNNR
        MATNR
        NEW_VAL
        NEW_DAT
        NEW_CRE
        OLD_VAL
        OLD_DAT
        OLD_CRE
   FROM ZCUSTMAT_ROL INTO TABLE IT_ZCUSTMAT_ROL FOR ALL ENTRIES IN
                       it_knvv WHERE kunnr = it_knvv-kunnr ."WHERE kunnr eq p_kunnr.

if NOT IT_ZCUSTMAT_ROL IS INITIAL.
   loop at IT_ZCUSTMAT_ROL INTO WA_ZCUSTMAT_ROL.
     MOVE-CORRESPONDING WA_ZCUSTMAT_ROL to wa_rol.
     APPEND wa_rol to it_Rol.
     CLEAR wa_rol.
   ENDLOOP.
ENDIF.
*&------------------------------------------ Data logic - End -------------------------------------------------

*&--------------------------------------------- BAPI Return Messages--------------------------------------------------------------------
  IF it_Rol[] IS NOT INITIAL.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'S'   'SUCCESS!!! - Customer Material Rol Data Taken !!!'.
  ELSE.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'E'   'Error!!! - Customer Material Rol Data Not Taken !!!'.
  ENDIF.






ENDFUNCTION.
