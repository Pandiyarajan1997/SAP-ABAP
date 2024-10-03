FUNCTION zmaterial_create_master.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJID) TYPE  TABNAME16
*"  TABLES
*"      LT_OUTPUT_MM STRUCTURE  ZSTR_MAT_MASTER
*"----------------------------------------------------------------------
  " Created by: Samsudeen M
  " Created on: 03.01.2022
  " Reference by: Devasena & Suresh BV
*-----------------------------------------------------------------------

  REFRESH: lt_output_mm.

  IF objid = 'ZTBL_SH'.

    SELECT shcode, shtext FROM ztbl_sh INTO TABLE @DATA(lt_shades).
    IF sy-subrc = 0.
      APPEND LINES OF lt_shades TO lt_output_mm.
      SORT lt_output_mm[] BY code.
    ENDIF.

  ELSEIF objid = 'ZTBL_PS'.
    SELECT pscode, pstext, pstext_l FROM ztbl_ps INTO TABLE @DATA(lt_packsize).
    IF sy-subrc = 0.
      APPEND LINES OF lt_packsize TO lt_output_mm.
      SORT lt_output_mm[] BY code.
    ENDIF.

  ELSEIF objid = 'ZTBL_PC'.
    SELECT pccode, pctext FROM ztbl_pc INTO TABLE @DATA(lt_procat).
    IF sy-subrc = 0.
      APPEND LINES OF lt_procat TO lt_output_mm.
      SORT lt_output_mm[] BY code.
    ENDIF.

  ELSEIF objid = 'ZTBL_BR'.
    SELECT brcode, brtext FROM ztbl_br INTO TABLE @DATA(lt_brcode).
    IF sy-subrc = 0.
      APPEND LINES OF lt_brcode TO lt_output_mm.
      SORT lt_output_mm[] BY code.
    ENDIF.

  ELSEIF objid = 'ZTBL_CF'.
    SELECT cfcode, cftext FROM ztbl_cf INTO TABLE @DATA(lt_colorfam).
    IF sy-subrc = 0.
      APPEND LINES OF lt_colorfam TO lt_output_mm.
      SORT lt_output_mm[] BY code.
    ENDIF.

  ELSEIF objid = 'ZTBL_BU'.
    SELECT bucode, butext FROM ztbl_bu INTO TABLE @DATA(lt_busunit).
    IF sy-subrc = 0.
      APPEND LINES OF lt_busunit TO lt_output_mm.
      SORT lt_output_mm[] BY code.
    ENDIF.

  ELSEIF  objid = 'ZTBL_CH'.
    SELECT chcode, chtext FROM ztbl_ch INTO TABLE @DATA(lt_chemistry).
    IF sy-subrc = 0.
      APPEND LINES OF lt_chemistry TO lt_output_mm.
      SORT lt_output_mm[] BY code.
    ENDIF.

  ENDIF.



ENDFUNCTION.
