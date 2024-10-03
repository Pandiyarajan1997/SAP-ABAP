FUNCTION zbapi_get_cust_group.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MATNR) TYPE  MATNR OPTIONAL
*"     VALUE(STA_DAT) TYPE  ERDAT
*"     VALUE(END_DAT) TYPE  ERDAT
*"  TABLES
*"      IT_MAT_CUST STRUCTURE  ZMM_MAT_CUSGRP
*"----------------------------------------------------------------------
*** Created By : Puratchiveeran

*-----------------------------------------------------------------------
  SELECT * FROM
    zmm_mat_cusgrp INTO TABLE it_mat_cust
    WHERE erdat BETWEEN sta_dat AND end_dat.
  IF iv_matnr IS NOT INITIAL.
    DELETE it_mat_cust WHERE matnr NE iv_matnr.
  ENDIF.
ENDFUNCTION.
