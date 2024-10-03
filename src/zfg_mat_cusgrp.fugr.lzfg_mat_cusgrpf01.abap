*----------------------------------------------------------------------*
***INCLUDE LZFG_MAT_CUSGRPF01.
*----------------------------------------------------------------------*
FORM new_entry.
  IF zmm_mat_cusgrp-matnr IS NOT INITIAL.
    SELECT SINGLE matnr INTO @DATA(lv_matnr) FROM mara WHERE matnr = @zmm_mat_cusgrp-matnr.
    IF sy-subrc NE 0.
      MESSAGE 'Enter Valid Material Number' TYPE 'E'.
    ELSE.
      SELECT SINGLE maktx INTO @DATA(lv_desc) FROM makt WHERE matnr = @lv_matnr.
      IF sy-subrc = 0.
        zmm_mat_cusgrp-maktx = lv_desc.
      ENDIF.
      zmm_mat_cusgrp-erdat = sy-datum.
      zmm_mat_cusgrp-usnam = sy-uname.
    ENDIF.
  ENDIF.
  IF zmm_mat_cusgrp-kvgr1 IS NOT INITIAL.
    SELECT SINGLE * FROM tvv1t INTO @DATA(ls_cusgrp) WHERE kvgr1 = @zmm_mat_cusgrp-kvgr1.
    IF sy-subrc NE 0.
      MESSAGE 'Enter Valid Customer Group' TYPE 'E'.
    ELSE.
      zmm_mat_cusgrp-bezei = ls_cusgrp-bezei.
    ENDIF.
  ENDIF.
ENDFORM.
