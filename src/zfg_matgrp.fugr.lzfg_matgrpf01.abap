*----------------------------------------------------------------------*
***INCLUDE LZFG_MATGRPF01.
*----------------------------------------------------------------------*
FORM matgrp_description.
  DATA: ls_desc TYPE t023t.
  SELECT SINGLE * FROM t023t INTO ls_desc WHERE matkl = zmatgrp_perc-material_grp.
  IF sy-subrc EQ 0.
    zmatgrp_perc-matgrp_des = ls_desc-wgbez.
  ENDIF.
ENDFORM.
