FUNCTION zmat_group_ppl.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MATERIAL_GRP) TYPE  MATKL OPTIONAL
*"  TABLES
*"      IT_FINAL STRUCTURE  ZSTR_PRD_GROUP_PPL
*"----------------------------------------------------------------------
***---------------------------------------------------------------------
*& Date Created - 12.03.2021
*& Created By   - KPABAP
*& Description  - Program to set the people target for a material grp
*&---------------------------------------------------------------------*
* Change Date   - 22.04.2022
* TR NO         - DEVK931619
* Changed By    - KPABAP (Shamsudeen)
* Reference     - User Discussion of required changes
* Description   - to send all details from material grp table along with ppl_value(people_target)
*------------------------------------------------------------------------------------------------
   "actual output structure"
  DATA : ls_final TYPE zstr_prd_group_ppl .

    "Taking material grp description"
  DATA: lt_t023t TYPE STANDARD TABLE OF t023t,
        ls_t023t TYPE t023t.

     "Table with material grp and ppl_vale"
  DATA : lt_matgrp TYPE STANDARD TABLE OF zmm_matgrp_ppl,
         ls_matgrp TYPE zmm_matgrp_ppl.

 " Fetching Data from Table (ZMM_MATGRP_PPL) according to input"
  IF material_grp IS NOT INITIAL.
    SELECT * FROM zmm_matgrp_ppl
             INTO TABLE lt_matgrp
             WHERE matkl  EQ material_grp.

  ELSEIF material_grp IS INITIAL.
    SELECT * FROM zmm_matgrp_ppl
             INTO TABLE lt_matgrp.
  ENDIF.

  IF lt_matgrp[] IS NOT INITIAL.
    "Fetching material grp description with "
    SELECT * FROM t023t
             INTO TABLE lt_t023t
             WHERE spras = sy-langu.

    LOOP AT lt_matgrp INTO ls_matgrp .
      CLEAR ls_final.
      ls_final-material_group = ls_matgrp-matkl.
      ls_final-ppl_target = ls_matgrp-ppl_value .
      READ TABLE lt_t023t INTO ls_t023t WITH KEY matkl = ls_matgrp-matkl.
      IF sy-subrc EQ 0 .
        ls_final-material_grp_des = ls_t023t-wgbez60.
      ENDIF.
      APPEND ls_final TO it_final.
    ENDLOOP.
  ENDIF.
ENDFUNCTION.
