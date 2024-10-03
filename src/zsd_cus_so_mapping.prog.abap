*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 26.10.2023
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                : DEVK934554
*
*  Business Logic            : customer so mapping display XN missing & FN mismatch in alv report
*
*  Released on Date          :
*
*=======================================================================
REPORT zsd_cus_so_mapping.

DATA : lv_kunnr TYPE kna1-kunnr.

SELECT-OPTIONS : s_kunnr FOR lv_kunnr.

AT SELECTION-SCREEN.

  PERFORM alv.      "get datas from fm & display ALV


***************subroutines*****************

FORM alv.

  DATA : lt_final TYPE TABLE OF zstsd_cus_so_mapping,
         lt_fcat  TYPE slis_t_fieldcat_alv.   "data dec for fieldcat

  lt_fcat = VALUE #(
( col_pos = 1 fieldname = 'KUNNR' seltext_m = TEXT-002 )
( col_pos = 2 fieldname = 'NAME1' seltext_m = TEXT-003 )
( col_pos = 3 fieldname = 'KDGRP' seltext_m = TEXT-004 )
( col_pos = 4 fieldname = 'DESCR' seltext_m = TEXT-005 )
( col_pos = 5 fieldname = 'VKORG' seltext_m = TEXT-006 )
( col_pos = 6 fieldname = 'VTWEG' seltext_m = TEXT-007 )
( col_pos = 7 fieldname = 'SPART' seltext_m = TEXT-008 )
( col_pos = 8 fieldname = 'PARVW' seltext_m = TEXT-009 )
( col_pos = 9 fieldname = 'LIFNR' seltext_m = TEXT-010 )
( col_pos = 10 fieldname = 'PERNR' seltext_m = TEXT-011 )
( col_pos = 11 fieldname = 'POS_PERNR' seltext_m = TEXT-012 )
( col_pos = 12 fieldname = 'REMARKS' seltext_m = TEXT-013 ) ).

*******************call fm for get the datas of customer so mapping***********************

  CALL FUNCTION 'ZBAPI_SD_CUS_SO_MAPPING'
    TABLES
      lt_kunnr = s_kunnr
      lt_final = lt_final.

  IF lt_final IS NOT INITIAL.

    DATA(ls_layo) = VALUE slis_layout_alv( colwidth_optimize = abap_true
                                           zebra = abap_true ).   "data dec for layout design

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        is_layout   = ls_layo
        it_fieldcat = lt_fcat
      TABLES
        t_outtab    = lt_final.

  ELSE.

    MESSAGE : 'No Data Found' TYPE 'E'.

  ENDIF.

ENDFORM.
