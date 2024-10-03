*&---------------------------------------------------------------------*
*& Report ZDUPLICATE_CUSTGROUP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZDUPLICATE_CUSTGROUP.

Select a~CUSTOMER_NO as kunnr,
b~VKORG,
b~VTWEG,
b~SPART,
b~KDGRP FROM ZMIS_CUST_ST as a INNER JOIN knvv as b on a~CUSTOMER_NO = b~kunnr  into table @data(IT_KNVV) where STATUS = '01'.

  DATA : lt_final like IT_KNVV,
         lt_fcat  TYPE slis_t_fieldcat_alv.   "data dec for fieldcat

sort IT_KNVV by kunnr VKORG.

LOOP AT IT_KNVV ASSIGNING FIELD-SYMBOL(<fs_knvv>).

at new kunnr.

READ TABLE IT_KNVV INTO DATA(ls_knvv) WITH key <fs_knvv>-KUNNR BINARY SEARCH.

endat.

if <fs_knvv>-KDGRP NE ls_knvv-kdgrp.


APPEND <FS_KNVV> to LT_FINAL.



  ENDIF.

ENDLOOP.


  lt_fcat = VALUE #(
( col_pos = 1 fieldname = 'KUNNR' seltext_m = 'Customer No' )
( col_pos = 2 fieldname = 'VKORG' seltext_m = 'Sales Org' )
( col_pos = 3 fieldname = 'VTWEG' seltext_m = 'Dis Channel' )
( col_pos = 4 fieldname = 'SPART' seltext_m = 'Division' )
( col_pos = 5 fieldname = 'KDGRP' seltext_m = 'Cus Grp' ) ).



    DATA(ls_layo) = VALUE slis_layout_alv( colwidth_optimize = abap_true
                                           zebra = abap_true ).   "data dec for layout design

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        is_layout   = ls_layo
        it_fieldcat = lt_fcat
      TABLES
        t_outtab    = LT_FINAL.
