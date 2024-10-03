"Name: \TY:CVI_EI_ADAPTER\ME:GET_PARTNER_IDENTIFICATION\SE:END\EI
ENHANCEMENT 0 ZGROUP_CHAR_SET.
If lv_guid is not initial.
  data(ls_grpchar) = VALUE bp_group_feature( ).
  DATA:lt_return TYPE TABLE OF bapiret2.
  IMPORT ls_grpchar = ls_grpchar FROM MEMORY ID 'GRP'.
  IF ls_grpchar is NOT INITIAL.
   DATA(iv_attr) = VALUE  bapi_str_bupa_fs_treasury( group_feature   = ls_grpchar  ).
    DATA(iv_attrX) = VALUE  bapi_str_bupa_fs_treasury2_x( group_feature = abap_true ).
    DATA(l_grpfeaturex) = VALUE bapi_str_bupa_fs_treasury2_x( group_feature = abap_true ).
   CALL FUNCTION 'BAPI_BUPA_FS_ATTRIBUTES_SET'
      EXPORTING
        businesspartner = r_partner_ids-partner_id
        attributes      = iv_attr
        attributesx     = iv_attrX
      TABLES
        return          = lt_return.
   FREE MEMORY ID 'GRP'.
  data(iv_partnerid) = VALUE BU_PARTNER( ).
  UNPACK r_partner_ids-partner_id to iv_partnerid.
   EXPORT iv_partnerid = iv_partnerid to MEMORY ID 'PID'.
  ENDIF.
  ENDIF.
ENDENHANCEMENT.
