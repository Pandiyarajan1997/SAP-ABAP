@EndUserText.label: 'Table Function for Customer Block'
@ClientHandling.type: #CLIENT_DEPENDENT
@ClientHandling.algorithm: #SESSION_VARIABLE
define table function ZC_TAB_FUNC_CUST_BLK
  with parameters
    p_bukrs : bukrs,
    p_vkorg : vkorg,
    @Environment.systemField: #CLIENT
    p_clnt  : abap.clnt
returns
{
  mandt            : abap.clnt;
  bukrs            : bukrs;
  vkorg            : vkorg;
  kunnr            : kunnr;
  name1            : name1;
  werks            : werks_d;
  block            : loevm_x;
  dist             : zdist;
  dist_name        : name1;
  dist_werks       : werks_d;
  dist_block       : loevm_x;
  stras            : stras_gp;
  ort01            : ort01_gp;
  pstlz            : pstlz;
  regio            : regio;
  smtp_addr        : ad_smtpadr;
  telf1            : telf1;
  telf2            : telf2;
  ktokd            : ktokd;
  vkbur            : vkbur;
  kdgrp            : kdgrp;
  ktext            : vtxtk;
  lifnr            : lifnr;
  katr2            : katr2;
  vtext            : vtext;
  stcd3            : stcd3;
  j_1ipanno        : j_1ipanno;
  partner          : bu_partner;
  group_feature    : bp_group_feature;
  group_feature_na : bp_group_feature_name;
  so_pernr         : zso_pernr;
  so_pernr_name    : zso_emnam;
  so_position      : zso_position;
  so_position_name : zso_postext;
  ch_pernr         : zch_pernr;
  ch_pernr_name    : zch_emnam;
  ch_position      : zch_position;
  ch_position_name : zch_emnam;
  zeinv_flag       : zeinv_flag;
  zeinv_chk        : zeinv_chk;
}
implemented by method
  zcl_amdp_cust_blk=>get_cust_blk;
