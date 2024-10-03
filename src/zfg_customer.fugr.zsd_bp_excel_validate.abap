FUNCTION zsd_bp_excel_validate.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      LT_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
  " CREATED BY: SAMSUDEEN M
  " CREATED ON: 21.12.2022
  " PURPOSE FOR: VALIDATING EXCEL DATA FOR BP CREATION ***
*------------------------------------------------------------------------
  TYPES: BEGIN OF ty_bp_all,
           bp         TYPE bapibus1006_head-bpartner,
           bpcat      TYPE bapibus1006_head-partn_cat, " CATEGORY
           bpgrp      TYPE bapibus1006_head-partn_grp, "BP GROUP
           title      TYPE ad_titletx, "TITLE
           sterm1     TYPE sortl, "SEARCH TERM
           fname      TYPE bu_namep_f, "FIRST NAME
           lname      TYPE bu_namep_l, "LAST NAME
           name3      TYPE bu_nameor3,
           name4      TYPE bu_nameor4,
           street     TYPE ad_street,  "STREET
           addr1      TYPE ad_strspp1, "ADDRESS1
           addr2      TYPE ad_strspp1, "ADDRESS2
           addr3      TYPE ad_strspp1, "ADDRESS3
           houseno    TYPE ad_hsnm1, "HOUSE NUMBER
           district   TYPE ad_city2,
           country    TYPE land1, "COUNTRY
           langu      TYPE spras, "LANGUAGE
           region     TYPE regio, "REGION
           city       TYPE ad_city1, "CITY
           pcode      TYPE ad_pstcd1, "POSTAL CODE
           telno      TYPE ad_tlnmbr, "MOBILE NUMBER
           mobile     TYPE ad_tlnmbr, "MOBILE NUMBER
           email      TYPE ad_smtpadr, "EMAIL ID
           taxcat     TYPE bptaxtype,
           taxno      TYPE bapibus1006tax-taxnumber, "GST NUMBER
           bukrs      TYPE lfb1-bukrs, " COMPANY CODE
           akont      TYPE akont,      "RECON ACCOUNT
           zterm      TYPE dzterm,     "PAYMENT TERMS
           vkorg      TYPE vkorg, "SALES ORG
           vtweg      TYPE vtweg, "DISTRIBUTION CHANNEL
           spart      TYPE spart, "DIVISION
           bzirk      TYPE knvv-bzirk, " SALES DISTRICT
           kdgrp      TYPE knvv-kdgrp, " CUSTOMER GROUP
           vkbur      TYPE knvv-vkbur, " SALES OFFICE
           vkgrp      TYPE knvv-vkgrp, "SALES GROUP NEW
           ktgrd      TYPE knvv-ktgrd, "ACCT ASSIGNMENT GRP
           kalks      TYPE knvv-kalks, " PRICING
           incov      TYPE knvv-incov, "INCOMING TERMS VERSION
           inco1      TYPE knvv-inco1, " INCOTERMS (PART 1)
           inco2      TYPE knvv-inco2, " INCOTERMS (PART 2)
           inco_l     TYPE inco2_l, " INCO TERMS LOCATION
           vwerk      TYPE knvv-vwerk, "DELIVERY PLANT
           vsbed      TYPE knvv-vsbed, "SHIPPING CONDITIONS
           taxkd01(1), "TAX1-JOSG
           taxkd02(1), "TAX1-JOCG
           taxkd03(1), "TAX1-JOIG
           taxkd04(1), "TAX1-JOUG
           taxkd05(1), "TAX1-JOCS
           taxkd06(1), "TAX1-JTC1
           attr1      TYPE katr1, "ATTRIBUTE 1
           attr2      TYPE katr2, "ATTRIBUTE 2
           attr3      TYPE katr3, "ATTRIBUTE 3
           attr4      TYPE katr4, "ATTRIBUTE 4
           attr5      TYPE katr5, "ATTRIBUTE 5
           attr6      TYPE katr6, "ATTRIBUTE 6
           attr7      TYPE katr7, "ATTRIBUTE 7
           attr8      TYPE katr8, "ATTRIBUTE 8
           attr9      TYPE katr9, "ATTRIBUTE 9
           attr10     TYPE katr10, "ATTRIBUTE 10
           pf_ns      TYPE pernr_d, "PF1
           pf_rs      TYPE pernr_d, "PF2
           pf_as      TYPE pernr_d, "PF3
           pf_so      TYPE pernr_d, "PF4
           pf_sk      TYPE kunnr, "PF5
           pf_za      TYPE pernr_d, "PF6
           pf_zc      TYPE pernr_d, "PF7
           pf_zh      TYPE pernr_d, "PF8
           pf_zs      TYPE pernr_d, "PF9
           lprio      TYPE lprio_di,
           pltyp      TYPE pltyp,
         END OF ty_bp_all.
  DATA: gt_raw TYPE TABLE OF truxs_t_text_data.
  DATA: gt_data   TYPE TABLE OF ty_bp_all,
        gs_data   TYPE ty_bp_all,
        ls_bp_all TYPE ty_bp_all.
  DATA: lt_domain TYPE TABLE OF dd07v.
  DATA: ls_error TYPE cvis_message.
  DATA: ls_msg TYPE bapiret2.
  DATA: lv_msg TYPE bapiret2-message.
  DATA: lt_t005 TYPE TABLE OF t005.
  DATA: lv_cntry  TYPE p1028-land1,
        lv_pcode  TYPE  p1028-pstlz,
        lv_region TYPE p1028-regio.
  DATA: return TYPE TABLE OF bapiret2.
  TYPES: BEGIN OF ty_hrp1001_obj,
           objid TYPE hrp1001-objid,
         END OF ty_hrp1001_obj.
  DATA: lv_pernr TYPE pernr_d.
  DATA: lv_pernr_cp TYPE hrp1001-objid.
  DATA: lv_pernr_bp TYPE hrp1001-objid.
  DATA: lt_hrp1001_obj TYPE TABLE OF ty_hrp1001_obj.
  REFRESH: lt_domain,return.
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'BU_TYPE'
    TABLES
      values_tab = lt_domain[].
  DATA: lv_r2 TYPE xfeld.
  CLEAR ls_bp_all.
  IMPORT ls_bp_all TO gs_data FROM MEMORY ID 'FM_CHECK'.
  FREE MEMORY ID 'FM_CHECK'.

  IMPORT lv_r2 TO lv_r2 FROM MEMORY ID 'EXTN'.
  FREE MEMORY ID 'EXTN'.

***** ALL PROCESS IN SINGLE SHOT CREATION VALIDATION *****
  IF gs_data IS NOT INITIAL.
    IF lv_r2 = abap_true.
********************************************************************************************
      IF gs_data-bp IS NOT INITIAL.
        SELECT SINGLE *  FROM  kna1 INTO @DATA(l_kna1) WHERE kunnr = @gs_data-bp.
        IF sy-subrc NE 0.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'BUSINESS PARTNER IS INCORRECT'
                          message_v1 = 'BP' ) TO return.
        ENDIF.
      ELSE.
********************************************************************************************
        IF gs_data-sterm1 IS NOT INITIAL.
          SELECT SINGLE * FROM kna1 INTO @DATA(ls_kna1) WHERE sortl = @gs_data-sterm1.
          IF sy-subrc = 0.
            APPEND VALUE #( type = 'E'
                            number = '002'
                            message = 'SEARCH TERM IS ALREADY EXISTS FOR CUSTOMER'
                            message_v1 = 'STERM1' ) TO return.
          ENDIF.
        ELSE.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'SEARCH TERM IS REQUIRED FIELD'
                          message_v1 = 'STERM1' ) TO return.
        ENDIF.
      ENDIF.
    ENDIF.

********************************************************************************************
    IF gs_data-taxno IS NOT INITIAL.
      SELECT SINGLE *  FROM  dfkkbptaxnum INTO @DATA(ls_stcd3) WHERE taxnumxl = @gs_data-taxno
                                                               AND taxtype = 'IN3'.
      IF sy-subrc EQ 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'GST NUMBER IS ALREADY EXISTS'
                        message_v1 = 'TAXNO' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'GST NUMBER IS MANDATORY'
                      message_v1 = 'TAXNO' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-taxcat IS INITIAL.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'TAX CATEGORY IS MANDATORY'
                      message_v1 = 'TAXCAT' ) TO return.
    ENDIF.

    IF gs_data-country IS NOT INITIAL AND gs_data-pcode IS NOT INITIAL
                                      AND gs_data-region IS NOT INITIAL.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'CHECK COUNTRY REGION POSTALCODE'
                      message_v1 = 'COUNTRY' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-bpcat IS NOT INITIAL.
      DATA(lv_bpcat) = VALUE #( lt_domain[ domvalue_l = gs_data-bpcat ]-domvalue_l OPTIONAL ).
      IF lv_bpcat IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'BUSINESS PARTNER CATEGORY IS INCORRECT'
                        message_v1 = 'BPCAT' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'BUSINESS PARTNER CATEGORY NOT AVAILABLE'
                      message_v1 = 'BPCAT' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-bpgrp IS NOT INITIAL.
      SELECT SINGLE bu_group FROM tb001 INTO @DATA(lv_bpgrp) WHERE bu_group = @gs_data-bpgrp.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'BUSINESS PARTNER GROUP IS INCORRECT'
                        message_v1 = 'BPGRP' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'BUSINESS PARTNER GROUP IS NOT AVAILABLE'
                      message_v1 = 'BPGRP' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-title IS NOT INITIAL.
      SELECT SINGLE * FROM tsad3t INTO @DATA(ls_tsad3t) WHERE title_medi = @gs_data-title.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = | { 'TITLE' } { gs_data-title } { 'DOES NOT EXISTS' } |
                        message_v1 = 'TITLE' )  TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'TITLE IS REQUIRED FIELD'
                      message_v1 = 'TITLE' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-bukrs IS NOT INITIAL.
      DATA: t_record TYPE t001,
            lv_valid TYPE char01.
      CALL FUNCTION 'VALIDATE_COMPANY_CODE'
        EXPORTING
          i_company  = gs_data-bukrs
        IMPORTING
          e_record   = t_record
          e_valid    = lv_valid
        EXCEPTIONS
          incomplete = 1
          OTHERS     = 2.
      IF sy-subrc = 0.
        IF lv_valid <> 1.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'COMPANY CODE DOES NOT EXISTS'
                          message_v1 = 'BUKRS' ) TO return.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                     number = '002'
                     message = 'COMPANY CODE IS REQUIRED FIELD'
                     message_v1 = 'BUKRS' ) TO return.
    ENDIF.

********************************************************************************************
    IF gs_data-bzirk IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_knvv_bzirk_check
        EXPORTING
          iv_bzirk_new        = gs_data-bzirk
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'BZIRK' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'SALES DISTRICT IS REQUIRED FIELD'
                      message_v1 = 'BZIRK' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-kdgrp IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_knvv_kdgrp_check
        EXPORTING
          iv_kdgrp_new        = gs_data-kdgrp
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'KDGRP' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'CUSTOMER GROUP IS REQUIRED FIELD'
                      message_v1 = 'KDGRP' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-vkbur IS NOT INITIAL AND gs_data-vkorg IS NOT INITIAL
            AND gs_data-vtweg IS NOT INITIAL AND gs_data-spart IS NOT INITIAL AND gs_data-vkgrp IS NOT INITIAL.
**** Selection for checking sales org,sales Office, dist chnnel,division *******
      SELECT SINGLE * FROM tvkbz INTO @DATA(ls_sales) WHERE vkorg = @gs_data-vkorg
                                 AND vtweg = @gs_data-vtweg AND spart = @gs_data-spart
                                 AND vkbur = @gs_data-vkbur.
      IF ls_sales IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'PLEASE CHECK ALL SALES DATA'
                        message_v1 = 'VKBUR' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                     number = '002'
                     message = 'CHECK SALES AREA DATA PROPERLY'
                     message_v1 = 'VKBUR' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-ktgrd IS NOT INITIAL.
      CLEAR: ls_error.
      SELECT SINGLE * FROM tvkt INTO @DATA(ls_tvkt) WHERE ktgrd = @gs_data-ktgrd.
      IF ls_tvkt IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'KTGRD' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'ACC ASSIGNMENT GRP IS REQUIRED'
                      message_v1 = 'KTGRD' ) TO return.
    ENDIF.
*** PRICING CHECKS ***
    IF gs_data-kalks IS NOT INITIAL.
      SELECT SINGLE * FROM tvkd INTO @DATA(ls_price1) WHERE kalks = @gs_data-kalks.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'KALKS' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '0002'
                      message = 'PRICING IS REQUIRED FIELD'
                      message_v1 = 'KALKS' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-vsbed IS NOT INITIAL.
      SELECT SINGLE * FROM tvsb INTO @DATA(ls_vsbed) WHERE vsbed = @gs_data-vsbed.
      IF ls_vsbed IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'VSBED' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                     number = '0002'
                     message = 'SHIPPING CONDITION IS REQUIRED'
                     message_v1 = 'VSBED' ) TO return.
    ENDIF.
    IF gs_data-pltyp IS NOT INITIAL.
      SELECT SINGLE * FROM t189 INTO @DATA(ls_price) WHERE pltyp = @gs_data-pltyp.
      IF ls_price IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'PRICE CONDITION IS INCORRECT'
                        message_v1 = 'PLTYP' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'PRICE CONDITION IS INCORRECT'
                      message_v1 = 'PLTYP' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-taxkd01 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_knvi_tatyp_taxkd_check
        EXPORTING
          iv_tatyp_new        = 'JOSG'
          iv_taxkd_new        = gs_data-taxkd01
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'TAXKD01' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'JOSG TAX CLASSIFICATION IS REQUIRED'
                      message_v1 = 'TAXKD01' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-taxkd02 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_knvi_tatyp_taxkd_check
        EXPORTING
          iv_tatyp_new        = 'JOCG'
          iv_taxkd_new        = gs_data-taxkd02
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'TAXKD02'  ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'JOCG TAX CLASSIFICATION IS REQUIRED'
                      message_v1 = 'TAXKD02' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-taxkd03 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_knvi_tatyp_taxkd_check
        EXPORTING
          iv_tatyp_new        = 'JOIG'
          iv_taxkd_new        = gs_data-taxkd03
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'TAXKD03' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'JOIG TAX CLASSIFICATION IS REQUIRED'
                      message_v1 = 'TAXKD03' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-taxkd04 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_knvi_tatyp_taxkd_check
        EXPORTING
          iv_tatyp_new        = 'JOUG'
          iv_taxkd_new        = gs_data-taxkd04
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'TAXKD04' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'JOUG TAX CLASSIFICATION IS REQUIRED'
                      message_v1 = 'TAXKD04' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-taxkd05 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_knvi_tatyp_taxkd_check
        EXPORTING
          iv_tatyp_new        = 'JCOS'
          iv_taxkd_new        = gs_data-taxkd05
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'TAXKD05' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'JCOS TAX CLASSIFICATION IS REQUIRED'
                      message_v1 = 'TAXKD05' ) TO return.
    ENDIF.
********************************************************************************************
    IF gs_data-taxkd06 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_knvi_tatyp_taxkd_check
        EXPORTING
          iv_tatyp_new        = 'JTC1'
          iv_taxkd_new        = gs_data-taxkd06
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'TAXKD06' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'JTC1 TAX CLASSIFICATION IS REQUIRED'
                      message_v1 = 'TAXKD06' ) TO return.
    ENDIF.
*********************************************************************************************
    IF gs_data-attr1 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr1_check
        EXPORTING
          iv_katr1_new        = gs_data-attr1
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR1' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF gs_data-attr2 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr2_check
        EXPORTING
          iv_katr2_new        = gs_data-attr2
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR2' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF gs_data-attr3 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr3_check
        EXPORTING
          iv_katr3_new        = gs_data-attr3
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR3' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF gs_data-attr4 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr4_check
        EXPORTING
          iv_katr4_new        = gs_data-attr4
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR4' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF gs_data-attr5 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr5_check
        EXPORTING
          iv_katr5_new        = gs_data-attr5
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR5' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF gs_data-attr6 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr6_check
        EXPORTING
          iv_katr6_new        = gs_data-attr6
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR6' ) TO return.
      ENDIF.
    ENDIF.
********************************************************************************************
    IF gs_data-attr7 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr7_check
        EXPORTING
          iv_katr7_new        = gs_data-attr7
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR7' ) TO return.
      ENDIF.
    ENDIF.

    IF gs_data-attr8 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr8_check
        EXPORTING
          iv_katr8_new        = gs_data-attr8
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR8' ) TO return.
      ENDIF.
    ENDIF.

    IF gs_data-attr9 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr9_check
        EXPORTING
          iv_katr9_new        = gs_data-attr9
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR9' ) TO return.
      ENDIF.
    ENDIF.

    IF gs_data-attr10 IS NOT INITIAL.
      CLEAR ls_error.
      CALL METHOD cmd_ei_api_check=>field_kna1_katr10_check
        EXPORTING
          iv_katr10_new       = gs_data-attr10
          iv_collect_messages = 'X'
        IMPORTING
          es_error            = ls_error.
      IF ls_error-is_error = abap_true.
        CLEAR lv_msg.
        LOOP AT ls_error-messages INTO ls_msg.
          lv_msg = ls_msg-message.
          lv_msg = | { lv_msg } { ls_msg-message } |.
          CLEAR ls_msg.
        ENDLOOP.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = lv_msg
                        message_v1 = 'ATTR10' ) TO return.
      ENDIF.
    ENDIF.
************************************************************************************************************
*** PARTNER FUNCTION  ****
    SELECT pernr FROM pa0000
                 INTO TABLE @DATA(lt_pa0000)
                 WHERE begda LE @sy-datum
                 AND endda GE @sy-datum
                 AND stat2 = '3'.
    IF sy-subrc = 0.
      SORT lt_pa0000[] BY pernr.
*Get CP based on pernr
      REFRESH: lt_hrp1001_obj.
      SELECT objid, sobid
        FROM hrp1001
        INTO TABLE @DATA(lt_hrp1001_cp)
        FOR ALL ENTRIES IN @lt_pa0000
        WHERE otype = 'P' AND objid = @lt_pa0000-pernr
        AND plvar = '01' AND rsign = 'A'
        AND relat = '209' AND istat = '1'
        AND begda LE @sy-datum
        AND endda GE @sy-datum
        AND sclas = 'CP'.
      IF sy-subrc = 0.
        LOOP AT lt_hrp1001_cp INTO DATA(ls_hrp1001_cp).
          APPEND VALUE #( objid = ls_hrp1001_cp-sobid+0(8) ) TO lt_hrp1001_obj.
        ENDLOOP.
*Get CP based on pernr
        SELECT objid, sobid
          FROM hrp1001
          INTO TABLE @DATA(lt_hrp1001_bp)
          FOR ALL ENTRIES IN @lt_hrp1001_obj
          WHERE otype = 'CP' AND objid = @lt_hrp1001_obj-objid
          AND plvar = '01' AND rsign = 'B'
          AND relat = '207' AND istat = '1'
          AND begda LE @sy-datum
          AND endda GE @sy-datum
          AND sclas = 'BP'.
      ENDIF.
    ENDIF.
*************NS Partner Function ********************************************************************************
    IF gs_data-pf_ns IS NOT INITIAL.
      CLEAR lv_pernr.
      lv_pernr = VALUE #( lt_pa0000[ pernr = gs_data-pf_ns ]-pernr OPTIONAL ).
      IF lv_pernr IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'NS PARTNER FUNCTION PERNR INCORRECT'
                        message_v1 = 'PF_NS' ) TO return.
      ELSE.
        CLEAR: lv_pernr_cp,lv_pernr_bp.
        lv_pernr_cp = VALUE #( lt_hrp1001_cp[ objid = lv_pernr ]-sobid OPTIONAL ).
        IF lv_pernr_cp IS INITIAL.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'NS PARTNER FUNCTION PERNR INCORRECT'
                          message_v1 = 'PF_NS' ) TO return.
        ELSE.
          lv_pernr_bp = VALUE #( lt_hrp1001_bp[ objid = lv_pernr_cp ]-sobid OPTIONAL ).
          IF lv_pernr_bp IS INITIAL.
            APPEND VALUE #( type = 'E'
                            number = '002'
                            message = 'NS PARTNER FUNCTION PERNR INCORRECT'
                            message_v1 = 'PF_NS' ) TO return.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'NS PARTNER FUNCTION IS REQUIRED'
                      message_v1 = 'PF_NS' ) TO return.
    ENDIF.
*************NS Partner Function ********************************************************************************
    IF gs_data-pf_rs IS NOT INITIAL.
      CLEAR lv_pernr.
      lv_pernr = VALUE #( lt_pa0000[ pernr = gs_data-pf_rs ]-pernr OPTIONAL ).
      IF lv_pernr IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'RS PARTNER FUNCTION PERNR INCORRECT'
                        message_v1 = 'PF_RS' ) TO return.
      ELSE.
        CLEAR: lv_pernr_cp,lv_pernr_bp.
        lv_pernr_cp = VALUE #( lt_hrp1001_cp[ objid = lv_pernr ]-sobid OPTIONAL ).
        IF lv_pernr_cp IS INITIAL.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'RS PARTNER FUNCTION PERNR INCORRECT'
                          message_v1 = 'PF_RS' ) TO return.
        ELSE.
          lv_pernr_bp = VALUE #( lt_hrp1001_bp[ objid = lv_pernr_cp ]-sobid OPTIONAL ).
          IF lv_pernr_bp IS INITIAL.
            APPEND VALUE #( type = 'E'
                            number = '002'
                            message = 'RS PARTNER FUNCTION PERNR INCORRECT'
                            message_v1 = 'PF_RS' ) TO return.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'RS PARTNER FUNCTION PERNR IS REQUIRED'
                      message_v1 = 'PF_RS' ) TO return.
    ENDIF.

    IF gs_data-pf_as IS NOT INITIAL.
      CLEAR lv_pernr.
      lv_pernr = VALUE #( lt_pa0000[ pernr = gs_data-pf_as ]-pernr OPTIONAL ).
      IF lv_pernr IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'AS PARTNER FUNCTION PERNR INCORRECT'
                        message_v1 = 'PF_AS' ) TO return.
      ELSE.
        CLEAR: lv_pernr_cp,lv_pernr_bp.
        lv_pernr_cp = VALUE #( lt_hrp1001_cp[ objid = lv_pernr ]-sobid OPTIONAL ).
        IF lv_pernr_cp IS INITIAL.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'AS PARTNER FUNCTION PERNR INCORRECT'
                          message_v1 = 'PF_AS' ) TO return.
        ELSE.
          lv_pernr_bp = VALUE #( lt_hrp1001_bp[ objid = lv_pernr_cp ]-sobid OPTIONAL ).
          IF lv_pernr_bp IS INITIAL.
            APPEND VALUE #( type = 'E'
                            number = '002'
                            message = 'AS PARTNER FUNCTION PERNR INCORRECT'
                            message_v1 = 'PF_AS' ) TO return.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                     number = '002'
                     message = 'AS PARTNER FUNCTION PERNR IS REQUIRED'
                     message_v1 = 'PF_AS' ) TO return.
    ENDIF.
*************NS Partner Function ********************************************************************************
    IF gs_data-pf_so IS NOT INITIAL.
      CLEAR lv_pernr.
      lv_pernr = VALUE #( lt_pa0000[ pernr = gs_data-pf_so ]-pernr OPTIONAL ).
      IF lv_pernr IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'SO PARTNER FUNCTION PERNR INCORRECT'
                        message_v1 = 'PF_SO' ) TO return.
      ELSE.
        CLEAR: lv_pernr_cp,lv_pernr_bp.
        lv_pernr_cp = VALUE #( lt_hrp1001_cp[ objid = lv_pernr ]-sobid OPTIONAL ).
        IF lv_pernr_cp IS INITIAL.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'SO PARTNER FUNCTION PERNR INCORRECT'
                          message_v1 = 'PF_SO' ) TO return.
        ELSE.
          lv_pernr_bp = VALUE #( lt_hrp1001_bp[ objid = lv_pernr_cp ]-sobid OPTIONAL ).
          IF lv_pernr_bp IS INITIAL.
            APPEND VALUE #( type = 'E'
                            number = '002'
                            message = 'SO PARTNER FUNCTION PERNR INCORRECT'
                            message_v1 = 'PF_SO' ) TO return.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'SO PARTNER FUNCTION PERNR IS REQUIRED'
                      message_v1 = 'PF_SO' ) TO return.
    ENDIF.
*************NS Partner Function ********************************************************************************
    IF gs_data-pf_sk IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_data-pf_sk
        IMPORTING
          output = gs_data-pf_sk.

      SELECT SINGLE kunnr FROM kna1 INTO @DATA(lv_kunnr) WHERE kunnr = @gs_data-pf_sk.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'SK PARTNER NUMBER IS INCORRECT'
                        message_v1 = 'PF_SK' ) TO return.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'SK PARTNER NUMBER IS REQUIRED'
                      message_v1 = 'PF_SK' ) TO return.
    ENDIF.
*************NS Partner Function ********************************************************************************
    IF gs_data-pf_za IS NOT INITIAL.
      CLEAR lv_pernr.
      lv_pernr = VALUE #( lt_pa0000[ pernr = gs_data-pf_za ]-pernr OPTIONAL ).
      IF lv_pernr IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'ZA PARTNER FUNCTION PERNR INCORRECT'
                        message_v1 = 'PF_ZA' ) TO return.
      ELSE.
        CLEAR: lv_pernr_cp,lv_pernr_bp.
        lv_pernr_cp = VALUE #( lt_hrp1001_cp[ objid = lv_pernr ]-sobid OPTIONAL ).
        IF lv_pernr_cp IS INITIAL.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'ZA PARTNER FUNCTION PERNR INCORRECT'
                          message_v1 = 'PF_ZA' ) TO return.
        ELSE.
          lv_pernr_bp = VALUE #( lt_hrp1001_bp[ objid = lv_pernr_cp ]-sobid OPTIONAL ).
          IF lv_pernr_bp IS INITIAL.
            APPEND VALUE #( type = 'E'
                            number = '002'
                            message = 'ZA PARTNER FUNCTION PERNR INCORRECT'
                            message_v1 = 'PF_ZA' ) TO return.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'ZA PARTNER FUNCTION PERNR IS REQUIRED'
                      message_v1 = 'PF_ZA' ) TO return.
    ENDIF.
*************NS Partner Function ********************************************************************************
    IF gs_data-pf_zc IS NOT INITIAL.
      CLEAR lv_pernr.
      lv_pernr = VALUE #( lt_pa0000[ pernr = gs_data-pf_zc ]-pernr OPTIONAL ).
      IF lv_pernr IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'ZC PARTNER FUNCTION PERNR INCORRECT'
                        message_v1 = 'PF_ZC' ) TO return.
      ELSE.
        CLEAR: lv_pernr_cp,lv_pernr_bp.
        lv_pernr_cp = VALUE #( lt_hrp1001_cp[ objid = lv_pernr ]-sobid OPTIONAL ).
        IF lv_pernr_cp IS INITIAL.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'ZC PARTNER FUNCTION PERNR INCORRECT'
                          message_v1 = 'PF_ZC' ) TO return.
        ELSE.
          lv_pernr_bp = VALUE #( lt_hrp1001_bp[ objid = lv_pernr_cp ]-sobid OPTIONAL ).
          IF lv_pernr_bp IS INITIAL.
            APPEND VALUE #( type = 'E'
                            number = '002'
                            message = 'ZC PARTNER FUNCTION PERNR INCORRECT'
                            message_v1 = 'PF_ZC' ) TO return.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'ZC PARTNER FUNCTION PERNR IS REQUIRED'
                      message_v1 = 'PF_ZC' ) TO return.
    ENDIF.
*************NS Partner Function ********************************************************************************
    IF gs_data-pf_zh IS NOT INITIAL.
      CLEAR lv_pernr.
      lv_pernr = VALUE #( lt_pa0000[ pernr = gs_data-pf_zh ]-pernr OPTIONAL ).
      IF lv_pernr IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'ZH PARTNER FUNCTION PERNR INCORRECT'
                        message_v1 = 'PF_ZH' ) TO return.
      ELSE.
        CLEAR: lv_pernr_cp,lv_pernr_bp.
        lv_pernr_cp = VALUE #( lt_hrp1001_cp[ objid = lv_pernr ]-sobid OPTIONAL ).
        IF lv_pernr_cp IS INITIAL.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'ZH PARTNER FUNCTION PERNR INCORRECT'
                          message_v1 = 'PF_ZH' ) TO return.
        ELSE.
          lv_pernr_bp = VALUE #( lt_hrp1001_bp[ objid = lv_pernr_cp ]-sobid OPTIONAL ).
          IF lv_pernr_bp IS INITIAL.
            APPEND VALUE #( type = 'E'
                            number = '002'
                            message = 'ZH PARTNER FUNCTION PERNR INCORRECT'
                            message_v1 = 'PF_ZH' ) TO return.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'ZH PARTNER FUNCTION PERNR IS REQUIRED'
                      message_v1 = 'PF_ZH' ) TO return.
    ENDIF.
*************NS Partner Function ********************************************************************************
    IF gs_data-pf_zs IS NOT INITIAL.
      CLEAR lv_pernr.
      lv_pernr = VALUE #( lt_pa0000[ pernr = gs_data-pf_zs ]-pernr OPTIONAL ).
      IF lv_pernr IS INITIAL.
        APPEND VALUE #( type = 'E'
                        number = '002'
                        message = 'ZS PARTNER FUNCTION PERNR INCORRECT'
                        message_v1 = 'PF_ZS' ) TO return.
      ELSE.
        CLEAR: lv_pernr_cp,lv_pernr_bp.
        lv_pernr_cp = VALUE #( lt_hrp1001_cp[ objid = lv_pernr ]-sobid OPTIONAL ).
        IF lv_pernr_cp IS INITIAL.
          APPEND VALUE #( type = 'E'
                          number = '002'
                          message = 'ZS PARTNER FUNCTION PERNR INCORRECT'
                          message_v1 = 'PF_ZS' ) TO return.
        ELSE.
          lv_pernr_bp = VALUE #( lt_hrp1001_bp[ objid = lv_pernr_cp ]-sobid OPTIONAL ).
          IF lv_pernr_bp IS INITIAL.
            APPEND VALUE #( type = 'E'
                            number = '002'
                            message = 'ZS PARTNER FUNCTION PERNR INCORRECT'
                            message_v1 = 'PF_ZS' ) TO return.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( type = 'E'
                      number = '002'
                      message = 'ZS PARTNER FUNCTION PERNR IS REQUIRED'
                      message_v1 = 'PF_ZS' ) TO return.
    ENDIF.
    APPEND LINES OF return TO lt_return.
  ENDIF.
*************NS Partner Function ********************************************************************************
ENDFUNCTION.
