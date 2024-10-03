*&---------------------------------------------------------------------*
*& Include          ZSD_CUSTOMER_MASTER_FORMS
*&---------------------------------------------------------------------*
FORM value_request USING p_flname TYPE localfile.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name = sy-cprog
    IMPORTING
      file_name    = p_flname.
ENDFORM.
*** Excel Conversion for for all creation steps ***
FORM upload_xl1.
  REFRESH: gt_bp_all.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = gt_raw
      i_filename           = p_flname
    TABLES
      i_tab_converted_data = gt_bp_all.
ENDFORM.
FORM upload_xl2.
  REFRESH: gt_bp_create.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = gt_raw
      i_filename           = p_flname
    TABLES
      i_tab_converted_data = gt_bp_create.
ENDFORM.
FORM f_create_bp_all.
  DATA: lv_r2 TYPE xfeld.
  LOOP AT gt_bp_all ASSIGNING FIELD-SYMBOL(<fs_process>).
    CLEAR: ls_bp_all.
    MOVE-CORRESPONDING <fs_process> TO ls_bp_all.
    EXPORT ls_bp_all TO MEMORY ID 'FM_CHECK'.
    PERFORM f_fcall_error_check.
    IF i_return IS NOT INITIAL.
      LOOP AT i_return INTO DATA(is_ret).
        DATA(lv_rem) = is_ret-message_v1.
        lv_rem = | { lv_rem } { ',' } { is_ret-message_v1 } |.
      ENDLOOP.
      REPLACE ALL OCCURRENCES OF 'gs_data-' IN lv_rem WITH ''.
      CONDENSE lv_rem NO-GAPS.
      DATA(ls_alverr) = VALUE zsd_bp_creation( msgtyp = 'E'
                                               sortl = <fs_process>-sterm1
                                               bpname = <fs_process>-fname
                                               msg = | { 'Error in line of excel in search term' } { <fs_process>-sterm1 } |
                                               remarks = lv_rem ).
      APPEND ls_alverr TO gt_log.
    ELSE.
      CLEAR: businesspartner,gv_error,gv_bpcat.
      gv_bpcat = <fs_process>-bpcat.
      partnercategory = <fs_process>-bpcat.
      partnergroup = <fs_process>-bpgrp.
*** Central Data filling for business Partner ***
      SELECT SINGLE title FROM tsad3t INTO @DATA(lv_title) WHERE title_medi = @<fs_process>-title.
      DATA(ls_central) = VALUE bapibus1006_central( searchterm1 = <fs_process>-sterm1
                                                    title_key = lv_title ).

      IF partnercategory = 2.
        DATA(lv_name1) = <fs_process>-fname.
        DATA(lv_name2) = <fs_process>-lname.
        centraldataorganization-name1 = lv_name1.
        centraldataorganization-name2 = lv_name2.
        centraldataorganization-name3 = <fs_process>-name3.
        centraldataorganization-name4 = <fs_process>-name4.

      ELSEIF partnercategory = 1.
        centraldataperson-firstname = <fs_process>-fname.
        centraldataperson-lastname = <fs_process>-lname.
        centraldataperson-namcountry = <fs_process>-country.
        centraldataperson-correspondlanguage = 'EN'.
      ENDIF.

**** Adddress data filling in Business Partner Creation **
      DATA(ls_address1) = VALUE bapibus1006_address( street = <fs_process>-street
                                                    str_suppl1 = <fs_process>-addr3
                                                    str_suppl2 = <fs_process>-addr2
                                                    str_suppl3 = <fs_process>-addr3
                                                    house_no = <fs_process>-houseno
                                                    district = <fs_process>-district
                                                    postl_cod1 = <fs_process>-pcode
                                                    city = <fs_process>-city
                                                    country = <fs_process>-country
                                                    region = <fs_process>-region
                                                    langu = 'E' ).
      REFRESH: it_telephondata.
      it_telephondata[] = VALUE #( ( country = <fs_process>-country
                                     telephone = <fs_process>-telno
                                     std_no = 'X'
                                     r_3_user = '3'
                                     home_flag = 'X' )
                                   ( country = <fs_process>-country
                                     telephone = <fs_process>-mobile
                                     std_no = 'X'
                                     r_3_user = '1'
                                     home_flag = 'X' ) ).
      REFRESH: it_maildata.
      it_maildata[] = VALUE #( ( e_mail = <fs_process>-email
                                 std_no = 'X'
                                 std_recip = 'X'
                                 home_flag = 'X'
                                 consnumber = '001' ) ).
*** Bapi to create Business Partner ***
      REFRESH: return.
      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          partnercategory         = partnercategory
          partnergroup            = partnergroup
          centraldata             = ls_central
          centraldataperson       = centraldataperson
          centraldataorganization = centraldataorganization
          addressdata             = ls_address1
        IMPORTING
          businesspartner         = businesspartner
        TABLES
          telefondata             = it_telephondata
          e_maildata              = it_maildata
          return                  = return.

      READ TABLE return INTO DATA(ls_ret) WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        gv_error = abap_true.
        LOOP AT  return INTO DATA(ls_return) WHERE type = 'E'.
          APPEND VALUE #( sortl = <fs_process>-sterm1
                          msg  =  ls_return-message
                          msgtyp = ls_return-type
                          msgnr = ls_return-number
                          uzeit = sy-uzeit
                          erdat = sy-datum
                          uname = sy-uname ) TO gt_log.

        ENDLOOP.
      ELSE.
        IF businesspartner IS NOT INITIAL AND gv_error IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          DATA(ls_log) = VALUE zsd_bp_creation( bu_partner = businesspartner
                                                bu_partnerx = 'B'
                                                msgtyp = 'S'
                                                msg = 'Business Partner Created Successfully'
                                                uzeit = sy-uzeit
                                                erdat = sy-datum
                                                uname = sy-uname
                                                msgnr = '001'
                                                bpname = <fs_process>-fname
                                                sortl = ls_central-searchterm1 ).
          APPEND ls_log TO gt_log.
          MODIFY zsd_bp_creation FROM ls_log.
**** Adding role to the Business Partner ***
          PERFORM add_role_bp.
**** Adding the tax and Tax number ***
          IF <fs_process>-taxno IS NOT INITIAL AND <fs_process>-taxcat IS NOT INITIAL.
            DATA: t_return TYPE TABLE OF bapiret2.
            REFRESH t_return.
            CALL FUNCTION 'BAPI_BUPA_TAX_ADD'
              EXPORTING
                businesspartner = businesspartner
                taxtype         = <fs_process>-taxcat
                taxnumber       = <fs_process>-taxno
              TABLES
                return          = t_return.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.
***** Extending Sales Area and company code for Newly created Business Partner ****
          REFRESH: lt_tax_data,is_master_data-customers,lt_customers[],lt_sales[],lt_sales_func,lt_company[].
          CLEAR: ls_tax_data,ls_sales_func,ls_customers,ls_address,is_master_data,ls_sales_data,lt_sales,ls_company_data.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = businesspartner
            IMPORTING
              output = ls_customers-header-object_instance-kunnr.
          CLEAR kna1.
          SELECT SINGLE * FROM kna1 WHERE kunnr = ls_customers-header-object_instance-kunnr.
          ls_customers-header-object_task = 'U'. "Represents Insert or Create
          ls_address-postal-data-name          = kna1-name1. "Name of the Vendor
          ls_address-postal-data-country       = kna1-land1.
          ls_address-postal-datax-name         = 'X'.
          ls_address-postal-datax-country      = 'X'.
          ls_address-task                      = 'U'.
*   set the address for the vendor
          ls_customers-central_data-address = ls_address.
          DATA(ls_company) = VALUE cmds_ei_company( task = 'I'
                                                    data_key = <fs_process>-bukrs
                                                    data-zterm =  <fs_process>-zterm
                                                    datax-akont = 'X'
                                                    datax-zterm = 'X' ).
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = <fs_process>-akont
            IMPORTING
              output = ls_company-data-akont.
          APPEND ls_company TO lt_company.
          ls_company_data-company = lt_company[].
          ls_customers-company_data = ls_company_data.
*        ls_sales-task = 'I'.
          DATA(ls_sales) = VALUE cmds_ei_sales(  task = 'I'
                                                 data_key-vkorg = <fs_process>-vkorg
                                                 data_key-vtweg = <fs_process>-vtweg
                                                 data_key-spart = <fs_process>-spart
                                                 data-bzirk = <fs_process>-bzirk
                                                 data-kdgrp = <fs_process>-kdgrp
                                                 data-ktgrd = <fs_process>-ktgrd
                                                 data-vkbur = <fs_process>-vkbur
                                                 data-vkgrp = <fs_process>-vkgrp
                                                 data-kalks = <fs_process>-kalks
                                                 data-incov = <fs_process>-incov
                                                 data-inco1 = <fs_process>-inco1
                                                 data-inco2 = <fs_process>-inco2
                                                 data-vwerk = <fs_process>-vwerk
                                                 data-vsbed = <fs_process>-vsbed
                                                 data-zterm = <fs_process>-zterm
                                                 data-inco2_l = <fs_process>-inco_l
                                                 data-lprio = <fs_process>-lprio
                                                 data-pltyp = <fs_process>-pltyp
                                                 data-waers = 'INR'
                                                 datax-bzirk = 'X'
                                                 datax-kdgrp = 'X'
                                                 datax-ktgrd = 'X'
                                                 datax-vkbur = 'X'
                                                 datax-vkgrp = 'X'
                                                 datax-kalks = 'X'
                                                 datax-incov = 'X'
                                                 datax-inco1 = 'X'
                                                 datax-inco2 = 'X'
                                                 datax-vwerk = 'X'
                                                 datax-vsbed = 'X'
                                                 datax-zterm = 'X'
                                                 datax-waers = 'X'
                                                 datax-inco2_l = 'X'
                                                 datax-lprio = 'X'
                                                 datax-pltyp = 'X' ).
          DATA iv_ktokd TYPE ktokd. " Customer Account Group
          DATA et_parvw TYPE cmds_parvw_t.
          REFRESH et_parvw.
          REFRESH lt_sales_func.
          CALL METHOD cmd_ei_api_check=>get_mand_partner_functions
            EXPORTING
              iv_ktokd = kna1-ktokd
            IMPORTING
              et_parvw = et_parvw.

          LOOP AT et_parvw INTO DATA(wa_parvw).
            ls_sales_func-task = 'I'.
            ls_sales_func-data_key-parvw = wa_parvw-parvw.
            CASE wa_parvw-parvw.
              WHEN 'AG'.
                ls_sales_func-data-partner = kna1-kunnr.
              WHEN 'WE'.
                ls_sales_func-data-partner = kna1-kunnr.
              WHEN 'RG'.
                ls_sales_func-data-partner = kna1-kunnr.
              WHEN 'RE'.
                ls_sales_func-data-partner = kna1-kunnr.
              WHEN 'L1'.
                ls_sales_func-data-partner = <fs_process>-pf_ns.
              WHEN 'L2'.
                ls_sales_func-data-partner = <fs_process>-pf_rs.
              WHEN 'L3'.
                ls_sales_func-data-partner = <fs_process>-pf_as.
              WHEN 'L5'.
                ls_sales_func-data-partner = <fs_process>-pf_so.
              WHEN 'ZA'.
                ls_sales_func-data-partner = <fs_process>-pf_za.
              WHEN 'ZC'.
                ls_sales_func-data-partner = <fs_process>-pf_zc.
              WHEN 'ZH'.
                ls_sales_func-data-partner = <fs_process>-pf_zh.
              WHEN 'ZS'.
                ls_sales_func-data-partner = <fs_process>-pf_zs.
              WHEN OTHERS.
            ENDCASE.
            ls_sales_func-datax-partner = 'X'.
            APPEND ls_sales_func TO lt_sales_func.
            CLEAR ls_sales_func.
          ENDLOOP. " LOOP AT et_parvw INTO DATA(wa_parvw)
          ls_sales_func-task = 'I'.
          ls_sales_func-data_key-parvw = 'SK'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = <fs_process>-pf_sk
            IMPORTING
              output = <fs_process>-pf_sk.
          ls_sales_func-data-partner = <fs_process>-pf_sk.
          ls_sales_func-datax-partner = 'X'.
          APPEND ls_sales_func TO lt_sales_func.
          CLEAR ls_sales_func.
          ls_sales-functions-functions = lt_sales_func[].
          APPEND ls_sales TO lt_sales.

          ls_sales_data-sales = lt_sales[].
          SELECT SINGLE kunnr FROM knvv INTO @DATA(l_kunnr) WHERE kunnr = @ls_customers-header-object_instance-kunnr.
          IF sy-subrc <> 0.
            " Adding tax_data

            CLEAR gwa_knvi.
            SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                      AND aland = 'IN'
                                                      AND tatyp = 'JOIG' .
            IF gwa_knvi IS INITIAL.
              APPEND VALUE #( task = 'I'
                              data_key-aland = 'IN'
                              data_key-tatyp = 'JOIG'
                              data-taxkd = <fs_process>-taxkd03
                              datax-taxkd = 'X' ) TO lt_tax_data.
            ENDIF.

            CLEAR gwa_knvi.
            SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                      AND aland = 'IN'
                                                      AND tatyp = 'JOCG' .

            IF gwa_knvi IS INITIAL.
              APPEND VALUE #( task = 'I'
                  data_key-aland = 'IN'
                  data_key-tatyp = 'JOCG'
                  data-taxkd = <fs_process>-taxkd02
                  datax-taxkd = 'X' ) TO lt_tax_data.
            ENDIF.

            CLEAR gwa_knvi.

            SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                      AND aland = 'IN'
                                                      AND tatyp = 'JOSG' .

            IF gwa_knvi IS INITIAL.
              APPEND VALUE #( task = 'I'
                              data_key-aland = 'IN'
                              data_key-tatyp = 'JOSG'
                              data-taxkd = <fs_process>-taxkd01
                              datax-taxkd = 'X' ) TO lt_tax_data.
            ENDIF.


            CLEAR gwa_knvi.
            SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                      AND aland = 'IN'
                                                      AND tatyp = 'JOUG' .

            IF gwa_knvi IS INITIAL.
              APPEND VALUE #( task = 'I'
                            data_key-aland = 'IN'
                            data_key-tatyp = 'JOUG'
                            data-taxkd = <fs_process>-taxkd04
                            datax-taxkd = 'X' ) TO lt_tax_data.
            ENDIF.

            CLEAR gwa_knvi.
            SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                      AND aland = 'IN'
                                                      AND tatyp = 'JTC1' .

            IF gwa_knvi IS INITIAL.
              APPEND VALUE #( task = 'I'
                            data_key-aland = 'IN'
                            data_key-tatyp = 'JTC1'
                            data-taxkd = <fs_process>-taxkd06
                            datax-taxkd = 'X' ) TO lt_tax_data.
            ENDIF.
            CLEAR gwa_knvi.
            SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = businesspartner
                                                      AND aland = 'IN'
                                                      AND tatyp = 'JCOS' .

            IF gwa_knvi IS INITIAL.
              APPEND VALUE #( task = 'I'
                            data_key-aland = 'IN'
                            data_key-tatyp = 'JCOS'
                            data-taxkd = <fs_process>-taxkd05
                            datax-taxkd = 'X' ) TO lt_tax_data.
            ENDIF.

            ls_central_data-tax_ind-tax_ind = lt_tax_data.
          ENDIF. " IF sy-subrc <> 0
          ls_customers-central_data = ls_central_data.
          ls_customers-sales_data = ls_sales_data.
          APPEND ls_customers TO lt_customers.
          is_master_data-customers = lt_customers[].
          cmd_ei_api=>initialize( ).
          CALL METHOD cmd_ei_api=>maintain_bapi
            EXPORTING
*             IV_TEST_RUN              = SPACE
*             IV_COLLECT_MESSAGES      = SPACE
              is_master_data           = is_master_data
            IMPORTING
              es_master_data_correct   = es_master_data_correct
              es_message_correct       = es_message_correct
              es_master_data_defective = es_master_data_defective
              es_message_defective     = es_message_defective.
          IF es_message_defective-is_error IS INITIAL.
            COMMIT WORK.
            WAIT UP TO 1 SECONDS.
            DATA(ls_sarea) = VALUE zsd_bp_creation(
                           bu_partner = businesspartner
                           bu_partnerx = 'C'
                           msg = | { 'Customer Extended to sales area and comp code' } { <fs_process>-vkorg } { <fs_process>-vtweg } { <fs_process>-spart } |
                           msgtyp = 'S'
                           uzeit = sy-uzeit
                           uname = sy-uname
                           sa_extend = 'X'
                           cc_extend = 'X'
                           msgnr = '002'
                           bpname = kna1-name1 ).
            APPEND ls_sarea TO gt_log.
            UPDATE zsd_bp_creation SET sa_extend = 'X'
                                       cc_extend = 'X'
                                       WHERE bu_partner = businesspartner.
**** Updating additiona data like Attributes for Customer ***
            DATA: ls_kna1_up TYPE kna1.
            SELECT SINGLE * FROM kna1 INTO @DATA(l_kna1) WHERE kunnr = @businesspartner.
            IF sy-subrc = 0.
              MOVE-CORRESPONDING l_kna1 TO ls_kna1_up.
              ls_kna1_up-stcd3 = <fs_process>-taxno.
              ls_kna1_up-katr1 = <fs_process>-attr1.
              ls_kna1_up-katr2 = <fs_process>-attr2.
              ls_kna1_up-katr3 = <fs_process>-attr3.
              ls_kna1_up-katr4 = <fs_process>-attr4.
              ls_kna1_up-katr5 = <fs_process>-attr5.
              ls_kna1_up-katr6 = <fs_process>-attr6.
              ls_kna1_up-katr7 = <fs_process>-attr7.
              ls_kna1_up-katr8 = <fs_process>-attr8.
              ls_kna1_up-katr9 = <fs_process>-attr9.
              ls_kna1_up-katr10 = <fs_process>-attr10.

              CALL FUNCTION 'J_3G_KNA1_MODIFY'
                EXPORTING
                  i_kna1 = ls_kna1_up.
            ENDIF.
          ELSE.
            LOOP AT es_message_defective-messages INTO DATA(wa).
              WAIT UP TO 1 SECONDS.
              DATA(lv_msg) = wa-message.
              lv_msg = | { lv_msg } { wa-message } |.
            ENDLOOP.
            DATA(ls_error) = VALUE zsd_bp_creation( bu_partner = businesspartner
                                                    bu_partnerx = 'C'
                                                    msg  = lv_msg
                                                    msgtyp = wa-type
                                                    msgnr = wa-number
                                                    uzeit = sy-uzeit
                                                    erdat = sy-datum
                                                    uname = sy-uname
                                                    bpname = kna1-name1
                                                    customer = businesspartner ).
            APPEND ls_error TO gt_log.
            CLEAR lv_rem.
            REFRESH i_return.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM add_role_bp.
  DATA businesspartnerrolecategory TYPE bapibus1006_bproles-partnerrolecategory. " BP Role Category
  DATA all_businesspartnerroles    TYPE bapibus1006_x-mark. " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
  DATA businesspartnerrole         TYPE bapibus1006_bproles-partnerrole. " BP Role
  DATA differentiationtypevalue    TYPE bapibus1006_bproles-difftypevalue. " BP: Differentiation type value
  DATA validfromdate               TYPE bapibus1006_bprole_validity-bprolevalidfrom.
  DATA validuntildate              TYPE bapibus1006_bprole_validity-bprolevalidto.
  DATA return                      TYPE STANDARD TABLE OF bapiret2. " Return Parameter
*** Tvarvc variable for Adding roles to BP ****
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarv)
                       WHERE name = 'BP_ROLE_EXTENSION'
                       AND type = 'S'.
  IF sy-subrc = 0.
    LOOP AT lt_tvarv INTO DATA(ls_tvarv).

      businesspartnerrole = ls_tvarv-low.
      CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
        EXPORTING
          businesspartner             = businesspartner
          businesspartnerrolecategory = businesspartnerrolecategory
          all_businesspartnerroles    = ' '
          businesspartnerrole         = businesspartnerrole
          differentiationtypevalue    = differentiationtypevalue
*         VALIDFROMDATE               = P_DATE
          validuntildate              = '99991231'
        TABLES
          return                      = return.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      DO 5 TIMES.
        SELECT SINGLE rltyp INTO @DATA(lv_rltyp) FROM but100 WHERE partner = @businesspartner
                                                        AND rltyp = @ls_tvarv-low.

        IF lv_rltyp IS INITIAL.
          WAIT UP TO 1 SECONDS.  "comment by Roton
        ELSE.
          CONTINUE.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.
ENDFORM.
****  Only Business Partner Creation ***
FORM f_create_businesspartner.
  REFRESH: gt_log.
  DATA: lv_remarks TYPE string.
  LOOP AT gt_bp_create ASSIGNING FIELD-SYMBOL(<fs_bp_create>).
    EXPORT <fs_bp_create> FROM <fs_bp_create> TO MEMORY ID 'FM_CHECK1'.
    CLEAR businesspartner.
    REFRESH: i_return.
    CALL FUNCTION 'ZSD_BP_EXCEL_VALIDATE'
      TABLES
        lt_return = i_return.
    IF i_return IS NOT INITIAL.
      CLEAR lv_remarks.
      LOOP AT i_return INTO DATA(ls_ret).
        lv_remarks = ls_ret-message_v1.
        lv_remarks = | { lv_remarks } { ',' } { ls_ret-message_v1 } |.
      ENDLOOP.
      DATA(ls_bperr) = VALUE zsd_bp_creation( msgtyp = 'E'
                                              bpname = <fs_bp_create>-fname
                                              msg = | { 'Error in excel line' } { <fs_bp_create>-sterm1 } |
                                             remarks = lv_remarks ).
      APPEND ls_bperr TO gt_log.
    ELSE.
      CLEAR: businesspartner,gv_error.
      partnercategory = <fs_bp_create>-bpcat.
      partnergroup = <fs_bp_create>-bpgrp.
*** Central Data filling for business Partner ***
      SELECT SINGLE title FROM tsad3t INTO @DATA(lv_title) WHERE title_medi = @<fs_bp_create>-title.
      DATA(ls_central1) = VALUE bapibus1006_central( searchterm1 = <fs_bp_create>-sterm1
                                                    title_key = lv_title ).

      IF partnercategory = 2.
        centraldataorganization-name1 = <fs_bp_create>-fname.
        centraldataorganization-name2 = <fs_bp_create>-lname.
      ELSEIF partnercategory = 1.
        centraldataperson-firstname = <fs_bp_create>-fname.
        centraldataperson-lastname = <fs_bp_create>-lname.
      ENDIF.
      centraldataperson-namcountry = <fs_bp_create>-country.
      centraldataperson-correspondlanguage = 'EN'.
**** Adddress data filling in Business Partner Creation **
      DATA(ls_address_bp) = VALUE bapibus1006_address( street = <fs_bp_create>-street
                                                       str_suppl1 = <fs_bp_create>-addr3
                                                       str_suppl2 = <fs_bp_create>-addr2
                                                       str_suppl3 = <fs_bp_create>-addr3
                                                       house_no = <fs_bp_create>-houseno
                                                       district = <fs_bp_create>-district
                                                       postl_cod1 = <fs_bp_create>-pcode
                                                       city = <fs_bp_create>-city
                                                       country = <fs_bp_create>-country
                                                       region = <fs_bp_create>-region ).
      REFRESH: it_telephondata.
      it_telephondata[] = VALUE #( ( country = <fs_bp_create>-country
                                     telephone = <fs_bp_create>-telno
                                     std_no = 'X'
                                     r_3_user = '3'
                                     home_flag = 'X' )
                                   ( country = <fs_bp_create>-country
                                     telephone = <fs_bp_create>-mobile
                                     std_no = 'X'
                                     r_3_user = '1'
                                     home_flag = 'X' ) ).
      REFRESH: it_maildata.
      it_maildata[] = VALUE #( ( e_mail = <fs_bp_create>-email
                                 std_no = 'X'
                                 std_recip = 'X'
                                 home_flag = 'X'
                                 consnumber = '001' ) ).
*** Bapi to create Business Partner ***
      REFRESH: return.
      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          partnercategory   = partnercategory
          partnergroup      = partnergroup
          centraldata       = ls_central1
          centraldataperson = centraldataperson
          addressdata       = ls_address_bp
        IMPORTING
          businesspartner   = businesspartner
        TABLES
          telefondata       = it_telephondata
          e_maildata        = it_maildata
          return            = return.

      READ TABLE return INTO DATA(ls_ret1) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        gv_error = abap_true.
        LOOP AT  return INTO DATA(ls_return) WHERE type = 'E'.
          APPEND VALUE #( sortl = <fs_bp_create>-sterm1
                          msg  =  ls_return-message
                          msgtyp = ls_return-type
                          msgnr = ls_return-number
                          uzeit = sy-uzeit
                          erdat = sy-datum
                          uname = sy-uname ) TO gt_log.

        ENDLOOP.
      ELSE.
        IF businesspartner IS NOT INITIAL AND gv_error IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          DATA(ls_log) = VALUE zsd_bp_creation( bu_partner = businesspartner
                                                bu_partnerx = 'B'
                                                msgtyp = 'S'
                                                msg = 'Business Partner Created Successfully'
                                                uzeit = sy-uzeit
                                                erdat = sy-datum
                                                uname = sy-uname
                                                bpname = <fs_bp_create>-fname
                                                sortl = ls_central1-searchterm1 ).
          APPEND ls_log TO gt_log.
          INSERT zsd_bp_creation FROM ls_log.
**** Adding role to the Business Partner ***
          PERFORM add_role_bp.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM f_display_alv.
  DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
        lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
        lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
        lo_content     TYPE REF TO cl_salv_form_element,
        lv_title       TYPE string,
        lv_rows        TYPE string.

  DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

  DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
        lo_columns    TYPE REF TO cl_salv_columns,
        lo_column     TYPE REF TO cl_salv_column_table.

* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_log.
    CATCH cx_salv_msg.
  ENDTRY.
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

  lo_columns->set_column_position( columnname = 'MSGTYP'       position =  1 ).
  lo_columns->set_column_position( columnname = 'BU_PARTNERX'  position =  2 ).
  lo_columns->set_column_position( columnname = 'BU_PARTNER'   position =  3 ).
  lo_columns->set_column_position( columnname = 'BPNAME'       position =  4 ).
  lo_columns->set_column_position( columnname = 'SORTL'        position =  5 ).
  lo_columns->set_column_position( columnname = 'UNAME'        position =  6 ).
  lo_columns->set_column_position( columnname = 'ERDAT'        position =  7 ).
  lo_columns->set_column_position( columnname = 'SA_EXTEND'    position =  8 ).
  lo_columns->set_column_position( columnname = 'CC_EXTEND'    position =  9 ).
  lo_columns->set_column_position( columnname = 'REMARKS'      position = 10 ).

  TRY.
      lo_column ?= lo_columns->get_column( 'MANDT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'Message type' ).
      lo_column->set_medium_text( 'Msgtype' ).
      lo_column->set_short_text( 'Msgtyp' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'MSTYP' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Message type' ).
      lo_column->set_medium_text( 'Msgtype' ).
      lo_column->set_short_text( 'Msgtyp' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'SORTL' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Search term' ).
      lo_column->set_medium_text( 'Sterm1 ' ).
      lo_column->set_short_text( 'Sch term' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'REMARKS' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Remarks' ).
      lo_column->set_medium_text( 'Remarks ' ).
      lo_column->set_short_text( 'Remarks' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'MSGNR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'Remarks' ).
      lo_column->set_medium_text( 'Remarks ' ).
      lo_column->set_short_text( 'Remarks' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'UZEIT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'Remarks' ).
      lo_column->set_medium_text( 'Remarks ' ).
      lo_column->set_short_text( 'Remarks' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'SA_EXTEND' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'SA extend' ).
      lo_column->set_medium_text( 'SA extend ' ).
      lo_column->set_short_text( 'SA extend' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CC_EXTEND' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'CC extend' ).
      lo_column->set_medium_text( 'CC extend' ).
      lo_column->set_short_text( 'CC extend' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VENDOR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'CC extend' ).
      lo_column->set_medium_text( 'CC extend' ).
      lo_column->set_short_text( 'CC extend' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.
FORM f_fcall_error_check.
*** Function module to check the errors in BP creation ***
  REFRESH: i_return.
  CALL FUNCTION 'ZSD_BP_EXCEL_VALIDATE'
    TABLES
      lt_return = i_return.
ENDFORM.
*&---------------------------------------------------------------------*
FORM f_sales_area_extn .
***** Extending Sales Area and company code for Newly created Business Partner ****
  REFRESH gt_log.
  LOOP AT gt_bp_all ASSIGNING FIELD-SYMBOL(<fs_ccsal>).
    REFRESH: lt_tax_data,is_master_data-customers,lt_customers[],lt_sales[],lt_sales_func,lt_company[].
    CLEAR: ls_tax_data,ls_sales_func,ls_customers,ls_address,is_master_data,ls_sales_data,lt_sales,ls_company_data.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_ccsal>-bp
      IMPORTING
        output = <fs_ccsal>-bp.
    CLEAR: ls_bp_all.
    MOVE-CORRESPONDING <fs_ccsal> TO ls_bp_all.
    EXPORT ls_bp_all TO MEMORY ID 'FM_CHECK'.
***** Function Module For Error Checking ****
    PERFORM f_fcall_error_check.
    IF i_return IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_ccsal>-bp
        IMPORTING
          output = ls_customers-header-object_instance-kunnr.
      CLEAR kna1.
      SELECT SINGLE * FROM kna1 WHERE kunnr = ls_customers-header-object_instance-kunnr.
      ls_customers-header-object_task = 'U'. "Represents Insert or Create
      ls_address-postal-data-name          = kna1-name1. "Name of the Vendor
      ls_address-postal-data-country       = kna1-land1.
      ls_address-postal-datax-name         = 'X'.
      ls_address-postal-datax-country      = 'X'.
      ls_address-task                      = 'U'.
*   set the address for the vendor
      ls_customers-central_data-address = ls_address.
      DATA(ls_company) = VALUE cmds_ei_company( task = 'I'
                                                data_key = <fs_ccsal>-bukrs
                                                data-zterm =  <fs_ccsal>-zterm
                                                datax-akont = 'X'
                                                datax-zterm = 'X' ).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_ccsal>-akont
        IMPORTING
          output = ls_company-data-akont.
      APPEND ls_company TO lt_company.
      ls_company_data-company = lt_company[].
      ls_customers-company_data = ls_company_data.
      DATA(ls_sales) = VALUE cmds_ei_sales(  task = 'I'
                                                      data_key-vkorg = <fs_ccsal>-vkorg
                                                      data_key-vtweg = <fs_ccsal>-vtweg
                                                      data_key-spart = <fs_ccsal>-spart
                                                      data-bzirk = <fs_ccsal>-bzirk
                                                      data-kdgrp = <fs_ccsal>-kdgrp
                                                      data-ktgrd = <fs_ccsal>-ktgrd
                                                      data-vkbur = <fs_ccsal>-vkbur
                                                      data-vkgrp = <fs_ccsal>-vkgrp
                                                      data-kalks = <fs_ccsal>-kalks
                                                      data-incov = <fs_ccsal>-incov
                                                      data-inco1 = <fs_ccsal>-inco1
                                                      data-inco2 = <fs_ccsal>-inco2
                                                      data-vwerk = <fs_ccsal>-vwerk
                                                      data-vsbed = <fs_ccsal>-vsbed
                                                      data-zterm = <fs_ccsal>-zterm
                                                      data-inco2_l = <fs_ccsal>-inco_l
                                                      data-lprio = <fs_ccsal>-lprio
                                                      data-pltyp = <fs_ccsal>-pltyp
                                                      data-waers = 'INR'
                                                      datax-bzirk = 'X'
                                                      datax-kdgrp = 'X'
                                                      datax-ktgrd = 'X'
                                                      datax-vkbur = 'X'
                                                      datax-vkgrp = 'X'
                                                      datax-kalks = 'X'
                                                      datax-incov = 'X'
                                                      datax-inco1 = 'X'
                                                      datax-inco2 = 'X'
                                                      datax-vwerk = 'X'
                                                      datax-vsbed = 'X'
                                                      datax-zterm = 'X'
                                                      datax-waers = 'X'
                                                      datax-inco2_l = 'X'
                                                      datax-lprio = 'X'
                                                      datax-pltyp = 'X' ).
      DATA iv_ktokd TYPE ktokd. " Customer Account Group
      DATA et_parvw TYPE cmds_parvw_t.
      REFRESH et_parvw.
      REFRESH lt_sales_func.
      CALL METHOD cmd_ei_api_check=>get_mand_partner_functions
        EXPORTING
          iv_ktokd = kna1-ktokd
        IMPORTING
          et_parvw = et_parvw.

      LOOP AT et_parvw INTO DATA(wa_parvw).
        ls_sales_func-task = 'I'.
        ls_sales_func-data_key-parvw = wa_parvw-parvw.
        CASE wa_parvw-parvw.
          WHEN 'AG'.
            ls_sales_func-data-partner = kna1-kunnr.
          WHEN 'WE'.
            ls_sales_func-data-partner = kna1-kunnr.
          WHEN 'RG'.
            ls_sales_func-data-partner = kna1-kunnr.
          WHEN 'RE'.
            ls_sales_func-data-partner = kna1-kunnr.
          WHEN 'L1'.
            ls_sales_func-data-partner = <fs_ccsal>-pf_ns.
          WHEN 'L2'.
            ls_sales_func-data-partner = <fs_ccsal>-pf_rs.
          WHEN 'L3'.
            ls_sales_func-data-partner = <fs_ccsal>-pf_as.
          WHEN 'L5'.
            ls_sales_func-data-partner = <fs_ccsal>-pf_so.
          WHEN 'ZA'.
            ls_sales_func-data-partner = <fs_ccsal>-pf_za.
          WHEN 'ZC'.
            ls_sales_func-data-partner = <fs_ccsal>-pf_zc.
          WHEN 'ZH'.
            ls_sales_func-data-partner = <fs_ccsal>-pf_zh.
          WHEN 'ZS'.
            ls_sales_func-data-partner = <fs_ccsal>-pf_zs.
          WHEN OTHERS.
        ENDCASE.
        ls_sales_func-datax-partner = 'X'.
        APPEND ls_sales_func TO lt_sales_func.
        CLEAR ls_sales_func.
      ENDLOOP. " LOOP AT et_parvw INTO DATA(wa_parvw)
      ls_sales_func-task = 'I'.
      ls_sales_func-data_key-parvw = 'SK'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_ccsal>-pf_sk
        IMPORTING
          output = <fs_ccsal>-pf_sk.
      ls_sales_func-data-partner = <fs_ccsal>-pf_sk.
      ls_sales_func-datax-partner = 'X'.
      APPEND ls_sales_func TO lt_sales_func.
      CLEAR ls_sales_func.
      ls_sales-functions-functions = lt_sales_func[].
      APPEND ls_sales TO lt_sales.

      ls_sales_data-sales = lt_sales[].
      SELECT SINGLE kunnr FROM knvv INTO @DATA(l_kunnr) WHERE kunnr = @ls_customers-header-object_instance-kunnr.
      IF sy-subrc <> 0.
        " Adding tax_data

        CLEAR gwa_knvi.
        SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = <fs_ccsal>-bp
                                                  AND aland = 'IN'
                                                  AND tatyp = 'JOIG' .
        IF gwa_knvi IS INITIAL.
          APPEND VALUE #( task = 'I'
                          data_key-aland = 'IN'
                          data_key-tatyp = 'JOIG'
                          data-taxkd = <fs_ccsal>-taxkd03
                          datax-taxkd = 'X' ) TO lt_tax_data.
        ENDIF.

        CLEAR gwa_knvi.
        SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = <fs_ccsal>-bp
                                                  AND aland = 'IN'
                                                  AND tatyp = 'JOCG' .

        IF gwa_knvi IS INITIAL.
          APPEND VALUE #( task = 'I'
              data_key-aland = 'IN'
              data_key-tatyp = 'JOCG'
              data-taxkd = <fs_ccsal>-taxkd02
              datax-taxkd = 'X' ) TO lt_tax_data.
        ENDIF.

        CLEAR gwa_knvi.

        SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = <fs_ccsal>-bp
                                                  AND aland = 'IN'
                                                  AND tatyp = 'JOSG' .

        IF gwa_knvi IS INITIAL.
          APPEND VALUE #( task = 'I'
                          data_key-aland = 'IN'
                          data_key-tatyp = 'JOSG'
                          data-taxkd = <fs_ccsal>-taxkd01
                          datax-taxkd = 'X' ) TO lt_tax_data.
        ENDIF.


        CLEAR gwa_knvi.
        SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = <fs_ccsal>-bp
                                                  AND aland = 'IN'
                                                  AND tatyp = 'JOUG' .

        IF gwa_knvi IS INITIAL.
          APPEND VALUE #( task = 'I'
                        data_key-aland = 'IN'
                        data_key-tatyp = 'JOUG'
                        data-taxkd = <fs_ccsal>-taxkd04
                        datax-taxkd = 'X' ) TO lt_tax_data.
        ENDIF.

        CLEAR gwa_knvi.
        SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = <fs_ccsal>-bp
                                                  AND aland = 'IN'
                                                  AND tatyp = 'JTC1' .

        IF gwa_knvi IS INITIAL.
          APPEND VALUE #( task = 'I'
                        data_key-aland = 'IN'
                        data_key-tatyp = 'JTC1'
                        data-taxkd = <fs_ccsal>-taxkd06
                        datax-taxkd = 'X' ) TO lt_tax_data.
        ENDIF.
        CLEAR gwa_knvi.
        SELECT SINGLE * FROM knvi INTO gwa_knvi WHERE kunnr = <fs_ccsal>-bp
                                                  AND aland = 'IN'
                                                  AND tatyp = 'JCOS' .

        IF gwa_knvi IS INITIAL.
          APPEND VALUE #( task = 'I'
                        data_key-aland = 'IN'
                        data_key-tatyp = 'JCOS'
                        data-taxkd = <fs_ccsal>-taxkd05
                        datax-taxkd = 'X' ) TO lt_tax_data.
        ENDIF.

        ls_central_data-tax_ind-tax_ind = lt_tax_data.
      ENDIF. " IF sy-subrc <> 0
      ls_customers-central_data = ls_central_data.
      ls_customers-sales_data = ls_sales_data.
      APPEND ls_customers TO lt_customers.
      is_master_data-customers = lt_customers[].
      cmd_ei_api=>initialize( ).
      CALL METHOD cmd_ei_api=>maintain_bapi
        EXPORTING
*         IV_TEST_RUN              = SPACE
*         IV_COLLECT_MESSAGES      = SPACE
          is_master_data           = is_master_data
        IMPORTING
          es_master_data_correct   = es_master_data_correct
          es_message_correct       = es_message_correct
          es_master_data_defective = es_master_data_defective
          es_message_defective     = es_message_defective.
      IF es_message_defective-is_error IS INITIAL.
        COMMIT WORK.
        WAIT UP TO 1 SECONDS.
        APPEND VALUE #( msgtyp = 'S'
                        bpname = kna1-name1
                        sortl = <fs_ccsal>-sterm1
                        uname = sy-uname
                        erdat = sy-datum
                        uzeit = sy-uzeit
                        bu_partnerx = <fs_ccsal>-bpcat
                        msg = |Sales Area and Company Code is Extended Successfully|
                        msgnr = '002'
                        bu_partner = <fs_ccsal>-bp
                        sa_extend = 'X'
                        cc_extend = 'X' ) TO gt_log.
        SELECT SINGLE * FROM zsd_bp_creation INTO @DATA(ls_create) WHERE bu_partner = @<fs_ccsal>-bp.
        IF sy-subrc = 0.
          UPDATE zsd_bp_creation SET sa_extend = 'X'
                                     cc_extend = 'X'
                                     WHERE bu_partner = <fs_ccsal>-bp.
        ELSE.
          DATA(ls_update) = VALUE #( gt_log[ bu_partnerx =  <fs_ccsal>-bp ] OPTIONAL ).
          MODIFY zsd_bp_creation FROM ls_update.
          COMMIT WORK AND WAIT.
        ENDIF.

      ELSE.
        LOOP AT es_message_defective-messages INTO DATA(ws).
          DATA(lw_exmsg) =  ws-message.
          lw_exmsg = | { lw_exmsg } { ws-message }  |.
        ENDLOOP.
        APPEND VALUE #( msgtyp = 'E'
                        bpname = kna1-name1
                        sortl = <fs_ccsal>-sterm1
                        uname = sy-uname
                        erdat = sy-datum
                        uzeit = sy-uzeit
                        bu_partnerx = <fs_ccsal>-bpcat
                        msg = lw_exmsg
                        msgnr = '002'
                        bu_partner = <fs_ccsal>-bp
                        sa_extend = ''
                        cc_extend = '' ) TO gt_log.
        CLEAR lw_exmsg.
      ENDIF.
    ELSE.
      LOOP AT i_return INTO DATA(lw_error).
        DATA(lv_exmsg) =  lw_error-message_v1.
        lv_exmsg = | { lv_exmsg } { lw_error-message_v1 }  |.
      ENDLOOP.
      APPEND VALUE #( msgtyp = 'E'
                      bpname = kna1-name1
                      sortl = <fs_ccsal>-sterm1
                      uname = sy-uname
                      erdat = sy-datum
                      uzeit = sy-uzeit
                      bu_partnerx = <fs_ccsal>-bpcat
                      msg = lv_exmsg
                      msgnr = '002'
                      bu_partner = <fs_ccsal>-bp
                      sa_extend = ''
                      cc_extend = '' ) TO gt_log.
      CLEAR lv_exmsg.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_compcode_extend
*&---------------------------------------------------------------------*
FORM f_compcode_extend .
  IF p_flname IS NOT INITIAL.
    REFRESH: gt_cc_excel.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = 'X'
        i_tab_raw_data       = gt_raw
        i_filename           = p_flname
      TABLES
        i_tab_converted_data = gt_cc_excel.
  ENDIF.

  IF gt_cc_excel IS NOT INITIAL.
    REFRESH: gt_cc_alv.
    LOOP AT gt_cc_excel ASSIGNING FIELD-SYMBOL(<fs_cc_excel>).
      DATA(lv_cust) = |{ <fs_cc_excel>-customer ALPHA = IN }|.
      SELECT SINGLE * FROM kna1 INTO @DATA(l_kna1)
        WHERE kunnr = @lv_cust.
      IF sy-subrc NE 0.
        DATA(l_alv) = VALUE ty_cc_alv( ).
        l_alv = CORRESPONDING #( BASE ( l_alv ) <fs_cc_excel> ).
        l_alv-type = 'E'.
        l_alv-msg  = |Incoorect Customer Number|. APPEND l_alv TO gt_cc_alv.
        CONTINUE.
      ENDIF.
      SELECT SINGLE bukrs FROM t001 INTO @DATA(l_bukrs)
        WHERE bukrs = @<fs_cc_excel>-bukrs.
      IF sy-subrc NE 0.
        l_alv = CORRESPONDING #( BASE ( l_alv ) <fs_cc_excel> ).
        l_alv-name = l_kna1-name1.
        l_alv-type = 'E'.
        l_alv-msg = |Incorrect Company Code|. APPEND l_alv TO gt_cc_alv.
        CONTINUE.
      ENDIF.
      DATA(lv_akont) = |{ <fs_cc_excel>-akont ALPHA = IN }|.
      SELECT SINGLE saknr FROM skat INTO @DATA(l_akont)
        WHERE  spras = @sy-langu
        AND ktopl = 'YAIN' AND
        saknr = @lv_akont.
      IF sy-subrc NE 0.
        l_alv = CORRESPONDING #( BASE ( l_alv ) <fs_cc_excel> ).
        l_alv-name = l_kna1-name1.
        l_alv-type = 'E'.
        l_alv-msg = |Incorrect G/L Account|. APPEND l_alv TO gt_cc_alv.
        CONTINUE.
      ENDIF.
      IF <fs_cc_excel>-zterm IS INITIAL.
        l_alv = CORRESPONDING #( BASE ( l_alv ) <fs_cc_excel> ).
        l_alv-name = l_kna1-name1.
        l_alv-type = 'E'.
        l_alv-msg = |Payment Term is Mandatory|. APPEND l_alv TO gt_cc_alv.
        CONTINUE.
      ENDIF.
      SELECT SINGLE * FROM knb1 INTO @DATA(l_cc_cust)
        WHERE kunnr = @lv_cust
        AND bukrs = @<fs_cc_excel>-bukrs.
      IF sy-subrc EQ 0.
        l_alv = CORRESPONDING #( BASE ( l_alv ) <fs_cc_excel> ).
        l_alv-name = l_kna1-name1.
        l_alv-type = 'E'.
        l_alv-msg = |Customer Already Extended to this Company Code|. APPEND l_alv TO gt_cc_alv.
        CONTINUE.
      ENDIF.
*** Data Population to be Updated in Business Partner
      CLEAR: is_master_data,ls_customers,lt_customers,ls_address,ls_company_data,lt_company,es_message_defective.
      ls_customers-header-object_instance-kunnr = lv_cust.
      ls_customers-header-object_task = 'U'. "Represents Insert or Create
      ls_address-postal-data-name          = l_kna1-name1. "Name of the Vendor
      ls_address-postal-data-country       = l_kna1-land1.
      ls_address-postal-datax-name         = 'X'.
      ls_address-postal-datax-country      = 'X'.
      ls_address-task                      = 'U'.
*   set the address for the vendor
      ls_customers-central_data-address = ls_address.
      DATA(ls_company) = VALUE cmds_ei_company( task = 'I'
                                                data_key = <fs_cc_excel>-bukrs
                                                data-akont = |{ <fs_cc_excel>-akont ALPHA = IN }|
                                                data-zterm =  <fs_cc_excel>-zterm
                                                datax-akont = 'X'
                                                datax-zterm = 'X' ).
      APPEND ls_company TO lt_company.
      ls_company_data-company = lt_company[].
      ls_customers-company_data = ls_company_data.
      APPEND ls_customers TO lt_customers.
      is_master_data-customers = lt_customers[].
      cmd_ei_api=>initialize( ).
      CALL METHOD cmd_ei_api=>maintain_bapi
        EXPORTING
*         IV_TEST_RUN              = SPACE
*         IV_COLLECT_MESSAGES      = SPACE
          is_master_data           = is_master_data
        IMPORTING
          es_master_data_correct   = es_master_data_correct
          es_message_correct       = es_message_correct
          es_master_data_defective = es_master_data_defective
          es_message_defective     = es_message_defective.
      IF es_message_defective-is_error IS INITIAL.
        COMMIT WORK.
        WAIT UP TO 1 SECONDS.
        l_alv = CORRESPONDING #( BASE ( l_alv ) <fs_cc_excel> ).
        l_alv-name = l_kna1-name1.
        l_alv-type = 'S'.
        l_alv-msg = |Customer Extended to Company Code|. APPEND l_alv TO gt_cc_alv.
      ELSE.
        LOOP AT es_message_defective-messages INTO DATA(ws) WHERE ( type = 'E' OR type = 'A' ).
          DATA(lw_exmsg) =  ws-message.
          lw_exmsg = | { lw_exmsg } { ws-message }  |.
        ENDLOOP.
        l_alv = CORRESPONDING #( BASE ( l_alv ) <fs_cc_excel> ).
        l_alv-name = l_kna1-name1.
        l_alv-type = 'E'.
        l_alv-msg = lw_exmsg. APPEND l_alv TO gt_cc_alv.
        CLEAR lw_exmsg.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM f_display_ccalv .
  DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
        lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
        lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
        lo_content     TYPE REF TO cl_salv_form_element,
        lv_title       TYPE string,
        lv_rows        TYPE string.

  DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

  DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
        lo_columns    TYPE REF TO cl_salv_columns,
        lo_column     TYPE REF TO cl_salv_column_table.

* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_cc_alv.
    CATCH cx_salv_msg.
  ENDTRY.
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
  TRY.
      lo_column ?= lo_columns->get_column( 'CUSTOMER' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Customer' ).
      lo_column->set_medium_text( 'Customer' ).
      lo_column->set_short_text( 'Customer' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'NAME' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Cusname' ).
      lo_column->set_medium_text( 'Cusname' ).
      lo_column->set_short_text( 'Cusname' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'BUKRS' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Compcode' ).
      lo_column->set_medium_text( 'Compcode ' ).
      lo_column->set_short_text( 'Compcode' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'ZTERM' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Payterms' ).
      lo_column->set_medium_text( 'Payterms ' ).
      lo_column->set_short_text( 'Payterms' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'AKONT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'GL Account' ).
      lo_column->set_medium_text( 'GL Account' ).
      lo_column->set_short_text( 'GL Account' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TYPE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Msgtyp' ).
      lo_column->set_medium_text( 'Msgtyp' ).
      lo_column->set_short_text( 'Msgtyp' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'MSG' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Message' ).
      lo_column->set_medium_text( 'Message' ).
      lo_column->set_short_text( 'Message' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form modify_screen
*&---------------------------------------------------------------------*
FORM modify_screen .
*Added by Samsudeen M 0n 16.12.2023 for DMS Purpose
  LOOP AT SCREEN.
    IF r6 EQ abap_true.

      IF screen-group1   = 'BL2'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.

      IF screen-group1   = 'BL4'.
        screen-input     = 1.
        screen-invisible = 0.
      ENDIF.
    ELSE.
      IF screen-group1   = 'BL4'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form dms_customer_cc_sa_extension
*&---------------------------------------------------------------------*
FORM dms_customer_cc_sa_extension .

  DATA: lv_errmsg TYPE string.

  LOOP AT gt_dms_extn ASSIGNING FIELD-SYMBOL(<lfs_dms_extn>) WHERE status NE icon_green_light.
*** DATA population to be updated in business partner
    CLEAR: is_master_data,ls_customers,lt_customers,ls_address,ls_company_data,lt_company,es_message_defective.
    ls_customers-header-object_instance-kunnr = <lfs_dms_extn>-kunnr.
    ls_customers-header-object_task = 'U'. "Represents Insert or Create
    ls_address-postal-data-name          = <lfs_dms_extn>-name1. "Name of the Vendor
    ls_address-postal-data-country       = <lfs_dms_extn>-land1.
    ls_address-postal-datax-name         = 'X'.
    ls_address-postal-datax-country      = 'X'.
    ls_address-task                      = 'U'.

    ls_customers-central_data-address = ls_address.
*Company Code Extension
    DATA(ls_company) = VALUE cmds_ei_company( task        = 'I'
                                              data_key    = <lfs_dms_extn>-bukrs
*                                              data-akont  = |{ <lfs_dms_extn>-akont ALPHA = IN }|
                                              data-akont  = '0023100001'
                                              data-zterm  =  <lfs_dms_extn>-zterm
                                              datax-akont = 'X'
                                              datax-zterm = 'X' ).
    APPEND ls_company TO lt_company.
    ls_company_data-company = lt_company[].
    ls_customers-company_data = ls_company_data.
*Sales Area Extension
    DATA(ls_sales) = VALUE cmds_ei_sales(  task           = 'I'
                                           data_key-vkorg = <lfs_dms_extn>-vkorg
                                           data_key-vtweg = <lfs_dms_extn>-vtweg
                                           data_key-spart = <lfs_dms_extn>-spart
                                           data-bzirk     = <lfs_dms_extn>-bzirk
                                           data-kdgrp     = <lfs_dms_extn>-kdgrp
                                           data-ktgrd     = <lfs_dms_extn>-ktgrd
                                           data-vkbur     = <lfs_dms_extn>-vkbur
                                           data-vkgrp     = <lfs_dms_extn>-vkgrp
                                           data-kalks     = <lfs_dms_extn>-kalks
                                           data-incov     = <lfs_dms_extn>-incov
                                           data-inco1     = <lfs_dms_extn>-inco1
                                           data-inco2     = <lfs_dms_extn>-inco2
                                           data-vwerk     = <lfs_dms_extn>-vwerk
                                           data-vsbed     = <lfs_dms_extn>-vsbed
                                           data-zterm     = <lfs_dms_extn>-zterm
                                           data-inco2_l   = <lfs_dms_extn>-inco_l
                                           data-lprio     = <lfs_dms_extn>-lprio
                                           data-pltyp     = <lfs_dms_extn>-pltyp
                                           data-kabss     = <lfs_dms_extn>-kabss
                                           data-kkber     = <lfs_dms_extn>-kkber
                                           data-waers     = 'INR'
                                           datax-bzirk    = 'X'
                                           datax-kdgrp    = 'X'
                                           datax-ktgrd    = 'X'
                                           datax-vkbur    = 'X'
                                           datax-vkgrp    = 'X'
                                           datax-kalks    = 'X'
                                           datax-incov    = 'X'
                                           datax-inco1    = 'X'
                                           datax-inco2    = 'X'
                                           datax-vwerk    = 'X'
                                           datax-vsbed    = 'X'
                                           datax-zterm    = 'X'
                                           datax-waers    = 'X'
                                           datax-inco2_l  = 'X'
                                           datax-lprio    = 'X'
                                           datax-pltyp    = 'X'
                                           datax-kabss    = 'X'
                                           datax-kkber    = 'X'
                                            ).
*Customer Partner Function Extension
    REFRESH: lt_sales_func[],lt_sales[].
    CLEAR ls_sales_func.
    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'AG'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_sp.
    ls_sales_func-datax-partner  = 'X'.
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'RE'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_bp.
    ls_sales_func-datax-partner  = 'X' .
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'RG'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_py.
    ls_sales_func-datax-partner  = 'X' .
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    IF <lfs_dms_extn>-pf_sk IS NOT INITIAL.
      ls_sales_func-task           = 'I'.
      ls_sales_func-data_key-parvw = 'SK'.
      ls_sales_func-data-partner   = <lfs_dms_extn>-pf_sk.
      ls_sales_func-datax-partner  = 'X' .
      APPEND ls_sales_func TO lt_sales_func.
      CLEAR ls_sales_func.
    ENDIF.

    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'WE'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_sh.
    ls_sales_func-datax-partner  = 'X' .
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'L1'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_ns.
    ls_sales_func-datax-partner  = 'X' .
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'L2'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_rs.
    ls_sales_func-datax-partner  = 'X'.
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'L3'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_as.
    ls_sales_func-datax-partner  = 'X'.
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'L5'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_so.
    ls_sales_func-datax-partner  = 'X' .
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'ZC'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_zc.
    ls_sales_func-datax-partner  = 'X' .
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    ls_sales_func-task           = 'I'.
    ls_sales_func-data_key-parvw = 'ZS'.
    ls_sales_func-data-partner   = <lfs_dms_extn>-pf_zs.
    ls_sales_func-datax-partner  = 'X' .
    APPEND ls_sales_func TO lt_sales_func.
    CLEAR ls_sales_func.

    IF <lfs_dms_extn>-pf_xj IS NOT INITIAL.
      ls_sales_func-task           = 'I'.
      ls_sales_func-data_key-parvw = 'XJ'.
      ls_sales_func-data-partner   = <lfs_dms_extn>-pf_xj.
      ls_sales_func-datax-partner  = 'X' .
      APPEND ls_sales_func TO lt_sales_func.
      CLEAR ls_sales_func.
    ENDIF.

    IF <lfs_dms_extn>-pf_xk IS NOT INITIAL.
      ls_sales_func-task           = 'I'.
      ls_sales_func-data_key-parvw = 'XK'.
      ls_sales_func-data-partner   = <lfs_dms_extn>-pf_xk.
      ls_sales_func-datax-partner  = 'X' .
      APPEND ls_sales_func TO lt_sales_func.
      CLEAR ls_sales_func.
    ENDIF.

    IF <lfs_dms_extn>-pf_xl IS NOT INITIAL.
      ls_sales_func-task           = 'I'.
      ls_sales_func-data_key-parvw = 'XL'.
      ls_sales_func-data-partner   = <lfs_dms_extn>-pf_xl.
      ls_sales_func-datax-partner  = 'X' .
      APPEND ls_sales_func TO lt_sales_func.
      CLEAR ls_sales_func.
    ENDIF.

    IF <lfs_dms_extn>-pf_yd IS NOT INITIAL.
      ls_sales_func-task           = 'I'.
      ls_sales_func-data_key-parvw = 'YD'.
      ls_sales_func-data-partner   = <lfs_dms_extn>-pf_yd.
      ls_sales_func-datax-partner  = 'X'.
      APPEND ls_sales_func TO lt_sales_func.
      CLEAR ls_sales_func.
    ENDIF.
    IF <lfs_dms_extn>-pf_xn IS NOT INITIAL.
      ls_sales_func-task           = 'I'.
      ls_sales_func-data_key-parvw = 'XN'.
      ls_sales_func-data-partner   = <lfs_dms_extn>-pf_xn.
      ls_sales_func-datax-partner  = 'X'.
      APPEND ls_sales_func TO lt_sales_func.
      CLEAR ls_sales_func.
    ENDIF.

    ls_sales-functions-functions = lt_sales_func[].
    APPEND ls_sales TO lt_sales.
    ls_sales_data-sales = lt_sales[].

    REFRESH lt_tax_data[].
    lt_tax_data = VALUE #( ( task           = 'M'
                             data_key-aland = 'IN'
                             data_key-tatyp = 'JOSG'
                             data-taxkd     = <lfs_dms_extn>-taxkd01
                             datax-taxkd    = 'X')
                           ( task           = 'M'
                             data_key-aland = 'IN'
                             data_key-tatyp = 'JOCG'
                             data-taxkd     = <lfs_dms_extn>-taxkd02
                             datax-taxkd    = 'X')
                           ( task           = 'M'
                             data_key-aland = 'IN'
                             data_key-tatyp = 'JOIG'
                             data-taxkd     = <lfs_dms_extn>-taxkd03
                             datax-taxkd    = 'X')
                           ( task           = 'M'
                             data_key-aland = 'IN'
                             data_key-tatyp = 'JOUG'
                             data-taxkd     = <lfs_dms_extn>-taxkd04
                             datax-taxkd    = 'X')
                           ( task           = 'M'
                             data_key-aland = 'IN'
                             data_key-tatyp = 'JCOS'
                             data-taxkd     = <lfs_dms_extn>-taxkd05
                             datax-taxkd    = 'X')
                           ( task           = 'M'
                             data_key-aland = 'IN'
                             data_key-tatyp = 'JTC1'
                             data-taxkd     = <lfs_dms_extn>-taxkd06
                             datax-taxkd    = 'X')
                          ).
    ls_central_data-tax_ind-tax_ind = lt_tax_data.

    ls_customers-central_data = ls_central_data.
    ls_customers-sales_data = ls_sales_data.
    APPEND ls_customers TO lt_customers.
    is_master_data-customers = lt_customers[].

    cmd_ei_api=>initialize( ).
    CLEAR: lv_errmsg,es_message_defective.
    CALL METHOD cmd_ei_api=>maintain_bapi
      EXPORTING
        is_master_data           = is_master_data
      IMPORTING
        es_master_data_correct   = es_master_data_correct
        es_message_correct       = es_message_correct
        es_master_data_defective = es_master_data_defective
        es_message_defective     = es_message_defective.
    IF es_message_defective-is_error IS INITIAL.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
      <lfs_dms_extn>-status  = icon_green_light.
      <lfs_dms_extn>-message = TEXT-s01.
    ELSE.
      LOOP AT es_message_defective-messages INTO DATA(wa) WHERE type = 'E' OR type = 'A'.
        lv_errmsg = |{ lv_errmsg }{ wa-message }|.
      ENDLOOP.
      <lfs_dms_extn>-status  = icon_red_light.
      <lfs_dms_extn>-message = lv_errmsg.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form sub_dealer_data_fetch
*&---------------------------------------------------------------------*
FORM sub_dealer_data_fetch .
  REFRESH: gt_kna1[], gt_knb1[], gt_knvv[], gt_knvp[].
*Customer General Data
  SELECT kunnr
         name1
         land1 FROM kna1 INTO TABLE gt_kna1
               WHERE kunnr IN s_kunnr
               ORDER BY kunnr.
  IF sy-subrc = 0.
*Customer Company Code Data
    SELECT kunnr
           bukrs
           akont
           zterm FROM knb1 INTO TABLE gt_knb1
                 FOR ALL ENTRIES IN gt_kna1
                 WHERE kunnr = gt_kna1-kunnr
                 AND bukrs IN ('1000', '6000').
    IF sy-subrc = 0.
      SORT gt_knb1[] BY kunnr.
    ENDIF.
*Customer Sales Area Data
    SELECT kunnr
           vkorg
           vtweg
           spart
           bzirk
           kdgrp
           vkbur
           vkgrp
           ktgrd
           kalks
           incov
           inco1
           inco2
           inco2_l
           vwerk
           vsbed
           lprio
           pltyp
           kabss
           kkber FROM knvv INTO TABLE gt_knvv
                 FOR ALL ENTRIES IN gt_kna1
                 WHERE kunnr = gt_kna1-kunnr
                 AND vkorg = '6000'
                 AND vtweg = '20'
                 AND spart = '10'.
    IF sy-subrc = 0.
      SORT gt_knvv[] BY kunnr.
    ENDIF.
* Customer Partner Function Data
    SELECT kunnr
           vkorg
           vtweg
           spart
           parvw
           parza
           kunn2
           lifnr
           pernr FROM knvp INTO
                 CORRESPONDING FIELDS OF TABLE gt_knvp
                 FOR ALL ENTRIES IN gt_knvv
                 WHERE kunnr = gt_knvv-kunnr
                 AND vkorg = gt_knvv-vkorg
                 AND vtweg = gt_knvv-vtweg
                 AND spart = gt_knvv-spart.
    IF sy-subrc = 0.
      SORT gt_knvp[] BY kunnr.
    ENDIF.
  ELSE.
    MESSAGE TEXT-e01 TYPE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form data_manipulation
*&---------------------------------------------------------------------*
FORM data_manipulation .
***********zterm hardcode - NT30*********
  DATA(ls_dms_extn) = VALUE ts_dms_extn( ).

  DATA : lv_plant TYPE werks_d.

  REFRESH: gt_dms_extn.
*************customer block check***********
  DATA : lo_block TYPE REF TO zcl_common_check.
  CREATE OBJECT lo_block.
  DATA : lt_block TYPE TABLE OF zsd_st_cust_block.
  REFRESH : lt_block.
  lo_block->customer_block_check(
    EXPORTING
      bukrs                     = '1000'      " Company Code
      vkorg                     = '1000'      " Sales Organization
    CHANGING
      cust_tab                  = lt_block    " Table Type for Customer Block Check
      ).
  IF lt_block IS NOT INITIAL.
    SORT : lt_block BY kunnr block.
  ENDIF.

  LOOP AT gt_kna1 ASSIGNING FIELD-SYMBOL(<lfs_kna1>).

    CLEAR ls_dms_extn.
    ls_dms_extn-status = icon_create.
    ls_dms_extn-kunnr  = <lfs_kna1>-kunnr.
    ls_dms_extn-name1  = <lfs_kna1>-name1.
    ls_dms_extn-land1  = <lfs_kna1>-land1.
*Customer Company Code Data
    ls_dms_extn-bukrs  = 'DMS1'.
    ls_dms_extn-akont  = VALUE #( gt_knb1[ kunnr = <lfs_kna1>-kunnr ]-akont OPTIONAL ).
*    ls_dms_extn-zterm  = VALUE #( gt_knb1[ kunnr = <lfs_kna1>-kunnr ]-zterm OPTIONAL ).
    ls_dms_extn-zterm  = 'NT30'.
*Any Company Code Mandatory Data Missing Means Giving Error
    DATA(l_error) = COND #( WHEN ls_dms_extn-bukrs IS INITIAL THEN 'X'
                            WHEN ls_dms_extn-akont IS INITIAL THEN 'X'
                            WHEN ls_dms_extn-zterm IS INITIAL THEN 'X' ).

    IF l_error IS NOT INITIAL.
      ls_dms_extn-message = TEXT-a01.
      APPEND ls_dms_extn TO gt_dms_extn.
      CONTINUE.
    ELSE.
*Customer Sales Area Data
      ls_dms_extn-vkorg  = 'SDMS'.
      ls_dms_extn-vtweg  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-vtweg  OPTIONAL ).
      ls_dms_extn-spart  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-spart  OPTIONAL ).
      ls_dms_extn-bzirk  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-bzirk  OPTIONAL ).
      ls_dms_extn-kdgrp  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-kdgrp  OPTIONAL ).
      ls_dms_extn-vkgrp  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-vkgrp  OPTIONAL ).
      ls_dms_extn-ktgrd  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-ktgrd  OPTIONAL ).
      ls_dms_extn-kalks  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-kalks  OPTIONAL ).
      ls_dms_extn-incov  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-incov  OPTIONAL ).
      ls_dms_extn-inco1  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-inco1  OPTIONAL ).
      ls_dms_extn-inco2  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-inco2  OPTIONAL ).
      ls_dms_extn-inco_l = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-inco_l OPTIONAL ).
      ls_dms_extn-vsbed  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-vsbed  OPTIONAL ).
      ls_dms_extn-lprio  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-lprio  OPTIONAL ).
      ls_dms_extn-pltyp  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-pltyp  OPTIONAL ).
      ls_dms_extn-kabss  = VALUE #( gt_knvv[ kunnr = <lfs_kna1>-kunnr ]-kabss  OPTIONAL ).
      ls_dms_extn-kkber  = 'SDMS'.
*Customer Partner Function Data
      ls_dms_extn-pf_sp = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'AG' ]-kunn2  OPTIONAL ).
      ls_dms_extn-pf_bp = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'RE' ]-kunn2  OPTIONAL ).
      ls_dms_extn-pf_py = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'RG' ]-kunn2  OPTIONAL ).

**************check the sk is active or not************
      ls_dms_extn-pf_sk = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'SK' ]-kunn2  OPTIONAL ).

*****************check distributor block or not*******************
      IF ls_dms_extn-pf_sk IS INITIAL.
        lv_plant = 'DXXX'.
      ELSE.
        IF lt_block IS NOT INITIAL.
          READ TABLE lt_block TRANSPORTING NO FIELDS WITH KEY kunnr = ls_dms_extn-pf_sk
                                                              block = abap_true BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR : ls_dms_extn-pf_sk.
            lv_plant = 'DXXX'.
          ELSE.
            CLEAR : lv_plant.
            SELECT SINGLE werks FROM kna1 INTO lv_plant WHERE kunnr = ls_dms_extn-pf_sk.
            IF lv_plant IS INITIAL.
              ls_dms_extn-message = |Distributor Plant is not Maintained { ls_dms_extn-pf_sk } |.
              APPEND ls_dms_extn TO gt_dms_extn.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*****pass the sales office & delv plant********
      ls_dms_extn-vwerk  = lv_plant.
      ls_dms_extn-vkbur  = lv_plant.

      ls_dms_extn-pf_sh = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'WE' ]-kunn2  OPTIONAL ).
***********check the pernr is active or not********
*nsm
      ls_dms_extn-pf_ns = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'L1' ]-pernr  OPTIONAL ).
      IF ls_dms_extn-pf_ns IS INITIAL.
        ls_dms_extn-pf_ns = '09005599'.
      ELSEIF ls_dms_extn-pf_ns = '09005599'.
      ELSE.
        READ TABLE gt_pa0000 INTO gs_pa0000 WITH KEY pernr = ls_dms_extn-pf_ns stat2 = '3'.
        IF sy-subrc NE 0.
          ls_dms_extn-pf_ns = '09005599'.
        ENDIF.
      ENDIF.

*rsm
      ls_dms_extn-pf_rs = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'L2' ]-pernr  OPTIONAL ).
      IF ls_dms_extn-pf_rs IS INITIAL.
        ls_dms_extn-pf_rs = '09003003'.
      ELSEIF ls_dms_extn-pf_rs = '09003003'.
      ELSE.
        READ TABLE gt_pa0000 INTO gs_pa0000 WITH KEY pernr = ls_dms_extn-pf_rs stat2 = '3'.
        IF sy-subrc NE 0.
          ls_dms_extn-pf_rs = '09003003'.
        ENDIF.
      ENDIF.

*SM
      ls_dms_extn-pf_as = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'L3' ]-pernr  OPTIONAL ).
      IF ls_dms_extn-pf_as IS INITIAL.
        ls_dms_extn-pf_as = '09003002'.
      ELSEIF ls_dms_extn-pf_as = '09003002'.
      ELSE.
        READ TABLE gt_pa0000 INTO gs_pa0000 WITH KEY pernr = ls_dms_extn-pf_as stat2 = '3'.
        IF sy-subrc NE 0.
          ls_dms_extn-pf_as = '09003002'.
        ENDIF.
      ENDIF.

*SO
      ls_dms_extn-pf_so = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'L5' ]-pernr  OPTIONAL ).
      IF ls_dms_extn-pf_so IS INITIAL.
        ls_dms_extn-pf_so = '09003001'.
      ELSEIF ls_dms_extn-pf_so = '09003001'.
      ELSE.
        READ TABLE gt_pa0000 INTO gs_pa0000 WITH KEY pernr = ls_dms_extn-pf_so stat2 = '3'.
        IF sy-subrc NE 0.
          ls_dms_extn-pf_so = '09003001'.
        ENDIF.
      ENDIF.

*Channel Manager
      ls_dms_extn-pf_zc = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'ZC' ]-pernr  OPTIONAL ).
      IF ls_dms_extn-pf_zc IS INITIAL.
        ls_dms_extn-pf_zc = '09003007'.
      ELSEIF ls_dms_extn-pf_zc = '09003007'.
      ELSE.
        READ TABLE gt_pa0000 INTO gs_pa0000 WITH KEY pernr = ls_dms_extn-pf_zc stat2 = '3'.
        IF sy-subrc NE 0.
          ls_dms_extn-pf_zc = '09003007'.
        ENDIF.
      ENDIF.

*ZSM
      ls_dms_extn-pf_zs = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'ZS' ]-pernr  OPTIONAL ).
      IF ls_dms_extn-pf_zs IS INITIAL.
        ls_dms_extn-pf_zs = '09003004'.
      ELSEIF ls_dms_extn-pf_zs = '09003004'.
      ELSE.
        READ TABLE gt_pa0000 INTO gs_pa0000 WITH KEY pernr = ls_dms_extn-pf_zs stat2 = '3'.
        IF sy-subrc NE 0.
          ls_dms_extn-pf_zs = '09003004'.
        ENDIF.
      ENDIF.


      ls_dms_extn-pf_xj = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'XJ' ]-lifnr  OPTIONAL ).
      ls_dms_extn-pf_xk = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'XK' ]-lifnr  OPTIONAL ).
      ls_dms_extn-pf_xl = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'XL' ]-lifnr  OPTIONAL ).
      ls_dms_extn-pf_xn = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'XN' ]-lifnr  OPTIONAL ).
      ls_dms_extn-pf_yd = VALUE #( gt_knvp[ kunnr = <lfs_kna1>-kunnr
                                            parvw = 'YD' ]-lifnr  OPTIONAL ).
*Customer Tax Data
      ls_dms_extn-taxkd01 = '0'.
      ls_dms_extn-taxkd02 = '0'.
      ls_dms_extn-taxkd03 = '0'.
      ls_dms_extn-taxkd04 = '1'.
      ls_dms_extn-taxkd05 = '1'.
      ls_dms_extn-taxkd06 = '0'.

      APPEND ls_dms_extn TO gt_dms_extn.

    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_custom_container
*&---------------------------------------------------------------------*
FORM create_custom_container .

  IF gr_grid IS INITIAL.

    DATA(container) =
    NEW cl_gui_custom_container(
      parent =  cl_gui_container=>default_screen
      container_name = 'ALV_GRID').

    gr_grid = NEW cl_gui_alv_grid( i_parent = container ).

    PERFORM display_alv.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_alv
*&---------------------------------------------------------------------*
FORM display_alv .

  REFRESH gt_fieldcat[].
  PERFORM build_fieldcat.

  DATA(ls_layout) = VALUE lvc_s_layo( zebra      = abap_true
                                      cwidth_opt = abap_true ).

  CALL METHOD gr_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = ls_layout
    CHANGING
      it_outtab                     = gt_dms_extn
      it_fieldcatalog               = gt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_fieldcat
*&---------------------------------------------------------------------*
FORM build_fieldcat.
  DATA(ls_fieldcat) = VALUE lvc_s_fcat( ).

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATUS'.
  ls_fieldcat-col_pos    = 1.
  ls_fieldcat-coltext   = 'Status'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KUNNR'.
  ls_fieldcat-col_pos    = 2.
  ls_fieldcat-coltext   = 'Customer'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NAME1'.
  ls_fieldcat-col_pos    = 3.
  ls_fieldcat-coltext   = 'CusName'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LAND1'.
  ls_fieldcat-col_pos    = 4.
  ls_fieldcat-coltext   = 'Country'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BUKRS'.
  ls_fieldcat-col_pos    = 5.
  ls_fieldcat-coltext   = 'CCode'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AKONT'.
  ls_fieldcat-col_pos    = 6.
  ls_fieldcat-coltext   = 'G/L Acc'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ZTERM'.
  ls_fieldcat-col_pos    = 7.
  ls_fieldcat-coltext   = 'Payterms'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VKORG'.
  ls_fieldcat-col_pos    = 8.
  ls_fieldcat-coltext   = 'Salesorg'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VTWEG'.
  ls_fieldcat-col_pos    = 9.
  ls_fieldcat-coltext   = 'DistChannel'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SPART'.
  ls_fieldcat-col_pos    = 10.
  ls_fieldcat-coltext   = 'Division'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BZIRK'.
  ls_fieldcat-col_pos    = 11.
  ls_fieldcat-coltext   = 'Sales Dist'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KDGRP'.
  ls_fieldcat-col_pos    = 12.
  ls_fieldcat-coltext   = 'Cus Group'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VKBUR'.
  ls_fieldcat-col_pos    = 13.
  ls_fieldcat-coltext   = 'Salesoff'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VKGRP'.
  ls_fieldcat-col_pos    = 14.
  ls_fieldcat-coltext   = 'Salgrp New'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KTGRD'.
  ls_fieldcat-col_pos    = 15.
  ls_fieldcat-coltext   = 'AccAssGrp'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KALKS'.
  ls_fieldcat-col_pos    = 16.
  ls_fieldcat-coltext   = 'Pricing'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'INCOV'.
  ls_fieldcat-col_pos    = 17.
  ls_fieldcat-coltext   = 'IncoVersion'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'INCO1'.
  ls_fieldcat-col_pos    = 18.
  ls_fieldcat-coltext   = 'Incoterms1'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'INCO2'.
  ls_fieldcat-col_pos    = 19.
  ls_fieldcat-coltext   = 'Incoterms2'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'INCO_L'.
  ls_fieldcat-col_pos    = 20.
  ls_fieldcat-coltext   = 'Incoterm Loc'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VWERK'.
  ls_fieldcat-col_pos    = 21.
  ls_fieldcat-coltext   = 'Delivery Plant'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VSBED'.
  ls_fieldcat-col_pos    = 22.
  ls_fieldcat-coltext   = 'Ship Cond'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TAXKD01'.
  ls_fieldcat-col_pos    = 23.
  ls_fieldcat-coltext   = 'JOSG'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'taxkd02'.
  ls_fieldcat-col_pos    = 24.
  ls_fieldcat-coltext   = 'JOCG'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TAXKD03'.
  ls_fieldcat-col_pos    = 25.
  ls_fieldcat-coltext   = 'JOIG'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TAXKD04'.
  ls_fieldcat-col_pos    = 26.
  ls_fieldcat-coltext   = 'JOUG'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TAXKD05'.
  ls_fieldcat-col_pos    = 27.
  ls_fieldcat-coltext   = 'JOCS'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TAXKD06'.
  ls_fieldcat-col_pos    = 28.
  ls_fieldcat-coltext   = 'JTC1'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LPRIO'.
  ls_fieldcat-col_pos    = 29.
  ls_fieldcat-coltext   = 'Del priority'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PLTYP'.
  ls_fieldcat-col_pos    = 30.
  ls_fieldcat-coltext   = 'Plist Type'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_SP'.
  ls_fieldcat-col_pos    = 31.
  ls_fieldcat-coltext   = 'Sold to Party'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_BP'.
  ls_fieldcat-col_pos    = 32.
  ls_fieldcat-coltext   = 'Bill to Party'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_PY'.
  ls_fieldcat-col_pos    = 33.
  ls_fieldcat-coltext   = 'Payer'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_SK'.
  ls_fieldcat-col_pos    = 34.
  ls_fieldcat-coltext   = 'Distributor'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_SH'.
  ls_fieldcat-col_pos    = 35.
  ls_fieldcat-coltext   = 'Ship to party'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_NS'.
  ls_fieldcat-col_pos    = 36.
  ls_fieldcat-coltext   = 'National SM'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_RS'.
  ls_fieldcat-col_pos    = 37.
  ls_fieldcat-coltext   = 'Regional SM'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_AS'.
  ls_fieldcat-col_pos    = 38.
  ls_fieldcat-coltext   = 'Area SM'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_SO'.
  ls_fieldcat-col_pos    = 39.
  ls_fieldcat-coltext   = 'Sales Officer'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_ZC'.
  ls_fieldcat-col_pos    = 40.
  ls_fieldcat-coltext   = 'Channel Mgr'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_ZS'.
  ls_fieldcat-col_pos    = 41.
  ls_fieldcat-coltext   = 'Zonal SM'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_XJ'.
  ls_fieldcat-col_pos    = 42.
  ls_fieldcat-coltext   = 'NSM Vendor'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_XK'.
  ls_fieldcat-col_pos    = 43.
  ls_fieldcat-coltext   = 'RSM Vendor'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_XL'.
  ls_fieldcat-col_pos    = 44.
  ls_fieldcat-coltext   = 'ASM Vendor'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_XN'.
  ls_fieldcat-col_pos    = 45.
  ls_fieldcat-coltext   = 'SO Vendor'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PF_YD'.
  ls_fieldcat-col_pos    = 46.
  ls_fieldcat-coltext   = 'ZSM Vendor'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MESSAGE'.
  ls_fieldcat-col_pos    = 47.
  ls_fieldcat-coltext   = 'Message'.
  ls_fieldcat-fix_column   = abap_true.
  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.
