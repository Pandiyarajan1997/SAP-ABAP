*&---------------------------------------------------------------------*
*& Include          ZSD_POS_VENDOR_CREATE_CLS
*&---------------------------------------------------------------------*
CLASS lcl_position_vendor DEFINITION.
  PUBLIC SECTION.
    METHODS: f_get_f4,
      f_convert_excel_data,
      f_create_extend_bp,
*      f_role_extend_bp,
      f_display_alv.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_excel,
             bpnumber TYPE bapibus1006_head-bpartner,
             bpcat    TYPE bapibus1006_head-partn_cat,
*             bpgrp    TYPE bapibus1006_head-partn_grp,
             title    TYPE ad_titletx,
*             sterm    TYPE bu_sort1,
*             name1    TYPE bu_nameor1,
             mail     TYPE ad_smtpadr,
             mobile   TYPE ad_tlnmbr,
           END OF ty_excel.
    TYPES: BEGIN OF alv,
             type(01) TYPE c,
             bpnumber TYPE bapibus1006_head-bpartner,
             sterm    TYPE bu_sort1,
             message  TYPE string,
             r_flag   TYPE xfeld,
             rel_flag TYPE xfeld,
           END OF alv.
    DATA: gt_raw         TYPE truxs_t_text_data.
    DATA: gt_excel       TYPE TABLE OF ty_excel.
    DATA: gt_alv         TYPE TABLE OF alv.
    DATA: partnercategory         TYPE bapibus1006_head-partn_cat, " Business Partner Category
          partnergroup            TYPE bapibus1006_head-partn_grp, " Business Partner Grouping
          centraldataorganization TYPE bapibus1006_central_organ. " SAP BP: BAPI Structure for Organization Data
    DATA: it_telephondata TYPE STANDARD TABLE OF bapiadtel, " BAPI Structure for Telephone Numbers (Bus. Address Services)
          it_maildata     TYPE STANDARD TABLE OF bapiadsmtp, " BAPI Structure for E-Mail Addresses (Bus. Address Services)
          return          TYPE STANDARD TABLE OF bapiret2. " Return Parameter
    DATA: gv_bpcat(01) TYPE n,
          gv_error     TYPE flag.
ENDCLASS.
CLASS lcl_position_vendor IMPLEMENTATION.
  METHOD f_get_f4.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        program_name  = syst-repid
        dynpro_number = syst-dynnr
        field_name    = ' '
        static        = ' '
        mask          = ' '
      CHANGING
        file_name     = p_fname.
  ENDMETHOD.
  METHOD f_convert_excel_data.
    IF p_fname IS NOT INITIAL.
      REFRESH: gt_raw,gt_excel.
      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
          i_line_header        = 'X'
          i_tab_raw_data       = gt_raw
          i_filename           = p_fname
        TABLES
          i_tab_converted_data = gt_excel
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Error in excel conversion' TYPE 'E'.
      ENDIF.
    ELSE.
      MESSAGE 'File is Mandatory' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD f_create_extend_bp.
    DATA: lv_error TYPE flag.
    REFRESH gt_alv.
    LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).
******* Validation for Position ID exsits or not *****************************************************
      SELECT SINGLE * FROM hrp1000 INTO @DATA(ls_hrp1000) WHERE plvar = '01'
                                                          AND otype = 'S'
                                                          AND objid = @<fs_excel>-bpnumber
                                                          AND begda LE @sy-datum
                                                          AND endda GE @sy-datum.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        bpnumber = <fs_excel>-bpnumber
                        sterm = <fs_excel>-bpnumber
                        message = 'Valid Poistion Code not in SAP' ) TO gt_alv.
        CONTINUE.
      ENDIF.
***********Same Search term error ****************************************************************************
      SELECT SINGLE bu_sort1 FROM but000 INTO @DATA(lv_sterm) WHERE partner = @<fs_excel>-bpnumber.
      IF sy-subrc EQ 0.
        APPEND VALUE #( type = 'E'
                        bpnumber = <fs_excel>-bpnumber
                        sterm = <fs_excel>-bpnumber
                        message = 'Same Search term available for another vendor' ) TO gt_alv.
        CONTINUE.
      ENDIF.
************Same Search term error ****************************************************************************
*      SELECT SINGLE name_org1 FROM but000 INTO @DATA(lv_name) WHERE name_org1 = @<fs_excel>-name1.
*      IF sy-subrc EQ 0.
*        APPEND VALUE #( type = 'E'
*                        bpnumber = <fs_excel>-bpnumber
*                        sterm = <fs_excel>-bpnumber
*                        message = 'Same Name Position vendor already exists' ) TO gt_alv.
*        CONTINUE.
*      ENDIF.
******** Title Validation for existence ***************************************************************
      SELECT SINGLE title FROM tsad3t INTO @DATA(lv_title) WHERE title_medi = @<fs_excel>-title.
      IF sy-subrc NE 0.
        APPEND VALUE #( type = 'E'
                        bpnumber = <fs_excel>-bpnumber
                        sterm = <fs_excel>-bpnumber
                        message = 'Title Does not exists' ) TO gt_alv.
        CONTINUE.
      ENDIF.

      CLEAR: businesspartner,gv_bpcat,partnercategory,partnergroup,centraldataorganization,gv_error.
      gv_bpcat = <fs_excel>-bpcat.
      partnercategory = <fs_excel>-bpcat.
*      partnergroup = <fs_excel>-bpgrp.
      partnergroup = 'YEVP'.

      DATA(ls_central) = VALUE bapibus1006_central( searchterm1 = <fs_excel>-bpnumber
                                                    title_key = lv_title ).

      SELECT SINGLE stext FROM hrp1000 INTO @DATA(lv_name) WHERE plvar = '01'
                                          AND otype = 'S'
                                          AND objid = @<fs_excel>-bpnumber
                                          AND begda LE @sy-datum
                                          AND endda GE @sy-datum.
      IF sy-subrc = 0.
        centraldataorganization-name1 = lv_name.
      ENDIF.
**** Adddress data filling in Business Partner Creation **
      DATA(ls_address) = VALUE bapibus1006_address( country = 'IN'
                                                     langu = 'E' ).
      REFRESH: it_telephondata.
      it_telephondata[] = VALUE #(  ( country = 'IN'
                                      telephone = <fs_excel>-mobile
                                      std_no = 'X'
                                      r_3_user = '3'
                                      home_flag = 'X' )  ).
      REFRESH: it_maildata.
      it_maildata[] = VALUE #( ( e_mail = <fs_excel>-mail
                                 std_no = 'X'
                                 std_recip = 'X'
                                 home_flag = 'X'
                                 consnumber = '001' ) ).
*** Bapi to create Business Partner ***
      REFRESH: return.
      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          businesspartnerextern   = <fs_excel>-bpnumber
          partnercategory         = partnercategory
          partnergroup            = partnergroup
          centraldata             = ls_central
*         centraldataperson       = centraldataperson
          centraldataorganization = centraldataorganization
          addressdata             = ls_address
          DUPLICATE_MESSAGE_TYPE  = 'W'
        IMPORTING
          businesspartner         = businesspartner
        TABLES
          telefondata             = it_telephondata
          e_maildata              = it_maildata
          return                  = return.
      READ TABLE return INTO DATA(ls_ret) WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        gv_error = abap_true.
        LOOP AT return INTO DATA(ls_return) WHERE type = 'E' .
          DATA(lv_remarks) = ls_return-message.
          lv_remarks = | { lv_remarks } { ls_return-message } |.
        ENDLOOP.
        APPEND VALUE #( bpnumber = <fs_excel>-bpnumber
                        sterm = <fs_excel>-bpnumber
                        message = lv_remarks ) TO gt_alv.
      ELSE.
        IF gv_error IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
*------- Roles of Business Partner Extension for newly created BP ----*
          PERFORM f_role_extend_bp USING businesspartner CHANGING lv_error.
*------- Relationship a newly created Business Partner is assigned to Position -----*
          IF lv_error = abap_true.
            CONTINUE.
          ELSE.
            CLEAR gv_error.
            PERFORM f_relationship_s_to_bp USING businesspartner CHANGING gv_error.
          ENDIF.
*-----------------------------------------------------------------------------------*
          IF lv_error IS INITIAL AND gv_error IS INITIAL.
            APPEND VALUE #( type = 'S'
                            bpnumber = <fs_excel>-bpnumber
                            sterm = <fs_excel>-bpnumber
                            message = |Business Partner Created Successfully|
                            r_flag = 'X'
                            rel_flag = 'X' ) TO gt_alv.
          ELSE.
            IF lv_error = abap_true AND gv_error = abap_true.
              APPEND VALUE #( type = 'S'
                              bpnumber = <fs_excel>-bpnumber
                              sterm = <fs_excel>-bpnumber
                              message = |Business Partner Created Successfully, But Role is not extended and relshp not created| ) TO gt_alv.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD f_display_alv.
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

    IF gt_alv IS NOT INITIAL.
* Create the ALV object
      TRY.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = lo_gr_alv
            CHANGING
              t_table      = gt_alv.
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.

    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( abap_true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
    TRY.
        lo_column ?= lo_columns->get_column( 'TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Message Type' ).
        lo_column->set_medium_text( 'Msgtyp' ).
        lo_column->set_short_text( 'Msgtyp' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'BPNUMBER' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Business Partner' ).
        lo_column->set_medium_text( 'Business Partner' ).
        lo_column->set_short_text( 'BP Number' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'STERM' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Search Term' ).
        lo_column->set_medium_text( 'Search Term' ).
        lo_column->set_short_text( 'searchterm' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'R_FLAG' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Extension' ).
        lo_column->set_medium_text( 'Extension' ).
        lo_column->set_short_text( 'Extension' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'REL_FLAG' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Relshp flag' ).
        lo_column->set_medium_text( 'relflag' ).
        lo_column->set_short_text( 'Relflag' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Message' ).
        lo_column->set_medium_text( 'Message' ).
        lo_column->set_short_text( 'Message' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    lo_gr_alv->display( ).
  ENDMETHOD.
ENDCLASS.
FORM f_role_extend_bp USING p_partner TYPE bapibus1006_head-bpartner CHANGING error TYPE flag.
  DATA: partner  TYPE bapibus1006_head-bpartner.
  DATA businesspartnerrolecategory TYPE bapibus1006_bproles-partnerrolecategory. " BP Role Category
  DATA all_businesspartnerroles    TYPE bapibus1006_x-mark. " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
  DATA businesspartnerrole         TYPE bapibus1006_bproles-partnerrole. " BP Role
  DATA differentiationtypevalue    TYPE bapibus1006_bproles-difftypevalue. " BP: Differentiation type value
  DATA validfromdate               TYPE bapibus1006_bprole_validity-bprolevalidfrom.
  DATA validuntildate              TYPE bapibus1006_bprole_validity-bprolevalidto.
  DATA return                      TYPE STANDARD TABLE OF bapiret2. " Return Parameter

  CLEAR partner.
  partner = p_partner.
*** Tvarvc variable for Adding roles to BP ****
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarv)
                       WHERE name = 'BP_POS_ROLE_EXTEND'
                       AND type = 'S'.
  IF sy-subrc = 0.

  ENDIF.
  LOOP AT lt_tvarv INTO DATA(ls_tvarv).

    businesspartnerrole = ls_tvarv-low.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = partner
      IMPORTING
        output = partner.

    CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
      EXPORTING
        businesspartner             = partner
        businesspartnerrolecategory = businesspartnerrolecategory
        all_businesspartnerroles    = ' '
        businesspartnerrole         = businesspartnerrole
        differentiationtypevalue    = differentiationtypevalue
*       VALIDFROMDATE               = P_DATE
        validuntildate              = '99991231'
      TABLES
        return                      = return.
    READ TABLE return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type = 'E'.
    IF sy-subrc = 0 .
      error = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

    DO 5 TIMES.
      SELECT rltyp INTO TABLE @DATA(lv_rltyp) FROM but100 UP TO 1 ROWS WHERE partner = @businesspartner
                                                                 AND rltyp = @ls_tvarv-low.

      IF lv_rltyp IS INITIAL.
        WAIT UP TO 1 SECONDS.  "comment by Roton
      ELSE.
        CONTINUE.
      ENDIF.
    ENDDO.
  ENDLOOP.
ENDFORM.
FORM f_relationship_s_to_bp USING p_partner TYPE bapibus1006_head-bpartner CHANGING error TYPE flag.
  CONSTANTS: gc_high_date TYPE hrp1000-endda VALUE '99991231'.
  DATA ls_p1000 TYPE p1000.
  DATA ls_p1001 TYPE p1001.
  DATA: lv_objid TYPE p1001-objid.
  DATA lt_p1000 TYPE TABLE OF p1000.
  DATA lt_p1001 TYPE TABLE OF p1001.

  DATA: lt_enque_tab TYPE TABLE OF ppenq,
        ls_enque_tab TYPE ppenq.
  CONSTANTS: act_vtask LIKE hrrhap-vtask VALUE 'B'.
  DATA(lv_partner) = p_partner.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_partner
    IMPORTING
      output = lv_partner.
  CLEAR lv_objid.
** POSITION id checks **
  SELECT SINGLE objid, begda FROM hrp1000
                            INTO @DATA(ls_hrp1000)
                            WHERE plvar = '01'
                            AND otype EQ 'S'
                            AND istat EQ '1'
                            AND objid = @p_partner
                            AND begda <= @sy-datum AND endda GE @sy-datum
                            AND langu = @sy-langu.
  IF sy-subrc EQ 0.
    CLEAR lt_p1001.
    CLEAR ls_p1001.
    ls_p1001-objid = ls_hrp1000-objid.
    ls_p1001-otype = 'S'.
    ls_p1001-istat = '1'.
    ls_p1001-plvar = '01'.
    ls_p1001-infty = '1001'.
    ls_p1001-rsign = 'A'.
    ls_p1001-relat = '008'.
    ls_p1001-prozt = 0.
    ls_p1001-begda = ls_hrp1000-begda.
    ls_p1001-endda = gc_high_date.
    ls_p1001-sclas = 'BP'.
    ls_p1001-sobid = lv_partner.
    APPEND ls_p1001 TO lt_p1001.

    CLEAR ls_enque_tab.
    REFRESH: lt_enque_tab.

    ls_enque_tab-plvar = ls_p1001-plvar.
    ls_enque_tab-otype = ls_p1001-sclas.
    ls_enque_tab-objid = lv_objid+0(8).
    APPEND ls_enque_tab TO lt_enque_tab.

    CALL FUNCTION 'RH_ENQUEUE_LIST'
      TABLES
        enq_tab = lt_enque_tab
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        fcode               = 'INSE'
        vtask               = act_vtask
      TABLES
        innnn               = lt_p1001
      EXCEPTIONS
        corr_exit           = 01
        no_authorization    = 08
        error_during_insert = 04
        OTHERS              = 04.

    IF sy-subrc EQ 4.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'RH_DEQUEUE_LIST'
        TABLES
          deq_tab = lt_enque_tab.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'DEQUEUE_ALL'.
      error = 'X'.
      EXIT.
    ELSEIF sy-subrc EQ 8.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'RH_DEQUEUE_LIST'
        TABLES
          deq_tab = lt_enque_tab.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'DEQUEUE_ALL'.
      error = 'X'.
      EXIT.
    ELSEIF sy-subrc EQ 1.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'RH_DEQUEUE_LIST'
        TABLES
          deq_tab = lt_enque_tab.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'DEQUEUE_ALL'.
      error = 'X'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'RH_UPDATE_DATABASE'
      EXPORTING
        vtask     = 'D'
      EXCEPTIONS
        corr_exit = 1
        OTHERS    = 2.

    IF sy-subrc GT 0.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      error = 'X'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'RH_DEQUEUE_LIST'
      TABLES
        deq_tab = lt_enque_tab.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    CALL FUNCTION 'DEQUEUE_ALL'.
  ENDIF.
ENDFORM.
