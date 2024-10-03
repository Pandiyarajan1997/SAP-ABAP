class ZCL_API_HR_POSITION_CREATE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_HR_POSITION_CREATE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*Created By: Samsudeen M
*Created On: 07.11.2023
*Purpose : Position Master Creation from MIS Based on Required Inputs
*---------------------------------------------------------------*
    TYPES: BEGIN OF ts_input,
             orgid      TYPE orgeh,
             jobcode    TYPE stell,
             posdesc    TYPE short_d,
             posldesc   TYPE stext,
             startdate  TYPE begda,
             repmgpos   TYPE plans,
             latitude   TYPE zhrlatit,
             longitude  TYPE zhrlong,
             travel     TYPE zhr_travel,
             category   TYPE zhr_cat,
             costcenter TYPE kostl,
             emptyp(01) TYPE c,
           END OF ts_input.

    DATA: gs_input TYPE ts_input.

    TYPES: BEGIN OF ts_output,
             positionid TYPE plans,
             posdesc    TYPE short_d,
             posldesc   TYPE stext,
             startdate  TYPE begda,
             repmgpos   TYPE plans,
             latitude   TYPE zhrlatit,
             longitude  TYPE zhrlong,
             travel     TYPE zhr_travel,
             category   TYPE zhr_cat,
             costcenter TYPE kostl,
             bpartner   TYPE bu_partner,
             type(01)   TYPE c,
             msg        TYPE string,
           END OF ts_output.
    DATA: gt_output TYPE TABLE OF ts_output.

    CONSTANTS: c_e TYPE c VALUE 'E',
               c_s TYPE c VALUE 'S'.

    DATA: lr_request  TYPE REF TO if_http_request,
          lr_response TYPE REF TO if_http_response.

    DATA: lv_msg TYPE string,
          l_msg  TYPE string.

    DATA: v_jsonload TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    DATA: lo_position_cls  TYPE REF TO zcl_hr_position_master.
    CREATE OBJECT lo_log_upd.
    CREATE OBJECT lo_position_cls.

    DATA ls_p1000 TYPE p1000.
    DATA ls_p1001 TYPE p1001.
    DATA ls_p9013 TYPE p9013.
    DATA ls_p9011 TYPE p9011.


    DATA lt_p9013 TYPE TABLE OF p9013.
    DATA lt_p9011 TYPE TABLE OF p9011.

    DATA lt_p1000 TYPE TABLE OF p1000.
    DATA lt_p1001 TYPE TABLE OF p1001.

    DATA: lt_enque_tab TYPE TABLE OF ppenq,
          ls_enque_tab TYPE ppenq.

    CONSTANTS: act_vtask    TYPE hrrhap-vtask VALUE 'B',
               gc_otype_o   TYPE hrp1000-otype VALUE 'O',
               gc_otype_s   TYPE hrp1000-otype VALUE 'S',
               gc_otype_c   TYPE hrp1000-otype VALUE 'C',
               gc_otype_k   TYPE hrp1000-otype VALUE 'K',
               gc_plan_ver  TYPE hrp1000-plvar VALUE '01',
               gc_rsign_a   TYPE hrp1001-rsign VALUE 'A',
               gc_rsign_b   TYPE hrp1001-rsign VALUE 'B',
               gc_high_date TYPE hrp1000-endda VALUE '99991231'.

    CONSTANTS: gc_relat_002 TYPE hrp1001-relat VALUE '002',
               gc_relat_003 TYPE hrp1001-relat VALUE '003',
               gc_relat_007 TYPE hrp1001-relat VALUE '007',
               gc_relat_011 TYPE hrp1001-relat VALUE '011'.

    lr_request = server->request.
    lr_response = server->response.

* Check the Calling Method
    IF lr_request->get_method( ) EQ 'POST'.
      CALL METHOD lr_request->get_cdata RECEIVING data = DATA(lv_data_tmp).

** Deserialize the input our required input ***
      /ui2/cl_json=>deserialize(
      EXPORTING
       json         = lv_data_tmp
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
       assoc_arrays = abap_true
      CHANGING
       data         = gs_input ).

      IF gs_input IS NOT INITIAL.

        CLEAR lv_msg.

        IF gs_input-orgid IS INITIAL.
          lv_msg = 'Org Unit ID Missing'.

        ELSEIF gs_input-jobcode IS INITIAL.
          lv_msg = 'Job Code is Missing'.

        ELSEIF gs_input-posdesc IS INITIAL.
          lv_msg = 'Position Short Description Missing'.

        ELSEIF gs_input-posldesc IS INITIAL.
          lv_msg = 'Position Long Description is Missing'.

        ELSEIF gs_input-repmgpos IS INITIAL.
          lv_msg = 'Reporting Manager Position is Missing'.

        ELSEIF gs_input-startdate IS INITIAL.
          lv_msg = 'Start Date is Missing'.

*        ELSEIF gs_input-travel IS INITIAL.
*          lv_msg = 'Travel is Missing'.

        ELSEIF gs_input-travel EQ abap_true AND gs_input-category IS INITIAL.
          lv_msg = 'Travel Category is Mandatory'.

        ELSEIF gs_input-costcenter IS INITIAL.
          lv_msg = 'Costcenter is Mandatory'.
        ENDIF.

        IF gs_input-orgid IS NOT INITIAL.
          SELECT SINGLE objid FROM hrp1000 INTO @DATA(l_orgid) WHERE plvar = @gc_plan_ver
                              AND otype = @gc_otype_o
                              AND objid = @gs_input-orgid
                              AND istat = '1'
                              AND begda LE @sy-datum
                              AND endda GE @sy-datum.
          IF sy-subrc NE 0.
            lv_msg = |{ lv_msg },Org Unit ID is Incorrect|.
          ENDIF.
        ENDIF.

        IF gs_input-jobcode IS NOT INITIAL.
          SELECT SINGLE objid FROM hrp1000 INTO @DATA(l_jobcode) WHERE plvar = @gc_plan_ver
                              AND otype = @gc_otype_c
                              AND objid = @gs_input-jobcode
                              AND istat = '1'
                              AND begda LE @sy-datum
                              AND endda GE @sy-datum.
          IF sy-subrc NE 0.
            lv_msg = |{ lv_msg },Jobcode is Incorrect|.
          ENDIF.
        ENDIF.

        IF gs_input-costcenter IS NOT INITIAL.
          TRANSLATE gs_input-costcenter TO UPPER CASE.
          SELECT SINGLE kostl FROM csks INTO @DATA(l_costcenter) WHERE kostl = @gs_input-costcenter.
          IF sy-subrc NE 0.
            lv_msg = |{ lv_msg },Costcenter is Incorrect|.
          ENDIF.
        ENDIF.

        DATA(ls_output) = VALUE ts_output( ).
        ls_output = CORRESPONDING #( gs_input ).
        REFRESH: gt_output.

        IF lv_msg IS NOT INITIAL.
          ls_output-type = c_e.
          ls_output-msg = lv_msg.
          APPEND ls_output TO gt_output.

        ELSE.
*Getting Next Position ID Code
          CLEAR ls_p1000.
          CALL METHOD lo_position_cls->get_position_id
            IMPORTING
              position_id = DATA(lv_position).
          ls_output-positionid = lv_position.

          REFRESH: lt_p1000,lt_p1001,lt_p9011,lt_p9013.
          "Info Type HRP1000 Updation
          APPEND VALUE #( objid = ls_output-positionid
                          otype = gc_otype_s
                          istat = '1'
                          plvar = gc_plan_ver
                          infty = '1000'
                          langu = sy-langu
                          short = gs_input-posdesc
                          stext = gs_input-posldesc
                          begda = gs_input-startdate
                          endda = gc_high_date ) TO lt_p1000.

          "Info Type HRP1001 Mapping for Org unit ID
          APPEND VALUE #( objid = ls_output-positionid
                          otype = gc_otype_s
                          istat = '1'
                          plvar = gc_plan_ver
                          infty = '1001'
                          rsign = gc_rsign_a
                          relat = gc_relat_003
                          prozt = 0
                          begda = gs_input-startdate
                          endda = gc_high_date
                          sclas = gc_otype_o
                          sobid = gs_input-orgid ) TO lt_p1001.

          "Info Type HRP1001 Mapping for JobCode
          APPEND VALUE #( objid = ls_output-positionid
                          otype = gc_otype_s
                          istat = '1'
                          plvar = gc_plan_ver
                          infty = '1001'
                          rsign = gc_rsign_a
                          relat = gc_relat_003
                          prozt = 0
                          begda = gs_input-startdate
                          endda = gc_high_date
                          sclas = gc_otype_c
                          sobid = gs_input-jobcode ) TO lt_p1001.

          "Info Type HRP1001 Mapping for Reporting Manager Position
          APPEND VALUE #( objid = ls_output-positionid
                          otype = gc_otype_s
                          istat = '1'
                          plvar = gc_plan_ver
                          infty = '1001'
                          rsign = gc_rsign_a
                          relat = gc_relat_003
                          prozt = 0
                          begda = gs_input-startdate
                          endda = gc_high_date
                          sclas = gc_otype_s
                          sobid = gs_input-repmgpos ) TO lt_p1001.

          "Info Type HRP9013 Mapping For Travel Category
          APPEND VALUE #( objid = ls_output-positionid
                          otype = gc_otype_s
                          istat = '1'
                          plvar = gc_plan_ver
                          infty = '9013'
                          begda = gs_input-startdate
                          endda = gc_high_date
                          travel = gs_input-travel
                          category = gs_input-category ) TO lt_p9013.

**Latitude & Longitide Mapping its Optional One
*          IF gs_input-latitude IS NOT INITIAL AND gs_input-longitude IS NOT INITIAL.
*            "Info Type HRP9011 Mapping For Latitude & Longitude
*            APPEND VALUE #( objid = ls_output-positionid
*                            otype = gc_otype_s
*                            istat = '1'
*                            plvar = gc_plan_ver
*                            infty = '9011'
*                            begda = gs_input-startdate
*                            endda = gc_high_date
*                            latitude = gs_input-latitude
*                            longitude = gs_input-longitude ) TO lt_p9011.
*          ENDIF.

          CLEAR ls_enque_tab.
          REFRESH: lt_enque_tab.
          "Infotype HRP1000
          ls_enque_tab-plvar = gc_plan_ver.
          ls_enque_tab-otype = gc_otype_s.
          ls_enque_tab-objid = ls_output-positionid.
          APPEND ls_enque_tab TO lt_enque_tab.
          "Infotype HRP1001 for Org Unit ID
          ls_enque_tab-plvar = gc_plan_ver.
          ls_enque_tab-otype = gc_otype_o.
          ls_enque_tab-objid = gs_input-orgid+0(8).
          APPEND ls_enque_tab TO lt_enque_tab.
          "Infotype HRP1001 for Jobcode
          ls_enque_tab-plvar = gc_plan_ver.
          ls_enque_tab-otype = gc_otype_c.
          ls_enque_tab-objid = gs_input-jobcode+0(8).
          APPEND ls_enque_tab TO lt_enque_tab.
          "Infotype HRP1001 for Rep Manager Position
          ls_enque_tab-plvar = gc_plan_ver.
          ls_enque_tab-otype = gc_otype_s.
          ls_enque_tab-objid = gs_input-repmgpos+0(8).
          APPEND ls_enque_tab TO lt_enque_tab.

*Enqueue List Method to Enqueue Specific Position'
          CALL METHOD lo_position_cls->enqueue_list
            EXPORTING
              zhr_enqueue_list = lt_enque_tab
            IMPORTING
              msg              = lv_msg.

          IF lv_msg IS NOT INITIAL.
            ls_output-type = c_e.
            ls_output-msg = lv_msg.
            APPEND ls_output TO gt_output.
          ELSE.
*Insert Operation for Infotype HRP1000 method
            CALL METHOD lo_position_cls->infty_1000
              EXPORTING
                it_p1000        = lt_p1000
                it_enqueue_list = lt_enque_tab
              IMPORTING
                msg             = lv_msg.

            IF lv_msg IS NOT INITIAL.
              ls_output-type = c_e.
              ls_output-msg = lv_msg.
              APPEND ls_output TO gt_output.
            ELSE.
*Insert Operation for Infotype HRP1001 method
              CALL METHOD lo_position_cls->infty_1001
                EXPORTING
                  it_p1001        = lt_p1001
                  it_enqueue_list = lt_enque_tab
                IMPORTING
                  msg             = lv_msg.

              IF lv_msg IS NOT INITIAL.
                ls_output-type = c_e.
                ls_output-msg = lv_msg.
                APPEND ls_output TO gt_output.
              ELSE.
              ENDIF.

*Insert Operation for Infotype HRP9011 Method
              IF gs_input-category IS NOT INITIAL.
                CALL METHOD lo_position_cls->infty_9011
                  EXPORTING
                    it_p9011        = lt_p9011
                    it_enqueue_list = lt_enque_tab
                  IMPORTING
                    msg             = lv_msg.

                IF lv_msg IS NOT INITIAL.
                  ls_output-type = c_e.
                  ls_output-msg = lv_msg.
                  APPEND ls_output TO gt_output.
                ELSE.
                ENDIF.
              ENDIF.

*Insert Operation for Infortype HRP9013 Method
              IF gs_input-latitude IS NOT INITIAL AND gs_input-longitude IS NOT INITIAL.
                CALL METHOD lo_position_cls->infty_9013
                  EXPORTING
                    it_p9013        = lt_p9013
                    it_enqueue_list = lt_enque_tab
                  IMPORTING
                    msg             = lv_msg.
                IF lv_msg IS NOT INITIAL.
                  ls_output-type = c_e.
                  ls_output-msg = lv_msg.
                  APPEND ls_output TO gt_output.
                ELSE.

                ENDIF.
              ENDIF.

              CALL METHOD lo_position_cls->update_database.

*Costcenter Relationship Mapping Creation
              CALL METHOD lo_position_cls->costcenter_relationship
                EXPORTING
                  position_id = ls_output-positionid
                  start_date  = gs_input-startdate
                  costcenter  = gs_input-costcenter
                IMPORTING
                  msg         = lv_msg.

              IF lv_msg IS NOT INITIAL.
                ls_output-type = c_e.
                ls_output-msg = lv_msg.
                APPEND ls_output TO gt_output.
              ELSE.
              ENDIF.

*If the Position is for Non Sales Employeee
              IF gs_input-emptyp EQ 'S'.
                "Extending the Business Partner
                CALL METHOD lo_position_cls->business_partner_extend
                  EXPORTING
                    position_id     = ls_output-positionid
                    posldesc        = gs_input-posldesc
                  IMPORTING
                    businesspartner = ls_output-bpartner
                    msg             = lv_msg.
                IF ls_output-bpartner IS NOT INITIAL.
                  CLEAR ls_p1001.
                  CLEAR ls_p1001.
                  ls_p1001-objid = ls_output-positionid.
                  ls_p1001-otype = gc_otype_s.
                  ls_p1001-istat = '1'.
                  ls_p1001-plvar = gc_plan_ver.
                  ls_p1001-infty = '1001'.
                  ls_p1001-rsign = gc_rsign_a.
                  ls_p1001-relat = '008'.
                  ls_p1001-prozt = 0.
                  ls_p1001-begda = gs_input-startdate.
                  ls_p1001-endda = gc_high_date.
                  ls_p1001-sclas = 'BP'.
                  ls_p1001-sobid = ls_output-bpartner.
                  "Relationship Mapping Between Position and Business Partner
                  CALL METHOD lo_position_cls->business_partner_relationship
                    EXPORTING
                      is_p1001 = ls_p1001
                    IMPORTING
                      msg      = lv_msg.

                  IF lv_msg IS NOT INITIAL.
                    ls_output-type = c_e.
                    ls_output-msg = lv_msg.
                    APPEND ls_output TO gt_output.
                  ELSE.
                    ls_output-type = c_s.
                    ls_output-msg = |All Objects Creation and Mapping Completed |.
                    APPEND ls_output TO gt_output.
                  ENDIF.

                ELSE.
                  ls_output-type = c_e.
                  ls_output-msg = lv_msg.
                  APPEND ls_output TO gt_output.
                ENDIF.
              ELSE.
                ls_output-type = c_s.
                ls_output-msg  = |All Objects Creation and Mapping Completed |.
                APPEND ls_output TO gt_output.
              ENDIF.
*              ENDIF.
*                ENDIF.
*              ENDIF.
*                ENDIF.
*              ENDIF.
*              ENDIF.
            ENDIF.

          ENDIF.
        ENDIF.
*No Input is Captured in Deserialization
      ELSE.
        v_jsonload = 'No Input is Captured'.
*output Entry in LOG Table
        CALL METHOD lo_log_upd->log_entry_store
          EXPORTING
            apiname         = 'POSITION'
            ijson           = lv_data_tmp
            ojson           = v_jsonload
          EXCEPTIONS
            apiname_missing = 1
            json_missing    = 2
            OTHERS          = 3.
        IF sy-subrc = 0.

        ENDIF.
*Set JSON Content-Type
        CALL METHOD lr_response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        CALL METHOD lr_response->set_cdata( data = v_jsonload ).
      ENDIF.
      IF gt_output[] IS NOT INITIAL.
** serialize the output for response ***
        /ui2/cl_json=>serialize(
        EXPORTING
         data         =  gt_output
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        RECEIVING
         r_json         = DATA(lv_body) ).

*output Entry in LOG Table
        CALL METHOD lo_log_upd->log_entry_store
          EXPORTING
            apiname         = 'POSITION'
            ijson           = lv_data_tmp
            ojson           = lv_body
          EXCEPTIONS
            apiname_missing = 1
            json_missing    = 2
            OTHERS          = 3.
        IF sy-subrc = 0.

        ENDIF.

*Set JSON Content-Type
        CALL METHOD lr_response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        CALL METHOD lr_response->set_cdata( data = lv_body ).

      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
