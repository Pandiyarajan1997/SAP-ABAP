class ZCL_API_HR_EMP_POSITION_CHANGE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_HR_EMP_POSITION_CHANGE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*****API for employee / manager position changes
    "Created by: Pandiarajan
    "Created on: 30.09.2024
    "Reference by: Ramakrishnan J
    "-----------------------------------------------------------------------------------*
    TYPES: BEGIN OF ty_input,
             emp_id              TYPE p_pernr,
             empcurrent_position TYPE hrp1000-objid,
             action_type         TYPE char02,               "MC - reporting manager change
             effective_date      TYPE datum,                "AP - additional Position assignment
             new_position        TYPE hrp1000-objid,
           END OF ty_input.

    TYPES: BEGIN OF ty_msg,
             emp_id              TYPE p_pernr,
             empcurrent_position TYPE hrp1000-objid,
             action_type         TYPE char02,
             effective_date      TYPE datum,
             new_position        TYPE hrp1000-objid,
             status              TYPE bapi_mtype,
             message             TYPE string,
           END OF ty_msg.

    DATA : gt_input TYPE TABLE OF ty_input.

    DATA : gt_response TYPE TABLE OF ty_msg.
    DATA : gs_response TYPE ty_msg.

    DATA : lv_body TYPE string.
    DATA : v_jsonload TYPE string.
    DATA : lv_data TYPE string.
    DATA : lv_msg TYPE string.

    DATA : ls_p1001 TYPE p1001.
    DATA : lt_p1001 TYPE TABLE OF p1001.

    DATA : lt_enque_tab TYPE TABLE OF ppenq,
           ls_enque_tab TYPE ppenq.
    CONSTANTS: act_vtask TYPE hrrhap-vtask VALUE 'B'.
    CONSTANTS: gc_plan_ver TYPE hrp1000-plvar VALUE '01'.
    CONSTANTS: gc_rsign_a TYPE hrp1001-rsign VALUE 'A',
               gc_otype_s TYPE hrp1000-otype VALUE 'S'.

**********normal api log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = gt_input ). "lr_data ).

    LOOP AT gt_input INTO DATA(gs_input).
*    for reporting manager change
      IF gs_input-action_type = 'MC'.
        DATA(lv_sclas) = 'S'.
        DATA(lv_relat) = '002'.
        DATA(lv_objid) = gs_input-empcurrent_position.  "emp current position
        DATA(lv_sobji) = gs_input-new_position.         "reporting manager position
*    for additional position change
      ELSEIF gs_input-action_type = 'AP'.
        lv_sclas = 'P'.
        lv_relat = '008'.
        lv_objid = gs_input-new_position.               "emp new additional position
        lv_sobji = gs_input-emp_id.                     "emp id
      ENDIF.

*Position to existing Manager Position check
      SELECT SINGLE sobid FROM hrp1001
        INTO @DATA(l_pos_pernr)
        WHERE otype = 'S'
        AND objid = @lv_objid
        AND plvar = '01'
        AND rsign = 'A'
        AND relat = @lv_relat
        AND begda LE @sy-datum
        AND endda GE @sy-datum
        AND sclas = @lv_sclas
        AND sobid = @lv_sobji.
      IF sy-subrc = 0.

        gs_response-status  = 'S'.
        IF gs_input-action_type = 'MC'.
          gs_response-message = |Position already assigned to given manager - { gs_input-new_position }|.
        ELSEIF gs_input-action_type = 'AP'.
          gs_response-message = |Employee already assigned to Given addtl position - { gs_input-new_position }|.
        ENDIF.

      ELSE.

*validate employee number
        SELECT SINGLE stat2 FROM pa0000
          INTO @DATA(empstatus)
          WHERE pernr = @gs_input-emp_id
          AND begda LE @sy-datum
          AND endda GE @sy-datum.
        IF sy-subrc <> 0.
          lv_msg = |incorrect employee code { gs_input-emp_id } , { lv_msg }|.
        ELSE.
          IF empstatus <> '3'.
            lv_msg = |employee code { gs_input-emp_id } is not Active , { lv_msg }|.
          ENDIF.
        ENDIF.

*******************get the employee position**********
        SELECT SINGLE sobid FROM hrp1001
          INTO @l_pos_pernr
          WHERE otype = 'S'
          AND objid = @gs_input-empcurrent_position
          AND plvar = '01'
          AND rsign = 'A'
          AND relat = '008'
          AND begda LE @sy-datum
          AND endda GE @sy-datum
          AND sclas = 'P'
          AND sobid = @gs_input-emp_id.
        IF sy-subrc <> 0.
          lv_msg = |Current position { gs_input-empcurrent_position } is mismatch with employee code { gs_input-emp_id } , { lv_msg }|.
        ENDIF.

        SELECT SINGLE objid FROM hrp1000
          INTO @DATA(lv_pos)
          WHERE plvar = '01'
          AND   otype = 'S'
          AND   objid = @gs_input-new_position
          AND   begda LE @sy-datum
          AND   endda GE @sy-datum.
        IF sy-subrc <> 0.
          lv_msg = |incorrect New position - { gs_input-new_position } , { lv_msg }|.
        ENDIF.

*********************check the current and new position id**************
        IF gs_input-empcurrent_position = gs_input-new_position.
          lv_msg = |Current position { gs_input-empcurrent_position } & new position both are same { gs_input-new_position } , { lv_msg }|.
        ENDIF.

        IF lv_msg IS INITIAL.

          REFRESH: lt_p1001.
          CLEAR ls_p1001.
          ls_p1001-objid = lv_objid.
          ls_p1001-otype = gc_otype_s.
          ls_p1001-istat = '1'.
          ls_p1001-plvar = gc_plan_ver.
          ls_p1001-infty = '1001'.
          ls_p1001-rsign = gc_rsign_a.
          ls_p1001-relat = lv_relat.
          ls_p1001-prozt = 0.
          ls_p1001-begda = gs_input-effective_date.
          ls_p1001-endda = '99991231'.
          ls_p1001-sclas = lv_sclas.
          ls_p1001-sobid = lv_sobji.
          APPEND ls_p1001 TO lt_p1001.

          CLEAR ls_enque_tab.
          REFRESH: lt_enque_tab.

          ls_enque_tab-plvar = ls_p1001-plvar.
          ls_enque_tab-otype = ls_p1001-sclas.
          ls_enque_tab-objid = gs_input-new_position+0(8).
          APPEND ls_enque_tab TO lt_enque_tab.

          CALL FUNCTION 'RH_ENQUEUE_LIST'
            TABLES
              enq_tab = lt_enque_tab
            EXCEPTIONS
              OTHERS  = 1.
          IF sy-subrc NE 0.
            gs_response-status = 'E'.
            MESSAGE ID sy-msgid TYPE sy-msgty
                    NUMBER sy-msgno
                    INTO gs_response-message
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
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
              gs_response-status  = 'E'.
              gs_response-message = 'Error during creating the relationship record'.
            ELSEIF sy-subrc EQ 8.
              CALL FUNCTION 'RH_CLEAR_BUFFER'.
              CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
              CALL FUNCTION 'RH_DEQUEUE_LIST'
                TABLES
                  deq_tab = lt_enque_tab.
              CALL FUNCTION 'RH_CLEAR_BUFFER'.
              CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
              CALL FUNCTION 'DEQUEUE_ALL'.
              gs_response-status  = 'E'.
              gs_response-message = 'No Authorization to Create Relationship record'.
            ELSEIF sy-subrc EQ 1.
              CALL FUNCTION 'RH_CLEAR_BUFFER'.
              CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
              CALL FUNCTION 'RH_DEQUEUE_LIST'
                TABLES
                  deq_tab = lt_enque_tab.
              CALL FUNCTION 'RH_CLEAR_BUFFER'.
              CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
              CALL FUNCTION 'DEQUEUE_ALL'.
              gs_response-status  = 'E'.
              gs_response-message = 'Error during Saving the Relationship Record'.

            ELSE.

              CALL FUNCTION 'RH_UPDATE_DATABASE'
                EXPORTING
                  vtask     = 'D'
                EXCEPTIONS
                  corr_exit = 1
                  OTHERS    = 2.

              IF sy-subrc GT 0.
                CALL FUNCTION 'RH_CLEAR_BUFFER'.
                CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
                gs_response-status  = 'E'.
                gs_response-message = 'Error during updating the database'.
              ELSE.
                gs_response-status  = 'S'.

                IF gs_input-action_type = 'MC'.
                  gs_response-message = 'Reporting Manager Changed Successfully'.
                ELSEIF gs_input-action_type = 'AP'.
                  gs_response-message = 'Additional position Assigned Successfully'.
                ENDIF.
              ENDIF.
            ENDIF.

            CALL FUNCTION 'RH_DEQUEUE_LIST'
              TABLES
                deq_tab = lt_enque_tab.
            CALL FUNCTION 'RH_CLEAR_BUFFER'.
            CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
            CALL FUNCTION 'DEQUEUE_ALL'.
          ENDIF.
        ELSE.
          gs_response-status  = 'E'.
          gs_response-message = lv_msg.
        ENDIF.
      ENDIF.

      gs_response-action_type         = gs_input-action_type        .
      gs_response-effective_date      = gs_input-effective_date     .
      gs_response-emp_id              = gs_input-emp_id             .
      gs_response-empcurrent_position = gs_input-empcurrent_position.
      gs_response-new_position        = gs_input-new_position       .
      APPEND : gs_response TO gt_response.
      CLEAR  : gs_input,gs_response,lv_sclas,lv_relat,lv_objid,lv_sobji.

    ENDLOOP.

** serialize the output for response ***
    /ui2/cl_json=>serialize(
    EXPORTING
     data         =  gt_response
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    RECEIVING
     r_json         = lv_body ).

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'POSITION_CHANGE'
        ijson           = lv_data
        ojson           = lv_body
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = lv_body ).

  ENDMETHOD.
ENDCLASS.
