class ZCL_HR_POSITION_MASTER definition
  public
  final
  create public .

public section.

  types:
    it_p9013_ty TYPE STANDARD TABLE OF p9013 .
  types:
    it_p9011_ty TYPE STANDARD TABLE OF p9011 .
  types:
    it_p1001_ty TYPE STANDARD TABLE OF p1001 .
  types:
    it_p1000_ty      TYPE STANDARD TABLE OF p1000 .
  types:
    zhr_enqueue_list TYPE STANDARD TABLE OF ppenq .

  constants ACT_VTASK type HRRHAP-VTASK value 'B' ##NO_TEXT.
  data WA_BDCDATA type BDCDATA .
  data:
    it_bdcdata TYPE STANDARD TABLE OF bdcdata .
  data:
    gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll .
  data GS_BDCMSG type BDCMSGCOLL .

  methods GET_POSITION_ID
    exporting
      !POSITION_ID type PLOG-OBJID .
  methods ENQUEUE_LIST
    importing
      !ZHR_ENQUEUE_LIST type ZHR_ENQUEUE_LIST
    exporting
      !MSG type STRING .
  methods INFTY_1000
    importing
      !IT_P1000 type IT_P1000_TY
      !IT_ENQUEUE_LIST type ZHR_ENQUEUE_LIST
    exporting
      !MSG type STRING .
  methods INFTY_1001
    importing
      !IT_P1001 type IT_P1001_TY
      !IT_ENQUEUE_LIST type ZHR_ENQUEUE_LIST
    exporting
      !MSG type STRING .
  methods INFTY_9011
    importing
      !IT_P9011 type IT_P9011_TY
      !IT_ENQUEUE_LIST type ZHR_ENQUEUE_LIST
    exporting
      !MSG type STRING .
  methods INFTY_9013
    importing
      !IT_P9013 type IT_P9013_TY
      !IT_ENQUEUE_LIST type ZHR_ENQUEUE_LIST
    exporting
      !MSG type STRING .
  methods UPDATE_DATABASE .
  methods DEQUEUE_LIST
    importing
      !IT_ENQUEUE_LIST type ZHR_ENQUEUE_LIST .
  methods COSTCENTER_RELATIONSHIP
    importing
      !POSITION_ID type HRP1000-OBJID
      !START_DATE type BEGDA
      !COSTCENTER type CSKS-KOSTL
    exporting
      !MSG type STRING .
  methods BUSINESS_PARTNER_EXTEND
    importing
      !POSITION_ID type HRP1000-OBJID
      !POSLDESC type HRP1000-STEXT
    exporting
      !BUSINESSPARTNER type BU_PARTNER
      !MSG type STRING .
  methods BUSINESS_PARTNER_ROLE_EXTEND
    importing
      !BUSINESSPARTNER type BU_PARTNER .
  methods BUSINESS_PARTNER_RELATIONSHIP
    importing
      !IS_P1001 type P1001
    exporting
      !MSG type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HR_POSITION_MASTER IMPLEMENTATION.


  METHOD business_partner_extend.

    DATA: partnercategory         TYPE bapibus1006_head-partn_cat, " Business Partner Category
          partnergroup            TYPE bapibus1006_head-partn_grp, " Business Partner Grouping
          centraldataorganization TYPE bapibus1006_central_organ. " SAP BP: BAPI Structure for Organization Data
    DATA: it_telephondata TYPE STANDARD TABLE OF bapiadtel, " BAPI Structure for Telephone Numbers (Bus. Address Services)
          it_maildata     TYPE STANDARD TABLE OF bapiadsmtp, " BAPI Structure for E-Mail Addresses (Bus. Address Services)
          return          TYPE STANDARD TABLE OF bapiret2. " Return Parameter
    DATA: l_msg TYPE string.

    IF position_id IS NOT INITIAL.

      DATA(lv_partner) = CONV bapibus1006_head-bpartner( position_id ).
      CLEAR: businesspartner,partnercategory,partnergroup,centraldataorganization.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_partner
        IMPORTING
          output = lv_partner.

*  gv_bpcat = <fs_excel>-bpcat.
      partnercategory = '2'.
      partnergroup = 'YEVP'.
      DATA(ls_central) = VALUE bapibus1006_central( searchterm1 = lv_partner
                                                    title_key = '0003' ).
      centraldataorganization-name1 = posldesc.
*  CONCATENATE gs_display-pos_sdes lv_partner
*      INTO centraldataorganization-name1 SEPARATED BY space.

**** adddress DATA filling in business partner creation **
      DATA(ls_address) = VALUE bapibus1006_address( country = 'IN'
                                                     langu = 'E' ).
      REFRESH: it_telephondata.
      it_telephondata[] = VALUE #(  ( country = 'IN'
                                      telephone = '1234567890'
                                      std_no = 'X'
                                      r_3_user = '3'
                                      home_flag = 'X' )  ).
      REFRESH: it_maildata.
      it_maildata[] = VALUE #( ( e_mail = 'noreply@sheenlac.in'
                                 std_no = 'X'
                                 std_recip = 'X'
                                 home_flag = 'X'
                                 consnumber = '001' ) ).
*** Bapi to create Business Partner ***
      REFRESH: return.
      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          businesspartnerextern   = lv_partner
          partnercategory         = partnercategory
          partnergroup            = partnergroup
          centraldata             = ls_central
*         centraldataperson       = centraldataperson
          centraldataorganization = centraldataorganization
          addressdata             = ls_address
          duplicate_message_type  = 'W'
        IMPORTING
          businesspartner         = businesspartner
        TABLES
          telefondata             = it_telephondata
          e_maildata              = it_maildata
          return                  = return.
      READ TABLE return INTO DATA(ls_ret) WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        LOOP AT return INTO ls_ret WHERE type = 'E'.
          CLEAR l_msg.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = ls_ret-id
              msgnr               = ls_ret-number
              msgv1               = ls_ret-message_v1
              msgv2               = ls_ret-message_v2
              msgv3               = ls_ret-message_v3
              msgv4               = ls_ret-message_v4
            IMPORTING
              message_text_output = l_msg.
          msg = |{ msg },{ l_msg }|.
        ENDLOOP.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CALL METHOD me->business_partner_role_extend
          EXPORTING
            businesspartner = businesspartner.
        " Business Partner Number
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD business_partner_relationship.

    DATA: lt_enque_tab TYPE TABLE OF ppenq,
          ls_enque_tab TYPE ppenq.

    DATA: lt_p1001 TYPE TABLE OF p1001.

    IF is_p1001 IS NOT INITIAL.

      APPEND is_p1001 TO lt_p1001.

      CLEAR ls_enque_tab.
      REFRESH lt_enque_tab.
      ls_enque_tab-plvar = '01'.
      ls_enque_tab-otype = 'BP'.
      ls_enque_tab-objid = is_p1001-objid+0(8).
      APPEND ls_enque_tab TO lt_enque_tab.

      CALL FUNCTION 'RH_ENQUEUE_LIST'
        TABLES
          enq_tab = lt_enque_tab
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc NE 0.
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
      ENDIF.

      CALL FUNCTION 'RH_DEQUEUE_LIST'
        TABLES
          deq_tab = lt_enque_tab.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.
  ENDMETHOD.


  METHOD business_partner_role_extend.

    IF businesspartner IS NOT INITIAL.
      DATA businesspartnerrolecategory TYPE bapibus1006_bproles-partnerrolecategory. " BP Role Category
      DATA all_businesspartnerroles    TYPE bapibus1006_x-mark. " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
      DATA businesspartnerrole         TYPE bapibus1006_bproles-partnerrole. " BP Role
      DATA differentiationtypevalue    TYPE bapibus1006_bproles-difftypevalue. " BP: Differentiation type value
      DATA validfromdate               TYPE bapibus1006_bprole_validity-bprolevalidfrom.
      DATA validuntildate              TYPE bapibus1006_bprole_validity-bprolevalidto.
      DATA return                      TYPE STANDARD TABLE OF bapiret2. " Return Parameter
*** Tvarvc variable for Adding roles to BP ****
      SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarv)
                           WHERE name = 'BP_POS_ROLE_EXTEND'
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
*             VALIDFROMDATE               = P_DATE
              validuntildate              = '99991231'
            TABLES
              return                      = return.
          READ TABLE return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type = 'E'.
          IF sy-subrc NE 0 .
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD costcenter_relationship.

    DATA: lv_coarea TYPE kokrs VALUE '1000'.
    DATA: lv_sobid TYPE sobid.

    DATA: lt_p1001 TYPE STANDARD TABLE OF hri1001.

    IF position_id IS NOT INITIAL AND costcenter IS NOT INITIAL.
      CLEAR lv_sobid.
      lv_sobid = costcenter.
      lv_sobid+10 = lv_coarea.
      SHIFT lv_sobid LEFT DELETING LEADING ''.

      lt_p1001 = VALUE #( (  plvar = '01'
                             otype = 'S'
                             objid = position_id
                             infty = '1001'
                             istat = '1'
                             begda = start_date
                             endda = '99991231'
                             rsign = 'A'
                             relat = '011'
                             sclas = 'K'
                             sobid = lv_sobid
                                   ) ) .
      CALL FUNCTION 'RH_INSERT_INFTY_1001_EXT'
        EXPORTING
          fcode                   = 'INSE'
          vtask                   = 'D'
        TABLES
          innnn                   = lt_p1001
        EXCEPTIONS
          no_authorization        = 1
          error_during_insert     = 2
          relation_not_reversible = 3
          corr_exit               = 4
          begda_greater_endda     = 5
          OTHERS                  = 6.
      CASE sy-subrc.
        WHEN '1'.
          msg = |no_authorization: for Costcenter Mapping|.
        WHEN '2'.
          msg = |error_during_insert: for Costcenter Mapping|.
        WHEN '3'.
          msg = |relation_not_reversible: for Costcenter Mapping|.
        WHEN '4'.
          msg = |corr_exit: for Costcenter Mapping|.
        WHEN '5'.
          msg = |begda_greater_endda: for Costcenter Mapping|.
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD dequeue_list.

    IF it_enqueue_list[] IS NOT INITIAL.
      CALL FUNCTION 'RH_DEQUEUE_LIST'
        TABLES
          deq_tab = it_enqueue_list.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.

  ENDMETHOD.


  METHOD enqueue_list.

    IF zhr_enqueue_list[] IS NOT INITIAL.

      CALL FUNCTION 'RH_ENQUEUE_LIST'
        TABLES
          enq_tab = zhr_enqueue_list
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc NE 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
          IMPORTING
            message_text_output = msg.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD get_position_id.
*
*    DATA: BEGIN OF dum.
*            INCLUDE STRUCTURE objec.
*    DATA: END   OF dum.
    DATA: ls_dum TYPE plog-objid.

    DATA: gc_plan_ver TYPE plvar VALUE '01',
          gc_otype_s  TYPE hrp1001-otype VALUE 'S'.

    CLEAR ls_dum.
    CALL FUNCTION 'RH_GET_NEXT_NUMBER'
      EXPORTING
        action     = 'DIRECT'
        ext_number = ls_dum
        otype      = gc_otype_s
        plvar      = gc_plan_ver
      IMPORTING
        number     = position_id.

  ENDMETHOD.


  METHOD infty_1000.

    IF it_p1000[] IS NOT INITIAL.
*  prepare db-update
      CALL FUNCTION 'RH_INSERT_INFTY'
        EXPORTING
          fcode               = 'INSE'
          vtask               = act_vtask
        TABLES
          innnn               = it_p1000
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
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'Error during createing the object record'.
      ELSEIF sy-subrc EQ 8.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'RH_DEQUEUE_LIST'
          TABLES
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'No Authorization to Create object record'.
      ELSEIF sy-subrc EQ 1.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'RH_DEQUEUE_LIST'
          TABLES
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'Error during Saving the object Record'. "MESSAGE e377.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD infty_1001.

    IF it_p1001[] IS NOT INITIAL.

      CALL FUNCTION 'RH_INSERT_INFTY'
        EXPORTING
          fcode               = 'INSE'
          vtask               = act_vtask
        TABLES
          innnn               = it_p1001
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
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'Error during creating the relationship record'.
      ELSEIF sy-subrc EQ 8.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'RH_DEQUEUE_LIST'
          TABLES
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'No Authorization to Create Relationship record'.
      ELSEIF sy-subrc EQ 1.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'RH_DEQUEUE_LIST'
          TABLES
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'Error during Saving the Relationship Record'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD infty_9011.

    IF it_p9011[] IS NOT INITIAL.

      CALL FUNCTION 'RH_INSERT_INFTY'
        EXPORTING
          fcode               = 'INSE'
          vtask               = act_vtask
        TABLES
          innnn               = it_p9011
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
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'Error during creating the relationship record'.
      ELSEIF sy-subrc EQ 8.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'RH_DEQUEUE_LIST'
          TABLES
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'No Authorization to Create Relationship record'.
      ELSEIF sy-subrc EQ 1.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'RH_DEQUEUE_LIST'
          TABLES
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'Error during Saving the Relationship Record'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD infty_9013.
    IF it_p9013[] IS NOT INITIAL.

      CALL FUNCTION 'RH_INSERT_INFTY'
        EXPORTING
          fcode               = 'INSE'
          vtask               = act_vtask
        TABLES
          innnn               = it_p9013
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
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'Error during creating the relationship record'.
      ELSEIF sy-subrc EQ 8.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'RH_DEQUEUE_LIST'
          TABLES
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'No Authorization to Create Relationship record'.
      ELSEIF sy-subrc EQ 1.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'RH_DEQUEUE_LIST'
          TABLES
            deq_tab = it_enqueue_list.
        CALL FUNCTION 'RH_CLEAR_BUFFER'.
        CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
        CALL FUNCTION 'DEQUEUE_ALL'.
        msg = 'Error during Saving the Relationship Record'.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD update_database.
      CALL FUNCTION 'RH_UPDATE_DATABASE'
    EXPORTING
      vtask     = 'D'
    EXCEPTIONS
      corr_exit = 1
      OTHERS    = 2.
  ENDMETHOD.
ENDCLASS.
