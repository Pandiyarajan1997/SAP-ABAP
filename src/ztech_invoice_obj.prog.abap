*&---------------------------------------------------------------------*
*&  Include           ZD_EINVOICE_CLS
*&---------------------------------------------------------------------*
*CLASS : lcl_event_receiver DEFINITION DEFERRED.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*

CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    DATA GV_OBJECT_TEXT    TYPE CHAR30.

    METHODS:
      CONSTRUCTOR
        IMPORTING E_OBJECT_TEXT TYPE CHAR30.

    METHODS : HANDLE_TOOLBAR
                  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
      IMPORTING SENDER E_OBJECT E_INTERACTIVE.

    METHODS : HANDLE_USER_COMMAND
                  FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING E_UCOMM.

    METHODS : HANDLE_DATA_CHANGED
                  FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
      IMPORTING ER_DATA_CHANGED.

    METHODS : HANDLE_DOUBLE_CLICK
                  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW
                  E_COLUMN.

    METHODS : HANDLE_HOTSPOT_CLICK
                  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID
                  E_COLUMN_ID
                  ES_ROW_NO.
    METHODS : HANDLE_ONF4
                  FOR EVENT ONF4  OF CL_GUI_ALV_GRID
      IMPORTING SENDER
                  E_FIELDNAME
                  E_FIELDVALUE
                  ES_ROW_NO
                  ER_EVENT_DATA
                  ET_BAD_CELLS
                  E_DISPLAY.

    METHODS :  HANDLE_BUTTON_CLICK
          FOR EVENT BUTTON_CLICK  OF CL_GUI_ALV_GRID
      IMPORTING
          ES_COL_ID
          ES_ROW_NO.

ENDCLASS. "(LCL_EVENT_RECEIVER DEFINITION)
DATA GV_EVENT_RECEIVER       TYPE REF TO LCL_EVENT_RECEIVER.
*----------------------------------------------------------------------*
* LOCAL CLASSES: Implementation
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD CONSTRUCTOR.
    CALL METHOD SUPER->CONSTRUCTOR.
    GV_OBJECT_TEXT = E_OBJECT_TEXT.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD HANDLE_TOOLBAR.
    PERFORM SET_TOOLBAR USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

  METHOD HANDLE_USER_COMMAND.
    PERFORM SET_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command

  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                    "handle_data_changed

  METHOD HANDLE_BUTTON_CLICK.
*    PERFORM data_button_click  USING    es_col_id es_row_no.
  ENDMETHOD.                    "handle_data_changed

*-- Double Click
  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM DOUBLE_CLICK  USING GV_OBJECT_TEXT E_ROW E_COLUMN.
  ENDMETHOD.                    "handle_double_click

  METHOD HANDLE_HOTSPOT_CLICK.
*    PERFORM hotspot_click USING gv_object_text
*                                 e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD HANDLE_ONF4.
    PERFORM ONF4 USING E_FIELDNAME
                       E_FIELDVALUE
                       ES_ROW_NO
                       ER_EVENT_DATA
                       ET_BAD_CELLS
                       E_DISPLAY.
  ENDMETHOD.                    "Handle_OnF4
ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  SET_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM SET_TOOLBAR  USING P_E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                       P_E_INTERACTIVE.


  DATA: LS_TOOLBAR  TYPE STB_BUTTON.
* Append seperator to the normal toolbar
  MOVE 3            TO LS_TOOLBAR-BUTN_TYPE.
  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.
  CLEAR  LS_TOOLBAR.

  IF P_RAD1 IS NOT INITIAL.
    PERFORM ADD_FUNCTION USING P_E_OBJECT 'ZNEW'.
  ELSEIF P_RAD2 IS NOT INITIAL.

    AUTHORITY-CHECK OBJECT 'ZIRNCAN' ID 'ACTVT' FIELD '01'.

    IF SY-SUBRC EQ 0.
      PERFORM ADD_FUNCTION USING P_E_OBJECT 'CANEWY'.
      PERFORM ADD_FUNCTION USING P_E_OBJECT 'CANIRN'.
*    IF sy-uname = '264380' OR  sy-uname = '304377'.     "Added Two Users for cancelling the IRN

*    ENDIF.
    ELSEIF SY-SUBRC NE 0.
      MESSAGE 'You are not Authorized.' TYPE 'E'.
    ENDIF.
  ELSEIF P_RAD4 IS NOT INITIAL.
    PERFORM ADD_FUNCTION USING P_E_OBJECT 'CREWAY'.
  ENDIF.

ENDFORM.                    "set_toolbar
*&---------------------------------------------------------------------*
*&      Form  SET_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM SET_COMMAND   USING    P_UCOMM.
  DATA:
* Internal table
    GI_INDEX_ROWS TYPE LVC_T_ROW,
     LW_INDEX            TYPE LVC_S_ROW.
  DATA          L_ANSWER.

  DATA:   LT_TAB TYPE ESP1_MESSAGE_TAB_TYPE.
  DATA:   LS_TAB TYPE ESP1_MESSAGE_WA_TYPE.
  DATA :  IM_EINV_OUT  TYPE ZST_IRN_OUT,
          IM_EWAY_OUT  TYPE J_1IG_EWAYBILL,
          RESPONSE     TYPE STRING,
          ET_ERROR_MSG TYPE BAPIRET2_T,
          L_WRK_MSG    TYPE BAPIRET2.
*  FIELD-SYMBOLS: <lfs_einv_final> TYPE any.
  FIELD-SYMBOLS: <LFS_EINV_FINAL> TYPE TY_EINVOICE.

  CASE P_UCOMM.
    WHEN 'ZNEW'.                       "Create IRN
* Read index of selected rows
      CALL METHOD GV_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = GI_INDEX_ROWS.
*      READ TABLE gi_index_rows INTO DATA(lw_index) INDEX 1.
      READ TABLE GI_INDEX_ROWS INTO LW_INDEX INDEX 1.
      IF SY-SUBRC IS INITIAL.
*        READ TABLE lt_einv_final ASSIGNING FIELD-SYMBOL(<lfs_einv_final>) INDEX lw_index-index.
        READ TABLE LT_EINV_FINAL ASSIGNING  <LFS_EINV_FINAL> INDEX LW_INDEX-INDEX.
        IF SY-SUBRC = 0.

          CALL FUNCTION 'YSD_FM_IRN_CREATE'
            EXPORTING
              IM_VBELN  = <LFS_EINV_FINAL>-VBELN
            IMPORTING
              EINV_OUT  = IM_EINV_OUT
              EWAY_OUT  = IM_EWAY_OUT
            TABLES
              ERROR_MSG = ET_ERROR_MSG.

          IF ET_ERROR_MSG[] IS INITIAL AND IM_EINV_OUT-IRN IS NOT INITIAL.
            <LFS_EINV_FINAL>-IRN = IM_EINV_OUT-IRN.
            <LFS_EINV_FINAL>-ACK_NO = IM_EINV_OUT-ACK_NO.
            <LFS_EINV_FINAL>-ACK_DATE = IM_EINV_OUT-ACK_DATE.
            <LFS_EINV_FINAL>-IRN_STATUS = IM_EINV_OUT-IRN_STATUS.
            IF <LFS_EINV_FINAL>-IRN_STATUS = 'ACT'.
              <LFS_EINV_FINAL>-STATUS = ICON_LED_GREEN.
            ENDIF.
*            lw_einv_final-ebillno = im_eway_out-ebillno.
          ELSE.
            DATA LV_LINE TYPE CHAR03 .

*            LOOP AT et_error_msg INTO DATA(l_wrk_msg).
            LOOP AT ET_ERROR_MSG INTO L_WRK_MSG.
              CONCATENATE L_WRK_MSG-MESSAGE L_WRK_MSG-MESSAGE_V1 INTO  L_WRK_MSG-MESSAGE SEPARATED BY '-'.
              LS_TAB-MSGTY  = 'E'.
              LS_TAB-MSGID = 'ZEINVOICE'.
              LS_TAB-MSGNO = '002'.
              LS_TAB-MSGV1  = L_WRK_MSG-MESSAGE.
              LS_TAB-MSGV2  = L_WRK_MSG-MESSAGE.
              LS_TAB-LINENO = LV_LINE + 1.
              APPEND LS_TAB TO LT_TAB.
            ENDLOOP.

            CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
              TABLES
                I_MESSAGE_TAB = LT_TAB.

          ENDIF.
        ENDIF.
      ENDIF.
    WHEN 'CANEWY'.

* Read index of selected rows
      CALL METHOD GV_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = GI_INDEX_ROWS.
      READ TABLE GI_INDEX_ROWS INTO LW_INDEX INDEX 1.
*      READ TABLE lt_einv_final INTO lw_einv_final INDEX lw_index-index.
      READ TABLE LT_EINV_FINAL ASSIGNING <LFS_EINV_FINAL> INDEX LW_INDEX-INDEX.
      IF SY-SUBRC = 0.

        CALL FUNCTION 'YSD_FM_EWAYBILL_CANCEL'
          EXPORTING
            IM_VBELN    = <LFS_EINV_FINAL>-VBELN
            IM_EWBILLNO = <LFS_EINV_FINAL>-EBILLNO
          TABLES
            ERROR_MSG   = ET_ERROR_MSG.

        IF ET_ERROR_MSG[] IS INITIAL.
          <LFS_EINV_FINAL>-STATUS      = ICON_SYSTEM_CANCEL.
          <LFS_EINV_FINAL>-CAN_STATUS  = 'Cancelled'.
*          MODIFY lt_einv_final FROM lw_einv_final TRANSPORTING status can_status INDEX lw_index-index.
        ELSE.
          CLEAR: LV_LINE.
          LOOP AT ET_ERROR_MSG INTO L_WRK_MSG.
            CONCATENATE L_WRK_MSG-MESSAGE L_WRK_MSG-MESSAGE_V1 INTO  L_WRK_MSG-MESSAGE SEPARATED BY '-'.
            LS_TAB-MSGTY  = 'E'.
            LS_TAB-MSGID = 'ZEINVOICE'.
            LS_TAB-MSGNO = '002'.
            LS_TAB-MSGV1  = L_WRK_MSG-MESSAGE.
*            ls_tab-msgv2  = l_wrk_msg-message.
            LS_TAB-LINENO = LV_LINE + 1.
            APPEND LS_TAB TO LT_TAB.
          ENDLOOP.

          CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
            TABLES
              I_MESSAGE_TAB = LT_TAB.
        ENDIF.

      ENDIF.
    WHEN 'CANIRN'.
* Read index of selected rows
      CALL METHOD GV_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = GI_INDEX_ROWS.

      READ TABLE GI_INDEX_ROWS INTO LW_INDEX INDEX 1.
      IF SY-SUBRC IS INITIAL.
*        READ TABLE lt_einv_final ASSIGNING <lfs_einv_final> INDEX lw_index-index.
        READ TABLE LT_EINV_FINAL ASSIGNING <LFS_EINV_FINAL> INDEX LW_INDEX-INDEX.

        IF <LFS_EINV_FINAL> IS ASSIGNED.
*        IF lw_einv_final  IS NOT INITIAL.

          DATA :IM_CANCEL_OUT TYPE ZSD_IRN_COUT .

          CALL FUNCTION 'YSD_FM_IRN_CANCEL'
            EXPORTING
              IM_IRN        = <LFS_EINV_FINAL>-IRN
              IM_VBELN      = <LFS_EINV_FINAL>-VBELN
            IMPORTING
*             ex_error      =
              EX_CANCEL_OUT = IM_CANCEL_OUT
            TABLES
              T_ERROR_MSG   = ET_ERROR_MSG
            EXCEPTIONS
              UPDATE_ERROR  = 1
              OTHERS        = 2.

          IF ET_ERROR_MSG[] IS INITIAL.
            <LFS_EINV_FINAL>-STATUS = ICON_DELETE.
            <LFS_EINV_FINAL>-IRN_STATUS = IM_CANCEL_OUT-IRN_STATUS.
          ELSE.
            LOOP AT ET_ERROR_MSG INTO L_WRK_MSG.
              CONCATENATE L_WRK_MSG-MESSAGE L_WRK_MSG-MESSAGE_V1 INTO  L_WRK_MSG-MESSAGE SEPARATED BY '-'.
              LS_TAB-MSGTY  = 'E'.
              LS_TAB-MSGID = 'ZEINVOICE'.
              LS_TAB-MSGNO = '002'.
              LS_TAB-MSGV1  = L_WRK_MSG-MESSAGE.
*            ls_tab-msgv2  = l_wrk_msg-message.
              LS_TAB-LINENO = LV_LINE + 1.
              APPEND LS_TAB TO LT_TAB.
            ENDLOOP.

            CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
              TABLES
                I_MESSAGE_TAB = LT_TAB.

          ENDIF.
        ENDIF.
      ENDIF.


    WHEN 'CREWAY'.                       "Create E-Way Bill BY IRN details
* Read index of selected rows
      DATA: WA_VEH TYPE ZST_EWAYBILL.

      CALL METHOD GV_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = GI_INDEX_ROWS.

      READ TABLE GI_INDEX_ROWS INTO LW_INDEX INDEX 1.
      IF SY-SUBRC IS INITIAL.
        READ TABLE LT_EINV_FINAL ASSIGNING <LFS_EINV_FINAL> INDEX LW_INDEX-INDEX.

        IF SY-SUBRC = 0.
          CONCATENATE '"' <LFS_EINV_FINAL>-IRN '"' INTO WA_VEH-IRN.

          IF P_VEHNO IS NOT INITIAL.
            CONCATENATE '"' P_VEHNO '"' INTO WA_VEH-VEHNO.
          ELSE.
            MOVE 'null' TO WA_VEH-VEHNO.
          ENDIF.

          IF P_TID IS NOT INITIAL.
            CONCATENATE '"' P_TID '"'   INTO WA_VEH-TRANSID.
          ELSE.
            MOVE 'null' TO WA_VEH-TRANSID.
          ENDIF.

          IF P_TNM IS NOT INITIAL.
            CONCATENATE '"' P_TNM '"'   INTO WA_VEH-TRANSNAME.
          ELSE.
            MOVE 'null' TO WA_VEH-TRANSNAME.
          ENDIF.

          IF P_DOCNO IS NOT INITIAL.
            CONCATENATE '"' P_DOCNO '"' INTO WA_VEH-TRANSDOC.
          ELSE.
            MOVE 'null' TO WA_VEH-TRANSDOC.
          ENDIF.

          IF P_DOCDT IS NOT INITIAL.
            CONCATENATE '"' P_DOCDT+6(2) '/' P_DOCDT+4(2) '/'  P_DOCDT+0(4) '"' INTO WA_VEH-TRANSDT.
          ELSE.
            MOVE 'null' TO WA_VEH-TRANSDT.
          ENDIF.

          IF P_DIS IS NOT INITIAL.
            MOVE P_DIS                    TO WA_VEH-DISTANCE.      "Distance
          ENDIF.

          MOVE <LFS_EINV_FINAL>-VBELN          TO WA_VEH-VBELN.
          MOVE <LFS_EINV_FINAL>-BUKRS          TO WA_VEH-BUKRS.
          MOVE <LFS_EINV_FINAL>-FKART          TO WA_VEH-DOCTYP.
          MOVE <LFS_EINV_FINAL>-FKDAT(4)       TO WA_VEH-GJAHR.
          CONCATENATE     '"' 'R' '"'       INTO WA_VEH-VEHTYP.      "Regular
          CONCATENATE     '"' '1' '"'       INTO WA_VEH-TRANSMODE.   "1 - Road , 2-Rail , Air-3 , Ship-4

          CALL FUNCTION 'YSD_FM_EWAYBILL_CREATE'
            EXPORTING
              IM_EWB    = WA_VEH
            IMPORTING
              RESPONSE  = RESPONSE
              EWAY_OUT  = IM_EWAY_OUT
            TABLES
              ERROR_MSG = ET_ERROR_MSG.

          IF ET_ERROR_MSG[] IS INITIAL AND IM_EWAY_OUT-EBILLNO IS NOT INITIAL.
            <LFS_EINV_FINAL>-STATUS     = ICON_LED_GREEN.
            <LFS_EINV_FINAL>-EBILLNO    = IM_EWAY_OUT-EBILLNO.
*            <lfs_einv_final>-VDFMDATE   = im_eway_out-VDFMDATE.
*            <lfs_einv_final>-VDFMTIME   = im_eway_out-VDFMTIME.
          ELSE.
            CLEAR: LV_LINE.
            LOOP AT ET_ERROR_MSG INTO L_WRK_MSG.
              CONCATENATE L_WRK_MSG-MESSAGE L_WRK_MSG-MESSAGE_V1 INTO  L_WRK_MSG-MESSAGE SEPARATED BY '-'.
              LS_TAB-MSGTY  = 'E'.
              LS_TAB-MSGID = 'ZEINVOICE'.
              LS_TAB-MSGNO = '004'.
              LS_TAB-MSGV1  = L_WRK_MSG-MESSAGE.
*            ls_tab-msgv2  = l_wrk_msg-message.
              LS_TAB-LINENO = LV_LINE + 1.
              APPEND LS_TAB TO LT_TAB.
            ENDLOOP.

            CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
              TABLES
                I_MESSAGE_TAB = LT_TAB.

          ENDIF.

        ENDIF.

      ENDIF.

  ENDCASE.


ENDFORM.                    "set_command
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM DATA_CHANGED  USING P_DATA_CHANGED TYPE REF TO
                                         CL_ALV_CHANGED_DATA_PROTOCOL.



ENDFORM.                    "data_changed
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_OBJECT_TEXT  text
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*----------------------------------------------------------------------*
FORM DOUBLE_CLICK  USING    P_GV_OBJECT_TEXT
                            P_E_ROW
                            P_E_COLUMN.



ENDFORM.                    "double_click
*&---------------------------------------------------------------------*
*&      Form  ONF4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_E_FIELDVALUE  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM ONF4  USING    P_FIELDNAME
                    P_FIELDVALUE
                    PS_ROW_NO     TYPE LVC_S_ROID
                    PR_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA
                    PT_BAD_CELLS
                    P_DISPLAY.

ENDFORM.                    "onf4
