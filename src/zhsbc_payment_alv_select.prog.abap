*----------------------------------------------------------------------*
***INCLUDE ZHSBC_PAYMENT_ALV_SELECT.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0120 OUTPUT.
  SET PF-STATUS 'HSBC_120'.
  SET TITLEBAR 'HSBC_120'.

ENDMODULE.                 " STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0120 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'DECLINE'.
      PERFORM CLEARING.
      V = 0.
      CALL SCREEN BACK_SCREEN.
    WHEN 'APPROVE'.
      DATA : PPC TYPE N,
             EXCEED,
             MESSAGE TYPE STRING.

      CLEAR : PPC , EXCEED.

      CALL METHOD ALV_SELECT_GRID->CHECK_CHANGED_DATA
* IMPORTING
*    E_VALID   =
*  CHANGING
*    C_REFRESH = 'X'
          .

      LOOP AT IT_SELECT INTO WA_SELECT WHERE CHK = 'X' AND TYPE = 'RTGS'.

        IF WA_SELECT-PPC IS INITIAL.

          PPC = 1.

          EXIT.

        ENDIF.

      ENDLOOP.

      IF AMOUNT_EXCEED = 'X'.

        LOOP AT IT_SELECT INTO WA_SELECT WHERE CHK = 'X'.

          IF WA_SELECT-TCODE = 'NEFT' AND WA_SELECT-PSWBT >= '5000000.00'.

            MESSAGE = 'NEFT amount does not exceed to 50 lahks'.
            EXCEED = 'X'.

*          ELSEIF WA_SELECT-TCODE = 'RTGS' AND WA_SELECT-PSWBT >= '5000000.00'.
*
*            MESSAGE = 'RTGS amount does not exceed to 50 lahks'.
*            EXCEED = 'X'.
*          ELSEIF WA_SELECT-TCODE = 'IMPS' AND WA_SELECT-PSWBT >= '1000000.00'.
*
*            MESSAGE = 'IMPS amount does not exceed to 10 Lakhs '.
*            EXCEED = 'X'.
          ENDIF.


        ENDLOOP.

        IF  EXCEED = 'X'.

          MESSAGE MESSAGE TYPE 'W'.
          CALL SCREEN '120'.

        ENDIF.

      ENDIF.


      IF PPC = 1.

        LOOP AT IT_SELECT INTO WA_SELECT WHERE CHK = 'X' .

          IF WA_SELECT-TYPE  = 'RTGS'.

            CLEAR : LS_STYLEROW, WA_SELECT-FIELD_STYLE.
            LS_STYLEROW-FIELDNAME = 'PPC' .
            LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
            "set field to disabled
            APPEND LS_STYLEROW  TO WA_SELECT-FIELD_STYLE.

            MODIFY IT_SELECT FROM WA_SELECT.


          ELSE.

*     clear : LS_STYLEROW, WA_SELECT-field_style.
*      ls_stylerow-fieldname = 'PPC' .
*      ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
*                                             "set field to disabled
*      APPEND ls_stylerow  TO WA_SELECT-field_style.
*
*     MODIFY IT_SELECT FROM WA_SELECT.


          ENDIF.


          CLEAR WA_SELECT.



        ENDLOOP.

        CALL METHOD ALV_SELECT_GRID->REFRESH_TABLE_DISPLAY( ).

        MESSAGE 'Choose the purpose Code for RTGS Transactions.....' TYPE 'W'.

        CALL SCREEN '120'.


      ELSE.
        CALL SCREEN '121'.

      ENDIF.

    WHEN 'SELAL'.

      LOOP AT IT_SELECT INTO WA_SELECT.

        WA_SELECT-CHK = 'X'.
        MODIFY IT_SELECT FROM WA_SELECT.
        CLEAR WA_SELECT.

      ENDLOOP.
      CALL METHOD ALV_SELECT_GRID->REFRESH_TABLE_DISPLAY( ).
      CALL SCREEN '120'.
    WHEN 'DESEL'.

      LOOP AT IT_SELECT INTO WA_SELECT.

        WA_SELECT-CHK = ' '.
        MODIFY IT_SELECT FROM WA_SELECT.
        CLEAR WA_SELECT.

      ENDLOOP.
      CALL METHOD ALV_SELECT_GRID->REFRESH_TABLE_DISPLAY( ).
      CALL SCREEN '120'.


  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_DISPLAY_SELECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALV_DISPLAY_SELECT OUTPUT.

  IF V = 0.

    CREATE OBJECT ALV_SELECT_CONTAINER
      EXPORTING
*    PARENT                      =
        CONTAINER_NAME               = 'CC_SELECT'
*    STYLE                       =
*    LIFETIME                    = lifetime_default
*    REPID                       =
*    DYNNR                       =
*    NO_AUTODEF_PROGID_DYNNR     =
*  EXCEPTIONS
*    CNTL_ERROR                  = 1
*    CNTL_SYSTEM_ERROR           = 2
*    CREATE_ERROR                = 3
*    LIFETIME_ERROR              = 4
*    LIFETIME_DYNPRO_DYNPRO_LINK = 5
*    OTHERS                      = 6
        .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT ALV_SELECT_GRID
      EXPORTING
*    I_SHELLSTYLE      = 0
*    I_LIFETIME        =
         I_PARENT          = ALV_SELECT_CONTAINER
*    I_APPL_EVENTS     = space
*    I_PARENTDBG       =
*    I_APPLOGPARENT    =
*    I_GRAPHICSPARENT  =
*    I_NAME            =
*    I_FCAT_COMPLETE   = SPACE
*  EXCEPTIONS
*    ERROR_CNTL_CREATE = 1
*    ERROR_CNTL_INIT   = 2
*    ERROR_CNTL_LINK   = 3
*    ERROR_DP_CREATE   = 4
*    OTHERS            = 5
        .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM FCAT_SELECT.

    PERFORM DROPDOWN.

    PERFORM DISPLAY_SELECT.

    V = 1.

  ELSE.

    DATA : X VALUE 'X'.

*     CALL METHOD ALV_SELECT_GRID->REFRESH_TABLE_DISPLAY
*              EXPORTING
**            IS_STABLE      =
*                I_SOFT_REFRESH  =  X
**          EXCEPTIONS
**            FINISHED       = 1
**            OTHERS         = 2
*                    .
*            IF SY-SUBRC <> 0.
**         Implement suitable error handling here
*            ENDIF.

    CALL METHOD ALV_SELECT_GRID->CHECK_CHANGED_DATA
* IMPORTING
*    E_VALID   =
*  CHANGING
*    C_REFRESH = 'X'
    .


  ENDIF.




ENDMODULE.                 " ALV_DISPLAY_SELECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  FCAT_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FCAT_SELECT .

data : h TYPE i.

clear h.

  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-CHECKBOX = 'X'.
  WA_FCAT_SELECT-EDIT = 'X'.
  WA_FCAT_SELECT-SCRTEXT_S = 'Chk'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Check Box'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Check Box'.
  WA_FCAT_SELECT-FIELDNAME = 'CHK'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*  WA_FCAT_SELECT-OUTPUTLEN = '3'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.


  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Clr Doc'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Clearing Doc'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Clearing Document'.
  WA_FCAT_SELECT-FIELDNAME = 'BELNR'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*    WA_FCAT_SELECT-OUTPUTLEN = '15'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.

*wA_FCAT_SELECT-COL_POS = '3'.
*  WA_FCAT_SELECT-SCRTEXT_S = 'Item'.
*  WA_FCAT_SELECT-SCRTEXT_M = 'Line Item'.
*  WA_FCAT_SELECT-SCRTEXT_L = 'Line Item'.
*  WA_FCAT_SELECT-FIELDNAME = 'BUZEI'.
**    WA_FCAT_SELECT-OUTPUTLEN = '15'.
*
*  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
*  CLEAR  WA_FCAT_SELECT.

  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Acc No'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Account No'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Account Number'.
  WA_FCAT_SELECT-FIELDNAME = 'VBANKN'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*   WA_FCAT_SELECT-OUTPUTLEN = '18'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.


  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'IFSC'.
  WA_FCAT_SELECT-SCRTEXT_M = 'IFSC Code'.
  WA_FCAT_SELECT-SCRTEXT_L = 'IFSC Code'.
  WA_FCAT_SELECT-FIELDNAME = 'VBANKL'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*   WA_FCAT_SELECT-OUTPUTLEN = '16'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.

 h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Pay Md'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Payment Method'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Payment Method'.
  WA_FCAT_SELECT-FIELDNAME = 'TYPE'.
  WA_FCAT_SELECT-DRDN_FIELD = 'DROPDOWN'.
  WA_FCAT_SELECT-EDIT = 'X'.
*  WA_FCAT_SELECT-DRDN_HNDL = '1'.
  WA_FCAT_SELECT-CHECKTABLE = '!'.


  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.

  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'PPC'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Purpose Code'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Purpose Code'.
  WA_FCAT_SELECT-FIELDNAME = 'PPC'.
  WA_FCAT_SELECT-DRDN_FIELD = 'DROPDOWN1'.
  WA_FCAT_SELECT-OUTPUTLEN = '30'.
  WA_FCAT_SELECT-EDIT = 'X'.
*  WA_FCAT_SELECT-DRDN_HNDL = '1'.
  WA_FCAT_SELECT-CHECKTABLE = '!'.
*   WA_FCAT_SELECT-OUTPUTLEN = '6'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.



if BACK_SCREEN = '101' or BACK_SCREEN = '102'.
  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Vendor Code'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Vendor Code'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Vendor Code'.
  WA_FCAT_SELECT-FIELDNAME = 'LIFNR'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*   WA_FCAT_SELECT-OUTPUTLEN = '15'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.


  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Vendor Name'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Vendor Name'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Vendor Name'.
  WA_FCAT_SELECT-FIELDNAME = 'VKOINH'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*   WA_FCAT_SELECT-OUTPUTLEN = '25'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.

ELSEIF BACK_SCREEN = '103' or BACK_SCREEN = '104'.

   h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'GL Account'.
  WA_FCAT_SELECT-SCRTEXT_M = 'GL Account'.
  WA_FCAT_SELECT-SCRTEXT_L = 'GL Account'.
  WA_FCAT_SELECT-FIELDNAME = 'HKONT1'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*   WA_FCAT_SELECT-OUTPUTLEN = '15'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.


  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Name'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Name'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Name'.
  WA_FCAT_SELECT-FIELDNAME = 'VKOINH'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*   WA_FCAT_SELECT-OUTPUTLEN = '25'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.




endif.

  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Amount'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Amount'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Amount'.
  WA_FCAT_SELECT-FIELDNAME = 'PSWBT'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*   WA_FCAT_SELECT-OUTPUTLEN = '20'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.

  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Currency'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Currency Key'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Currency Key'.
  WA_FCAT_SELECT-FIELDNAME = 'PSWSL'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*   WA_FCAT_SELECT-OUTPUTLEN = '20'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.

  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Typ'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Document Type'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Document Type'.
  WA_FCAT_SELECT-FIELDNAME = 'BLART'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*WA_FCAT_SELECT-OUTPUTLEN = '6'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.

h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'User Name'.
  WA_FCAT_SELECT-SCRTEXT_M = 'User Name'.
  WA_FCAT_SELECT-SCRTEXT_L = 'User Name'.
  WA_FCAT_SELECT-FIELDNAME = 'USNAM'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*   WA_FCAT_SELECT-OUTPUTLEN = '15'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.

h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Tcode'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Tcode'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Tcode'.
  WA_FCAT_SELECT-FIELDNAME = 'TCODE'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*  WA_FCAT_SELECT-OUTPUTLEN = '10'.
  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.



h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Company'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Company Code'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Company Code'.
  WA_FCAT_SELECT-FIELDNAME = 'BUKRS'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*    WA_FCAT_SELECT-OUTPUTLEN = '5'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.

h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Year'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Fiscal Year'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Fiscal Year'.
  WA_FCAT_SELECT-FIELDNAME = 'GJAHR'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*WA_FCAT_SELECT-OUTPUTLEN = '5'.
  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.


h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Posting Date'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Posting Date'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Posting Date'.
  WA_FCAT_SELECT-FIELDNAME = 'BLDAT'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*WA_FCAT_SELECT-OUTPUTLEN = '10'.
  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.



h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Email'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Email ID'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Email ID'.
  WA_FCAT_SELECT-FIELDNAME = 'EMAIL'.
  WA_FCAT_SELECT-COL_OPT = 'X'.
*  WA_FCAT_SELECT-OUTPUTLEN = '20'.

  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.


  h = h + 1.
  WA_FCAT_SELECT-COL_POS = h.
  WA_FCAT_SELECT-SCRTEXT_S = 'Reference'.
  WA_FCAT_SELECT-SCRTEXT_M = 'Reference'.
  WA_FCAT_SELECT-SCRTEXT_L = 'Reference'.
  WA_FCAT_SELECT-FIELDNAME = 'REF'.
  WA_FCAT_SELECT-EDIT = 'X'.
  WA_FCAT_SELECT-OUTPUTLEN = '100'.


  APPEND WA_FCAT_SELECT TO IT_FCAT_SELECT.
  CLEAR  WA_FCAT_SELECT.









  WA_LAYOUT_SELECT-ZEBRA = 'X'.
*  WA_LAYOUT_SELECT-CWIDTH_OPT = 'X'.
  WA_LAYOUT_SELECT-STYLEFNAME = 'FIELD_STYLE'.




ENDFORM.                    " FCAT_SELECT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_SELECT .

  data :  t_fun TYPE ui_functions,
          fs_fun TYPE ui_func.

  fs_fun = cl_gui_alv_grid=>MC_FC_LOC_CUT.
APPEND fs_fun TO t_fun.
clear fs_fun.

 fs_fun = cl_gui_alv_grid=>MC_FC_LOC_APPEND_ROW.
APPEND fs_fun TO t_fun.
clear fs_fun.

 fs_fun = cl_gui_alv_grid=>MC_FC_LOC_COPY_ROW.
APPEND fs_fun TO t_fun.
clear fs_fun.

 fs_fun = cl_gui_alv_grid=>MC_FC_LOC_COPY.
APPEND fs_fun TO t_fun.
clear fs_fun.

 fs_fun = cl_gui_alv_grid=>MC_FC_LOC_INSERT_ROW.
APPEND fs_fun TO t_fun.
clear fs_fun.

 fs_fun = cl_gui_alv_grid=>MC_FC_LOC_MOVE_ROW.
APPEND fs_fun TO t_fun.
clear fs_fun.

 fs_fun = cl_gui_alv_grid=>MC_FC_LOC_PASTE_NEW_ROW.
APPEND fs_fun TO t_fun.
clear fs_fun.

 fs_fun = cl_gui_alv_grid=>MC_FC_LOC_PASTE.
APPEND fs_fun TO t_fun.
clear fs_fun.

 fs_fun = cl_gui_alv_grid=>MC_FC_LOC_DELETE_ROW.
APPEND fs_fun TO t_fun.
clear fs_fun.

 fs_fun = cl_gui_alv_grid=>MC_FC_LOC_UNDO.
APPEND fs_fun TO t_fun.
clear fs_fun.


  CALL METHOD ALV_SELECT_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*    I_BUFFER_ACTIVE               =
*    I_BYPASSING_BUFFER            =
*    I_CONSISTENCY_CHECK           =
*    I_STRUCTURE_NAME              =
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
       IS_LAYOUT                     = WA_LAYOUT_SELECT
*    IS_PRINT                      =
*    IT_SPECIAL_GROUPS             =
    IT_TOOLBAR_EXCLUDING          = t_fun
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
*    IT_EXCEPT_QINFO               =
*    IR_SALV_ADAPTER               =
    CHANGING
      IT_OUTTAB                      = IT_SELECT
       IT_FIELDCATALOG               = IT_FCAT_SELECT
*    IT_SORT                       =
*    IT_FILTER                     =
*  EXCEPTIONS
*    INVALID_PARAMETER_COMBINATION = 1
*    PROGRAM_ERROR                 = 2
*    TOO_MANY_LINES                = 3
*    OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

*data event TYPE REF TO lc_event_handler.
*
*CREATE OBJECT event.
*
*set HANDLER event->drop for ALV_SELECT_GRID.


* Set editable cells to ready for input initially
  CALL METHOD ALV_SELECT_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

*  ALV_SELECT_GRID->register_edit_event(
*     EXPORTING
*       i_event_id = cl_gui_alv_grid=>mc_evt_modified ). "Registering edit events
*
*CREATE OBJECT event.
*
*set HANDLER event->drop for ALV_SELECT_GRID.





ENDFORM.                    " DISPLAY_SELECT
*&---------------------------------------------------------------------*
*&      Form  DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DROPDOWN .


  CALL METHOD ALV_SELECT_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

  DATA: LT_DROPDOWN TYPE LVC_T_DROP,
        LS_DROPDOWN TYPE LVC_S_DROP.

  DATA : IT_PPC TYPE TABLE OF ZHSBC_PPC,
         WA_PPC TYPE ZHSBC_PPC,
         T_NEW TYPE STRING.

  refresh: it_ppc, LT_DROPDOWN.
  clear: wa_ppc, LS_DROPDOWN, T_NEW .

  SELECT * from ZHSBC_PPC INTO TABLE IT_PPC.


  LS_DROPDOWN-HANDLE = '1'.
  LS_DROPDOWN-VALUE = 'NEFT'.
  APPEND LS_DROPDOWN TO LT_DROPDOWN.

  LS_DROPDOWN-HANDLE = '1'.
  LS_DROPDOWN-VALUE = 'RTGS'.
  APPEND LS_DROPDOWN TO LT_DROPDOWN.

*  ls_dropdown-handle = '1'.
*  ls_dropdown-value = 'IMPS'.
*  APPEND ls_dropdown TO lt_dropdown.

  LS_DROPDOWN-HANDLE = '2'.
  LS_DROPDOWN-VALUE = 'NEFT'.
  APPEND LS_DROPDOWN TO LT_DROPDOWN.

*  LS_DROPDOWN-HANDLE = '2'.
*  LS_DROPDOWN-VALUE = 'IMPS'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.



  LOOP AT IT_PPC INTO WA_PPC.

    CLEAR T_NEW.
    CONCATENATE WA_PPC-PURCD '-' WA_PPC-PURDES INTO T_NEW.
    CONDENSE T_NEW.
    LS_DROPDOWN-HANDLE = '3'.
    LS_DROPDOWN-VALUE = T_NEW.
    APPEND LS_DROPDOWN TO LT_DROPDOWN.
    CLEAR : LS_DROPDOWN , WA_PPC.

  ENDLOOP.

*  LS_DROPDOWN-HANDLE = '4'.
*  LS_DROPDOWN-VALUE = 'NEFT'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.

  LS_DROPDOWN-HANDLE = '4'.
  LS_DROPDOWN-VALUE = 'RTGS'.
  APPEND LS_DROPDOWN TO LT_DROPDOWN.



*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'CASH-Cash Management Transfer'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'CORT-Trade Settlement Payment'.
*
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'DIVI-Dividend'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'GOVT-Government Payment'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'HEDG-Hedging'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'INTC-Intra Company Payment'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'INTE-Interest'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'LOAN-Loan'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'PENS-Pension Payment'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'SALA-Salary Payment'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'SECU-Securities'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'SSBE-Social Security Benefit'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'SUPP-Supplier Payment'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'TAXS-Tax Payment'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'TRAD-Trade'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'TREA-Treasury Payment'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'VATX-Value Added Tax Payment'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.
*
*  LS_DROPDOWN-HANDLE = '3'.
*  LS_DROPDOWN-VALUE = 'WHLD-With Holding'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.


  CALL METHOD ALV_SELECT_GRID->SET_DROP_DOWN_TABLE
    EXPORTING
      IT_DROP_DOWN = LT_DROPDOWN.


ENDFORM.                    " DROPDOWN
