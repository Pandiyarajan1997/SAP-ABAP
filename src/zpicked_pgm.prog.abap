*&---------------------------------------------------------------------*
*& Report  ZTEST_NEW_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
"testing
REPORT zpicked_pgm.

TYPES : BEGIN OF t_likp,
          vbeln TYPE likp-vbeln,
          erdat TYPE likp-erdat,
          vstel TYPE likp-vstel,
          kunnr TYPE likp-kunnr,
        END OF t_likp.

TYPES : BEGIN OF t1_likp,
          vbeln  TYPE likp-vbeln,
          erdat  TYPE likp-erdat,
          vstel  TYPE likp-vstel,
          kunnr  TYPE likp-kunnr,
          name1  TYPE name1,
          ort01  TYPE ort01,
          p_name TYPE name1,
        END OF t1_likp.

TYPES : BEGIN OF t_kna1 ,
          kunnr TYPE kna1-kunnr,
          name1 TYPE kna1-name1,
          ort01 TYPE kna1-ort01,
        END OF t_kna1.

TYPES : BEGIN OF t_t001w ,
          werks TYPE t001w-werks,
          name1 TYPE t001w-name1,
          "ORT01 TYPE T001W-ORT01 ,
        END OF t_t001w.

DATA : it_likp TYPE TABLE OF t_likp,
       wa_likp TYPE t_likp.

DATA : it_kna1 TYPE TABLE OF t_kna1,
       wa_kna1 TYPE t_kna1.

DATA : it_t001w TYPE TABLE OF t_t001w,
       wa_t001w TYPE t_t001w.


TYPES : BEGIN OF t_final,
          vbeln   TYPE likp-vbeln,
          erdat   TYPE likp-erdat,
          vstel   TYPE likp-vstel,
          kunnr   TYPE likp-kunnr,
          name1   TYPE kna1-name1,
          ort01   TYPE kna1-ort01,
          p_name  TYPE t001w-name1,
          flag(1), " for selection of records
        END OF t_final.

DATA : it_final TYPE TABLE OF t_final,
       wa_final TYPE t_final.

DATA : it1_likp TYPE TABLE OF t1_likp,
       wa1_likp TYPE t1_likp.

DATA : fm_name TYPE rs38l_fnam.

DATA : lv_del TYPE likp-vbeln .

DATA : layout TYPE slis_layout_alv.

* table for messages
TYPES : BEGIN OF t_message,
          aufnr    TYPE aufk-aufnr, "order number
          msg(200), "message text
        END OF t_message.

*&---------------------------------------------------------------------*
*          CONTANTS DECLARATION
*&---------------------------------------------------------------------*
CONSTANTS : c_check(1) VALUE 'X',    " value used to set X for a field
            c_langu(1) VALUE 'E',    " language used
            c_ustat(4) VALUE 'TECO'. " object status description

*&---------------------------------------------------------------------*
*          RANGES DECLARATION
*&---------------------------------------------------------------------*
*          RANGE VALUE TO BE USED FOR THE ORDER TYPE
*&---------------------------------------------------------------------*
RANGES : r_auart FOR aufk-auart. "for order type

DATA : lv_vbeln TYPE likp-vbeln,
       lv_erdat TYPE likp-erdat,
       lv_werks TYPE likp-werks.

*&---------------------------------------------------------------------*
*          PARAMETERS FOR SELECTION-SCREEN
*&---------------------------------------------------------------------*
*          SELECTION SCREEN BLOCK FOR THE VARIANT NAME
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS  : so_vbeln FOR lv_vbeln,
                    so_erdat FOR lv_erdat,
                    so_werks FOR lv_werks.
  "PARAMETERS : p_var TYPE disvariant-variant. " variant parameter
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*          VARIABLE DECLARATIONS
*&---------------------------------------------------------------------*
DATA : v_rep_id     TYPE sy-repid,   " report id
       v_istat      TYPE tj02t-istat, " order status
       v_cdate      TYPE sy-datum,    " current system date
       v_line_count TYPE i,      " number of lines in final internal table
       v_filename   TYPE string.   " path for download error log

*&---------------------------------------------------------------------*
*          INTERNAL TABLE & WORK AREA DECLARATIONS
*&---------------------------------------------------------------------*

* TYPE OF T_PROCESS
* final internal table to select final records to be displayed

* FOR VARIANT
DATA : wa_variant TYPE disvariant.
* FOR VARIANT IMPORT
DATA : wa_i_variant TYPE disvariant.

*&---------------------------------------------------------------------*
*          ALV INTERNAL TABLE & WORK AREA DECLARATIONS
*&---------------------------------------------------------------------*
* FIELD CATALOG
DATA : it_field TYPE slis_t_fieldcat_alv, "internal table for field catalog
       wa_field TYPE slis_fieldcat_alv. "work area for field catalog

* SORTING INFO
DATA : it_sort TYPE slis_t_sortinfo_alv, "internal table for sorting field
       wa_sort TYPE slis_sortinfo_alv. "work area for sorting field

* FOR LAYOUT OF ALV GRID
DATA : wa_layout TYPE slis_layout_alv. "work area for layout design

*&---------------------------------------------------------------------*
*          ALV INTERNAL TABLE & WORK AREA DECLARATIONS
*&---------------------------------------------------------------------*
* FIELD CATALOG
DATA : it_field1 TYPE slis_t_fieldcat_alv, "internal table for field catalog
       wa_field1 TYPE slis_fieldcat_alv. "work area for field catalog

* SORTING INFO
DATA : it_sort1 TYPE slis_t_sortinfo_alv, "internal table for sorting field
       wa_sort1 TYPE slis_sortinfo_alv. "work area for sorting field

* FOR LAYOUT OF ALV GRID
DATA : wa_layout1 TYPE slis_layout_alv. "work area for layout design

*&---------------------------------------------------------------------*
*          INITIALIZATION
*&---------------------------------------------------------------------*
*          TO INITIALIZE ALL THE VARIABLES TO BE USED
*          IN THE PROGRAM
*&---------------------------------------------------------------------*
INITIALIZATION.
  v_rep_id = sy-repid.            " report id
  r_auart-sign = 'I'.             " for inclusive
  r_auart-option = 'BT'.          " for between operator
  r_auart-low = 'PM01'.           " order type(low value)
  r_auart-high = 'PM02'.          " order type(high value)
  APPEND r_auart.                 " append values for range
  v_cdate = sy-datum - 7.         " take date 7 days before current date
  wa_variant-report = v_rep_id.   " report for using variants

*&---------------------------------------------------------------------*
*          AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
*          TO CLEAR THE VALUE OF THE VARIANT PARAMETER WHEN USER
*          RETURNS FROM THE OUTPUT SCREEN TO THE SELECTION SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  "CLEAR p_var. "clear variant value

*&---------------------------------------------------------------------*
*          AT-SELECTION SCREEN ON VALUE REQUEST
*&---------------------------------------------------------------------*
*          TO GET THE F4 HELP FOR EXISTING VARIANTS
*&---------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
*  PERFORM f4_variant_help USING p_var.

*&---------------------------------------------------------------------*
*          AT-SELECTION SCREEN
*&---------------------------------------------------------------------*
*          TO PASS THE DEFAULT VALUE OF THE VARIANT IF THE PARAMETER
*          FOR VARIANT IS LEFT AS BLANK
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*&---------------------------------------------------------------------*
*         IF THE USER DOESN'T ENTERS ANY VALUE FOR VARIANT
*         THEN THE DEFAULT VARIANT SHOULD BE USED TO DISPLAY
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*          START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*&---------------------------------------------------------------------*
*          CLEAR ALL THE CONTENTS OF ALL INTERBAL TABLES
*&---------------------------------------------------------------------*
  REFRESH it_final.
*&---------------------------------------------------------------------*
*          select user status from TJ02T for TXT04(status of an object)
*          into variable V_ISTAT
*&---------------------------------------------------------------------*

  SELECT
     vbeln
     erdat
     vstel
     kunnr
        FROM likp INTO TABLE it_likp WHERE vbeln IN so_vbeln AND erdat IN so_erdat AND vstel IN so_werks.

  SELECT
    kunnr
    name1
    ort01
      FROM kna1 INTO TABLE it_kna1 FOR ALL ENTRIES IN it_likp WHERE kunnr = it_likp-kunnr.

  SELECT
     werks
     name1
         FROM t001w INTO TABLE it_t001w FOR ALL ENTRIES IN it_likp WHERE werks = it_likp-vstel.









  LOOP AT it_likp INTO wa_likp .

    wa_final-vbeln = wa_likp-vbeln.
    wa_final-erdat = wa_likp-erdat.
    wa_final-vstel = wa_likp-vstel.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_likp-kunnr.
    IF  sy-subrc EQ '0'.
      wa_final-kunnr = wa_kna1-kunnr.
      wa_final-name1 = wa_kna1-name1.
      wa_final-ort01 = wa_kna1-ort01.
    ENDIF.

    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_likp-vstel.
    IF  sy-subrc EQ '0'.
      wa_final-p_name = wa_t001w-name1.
    ENDIF.

    APPEND wa_final TO it_final .
    CLEAR wa_final.

  ENDLOOP.


*&---------------------------------------------------------------------*
*          TO COUNT THE NUMBER OF RECORDS IN THE FINAL INTERNAL
*          TABLE AND RETURN TO SELECTION SCREEN
*          IF NO RECORDS EXISTS THEN AN INFORMATION MESSGE IS DISPLAYED
*&---------------------------------------------------------------------*
  DESCRIBE TABLE it_final
  LINES v_line_count.
  IF ( v_line_count EQ 0 ).
    "MESSAGE i002. "error message
    EXIT.
  ENDIF.

*&---------------------------------------------------------------------*
*          END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*          FIELD CATALOG FOR FIRST GRID DISPLAY
*&---------------------------------------------------------------------*
  PERFORM field_catalog.

*&---------------------------------------------------------------------*
*          SORT W.R.T. WORK ORDER NUMBER FOR FIRST GRID DISPLAY
*&---------------------------------------------------------------------*
  PERFORM sort_field.

*&---------------------------------------------------------------------*
*          FOR LAYOUT FOR FIRST GRID DISPLAY
*&---------------------------------------------------------------------*
  PERFORM set_layout.

*&---------------------------------------------------------------------*
*          DISPLAY RECORDS IN ALV GRID FOR FIRST GRID DISPLAY
*&---------------------------------------------------------------------*
  PERFORM alv_display.

*&---------------------------------------------------------------------*
*&      SUBROUTINE DEFINITIONS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
*       SUB-ROUTINE FIELD_CATALOG USED TO SET THE COLUMNS FOR
*       THE ALV GRID (OUTPUT FORMAT)
*       SETS THE COLUMN NAME AND THE OUTPUT LENGTH FOR THE FIELDS
*----------------------------------------------------------------------*
FORM field_catalog .

  wa_field-fieldname = 'FLAG'.   " name of field from internal table
  wa_field-tabname = 'IT_FINAL'. " internal table name
  "wa_field-outputlen = 2.        " output length on screen
  wa_field-checkbox = c_check.   " print as checkbox
  wa_field-edit = c_check.       " make field open for input
  wa_field-seltext_l = 'Flag'.      " header information
  APPEND wa_field TO it_field.   " append field catalog internal table
  CLEAR wa_field.                " clear field catalog work area

  wa_field-fieldname = 'VBELN'.  " name of field from internal table
  " WA_Field-SELTEXT_L =  .
  wa_field-tabname = 'IT_FINAL'. " internal table name
  "wa_field-outputlen = 20.       " output length on screen
  wa_field-seltext_l = 'Delivery'. " header information
  APPEND wa_field TO it_field.   " append field catalog internal table
  CLEAR wa_field.                " clear field catalog work area

  wa_field-fieldname = 'NAME1'.  " name of field from internal table
  wa_field-tabname = 'IT_FINAL'. " internal table name
  "wa_field-outputlen = 35.       " output length on screen
  wa_field-seltext_l = 'Customer Name'. " header information
  APPEND wa_field TO it_field.   " append field catalog internal table
  CLEAR wa_field.

  wa_field-fieldname = 'ORT01'.  " name of field from internal table
  " WA_Field-SELTEXT_L =  .
  wa_field-tabname = 'IT_FINAL'. " internal table name
  "wa_field-outputlen = 20.       " output length on screen
  wa_field-seltext_l = 'City'. " header information
  APPEND wa_field TO it_field.   " append field catalog internal table
  CLEAR wa_field.

  wa_field-fieldname = 'VSTEL'.  " name of field from internal table
  wa_field-tabname = 'IT_FINAL'. " internal table name
  "wa_field-outputlen = 22.       " output length on screen
  wa_field-seltext_l = 'Ship Plant'. " header information
  APPEND wa_field TO it_field.   " append field catalog internal table
  CLEAR wa_field.                " clear field catalog work area


  wa_field-fieldname = 'P_NAME'.  " name of field from internal table
  wa_field-tabname = 'IT_FINAL'. " internal table name
  "wa_field-outputlen = 35.       " output length on screen
  wa_field-seltext_l = 'Ship Plant Desc'. " header information
  APPEND wa_field TO it_field.   " append field catalog internal table
  CLEAR wa_field.                " clear field catalog work area


  wa_field-fieldname = 'ERDAT'.  " name of field from internal table
  wa_field-tabname = 'IT_FINAL'. " internal table name
  "wa_field-outputlen = 45.       " output length on screen
  wa_field-seltext_l = 'Created Date'. " header information
  APPEND wa_field TO it_field.   " append field catalog internal table
  CLEAR wa_field.                " clear field catalog work area



ENDFORM.                    " FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  SORT_FIELD
*&---------------------------------------------------------------------*
*       SUB-ROUTINE SORT_FIELD IS USED TO SORT THE RECORDS IN THE
*       INTERNAL TABLE BASED ON THE GIVEN FIELD AND NATURE OF
*       SORTING TO BE DONE (ASCENDING OR DESCENDING)
*----------------------------------------------------------------------*
FORM sort_field .

  wa_sort-spos = 1.             " sort priority
  wa_sort-fieldname = 'VBELN'.  " field on which records sorted
  wa_sort-tabname = 'IT_FINAL'. " internal table name
  wa_sort-up = c_check.         " sort ascending
  APPEND wa_sort TO it_sort.    " append sort info internal table
  CLEAR wa_sort.                " clear sort info work area

ENDFORM.                    " SORT_FIELD
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       SUB-ROUTINE SET_LAYOUT IS USED TO SET THE DISPLAY OF THE
*       ALV GRID LINES IN ALTERNATIVE COLOURS
*----------------------------------------------------------------------*
FORM set_layout .

  wa_layout-zebra = c_check.    " so set colors of line alternatively

ENDFORM.                    " SET_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       SUB-ROUTINE ALV_DISPLAY IS USED TO SET THE PARAMETERS
*       FOR THE FUNCTION MODULE REUSE_ALV_GRID_DISPLAY
*       AND PASS THE INTERNAL TABLE EXISTING THE RECORDS TO BE
*       DISPLAYED IN THE GRID FORMAT
*----------------------------------------------------------------------*
FORM alv_display .

  wa_layout-colwidth_optimize = 'X'.                                  "Added by S.Savariar as on 20/10/2014.
  wa_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = v_rep_id       " report id
      i_callback_pf_status_set = 'PF'           " for PF-STATUS
      i_callback_user_command  = 'USER_COMMAND' " for User-Command
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout                = wa_layout      " for layout
      it_fieldcat              = it_field       " field catalog
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
      it_sort                  = it_sort        " sort info
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     i_save                   = 'A'
*     is_variant               = wa_variant     " variant name
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = it_final      " internal table
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  pf
*&---------------------------------------------------------------------*
*       SUB-ROUTINE PF IS USED TO SET THE PF-STATUS OF THE SCREEN
*       ON WHICH THE ALV GRID IS DISPLAYED
*----------------------------------------------------------------------*
*       -->RT_EXTAB
*----------------------------------------------------------------------*
FORM pf USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZTDEL'.
ENDFORM.                    "pf

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       SUB-ROUTINE USER_COMMAND IS USED TO HANDLE THE USER ACTION
*       AND EXECUTE THE APPROPIATE CODE
*----------------------------------------------------------------------*
*      -->LV_OKCODE   used to capture the function code
*                     of the user-defined push-buttons
*      -->L_SELFIELD   text
*----------------------------------------------------------------------*
FORM user_command USING lv_okcode LIKE sy-ucomm l_selfield TYPE slis_selfield.

* assign the function code to variable v_okcode
  lv_okcode = sy-ucomm.

* handle the code execution based on the function code encountered
  CASE lv_okcode.

* when the function code is EXECUTE then process the selected records
    WHEN 'EXECUTE' OR 'PRINT'.

* refresh it_process when user processes selected records
      "REFRESH it_process.
      REFRESH it1_likp.
* to reflect the data changed into internal table
      DATA : ref_grid TYPE REF TO cl_gui_alv_grid. "new

      IF ref_grid IS INITIAL.
        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            e_grid = ref_grid.
      ENDIF.

      IF NOT ref_grid IS INITIAL.
        CALL METHOD ref_grid->check_changed_data.
      ENDIF.

*----------------------------------------------------------------------*
* sort the internal table by flag descending so that the selected
* records are appended at the beginning of internal table
*----------------------------------------------------------------------*
      SORT it_final BY flag DESCENDING.

*----------------------------------------------------------------------*
* move the selected records from final internal table into another
* internal table so that they can be processed
*----------------------------------------------------------------------*
      LOOP AT it_final INTO wa_final WHERE flag = 'X'.
        wa1_likp-vbeln = wa_final-vbeln.
        wa1_likp-erdat = wa_final-erdat.
        wa1_likp-vstel = wa_final-vstel.
        wa1_likp-kunnr = wa_final-kunnr.
        wa1_likp-name1 = wa_final-name1.
        wa1_likp-ort01 = wa_final-ort01.
        wa1_likp-p_name = wa_final-p_name.
        APPEND wa1_likp TO it1_likp.
      ENDLOOP.

* refresh the ALV Grid output from internal table
      l_selfield-refresh = c_check.

* now all the selected records by the user at run-time are appended into
* into a new internal table which can now be used to processed as per the
* user requirements
      DATA : line_count1 TYPE i.

      DESCRIBE TABLE it1_likp
      LINES line_count1.
      IF line_count1 GE 1.
        PERFORM user_action.
      ELSE.
        "MESSAGE e002.
      ENDIF.

    WHEN 'SEL_ALL'.
* to select all the records displayed in ALV Grid
      LOOP AT it_final INTO wa_final.
        wa_final-flag = 'X'.
        MODIFY it_final FROM wa_final.
      ENDLOOP.
* refresh the ALV Grid output from internal table
      l_selfield-refresh = c_check.

    WHEN 'DESEL_ALL'.
* to deselect all the records displayed in ALV Grid
      LOOP AT it_final INTO wa_final.
        wa_final-flag = ' '.
        MODIFY it_final FROM wa_final.
      ENDLOOP.
* refresh the ALV Grid output from internal table
      l_selfield-refresh = c_check.

  ENDCASE.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F4_VARIANT_HELP
*&---------------------------------------------------------------------*
*       SUB-ROUTINE f4_variant_help TO GET A F4 HELP FOR VARIANT
*       SELECTION AND DISPLAY DATA ACCORDINGLY
*----------------------------------------------------------------------*
*      <--P_P_VAR  text
*----------------------------------------------------------------------*
FORM f4_variant_help USING p_p_var.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant         = wa_variant  " export variant
*     I_TABNAME_HEADER   =
*     I_TABNAME_ITEM     =
*     IT_DEFAULT_FIELDCAT       =
      i_save             = 'A'          " layout for all users
      i_display_via_grid = 'X'          " grid view of variants
    IMPORTING
*     E_EXIT             =
      es_variant         = wa_i_variant " import variant
    EXCEPTIONS
      not_found          = 1
      program_error      = 2
      OTHERS             = 3.
  IF sy-subrc = 0.
* PASS THE SELECTED VARIANT TO THE SELECTION SCREEN FIELD
    p_p_var = wa_i_variant-variant.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F4_VARIANT_HELP
*&---------------------------------------------------------------------*
*&      Form  GET_DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*       SUB-ROUTINE GET_DEFAULT_VARIANT TO PASS THE DEFAULT VARIANT
*       IF USER DOESN'T ENTERS ANY VALUE FOR THE VARIANT
*----------------------------------------------------------------------*
*      <--P_P_VAR  variant name
*----------------------------------------------------------------------*
FORM get_default_variant USING l_p_var.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = wa_variant "variant name
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.
* IF DEFAULT VARIANT FOUND
  IF sy-subrc = 0.
* PASS THE DEFAULT VARIANT TO THE SELECTION SCREEN FIELD
    l_p_var = wa_variant-variant.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  CHECK_VARIANT_EXISTENCE
*&---------------------------------------------------------------------*
*       SUB-ROUTINE CHECK_VARIANT_EXISTENCE TO VALIDATE THE VARIANT
*       NAME ENTERED BY THE USER
*       IF VARIANT FOUND THEN EXECUTE
*       ELSE DISPLAY ERROR MESSAGE
*----------------------------------------------------------------------*
*      <--P_P_VAR  variant name
*----------------------------------------------------------------------*
FORM check_variant_existence USING l_p_var.

*&---------------------------------------------------------------------*
*          ASSIGN THE VALUE OF THE VARIANT ENTERED BY USER TO THE
*          WORK AREA FIELD AND CHECK FOR ITS EXISTENCE
*&---------------------------------------------------------------------*
  wa_variant-variant = l_p_var.
*&---------------------------------------------------------------------*
*          TO CHECK THE EXISTENCE FOR VARIANT CORRESPONDING TO
*          EXISTING VARIANT IF THE USER ENTERS SOME VALUE FOR
*          THE PARAMETER
*&---------------------------------------------------------------------*
  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = wa_variant " variant name
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    "MESSAGE e001.
  ENDIF.

ENDFORM.                    " CHECK_VARIANT_EXISTENCE
*&---------------------------------------------------------------------*
*&      Form  USER_ACTION
*&---------------------------------------------------------------------*
*       SUB-ROUTINE USER_ACTION CAN BE USED AS PER THE USER REQUIREMENT
*       TO PROCESS THE SELECTED RECORDS IN ALV GRID DISPLAY
*----------------------------------------------------------------------*
FORM user_action.
*       user code to process selected records in internal table
*       it_process

*&---------------------------------------------------------------------*
*          FIELD CATALOG FOR SELECTED RECORDS
*&---------------------------------------------------------------------*
  PERFORM field_catalog1.


ENDFORM.                    " F4_FILE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG1 FOR SELECTED RECORDS
*&---------------------------------------------------------------------*
*       SUB-ROUTINE FIELD_CATALOG USED TO SET THE COLUMNS FOR
*       THE ALV GRID (OUTPUT FORMAT)
*       SETS THE COLUMN NAME AND THE OUTPUT LENGTH FOR THE FIELDS
*----------------------------------------------------------------------*
FORM field_catalog1 .

  lv_del = 80000133 .

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZPICK_LIST_SFORM_NEW'
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  DESCRIBE TABLE it1_likp LINES DATA(w_cnt).
  DATA: lt_likp            LIKE it1_likp,
        control_parameters TYPE ssfctrlop.
  LOOP AT it1_likp INTO DATA(st_tab).
    DATA(w_cnt2) = sy-tabix .
    CLEAR lt_likp.
    APPEND st_tab TO lt_likp.
    IF w_cnt = 1.
      CALL FUNCTION fm_name " '/1BCDWB/SF00000218'
        EXPORTING
          ip_del           = lv_del
        TABLES
          it1_likp         = lt_likp[]
        EXCEPTIONS
          formatting_error = 1
          internal_error   = 2
          send_error       = 3
          user_canceled    = 4
          OTHERS           = 5.
    ELSE.
      CASE w_cnt2.
        WHEN 1.
          control_parameters-no_open   = space .
          control_parameters-no_close  = 'X' .
        WHEN w_cnt .
          control_parameters-no_open   = 'X' .
          control_parameters-no_close  = space .
        WHEN OTHERS.
          control_parameters-no_open   = 'X' .
          control_parameters-no_close  = 'X' .
      ENDCASE.
      CALL FUNCTION fm_name " '/1BCDWB/SF00000218'
        EXPORTING
*         ARCHIVE_INDEX      =
*         ARCHIVE_INDEX_TAB  =
*         ARCHIVE_PARAMETERS =
          control_parameters = control_parameters
*         MAIL_APPL_OBJ      =
*         MAIL_RECIPIENT     =
*         MAIL_SENDER        =
*         OUTPUT_OPTIONS     =
*         USER_SETTINGS      = 'X'
          ip_del             = lv_del
* IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
*         JOB_OUTPUT_INFO    =
*         JOB_OUTPUT_OPTIONS =
        TABLES
          it1_likp           = lt_likp[]
* EXCEPTIONS
*         FORMATTING_ERROR   = 1
*         INTERNAL_ERROR     = 2
*         SEND_ERROR         = 3
*         USER_CANCELED      = 4
*         OTHERS             = 5
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
  ENDLOOP.
*  REFRESH it_field1.
*  CLEAR wa_field1.
*
*  wa_field1-fieldname = 'VBELN'.    " name of field from internal table
*  wa_field1-tabname = 'IT1_LIKP'. " internal table name
*  wa_field1-outputlen = 20.         " output length on screen
*  wa_field1-seltext_l = text-003.   " header information
*  APPEND wa_field1 TO it_field1.    " append field catalog internal table
*  CLEAR wa_field1.                  " clear field catalog work area
*
*  wa_field1-fieldname = 'ERDAT'.    " name of field from internal table
*  wa_field1-tabname = 'IT1_LIKP'. " internal table name
*  wa_field1-outputlen = 45.         " output length on screen
*  wa_field1-seltext_l = text-004.   " header information
*  APPEND wa_field1 TO it_field1.    " append field catalog internal table
*  CLEAR wa_field1.                  " clear field catalog work area
*
*  wa_field1-fieldname = 'VSTEL'.    " name of field from internal table
*  wa_field1-tabname = 'IT1_LIKP'. " internal table name
*  wa_field1-outputlen = 22.         " output length on screen
*  wa_field1-seltext_l = text-005.   " header information
*  APPEND wa_field1 TO it_field1.    " append field catalog internal table
*  CLEAR wa_field1.                  " clear field catalog work area

ENDFORM.                    " FIELD_CATALOG1
*&---------------------------------------------------------------------*
*&      Form  SORT_FIELD1 FOR SELECTED RECORDS
*&---------------------------------------------------------------------*
*       SUB-ROUTINE SORT_FIELD IS USED TO SORT THE RECORDS IN THE
*       INTERNAL TABLE BASED ON THE GIVEN FIELD AND NATURE OF
*       SORTING TO BE DONE (ASCENDING OR DESCENDING)
*----------------------------------------------------------------------*
FORM sort_field1.

  wa_sort1-spos = 1.               " sort priority
  wa_sort1-fieldname = 'VBELN'.    " field on which records sorted
  wa_sort1-tabname = 'IT1_LIKP'. " internal table name
  wa_sort1-up = c_check.           " sort ascending
  APPEND wa_sort1 TO it_sort1.     " append sort info internal table
  CLEAR wa_sort1.                  " clear sort info work area

ENDFORM.                    " SORT_FIELD1
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT1 FOR SELECTED RECORDS
*&---------------------------------------------------------------------*
*       SUB-ROUTINE SET_LAYOUT IS USED TO SET THE DISPLAY OF THE
*       ALV GRID LINES IN ALTERNATIVE COLOURS
*----------------------------------------------------------------------*
FORM set_layout1 .

  wa_layout1-zebra = c_check.    " so set colors of line alternatively

ENDFORM.                    " SET_LAYOUT1
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY1 FOR SELECTED RECORDS
*&---------------------------------------------------------------------*
*       SUB-ROUTINE ALV_DISPLAY IS USED TO SET THE PARAMETERS
*       FOR THE FUNCTION MODULE REUSE_ALV_GRID_DISPLAY
*       AND PASS THE INTERNAL TABLE EXISTING THE RECORDS TO BE
*       DISPLAYED IN THE GRID FORMAT
*----------------------------------------------------------------------*
FORM alv_display1.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = v_rep_id        " report id
*     i_callback_pf_status_set          = ' '
*     i_callback_user_command           = ' '
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout          = wa_layout1      " for layout
      it_fieldcat        = it_field1       " field catalog
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
      it_sort            = it_sort1        " sort info
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
*     i_save             = ' '
*     is_variant         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab           = it1_likp     " internal table
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
