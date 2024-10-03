*&----------------------------------------------------------------------------*
*& Report  ZSD_CUSTOMER_UPDATE
*&
*&----------------------------------------------------------------------------*
*=============================================================================*
*  MODULE                 : SD
*  Functional Consultant  : Bijay
*  Created By             : Himansu Patnaik
*  Start Date             : 15.10.2020
*  Description            : Update the customer update report
*  TR                     : DEVK921457
*==============================================================================*
*                     Modify Log History.
* -----------------------------------------------------------------------------*
* DATE       |User ID    |TS Ver  | Transport Request| Description
* -----------------------------------------------------------------------------*
*
*-------------------------------------------------------------------------------*
REPORT zsd_customer_update.

*------------------------------------------------------------------------------*
*Structure for error message
*------------------------------------------------------------------------------*

TYPE-POOLS truxs.

TYPES : BEGIN   OF ty_final,
        kunnr TYPE kunnr,
        bukrs TYPE bukrs,
        vkorg TYPE vkorg,
        vtweg TYPE vtweg,
        spart TYPE spart,
        taxkd TYPE takld,
        END     OF ty_final.

TYPES : BEGIN          OF ty_mess,
        kunnr        TYPE kunnr ,
        tcode        TYPE tstct-tcode,    "Tcode
        ttext        TYPE tstct-ttext,    "Tcode description
        remarks(100) TYPE c,
        END            OF ty_mess.

TYPES: fs_struct(4096) TYPE c OCCURS 0 .
*------------------------------------------------------------------------*
* Structure Decleration
*------------------------------------------------------------------------*

DATA : it_final  TYPE STANDARD TABLE OF ty_final,
       it_out    TYPE STANDARD TABLE OF ty_mess,
       it_msg    TYPE TABLE OF bdcmsgcoll,   " Collecting Error messages
       it_file   TYPE truxs_t_text_data,
       it_data   TYPE soi_generic_table,
       wa_data   TYPE soi_generic_item,
       wa_msg    TYPE bdcmsgcoll,
       wa_final  TYPE ty_final,
       wa_out    TYPE ty_mess.


DATA : lv_xd02          TYPE c,
       lv_count(5)      TYPE n,
       lv_mode          TYPE c,
       lv_update        TYPE c,
       lv_text(132)     TYPE c,
       lv_tabix         TYPE sy-tabix,
       lv_file           TYPE string.


DATA : salv_table       TYPE REF TO cl_salv_table.

*------------------------------------------------------------------------*
*  LIKE Structure Decleration
*------------------------------------------------------------------------*
DATA : it_bdcdata  TYPE TABLE OF bdcdata,
       wa_bdcdata  TYPE bdcdata.

*----------------------------------------------------------------------*
*-----Define  CONSTANTS
*----------------------------------------------------------------------*

CONSTANTS:  c_vd02   TYPE tstct-tcode  VALUE 'VD02'.

*------------------------------------------------------------------------*
*Selection screen
*------------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_file   TYPE rlgrap-filename.                 " File Path

SELECTION-SCREEN END OF BLOCK  b1.

*------------------------------------------------------------------------*
*initialization
*------------------------------------------------------------------------*
INITIALIZATION.
  CLEAR: lv_count.
*------------------------------------------------------------------------*
*AT SELECTION SCREEN
*------------------------------------------------------------------------*

AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
*----- AT SELECTION-SCREEN
*----------------------------------------------------------------------*
* Opening window for path selection

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

* To provide F4 help for the file

  PERFORM sub_file_f4.

*--------------------------------------------------------------------*
*   start of selection event.
*--------------------------------------------------------------------*

START-OF-SELECTION.
* take the excel data and save in internal table

  PERFORM read_excel_data.

  LOOP AT it_final INTO wa_final .
*    lv_tabix = sy-tabix.
*    IF lv_tabix > 1 .
    REFRESH: it_bdcdata.
    CLEAR wa_bdcdata.

    PERFORM populate_bdcdata.

    PERFORM insert_data.

*    ENDIF.
  ENDLOOP.

*--------------------------------------------------------------------*
*   End of selection event.
*--------------------------------------------------------------------*
END-OF-SELECTION.

* Error report .
  IF it_out[] IS NOT INITIAL.
    PERFORM display_error_output.
  ENDIF.

* Output ALV report to user
  IF it_out[] IS NOT INITIAL.
    salv_table->display( ).
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  populate_bdcdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM populate_bdcdata.

  PERFORM bdc_dynpro      USING 'SAPMF02D' '0108'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF02D-D0320'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RF02D-KUNNR'
                                wa_final-kunnr.
  PERFORM bdc_field       USING 'RF02D-VKORG'
                                wa_final-vkorg.
  PERFORM bdc_field       USING 'RF02D-VTWEG'
                                wa_final-vtweg.
  PERFORM bdc_field       USING 'RF02D-SPART'
                                wa_final-spart.
  PERFORM bdc_field       USING 'RF02D-D0320'
                                 'X'.
  PERFORM bdc_dynpro      USING 'SAPMF02D' '0320'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KNVV-INCO1'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTS'.

  PERFORM bdc_dynpro      USING 'SAPMF02D' '1350'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KNVI-TAXKD(06)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTR'.
  PERFORM bdc_field       USING 'KNVI-TAXKD(06)'
                                wa_final-taxkd.
  PERFORM bdc_dynpro      USING 'SAPMF02D' '1350'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KNVI-TAXKD(06)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTR'.



ENDFORM.                    "populate_bdcdata
*----------------------------------------------------------------------*
*Start new screen
*----------------------------------------------------------------------*
FORM bdc_dynpro USING p_program p_dynpro.
  CLEAR wa_bdcdata.
  wa_bdcdata-program  = p_program.
  wa_bdcdata-dynpro   = p_dynpro.
  wa_bdcdata-dynbegin = 'X'.
  APPEND wa_bdcdata TO it_bdcdata.
ENDFORM.                    "BDC_DYNPRO
*----------------------------------------------------------------------*
*Insert field
*----------------------------------------------------------------------*
FORM bdc_field USING p_fnam p_fval.
  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = p_fnam.
  wa_bdcdata-fval = p_fval.
  APPEND wa_bdcdata TO it_bdcdata.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  insert_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM insert_data.

* Call transaction 'XD02'
  lv_mode = 'N'.
  lv_update = 'S'.

  REFRESH it_msg.
  CALL TRANSACTION c_VD02 USING it_bdcdata "#EC CI_USAGE_OK[2265093] " Added by <IT-CAR Tool> during Code Remediation
                   MODE   lv_mode
                   UPDATE lv_update
                   MESSAGES INTO it_msg.

  READ TABLE  it_msg INTO wa_msg WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0 .
    PERFORM get_message TABLES it_msg
                        USING  wa_final-kunnr
                               c_vd02.

  ELSE.
    lv_xd02 = 'X'.
    lv_count = lv_count + 1.
    lv_text = text-003.
    REPLACE '&1' WITH lv_count INTO lv_text.
    COMMIT WORK.
    MESSAGE lv_text TYPE 'S'.
  ENDIF.

  CLEAR: wa_bdcdata,it_bdcdata,
        it_msg[], wa_msg.

ENDFORM.                    "insert_data
*&---------------------------------------------------------------------*
*&      Form  GET_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MESSTAB  text
*      -->P2_PSPID   text
*      -->P2_TCODE   text
*      -->P2_ERR_IND text
*----------------------------------------------------------------------*
FORM get_message  TABLES   p_messtab STRUCTURE bdcmsgcoll
                  USING    p2_kunnr
                           p2_xd02.


*  READ TABLE GT_MESSTAB INTO wa_msg WITH KEY MSGTYP = 'E'.
  LOOP AT it_msg INTO wa_msg WHERE msgtyp = 'E'.
    IF sy-subrc = 0.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = wa_msg-msgid
          lang      = 'EN'
          no        = wa_msg-msgnr
          v1        = wa_msg-msgv1
          v2        = wa_msg-msgv2
          v3        = wa_msg-msgv3
          v4        = wa_msg-msgv4
        IMPORTING
          msg       = wa_out-remarks
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF wa_out-remarks IS NOT INITIAL.
        wa_out-kunnr  = p2_kunnr.
        wa_out-tcode  = p2_xd02.
        wa_out-ttext  = wa_msg-msgv1.

        APPEND wa_out TO it_out.
        CLEAR wa_out.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ERROR_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_error_output.
* Create an instance of the SALV table object

  CALL METHOD cl_salv_table=>factory
    EXPORTING
      list_display = if_salv_c_bool_sap=>false
    IMPORTING
      r_salv_table = salv_table
    CHANGING
      t_table      = it_out.

ENDFORM.                    "Display_error_output
*&---------------------------------------------------------------------*
*&      Form  SUB_FILE_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_file_f4 .
  DATA:
    l_desktop       TYPE string,
    l_i_files       TYPE filetable,
    l_wa_files      TYPE file_table,
    l_rcode         TYPE int4.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
*     program_name  = syst-cprog
*     dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.


ENDFORM.                    " SUB_FILE_F4
*&---------------------------------------------------------------------*
*&      Form  READ_EXCEL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_excel_data.

* Uploading excel file.
*  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
*    EXPORTING
**     i_field_seperator    = 'X'
**     i_line_header        = 'X'
*      i_tab_raw_data       = it_file
*      i_filename           = p_file
*    TABLES
*      i_tab_converted_data = it_final[]
*    EXCEPTIONS
*      conversion_failed    = 1
*      OTHERS               = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
  lv_file = p_file.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename            = lv_file
      filetype            = 'ASC'
      has_field_separator = 'X'
    TABLES
      data_tab            = it_final[].


ENDFORM.                    "sub_file_f4
