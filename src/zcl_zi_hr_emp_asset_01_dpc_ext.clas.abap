class ZCL_ZI_HR_EMP_ASSET_01_DPC_EXT definition
  public
  inheriting from ZCL_ZI_HR_EMP_ASSET_01_DPC
  create public .

public section.
methods /iwbep/if_mgw_appl_srv_runtime~create_stream REDEFINITION.
methods process_excel_upload IMPORTING is_media_resource type ty_s_media_resource
                                       it_key_tab type /iwbep/t_mgw_name_value_pair OPTIONAL.
protected section.
methods fileuploadset_get_entityset REDEFINITION.
private section.
ENDCLASS.



CLASS ZCL_ZI_HR_EMP_ASSET_01_DPC_EXT IMPLEMENTATION.
  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM.
  TYPES: BEGIN OF ty_pernr,
         pernr TYPE pernr_d,
         sname TYPE smnam,
       END OF ty_pernr.
  TYPES: BEGIN OF ty_pernr_hire,    "Structure Employee Hire Date Table Data
     pernr TYPE pernr_d,
     dat01 TYPE dardt,
   END OF ty_pernr_hire.
   types:BEGIN OF ty_data,
         data type zemp_ast_upload,
         end of ty_data,
         tt_data type table of ty_data.
 DATA:gs_data type     ty_data,
      gt_data type tt_data.
** Initial pernr Selection table for Checks **
DATA: gt_pernr TYPE TABLE OF ty_pernr,
      gs_pernr TYPE ty_pernr,
      ls_ret type bapireturn1 .
DATA: ls_0040 TYPE p0040.
DATA: gt_hire_date TYPE TABLE OF ty_pernr_hire.
DATA:obj_msg_con type ref to /iwbep/if_message_container.
               "process excel file into internal table
               data : numberofcolumns type i value 8,
               date_string     type string,
               upload_for_db   type table of zemp_ast_upload,
               emp_ast_db        type zemp_ast_upload,
               pernr        type zemp_ast_upload-pernr.
DATA:ls_message            type          scx_t100key.
    case  iv_entity_name.
      when 'FileUpload'.
      data: lt_bapireturn type TABLE of bapiret2,
            cx_flag type flag.
"Initilization
** Pernr Selction for Checks **
  SELECT a~pernr
         b~sname INTO CORRESPONDING FIELDS OF TABLE gt_pernr
                 FROM pa0000 AS a INNER JOIN pa0001 AS b
                 ON a~pernr = b~pernr
                 WHERE a~begda LE sy-datum
                 AND a~endda GE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_pernr[] BY pernr.
  ENDIF.
** Fetching Hiring Date From PA0040 **
  SELECT pernr
         dat01 FROM pa0041
               INTO TABLE gt_hire_date
               WHERE dar01 = 'S1'
               AND begda LE sy-datum
               AND endda GE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_hire_date[] BY pernr.
  ENDIF.
        try.
            data(excel) = new cl_fdt_xl_spreadsheet( document_name = 'file' xdocument = is_media_resource-value ).
            excel->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = data(worksheets) ).
            data(firstworksheet_data) = excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( worksheets[ 1 ] ).

            field-symbols <excel_data> type table.
            ASSIGN firstworksheet_data->* to <excel_data>.

           field-symbols : <field> type any.
               data(iv_lines) = Lines( <excel_data> ).
          LOOP AT <excel_data> assigning field-symbol(<row>) from 2 .
             do numberofcolumns times.
             assign COMPONENT sy-index of STRUCTURE <row> to <field>.
             if sy-subrc = 0.
             case sy-index.
             When  1. "Pernr
             emp_ast_db-pernr = <field>.
             WHEN 2. "Subty
              emp_ast_db-subty = <field>.
             WHEN 3."Lobnr
               emp_ast_db-lobnr = <field>.
             WHEN 4. "SERIAL_NO
                 emp_ast_db-serial_no = <field>.
             WHEN 5. "ASSET_TAG
                emp_ast_db-asset_tag = <field>.
             WHEN 6. "BUKRS
                emp_ast_db-bukrs = <field>.
             WHEN 7. "werks
                emp_ast_db-werks = <field>.
             WHEN 8. "kostl
                emp_ast_db-kostl = <field>.
             ENDCASE.
             endif.
             ENDDO.
             iv_lines  = iv_lines - 1.
      clear:cx_flag.
             IF emp_ast_db-pernr is INITIAL.
             APPEND VALUE #( type = 'E'
                             number = '002'
                             message = 'Enter Employee number'
                             message_v1 = 'PERNR' ) TO lt_bapireturn.
                 data(msg) = CONV bapi_msg( 'Enter Employee number' ).
*           obj_msg_con->add_message(
*          EXPORTING
*            iv_msg_type               = 'E'
*            iv_msg_id                 = '5Q'
*            iv_msg_number             = '002'
*            iv_msg_text               = 'Enter Employee number'
*             iv_msg_v1                =  |PERNR|
*        ).
             cx_flag = 'X'.
             ENDIF.
            IF emp_ast_db-subty is INITIAL.
            APPEND VALUE #( type = 'E'
                             number = '002'
                             message = 'Enter Subtype'
                             message_v1 = 'SUBTYPE' ) TO lt_bapireturn.
                msg = 'Enter Subtype'.
             cx_flag = 'X'.
            ENDIF.

IF cx_flag is initial.
*** Already assigned to any other employee Checks ***
      SELECT SINGLE * FROM pa0040 INTO @DATA(l_duplicate_chk)
        WHERE lobnr = @emp_ast_db-lobnr
        AND serial_no = @emp_ast_db-serial_no
        AND asset_tag = @emp_ast_db-asset_tag
        AND begda LE @sy-datum
        AND endda GE @sy-datum.
      IF sy-subrc EQ 0.
      data(sname) = VALUE #( gt_pernr[ pernr = emp_ast_db-pernr ]-sname optional ).
       APPEND VALUE #( type = 'E'
                             number = '002'
                             message = |Already assets are assigned to someone |
                             message_v1 = |EMP_ID:{ emp_ast_db-pernr },Name:{ sname }| ) TO lt_bapireturn.
                      msg =  |Already assets are assigned to someone |.
          ls_message = VALUE #( msgid = 'SY'  msgno = '002' attr1 = |Already assets are assigned to someone, { sname }|
                                attr2 =  |EMP_ID:{ emp_ast_db-pernr },Name:{ sname }|  ).
*        obj_msg_con->add_message(
*          EXPORTING
*            iv_msg_type               = 'E'
*            iv_msg_id                 = '5Q'
*            iv_msg_number             = '002'
*            iv_msg_text               = 'Already assets are assigned to someone'
*             iv_msg_v1                =  |EMP_ID:{ emp_ast_db-pernr },Name:{ sname }|
*        ).
            cx_flag = 'X'.
      ENDIF.
             IF cx_flag is initial.
             append emp_ast_db to upload_for_db.
             gs_data-data = emp_ast_db.
             append gs_data to gt_data.
***Lock pernr For Data update ***
      CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
        EXPORTING
          number = emp_ast_db-pernr
        IMPORTING
          return = ls_ret.
           IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
            CLEAR:ls_0040, ls_ret.
        ls_0040-pernr = |{ emp_ast_db-pernr ALPHA = IN }|.
        ls_0040-subty = emp_ast_db-subty.
        READ TABLE gt_hire_date INTO data(gs_hire_date) WITH KEY pernr = emp_ast_db-pernr.
        IF sy-subrc EQ 0.
          ls_0040-begda = gs_hire_date-dat01.
        ENDIF.
        ls_0040-endda = '99991231'.
        ls_0040-lobnr = emp_ast_db-lobnr. " Asset ID
        ls_0040-serial_no = emp_ast_db-serial_no. "Device Serial Number
        ls_0040-asset_tag = emp_ast_db-asset_tag. "Asset Tag
        ls_0040-bukrs = emp_ast_db-bukrs. "Company Code
        ls_0040-werks = emp_ast_db-werks. "Plant
        ls_0040-kostl = emp_ast_db-kostl. "Costcenter
**Function Module To update Infotype **
        CLEAR: ls_ret.
         CALL FUNCTION 'HR_INFOTYPE_OPERATION'
          EXPORTING
            infty         = '0040'
            number        = ls_0040-pernr
            subtype       = ls_0040-subty
            validityend   = ls_0040-endda
            validitybegin = ls_0040-begda
            record        = ls_0040
            operation     = 'INS'
          IMPORTING
            return        = ls_ret.
           ENDIF.

       IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
           CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
          EXPORTING
            number = emp_ast_db-pernr
          IMPORTING
            return = ls_ret.
            ELSE.
                 APPEND VALUE #( type = ls_ret-type
                             number = ls_ret-number
                             message = ls_ret-message
                             message_v1 = ls_ret-message_v1 ) TO lt_bapireturn.
                       msg = ls_ret-message.
          ENDIF.
          ENDIF.
         ENDIF.
          ENDLOOP.

        IF upload_for_db is NOT INITIAL.
          Modify zemp_ast_upload from TABLE upload_for_db.
          ENDIF.


            CATCH cx_root.

            mo_context->get_message_container( )->add_message(
              EXPORTING
                iv_msg_type               = /iwbep/cl_cos_logger=>error
                iv_msg_id                 = '5Q'
                iv_msg_number             = 395
            ).
        endtry.

     IF lt_bapireturn IS NOT INITIAL AND iv_lines = 1.
*       call method /iwbep/if_mgw_conv_srv_runtime~get_message_container
*         RECEIVING
*           ro_message_container = obj_msg_con
*         .
*            obj_msg_con = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
*            obj_msg_con->add_messages_from_bapi(
*              EXPORTING
*                it_bapi_messages          = lt_bapireturn
*                iv_add_to_response_header = abap_true
**               iv_determine_leading_msg =
*            ).
*            mo_context->get_message_container( )->add_messages_from_bapi(
*              EXPORTING
*                it_bapi_messages          = lt_bapireturn
*                iv_add_to_response_header = abap_true
*                iv_determine_leading_msg  =  /iwbep/if_message_container=>gcs_leading_msg_search_option-none
*            ).

            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                textid                 =  ls_message"/iwbep/cx_mgw_busi_exception=>business_error
*                previous               =
*                message_container      = me->mo_context->get_message_container( )
*                http_status_code       =
*                http_header_parameters =
*                sap_note_id            =
*                msg_code               = 'SY/520'
*                exception_category     =
*                entity_type            =
*                message                = CONV #( msg )"'Error in File Upload,Check Backend System!!'
*                message_unlimited      =
*                filter_param           =
*                operation_no           =
            .
            ELSE.
        copy_data_to_ref(
          EXPORTING
            is_data = emp_ast_db
          CHANGING
            cr_data = er_entity
        ).
          ENDIF.

    endcase.
  ENDMETHOD.

  METHOD PROCESS_EXCEL_UPLOAD.

  ENDMETHOD.



  METHOD FILEUPLOADSET_GET_ENTITYSET.
  BREAK-POINT.
    data(flag) = 'X'.
  ENDMETHOD.

ENDCLASS.
