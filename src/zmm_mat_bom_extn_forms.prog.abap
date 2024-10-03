*&---------------------------------------------------------------------*
*& Include          ZMM_MAT_BOM_EXTN_FORMS
*&---------------------------------------------------------------------*
CLASS lcl_bom_process DEFINITION.

  PUBLIC SECTION.
    METHODS:
      f4_value_help,
      convert_excel,
      f_create_extend_bom,
      display_alv.

ENDCLASS.
CLASS lcl_bom_process IMPLEMENTATION.
  "F4 help in selection Screen
  METHOD f4_value_help.
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

  METHOD convert_excel.

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
      MESSAGE 'Excel Conversion Error' TYPE 'E'.
    ENDIF.

  ENDMETHOD.
  "Bill of Material Process
  METHOD f_create_extend_bom.

    DATA: lt_return TYPE TABLE OF bapiret2.
    DATA: lv_matnr TYPE matnr.
    DATA: lv_refitem(02) TYPE n,
          lv_item(04)    TYPE n,
          lv_qty(16)     TYPE c,
          lv_datum(10)   TYPE c.
    DATA(lt_excel_tmp) = gt_excel[].
    DELETE ADJACENT DUPLICATES FROM lt_excel_tmp[] COMPARING matnr werks stlal.

    REFRESH: gt_alv.
    LOOP AT lt_excel_tmp ASSIGNING FIELD-SYMBOL(<fls_excel_tmp>).
      REFRESH:gt_bdcdata,gt_bdcmsg. CLEAR: lv_matnr,lv_item,lv_refitem.
      lv_refitem = '01'.
      lv_item = '0010'.
*----- Material Existence Checks ---------------------*
      SELECT SINGLE matnr FROM mara INTO lv_matnr WHERE matnr = <fls_excel_tmp>-matnr.
      IF sy-subrc NE 0.
        APPEND VALUE #( matnr = <fls_excel_tmp>-matnr
                        werks = <fls_excel_tmp>-werks
                        stlal = <fls_excel_tmp>-stlal
                        type = 'E'
                        message = |Material Number { <fls_excel_tmp>-matnr } Does not exists| ) TO gt_alv.
        CONTINUE.
      ENDIF.
      DATA(lv_bom_matnr) = <fls_excel_tmp>-matnr.
      DATA(lv_bom_plant) = <fls_excel_tmp>-werks.
      DATA(lv_date) =  sy-datum.
      CLEAR lv_datum.
      WRITE lv_date TO lv_datum DD/MM/YYYY.

      gt_bdcdata = VALUE #(
                    ( program = 'SAPLCSDI' dynpro = '0100' dynbegin = 'X' )
                    ( fnam = 'BDC_CURSOR'  fval = 'RC29N-STLAL' )
                    ( fnam = 'BDC_OKCODE'  fval = '/00' )
                    ( fnam = 'RC29N-MATNR' fval = lv_bom_matnr )
                    ( fnam = 'RC29N-WERKS' fval = lv_bom_plant )
                    ( fnam = 'RC29N-STLAN' fval = '1' )
                    ( fnam = 'RC29N-STLAL' fval = <fls_excel_tmp>-stlal )
                    ( fnam = 'RC29N-DATUV' fval = lv_date )
                    ( program = 'SAPLCSDI' dynpro = '0110' dynbegin = 'X' )
                    ( fnam = 'BDC_OKCODE'  fval = '/00' )
                    ( fnam = 'RC29K-BMENG' fval =  <fls_excel_tmp>-base_qty )
                    ( fnam = 'RC29K-STLST' fval =  <fls_excel_tmp>-base_uom )
                    ( fnam = 'BDC_CURSOR'  fval = 'RC29K-EXSTL' )
                    ( program = 'SAPLCSDI' dynpro = '0140' dynbegin = 'X' )
                    ( fnam = 'BDC_OKCODE'  fval = '/00' )
                    ( fnam = 'BDC_CURSOR'  fval = 'RC29P-MEINS(01)' )
                    ).

      LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fgs_excel>) WHERE matnr = <fls_excel_tmp>-matnr
                                                   AND werks = <fls_excel_tmp>-werks
                                                   AND stlal = <fls_excel_tmp>-stlal.
        CLEAR lv_matnr.
*--------------- Components Material Check incoorrect material ----------------------------------------------------*
        SELECT SINGLE matnr FROM mara INTO lv_matnr WHERE matnr = <fgs_excel>-matnr.
        IF sy-subrc NE 0.
          APPEND VALUE #( matnr = <fls_excel_tmp>-matnr
                          werks = <fls_excel_tmp>-werks
                          stlal = <fls_excel_tmp>-stlal
                          idnrk = <fgs_excel>-idnrk
                          type = 'E'
                          message = |Material Number { <fgs_excel>-idnrk } Does not exists| ) TO gt_alv.
          CONTINUE.
        ENDIF.
*------------------------ Lineitems Preparation of components ------------------------------------------------------*
        DATA(lv_item_tmp) = lv_item.
        lv_qty = <fgs_excel>-item_qty.
        CONDENSE lv_qty.
        APPEND VALUE #( fnam = |RC29P-IDNRK({ lv_refitem } |    fval = <fgs_excel>-idnrk ) TO gt_bdcdata.
        APPEND VALUE #( fnam = |RC29P-MENGE({ lv_refitem }) |    fval = lv_qty ) TO gt_bdcdata.
        APPEND VALUE #( fnam = |RC29P-MEINS({ lv_refitem }) |    fval = <fgs_excel>-item_uom ) TO gt_bdcdata.
        APPEND VALUE #( fnam = |RC29P-POSTP({ lv_refitem }) |    fval = 'L' ) TO gt_bdcdata.
        lv_item = lv_item + 10.
        lv_refitem = lv_refitem + 1.
      ENDLOOP.

      CLEAR lv_item.
      lv_item = '0010'.
      LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fgs_excel1>) WHERE matnr = <fls_excel_tmp>-matnr
                                                   AND werks = <fls_excel_tmp>-werks
                                                   AND stlal = <fls_excel_tmp>-stlal.
        CLEAR lv_qty.
        lv_qty = <fgs_excel1>-item_qty.
        CONDENSE lv_qty.
        APPEND VALUE #( program = 'SAPLCSDI' dynpro = '0130' dynbegin = 'X' ) TO gt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE'  fval = '/00' ) TO gt_bdcdata.
        APPEND VALUE #( fnam = 'RC29P-POSNR'   fval = lv_item ) TO gt_bdcdata.
        APPEND VALUE #( fnam = 'RC29P-IDNRK'   fval = <fgs_excel1>-idnrk ) TO gt_bdcdata.
        APPEND VALUE #( fnam = 'RC29P-MEINS'   fval = lv_qty ) TO gt_bdcdata.
        APPEND VALUE #( fnam = 'RC29P-MEINS'   fval = <fgs_excel1>-item_uom ) TO gt_bdcdata.
        lv_item = lv_item + 10.
      ENDLOOP.

      APPEND VALUE #( program = 'SAPLCSDI' dynpro = '0140' dynbegin = 'X' ) TO gt_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'  fval = '=FCBU' ) TO gt_bdcdata.

      DATA(lv_tcode) = 'CS01'.
      DATA(ls_options) = VALUE ctu_params(  dismode = 'N'
                                            updmode = 'S'
                                            defsize = ''
                                            nobinpt = 'X'
                                            racommit = 'X' ).
      TRY.
          CALL TRANSACTION lv_tcode WITH AUTHORITY-CHECK
                                  USING gt_bdcdata OPTIONS FROM ls_options
                                  MESSAGES INTO gt_bdcmsg.
        CATCH cx_sy_authorization_error ##NO_HANDLER.
      ENDTRY.
      COMMIT WORK AND WAIT.

    ENDLOOP.
  ENDMETHOD.

  METHOD display_alv.
  ENDMETHOD.

ENDCLASS.
FORM bdc_dynpro  USING program TYPE any
                        dynpro TYPE any.
  CLEAR:gw_bdcdata.
  gw_bdcdata-program  = program.
  gw_bdcdata-dynpro   = dynpro.
  gw_bdcdata-dynbegin = 'X'.
  APPEND gw_bdcdata TO gt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM bdc_field  USING  fnam TYPE any
                      fval TYPE any.
  CLEAR:gw_bdcdata.
  gw_bdcdata-fnam = fnam.
  gw_bdcdata-fval = fval.
  APPEND gw_bdcdata TO gt_bdcdata.
ENDFORM.
