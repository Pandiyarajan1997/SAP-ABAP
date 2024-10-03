*&---------------------------------------------------------------------*
*& Report ZMM_STOCK_MVT_UPD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_stock_mvt_upd.
DATA : n_matnr TYPE TABLE OF rsdsselopt.
DATA : o_matnr  TYPE TABLE OF rsdsselopt,
       nw_matnr TYPE    rsdsselopt,
       ow_matnr TYPE    rsdsselopt
       .


TYPES: BEGIN OF ty_material,
         matnr     TYPE mara-matnr,
         old_matnr TYPE  mara-matnr,
       END OF ty_material.
DATA : it_mara TYPE TABLE OF ty_material WITH HEADER LINE,
       wa_mara TYPE ty_material.
DATA: lt_excel TYPE TABLE OF alsmex_tabline,
      ls_excel TYPE alsmex_tabline.

DATA : wa_goodsmvt_header  TYPE bapi2017_gm_head_01,
       wa_goodsmvt_code    TYPE bapi2017_gm_code,
       wa_materialdocument TYPE bapi2017_gm_head_ret,
       wa_matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year,
       wa_goodsmvt_headret TYPE bapi2017_gm_head_ret,
       it_goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
       wa_goodsmvt_item    TYPE bapi2017_gm_item_create,
       it_return           TYPE TABLE OF bapiret2,
       wa_return           TYPE  bapiret2.

PARAMETERS:         p_file TYPE rlgrap-filename OBLIGATORY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_file.


START-OF-SELECTION.
  IF p_file IS INITIAL.
    MESSAGE 'Please select a file' TYPE 'E'.
    EXIT.
  ENDIF.

START-OF-SELECTION.
  IF p_file IS INITIAL.
    MESSAGE 'Please select a file' TYPE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 14
      i_end_row               = 1000
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE 'Error reading Excel file' TYPE 'E'.
  ENDIF.

**   Process Excel data into lt_materials
  LOOP AT lt_excel INTO ls_excel.
    CASE ls_excel-col.
      WHEN 1.
        wa_mara-matnr = ls_excel-value.
        nw_matnr-sign = 'I'.
        nw_matnr-option = 'EQ'.
        nw_matnr-low = ls_excel-value.
        APPEND nw_matnr TO n_matnr.
        CLEAR :nw_matnr.
      WHEN 2.
        wa_mara-old_matnr = ls_excel-value.
        APPEND wa_mara TO it_mara.
        CLEAR wa_mara.


        ow_matnr-sign = 'I'.
        ow_matnr-option = 'EQ'.
        ow_matnr-low = ls_excel-value.
        APPEND ow_matnr TO o_matnr.
        CLEAR :ow_matnr.
    ENDCASE.
  ENDLOOP.
  SORT it_mara BY matnr ASCENDING.

  SELECT mc~matnr, mc~werks, mc~lgort,mc~charg,mc~ersda,
  SUM( mc~clabs ) AS clabs ,SUM( mc~cinsm ) AS cinsm ,SUM( mc~cspem ) AS cspem
     FROM mchb AS mc
   WHERE mc~matnr IN @o_matnr
    GROUP BY mc~matnr, mc~werks, mc~lgort,mc~charg,mc~ersda INTO TABLE @DATA(it_old_matnr).

  DELETE  it_old_matnr WHERE  clabs EQ 0 AND cinsm  EQ 0 AND cspem EQ 0.
  SORT it_old_matnr BY matnr ASCENDING werks ASCENDING lgort ASCENDING charg .

  SELECT mc~matnr, mc~werks, mc~lgort,mc~charg,
SUM( mc~clabs ) AS clabs ,SUM( mc~cinsm ) AS cinsm ,SUM( mc~cspem ) AS cspem
 FROM mchb AS mc
WHERE mc~matnr IN @n_matnr
GROUP BY mc~matnr, mc~werks, mc~lgort,mc~charg INTO TABLE @DATA(it_new_matnr).
  DELETE  it_new_matnr WHERE  clabs EQ 0 AND cinsm  EQ 0 AND cspem EQ 0.
  SORT it_new_matnr BY matnr ASCENDING werks ASCENDING lgort ASCENDING charg .

  SELECT mb~matnr, mb~bwkey, mb~vprsv, mb~verpr, mb~stprs FROM  @it_old_matnr AS t1
    INNER JOIN mbew AS mb ON t1~werks = mb~bwkey AND t1~matnr = mb~matnr INTO TABLE @DATA(it_mbew).

  SORT it_mbew  BY matnr ASCENDING bwkey ASCENDING.


  IF it_old_matnr IS NOT INITIAL.

    DATA : lv_rate TYPE mbew-verpr .
    DATA : lv_amounts TYPE p DECIMALS 2 .
    LOOP AT it_old_matnr INTO DATA(wa_old_matnr).
       DATA(lv_index2) = line_index( it_mara[ old_matnr = wa_old_matnr-matnr ] ).
       IF lv_index2 > 0.
      wa_mara = it_mara[ lv_index2 ].
      DATA(lv_index1) = line_index( it_new_matnr[ matnr = wa_mara-matnr
       werks = wa_old_matnr-werks
        lgort = wa_old_matnr-lgort
         charg = wa_old_matnr-charg
*         clabs = wa_old_matnr-clabs
*         cinsm = wa_old_matnr-cinsm
*         cspem = wa_old_matnr-cspem
          ] ).

      IF lv_index1 < 1.




          DATA(lv_mbew) = line_index( it_mbew[ matnr = wa_old_matnr-matnr bwkey = wa_old_matnr-werks ] ).
          IF lv_mbew > 0.
            DATA(wa_mbew)  = it_mbew[ lv_mbew ].
            IF wa_mbew-vprsv = 'V'.
              lv_rate = wa_mbew-verpr.
            ELSEIF wa_mbew-vprsv = 'S'.
              lv_rate = wa_mbew-stprs.
            ENDIF.
          ENDIF.




          IF wa_old_matnr-clabs NE 0.
            wa_goodsmvt_item-material  =  wa_mara-matnr.
            wa_goodsmvt_item-plant  =  wa_old_matnr-werks.
            wa_goodsmvt_item-stge_loc  =  wa_old_matnr-lgort .
            wa_goodsmvt_item-batch  =  wa_old_matnr-charg .
            wa_goodsmvt_item-move_type  = 561 .
            wa_goodsmvt_item-entry_qnt  = wa_old_matnr-clabs .
            lv_amounts = wa_old_matnr-clabs * lv_rate .
            wa_goodsmvt_item-amount_lc  = lv_amounts.
            wa_goodsmvt_item-prod_date  = wa_old_matnr-ersda.

            APPEND wa_goodsmvt_item TO it_goodsmvt_item.

          ENDIF  .


          IF wa_old_matnr-cinsm NE 0.
            wa_goodsmvt_item-material  =  wa_mara-matnr.
            wa_goodsmvt_item-plant  =  wa_old_matnr-werks.
            wa_goodsmvt_item-stge_loc  =  wa_old_matnr-lgort .
            wa_goodsmvt_item-batch  =  wa_old_matnr-charg .
            wa_goodsmvt_item-move_type  = 563 .
            wa_goodsmvt_item-entry_qnt  = wa_old_matnr-cinsm .
            lv_amounts = wa_old_matnr-cinsm * lv_rate .
            wa_goodsmvt_item-amount_lc  = lv_amounts.
             wa_goodsmvt_item-prod_date  = wa_old_matnr-ersda.

            APPEND wa_goodsmvt_item TO it_goodsmvt_item.

          ENDIF  .


          IF wa_old_matnr-cspem NE 0.
            wa_goodsmvt_item-material  =  wa_mara-matnr.
            wa_goodsmvt_item-plant  =  wa_old_matnr-werks.
            wa_goodsmvt_item-stge_loc  =  wa_old_matnr-lgort .
            wa_goodsmvt_item-batch  =  wa_old_matnr-charg .
            wa_goodsmvt_item-move_type  = 565 .
            wa_goodsmvt_item-entry_qnt  = wa_old_matnr-cspem .
            lv_amounts = wa_old_matnr-cspem * lv_rate .
            wa_goodsmvt_item-amount_lc  = lv_amounts.
             wa_goodsmvt_item-prod_date  = wa_old_matnr-ersda.
            APPEND wa_goodsmvt_item TO it_goodsmvt_item.

          ENDIF  .

        ENDIF.

      ENDIF.

      IF it_goodsmvt_item IS NOT INITIAL.
        wa_goodsmvt_header-pstng_date = sy-datum.
        wa_goodsmvt_header-doc_date = sy-datum.
        wa_goodsmvt_header-ref_doc_no = 'MASS ULOAD'.
        wa_goodsmvt_header-bill_of_lading = 'MASS ULOAD'.
        wa_goodsmvt_header-header_txt = 'MASS ULOAD'.
        wa_goodsmvt_code-gm_code = '05'.
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            goodsmvt_header  = wa_goodsmvt_header
            goodsmvt_code    = wa_goodsmvt_code
*           TESTRUN          = ' '
*           GOODSMVT_REF_EWM =
*           GOODSMVT_PRINT_CTRL           =
          IMPORTING
            goodsmvt_headret = wa_goodsmvt_headret
            materialdocument = wa_materialdocument-mat_doc
            matdocumentyear  = wa_materialdocument-doc_year
          TABLES
            goodsmvt_item    = it_goodsmvt_item
*           GOODSMVT_SERIALNUMBER         =
            return           = it_return
*           GOODSMVT_SERV_PART_DATA       =
*           EXTENSIONIN      =
*           GOODSMVT_ITEM_CWM             =
          .
        IF it_return IS  INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          WRITE : / wa_mara-matnr, 'SUCCESS :' ,wa_materialdocument-mat_doc, ' - ',wa_materialdocument-doc_year.

        ELSE.

          LOOP AT it_return INTO wa_return.

            WRITE : / wa_mara-matnr, ' Failed : ',wa_old_matnr-werks, '/' ,wa_old_matnr-lgort, '/',wa_old_matnr-charg, ' |', ':' ,wa_return-message.

          ENDLOOP.
        ENDIF.

      ENDIF.
      CLEAR : wa_goodsmvt_header,wa_goodsmvt_code,wa_goodsmvt_headret,wa_goodsmvt_headret.
      REFRESH : it_goodsmvt_item,it_return.

      CLEAR : wa_goodsmvt_item, wa_mbew,wa_mara,lv_rate.

    ENDLOOP.

  ENDIF.
