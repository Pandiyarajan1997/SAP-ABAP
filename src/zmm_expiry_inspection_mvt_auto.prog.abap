*&---------------------------------------------------------------------*
*& Report ZMM_STOCK_MVT_UPD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_expiry_inspection_mvt_auto.


DATA : wa_goodsmvt_header  TYPE bapi2017_gm_head_01,
       wa_goodsmvt_code    TYPE bapi2017_gm_code,
       wa_materialdocument TYPE bapi2017_gm_head_ret,
       wa_matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year,
       wa_goodsmvt_headret TYPE bapi2017_gm_head_ret,
       it_goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
       wa_goodsmvt_item    TYPE bapi2017_gm_item_create,
       it_return           TYPE TABLE OF bapiret2,
       wa_return           TYPE  bapiret2.

START-OF-SELECTION.


select md~werks,md~lgort,md~matnr,md~menge,md~charg from matdoc as md
where md~BUDAT = @sy-datum and md~bwart = '344' and md~shkzg = 'S' and md~tcode2 = 'QA07' into TABLE @DATA(IT_TAB).






  IF IT_TAB IS NOT INITIAL.


    LOOP AT IT_TAB INTO DATA(wa_tab).
      IF wa_tab-menge NE 0.
        wa_goodsmvt_item-material  =  wa_tab-matnr.
        wa_goodsmvt_item-plant  =  wa_tab-werks.
        wa_goodsmvt_item-stge_loc  =  wa_tab-lgort .
        wa_goodsmvt_item-batch  =  wa_tab-charg .
        wa_goodsmvt_item-move_type  =  '349' .
        wa_goodsmvt_item-entry_qnt  = wa_tab-menge .


        APPEND wa_goodsmvt_item TO it_goodsmvt_item.
        clear : wa_goodsmvt_item.
      ENDIF  .









      IF it_goodsmvt_item IS NOT INITIAL.
        wa_goodsmvt_header-pstng_date = sy-datum.
        wa_goodsmvt_header-doc_date = sy-datum.
        wa_goodsmvt_header-ref_doc_no = 'MASS ULOAD'.
        wa_goodsmvt_header-bill_of_lading = 'MASS ULOAD'.
        wa_goodsmvt_header-header_txt = 'MASS ULOAD'.
        wa_goodsmvt_code-gm_code = '04'.
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
          WRITE : / wa_tab-matnr, 'SUCCESS :' ,wa_materialdocument-mat_doc, ' - ',wa_materialdocument-doc_year.

        ELSE.

          LOOP AT it_return INTO wa_return.

            WRITE : / wa_tab-matnr, ' Failed : ',wa_tab-werks, '/' ,wa_tab-lgort, '/',wa_tab-charg, ' |', ':' ,wa_return-message.

          ENDLOOP.
        ENDIF.

      ENDIF.
      CLEAR : wa_goodsmvt_header,wa_goodsmvt_code,wa_goodsmvt_headret,wa_goodsmvt_headret.
      REFRESH : it_goodsmvt_item,it_return.

      CLEAR : wa_goodsmvt_item.

    ENDLOOP.

  ENDIF.
