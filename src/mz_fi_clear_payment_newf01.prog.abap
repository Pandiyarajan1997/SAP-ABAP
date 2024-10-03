*&---------------------------------------------------------------------*
*&  Include           MZ_FI_FB05_INCOMING_PAYMENTF01
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                          p_table_name
                          p_mark_name
                 CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA: l_ok     TYPE sy-ucomm,
         l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
   SEARCH p_ok FOR p_tc_name.
   IF sy-subrc <> 0.
     EXIT.
   ENDIF.
   l_offset = strlen( p_tc_name ) + 1.
   l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
   CASE l_ok.
     WHEN 'INSR'.                      "insert row
       PERFORM fcode_insert_row USING    p_tc_name
                                         p_table_name.
       CLEAR p_ok.

     WHEN 'DELE'.                      "delete row
       PERFORM fcode_delete_row USING    p_tc_name
                                         p_table_name
                                         p_mark_name.
       CLEAR p_ok.

     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM compute_scrolling_in_tc USING p_tc_name
                                             l_ok.
       CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
     WHEN 'MARK'.                      "mark all filled lines
       PERFORM fcode_tc_mark_lines USING p_tc_name
                                         p_table_name
                                         p_mark_name   .
       CLEAR p_ok.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM fcode_tc_demark_lines USING p_tc_name
                                           p_table_name
                                           p_mark_name .
       CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_insert_row
               USING    p_tc_name           TYPE dynfnam
                        p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_lines_name       LIKE feld-name.
   DATA l_selline          LIKE sy-stepl.
   DATA l_lastline         TYPE i.
   DATA l_line             TYPE i.
   DATA l_table_name       LIKE feld-name.
   FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
   FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
   FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
   ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
   GET CURSOR LINE l_selline.
   IF sy-subrc <> 0.                   " append line to table
     l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
     IF l_selline > <lines>.
       <tc>-top_line = l_selline - <lines> + 1 .
     ELSE.
       <tc>-top_line = 1.
     ENDIF.
   ELSE.                               " insert line into table
     l_selline = <tc>-top_line + l_selline - 1.
     l_lastline = <tc>-top_line + <lines> - 1.
   ENDIF.
*&SPWIZARD: set new cursor line                                        *
   l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
   INSERT INITIAL LINE INTO <table> INDEX l_selline.
   <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
   SET CURSOR LINE l_line.

 ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_delete_row
               USING    p_tc_name           TYPE dynfnam
                        p_table_name
                        p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
   DESCRIBE TABLE <table> LINES <tc>-lines.

   LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     IF <mark_field> = 'X'.
       DELETE <table> INDEX syst-tabix.
       IF sy-subrc = 0.
         <tc>-lines = <tc>-lines - 1.
       ENDIF.
     ENDIF.
   ENDLOOP.

 ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM compute_scrolling_in_tc USING    p_tc_name
                                       p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_tc_new_top_line     TYPE i.
   DATA l_tc_name             LIKE feld-name.
   DATA l_tc_lines_name       LIKE feld-name.
   DATA l_tc_field_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
   ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
   IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
     l_tc_new_top_line = 1.
   ELSE.
*&SPWIZARD: no, ...                                                    *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
       EXPORTING
         entry_act             = <tc>-top_line
         entry_from            = 1
         entry_to              = <tc>-lines
         last_page_full        = 'X'
         loops                 = <lines>
         ok_code               = p_ok
         overlapping           = 'X'
       IMPORTING
         entry_new             = l_tc_new_top_line
       EXCEPTIONS
*        NO_ENTRY_OR_PAGE_ACT  = 01
*        NO_ENTRY_TO           = 02
*        NO_OK_CODE_OR_PAGE_GO = 03
         OTHERS                = 0.
   ENDIF.

*&SPWIZARD: get actual tc and column                                   *
   GET CURSOR FIELD l_tc_field_name
              AREA  l_tc_name.

   IF syst-subrc = 0.
     IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
       SET CURSOR FIELD l_tc_field_name LINE 1.
     ENDIF.
   ENDIF.

*&SPWIZARD: set the new top line                                       *
   <tc>-top_line = l_tc_new_top_line.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
 FORM fcode_tc_mark_lines USING p_tc_name
                                p_table_name
                                p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
   LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     <mark_field> = 'X'.
   ENDLOOP.
 ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
 FORM fcode_tc_demark_lines USING p_tc_name
                                  p_table_name
                                  p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
   LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     <mark_field> = space.
   ENDLOOP.
 ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Form  ADD_LINES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM add_lines.

   APPEND INITIAL LINE TO gt_tab.

 ENDFORM.                    "add_lines
*&---------------------------------------------------------------------*
*&      Form  BAPI_AR_ACC_GETOPENITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM bapi_ar_acc_getopenitems  USING    p_bukrs TYPE bukrs
                                         p_kunnr TYPE kunnr
                                         p_belnr TYPE belnr_d
                                 CHANGING  ps_bapireturn LIKE gs_bapireturn
                                           pt_lineitems LIKE gt_lineitems.

   PERFORM progress_indicator USING 'Fetching open items' 'for Customer:' gs_tab-kunnr 'is in progress'.

   CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'"#EC CI_USAGE_OK[2628704]
     EXPORTING "Added by SPLABAP during code remediation
       companycode = p_bukrs
       customer    = p_kunnr
       keydate     = sy-datum
*      NOTEDITEMS  = ' '
*      SECINDEX    = ' '
     IMPORTING
       return      = ps_bapireturn
     TABLES
       lineitems   = pt_lineitems.

   IF sy-subrc IS NOT INITIAL.                             "#EC FB_NORC
     gs_message-mess_type = gs_bapireturn-type.
     gs_message-message = gs_bapireturn-message.
     MOVE-CORRESPONDING gs_tab TO gs_message.
     APPEND gs_message TO gt_message.
     CLEAR gs_bapireturn.
     ENDIF.
   "ELSE.
* if document number entered in table control, then we need to exclude other open items
* for that customer
*     IF p_belnr IS NOT INITIAL.
*       DELETE  pt_lineitems WHERE fisc_year NE gs_header-gjahr .
*       LOOP AT pt_lineitems INTO gs_lineitems.
*         IF gs_lineitems-doc_no EQ p_belnr OR gs_lineitems-inv_ref EQ p_belnr.
*         ELSE.
*           DELETE  pt_lineitems INDEX sy-tabix.
*         ENDIF.
*         CLEAR gs_lineitems.
*       ENDLOOP.
*     ENDIF.
*   ENDIF.
*
*
*   LOOP AT pt_lineitems INTO gs_lineitems where sp_gl_ind NE ''.
*     IF gs_lineitems-sp_gl_ind NE 'A'.
*       DELETE pt_lineitems INDEX sy-tabix..
*     ENDIF.
*     CLEAR gs_lineitems.
*   ENDLOOP.

 ENDFORM.                    "bapi_ar_acc_getopenitems
*&---------------------------------------------------------------------*
*&      Form  DUE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM determine_due_date  USING    pt_lineitems LIKE gt_lineitems
                          CHANGING pt_openitems LIKE gt_openitems.

   CLEAR gs_lineitems.

   IF pt_lineitems IS NOT INITIAL.
     SELECT * FROM bkpf INTO TABLE gt_bkpf
              FOR ALL ENTRIES IN pt_lineitems
              WHERE bukrs = pt_lineitems-comp_code
              AND   belnr = pt_lineitems-doc_no
              AND   gjahr = pt_lineitems-fisc_year.
   ENDIF.
   LOOP AT pt_lineitems INTO gs_lineitems.

     IF gs_lineitems-doc_type = 'DR' OR ( gs_lineitems-doc_type = 'AB' )" AND  gs_lineitems-db_cr_ind = 'S' )
       OR gs_lineitems-doc_type = 'RV' OR gs_lineitems-doc_type = 'DZ' OR gs_lineitems-doc_type = 'DG' OR
       gs_lineitems-doc_type = 'VG' OR gs_lineitems-doc_type = 'KR'.
       MOVE-CORRESPONDING gs_lineitems TO gs_openitems.
*   Get Net due date
       CLEAR: gs_faede.
       gs_faede-shkzg = gs_lineitems-db_cr_ind.
       gs_faede-koart = 'D'.
       gs_faede-zfbdt = gs_lineitems-bline_date.
       gs_faede-zbd1t = gs_lineitems-dsct_days1.
       gs_faede-zbd2t = gs_lineitems-dsct_days2.
       gs_faede-zbd3t = gs_lineitems-netterms.
       gs_faede-rebzg = gs_lineitems-inv_ref.
       gs_faede-bldat = gs_lineitems-doc_date.
       CALL FUNCTION 'DETERMINE_DUE_DATE'
         EXPORTING
           i_faede                    = gs_faede
         IMPORTING
           e_faede                    = gs_faede
         EXCEPTIONS
           account_type_not_supported = 1
           OTHERS                     = 2.
       gs_openitems-due_date = gs_faede-netdt.
       gs_openitems-due_cashd1 = gs_faede-sk1dt.
       gs_openitems-due_cashd2 = gs_faede-sk2dt.
       CLEAR gs_bkpf.
       READ TABLE gt_bkpf INTO gs_bkpf WITH KEY bukrs = gs_lineitems-comp_code
                                                belnr = gs_lineitems-doc_no
                                                gjahr = gs_lineitems-fisc_year.
       IF sy-subrc IS INITIAL.
         gs_openitems-cpudt = gs_bkpf-cpudt.
         gs_openitems-cputm = gs_bkpf-cputm.
       ENDIF.
       APPEND gs_openitems TO pt_openitems.
       CLEAR : gs_lineitems, gs_openitems.
     ENDIF.
   ENDLOOP.

   SORT pt_openitems BY due_date ASCENDING cpudt ASCENDING cputm ASCENDING.

 ENDFORM.                    "determine_due_date
*&---------------------------------------------------------------------*
*&      Form  process_documents
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM process_documents CHANGING pt_openitems LIKE gt_openitems.

   DATA: lv_index           TYPE sy-tabix,
         lv_index1          TYPE sy-tabix,
         lv_count_s         TYPE i,
         lv_count_h         TYPE i,
         lv_count           TYPE i,
         lv_root_doc        TYPE belnr_d,
         lv_root_year       TYPE gjahr,
         lv_root_item       TYPE buzei,
         lv_due_date        TYPE netdt,
         lv_disc            TYPE dzproz,
         lv_disc_amnt       TYPE wrbtr,
         lv_prev            TYPE char1,
         lv_rootdocyear(17) TYPE c,
         lv_cpudt TYPE cpudt,                "Entry Date
         lv_cputm TYPE cputm.                "Entry time

   PERFORM progress_indicator USING 'Processing open items' 'for Customer:' gs_tab-kunnr 'is in progress'.

   CLEAR: gs_openitems, gs_openitems1.

   LOOP AT pt_openitems INTO gs_openitems.
     lv_index = sy-tabix.
     IF gs_openitems-inv_ref IS NOT INITIAL.
*       "means this doc can be linked to previous invoice in different
*       ways , such as partial residual etc

       READ TABLE pt_openitems INTO gs_openitems1 WITH KEY  doc_no = gs_openitems-inv_ref
                                                            fisc_year = gs_openitems-inv_year
                                                            item_num  = gs_openitems-inv_item
                                                            .
       IF sy-subrc IS INITIAL.
         lv_index1 = sy-tabix. "lv_index1 is the index of parent doc
         gs_openitems1-root_doc = gs_openitems1-doc_no.  "original incoice will be called root doc
         gs_openitems1-root_year = gs_openitems1-fisc_year. "original incoice year will be called root year
         gs_openitems1-root_item = gs_openitems1-item_num. "original incoice item will be called root item
         IF gs_openitems1-doc_type = 'DZ'.
* parent/root doc is residual doc
           gs_openitems1-previous = gc_r.
         ELSEIF gs_openitems1-doc_type = 'DG' OR gs_openitems1-doc_type = 'VG'.
           gs_openitems1-previous = gc_c.   "Credit note
         ELSEIF gs_openitems1-doc_type = 'KR'.
           READ TABLE gt_cusven WITH KEY kunnr = gs_tab-kunnr TRANSPORTING NO FIELDS.
           IF sy-subrc IS NOT INITIAL.
             DELETE pt_openitems INDEX lv_index.
             CONTINUE.
           ELSE.
             gs_openitems1-previous = gc_k."customer also vendor, KR docshould consider similar to credit note
           ENDIF.
         ELSE.
* this is a partial doc done for one of the previous invoices
           gs_openitems1-previous = gc_p.
         ENDIF.
         CONCATENATE  gs_openitems1-root_year gs_openitems1-root_doc gs_openitems1-root_item
                                       INTO gs_openitems1-rootdocyear.

* root doc is assigned for this partial type of document
         MODIFY pt_openitems FROM gs_openitems1 INDEX lv_index
                                        TRANSPORTING previous root_doc root_year root_item rootdocyear.
         IF gs_openitems1-root_doc IS INITIAL.
* root doc is assigned same as that of doc number
           MODIFY pt_openitems FROM gs_openitems1 INDEX lv_index1
                                       TRANSPORTING previous root_doc root_year root_item rootdocyear.
         ENDIF.
       ELSE.   " this doc's parent is not available in the available open items
         IF gs_openitems-db_cr_ind  = 'H'.
           IF gs_openitems-doc_type = 'DG' OR gs_openitems-doc_type = 'VG'.
             gs_openitems-previous = gc_c.    " for Credit note case
           ELSEIF gs_openitems-doc_type = 'KR'. "customer also vendor, should consider similar to credit note
             READ TABLE gt_cusven WITH KEY kunnr = gs_tab-kunnr TRANSPORTING NO FIELDS.
             IF sy-subrc IS NOT INITIAL.
               DELETE pt_openitems INDEX lv_index.
               CONTINUE.
             ELSE.
               gs_openitems-previous = gc_k.
             ENDIF.
           ELSE.
* these can be partial doc's, whose parent invoice is not available, so consider them as credit note
             gs_openitems-previous = gc_c.    " for Credit note case
           ENDIF.
         ELSE.                      " for previous residual payment
           gs_openitems-previous = gc_r.
         ENDIF.
         gs_openitems-root_doc = gs_openitems-doc_no.
         gs_openitems-root_year = gs_openitems-fisc_year.
         gs_openitems-root_item = gs_openitems-item_num.
         CONCATENATE  gs_openitems-root_year gs_openitems-root_doc gs_openitems-root_item
                                       INTO gs_openitems-rootdocyear.
         MODIFY pt_openitems FROM gs_openitems INDEX lv_index
                                        TRANSPORTING previous root_doc root_year root_item rootdocyear.
       ENDIF.
     ELSE.  " if inv_ref is not there for this open item, this can be a direct invoice to process
       IF gs_openitems-doc_type = 'DG' OR gs_openitems-doc_type = 'VG'.
         gs_openitems-previous = gc_c.    " for Credit note
       ELSEIF gs_openitems-doc_type = 'DZ' AND gs_openitems-ref_doc_no = 'EXCESS PAYMENT'.
         gs_openitems-previous = gc_c.    " for Credit note
       ELSEIF gs_openitems-doc_type = 'KR'. " customer as  a  vendor scenario, considered as a credit note
         READ TABLE gt_cusven WITH KEY kunnr = gs_tab-kunnr TRANSPORTING NO FIELDS.
         IF sy-subrc IS NOT INITIAL.
           DELETE pt_openitems INDEX lv_index.
           CONTINUE.
         ELSE.
           gs_openitems-previous = gc_k.
         ENDIF.
       ELSE.
         IF gs_openitems-db_cr_ind  = 'H'.
           gs_openitems-previous = gc_c.    " for Credit note
         ELSE.
           gs_openitems-previous = gc_n.  " plain invoice
         ENDIF.
       ENDIF.
* all root values are assigned with same documet no's
       gs_openitems-root_doc = gs_openitems-doc_no.
       gs_openitems-root_year = gs_openitems-fisc_year.
       gs_openitems-root_item = gs_openitems-item_num.
       CONCATENATE  gs_openitems-root_year gs_openitems-root_doc gs_openitems-root_item
                                     INTO gs_openitems-rootdocyear.
       MODIFY pt_openitems FROM gs_openitems INDEX lv_index
                                      TRANSPORTING previous root_doc root_year root_item rootdocyear.
     ENDIF.
     CLEAR : gs_openitems, gs_openitems1, lv_index, lv_index1.
   ENDLOOP.
*
* sort in ascending order of year and date
   CLEAR: gs_openitems, gs_openitems1.
   SORT pt_openitems BY  root_year root_doc  pstng_date ASCENDING item_num ASCENDING.
   LOOP AT pt_openitems INTO gs_openitems .
     lv_index = sy-tabix.
* docyear
     CONCATENATE  gs_openitems-fisc_year gs_openitems-doc_no gs_openitems-item_num
                                   INTO gs_openitems-docyear.
* docyear updation
     MODIFY pt_openitems FROM gs_openitems INDEX lv_index
                                    TRANSPORTING docyear.
* due date is made same for all docs with same root doc
     CLEAR gs_openitems1.
     READ TABLE pt_openitems INTO gs_openitems1 WITH KEY doc_no = gs_openitems-root_doc
                                                         fisc_year = gs_openitems-root_year
                                                         item_num = gs_openitems-root_item.
     IF sy-subrc IS INITIAL.
       gs_openitems-due_date = gs_openitems1-due_date.
       MODIFY pt_openitems FROM gs_openitems INDEX lv_index TRANSPORTING due_date.
     ENDIF.

*  move the credit notes to  internal table 'gt_openitemsc'.
     IF gs_openitems-doc_no EQ gs_openitems-root_doc AND
        gs_openitems-fisc_year EQ gs_openitems-root_year AND
        ( gs_openitems-previous = gc_c OR gs_openitems-previous = gc_k ).
       APPEND gs_openitems TO gt_openitemsc.
       DELETE pt_openitems INDEX lv_index.
       CONTINUE.
     ENDIF.
     CLEAR gs_openitems.
   ENDLOOP.

   CHECK pt_openitems[] IS NOT INITIAL.
* delete normal credit notes in case of KR doc scenario where these KR docs will be cleared using F-32, then fresh open items are pulled and
* FB05 is processed.
   IF gt_cusven[] IS NOT INITIAL.
     READ TABLE gt_openitemsc WITH KEY previous = gc_k TRANSPORTING  NO FIELDS.
     IF sy-subrc IS INITIAL.
       DELETE gt_openitemsc WHERE previous = gc_c.
     ENDIF.
   ENDIF.

   SORT gt_openitemsc BY due_date cpudt ASCENDING cputm ASCENDING.

   SORT pt_openitems BY  root_year root_doc due_date DESCENDING cpudt ASCENDING cputm ASCENDING item_num DESCENDING.

   CLEAR: gs_openitems,  gs_amnt, lv_root_doc, lv_root_year, lv_due_date, lv_rootdocyear.
   LOOP AT pt_openitems INTO gs_openitems.
     lv_index = sy-tabix.
* when ever root doc changes, these are grouped to  internal table 'gt_amnt'
     IF sy-tabix NE 1 AND lv_rootdocyear NE gs_openitems-rootdocyear.
       gs_amnt-root_doc  = lv_root_doc.
       gs_amnt-root_year = lv_root_year.
       gs_amnt-root_item = lv_root_item.
       gs_amnt-due_date =  lv_due_date .
       gs_amnt-count_s = lv_count_s.
       gs_amnt-count_h = lv_count_h.
       gs_amnt-count = lv_count.
       gs_amnt-prev = lv_prev.
       gs_amnt-disc = lv_disc.
       gs_amnt-disc_amnt = lv_disc_amnt.
       gs_amnt-net_amnt = gs_amnt-net_amnt - gs_amnt-disc_amnt.
       gs_amnt-rootdocyear = lv_rootdocyear.
       gs_amnt-cpudt =  lv_cpudt.
       gs_amnt-cputm =  lv_cputm.
       APPEND gs_amnt TO gt_amnt.
       CLEAR: gs_amnt, lv_count_s, lv_count, lv_due_date, lv_prev, lv_disc, lv_disc_amnt.
     ENDIF.
     lv_rootdocyear = gs_openitems-rootdocyear.
     lv_root_doc  = gs_openitems-root_doc.
     lv_root_year = gs_openitems-root_year.
     lv_root_item = gs_openitems-root_item.

     IF gs_openitems-root_doc = gs_openitems-doc_no AND gs_openitems-root_year = gs_openitems-fisc_year
                                                    AND gs_openitems-root_item = gs_openitems-item_num .
       IF gs_openitems-pmnttrms IS NOT INITIAL.
         CLEAR : gs_amnt-disc, gs_amnt-disc_amnt,
                 lv_disc, lv_disc_amnt.
* calculate discount amount based on payment terms for the original invoice
         PERFORM discount_calc USING gs_openitems
                                CHANGING lv_disc lv_disc_amnt.
       ENDIF.
       lv_cpudt = gs_openitems-cpudt.
       lv_cputm = gs_openitems-cputm.
     ENDIF.
* if the open item is not a stand alone doc
     IF gs_openitems-root_doc NE gs_openitems-doc_no.
       IF gs_openitems-previous NE gc_r.
         lv_prev = gc_p.
       ELSE.
         lv_prev = gc_r.
       ENDIF.
     ENDIF.

     IF lv_due_date IS INITIAL.
       lv_due_date = gs_openitems-due_date.
     ELSEIF lv_due_date GT gs_openitems-due_date.
       lv_due_date = gs_openitems-due_date.
     ENDIF.
* calculate net amount payable for the all the docs with same root document
* based on debit credit indicator
     IF gs_openitems-db_cr_ind  = 'S'.
       gs_amnt-net_amnt = gs_amnt-net_amnt + gs_openitems-lc_amount."#EC CI_FLDEXT_OK[2610650]
         "Added by SPLABAP during code remediation
       gs_amnt-root_amnt = gs_amnt-root_amnt + gs_amnt-net_amnt.
       lv_count_s = lv_count_s + 1.
       lv_count = lv_count + 1.
     ELSE.
       gs_amnt-net_amnt = gs_amnt-net_amnt - gs_openitems-lc_amount."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
       lv_count_h = lv_count_h + 1.
       lv_count = lv_count + 1.
     ENDIF.
   ENDLOOP.
* last doc is appened because of exit from the loop
   gs_amnt-root_doc  = gs_openitems-root_doc.
   gs_amnt-root_year = gs_openitems-root_year.
   gs_amnt-root_item = gs_openitems-root_item.
   gs_amnt-due_date =  lv_due_date .
   gs_amnt-count_s = lv_count_s.
   gs_amnt-count_h = lv_count_h.
   gs_amnt-count = lv_count.
   gs_amnt-prev = lv_prev.
   gs_amnt-disc = lv_disc.
   gs_amnt-disc_amnt = lv_disc_amnt.
   gs_amnt-net_amnt = gs_amnt-net_amnt - gs_amnt-disc_amnt.
   gs_amnt-rootdocyear = lv_rootdocyear.
   gs_amnt-cpudt =  lv_cpudt.
   gs_amnt-cputm =  lv_cputm.
   APPEND gs_amnt TO gt_amnt.
   CLEAR: gs_amnt, lv_count_s, lv_count, lv_due_date, lv_prev, lv_disc, lv_disc_amnt.
* FIFO process, oldest doc needs to be processed first.
   SORT gt_amnt BY due_date  ASCENDING cpudt ASCENDING cputm ASCENDING root_year DESCENDING root_doc DESCENDING root_item.
* sort open items back to ascending
   SORT pt_openitems BY  root_year root_doc root_item due_date ASCENDING cpudt ASCENDING cputm ASCENDING item_num ASCENDING.

   DELETE gt_amnt WHERE net_amnt LT 0.

 ENDFORM.                    "process_documents

*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM create_container .

   IF gv_container IS INITIAL.
     CREATE OBJECT gv_container
       EXPORTING
*        parent                      =
         container_name              = 'CONTAINER'
       EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5
         OTHERS                      = 6.
     IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
   ENDIF.

 ENDFORM.                    "create_container
*&---------------------------------------------------------------------*
*&      Form  ALV_OBJECT_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM alv_object_create .

   IF gv_alv IS INITIAL.
     CREATE OBJECT gv_alv
       EXPORTING
*        i_shellstyle      = 0
*        i_lifetime        =
         i_parent          = gv_container
       EXCEPTIONS
         error_cntl_create = 1
         error_cntl_init   = 2
         error_cntl_link   = 3
         error_dp_create   = 4
         OTHERS            = 5.
     IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
   ENDIF.

 ENDFORM.                    "alv_object_create
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM build_fcat .

   FREE gt_fcat.
   CLEAR gs_message.
   LOOP AT gt_message INTO gs_message.
     IF gs_message-mess_type = 'E'.
       gs_message-status =  '@0A@'.
     ELSEIF gs_message-mess_type = 'W'.
       gs_message-status =  '@09@'.
     ELSEIF gs_message-mess_type = 'S'.
       gs_message-status = '@08@'.
     ENDIF.
     MODIFY gt_message FROM gs_message.
   ENDLOOP.

   gs_fcat-fieldname = 'STATUS'.
   gs_fcat-scrtext_s = 'Status'.
   gs_fcat-outputlen = 6.
   gs_fcat-just = 'C'.
   APPEND gs_fcat TO gt_fcat.
   CLEAR gs_fcat.
   gs_fcat-scrtext_s = 'Customer'.
   gs_fcat-fieldname = 'KUNNR'.
   gs_fcat-outputlen = 12.
   APPEND gs_fcat TO gt_fcat.
   CLEAR gs_fcat.
   gs_fcat-scrtext_s = 'Name'.
   gs_fcat-fieldname = 'NAME1'.
   gs_fcat-outputlen = 35.
   APPEND gs_fcat TO gt_fcat.
   CLEAR gs_fcat.
*   gs_fcat-fieldname = 'WRBTR'.
*   gs_fcat-scrtext_m = 'Amount'.
*   APPEND gs_fcat TO gt_fcat.
*   CLEAR gs_fcat.
   gs_fcat-fieldname = 'BELNR'.
   gs_fcat-scrtext_s = 'Bill No'.
   APPEND gs_fcat TO gt_fcat.
   CLEAR gs_fcat.
   gs_fcat-fieldname = 'MESSAGE'.
   gs_fcat-scrtext_l = 'Message'.
   gs_fcat-outputlen = 55.
   APPEND gs_fcat TO gt_fcat.
   CLEAR gs_fcat.

 ENDFORM.                    "build_fcat
*&---------------------------------------------------------------------*
*&      Form  SET_TABLE_FOR_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM set_table_for_display .

   CALL METHOD gv_alv->set_table_for_first_display
*     EXPORTING
*      i_buffer_active =
*      i_bypassing_buffer            =
*      i_consistency_check           =
*      i_structure_name              =
*      is_variant      =
*      i_save          =
*      i_default       = 'X'
*       is_layout       = gs_layout
*      is_print        =
*      it_special_groups             =
*      it_toolbar_excluding          =
*      it_hyperlink    =
*      it_alv_graphics =
*      it_except_qinfo =
*      ir_salv_adapter =
     CHANGING
       it_outtab       = gt_message
       it_fieldcatalog = gt_fcat
*      it_sort         =
*      it_filter       =
*            EXCEPTIONS
*      invalid_parameter_combination = 1
*      program_error   = 2
*      too_many_lines  = 3
*      others          = 4
     .
   IF sy-subrc <> 0.
*           Implement suitable error handling here
   ENDIF.

 ENDFORM.                    "set_table_for_display
*&---------------------------------------------------------------------*
*&      Form  CONTAINER_FREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM container_free CHANGING p_container TYPE REF TO cl_gui_custom_container.

   IF NOT p_container IS INITIAL.

     CALL METHOD p_container->free
       EXCEPTIONS
         cntl_error        = 1
         cntl_system_error = 2
         OTHERS            = 3.
     IF sy-subrc <> 0.
* Implement suitable error handling here
     ENDIF.

   ENDIF.

 ENDFORM.                    "container_free
*&---------------------------------------------------------------------*
*&      Form  FB05_BDC
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM fb05_bdc CHANGING p_lv_check TYPE any.

   FREE: messtab[], gs_messtab, gt_bdc[].
   CLEAR: p_lv_check, gs_bdc.
   DATA: lv_mvar1      TYPE balm-msgv1,
         lv_mvar2      TYPE balm-msgv1,
         lv_mvar3      TYPE balm-msgv1,
         lv_mvar4      TYPE balm-msgv1,
         lv_okcode(5)  TYPE c,
         lv_cursor(15) TYPE c,
         lv_cnt(2)     TYPE n,
         lv_f          TYPE p DECIMALS 2,
         lv_index      TYPE sy-tabix,
         lv_lines      TYPE sy-tabix,
         lv_abpos      TYPE posnr,
         lv_last       TYPE sy-tabix,
         lv_ceil       TYPE sy-tabix,
         lv_floor      TYPE sy-tabix,
         lv_exit       TYPE sy-tabix.

   CALL FUNCTION 'BPAR_P_FI_CUSTOMER_DEQUEUE'
     EXPORTING
       customer       = gs_tab-kunnr
     EXCEPTIONS
       system_failure = 1
       OTHERS         = 2.
   IF sy-subrc <> 0.
* Implement suitable error handling here
   ENDIF.


* 1st screen
   PERFORM bdc_dynpro      USING 'SAPMF05A' '0122'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'RF05A-NEWKO'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '/00'.
   WRITE gs_header-bldat TO gv_value.
   PERFORM bdc_field       USING 'BKPF-BLDAT' "document date
                                  gv_value.
   PERFORM bdc_field       USING 'BKPF-BLART'
                                 'DZ'.
   PERFORM bdc_field       USING 'BKPF-BUKRS'
                                 gs_header-bukrs.
   WRITE gs_header-budat TO gv_value.
   PERFORM bdc_field       USING 'BKPF-BUDAT' "Posting Date
                                  gv_value.
   PERFORM bdc_field       USING 'BKPF-MONAT'  "posting period
                                 gs_header-monat.
   PERFORM bdc_field       USING 'BKPF-WAERS'
                                 gs_header-waers.
   PERFORM bdc_field       USING 'BKPF-XBLNR'
                                 gs_header-xblnr.
   PERFORM bdc_field       USING 'BKPF-BKTXT'
                                 gs_header-bktxt.
*   CONCATENATE 'Payment from Customer:' gs_tab-kunnr
*   INTO  gs_header-augtx SEPARATED BY space.
   PERFORM bdc_field       USING 'RF05A-AUGTX'
                                 GS_HEADER-XTEXT. "modified on 14/3

   PERFORM bdc_field       USING 'FS006-DOCID'
                                  '*'.
   PERFORM bdc_field       USING 'RF05A-NEWBS'
                                 '40'.
   PERFORM bdc_field       USING 'RF05A-NEWKO'
                                 gs_header-newko.
* 2nd  Second screen
   PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'BSEG-WRBTR'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=PA'.

   WRITE gs_tab-wrbtr TO gv_value.                      "#EC UOM_IN_MES
   CONDENSE gv_value.

   PERFORM bdc_field       USING 'BSEG-WRBTR'
                                 gv_value.
   CLEAR gv_value.
  " CONCATENATE 'Payment from Customer:' gs_tab-kunnr INTO gv_value SEPARATED BY space.
   "gv_value = gs_header-xtext .
   "CONDENSE gv_value.

   PERFORM bdc_field       USING 'BSEG-SGTXT'
                                 gs_header-xtext. "added on 14/3
* subscreen (pop up)
   PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.

   PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'COBL-PRCTR'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=ENTE'.
* 3rd Third screen

   PERFORM bdc_dynpro      USING 'SAPMF05A' '0710'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'RF05A-AGKON'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=SLB'.
   PERFORM bdc_field       USING 'RF05A-AGBUK'
                                 gs_header-bukrs.
   PERFORM bdc_field       USING 'RF05A-AGKON'
                                 gs_tab-kunnr.
   PERFORM bdc_field       USING 'RF05A-AGKOA'
                                 'D'.
   PERFORM bdc_field       USING 'RF05A-XNOPS'
                                 'X'.
   PERFORM bdc_field       USING 'RF05A-XFIFO'
                                 ''.
   PERFORM bdc_field       USING 'RF05A-XPOS1(01)'
                                 'X'.

* 4th screen

   CLEAR: gs_bdc, gs_amnt, gs_openitems, lv_lines.
   DESCRIBE TABLE gt_amnt LINES lv_last.

*
   CLEAR gs_amnt.
   READ TABLE gt_amnt INTO gs_amnt INDEX lv_last.
   " This is the line where partial adjustment can be done in bdc via clicking
   DELETE gt_amnt INDEX lv_last.
   INSERT gs_amnt INTO gt_amnt INDEX 1.
*
   LOOP AT gt_amnt INTO gs_amnt.
     lv_index = sy-tabix.

     gs_bdc-fval1 = 'BELNR'.

     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
         input  = gs_amnt-root_doc
       IMPORTING
         output = gs_amnt-root_doc.

     CONCATENATE gs_amnt-root_doc gs_amnt-root_year gs_amnt-root_item INTO gs_bdc-fval2.
     lv_lines = lv_lines + 1.

     APPEND gs_bdc TO gt_bdc.

     CLEAR gs_amnt.
   ENDLOOP.

   LOOP AT gt_amnt INTO gs_amnt.
* because 1st record is the one where we double click in BDC to set the incoming amount.
     IF sy-tabix = 1  AND  gv_partial = 'X'.
       " No Need to add the partials for the root doc in BDC processing
       CONTINUE.
     ENDIF.

     CLEAR: gs_bdc, gs_openitems.
     LOOP AT gt_openitems INTO gs_openitems WHERE rootdocyear = gs_amnt-rootdocyear
                                            AND   docyear NE gs_amnt-rootdocyear.

       gs_bdc-fval1 = 'BELNR'.

       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
           input  = gs_openitems-doc_no
         IMPORTING
           output = gs_openitems-doc_no.

       CONCATENATE gs_openitems-doc_no gs_openitems-fisc_year gs_openitems-item_num INTO gs_bdc-fval2.
       lv_lines = lv_lines + 1.
       APPEND gs_bdc TO gt_bdc.
       CLEAR: gs_bdc, gs_openitems.
     ENDLOOP.

     CLEAR : gs_amnt.
   ENDLOOP.


   CLEAR: lv_index, lv_last, gs_bdc.
   DESCRIBE TABLE gt_bdc LINES lv_last.
   lv_f = lv_last / 18.
   lv_ceil = ceil( lv_f ).
   lv_floor = floor( lv_f ).


   DO lv_ceil TIMES.

     PERFORM bdc_dynpro      USING 'SAPMF05A' '0733'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'RF05A-SEL01(01)'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=PA'.

     lv_index = lv_index + 1.
     lv_cnt = 0.
     DO 18 TIMES.
       lv_exit = lv_exit + 1.
       lv_cnt = lv_cnt + 1.
       CLEAR gs_bdc.
       READ TABLE gt_bdc INTO gs_bdc INDEX lv_exit.
       IF sy-subrc IS INITIAL.

         CONCATENATE 'RF05A-FELDN(' lv_cnt ')' INTO gs_bdc-fnam1.
         CONCATENATE 'RF05A-SEL01(' lv_cnt ')' INTO gs_bdc-fnam2.

         PERFORM bdc_field       USING gs_bdc-fnam1
                                       gs_bdc-fval1.
         PERFORM bdc_field       USING gs_bdc-fnam2
                                       gs_bdc-fval2.
       ENDIF.
       IF lv_exit = lv_last.
         EXIT.
       ENDIF.

     ENDDO.


* select more items
     IF lv_index NE lv_ceil.
       PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
       PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'DF05B-PSSKT(01)'.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '=SL'.
       PERFORM bdc_field       USING 'RF05A-ABPOS'
                                     '1'.
* next screen.

       PERFORM bdc_dynpro      USING 'SAPMF05A' '0710'.
       PERFORM bdc_field       USING 'BDC_CURSOR'
                                     'RF05A-AGKON'.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '=SLB'.
       PERFORM bdc_field       USING 'RF05A-AGBUK'
                                     gs_header-bukrs.
       PERFORM bdc_field       USING 'RF05A-AGKON'
                                     gs_tab-kunnr.
       PERFORM bdc_field       USING 'RF05A-AGKOA'
                                     'D'.
       PERFORM bdc_field       USING 'RF05A-XNOPS'
                                     'X'.
       PERFORM bdc_field       USING 'RF05A-XFIFO'
                                     ''.
       PERFORM bdc_field       USING 'RF05A-XPOS1(01)'
                                     'X'.
     ENDIF.
   ENDDO.

* 5th screen

   CLEAR: lv_okcode.

   lv_okcode = '=PART'.
   lv_cursor = 'DF05B-PSZAH(01)'.

   PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 lv_okcode.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                'DF05B-PSSKT(01)'.
   PERFORM bdc_field       USING 'RF05A-ABPOS'
                                 '1'.

   LOOP AT gt_amnt INTO gs_amnt.

     lv_abpos = sy-tabix.

     PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '/00'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   lv_cursor.
     PERFORM bdc_field       USING 'RF05A-ABPOS'
                                   lv_abpos.
     IF gs_amnt-disc_amnt GT 0 .
       PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '=Z+S'.
     ELSE.
       PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '=Z-S'.
     ENDIF.
   ENDLOOP.

   PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '/00'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 lv_cursor.
   PERFORM bdc_field       USING 'RF05A-ABPOS'
                                 '1'.

   IF gv_excess NE 'X'.
* for partial or exact payment.
     PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=PI'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'DF05B-PSZAH(01)'.
     PERFORM bdc_field       USING 'RF05A-ABPOS'
                                   '1'.

     PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=BU'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'DF05B-PSZAH(01)'.
     PERFORM bdc_field       USING 'RF05A-ABPOS'
                                   '1'.

   ELSE. " for excess payment
     PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=KMD'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'DF05B-PSZAH(01)'.
     PERFORM bdc_field       USING 'RF05A-ABPOS'
                                   '1'.

     PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'RF05A-NEWKO'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=BU'.
     PERFORM bdc_field       USING 'RF05A-NEWBS'
                                   '11'.
     PERFORM bdc_field       USING 'RF05A-NEWKO'
                                   gs_tab-kunnr.

     PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'BSEG-WRBTR'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '/00'.
     WRITE gv_wrbtr TO gv_value.                        "#EC UOM_IN_MES
     CONDENSE gv_value.
     PERFORM bdc_field       USING 'BSEG-WRBTR'
                                   gv_value.
     CLEAR gv_value.
     CONCATENATE 'Excess Payment from Customer:' gs_tab-kunnr
     INTO  gv_value SEPARATED BY space.
     PERFORM bdc_field       USING 'BSEG-SGTXT'
                                   gv_value.


     PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'BSEG-WRBTR'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=BU'.
   ENDIF.


   REFRESH messtab.
   CALL TRANSACTION 'FB05'
                    USING bdcdata
                    MODE   gv_mode
                    UPDATE 'A'
                    MESSAGES INTO messtab.

   WAIT UP TO 2 SECONDS.
   COMMIT WORK AND WAIT.

   CLEAR: gs_messtab, gs_message.

   READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'S'
                                               msgnr = '312'.
   IF sy-subrc IS INITIAL.
     COMMIT WORK AND WAIT.

     DATA: lv_belnr TYPE belnr_d.
     DO 10 TIMES.
       SELECT SINGLE belnr FROM bkpf INTO lv_belnr
                           WHERE belnr = gs_messtab-msgv1.
       IF sy-subrc IS NOT INITIAL.
         WAIT UP TO 2 SECONDS.
       ELSE.
         EXIT.
       ENDIF.
     ENDDO.

     LOOP AT gt_amnt INTO gs_amnt.
       CLEAR : gs_message-wrbtr, gs_message-belnr.
       gs_message-mess_type = 'S'.
       MOVE-CORRESPONDING gs_tab TO gs_message.
       gs_message-belnr = gs_amnt-root_doc.
       CONCATENATE 'Payment Document:' gs_messtab-msgv1 'Posted Successfully' INTO
                   gs_message-message  SEPARATED BY space.

       APPEND gs_message TO gt_message.
     ENDLOOP.
     PERFORM progress_indicator USING 'Posting and clearing' 'for Customer:' gs_tab-kunnr 'complete'.

   ELSE.

     READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'E'.
     IF sy-subrc = 0 .
       p_lv_check = 'E'.
       lv_mvar1 = gs_messtab-msgv1.
       lv_mvar2 = gs_messtab-msgv2.
       lv_mvar3 = gs_messtab-msgv3.
       lv_mvar4 = gs_messtab-msgv4.
       CALL FUNCTION 'MESSAGE_PREPARE'
         EXPORTING
           language               = sy-langu
           msg_id                 = gs_messtab-msgid
           msg_no                 = gs_messtab-msgnr
           msg_var1               = lv_mvar1
           msg_var2               = lv_mvar2
           msg_var3               = lv_mvar3
           msg_var4               = lv_mvar4
         IMPORTING
           msg_text               = gs_message-message
         EXCEPTIONS
           function_not_completed = 1
           message_not_found      = 2
           OTHERS                 = 3.
       gs_message-mess_type = 'E'.
       APPEND gs_message TO gt_message.
     ELSE.

       LOOP AT messtab INTO gs_messtab WHERE msgtyp = 'S' .
         lv_mvar1 = gs_messtab-msgv1.
         lv_mvar2 = gs_messtab-msgv2.
         lv_mvar3 = gs_messtab-msgv3.
         lv_mvar4 = gs_messtab-msgv4.
         CALL FUNCTION 'MESSAGE_PREPARE'
           EXPORTING
             language               = sy-langu
             msg_id                 = gs_messtab-msgid
             msg_no                 = gs_messtab-msgnr
             msg_var1               = lv_mvar1
             msg_var2               = lv_mvar2
             msg_var3               = lv_mvar3
             msg_var4               = lv_mvar4
           IMPORTING
             msg_text               = gs_message-message
           EXCEPTIONS
             function_not_completed = 1
             message_not_found      = 2
             OTHERS                 = 3.
         gs_message-mess_type = 'W'.
         APPEND gs_message TO gt_message.
         CLEAR gs_messtab.
       ENDLOOP.
     ENDIF.
   ENDIF.


   FREE :bdcdata[].
   CLEAR: gs_message.

 ENDFORM.                    "fb05_bdc
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
 FORM bdc_dynpro USING program dynpro.
   CLEAR bdcdata.
   bdcdata-program  = program.
   bdcdata-dynpro   = dynpro.
   bdcdata-dynbegin = 'X'.
   APPEND bdcdata.
 ENDFORM.                    "bdc_dynpro
*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
 FORM bdc_field USING fnam fval.

   CLEAR bdcdata.
   bdcdata-fnam = fnam.
   bdcdata-fval = fval.
   APPEND bdcdata.

 ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  ALV_FREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM alv_free .

   IF NOT gv_alv IS INITIAL.
     CALL METHOD gv_alv->free
       EXCEPTIONS
         cntl_error        = 1
         cntl_system_error = 2
         OTHERS            = 3.
     IF sy-subrc <> 0.
* Implement suitable error handling here
     ENDIF.
   ENDIF.

 ENDFORM.                    "alv_free
*&---------------------------------------------------------------------*
*&      Form  PAI_F4_NEWKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM pai_f4_newko .

   mcobjekt = 'SAKO'.

   CLEAR selstr.
   PERFORM dynp_values_read USING 'GS_HEADER-NEWKO' f4rc.
   CHECK f4rc = 0.
   READ TABLE f4hlp INDEX 1.


*------------------ Kurznotation eingegeben ? --------------------------
   IF f4hlp-fieldvalue(1) = '='.
     selstr = f4hlp-fieldvalue.
     TRANSLATE selstr TO UPPER CASE.                     "#EC TRANSLANG
   ENDIF.

*----------------------- Hilfedialog aufrufen --------------------------
   PERFORM hlp_f4 USING mcobjekt 'GS_HEADER-NEWKO'.

 ENDFORM.                    "pai_f4_newko
*&---------------------------------------------------------------------*
*&      Form  DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM dynp_values_read  USING
      fname LIKE dynpread-fieldname
      dv_rc LIKE sy-subrc.
   REFRESH f4hlp.
   CLEAR   f4hlp.
   f4hlp-fieldname = fname.
   APPEND f4hlp.

*------- Nicht SY-DYNNR, SY-REPID direkt verwenden, gibt Probleme ------
   f4dyn = cl_fin_ui_deco=>sydynnr( ).

   CALL FUNCTION 'DYNP_VALUES_READ'
     EXPORTING
       dyname               = 'SAPMZ_FI_CLEAR_PAYMENT_NEW'
       dynumb               = f4dyn
       determine_loop_index = 'X'
     TABLES
       dynpfields           = f4hlp
     EXCEPTIONS
       OTHERS               = 01.
   dv_rc = sy-subrc.
 ENDFORM.                    "dynp_values_read
*&---------------------------------------------------------------------*
*&      Form  HLP_F4
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM hlp_f4  USING value(sh) TYPE c
            value(feldname) TYPE c.
   DATA: nachname LIKE dd03v-fieldname.
   DATA: koart LIKE tbsl-koart.

*------------ Aus RF05A-XXXXX mache XXXXX ------------------------------
   nachname = feldname.
   SHIFT nachname BY 10 PLACES.
   koart = 'S'.

   shlpname = sh.
   CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
     EXPORTING
       shlpname = shlpname
     IMPORTING
       shlp     = shlp_mf05a
     EXCEPTIONS
       OTHERS   = 1.

   PERFORM sh_interface USING 'SAKNR'
                              feldname f4hlp-fieldvalue.
   PERFORM sh_interface USING 'SAKAN'
                              feldname f4hlp-fieldvalue.

   PERFORM sh_interface USING 'BUKRS' space gs_header-bukrs.

   REFRESH return_values.

   CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
     EXPORTING
       shlp          = shlp_mf05a
     TABLES
       return_values = return_values
     EXCEPTIONS
       OTHERS        = 1.

   PERFORM dynp_values_update USING feldname.

 ENDFORM.                    "hlp_f4
*&---------------------------------------------------------------------*
*&      Form  SH_INTERFACE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*       Modify einer Zeile der Tabelle  INTERFACE:
*       VALFIELD:  Dynprofeldname, FIELDVALUE: dessen Inhalt
*       SHLPFIELD: Feld im Suchhilfe-Interface
*----------------------------------------------------------------------*
 FORM sh_interface  USING value(shlpfield) TYPE c
            value(valfield) TYPE c
            value(fieldvalue) TYPE c.
   READ TABLE shlp_mf05a-interface WITH KEY shlpfield = shlpfield
                                   INTO interface.
   IF sy-subrc EQ 0.
     interface-valfield = valfield.
     interface-value    = fieldvalue.
     MODIFY shlp_mf05a-interface INDEX sy-tabix FROM interface.
   ENDIF.

 ENDFORM.                    "sh_interface
*&---------------------------------------------------------------------*
*&      Form  DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM dynp_values_update  USING value(fieldname) TYPE c.
   DATA: lv_sydynnr TYPE sydynnr. "UI-Decoupling
   CLEAR f4hlp.
   REFRESH f4hlp.
   READ TABLE return_values INDEX 1.
   IF sy-subrc = 0.
     f4hlp-fieldname  = fieldname.
     f4hlp-fieldvalue = return_values-fieldval.

     APPEND f4hlp.
     lv_sydynnr = cl_fin_ui_deco=>sydynnr( ).
     CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
         dyname     = 'SAPMZ_FI_INCOMING_PAYMENT_NEW'
         dynumb     = lv_sydynnr
       TABLES
         dynpfields = f4hlp
       EXCEPTIONS
         OTHERS     = 8.
   ENDIF.

 ENDFORM.                    "dynp_values_update
*&---------------------------------------------------------------------*
*&      Form  DISCOUNT_CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_OPENITEMS  text
*      <--P_GS_AMNT_DISC  text
*      <--P_GS_AMNT_DISC_AMNT  text
*----------------------------------------------------------------------*
 FORM discount_calc  USING    ps_openitems TYPE ty_openitems
                     CHANGING p_disc TYPE dzproz
                              p_disc_amnt TYPE wrbtr.

   CLEAR : p_disc, p_disc_amnt.
* entered posting date is checked w.r.t due dates
   IF gs_header-bldat LE ps_openitems-due_cashd1.
     p_disc = ps_openitems-dsct_pct1.
   ELSEIF gs_header-bldat LE ps_openitems-due_cashd2.
     p_disc = ps_openitems-dsct_pct2.
   ENDIF.
   p_disc_amnt = ps_openitems-disc_base * p_disc / 100."#EC CI_FLDEXT_OK[2610650]
   "Added by SPLABAP during code remediation

 ENDFORM.                    " DISCOUNT_CALC
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM progress_indicator  USING p_desc1 p_desc2 p_kunnr p_desc3 .

   DATA: w_text(60).

   CONCATENATE  p_desc1 p_desc2 p_kunnr p_desc3 INTO w_text SEPARATED BY space.
* This check needs to be in otherwise when looping around big tables
* SAP will re-display indicator too many times causing report to run
* very slow. (No need to re-display same percentage anyway)

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
*      percentage = w_percentage
       text       = w_text.
*     gd_percent = w_percentage.


 ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  FILL_CUSTOMER_NAME
*&---------------------------------------------------------------------*
 FORM fill_customer_name .

   DATA: lt_message TYPE STANDARD TABLE OF ty_message,
         lv_index TYPE sy-tabix.

   FREE lt_message[].
   lt_message[] = gt_message[].
   SORT lt_message BY kunnr.
   DELETE ADJACENT DUPLICATES FROM lt_message COMPARING kunnr.

   CHECK lt_message[] IS NOT INITIAL.
   SELECT kunnr name1 FROM kna1 INTO TABLE gt_kna1
                      FOR ALL ENTRIES IN lt_message
                      WHERE kunnr = lt_message-kunnr.

   LOOP AT gt_message INTO gs_message.
     lv_index = sy-tabix.
     CLEAR gs_kna1.
     READ TABLE gt_kna1 INTO gs_kna1 WITH KEY kunnr = gs_message-kunnr.
     IF sy-subrc IS INITIAL.
       gs_message-name1 = gs_kna1-name1.
       MODIFY gt_message FROM gs_message INDEX lv_index TRANSPORTING name1.
     ENDIF.
     CLEAR gs_message.
   ENDLOOP.

 ENDFORM.                    " FILL_CUSTOMER_NAME
*&---------------------------------------------------------------------*
*&      Form  CHECK_CREDITNOTES
*----------------------------------------------------------------------*
 FORM check_creditnotes.

   DATA: lv_index TYPE sy-tabix,
         lv_wrbtr TYPE wrbtr,
         lv_hwrbtr TYPE wrbtr. "high amount

   CLEAR gv_wrbtro.
* calculate open items amount
   LOOP AT gt_amnt INTO gs_amnt.
     gv_wrbtro = gv_wrbtro + gs_amnt-net_amnt.
     CLEAR gs_amnt.
   ENDLOOP.
   CLEAR gv_wrbtrc.
*calculate credit notes amount.
* delete credit note with amounts greater than (total of outstanding invoices )
   DELETE gt_openitemsc WHERE lc_amount GT gv_wrbtro."#EC CI_FLDEXT_OK[2610650]
   "Added by SPLABAP during code remediation

   IF gt_openitems[] IS INITIAL.
     CLEAR gv_wrbtrc.
     EXIT.
   ENDIF.

   LOOP AT gt_openitemsc INTO gs_openitems.
     lv_index = sy-tabix.
     IF gv_wrbtrc GT gv_wrbtro.
       DELETE gt_openitemsc INDEX lv_index.
     ELSE.
       gv_wrbtrc = gv_wrbtrc + gs_openitems-lc_amount."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
     ENDIF.
     CLEAR gs_openitems.
   ENDLOOP.

   IF gt_openitems[] IS INITIAL.
     CLEAR gv_wrbtrc.
     EXIT.
   ENDIF.

   CLEAR: lv_wrbtr, lv_index.
* if the total credit note amount is greater than existing invoice's amount
*   then delete appropriate credit notes from the internal table for future invoices as and when they come
   IF gv_wrbtrc GT gv_wrbtro.
     LOOP AT gt_openitemsc INTO gs_openitems.
       lv_wrbtr = lv_wrbtr + gs_openitems-lc_amount."#EC CI_FLDEXT_OK[2610650]
       "Added by SPLABAP during code remediation
       lv_index = sy-tabix.
       gs_openitems-cnote = 'X'.
* this record needs to processed for  credit note related  logic
       IF gv_wrbtro GE lv_wrbtr.
         MODIFY gt_openitemsc FROM gs_openitems INDEX lv_index TRANSPORTING cnote.
       ELSE.
         EXIT.
       ENDIF.
       CLEAR gs_openitems.
     ENDLOOP.
*then delete appropriate credit notes from the internal table for future invoices as and when they come
     DELETE gt_openitemsc WHERE cnote NE 'X'.
   ENDIF.

   CLEAR: lv_wrbtr, lv_index.
* check no.of records to be processed for the available credit notes
   LOOP AT gt_amnt INTO gs_amnt.
     lv_wrbtr = lv_wrbtr + gs_amnt-net_amnt.
     lv_index = sy-tabix.
* high priced of all the net amount is captured in lv_hwrbtr
     IF lv_hwrbtr LE gs_amnt-net_amnt.
       lv_hwrbtr = gs_amnt-net_amnt.
     ENDIF.
     gs_amnt-cnote = 'X'.
* this record needs to processed for  credit note related  logic
     MODIFY gt_amnt FROM gs_amnt INDEX lv_index TRANSPORTING cnote.
     IF gv_wrbtrc EQ lv_wrbtr.
       CLEAR gv_wrbtr1.
       EXIT.
     ENDIF.
     IF gv_wrbtrc LT lv_wrbtr.          " total credit note amount less than cumulative of open items amount
       IF gt_lineitemsv[] IS INITIAL.           "No vendor payments to be considered for customer as a vendor
         IF gv_wrbtr LT ( lv_wrbtr - gv_wrbtrc ). " if entered amount less than cumulatives of all amounts
*                                                 related to same root doc and available credit note amounts
           gv_wrbtr1 = gv_wrbtr.  " needs to be populated in BDC
           gv_partial = 'X'.  " indicator used in BDC
           gv_pwrbtr = lv_hwrbtr - ( lv_wrbtr - gv_wrbtrc ) + gv_wrbtr.
*           IF gs_tab-residual = 'X'.  " if selected in selection screen
*             gv_resid = 'X'. " indicator used in BDC
*           ENDIF.
         ELSE.
           gv_wrbtr1 = lv_wrbtr - gv_wrbtrc.  "if entered amount is greater than
*                                             ( diff between open and credit notes)
           gv_pwrbtr = lv_hwrbtr.
         ENDIF.
         EXIT.
       ELSE.
         EXIT.
       ENDIF.
     ENDIF.
     CLEAR gs_amnt.
   ENDLOOP.

 ENDFORM.                    " CHECK_CREDITNOTES
*&---------------------------------------------------------------------*
*&      Form  FB05_BDC_EXCESS
*----------------------------------------------------------------------*
 FORM fb05_bdc_excess  CHANGING p_lv_check.

   PERFORM progress_indicator USING 'Excess payment being posted' 'for Customer:' gs_tab-kunnr 'is in progress'.

   FREE: messtab[], gs_messtab.
   CLEAR: p_lv_check.
   DATA: lv_mvar1      TYPE balm-msgv1,
         lv_mvar2      TYPE balm-msgv1,
         lv_mvar3      TYPE balm-msgv1,
         lv_mvar4      TYPE balm-msgv1.

   CALL FUNCTION 'BPAR_P_FI_CUSTOMER_DEQUEUE'
     EXPORTING
       customer       = gs_tab-kunnr
     EXCEPTIONS
       system_failure = 1
       OTHERS         = 2.
   IF sy-subrc <> 0.
* Implement suitable error handling here
   ENDIF.

* 1st screen
   PERFORM bdc_dynpro      USING 'SAPMF05A' '0122'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'RF05A-NEWKO'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '/00'.
   CLEAR gv_value.
   WRITE gs_header-bldat TO gv_value.
   PERFORM bdc_field       USING 'BKPF-BLDAT' "document date
                                  gv_value.
   PERFORM bdc_field       USING 'BKPF-BLART'
                                 'DZ'.
   PERFORM bdc_field       USING 'BKPF-BUKRS'
                                 gs_header-bukrs.
   CLEAR gv_value.
   WRITE gs_header-budat TO gv_value.
   PERFORM bdc_field       USING 'BKPF-BUDAT' "Posting Date
                                  gv_value.
   PERFORM bdc_field       USING 'BKPF-MONAT'  "posting period
                                 gs_header-monat.
   PERFORM bdc_field       USING 'BKPF-WAERS'
                                 gs_header-waers.
   gs_header-xblnr = 'EXCESS PAYMENT'.
   PERFORM bdc_field       USING 'BKPF-XBLNR'
                                 gs_header-xblnr.
   gs_header-bktxt =  'EXCESS PAYMENT'.
   PERFORM bdc_field       USING 'BKPF-BKTXT'
                                 gs_header-bktxt.
   CONCATENATE 'EXCESS PAYMENT FOR CUSTOMER'  gs_tab-kunnr
   INTO  gs_header-augtx SEPARATED BY space.
   PERFORM bdc_field       USING 'RF05A-AUGTX'
                                 gs_header-augtx.

   PERFORM bdc_field       USING 'FS006-DOCID'
                                  '*'.
   PERFORM bdc_field       USING 'RF05A-NEWBS'
                                 '40'.
   PERFORM bdc_field       USING 'RF05A-NEWKO'
                                 gs_header-newko.

* 2nd  Second screen
   PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'BSEG-WRBTR'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=PA'.
   WRITE gv_wrbtr TO gv_value.                          "#EC UOM_IN_MES
   CONDENSE gv_value.
   PERFORM bdc_field       USING 'BSEG-WRBTR'
                                 gv_value.
   CLEAR gv_value.
   WRITE gs_header-budat TO gv_value.
   PERFORM bdc_field       USING 'BSEG-VALUT'
                                   gv_value.
   CLEAR gv_value.
   gv_value = 'EXCESS PAYMENT'.
   PERFORM bdc_field       USING 'BSEG-SGTXT'
                                 gv_value.
   PERFORM bdc_field       USING 'RF05A-NEWBS'
                                 '11'.
   PERFORM bdc_field       USING 'RF05A-NEWKO'
                                 gs_tab-kunnr.

* subscreen (pop up)
   PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.

   PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'COBL-PRCTR'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=ENTE'.

* 3rd screen
   PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
   WRITE gv_wrbtr TO gv_value.                          "#EC UOM_IN_MES
   CONDENSE gv_value.
   PERFORM bdc_field       USING 'BSEG-WRBTR'
                                 gv_value.
   CLEAR gv_value.
   WRITE gs_header-budat TO gv_value.
   PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                   gv_value.
   PERFORM bdc_field       USING 'BSEG-SGTXT'
                                 'EXCESS PAYMENT'.


   PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=BU'.
   WRITE gv_wrbtr TO gv_value.                          "#EC UOM_IN_MES
   CONDENSE gv_value.
   PERFORM bdc_field       USING 'BSEG-WRBTR'
                                 gv_value.
   CLEAR gv_value.
   WRITE gs_header-budat TO gv_value.
   PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                   gv_value.
   PERFORM bdc_field       USING 'BSEG-SGTXT'
                                 'EXCESS PAYMENT'.

   REFRESH messtab.
   CALL TRANSACTION 'FB05'
                    USING bdcdata
                    MODE   gv_mode
                    UPDATE 'A'
                    MESSAGES INTO messtab.

   WAIT UP TO 2 SECONDS.
   COMMIT WORK AND WAIT.

   CLEAR: gs_messtab, gs_message.

   READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'S'
                                               msgnr = '312'.
   IF sy-subrc IS INITIAL.
     COMMIT WORK AND WAIT.

     DATA: lv_belnr TYPE belnr_d.
     DO 10 TIMES.
       SELECT SINGLE belnr FROM bkpf INTO lv_belnr
                           WHERE belnr = gs_messtab-msgv1.
       IF sy-subrc IS NOT INITIAL.
         WAIT UP TO 2 SECONDS.
       ELSE.
         EXIT.
       ENDIF.
     ENDDO.

     gs_message-mess_type = 'S'.
     MOVE-CORRESPONDING gs_tab TO gs_message.
     IF gs_tab-belnr IS INITIAL.
       gs_message-belnr = gs_amnt-root_doc.
     ENDIF.
     gs_message-wrbtr = gv_wrbtr.
     CONCATENATE gs_messtab-msgv1 'posted successfully as Excess Payment' INTO
                 gs_message-message  SEPARATED BY space.

     APPEND gs_message TO gt_message.
     PERFORM progress_indicator USING 'Excess payment being posted' 'for Customer:' gs_tab-kunnr 'complete'.
   ELSE.

     READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'E'.
     IF sy-subrc = 0 .
       p_lv_check = 'E'.
       lv_mvar1 = gs_messtab-msgv1.
       lv_mvar2 = gs_messtab-msgv2.
       lv_mvar3 = gs_messtab-msgv3.
       lv_mvar4 = gs_messtab-msgv4.
       CALL FUNCTION 'MESSAGE_PREPARE'
         EXPORTING
           language               = sy-langu
           msg_id                 = gs_messtab-msgid
           msg_no                 = gs_messtab-msgnr
           msg_var1               = lv_mvar1
           msg_var2               = lv_mvar2
           msg_var3               = lv_mvar3
           msg_var4               = lv_mvar4
         IMPORTING
           msg_text               = gs_message-message
         EXCEPTIONS
           function_not_completed = 1
           message_not_found      = 2
           OTHERS                 = 3.
       gs_message-mess_type = 'E'.
       APPEND gs_message TO gt_message.

     ELSE.
       LOOP AT messtab INTO gs_messtab WHERE msgtyp = 'S' .
         lv_mvar1 = gs_messtab-msgv1.
         lv_mvar2 = gs_messtab-msgv2.
         lv_mvar3 = gs_messtab-msgv3.
         lv_mvar4 = gs_messtab-msgv4.
         CALL FUNCTION 'MESSAGE_PREPARE'
           EXPORTING
             language               = sy-langu
             msg_id                 = gs_messtab-msgid
             msg_no                 = gs_messtab-msgnr
             msg_var1               = lv_mvar1
             msg_var2               = lv_mvar2
             msg_var3               = lv_mvar3
             msg_var4               = lv_mvar4
           IMPORTING
             msg_text               = gs_message-message
           EXCEPTIONS
             function_not_completed = 1
             message_not_found      = 2
             OTHERS                 = 3.
         gs_message-mess_type = 'W'.
         APPEND gs_message TO gt_message.
         CLEAR gs_messtab.
       ENDLOOP.
     ENDIF.

   ENDIF.


   FREE :bdcdata[].
   CLEAR: gs_message.

 ENDFORM.                    " FB05_BDC_EXCESS
*&---------------------------------------------------------------------*
*&      Form  BAPI_AP_ACC_GETOPENITEMS
*----------------------------------------------------------------------*
 FORM bapi_ap_acc_getopenitems  USING    p_bukrs TYPE bukrs
                                          p_kunnr TYPE kunnr
                                  CHANGING  ps_bapireturn LIKE gs_bapireturn.

   DATA: pt_lineitemsv  TYPE STANDARD TABLE OF bapi3008_2.     " line items for vendor


   LOOP AT gt_cusven INTO gs_cusven WHERE kunnr = p_kunnr.
     CLEAR:  ps_bapireturn, gs_lineitemsv.
* get open items for vendor
     CALL FUNCTION 'BAPI_AP_ACC_GETOPENITEMS'"#EC CI_USAGE_OK[2628704]
     "Added by SPLABAP during code remediation
       EXPORTING
         companycode = p_bukrs
         vendor      = gs_cusven-lifnr
         keydate     = sy-datum
*        NOTEDITEMS  = ' '
       IMPORTING
         return      = ps_bapireturn
       TABLES
         lineitems   = pt_lineitemsv.
* KR docs only considered as credit notes for further processing
     DELETE pt_lineitemsv WHERE doc_type NE 'KR'.
     DELETE pt_lineitemsv WHERE db_cr_ind NE 'H'.
* add these vendor items to customer open items gathered before
     LOOP AT pt_lineitemsv INTO gs_lineitemsv.
       APPEND gs_lineitemsv TO gt_lineitemsv.
       CLEAR gs_lineitems.
       MOVE-CORRESPONDING gs_lineitemsv TO gs_lineitems.
       gs_lineitems-customer = p_kunnr.
       APPEND gs_lineitems TO gt_lineitems.
     ENDLOOP.
     FREE pt_lineitemsv.
     CLEAR :gs_cusven, ps_bapireturn .
   ENDLOOP.

 ENDFORM.                    " BAPI_AP_ACC_GETOPENITEMS
*&---------------------------------------------------------------------*
*&      Form  F32_BDC_CN
*----------------------------------------------------------------------*
 FORM f32_bdc_cn  CHANGING p_lv_check TYPE any.

   FREE: messtab[], gs_messtab,  gt_bdc[], gt_bdcc[].
   CLEAR: p_lv_check, gs_bdc, gs_bdcc.

   DATA: lv_mvar1      TYPE balm-msgv1,
         lv_mvar2      TYPE balm-msgv1,
         lv_mvar3      TYPE balm-msgv1,
         lv_mvar4      TYPE balm-msgv1,
         lv_okcode(5)  TYPE c,
         lv_cursor(15) TYPE c,
         lv_cnt(2)     TYPE n,
         lv_f          TYPE p DECIMALS 2,
         lv_lines      TYPE sy-tabix,
         lv_abpos      TYPE posnr,
         lv_index      TYPE sy-tabix,
         lv_last       TYPE sy-tabix,
         lv_ceil       TYPE sy-tabix,
         lv_exit       TYPE sy-tabix.

   PERFORM progress_indicator USING 'Clearing using F-32' 'for Customer:' gs_tab-kunnr 'is in progress'.

   CHECK  gv_sclear = 'X' OR  gv_vclear = 'X' OR  gv_nclear = 'X'.

   CALL FUNCTION 'BPAR_P_FI_CUSTOMER_DEQUEUE'
     EXPORTING
       customer       = gs_tab-kunnr
     EXCEPTIONS
       system_failure = 1
       OTHERS         = 2.
   IF sy-subrc <> 0.
* Implement suitable error handling here
   ENDIF.

* 1st screen
   PERFORM bdc_dynpro      USING 'SAPMF05A' '0131'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'RF05A-XPOS1(03)'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=SLB'.
   PERFORM bdc_field       USING 'BKPF-BUKRS'
                                 gs_header-bukrs.
   PERFORM bdc_field       USING 'RF05A-AGKON'
                                 gs_tab-kunnr.
   PERFORM bdc_field       USING 'RF05A-AGUMS'
                                 'A'.
   WRITE gs_header-budat TO gv_value.
   PERFORM bdc_field       USING 'BKPF-BUDAT' "Posting Date
                                  gv_value.
   PERFORM bdc_field       USING 'RF05A-XNOPS'
                                 'X'.


* 2nd screen

   IF gv_vclear = 'X' OR gv_nclear = 'X'.
     DELETE gt_amnt  WHERE cnote NE 'X'.
   ENDIF.

   CLEAR: gs_bdc, gs_amnt, gs_openitems.
   DESCRIBE TABLE gt_amnt LINES lv_last.
*
   CLEAR gs_amnt.
   READ TABLE gt_amnt INTO gs_amnt INDEX lv_last.
   " This is the line where partial adjustment can be done in bdc via clicking
   DELETE gt_amnt INDEX lv_last.
   INSERT gs_amnt INTO gt_amnt INDEX 1.
*
   LOOP AT gt_amnt INTO gs_amnt.
     lv_index = sy-tabix.

     gs_bdc-fval1 = 'BELNR'.

     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
         input  = gs_amnt-root_doc
       IMPORTING
         output = gs_amnt-root_doc.

     CONCATENATE gs_amnt-root_doc gs_amnt-root_year gs_amnt-root_item INTO gs_bdc-fval2.
     lv_lines = lv_lines + 1.
*     IF lv_index = lv_last.
*       INSERT gs_bdc INTO gt_bdc INDEX 1.   " This is the line where partial adjustment can be done in bdc via clicking
*     ELSE.
     APPEND gs_bdc TO gt_bdc.
*     ENDIF.
     CLEAR gs_amnt.
   ENDLOOP.

   LOOP AT gt_amnt INTO gs_amnt.
     CLEAR: gs_bdc, gs_openitems.

     LOOP AT gt_openitems INTO gs_openitems WHERE rootdocyear = gs_amnt-rootdocyear
                                            AND   docyear NE gs_amnt-rootdocyear.

       gs_bdc-fval1 = 'BELNR'.

       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
           input  = gs_openitems-doc_no
         IMPORTING
           output = gs_openitems-doc_no.

       CONCATENATE gs_openitems-doc_no gs_openitems-fisc_year gs_openitems-item_num INTO gs_bdc-fval2.
       lv_lines = lv_lines + 1.
       APPEND gs_bdc TO gt_bdc.
       CLEAR: gs_bdc, gs_openitems.
     ENDLOOP.
     CLEAR gs_amnt.
   ENDLOOP.


   IF gv_nclear = 'X'.
     DELETE gt_openitemsc WHERE previous NE gc_c.
   ENDIF.

   IF gv_vclear = 'X'.
     DELETE gt_openitemsc WHERE previous NE gc_k.
   ENDIF.

   CLEAR gs_openitems.
   LOOP AT gt_openitemsc INTO gs_openitems.
     gs_bdcc-fval1 = 'BELNR'.

     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
         input  = gs_openitems-doc_no
       IMPORTING
         output = gs_openitems-doc_no.

     CONCATENATE gs_openitems-doc_no gs_openitems-fisc_year gs_openitems-item_num INTO gs_bdcc-fval2.
     lv_lines = lv_lines + 1.
     APPEND gs_bdcc TO gt_bdcc.
     CLEAR: gs_bdcc, gs_openitems.
   ENDLOOP.


* for regular invoices entries
   CLEAR: lv_index, lv_last, lv_exit, gs_bdc.
   DESCRIBE TABLE gt_bdc LINES lv_last.
   lv_f = lv_last / 18.
   lv_ceil = ceil( lv_f ).

   DO lv_ceil TIMES.

     PERFORM bdc_dynpro      USING 'SAPMF05A' '0733'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'RF05A-SEL01(01)'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=PA'.

     lv_index = lv_index + 1.
     lv_cnt = 0.

     DO 18 TIMES.
       lv_exit = lv_exit + 1.
       lv_cnt = lv_cnt + 1.
       CLEAR gs_bdc.
       READ TABLE gt_bdc INTO gs_bdc INDEX lv_exit.
       IF sy-subrc IS INITIAL.

         CONCATENATE 'RF05A-FELDN(' lv_cnt ')' INTO gs_bdc-fnam1.
         CONCATENATE 'RF05A-SEL01(' lv_cnt ')' INTO gs_bdc-fnam2.

         PERFORM bdc_field       USING gs_bdc-fnam1
                                       gs_bdc-fval1.
         PERFORM bdc_field       USING gs_bdc-fnam2
                                       gs_bdc-fval2.
       ENDIF.
       IF lv_exit = lv_last.
         EXIT.
       ENDIF.

     ENDDO.


* select more items
     IF lv_index NE lv_ceil.
       PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
       PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'DF05B-PSSKT(01)'.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '=SL'.
       PERFORM bdc_field       USING 'RF05A-ABPOS'
                                     '1'.
* next screen.

       PERFORM bdc_dynpro      USING 'SAPMF05A' '0710'.
       PERFORM bdc_field       USING 'BDC_CURSOR'
                                     'RF05A-AGKON'.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '=SLB'.
       PERFORM bdc_field       USING 'RF05A-AGBUK'
                                     gs_header-bukrs.
       PERFORM bdc_field       USING 'RF05A-AGKON'
                                     gs_tab-kunnr.
       PERFORM bdc_field       USING 'RF05A-AGKOA'
                                     'D'.
       PERFORM bdc_field       USING 'RF05A-XNOPS'
                                     'X'.
       PERFORM bdc_field       USING 'RF05A-XFIFO'
                                     ''.
       PERFORM bdc_field       USING 'RF05A-XPOS1(01)'
                                     'X'.
     ENDIF.
   ENDDO.

**
* for credit notes entries

   CLEAR: lv_index, lv_last, lv_exit, gs_bdcc.
   DESCRIBE TABLE gt_bdcc LINES lv_last.
   lv_f = lv_last / 18.
   lv_ceil = ceil( lv_f ).

   DO lv_ceil TIMES.

     lv_index = lv_index + 1.
     lv_cnt = 0.

* select more items

     PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'DF05B-PSSKT(01)'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=SL'.
     PERFORM bdc_field       USING 'RF05A-ABPOS'
                                   '1'.
* next screen.
     PERFORM bdc_dynpro      USING 'SAPMF05A' '0710'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'RF05A-AGKON'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=SLB'.
     PERFORM bdc_field       USING 'RF05A-AGBUK'
                                   gs_header-bukrs.

     IF gv_vclear = 'X'.
       CLEAR gs_cusven.
       READ TABLE gt_cusven INTO gs_cusven WITH KEY kunnr = gs_tab-kunnr.
       IF sy-subrc IS INITIAL.
         PERFORM bdc_field       USING 'RF05A-AGKON'
                                       gs_cusven-lifnr.
       ELSE.
         PERFORM bdc_field       USING 'RF05A-AGKON'
                                       ''.
       ENDIF.
       PERFORM bdc_field       USING 'RF05A-AGKOA'
                                     'K'.

     ENDIF.

     IF gv_nclear = 'X'.
       PERFORM bdc_field       USING 'RF05A-AGKON'
                                     gs_tab-kunnr.
       PERFORM bdc_field       USING 'RF05A-AGKOA'
                                     'D'.
     ENDIF.

     PERFORM bdc_field       USING 'RF05A-XNOPS'
                                   'X'.
     PERFORM bdc_field       USING 'RF05A-XFIFO'
                                   ''.
     PERFORM bdc_field       USING 'RF05A-XPOS1(01)'
                                   'X'.


     PERFORM bdc_dynpro      USING 'SAPMF05A' '0733'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'RF05A-SEL01(01)'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=PA'.


     DO 18 TIMES.
       lv_exit = lv_exit + 1.
       lv_cnt = lv_cnt + 1.
       CLEAR gs_bdcc.
       READ TABLE gt_bdcc INTO gs_bdcc INDEX lv_exit.
       IF sy-subrc IS INITIAL.

         CONCATENATE 'RF05A-FELDN(' lv_cnt ')' INTO gs_bdcc-fnam1.
         CONCATENATE 'RF05A-SEL01(' lv_cnt ')' INTO gs_bdcc-fnam2.

         PERFORM bdc_field       USING gs_bdcc-fnam1
                                       gs_bdcc-fval1.
         PERFORM bdc_field       USING gs_bdcc-fnam2
                                       gs_bdcc-fval2.
       ENDIF.
       IF lv_exit = lv_last.
         EXIT.
       ENDIF.

     ENDDO.
   ENDDO.

* Next
   CLEAR: lv_okcode.

   lv_okcode = '=PART'.
   lv_cursor = 'DF05B-PSZAH(01)'.

   PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 lv_okcode.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                'DF05B-PSSKT(01)'.
   PERFORM bdc_field       USING 'RF05A-ABPOS'
                                 '1'.


   LOOP AT gt_amnt INTO gs_amnt .

     lv_abpos = sy-tabix.

     PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '/00'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   lv_cursor.
     PERFORM bdc_field       USING 'RF05A-ABPOS'
                                   lv_abpos.
     IF gs_amnt-disc_amnt GT 0 .
       PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '=Z+S'.
     ELSE.
       PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '=Z-S'.
     ENDIF.
   ENDLOOP.

   PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '/00'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 lv_cursor.
   PERFORM bdc_field       USING 'RF05A-ABPOS'
                                 '1'.

   PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=PI'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 lv_cursor.
   PERFORM bdc_field       USING 'RF05A-ABPOS'
                                 '1'.

   PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=BU'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'DF05B-PSZAH(01)'.
   PERFORM bdc_field       USING 'RF05A-ABPOS'
                                 '1'.


   REFRESH messtab.
   CALL TRANSACTION 'F-32'
                    USING bdcdata
                    MODE   gv_mode
                    UPDATE 'A'
                    MESSAGES INTO messtab.

   WAIT UP TO 2 SECONDS.
   COMMIT WORK AND WAIT.

   CLEAR: gs_messtab, gs_message.

   READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'S'
                                               msgnr = '312'.
   IF sy-subrc IS INITIAL.
     COMMIT WORK AND WAIT.

     DATA: lv_belnr TYPE belnr_d.
     DO 10 TIMES.
       SELECT SINGLE belnr FROM bkpf INTO lv_belnr
                           WHERE belnr = gs_messtab-msgv1.
       IF sy-subrc IS NOT INITIAL.
         WAIT UP TO 2 SECONDS.
       ELSE.
         EXIT.
       ENDIF.
     ENDDO.

     gs_message-mess_type = 'S'.
     MOVE-CORRESPONDING gs_tab TO gs_message.

     LOOP AT gt_amnt INTO gs_amnt.
       CLEAR : gs_message-wrbtr, gs_message-belnr.
       gs_message-belnr = gs_amnt-root_doc.
       CONCATENATE 'Clearing Document:' gs_messtab-msgv1 'Posted Successfully' INTO
                   gs_message-message  SEPARATED BY space.

       APPEND gs_message TO gt_message.
     ENDLOOP.
     PERFORM progress_indicator USING 'Clearing using F-32' 'for Customer:' gs_tab-kunnr 'complete'.
   ELSE.

     READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'E'.
     IF sy-subrc = 0 .
       p_lv_check = 'E'.
       lv_mvar1 = gs_messtab-msgv1.
       lv_mvar2 = gs_messtab-msgv2.
       lv_mvar3 = gs_messtab-msgv3.
       lv_mvar4 = gs_messtab-msgv4.
       CALL FUNCTION 'MESSAGE_PREPARE'
         EXPORTING
           language               = sy-langu
           msg_id                 = gs_messtab-msgid
           msg_no                 = gs_messtab-msgnr
           msg_var1               = lv_mvar1
           msg_var2               = lv_mvar2
           msg_var3               = lv_mvar3
           msg_var4               = lv_mvar4
         IMPORTING
           msg_text               = gs_message-message
         EXCEPTIONS
           function_not_completed = 1
           message_not_found      = 2
           OTHERS                 = 3.
       gs_message-mess_type = 'E'.
       APPEND gs_message TO gt_message.

     ELSE.

       LOOP AT messtab INTO gs_messtab WHERE msgtyp = 'S' .
         lv_mvar1 = gs_messtab-msgv1.
         lv_mvar2 = gs_messtab-msgv2.
         lv_mvar3 = gs_messtab-msgv3.
         lv_mvar4 = gs_messtab-msgv4.
         CALL FUNCTION 'MESSAGE_PREPARE'
           EXPORTING
             language               = sy-langu
             msg_id                 = gs_messtab-msgid
             msg_no                 = gs_messtab-msgnr
             msg_var1               = lv_mvar1
             msg_var2               = lv_mvar2
             msg_var3               = lv_mvar3
             msg_var4               = lv_mvar4
           IMPORTING
             msg_text               = gs_message-message
           EXCEPTIONS
             function_not_completed = 1
             message_not_found      = 2
             OTHERS                 = 3.
         gs_message-mess_type = 'W'.
         APPEND gs_message TO gt_message.
         CLEAR gs_messtab.
       ENDLOOP.

     ENDIF.
   ENDIF.


   FREE :bdcdata[].
   CLEAR: gs_message.


 ENDFORM.                    " F32_BDC_CN
*&---------------------------------------------------------------------*
*&      Form  BLDAT_CHECK
*----------------------------------------------------------------------*
 FORM bldat_check USING p_dat TYPE  bkpf-bldat
                  CHANGING p_check LIKE check.

   CLEAR : p_check.

   DATA: lv_monc(2) TYPE  c, " Current month
         lv_yearc(4) TYPE c, " current year
         lv_spmonc(6),       " year month
         lv_mond(2) TYPE c, "document month
         lv_yeard(4) TYPE c, " document year
         lv_spmond(6),       " doc year month
         lv_mon TYPE i,  " month
         lv_year(4) TYPE c, "  year
         lv_spmon(6),       " year month
         lv_mdiff TYPE i,   " diff in months
         lv_mondi TYPE i. "document month

   CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
     EXPORTING
       i_date  = sy-datum
     IMPORTING
       e_month = lv_monc
       e_year  = lv_yearc.

   CONCATENATE lv_yearc lv_monc INTO lv_spmonc IN CHARACTER MODE.

   CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
     EXPORTING
       i_date  = p_dat
     IMPORTING
       e_month = lv_mond
       e_year  = lv_yeard.

   CONCATENATE lv_yeard lv_mond INTO lv_spmond IN CHARACTER MODE.
   lv_mondi = lv_mond.

   IF lv_spmond LT lv_spmonc.
     CLEAR : lv_spmon, lv_mdiff.
     DO.
       IF lv_mdiff EQ 0.
         lv_mon = lv_monc.
         lv_year = lv_yearc.
       ENDIF.

       IF lv_year EQ lv_yeard AND lv_mon = lv_mondi.
         EXIT.
       ENDIF.
       IF lv_mon = 1.
         lv_mon = '12'.
         lv_year = lv_year - 1.
       ELSE.
         lv_mon = lv_mon - 1.
       ENDIF.

       lv_mdiff = lv_mdiff + 1.
       IF lv_mdiff GT 2.
         p_check = 'X'.
         EXIT.
       ENDIF.
     ENDDO.
   ENDIF.

 ENDFORM.                    " BLDAT_CHECK
*&---------------------------------------------------------------------*
*&      Form  F13_BDC
*&---------------------------------------------------------------------*
 FORM f13_bdc  CHANGING p_lv_check TYPE any.

   FREE: messtab[], gs_messtab.
   CLEAR: p_lv_check.

   DATA: lv_mvar1      TYPE balm-msgv1,
         lv_mvar2      TYPE balm-msgv1,
         lv_mvar3      TYPE balm-msgv1,
         lv_mvar4      TYPE balm-msgv1.

   PERFORM progress_indicator USING 'Automatic clearing' 'for Customer:' gs_tab-kunnr 'is in progress'.


   CALL FUNCTION 'BPAR_P_FI_CUSTOMER_DEQUEUE'
     EXPORTING
       customer       = gs_tab-kunnr
     EXCEPTIONS
       system_failure = 1
       OTHERS         = 2.
   IF sy-subrc <> 0.
* Implement suitable error handling here
   ENDIF.

***
   PERFORM bdc_dynpro      USING 'SAPF124' '1000'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'X_FEHLER'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=ONLI'.
   PERFORM bdc_field       USING 'BUKRX-LOW'
                                 gs_header-bukrs.
   PERFORM bdc_field       USING 'X_KUNNR'
                                 'X'.
   PERFORM bdc_field       USING 'KONTD-LOW'
                                 gs_tab-kunnr.
   PERFORM bdc_field       USING 'X_TESTL'
                                 ''.
   PERFORM bdc_field       USING 'XAUSBEL'
                                 'X'.
   PERFORM bdc_field       USING 'XNAUSBEL'
                                 ''.
   PERFORM bdc_field       USING 'X_FEHLER'
                                 ''.

***
   PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '=%EX'.
***

   PERFORM bdc_dynpro      USING 'SAPF124' '1000'.
   PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'BUKRX-LOW'.
   PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '/EENDE'.
***

   REFRESH messtab.
   CALL TRANSACTION 'F.13'
                    USING bdcdata
                    MODE   gv_mode
                    UPDATE 'A'
                    MESSAGES INTO messtab.

   WAIT UP TO 2 SECONDS.
   COMMIT WORK AND WAIT.

   CLEAR: gs_messtab, gs_message.
   READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'E'.
   IF sy-subrc = 0 .
     p_lv_check = 'E'.
     lv_mvar1 = gs_messtab-msgv1.
     lv_mvar2 = gs_messtab-msgv2.
     lv_mvar3 = gs_messtab-msgv3.
     lv_mvar4 = gs_messtab-msgv4.
     CALL FUNCTION 'MESSAGE_PREPARE'
       EXPORTING
         language               = sy-langu
         msg_id                 = gs_messtab-msgid
         msg_no                 = gs_messtab-msgnr
         msg_var1               = lv_mvar1
         msg_var2               = lv_mvar2
         msg_var3               = lv_mvar3
         msg_var4               = lv_mvar4
       IMPORTING
         msg_text               = gs_message-message
       EXCEPTIONS
         function_not_completed = 1
         message_not_found      = 2
         OTHERS                 = 3.
     gs_message-mess_type = 'E'.
     APPEND gs_message TO gt_message.
   ENDIF.

   FREE :bdcdata[].
   CLEAR: gs_message.

 ENDFORM.                    " F13_BDC
