*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 26.12.2023
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                : DEVK934875
*
*  Business Logic            : Report for region wise update gst condition for material
*
*  Released on Date          :
*
* Hardcoded                  : fval        - '18' - 'GE'
*                              fval        - '12' - 'GH'

*=======================================================================
REPORT zsd_mat_gst_ext_dms.

DATA : gv_kunnr TYPE vbrk-kunag,
       gv_vbeln TYPE vbrk-vbeln.

SELECT-OPTIONS : so_kunnr FOR gv_kunnr,
                 so_date  FOR sy-datum,
                 so_vbeln FOR gv_vbeln.
DATA: lt_bdcdata TYPE TABLE OF bdcdata.
*&---------------------------------------------------------------------*
*& Include          ZMM_MAT_MASTER_C01
*&---------------------------------------------------------------------*
CLASS lcl_mat_gst DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_final,        "for alv changes
              to_reg   TYPE regio,
              matnr    TYPE matnr,
              vbeln    TYPE vbeln,
              posnr    TYPE posnr,
              knumv    TYPE knumv,
              kunag    TYPE kunag,
              from_reg TYPE regio,
              to_plnt  TYPE werks_d,
              amnt     TYPE kbetr,
              joig     TYPE kbetr,
              mwskz    TYPE mwskz,
              type     TYPE bapi_mtype,
              msg      TYPE string,
            END OF ty_final.

    DATA : gt_final  TYPE TABLE OF ty_final,     "final it
           lt_return TYPE TABLE OF bapiret2.
*methods dec
    METHODS : fetch,           "fetching
      alv,             "mat master alv
      bdc_process.     "call bdc

ENDCLASS.


CLASS lcl_mat_gst IMPLEMENTATION.
  METHOD fetch.
***************fetch only DMS distributor***************
    SELECT bwkey, bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
    IF sy-subrc = 0.

      SELECT a~vbeln,
             a~knumv,
             a~kunag,
             b~posnr,
             b~matnr,
             b~wkreg AS from_reg,
             c~regio AS to_reg,
             c~werks AS to_plnt FROM vbrk AS a
                                INNER JOIN vbrp AS b ON a~vbeln EQ b~vbeln
                                INNER JOIN kna1 AS c ON a~kunag EQ c~kunnr
                                INTO CORRESPONDING FIELDS OF TABLE @gt_final
                                FOR ALL ENTRIES IN @lt_t001k
                                WHERE a~vbeln IN @so_vbeln
                                AND   fkart   EQ 'YBBR'
                                AND   kunag   IN @so_kunnr
                                AND   fkdat   IN @so_date
                                AND   fksto   NE @abap_true
                                AND   c~werks EQ @lt_t001k-bwkey.
    ENDIF.
    IF sy-subrc = 0.
      bdc_process( ).
      alv( ).
      CALL SCREEN 9000.
    ELSE.
      MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD alv.
    DATA : lv_alv  TYPE REF TO cl_gui_alv_grid, "for alv object
           lt_fcat TYPE lvc_t_fcat.   "data dec for fieldcat
    DATA(ls_layo) = VALUE lvc_s_layo( cwidth_opt = abap_true
                                      zebra = abap_true ).   "data dec for layout design
    IF lv_alv IS INITIAL.
**create the object for alv
      CREATE OBJECT lv_alv
        EXPORTING
          i_parent = cl_gui_container=>default_screen.

    ENDIF.
    lt_fcat = VALUE #(
    ( col_pos = 1  fieldname = 'VBELN'       ref_field = 'VBELN' ref_table = 'VBRK')
    ( col_pos = 2  fieldname = 'KNUMV'       ref_field = 'KNUMV' ref_table = 'VBRK')
    ( col_pos = 3  fieldname = 'MATNR'       ref_field = 'MATNR' ref_table = 'VBRP')
    ( col_pos = 4  fieldname = 'KUNAG'       ref_field = 'KUNNR' ref_table = 'KNA1')
    ( col_pos = 5  fieldname = 'FROM_REG '   scrtext_m = 'From Region')
    ( col_pos = 6  fieldname = 'TO_REG '     scrtext_m = 'To Region')
    ( col_pos = 7  fieldname = 'AMNT'        scrtext_m = 'JOCG')
    ( col_pos = 8  fieldname = 'AMNT'        scrtext_m = 'JOSG')
    ( col_pos = 8  fieldname = 'JOIG'        scrtext_m = 'JOIG')
    ( col_pos = 9  fieldname = 'MWSKZ'       ref_field = 'MWSK1' ref_table = 'PRCD_ELEMENTS')
    ( col_pos = 10 fieldname = 'TO_PLNT'     scrtext_m = 'To Plant' )
    ( col_pos = 11 fieldname = 'TYPE'        scrtext_m = 'Type')
    ( col_pos = 12 fieldname = 'MSG'         scrtext_m = 'Message') ).
*display the alv
    lv_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layo    "Layout
      CHANGING
        it_outtab                     = gt_final    "Output Table
        it_fieldcatalog               = lt_fcat     "Field Catalog
    ).
    REFRESH lt_fcat.        "refresh fieldcat

  ENDMETHOD.
  METHOD bdc_process.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.
    DATA : ls_bdcdata TYPE bdcdata.
***********messages of call transaction
    DATA : lt_msg TYPE TABLE OF bdcmsgcoll.
*********local variable dec
    DATA : lv_matnr  TYPE string,
           lv_kbetr  TYPE string,
           lv_mwsk1  TYPE string,
           lv_item   TYPE i,
           lv_amnt   TYPE char10,
           lv_check1 TYPE char1,
           lv_check2 TYPE char1,
           lv_check3 TYPE char1,
           lv_msg    TYPE string.
******************fetch the pricing elements************
    SELECT knumv,kposn,kschl,kbetr,mwsk1 FROM prcd_elements INTO TABLE @DATA(lt_cond)
                                                            FOR ALL ENTRIES IN @gt_final
                                                            WHERE knumv =  @gt_final-knumv
                                                            AND   kschl IN ( 'JOIG' , 'JOCG' , 'JOSG' ).
    IF sy-subrc = 0.
      SORT lt_cond BY knumv kposn kschl.
    ENDIF.
******************fetch the pricing elements************
    SELECT kschl,wkreg,regio,matnr FROM a709 INTO TABLE @DATA(lt_a709)
*                                             FOR ALL ENTRIES IN @gt_final
                                             WHERE kschl IN ( 'JOIG' , 'JOCG' , 'JOSG' )
*                                             AND   matnr EQ @gt_final-matnr
                                             AND   datab LE @sy-datum
                                             AND   datbi GE @sy-datum.
    IF sy-subrc = 0.
      SORT lt_a709 BY kschl wkreg regio matnr.
    ENDIF.
********************fetch the region mapping**********
    SELECT * FROM zdms_regio_map INTO TABLE @DATA(lt_regio) FOR ALL ENTRIES IN @gt_final
                                                            WHERE from_reg = @gt_final-to_reg.
    IF sy-subrc = 0.
      SORT lt_regio BY from_reg.
    ENDIF.
***********sorting the final table.**************
    SORT : gt_final BY to_reg matnr.
************delete the dublicates material***********
    DELETE ADJACENT DUPLICATES FROM gt_final COMPARING to_reg matnr.
********************data fill & call bapi process( material save )************
    LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final>).
*************************BDC Data Filling*********************
      AT NEW to_reg.
        PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '/00'.
        PERFORM bdc_field       USING 'RV13A-KSCHL'
                                      'JOSG'.
        PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=WEIT'.
        PERFORM bdc_dynpro      USING 'SAPMV13A' '1709'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '/00'.
        PERFORM bdc_field       USING 'KOMG-WKREG'
                                      <fs_final>-to_reg.
        PERFORM bdc_field       USING 'KOMG-REGIO'
                                      <fs_final>-to_reg.
        PERFORM bdc_field       USING 'KOMG-TAXK1'
                                      '0'.
      ENDAT.

*      AT NEW matnr.
***********************Old material code conversion************
      IF <fs_final>-matnr(1) NE 'Z'.
        SELECT SINGLE matnr,bismt,mtart,lvorm,status FROM mara INTO @DATA(ls_old)
                                        WHERE matnr = @<fs_final>-matnr.

*******************get the old material code*************
        IF ls_old-mtart NE 'HAWA'.
          IF ls_old-bismt IS INITIAL.
            <fs_final>-msg = | New material - { <fs_final>-matnr } is not maintained in old material code|.
            <fs_final>-type = 'E'.
          ELSE.
            CONDENSE ls_old-bismt.
            <fs_final>-matnr = ls_old-bismt.
          ENDIF.
        ENDIF.
      ENDIF.
*      ENDIF.
      SELECT SINGLE matnr,bismt,mtart,lvorm,status FROM mara INTO @DATA(ls_new)
                                      WHERE matnr = @<fs_final>-matnr.

*******************check the material is blocked or not*************
      IF ls_new-lvorm = 'X' OR ls_new-status = 'X'.
        <fs_final>-msg = |Material - { <fs_final>-matnr } is Blocked|.
        <fs_final>-type = 'E'.
      ENDIF.
      CLEAR : ls_old,ls_new.
***********distributor plant check*************
      IF <fs_final>-to_plnt IS INITIAL.
        <fs_final>-msg  = | Distributor plant not maintained |.
        <fs_final>-type = 'E'.
      ENDIF.

      IF <fs_final>-type = 'E'..
*************error log store************
        lo_errorlog_dms->log_entry_store(
          EXPORTING
            type                = 22
            status              = 10
            plant               = <fs_final>-to_plnt
            material            = <fs_final>-matnr
            distributor         = <fs_final>-kunag
            msg                 = <fs_final>-msg ).
*        CONTINUE.
      ELSE.
*****************check the material already extent or not**********
        CLEAR : lv_check1,lv_check2,lv_check3.
*************JOSG check**********
        READ TABLE lt_a709 TRANSPORTING NO FIELDS WITH KEY kschl = 'JOSG'
                                                           wkreg = <fs_final>-to_reg
                                                           regio = <fs_final>-to_reg
                                                           matnr = <fs_final>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          lv_check1 = abap_true.
        ENDIF.
*************JOCG check**********
        READ TABLE lt_a709 TRANSPORTING NO FIELDS WITH KEY kschl = 'JOCG'
                                                           wkreg = <fs_final>-to_reg
                                                           regio = <fs_final>-to_reg
                                                           matnr = <fs_final>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          lv_check2 = abap_true.
        ENDIF.
*************JOIG check**********
        LOOP AT lt_regio INTO DATA(ls_regio) WHERE from_reg = <fs_final>-to_reg.
          lv_check3 = abap_true.
          READ TABLE lt_a709 TRANSPORTING NO FIELDS WITH KEY kschl = 'JOIG'
                                                             wkreg = ls_regio-from_reg
                                                             regio = ls_regio-to_reg
                                                             matnr = <fs_final>-matnr BINARY SEARCH.
          IF sy-subrc NE 0.
            lv_check3 = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
*********************Material item data filling*************
        IF lv_check1 = abap_false OR lv_check2 = abap_false OR lv_check3 = abap_false.
******************get the gst percentage***********
          IF <fs_final>-from_reg EQ <fs_final>-to_reg.
            READ TABLE lt_cond INTO DATA(ls_cond) WITH KEY knumv = <fs_final>-knumv
                                                           kposn = <fs_final>-posnr
                                                           kschl = 'JOSG' BINARY SEARCH.
          ELSE.
            READ TABLE lt_cond INTO ls_cond WITH KEY knumv = <fs_final>-knumv
                                                     kposn = <fs_final>-posnr
                                                     kschl = 'JOIG' BINARY SEARCH.
          ENDIF.
          IF sy-subrc = 0.
            CLEAR : lv_amnt.
            IF ls_cond-kschl = 'JOIG'.
              <fs_final>-amnt = ls_cond-kbetr / 2.
            ELSE.
              <fs_final>-amnt = ls_cond-kbetr.
            ENDIF.
            IF <fs_final>-amnt = 9.
              <fs_final>-mwskz = 'GD'.
              <fs_final>-joig = 18.
            ELSEIF <fs_final>-amnt = 6.
              <fs_final>-mwskz = 'GG'.
              <fs_final>-joig = 12.
            ENDIF.
            lv_item  = lv_item + 1.
            lv_matnr = |KOMG-MATNR({ lv_item })|.
            lv_kbetr = |KONP-KBETR({ lv_item })|.
            lv_mwsk1 = |KONP-MWSK1({ lv_item })|.
            lv_amnt = <fs_final>-amnt.
            CONDENSE lv_amnt NO-GAPS.
            PERFORM  bdc_field      USING lv_matnr
                                          <fs_final>-matnr.     "'ENAFT585C1'.
            PERFORM bdc_field       USING lv_kbetr
                                          lv_amnt.              "'               9'.
            PERFORM bdc_field       USING lv_mwsk1
                                          <fs_final>-mwskz.     "'GD'.
            PERFORM bdc_dynpro      USING 'SAPMV13A' '1709'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '/00'.
            PERFORM bdc_dynpro      USING 'SAPMV13A' '1709'.
            IF lv_item = 14.
              CLEAR lv_item.
            ENDIF.
          ENDIF.
        ELSE.
          <fs_final>-msg  = |JOSG & JOCG & JOIG Already Maintained for Material - { <fs_final>-mwskz }|.
          <fs_final>-type = 'S'.
        ENDIF.
      ENDIF.
*      ENDAT.
**************at end of every region to call bdc program***********
      AT END OF to_reg.
*******************check bdc item data filled or not***********
        READ TABLE lt_bdcdata TRANSPORTING NO FIELDS WITH KEY fnam = 'KOMG-MATNR(1)'.
        IF sy-subrc NE 0.
          REFRESH : lt_bdcdata.
        ENDIF.
        IF lt_bdcdata IS NOT INITIAL.
***********************JOSG Process*************
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=SICH'.
          CALL TRANSACTION 'VK11' USING lt_bdcdata
                                  MODE   'N'
                                  UPDATE 'S'
                                  MESSAGES INTO lt_msg.
          READ TABLE lt_msg INTO DATA(ls_msg) WITH KEY msgtyp = 'E'.
          IF sy-subrc = 0.
*********************error log***********
            lv_msg = | from Reg { <fs_final>-to_reg } - To Reg { <fs_final>-to_reg } , { ls_msg-msgv1 }| .
            <fs_final>-msg  = lv_msg.
            <fs_final>-type = 'E'.
            lo_errorlog_dms->log_entry_store(
              EXPORTING
                type                = 23
                status              = 10
                dms_orderid         = 'JOSG'
                msg                 = lv_msg ).
          ENDIF.
***********************JOCG Process*************
          ls_bdcdata-fval = 'JOCG'.
          MODIFY lt_bdcdata INDEX 3 FROM ls_bdcdata TRANSPORTING fval.
          IF sy-subrc = 0.
            REFRESH :lt_msg.
            CALL TRANSACTION 'VK11' USING lt_bdcdata
                                    MODE   'N'
                                    UPDATE 'S'
                                    MESSAGES INTO lt_msg.
            READ TABLE lt_msg INTO ls_msg WITH KEY msgtyp = 'E'.
            IF sy-subrc = 0.
*********************error log***********
              lv_msg = | from Reg { <fs_final>-to_reg } - To Reg { <fs_final>-to_reg }, { ls_msg-msgv1 }| .
              <fs_final>-msg  = lv_msg.
              <fs_final>-type = 'E'.
              lo_errorlog_dms->log_entry_store(
                EXPORTING
                  type                = 23
                  status              = 10
                  dms_orderid         = 'JOCG'
                  msg                 = lv_msg ).
            ELSE.
              <fs_final>-msg  = |JOCG / JOSG Updated from Reg { <fs_final>-to_reg } - To Reg { <fs_final>-to_reg }| .
              <fs_final>-type = 'S'.
            ENDIF.
          ENDIF.
***********************JOIG Process*************
          CLEAR   : lv_item,ls_bdcdata.
          READ TABLE lt_regio TRANSPORTING NO FIELDS WITH KEY from_reg = <fs_final>-to_reg BINARY SEARCH.
          IF sy-subrc = 0.
            ls_bdcdata-fval = 'JOIG'.
            MODIFY lt_bdcdata INDEX 3 FROM ls_bdcdata TRANSPORTING fval.
            ls_bdcdata-fval = '18.00'.
            MODIFY lt_bdcdata FROM ls_bdcdata TRANSPORTING fval WHERE fval = '9.00'.
            ls_bdcdata-fval = '12.00'.
            MODIFY lt_bdcdata FROM ls_bdcdata TRANSPORTING fval WHERE fval = '6.00'.
            ls_bdcdata-fval = 'GE'.
            MODIFY lt_bdcdata FROM ls_bdcdata TRANSPORTING fval WHERE fval = 'GD'.
            ls_bdcdata-fval = 'GH'.
            MODIFY lt_bdcdata FROM ls_bdcdata TRANSPORTING fval WHERE fval = 'GG'.
************loop process****************
            LOOP AT lt_regio INTO ls_regio WHERE from_reg = <fs_final>-to_reg.
              ls_bdcdata-fval = ls_regio-to_reg.
              MODIFY lt_bdcdata FROM ls_bdcdata TRANSPORTING fval WHERE fnam = 'KOMG-REGIO'.
              REFRESH lt_msg.
              CALL TRANSACTION 'VK11' USING lt_bdcdata
                                      MODE   'N'
                                      UPDATE 'S'
                                      MESSAGES INTO lt_msg.
              READ TABLE lt_msg INTO ls_msg WITH KEY msgtyp = 'E'.
              IF sy-subrc = 0.
*********************error log***********
                lv_msg = | from Reg { ls_regio-from_reg } - To Reg { ls_regio-to_reg }, { ls_msg-msgv1 }| .
                <fs_final>-msg  = lv_msg.
                <fs_final>-type = 'E'.
                lo_errorlog_dms->log_entry_store(
                  EXPORTING
                    type                = 23
                    status              = 10
                    dms_orderid         = 'JOIG'
                    msg                 = lv_msg ).
              ELSE.
                <fs_final>-msg  = |{ <fs_final>-msg } , JOIG Updated from Reg { <fs_final>-to_reg } - To Reg { ls_regio-to_reg }| .
                <fs_final>-type = 'S'.
              ENDIF.
            ENDLOOP.
          ENDIF.
          REFRESH : lt_bdcdata,lt_msg.
        ENDIF.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  DATA: ls_bdcdata TYPE bdcdata.
  CLEAR ls_bdcdata.
  ls_bdcdata-program  = program.
  ls_bdcdata-dynpro   = dynpro.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  IF fval IS NOT INITIAL.
    DATA: ls_bdcdata TYPE bdcdata.
    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = fnam.
    ls_bdcdata-fval = fval.
    APPEND ls_bdcdata TO lt_bdcdata.
  ENDIF.
ENDFORM.
***********Screen events***************
INITIALIZATION.
  "create the object for local class
  DATA lobj_mat TYPE REF TO lcl_mat_gst.
  CREATE OBJECT lobj_mat.
************call the class method***************
START-OF-SELECTION.
  lobj_mat->fetch( ).
************PAI**************
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR lobj_mat.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
