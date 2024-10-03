*&---------------------------------------------------------------------*
*& Report ZPP_CRT_SAMP_MAN_INSP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Functional                  : Ragu M                                *
*& Developer                   : Himansu Patnaik                       *
*& Created On                  : 29.08.2022                            *
*& Description                 : Create Sample and Manual Inspection   *
*                                Lots for Samples.                     *
*                                view.                                 *
*& Report Name                 : ZPP_CRT_SAMP_MAN_INSP                 *
*& Report Name                 : ZQPR5                                 *
*& Transport Request           : DEVK932103                            *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*                          Modification Log                            *
*----------------------------------------------------------------------*
* Date         |     Name          |   Request No.  |   Description    *
*              |                   |                |                  *
*              |                   |                |                  *
*              |                   |                |                  *
*----------------------------------------------------------------------*

REPORT zpp_crt_samp_man_insp.

*----------------------------------------------------------------------*
*-Data Types Declaration                                               *
*----------------------------------------------------------------------*

TYPES : BEGIN   OF ty_mchb,
          matnr TYPE matnr,
          werks TYPE werks_d,
          lgort TYPE lgort_d,
          charg TYPE charg_d,
        END     OF ty_mchb.

TYPES : BEGIN    OF ty_err_alv,
          tcode  TYPE bdc_tcode,
          dyname TYPE bdc_module,
          dynumb TYPE bdc_dynnr,
          msgtyp TYPE bdc_mart,
          msgv1	 TYPE	bdc_vtext1,
        END      OF ty_err_alv.

DATA : lv_charg TYPE mchb-charg.
DATA: gt_bdcdata TYPE STANDARD TABLE OF bdcdata,
      gs_bdcdata TYPE bdcdata.

DATA : gt_alv     TYPE REF TO cl_salv_table,
       gt_message TYPE REF TO cx_salv_msg,
       gt_err     TYPE STANDARD TABLE OF ty_err_alv,
       gs_err     TYPE ty_err_alv,
       gt_msgcoll TYPE STANDARD TABLE OF bdcmsgcoll,
       gt_mchb    TYPE STANDARD TABLE OF ty_mchb,
       gs_mchb    TYPE ty_mchb,
       gv_mode    TYPE c VALUE 'N',
*      gv_flag    TYPE c,
       gv_count   TYPE char2.

DATA: jobname            TYPE tbtcjob-jobname VALUE 'ZINSP',
      starttimeimmediate TYPE btch0000-char1 VALUE 'X',
      jobcount           TYPE tbtcjob-jobcount.
DATA: lt_seltab TYPE TABLE OF rsparams,
      ls_seltab LIKE LINE OF lt_seltab,
      lv_date   TYPE sy-datum,
      lv_date1  TYPE sy-datum,
      lv_u      TYPE char1 VALUE '_',
      lv_time   TYPE sy-uzeit,
      lv_ktext  TYPE qtext_prs.

DATA: starttime TYPE tbtcstrt.

*----------------------------------------------------------------------*
*-SELECTION SCREEN                                                     *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : s_charg FOR lv_charg OBLIGATORY.
  SKIP 2.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : p_evm   RADIOBUTTON GROUP r1,
               p_etwom RADIOBUTTON GROUP r1,
               p_ethem RADIOBUTTON GROUP r1,
               p_eform RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b2.

*---------------------------------------------------------------------*
*       INITIALIZATION                                                *
*---------------------------------------------------------------------*
INITIALIZATION.

  FREE : gt_err,gt_message,gt_alv,gt_err,gt_msgcoll.
  CLEAR : gt_err,gt_message,gt_alv,gt_err,gt_msgcoll.

*---------------------------------------------------------------------*
*       CLASS lcl_insp_Eval DEFINITION                                *
*---------------------------------------------------------------------*
CLASS lcl_insp_eval DEFINITION.

  PUBLIC SECTION.
    METHODS get_data.
    METHODS create_sample.
    METHODS manual_inspection_lots.
    METHODS get_alv_instance.
    METHODS disp_alv.

ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS lcl_insp_Eval IMPLEMENTATION                            *
*---------------------------------------------------------------------*
CLASS lcl_insp_eval IMPLEMENTATION.

  METHOD get_data.

*every one month
    IF p_evm IS NOT INITIAL.
      CLEAR gv_count.
      gv_count = 12.
    ENDIF.
*Every two months
    IF p_etwom IS NOT INITIAL.
      CLEAR gv_count.
      gv_count = 6.
    ENDIF.
*Every three months
    IF p_ethem IS NOT INITIAL.
      CLEAR gv_count.
      gv_count = 4.
    ENDIF.
*Every four months
    IF p_eform IS NOT INITIAL.
      CLEAR gv_count.
      gv_count = 3.
    ENDIF.

    SELECT matnr,werks,lgort,charg
           FROM mchb INTO TABLE @DATA(lt_mchb)
                                 WHERE charg IN @s_charg.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING lt_mchb TO gt_mchb.
    ENDIF.
  ENDMETHOD.
  METHOD create_sample.
    CLEAR : gt_bdcdata,lv_ktext.
    LOOP AT  gt_mchb ASSIGNING FIELD-SYMBOL(<fs_mchb>).

      DATA(lv_date) = sy-datum.
      DATA(lv_mat)  = <fs_mchb>-matnr.
      SHIFT lv_mat LEFT DELETING LEADING '0'.
      CONCATENATE lv_mat <fs_mchb>-werks lv_date
                  <fs_mchb>-charg INTO lv_ktext SEPARATED BY '_'.
      CONDENSE lv_ktext.

      PERFORM bdc_dynpro      USING 'SAPLQPRS'          '0100'.

      PERFORM bdc_field       USING 'BDC_CURSOR'        'RQPRS-PRART'.
      PERFORM bdc_field       USING 'BDC_OKCODE'        '/00'.
      PERFORM bdc_field       USING 'RQPRS-PHYNR'       <fs_mchb>-charg.
      PERFORM bdc_field       USING 'RQPRS-PRART'       '01'.
      PERFORM bdc_field       USING 'RQPRS-XPR_O'       'X'.
      PERFORM bdc_field       USING 'RQPRS-XPNRN'       'X'.

      PERFORM bdc_dynpro      USING 'SAPLQPRS'          '0150'.

      PERFORM bdc_field       USING 'BDC_CURSOR'        'RQPRS-CHARG'.
      PERFORM bdc_field       USING 'BDC_OKCODE'        '=CLSE'.
      PERFORM bdc_field       USING 'RQPRS-MATNR'       <fs_mchb>-matnr.
      PERFORM bdc_field       USING 'RQPRS-WERKS'       <fs_mchb>-werks.
      PERFORM bdc_field       USING 'RQPRS-ALORT'       '0006'.
      PERFORM bdc_field       USING 'RQPRS-CHARG'       <fs_mchb>-charg.

      PERFORM bdc_dynpro      USING 'SAPLQPRS'          '0101'.

      PERFORM bdc_field       USING 'BDC_CURSOR'        'RQPRS-KTEXT'.
      PERFORM bdc_field       USING 'BDC_OKCODE'        '=ALLG'.
      PERFORM bdc_field       USING 'RQPRS-KTEXT'       lv_ktext."<fs_mchb>-charg.
      PERFORM bdc_field       USING 'RQPRS-MATNR'       <fs_mchb>-matnr.
      PERFORM bdc_field       USING 'RQPRS-CHARG'       <fs_mchb>-charg.

      PERFORM bdc_dynpro      USING 'SAPLQPRS'          '0101'.

      PERFORM bdc_field       USING 'BDC_OKCODE'        '=BFRE'.
      PERFORM bdc_field       USING 'RQPRS-KTEXT'       lv_ktext."<fs_mchb>-charg.
      PERFORM bdc_field       USING 'BDC_CURSOR'        'RQPRS-MEINH'.
      PERFORM bdc_field       USING 'RQPRS-MENGE'       '1'.
      PERFORM bdc_field       USING 'RQPRS-MEINH'       'KG'.

      PERFORM bdc_dynpro      USING 'SAPLQPRS'          '0101'.

      PERFORM bdc_field       USING 'BDC_CURSOR'        'RQPRS-KTEXT'.
      PERFORM bdc_field       USING 'BDC_OKCODE'        '=BU'.
      PERFORM bdc_field       USING 'RQPRS-KTEXT'       lv_ktext."<fs_mchb>-charg.
      PERFORM bdc_field       USING 'RQPRS-MENGE'       '1'.
      PERFORM bdc_field       USING 'RQPRS-MEINH'       'KG'.

      CALL TRANSACTION 'QPR1' USING gt_bdcdata
                               MODE gv_mode
                               UPDATE 'S'
                               MESSAGES  INTO gt_msgcoll.

      READ TABLE gt_msgcoll ASSIGNING FIELD-SYMBOL(<fs_msgcoll>)
                            WITH KEY msgtyp = 'E'.
      IF sy-subrc <> 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        READ TABLE gt_msgcoll ASSIGNING FIELD-SYMBOL(<fs_msgcoll_2>)
                                        WITH KEY msgtyp = 'S'.
        CONCATENATE <fs_msgcoll_2>-msgv1 'Successfully Create Sample'
                    INTO DATA(lv_msg) SEPARATED BY space.
        gs_err-tcode  = <fs_msgcoll_2>-tcode.
        gs_err-dyname = <fs_msgcoll_2>-dyname.
        gs_err-dynumb = <fs_msgcoll_2>-dynumb.
        gs_err-msgtyp = <fs_msgcoll_2>-msgtyp.
        gs_err-msgv1  = lv_msg.
        APPEND gs_err TO gt_err.
        CLEAR gs_err.
      ELSE.
        CONCATENATE <fs_msgcoll>-msgv1 'Sample has already created'
                    INTO DATA(lv_msg1) SEPARATED BY space.

        gs_err-tcode  = <fs_msgcoll>-tcode.
        gs_err-dyname = <fs_msgcoll>-dyname.
        gs_err-dynumb = <fs_msgcoll>-dynumb.
        gs_err-msgtyp = <fs_msgcoll>-msgtyp.
        gs_err-msgv1  = lv_msg1.
        APPEND gs_err TO gt_err.
        CLEAR gs_err.
      ENDIF.
      CLEAR : gt_bdcdata,gt_msgcoll.
    ENDLOOP.

  ENDMETHOD.
  METHOD manual_inspection_lots.

    DATA : lv_count TYPE char2,
           jobname1 TYPE tbtcjob-jobname VALUE 'ZINSP'.
    CLEAR : lv_date,gt_bdcdata,lv_count.

    LOOP AT  gt_mchb ASSIGNING FIELD-SYMBOL(<fs_mchb_temp>).
      CONCATENATE  jobname lv_u <fs_mchb_temp>-charg INTO jobname1.
      CONDENSE jobname1.
      SELECT SINGLE jobname,progname FROM tbtcp
                    INTO @DATA(ls_tbtcp)
                       WHERE jobname  = @jobname1
                         AND progname = 'ZPP_INSP_LOT_CREATE'.
      IF sy-subrc = 0.
        DELETE gt_mchb WHERE charg = <fs_mchb_temp>-charg.
        CONCATENATE 'Inspection lot created for job' ls_tbtcp-jobname
              INTO DATA(lv_msgv1) SEPARATED BY ' '.
        gs_err-tcode  = 'QPR5'.
        gs_err-dyname = ' '.
        gs_err-dynumb = ' '.
        gs_err-msgtyp = 'E'.
        gs_err-msgv1  = lv_msgv1.
        APPEND gs_err TO gt_err.
        CLEAR gs_err.
      ENDIF.
    ENDLOOP.

    LOOP AT  gt_mchb ASSIGNING FIELD-SYMBOL(<fs_mchb1>).

      DO gv_count TIMES.
        lv_count = lv_count + 1.
        IF lv_count = 1.
          PERFORM bdc_dynpro      USING 'RQPRLS10'          '1000'.

          PERFORM bdc_field       USING 'BDC_CURSOR'        'S_PHYNR-LOW'.
          PERFORM bdc_field       USING 'BDC_OKCODE'        '=ONLI'.
          PERFORM bdc_field       USING 'S_PRART-LOW'       '01'.
          PERFORM bdc_field       USING 'S_PHYNR-LOW'       <fs_mchb1>-charg.

          PERFORM bdc_dynpro      USING 'SAPMSSY0'          '0120'.

          PERFORM bdc_field       USING 'BDC_CURSOR'        '04/03'.
          PERFORM bdc_field       USING 'BDC_OKCODE'        '=&ALL'.

          PERFORM bdc_dynpro      USING 'SAPMSSY0'          '0120'.

          PERFORM bdc_field       USING 'BDC_CURSOR'        '04/03'.
          PERFORM bdc_field       USING 'BDC_OKCODE'        '=XLOS'.

          PERFORM bdc_dynpro      USING 'SAPMSSY0'          '0120'.

          PERFORM bdc_field       USING 'BDC_CURSOR'        '04/03'.
          PERFORM bdc_field       USING 'BDC_OKCODE'        '=XSAV'.

          CALL TRANSACTION 'QPR5' USING gt_bdcdata
                              MODE gv_mode
                              UPDATE 'S'
                              MESSAGES  INTO gt_msgcoll.

          READ TABLE gt_msgcoll ASSIGNING FIELD-SYMBOL(<fs_msgcoll>)
                                WITH KEY msgtyp = 'E'.
          IF sy-subrc <> 0.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            READ TABLE gt_msgcoll ASSIGNING FIELD-SYMBOL(<fs_msgcoll_1>)
                                                 WITH KEY msgtyp = 'S'.
            gs_err-tcode  = <fs_msgcoll_1>-tcode.
            gs_err-dyname = <fs_msgcoll_1>-dyname.
            gs_err-dynumb = <fs_msgcoll_1>-dynumb.
            gs_err-msgtyp = <fs_msgcoll_1>-msgtyp.
            gs_err-msgv1  = 'Inspection lot created'.
            APPEND gs_err TO gt_err.
            CLEAR gs_err.
          ELSE.

            gs_err-tcode  = <fs_msgcoll>-tcode.
            gs_err-dyname = <fs_msgcoll>-dyname.
            gs_err-dynumb = <fs_msgcoll>-dynumb.
            gs_err-msgtyp = <fs_msgcoll>-msgtyp.
            gs_err-msgv1  = 'Inspection lot not created'.
            APPEND gs_err TO gt_err.
            CLEAR gs_err.
          ENDIF.
          CLEAR : gt_bdcdata,gt_msgcoll.

        ELSE.
          IF lv_date1 IS INITIAL.
            lv_date = sy-datum.
          ENDIF.

          IF p_evm IS NOT INITIAL.
            lv_date = lv_date + lv_date1 + 30.
          ENDIF.

          IF p_etwom IS NOT INITIAL.
            lv_date = lv_date + lv_date1 + 60.
          ENDIF.

          IF p_ethem IS NOT INITIAL.
            lv_date = lv_date + lv_date1 + 90.
          ENDIF.

          IF p_eform IS NOT INITIAL.
            lv_date = lv_date + lv_date1 + 120.
          ENDIF.

          CONCATENATE jobname lv_u <fs_mchb1>-charg "lv_u "lv_date
                 INTO jobname.
          CONDENSE jobname.

          CALL FUNCTION 'JOB_OPEN'
            EXPORTING
              delanfrep        = ' '
              jobgroup         = ' '
              jobname          = jobname
              sdlstrtdt        = lv_date
              sdlstrttm        = sy-uzeit
            IMPORTING
              jobcount         = jobcount
            EXCEPTIONS
              cant_create_job  = 01
              invalid_job_data = 02
              jobname_missing  = 03.
          IF sy-subrc NE 0.
            "error processing
          ENDIF.

          REFRESH: lt_seltab.
          CLEAR ls_seltab.
          ls_seltab-selname = 'P_PRART'.          " Name of parameter on submitted program
          ls_seltab-sign    = 'I'.
          ls_seltab-option  = 'EQ'.
          ls_seltab-low     = '01'.
          APPEND ls_seltab TO lt_seltab.

          CLEAR ls_seltab.
          ls_seltab-selname = 'P_PHYNR'.          " Name of parameter on submitted program
          ls_seltab-sign    = 'I'.
          ls_seltab-option  = 'EQ'.
          ls_seltab-low     = <fs_mchb1>-charg.
          APPEND ls_seltab TO lt_seltab.

          SUBMIT zpp_insp_lot_create AND RETURN
                 WITH SELECTION-TABLE lt_seltab
                                 USER sy-uname
                              VIA JOB jobname
                               NUMBER jobcount.
          IF sy-subrc > 0.
            "error processing
          ENDIF.

*close job
          starttime-sdlstrtdt = lv_date."sy-datum + 30.
          starttime-sdlstrttm = '220000'.
          lv_time = sy-uzeit + 1.

          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              jobcount             = jobcount
              jobname              = jobname
*             strtimmed            = starttimeimmediate
              laststrtdt           = lv_date
              laststrttm           = lv_time
              prddays              = '1'
              prdhours             = '1'
              prdmins              = '1'
              prdmonths            = '1'
              prdweeks             = '1'
              sdlstrtdt            = lv_date
              sdlstrttm            = sy-uzeit
            EXCEPTIONS
              cant_start_immediate = 01
              invalid_startdate    = 02
              jobname_missing      = 03
              job_close_failed     = 04
              job_nosteps          = 05
              job_notex            = 06
              lock_failed          = 07
              OTHERS               = 99.
          IF sy-subrc EQ 0.
            lv_date1 = lv_date.
            CLEAR : jobname,lv_date.
            jobname = 'ZINSP'.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_alv_instance.
    DATA : functions       TYPE REF TO cl_salv_functions_list,
           layout_settings TYPE REF TO cl_salv_layout,
           layout_key      TYPE salv_s_layout_key,
           columns         TYPE REF TO cl_salv_columns_table.


    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = gt_alv
          CHANGING
            t_table      = gt_err.
      CATCH cx_salv_msg INTO gt_message.
    ENDTRY.

* Let's show all default buttons of ALV
    functions = gt_alv->get_functions( ).
    functions->set_all( ).

    layout_settings   = gt_alv->get_layout( ).

    layout_key-report = sy-repid.
    layout_settings->set_key( layout_key ).
    layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

    columns = gt_alv->get_columns( ).

    columns->set_optimize( ).

  ENDMETHOD.
  METHOD disp_alv.
    CALL METHOD gt_alv->display.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.

  gs_bdcdata-program  = program.
  gs_bdcdata-dynpro   = dynpro.
  gs_bdcdata-dynbegin = 'X'.
  APPEND gs_bdcdata TO gt_bdcdata.
  CLEAR gs_bdcdata.
ENDFORM.                    "bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.

  gs_bdcdata-fnam = fnam.
  gs_bdcdata-fval = fval.
  APPEND gs_bdcdata TO gt_bdcdata.
  CLEAR gs_bdcdata .
ENDFORM.                    "bdc_field
*---------------------------------------------------------------------*
*     START-OF-SELECTION                                              *
*---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA lo_cl_insp TYPE REF TO lcl_insp_eval.
  CREATE OBJECT lo_cl_insp.

  lo_cl_insp->get_data( ).
  lo_cl_insp->create_sample( ).
  lo_cl_insp->manual_inspection_lots( ).
  lo_cl_insp->get_alv_instance( ).
  lo_cl_insp->disp_alv( ).

  CASE sy-ucomm.
    WHEN '&F03'.
      LEAVE LIST-PROCESSING.
  ENDCASE.
