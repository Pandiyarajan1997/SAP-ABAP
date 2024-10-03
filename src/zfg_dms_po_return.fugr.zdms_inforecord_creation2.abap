FUNCTION zdms_inforecord_creation2.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_INPUT) TYPE  ZDMS_INFORECORD_ST
*"  EXPORTING
*"     REFERENCE(MSG_TYPE) TYPE  BAPI_MTYPE
*"     REFERENCE(MSG) TYPE  STRING
*"--------------------------------------------------------------------
  "Created by: Pandiarajan
  "Created On: 13.03.2024
  "Reference: Ramakrishnan J & Gopalraja
  "Purpose: Inforecord Creation
*-----------------------------------------------------------------------

  DATA(l_input) = im_input.
  REFRESH: gt_bdcdata,gt_bdcmsg.
  CLEAR: lv_datum,lv_date1.
  lv_date1 = '31.12.9999'.

  IF l_input-valid_from IS INITIAL.

    l_input-valid_from = sy-datum.

  ENDIF.
  WRITE l_input-valid_from TO lv_datum.
  WRITE lv_date1 TO lv_date1.

  "Options for BDC Processing
  DATA(ls_options) = VALUE ctu_params( dismode = 'N'
                                       updmode = 'S' ).
*                                       defsize = ''
*                                       nobinpt = 'X'
*                                       racommit = 'X' ).
  "Tcode for Inforecord Creation
  DATA(lv_tcode) = 'ME11'.
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-LIFNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'EINA-LIFNR'
                                l_input-lifnr.         "'10002452'.
  PERFORM bdc_field       USING 'EINA-MATNR'
                                l_input-matnr.         "'NCAFA001C1'.
  PERFORM bdc_field       USING 'EINE-EKORG'
                                l_input-ekorg.
  PERFORM bdc_field       USING 'EINE-WERKS'
                                l_input-werks.         "'D085'.
  PERFORM bdc_field       USING 'RM06I-NORMB'
                                'X'.
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-INFNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINE-NETPR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'EINE-APLFZ'
                                '1'.
  PERFORM bdc_field       USING 'EINE-EKGRP'
                                l_input-ekgrp.
  PERFORM bdc_field       USING 'EINE-NORBM'
                                '1'.
  PERFORM bdc_field       USING 'EINE-IPRKZ'
                                'D'.
  PERFORM bdc_field       USING 'EINE-MWSKZ'
                                l_input-mwskz.         "'PD'.
  PERFORM bdc_field       USING 'EINE-NETPR'
                                l_input-unit_price.    "'          2555'.
  PERFORM bdc_field       USING 'EINE-WAERS'
                                'INR'.
  PERFORM bdc_field       USING 'EINE-PEINH'
                                '1'.
  PERFORM bdc_field       USING 'EINE-BPRME'
                                l_input-vrkme.
  PERFORM bdc_field       USING 'EINE-BPUMZ'
                                '1'.
  PERFORM bdc_field       USING 'EINE-BPUMN'
                                '1'.
*****************************************************
*  PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'EINA-MATNR'.
**  PERFORM bdc_field       USING 'BDC_OKCODE'
**                                '/00'.
*  PERFORM bdc_field       USING 'EINA-LIFNR'
*                                 l_input-lifnr.
*  PERFORM bdc_field       USING 'EINA-MATNR'
*                                 l_input-matnr.
*  PERFORM bdc_field       USING 'EINE-EKORG'
*                                'DMS1'.
*  PERFORM bdc_field       USING 'EINE-WERKS'
*                                 l_input-werks.
*  PERFORM bdc_field       USING 'RM06I-NORMB'
*                                'X'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'EINA-MAHN1'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=EINE'.
*  PERFORM bdc_field       USING 'EINA-URZLA'
*                                'IN'.
*  PERFORM bdc_field       USING 'EINA-REGIO'
*                                'TN'.
*  PERFORM bdc_field       USING 'EINA-TELF1'
*                                '9445866583'.
*  PERFORM bdc_field       USING 'EINA-MEINS'
*                                 l_input-vrkme.
*  PERFORM bdc_field       USING 'EINA-UMREZ'
*                                '1'.
*  PERFORM bdc_field       USING 'EINA-UMREN'
*                                '1'.
*  PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'EINE-NETPR'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=KO'.
*  PERFORM bdc_field       USING 'EINE-APLFZ'
*                                '1'.
*  PERFORM bdc_field       USING 'EINE-EKGRP'
*                                'DMS'.
*  PERFORM bdc_field       USING 'EINE-NORBM'
*                                '1'.
*  PERFORM bdc_field       USING 'EINE-IPRKZ'
*                                'D'.
*  PERFORM bdc_field       USING 'EINE-MWSKZ'
*                                 l_input-mwskz.
*  PERFORM bdc_field       USING 'EINE-NETPR'
*                                 l_input-unit_price.
*  PERFORM bdc_field       USING 'EINE-WAERS'
*                                'INR'.
*  PERFORM bdc_field       USING 'EINE-PEINH'
*                                '1'.
*  PERFORM bdc_field       USING 'EINE-BPRME'
*                                 l_input-vrkme.
*  PERFORM bdc_field       USING 'EINE-BPUMZ'
*                                '1'.
*  PERFORM bdc_field       USING 'EINE-BPUMN'
*                                '1'.
*  PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'RV13A-DATBI'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=SICH'.
*  PERFORM bdc_field       USING 'RV13A-DATAB'
*                                lv_datum.
*  PERFORM bdc_field       USING 'RV13A-DATBI'
*                                lv_date1.

  TRY.
      CALL TRANSACTION lv_tcode WITH AUTHORITY-CHECK
                              USING gt_bdcdata OPTIONS FROM ls_options
                              MESSAGES INTO gt_bdcmsg.
    CATCH cx_sy_authorization_error ##NO_HANDLER.
  ENDTRY.

  COMMIT WORK AND WAIT.
  READ TABLE gt_bdcmsg INTO gw_bdcmsg WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0.
    LOOP AT gt_bdcmsg INTO gw_bdcmsg WHERE msgtyp = 'E'.
      CLEAR lv_msg_text.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = gw_bdcmsg-msgid
          lang      = sy-langu
          no        = gw_bdcmsg-msgnr
          v1        = gw_bdcmsg-msgv1
          v2        = gw_bdcmsg-msgv2
          v3        = gw_bdcmsg-msgv3
          v4        = gw_bdcmsg-msgv4
        IMPORTING
          msg       = lv_msg_text
        EXCEPTIONS
          not_found = 01.
      lv_msg_text = |{ lv_msg_text } { lv_msg_text }|.
    ENDLOOP.
    msg_type = 'E'.
    msg = lv_msg_text.
  ELSE.
    msg_type = 'S'.
    msg = |Info record Created Successfully|.
  ENDIF.

ENDFUNCTION.
