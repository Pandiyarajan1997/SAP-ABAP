*&---------------------------------------------------------------------*
*& Include          ZMM_INFOREC_COPY_FORMS
*&---------------------------------------------------------------------*
FORM f_dynpro_read.
  CLEAR: gwa_values, gt_values.
  REFRESH gt_values.
  gwa_values-fieldname = 'P_LIST'.
  APPEND gwa_values TO gt_values.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-cprog
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = gt_values.

  READ TABLE gt_values INDEX 1 INTO gwa_values.
  IF sy-subrc = 0 AND gwa_values-fieldvalue IS NOT INITIAL.
    READ TABLE gt_list INTO gwa_list
                      WITH KEY key = gwa_values-fieldvalue.
    IF sy-subrc = 0.
      gv_selected_value = gwa_list-text.
    ENDIF.
  ENDIF.
ENDFORM.
FORM f_initial_list.
  gwa_list-key = 'L'.
  gwa_list-text = 'Lowest Value'.
  APPEND gwa_list TO gt_list.
  gwa_list-key = 'H'.
  gwa_list-text = 'Highest Value'.
  APPEND gwa_list TO gt_list.
  gwa_list-key = 'A'.
  gwa_list-text = 'Average Value'.
  APPEND gwa_list TO gt_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_LIST'
      values          = gt_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
ENDFORM.
FORM f_initial_selection.
  REFRESH: gt_mara,gt_info_log,gt_marc,gt_inre_1401,gt_inre_1005,lt_log.
  SELECT matnr FROM mara INTO TABLE gt_mara WHERE matnr IN s_matnr.
  IF sy-subrc = 0.
    SELECT * FROM zmm_inforec_log
             INTO TABLE gt_info_log
             FOR ALL ENTRIES IN gt_mara
             WHERE matnr = gt_mara-matnr
             AND type = ''.
    IF sy-subrc = 0.
      SORT gt_info_log[] BY matnr.
      lt_log[] = gt_info_log[].
      DELETE ADJACENT DUPLICATES FROM gt_info_log COMPARING matnr.
*----- Source list and condition record Fetching ----------------------------*
      SELECT a~matnr
             a~werks
             b~lifnr
             b~knumh
             c~kschl
             c~kbetr INTO CORRESPONDING FIELDS OF TABLE gt_inre_1005
                     FROM eord AS a INNER JOIN a017 AS b
                     ON a~matnr = b~matnr
                     AND a~lifnr = b~lifnr
                     JOIN konp AS c ON b~knumh = c~knumh
                     FOR ALL ENTRIES IN gt_info_log
                     WHERE a~matnr = gt_info_log-matnr
                     AND a~werks = p_fplant
                     AND a~vdatu LE sy-datum
                     AND a~bdatu GE sy-datum
                     AND b~werks EQ p_fplant
                     AND b~datab LE sy-datum
                     AND b~datbi GE sy-datum
                     AND c~kschl IN ( 'P000' , 'FRC1' )
                     AND c~loevm_ko NE 'X'.
      IF sy-subrc = 0.
        SORT gt_inre_1005[] BY matnr.
      ENDIF.
*---- Material Extension Checks in Plant Data -------------------------------*
      SELECT matnr FROM marc INTO TABLE gt_marc
                   FOR ALL ENTRIES IN gt_info_log
                   WHERE matnr EQ gt_info_log-matnr
      AND werks EQ p_plant.
      IF sy-subrc = 0.
        SORT gt_marc[] BY matnr.
      ENDIF.
*---- Fetching Inforecord Data of tech service Purchasing Organization ------*
      SELECT a~matnr
             a~werks
             a~lifnr
             b~knumh
             b~kschl
             b~kbetr INTO CORRESPONDING FIELDS OF TABLE gt_inre_1401
                     FROM a017 AS a INNER JOIN konp AS b
                     ON a~knumh = b~knumh
                     FOR ALL ENTRIES IN gt_info_log
                     WHERE a~matnr = gt_info_log-matnr
                     AND a~lifnr = p_lifnr
                     AND a~werks EQ p_plant
                     AND a~ekorg EQ p_ekorg
                     AND a~datab LE sy-datum
                     AND a~datbi GE sy-datum
                     AND b~kschl EQ 'P000'
                     AND b~loevm_ko NE 'X'.
      IF sy-subrc = 0.
        SORT gt_inre_1401[] BY matnr.
      ENDIF.
    ELSE.
      MESSAGE 'No data for Updation' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
FORM f_actual_process.
  DATA: lv_type TYPE bapi_mtype,
        lv_msg  TYPE string.
  DATA: lv_lines TYPE i.
  REFRESH gt_alv.
  DATA(lt_info_1005) = gt_inre_1005[].
  LOOP AT gt_info_log ASSIGNING FIELD-SYMBOL(<fs_info_log>).
    DATA(lv_material) = VALUE #( gt_marc[ matnr = <fs_info_log>-matnr ]-matnr OPTIONAL ).
    IF lv_material IS INITIAL.
      APPEND VALUE #( matnr = <fs_info_log>-matnr
                      fplant = <fs_info_log>-werks
                      f_org = '1000'
                      tplant = p_plant
                      t_org = p_ekorg
                      type = 'E'
                      message = |Material { <fs_info_log>-matnr } is not extended in Plant { p_plant }| ) TO gt_alv.
      CONTINUE.
    ENDIF.

    DELETE lt_info_1005 WHERE matnr NE <fs_info_log>-matnr.
    DATA(lt_info_tmp) = lt_info_1005[].
    DELETE ADJACENT DUPLICATES FROM lt_info_1005[] COMPARING matnr lifnr.
    LOOP AT lt_info_1005 ASSIGNING FIELD-SYMBOL(<fs_info_1005>).
      "P000 Condition Prices in Info record of from Plant
      DATA(lv_fprice) = VALUE #( lt_info_tmp[ matnr = <fs_info_1005>-matnr
                                             lifnr = <fs_info_1005>-lifnr
                                             knumh = <fs_info_1005>-knumh
                                             kschl = 'P000' ]-kbetr OPTIONAL ).
      "FRC1 Condition Price in Infor Record of from Plant
      DATA(lv_fri_price) = VALUE #( lt_info_tmp[ matnr = <fs_info_1005>-matnr
                                                 lifnr = <fs_info_1005>-lifnr
                                                 knumh = <fs_info_1005>-knumh
                                                 kschl = 'FRC1' ]-kbetr OPTIONAL ).
      DATA(lv_amount) = ( lv_fprice + lv_fri_price ).
      <fs_info_1005>-f_amnt = lv_amount.
      CLEAR: lv_amount,lv_fprice,lv_fri_price.
    ENDLOOP.
*--------- Lowest value calculation for Updating Inforecord ----------------------------------------------*
    CLEAR lv_lines.
    IF gwa_list-key = 'L'.
      SORT lt_info_1005[] BY f_amnt ASCENDING.
      DESCRIBE TABLE lt_info_1005[] LINES lv_lines.
      DELETE ADJACENT DUPLICATES FROM lt_info_1005 COMPARING matnr.
*--------- Highest value calculation for Updating Inforecord ----------------------------------------------*
    ELSEIF gwa_list-key = 'H'.
      SORT lt_info_1005[] BY f_amnt DESCENDING.
      DESCRIBE TABLE lt_info_1005[] LINES lv_lines.
      DELETE ADJACENT DUPLICATES FROM lt_info_1005 COMPARING matnr.
*--------- Average value calculation for Updating Inforecord ----------------------------------------------*
    ELSEIF gwa_list-key = 'A'.
      CLEAR lv_unit_price.
      LOOP AT lt_info_1005 ASSIGNING FIELD-SYMBOL(<fs_avg_cal>).
        lv_lines = lv_lines + 1.
        lv_unit_price = ( lv_unit_price + <fs_avg_cal>-f_amnt ).
      ENDLOOP.
      lv_unit_price = lv_unit_price / lv_lines.
    ENDIF.

    DATA(ls_to_plant) = VALUE #( gt_inre_1401[ matnr = <fs_info_log>-matnr ] OPTIONAL ).
    IF gwa_list-key = 'L' OR gwa_list-key = 'H'.
      DATA(ls_final_price) = VALUE #( lt_info_1005[ matnr = <fs_info_log>-matnr ] OPTIONAL ).
      CLEAR lv_unit_price.
      lv_unit_price =  ls_final_price-f_amnt.
    ENDIF.
    lv_unit_price = CONV netpr( lv_unit_price ).
*--------- If No Inforecord Presents for Tech Services ---------------------------------------------------*
    IF ls_to_plant IS INITIAL.
      "Creation of Info Record in To Plant
      PERFORM f_cretae_inforecord USING <fs_info_log>-matnr lv_unit_price CHANGING lv_type lv_msg.
      IF lv_type = 'E'."Error Scenerio of Creation
        APPEND VALUE #( matnr = <fs_info_log>-matnr
                        fplant = <fs_info_log>-werks
                        f_org = '1000'
                        lifnr = ls_final_price-lifnr
                        tplant = p_plant
                        t_org = p_ekorg
                        vendor = p_lifnr
                        o_price = ls_to_plant-kbetr
                        n_price = ls_final_price-f_amnt
                        count = lv_lines
                        type = 'E'
                        message = lv_msg ) TO gt_alv.
      ELSE. "Success Scenerio Of creation
        APPEND VALUE #( matnr = <fs_info_log>-matnr
                        fplant = <fs_info_log>-werks
                        f_org = '1000'
                        lifnr = ls_final_price-lifnr
                        tplant = p_plant
                        t_org = p_ekorg
                        vendor = p_lifnr
                        o_price = ls_to_plant-kbetr
                        n_price = lv_unit_price
                        count = lv_lines
                        type = 'S'
                        message = | Info record Created Succesfully | ) TO gt_alv.
*---- Function module to lock the log table for Updating ----------------*
        CALL FUNCTION 'ENQUEUE_EZMM_INFOLOG' "Enqueue Table
          EXPORTING
            mode_zmm_inforec_log = 'E'
            mandt                = sy-mandt
            matnr                = <fs_info_log>-matnr
            supplier             = ls_final_price-lifnr
            werks                = <fs_info_log>-werks
            erdat                = <fs_info_log>-erdat
          EXCEPTIONS
            foreign_lock         = 1
            system_failure       = 2
            OTHERS               = 3.
        IF sy-subrc <> 0.
          MESSAGE 'Object is locked by other user' TYPE 'S'.
        ELSE.
          IF gwa_list-key = 'L' OR gwa_list-key = 'H'.
            UPDATE zmm_inforec_log SET plant = p_plant
                                       lifnr = p_lifnr
                                       type = 'S'
                                       message = 'Info Record Created Successfully'
                                       WHERE matnr = <fs_info_log>-matnr
                                       AND supplier = ls_final_price-lifnr
                                       AND werks = <fs_info_log>-werks
                                       AND erdat = <fs_info_log>-erdat.
            LOOP AT lt_log INTO DATA(ls_log) WHERE matnr = <fs_info_log>-matnr
                                             AND werks = <fs_info_log>-werks
                                             AND supplier NE ls_final_price-lifnr
                                             AND erdat EQ sy-datum.
              ls_log-type = 'X'.
              ls_log-message = 'Its Not Considerable'.
              MODIFY zmm_inforec_log FROM ls_log.
            ENDLOOP.
          ELSE. "Average Price Updation in Log table
            DATA(ls_average1) = VALUE zmm_inforec_log( matnr =  <fs_info_log>-matnr
                                                      werks = <fs_info_log>-werks
                                                      plant = p_plant
                                                      lifnr = p_lifnr
                                                      type = 'A'
                                                      erdat = sy-datum
                                                      price = lv_unit_price
                                                      message = 'Average Price Created successfully' ).
            MODIFY zmm_inforec_log FROM ls_average1.
          ENDIF.
        ENDIF.
        CALL FUNCTION 'DEQUEUE_EZMM_INFOLOG' "Dequeue Table
          EXPORTING
            mode_zmm_inforec_log = 'E'
            mandt                = sy-mandt
            matnr                = <fs_info_log>-matnr
            supplier             = ls_final_price-lifnr
            werks                = <fs_info_log>-werks
            erdat                = <fs_info_log>-erdat.
      ENDIF.
    ELSE. " If Info Record Presents and prices are Same in both plants
      IF lv_unit_price EQ ls_to_plant-kbetr.
        APPEND VALUE #( matnr = <fs_info_log>-matnr
                        fplant = <fs_info_log>-werks
                        f_org = '1000'
                        lifnr = ls_final_price-lifnr
                        tplant = p_plant
                        t_org = p_ekorg
                        vendor = p_lifnr
                        o_price = ls_to_plant-kbetr
                        n_price = lv_unit_price
                        count = lv_lines
                        type = 'S'
                        message = | Old and New Prices are Same| ) TO gt_alv.
        CONTINUE.
      ELSE.
        "If Inforecord exists but the Prices or Different then Updation will happen
        PERFORM f_change_inforecord USING <fs_info_log>-matnr lv_unit_price
                                          <fs_info_log>-prdat CHANGING lv_type lv_msg.
        IF lv_type = 'E'.
          APPEND VALUE #( matnr = <fs_info_log>-matnr
                          fplant = <fs_info_log>-werks
                          f_org = '1000'
                          lifnr = ls_final_price-lifnr
                          tplant = p_plant
                          t_org = p_ekorg
                          vendor = p_lifnr
                          o_price = ls_to_plant-kbetr
                          n_price = lv_unit_price
                          count = lv_lines
                          type = 'E'
                          message = lv_msg ) TO gt_alv.
          CONTINUE.
        ELSE.
          APPEND VALUE #( matnr = <fs_info_log>-matnr
                         fplant = <fs_info_log>-werks
                         f_org = '1000'
                         lifnr = ls_final_price-lifnr
                         tplant = p_plant
                         t_org = p_ekorg
                         vendor = p_lifnr
                         o_price = ls_to_plant-kbetr
                         n_price = lv_unit_price
                         count = lv_lines
                         type = 'S'
                         message = lv_msg ) TO gt_alv.
*---- Function module to lock the log table for Updating ----------------*
          CALL FUNCTION 'ENQUEUE_EZMM_INFOLOG' "Enqueue Table
            EXPORTING
              mode_zmm_inforec_log = 'E'
              mandt                = sy-mandt
              matnr                = <fs_info_log>-matnr
              supplier             = ls_final_price-lifnr
              werks                = <fs_info_log>-werks
              erdat                = <fs_info_log>-erdat
            EXCEPTIONS
              foreign_lock         = 1
              system_failure       = 2
              OTHERS               = 3.
          IF sy-subrc <> 0.
            MESSAGE 'Object is locked by other user' TYPE 'S'.
          ELSE.
            IF gwa_list-key = 'L' OR gwa_list-key = 'H'.
              UPDATE zmm_inforec_log SET plant = p_plant
                                         lifnr = p_lifnr
                                         type = 'S'
                                         message = 'Info record Updated Successfully'
                                         WHERE matnr = <fs_info_log>-matnr
                                         AND supplier = ls_final_price-lifnr
                                         AND werks = <fs_info_log>-werks
                                         AND erdat = <fs_info_log>-erdat.
              LOOP AT lt_log INTO DATA(ls_log1) WHERE matnr = <fs_info_log>-matnr
                                               AND werks = <fs_info_log>-werks
                                               AND supplier NE ls_final_price-lifnr
                                               AND erdat EQ sy-datum.
                ls_log1-type = 'X'.
                ls_log1-message = 'Its Not Considerable'.
                MODIFY zmm_inforec_log FROM ls_log1.
              ENDLOOP.
            ELSE. "Average Price Updation in Log table
              DATA(ls_average) = VALUE zmm_inforec_log( matnr =  <fs_info_log>-matnr
                                                        werks = <fs_info_log>-werks
                                                        plant = p_plant
                                                        lifnr = p_lifnr
                                                        type = 'A'
                                                        erdat = sy-datum
                                                        price = lv_unit_price
                                                        message = 'Average Price Updated successfully' ).
              MODIFY zmm_inforec_log FROM ls_average.
            ENDIF.
          ENDIF.
          CALL FUNCTION 'DEQUEUE_EZMM_INFOLOG' "Dequeue Table
            EXPORTING
              mode_zmm_inforec_log = 'E'
              mandt                = sy-mandt
              matnr                = <fs_info_log>-matnr
              supplier             = ls_final_price-lifnr
              werks                = <fs_info_log>-werks
              erdat                = <fs_info_log>-erdat.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: lv_msg,lv_type,ls_log,ls_log1.
  ENDLOOP.
ENDFORM.

FORM f_cretae_inforecord USING pi_matnr pi_price CHANGING p_type p_msg.
  REFRESH: gt_bdcdata,gt_bdcmsg.
  CLEAR: lv_msg_text,lv_amount.
  lv_amount = pi_price.
*  CONDENSE lv_amount NO-GAPS.
  DATA: lv_datum(10) TYPE c,
        lv_date1(10) TYPE c.
  CLEAR: lv_datum,lv_date1.
  lv_date1 = '31.12.9999'.
  WRITE sy-datum TO lv_datum DD/MM/YYYY.
*  WRITE '99991231' TO lv_date1 DD/MM/YYYY.
*  CONDENSE lv_amount NO-GAPS.
  DATA(ls_options) = VALUE ctu_params(  dismode = 'N'
                                        updmode = 'S'
                                        defsize = ''
                                        nobinpt = 'X'
                                        racommit = 'X' ).
  DATA(lv_tcode) = 'ME11'.

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-LIFNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'EINA-LIFNR'
                                 p_lifnr.
  PERFORM bdc_field       USING 'EINA-MATNR'
                                 pi_matnr.
  PERFORM bdc_field       USING 'EINE-EKORG'
                                 p_ekorg.
  PERFORM bdc_field       USING 'EINE-WERKS'
                                 p_plant.
  PERFORM bdc_field       USING 'RM06I-NORMB'
                                'X'.
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-INFNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=EINE'.
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINE-MWSKZ'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KO'.
  PERFORM bdc_field       USING 'EINE-APLFZ'
                                '10'.
  PERFORM bdc_field       USING 'EINE-EKGRP'
                                'TEC'.
  PERFORM bdc_field       USING 'EINE-NORBM'
                                '1'.
  PERFORM bdc_field       USING 'EINE-WEBRE'
                                'X'.
  PERFORM bdc_field       USING 'EINE-IPRKZ'
                                'D'.
  PERFORM bdc_field       USING 'EINE-MWSKZ'
                                'PD'.
  PERFORM bdc_field       USING 'EINE-NETPR'
                                 pi_price.
  PERFORM bdc_field       USING 'EINE-WAERS'
                                'INR'.
  PERFORM bdc_field       USING 'EINE-PEINH'
                                '1'.
  PERFORM bdc_field       USING 'EINE-BPRME'
                                'KG'.
  PERFORM bdc_field       USING 'EINE-BPUMZ'
                                '1'.
  PERFORM bdc_field       USING 'EINE-BPUMN'
                                '1'.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-DATAB'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
  PERFORM bdc_field       USING 'RV13A-DATAB'
                                 lv_datum.
  PERFORM bdc_field       USING 'RV13A-DATBI'
                                 lv_date1.
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
    p_type = 'E'.
    p_msg = lv_msg_text.
  ELSE.
    p_type = 'S'.
    p_msg = |Info record Created Successfully|.
  ENDIF.
ENDFORM.
FORM f_change_inforecord USING pi_matnr pi_price p_validity CHANGING p_type p_msg.
  DATA: lv_datum(10) TYPE c.
  REFRESH: gt_bdcdata,gt_bdcmsg.
  CLEAR: lv_msg_text,lv_amount,lv_date,lv_datum.
*  CONDENSE lv_amount NO-GAPS.
  WRITE p_validity to lv_date DD/MM/YYYY.
  WRITE sy-datum TO lv_datum DD/MM/YYYY.
  DATA(ls_options) = VALUE ctu_params(  dismode = 'N'
                                        updmode = 'S'
                                        defsize = ''
                                        nobinpt = 'X'
                                        racommit = 'X' ).
  DATA(lv_tcode) = 'ME12'.

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-LIFNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'EINA-LIFNR'
                                 p_lifnr.
  PERFORM bdc_field       USING 'EINA-MATNR'
                                 pi_matnr.
  PERFORM bdc_field       USING 'EINE-EKORG'
                                 p_ekorg.
  PERFORM bdc_field       USING 'EINE-WERKS'
                                p_plant.
  PERFORM bdc_field       USING 'RM06I-NORMB'
                                'X'.
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-MAHN1'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KO'.
  PERFORM bdc_field       USING 'EINA-MEINS'
                                'KG'.
  PERFORM bdc_field       USING 'EINA-UMREZ'
                                '1'.
  PERFORM bdc_field       USING 'EINA-UMREN'
                                '1'.
  PERFORM bdc_dynpro      USING 'SAPLV14A' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VAKE-DATBI(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PICK'.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONP-KBETR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
  PERFORM bdc_field       USING 'RV13A-DATAB'
                                lv_datum.
  PERFORM bdc_field       USING 'RV13A-DATBI'
                                lv_date.
  PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                pi_price.
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
    p_type = 'E'.
    p_msg = lv_msg_text.
  ELSE.
    p_type = 'S'.
    p_msg = |Info record Updated Successfully|.
  ENDIF.
ENDFORM.
FORM f_display_alv.
  DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
        lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
        lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
        lo_content     TYPE REF TO cl_salv_form_element,
        lv_title       TYPE string,
        lv_rows        TYPE string.

  DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

  DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
        lo_columns    TYPE REF TO cl_salv_columns,
        lo_column     TYPE REF TO cl_salv_column_table.

  IF gt_info_log IS NOT INITIAL.
* Create the ALV object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.
  ENDIF.

  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

  TRY.
      lo_column ?= lo_columns->get_column( 'FPLANT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'From Plant' ).
      lo_column->set_medium_text( 'From Plant' ).
      lo_column->set_short_text( 'Fromplnt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'F_ORG' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'From Porg' ).
      lo_column->set_medium_text( 'From Porg' ).
      lo_column->set_short_text( 'FromPorg' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'LIFNR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'From Vendor' ).
      lo_column->set_medium_text( 'From Vendor' ).
      lo_column->set_short_text( 'From Vend' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TPLANT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'To Plant' ).
      lo_column->set_medium_text( 'To Plant' ).
      lo_column->set_short_text( 'Toplnt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'T_ORG' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'To Porg' ).
      lo_column->set_medium_text( 'To Porg' ).
      lo_column->set_short_text( 'ToPorg' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VENDOR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'To Vendor' ).
      lo_column->set_medium_text( 'To Vendor' ).
      lo_column->set_short_text( 'To Vend' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'O_PRICE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Old Price' ).
      lo_column->set_medium_text( 'Old Price' ).
      lo_column->set_short_text( 'Oldpri' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'N_PRICE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'New Price' ).
      lo_column->set_medium_text( 'New Price' ).
      lo_column->set_short_text( 'Newpri' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'COUNT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'No.of.Vendors' ).
      lo_column->set_medium_text( 'NOV' ).
      lo_column->set_short_text( 'NOV' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TYPE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Msgtype' ).
      lo_column->set_medium_text( 'Msgtyp' ).
      lo_column->set_short_text( 'Msgtyp' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'MESSAGE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Message' ).
      lo_column->set_medium_text( 'Message' ).
      lo_column->set_short_text( 'Message' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.
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
FORM bdc_field  USING  fnam TYPE any
                      fval TYPE any.
  CLEAR:gw_bdcdata.
  gw_bdcdata-fnam = fnam.
  gw_bdcdata-fval = fval.
  APPEND gw_bdcdata TO gt_bdcdata.
ENDFORM.
