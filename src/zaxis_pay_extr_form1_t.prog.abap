* *&---------------------------------------------------------------------*
*&  Include           ZAXIS_PAY_EXTR_FORM1
*======================================================================*
*       text          ALL SELECT QUERIES
*======================================================================*


FORM select_data .

  PERFORM fill_document_types.           " IF DOCUMENT TYPE RESTICTION REQUIRED.

  PERFORM fill_doc_post_user_list.       " AUTHORIZED USERS POSTED DOCUMENTS ONLY NEED TO PIC

  PERFORM read_bsis_bsas_reguh.

  PERFORM read_bsak_reguh.

  PERFORM add_cross_company_payments.     " Add Cross Company related payments

  PERFORM add_customer_payments.          " Add Customer adjustment etc payments

  PERFORM collect_residual_payments.      " Add residual payments

  PERFORM collect_partial_payments.       " Add Partial Payments

  PERFORM segrigate_payment_invoice.      " Separate Payment Invoice ito diff tables

  PERFORM read_regup.


  IF  g_tab_bsak_pymnt[] IS INITIAL.

    EXIT.

  ENDIF.

  PERFORM update_inv_details.           " Update Payment line with Invoice details

  PERFORM payment_method_updating.      " Update Pay Method from invoice etc

  PERFORM read_payr.

  PERFORM read_lfa1_lfb1_lfbk.

  PERFORM read_bsec.

  PERFORM read_adrc.

  PERFORM read_bnka.

  PERFORM read_t012_t012k.

  PERFORM read_bseg.

  PERFORM read_with_item.

*  PERFORM READ_PRINT_LOC.

  PERFORM collect_bsak_pymnt.

  PERFORM validate_payments.          " VALIDATE SELECTED PAYMENTS.

ENDFORM.                    " select_data


*&----------------------------------------------------------------------------------------*
*&      Form  FILL_DOCUMENT_TYPES
*&----------------------------------------------------------------------------------------*
*       text
*-----------------------------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*-----------------------------------------------------------------------------------------*
FORM fill_document_types .
  CLEAR : wa_convers.
  READ TABLE git_convers INTO wa_convers WITH KEY bank_code   = c_bank_code
                                                  record_type = 'H'
                                                  fieldname   = 'VEND'
                                                  oldvalue    = 'DOC_TYPE'.
  IF sy-subrc = 0 AND  wa_convers-newvalue = 'Y'.

    SELECT * FROM zaxis_doc_types INTO TABLE git_doc_types
                                        WHERE koart = 'K'.

    LOOP AT git_doc_types INTO wa_doc_types.

      wa_blart-sign   = 'I'.
      wa_blart-option = 'EQ'.
      wa_blart-low    = wa_doc_types-blart.
      APPEND wa_blart TO r_blart.
      CLEAR : wa_doc_types,wa_blart.

    ENDLOOP.
  ENDIF.
  CLEAR : wa_convers ,wa_doc_types.
ENDFORM.                    " FILL_DOCUMENT_TYPES



*&----------------------------------------------------------------------------------------*
*&      Form  FILL_DOC_POST_USER_LIST
*&----------------------------------------------------------------------------------------*
*       text
*-----------------------------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*-----------------------------------------------------------------------------------------*
FORM fill_doc_post_user_list .

  CLEAR : wa_convers.
  READ TABLE git_convers INTO wa_convers WITH KEY bank_code   = c_bank_code
                                                  record_type = 'H'
                                                  fieldname   = 'VEND'
                                                  oldvalue    = 'POST_USER'.
  IF sy-subrc = 0 AND  wa_convers-newvalue = 'Y'.

    SELECT * FROM zaxis_post_users INTO TABLE git_post_users
                                        WHERE bukrs = p_bukrs.

    LOOP AT git_post_users INTO wa_post_users.

      wa_usnam-sign   = 'I'.
      wa_usnam-option = 'EQ'.
      wa_usnam-low    = wa_post_users-user_name.
      APPEND wa_usnam TO r_usnam.
      CLEAR : wa_usnam,wa_post_users.

    ENDLOOP.
  ENDIF.
  CLEAR : wa_convers ,wa_post_users.
ENDFORM.                    " FILL_DOC_POST_USER_LIST



*&----------------------------------------------------------------------------------------*
*&      Form  read_bsis_bsas_reguh
*&----------------------------------------------------------------------------------------*
*       text
*-----------------------------------------------------------------------------------------*
FORM read_bsis_bsas_reguh .

  DATA : lt_bkpf_tmp TYPE TABLE OF bkpf.
  DATA : wa_vblnr LIKE LINE OF s_vblnr.

*Added by Ramakrishnan 704322
  DATA: ls_invoice_park TYPE zca_invoice_park,
        ls_deltabix     TYPE sy-tabix.


  CASE sy-tcode.

    WHEN c_tcode_extr.                 "Extraction
*-----------------------------------------------------------------------------------------*
*------------------ For automatic payments extraction-----------------------------------***
*-----------------------------------------------------------------------------------------*

      IF p_auto = abap_true.  "'X'.                                  "Automatic

        SELECT * FROM reguh INTO TABLE g_tab_reguh
                              WHERE  laufd IN s_laufd
                                AND laufi IN s_laufi
                                AND xvorl = ' '
                                AND zbukr = p_bukrs
                                AND vblnr IS NOT NULL
                                AND srtgb IN s_gsber
                                AND hktid IN s_hktid
                                AND hbkid IN s_hbkid.

        IF g_tab_reguh[] IS INITIAL.
          PERFORM pop_up_error USING TEXT-111.          "No Data Found, please Check the Input Data Provided.
        ENDIF.


        IF g_tab_zaxis_tab_ctrltab[] IS NOT INITIAL.
          LOOP AT g_tab_reguh INTO reguh.                 "deletee already extracted data from  g_tab_regh table

            READ TABLE g_tab_zaxis_tab_ctrltab INTO wa_zaxis_tab_ctrltab
                                              WITH KEY belnr = reguh-vblnr
                                                       bukrs = reguh-zbukr
                                                       laufd = reguh-laufd
                                                       laufi = reguh-laufi.
            IF sy-subrc EQ 0.
              DELETE g_tab_reguh.
            ENDIF.
            CLEAR : reguh,wa_zaxis_tab_ctrltab.
          ENDLOOP.
        ENDIF.

        IF g_tab_reguh[] IS INITIAL.
          PERFORM pop_up_error USING TEXT-112.          "DATA ALREADY EXTRACTED.PLEASE CHECK
        ENDIF.

        IF g_tab_reguh[] IS NOT INITIAL .            "*SELEC THE CONTROL RECORDS FROM THE TBLE REGUV

          SELECT DISTINCT * FROM reguv INTO TABLE g_tab_reguv FOR ALL
                 ENTRIES IN g_tab_reguh WHERE laufd = g_tab_reguh-laufd
                                        AND laufi = g_tab_reguh-laufi
                                        AND xvore = c_xvore
                                        AND xecht NE 'X'.
          IF g_tab_reguv[] IS NOT INITIAL.
            LOOP AT g_tab_reguh INTO reguh.           "*DELETE THE PROPOSAL RUN RECORDS
              READ TABLE g_tab_reguv TRANSPORTING NO FIELDS WITH KEY laufd = reguh-laufd
                                                                     laufi = reguh-laufi
                                                                     xvore = c_xvore.
              IF sy-subrc = 0.
                DELETE g_tab_reguh.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
*-----------------------------------------------------------------------------------------*
*------------------fetch Bkpf Data based on Reguh table---------------------------------***
*-----------------------------------------------------------------------------------------*

        IF  g_tab_reguh[] IS NOT INITIAL.

          SELECT * FROM bkpf INTO TABLE g_tab_bkpf
                                              FOR ALL ENTRIES IN g_tab_reguh
                                               WHERE bukrs EQ g_tab_reguh-zbukr
                                                 AND belnr EQ g_tab_reguh-vblnr
                                                 AND budat EQ g_tab_reguh-zaldt
                                                 AND blart IN r_blart
                                                 AND usnam IN r_usnam
                                                 AND stblg = space.



*-----------------------------------------------------------------------------------------*
**          ""Fetch all the records without validation to give error description
**-----------------------------------------------------------------------------------------*

          SELECT * FROM bkpf INTO TABLE lt_bkpf_tmp FOR ALL ENTRIES IN g_tab_reguh
                                            WHERE bukrs EQ g_tab_reguh-zbukr
                                              AND belnr EQ g_tab_reguh-vblnr
                                              AND budat EQ g_tab_reguh-zaldt
                                              AND stblg = space.

          CLEAR : bkpf.
          LOOP AT lt_bkpf_tmp INTO bkpf.
            READ TABLE g_tab_bkpf TRANSPORTING NO FIELDS WITH KEY
                                               bukrs = bkpf-bukrs
                                               belnr = bkpf-belnr
                                               gjahr = bkpf-gjahr.
            IF sy-subrc NE 0.
              MOVE-CORRESPONDING bkpf TO bsak1.
              bsak1-augbl = bkpf-belnr.
              PERFORM append_display USING TEXT-e23     " Not Allowed
                                           TEXT-126    "Doc posted user or doc type was not maintained ZAXIS tables.
                                           c_nrm_colour.
              CLEAR : bsak1.
            ENDIF.
            CLEAR : bkpf.
          ENDLOOP.


**--------CHECK WHETHER THIS DOCUMENTS ARE EXTRACTED OR NOT-------------------------------*
***--------------BASED ON FISICAL YEAR AND DOC NO-----------------------------------------*

          IF g_tab_bkpf[] IS NOT INITIAL.
            SELECT * FROM zaxis_tab_ctlta1 INTO TABLE git_ctrltab_temp
                                              FOR ALL ENTRIES IN g_tab_bkpf
                                                 WHERE bukrs = p_bukrs
                                                  AND belnr EQ g_tab_bkpf-belnr
                                                  AND gjahr EQ g_tab_bkpf-gjahr.
            IF git_ctrltab_temp[] IS NOT INITIAL.
              LOOP AT g_tab_bkpf INTO wa_tab_bkpf.
                READ TABLE git_ctrltab_temp TRANSPORTING NO FIELDS WITH KEY
                                                        bukrs = wa_tab_bkpf-bukrs
                                                        belnr = wa_tab_bkpf-belnr
                                                        gjahr = wa_tab_bkpf-gjahr.
                IF sy-subrc = 0.
                  DELETE g_tab_bkpf.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.


*-----------------------------------------------------------------------------------------*
**          ""Delete already extracted data from reguh based on bkpf
**-----------------------------------------------------------------------------------------*
          IF g_tab_bkpf[] IS NOT INITIAL.

            LOOP AT g_tab_reguh INTO wa_g_tab_reguh.

              READ TABLE g_tab_bkpf INTO wa_tab_bkpf WITH KEY belnr = wa_g_tab_reguh-vblnr
                                                              bukrs = wa_g_tab_reguh-zbukr
                                                              budat = wa_g_tab_reguh-zaldt.
              IF sy-subrc NE 0.
                DELETE g_tab_reguh.
              ENDIF.
            ENDLOOP.

          ELSE.
            IF lt_bkpf_tmp[] IS NOT INITIAL.
              PERFORM pop_up_error USING TEXT-126.  "Doc posted user or doc type was not maintained ZAXIS tables.
            ELSE.
              PERFORM pop_up_error USING TEXT-118.  "DATA ALREADY EXTRACTED.PLEASE CHECK
            ENDIF.
          ENDIF.
*-----------------------------------------------------------------------------------------*
*------------------fetch BSIS Data based on Reguh table---------------------------------***
*-----------------------------------------------------------------------------------------*
          IF g_tab_reguh[] IS NOT INITIAL.
            SELECT * FROM bsis INTO TABLE g_tab_bsis
                                             FOR ALL ENTRIES IN g_tab_reguh
                                               WHERE bukrs = g_tab_reguh-zbukr
                                                 AND hkont IN r_hkont
                                                 AND belnr = g_tab_reguh-vblnr
                                                 AND budat = g_tab_reguh-zaldt
                                                 AND gsber IN s_gsber
                                                 AND prctr IN s_prctr.

            SELECT * FROM bsas INTO TABLE g_tab_bsas
                                           FOR ALL ENTRIES IN g_tab_reguh
                                             WHERE bukrs = g_tab_reguh-zbukr
                                               AND hkont IN r_hkont
                                               AND belnr = g_tab_reguh-vblnr
                                               AND budat = g_tab_reguh-zaldt
                                               AND gsber IN s_gsber
                                               AND prctr IN s_prctr.
          ELSE.
            PERFORM pop_up_error USING TEXT-124 .           "Documents might be reversed or Input details might be wrong
            EXIT.
          ENDIF.
          IF g_tab_bsis[] IS INITIAL AND g_tab_bsas[] IS INITIAL.
            PERFORM pop_up_error USING TEXT-113 .           "Please make payment with correct G/L Account
            EXIT.
          ENDIF.

        ENDIF.      "g_tab_reguh[] IS NOT INITIAL
*-----------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------*
*------------------ For Manual payments extraction--------------------------------***
*-----------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------*
      ELSE.
        SELECT * FROM bkpf INTO TABLE g_tab_bkpf
                                                 WHERE bukrs = p_bukrs
                                                   AND belnr IN s_vblnr
                                                   AND gjahr = p_gjahr
                                                   AND budat IN s_budat
                                                   AND blart IN r_blart
                                                   AND usnam IN r_usnam
                                                   AND stblg = space
                                                   AND tcode NE 'F110'.

        LOOP AT g_tab_bkpf INTO wa_tab_bkpf.
          CLEAR ls_deltabix.
          ls_deltabix = sy-tabix.
          SELECT SINGLE * FROM zca_invoice_park INTO ls_invoice_park WHERE reference = wa_tab_bkpf-belnr.
          IF sy-subrc = 0.
            DELETE g_tab_bkpf INDEX ls_deltabix.
          ENDIF.
        ENDLOOP.
*-----------------------------------------------------------------------------------------*
**          ""Fetch all the records without validation to give error description
**-----------------------------------------------------------------------------------------*

        SELECT * FROM bkpf INTO TABLE lt_bkpf_tmp
                                          WHERE bukrs = p_bukrs
                                            AND belnr IN s_vblnr
                                            AND budat IN s_budat
                                            AND gjahr = p_gjahr
                                            AND stblg = space
                                            AND tcode NE 'F110'.
        CLEAR : bkpf.
        LOOP AT lt_bkpf_tmp INTO bkpf.
          CLEAR ls_deltabix.
          ls_deltabix = sy-tabix.
          SELECT SINGLE * FROM zca_invoice_park INTO ls_invoice_park WHERE reference = bkpf-belnr.
          IF sy-subrc = 0.
            DELETE lt_bkpf_tmp INDEX ls_deltabix.
          ENDIF.
        ENDLOOP.

        CLEAR : bkpf.
        LOOP AT lt_bkpf_tmp INTO bkpf.
          READ TABLE g_tab_bkpf TRANSPORTING NO FIELDS WITH KEY
                                             bukrs = bkpf-bukrs
                                             belnr = bkpf-belnr
                                             gjahr = bkpf-gjahr.
          IF sy-subrc NE 0.
            MOVE-CORRESPONDING bkpf TO bsak1.
            bsak1-augbl = bkpf-belnr.
            PERFORM append_display USING TEXT-e23    " Not Allowed
                                         TEXT-126    "Doc posted user or doc type was not maintained ZAXIS tables.
                                         c_nrm_colour.
            CLEAR : bsak1.
          ENDIF.
          CLEAR : bkpf.
        ENDLOOP.

*-----------------------------------------------------------------------------------*
**          ""Check the exact reason for doc not found in bkpf table
**-----------------------------------------------------------------------------------*

        IF g_tab_bkpf[] IS INITIAL.
          IF lt_bkpf_tmp[] IS NOT INITIAL.
            PERFORM pop_up_error USING TEXT-126.         "Doc posted user or doc type was not maintained ZAXIS tables.
          ELSE.
            PERFORM pop_up_error USING TEXT-111.          "No Data Found, please Check the Input Data Provided.
          ENDIF.
        ENDIF.

*-----------------------------------------------------------------------------------------*
**          ""Delete already extracted data from bkpf table
**-----------------------------------------------------------------------------------------*
        LOOP AT g_tab_bkpf INTO wa_tab_bkpf.

          READ TABLE g_tab_zaxis_tab_ctrltab INTO wa_zaxis_tab_ctrltab
                                            WITH KEY belnr = wa_tab_bkpf-belnr
                                                     bukrs = wa_tab_bkpf-bukrs
                                                     gjahr = wa_tab_bkpf-gjahr.
          IF sy-subrc EQ 0.
            DELETE g_tab_bkpf .
          ENDIF.
        ENDLOOP.
*-----------------------------------------------------------------------------------------*
**          ""Fetch BSIS & BSAS Table Data
**-----------------------------------------------------------------------------------------*
        IF g_tab_bkpf[] IS INITIAL.
          PERFORM pop_up_error USING TEXT-112.          "DATA ALREADY EXTRACTED.PLEASE CHECK
        ELSE.
          SELECT * FROM bsis INTO TABLE g_tab_bsis
                                   FOR ALL ENTRIES IN g_tab_bkpf
                                     WHERE bukrs = g_tab_bkpf-bukrs
                                       AND hkont IN r_hkont
                                       AND gjahr = g_tab_bkpf-gjahr
                                       AND belnr = g_tab_bkpf-belnr
                                       AND budat = g_tab_bkpf-budat
                                       AND gsber IN s_gsber
                                       AND prctr IN s_prctr.

          SELECT * FROM bsas INTO TABLE g_tab_bsas        " FETCH CLEARED ITEMS FOR VENDOR PAYMENTS
                                        FOR ALL ENTRIES IN g_tab_bkpf
                                          WHERE bukrs = g_tab_bkpf-bukrs
                                            AND hkont IN r_hkont
                                            AND gjahr = g_tab_bkpf-gjahr
                                            AND belnr = g_tab_bkpf-belnr
                                            AND budat = g_tab_bkpf-budat
                                            AND gsber IN s_gsber
                                            AND prctr IN s_prctr.

          IF g_tab_bsis[] IS INITIAL AND g_tab_bsas[] IS INITIAL.
            PERFORM pop_up_error USING TEXT-113 .           "Please make payment with correct G/L Account or Data might be extracted please check the Input Details
            EXIT.
          ENDIF.
        ENDIF.                "g_tab_bkpf[] IS INITIAL
      ENDIF.
*-----------------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------*
*------------------ For FOR REEXTRACTION PROGRAM----------------------------------------***
*-----------------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------*
    WHEN c_tcode_rextr.                "Re-Extraction

      IF g_tab_zaxis_tab_ctrltab[] IS NOT INITIAL.

        IF p_auto = abap_true.
          SELECT * FROM reguh INTO TABLE g_tab_reguh
                                         FOR ALL ENTRIES IN g_tab_zaxis_tab_ctrltab
                                           WHERE laufd = g_tab_zaxis_tab_ctrltab-laufd
                                             AND laufi = g_tab_zaxis_tab_ctrltab-laufi
                                             AND zbukr = g_tab_zaxis_tab_ctrltab-bukrs
                                             AND vblnr = g_tab_zaxis_tab_ctrltab-belnr
                                             AND srtgb IN s_gsber
                                             AND hktid IN s_hktid
                                             AND hbkid IN s_hbkid.
        ENDIF.

        SELECT * FROM bkpf INTO TABLE g_tab_bkpf
                                         FOR ALL ENTRIES IN g_tab_zaxis_tab_ctrltab
                                         WHERE bukrs = g_tab_zaxis_tab_ctrltab-bukrs
                                           AND belnr = g_tab_zaxis_tab_ctrltab-belnr
                                           AND gjahr = g_tab_zaxis_tab_ctrltab-gjahr
                                           AND blart IN r_blart
                                           AND usnam IN r_usnam
                                           AND stblg = space.
        IF g_tab_bkpf[] IS INITIAL.
          PERFORM pop_up_error USING TEXT-124 .           "Documents might be reversed or Input details might be wrong
          EXIT.
        ENDIF.

* FETCH THE OPEN ITEMS FOR THE OPEN INVOICES FOR RE EXTRATION.
        SELECT * FROM bsis INTO TABLE g_tab_bsis
                            FOR ALL ENTRIES IN g_tab_zaxis_tab_ctrltab
                              WHERE bukrs = g_tab_zaxis_tab_ctrltab-bukrs
                                AND hkont IN r_hkont
                                AND gjahr = g_tab_zaxis_tab_ctrltab-gjahr
                                AND belnr = g_tab_zaxis_tab_ctrltab-belnr
                                AND gsber IN s_gsber
                                AND prctr IN s_prctr
                                AND blart IN r_blart.

* FETCH CLOSED ITEMS FOR RE EXTRACTION.
        SELECT * FROM bsas INTO TABLE g_tab_bsas
                              FOR ALL ENTRIES IN g_tab_zaxis_tab_ctrltab
                                WHERE bukrs = g_tab_zaxis_tab_ctrltab-bukrs
                                  AND hkont IN r_hkont
                                  AND gjahr = g_tab_zaxis_tab_ctrltab-gjahr
                                  AND belnr = g_tab_zaxis_tab_ctrltab-belnr
                                  AND gsber IN s_gsber
                                  AND prctr IN s_prctr
                                  AND blart IN r_blart.
      ENDIF.
  ENDCASE.

  APPEND LINES OF g_tab_bsas TO g_tab_bsis.
  SORT g_tab_reguh BY zbukr vblnr zaldt.
  DELETE ADJACENT DUPLICATES FROM g_tab_reguh COMPARING ALL FIELDS.
ENDFORM.                    " read_bsis_bsas_reguh

*&----------------------------------------------------------------------------------------*
*&      Form  read_bsak_reguh
*&----------------------------------------------------------------------------------------*
*       text
*-----------------------------------------------------------------------------------------*
FORM read_bsak_reguh .
  DATA: kindex1 TYPE i,
        kindex  TYPE sy-tabix.

  CASE sy-tcode.

    WHEN c_tcode_extr.                 "Payments Extraction
*-----------------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------*
*------------------ For automatic payments extraction-----------------------------------***
*-----------------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------*
      IF p_auto = abap_true.  "'X'.                 "Automatic Payments

        IF  g_tab_reguh[] IS NOT INITIAL.
          SELECT * FROM bsak INTO TABLE g_tab_bsak
                              FOR ALL ENTRIES IN g_tab_reguh
                                WHERE bukrs = g_tab_reguh-zbukr
                                  AND lifnr = g_tab_reguh-lifnr
                                  AND augdt = g_tab_reguh-zaldt
                                  AND augbl = g_tab_reguh-vblnr
                                  AND gsber IN s_gsber.

          SELECT * FROM bsad INTO TABLE g_tab_bsad
                              FOR ALL ENTRIES IN g_tab_reguh
                                WHERE bukrs = g_tab_reguh-zbukr
                                  AND kunnr = g_tab_reguh-kunnr
                                  AND augdt = g_tab_reguh-zaldt
                                  AND augbl = g_tab_reguh-vblnr
                                  AND gsber IN s_gsber.
        ENDIF.
      ELSE.
*-----------------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------*
*------------------ For manual payments extraction-----------------------------------***
*-----------------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------*
        IF g_tab_bsis[] IS NOT INITIAL.

          SELECT * FROM bseg INTO TABLE g_tab_bseg "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
                                   FOR ALL ENTRIES IN g_tab_bsis
                                     WHERE bukrs = g_tab_bsis-bukrs
                                       AND belnr = g_tab_bsis-belnr
                                       AND gjahr = g_tab_bsis-gjahr.

          CLEAR: bseg,wa_lifnr,r_lifnr[],wa_kunnr,r_kunnr[].

          LOOP AT g_tab_bseg INTO bseg .

            IF bseg-lifnr IS NOT INITIAL.
              wa_lifnr-sign   = 'I'.
              wa_lifnr-option = 'EQ'.
              wa_lifnr-low    = bseg-lifnr.
              APPEND wa_lifnr TO r_lifnr.
            ENDIF.

            IF bseg-kunnr IS NOT INITIAL.
              wa_kunnr-sign   = 'I'.
              wa_kunnr-option = 'EQ'.
              wa_kunnr-low    = bseg-kunnr.
              APPEND wa_kunnr TO r_kunnr.
            ENDIF.

            CLEAR : wa_lifnr,wa_kunnr.
          ENDLOOP.
          SORT r_lifnr BY low.
          SORT r_kunnr BY low.
          DELETE ADJACENT DUPLICATES FROM r_lifnr COMPARING ALL FIELDS.
          DELETE ADJACENT DUPLICATES FROM r_kunnr COMPARING ALL FIELDS.

          SELECT * FROM bsak INTO TABLE g_tab_bsak
                              FOR ALL ENTRIES IN g_tab_bsis
                                WHERE bukrs = g_tab_bsis-bukrs
                                  AND lifnr IN r_lifnr
                                  AND augdt = g_tab_bsis-budat
                                  AND augbl = g_tab_bsis-belnr.
*                                  AND AUGGJ = G_TAB_BSIS-GJAHR.
*                %_HINTS ORACLE 'INDEX("BSAK" "BSAK~AUG")'.

          SELECT * FROM bsad INTO TABLE g_tab_bsad
                              FOR ALL ENTRIES IN g_tab_bsis
                                WHERE bukrs = g_tab_bsis-bukrs
                                  AND kunnr IN r_kunnr
                                  AND augdt = g_tab_bsis-budat
                                  AND augbl = g_tab_bsis-belnr.
*                                  AND AUGGJ = G_TAB_BSIS-GJAHR.
*            %_HINTS ORACLE 'INDEX("BSAD" "BSAD~AUG")'.

        ENDIF.
      ENDIF.
******************************-------------------********************************************
******************************-------------------********************************************
****---------CODE FOR RE-EXTRACTION PAYMENTS
******************************-------------------********************************************
******************************-------------------********************************************

    WHEN c_tcode_rextr.                "Re-Extraction

      IF g_tab_zaxis_tab_ctrltab[] IS NOT INITIAL.

        SELECT * FROM bseg INTO TABLE g_tab_bseg "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
                                 FOR ALL ENTRIES IN g_tab_zaxis_tab_ctrltab
                                   WHERE bukrs = g_tab_zaxis_tab_ctrltab-bukrs
                                     AND belnr = g_tab_zaxis_tab_ctrltab-belnr
                                     AND gjahr = g_tab_zaxis_tab_ctrltab-gjahr
                                     AND gsber IN s_gsber.

        CLEAR: bseg,wa_lifnr,r_lifnr[],wa_kunnr,r_kunnr[].

        LOOP AT g_tab_bseg INTO bseg .

          IF bseg-lifnr IS NOT INITIAL.
            wa_lifnr-sign   = 'I'.
            wa_lifnr-option = 'EQ'.
            wa_lifnr-low    = bseg-lifnr.
            APPEND wa_lifnr TO r_lifnr.
          ENDIF.

          IF bseg-kunnr IS NOT INITIAL.
            wa_kunnr-sign   = 'I'.
            wa_kunnr-option = 'EQ'.
            wa_kunnr-low    = bseg-kunnr.
            APPEND wa_kunnr TO r_kunnr.
          ENDIF.

          CLEAR : wa_lifnr,wa_kunnr.
        ENDLOOP.
        SORT r_lifnr BY low.
        SORT r_kunnr BY low.
        DELETE ADJACENT DUPLICATES FROM r_lifnr COMPARING ALL FIELDS.
        DELETE ADJACENT DUPLICATES FROM r_kunnr COMPARING ALL FIELDS.

        SELECT * FROM bsak INTO TABLE g_tab_bsak
                                  FOR ALL ENTRIES IN g_tab_zaxis_tab_ctrltab
                                    WHERE bukrs = g_tab_zaxis_tab_ctrltab-bukrs
                                      AND lifnr IN r_lifnr
                                      AND augdt = g_tab_zaxis_tab_ctrltab-budat
                                      AND augbl = g_tab_zaxis_tab_ctrltab-belnr
*                                      AND AUGGJ = G_TAB_ZAXIS_TAB_CTRLTAB-GJAHR
                                      AND gsber IN s_gsber.
*            %_HINTS ORACLE 'INDEX("BSAK" "BSAK~AUG")'.

        SELECT * FROM bsad INTO TABLE g_tab_bsad
                                  FOR ALL ENTRIES IN g_tab_zaxis_tab_ctrltab
                                    WHERE bukrs = g_tab_zaxis_tab_ctrltab-bukrs
                                      AND kunnr IN r_kunnr
                                      AND augdt = g_tab_zaxis_tab_ctrltab-budat
                                      AND augbl = g_tab_zaxis_tab_ctrltab-belnr
*                                      AND AUGGJ = G_TAB_ZAXIS_TAB_CTRLTAB-GJAHR
                                      AND gsber IN s_gsber.
*           %_HINTS ORACLE 'INDEX("BSAD" "BSAD~AUG")'.

        SELECT * FROM bsid INTO TABLE g_tab_bsid
                                FOR ALL ENTRIES IN g_tab_zaxis_tab_ctrltab
                                  WHERE bukrs = g_tab_zaxis_tab_ctrltab-bukrs
                                    AND kunnr IN r_kunnr
                                    AND belnr = g_tab_zaxis_tab_ctrltab-belnr
                                    AND budat = g_tab_zaxis_tab_ctrltab-budat
*                                    AND GJAHR = G_TAB_ZAXIS_TAB_CTRLTAB-GJAHR
                                    AND gsber IN s_gsber.
*            %_HINTS ORACLE 'INDEX("BSID" "BSID~AUG")'.
      ENDIF.
  ENDCASE.
ENDFORM.                    " read_bsak_reguh


*&---------------------------------------------------------------------*
*&      Form  ADD_CROSS_COMPANY_PAYMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_cross_company_payments .

  IF g_tab_bkpf[] IS NOT INITIAL.

    SELECT * FROM bvor INTO TABLE g_tab_bvor
              FOR ALL ENTRIES IN g_tab_bkpf
                WHERE bvorg = g_tab_bkpf-bvorg
                  AND bukrs <> g_tab_bkpf-bukrs
                  AND gjahr = g_tab_bkpf-gjahr.

    IF sy-subrc  = 0.
      g_cross_company_check = abap_true. "'X'.
    ENDIF.

    IF g_tab_bvor[] IS NOT INITIAL.
      SELECT * FROM bkpf INTO TABLE g_tab_bkpf1         " temp folder to store cc payments
                         FOR ALL ENTRIES IN g_tab_bvor
                          WHERE bvorg = g_tab_bvor-bvorg
                          AND   bukrs = g_tab_bvor-bukrs
                          AND   gjahr = g_tab_bvor-gjahr.
    ENDIF.
  ENDIF.

  IF  g_cross_company_check = abap_true. "'X'.
    IF g_tab_bvor[] IS NOT INITIAL.

      SELECT * FROM bkpf APPENDING TABLE g_tab_bkpf
                               FOR ALL ENTRIES IN g_tab_bvor
                               WHERE bukrs = g_tab_bvor-bukrs
                                 AND belnr = g_tab_bvor-belnr
                                 AND gjahr = g_tab_bvor-gjahr.

      SELECT * FROM bsak APPENDING TABLE g_tab_bsak
                               FOR ALL ENTRIES IN g_tab_bvor
                               WHERE bukrs = g_tab_bvor-bukrs
                                 AND augbl = g_tab_bvor-belnr
                                 AND gjahr = g_tab_bvor-gjahr.

      SELECT * FROM bsad APPENDING TABLE g_tab_bsad
                               FOR ALL ENTRIES IN g_tab_bvor
                               WHERE bukrs = g_tab_bvor-bukrs
                                 AND augbl = g_tab_bvor-belnr
                                 AND gjahr = g_tab_bvor-gjahr.
    ENDIF.
  ENDIF.  " g_cross_company_check = 'X'.
ENDFORM.                    " ADD_CROSS_COMPANY_PAYMENTS
*&---------------------------------------------------------------------*
*&      Form  ADD_CUSTOMER_PAYMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* For customer payments
*----------------------------------------------------------------------*
FORM add_customer_payments .
  DATA : lt_kna1 TYPE TABLE OF kna1.

  IF g_tab_bsad[] IS NOT INITIAL.
    SELECT * FROM kna1 INTO TABLE lt_kna1
                       FOR ALL ENTRIES IN g_tab_bsad
                       WHERE kunnr = g_tab_bsad-kunnr.
  ENDIF.
  IF g_tab_bsid[] IS NOT INITIAL.
    SELECT * FROM kna1 APPENDING TABLE lt_kna1
                       FOR ALL ENTRIES IN g_tab_bsid
                       WHERE kunnr = g_tab_bsid-kunnr.
  ENDIF.

  CLEAR : bsad , kna1.
  LOOP AT g_tab_bsad INTO bsad.
    READ TABLE lt_kna1 INTO kna1 WITH KEY kunnr = bsad-kunnr.
    IF sy-subrc EQ 0 AND kna1-lifnr IS NOT INITIAL.
      bsad-kunnr = kna1-lifnr.
      MODIFY g_tab_bsad FROM bsad.
      "appending to bsak table from bsad table
      MOVE-CORRESPONDING bsad TO bsak.
      bsak-lifnr = kna1-lifnr.
      APPEND bsak TO g_tab_bsak .

    ELSE.
      DELETE g_tab_bsad.                                    "#EC *
    ENDIF.
    CLEAR : bsad,bsid,bsak,bsik,kna1.
  ENDLOOP.
  CLEAR : bsad,bsid,bsak,bsik,kna1.

  LOOP AT g_tab_bsid  INTO bsid.
    READ TABLE lt_kna1 INTO kna1 WITH KEY kunnr = bsid-kunnr.
    IF sy-subrc EQ 0 AND kna1-lifnr IS NOT INITIAL.
      bsid-kunnr = kna1-lifnr.
      MODIFY g_tab_bsid FROM bsid.
      "appending to bsik table from bsid table
      MOVE-CORRESPONDING bsid TO bsik.
      bsik-lifnr = kna1-lifnr.
      APPEND bsik TO g_tab_bsik1.
    ELSE.
      DELETE g_tab_bsad.                                    "#EC *
    ENDIF.
    CLEAR : bsad,bsid,bsak,bsik,kna1.
  ENDLOOP.
  CLEAR : bsad,bsid,bsak,bsik,kna1.

ENDFORM.                    " ADD_CUSTOMER_PAYMENTS
*&---------------------------------------------------------------------*
*&      Form  COLLECT_RESIDUAL_PAYMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_residual_payments .

  IF g_tab_bsis IS NOT INITIAL.
    SELECT * FROM bsik INTO TABLE g_tab_bsik_res        "bsik2      " only resedual items.
                        FOR ALL ENTRIES IN g_tab_bsis
                        WHERE bukrs = g_tab_bsis-bukrs
                        AND lifnr IN r_lifnr
                        AND belnr = g_tab_bsis-belnr
                        AND gjahr = g_tab_bsis-gjahr
                        AND budat = g_tab_bsis-budat
                        AND rebzt = 'V'.
*      %_HINTS ORACLE 'INDEX("BSIK" "BSIK~5")'.

    SELECT * FROM bsak APPENDING TABLE g_tab_bsik_res          " only resedual items.
                        FOR ALL ENTRIES IN g_tab_bsis
                        WHERE bukrs = g_tab_bsis-bukrs
                        AND lifnr IN r_lifnr
                        AND belnr = g_tab_bsis-belnr
                        AND gjahr = g_tab_bsis-gjahr
                        AND budat = g_tab_bsis-budat
                        AND rebzt = 'V'.                    "#EC ENHOK
*       %_HINTS ORACLE 'INDEX("BSAK" "BSAK~5")'.
  ENDIF.

  g_tab_bseg_res[] = g_tab_bseg[].
  DELETE g_tab_bseg_res WHERE rebzg EQ abap_false.   "' '.

  IF g_tab_bseg_res[] IS NOT INITIAL.
    SELECT * FROM bkpf INTO TABLE g_tab_bkpf_res FOR ALL ENTRIES IN g_tab_bseg_res
                                        WHERE bukrs   = g_tab_bseg_res-bukrs AND
                                              belnr   = g_tab_bseg_res-rebzg AND
                                              gjahr   = g_tab_bseg_res-rebzj AND
                                              blart   = 'KZ'.
    IF g_tab_bkpf_res[] IS NOT INITIAL.
      SELECT * FROM bseg INTO TABLE g_tab_bseg_res1 FOR ALL ENTRIES IN g_tab_bkpf_res "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
                                        WHERE bukrs  EQ g_tab_bkpf_res-bukrs AND
                                              belnr  EQ g_tab_bkpf_res-belnr AND
                                              gjahr  EQ g_tab_bkpf_res-gjahr ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
    ENDIF.
  ENDIF.

  LOOP AT g_tab_bseg_res INTO bseg.
    READ TABLE g_tab_bsak INTO bsak WITH KEY belnr = bseg-rebzg
                                          gjahr = bseg-rebzj
                                          bukrs = bseg-bukrs
                                          zlsch = bseg-zlsch.

    IF sy-subrc NE 0 .     " for resedual payments check any privios payment exist or not
      READ TABLE g_tab_bkpf_res INTO bkpf WITH KEY bukrs   = bseg-bukrs
                                                   belnr   = bseg-rebzg
                                                   gjahr   = bseg-rebzj
                                                   blart   = 'KZ'.

      IF sy-subrc EQ 0.
        READ TABLE g_tab_bseg_res1 INTO bseg1 WITH KEY bukrs = bkpf-bukrs
                                                       belnr = bkpf-belnr
                                                       gjahr = bkpf-gjahr
                                                       buzei = bseg-rebzz.

        MOVE-CORRESPONDING bseg1 TO bsak1.  " append first line item to bsak table for residual
        MOVE bseg-belnr  TO bsak1-augbl.
        MOVE bkpf-budat  TO bsak1-augdt.
        MOVE bkpf-blart  TO bsak1-blart.
        MOVE bkpf-budat  TO bsak1-budat.
        MOVE bseg-dmbtr  TO bsak1-dmbtr.
        MOVE bseg-wrbtr  TO bsak1-wrbtr.
        MOVE bseg-zlsch  TO bsak1-zlsch.
        APPEND bsak1     TO g_tab_bsak.
        CLEAR : bsak1.

        MOVE-CORRESPONDING bseg  TO bsak1.  " " append 2nd line item to bsak table for resedual
        MOVE bseg-belnr TO bsak1-augbl.
        MOVE bkpf-budat TO bsak1-augdt.
        MOVE bkpf-blart TO bsak1-blart.
        MOVE bkpf-budat TO bsak1-budat.
        APPEND bsak1    TO g_tab_bsak.
        CLEAR : bsak1 , bsak , bseg ,bkpf ,bseg1.
      ENDIF.
    ENDIF.
    CLEAR : bsak1 , bsak , bseg , bkpf , bseg1.
  ENDLOOP.

***-----for the above calculate proper bsak table data -------------**
  IF g_tab_bsik_res IS NOT INITIAL.

    lt_bsik_temp = g_tab_bsik_res[].
    SORT lt_bsik_temp BY bukrs belnr gjahr.
    DELETE ADJACENT DUPLICATES FROM lt_bsik_temp COMPARING bukrs belnr gjahr.

    IF lt_bsik_temp IS NOT INITIAL.

      SELECT * FROM bseg APPENDING TABLE g_tab_bseg_res FOR ALL ENTRIES IN lt_bsik_temp "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
                                                    WHERE bukrs = lt_bsik_temp-bukrs
                                                    AND   belnr = lt_bsik_temp-belnr
                                                    AND   gjahr = lt_bsik_temp-gjahr
                                                    AND   rebzt = 'V' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

    ENDIF.
***-------------------update the items---------------------------------------***

    SORT g_tab_bseg_res BY bukrs gjahr belnr buzei.
    DELETE ADJACENT DUPLICATES FROM g_tab_bseg_res[] COMPARING ALL FIELDS.

    LOOP AT g_tab_bseg_res INTO bseg.
      READ TABLE g_tab_bsak INTO bsak WITH KEY bukrs = bseg-bukrs
                                              gjahr = bseg-rebzj
                                              augbl = bseg-belnr
                                              belnr = bseg-rebzg.
      IF sy-subrc EQ 0.

        kindex1 = sy-tabix.
        bsak-dmbtr =  bsak-dmbtr - bseg-dmbtr.
        bsak-wrbtr =  bsak-wrbtr - bseg-wrbtr.
        MODIFY g_tab_bsak FROM bsak INDEX kindex1 TRANSPORTING dmbtr wrbtr.

      ELSE.
        READ TABLE g_tab_bsak INTO bsak WITH KEY bukrs = bseg-bukrs
                                              gjahr = bseg-rebzj
                                              augbl = bseg-belnr
                                              rebzg = bseg-rebzg.

        IF sy-subrc EQ 0.
          kindex1 = sy-tabix.
          bsak-dmbtr = bsak-dmbtr - bseg-dmbtr.
          bsak-wrbtr = bsak-wrbtr - bseg-wrbtr.
          MODIFY g_tab_bsak FROM bsak INDEX kindex1 TRANSPORTING dmbtr wrbtr.
        ENDIF.
      ENDIF.
      CLEAR : bseg, bsak.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " COLLECT_RESIDUAL_PAYMENTS
*&---------------------------------------------------------------------*
*&      Form  COLLECT_PARTIAL_PAYMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_partial_payments .

  IF g_tab_bsis[] IS NOT INITIAL.

******** for not cleared Partial Payment
    SELECT * FROM bsik INTO TABLE g_tab_bsik
                          FOR ALL ENTRIES IN g_tab_bsis
                            WHERE bukrs = g_tab_bsis-bukrs
                              AND lifnr IN r_lifnr
                              AND belnr = g_tab_bsis-belnr
                              AND budat = g_tab_bsis-budat
                              AND shkzg = 'S'.

******** For already cleared Partial Payment
    SELECT * FROM bsak  APPENDING TABLE g_tab_bsik
                            FOR ALL ENTRIES IN g_tab_bsis
                              WHERE bukrs = g_tab_bsis-bukrs
                                AND ( augbl NE  space OR augbl NE g_tab_bsis-belnr )
                                AND belnr = g_tab_bsis-belnr
                                AND budat = g_tab_bsis-budat
                                AND gsber IN s_gsber
                                AND rebzg NE space
                                AND rebzt EQ 'Z'
                                AND shkzg = 'S'.
  ENDIF.

  IF g_tab_bsik IS NOT INITIAL.

******** for not cleared Partial Payment
    SELECT * FROM bsik INTO TABLE g_tab_bsik1
                      FOR ALL ENTRIES IN g_tab_bsik
                      WHERE bukrs = g_tab_bsik-bukrs
                      AND   gjahr = g_tab_bsik-rebzj
                      AND   belnr = g_tab_bsik-rebzg
                      AND   buzei = g_tab_bsik-rebzz.

******** for already cleared Partial Payment
    SELECT * FROM bsak APPENDING TABLE g_tab_bsik1
                   FOR ALL ENTRIES IN g_tab_bsik
                   WHERE bukrs = g_tab_bsik-bukrs
                   AND   gjahr = g_tab_bsik-rebzj
                   AND   belnr = g_tab_bsik-rebzg
                   AND   buzei = g_tab_bsik-rebzz.
  ENDIF.

  IF g_tab_bsik IS NOT INITIAL.

    SORT g_tab_bsik.
    LOOP AT g_tab_bsik INTO wa_bsik.

      READ TABLE g_tab_bsik1 INTO wa_bsik1 WITH KEY bukrs = wa_bsik-bukrs
                                                    gjahr = wa_bsik-rebzj
                                                    belnr = wa_bsik-rebzg
                                                    buzei = wa_bsik-rebzz.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING wa_bsik1 TO wa_bsak.

        wa_bsak-augbl = wa_bsik-belnr.
        wa_bsak-augdt = wa_bsik-budat.
        wa_bsak-dmbtr = wa_bsik-dmbtr.
        wa_bsak-wrbtr = wa_bsik-wrbtr.
        wa_bsak-qbshb = wa_bsik-qbshb.
        APPEND wa_bsak TO g_tab_bsak.
        CLEAR : wa_bsik , wa_bsik1 , wa_bsak.

      ENDIF.
      CLEAR : wa_bsik.
    ENDLOOP.

    CLEAR : wa_bsik , wa_bsik1 , wa_bsak.
    LOOP AT g_tab_bsik INTO wa_bsik.

      MOVE-CORRESPONDING wa_bsik TO wa_bsak.
      wa_bsak-augbl = wa_bsik-belnr.
      wa_bsak-augdt = wa_bsik-budat.

      IF wa_bsik-umskz IS INITIAL.
        wa_bsak-dmbtr = wa_bsik-dmbtr - wa_bsik-qbshb.
      ELSE.
        wa_bsak-dmbtr = wa_bsik-dmbtr.
      ENDIF.
      APPEND wa_bsak TO g_tab_bsak.
      CLEAR : wa_bsik, wa_bsak.
    ENDLOOP.
  ENDIF.
  CLEAR : wa_bsik, wa_bsak , wa_bsak1.
ENDFORM.                    " COLLECT_PARTIAL_PAYMENTS
*&---------------------------------------------------------------------*
*&      Form  SEGRIGATE_PAYMENT_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM segrigate_payment_invoice .

  DATA : lt_bkpf_tmp TYPE TABLE OF bkpf.
  SORT g_tab_bsak BY augdt augbl umskz.
  CLEAR   : wa_bsak , wa_tab_bkpf , wa_bkpf.
  REFRESH : lt_bkpf_tmp.

**  ""---to check wheter the advace req booked by f-47 and cleared with f-48------
  IF g_tab_bsak[] IS NOT INITIAL.
    SELECT * FROM bkpf INTO TABLE lt_bkpf_tmp
                            FOR ALL ENTRIES IN g_tab_bsak WHERE
                                    bukrs = g_tab_bsak-bukrs AND
                                    belnr = g_tab_bsak-belnr AND
                                    gjahr = g_tab_bsak-gjahr.
  ENDIF.

**  --------------------segrigate Payments and invoices ------------------------------
  LOOP AT g_tab_bsak INTO wa_bsak.

    IF wa_bsak-augbl = wa_bsak-belnr.     " create payment entry.
      READ TABLE g_tab_bkpf INTO wa_tab_bkpf WITH KEY bukrs = wa_bsak-bukrs
                                                      belnr = wa_bsak-belnr
                                                      gjahr = wa_bsak-gjahr.
      IF sy-subrc EQ 0.

        CASE wa_tab_bkpf-tcode.
          WHEN 'F110'.                              " AUTOMATIC PAYMENTS.
            IF p_auto = abap_true. "'X'.
              APPEND wa_bsak TO g_tab_bsak_pymnt.
              DELETE g_tab_bsak.
            ENDIF.

          WHEN 'FBZ2' .                             " MANUAL F-53
            APPEND wa_bsak TO g_tab_bsak_pymnt.
            DELETE g_tab_bsak.

          WHEN 'FBZ4' .                             " MANUAL F-58
            APPEND wa_bsak TO g_tab_bsak_pymnt.
            DELETE g_tab_bsak.

          WHEN 'FB05' .                             " Entries through API using MAPOL_API Id
            IF wa_tab_bkpf-usnam = 'MAPOL_API'.
              APPEND wa_bsak TO g_tab_bsak_pymnt.
              DELETE g_tab_bsak.
            ENDIF.

          WHEN 'FBA7'.                              " MANUAL ADVANCE PAYMENTS.
            APPEND wa_bsak TO g_tab_bsak_pymnt.

          WHEN 'FB01'.
            IF wa_tab_bkpf-blart = c_vend_doc_type OR      " 'KZ'
               wa_tab_bkpf-blart = c_vend_adv_doc_type.    " 'KA'

              APPEND wa_bsak TO g_tab_bsak_pymnt.
            ENDIF.
          WHEN 'FBR2'.
            IF wa_tab_bkpf-blart = c_vend_doc_type.          " if it is normal Payment Doc
              APPEND wa_bsak TO g_tab_bsak_pymnt.
              DELETE g_tab_bsak.

            ELSEIF wa_tab_bkpf-blart = c_vend_adv_doc_type. " if it is adv apyment
              APPEND wa_bsak TO g_tab_bsak_pymnt.
            ENDIF.

          WHEN OTHERS.
            CLEAR : gv_text.
            CONCATENATE 'TCODE :' wa_tab_bkpf-tcode TEXT-502 INTO gv_text. "'Was not allowed in ZAXIS extractor program'
            MOVE-CORRESPONDING wa_bsak TO bsak1.

            PERFORM append_display USING TEXT-e23     " Not Allowed
                                         gv_text
                                         c_nrm_colour.
**            PERFORM APPEND_EXCEPTION USING TEXT-E01     " ERROR
**                                           GV_TEXT    " Corresponding Payment method was not maintained in Payment Method table please check
**                                           C_RED_COLOUR.
            CLEAR : gv_text.

        ENDCASE.
      ENDIF.
    ELSE.
      READ TABLE lt_bkpf_tmp INTO wa_tab_bkpf WITH KEY bukrs = wa_bsak-bukrs
                                                       belnr = wa_bsak-belnr
                                                       gjahr = wa_bsak-gjahr.
      IF sy-subrc EQ 0 AND wa_tab_bkpf-tcode EQ 'FBA6'.     "f-47

        READ TABLE g_tab_bkpf INTO wa_bkpf WITH KEY bukrs = wa_bsak-bukrs
                                                    belnr = wa_bsak-augbl
                                                    gjahr = wa_bsak-auggj.
        IF sy-subrc EQ 0 AND wa_bkpf-tcode = 'FBA7'.        " f-48

          DELETE g_tab_bsak.

        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR : wa_bsak , wa_tab_bkpf , wa_bkpf.
  ENDLOOP.
ENDFORM.                    " SEGRIGATE_PAYMENT_INVOICE

*&---------------------------------------------------------------------*
*&      Form  read_regup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_regup .
  REFRESH g_tab_regup.
  IF g_tab_reguh[] IS NOT INITIAL.
* FETCH THE DETAILS FROM (REGUP).
    SELECT * FROM regup INTO TABLE g_tab_regup
                        FOR ALL ENTRIES IN g_tab_reguh
                          WHERE laufd = g_tab_reguh-laufd
                            AND laufi = g_tab_reguh-laufi
                            AND xvorl = ' '
                            AND zbukr = g_tab_reguh-zbukr
                            AND vblnr = g_tab_reguh-vblnr.

  ENDIF.
ENDFORM.                    " read_regup

*&---------------------------------------------------------------------*
*&      Form  read_payr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_payr .

  REFRESH g_tab_payr.
  IF g_tab_bsak_pymnt[] IS NOT INITIAL.

* FETCH ALL THE RECORDS FROM (PAYR) TABLE BASED ON BSAK RECORDS.
    SELECT * FROM payr INTO TABLE g_tab_payr
                        FOR ALL ENTRIES IN g_tab_bsak_pymnt
                          WHERE zbukr = g_tab_bsak_pymnt-bukrs
                            AND vblnr = g_tab_bsak_pymnt-belnr
                            AND gjahr = g_tab_bsak_pymnt-gjahr
                            AND voidd = '00000000'.
  ENDIF.

  IF g_tab_bsad_pymnt[] IS NOT INITIAL.

* FETCH ALL THE RECORDS FROM (PAYR) TABLE BASED ON BSAD RECORDS.
    SELECT * FROM payr APPENDING TABLE g_tab_payr
                        FOR ALL ENTRIES IN g_tab_bsad_pymnt
                          WHERE zbukr = g_tab_bsad_pymnt-bukrs
                            AND vblnr = g_tab_bsad_pymnt-belnr
                            AND gjahr = g_tab_bsad_pymnt-gjahr
                            AND voidd = '00000000'.
  ENDIF.
  SORT g_tab_payr BY zbukr vblnr gjahr.
ENDFORM.                    " read_payr

*&---------------------------------------------------------------------*
*&      Form  read_lfa1_lfb1_lfbk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_lfa1_lfb1_lfbk .

  REFRESH: g_tab_lfa1, g_tab_lfbk.
  IF g_tab_bsak_pymnt[] IS NOT INITIAL.

*&---------------------------------------------------------------------*
**--------------SEELCT THE VENDOR MASTER DETAILS-----------------------*
*----------------------------------------------------------------------*
    SELECT * FROM lfa1 INTO TABLE g_tab_lfa1
                        FOR ALL ENTRIES IN g_tab_bsak_pymnt
                          WHERE lifnr = g_tab_bsak_pymnt-lifnr.

    SELECT * FROM lfa1 APPENDING TABLE g_tab_lfa1                 " for selecting individual payee
                           FOR ALL ENTRIES IN g_tab_bsak_pymnt
                           WHERE lifnr = g_tab_bsak_pymnt-empfb.

*&---------------------------------------------------------------------*
**----get alternate payee details Based on Lfa1-lnrza field------------*
*----------------------------------------------------------------------*
    IF g_tab_lfa1[] IS NOT INITIAL.

      SELECT * FROM lfa1 APPENDING TABLE g_tab_lfa1
                         FOR ALL ENTRIES IN g_tab_lfa1
                         WHERE lifnr = g_tab_lfa1-lnrza.

      SELECT * FROM lfb1 INTO TABLE g_tab_lfb1_temp
                          FOR ALL ENTRIES IN g_tab_lfa1
                          WHERE lifnr = g_tab_lfa1-lifnr
                          AND bukrs   = p_bukrs.

*&---------------------------------------------------------------------*
**----get alternate payee details Based on Lfb1-lnrzb field------------*
*----------------------------------------------------------------------*
      IF g_tab_lfb1_temp[] IS NOT INITIAL.
        SELECT * FROM lfa1 APPENDING TABLE g_tab_lfa1
                           FOR ALL ENTRIES IN g_tab_lfb1_temp
                           WHERE lifnr = g_tab_lfb1_temp-lnrzb.

      ENDIF.    "IF G_TAB_LFB1_temp[]
    ENDIF.      "IF G_TAB_LFA1[]

    SORT g_tab_lfa1[] BY lifnr.
    DELETE ADJACENT DUPLICATES FROM g_tab_lfa1[] COMPARING lifnr.
*&---------------------------------------------------------------------*
**----vendor bank details from the vendor master LFA1 Table------------*
*----------------------------------------------------------------------*
    IF NOT g_tab_lfa1[] IS INITIAL.

      SELECT * FROM lfb1 INTO TABLE g_tab_lfb1
                          FOR ALL ENTRIES IN g_tab_lfa1
                          WHERE lifnr = g_tab_lfa1-lifnr
                          AND bukrs   = p_bukrs.

      SELECT * FROM lfbk INTO TABLE g_tab_lfbk
                          FOR ALL ENTRIES IN g_tab_lfa1
                            WHERE lifnr = g_tab_lfa1-lifnr.
    ENDIF.
  ENDIF.
  SORT : g_tab_lfa1 BY lifnr, g_tab_lfb1 BY lifnr bukrs, g_tab_lfbk BY lifnr.
ENDFORM.                    " read_lfa1_lfb1_lfbk

*&---------------------------------------------------------------------*
*&      Form  read_bsec
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_bsec .

  REFRESH g_tab_bsec.
  IF g_tab_bsak_pymnt[] IS NOT INITIAL.

* FETCH THE DETAILS FROM (BSEC) BASED ON BSAK RECORDS.
    SELECT * FROM bsec INTO TABLE g_tab_bsec
                        FOR ALL ENTRIES IN g_tab_bsak_pymnt
                          WHERE bukrs = g_tab_bsak_pymnt-bukrs
                            AND gjahr = g_tab_bsak_pymnt-gjahr
                            AND belnr = g_tab_bsak_pymnt-belnr ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
  ENDIF.

  IF g_tab_bsad_pymnt[] IS NOT INITIAL.

* FETCH THE DETAILS FROM (BSEC) BASEDO ON BSAD RECORDS.
    SELECT * FROM bsec APPENDING TABLE g_tab_bsec
                            FOR ALL ENTRIES IN g_tab_bsad_pymnt
                              WHERE bukrs = g_tab_bsad_pymnt-bukrs
                                AND gjahr = g_tab_bsad_pymnt-gjahr
                                AND belnr = g_tab_bsad_pymnt-belnr ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

  ENDIF.
  SORT g_tab_bsec BY bukrs belnr gjahr.
ENDFORM.                    " read_bsec

*&---------------------------------------------------------------------*
*&      Form  read_adrc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_adrc .

  REFRESH g_tab_adrc.
  IF g_tab_lfa1[] IS NOT INITIAL.

* FETCH THE VENDOR ADDRESS DETAILS FROM (ADRC)
    SELECT * FROM adrc INTO TABLE g_tab_adrc
                        FOR ALL ENTRIES IN g_tab_lfa1
                          WHERE addrnumber = g_tab_lfa1-adrnr.

    SELECT * FROM adr6 INTO TABLE g_tab_adr6 FOR ALL ENTRIES IN g_tab_lfa1
                          WHERE addrnumber = g_tab_lfa1-adrnr.
  ENDIF.

  IF g_tab_kna1[] IS NOT INITIAL.

* FETCH THE CUSTOMER ADDRESS DETAILS FROM (ADRC)
    SELECT * FROM adrc APPENDING TABLE g_tab_adrc
                        FOR ALL ENTRIES IN g_tab_kna1
                          WHERE addrnumber = g_tab_kna1-adrnr.

    SELECT * FROM adr6 APPENDING TABLE g_tab_adr6 FOR ALL ENTRIES IN g_tab_kna1
                          WHERE addrnumber = g_tab_kna1-adrnr.
  ENDIF.

  IF  g_tab_bkpf[] IS NOT INITIAL.
    SELECT * FROM usr21 INTO TABLE g_tab_usr21 FOR ALL ENTRIES IN g_tab_bkpf
                                               WHERE bname = g_tab_bkpf-usnam.

    IF  g_tab_usr21[] IS NOT INITIAL.
      SELECT * FROM adr6 APPENDING TABLE g_tab_adr6 FOR ALL ENTRIES IN g_tab_usr21
                         WHERE addrnumber = g_tab_usr21-addrnumber
                         AND   persnumber = g_tab_usr21-persnumber.
    ENDIF.
  ENDIF.
  SORT g_tab_adrc BY addrnumber.
ENDFORM.                    " read_adrc

*&---------------------------------------------------------------------*
*&      Form  read_bnka
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_bnka .

  REFRESH g_tab_bnka.
  IF g_tab_lfbk[] IS NOT INITIAL.

* FETCH THE VENDOR BANK DETAILS FROM BANK MASTER (BNKA)
    SELECT * FROM bnka INTO TABLE g_tab_bnka
                        FOR ALL ENTRIES IN g_tab_lfbk
                          WHERE banks = g_tab_lfbk-banks
                            AND bankl = g_tab_lfbk-bankl.
  ENDIF.
  IF g_tab_knbk[] IS NOT INITIAL.

* FETCH THE CUSTOMER BANK DETAILS FROM BANK MASTER (BNKA)
    SELECT * FROM bnka APPENDING TABLE g_tab_bnka
                        FOR ALL ENTRIES IN g_tab_knbk
                          WHERE banks = g_tab_knbk-banks
                            AND bankl = g_tab_knbk-bankl.
  ENDIF.

  IF g_tab_bsec[] IS NOT INITIAL.
* FETCH THE BANK DETAILS FROM BANK MASTER (BNKA)
    SELECT * FROM bnka APPENDING TABLE g_tab_bnka
                        FOR ALL ENTRIES IN g_tab_bsec
                          WHERE banks = g_tab_bsec-banks
                            AND bankl = g_tab_bsec-bankl.
  ENDIF.
  SORT g_tab_bnka BY banks bankl.
ENDFORM.                    " read_bnka



*&---------------------------------------------------------------------*
*&      Form  read_t012_t012k
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_t012_t012k .

  REFRESH: g_tab_t012k, g_tab_t012.
* FETCH THE HOUSE BANK DETAILS FROM (T012)
  SELECT * FROM t012 INTO TABLE g_tab_t012
                        WHERE bukrs = p_bukrs
                          AND hbkid IN s_hbkid.

  IF g_tab_t012[] IS NOT INITIAL.
    SELECT * FROM t012k INTO TABLE g_tab_t012k
                        FOR ALL ENTRIES IN g_tab_t012
                        WHERE bukrs = g_tab_t012-bukrs
                          AND hbkid = g_tab_t012-hbkid.
  ENDIF.
  SORT g_tab_t012  BY bukrs hbkid.
  SORT g_tab_t012k BY bukrs hbkid hktid.
ENDFORM.                    " read_t012_t012k



*&---------------------------------------------------------------------*
*&      Form  read_bseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_bseg .

  REFRESH g_tab_bseg.
  IF g_tab_bsak_pymnt[] IS NOT INITIAL.
* FETCH THE DETAILS FROM (BSEG) BASED ON BSAK
    SELECT * FROM bseg INTO TABLE g_tab_bseg "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
                        FOR ALL ENTRIES IN g_tab_bsak_pymnt
                          WHERE bukrs = g_tab_bsak_pymnt-bukrs
                            AND belnr = g_tab_bsak_pymnt-belnr
                            AND gjahr = g_tab_bsak_pymnt-gjahr
                            AND buzei = g_tab_bsak_pymnt-buzei ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
  ENDIF.

  IF g_tab_bsad_pymnt[] IS NOT INITIAL.
* FETCH THE DETAILS FROM (BSEG) BASED ON BSAD
    SELECT * FROM bseg APPENDING TABLE g_tab_bseg "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
                        FOR ALL ENTRIES IN g_tab_bsad_pymnt
                          WHERE bukrs = g_tab_bsad_pymnt-bukrs
                            AND belnr = g_tab_bsad_pymnt-belnr
                            AND gjahr = g_tab_bsad_pymnt-gjahr
                            AND buzei = g_tab_bsad_pymnt-buzei ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
  ENDIF.
ENDFORM.                    " read_bseg

*&---------------------------------------------------------------------*
*&      Form  read_with_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_with_item .

  REFRESH g_tab_with_item.
  IF g_tab_bsak_pymnt[] IS NOT INITIAL.
* GETTING THE TAXES LIKE TDS/WD/ESIC/OTHERS FROM WITH_ITEM TABLE
*   BASED ON CLEARING DOCUMENT NUMBER
    SELECT * FROM with_item INTO TABLE g_tab_with_item
                                  FOR ALL ENTRIES IN g_tab_bsak_pymnt
                                  WHERE bukrs = g_tab_bsak_pymnt-bukrs
                                  AND   belnr = g_tab_bsak_pymnt-belnr
                                  AND   gjahr = g_tab_bsak_pymnt-gjahr
                                  AND   wt_qbshh <> ' '.
  ENDIF.

  IF g_tab_bsak[] IS NOT INITIAL.
* GETTING THE TAXES LIKE TDS/WD/ESIC/OTHERS FROM WITH_ITEM TABLE
*   BASED ON INVOICE NUMBER

    SELECT * FROM with_item APPENDING TABLE g_tab_with_item
                               FOR ALL ENTRIES IN g_tab_bsak
                               WHERE bukrs = g_tab_bsak-bukrs
                               AND   belnr = g_tab_bsak-belnr
                               AND   gjahr = g_tab_bsak-gjahr
                               AND   wt_qbshh <> ' '.
  ENDIF.
  SORT g_tab_with_item  . " Added by <IT-CAR Tool> during Code Remediation
  SORT g_tab_with_item  . " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM g_tab_with_item COMPARING ALL FIELDS.

*************{modification done to avoide the GTA service tax in TDS calculation#20171012 by ATK
  DELETE g_tab_with_item WHERE ( hkont EQ '0016615301' OR
                                 hkont EQ '0016615304' OR
                                 hkont EQ '0016615305' OR
                                 hkont EQ '0016615306' OR
                                 hkont EQ '0016615308' ).
*************modification done to avoide the GTA service tax in TDS calculation#20171012 by ATK}
ENDFORM.                    " read_with_item


*&---------------------------------------------------------------------*
*&      Form  read_print_loc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_print_loc .

  DATA lw_payloc  TYPE zaxis_tab_payloc.
  DATA: str1 TYPE char50,    "Code added on print location on 29.12.2015
        str2 TYPE char50,
        str3 TYPE char50,
        str4 TYPE char50.

*SELECT THE RECORDS FROM THE TABLE PAYMENTS LOCATION
*THIS TABLE IS USED TO FIND THE PRINT LOCATION
*  FOR THE PAYMENT MADE THRU LBC CHEQUES
  SELECT * FROM zaxis_tab_payloc INTO TABLE g_tab_zaxis_tab_payloc .

  LOOP AT  g_tab_zaxis_tab_payloc INTO lw_payloc.

    SET LOCALE LANGUAGE 'E'.
    TRANSLATE lw_payloc-sap_city  TO LOWER CASE.
    TRANSLATE lw_payloc-pay_loc   TO LOWER CASE.
    TRANSLATE lw_payloc-print_loc TO LOWER CASE.
    TRANSLATE lw_payloc-bank_nme  TO LOWER CASE.

***-----make first letter capital for print loc----------------------****
    SPLIT lw_payloc-print_loc AT space INTO str1 str2.
    TRANSLATE str1+0(1)   TO UPPER CASE.
    TRANSLATE str2+0(1)   TO UPPER CASE.
    CONCATENATE str1 str2 INTO lw_payloc-print_loc.

***-----make first letter capital for pay loc ----------------------****
    SPLIT lw_payloc-pay_loc AT space INTO str3 str4.
    TRANSLATE str3+0(1)   TO UPPER CASE.
    TRANSLATE str4+0(1)   TO UPPER CASE.
    CONCATENATE str3 str4 INTO lw_payloc-pay_loc.

    MODIFY g_tab_zaxis_tab_payloc FROM lw_payloc.
    CLEAR : lw_payloc.
  ENDLOOP.
ENDFORM.                    " read_print_loc


*&---------------------------------------------------------------------*
*&      Form  COLLECT_BSAK_PYMNT
*&---------------------------------------------------------------------*
*       If Multiple line items are there then collct all into one
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_bsak_pymnt .

  CLEAR : wa_bsak.
  LOOP AT g_tab_bsak_pymnt ASSIGNING <fs_bsak>.

    LOOP AT g_tab_bsak_pymnt INTO wa_bsak WHERE bukrs = <fs_bsak>-bukrs
                                          AND gjahr = <fs_bsak>-gjahr
                                          AND belnr = <fs_bsak>-belnr
                                          AND buzei NE <fs_bsak>-buzei.
      IF <fs_bsak>-shkzg EQ 'S'.

        <fs_bsak>-dmbtr = <fs_bsak>-dmbtr +  wa_bsak-dmbtr.
        <fs_bsak>-wrbtr = <fs_bsak>-wrbtr +  wa_bsak-wrbtr.
        <fs_bsak>-qbshb = <fs_bsak>-qbshb +  wa_bsak-qbshb.

      ELSEIF <fs_bsak>-shkzg EQ 'H'.

        <fs_bsak>-dmbtr = <fs_bsak>-dmbtr -  wa_bsak-dmbtr.
        <fs_bsak>-wrbtr = <fs_bsak>-wrbtr -  wa_bsak-wrbtr.
        <fs_bsak>-qbshb = <fs_bsak>-qbshb -  wa_bsak-qbshb.

      ENDIF.
      DELETE g_tab_bsak_pymnt.
      CLEAR : wa_bsak.
    ENDLOOP.
  ENDLOOP.

*&---------------------------------------------------------------------*
*       Take Exact payment Amount from bsis table GL line item
*----------------------------------------------------------------------*
  CLEAR : wa_bsis,wa_bkpf,wa_bvor.
  LOOP AT g_tab_bsak_pymnt ASSIGNING <fs_bsak>.
***-------------for normal payments----------------------------------***

    READ TABLE g_tab_bsis INTO wa_bsis WITH KEY belnr = <fs_bsak>-belnr
                                                gjahr = <fs_bsak>-gjahr
                                                bukrs = <fs_bsak>-bukrs.
    IF sy-subrc EQ 0.
      MOVE wa_bsis-dmbtr TO <fs_bsak>-dmbtr.
      MOVE wa_bsis-wrbtr TO <fs_bsak>-wrbtr.
    ELSE.

***-------------for Cross Company payments----------------------------------***
      READ TABLE g_tab_bvor INTO wa_bvor WITH KEY belnr = <fs_bsak>-belnr
                                                  gjahr = <fs_bsak>-gjahr
                                                  bukrs = <fs_bsak>-bukrs.

      READ TABLE g_tab_bvor INTO wa_bkpf WITH KEY bvorg = wa_bvor-bvorg
                                                  gjahr = wa_bvor-gjahr.

      READ TABLE g_tab_bsis INTO wa_bsis WITH KEY belnr = wa_bkpf-belnr
                                                  gjahr = wa_bkpf-gjahr
                                                  bukrs = wa_bkpf-bukrs.
      IF sy-subrc EQ 0.
        MOVE wa_bsis-dmbtr TO <fs_bsak>-dmbtr.
        MOVE wa_bsis-wrbtr TO <fs_bsak>-wrbtr.
      ENDIF.
    ENDIF.
    CLEAR : wa_bsis,wa_bkpf,wa_bvor.
  ENDLOOP.
  CLEAR : wa_bsis,wa_bkpf,wa_bvor.
ENDFORM.                    " COLLECT_BSAK_PYMNT


*&---------------------------------------------------------------------*
*&      Form  UPDATE_INV_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_inv_details .
*&---------------------------------------------------------------------*
*      Update partner bank type
*----------------------------------------------------------------------*
  LOOP AT g_tab_bsak_pymnt ASSIGNING <fs_bsak> WHERE bvtyp IS INITIAL.
    LOOP AT g_tab_bsak INTO bsak WHERE augbl = <fs_bsak>-belnr
                               AND   bukrs = <fs_bsak>-bukrs
                               AND   augdt = <fs_bsak>-augdt
                               AND   bvtyp <> space.

      IF bsak-bvtyp IS NOT INITIAL.
        <fs_bsak>-bvtyp = bsak-bvtyp.
        CLEAR : bsak.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
*&---------------------------------------------------------------------*
*      Update individual payee
*----------------------------------------------------------------------*
  LOOP AT g_tab_bsak_pymnt ASSIGNING <fs_bsak>.
    LOOP AT g_tab_bsak INTO bsak WHERE augbl = <fs_bsak>-belnr
                               AND   bukrs = <fs_bsak>-bukrs
                               AND   augdt = <fs_bsak>-augdt
                               AND   empfb <> space.
      IF bsak-empfb IS NOT INITIAL.
        <fs_bsak>-empfb = bsak-empfb.
        CLEAR : bsak.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " UPDATE_INV_DETAILS


*&---------------------------------------------------------------------*
*&      Form  PAYMENT_METHOD_UPDATING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM payment_method_updating .

  IF p_auto NE abap_true.  "'X'.  " FOR MANUAL PAYMENTS.

    LOOP AT g_tab_bsak_pymnt ASSIGNING <fs_bsak> WHERE zlsch IS INITIAL.
      READ TABLE g_tab_payr INTO payr WITH KEY vblnr = <fs_bsak>-belnr
                                             zbukr = <fs_bsak>-bukrs
                                             gjahr = <fs_bsak>-gjahr.
      IF sy-subrc = 0.
        IF payr-rzawe IS NOT INITIAL .    " IF PAYMENT METHOD MAIINTAINED IN F-58 THEN PICK IT
          <fs_bsak>-zlsch = payr-rzawe.
        ELSE.
          <fs_bsak>-zlsch = 'C'.
        ENDIF.
      ELSE.               " IF PAYMENT METHOD MAIINTAINED IN INV THEN PICK IT

        LOOP AT g_tab_bsak INTO bsak WHERE augbl = <fs_bsak>-belnr
                                     AND   bukrs = <fs_bsak>-bukrs
                                     AND   augdt = <fs_bsak>-augdt
                                     AND   zlsch NE space.
          IF bsak-zlsch IS NOT INITIAL.
            <fs_bsak>-zlsch = bsak-zlsch.
            EXIT.
          ENDIF.
          CLEAR : bsak.
        ENDLOOP.    " g_tab_BSAK
      ENDIF.         " SY-SUBRC
      CLEAR : payr.
    ENDLOOP.       " g_tab_BSAK_PYMNT

  ELSE.        " FOR AUTOMATIC PAYMENTS.

    LOOP AT g_tab_bsak_pymnt ASSIGNING <fs_bsak>.
      READ TABLE g_tab_reguh INTO wa_g_tab_reguh WITH KEY vblnr = <fs_bsak>-belnr
                                                          zaldt = <fs_bsak>-budat
                                                          zbukr = <fs_bsak>-bukrs.
      IF sy-subrc EQ 0.

        IF wa_g_tab_reguh-rzawe IS NOT INITIAL.
          <fs_bsak>-zlsch = wa_g_tab_reguh-rzawe.    " pay mtd for automatic payments
        ENDIF.
        IF wa_g_tab_reguh-zbvty IS NOT INITIAL.
          <fs_bsak>-bvtyp = wa_g_tab_reguh-zbvty.    " bank type for the automatic payments
        ENDIF.
      ENDIF.
      CLEAR : wa_g_tab_reguh.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " PAYMENT_METHOD_UPDATING

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_PAYMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_payments .
*&---------------------------------------------------------------------*
*     **----check currency
*----------------------------------------------------------------------*
  LOOP AT g_tab_bsak_pymnt INTO bsak1.

    IF bsak1-waers NE 'INR'.
      PERFORM append_display USING TEXT-e23     " not allowd
                                   TEXT-e12     " Document with Non INR currency was not able to extract
                                   c_nrm_colour.
      DELETE g_tab_bsak_pymnt.
    ENDIF.
    CLEAR : bsak1.
  ENDLOOP.
*&---------------------------------------------------------------------*
*     **----check ZERO AMOUNT
*----------------------------------------------------------------------*
  LOOP AT g_tab_bsak_pymnt INTO bsak1.

    IF bsak1-dmbtr IS INITIAL OR bsak1-dmbtr EQ '0.00' OR bsak1-dmbtr EQ '0'.
      PERFORM append_display USING TEXT-e23     " not allowd
                                   TEXT-e13     " Document with zero amount was not able to extract
                                   c_nrm_colour.
      DELETE g_tab_bsak_pymnt.
    ENDIF.
    CLEAR : bsak1.
  ENDLOOP.
*&---------------------------------------------------------------------*
*     **----check CORPORATE amount limit
*----------------------------------------------------------------------*
  CLEAR : wa_convers.
  READ TABLE git_convers INTO wa_convers WITH KEY bank_code   = ' '
                                                  record_type = ' '
                                                  fieldname   = ' '
                                                  oldvalue    = 'AMT_LIMIT'.
  IF sy-subrc = 0 AND wa_convers-newvalue IS NOT INITIAL AND
                      wa_convers-newvalue NE '0' AND
                      wa_convers-newvalue NE '0.00' .

    LOOP AT g_tab_bsak_pymnt INTO bsak1.
      IF bsak1-dmbtr GT wa_convers-newvalue. "#EC CI_FLDEXT_OK[2610650]

        "Added by SPLABAP during code remediation
        PERFORM append_display USING TEXT-e23     " Not Allowed
                                     TEXT-e20     " Amount Limit was exceded so this Document was Avoided for E-Payment Process.
                                     c_nrm_colour.
        DELETE g_tab_bsak_pymnt.
      ENDIF.
      CLEAR : bsak1.
    ENDLOOP.
  ENDIF.
*&---------------------------------------------------------------------*
*     **----check processing date.
*----------------------------------------------------------------------*
  LOOP AT g_tab_bsak_pymnt INTO bsak1.
    IF bsak1-budat LE sy-datum.  " For future dated payments.
      bsak1-budat = sy-datum.
      MODIFY g_tab_bsak_pymnt FROM bsak1.

    ELSEIF bsak1-budat GT sy-datum.
      CALL FUNCTION 'CALCULATE_DATE'
        EXPORTING
          days        = lc_date
          months      = '0'
          start_date  = sy-datum
        IMPORTING
          result_date = gv_budat.

      IF bsak1-budat GT gv_budat.
        PERFORM append_display USING TEXT-e23     " Not Allowed
                                     TEXT-e14     " Invalid processing date so Document was not able to extract
                                     c_nrm_colour.
        DELETE g_tab_bsak_pymnt.
      ENDIF.

    ENDIF.
    CLEAR : bsak1.
  ENDLOOP.
*&---------------------------------------------------------------------*
*     **----check REVERSE FEED STATUS FOR RE EXTRACTION
*----------------------------------------------------------------------*
  IF sy-tcode = 'ZAXIS_REXTRA' AND g_tab_bsak_pymnt[] IS NOT INITIAL.

    SELECT * FROM zaxis_tab_ctltab INTO TABLE g_it_ctltab
                           FOR ALL ENTRIES IN g_tab_bsak_pymnt
                           WHERE bukrs = g_tab_bsak_pymnt-bukrs AND
                                 belnr = g_tab_bsak_pymnt-augbl AND
                                 gjahr = g_tab_bsak_pymnt-gjahr.

    CLEAR bsak1.
    LOOP AT g_tab_bsak_pymnt INTO bsak1.

      READ TABLE g_it_ctltab INTO wa_ctltab WITH KEY bukrs = bsak1-bukrs
                                                     belnr = bsak1-augbl
                                                     gjahr = bsak1-gjahr.
      IF sy-subrc = 0.

        IF wa_ctltab-utr_number IS NOT INITIAL
        AND ( wa_ctltab-reason+0(6) NE 'RETURN'
          AND wa_ctltab-reason+0(8) NE 'REJECTED'
          AND wa_ctltab-reason+0(7) EQ 'SUCCESS' ).

          DELETE g_tab_bsak_pymnt.

          CLEAR : gv_text.
          lfa1-lifnr     = bsak1-lifnr.
          CONCATENATE TEXT-e17 TEXT-504 wa_ctltab-utr_number INTO gv_text SEPARATED BY space.
          PERFORM append_display USING TEXT-e23     " Not Allowed
                                       gv_text      " Transaction has been processed successfully
                                       c_nrm_colour.
          CLEAR : gv_text,lfa1-lifnr.
        ENDIF.
      ENDIF.
      CLEAR : bsak1, wa_ctltab.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " VALIDATE_PAYMENTS
