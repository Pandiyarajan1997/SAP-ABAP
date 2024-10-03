*&---------------------------------------------------------------------*
*& Include          ZDS_MRPPRICE_SUBROUTINES
*&---------------------------------------------------------------------*
** Selection screen **
DATA: lv_matnr TYPE mara-matnr.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_matnr FOR lv_matnr.

  SELECTION-SCREEN SKIP.
  PARAMETERS p_run AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& Form f_get_filtervalues
*&---------------------------------------------------------------------*
FORM f_get_filtervalues .
** Region Filter **
  SELECT * FROM tvarvc
           INTO TABLE @DATA(lt_tvarvc)
           WHERE name = 'ZMRP_REGION'
           AND type = 'S'.
  LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).
    CLEAR ls_regio.
    ls_regio-sign = 'I'.
    ls_regio-option = 'EQ'.
    ls_regio-low = ls_tvarvc-low.
    APPEND ls_regio TO lt_regio.
    CLEAR ls_tvarvc.
  ENDLOOP.
** Sales org Filter **
  SELECT * FROM tvarvc
           INTO TABLE @DATA(lt_tvarvc1)
           WHERE name = 'ZMRP_SALORG'
           AND type = 'S'.
  LOOP AT lt_tvarvc1 INTO DATA(ls_tvarvc1).
    CLEAR ls_vkorg.
    ls_vkorg-sign = 'I'.
    ls_vkorg-option = 'EQ'.
    ls_vkorg-low = ls_tvarvc1-low.
    APPEND ls_vkorg TO lt_vkorg.
    CLEAR ls_tvarvc1.
  ENDLOOP.
** Plant Filter **
  SELECT * FROM tvarvc
         INTO TABLE @DATA(lt_tvarvc2)
         WHERE name = 'ZMRP_PLANT'
         AND type = 'S'.
  LOOP AT lt_tvarvc2 INTO DATA(ls_tvarvc2).
    CLEAR ls_werks.
    ls_werks-sign = 'I'.
    ls_werks-option = 'EQ'.
    ls_werks-low = ls_tvarvc2-low.
    APPEND ls_werks TO lt_werks.
    CLEAR ls_tvarvc1.
  ENDLOOP.
  lt_werks1[] = lt_werks[].
  lt_werks2[] = lt_werks[].
  DELETE lt_werks1 WHERE low = '1401'.
  DELETE lt_werks2 WHERE low NE '1401'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_initial_selection
*&---------------------------------------------------------------------*
FORM f_initial_selection .
** PR00 Condition Record Based on Region **
  REFRESH gt_a832.
  SELECT kappl
         kschl
         regio
         matnr
         datbi
         datab
         knumh FROM a832
               INTO TABLE gt_a832
               WHERE kappl = 'V'
               AND kschl = 'PR00'
               AND regio IN lt_regio
               AND matnr IN s_matnr
               AND datbi GE sy-datum
               AND datab LE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_a832[] BY matnr regio.
    DELETE ADJACENT DUPLICATES FROM gt_a832[] COMPARING matnr regio.
  ENDIF.

  IF gt_a832[] IS NOT INITIAL.
** Condition Price From KONP **
    REFRESH gt_konp.
    SELECT knumh
           kappl
           kschl
           kbetr
           konwa
           kmein FROM konp
                 INTO TABLE gt_konp
                 FOR ALL ENTRIES IN gt_a832
                 WHERE knumh EQ gt_a832-knumh
                 AND kappl = 'V'
                 AND kschl EQ 'PR00'
                 AND loevm_ko NE 'X'.
    IF sy-subrc EQ 0.
      SORT gt_konp[] BY knumh.
    ENDIF.
** deletion a832 table **
    LOOP AT gt_a832 INTO gs_a832.
      CLEAR gs_konp.
      READ TABLE gt_konp INTO gs_konp WITH KEY knumh = gs_a832-knumh BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE gt_a832 WHERE matnr EQ gs_a832-matnr AND regio NE gs_a832-regio.
      ENDIF.
    ENDLOOP.
** MRP Price Condition Records Selection **
    REFRESH gt_a055.
    SELECT * FROM a055
             INTO TABLE gt_a055
             FOR ALL ENTRIES IN gt_a832
             WHERE kappl = 'V'
             AND kschl = 'PI01'
             AND vkorgau IN lt_vkorg
             AND werks IN lt_werks
             AND matnr EQ gt_a832-matnr
             AND datbi GE sy-datum
             AND datab LE sy-datum.
    IF sy-subrc EQ 0.
      SORT gt_a055[] BY vkorgau werks matnr.
      gt1_a055[] = gt_a055[].
      DELETE gt_a055[] WHERE vkorgau = '1000' AND werks = '1401'.
      DELETE gt_a055[] WHERE vkorgau = '1400'.
      DELETE gt1_a055[] WHERE vkorgau = '1400' AND werks NE '1401'.
      DELETE gt1_a055[] WHERE vkorgau = '1000'.
** Condition Price From KONP **
      REFRESH gt_konp1.
      SELECT knumh
             kappl
             kschl
             kbetr
             konwa
             kmein FROM konp
                   INTO TABLE gt_konp1
                   FOR ALL ENTRIES IN gt_a055
                   WHERE knumh EQ gt_a055-knumh
                   AND kappl = 'V'
                   AND kschl EQ 'PI01'
                   AND loevm_ko NE 'X'..
      IF sy-subrc EQ 0.
        SORT gt_konp1[] BY knumh.
      ENDIF.
    ENDIF.
  ENDIF.

** If Material is not present in a832 **
  SELECT * FROM a304
         INTO TABLE gt_a304
         WHERE kappl = 'V'
         AND kschl = 'PR00'
         AND vkorg = '1000'
         AND matnr IN s_matnr
         AND datbi GE sy-datum
         AND datab LE sy-datum.
  IF gt_a304[] IS NOT INITIAL.
    DATA: lv_tavbix_a832 TYPE sy-tabix.

    IF gt_a832[] IS NOT INITIAL.
      LOOP AT gt_a832 INTO gs_a832.
        lv_tavbix_a832 = sy-tabix.
        READ TABLE gt_konp INTO gs_konp WITH KEY knumh = gs_a832-knumh.
        IF sy-subrc = 0 .
          DELETE gt_a304[] WHERE matnr EQ gs_a832-matnr.
          CLEAR gs_a832.
        ELSE.
          DELETE gt_a832 INDEX lv_tavbix_a832.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF gt_a304[] IS NOT INITIAL.

      REFRESH gt2_a055.
      SELECT * FROM a055
               INTO TABLE gt2_a055
               FOR ALL ENTRIES IN gt_a304
               WHERE kappl = 'V'
               AND kschl = 'PI01'
               AND vkorgau IN lt_vkorg
               AND werks IN lt_werks
               AND matnr EQ gt_a304-matnr
               AND datbi GE sy-datum
               AND datab LE sy-datum.
      IF sy-subrc EQ 0.
        SORT gt2_a055[] BY vkorgau werks matnr.
        gt3_a055[] = gt2_a055[].
        DELETE gt2_a055[] WHERE vkorgau = '1000' AND werks = '1401'.
        DELETE gt2_a055[] WHERE vkorgau = '1400'.
        DELETE gt3_a055[] WHERE vkorgau = '1400' AND werks NE '1401'.
        DELETE gt3_a055[] WHERE vkorgau = '1000'.

        REFRESH gt_konp3.
        SELECT knumh
               kappl
               kschl
               kbetr
               konwa
               kmein FROM konp
                     INTO TABLE gt_konp3
                     FOR ALL ENTRIES IN gt2_a055
                     WHERE knumh EQ gt2_a055-knumh
                     AND kappl = 'V'
                     AND kschl EQ 'PI01'
                     AND loevm_ko NE 'X'..
        IF sy-subrc EQ 0.
          SORT gt_konp3[] BY knumh.
        ENDIF.
        REFRESH gt_konp4.
        SELECT knumh
               kappl
               kschl
               kbetr
               konwa
               kmein FROM konp
                     INTO TABLE gt_konp4
                     FOR ALL ENTRIES IN gt3_a055
                     WHERE knumh EQ gt3_a055-knumh
                     AND kappl = 'V'
                     AND kschl EQ 'PI01'
                     AND loevm_ko NE 'X'..
        IF sy-subrc EQ 0.
          SORT gt_konp4[] BY knumh.
        ENDIF.
      ENDIF.
** Condition amount **
      SELECT knumh
             kappl
             kschl
             kbetr
             konwa
             kmein FROM konp
                   INTO TABLE gt_konp2
                   FOR ALL ENTRIES IN gt_a304
                   WHERE knumh EQ gt_a304-knumh
                   AND kappl = 'V'
                   AND kschl EQ 'PR00'
                   AND loevm_ko NE 'X'.
      IF sy-subrc EQ 0.
        SORT gt_konp2[] BY knumh.
      ENDIF.
    ENDIF.
  ENDIF.
** Material Mrp percentage from Material Master **
  REFRESH gt_mara.
  SELECT matnr
         zmrp_percen FROM mara
                     INTO TABLE gt_mara.
  IF sy-subrc EQ 0.
    SORT gt_mara[] BY matnr.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_actual_process
*&---------------------------------------------------------------------*
FORM f_actual_process .
  REFRESH gt_alv.
  LOOP AT gt_a832 INTO gs_a832.
** PR00 Price and Material Grp Percentage calculation **
    CLEAR:gs_konp,lv_pri_pr00,lv_uom.
    READ TABLE gt_konp INTO gs_konp WITH KEY knumh = gs_a832-knumh BINARY SEARCH.
    IF sy-subrc EQ 0.
** PR00 Price is not there this for some where it has zero price **
      IF gs_konp-kbetr IS INITIAL.
        CLEAR gs_alv.
        gs_alv-matnr = gs_a832-matnr.
        gs_alv-vkorg = gs_a055-vkorgau.
        gs_alv-plant = gs_a055-werks.
        gs_alv-msg = 'PR00 price is not available'.
        APPEND gs_alv TO gt_alv.
        CONTINUE.
      ELSE.
        lv_pri_pr00 = gs_konp-kbetr. " PR00 Price
        lv_uom = gs_konp-kmein.
      ENDIF.
** To get Material Group from Material master to which grp is material Mapped **
      CLEAR gs_mara.
      READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_a832-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_price = ( lv_pri_pr00 * gs_mara-zmrp_percen ) / 100.
        CLEAR: lv_calprice,lv_final.
        lv_calprice = lv_pri_pr00 + lv_price. " Final calculated Price
** Price variable for Checks if both PR00 And PI01 is same or not **
        lv_final = lv_calprice.
        CLEAR lv_calprice.
        lv_calprice = lv_final.
*** If Material Mrp percentage is not there  ***
        IF gs_mara-zmrp_percen IS INITIAL.
          CLEAR gs_alv.
          gs_alv-matnr = gs_a832-matnr.
          gs_alv-msg = 'Percentage is not set'.
          APPEND gs_alv TO gt_alv.
          CONTINUE.
        ENDIF.
      ENDIF.
** sales organization 1000 **
      LOOP AT lt_werks1 INTO ls_werks1.
        LOOP AT gt_a055 INTO gs_a055 WHERE matnr = gs_a832-matnr AND werks = ls_werks1-low.
          CLEAR: gs_konp1,lv_pri_pi01.
          READ TABLE gt_konp1 INTO gs_konp1 WITH KEY knumh = gs_a055-knumh BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_pri_pi01 = gs_konp1-kbetr. "PI01 price
          ENDIF.
** Checking PR00 And PI01 Prices If Same Does Not change **
          IF lv_final EQ lv_pri_pi01.
            IF p_run NE abap_true.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a832-matnr.
              gs_alv-vkorg = '1000'.
              gs_alv-plant = gs_a055-werks.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'MRP already updated in Same Price'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ELSE.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a832-matnr.
              gs_alv-vkorg = '1000'.
              gs_alv-plant = gs_a055-werks.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'Test Run Display'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ENDIF.
          ELSE.
            IF p_run = abap_true.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a832-matnr.
              gs_alv-vkorg = '1000'.
              gs_alv-plant = gs_a055-werks.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'Test run display'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ELSE.
              PERFORM condition_change.  "Condition Record Change
              CONTINUE.
            ENDIF.
          ENDIF.
          CLEAR gs_a055.
        ENDLOOP.
** IF condition record does not exists **
        IF sy-subrc NE 0.
          IF p_run = abap_true.
            CLEAR gs_alv.
            gs_alv-matnr = gs_a832-matnr.
            gs_alv-vkorg = '1000'.
            gs_alv-plant = gs_a055-werks.
            gs_alv-dpl = lv_pri_pr00.
            gs_alv-percen = gs_mara-zmrp_percen.
            gs_alv-old_pri = lv_pri_pi01.
            gs_alv-price = lv_final.
            gs_alv-uom = lv_uom.
            gs_alv-msg = 'Test run display'.
            APPEND gs_alv TO gt_alv.
            CONTINUE.
          ELSE.
            PERFORM condition_create. " Condition Record Create
            CONTINUE.
          ENDIF.
        ENDIF.
        CLEAR ls_werks1.
      ENDLOOP.
      CLEAR ls_werks1.
** Sales Organization 1400 **
      LOOP AT lt_werks2 INTO ls_werks2.
        LOOP AT gt1_a055 INTO gs1_a055 WHERE matnr = gs_a832-matnr
                                      AND werks = ls_werks2-low.
** If MRP condition record already exists Means **
          CLEAR gs_konp1.
          READ TABLE gt_konp1 INTO gs_konp1 WITH KEY knumh = gs1_a055-knumh BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_pri_pi01 = gs_konp1-kbetr. " PI01 Price
          ENDIF.
** Checking PR00 And PI01 Prices If Same Does Not change **
          IF lv_final EQ lv_pri_pi01.
            IF p_run NE abap_true.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a832-matnr.
              gs_alv-vkorg = '1400'.
              gs_alv-plant = '1401'.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'MRP already updated in Same Price'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ELSE.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a832-matnr.
              gs_alv-vkorg = '1400'.
              gs_alv-plant = '1401'.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'Test run display'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ENDIF.
** If MRP price is not same means Updating ***
          ELSE.
            IF p_run = abap_true.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a832-matnr.
              gs_alv-vkorg = '1400'.
              gs_alv-plant = '1401'.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'Test run display'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ELSE.
              PERFORM condition_change.  "Condition Record Change
              CONTINUE.
            ENDIF.
          ENDIF.
          CLEAR gs1_a055.
        ENDLOOP.
** If MRP Condition Record Does not exists **
        IF sy-subrc NE 0.
          IF p_run = abap_true.
            CLEAR gs_alv.
            gs_alv-matnr = gs_a832-matnr.
            gs_alv-vkorg = '1400'.
            gs_alv-plant = '1401'.
            gs_alv-dpl = lv_pri_pr00.
            gs_alv-percen = gs_mara-zmrp_percen.
            gs_alv-old_pri = lv_pri_pi01.
            gs_alv-price = lv_final.
            gs_alv-uom = lv_uom.
            gs_alv-msg = 'Test run display'.
            APPEND gs_alv TO gt_alv.
            CONTINUE.
          ELSE.
            PERFORM condition_create. " Condition Record Create
            CONTINUE.
          ENDIF.
        ENDIF.
        CLEAR ls_werks2.
      ENDLOOP.
      CLEAR ls_werks2.
** End of sales org 1400 **
    ENDIF.
    CLEAR gs_a832.
  ENDLOOP.

** If the price is not set for both region we have to check this Material with release status **
  LOOP AT gt_a304 INTO gs_a304.
** PR00 Price and Material Grp Percentage calculation **

    CLEAR:gs_konp2,lv_pri_pr00,lv_uom.
    READ TABLE gt_konp2 INTO gs_konp2 WITH KEY knumh = gs_a304-knumh BINARY SEARCH.
    IF sy-subrc EQ 0.
** PR00 Price is not there this for some where it has zero price **
      IF gs_konp2-kbetr IS INITIAL.
        CLEAR gs_alv.
        gs_alv-matnr = gs_a304-matnr.
        gs_alv-vkorg = gs_a055-vkorgau.
        gs_alv-plant = gs_a055-werks.
        gs_alv-msg = 'PR00 price is not available'.
        APPEND gs_alv TO gt_alv.
        CONTINUE.
      ELSE.
        lv_pri_pr00 = gs_konp2-kbetr. " PR00 Price
        lv_uom = gs_konp2-kmein.
      ENDIF.
** To get Material Group from Material master to which grp is material Mapped **
      CLEAR gs_mara.
      READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_a304-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_price = ( lv_pri_pr00 * gs_mara-zmrp_percen ) / 100.
        CLEAR: lv_calprice,lv_final.
        lv_calprice = lv_pri_pr00 + lv_price. " Final calculated Price
** Price variable for Checks if both PR00 And PI01 is same or not **
        lv_final = lv_calprice.
        CLEAR lv_calprice.
        lv_calprice = lv_final.
*** If Material Mrp percentage is not there  ***
        IF gs_mara-zmrp_percen IS INITIAL.
          CLEAR gs_alv.
          gs_alv-matnr = gs_a832-matnr.
          gs_alv-msg = 'Percentage is not set'.
          APPEND gs_alv TO gt_alv.
          CONTINUE.
        ENDIF.
      ENDIF.
** sales organization 1000 **
      LOOP AT lt_werks1 INTO ls_werks1.
        LOOP AT gt2_a055 INTO gs2_a055 WHERE matnr = gs_a304-matnr AND werks = ls_werks1-low.
          CLEAR: gs_konp1,lv_pri_pi01.
          READ TABLE gt_konp3 INTO gs_konp3 WITH KEY knumh = gs2_a055-knumh BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_pri_pi01 = gs_konp3-kbetr. "PI01 price
          ENDIF.
** Checking PR00 And PI01 Prices If Same Does Not change **
          IF lv_final EQ lv_pri_pi01.
            IF p_run NE abap_true.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a304-matnr.
              gs_alv-vkorg = '1000'.
              gs_alv-plant = gs2_a055-werks.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'MRP already updated in Same Price'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ELSE.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a304-matnr.
              gs_alv-vkorg = '1000'.
              gs_alv-plant = gs2_a055-werks.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'Test run display'.
              APPEND gs_alv TO gt_alv.
            ENDIF.
          ELSE.
            IF p_run = abap_true.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a304-matnr.
              gs_alv-vkorg = '1000'.
              gs_alv-plant = gs2_a055-werks.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'Test run display'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ELSE.
              PERFORM condition_change2.  "Condition Record Change
              CONTINUE.
            ENDIF.
          ENDIF.
          CLEAR gs2_a055.
        ENDLOOP.
** IF condition record does not exists **
        IF sy-subrc NE 0.
          IF p_run = abap_true.
            CLEAR gs_alv.
            gs_alv-matnr = gs_a304-matnr.
            gs_alv-vkorg = '1000'.
            gs_alv-plant = gs2_a055-werks.
            gs_alv-dpl = lv_pri_pr00.
            gs_alv-percen = gs_mara-zmrp_percen.
            gs_alv-old_pri = lv_pri_pi01.
            gs_alv-price = lv_final.
            gs_alv-uom = lv_uom.
            gs_alv-msg = 'Test run display'.
            APPEND gs_alv TO gt_alv.
            CONTINUE.
          ELSE.
            PERFORM condition_create2. " Condition Record Create
            CONTINUE.
          ENDIF.
        ENDIF.
        CLEAR ls_werks1.
      ENDLOOP.
      CLEAR ls_werks1.

      LOOP AT lt_werks2 INTO ls_werks2.
        LOOP AT gt3_a055 INTO gs3_a055 WHERE matnr = gs_a304-matnr
                                      AND werks = ls_werks2-low.
** If MRP condition record already exists Means **
          CLEAR gs_konp4.
          READ TABLE gt_konp4 INTO gs_konp4 WITH KEY knumh = gs3_a055-knumh BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_pri_pi01 = gs_konp4-kbetr. " PI01 Price
          ENDIF.
** Checking PR00 And PI01 Prices If Same Does Not change **
          IF lv_final EQ lv_pri_pi01.
            IF p_run NE abap_true.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a304-matnr.
              gs_alv-vkorg = '1400'.
              gs_alv-plant = '1401'.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'MRP already updated in Same Price'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ELSE.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a304-matnr.
              gs_alv-vkorg = '1400'.
              gs_alv-plant = '1401'.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'Test run display'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ENDIF.
** If MRP price is not same means Updating ***
          ELSE.
            IF p_run = abap_true.
              CLEAR gs_alv.
              gs_alv-matnr = gs_a304-matnr.
              gs_alv-vkorg = '1400'.
              gs_alv-plant = '1401'.
              gs_alv-dpl = lv_pri_pr00.
              gs_alv-percen = gs_mara-zmrp_percen.
              gs_alv-old_pri = lv_pri_pi01.
              gs_alv-price = lv_final.
              gs_alv-uom = lv_uom.
              gs_alv-msg = 'Test run display'.
              APPEND gs_alv TO gt_alv.
              CONTINUE.
            ELSE.
              PERFORM condition_change2.  "Condition Record Change
              CONTINUE.
            ENDIF.
          ENDIF.
          CLEAR gs3_a055.
        ENDLOOP.
** If MRP Condition Record Does not exists **
        IF sy-subrc NE 0.
          IF p_run = abap_true.
            CLEAR gs_alv.
            gs_alv-matnr = gs_a304-matnr.
            gs_alv-vkorg = '1400'.
            gs_alv-plant = '1401'.
            gs_alv-dpl = lv_pri_pr00.
            gs_alv-percen = gs_mara-zmrp_percen.
            gs_alv-old_pri = lv_pri_pi01.
            gs_alv-price = lv_final.
            gs_alv-uom = lv_uom.
            gs_alv-msg = 'Test run display'.
            APPEND gs_alv TO gt_alv.
            CONTINUE.
          ELSE.
            PERFORM condition_create2. " Condition Record Create
            CONTINUE.
          ENDIF.
        ENDIF.
        CLEAR ls_werks2.
      ENDLOOP.
      CLEAR ls_werks2.
    ENDIF.
    CLEAR gs_a304.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form condition_change
*&---------------------------------------------------------------------*
FORM condition_change.
*    start bdc update.
  CLEAR: lv_org,lv_werks.
  IF ls_werks1-low = '1003' OR ls_werks1-low = '1009'.
    lv_org = '1000'.
    WRITE ls_werks1-low TO lv_werks.
  ELSEIF ls_werks2-low = '1401'.
    lv_org = '1400'.
    WRITE ls_werks2-low TO lv_werks.
  ENDIF.

  DATA: lv_matnr TYPE matnr.
  IF gs_a055-matnr IS NOT INITIAL.
    CLEAR lv_matnr.
    lv_matnr = gs_a055-matnr.
  ELSE.
    CLEAR lv_matnr.
    lv_matnr = gs1_a055-matnr.
  ENDIF.
  WRITE sy-datum TO lv_datum DD/MM/YYYY.

  REFRESH it_bdcdata.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-KSCHL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RV13A-KSCHL'
                                'PI01'.
  PERFORM bdc_dynpro      USING 'RV13A055' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'SEL_DATE'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ONLI'.
  PERFORM bdc_field       USING 'F001'
                                lv_org.
  PERFORM bdc_field       USING 'F002'
                                lv_werks.
  PERFORM bdc_field       USING 'F003-LOW'
                                lv_matnr.
  PERFORM bdc_field       USING 'SEL_DATE'
                                lv_datum.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONP-KBETR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                lv_calprice.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-DATAB(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RV13A-DATAB(01)'
                                lv_datum.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-DATAB(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
  PERFORM bdc_transaction USING 'VK12'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form condition_change
*&---------------------------------------------------------------------*
FORM condition_change2.
*    start bdc update.
  CLEAR: lv_org,lv_werks.
  IF ls_werks1-low = '1003' OR ls_werks1-low = '1009'.
    lv_org = '1000'.
    WRITE ls_werks1-low TO lv_werks.
  ELSEIF ls_werks2-low = '1401'.
    lv_org = '1400'.
    WRITE ls_werks2-low TO lv_werks.
  ENDIF.
  DATA: lv_matnr TYPE matnr.
  IF gs2_a055-matnr IS NOT INITIAL.
    CLEAR lv_matnr.
    lv_matnr = gs2_a055-matnr.
  ELSE.
    CLEAR lv_matnr.
    lv_matnr = gs3_a055-matnr.
  ENDIF.

  WRITE sy-datum TO lv_datum DD/MM/YYYY.

  REFRESH it_bdcdata.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-KSCHL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RV13A-KSCHL'
                                'PI01'.
  PERFORM bdc_dynpro      USING 'RV13A055' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'SEL_DATE'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ONLI'.
  PERFORM bdc_field       USING 'F001'
                                lv_org.
  PERFORM bdc_field       USING 'F002'
                                lv_werks.
  PERFORM bdc_field       USING 'F003-LOW'
                                lv_matnr.
  PERFORM bdc_field       USING 'SEL_DATE'
                                lv_datum.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONP-KBETR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                lv_calprice.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-DATAB(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RV13A-DATAB(01)'
                                lv_datum.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-DATAB(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
  PERFORM bdc_transaction USING 'VK12'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form condition_create
*&---------------------------------------------------------------------*
FORM condition_create .

  CLEAR: lv_org,lv_werks.
  IF ls_werks1-low = '1003' OR ls_werks1-low = '1009'.
    lv_org = '1000'.
    WRITE ls_werks1-low TO lv_werks.
  ELSEIF ls_werks2-low = '1401'.
    lv_org = '1400'.
    WRITE ls_werks2-low TO lv_werks.
  ENDIF.

  WRITE sy-datum TO lv_datum DD/MM/YYYY.
  REFRESH it_bdcdata.
  IF lv_uom EQ 'BOT'.
    CLEAR lv_uom1.
    lv_uom1 = 'BT'.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RV13A-KSCHL'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RV13A-KSCHL'
                                  'PI01'.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'KONP-KMEIN(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'KOMG-VKORGAU'
                                  lv_org.
    PERFORM bdc_field       USING 'KOMG-WERKS'
                                  lv_werks.
    PERFORM bdc_field       USING 'KOMG-MATNR(01)'
                                  gs_a832-matnr.
    PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                  lv_calprice.
    PERFORM bdc_field       USING 'KONP-KONWA(01)'
                                  'inr'.
    PERFORM bdc_field       USING 'KONP-KMEIN(01)'
                                  lv_uom1.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'KOMG-MATNR(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SICH'.
    PERFORM bdc_transaction USING 'VK11'.
  ELSE.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RV13A-KSCHL'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RV13A-KSCHL'
                                  'PI01'.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'KONP-KMEIN(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'KOMG-VKORGAU'
                                  lv_org.
    PERFORM bdc_field       USING 'KOMG-WERKS'
                                  lv_werks.
    PERFORM bdc_field       USING 'KOMG-MATNR(01)'
                                  gs_a832-matnr.
    PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                  lv_calprice.
    PERFORM bdc_field       USING 'KONP-KONWA(01)'
                                  'inr'.
    PERFORM bdc_field       USING 'KONP-KMEIN(01)'
                                  lv_uom1.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'KOMG-MATNR(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SICH'.
    PERFORM bdc_transaction USING 'VK11'.
  ENDIF.
  CLEAR lv_uom1.
ENDFORM.
FORM condition_create2.

  CLEAR: lv_org,lv_werks.
  IF ls_werks1-low = '1003' OR ls_werks1-low = '1009'.
    lv_org = '1000'.
    WRITE ls_werks1-low TO lv_werks.
  ELSEIF ls_werks2-low = '1401'.
    lv_org = '1400'.
    WRITE ls_werks2-low TO lv_werks.
  ENDIF.

  REFRESH it_bdcdata.

  IF lv_uom EQ 'BOT'.
    CLEAR lv_uom1.
    lv_uom1 = 'BT'.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RV13A-KSCHL'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RV13A-KSCHL'
                                  'PI01'.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'KONP-KMEIN(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'KOMG-VKORGAU'
                                  lv_org.
    PERFORM bdc_field       USING 'KOMG-WERKS'
                                  lv_werks.
    PERFORM bdc_field       USING 'KOMG-MATNR(01)'
                                  gs_a304-matnr.
    PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                  lv_calprice.
    PERFORM bdc_field       USING 'KONP-KONWA(01)'
                                  'inr'.
    PERFORM bdc_field       USING 'KONP-KMEIN(01)'
                                  lv_uom1.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'KOMG-MATNR(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SICH'.
    PERFORM bdc_transaction USING 'VK11'.
  ELSE.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RV13A-KSCHL'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RV13A-KSCHL'
                                  'PI01'.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'KONP-KMEIN(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'KOMG-VKORGAU'
                                  lv_org.
    PERFORM bdc_field       USING 'KOMG-WERKS'
                                  lv_werks.
    PERFORM bdc_field       USING 'KOMG-MATNR(01)'
                                  gs_a304-matnr.
    PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                  lv_calprice.
    PERFORM bdc_field       USING 'KONP-KONWA(01)'
                                  'inr'.
    PERFORM bdc_field       USING 'KONP-KMEIN(01)'
                                  lv_uom1.
    PERFORM bdc_dynpro      USING 'SAPMV13A' '1055'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'KOMG-MATNR(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SICH'.
    PERFORM bdc_transaction USING 'VK11'.
  ENDIF.
  CLEAR lv_uom1.
ENDFORM.
*      -->Program    Program Name
*      -->Dynpro     Screen Number
*----------------------------------------------------------------------*
FORM bdc_dynpro  USING  program TYPE any
                        dynpro TYPE any.

  CLEAR wa_bdcdata.
  wa_bdcdata-program  = program.
  wa_bdcdata-dynpro   = dynpro.
  wa_bdcdata-dynbegin = 'X'.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    " BDC_DYNPRO
*----------------------------------------------------------------------*
FORM bdc_field  USING fnam TYPE any
                      fval TYPE any.

  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = fnam.
  wa_bdcdata-fval = fval.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.
FORM bdc_transaction  USING  tcode TYPE any.
  CALL TRANSACTION tcode
               USING it_bdcdata
               MODE 'N' "'A'
               UPDATE 'S'
               MESSAGES INTO gt_bdcmsg.

  READ TABLE gt_bdcmsg INTO gs_bdcmsg WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0.
    IF tcode = 'VK11'.
      READ TABLE gt_bdcmsg INTO gs_bdcmsg WITH KEY msgnr = '057'.
      IF sy-subrc EQ 0.
        CLEAR gs_alv.
        IF gs_a832-matnr IS NOT INITIAL.
          gs_alv-matnr = gs_a832-matnr.
        ELSE.
          gs_alv-matnr = gs_a304-matnr.
        ENDIF.
        gs_alv-vkorg = lv_org.
        gs_alv-plant = lv_werks.
        gs_alv-msg = 'Plant not extended While Creation'.
        APPEND gs_alv TO gt_alv.
      ENDIF.
    ELSE.
      READ TABLE gt_bdcmsg INTO gs_bdcmsg WITH KEY msgnr = '057'.
      IF sy-subrc EQ 0.
        CLEAR gs_alv.
        IF gs_a832-matnr IS NOT INITIAL.
          gs_alv-matnr = gs_a832-matnr.
        ELSE.
          gs_alv-matnr = gs_a304-matnr.
        ENDIF.
        gs_alv-vkorg = lv_org.
        gs_alv-plant = lv_werks.
        gs_alv-msg = 'Plant not extended While Updation'.
        APPEND gs_alv TO gt_alv.
      ENDIF.
    ENDIF.
  ELSE.
    IF tcode = 'VK11'.
      CLEAR gs_alv.
      IF gs_a832-matnr IS NOT INITIAL.
        gs_alv-matnr = gs_a832-matnr.
      ELSE.
        gs_alv-matnr = gs_a304-matnr.
      ENDIF.
      gs_alv-vkorg = lv_org.
      gs_alv-plant = lv_werks.
      gs_alv-dpl = lv_pri_pr00.
      gs_alv-percen = gs_mara-zmrp_percen.
      gs_alv-old_pri = lv_pri_pi01.
      gs_alv-price = lv_final.
      gs_alv-uom = lv_uom.
      gs_alv-msg = 'MRP Price Record Created successfully'.
      APPEND gs_alv TO gt_alv.
    ELSE.
      CLEAR gs_alv.
      IF gs_a832-matnr IS NOT INITIAL.
        gs_alv-matnr = gs_a832-matnr.
      ELSE.
        gs_alv-matnr = gs_a304-matnr.
      ENDIF.
      gs_alv-vkorg = lv_org.
      gs_alv-plant = lv_werks.
      gs_alv-dpl = lv_pri_pr00.
      gs_alv-percen = gs_mara-zmrp_percen.
      gs_alv-old_pri = lv_pri_pi01.
      gs_alv-price = lv_final.
      gs_alv-uom = lv_uom.
      gs_alv-msg = 'MRP Price Record Updated successfully'.
      APPEND gs_alv TO gt_alv.
    ENDIF.
  ENDIF.
  REFRESH: gt_bdcmsg.
ENDFORM.
** Filling the log table from alv output ***
FORM f_table_update.
  DATA: lt_logs TYPE STANDARD TABLE OF zmrp_updt_log.
  REFRESH: lt_logs.
  LOOP AT gt_alv INTO gs_alv.
    DATA(ls_log) = VALUE zmrp_updt_log( mandt = sy-mandt
                                        material = gs_alv-matnr
                                        vkorg = gs_alv-vkorg
                                        werks = gs_alv-plant
                                        erdat = sy-datum
                                        time = sy-uzeit
                                        ernam = sy-uname
                                        dpl_price = gs_alv-dpl
                                        mrp_percen = gs_alv-percen
                                        old_mrp = gs_alv-old_pri
                                        new_mrp = gs_alv-price
                                        kmein = gs_alv-uom
                                        remarks = gs_alv-msg ).
    APPEND ls_log TO lt_logs.
    CLEAR gs_alv.
  ENDLOOP.
  LOOP AT lt_logs INTO DATA(ls_log1).
** Updating the log table **
    CALL FUNCTION 'ENQUEUE_EZZMRP_UPDT_LOG'
      EXPORTING
        mode_zmrp_updt_log = 'E'
        mandt              = sy-mandt
        material           = ls_log1-material
        vkorg              = ls_log1-vkorg
        werks              = ls_log1-werks
        erdat              = ls_log1-erdat
      EXCEPTIONS
        foreign_lock       = 1
        system_failure     = 2
        OTHERS             = 3.
    IF sy-subrc EQ 0.
      MODIFY zmrp_updt_log FROM ls_log1.
      COMMIT WORK AND WAIT.
    ELSE.
      MESSAGE 'Table is already locked' TYPE 'E'.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZZMRP_UPDT_LOG'
      EXPORTING
        mode_zmrp_updt_log = 'E'
        mandt              = sy-mandt
        material           = ls_log1-material
        vkorg              = ls_log1-vkorg
        werks              = ls_log1-werks
        erdat              = ls_log1-erdat.
  ENDLOOP.


ENDFORM.
** ALV Display **
FORM alv_display.

  REFRESH: gt_fcat.
  PERFORM f_fieldcat USING 'MATNR'   'Material Number' 1 space.
  PERFORM f_fieldcat USING 'VKORG'   'Sales Org'       2 space.
  PERFORM f_fieldcat USING 'PLANT'   'Plant'           3 space.
  PERFORM f_fieldcat USING 'DPL'     'Dpl Price'       4 space.
  PERFORM f_fieldcat USING 'PERCEN'  'MatGrp Percen'   5 space.
  PERFORM f_fieldcat USING 'OLD_PRI' 'Old Price'       6 space.
  PERFORM f_fieldcat USING 'PRICE'   'Price'           7 space.
  PERFORM f_fieldcat USING 'UOM'     'Unit'            8 space.
  PERFORM f_fieldcat USING 'MSG'     'Message'         9 space.

  IF gt_alv[] IS NOT INITIAL.
    gs_layout-colwidth_optimize = 'X'.
***********ALV DISPLAY  *******************
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = gs_layout
        it_fieldcat        = gt_fcat
      TABLES
        t_outtab           = gt_alv[].
  ENDIF.
ENDFORM.

FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 .
  gs_fcat-fieldname = f_var1.
  gs_fcat-seltext_m = f_var2.
  gs_fcat-col_pos = f_var3.
  gs_fcat-edit = f_var4.
  APPEND gs_fcat TO gt_fcat.
ENDFORM.
