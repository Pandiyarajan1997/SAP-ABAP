*----------------------------------------------------------------------*
***INCLUDE LZJSONF03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IM_VBELN  text
*      <--P_EI_IN  text
*      <--P_TT_ITEMDTLS  text
*----------------------------------------------------------------------*
FORM build_data  USING    p_vbeln TYPE vbeln_vf
                 CHANGING p_e_in    TYPE zst_irn_hdr
                          p_tt_itemdtls TYPE zst_lt_irn_item_json.

  REFRESH: l_tab_vbrk[],l_tab_vbrp[],l_tab_prcd[],lt_tab_t001w[],l_tab_branch[],
         l_tab_makt[],l_tab_marc[],l_tab_t005u[], l_tab_kna1_adrc[].

*Get billing document based on im_vbeln.-->>.
  SELECT  vbeln      "Doc No.
          fkart      "Doc type   & Item Is Service
          vkorg
          knumv
          fkdat     "Doc date
          bukrs
          xblnr
          FROM vbrk  INTO TABLE  l_tab_vbrk
          WHERE vbeln =  p_vbeln." AND
*               vkorg IN ( '1300','1700') AND
*               bukrs IN ('1000').

  IF l_tab_vbrk[] IS NOT INITIAL.
    SELECT a~vbeln
           a~posnr     "SL no of Item
           a~fkimg     "Item Qty
           a~vrkme     "Item Unit
           a~netwr
           a~vgbel
           a~matnr
           a~arktx
           a~werks
           b~steuc      "Item HsnCd
      FROM vbrp AS a
      INNER JOIN marc AS b
        ON a~matnr = b~matnr
        AND a~werks = b~werks
        INTO TABLE l_tab_vbrp
        FOR ALL ENTRIES IN l_tab_vbrk
        WHERE a~vbeln = l_tab_vbrk-vbeln .

    "Price details from prcd_elements---->>>
    SELECT knumv
           kposn
           kschl
           kbetr
           kwert
           kawrt
      FROM PRCD_ELEMENTS   ""konv
      INTO TABLE l_tab_prcd
      FOR ALL ENTRIES IN l_tab_vbrk
      WHERE knumv = l_tab_vbrk-knumv .


    IF l_tab_vbrp[] IS NOT INITIAL.
      "Get the Seller Gstin--->>
      SELECT werks
       name1
       adrnr
       j_1bbranch FROM t001w
             INTO TABLE lt_tab_t001w
             FOR ALL ENTRIES IN  l_tab_vbrp
              WHERE werks EQ l_tab_vbrp-werks.

      IF lt_tab_t001w IS NOT INITIAL.
        SELECT bukrs
               branch
               gstin    "Seller GSTIN
          FROM j_1bbranch
          INTO TABLE l_tab_branch
          FOR ALL ENTRIES IN lt_tab_t001w
          WHERE branch = lt_tab_t001w-j_1bbranch.
      ENDIF.

      "Get the material description->>
      SELECT matnr maktx FROM makt
        INTO TABLE l_tab_makt
        FOR ALL ENTRIES IN l_tab_vbrp
        WHERE matnr EQ l_tab_vbrp-matnr
        AND   spras EQ sy-langu.

      SELECT matnr steuc werks FROM marc
        INTO TABLE l_tab_marc
        FOR ALL ENTRIES IN l_tab_vbrp
        WHERE matnr EQ l_tab_vbrp-matnr.

      "Get Plant address details for Seller Addr1
      SELECT a~werks
             a~pstlz
             a~land1
             a~regio
             b~bezei
             c~addrnumber      "Seller Legal Name
             c~name1           "Despatch from Name\
             c~house_num1
             c~street          "Street     &"Despatch from add1
             c~str_suppl3      "
             c~location        "Despatch from add1
             c~post_code1
             c~region
             c~mc_city1
                    FROM ( ( t001w AS a
                      INNER JOIN t005u AS b
                      ON  b~spras = sy-langu
                      AND a~land1 = b~land1
                      AND a~regio = b~bland )
                      INNER JOIN adrc AS c
                      ON  a~adrnr = c~addrnumber )
                     INTO TABLE l_tab_t005u
                FOR ALL ENTRIES IN l_tab_vbrp
                WHERE a~werks = l_tab_vbrp-werks.

      "Get Shipping details
      SELECT a~vbeln
             a~kunnr
             a~kunag
             b~name1       "Buyer Legal Name & "Ship to Legal Name
             b~stcd3       "Buyer Gstin    & "Ship to SGTIN
             b~land1
             b~regio       "Buyer State Code
             c~addrnumber
             c~post_code1  "Buyer Pin Code
             c~str_suppl1
             c~house_num1   "Ship to LEFT
             c~street       "Address1
             c~str_suppl3   "Buyer Address1
             c~city1        "Buyer Address1
             c~location     "Despatch from LEFT
             c~region       "Ship to State Code
             d~bezei
              FROM ( ( ( likp AS a
                    INNER JOIN kna1 AS b
                   ON a~kunnr = b~kunnr
                   INNER JOIN adrc AS c
                   ON b~adrnr = c~addrnumber )
                   LEFT OUTER JOIN t005u AS d
                   ON d~spras = sy-langu
                   AND b~land1 = d~land1
                   AND b~regio = d~bland ) )
                  INTO TABLE l_tab_kna1_adrc
                  FOR ALL ENTRIES IN l_tab_vbrp
                 WHERE a~vbeln = l_tab_vbrp-vgbel.

      "Get Buyer details
      SELECT a~vbeln
             a~kunnr
             a~kunag
             b~name1       "Buyer Legal Name & "Ship to Legal Name
             b~stcd3       "Buyer Gstin    & "Ship to SGTIN
             b~land1
             b~regio       "Buyer State Code
             c~addrnumber
             c~post_code1  "Buyer Pin Code
             c~str_suppl1
             c~house_num1   "Ship to LEFT
             c~street       "Address1
             c~str_suppl3   "Buyer Address1
             c~city1        "Buyer Address1
             c~location     "Despatch from LEFT
             c~region       "Ship to State Code
             d~bezei
              FROM ( ( ( likp AS a
                    INNER JOIN kna1 AS b
                   ON a~kunag = b~kunnr
                   INNER JOIN adrc AS c
                   ON b~adrnr = c~addrnumber )
                   LEFT OUTER JOIN t005u AS d
                   ON d~spras = sy-langu
                   AND b~land1 = d~land1
                   AND b~regio = d~bland ) )
                  INTO TABLE l_tab_kna1_adrc1
                  FOR ALL ENTRIES IN l_tab_vbrp
                 WHERE a~vbeln = l_tab_vbrp-vgbel.

********Without Delivery directly taking from Master Partner func
      IF l_tab_kna1_adrc IS INITIAL OR l_tab_kna1_adrc1 IS INITIAL.
        "Get BUYER details
        SELECT a~vbeln
               a~kunnr
               a~parvw       "Partner Functn
               b~name1       "Buyer Legal Name & "Ship to Legal Name
               b~stcd3       "Buyer Gstin    & "Ship to SGTIN
               b~land1
               b~regio       "Buyer State Code
               c~addrnumber
               c~post_code1  "Buyer Pin Code
               c~str_suppl1
               c~house_num1   "Ship to LEFT
               c~street       "Address1
               c~str_suppl3   "Buyer Address1
               c~city1        "Buyer Address1
               c~location     "Despatch from LEFT
               c~region       "Ship to State Code
               d~bezei
                FROM ( ( ( vbpa AS a
                      INNER JOIN kna1 AS b
                     ON a~kunnr = b~kunnr
                     INNER JOIN adrc AS c
                     ON b~adrnr = c~addrnumber )
                     LEFT OUTER JOIN t005u AS d
                     ON d~spras = sy-langu
                     AND b~land1 = d~land1
                     AND b~regio = d~bland ) )
                    INTO TABLE l_tab_vbpa_adrc
                    FOR ALL ENTRIES IN l_tab_vbrp
                   WHERE a~vbeln = l_tab_vbrp-vbeln AND parvw = 'AG'. "SP AS AG

        "Get Shipping details
        SELECT a~vbeln
               a~kunnr
               a~parvw       "Partner Functn
               b~name1       "Buyer Legal Name & "Ship to Legal Name
               b~stcd3       "Buyer Gstin    & "Ship to SGTIN
               b~land1
               b~regio       "Buyer State Code
               c~addrnumber
               c~post_code1  "Buyer Pin Code
               c~str_suppl1
               c~house_num1   "Ship to LEFT
               c~street       "Address1
               c~str_suppl3   "Buyer Address1
               c~city1        "Buyer Address1
               c~location     "Despatch from LEFT
               c~region       "Ship to State Code
               d~bezei
                FROM ( ( ( vbpa AS a
                      INNER JOIN kna1 AS b
                     ON a~kunnr = b~kunnr
                     INNER JOIN adrc AS c
                     ON b~adrnr = c~addrnumber )
                     LEFT OUTER JOIN t005u AS d
                     ON d~spras = sy-langu
                     AND b~land1 = d~land1
                     AND b~regio = d~bland ) )
                    INTO TABLE l_tab_vbpa_adrc1
                    FOR ALL ENTRIES IN l_tab_vbrp
                   WHERE a~vbeln = l_tab_vbrp-vbeln AND parvw = 'WE'.   "SH AS WE

      ENDIF.

    ENDIF.
  ENDIF.
******End of Select data****************

****Pass the values to Json--->>
  SORT l_tab_vbrp BY vbeln.

 DATA:  V_EMAILID  TYPE AD_SMTPADR,
        V_TEL      TYPE AD_TLNMBR.

  LOOP AT l_tab_vbrp INTO w_tab_vbrp.
    CLEAR: V_EMAILID , V_TEL.
****Header Details*****
    IF sy-tabix = 1.
      READ TABLE l_tab_vbrk INTO w_tab_vbrk WITH KEY vbeln = w_tab_vbrp-vbeln.
      IF sy-subrc = 0.
        p_e_in-bukrs     = w_tab_vbrk-bukrs.
        p_e_in-gjahr     = w_tab_vbrk-fkdat+0(4).

        CASE w_tab_vbrk-fkart.
          WHEN 'YRF2' OR 'YBFS' OR 'YBDP' OR 'YBBR' OR 'IV' OR 'YSTO' OR 'YBTE' OR 'YBEO' OR 'YSCR'.  "
            p_e_in-doctyp    = 'INV'.
          WHEN 'YBRE' OR 'YIRE' OR 'YFRE'.      "ZRE,ZMS2,ZMRE,ZMG2,ZG2,G2,RE
            p_e_in-doctyp    = 'CRN'.
          WHEN 'ZSIN' OR 'ZMSP' OR 'ZML2' OR 'ZL2' OR 'L2'.
            p_e_in-doctyp    = 'DBN'.
        ENDCASE.

        IF w_tab_vbrk-fkart = 'YBEO'.    "Export
          p_e_in-suptyp  = 'EXPWOP'.
          p_e_in-regrev  = 'N'.
          p_e_in-pos     = '96'.         "POS
        ElseIF w_tab_vbrk-fkart = 'YBTE' OR w_tab_vbrk-fkart = 'YBTR'.  "YBTE  Branch Tax Exe Sales
          p_e_in-suptyp  = 'SEZWP'.    "SEZWP or SEZWOP
          p_e_in-regrev  = 'N'.
        ELSE.
          p_e_in-suptyp  = 'B2B'.
          p_e_in-regrev  = 'N'.
        ENDIF.

        p_e_in-xblnr     = w_tab_vbrk-xblnr .    "OFficial Invoice No. "w_tab_vbrk-vbeln.
        p_e_in-vbeln     = w_tab_vbrk-vbeln.     "Billing doc No.

        CONCATENATE w_tab_vbrk-fkdat+6(2) '/' w_tab_vbrk-fkdat+4(2) '/'  w_tab_vbrk-fkdat+0(4) INTO p_e_in-ddat.

****SELLER GSTIN.
        READ TABLE lt_tab_t001w INTO ls_tab_t001w WITH KEY werks = w_tab_vbrp-werks.

        IF sy-subrc IS INITIAL.
          READ TABLE l_tab_branch INTO w_tab_branch WITH KEY branch = ls_tab_t001w-j_1bbranch.
          IF sy-subrc = 0.
            IF NOT w_tab_branch-gstin IS INITIAL.
              p_e_in-gstin     = w_tab_branch-gstin.   "'26ADDPT0274H032'
            ELSE.
              p_e_in-gstin     = w_tab_branch-gstin.
            ENDIF.
          ELSE.
            p_e_in-gstin      = w_tab_branch-gstin.
          ENDIF.
        ENDIF.

****SELLER DETAILS.
        READ TABLE l_tab_t005u INTO w_tab_t005u WITH KEY werks = w_tab_vbrp-werks.
        IF sy-subrc = 0.
          IF NOT w_tab_t005u-name1 IS INITIAL.
            p_e_in-name1      = w_tab_t005u-name1.
          ELSE.
            p_e_in-name1      = c_null.
          ENDIF.

          IF NOT w_tab_t005u-street IS INITIAL.
            p_e_in-street      = w_tab_t005u-street.
          ELSE.
            p_e_in-street      = c_null.
          ENDIF.

          IF NOT w_tab_t005u-location IS INITIAL.
*            p_e_in-location      =  'Bangalore'.
            p_e_in-location      =  w_tab_t005u-location.
          ELSE.
            p_e_in-location      = c_null.
          ENDIF.

          IF NOT w_tab_t005u-post_code1 IS INITIAL.
*            p_e_in-post_code1      = '562160'.
            p_e_in-post_code1      = w_tab_t005u-post_code1. "'396193'
          ELSE.
            p_e_in-post_code1      =  c_null.
          ENDIF.

          IF NOT w_tab_t005u-region IS INITIAL.
            p_e_in-region      =  w_tab_t005u-region.
          ELSE.
            p_e_in-region      = c_null.
          ENDIF.
          p_e_in-region = p_e_in-gstin(2).

***********Seller and Dispatch are Same here *********************
****Dispatch details.
          IF NOT w_tab_t005u-name1 IS INITIAL.
            p_e_in-dis_name1    = w_tab_t005u-name1.
          ELSE.
            p_e_in-dis_name1      = c_null.
          ENDIF.

          IF NOT w_tab_t005u-street IS INITIAL.
            p_e_in-dis_add1     = w_tab_t005u-street.
          ELSE.
            p_e_in-dis_add1      = c_null.
          ENDIF.

          IF NOT w_tab_t005u-location IS INITIAL.
            p_e_in-dis_loc      = w_tab_t005u-location.
          ELSE.
            p_e_in-dis_loc      = c_null.
          ENDIF.

          IF NOT w_tab_t005u-post_code1 IS INITIAL.
*            p_e_in-dis_postcode     =  '562160'.
            p_e_in-dis_postcode     =  w_tab_t005u-post_code1. "'396193'
          ELSE.
            p_e_in-dis_postcode      = c_null .             "'490042'.
          ENDIF.

          IF NOT w_tab_t005u-region IS INITIAL.
            p_e_in-dis_region   = w_tab_t005u-region.
          ELSE.
            p_e_in-dis_region      = c_null.
          ENDIF.
          p_e_in-dis_region = p_e_in-gstin(2).

        ENDIF.

***Buyer details.
        READ TABLE l_tab_kna1_adrc1 INTO w_tab_kna1_adrc WITH KEY  vbeln = w_tab_vbrp-vgbel.
        IF sy-subrc = 0.

          IF  w_tab_vbrk-fkart = 'YBEO'.    "Export
            p_e_in-stcd3     =  'URP'.
          ELSE.
            IF NOT w_tab_kna1_adrc-stcd3 IS INITIAL.
              p_e_in-stcd3     =  w_tab_kna1_adrc-stcd3. "Testing
            ELSE.
              p_e_in-stcd3      = c_null.
            ENDIF.
          ENDIF.

          IF NOT w_tab_kna1_adrc-name1 IS INITIAL.
            p_e_in-buy_name1     = w_tab_kna1_adrc-name1.
          ELSE.
            p_e_in-buy_name1      = c_null.
          ENDIF.

          IF NOT w_tab_kna1_adrc-street IS INITIAL.
            p_e_in-stras     =  w_tab_kna1_adrc-street.
            REPLACE ALL OCCURRENCES OF '"' IN p_e_in-stras WITH space.
          ELSE.
            p_e_in-stras       = c_null.
          ENDIF.

          IF NOT w_tab_kna1_adrc-city1 IS INITIAL.
            p_e_in-ort01     = w_tab_kna1_adrc-city1.
          ELSE.
            p_e_in-ort01      = c_null.
          ENDIF.

          IF w_tab_vbrk-fkart = 'YBEO'.    "Export
            p_e_in-pstlz      = '999999'.
          ELSE.
            IF NOT w_tab_kna1_adrc-post_code1 IS INITIAL.
              p_e_in-pstlz     = w_tab_kna1_adrc-post_code1.
            ELSE.
              p_e_in-pstlz     = c_null.
            ENDIF.
          ENDIF.


          IF NOT w_tab_kna1_adrc-post_code1 IS INITIAL.
            p_e_in-regio     = w_tab_kna1_adrc-regio.
          ELSE.
            p_e_in-regio     = c_null.
          ENDIF.

          IF w_tab_vbrk-fkart = 'YBEO'.    "Export
            p_e_in-regio = '96'.
          ELSE.
            p_e_in-regio   = p_e_in-stcd3(2).
            p_e_in-pos     = p_e_in-regio.      "Place of Supply
          ENDIF.

****Ship to Details.
          CLEAR:  w_tab_kna1_adrc.
          READ TABLE l_tab_kna1_adrc INTO w_tab_kna1_adrc WITH KEY  vbeln = w_tab_vbrp-vgbel.
          IF sy-subrc = 0.
            IF  w_tab_vbrk-fkart = 'YBEO'.    "Export
              p_e_in-sh_gstin     =  'URP'.
            ELSE.
              IF NOT w_tab_kna1_adrc-stcd3 IS INITIAL.
                p_e_in-sh_gstin    = w_tab_kna1_adrc-stcd3.
              ELSE.
                p_e_in-sh_gstin     = c_null.
              ENDIF.
            ENDIF.

            IF NOT w_tab_kna1_adrc-name1 IS INITIAL.
              p_e_in-sh_name1    = w_tab_kna1_adrc-name1.
            ELSE.
              p_e_in-sh_name1     = c_null.
            ENDIF.

            IF NOT w_tab_kna1_adrc-street IS INITIAL.
              p_e_in-sh_add1     = w_tab_kna1_adrc-street.
              REPLACE ALL OCCURRENCES OF '"' IN p_e_in-sh_add1 WITH space.
            ELSE.
              p_e_in-sh_add1     = c_null.
            ENDIF.

            IF NOT w_tab_kna1_adrc-city1 IS INITIAL.
              p_e_in-sh_loc      = w_tab_kna1_adrc-city1.
            ELSE.
              p_e_in-sh_loc     = c_null.
            ENDIF.

            IF w_tab_vbrk-fkart = 'YBEO'.    "Export
              p_e_in-sh_postcode      = '999999'.
            ELSE.
              IF NOT w_tab_kna1_adrc-post_code1 IS INITIAL.
                p_e_in-sh_postcode = w_tab_kna1_adrc-post_code1.
              ELSE.
                p_e_in-sh_postcode     = c_null.
              ENDIF.
            ENDIF.

            IF w_tab_vbrk-fkart = 'YBEO'.    "Export
              p_e_in-sh_region      = '96'.
            ELSE.
              IF NOT w_tab_kna1_adrc-regio IS INITIAL.
                p_e_in-sh_region     = w_tab_kna1_adrc-regio.
              ELSE.
                p_e_in-sh_region     = c_null.
              ENDIF.
            ENDIF.

            IF w_tab_vbrk-fkart = 'YBEO'.    "Export
              p_e_in-sh_region      = '96'.
            ELSE.
              p_e_in-sh_region    = w_tab_kna1_adrc-stcd3(2).
            ENDIF.

           ELSE.
             PERFORM buyer_ship_details CHANGING p_e_in.
          ENDIF.

        ELSE.
          PERFORM buyer_ship_details CHANGING p_e_in.
       ENDIF.

*******Buyer e-Mail id
        SELECT SMTP_ADDR  UP TO 1 ROWS FROM adr6 INTO v_Emailid WHERE ADDRNUMBER = w_tab_kna1_adrc-addrnumber ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
        IF l_tab_vbpa_adrc IS NOT INITIAL.
         SELECT SMTP_ADDR  UP TO 1 ROWS FROM adr6 INTO v_Emailid WHERE ADDRNUMBER = w_tab_VBPA_adrc-addrnumber ORDER BY PRIMARY KEY.
         ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
        ENDIF.

        IF V_EMAILID IS NOT INITIAL.
          CONCATENATE '"' V_EMAILID '"' INTO  p_e_in-BUY_MAIL.
        ELSE.
          p_e_in-BUY_MAIL = '"buyer@sheenlac.com"'.
        ENDIF.

*****Buyer Telephone no.
        SELECT TEL_NUMBER  UP TO 1 ROWS FROM adr2 INTO v_tel WHERE ADDRNUMBER = w_tab_kna1_adrc-addrnumber ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
        IF l_tab_vbpa_adrc IS NOT INITIAL.
         SELECT TEL_NUMBER  UP TO 1 ROWS FROM adr2 INTO v_tel WHERE ADDRNUMBER = w_tab_VBPA_adrc-addrnumber ORDER BY PRIMARY KEY.
         ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
        ENDIF.

        IF V_TEL IS NOT INITIAL.
          CONCATENATE '"' V_TEL '"' INTO  p_e_in-TEL_NO.
        ELSE.
          p_e_in-TEL_NO = '"9000000000"'.
        ENDIF.

        CONCATENATE w_tab_vbrk-fkdat+6(2) '/' w_tab_vbrk-fkdat+4(2) '/'  w_tab_vbrk-fkdat+0(4) INTO p_e_in-inv_st.
        CONCATENATE w_tab_vbrk-fkdat+6(2) '/' w_tab_vbrk-fkdat+4(2) '/'  w_tab_vbrk-fkdat+0(4) INTO p_e_in-inv_end.
        p_e_in-ref_no    = w_tab_vbrp-vgbel.
        CONCATENATE w_tab_vbrk-fkdat+6(2) '/' w_tab_vbrk-fkdat+4(2) '/'  w_tab_vbrk-fkdat+0(4) INTO p_e_in-inv_dt.
      ENDIF.
    ENDIF.

***Item Details---->>

    wa_itemdtls-vbeln = w_tab_vbrp-vbeln.
    wa_itemdtls-posnr = w_tab_vbrp-posnr.

    READ TABLE l_tab_makt INTO w_tab_makt WITH KEY  matnr = w_tab_vbrp-matnr.
    IF w_tab_makt-maktx IS NOT INITIAL.
      wa_itemdtls-makt = w_tab_makt-maktx.
    ELSE.
      wa_itemdtls-makt = c_null.
    ENDIF.
*Item Is Servcice
    wa_itemdtls-itm_service    = 'N'.

    READ TABLE l_tab_marc INTO w_tab_marc WITH KEY  matnr = w_tab_vbrp-matnr werks = w_tab_vbrp-werks.
    IF w_tab_marc-steuc IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '.' IN w_tab_marc-steuc    WITH space .
      CONDENSE w_tab_marc-steuc NO-GAPS.
**      wa_itemdtls-steuc = w_tab_marc-steuc(4).
      wa_itemdtls-steuc = w_tab_marc-steuc(8).    "HSNCODE   "Changed on 02.04.21
    ELSE.
      wa_itemdtls-steuc = c_null.
    ENDIF.

    IF  w_tab_vbrp-fkimg IS NOT INITIAL.
      wa_itemdtls-fkimg = w_tab_vbrp-fkimg.
    ELSE.
      wa_itemdtls-fkimg = c_zero.
    ENDIF.

***********Begin of Unit for Sending data to API *************
    IF w_tab_vbrp-vrkme = 'EA'.         "Each
      wa_itemdtls-vrkme = 'OTH'.
    ELSEIF w_tab_vbrp-vrkme = 'KG'.     "Kilograms
      wa_itemdtls-vrkme = 'KGS'.
    ELSEIF w_tab_vbrp-vrkme = 'BT'.     "Bottle
      wa_itemdtls-vrkme = 'BTL'.
    ELSEIF w_tab_vbrp-vrkme = 'L'.      "Litre
      wa_itemdtls-vrkme = 'LTR'.
    ELSEIF w_tab_vbrp-vrkme = 'M'.      "Metre
      wa_itemdtls-vrkme = 'MTR'.
    ELSEIF w_tab_vbrp-vrkme = 'PAA'.    "Pairs
      wa_itemdtls-vrkme = 'PRS'.
    ELSE.
      wa_itemdtls-vrkme = w_tab_vbrp-vrkme.
    ENDIF.
***********End of Unit for Sending data to API *************

    DATA : lv_igstrate TYPE char15,
           lv_cgstrate TYPE char15,
           lv_sgstrate TYPE char15,
           v_amt_amort TYPE char15,
           v_uom_amort TYPE char15,
           v_flag      TYPE c,
           v_amt       TYPE p DECIMALS 2,
           v_amtval    TYPE p DECIMALS 2,
           v_discount  TYPE p DECIMALS 2,
           v_dec_igst  TYPE p DECIMALS 2,
           v_dec_cgst  TYPE p DECIMALS 2,
           v_dec_sgst  TYPE p DECIMALS 2.

    CLEAR: v_dec_igst,v_dec_sgst,v_dec_cgst,v_amt,lv_igstrate,lv_cgstrate,lv_sgstrate,v_amt_amort,v_uom_amort,v_flag.

    READ TABLE l_tab_vbrk INTO w_tab_vbrk WITH KEY vbeln = w_tab_vbrp-vbeln.
    IF sy-subrc IS INITIAL.

*      PERFORM discount.

      CLEAR:w_tab_prcd.
      READ TABLE l_tab_prcd INTO w_tab_prcd WITH KEY  knumv = w_tab_vbrk-knumv
                                                      kposn = w_tab_vbrp-posnr
                                                      kschl = 'PR00'.             "Price

      IF sy-subrc = 0.
        wa_itemdtls-uprice = w_tab_prcd-kbetr * 1.
*        v_amt              = w_tab_vbrp-fkimg * w_tab_prcd-kbetr.         "Two Decimal
        v_amt              = w_tab_prcd-kwert.                             "Two Decimal
        wa_itemdtls-amount = v_amt.

        IF wa_itemdtls-uprice IS NOT INITIAL.
          v_assval = w_tab_prcd-kbetr.
          CONDENSE v_assval.
*          wa_itemdtls-assval = wa_itemdtls-amount.  " Ass amt = Total amt - Discount
          wa_itemdtls-amount = w_tab_vbrp-netwr.  " Total Item Value
          wa_itemdtls-assval = w_tab_vbrp-netwr.  " Ass amt = Total amt - Discount
        ENDIF.
      ENDIF.

      CLEAR: w_tab_prcd,v_amt,v_assval.
      READ TABLE l_tab_prcd INTO w_tab_prcd WITH KEY  knumv = w_tab_vbrk-knumv
                                                      kposn = w_tab_vbrp-posnr
                                                      kschl = 'ZTRP'.

      IF sy-subrc = 0.
        wa_itemdtls-uprice = w_tab_prcd-kbetr * 1.
*        v_amt              = w_tab_vbrp-fkimg * w_tab_prcd-kbetr.         "Two Decimal
        v_amt              = w_tab_prcd-kwert.                             "Two Decimal
        wa_itemdtls-amount = v_amt.

        IF wa_itemdtls-uprice IS NOT INITIAL.
          v_assval = w_tab_prcd-kbetr.
          CONDENSE v_assval.
*          wa_itemdtls-assval = wa_itemdtls-amount.  " Ass amt = Total amt - Discount
          wa_itemdtls-amount = w_tab_vbrp-netwr.     " Total Item Value
          wa_itemdtls-assval = w_tab_vbrp-netwr.     " Ass amt = Total amt - Discount
        ENDIF.
      ENDIF.

****INTER COMPANY CONDITION TYPE
      CLEAR: w_tab_prcd,v_amt,v_assval.
      READ TABLE l_tab_prcd INTO w_tab_prcd WITH KEY  knumv = w_tab_vbrk-knumv
                                                      kposn = w_tab_vbrp-posnr
                                                      kschl = 'PB01'.

      IF sy-subrc = 0.
        wa_itemdtls-uprice = w_tab_prcd-kbetr * 1.
        v_amt              = w_tab_prcd-kwert.                             "Two Decimal
        wa_itemdtls-amount = v_amt.

        IF wa_itemdtls-uprice IS NOT INITIAL.
          v_assval = w_tab_prcd-kbetr.
          CONDENSE v_assval.
*          wa_itemdtls-assval = wa_itemdtls-amount.  " Ass amt = Total amt - Discount
          wa_itemdtls-amount = w_tab_vbrp-netwr.  " Total Item Value
          wa_itemdtls-assval = w_tab_vbrp-netwr.  " Ass amt = Total amt - Discount
        ENDIF.
      ENDIF.


      IF wa_itemdtls-uprice IS INITIAL.       "Unit Price
        wa_itemdtls-uprice    = c_zero.
      ENDIF.

      IF wa_itemdtls-amount IS INITIAL.       "Amount & Ass Value
        wa_itemdtls-amount    = c_zero.
        wa_itemdtls-assval    = c_zero.
      ENDIF.

*******IGST Rate and Amount *******
      READ TABLE l_tab_prcd INTO w_tab_prcd1 WITH KEY  knumv = w_tab_vbrk-knumv kschl = 'JOIG'
                                                       kposn = w_tab_vbrp-posnr.

      IF sy-subrc IS INITIAL.         "IGST RATE
        IF w_tab_prcd1-kwert IS NOT INITIAL.   "IGST AMOUNT
          wa_itemdtls-igstamt   = w_tab_prcd1-kwert.       "w_tab_prcd-Kawrt  * lv_igstrate
**          lv_igstrate           = w_tab_prcd1-kbetr / 10. "wa_itemdtls-igstamt.
          lv_igstrate           = w_tab_prcd1-kbetr. "wa_itemdtls-igstamt.  "By SPLABAP during Code Remediation
        ELSE.
          wa_itemdtls-igstamt   = c_zero.
          wa_itemdtls-gst_rate  = c_zero.
        ENDIF.
      ELSE.
        wa_itemdtls-gst_rate = c_zero.
        wa_itemdtls-igstamt = c_zero.
      ENDIF.

*******CGST Rate & Amount *******
      CLEAR: w_tab_prcd1.
      READ TABLE l_tab_prcd INTO w_tab_prcd1 WITH KEY  knumv = w_tab_vbrk-knumv kschl = 'JOCG'
                                                       kposn = w_tab_vbrp-posnr.

      IF sy-subrc IS INITIAL.         "CGST RATE

        IF w_tab_prcd1-kwert IS NOT INITIAL. "CGST AMOUNT
          wa_itemdtls-cgstamt = w_tab_prcd1-kwert.         "w_tab_prcd-Kawrt  * lv_Cgstrate
**          lv_cgstrate         = w_tab_prcd1-kbetr / 10.  "wa_itemdtls-cgstamt.
          lv_cgstrate         = w_tab_prcd1-kbetr.  "wa_itemdtls-cgstamt.
        ELSE.
          wa_itemdtls-cgstamt = c_zero.
          wa_itemdtls-gst_rate = c_zero.
        ENDIF.
      ELSE.
        wa_itemdtls-gst_rate = c_zero.
        wa_itemdtls-cgstamt = c_zero.
      ENDIF.

*******SGST Rate & Amount *******
      CLEAR: w_tab_prcd1.
      READ TABLE l_tab_prcd INTO w_tab_prcd1 WITH KEY  knumv = w_tab_vbrk-knumv kschl = 'JOSG'
                                                       kposn = w_tab_vbrp-posnr.
      IF sy-subrc IS INITIAL.         "SGST   RATE

        IF w_tab_prcd1-kwert IS NOT INITIAL.   "SGST AMOUNT
          wa_itemdtls-sgstamt = w_tab_prcd1-kwert.        "w_tab_prcd-Kawrt  * lv_Sgstrate
*          lv_sgstrate         = w_tab_prcd1-kbetr / 10.  "wa_itemdtls-cgstamt + wa_itemdtls-sgstamt.
          lv_sgstrate         = w_tab_prcd1-kbetr.  "wa_itemdtls-cgstamt + wa_itemdtls-sgstamt.
        ELSE.
          wa_itemdtls-sgstamt = c_zero.
          wa_itemdtls-gst_rate = c_zero.
        ENDIF.
      ELSE.
        wa_itemdtls-gst_rate = c_zero.
        wa_itemdtls-sgstamt = c_zero.
      ENDIF.

*******TCS Amount *******
        CLEAR: w_tab_prcd1.
        READ TABLE l_tab_prcd INTO w_tab_prcd1 WITH KEY  knumv = w_tab_vbrk-knumv kschl = 'JTC1'
                                                         kposn = w_tab_vbrp-posnr.
        IF sy-subrc IS INITIAL.         "TCS AMT
          v_flag = 'X'.
          IF w_tab_prcd1-kwert IS NOT INITIAL.   "TCS AMT
            wa_itemdtls-othchrg = w_tab_prcd1-kwert.        "TCS Condition Value
          ELSE.
            wa_itemdtls-othchrg = c_zero.
          ENDIF.
        ELSE.
          wa_itemdtls-othchrg = c_zero.
        ENDIF.

*******Scrap Sales TCS *********
       IF V_FLAG NE 'X'.
        CLEAR: w_tab_prcd1.
        READ TABLE l_tab_prcd INTO w_tab_prcd1 WITH KEY  knumv = w_tab_vbrk-knumv kschl = 'JTC2'
                                                         kposn = w_tab_vbrp-posnr.
        IF sy-subrc IS INITIAL.         "TCS AMT
          IF w_tab_prcd1-kwert IS NOT INITIAL.   "TCS AMT
            wa_itemdtls-othchrg = w_tab_prcd1-kwert.        "TCS Condition Value
          ELSE.
            wa_itemdtls-othchrg = c_zero.
          ENDIF.
        ELSE.
          wa_itemdtls-othchrg = c_zero.
        ENDIF.
       ENDIF.

      wa_itemdtls-gst_rate = lv_igstrate + lv_cgstrate + lv_sgstrate.
      v_amtval = wa_itemdtls-assval + wa_itemdtls-igstamt + wa_itemdtls-cgstamt + wa_itemdtls-sgstamt + wa_itemdtls-othchrg.
      wa_itemdtls-totamtval = v_amtval.
    ELSE.
      wa_itemdtls-uprice = c_zero.
      wa_itemdtls-amount = c_zero.
      wa_itemdtls-assval = c_zero.
      wa_itemdtls-igstamt = c_zero.
      wa_itemdtls-othchrg = c_zero.
      wa_itemdtls-cgstamt = c_zero.
      wa_itemdtls-sgstamt = c_zero.
      wa_itemdtls-gst_rate = c_zero.
      wa_itemdtls-totamtval = c_zero.
    ENDIF.


********* Eway Bill Details ********
    p_e_in-transid    = c_null.
    p_e_in-transname  = c_null.

    IF p_e_in-post_code1 = p_e_in-pstlz.    "Seller Pincode Same as Buyer Pincode
      p_e_in-distance   = '100'.
    ELSE.
      p_e_in-distance   = '0'.
    ENDIF.

    p_e_in-vehno      = c_null.
    p_e_in-vehtyp     = c_null.       "Null as Empty R - Regular
    p_e_in-transmode  = c_null.       "Null as Empty 1 - Road,  2-Rail , Air-3 , Ship-4

    APPEND wa_itemdtls TO p_tt_itemdtls.
    CLEAR: wa_itemdtls,w_tab_vbrp,w_tab_prcd,w_tab_branch,w_tab_makt,w_tab_marc,V_TEL,V_EMAILID,v_flag,
           w_tab_t005u,w_tab_kna1_adrc,v_assval, lv_sgstrate, lv_igstrate, lv_cgstrate,v_amtval,v_amt,v_uom_amort,v_amt_amort.
  ENDLOOP.
  REFRESH: l_tab_vbrk[],l_tab_vbrp[],l_tab_kna1_adrc[],l_tab_branch[],l_tab_prcd[],l_tab_marc[].


ENDFORM.                    " BUILD_DATA
*&---------------------------------------------------------------------*
*&      Form  ERROR_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ERROR_MSG  text
*----------------------------------------------------------------------*
FORM error_msg  TABLES   p_error_msg TYPE bapiret2_t.
  "Insert correct name for <...>.
*  ********************************SANDBOX IRN API RESPONSE ERROR MESSAGE SPLITTING****************************
  SPLIT response AT '},"group_id"' INTO v_str v_errorcode.
  SPLIT v_str AT '"ErrorDetails":' INTO v_str1 v_str2.

  CLEAR:v_str.
  SPLIT v_str2 AT '"error_code":"'   INTO v_str wa_error-error_code.
  CLEAR: v_str.
  SPLIT v_str2 AT '"error_message":"' INTO v_str1 wa_error-error_message.
  CLEAR: v_str.
  SPLIT v_str2 AT '"error_source":"' INTO v_str1 wa_error-error_source.

*  /ui2/cl_json=>deserialize( EXPORTING json = v_str2
*                               pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*                      CHANGING data = it_error ).

*  LOOP AT it_error INTO wa_error.0
  wa_bapiret-type = 'E'.
  wa_bapiret-id         = wa_error-error_code.
  wa_bapiret-message    = wa_error-error_message.
  wa_bapiret-message_v1 = wa_error-error_source.
  APPEND wa_bapiret TO p_error_msg.
  CLEAR wa_bapiret.
*  ENDLOOP.
********************END OF ERROR MESSAGE*********************************************************************

ENDFORM.                    " ERROR_MSG
*&---------------------------------------------------------------------*
*&      Form  RESPONSE_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EINV_OUT  text
*      -->P_EWAY_OUT  text
*----------------------------------------------------------------------*
FORM response_move  USING    p_einv_out TYPE zst_irn_out
                             p_eway_out TYPE j_1ig_ewaybill.

*  ***Einvoice IRN Invrefnum details.--->>
  wa_j_1ig_invrefnum-bukrs         = p_einv_out-bukrs.
  wa_j_1ig_invrefnum-doc_year      = p_einv_out-gjahr.
*  SHIFT p_einv_out-vbeln LEFT DELETING LEADING '0'.
  wa_j_1ig_invrefnum-docno         = p_einv_out-vbeln.
  wa_j_1ig_invrefnum-version       = p_einv_out-version.
  wa_j_1ig_invrefnum-doc_type      = p_einv_out-doctyp.
  wa_j_1ig_invrefnum-irn           = p_einv_out-irn.
  wa_j_1ig_invrefnum-ack_no        = p_einv_out-ack_no.
  wa_j_1ig_invrefnum-ack_date      = p_einv_out-ack_date.
  wa_j_1ig_invrefnum-irn_status    = p_einv_out-irn_status.
  wa_j_1ig_invrefnum-ernam         = p_einv_out-ernam.
  wa_j_1ig_invrefnum-erdat         = p_einv_out-erdat.
  wa_j_1ig_invrefnum-erzet         = p_einv_out-erzet.
  wa_j_1ig_invrefnum-signed_inv    = p_einv_out-signed_inv.
  wa_j_1ig_invrefnum-signed_qrcode = p_einv_out-signed_qrcode.

***Ewaybill details--->>>
  wa_j_1ig_ewaybill-bukrs           = p_eway_out-bukrs.
  wa_j_1ig_ewaybill-doctyp          = p_eway_out-doctyp.
*  SHIFT p_eway_out-docno LEFT DELETING LEADING '0'.
  wa_j_1ig_ewaybill-docno           = p_eway_out-docno.
  wa_j_1ig_ewaybill-gjahr           = p_eway_out-gjahr.
  wa_j_1ig_ewaybill-ebillno         = p_eway_out-ebillno.
  wa_j_1ig_ewaybill-egen_dat        = p_eway_out-egen_dat.
  wa_j_1ig_ewaybill-egen_time       = p_eway_out-egen_time.
  wa_j_1ig_ewaybill-vdfmdate        = p_eway_out-vdfmdate.
  wa_j_1ig_ewaybill-vdfmtime        = p_eway_out-vdfmtime.
  wa_j_1ig_ewaybill-vdtodate        = p_eway_out-vdtodate.
  wa_j_1ig_ewaybill-vdtotime        = p_eway_out-vdtotime.
  wa_j_1ig_ewaybill-status          = p_eway_out-status.
  wa_j_1ig_ewaybill-ernam           = p_eway_out-ernam.
  wa_j_1ig_ewaybill-erdat           = p_eway_out-erdat.
  wa_j_1ig_ewaybill-aenam           = p_eway_out-aenam.
  wa_j_1ig_ewaybill-aedat           = p_eway_out-aedat.

ENDFORM.                    " RESPONSE_MOVE
*&---------------------------------------------------------------------*
*&      Form  BUYER_SHIP_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_E_IN  text
*----------------------------------------------------------------------*
FORM buyer_ship_details  CHANGING pi_e_in TYPE zst_irn_hdr.

********Buyer Details from Partner Function
  READ TABLE l_tab_vbpa_adrc INTO w_tab_vbpa_adrc WITH KEY  vbeln = w_tab_vbrp-vbeln.
  IF sy-subrc = 0.

    IF  w_tab_vbrk-fkart = 'YBEO'.    "Export
      pi_e_in-stcd3     =  'URP'.
    ELSE.
      IF NOT w_tab_vbpa_adrc-stcd3 IS INITIAL.
        pi_e_in-stcd3     =  w_tab_vbpa_adrc-stcd3. "Testing
      ELSE.
        pi_e_in-stcd3      = c_null.
      ENDIF.
    ENDIF.

    IF NOT w_tab_vbpa_adrc-name1 IS INITIAL.
      pi_e_in-buy_name1     = w_tab_vbpa_adrc-name1.
    ELSE.
      pi_e_in-buy_name1      = c_null.
    ENDIF.

    IF NOT w_tab_vbpa_adrc-street IS INITIAL.
      pi_e_in-stras     = w_tab_vbpa_adrc-street.
      REPLACE ALL OCCURRENCES OF '"' IN pi_e_in-stras WITH space.
    ELSE.
      pi_e_in-stras       = c_null.
    ENDIF.

    IF NOT w_tab_vbpa_adrc-city1 IS INITIAL.
      pi_e_in-ort01     = w_tab_vbpa_adrc-city1.
    ELSE.
      pi_e_in-ort01      = c_null.
    ENDIF.

    IF w_tab_vbrk-fkart = 'YBEO'.    "Export
      pi_e_in-pstlz      = '999999'.
    ELSE.
      IF NOT w_tab_vbpa_adrc-post_code1 IS INITIAL.
        pi_e_in-pstlz     = w_tab_vbpa_adrc-post_code1.
      ELSE.
        pi_e_in-pstlz     = c_null.
      ENDIF.
    ENDIF.


    IF NOT w_tab_vbpa_adrc-post_code1 IS INITIAL.
      pi_e_in-regio     = w_tab_vbpa_adrc-regio.
    ELSE.
      pi_e_in-regio     = c_null.
    ENDIF.

    IF w_tab_vbrk-fkart = 'ZEF2'.    "Export
      pi_e_in-regio = '96'.
    ELSE.
      pi_e_in-regio   = pi_e_in-stcd3(2).
      pi_e_in-pos     = pi_e_in-regio.      "Place of Supply
    ENDIF.

****Ship to Details.
    CLEAR:  w_tab_vbpa_adrc.
    READ TABLE l_tab_vbpa_adrc1 INTO w_tab_vbpa_adrc WITH KEY  vbeln = w_tab_vbrp-vbeln.
    IF sy-subrc = 0.

      IF  w_tab_vbrk-fkart = 'YBEO'.    "Export
        pi_e_in-sh_gstin     =  'URP'.
      ELSE.
        IF NOT w_tab_vbpa_adrc-stcd3 IS INITIAL.
          pi_e_in-sh_gstin    = w_tab_vbpa_adrc-stcd3.
        ELSE.
          pi_e_in-sh_gstin     = c_null.
        ENDIF.
      ENDIF.

      IF NOT w_tab_vbpa_adrc-name1 IS INITIAL.
        pi_e_in-sh_name1    = w_tab_vbpa_adrc-name1.
      ELSE.
        pi_e_in-sh_name1     = c_null.
      ENDIF.

      IF NOT w_tab_vbpa_adrc-street IS INITIAL.
        pi_e_in-sh_add1     = w_tab_vbpa_adrc-street.
        REPLACE ALL OCCURRENCES OF '"' IN pi_e_in-sh_add1 WITH space.
      ELSE.
        pi_e_in-sh_add1     = c_null.
      ENDIF.

      IF NOT w_tab_vbpa_adrc-city1 IS INITIAL.
        pi_e_in-sh_loc      = w_tab_vbpa_adrc-city1.
      ELSE.
        pi_e_in-sh_loc     = c_null.
      ENDIF.

      IF w_tab_vbrk-fkart = 'YBEO'.    "Export
        pi_e_in-sh_postcode      = '999999'.
      ELSE.
        IF NOT w_tab_vbpa_adrc-post_code1 IS INITIAL.
          pi_e_in-sh_postcode = w_tab_vbpa_adrc-post_code1.
        ELSE.
          pi_e_in-sh_postcode     = c_null.
        ENDIF.
      ENDIF.

      IF w_tab_vbrk-fkart = 'YBEO'.    "Export
        pi_e_in-sh_region      = '96'.
      ELSE.
        IF NOT w_tab_vbpa_adrc-regio IS INITIAL.
          pi_e_in-sh_region     = w_tab_vbpa_adrc-regio.
        ELSE.
          pi_e_in-sh_region     = c_null.
        ENDIF.
      ENDIF.

      IF w_tab_vbrk-fkart = 'YBEO'.    "Export
        pi_e_in-sh_region      = '96'.
      ELSE.
        pi_e_in-sh_region    = w_tab_vbpa_adrc-stcd3(2).
      ENDIF.

    ENDIF.
  ENDIF.


ENDFORM.                    " BUYER_SHIP_DETAILS
*&---------------------------------------------------------------------*
*&      Form  DISCOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
**----------------------------------------------------------------------*
*FORM discount .
*
*  DATA: lv_discount1 TYPE p DECIMALS 2,
*        lv_discount2 TYPE p DECIMALS 2,
*        lv_discount3 TYPE p DECIMALS 2,
*        lv_discount4 TYPE p DECIMALS 2,
*        lv_discount5 TYPE p DECIMALS 2,
*        lv_discount6 TYPE p DECIMALS 2,
*        lv_discount7 TYPE p DECIMALS 2,
*        lv_discount8 TYPE p DECIMALS 2.
*
*  CLEAR: lv_discount1,lv_discount2,lv_discount3,lv_discount4,lv_discount5,lv_discount6,
*          lv_discount7,lv_discount8.
*
*  PERFORM discount_amt USING 'Y004' CHANGING lv_discount1.
*  PERFORM discount_amt USING 'Y005' CHANGING lv_discount2.
*  PERFORM discount_amt USING 'Y006' CHANGING lv_discount3.
*  PERFORM discount_amt USING 'Y007' CHANGING lv_discount4.
*  PERFORM discount_amt USING 'Y008' CHANGING lv_discount5.
*  PERFORM discount_amt USING 'ZPRB' CHANGING lv_discount6.
*  PERFORM discount_amt USING 'ZCAD' CHANGING lv_discount7.
*  PERFORM discount_amt USING 'YBSD' CHANGING lv_discount8.
*
*
*ENDFORM.                    " DISCOUNT
*&---------------------------------------------------------------------*
*&      Form  DISCOUNT_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2385   text
*      <--P_LV_DISCOUNT1  text
*----------------------------------------------------------------------*
*FORM discount_amt  USING    P_KSCHL TYPE KONV-KSCHL
*                   CHANGING p_lv_discount1 .
*
*   CLEAR:w_tab_prcd.
*  READ TABLE l_tab_prcd INTO w_tab_prcd WITH KEY  knumv = w_tab_vbrk-knumv
*                                                  kposn = w_tab_vbrp-posnr
*                                                  kschl = P_KSCHL.             "Discount Price
*  IF sy-subrc IS INITIAL.
*      w_tab_prcd-kwert = ABS( w_tab_prcd-kwert ).
*      p_lv_discount1    = w_tab_prcd-kwert.
*  ENDIF.
*
*ENDFORM.                    " DISCOUNT_AMT
