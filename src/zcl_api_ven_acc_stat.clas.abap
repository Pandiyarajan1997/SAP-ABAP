class ZCL_API_VEN_ACC_STAT definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_ACC_STAT IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: date_frm TYPE string,
          date_to  TYPE string,
          vendor   TYPE string.
    DATA: lv_path_info      TYPE string,
          lv_request_method TYPE string,
          lv_response_data  TYPE string.
    DATA: it_inputparams TYPE tihttpnvp,
          ls_inputparams TYPE LINE OF tihttpnvp.

    TYPES: BEGIN OF gs_lfa1,
             lifnr TYPE lfa1-lifnr,                  " Vendor Code
             name1 TYPE lfa1-name1,                  " Customer Name
             adrnr TYPE lfa1-adrnr,                  " Address Number
           END OF gs_lfa1.

    DATA: gt_lfa1 TYPE TABLE OF gs_lfa1,
          wa_lfa1 TYPE gs_lfa1.
    TYPES : BEGIN OF gs_t003t,
              blart TYPE t003t-blart,
              ltext TYPE t003t-ltext,
              spras TYPE t003t-spras,
            END OF gs_t003t.

    DATA : gt_t003t TYPE  TABLE OF gs_t003t,
           wa_t003t TYPE gs_t003t.

    DATA : gt_t003t1 TYPE  TABLE OF gs_t003t,
           wa_t003t1 TYPE gs_t003t.

    DATA : gt_t003t2 TYPE  TABLE OF gs_t003t,
           wa_t003t2 TYPE gs_t003t.

    TYPES: BEGIN OF gs_bsik,
             prctr TYPE bsik-prctr,                  " Profit Center
             lifnr TYPE bsik-lifnr,                  " Customer Number
             augbl TYPE bsik-augbl,
             budat TYPE bsik-budat,                  " Posting Date in the Document
             bldat TYPE bsik-bldat,
             blart TYPE bsik-blart,
             zfbdt TYPE bsik-zfbdt,                  " Baseline Date for Due Date Calculation
             dmbtr TYPE bsik-dmbtr,                  " Amount in Local Currency
             shkzg TYPE bsik-shkzg,                  " Debit/Credit Indicator
             xblnr TYPE bsik-xblnr,                  " Reference Document Number
             belnr TYPE bsik-belnr,                  " Bill Number
             gsber TYPE bsik-gsber,                  " Business Area
             sgtxt TYPE bsik-sgtxt,                  " Text Or Remarks
             ebeln TYPE bsik-ebeln,
             umskz TYPE bsik-umskz,
             bukrs TYPE bsik-bukrs,
             gjahr TYPE bsik-gjahr,
             sknto TYPE bsak-sknto,
             zterm TYPE bsak-zterm,
             zbd1t TYPE bsak-zbd1t,
             umsks TYPE bsak-umsks,
             augdt TYPE bsak-augdt,
             zuonr TYPE bsak-zuonr,
             buzei TYPE bsak-buzei,
           END OF gs_bsik.

    DATA: gt_bsik  TYPE TABLE OF gs_bsik,
          gt_bsik1 TYPE TABLE OF gs_bsik,
          wa_bsik  TYPE gs_bsik,
          wa_bsik1 TYPE gs_bsik.

    DATA: lv_ind TYPE i.

    TYPES: BEGIN OF gs_bsak,
             prctr TYPE bsak-prctr,                  " Profit Center
             lifnr TYPE bsak-lifnr,                  " Customer Number
             augbl TYPE bsak-augbl,
             budat TYPE bsak-budat,                  " Posting Date in the Document
             bldat TYPE bsak-bldat,
             blart TYPE bsak-blart,
             zfbdt TYPE bsak-zfbdt,                  " Baseline Date for Due Date Calculation
             dmbtr TYPE bsak-dmbtr,                  " Amount in Local Currency
             shkzg TYPE bsak-shkzg,                  " Debit/Credit Indicator
             xblnr TYPE bsak-xblnr,                  " Reference Document Number
             belnr TYPE bsak-belnr,                  " Bill Number
             gsber TYPE bsak-gsber,                  " Business Area
             sgtxt TYPE bsak-sgtxt,                  " Text OR Remarks
             ebeln TYPE bsak-ebeln,                  " Billing Doc.No
             umskz TYPE bsak-umskz,                  " Spl.Gl.Ind
             bukrs TYPE bsak-bukrs,
             gjahr TYPE bsak-gjahr,
             sknto TYPE bsak-sknto,
             zterm TYPE bsak-zterm,
             zbd1t TYPE bsak-zbd1t,
             umsks TYPE bsak-umsks,
             augdt TYPE bsak-augdt,
             zuonr TYPE bsak-zuonr,
             buzei TYPE bsak-buzei,

           END OF gs_bsak.

    DATA: gt_bsak  TYPE TABLE OF gs_bsak,
          wa_bsak  TYPE gs_bsak,
          wa_bsak1 TYPE gs_bsak,
          gt_bsak1 TYPE TABLE OF gs_bsak,
          gt_bsak2 TYPE TABLE OF gs_bsak.

    DATA: gt_bsak7 TYPE TABLE OF gs_bsak,
          wa_bsak7 TYPE gs_bsak.

    TYPES: BEGIN OF gs_adrc,
             addrnumber TYPE adrc-addrnumber,        " Address Number
             street     TYPE adrc-street,                " Street
             city1      TYPE adrc-city1,                  " District
             city2      TYPE adrc-city2,                  " City
             post_code1 TYPE adrc-post_code1,        " Postal Code
           END OF gs_adrc.

    DATA: gt_adrc TYPE TABLE OF gs_adrc,
          wa_adrc TYPE gs_adrc.

    TYPES : BEGIN OF ty_chk,
              belnr  TYPE bsak-belnr,                  " Bill Number
              budat  TYPE bsik-budat,                 " Posting Date
              blart  TYPE bsik-blart,                  " Bill Doc. Type
              cr_amt TYPE bsak-dmbtr,                 " Cr Amount
              dr_amt TYPE bsak-dmbtr,                 " Dr Amount
              shkzg  TYPE bsik-shkzg,
            END OF ty_chk.

    TYPES: BEGIN OF gs_final,
             lifnr    TYPE bsak-lifnr,                  " Customer Number
             umskz    TYPE bsik-umskz,                  " Spl.GL Ind
             belnr    TYPE bsak-belnr,                  " Bill Number
             ebeln    TYPE bsak-ebeln,                  " Billing Document Number
             xblnr    TYPE bsik-xblnr,                  " Reference Document Number
             bldat    TYPE bsak-bldat,                  " Posting Date in the Document
             zfbdt TYPE bsak-zfbdt,
             cr_amt   TYPE String,                 " Cr Amount
             dr_amt   TYPE string,                 " Dr Amount
             budat    TYPE bsik-budat,                 " Posting Date
             name1    TYPE lfa1-name1,                 " Customer Name
             blart    TYPE bsik-blart,                  " Bill Doc. Type
             bschl    TYPE bsid-bschl,
             sgtxt    TYPE bsik-sgtxt,                  " Text OR Remarks
             gsber    TYPE bsik-gsber,                  " Business Area
             shkzg    TYPE bsik-shkzg,                  " Dbit/Crdit ind.
             bal_amt  TYPE bsak-dmbtr,                " Balance Amount
             ltext    TYPE t003t-ltext,                  " Bill Doc .Type Descrption
             bukrs    TYPE bsik-bukrs,
             gjahr    TYPE bsik-gjahr,
             sknto    TYPE bsak-sknto,
             lv_count TYPE int1,
             sta      TYPE int1,
             t_tttext TYPE char20,
             zterm    TYPE bsak-zterm,
             zbd1t    TYPE string,
           END OF gs_final.

    DATA: gt_final  TYPE TABLE OF gs_final,
          wa_final  TYPE gs_final,
          gt_final1 TYPE TABLE OF gs_final,
          wa_final1 TYPE gs_final,
          gt_final2 TYPE TABLE OF gs_final,
          wa_final2 TYPE gs_final,
          gt_final3 TYPE TABLE OF gs_final,
          wa_final3 TYPE gs_final,
          gs_chk    TYPE ty_chk.

    TYPES: BEGIN OF gs_faglflexa,
             docnr TYPE faglflexa-docnr,
             prctr TYPE faglflexa-prctr,
             bschl TYPE faglflexa-bschl,
           END OF gs_faglflexa.

    DATA: gt_faglflexa TYPE TABLE OF gs_faglflexa,
          wa_faglflexa TYPE gs_faglflexa.

    TYPES : BEGIN OF gs_lfc1,
              lifnr TYPE lfc1-lifnr,
              bukrs TYPE lfc1-bukrs,
              umsav TYPE lfc1-umsav,
              gjahr TYPE lfc1-gjahr,
            END OF gs_lfc1.

    DATA: gt_lfc1 TYPE TABLE OF gs_lfc1,
          wa_lfc1 TYPE gs_lfc1.

    DATA: or_budat TYPE  bsak-budat,
          or_bukrs TYPE bsik-bukrs,
          or_lifnr TYPE lfa1-lifnr,
          fm_name  TYPE rs38l_fnam,
          or_prctr TYPE cepc-prctr,
          or_umskz TYPE bsik-umskz.

    DATA: l_lifnr TYPE lfa1-lifnr,
          l_prctr TYPE cepc-prctr.

    DATA: b_date TYPE sy-datum.

    DATA: lv_opn   TYPE bsak-dmbtr,
          rv_opn   TYPE bsak-dmbtr,
          lv_total TYPE bsak-dmbtr,
          lv_trans TYPE bsak-dmbtr,
          lv_date  TYPE sy-datum,
          lv_frdat TYPE sy-datum,
          lv_todat TYPE sy-datum,
          lv_sum   TYPE bsak-dmbtr,
          lv_buk   TYPE t001-bukrs,
          lv_cramt TYPE bsak-dmbtr,
          lv_dbamt TYPE bsak-dmbtr,
          lv_cdisc TYPE bsak-dmbtr.

    TYPES: BEGIN OF tem_lfa1,
             lifnr TYPE lfa1-lifnr,
             name1 TYPE lfa1-name1,
             adrnr TYPE lfa1-adrnr,
           END OF tem_lfa1.
    DATA: z_lfa1 TYPE TABLE OF tem_lfa1,
          y_lfa1 TYPE tem_lfa1.

    TYPES: BEGIN OF tem_lfb1,
             lifnr TYPE lfb1-lifnr,
             bukrs TYPE lfb1-bukrs,
           END OF tem_lfb1.
    DATA: z_lfb1 TYPE TABLE OF tem_lfb1,
          y_lfb1 TYPE tem_lfb1.

    DATA:so_lifnr TYPE lfa1-lifnr,
         so_bukrs TYPE lfb1-bukrs.

    TYPES:BEGIN OF ty_doc,
            belnr TYPE bsak-belnr,
          END OF ty_doc.

    DATA: it_doc TYPE TABLE OF ty_doc,
          wa_doc TYPE ty_doc.
    DATA: it_vbsegk TYPE TABLE OF vbsegk,
          wa_vbsegk TYPE vbsegk.

    TYPES : BEGIN OF t_body,
              Accitem TYPE string,
            END OF t_body.
    DATA: it_acc_body TYPE STANDARD TABLE OF t_body,
          wa_acc_body TYPE t_body.

    CONSTANTS : c_comma                TYPE c VALUE ',',
                c_curly_brackets_open  TYPE char01 VALUE '{',
                c_curly_brackets_close TYPE char01 VALUE '}',
                c_quatation_mark       TYPE char01 VALUE '"',
                c_colon                TYPE char01 VALUE':'.
    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

* get the request attributes
    lv_path_info = server->request->get_header_field( name = '~path_info' ).
    SHIFT lv_path_info LEFT BY 1 PLACES.

    FIELD-SYMBOLS <fs_param> TYPE LINE OF tihttpnvp.

*Get GET parameters
    CALL METHOD server->request->get_form_fields
      CHANGING
        fields = it_inputparams.
    UNASSIGN <fs_param>.
    LOOP AT it_inputparams ASSIGNING <fs_param>.
      TRANSLATE <fs_param>-name TO UPPER CASE.
    ENDLOOP.

    CLEAR: date_frm,ls_inputparams.

    READ TABLE it_inputparams INTO ls_inputparams WITH KEY name = 'DATE_FRM'.
    date_frm = ls_inputparams-value.
    CLEAR ls_inputparams.
    READ TABLE it_inputparams INTO ls_inputparams WITH KEY name = 'DATE_TO'.
    date_to = ls_inputparams-value.
    CLEAR ls_inputparams.
    READ TABLE it_inputparams INTO ls_inputparams WITH KEY name = 'VENDOR'.
    vendor = ls_inputparams-value.
    CLEAR ls_inputparams.

    so_bukrs = 1000 .
    so_lifnr = vendor.
    SELECT lifnr bukrs FROM lfb1 INTO TABLE z_lfb1 WHERE bukrs EQ so_bukrs AND loevm <> 'X' AND lifnr = so_lifnr .

    IF  z_lfb1 IS NOT INITIAL.
      SORT z_lfb1 BY lifnr.
      CLEAR: lv_buk.
      LOOP AT z_lfb1 INTO y_lfb1.
        SELECT bukrs  FROM t001 INTO lv_buk WHERE bukrs EQ y_lfb1-bukrs.
          CLEAR: gt_bsak.
          SELECT
             prctr
             lifnr
             umskz
             augbl
             budat
             bldat
             blart
             zfbdt
             dmbtr
             shkzg
             xblnr
             belnr
             gsber
             sgtxt
             ebeln
             bukrs
             gjahr
             sknto
             zterm
             zbd1t
             umsks
             augdt
             zuonr
             buzei
             FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak WHERE budat GE date_frm AND budat LE date_to AND bukrs EQ y_lfb1-bukrs
                                                            AND lifnr EQ y_lfb1-lifnr AND umskz <> 'H'  AND umskz <> 'F' .

          CLEAR:gt_bsik.
          SELECT
             prctr
             lifnr
             augbl
             budat
             bldat
             blart
             zfbdt
             dmbtr
             shkzg
             xblnr
             belnr
             gsber
             sgtxt
             ebeln
             umskz
             bukrs
             gjahr
             sknto
            zterm
            zbd1t FROM bsik INTO CORRESPONDING FIELDS OF TABLE gt_bsik WHERE budat GE date_frm AND budat LE date_to AND bukrs EQ y_lfb1-bukrs
                                                            AND lifnr EQ y_lfb1-lifnr AND umskz <> 'H' AND umskz <> 'F'.

          CLEAR:gt_bsak1.
          SELECT
            prctr
            lifnr
            umskz
            augbl
            budat
            bldat
            blart
            zfbdt
            dmbtr
            shkzg
            xblnr
            belnr
            gsber
            sgtxt
            ebeln
            bukrs
            gjahr
            sknto
            umsks
            augdt
            zuonr
            buzei
            zterm
            zbd1t FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak1
                  WHERE  budat BETWEEN '01.04.2014' AND date_frm AND bukrs EQ y_lfb1-bukrs AND lifnr EQ y_lfb1-lifnr AND umskz <> 'H' AND umskz <> 'F' .

          CLEAR: gt_bsik1.
          SELECT
            prctr
            lifnr
            augbl
            budat
            bldat
            blart
            zfbdt
            dmbtr
            shkzg
            xblnr
            belnr
            gsber
            sgtxt
            ebeln
            umskz
            bukrs
            gjahr
            sknto
            zterm
            zbd1t FROM bsik INTO CORRESPONDING FIELDS OF TABLE gt_bsik1
                  WHERE budat BETWEEN '01.04.2014'  AND  date_frm AND bukrs EQ y_lfb1-bukrs AND lifnr EQ y_lfb1-lifnr AND umskz <> 'H' AND umskz <> 'F'.
          REFRESH it_doc.
          LOOP AT gt_bsak INTO wa_bsak.
            wa_doc-belnr =  wa_bsak-belnr .
            APPEND wa_doc TO it_doc .
          ENDLOOP.

          LOOP AT  gt_bsik INTO wa_bsik.
            wa_doc-belnr = wa_bsik-belnr.
            APPEND wa_doc TO it_doc.
          ENDLOOP.

          LOOP AT gt_bsak1 INTO wa_bsak1.
            wa_doc-belnr =  wa_bsak1-belnr .
            APPEND wa_doc TO it_doc .
          ENDLOOP.

          LOOP AT  gt_bsik1 INTO wa_bsik1.
            wa_doc-belnr = wa_bsik1-belnr.
            APPEND wa_doc TO it_doc.
          ENDLOOP.

          SELECT * FROM vbsegk INTO TABLE it_vbsegk FOR ALL ENTRIES IN it_doc WHERE belnr = it_doc-belnr AND lifnr = so_lifnr .
          LOOP AT it_vbsegk INTO wa_vbsegk.
            DELETE gt_bsak WHERE belnr = wa_vbsegk-belnr.
            DELETE gt_bsik WHERE belnr = wa_vbsegk-belnr.
            DELETE gt_bsak1 WHERE belnr = wa_vbsegk-belnr.
            DELETE gt_bsik1 WHERE belnr = wa_vbsegk-belnr.
          ENDLOOP.

          CLEAR:gt_bsak7.
          SORT gt_bsak BY belnr augbl.
          APPEND LINES OF gt_bsak TO gt_bsik.
          APPEND LINES OF gt_bsak TO gt_bsak7.
          DELETE gt_bsak7 WHERE shkzg = 'S'.
          APPEND LINES OF gt_bsak1 TO gt_bsik1.

          IF gt_bsik[] IS NOT INITIAL.
            CLEAR: gt_lfa1[].
            SELECT
              lifnr
              name1
              adrnr FROM lfa1 INTO TABLE gt_lfa1 FOR ALL ENTRIES IN gt_bsik
                              WHERE lifnr = gt_bsik-lifnr.
            CLEAR: gt_t003t.
            SELECT blart
                   ltext
                   spras FROM  t003t INTO TABLE gt_t003t FOR ALL ENTRIES IN gt_bsik WHERE blart = gt_bsik-blart AND spras = 'E' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
            CLEAR:gt_t003t1.
            SELECT  blart
                    ltext
                    spras FROM  t003t INTO TABLE gt_t003t1 FOR ALL ENTRIES IN gt_bsak WHERE blart = gt_bsak-blart AND spras = 'E' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

            APPEND  LINES OF gt_t003t1 TO gt_t003t.
            CLEAR:gt_t003t2.
            SELECT  blart
                    ltext
                    spras FROM  t003t INTO TABLE gt_t003t2 FOR ALL ENTRIES IN gt_bsik WHERE blart = gt_bsik-blart AND spras = 'E' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
            APPEND  LINES OF gt_t003t2 TO gt_t003t.

          ENDIF.
          IF gt_lfa1[] IS  NOT INITIAL.
            CLEAR: gt_adrc.
            SELECT  addrnumber
                    street
                    city1
                    city2
                    post_code1 FROM adrc INTO TABLE gt_adrc FOR ALL ENTRIES IN gt_lfa1 WHERE addrnumber = gt_lfa1-adrnr.

          ENDIF.
          CLEAR: gt_lfc1.
          IF gt_bsik IS NOT INITIAL.
            SELECT  lifnr
                    bukrs
                    umsav
                    gjahr FROM lfc1 INTO TABLE gt_lfc1 FOR ALL ENTRIES IN gt_bsik WHERE lifnr = gt_bsik-lifnr AND gjahr = gt_bsik-gjahr AND bukrs = gt_bsik-bukrs.
          ENDIF.
        ENDSELECT.

        CLEAR: lv_total.
        LOOP AT gt_lfc1 INTO wa_lfc1.
          lv_total =  wa_lfc1-umsav.
        ENDLOOP.
        CLEAR:wa_lfc1.

        LOOP AT gt_bsik1 INTO wa_bsik1 .
          IF  wa_bsik1-budat < date_frm.
            IF wa_bsik1-shkzg = 'S'.
              lv_opn = lv_opn  + wa_bsik1-dmbtr.
            ELSEIF wa_bsik1-shkzg = 'H'.
              lv_opn  = lv_opn - wa_bsik1-dmbtr.
            ENDIF.
          ENDIF.
          CLEAR wa_bsik1.
        ENDLOOP.

        SORT gt_bsik BY budat belnr blart shkzg.

        LOOP AT gt_bsik INTO wa_bsik.
          wa_final-xblnr = wa_bsik-xblnr.
          wa_final-bldat = wa_bsik-bldat.
          wa_final-lifnr = wa_bsik-lifnr.
          wa_final-belnr = wa_bsik-belnr.
          wa_final-budat = wa_bsik-budat.
          wa_final-gsber = wa_bsik-gsber.
          wa_final-sgtxt = wa_bsik-sgtxt.
          wa_final-ebeln = wa_bsik-ebeln.
          wa_final-umskz = wa_bsik-umskz.
          wa_final-bukrs = wa_bsik-bukrs.
          wa_final-zterm = wa_bsik-zterm.
          wa_final-zbd1t = wa_bsik-zbd1t.
          wa_final-zfbdt = wa_bsik-zfbdt.
          READ TABLE gt_t003t INTO wa_t003t WITH KEY blart = wa_bsik-blart.
          wa_final-ltext = wa_t003t-ltext.
          wa_final-blart = wa_bsik-blart.
          READ TABLE gt_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsik-lifnr.
          wa_final-name1 = wa_lfa1-name1.
          wa_final-shkzg = wa_bsik-shkzg.
          IF gs_chk-budat = wa_bsik-budat AND gs_chk-blart = wa_bsik-blart AND gs_chk-belnr = wa_bsik-belnr
                                          AND gs_chk-shkzg = wa_bsik-shkzg AND wa_bsik-shkzg = 'H'.
            wa_final-sknto = lv_cdisc + wa_bsik-sknto.
            lv_cdisc = wa_final-sknto.
            wa_final-cr_amt = lv_cramt + wa_bsik-dmbtr .
            lv_cramt = wa_final-cr_amt.
            DELETE gt_final INDEX lv_ind.
          ELSEIF gs_chk-budat = wa_bsik-budat AND gs_chk-blart = wa_bsik-blart AND gs_chk-belnr = wa_bsik-belnr
                                              AND gs_chk-shkzg = wa_bsik-shkzg AND wa_bsik-shkzg = 'S'.
            wa_final-dr_amt = lv_dbamt + wa_bsik-dmbtr.
            lv_dbamt = wa_final-dr_amt.
            DELETE gt_final INDEX lv_ind.
          ELSEIF gs_chk-budat = wa_bsik-budat AND gs_chk-blart = wa_bsik-blart AND gs_chk-belnr = wa_bsik-belnr
                                              AND gs_chk-shkzg = wa_bsik-shkzg AND wa_bsik-shkzg = 'C' AND wa_t003t-blart = 'AB'.
            wa_final-cr_amt = lv_cramt + wa_bsik-dmbtr.
            lv_cramt = wa_final-cr_amt.
            DELETE gt_final INDEX lv_ind.
          ELSEIF gs_chk-budat = wa_bsik-budat AND gs_chk-blart = wa_bsik-blart AND gs_chk-belnr = wa_bsik-belnr
                                              AND wa_bsik-shkzg = 'D' AND wa_t003t-blart = 'AB'.
            wa_final-dr_amt = lv_dbamt + wa_bsik-dmbtr.
            lv_dbamt = wa_final-dr_amt.
            DELETE gt_final INDEX lv_ind.
          ELSEIF ( gs_chk-budat <> wa_bsik-budat AND gs_chk-blart <> wa_bsik-blart AND gs_chk-belnr <> wa_bsik-belnr AND wa_bsik-shkzg <> 'S' )
              OR ( ( gs_chk-budat <> wa_bsik-budat OR gs_chk-blart <> wa_bsik-blart OR gs_chk-belnr <> wa_bsik-belnr ) AND wa_bsik-shkzg <> 'S' ) OR
                   ( gs_chk-shkzg <> wa_bsik-shkzg AND wa_bsik-shkzg <> 'S' ) .
            wa_final-sknto = wa_bsik-sknto.
            lv_cdisc = wa_final-sknto .
            wa_final-cr_amt = wa_bsik-dmbtr.
            lv_cramt = wa_final-cr_amt .
          ELSEIF ( gs_chk-budat <> wa_bsik-budat AND gs_chk-blart <> wa_bsik-blart AND gs_chk-belnr <> wa_bsik-belnr AND wa_bsik-shkzg <> 'H' )
              OR ( ( gs_chk-budat <> wa_bsik-budat OR gs_chk-blart <> wa_bsik-blart OR gs_chk-belnr <> wa_bsik-belnr ) AND wa_bsik-shkzg <> 'H' ) OR
                 ( gs_chk-shkzg <> wa_bsik-shkzg AND wa_bsik-shkzg <> 'H' ) .
            wa_final-dr_amt = wa_bsik-dmbtr .
            lv_dbamt = wa_final-dr_amt  .
          ELSEIF ( gs_chk-budat <> wa_bsik-budat AND gs_chk-blart <> wa_bsik-blart AND gs_chk-belnr <> wa_bsik-belnr AND
                   gs_chk-shkzg <> wa_bsik-shkzg AND wa_bsik-shkzg = 'C' )  OR  wa_t003t-blart = 'AB'.
            wa_final-cr_amt = wa_bsik-dmbtr.
            lv_cramt = wa_final-cr_amt.
          ELSEIF ( gs_chk-budat <> wa_bsik-budat AND gs_chk-blart <> wa_bsik-blart AND gs_chk-belnr <> wa_bsik-belnr AND
                   gs_chk-shkzg <> wa_bsik-shkzg AND wa_bsik-shkzg = 'D') OR wa_t003t-blart = 'AB'.
            wa_final-dr_amt = wa_bsik-dmbtr.
            lv_dbamt = wa_final-dr_amt.
          ENDIF.
          gs_chk-belnr = wa_bsik-belnr.
          gs_chk-budat = wa_bsik-budat.
          gs_chk-blart = wa_bsik-blart.
          gs_chk-shkzg = wa_bsik-shkzg.
          APPEND wa_final TO gt_final.
          CLEAR : wa_final,wa_bsik.
          DESCRIBE TABLE gt_final LINES lv_ind.
        ENDLOOP.

        CLEAR: gt_bsik[].
        CLEAR: gs_chk ,wa_bsik, wa_t003t, wa_lfa1 .

        APPEND LINES OF gt_final TO gt_final1.
        APPEND LINES OF gt_final TO gt_final2.

        TYPES: BEGIN OF a,
                 ln TYPE i,
               END OF a.
        DATA: gt_line TYPE TABLE OF a,
              gs_line TYPE a.

        LOOP AT gt_final1 INTO wa_final1.
          IF gs_chk-budat = wa_final1-budat AND
               gs_chk-blart = wa_final1-blart AND
               gs_chk-belnr = wa_final1-belnr.
            IF wa_final1-cr_amt GT gs_chk-cr_amt.
              gs_line-ln = sy-tabix.
            ENDIF.
          ENDIF.
          gs_chk-belnr = wa_final1-belnr.
          gs_chk-budat = wa_final1-budat.
          gs_chk-blart = wa_final1-blart.
          gs_chk-cr_amt = wa_final1-cr_amt.
          gs_chk-dr_amt = wa_final1-dr_amt.
        ENDLOOP.
        b_date = date_to.
        SORT gt_final BY budat shkzg.
        LOOP AT gt_final INTO wa_final.
          READ TABLE gt_final2 INTO wa_final2 WITH KEY belnr = wa_final-belnr shkzg = wa_final-shkzg .
          IF wa_final2-sknto IS NOT INITIAL .
            wa_final2-cr_amt = wa_final2-cr_amt - wa_final2-sknto.
            APPEND wa_final2 TO gt_final3.
            CLEAR: wa_final2-cr_amt.
            wa_final2-cr_amt = wa_final2-sknto.
            APPEND wa_final2 TO gt_final3.
            DELETE gt_final WHERE belnr = wa_final2-belnr AND shkzg = 'H' .
          ENDIF.
        ENDLOOP.

        APPEND LINES OF gt_final3 TO gt_final.

        LOOP AT gt_final INTO wa_final.
          lv_sum = lv_sum + wa_final-dr_amt - wa_final-cr_amt .
          wa_final-bal_amt = lv_sum.
          MODIFY gt_final FROM wa_final TRANSPORTING bal_amt.
          AT END OF lifnr.
            CLEAR :lv_sum,wa_final,wa_bsik.
          ENDAT.
        ENDLOOP.

        LOOP AT gt_final INTO wa_final WHERE lifnr = ' '.
          wa_final-lifnr = wa_lfa1-lifnr.
          MODIFY gt_final FROM wa_final TRANSPORTING lifnr.
          CLEAR  wa_final.
        ENDLOOP.
        CLEAR: rv_opn.
        rv_opn = lv_total - lv_opn.
        SORT gt_final BY budat shkzg.
        CLEAR  wa_final.
        IF gt_final IS NOT INITIAL.
          LOOP AT gt_final INTO wa_final.
            CONCATENATE
            '{'
            '"company_code":'     c_quatation_mark wa_final-bukrs c_quatation_mark c_comma
            '"vendor_code":'      c_quatation_mark wa_final-lifnr c_quatation_mark c_comma
            '"reference_no" :'    c_quatation_mark wa_final-belnr c_quatation_mark c_comma
            '"document_date":'    c_quatation_mark wa_final-bldat c_quatation_mark c_comma
            '"posting_date":'     c_quatation_mark wa_final-budat c_quatation_mark c_comma
            '"net_due_date":'     c_quatation_mark wa_final-zfbdt c_quatation_mark c_comma
            '"due_status":'       c_quatation_mark '-'  c_quatation_mark c_comma
            '"document_type":'    c_quatation_mark wa_final-blart c_quatation_mark c_comma
            '"document_type1":'   c_quatation_mark '-'  c_quatation_mark c_comma
            '"debit_or_credit":'  c_quatation_mark wa_final-shkzg c_quatation_mark c_comma
            '"amt_loc_curr_CR":'     c_quatation_mark wa_final-cr_amt c_quatation_mark c_comma
             '"amt_loc_curr_DR":'     c_quatation_mark wa_final-dr_amt c_quatation_mark c_comma
            '"clearing_doc_no":'  c_quatation_mark '-' c_quatation_mark c_comma
            '"item_text":'        c_quatation_mark '-' c_quatation_mark c_comma
            '"open_or_cleared":'  c_quatation_mark '-' c_quatation_mark c_comma
            '"entry_date":'       c_quatation_mark wa_final-bldat c_quatation_mark c_comma
            '"terms":'            c_quatation_mark wa_final-zterm c_quatation_mark c_comma
            '"termsofdays":'      c_quatation_mark  wa_final-zbd1t c_quatation_mark "c_comma
            '}'
            INTO wa_acc_body-accitem.
            APPEND wa_acc_body TO it_acc_body.
            CLEAR : wa_acc_body.
          ENDLOOP.

          DATA: lv_tabix  TYPE sy-tabix,
                lv_int(6) TYPE n.
          lv_int = 0.
          lv_int = lines( it_acc_body ).

          LOOP AT it_acc_body INTO wa_acc_body.
            lv_tabix = sy-tabix.
            IF lv_tabix < lv_int.
              CONCATENATE ls_json wa_acc_body-accitem ',' INTO ls_json.
            ELSE.
              CONCATENATE ls_json wa_acc_body-accitem INTO ls_json.
            ENDIF.
          ENDLOOP.
          CLEAR: lv_int,lv_tabix.

          CONCATENATE '[' ls_json ']'  INTO v_jsonload.

*Set JSON Content-Type
          CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
          CALL METHOD server->response->set_cdata( data = v_jsonload ).

        ENDIF.

        CLEAR: gt_final[] , wa_final , gt_final1[] ,wa_final1 ,gt_final2[] ,wa_final2 ,gt_final3[] ,wa_final3 , lv_opn , lv_buk ,gt_lfa1[].
      ENDLOOP.

    ENDIF.



  ENDMETHOD.
ENDCLASS.
