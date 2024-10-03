*&---------------------------------------------------------------------*
*& Include          ZDMS_COPY_ACC_DOC_TO_DISTRIBCL
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_output,
*             check   TYPE check,
             kunnr   TYPE kunnr,
             name1   TYPE name1,
             werks   TYPE werks_d,
             gsber   TYPE gsber,
             gtext   TYPE gtext,
             belnr   TYPE belnr_d,
             sgtxt   TYPE sgtxt,
             gjahr   TYPE gjahr,
             blart   TYPE blart,
             stblg   TYPE stblg,
             stjah   TYPE stjah,
             blart1  TYPE blart,
             bktxt   TYPE bktxt,
             cpudt   TYPE cpudt,
             budat   TYPE budat,
             bldat   TYPE bldat,
             vbeln   TYPE vbeln,
             shkzg   TYPE shkzg,
             dmbtr   TYPE dmbtr,
             netwr   TYPE netwr,
             mwsbk   TYPE mwsbp,
             kschl   TYPE kschl,
             kwert   TYPE kwert,
             josg    TYPE kwert,
             jocg    TYPE kwert,
             joig    TYPE kwert,
             jtc1    TYPE kwert,
             blart2  TYPE blart,
             belnr2  TYPE belnr_d,
             belnr3  TYPE belnr_d,
             type(1) TYPE c,
             remarks TYPE remarks,
           END OF ty_output.
    DATA: gt_final_tab TYPE STANDARD TABLE OF ty_output.
    DATA: gt_zdms_dgdr_glacc TYPE STANDARD TABLE OF zdms_dgdr_glacc.
    DATA: gw_final_tab TYPE ty_output.
    DATA: g_posted TYPE flag.
    DATA: gr_alv TYPE REF TO cl_salv_table.
    DATA: gr_selections TYPE REF TO cl_salv_selections,
          gt_rows       TYPE salv_t_row.

    METHODS:
      get_data,
      build_alv,
      post_documents,
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.                    "lcl_handle_events DEFINITION
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    CASE e_salv_function.
      WHEN 'POST'.
        IF g_posted IS INITIAL.

* Get the selected rows.
          gt_rows = gr_selections->get_selected_rows( ).
          IF gt_rows IS INITIAL.
            LOOP AT gt_final_tab ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE type NE 'P'.
              gw_final_tab = <fs_data>.
              " Post Method
              post_documents( ).
              " Get Doc No after Posting
              <fs_data>-belnr2 = gw_final_tab-belnr2.
              <fs_data>-belnr3 = gw_final_tab-belnr3.
              <fs_data>-type = gw_final_tab-type.
              <fs_data>-remarks = gw_final_tab-remarks.
            ENDLOOP.
          ELSE.
* Display the selected rows.
            LOOP AT gt_rows INTO DATA(lwa_rows).
              READ TABLE gt_final_tab ASSIGNING <fs_data> INDEX lwa_rows.
              IF <fs_data>-type NE 'P'.
                gw_final_tab = <fs_data>.
                " Post Method
                post_documents( ).
                " Get Doc No after Posting
                <fs_data>-belnr2 = gw_final_tab-belnr2.
                <fs_data>-belnr3 = gw_final_tab-belnr3.
                <fs_data>-type = gw_final_tab-type.
                <fs_data>-remarks = gw_final_tab-remarks.
              ENDIF.
            ENDLOOP.

          ENDIF.
          gr_alv->refresh( ).
          g_posted = 'X'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD get_data.

    TYPES ty_blart TYPE RANGE OF blart.
    IF so_docty[] IS INITIAL.
      DATA(lr_blart) = VALUE ty_blart( ( sign = 'I' option = 'EQ' low = 'RV' )
                                       ( sign = 'I' option = 'EQ' low = 'VG' ) ).
    ELSE.
      LOOP AT so_docty[] INTO so_docty WHERE low = 'RV' OR low = 'VG'.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = so_docty-low ) TO lr_blart.
      ENDLOOP.
    ENDIF.
    IF lr_blart IS NOT INITIAL.
      SELECT
         a~kunnr,
         d~name1,
         d~werks,
         e~gsber,
         f~gtext,
         a~belnr,
         a~sgtxt,
         a~gjahr,
         a~blart,
         a~cpudt,
         a~budat,
         a~bldat,
         a~vbeln,
         a~shkzg, "D/C Indicator
         a~dmbtr,
*   b~knumv, " Condition No
         b~netwr,
         b~mwsbk,
         c~kschl,
         c~kwert,
         c~kwert AS josg,
         c~kwert AS jocg,
         c~kwert AS joig,
         c~kwert AS jtc1,
         d~knurl AS remarks
        FROM bsid AS a INNER JOIN vbrk AS b ON  b~vbeln = a~vbeln
*                                            AND b~fksto NE 'X'  " Cancelled
*                                            AND b~sfakn = ''    " Cancelled Billing Document
                       INNER JOIN prcd_elements AS c ON c~knumv = b~knumv
                                                    AND c~kschl IN ('JOSG','JOCG','JOIG','JTC1')
                       INNER JOIN kna1 AS d ON d~kunnr = a~kunnr
                       INNER JOIN t134g AS e ON e~werks = d~werks AND e~spart = '10'
                       INNER JOIN tgsbt AS f ON f~spras = @sy-langu AND f~gsber = e~gsber
        INTO TABLE @DATA(lt_tab_data)
        WHERE a~bukrs = '1000'
        AND a~gjahr GE '2023'
        AND a~cpudt IN @so_docdt
        AND a~blart IN @lr_blart
        AND a~kunnr IN @gr_kunnr.

      SELECT
         a~kunnr,
         d~name1,
         d~werks,
         e~gsber,
         f~gtext,
         a~belnr,
         a~sgtxt,
         a~gjahr,
         a~blart,
         a~cpudt,
         a~budat,
         a~bldat,
         a~vbeln,
         a~shkzg, "D/C Indicator
         a~dmbtr,
*   b~knumv, " Condition No
         b~netwr,
         b~mwsbk,
         c~kschl,
         c~kwert,
         c~kwert AS josg,
         c~kwert AS jocg,
         c~kwert AS joig,
         c~kwert AS jtc1,
         d~knurl AS remarks
        FROM bsad AS a INNER JOIN vbrk AS b ON  b~vbeln = a~vbeln
*                                            AND b~fksto NE 'X'  " Cancelled
*                                            AND b~sfakn = ''    " Cancelled Billing Document
                       INNER JOIN prcd_elements AS c ON c~knumv = b~knumv
                                                    AND c~kschl IN ('JOSG','JOCG','JOIG','JTC1')
                       INNER JOIN kna1 AS d ON d~kunnr = a~kunnr
                       INNER JOIN t134g AS e ON e~werks = d~werks AND e~spart = '10'
                       INNER JOIN tgsbt AS f ON f~spras = @sy-langu AND f~gsber = e~gsber
        WHERE a~bukrs = '1000'
        AND a~gjahr GE '2023'
        AND a~cpudt IN @so_docdt
        AND a~blart IN @lr_blart
        AND a~kunnr IN @gr_kunnr
        APPENDING TABLE @lt_tab_data.

      " Get Reversal Doc Number
      SELECT
            belnr,
            gjahr,
            blart,
            stblg,
            stjah,
            bktxt
          FROM bkpf INTO TABLE @DATA(lt_bkpf_rev)
        FOR ALL ENTRIES IN @lt_tab_data
         WHERE  bukrs = '1000'
            AND belnr = @lt_tab_data-belnr
            AND gjahr = @lt_tab_data-gjahr
            AND blart = 'RV' .
    ENDIF.

    CLEAR lr_blart.

    " Doc Type DZ
    IF so_docty[] IS INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'DZ' ) TO lr_blart.
    ELSE.
      LOOP AT so_docty[] INTO so_docty WHERE low = 'DZ'.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = so_docty-low ) TO lr_blart.
      ENDLOOP.
    ENDIF.
    IF lr_blart IS NOT INITIAL.

      SELECT
         a~kunnr,
         d~name1,
         d~werks,
         e~gsber,
         f~gtext,
         a~belnr,
         a~sgtxt,
         a~gjahr,
         a~blart,
         a~cpudt,
         a~budat,
         a~bldat,
         a~vbeln,
         a~shkzg, "D/C Indicator
         a~dmbtr,
         a~dmbt1 AS netwr,
         a~dmbt2 AS mwsbk,
         a~gsber AS kschl,
         a~dmbtr,
         a~dmbtr AS josg,
         a~dmbtr AS jocg,
         a~dmbtr AS joig,
         a~dmbtr AS jtc1,
         d~knurl AS remarks
        FROM bsid AS a INNER JOIN kna1 AS d ON d~kunnr = a~kunnr
                       INNER JOIN t134g AS e ON e~werks = d~werks AND e~spart = '10'
                       INNER JOIN tgsbt AS f ON f~spras = @sy-langu AND f~gsber = e~gsber
        WHERE a~bukrs = '1000'
        AND a~gjahr GE '2023'
        AND a~cpudt IN @so_docdt
        AND a~blart IN @lr_blart
        AND a~kunnr IN @gr_kunnr
        APPENDING TABLE @lt_tab_data.

      SELECT
         a~kunnr,
         d~name1,
         d~werks,
         e~gsber,
         f~gtext,
         a~belnr,
         a~sgtxt,
         a~gjahr,
         a~blart,
         a~cpudt,
         a~budat,
         a~bldat,
         a~vbeln,
         a~shkzg, "D/C Indicator
         a~dmbtr,
         a~dmbt1 AS netwr,
         a~dmbt2 AS mwsbk,
         a~gsber AS kschl,
         a~dmbtr,
         a~dmbtr AS josg,
         a~dmbtr AS jocg,
         a~dmbtr AS joig,
         a~dmbtr AS jtc1,
         d~knurl AS remarks
        FROM bsad AS a INNER JOIN kna1 AS d ON d~kunnr = a~kunnr
                       INNER JOIN t134g AS e ON e~werks = d~werks AND e~spart = '10'
                       INNER JOIN tgsbt AS f ON f~spras = @sy-langu AND f~gsber = e~gsber
        WHERE a~bukrs = '1000'
        AND a~gjahr GE '2023'
        AND a~cpudt IN @so_docdt
        AND a~blart IN @lr_blart
        AND a~kunnr IN @gr_kunnr
        APPENDING TABLE @lt_tab_data.
    ENDIF.

    " Doc Type DG and DR
    CLEAR lr_blart.
    IF so_docty[] IS INITIAL.
      lr_blart = VALUE ty_blart( ( sign = 'I' option = 'EQ' low = 'DG' )
                                 ( sign = 'I' option = 'EQ' low = 'DR' )
                                 ( sign = 'I' option = 'EQ' low = 'DA' )  ).
    ELSE.
      LOOP AT so_docty[] INTO so_docty WHERE low = 'DG' OR low = 'DR' OR low = 'DA'.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = so_docty-low ) TO lr_blart.
      ENDLOOP.
    ENDIF.
    IF lr_blart IS NOT INITIAL.
*    " Select Doc No only for DR and DG
      SELECT
            a~kunnr,
            d~name1,
            d~werks,
            e~gsber,
            f~gtext,
            a~belnr,
            a~sgtxt,
            a~gjahr,
            a~blart,
            a~cpudt,
            a~budat,
            a~bldat,
            a~vbeln,
            a~shkzg, "D/C Indicator
            a~dmbtr,
            a~dmbt1 AS netwr,
            a~dmbt2 AS mwsbk,
            a~gsber AS kschl, " Condition Type
            a~dmbtr AS kwert,
            a~dmbtr AS josg,
            a~dmbtr AS jocg,
            a~dmbtr AS joig,
            a~dmbtr AS jtc1,
            d~knurl AS remarks
           FROM bsid AS a
                          INNER JOIN kna1 AS d ON d~kunnr = a~kunnr
                          INNER JOIN t134g AS e ON e~werks = d~werks AND e~spart = '10'
                          INNER JOIN tgsbt AS f ON f~spras = @sy-langu AND f~gsber = e~gsber
           WHERE a~bukrs = '1000'
           AND a~gjahr GE '2023'
           AND a~cpudt IN @so_docdt
           AND a~blart IN @lr_blart
           AND a~kunnr IN @gr_kunnr
           APPENDING TABLE @lt_tab_data.
      SELECT
            a~kunnr,
            d~name1,
            d~werks,
            e~gsber,
            f~gtext,
            a~belnr,
            a~sgtxt,
            a~gjahr,
            a~blart,
            a~cpudt,
            a~budat,
            a~bldat,
            a~vbeln,
            a~shkzg, "D/C Indicator
            a~dmbtr,
            a~dmbt1 AS netwr,
            a~dmbt2 AS mwsbk,
            a~gsber AS kschl, " Condition Type
            a~dmbtr AS kwert,
            a~dmbtr AS josg,
            a~dmbtr AS jocg,
            a~dmbtr AS joig,
            a~dmbtr AS jtc1,
            d~knurl AS remarks
           FROM bsad AS a
                          INNER JOIN kna1 AS d ON d~kunnr = a~kunnr
                          INNER JOIN t134g AS e ON e~werks = d~werks AND e~spart = '10'
                          INNER JOIN tgsbt AS f ON f~spras = @sy-langu AND f~gsber = e~gsber
           WHERE a~bukrs = '1000'
           AND a~gjahr GE '2023'
           AND a~cpudt IN @so_docdt
           AND a~blart IN @lr_blart
           AND a~kunnr IN @gr_kunnr
           APPENDING TABLE @lt_tab_data.

      IF lt_tab_data IS NOT INITIAL.
        " Get GST Data
        SELECT
            a~belnr,
            a~gjahr,
            a~blart,
            c~kschl, " Condition Type
            b~dmbtr AS kwert
            FROM bkpf AS a
            INNER JOIN bseg AS b ON b~bukrs = a~bukrs
                                AND b~gjahr = a~gjahr
                                AND b~belnr = a~belnr
            INNER JOIN zfi_gst_glacc AS c ON c~hkont = b~hkont
          FOR ALL ENTRIES IN @lt_tab_data
           WHERE a~bukrs = '1000'
           AND a~belnr = @lt_tab_data-belnr
           AND a~gjahr = @lt_tab_data-gjahr
           AND a~blart IN @lr_blart
           APPENDING CORRESPONDING FIELDS OF TABLE @lt_tab_data.

        " Get Reversal Doc Number
        SELECT
              belnr,
              gjahr,
              blart,
              stblg,
              stjah,
              bktxt
            FROM bkpf
          FOR ALL ENTRIES IN @lt_tab_data
           WHERE  bukrs = '1000'
              AND belnr = @lt_tab_data-belnr
              AND gjahr = @lt_tab_data-gjahr
              AND blart = 'DA'
          APPENDING TABLE @lt_bkpf_rev.

      ENDIF.
    ENDIF.

    SELECT * FROM zdms_dgdr_glacc
      INTO TABLE gt_zdms_dgdr_glacc.

    SELECT * FROM zdms_accpost_log INTO TABLE @DATA(lt_log_table)
      FOR ALL ENTRIES IN @lt_tab_data
      WHERE kunnr = @lt_tab_data-kunnr
        AND belnr = @lt_tab_data-belnr
        AND gjahr = @lt_tab_data-gjahr
        AND type IN @so_mtype[].

    DATA lw_fianle_data LIKE LINE OF gt_final_tab.
    LOOP AT lt_tab_data INTO DATA(lw_data).
      IF lt_log_table IS NOT INITIAL.
        READ TABLE lt_log_table TRANSPORTING NO FIELDS WITH KEY belnr = lw_data-belnr gjahr = lw_data-gjahr.
        IF sy-subrc NE 0 AND so_mtype[] IS NOT INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.
      READ TABLE gt_final_tab ASSIGNING FIELD-SYMBOL(<fs_data>) WITH KEY belnr = lw_data-belnr.
      IF sy-subrc = 0.
        IF lw_data-blart EQ 'RV' OR lw_data-blart EQ 'VG' OR " Salse Invoice and Sales Return
           lw_data-blart EQ 'DR' OR lw_data-blart EQ 'DG' OR  " Debit Note and Credit Note
           lw_data-blart EQ 'DA' .  " Reversal Document of  DR DG DZ
          IF lw_fianle_data-kschl IS NOT INITIAL.
            ASSIGN COMPONENT lw_data-kschl OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fvalue>).
            <fvalue> = <fvalue> + lw_data-kwert.
          ELSE.
            <fs_data>-dmbtr = <fs_data>-dmbtr + lw_data-dmbtr.
          ENDIF.
        ELSE.
          <fs_data>-dmbtr = <fs_data>-dmbtr + lw_data-dmbtr.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING lw_data TO lw_fianle_data.
        lw_fianle_data-josg = 0.
        lw_fianle_data-jocg = 0.
        lw_fianle_data-joig = 0.
        lw_fianle_data-jtc1 = 0.
        lw_fianle_data-mwsbk = 0.
        lw_fianle_data-netwr = 0.
        IF lw_fianle_data-kschl IS NOT INITIAL.
          ASSIGN COMPONENT lw_fianle_data-kschl OF STRUCTURE lw_fianle_data TO FIELD-SYMBOL(<f1value>).
          <f1value> = <f1value> + lw_fianle_data-kwert.
        ENDIF.
        IF lw_data-blart EQ 'DZ' OR
           lw_data-blart EQ 'DA'.
          lw_fianle_data-vbeln = ''.
        ENDIF.
        lw_fianle_data-kwert = 0.
        APPEND lw_fianle_data TO gt_final_tab.
      ENDIF.
    ENDLOOP.

    SORT gt_final_tab BY kunnr cpudt.

    LOOP AT gt_final_tab ASSIGNING <fs_data>.
      IF <fs_data>-blart = 'DA' OR <fs_data>-blart = 'RV'.
        <fs_data>-stblg = VALUE #( lt_bkpf_rev[ belnr = <fs_data>-belnr gjahr = <fs_data>-gjahr ]-stblg OPTIONAL ).
        <fs_data>-stjah = VALUE #( lt_bkpf_rev[ belnr = <fs_data>-belnr gjahr = <fs_data>-gjahr ]-stjah OPTIONAL ).
        <fs_data>-bktxt = VALUE #( lt_bkpf_rev[ belnr = <fs_data>-belnr gjahr = <fs_data>-gjahr ]-bktxt OPTIONAL ).
        IF <fs_data>-stblg IS NOT INITIAL.
          SELECT SINGLE blart FROM bkpf INTO <fs_data>-blart1
            WHERE bukrs = '1000'
            AND belnr = <fs_data>-stblg
            AND gjahr = <fs_data>-stjah.
        ENDIF.
      ENDIF.
      <fs_data>-mwsbk = <fs_data>-josg + <fs_data>-jocg + <fs_data>-joig + <fs_data>-jtc1.
      <fs_data>-netwr = <fs_data>-dmbtr - <fs_data>-mwsbk.
      CASE <fs_data>-blart.
        WHEN 'RV' OR 'VG'.
          <fs_data>-blart2 = 'RE'.
        WHEN 'DZ'.
          <fs_data>-blart2 = 'KZ'.
        WHEN 'DR'.
          <fs_data>-blart2 = 'KR'.
        WHEN 'DG'.
          <fs_data>-blart2 = 'KG'.
        WHEN 'DA'.
          <fs_data>-blart2 = 'KA'.
        WHEN OTHERS.
      ENDCASE.
      <fs_data>-belnr2 = VALUE #( lt_log_table[ belnr = <fs_data>-belnr gjahr = <fs_data>-gjahr ]-belnr2 OPTIONAL ).
      <fs_data>-belnr3 = VALUE #( lt_log_table[ belnr = <fs_data>-belnr gjahr = <fs_data>-gjahr ]-belnr3 OPTIONAL ).
      <fs_data>-type =  VALUE #( lt_log_table[ belnr = <fs_data>-belnr gjahr = <fs_data>-gjahr ]-type OPTIONAL ).
      <fs_data>-remarks = VALUE #( lt_log_table[ belnr = <fs_data>-belnr gjahr = <fs_data>-gjahr ]-remarks OPTIONAL ).

      IF <fs_data>-remarks IS INITIAL.
        <fs_data>-type = 'N'.
        <fs_data>-remarks = 'Document is to be Post'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD  post_documents.
*https://blogs.sap.com/2023/08/03/posting-gl-entries-vendor-invoice-customer-invoice-using-bapibapi_acc_document_post/

    DATA lw_zdms_accpost_log TYPE zdms_accpost_log.
    DATA: lt_payable   TYPE TABLE OF bapiacap09,
          lt_accountgl TYPE TABLE OF bapiacgl09,
          lt_currency  TYPE TABLE OF bapiaccr09.
    DATA lv_msg TYPE string.
    DATA: l_costcenter TYPE kostl,
          l_return     TYPE bapireturn1,
          lv_fisyear   TYPE bapi0002_4-fiscal_year,
          lv_month     TYPE bapi0002_4-fiscal_period.

*** function module to get fiscal year ***
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = 'DMS1'
        posting_date  = gw_final_tab-budat
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.
    IF l_return IS INITIAL.
      DATA(lv_period) = CONV num03( lv_month ).
      CALL FUNCTION 'FI_PERIOD_CHECK'
        EXPORTING
          i_bukrs          = 'DMS1'
          i_gjahr          = lv_fisyear
          i_koart          = 'S'
          i_monat          = lv_period
          i_glvor          = 'RFBU'
        EXCEPTIONS
          error_period     = 1
          error_period_acc = 2
          invalid_input    = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
          IMPORTING
            message_text_output = lv_msg.
        gw_final_tab-type = 'E'.
        gw_final_tab-remarks = lv_msg.
      ENDIF.
    ELSE.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = l_return-id
          msgnr               = l_return-number
          msgv1               = l_return-message_v1
          msgv2               = l_return-message_v2
          msgv3               = l_return-message_v3
          msgv4               = l_return-message_v4
        IMPORTING
          message_text_output = lv_msg.
      gw_final_tab-type = 'E'.
      gw_final_tab-remarks = lv_msg.
    ENDIF.
    CASE gw_final_tab-blart2.
      WHEN 'RE'.
        DATA(l_account) = '0017000001'.
      WHEN 'KG' OR 'KR'.
        " Get DG DR DA GL KAdata
        SELECT SINGLE b~skont
            FROM bseg AS a
            INNER JOIN zdms_dgdr_glacc AS b ON b~hkont = a~hkont
            INTO @DATA(l_hkont)
           WHERE  a~bukrs = '1000'
              AND a~belnr = @gw_final_tab-belnr
              AND a~gjahr = @gw_final_tab-gjahr
              AND b~blart = @gw_final_tab-blart.
        IF sy-subrc = 0 .
          l_account = l_hkont.
          IF l_account IS INITIAL.
            lv_msg = |GL/Ac { l_hkont } is not maintained in Table ZDMS_DGDR_GLACC|.
          ENDIF.
        ELSE.
          lv_msg = |Table ZDMS_DGDR_GLACC Gl/acc entried is not matched with BSEG records|.
        ENDIF.
        IF gw_final_tab-blart2 = 'KR'.
          l_costcenter = 'DMS_COMMON'.
        ENDIF.
      WHEN 'KA'.
        CASE gw_final_tab-blart1.
          WHEN 'VG'.
            l_account = '0017000001'.
          WHEN 'DZ'.
            SELECT SINGLE outgl
              FROM zdms_distb_gl INTO @DATA(l_outgl)
              WHERE kunnr = @gw_final_tab-kunnr
              AND   werks = @gw_final_tab-werks.
            IF sy-subrc <> 0 OR l_outgl IS INITIAL.
              lv_msg = |Table ZDMS_DISTB_GL outgoing Gl/acc entried is not found|.
            ELSE.
              l_account = l_outgl.
            ENDIF.
          WHEN 'DG' OR 'DR'.
            " Get DG DR DA GL KAdata
            SELECT SINGLE b~skont
                FROM bseg AS a
                INNER JOIN zdms_dgdr_glacc AS b ON b~hkont = a~hkont
                INTO @l_hkont
               WHERE  a~bukrs = '1000'
                  AND a~belnr = @gw_final_tab-belnr
                  AND a~gjahr = @gw_final_tab-gjahr
                  AND b~blart = @gw_final_tab-blart1.
            IF sy-subrc = 0 .
              l_account = l_hkont.
              IF l_account IS INITIAL.
                lv_msg = |GL/Ac { l_hkont } is not maintained in Table ZDMS_DGDR_GLACC|.
                EXIT.
              ENDIF.
            ELSE.
              lv_msg = |Table ZDMS_DGDR_GLACC Gl/acc entried is not matched with BSEG records|.
            ENDIF.
            IF gw_final_tab-blart1 = 'DR'.
              l_costcenter = 'DMS_COMMON'.
            ENDIF.
          WHEN OTHERS.
              lv_msg = 'DA - reversal documents no is not found'.
        ENDCASE.
      WHEN 'KZ'.
        SELECT SINGLE outgl
          FROM zdms_distb_gl INTO @l_outgl
          WHERE kunnr = @gw_final_tab-kunnr
          AND   werks = @gw_final_tab-werks.
        IF sy-subrc <> 0.
          lv_msg = |Maintained Table ZDMS_DISTB_GL outgoing Gl/acc entried is not found|.
        ELSE.
          l_account = l_outgl.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF lv_msg IS INITIAL.
*** Actual Process starts Here for Posting Invoice ****
      DATA(ls_header) = VALUE bapiache09( bus_act = 'RFBU'
                                          username = sy-uname
                                          comp_code = 'DMS1'
                                          header_txt = 'Header Text'
                                          doc_date = gw_final_tab-bldat
                                          pstng_date = gw_final_tab-budat
                                          fisc_year = gw_final_tab-gjahr
                                          fis_period = lv_period
                                          doc_type = gw_final_tab-blart2
                                          ref_doc_no = |{ gw_final_tab-belnr }/{ gw_final_tab-gjahr }| ).

**** Vendor Line items to be passed ***
      DATA(l_lifnr) = CONV lifnr( |{ p_lifnr ALPHA = IN }| ).
      IF gw_final_tab-shkzg = 'H'. " S- Debit H-Credit
        DATA(l_ind) = -1.
      ELSE.
        l_ind = 1.
      ENDIF.
      lt_payable = VALUE #(
                   ( itemno_acc = '1'
                     vendor_no = l_lifnr
                     bus_area   = gw_final_tab-gsber
                     bline_date = gw_final_tab-bldat
                     item_text  = gw_final_tab-sgtxt
                    ) ).

      APPEND VALUE #( itemno_acc = '2'
                      gl_account = l_account
                      bus_area   = gw_final_tab-gsber
                      costcenter  = l_costcenter
                      item_text = |{ gw_final_tab-vbeln }/{ gw_final_tab-gjahr }|  ) TO lt_accountgl. " gw_final_tab-sgtxt

      APPEND VALUE #( itemno_acc = '1'
                      currency = 'INR'
                      amt_doccur = gw_final_tab-dmbtr * l_ind * -1
                      amt_base = gw_final_tab-dmbtr ) TO lt_currency."Vendor Line item

      APPEND VALUE #( itemno_acc = '2'
                      currency = 'INR'
                      amt_doccur = gw_final_tab-netwr * l_ind
                      amt_base =  gw_final_tab-netwr
                    ) TO lt_currency .
      DATA(l_cgst) = '0025010001'.
      DATA(l_sgst) = '0025010002'.
      DATA(l_igst) = '0025010003'.
      DATA(l_tcs)  = '0025020001'.
      IF gw_final_tab-josg IS NOT INITIAL AND gw_final_tab-jocg IS NOT INITIAL.
        APPEND VALUE #( itemno_acc = '3'
                        bus_area   = gw_final_tab-gsber
                        gl_account = l_sgst  ) TO lt_accountgl.
        APPEND VALUE #( itemno_acc = '4'
                        bus_area   = gw_final_tab-gsber
                        gl_account = l_cgst  ) TO lt_accountgl.

        APPEND VALUE #( itemno_acc = '3'
                        currency = 'INR'
                        amt_doccur = gw_final_tab-josg * l_ind
                        amt_base = gw_final_tab-dmbtr  ) TO lt_currency.

        APPEND VALUE #( itemno_acc = '4'
                        currency = 'INR'
                        amt_doccur = gw_final_tab-jocg * l_ind
                        amt_base =  gw_final_tab-netwr
                      ) TO lt_currency .
        IF gw_final_tab-jtc1 IS NOT INITIAL.
          APPEND VALUE #( itemno_acc = '5'
                          bus_area   = gw_final_tab-gsber
                          gl_account = l_tcs  ) TO lt_accountgl.
          APPEND VALUE #( itemno_acc = '5'
                          currency = 'INR'
                          amt_doccur = gw_final_tab-jtc1 * l_ind
                          amt_base =  gw_final_tab-netwr
                        ) TO lt_currency .
        ENDIF.
      ENDIF.
      IF gw_final_tab-joig IS NOT INITIAL.
        APPEND VALUE #( itemno_acc = '3'
                        gl_account = l_igst  ) TO lt_accountgl.
        APPEND VALUE #( itemno_acc = '3'
                        currency = 'INR'
                        amt_doccur = gw_final_tab-joig * l_ind
                        amt_base = gw_final_tab-dmbtr  ) TO lt_currency.
        IF gw_final_tab-jtc1 IS NOT INITIAL.
          APPEND VALUE #( itemno_acc = '4'
                          gl_account = l_tcs  ) TO lt_accountgl.
          APPEND VALUE #( itemno_acc = '4'
                          currency = 'INR'
                          amt_doccur = gw_final_tab-jtc1 * l_ind
                          amt_base =  gw_final_tab-netwr
                        ) TO lt_currency .
        ENDIF.
      ENDIF.

      DATA: return TYPE bapiret2_t.
*** Document Check Before posting ***
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader = ls_header
        TABLES
          accountgl      = lt_accountgl
          accountpayable = lt_payable
          currencyamount = lt_currency
          return         = return.

      LOOP AT return INTO DATA(l_ret) WHERE type = 'E' OR type = 'A'.
        lv_msg = |{ lv_msg } { l_ret-message }|.
      ENDLOOP.
    ENDIF.
    IF lv_msg IS INITIAL.
*** Function Module to create Debit note ***
      DATA: lv_objtyp TYPE bapiache09-obj_type.
      DATA: lv_objkey TYPE bapiache09-obj_key.
      DATA: lv_objsys TYPE bapiache09-obj_sys.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = ls_header
        IMPORTING
          obj_type       = lv_objtyp
          obj_key        = lv_objkey
          obj_sys        = lv_objsys
        TABLES
          accountgl      = lt_accountgl
          accountpayable = lt_payable
          currencyamount = lt_currency
          return         = return.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
      gw_final_tab-remarks = |Document No:{ lv_objkey(10) } Posted Successfully |.
      gw_final_tab-type = 'P'.
      gw_final_tab-belnr2 = lv_objkey(10).
*      IF gw_final_tab-blart2 = 'RE'.
*        CLEAR: lv_objtyp,lv_objkey,lv_objsys,lt_payable,lt_accountgl,lt_currency.
*        l_account = '0022100002'.
*        APPEND VALUE #( itemno_acc = '1'
*                        gl_account = l_account
*                        bus_area   = gw_final_tab-gsber
*                        item_text = |{ gw_final_tab-vbeln }/{ gw_final_tab-gjahr }|  ) TO lt_accountgl. " gw_final_tab-sgtxt
*
*        l_account = '0017000001'.
*        APPEND VALUE #( itemno_acc = '2'
*                        gl_account = l_account
*                        bus_area   = gw_final_tab-gsber
*                        item_text = |{ gw_final_tab-vbeln }/{ gw_final_tab-gjahr }|  ) TO lt_accountgl. " gw_final_tab-sgtxt
*
*        APPEND VALUE #( itemno_acc = '1'
*                        currency = 'INR'
*                        amt_doccur = gw_final_tab-netwr * l_ind * -1
*                        amt_base = gw_final_tab-netwr ) TO lt_currency."Vendor Line item
*
*        APPEND VALUE #( itemno_acc = '2'
*                        currency = 'INR'
*                        amt_doccur = gw_final_tab-netwr * l_ind
*                        amt_base =  gw_final_tab-netwr
*                      ) TO lt_currency .
*        ls_header-doc_type = 'WI'.
*        CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
*          EXPORTING
*            documentheader = ls_header
*          IMPORTING
*            obj_type       = lv_objtyp
*            obj_key        = lv_objkey
*            obj_sys        = lv_objsys
*          TABLES
*            accountgl      = lt_accountgl
*            currencyamount = lt_currency
*            return         = return.
*        IF sy-subrc = 0.
*          COMMIT WORK AND WAIT.
*          gw_final_tab-belnr3 = lv_objkey(10).
*        ENDIF.
*        CLEAR lv_msg.
*        lv_msg = gw_final_tab-remarks .
*        LOOP AT return INTO l_ret WHERE type = 'E' OR type = 'A'.
*          lv_msg = |{ lv_msg } { l_ret-message }|.
*        ENDLOOP.
*        IF sy-subrc = 0.
*          gw_final_tab-remarks = lv_msg.
*          gw_final_tab-type = 'E'.
*        ENDIF.
*      ENDIF.
    ELSE.
      gw_final_tab-type = 'E'.
      gw_final_tab-remarks = lv_msg.
    ENDIF.
    MOVE-CORRESPONDING gw_final_tab TO lw_zdms_accpost_log.
    lw_zdms_accpost_log-cdate  = sy-datum.
    lw_zdms_accpost_log-ctime   = sy-uzeit.
    lw_zdms_accpost_log-cname  = sy-uname.
    lw_zdms_accpost_log-bukrs  = 'DMS1'.
    MODIFY zdms_accpost_log FROM lw_zdms_accpost_log.
  ENDMETHOD.
  METHOD build_alv.
    DATA: lr_columns    TYPE REF TO cl_salv_columns_table.

    CALL METHOD cl_salv_table=>factory
*        EXPORTING
*        r_container  = cl_gui_container=>default_screen "go_alv_custom " <== type ref to cl_gui_container
      EXPORTING
        list_display = if_salv_c_bool_sap=>false
      IMPORTING
        r_salv_table = gr_alv
      CHANGING
        t_table      = gt_final_tab. "lt_final.

    DATA  "gr_salv_func TYPE REF TO cl_salv_functions_list.
          lr_functions TYPE REF TO cl_salv_functions.



    gr_alv->set_screen_status(
*       pfstatus      =  'STANDARD'
       pfstatus      =  'PF_DMS'
       report        =  sy-repid
       set_functions = gr_alv->c_functions_all ).

*    lr_functions = gr_alv->get_functions( ).
*    lr_functions->set_all( abap_true ).
*
*    INCLUDE <icon>.
*    TRY.
*        lr_functions->add_function(
*          name     = 'POST'
*          icon     = CONV string( icon_system_save )
*          text     = `Post Document`
*          tooltip  = `Post Document`
*          position = if_salv_c_function_position=>right_of_salv_functions ).
*      CATCH cx_salv_existing cx_salv_wrong_call.
*    ENDTRY.

* Register events

*... §5 object for handling the events of cl_salv_table

    DATA: lr_events  TYPE REF TO cl_salv_events_table.
    lr_events  = gr_alv->get_event( ).

    SET HANDLER me->on_user_command FOR lr_events.

    lr_columns = gr_alv->get_columns( ).
    lr_columns->set_optimize( 'X' ).
    DATA not_found TYPE REF TO cx_salv_not_found.
    TRY.
        DATA(lr_column) = lr_columns->get_column( 'KSCHL' ).
        lr_column->set_visible( abap_false ).
        lr_column = lr_columns->get_column( 'KWERT' ).
        lr_column->set_visible( abap_false ).
        lr_column = lr_columns->get_column( 'JOSG' ).
        lr_column->set_short_text( 'Tax-SGST' ).
        lr_column = lr_columns->get_column( 'JOCG' ).
        lr_column->set_short_text( 'Tax-CGST' ).
        lr_column = lr_columns->get_column( 'JOIG' ).
        lr_column->set_short_text( 'Tax-IGST' ).
        lr_column = lr_columns->get_column( 'JTC1' ).
        lr_column->set_short_text( 'Tax-TCS' ).
        lr_column = lr_columns->get_column( 'BLART2' ).
        lr_column->set_medium_text( 'New Doc Type' ).
        lr_column = lr_columns->get_column( 'BELNR2' ).
        lr_column->set_medium_text( 'New Doc No' ).
        lr_column = lr_columns->get_column( 'BELNR3' ).
        lr_column->set_short_text( 'WI Doc/No' ).
        lr_column->set_medium_text( 'WI Doc/No' ).
        lr_column = lr_columns->get_column( 'TYPE' ).
        lr_column->set_medium_text( 'Message Type' ).
        lr_column = lr_columns->get_column( 'REMARKS' ).
        lr_column->set_medium_text( 'Message' ).
        lr_column->set_long_text( 'Message' ).
        lr_column->set_short_text( 'Message' ).
*
*        lr_column = lr_columns->get_column( 'CHECK' ).
*        lr_column->set_short_text( 'Select' ).
*        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*        lr_column->set_icon( if_salv_c_bool_sap=>true ).
**        lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
      CATCH cx_salv_not_found INTO not_found.
        " error handling
    ENDTRY.

* Column selection
    gr_selections = gr_alv->get_selections( ).
    gr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* display ALV
    gr_alv->display( ).


  ENDMETHOD.
ENDCLASS.

DATA: gr_main_cls TYPE REF TO lcl_handle_events.
