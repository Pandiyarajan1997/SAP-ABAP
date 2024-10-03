class ZCL_API_VEN_QAL_REG definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_QAL_REG IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: date_frm TYPE string,
          lifnr    TYPE string,
          date_to  TYPE string.
    DATA: lv_path_info      TYPE string,
          lv_request_method TYPE string,
          lv_response_data  TYPE string.
    DATA: it_inputparams TYPE tihttpnvp,
          ls_inputparams TYPE LINE OF tihttpnvp.

    TYPES: BEGIN OF ty_lfa1,
             lifnr TYPE lfa1-lifnr,
             name1 TYPE lfa1-name1,
           END OF ty_lfa1.

    DATA: wa_lfa1 TYPE ty_lfa1,
          it_lfa1 TYPE TABLE OF ty_lfa1.

    TYPES: BEGIN OF ty_makt,
             matnr TYPE makt-matnr,
             maktx TYPE makt-maktx,
           END OF ty_makt.

    DATA: wa_makt TYPE ty_makt,
          it_makt TYPE TABLE OF ty_makt.

    TYPES: BEGIN OF ty_t001w,
             werks TYPE t001w-werks,
             name1 TYPE t001w-name1,
           END OF ty_t001w.

    DATA: wa_t001w TYPE ty_t001w,
          it_t001w TYPE TABLE OF ty_t001w.


    TYPES: BEGIN OF ty_mseg,
             lifnr TYPE mseg-lifnr,
             matnr TYPE mseg-matnr,
             werks TYPE mseg-werks,
             bwart TYPE mseg-bwart,
             grund TYPE mseg-grund,
             mblnr TYPE mseg-mblnr,
             zeile TYPE mseg-zeile,
             mjahr TYPE mseg-mjahr,
             budat TYPE mseg-budat_mkpf,
             ebeln TYPE mseg-ebeln,
             ebelp TYPE mseg-ebelp,
             charg TYPE mseg-charg,
             menge TYPE mseg-menge,
             meins TYPE mseg-meins,
             tcode TYPE mseg-tcode2_mkpf,
           END OF ty_mseg.


    DATA: wa_mseg  TYPE ty_mseg,
          it_mseg  TYPE TABLE OF ty_mseg,
          wa_mseg1 TYPE ty_mseg,
          it_mseg1 TYPE TABLE OF ty_mseg.


    TYPES: BEGIN OF ty_final,
             lifnr    TYPE mseg-lifnr,
             name     TYPE lfa1-name1,
             matnr    TYPE mseg-matnr,
             maktx    TYPE makt-maktx,
             werks    TYPE mseg-werks,
             name1    TYPE t001w-name1,
             bwart    TYPE mseg-bwart,
             grund    TYPE mseg-grund,
             mblnr    TYPE mseg-mblnr,
             mjahr    TYPE mseg-mjahr,
             budat    TYPE mseg-budat_mkpf,
             ebeln    TYPE mseg-ebeln,
             ebelp    TYPE mseg-ebelp,
             prueflos TYPE qamb-prueflos,
             charg    TYPE mseg-charg,
             licha    TYPE mch1-licha,
             xblnr    TYPE mkpf-xblnr,
             menge    TYPE mseg-menge,
             meins    TYPE mseg-meins,
             menge1   TYPE mseg-menge,
             meins1   TYPE mseg-meins,
             grtxt    TYPE  t157e-grtxt,
             categ    TYPE char30,
             art      TYPE qals-art,
             zeile    TYPE mseg-zeile,
           END OF ty_final.
    TYPES : BEGIN OF TY_output,
              lifnr    TYPE  lifnr,
              name     TYPE  name1_gp,
              matnr    TYPE  matnr,
              maktx    TYPE  maktx,
              prueflos TYPE  qplos,
              menge    TYPE  string,
              meins    TYPE  meins,
              ebeln    TYPE  ebeln,
              ebelp    TYPE  ebelp,
              budat    TYPE  budat,
              menge1   TYPE  menge_d,
              meins1   TYPE  meins,
              grtxt    TYPE  grtxt,
              charg    TYPE string,
              art      TYPE qpart,
              zeile    TYPE string,
              mblnr    TYPE mblnr,
            END OF ty_output.

    DATA: wa_final  TYPE ty_final,
          it_final  TYPE TABLE OF ty_final,
          wa_output TYPE ty_output,
          it_output TYPE TABLE OF ty_output.


    TYPES : BEGIN OF ty_t157e,
              bwart TYPE t157e-bwart,
              grund TYPE t157e-grund,
              grtxt TYPE t157e-grtxt,
            END OF ty_t157e.

    DATA: wa_t157e TYPE ty_t157e,
          it_t157e TYPE TABLE OF ty_t157e.

    TYPES: BEGIN OF ty_qamb,
             prueflos TYPE qamb-prueflos,
             mblnr    TYPE qamb-mblnr,
             mjahr    TYPE qamb-mjahr,
             zeile    TYPE qamb-zeile,
           END OF ty_qamb.
    TYPES: BEGIN OF ty_qals,
             mblnr TYPE qals-mblnr,
             mjahr TYPE qals-mjahr,
             zeile TYPE qals-zeile,
             art   TYPE qals-art,
             charg TYPE qals-charg,
           END OF ty_qals.

    DATA: wa_qamb1 TYPE ty_qamb,
          wa_qamb2 TYPE ty_qamb,
          wa_qals  TYPE ty_qals,
          it_qamb1 TYPE TABLE OF ty_qamb,
          it_qamb2 TYPE TABLE OF ty_qamb,
          it_qals  TYPE TABLE OF ty_qals.


    TYPES: BEGIN OF ty_new,
             menge TYPE mseg-menge,
             meins TYPE mseg-meins,
           END OF ty_new.

    DATA: wa_new TYPE ty_new,
          it_new TYPE TABLE OF ty_new.

    TYPES: BEGIN OF ty_mch1,
             lifnr TYPE mch1-lifnr,
             matnr TYPE mch1-matnr,
             charg TYPE mch1-charg,
             licha TYPE mch1-licha,
           END OF ty_mch1.

    DATA : wa_mch1 TYPE ty_mch1,
           it_mch1 TYPE TABLE OF ty_mch1.

    TYPES: BEGIN OF ty_mkpf,
             mblnr TYPE mkpf-mblnr,
             mjahr TYPE mkpf-mjahr,
             xblnr TYPE mkpf-xblnr,
           END OF ty_mkpf.

    DATA: wa_mkpf TYPE ty_mkpf,
          it_mkpf TYPE TABLE OF ty_mkpf.


    TYPES: BEGIN OF ty_qmel,
             prueflos TYPE qmel-prueflos,
           END OF ty_qmel.

    DATA: wa_qmel TYPE ty_qmel,
          it_qmel TYPE TABLE OF ty_qmel.

    TYPES : BEGIN OF t_body,
              qalitem TYPE string,
            END OF t_body.
    DATA: it_qal_body TYPE STANDARD TABLE OF t_body,
          wa_qal_body TYPE t_body.

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
    READ TABLE it_inputparams INTO ls_inputparams WITH KEY name = 'LIFNR'.
    lifnr = ls_inputparams-value.
    CLEAR ls_inputparams.

    IF lifnr IS NOT INITIAL.

      SELECT lifnr
             matnr
             werks
             bwart
             grund
             mblnr
             zeile
             mjahr
             budat_mkpf
             ebeln
             ebelp
             charg
             menge
             meins
             tcode2_mkpf FROM mseg INTO TABLE it_mseg WHERE  lifnr = lifnr AND
                                                             budat_mkpf GE date_frm AND budat_mkpf LE date_to
                                                             AND bwart = '101' AND tcode2_mkpf = 'MIGO_GR'.
    ELSE.

      SELECT lifnr
           matnr
           werks
           bwart
           grund
           mblnr
           zeile
           mjahr
           budat_mkpf
           ebeln
           ebelp
           charg
           menge
           meins
           tcode2_mkpf FROM mseg INTO TABLE it_mseg WHERE  budat_mkpf GE date_frm AND budat_mkpf LE date_to AND
                                                           bwart = '101' AND tcode2_mkpf = 'MIGO_GR'.

    ENDIF.


    IF it_mseg IS NOT INITIAL.


      SELECT prueflos
             mblnr
             mjahr
             zeile FROM qamb INTO TABLE it_qamb1
                   FOR ALL ENTRIES IN it_mseg
                   WHERE mblnr = it_mseg-mblnr AND
                         mjahr = it_mseg-mjahr AND
                         zeile = it_mseg-zeile.


      SELECT prueflos
           mblnr
           mjahr
           zeile FROM qamb INTO TABLE it_qamb2
                 FOR ALL ENTRIES IN it_qamb1
                 WHERE prueflos = it_qamb1-prueflos.

      SELECT prueflos FROM qmel INTO TABLE it_qmel
                      FOR ALL ENTRIES IN it_qamb2
                       WHERE prueflos = it_qamb2-prueflos AND
                             qmart = 'F2'.

      SELECT mblnr
             mjahr
             zeile
             art charg FROM qals INTO TABLE it_QALS FOR ALL ENTRIES IN it_mseg
      WHERE mblnr = it_mseg-mblnr AND
                       mjahr = it_mseg-mjahr AND
                       zeile = it_mseg-zeile.

      SELECT lifnr
         matnr
         werks
         bwart
         grund
         mblnr
         zeile
         mjahr
         budat_mkpf
         ebeln
         ebelp
         charg
         menge
         meins
         tcode2_mkpf FROM mseg INTO TABLE it_mseg1 FOR ALL ENTRIES IN it_qamb2
                                                   WHERE  mblnr = it_qamb2-mblnr AND
                                                          mjahr = it_qamb2-mjahr AND
                                                          zeile = it_qamb2-zeile AND
                                                          bwart = '350'.

      DATA i TYPE i.

      LOOP AT  it_qamb2 INTO wa_qamb2.

        READ TABLE it_mseg INTO wa_mseg WITH KEY mblnr = wa_qamb2-mblnr
                                                 mjahr = wa_qamb2-mjahr
                                                 zeile = wa_qamb2-zeile
                                                 bwart = '101'.
        READ TABLE it_qals INTO wa_qals WITH KEY mblnr = wa_qamb2-mblnr
                                                 mjahr = wa_qamb2-mjahr
                                                 zeile = wa_qamb2-zeile.

        IF sy-subrc = 0.
          i = i + 1.
          wa_final-prueflos = wa_qamb2-prueflos.
          wa_final-lifnr = wa_mseg-lifnr.

*        SHIFT WA_MSEG-MATNR LEFT DELETING LEADING '0'.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input  = wa_mseg-matnr
            IMPORTING
              output = wa_final-matnr
* EXCEPTIONS
*             LENGTH_ERROR       = 1
*             OTHERS = 2
            .
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.



*        WA_FINAL-MATNR = WA_MSEG-MATNR.
          wa_final-werks = wa_mseg-werks.
          wa_final-budat = wa_mseg-budat.
          wa_final-mblnr = wa_mseg-mblnr.
          wa_final-mjahr = wa_mseg-mjahr.
          wa_final-ebeln = wa_mseg-ebeln.

          wa_final-ebelp = wa_mseg-ebelp.

          wa_final-charg = wa_mseg-charg.

          wa_final-menge = wa_mseg-menge.

          wa_final-meins = wa_mseg-meins.

          wa_final-art = wa_qals-art.
          wa_final-zeile = wa_mseg-zeile.



        ENDIF.

        READ TABLE it_mseg1 INTO wa_mseg1 WITH KEY mblnr = wa_qamb2-mblnr
                                                 mjahr = wa_qamb2-mjahr
                                                 zeile = wa_qamb2-zeile
                                                 bwart = '350'.

        IF sy-subrc = 0.

          i = i + 1.
          wa_final-menge1 = wa_mseg1-menge.

          wa_final-meins1 = wa_mseg1-meins.
          wa_final-bwart = wa_mseg1-bwart.
          wa_final-grund = wa_mseg1-grund.

          READ TABLE it_qmel INTO wa_qmel WITH KEY prueflos = wa_final-prueflos.

          IF sy-subrc = 0.

            wa_final-categ = 'Online Rejection'.

          ELSE.

            wa_final-categ = 'GRN Rejection'.

          ENDIF.
        ENDIF.

        AT END OF prueflos. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation

          IF i = 2.

            APPEND wa_final TO it_final.
            CLEAR wa_final.

          ELSE.

            CLEAR i.

          ENDIF.
        ENDAT.

        CLEAR: wa_qamb2, wa_mseg1, wa_mseg.

      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM it_final COMPARING ALL FIELDS.
      DELETE it_final WHERE menge = 0 .
      IF it_final IS NOT INITIAL.


        SELECT lifnr name1
               FROM lfa1 INTO TABLE it_lfa1
               FOR ALL ENTRIES IN it_final
               WHERE lifnr = it_final-lifnr.


        SELECT matnr maktx FROM makt
                           INTO TABLE it_makt
                           FOR ALL ENTRIES IN it_final
                           WHERE matnr = it_final-matnr.


        SELECT werks
               name1 FROM t001w INTO TABLE it_t001w
                     FOR ALL ENTRIES IN it_final
                     WHERE werks = it_final-werks.

        SELECT bwart
               grund
               grtxt FROM t157e INTO TABLE it_t157e
                     FOR ALL ENTRIES IN it_final
                     WHERE bwart = it_final-bwart
                     AND   grund = it_final-grund.

        SELECT lifnr
               matnr
               charg
               licha FROM mch1 INTO TABLE it_mch1
                     FOR ALL ENTRIES IN it_final
                     WHERE lifnr = it_final-lifnr AND
                           matnr = it_final-matnr AND
                           charg = it_final-charg.

        SELECT mblnr
               mjahr
               xblnr FROM mkpf INTO TABLE it_mkpf
                     FOR ALL ENTRIES IN it_final
                     WHERE mblnr = it_final-mblnr AND
                           mjahr = it_final-mjahr.



        LOOP AT it_final INTO wa_final.

          READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final-lifnr.

          wa_final-name = wa_lfa1-name1.

          READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_final-matnr.

          wa_final-maktx = wa_makt-maktx.

          READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_final-werks.

          wa_final-name1 = wa_t001w-name1.

          READ TABLE it_t157e INTO wa_t157e WITH KEY bwart = wa_final-bwart
                                                     grund = wa_final-grund.

          TRANSLATE wa_t157e-grtxt TO UPPER CASE.

          wa_final-grtxt = wa_t157e-grtxt.

          READ TABLE it_mch1 INTO wa_mch1 WITH KEY lifnr = wa_final-lifnr
                                                   matnr = wa_final-matnr
                                                   charg = wa_final-charg.

          wa_final-licha = wa_mch1-licha.

          READ TABLE it_mkpf INTO wa_mkpf WITH KEY mblnr = wa_final-mblnr
                                                   mjahr = wa_final-mjahr.

          wa_final-xblnr = wa_mkpf-xblnr.


          MODIFY it_final FROM wa_final.
          CLEAR: wa_final, wa_lfa1 , wa_makt, wa_t001w, wa_t157e.

        ENDLOOP.

        SORT it_final BY budat ebeln.

        LOOP AT it_final INTO wa_final.

          MOVE-CORRESPONDING wa_final TO WA_output.

          IF wa_final-grtxt IS INITIAL.
            wa_output-grtxt = wa_final-grtxt.
          ENDIF.
          APPEND wa_output TO it_output.
          CLEAR :  wa_output , wa_final.

        ENDLOOP.

      ENDIF.

    ENDIF.

    DELETE ADJACENT DUPLICATES FROM it_output COMPARING ALL FIELDS.

    IF it_output IS NOT INITIAL.
      CLEAR :wa_output.
      LOOP AT it_output INTO wa_output.
        CONCATENATE
        '{'
        '"grn_number":'           c_quatation_mark wa_output-mblnr c_quatation_mark c_comma "ebeln
        '"grpo_line_item":'       c_quatation_mark wa_output-zeile c_quatation_mark c_comma "wa_output-ebelp
        '"inspection_type" :'     c_quatation_mark wa_output-art c_quatation_mark c_comma
        '"material_code":'        c_quatation_mark wa_output-matnr c_quatation_mark c_comma
        '"rejected_quantity":'    c_quatation_mark wa_output-menge c_quatation_mark c_comma
        '"uom":'                  c_quatation_mark wa_output-meins c_quatation_mark c_comma
        '"rejected_reason":'      c_quatation_mark wa_output-grtxt  c_quatation_mark c_comma
        '"inspection_batch_no":'  c_quatation_mark wa_output-charg c_quatation_mark c_comma
        '"inspection_date":'      c_quatation_mark wa_output-budat  c_quatation_mark c_comma
        '"entry_date":'           c_quatation_mark  wa_output-budat c_quatation_mark "c_comma
        '}'
        INTO wa_qal_body-qalitem.
        APPEND wa_qal_body TO it_qal_body.
        CLEAR : wa_qal_body,wa_output.
      ENDLOOP.
    ENDIF.

    DATA: lv_tabix  TYPE sy-tabix,
          lv_int(6) TYPE n.
    lv_int = 0.
    lv_int = lines( it_qal_body ).

    LOOP AT it_qal_body INTO wa_qal_body.
      lv_tabix = sy-tabix.
      IF lv_tabix < lv_int.
        CONCATENATE ls_json wa_qal_body-qalitem ',' INTO ls_json.
      ELSE.
        CONCATENATE ls_json wa_qal_body-qalitem INTO ls_json.
      ENDIF.
    ENDLOOP.
    CLEAR: lv_int,lv_tabix.

    CONCATENATE '[' ls_json ']'  INTO v_jsonload.

*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = v_jsonload ).

  ENDMETHOD.
ENDCLASS.
