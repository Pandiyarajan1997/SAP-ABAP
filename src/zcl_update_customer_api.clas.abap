CLASS zcl_update_customer_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_extension.
    TYPES:BEGIN OF ty_location,
            latitude  TYPE zhrlatit,
            longitude TYPE zhrlong,
          END OF ty_location.
    TYPES:BEGIN OF ty_input,
            Customer   TYPE kunnr,
            location   TYPE ty_location,
            Rol_days   TYPE zdays,
            Attribute2 TYPE katr2,
            Capacity   TYPE zcapacity,
          END OF ty_input.
    TYPES:BEGIN OF ty_resp,
            type TYPE bapiret2-type,
            msg  TYPE bapiret2-message,
          END OF ty_resp.

    METHODS: update_customer IMPORTING ls_kna1 TYPE kna1
                             EXPORTING ls_resp TYPE ty_resp.
    CLASS-METHODS: validation IMPORTING ls_input TYPE ty_input
                              EXPORTING ls_resp  TYPE ty_resp.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_update_customer_api IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    DATA:gt_input TYPE STANDARD TABLE OF ty_input,
         gt_resp  TYPE STANDARD TABLE OF ty_resp,
         gs_kna1  TYPE kna1.
**********normal api log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD Server->request->get_cdata RECEIVING data = DATA(lv_json_input).
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_json_input
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
*     assoc_arrays = abap_true
    CHANGING
     data         = gt_input ).

    LOOP AT gt_input INTO DATA(gs_input).
      gs_input-customer = |{ gs_input-customer ALPHA = IN  }|.

      CALL METHOD validation(
        EXPORTING
          ls_input = gs_input
        IMPORTING
          ls_resp  = DATA(gs_resp)
      ).

      IF gs_resp IS  INITIAL.
        gs_kna1-kunnr = gs_input-customer.
        gs_kna1-zlatitude = gs_input-location-latitude.
        gs_kna1-zlongitude = gs_input-location-longitude.
        gs_kna1-zdays = gs_input-rol_days.
        gs_kna1-katr2 = gs_input-attribute2.
        gs_kna1-zcapacity = gs_input-capacity.

        CALL METHOD update_customer
          EXPORTING
            ls_kna1 = gs_kna1
          IMPORTING
            ls_resp = gs_resp.
      ENDIF.
      APPEND gs_resp TO gt_resp.
      CLEAR gs_resp.
    ENDLOOP.

** serialize the output for response ***
    /ui2/cl_json=>serialize(
    EXPORTING
     data         =  gt_resp
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    RECEIVING
     r_json         = DATA(lv_body) ).

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'CUSTOMER_UPDATE'
        ijson           = lv_json_input
        ojson           = lv_body
*       distributor     =
*       retailer        = conv #( COND #( WHEN ls_input-account_type = 'K' then 'Vendor' ELSE ' ') )
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc = 0.

    ENDIF.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = lv_body ).

  ENDMETHOD.

  METHOD update_customer.
    DATA: iv_cust_extern   TYPE cmds_ei_extern,
          ls_header        TYPE cmds_ei_header,
          ls_data          TYPE cmds_ei_vmd_central_data,
          cust_central     TYPE cmds_ei_cmd_central,
          cust_centrl_data TYPE cmds_ei_central_data,
          is_partner      TYPE bus_ei_extern.

****Business partner data   ****
    Select SINGLE from but000 FIELDS partner_guid WHERE partner = @ls_kna1-kunnr INTO @data(lv_guid).
    is_partner = VALUE #( header = VALUE #(  object_instance = VALUE #( bpartner  = ls_kna1-kunnr bpartnerguid = lv_guid  )  object_task  = 'U' ) ).
****Header data****
    ls_header = VALUE #( object_instance = VALUE #( kunnr = ls_kna1-kunnr ) object_task     = 'U' ).

****Central data****
    cust_central = VALUE #(
        data  = CORRESPONDING #( BASE ( ls_data ) ls_kna1  )
        datax = VALUE #(
        aufsd              = COND #( WHEN ls_kna1-aufsd                IS NOT INITIAL THEN abap_true ELSE abap_false )
        bahne              = COND #( WHEN ls_kna1-bahne                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bahns              = COND #( WHEN ls_kna1-bahns                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bbbnr              = COND #( WHEN ls_kna1-bbbnr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bbsnr              = COND #( WHEN ls_kna1-bbsnr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        begru              = COND #( WHEN ls_kna1-begru                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        brsch              = COND #( WHEN ls_kna1-brsch                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bubkz              = COND #( WHEN ls_kna1-bubkz                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        faksd              = COND #( WHEN ls_kna1-faksd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        fiskn              = COND #( WHEN ls_kna1-fiskn                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        knrza              = COND #( WHEN ls_kna1-knrza                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        konzs              = COND #( WHEN ls_kna1-konzs                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ktokd              = COND #( WHEN ls_kna1-ktokd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kukla              = COND #( WHEN ls_kna1-kukla                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        lifnr              = COND #( WHEN ls_kna1-lifnr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        lifsd              = COND #( WHEN ls_kna1-lifsd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        locco              = COND #( WHEN ls_kna1-locco                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        loevm              = COND #( WHEN ls_kna1-loevm                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        niels              = COND #( WHEN ls_kna1-niels                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        counc              = COND #( WHEN ls_kna1-counc                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        cityc              = COND #( WHEN ls_kna1-cityc                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        rpmkr              = COND #( WHEN ls_kna1-rpmkr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        sperr              = COND #( WHEN ls_kna1-sperr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd1              = COND #( WHEN ls_kna1-stcd1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd2              = COND #( WHEN ls_kna1-stcd2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stkza              = COND #( WHEN ls_kna1-stkza                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stkzu              = COND #( WHEN ls_kna1-stkzu                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xzemp              = COND #( WHEN ls_kna1-xzemp                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        vbund              = COND #( WHEN ls_kna1-vbund                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stceg              = COND #( WHEN ls_kna1-stceg                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        gform              = COND #( WHEN ls_kna1-gform                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran1              = COND #( WHEN ls_kna1-bran1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran2              = COND #( WHEN ls_kna1-bran2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran3              = COND #( WHEN ls_kna1-bran3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran4              = COND #( WHEN ls_kna1-bran4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        bran5              = COND #( WHEN ls_kna1-bran5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        umjah              = COND #( WHEN ls_kna1-umjah                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        uwaer              = COND #( WHEN ls_kna1-uwaer                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        jmzah              = COND #( WHEN ls_kna1-jmzah                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        jmjah              = COND #( WHEN ls_kna1-jmjah                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr1              = COND #( WHEN ls_kna1-katr1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr2              = COND #( WHEN ls_kna1-katr2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr3              = COND #( WHEN ls_kna1-katr3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr4              = COND #( WHEN ls_kna1-katr4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr5              = COND #( WHEN ls_kna1-katr5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr6              = COND #( WHEN ls_kna1-katr6                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr7              = COND #( WHEN ls_kna1-katr7                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr8              = COND #( WHEN ls_kna1-katr8                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr9              = COND #( WHEN ls_kna1-katr9                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        katr10             = COND #( WHEN ls_kna1-katr10                IS NOT INITIAL THEN abap_true ELSE abap_false )
        stkzn              = COND #( WHEN ls_kna1-stkzn                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        umsa1              = COND #( WHEN ls_kna1-umsa1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        periv              = COND #( WHEN ls_kna1-periv                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ktocd              = COND #( WHEN ls_kna1-ktocd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dtams              = COND #( WHEN ls_kna1-dtams                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dtaws              = COND #( WHEN ls_kna1-dtaws                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        hzuor              = COND #( WHEN ls_kna1-hzuor                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        civve              = COND #( WHEN ls_kna1-civve                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        milve              = COND #( WHEN ls_kna1-milve                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        fityp              = COND #( WHEN ls_kna1-fityp                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcdt              = COND #( WHEN ls_kna1-stcdt                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd3              = COND #( WHEN ls_kna1-stcd3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd4              = COND #( WHEN ls_kna1-stcd4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xicms              = COND #( WHEN ls_kna1-xicms                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xxipi              = COND #( WHEN ls_kna1-xxipi                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xsubt              = COND #( WHEN ls_kna1-xsubt                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        cfopc              = COND #( WHEN ls_kna1-cfopc                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        txlw1              = COND #( WHEN ls_kna1-txlw1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        txlw2              = COND #( WHEN ls_kna1-txlw2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ccc01              = COND #( WHEN ls_kna1-ccc01                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ccc02              = COND #( WHEN ls_kna1-ccc02                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ccc03              = COND #( WHEN ls_kna1-ccc03                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        ccc04              = COND #( WHEN ls_kna1-ccc04                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        cassd              = COND #( WHEN ls_kna1-cassd                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg1              = COND #( WHEN ls_kna1-kdkg1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg2              = COND #( WHEN ls_kna1-kdkg2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg3              = COND #( WHEN ls_kna1-kdkg3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg4              = COND #( WHEN ls_kna1-kdkg4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        kdkg5              = COND #( WHEN ls_kna1-kdkg5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        nodel              = COND #( WHEN ls_kna1-nodel                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1kfrepre         = COND #( WHEN ls_kna1-j_1kfrepre            IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1kftbus          = COND #( WHEN ls_kna1-j_1kftbus             IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1kftind          = COND #( WHEN ls_kna1-j_1kftind             IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd5              = COND #( WHEN ls_kna1-stcd5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        stcd6              = COND #( WHEN ls_kna1-stcd6                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        cvp_xblck          = COND #( WHEN ls_kna1-cvp_xblck             IS NOT INITIAL THEN abap_true ELSE abap_false )
        suframa            = COND #( WHEN ls_kna1-suframa               IS NOT INITIAL THEN abap_true ELSE abap_false )
        rg                 = COND #( WHEN ls_kna1-rg                    IS NOT INITIAL THEN abap_true ELSE abap_false )
        exp                = COND #( WHEN ls_kna1-exp                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        uf                 = COND #( WHEN ls_kna1-uf                    IS NOT INITIAL THEN abap_true ELSE abap_false )
        rgdate             = COND #( WHEN ls_kna1-rgdate                IS NOT INITIAL THEN abap_true ELSE abap_false )
        ric                = COND #( WHEN ls_kna1-ric                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        rne                = COND #( WHEN ls_kna1-rne                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        rnedate            = COND #( WHEN ls_kna1-rnedate               IS NOT INITIAL THEN abap_true ELSE abap_false )
        cnae               = COND #( WHEN ls_kna1-cnae                  IS NOT INITIAL THEN abap_true ELSE abap_false )
        legalnat           = COND #( WHEN ls_kna1-legalnat              IS NOT INITIAL THEN abap_true ELSE abap_false )
        crtn               = COND #( WHEN ls_kna1-crtn                  IS NOT INITIAL THEN abap_true ELSE abap_false )
        icmstaxpay         = COND #( WHEN ls_kna1-icmstaxpay            IS NOT INITIAL THEN abap_true ELSE abap_false )
        indtyp             = COND #( WHEN ls_kna1-indtyp                IS NOT INITIAL THEN abap_true ELSE abap_false )
        tdt                = COND #( WHEN ls_kna1-tdt                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        comsize            = COND #( WHEN ls_kna1-comsize               IS NOT INITIAL THEN abap_true ELSE abap_false )
        decregpc           = COND #( WHEN ls_kna1-decregpc              IS NOT INITIAL THEN abap_true ELSE abap_false )
        ort02              = COND #( WHEN ls_kna1-ort02                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear6              = COND #( WHEN ls_kna1-dear6                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        lzone              = COND #( WHEN ls_kna1-lzone                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        xknza              = COND #( WHEN ls_kna1-xknza                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear1              = COND #( WHEN ls_kna1-dear1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear2              = COND #( WHEN ls_kna1-dear2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear3              = COND #( WHEN ls_kna1-dear3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        dear5              = COND #( WHEN ls_kna1-dear5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        werks              = COND #( WHEN ls_kna1-werks                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        adrnr              = COND #( WHEN ls_kna1-adrnr                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        delivery_date_rule = COND #( WHEN ls_kna1-delivery_date_rule    IS NOT INITIAL THEN abap_true ELSE abap_false )
        kna1_eew_cust      = COND #( WHEN ls_kna1-kna1_eew_cust         IS NOT INITIAL THEN abap_true ELSE abap_false )
        rule_exclusion     = COND #( WHEN ls_kna1-rule_exclusion        IS NOT INITIAL THEN abap_true ELSE abap_false )
        alc                = COND #( WHEN ls_kna1-alc                   IS NOT INITIAL THEN abap_true ELSE abap_false )
        pmt_office         = COND #( WHEN ls_kna1-pmt_office            IS NOT INITIAL THEN abap_true ELSE abap_false )
        fee_schedule       = COND #( WHEN ls_kna1-fee_schedule          IS NOT INITIAL THEN abap_true ELSE abap_false )
        duns               = COND #( WHEN ls_kna1-duns                  IS NOT INITIAL THEN abap_true ELSE abap_false )
        duns4              = COND #( WHEN ls_kna1-duns4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psofg              = COND #( WHEN ls_kna1-psofg                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psois              = COND #( WHEN ls_kna1-psois                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        pson1              = COND #( WHEN ls_kna1-pson1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        pson2              = COND #( WHEN ls_kna1-pson2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        pson3              = COND #( WHEN ls_kna1-pson3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psovn              = COND #( WHEN ls_kna1-psovn                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psotl              = COND #( WHEN ls_kna1-psotl                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psohs              = COND #( WHEN ls_kna1-psohs                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psost              = COND #( WHEN ls_kna1-psost                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo1              = COND #( WHEN ls_kna1-psoo1                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo2              = COND #( WHEN ls_kna1-psoo2                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo3              = COND #( WHEN ls_kna1-psoo3                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo4              = COND #( WHEN ls_kna1-psoo4                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        psoo5              = COND #( WHEN ls_kna1-psoo5                 IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexcd           = COND #( WHEN ls_kna1-j_1iexcd              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexrn           = COND #( WHEN ls_kna1-j_1iexrn              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexrg           = COND #( WHEN ls_kna1-j_1iexrg              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexdi           = COND #( WHEN ls_kna1-j_1iexdi              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexco           = COND #( WHEN ls_kna1-j_1iexco              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1icstno          = COND #( WHEN ls_kna1-j_1icstno             IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1ilstno          = COND #( WHEN ls_kna1-j_1ilstno             IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1ipanno          = COND #( WHEN ls_kna1-j_1ipanno             IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1iexcicu         = COND #( WHEN ls_kna1-j_1iexcicu            IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1isern           = COND #( WHEN ls_kna1-j_1isern              IS NOT INITIAL THEN abap_true ELSE abap_false )
        j_1ipanref         = COND #( WHEN ls_kna1-j_1ipanref            IS NOT INITIAL THEN abap_true ELSE abap_false )
        zkostl             = COND #( WHEN ls_kna1-zkostl                IS NOT INITIAL THEN abap_true ELSE abap_false )
        zcapacity          = COND #( WHEN ls_kna1-zcapacity             IS NOT INITIAL THEN abap_true ELSE abap_false )
        zlatitude          = COND #( WHEN ls_kna1-zlatitude             IS NOT INITIAL THEN abap_true ELSE abap_false )
        zlongitude         = COND #( WHEN ls_kna1-zlongitude            IS NOT INITIAL THEN abap_true ELSE abap_false )
        zfincode           = COND #( WHEN ls_kna1-zfincode              IS NOT INITIAL THEN abap_true ELSE abap_false )
        zcustype           = COND #( WHEN ls_kna1-zcustype              IS NOT INITIAL THEN abap_true ELSE abap_false )
        zdays              = COND #( WHEN ls_kna1-zdays                 IS NOT INITIAL THEN abap_true ELSE abap_false )
    ) ).

    "Update Central data to its central main structure"
    cust_centrl_data = VALUE #(  central  = cust_central ).

    iv_cust_extern = VALUE #(
        header       = ls_header
        central_data = cust_centrl_data
    ).

    ""Executing class
    cl_md_bp_maintain=>maintain(
      EXPORTING
        i_data     = VALUE #( ( partner = is_partner customer = iv_cust_extern ) )
*        i_test_run =
      IMPORTING
        e_return   = DATA(lt_return)
    ).

    IF lt_return IS NOT INITIAL.
      DATA(return_tab) = VALUE #( lt_return[ 1 ]-object_msg OPTIONAL ).
      IF line_exists( return_tab[ type = 'E' ] ) OR line_exists( return_tab[ type = 'A' ] ).
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        LOOP AT return_tab INTO DATA(is_return) WHERE type NE 'W'.
          ls_resp = VALUE #( type = 'E' msg = is_return-message ).
        ENDLOOP.
      ELSE.
*---------Commit the SAP LUW in Single DB LUW----------*
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
*-------------------------------------------------------*
        ls_resp = VALUE #( type = is_return-type msg = is_return-message ).
      ENDIF.
      ELSE.
*---------Commit the SAP LUW in Single DB LUW----------*
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
   ls_resp = VALUE #( type = 'S' msg = |Customer { ls_kna1-kunnr } is Updated| ).
*-------------------------------------------------------*
    ENDIF.

  ENDMETHOD.

  METHOD validation.
    SELECT SINGLE FROM zcust_blk_chk FIELDS block WHERE kunnr = @ls_input-customer INTO @DATA(ls_block).
    IF sy-subrc = 0.
      IF ls_block IS NOT INITIAL.
        ls_resp = VALUE #( type = 'E' msg = |Customer { ls_input-customer } is blocked| ).
      ENDIF.
    ELSE.
      ls_resp = VALUE #( type = 'E' msg = |Customer { ls_input-customer } is Incorrect| ).
    ENDIF.

    SELECT SINGLE FROM tvk2 FIELDS katr2 WHERE katr2 = @ls_input-attribute2 INTO @DATA(ls_attr2).
    IF sy-subrc <> 0.
      ls_resp = VALUE #( type = 'E' msg = |Attribute 2{ ls_input-attribute2 } is Incorrect| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
