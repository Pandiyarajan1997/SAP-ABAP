class ZCL_API_VEN_MATERIAL definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VEN_MATERIAL IMPLEMENTATION.


  method if_http_extension~handle_request.
    data: date_frm type string,
          date_to  type string.
    data: lv_path_info      type string,
          lv_request_method type string,
          lv_response_data  type string.
    data: it_inputparams type tihttpnvp,
          ls_inputparams type line of tihttpnvp.

    types:begin of ty_head,
            ersda type mara-ersda,
            matkl type mara-matkl,
            matnr type mara-matnr,
            mtart type mara-mtart,
            meins type mara-meins,
            ntgew type mara-ntgew,
            brgew type mara-brgew,
            volum type mara-volum,
            voleh type mara-voleh,
          end of ty_head.

    types : begin of gs_cdhdr,
              objectclas type cdhdr-objectclas,
              objectid   type cdhdr-objectid,
              changenr   type cdhdr-changenr,
              username   type cdhdr-username,
              udate      type cdhdr-udate,
              utime      type cdhdr-utime,
              tcode      type cdhdr-tcode,
            end of gs_cdhdr.

    types:begin of ty_mara,
            ersda type string,
            matkl type string,
            matnr type string,
            maktx type string,
            mtart type string,
            mtbez type string,
            steuc type marc-steuc,
            meins type string,
            ntgew type string,
            brgew type string,
            volum type string,
            voleh type string,
            WGBEZ60 TYPE string,
          end of ty_mara.

    types:begin of ty_marc,
            matnr type marc-matnr,
            steuc type marc-steuc,
          end of ty_marc.
    types:begin of ty_makt,
            matnr type makt-matnr,
            maktx type makt-maktx,
          end of ty_makt.
    types:begin of ty_LFA1,
            lifnr type lfa1-lifnr,
            ktokk type lfa1-ktokk,
          end of ty_LFA1.
    types:begin of ty_eina,
            lifnr type lfa1-lifnr,
            matnr type makt-matnr,
          end of ty_eina.
    types:begin of ty_t134t,
            mtart type t134t-mtart,
            mtbez type t134t-mtbez,
          end of ty_t134t.
    types:begin of ty_t023t,
            matkl  type t023t-matkl,
            text60 type t023t-WGBEZ60,
          end of ty_t023t.

    constants : c_comma                type c value ',',
                c_curly_brackets_open  type char01 value '{',
                c_curly_brackets_close type char01 value '}',
                c_quatation_mark       type char01 value '"',
                c_colon                type char01 value':'.

    data:ls_head  type table of ty_head,
         lw_head  type ty_head,
         it_mara  type table of ty_mara,
         wa_mara  type ty_mara,
         ls_marc  type table of ty_marc,
         lw_marc  type ty_marc,
         it_makt  type table of ty_makt,
         wa_makt  type ty_makt,
         it_lfa1  type table of ty_lfa1,
         wa_lfa1  type ty_lfa1,
         it_eina  type table of ty_eina,
         wa_eina  type ty_eina,
         it_t134t type table of ty_t134t,
         wa_t134t type ty_t134t,
         it_t023t type table of ty_t023t,
         wa_t023t type ty_t023t,
         lt_list  type table of bapiorders,
         ls_list  type bapiorders,
         xml_out  type string.
    data : gt_cdhdr type table of gs_cdhdr,
           wa_cdhdr type gs_cdhdr.

    data :ls_json    type string,
          v_jsonload type string.

    types : begin of t_body,
              Materialitem type string,
            end of t_body.
    data: it_mat_body type standard table of t_body,
          wa_mat_body type t_body.

* get the request attributes
    lv_path_info = server->request->get_header_field( name = '~path_info' ).
    shift lv_path_info left by 1 places.

    field-symbols <fs_param> type line of tihttpnvp.

*Get GET parameters
    call method server->request->get_form_fields
      changing
        fields = it_inputparams.
    unassign <fs_param>.
    loop at it_inputparams assigning <fs_param>.
      translate <fs_param>-name to upper case.
    endloop.

    clear: date_frm,ls_inputparams.

    read table it_inputparams into ls_inputparams with key name = 'DATE_FRM'.
    date_frm = ls_inputparams-value.
    clear ls_inputparams.
    read table it_inputparams into ls_inputparams with key name = 'DATE_TO'.
    date_to = ls_inputparams-value.
    clear ls_inputparams.
    if date_frm is not initial and date_to is not initial.
      select  objectclas objectid changenr username udate utime tcode from  cdhdr
      into table gt_cdhdr
      where objectclas = 'MATERIAL'
      and udate ge date_frm and udate le date_to
      and ( tcode = 'MM01' or  tcode = 'MM02' ).
    else.
      select  objectclas objectid changenr username udate utime tcode from  cdhdr
      into table gt_cdhdr
      where objectclas = 'MATERIAL'
      and ( tcode = 'MM01' or  tcode = 'MM02' ).
    endif.
    sort gt_cdhdr ascending by objectid.

* Get the Change data details
    if gt_cdhdr is not initial.
      select lifnr ktokk into table it_lfa1 from lfa1 where ktokk = 'YB01'.
      if it_lfa1 is not initial.
        select lifnr  matnr into table it_eina from eina for all entries in it_lfa1 where lifnr = it_lfa1-lifnr.
      endif.

      select ersda
             matkl
             matnr
             mtart
             meins
             ntgew
             brgew
             volum
             voleh
             into table ls_head from mara for all entries in gt_cdhdr where matnr = gt_cdhdr-objectid+0(40) and lvorm <> 'X'.
    endif.
* Get the Material details
    if ls_head is not initial.
      select matnr steuc into table ls_marc from marc for all entries in ls_head  where matnr = ls_head-matnr .
      select matnr
                 maktx from makt into table it_makt
                    for all entries in ls_head
                        where matnr = ls_head-matnr  and
                              spras = sy-langu.
      select matkl WGBEZ60 from t023t into table it_t023t for all entries in ls_head  where matkl = ls_head-matkl.
      select  mtart mtbez into  table it_t134t from t134t for all entries in ls_head where mtart = ls_head-mtart and spras = 'EN'.
    endif.


    sort ls_head ascending by matnr.
    sort it_makt ascending by matnr.
    sort ls_marc ascending by matnr.
    sort it_eina ascending by matnr.
    sort it_t134t ascending by mtart.

    loop at ls_head into lw_head.
      wa_mara-ersda = lw_head-ersda.
      wa_mara-matkl = lw_head-matkl.
      wa_mara-matnr = lw_head-matnr.
      wa_mara-mtart = lw_head-mtart.
      wa_mara-meins = lw_head-meins.
      wa_mara-ntgew = lw_head-ntgew.
      wa_mara-brgew = lw_head-brgew.
      wa_mara-volum = lw_head-volum.
      wa_mara-voleh = lw_head-voleh.
      read table ls_marc into lw_marc with key matnr = wa_mara-matnr binary search.
      if sy-subrc = 0.
        wa_mara-steuc = lw_marc-steuc.
      endif.
      read table it_t134t into wa_t134t with key mtart = wa_mara-mtart.
      wa_mara-mtbez = wa_t134t-mtbez.
      read table it_makt into wa_makt
            with key matnr = wa_mara-matnr binary search.

      if sy-subrc = 0.
        wa_mara-maktx = wa_makt-maktx.
      endif.
      READ TABLE it_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl.
      if sy-subrc = 0.
        wa_mara-wgbez60 = wa_t023t-text60.
      endif.

      append wa_mara to it_mara.
      clear: wa_mara,lw_head,lw_marc,wa_makt,wa_t023t,wa_t134t.
    endloop.
    sort it_eina ascending by matnr.
    delete adjacent duplicates from it_eina comparing matnr.
    loop at it_eina into wa_eina.
      loop at it_mara into wa_mara where matnr = wa_eina-matnr.
        replace '"' with '' into wa_mara-maktx.
        concatenate
        '{'
        '"created_date":'         c_quatation_mark wa_mara-ersda c_quatation_mark c_comma
        '"material_group_code":'  c_quatation_mark wa_mara-matkl c_quatation_mark c_comma
        '"material_group_name":'  c_quatation_mark wa_mara-wgbez60 c_quatation_mark c_comma
        '"material_code" :'       c_quatation_mark wa_mara-matnr c_quatation_mark c_comma
        '"description":'          c_quatation_mark wa_mara-maktx c_quatation_mark c_comma
        '"material_type":'        c_quatation_mark wa_mara-mtart c_quatation_mark c_comma
        '"material_type_desc":'   c_quatation_mark wa_mara-mtbez c_quatation_mark c_comma
        '"hsn_sac":'              c_quatation_mark wa_mara-steuc c_quatation_mark c_comma
        '"uom":'                  c_quatation_mark wa_mara-meins c_quatation_mark c_comma
        '"net_weight":'           c_quatation_mark wa_mara-ntgew c_quatation_mark c_comma
        '"gross_weight":'         c_quatation_mark wa_mara-brgew c_quatation_mark c_comma
        '"volume":'               c_quatation_mark wa_mara-volum c_quatation_mark c_comma
        '"volume_unit":'          c_quatation_mark wa_mara-voleh c_quatation_mark "c_comma
        '}'
        into wa_mat_body-materialitem.
        append wa_mat_body to it_mat_body.
        clear : wa_mat_body.
      endloop.
    endloop.
    data: lv_tabix  type sy-tabix,
          lv_int(6) type n.
    lv_int = 0.
    lv_int = lines( it_mat_body ).

    loop at it_mat_body into wa_mat_body.
      lv_tabix = sy-tabix.
      if lv_tabix < lv_int.
        concatenate ls_json wa_mat_body-materialitem ',' into ls_json.
      else.
        concatenate ls_json wa_mat_body-materialitem into ls_json.
      endif.
    endloop.
    clear: lv_int,lv_tabix.

    concatenate '[' ls_json ']'  into v_jsonload.

*Set JSON Content-Type
    call method server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    call method server->response->set_cdata( data = v_jsonload ).

  endmethod.
ENDCLASS.
