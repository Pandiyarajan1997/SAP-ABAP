class ZCL_API_DMSDISTSTOCK definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DMSDISTSTOCK IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*&Created by: Pandiarajan
*&Created On: 14.12.2023
*&Purpose   : API for distributor( plant ) wise stock details for dms
*&Reference : Ramakrishnan J
*-------------------------------------------------------------------------------------
*******input json structure**********
    TYPES: BEGIN OF ty_input,
             distributor TYPE kunnr,
             plant       TYPE werks_d,
             type        TYPE char1,
           END OF ty_input.
********return response through api
    TYPES : BEGIN OF ty_mat,
              material     TYPE matnr,
              mat_desc     TYPE makt-maktx,
              stock_uom    TYPE meins,
              stock_qty    TYPE labst,
              stock_ltrqty TYPE labst,
              stock_val    TYPE salk3,
            END OF ty_mat.
    DATA : BEGIN OF gt_res,
             distributor TYPE kunnr,
             plant       TYPE werks_d,
             status      TYPE bapi_mtype,
             message     TYPE string,
             details     TYPE TABLE OF ty_mat,
           END OF gt_res.

    DATA : gt_response LIKE TABLE OF gt_res.
    DATA : gt_details TYPE TABLE OF ty_mat.
    DATA : gt_input  TYPE TABLE OF ty_input.     "structure for json input

    DATA : lv_body    TYPE string,       "for output json string
           v_jsonload TYPE string,
           lv_data    TYPE string.          "for input json string

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.     "data dec for store api log table
    CREATE OBJECT lo_log_upd.

    "Input of API Request
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
***********FETCH the datas from mard & mara***********
      SELECT a~bwkey AS werks,
             a~matnr,
             b~meins ,
             a~lbkum,
             a~lbkum AS ltrqty,
             a~salk3
        INTO TABLE @DATA(gt_mard)
*        FROM mard AS a
        FROM mbew AS a
        INNER JOIN mara AS b
        ON b~matnr = a~matnr
        FOR ALL ENTRIES IN @gt_input
        WHERE bwkey = @gt_input-plant "werks = @gt_input-plant.
        AND lbkum <> @abap_false.
*********check distributor and plant*********
      SELECT kunnr,werks FROM kna1 INTO TABLE @DATA(gt_kna1)
                                   FOR ALL ENTRIES IN @gt_input
                                   WHERE werks = @gt_input-plant.
**********data process **********
      SORT : gt_mard  BY werks,
             gt_input BY plant type,
             gt_kna1  BY kunnr werks.
***********validation**********
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input2>).
        <fs_input2>-distributor = |{ <fs_input2>-distributor ALPHA = IN }|.
        READ TABLE gt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY kunnr = <fs_input2>-distributor
                                                                      werks = <fs_input2>-plant BINARY SEARCH.
        IF sy-subrc NE 0.
          APPEND VALUE #( distributor = <fs_input2>-distributor
                          plant       = <fs_input2>-plant
                          status      = 'E'
                          message     = |Distributor - { <fs_input2>-distributor } & Plant - { <fs_input2>-plant } Mismatch| ) TO gt_response.
          <fs_input2>-type = 'E'.
        ELSE.
          READ TABLE gt_mard ASSIGNING FIELD-SYMBOL(<fs_mard2>) WITH KEY werks = <fs_input2>-plant BINARY SEARCH.
          IF sy-subrc NE 0.
            APPEND VALUE #( distributor = <fs_input2>-distributor
                            plant       = <fs_input2>-plant
                            status      = 'E'
                            message     = |Distributor - { <fs_input2>-distributor } Stock not available| ) TO gt_response.
            <fs_input2>-type = 'E'.
          ELSE.
            <fs_input2>-type = 'S'.
          ENDIF.
        ENDIF.
      ENDLOOP.
***********data filling process*************
      LOOP AT gt_mard ASSIGNING FIELD-SYMBOL(<fs_mard>).
        AT NEW werks.
          READ TABLE gt_input ASSIGNING FIELD-SYMBOL(<fs_input>) WITH KEY plant = <fs_mard>-werks
                                                                          type  = 'S' BINARY SEARCH.
          IF sy-subrc EQ 0.
            APPEND VALUE #( distributor = <fs_input>-distributor
                            plant       = <fs_input>-plant
                            status      = 'S'
                            message     = |Distributor - { <fs_input>-distributor }  stock details| ) TO gt_response.
            DATA(lv_line)  = sy-tabix.
          ELSE.
            DATA(lv_error) = abap_true.
          ENDIF.
        ENDAT.
        IF lv_error = abap_true.
          CONTINUE.
        ENDIF.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = <fs_mard>-matnr
            i_in_me              = <fs_mard>-meins
            i_out_me             = 'L'
            i_menge              = <fs_mard>-lbkum
          IMPORTING
            e_menge              = <fs_mard>-ltrqty
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        SELECT SINGLE maktx INTO @DATA(l_mat_desc) FROM makt
            WHERE matnr = @<fs_mard>-matnr
              AND spras = @sy-langu.

        APPEND VALUE #(  material      = <fs_mard>-matnr
                         mat_desc      = l_mat_desc
                         stock_uom     = <fs_mard>-meins
                         stock_qty     = <fs_mard>-lbkum
                         stock_ltrqty  = <fs_mard>-ltrqty
                         stock_val     = <fs_mard>-salk3
                         ) TO gt_details.
        AT END OF werks.
          IF lv_line IS NOT INITIAL.
            gt_res-details[] = gt_details[].
            MODIFY gt_response INDEX lv_line FROM gt_res TRANSPORTING details.
          ENDIF.
          CLEAR : lv_line,gt_res,lv_error.
          REFRESH : gt_details.
        ENDAT.
      ENDLOOP.
    ELSE.
      APPEND VALUE #( status      = 'E'
                      message     = |Please fill the data| ) TO gt_response.
    ENDIF.

    IF gt_response IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMS_DIST_STOCK'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      v_jsonload = |{ lv_body }|.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
