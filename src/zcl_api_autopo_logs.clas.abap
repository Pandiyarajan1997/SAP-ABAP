class ZCL_API_AUTOPO_LOGS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_AUTOPO_LOGS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 23.05.2023
*&Purpose : For Auto Purchase Order entry update in log table
*&Reference: Ramakrishnan J & Praveen Kumar
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             uniqid   TYPE zpoid,
             plant    TYPE werks_d,
             material TYPE matnr,
             qty      TYPE menge_d,
           END OF ty_input.

    TYPES: BEGIN OF ty_output,
             uniqid   TYPE zpoid,
             plant    TYPE werks_d,
             material TYPE matnr,
             type     TYPE bapi_mtype,
             msg      TYPE string,
           END OF ty_output.

    DATA: gt_input TYPE TABLE OF ty_input,
          gs_input TYPE ty_input.
    DATA: gt_output TYPE TABLE OF ty_output.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.

    "Input of API Request
    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).
    DATA(lv_data_tmp) = |[ { lv_data } ]|.
    CONDENSE lv_data_tmp NO-GAPS.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data_tmp
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
      SELECT * FROM zmm_autopo_log
        INTO TABLE @DATA(lt_logs)
        FOR ALL ENTRIES IN @gt_input
        WHERE uniqueid = @gt_input-uniqid
        AND ( type = 'P' OR type = 'U' ).
      IF sy-subrc = 0.
        SORT lt_logs[] BY uniqueid.
      ENDIF.

      REFRESH: gt_output.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).

        IF <fs_input>-uniqid IS INITIAL.
          APPEND VALUE #( uniqid   = <fs_input>-uniqid
                          plant    = <fs_input>-plant
                          material = <fs_input>-material
                          type     = 'E'
                          msg      = |Unique ID is Missing| ) TO gt_output.
        ELSEIF <fs_input>-plant IS INITIAL.
          APPEND VALUE #( uniqid   = <fs_input>-uniqid
                          plant    = <fs_input>-plant
                          material = <fs_input>-material
                          type     = 'E'
                          msg      = |Plant is Missing| ) TO gt_output.
        ELSEIF <fs_input>-material IS INITIAL.
          APPEND VALUE #( uniqid   = <fs_input>-uniqid
                          plant    = <fs_input>-plant
                          material = <fs_input>-material
                          type     = 'E'
                          msg      = |Material is Missing| ) TO gt_output.
        ELSEIF <fs_input>-qty IS INITIAL.
          APPEND VALUE #( uniqid   = <fs_input>-uniqid
                          plant    = <fs_input>-plant
                          material = <fs_input>-material
                          type     = 'E'
                          msg      = |Quantity is Missing| ) TO gt_output.
        ENDIF.
*Unique ID Checks
        DATA(lv_uniqid) = VALUE #( lt_logs[ uniqueid = <fs_input>-uniqid ]-uniqueid OPTIONAL ).
        IF lv_uniqid IS NOT INITIAL.
          APPEND VALUE #( uniqid = <fs_input>-uniqid
                          plant = <fs_input>-plant
                          material = <fs_input>-material
                          type = 'E'
                          msg = |Already Same Unique ID Processed| ) TO gt_output.
        ENDIF.
*plant code existence check
        SELECT SINGLE werks FROM t001w
          INTO @DATA(l_plant)
          WHERE werks = @<fs_input>-plant.
        IF sy-subrc NE 0.
          APPEND VALUE #( uniqid = <fs_input>-uniqid
                          plant = <fs_input>-plant
                          material = <fs_input>-material
                          type = 'E'
                          msg = |Incorrect Plant Code { <fs_input>-plant }| ) TO gt_output.
        ENDIF.
*Material Number Checks
        DATA(lv_matnr) = CONV matnr18( <fs_input>-material ).
        lv_matnr = |{ lv_matnr ALPHA = IN }|.
        SELECT SINGLE matnr FROM mara
          INTO @DATA(l_matnr)
          WHERE matnr = @lv_matnr.
        IF sy-subrc NE 0.
          APPEND VALUE #( uniqid = <fs_input>-uniqid
                          plant = <fs_input>-plant
                          material = <fs_input>-material
                          type = 'E'
                          msg = |Incorrect Material Code { <fs_input>-material }| ) TO gt_output.
        ENDIF.
*Material and Plant Extension Checks
        SELECT SINGLE matnr FROM marc
          INTO @l_matnr
          WHERE matnr = @lv_matnr
          AND werks = @<fs_input>-plant.
        IF sy-subrc NE 0.
          APPEND VALUE #( uniqid = <fs_input>-uniqid
                          plant = <fs_input>-plant
                          material = <fs_input>-material
                          type = 'E'
                          msg = |Material Code { <fs_input>-material } not Extended in Plant { <fs_input>-plant }| ) TO gt_output.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT gt_input ASSIGNING <fs_input>.
      DATA(lv_error) = VALUE #( gt_output[ uniqid = <fs_input>-uniqid
                                           type   = 'E' ]-uniqid OPTIONAL ).
      IF lv_error IS INITIAL.
*material Number checks
        DATA(lv_matnr_tmp) = CONV matnr18( <fs_input>-material ).
        lv_matnr_tmp = |{ lv_matnr_tmp ALPHA = IN }|.
        SELECT SINGLE * FROM zmm_po_mapping
          INTO @DATA(l_po_mapping)
          WHERE werks = @<fs_input>-plant
          AND matnr = @lv_matnr_tmp
          AND datbi GE @sy-datum
          AND datab LE @sy-datum
          AND loevm NE 'X'.
        IF sy-subrc EQ 0.
          IF l_po_mapping-ident EQ 'I'.
            DATA(l_reswk) = l_po_mapping-reswk.
          ELSE.
            DATA(l_supplier) = l_po_mapping-lifnr.
          ENDIF.
          DATA(l_update) = VALUE zmm_autopo_log( mandt    = sy-mandt
                                                 uniqueid = <fs_input>-uniqid
                                                 werks    = <fs_input>-plant
                                                 matnr    = lv_matnr_tmp
                                                 menge    = <fs_input>-qty
                                                 reswk    = l_reswk
                                                 lifnr    = l_supplier
                                                 bsart    = l_po_mapping-bsart
                                                 erdat    = sy-datum
                                                 type     = 'I'
                                                 msg      = |Initiated| ).
          MODIFY zmm_autopo_log FROM l_update.
          APPEND VALUE #( uniqid   = <fs_input>-uniqid
                          plant    = <fs_input>-plant
                          material = <fs_input>-material
                          type     = 'S'
                          msg = |Updated in Log table| ) TO gt_output.
        ELSE.
          l_update = VALUE zmm_autopo_log( mandt    = sy-mandt
                                           uniqueid = <fs_input>-uniqid
                                           werks    = <fs_input>-plant
                                           matnr    = lv_matnr_tmp
                                           menge    = <fs_input>-qty
                                           reswk    = l_reswk
                                           lifnr    = l_supplier
                                           erdat    = sy-datum
                                           type     = 'N'
                                           msg      = |No Source list Mapping Available| ).
          MODIFY zmm_autopo_log FROM l_update.
          APPEND VALUE #( uniqid   = <fs_input>-uniqid
                          plant    = <fs_input>-plant
                          material = <fs_input>-material
                          type     = 'S'
                          msg = |No Valid Source list mapping Available| ) TO gt_output.
        ENDIF.
      ENDIF.
      CLEAR: l_supplier,l_reswk.
    ENDLOOP.

    CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
    /ui2/cl_json=>serialize(
    EXPORTING
     data         =  gt_output
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    RECEIVING
     r_json         = lv_body ).

    v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = v_jsonload ).


  ENDMETHOD.
ENDCLASS.
