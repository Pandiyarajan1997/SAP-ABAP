FUNCTION zmm_get_open_procord_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D
*"  TABLES
*"      IT_PROCORDDTLS STRUCTURE  ZPP_OPN_PROCORD_DTLS
*"      IM_AUFNR TYPE  FAGL_RANGE_T_AUFNR OPTIONAL
*"----------------------------------------------------------------------
*Created By: Samsudeen M
*Created On: 19.07.2023
*Purpose : Getting Open Process Order Details which is technically not Completed
*Reference By: Ramakrishnan J
*----------------------------------------------------------------------------------
  DATA: lw_order_objects TYPE bapi_pi_order_objects,
        header           TYPE TABLE OF bapi_order_header1,
        l_header         TYPE zpp_opn_procord_dtls.
*---- Fetching Process Order Number which is not teco completed ------------------*
  SELECT aufnr,
         werks,
         idat2 FROM aufk INTO TABLE @DATA(lt_procord)
         WHERE aufnr IN @im_aufnr
         AND werks EQ @plant.
  IF sy-subrc = 0.
    CLEAR: lw_order_objects.
    lw_order_objects-header = abap_true.
    LOOP AT lt_procord ASSIGNING FIELD-SYMBOL(<fs_procord>) WHERE idat2 IS INITIAL.
      CLEAR header[].
      "Getting Process Order Details
      CALL FUNCTION 'BAPI_PROCORD_GET_DETAIL'
        EXPORTING
          number        = <fs_procord>-aufnr
          order_objects = lw_order_objects
        TABLES
          header        = header.
      LOOP AT header ASSIGNING FIELD-SYMBOL(<fs_header>).
        CLEAR l_header.
        MOVE-CORRESPONDING <fs_header> TO l_header.
        APPEND l_header TO it_procorddtls.
      ENDLOOP.
    ENDLOOP.
  ENDIF.




ENDFUNCTION.
