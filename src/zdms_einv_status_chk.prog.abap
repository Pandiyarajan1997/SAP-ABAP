*&---------------------------------------------------------------------*
*& Report ZDMS_EINV_STATUS_CHK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdms_einv_status_chk.

TYPES : BEGIN OF ty_alv,
          distributor TYPE zdist,
          dealer      TYPE kunnr,
          vbeln       TYPE vbeln_vf,
          irn_status  TYPE j_1ig_irn_status,
          ack_no      TYPE j_1ig_ack_no,
          ack_date    TYPE j_1ig_ack_date,
          irn         TYPE j_1ig_irn,
          type        TYPE bapi_mtype,
          remarks     TYPE zremark,
        END OF ty_alv.

DATA : gt_alv    TYPE TABLE OF ty_alv,
       gv_vbeln  TYPE vbrk-vbeln,
       lobj_einv TYPE REF TO zcl_dms_einvoice_process,
       ls_irnlog TYPE zdms_invoice_irn.


SELECT-OPTIONS : so_vbrk FOR gv_vbeln.
SELECTION-SCREEN SKIP 1.
PARAMETERS : p_chk AS CHECKBOX.

START-OF-SELECTION.
********************get the IRN status****************
  CREATE OBJECT lobj_einv.

  SELECT * FROM zdms_invoice_irn INTO TABLE @DATA(gt_irn)
                                 WHERE docno IN @so_vbrk.
  IF sy-subrc = 0.

    LOOP AT gt_irn INTO DATA(ls_irn).

      IF ls_irn-irn IS NOT INITIAL.

        lobj_einv->get_irn_details(
          EXPORTING
            irn              = ls_irn-irn
            distributor_code = ls_irn-distrb
            retailer         = ls_irn-dealer
          IMPORTING
            response         = DATA(ls_response)
            type             = DATA(lv_type)
            message          = DATA(lv_msg) ).
      ELSE.
        lv_type = 'E'.
        lv_msg  = 'IRN value is missing'.
      ENDIF.
      APPEND VALUE #( distributor = ls_irn-distrb
                      dealer      = ls_irn-dealer
                      vbeln       = ls_irn-docno
                      ack_no      = ls_response-ackno
                      ack_date    = ls_response-ackdt
                      irn_status  = ls_response-status
                      irn         = ls_response-irn
                      type        = lv_type
                      remarks     = lv_msg ) TO gt_alv.

      IF p_chk = abap_true AND ls_response-ackno IS NOT INITIAL AND ls_response-signedqrcode IS NOT INITIAL.
*        ls_irnlog-docno = ls_irn-docno.
*        ls_irnlog-bukrs  = ls_irn-bukrs.
*        ls_irnlog-distrb  = ls_irn-distrb.
*        ls_irnlog-dealer  = ls_irn-dealer.
*        ls_irnlog-docno = ls_irn-docno.
*        ls_irnlog-ack_no = ls_response-ackno.
*        ls_irnlog-ack_date = ls_response-ackdt.
*        ls_irnlog-irn_status = ls_response-status.
*        ls_irnlog-signed_qrcode = ls_response-signedqrcode.
*        ls_irnlog-signed_inv = ls_response-signedinvoice.
*        MODIFY zdms_invoice_irn FROM ls_irnlog.
        UPDATE zdms_invoice_irn SET signed_qrcode = ls_response-signedqrcode
                                    signed_inv    = ls_response-signedinvoice
                                    irn           = ls_response-irn
                                    ack_date      = ls_response-ackdt
                                    ack_no        = ls_response-ackno
                                    irn_status    = ls_response-status
                                    version       = 11
                                    erdat         = sy-datum
                                    ernam         = sy-uname
                                    erzet         = sy-uzeit WHERE docno = ls_irn-docno.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.


      CLEAR : ls_irn,lv_msg,lv_type,ls_response,ls_irnlog.

    ENDLOOP.
  ENDIF.
*****************display the screen*************
  IF gt_alv IS NOT INITIAL.

    CALL FUNCTION 'Z_POPUP_ALV'
      TABLES
        it_alv = gt_alv.

  ELSE.
    MESSAGE : 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
