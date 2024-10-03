"Name: \PR:SAPMV45A\FO:USEREXIT_SAVE_DOCUMENT_PREPARE\SE:BEGIN\EN:ZSALES_REQ4\SE:BEGIN\EI
ENHANCEMENT 0 ZSD_SALESRETURN_CHECK_QUANTITY.
**Added By: Samsudeen M & Ramakrishnan J
**Purpose: To Restrict the Quantity excess for sales return
*  DATA: lt_komv TYPE TABLE OF komv,
*        lt_vbpa TYPE TABLE OF vbpavb,
*        lt_vbrk TYPE TABLE OF vbrkvb,
*        lt_vbrp TYPE TABLE OF vbrpvb.
*IF sy-tcode = 'VA01'.
* IF vbak-auart = 'YBRE'.
*  IF vbak-vgbel IS NOT INITIAL.
*    DATA(l_ref_invoice) = VALUE vbrk( vbeln = vbak-vgbel ).
*    REFRESH: lt_komv,lt_vbpa,lt_vbrk,lt_vbrp.
*    CALL FUNCTION 'RV_INVOICE_DOCUMENT_READ'
*      EXPORTING
*        vbrk_i                         = l_ref_invoice
*      tables
*        xkomv                          = lt_komv
*        xvbpa                          = lt_vbpa
*        xvbrk                          = lt_vbrk
*        xvbrp                          = lt_vbrp.
*    IF sy-subrc = 0.
*     LOOP AT xvbap ASSIGNING FIELD-SYMBOL(<fs_xvbap>).
*       LOOP AT lt_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>)  WHERE vbeln = <fs_xvbap>-vgbel
*                                                          AND   posnr = <fs_xvbap>-vgpos
*                                                          AND   matnr = <fs_xvbap>-matnr.
*       LOOP AT xvbapf ASSIGNING FIELD-SYMBOL(<fs_xvbapf>) WHERE vbeln = <fs_vbrp>-vbeln
*                                                          AND   posnr = <fs_vbrp>-posnr.
*         IF <fs_xvbapf>-hmenge IS NOT INITIAL.
*           DATA(l_overall_qty) = CONV mdr1menge( 0 ).
*           CALL FUNCTION 'MD_CONV_QUANTITY_FLOAT_TO_PACK'
*             EXPORTING
*               iv_menge       = <fs_xvbapf>-hmenge
*            IMPORTING
*              ev_menge       = l_overall_qty.
*         ENDIF.
*          IF l_overall_qty GT <fs_vbrp>-fkimg.
*           DATA(l_msg) = CONV string( |Sales Return Quantity and Actual Quantity is Not Matching for Material { <fs_xvbap>-matnr }| ).
*           MESSAGE l_msg TYPE 'E'.
*           fcode = 'ENT1'.
*           PERFORM fcode_bearbeiten.
**           fcode = fcode_gleiche_seite.
**            perform fcode_bearbeiten.
*          ENDIF.
*        ENDLOOP.
*       ENDLOOP.
*
*     ENDLOOP.
*    ENDIF.
*  ENDIF.
* ENDIF.
*ENDIF.
ENDENHANCEMENT.
