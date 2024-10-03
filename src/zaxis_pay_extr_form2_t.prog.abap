
**-----FOR CORESS COMPANY CODE LOGIC

*****      IF g_cross_company_check = 'X'.
*****
*****
*****        SORT g_tab_bsak BY augdt augbl umskz.
*****
*****        CLEAR : wa_bsak , wa_tab_bkpf , wa_bkpf.
*****
*****        LOOP AT g_tab_bsak INTO wa_bsak.
*****
*****          kindex = sy-tabix.
*****
*****          IF wa_bsak-augbl = wa_bsak-belnr.
*****
*****            READ TABLE g_tab_bkpf1 INTO wa_tab_bkpf WITH KEY bukrs = wa_bsak-bukrs
*****                                                            belnr = wa_bsak-belnr
*****                                                            gjahr = wa_bsak-gjahr.
*****
*****            IF sy-subrc EQ 0 AND  ( wa_tab_bkpf-tcode EQ 'FBZ2' OR wa_tab_bkpf-tcode EQ 'F110') .
*****
*****              APPEND wa_bsak TO g_tab_bsak_pymnt.
*****              DELETE g_tab_bsak.
*****              CLEAR: wa_bsak.
*****
*****            ENDIF.
*****          ENDIF.
*****        ENDLOOP.
*****
*****
*****        LOOP AT g_tab_bsak_pymnt INTO wa_bsak.
*****
*****          IF wa_bsak-shkzg = 'H'.
*****            wa_bask_payment-dmbtr =  wa_bsak-dmbtr * -1.
*****            wa_bask_payment-wrbtr =  wa_bsak-wrbtr * -1.
*****          ELSE.
*****            wa_bask_payment-dmbtr =   wa_bsak-dmbtr.
*****            wa_bask_payment-wrbtr =  wa_bsak-wrbtr.
*****          ENDIF.
*****
*****          wa_bask_payment-bukrs = p_bukrs.
*****          MOVE  wa_bsak-augbl TO wa_bask_payment-augbl.
*****          MOVE  wa_bsak-belnr TO wa_bask_payment-belnr.
*****          MOVE  wa_bsak-lifnr TO wa_bask_payment-lifnr.
*****          MOVE  wa_bsak-gjahr TO wa_bask_payment-gjahr.
*****          COLLECT wa_bask_payment INTO lt_bask_payment..
*****          CLEAR: wa_bsak,wa_bask_payment.
*****
*****        ENDLOOP.
*****
*****        DELETE ADJACENT DUPLICATES FROM g_tab_bsak_pymnt COMPARING augbl lifnr.
*****
*****        LOOP AT g_tab_bsak_pymnt INTO wa_bsak.
*****          READ TABLE lt_bask_payment INTO wa_bask_payment WITH  KEY
*****                                            augbl = wa_bsak-augbl
*****                                            lifnr = wa_bsak-lifnr.
*****          IF sy-subrc = 0.
*****            wa_bsak-bukrs = wa_bask_payment-bukrs.
*****            wa_bsak-dmbtr = wa_bask_payment-dmbtr.
*****            wa_bsak-wrbtr = wa_bask_payment-wrbtr.
*****            MODIFY  g_tab_bsak_pymnt FROM wa_bsak.
*****
*****          ENDIF.
*****        ENDLOOP.
*****      ENDIF.
****--Code added for Multiple vendor validation on 23.09.2013

*          LOOP AT g_tab_bseg INTO lw_tab_bseg.
*            lw_vend-sign = 'I'.
*            lw_vend-option = 'EQ'.
*            lw_vend-low = lw_tab_bseg-lifnr.
*            APPEND lw_vend TO lr_vend.
*          ENDLOOP.

***        g_tab_bsik3[] = g_tab_bsik[].
***          LOOP AT g_tab_bsik INTO bsik.
***          LOOP AT g_tab_bsik3 INTO wa_bsik3 WHERE belnr EQ bsik-belnr
***                                            AND   gjahr EQ bsik-gjahr.
***         IF wa_bsik3-lifnr NE bsik-lifnr.
***         DELETE g_tab_bsik WHERE belnr EQ wa_bsik3-belnr
***                            AND  gjahr EQ wa_bsik3-gjahr.
***         wa_bsik_tmp-bukrs   = wa_bsik3-bukrs.
***         wa_bsik_tmp-belnr   = wa_bsik3-belnr.
***         wa_bsik_tmp-gjahr   = wa_bsik3-gjahr.
***         wa_bsik_tmp-remarks = 'DOCUMENT NUMBER WITH MULTIPLE VENDORS CANNOT BE EXTRACTED'.
***         APPEND wa_bsik_tmp TO g_tab_bsik_tmp.
***         ENDIF.
***          ENDLOOP.
***          ENDLOOP.

**-----END OF CROSS COMPANY CODE LOGIC.
