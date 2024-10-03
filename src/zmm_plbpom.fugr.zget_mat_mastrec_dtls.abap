FUNCTION zget_mat_mastrec_dtls.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D
*"  TABLES
*"      IT_MASTREC_DTLS STRUCTURE  ZPP_MAT_MASTREC_DTLS
*"      IT_MATNR TYPE  /SAPAPO/MATNR_RTAB OPTIONAL
*"  EXCEPTIONS
*"      NO_PLANT
*"----------------------------------------------------------------------
*Created by: Samsudeen M
*Created On: 19.07.2023
*Purpose : Master Recipe Details for Material type Finished Goods and semi finished Goods
* Reference: Ramakrishn an J
*-----------------------------------------------------------------------
  DATA: lt_plpo_exp TYPE TABLE OF coplpo.

  IF plant IS NOT INITIAL.

    SELECT matnr FROM mara
           INTO TABLE @DATA(lt_mara)
           WHERE matnr IN @it_matnr
           AND mtart IN ( 'FERT', 'HALB' ).
    IF sy-subrc EQ 0.
      SORT lt_mara[] BY matnr ASCENDING.
      SELECT matnr,
             werks FROM marc
             INTO TABLE @DATA(lt_marc)
             FOR ALL ENTRIES IN @lt_mara
             WHERE matnr = @lt_mara-matnr
             AND werks = @plant.
    ENDIF.

    IF lt_marc[] IS NOT INITIAL.
      SORT lt_marc[] BY matnr werks.
      "Fetching Recipe Group based on Material
      SELECT matnr,
             werks,
             plnnr,
             plnal FROM mapl
             INTO TABLE @DATA(lt_mapl)
             FOR ALL ENTRIES IN @lt_marc
             WHERE matnr = @lt_marc-matnr
             AND werks = @plant
             AND plnty = '2'
             AND loekz NE 'X'.
      IF sy-subrc EQ 0.
        SORT lt_mapl[] BY matnr werks.
      ENDIF.

      IF lt_mapl[] IS NOT INITIAL.
        LOOP AT lt_mapl ASSIGNING FIELD-SYMBOL(<fls_mapl>).
          SELECT SINGLE name1 FROM t001w INTO @DATA(l_pname)
            WHERE werks = @<fls_mapl>-werks.
          SELECT SINGLE maktx FROM makt INTO @DATA(l_mname)
            WHERE matnr = @<fls_mapl>-matnr.
          REFRESH: lt_plpo_exp.
          CALL FUNCTION 'CP_EX_PLAN_READ'
            EXPORTING
              cmode_imp     = 'R'
              plnty_imp     = '2'
              plnnr_imp     = <fls_mapl>-plnnr
              plnal_imp     = <fls_mapl>-plnal
              sttag_imp     = sy-datum
            TABLES
              plpo_exp      = lt_plpo_exp
            EXCEPTIONS
              not_found     = 1
              plnal_initial = 2
              OTHERS        = 3.
          IF sy-subrc = 0.
            DELETE lt_plpo_exp[] WHERE phflg NE 'X'.
          ENDIF.
          LOOP AT lt_plpo_exp INTO DATA(lw_plpo_exp).
            SELECT SINGLE arbpl FROM crhd INTO @DATA(l_workcntr)
              WHERE objty = 'A' AND objid = @lw_plpo_exp-arbid.
            SELECT SINGLE ktext FROM crtx INTO @DATA(l_wctr_txt)
              WHERE objty = 'A' AND objid = @lw_plpo_exp-arbid
              AND spras EQ @sy-langu.
            APPEND VALUE #( plant          = <fls_mapl>-werks
                            plant_name     = l_pname
                            material       = <fls_mapl>-matnr
                            material_text  = l_mname
                            plnnr          = lw_plpo_exp-plnnr
                            plnal          = <fls_mapl>-plnal
                            plnkn          = lw_plpo_exp-plnkn
                            vornr          = lw_plpo_exp-vornr
                            workcntr       = l_workcntr
                            wctrtxt        = l_wctr_txt
                            ltxa1          = lw_plpo_exp-ltxa1
                            meinh          = lw_plpo_exp-meinh
                            bmsch          = lw_plpo_exp-bmsch
                            lar01          = lw_plpo_exp-lar01
                            vge01          = lw_plpo_exp-vge01
                            vgw01          = lw_plpo_exp-vgw01
                            lar02          = lw_plpo_exp-lar02
                            vge02          = lw_plpo_exp-vge02
                            vgw02          = lw_plpo_exp-vgw02
                            lar03          = lw_plpo_exp-lar03
                            vge03          = lw_plpo_exp-vge03
                            vgw03          = lw_plpo_exp-vgw03
                            lar04          = lw_plpo_exp-lar04
                            vge04          = lw_plpo_exp-vge04
                            vgw04          = lw_plpo_exp-vgw04
                            lar05          = lw_plpo_exp-lar05
                            vge05          = lw_plpo_exp-vge05
                            vgw05          = lw_plpo_exp-vgw05
                            lar06          = lw_plpo_exp-lar06
                            vge06          = lw_plpo_exp-vge06
                            vgw06          = lw_plpo_exp-vgw06 ) TO it_mastrec_dtls.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ELSE.
    RAISE no_plant.
  ENDIF.



ENDFUNCTION.
