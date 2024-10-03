class ZCL_IM_SIV_BADI_MM01 definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_MATERIAL_CHECK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SIV_BADI_MM01 IMPLEMENTATION.


method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_MARA_MEINS.
endmethod.


method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_PMATA.
endmethod.


METHOD if_ex_badi_material_check~check_data.
*Created by: Sivakumar M from Danam Infotech
*Requirement given by: Jayaraman Mr from Danam infotech.
*Purpose:material type FERT for plant 5100 and sales Organization 500 valuation category is mandatory
*Dated on : 15-10-2015
*Material creation for FERT and company 5000 valuation category mandatory

*
*
  IF sy-tcode EQ 'MM01'.
**  if WMARA-MTART eq 'FERT' and WMARC-WERKS eq '5100' and WMVKE-VKORG eq '5000'.
**    if WMBEW-BWTTY is initial.
**      MESSAGE 'Material creation for FERT and plant 5100 valuation category is mandatory' type 'E'.
**    ENDIF.
**    endif.
**
    DATA: bwkey TYPE t001k-bwkey.

    SELECT bwkey FROM t001k INTO bwkey WHERE bukrs EQ '5000' AND bwkey EQ '5100' .
      IF wmara-mtart EQ 'FERT' AND wmarc-werks EQ bwkey.
        IF wmbew-bwtty IS INITIAL.
          MESSAGE 'valuation category is mandatory' TYPE 'E'.
        ENDIF.
      ENDIF.
    ENDSELECT.
**
  ENDIF.


ENDMETHOD.


method IF_EX_BADI_MATERIAL_CHECK~CHECK_DATA_RETAIL.
endmethod.


method IF_EX_BADI_MATERIAL_CHECK~CHECK_MASS_MARC_DATA.




endmethod.


method IF_EX_BADI_MATERIAL_CHECK~FRE_SUPPRESS_MARC_CHECK.
endmethod.
ENDCLASS.
