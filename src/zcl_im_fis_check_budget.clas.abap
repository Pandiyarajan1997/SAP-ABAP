class ZCL_IM_FIS_CHECK_BUDGET definition
  public
  final
  create public .

public section.

  interfaces IF_EX_FMBW_CUSTOMER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_FIS_CHECK_BUDGET IMPLEMENTATION.


method IF_EX_FMBW_CUSTOMER~PUT_DATA_TO_SCREEN .
 me->if_ex_fmbw_customer~doc_header = doc_header.
 me->if_ex_fmbw_customer~processing_mode = processing_mode.
 IF sy-tcode = 'FMBBC'.
 DATA(lv_fisyear) = doc_header-docyear.
 ENDIF.
endmethod.
ENDCLASS.
