class ZCL_IM_FISYR_CHECK_BUDGET definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_FMBCCF_ENTRY_DOC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_FISYR_CHECK_BUDGET IMPLEMENTATION.


  method IF_EX_FMBCCF_ENTRY_DOC~CHANGE_DOCUMENT.
  endmethod.


  METHOD if_ex_fmbccf_entry_doc~update_budget_info.

    IF sy-tcode = 'FMBBC'.
      DATA(lv_fisyear) = i_f_budget_data-kngjahr.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
