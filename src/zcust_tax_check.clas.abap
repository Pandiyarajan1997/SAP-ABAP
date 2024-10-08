class ZCUST_TAX_CHECK definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CUSTOMER_ADD_DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCUST_TAX_CHECK IMPLEMENTATION.


method IF_EX_CUSTOMER_ADD_DATA~BUILD_TEXT_FOR_CHANGE_DETAIL.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_ACCOUNT_NUMBER.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_ADD_ON_ACTIVE.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_ALL_DATA.
*DATA :LV_TAX3 TYPE KNA1-STCD3.
*if sy-TCODE eq 'XD01' OR SY-TCODE EQ 'XD02'.
*  SELECT SINGLE STCD3 FROM KNA1 INTO LV_TAX3 WHERE STCD3 = S_KNA1-STCD3 AND KUNNR NE S_KNA1-KUNNR .
*ENDIF.
*IF S_KNA1-KTOKD NE 'YBAC'.
*IF LV_TAX3 IS NOT INITIAL.
*     MESSAGE 'Customer master record with the same tax number already exists' TYPE 'E' .
*ENDIF.
*ENDIF.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_DATA_CHANGED.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~GET_CHANGEDOCS_FOR_OWN_TABLES.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~INITIALIZE_ADD_ON_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~MODIFY_ACCOUNT_NUMBER.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~PRESET_VALUES_CCODE.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~PRESET_VALUES_SAREA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~READ_ADD_ON_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~SAVE_DATA.

endmethod.


method IF_EX_CUSTOMER_ADD_DATA~SET_USER_INPUTS.
endmethod.
ENDCLASS.
