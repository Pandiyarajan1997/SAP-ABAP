class ZCREDIT_LIMIT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CUSTOMER_ADD_DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCREDIT_LIMIT IMPLEMENTATION.


method IF_EX_CUSTOMER_ADD_DATA~BUILD_TEXT_FOR_CHANGE_DETAIL.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_ACCOUNT_NUMBER.
endmethod.


METHOD IF_EX_CUSTOMER_ADD_DATA~CHECK_ADD_ON_ACTIVE.

  DATA: L_FLG_ACTIVE TYPE BOOLE-BOOLE.

  IF I_SCREEN_GROUP = 'CT'.

    E_ADD_ON_ACTIVE = 'X'.

  ENDIF.

ENDMETHOD.


method IF_EX_CUSTOMER_ADD_DATA~CHECK_ALL_DATA.
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
