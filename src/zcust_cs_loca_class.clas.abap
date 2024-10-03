class ZCUST_CS_LOCA_CLASS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CUSTOMER_ADD_DATA_CS .
protected section.
private section.
ENDCLASS.



CLASS ZCUST_CS_LOCA_CLASS IMPLEMENTATION.


method IF_EX_CUSTOMER_ADD_DATA_CS~GET_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~GET_FIELDNAME_FOR_CHANGEDOC.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~GET_TAXI_SCREEN.

   case i_taxi_fcode.

    when 'SL_TAB'.

      e_screen  = '1111'.  "Eart + Objekt

      e_program = 'ZCUST_LOCATION'.

      e_headerscreen_layout = ' '.

  endcase.

endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~SET_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~SET_FCODE.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~SUPPRESS_TAXI_TABSTRIPS.
endmethod.
ENDCLASS.
