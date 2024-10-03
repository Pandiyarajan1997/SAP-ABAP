class ZCREDIT_TYPE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CUSTOMER_ADD_DATA_CS .
protected section.
private section.
ENDCLASS.



CLASS ZCREDIT_TYPE IMPLEMENTATION.


method IF_EX_CUSTOMER_ADD_DATA_CS~GET_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~GET_FIELDNAME_FOR_CHANGEDOC.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~GET_TAXI_SCREEN.

     case i_taxi_fcode.

    when 'CT_TAB'.

      e_screen  = '2222'.  "Eart + Objekt

      e_program = 'ZCREDIT_TYPE'.

      e_headerscreen_layout = ' '.
    ENDCASE.

endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~SET_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~SET_FCODE.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~SUPPRESS_TAXI_TABSTRIPS.
endmethod.
ENDCLASS.
