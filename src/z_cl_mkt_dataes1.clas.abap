class Z_CL_MKT_DATAES1 definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CUSTOMER_ADD_DATA_CS .
protected section.
private section.
ENDCLASS.



CLASS Z_CL_MKT_DATAES1 IMPLEMENTATION.


method IF_EX_CUSTOMER_ADD_DATA_CS~GET_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~GET_FIELDNAME_FOR_CHANGEDOC.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~GET_TAXI_SCREEN.

IF SY-TCODE EQ 'XD02' OR  SY-TCODE EQ 'XD03' .
   CASE I_TAXI_FCODE.

    WHEN 'SK_MKT'.

      E_SCREEN  = '0100'.

      E_PROGRAM = 'ZMARKETING_PGM'.

      E_HEADERSCREEN_LAYOUT = ' '.

  ENDCASE.
ENDIF.

endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~SET_DATA.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~SET_FCODE.
endmethod.


method IF_EX_CUSTOMER_ADD_DATA_CS~SUPPRESS_TAXI_TABSTRIPS.
endmethod.
ENDCLASS.
