class ZCL_IM_MM_EQUNR_MIGO definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_MIGO_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_EQUNR_MIGO IMPLEMENTATION.


  method IF_EX_MB_MIGO_BADI~CHECK_HEADER.
  endmethod.


  method IF_EX_MB_MIGO_BADI~CHECK_ITEM.
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_DELETE.
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_LOAD.
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_SAVE.
  endmethod.


  method IF_EX_MB_MIGO_BADI~INIT.
  endmethod.


  method IF_EX_MB_MIGO_BADI~LINE_DELETE.
  endmethod.


  METHOD if_ex_mb_migo_badi~line_modify.
    IF sy-tcode = 'MIGO'.

    ENDIF.
  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~MAA_LINE_ID_ADJUST.
  endmethod.


  method IF_EX_MB_MIGO_BADI~MODE_SET.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_DETAIL.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_HEADER.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_DETAIL.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_HEADER.
  endmethod.


  method IF_EX_MB_MIGO_BADI~POST_DOCUMENT.
    IF sy-tcode = 'MIGO'.

    ENDIF.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PROPOSE_SERIALNUMBERS.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PUBLISH_MATERIAL_ITEM.
  endmethod.


  method IF_EX_MB_MIGO_BADI~RESET.
  endmethod.


  method IF_EX_MB_MIGO_BADI~STATUS_AND_HEADER.
  endmethod.
ENDCLASS.
