class ZCL_IM_MM_BKTXT_MANDT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_MIGO_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_BKTXT_MANDT IMPLEMENTATION.


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


method IF_EX_MB_MIGO_BADI~LINE_MODIFY.
endmethod.


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
*Created By Govind On 03/12/2014 For Header Text Mandatory Field Seted
* Requirment Given By RAM-MM

  DATA : V_BSART TYPE EKKO-BSART.
  DATA: GT_EKKO TYPE TABLE OF EKKO,
        WA_EKKO TYPE EKKO.

  SELECT *  FROM EKKO INTO TABLE  GT_EKKO FOR ALL ENTRIES IN IT_MSEG WHERE EBELN = IT_MSEG-EBELN.

  LOOP AT GT_EKKO INTO WA_EKKO.

    SELECT SINGLE BSART FROM EKKO INTO V_BSART WHERE EBELN = WA_EKKO-EBELN.
    CLEAR WA_EKKO.
  ENDLOOP.

  CASE V_BSART.
    WHEN 'ZNB'.
      IF IS_MKPF-BLART = 'WE'.
        IF IS_MKPF-BKTXT IS INITIAL.
          MESSAGE 'Enter HeaderText' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.
      ENDIF.
    WHEN 'ZUB'.
      IF IS_MKPF-BLART = 'WE'.
        IF IS_MKPF-BKTXT IS INITIAL.
          MESSAGE 'Enter HeaderText' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.
      ENDIF.
    WHEN 'UB' .
      IF IS_MKPF-BLART = 'WE'.
        IF IS_MKPF-BKTXT IS INITIAL.
          MESSAGE 'Enter HeaderText' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.
      ENDIF.
    WHEN 'ZIM'.
      IF IS_MKPF-BLART = 'WE'.
        IF IS_MKPF-BKTXT IS INITIAL.
          MESSAGE 'Enter HeaderText' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.
      ENDIF.
    WHEN 'ZSC'.
      IF IS_MKPF-BLART = 'WE'.
        IF IS_MKPF-BKTXT IS INITIAL.
          MESSAGE 'EnterHeaderText' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.
      ENDIF.
  ENDCASE.
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
