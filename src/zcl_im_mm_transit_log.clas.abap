class ZCL_IM_MM_TRANSIT_LOG definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_MIGO_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_TRANSIT_LOG IMPLEMENTATION.


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


METHOD IF_EX_MB_MIGO_BADI~POST_DOCUMENT.

  DATA: MBLNR  TYPE MBLNR,
       MJAHR TYPE MJAHR,
       XBLNR TYPE XBLNR.
  IMPORT MBLNR TO MBLNR FROM MEMORY ID 'CTAB'.
  IMPORT MJAHR TO MJAHR FROM MEMORY ID 'CTAB1'.
  IMPORT XBLNR TO XBLNR FROM MEMORY ID 'CTAB2'.
  "BREAK-POINT.


  DATA : "IT_MSEG1 TYPE TABLE OF TY_T_MSEG ,
         WA_MSEG LIKE LINE OF IT_MSEG ,
         LV_EBELN TYPE EKKO-EBELN ,
         LV_BSA TYPE EKKO-BSART,
         LV_DAT TYPE MKPF-CPUDT,
         LA_DAT TYPE MKPF-CPUDT,
         LV_NUM TYPE MKPF-MBLNR.

  IF SY-TCODE EQ 'MIGO'.
    IF IS_MKPF-BLART = 'WE' .
      READ TABLE IT_MSEG INTO WA_MSEG INDEX 1 .
      IF SY-SUBRC = 0.
        LV_EBELN = WA_MSEG-EBELN .
      ENDIF.
      IF WA_MSEG-BWART NE '102' .
         SELECT SINGLE BSART INTO LV_BSA FROM EKKO WHERE EBELN =  LV_EBELN .  " I_MSEG-EBELN.
        IF LV_BSA EQ 'UB' OR LV_BSA EQ 'ZUB' OR LV_BSA EQ 'ZSTO' .
          SELECT CPUDT INTO LV_DAT UP TO 1 ROWS FROM MKPF WHERE XBLNR = XBLNR  ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
          SELECT SINGLE MBLNR INTO LV_NUM FROM ZTRANSIT_RELEASE WHERE MBLNR = XBLNR .
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              DATE      = LV_DAT
              DAYS      = 05
              MONTHS    = 00
              SIGNUM    = '+'
              YEARS     = 00
            IMPORTING
              CALC_DATE = LA_DAT.
          IF LV_DAT NE '00000000' .
            IF SY-DATUM > LA_DAT .
              IF LV_NUM IS INITIAL.
                MESSAGE : 'Receipt not allowed days exceeded' TYPE 'E' DISPLAY LIKE 'I'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF IS_MKPF-BLART = 'WA' .
      DATA : LV_BLART TYPE MKPF-BLART.
      "LV_NUM TYPE ZTRANSIT_RELEASE-MBLNR .
      IF MBLNR IS NOT INITIAL.
        SELECT SINGLE BLART INTO LV_BLART FROM MKPF WHERE MBLNR = MBLNR AND MJAHR = MJAHR .
        IF LV_BLART EQ 'WA' .
          SELECT CPUDT_MKPF INTO LV_DAT UP TO 1 ROWS FROM MSEG WHERE MBLNR = MBLNR AND MJAHR = MJAHR   ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
          "  SELECT SINGLE MBLNR INTO LV_MBLNR FROM MSEG WHERE EBELN = I_MSEG-EBELN AND EBELP = I_MSEG-EBELP AND LINE_ID = I_MSEG-LINE_ID AND BUDAT_MKPF = I_MKPF-BLDAT .
          SELECT SINGLE MBLNR INTO LV_NUM FROM ZTRANSIT_RELEASE WHERE MBLNR = MBLNR .
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              DATE      = LV_DAT
              DAYS      = 05
              MONTHS    = 00
              SIGNUM    = '+'
              YEARS     = 00
            IMPORTING
              CALC_DATE = LA_DAT.
*   IF LV_DAT NE '00000000' .
          IF SY-DATUM > LA_DAT .
            IF LV_NUM IS INITIAL.
              MESSAGE : 'Receipt not allowed days exceeded' TYPE 'E' DISPLAY LIKE 'I'.
            ENDIF.
          ENDIF.
          "ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.
ENDMETHOD.


method IF_EX_MB_MIGO_BADI~PROPOSE_SERIALNUMBERS.
endmethod.


method IF_EX_MB_MIGO_BADI~PUBLISH_MATERIAL_ITEM.
endmethod.


method IF_EX_MB_MIGO_BADI~RESET.
endmethod.


method IF_EX_MB_MIGO_BADI~STATUS_AND_HEADER.
    DATA: MBLNR  TYPE MBLNR ,
          MJAHR TYPE MJAHR,
          XBLNR TYPE XBLNR.

    MBLNR =  IS_GOHEAD-MBLNR .
    MJAHR = IS_GOHEAD-MJAHR.
    XBLNR = IS_GOHEAD-XBLNR .
   EXPORT MBLNR FROM MBLNR TO MEMORY ID 'CTAB'.
   EXPORT MJAHR FROM MJAHR TO MEMORY ID 'CTAB1'.
   EXPORT XBLNR FROM XBLNR TO MEMORY ID 'CTAB2'.
"BREAK-POINT.
endmethod.
ENDCLASS.
