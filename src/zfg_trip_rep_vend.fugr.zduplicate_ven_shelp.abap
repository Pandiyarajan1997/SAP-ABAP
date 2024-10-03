FUNCTION ZDUPLICATE_VEN_SHELP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
   TYPES : BEGIN OF GS_FRE,
                VENDOR_CODE TYPE ZFREIGHT_HEADER-VENDOR_CODE ,
           END OF GS_FRE.

  DATA : IT_ZFRE TYPE TABLE OF GS_FRE.

* TYPES : BEGIN OF GS_FRE1,
*                VENDOR_CODE TYPE ZFREIGHT_HEADER-VENDOR_CODE ,
*           END OF GS_FRE1.
*
*  DATA : IT_ZFRE1 TYPE TABLE OF GS_FRE1.

        " gs_proj TYPE proj.

      "  SELECT VENDOR_CODE FROM ZFREIGHT_HEADER INTO TABLE IT_ZFRE1 .

 IF callcontrol-step = 'SELECT'.

   "SELECT VENDOR_CODE FROM ZFREIGHT_HEADER INTO TABLE IT_ZFRE1 .

    SELECT VENDOR_CODE FROM ZFREIGHT_HEADER INTO TABLE IT_ZFRE .

   "   APPEND LINES OF IT_ZFRE1 TO IT_ZFRE.

    SORT IT_ZFRE BY VENDOR_CODE.
       DELETE ADJACENT DUPLICATES FROM IT_ZFRE COMPARING VENDOR_CODE.
    CALL FUNCTION 'F4UT_RESULTS_MAP'
   EXPORTING
*   SOURCE_STRUCTURE         =
       apply_restrictions       = 'X'
      TABLES
        shlp_tab                 = shlp_tab
        record_tab               = record_tab
        source_tab               =  IT_ZFRE
      CHANGING
        shlp                     =  shlp
        callcontrol              =  callcontrol
* EXCEPTIONS
*   ILLEGAL_STRUCTURE        = 1
*   OTHERS                   = 2
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    FREE IT_ZFRE.
    callcontrol-step = 'DISP'.

  ENDIF.

ENDFUNCTION.
