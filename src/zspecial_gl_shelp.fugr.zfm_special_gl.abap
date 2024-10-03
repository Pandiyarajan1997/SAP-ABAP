FUNCTION ZFM_SPECIAL_GL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------


  TYPES : BEGIN OF gs_T074T,
                        SPRAS TYPE T074T-SPRAS,
                        KOART TYPE T074T-KOART,
                        SHBKZ TYPE T074T-SHBKZ,
                        LTEXT TYPE T074T-LTEXT,
                      END OF gs_T074T.
DATA : it_T074T TYPE TABLE OF gs_T074T,
       wa_T074T TYPE gs_T074T.

IF callcontrol-step = 'SELECT'.

   "SELECT VENDOR_CODE FROM ZFREIGHT_HEADER INTO TABLE IT_ZFRE1 .

    SELECT SPRAS KOART SHBKZ LTEXT FROM T074T INTO TABLE it_T074T WHERE
              SPRAS EQ 'EN'  ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

   "   APPEND LINES OF IT_ZFRE1 TO IT_ZFRE.

   " SORT IT_ZFRE BY VENDOR_CODE.
    "   DELETE ADJACENT DUPLICATES FROM IT_ZFRE COMPARING VENDOR_CODE.
    CALL FUNCTION 'F4UT_RESULTS_MAP'
   EXPORTING
*   SOURCE_STRUCTURE         =
       apply_restrictions       = 'X'
      TABLES
        shlp_tab                 = shlp_tab
        record_tab               = record_tab
        source_tab               =  IT_T074T
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
    FREE IT_T074T.
    callcontrol-step = 'DISP'.

  ENDIF.




ENDFUNCTION.
