*&---------------------------------------------------------------------*
*& Report  ZKP_MM_DELIVERY_CHALLAN
*&
*&---------------------------------------------------------------------*

REPORT zsd_branch_invoice.
*&---------------------------------------------------------------------*
*& Report  ZSD_BRANCH_INVOICE
*&
*&---------------------------------------------------------------------*
*&Functional                   : Santhosh   & Govind                            *
*& Developer                   : Govindarajan.M                        *
*& Created On                  : 05 Apr 2014                           *
*& Company                     : Sheenlac Paints Pvt. Ltd              *
*& Title                       : Stock Transper Invoice                *
*& Report Name                 : ZSD_BRANCH_INVOICE                    *
*& Development Id              : kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :  DEVK902835                           *
*& Related Information         : Stock Transper Invoice W/Wo Excise    *
*&---------------------------------------------------------------------*

TABLES : mkpf,   mseg.
*=================================================================================================
*    Type Declarations
*=================================================================================================
DATA : wa_mkpf   TYPE  mkpf.
DATA : wa_mseg   TYPE  mseg.
DATA : wa_mseg1   TYPE  mseg.
DATA : ws_mseg   TYPE  mseg.
DATA : wa_vbrp TYPE vbrp.
DATA : wa_vbrk TYPE vbrk.
DATA : wa_j_1irg23d TYPE j_1irg23d.
DATA : is_j_1irg23d TYPE j_1irg23d.
DATA : wa_j_1iexchdr TYPE j_1iexchdr.
DATA : wa_toplnt  TYPE  t001w.
DATA : wa_frplnt  TYPE  t001w.
DATA : control_param  TYPE ssfctrlop .
DATA : fm_get TYPE rs38l_fnam.
DATA : it_mkpf   TYPE  TABLE  OF mkpf WITH HEADER LINE.
DATA : it_mseg   TYPE  TABLE  OF mseg WITH HEADER LINE.
DATA : it_mseg1   TYPE  TABLE  OF mseg WITH HEADER LINE.
DATA : it_vbrk TYPE TABLE OF vbrk WITH HEADER LINE.
DATA : it_vbrp TYPE TABLE OF vbrp WITH HEADER LINE.
DATA : it_j_1irg23d TYPE TABLE OF j_1irg23d WITH HEADER LINE.
DATA : it_j_1iexchdr TYPE TABLE OF j_1iexchdr WITH HEADER LINE.
DATA : it1_j_1iexchdr TYPE TABLE OF j_1iexchdr WITH HEADER LINE.
DATA :   price TYPE j_1irg23d-exbed.


*=================================================================================================
*     Selection screens
*=================================================================================================
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : bukrs TYPE mseg-bukrs NO-DISPLAY.
  PARAMETERS : gmblnr  TYPE   mkpf-mblnr OBLIGATORY."NO INTERVALS .
*SELECT-OPTIONS : GMJAHR  FOR  MKPF-MJAHR.
  PARAMETERS : gmjahr LIKE mkpf-mjahr OBLIGATORY.
*PARAMETERS : GUMWRK LIKE MSEG-UMWRK.
*PARAMETERS : GWERKS LIKE MSEG-WERKS.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: com_inv  RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND ucom,
              exci_inv RADIOBUTTON GROUP g1,
              amazon   RADIOBUTTON GROUP g1.
  PARAMETERS:  noroo AS CHECKBOX.
  PARAMETERS:  cwh AS CHECKBOX.
  PARAMETERS:  cwh_to AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b2.

*=================================================================================================
*    get  data from mkpf
*=================================================================================================
START-OF-SELECTION.

*  BREAK-POINT.
  IF com_inv = 'X'.
    REFRESH it_mkpf.
    SELECT * FROM mkpf INTO CORRESPONDING FIELDS OF TABLE it_mkpf
             WHERE mblnr = gmblnr AND mjahr = gmjahr.

    IF it_mkpf[]  IS   INITIAL.
      MESSAGE : 'Your Selection criteria not matching' TYPE 'I'.
      LEAVE SCREEN.
    ENDIF.

    SELECT * FROM mseg  INTO CORRESPONDING FIELDS OF TABLE it_mseg FOR ALL ENTRIES IN it_mkpf
         WHERE mblnr = it_mkpf-mblnr AND mjahr = it_mkpf-mjahr AND  bwart = '351' AND shkzg = 'H' .

    IF it_mseg[] IS INITIAL. " Added By Govind 0n 04.04.204

      SELECT * FROM mseg  INTO CORRESPONDING FIELDS OF TABLE it_mseg FOR ALL ENTRIES IN it_mkpf
           WHERE mblnr = it_mkpf-mblnr AND mjahr = it_mkpf-mjahr AND  bwart = '641' AND shkzg = 'H' .
    ENDIF.

    """"""""""""""""""""""""""""""""ADDED  BY RAM ON FOR 352 MOVEMENT TYPE 4/2/2016

    SELECT * FROM mseg
         INTO CORRESPONDING FIELDS OF TABLE it_mseg1 " FOR ALL ENTRIES IN IT_MKPF
            WHERE smbln = gmblnr   AND  bwart = '352' AND shkzg = 'S'. " Changed By Govind On 30.04.2014 'S' In to 'H'

    IF it_mseg1 IS INITIAL.
      LOOP AT it_mseg INTO wa_mseg.
        LOOP AT it_mseg1 INTO wa_mseg1.
          DELETE it_mseg WHERE mblnr = wa_mseg1-smbln AND matnr = wa_mseg1-matnr AND menge = wa_mseg1-menge .
        ENDLOOP.
      ENDLOOP .
    ENDIF.
    """""""""""""""""""""""""""""ENDED BY RAM ON 4/2/2016

    IF it_mseg[]  IS   INITIAL.

      MESSAGE : 'No line item found - Check your entry ' TYPE 'I'.
      LEAVE SCREEN.
    ENDIF.

  ELSEIF exci_inv = 'X'.

    REFRESH : it_mkpf[],it_j_1irg23d[],
     it_mseg[].

    SELECT * FROM mkpf INTO CORRESPONDING FIELDS OF TABLE it_mkpf
             WHERE mblnr = gmblnr AND mjahr = gmjahr.

    IF it_mkpf[]  IS  INITIAL.
      MESSAGE : 'Your Selection criteria not matching' TYPE 'I'.
      LEAVE SCREEN.
    ENDIF.

    SELECT * FROM mseg  INTO CORRESPONDING FIELDS OF TABLE it_mseg FOR ALL ENTRIES IN it_mkpf
         WHERE mblnr = it_mkpf-mblnr   AND mblnr = gmblnr AND mjahr = it_mkpf-mjahr AND  bwart = '351' AND shkzg = 'S'. " Changed By Govind On 30.04.2014 'S' In to 'H'

    IF it_mseg[]  IS   INITIAL. " Added By Govind 0n 04.04.204

      SELECT * FROM mseg  INTO CORRESPONDING FIELDS OF TABLE it_mseg FOR ALL ENTRIES IN it_mkpf
           WHERE mblnr = it_mkpf-mblnr  AND mblnr = gmblnr  AND mjahr = it_mkpf-mjahr AND  bwart = '641' AND shkzg = 'S'.
    ENDIF.

    """"""""""""""""""""""""""""""""""""ADDED  BY RAM FOR 352 MOVEMENT TYPE ON 4/2/2016

    SELECT * FROM mseg
          INTO CORRESPONDING FIELDS OF TABLE it_mseg1 " FOR ALL ENTRIES IN IT_MKPF
             WHERE smbln = gmblnr   AND  bwart = '352' AND shkzg = 'S' . " Changed By Govind On 30.04.2014 'S' In to 'H'
    IF it_mseg1 IS INITIAL.

      LOOP AT it_mseg INTO wa_mseg.
        LOOP AT it_mseg1 INTO wa_mseg1.
          DELETE it_mseg WHERE mblnr = wa_mseg1-smbln AND matnr = wa_mseg1-matnr AND menge = wa_mseg1-menge .
        ENDLOOP.
      ENDLOOP .
    ENDIF.
    """""""""""""""""""""""""""""ENDED BY RAM ON 4/2/2016

    IF sy-subrc = 0.

      SELECT * FROM j_1irg23d INTO CORRESPONDING FIELDS OF TABLE it_j_1irg23d FOR ALL ENTRIES IN it_mkpf
        WHERE mblnr = it_mkpf-mblnr AND    mjahr = it_mkpf-mjahr AND  mjahr =  gmjahr  .
    ENDIF.



    IF it_j_1irg23d[] IS NOT INITIAL.
      SELECT * FROM j_1iexchdr INTO CORRESPONDING FIELDS OF TABLE it_j_1iexchdr FOR ALL ENTRIES IN it_j_1irg23d
      WHERE exnum = it_j_1irg23d-exnum  AND docyr = gmjahr .
    ENDIF.



    "ADDED BY RAM ON 12/1

    IF it_j_1iexchdr[] IS INITIAL.
      DATA : lv_year TYPE char4 .
      lv_year = gmjahr - 1.
      SELECT * FROM j_1iexchdr INTO CORRESPONDING FIELDS OF TABLE it_j_1iexchdr FOR ALL ENTRIES IN it_j_1irg23d
      WHERE exnum = it_j_1irg23d-exnum  AND docyr = lv_year  .
    ENDIF.

    "ENDED BY RAM ON 12/1


    IF sy-subrc = 0.
      SELECT * FROM vbrk INTO CORRESPONDING FIELDS OF TABLE it_vbrk FOR ALL ENTRIES IN it_j_1iexchdr
      WHERE vbeln = it_j_1iexchdr-rdoc AND   rfbsk <> 'E' .
    ENDIF.

    IF it_vbrk[] IS   NOT INITIAL .
      SELECT  * FROM vbrp INTO CORRESPONDING FIELDS OF TABLE it_vbrp FOR ALL ENTRIES IN it_vbrk
         WHERE vbeln = it_vbrk-vbeln AND netwr > 0 .
    ENDIF.

*LOOP AT IT_J_1IRG23D INTO IS_J_1IRG23D WHERE MBLNR = IT_MSEG-MBLNR.
*
*  IS_J_1IRG23D-EXBED = IS_J_1IRG23D-EXBED.
* MODIFY IT_J_1IRG23D FROM IS_J_1IRG23D.
*  ENDLOOP.

    IF it_mseg[]  IS   INITIAL.

      MESSAGE : 'No line item found - Check your entry ' TYPE 'I'.
      LEAVE SCREEN.
    ENDIF.

  ENDIF.


  IF com_inv = 'X'.
    CASE it_mseg-bwart .
      WHEN '351'.
        LOOP AT it_mseg  INTO wa_mseg.
          SELECT * UP TO 1 ROWS FROM  mseg INTO ws_mseg  WHERE   mblnr = wa_mseg-mblnr AND mjahr = wa_mseg-mjahr AND werks = wa_mseg-umwrk
                                                        AND   line_id = wa_mseg-parent_id AND bwart = '351' AND shkzg = 'H'  ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation


          IF  sy-subrc = 0.
            wa_mseg-charg =  ws_mseg-charg.
            IF wa_mseg-dmbtr = '0'.
              wa_mseg-dmbtr = ws_mseg-dmbtr.
              MODIFY it_mseg FROM wa_mseg TRANSPORTING charg dmbtr.
            ELSE .
              MODIFY it_mseg FROM wa_mseg TRANSPORTING charg.
            ENDIF.
          ENDIF.
          CLEAR wa_mseg.

        ENDLOOP.

    ENDCASE.

    CASE it_mseg-bwart . " Added By Govind 0n 04.04.204
      WHEN '641'.
        LOOP AT it_mseg  INTO wa_mseg.
          SELECT * UP TO 1 ROWS FROM  mseg INTO ws_mseg  WHERE   mblnr = wa_mseg-mblnr AND mjahr = wa_mseg-mjahr AND werks = wa_mseg-umwrk
                                                           AND   line_id = wa_mseg-parent_id AND bwart = '641' AND shkzg = 'H'  ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
          IF  sy-subrc = 0.
            wa_mseg-charg =  ws_mseg-charg.
            IF wa_mseg-dmbtr = '0'.
              wa_mseg-dmbtr = ws_mseg-dmbtr.
              MODIFY it_mseg FROM wa_mseg TRANSPORTING charg dmbtr.
            ELSE.
              MODIFY it_mseg FROM wa_mseg TRANSPORTING charg.
            ENDIF.
          ENDIF.
          CLEAR wa_mseg.
        ENDLOOP.
    ENDCASE.


  ELSEIF exci_inv = 'X'.
    CASE it_mseg-bwart .
      WHEN '351'.

        LOOP AT it_mseg  INTO wa_mseg.

          SELECT * UP TO 1 ROWS FROM  mseg INTO ws_mseg  WHERE   mblnr = wa_mseg-mblnr AND mjahr = wa_mseg-mjahr AND werks = wa_mseg-umwrk
                                                       AND   line_id = wa_mseg-parent_id AND bwart = '351' AND shkzg = 'H'  ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
          IF  sy-subrc = 0.
            IF wa_mseg-dmbtr = '0'.
              wa_mseg-dmbtr = ws_mseg-dmbtr.
              MODIFY it_mseg FROM wa_mseg TRANSPORTING charg dmbtr.
            ELSE.
              MODIFY it_mseg FROM wa_mseg TRANSPORTING charg.
            ENDIF.
          ENDIF.
          CLEAR wa_mseg.

        ENDLOOP.
    ENDCASE.


  ENDIF.

  CASE it_mseg-bwart . " Added By Govind 0n 04.04.204
    WHEN '641'.
      LOOP AT it_mseg  INTO wa_mseg.
        SELECT * UP TO 1 ROWS FROM  mseg INTO ws_mseg  WHERE   mblnr = wa_mseg-mblnr AND mjahr = wa_mseg-mjahr AND werks = wa_mseg-umwrk
                                                         AND   line_id = wa_mseg-parent_id AND bwart = '641' AND shkzg = 'H'  ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
        IF  sy-subrc = 0.
          wa_mseg-charg =  ws_mseg-charg.
          IF wa_mseg-dmbtr = '0'.
            wa_mseg-dmbtr = ws_mseg-dmbtr.
            MODIFY it_mseg FROM wa_mseg TRANSPORTING charg dmbtr.
          ELSE.
            MODIFY it_mseg FROM wa_mseg TRANSPORTING charg.
          ENDIF.
        ENDIF.
        CLEAR wa_mseg.
      ENDLOOP.
  ENDCASE.



*  BREAK-POINT.
*=================================================================================================
*     calling smartforms
*=================================================================================================
  IF amazon = abap_true.

    DATA : lo_main TYPE REF TO zcl_dms_einvoice_process.
    CREATE OBJECT lo_main.
    CALL METHOD lo_main->generate_pdf_sto_amazon
      EXPORTING
        mblnr = gmblnr
        mjahr = gmjahr
*                                     IMPORTING
*       message =
      .
  ELSE.
    IF noroo <> 'X' AND cwh <> 'X' AND cwh_to <> 'X'.
      IF com_inv = 'X'.
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname = 'YSD_BRANCH_TRANS_INVOICE2'
          IMPORTING
            fm_name  = fm_get.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ELSEIF exci_inv = 'X'.
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname = 'YSD_BRANCH_TRANS_EXINV'
          IMPORTING
            fm_name  = fm_get.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

      CALL FUNCTION fm_get
        EXPORTING
          control_parameters = control_param
*         WA_T001W           = WA_FRPLNT
*         IS_T001W           = WA_TOPLNT
*         WA_MKPF            = WA_MKPF
        TABLES
          it_mseg            = it_mseg
          it_mkpf            = it_mkpf
          it_vbrp            = it_vbrp
          it_vbrk            = it_vbrk
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
*         USER_CANCELED      = 4
*         OTHERS             = 5
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ELSEIF  noroo = 'X' AND cwh <> 'X' AND cwh_to <> 'X'.
      IF com_inv = 'X'.
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname = 'YSD_BRANCH_NOROO_INVOICE'
          IMPORTING
            fm_name  = fm_get.
        IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ELSEIF exci_inv = 'X'.
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname = 'YSD_BRANCH_NOROO_EXINV'
          IMPORTING
            fm_name  = fm_get.
        IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

      CALL FUNCTION fm_get
        EXPORTING
          control_parameters = control_param
*         WA_T001W           = WA_FRPLNT
*         IS_T001W           = WA_TOPLNT
*         WA_MKPF            = WA_MKPF
        TABLES
          it_mseg            = it_mseg
          it_mkpf            = it_mkpf
          it_vbrp            = it_vbrp
          it_vbrk            = it_vbrk
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
*         USER_CANCELED      = 4
*         OTHERS             = 5
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ELSEIF cwh = 'X' AND noroo <> 'X' AND cwh_to <> 'X' .

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname = 'YSD_CWH_BRANCH_TRANS_INVOICE2'
        IMPORTING
          fm_name  = fm_get.
      IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION fm_get
        EXPORTING
          control_parameters = control_param
*         WA_T001W           = WA_FRPLNT
*         IS_T001W           = WA_TOPLNT
*         WA_MKPF            = WA_MKPF
        TABLES
          it_mseg            = it_mseg
          it_mkpf            = it_mkpf
          it_vbrp            = it_vbrp
          it_vbrk            = it_vbrk
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
*         USER_CANCELED      = 4
*         OTHERS             = 5
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ELSEIF cwh_to = 'X' AND noroo <> 'X' AND cwh <> 'X' .
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname = 'YSD_BRANCH_TRANS_INVOICE_TOCWH'
        IMPORTING
          fm_name  = fm_get.
      IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION fm_get
        EXPORTING
          control_parameters = control_param
*         WA_T001W           = WA_FRPLNT
*         IS_T001W           = WA_TOPLNT
*         WA_MKPF            = WA_MKPF
        TABLES
          it_mseg            = it_mseg
          it_mkpf            = it_mkpf
          it_vbrp            = it_vbrp
          it_vbrk            = it_vbrk
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
*         USER_CANCELED      = 4
*         OTHERS             = 5
        .
      IF sy-subrc <> 0.
*        Implement suitable error handling here
      ENDIF.

    ELSE.

      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
        EXPORTING
          titel        = 'Message box '
          textline1    = 'Please Select Anyone Option'
          textline2    = 'Sheenlac Noroo / FROM-(CWH Vadakanallur) / TO-(CWH Vadakanallur)'
          start_column = 25
          start_row    = 10.

*    MESSAGE TYPE 'I'.
      SUBMIT zsd_branch_invoice VIA SELECTION-SCREEN.

    ENDIF.
  ENDIF.
**=================================================================================================
**     smartform open
**=================================================================================================
*
* CONTROL_PARAM-NO_OPEN = 'X'.
* CONTROL_PARAM-NO_CLOSE = 'X'.
*
* CALL FUNCTION 'SSF_OPEN'
* EXPORTING
*   CONTROL_PARAMETERS       = CONTROL_PARAM  .
*
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.


**=================================================================================================
**     calling function to smartforms with loop
**=================================================================================================
*LOOP AT IT_MKPF INTO WA_MKPF.
*     REFRESH IT_MSEG.
*   CLEAR : WA_TOPLNT, WA_FRPLNT.
*   PERFORM  GET_DATA.
*
*   CALL FUNCTION  FM_GET
*     EXPORTING
*       CONTROL_PARAMETERS         = CONTROL_PARAM
*       WA_T001W                   = WA_FRPLNT
*       IS_T001W                   = WA_TOPLNT
*       WA_MKPF                    = WA_MKPF
*     TABLES
*       IT_MSEG                    = IT_MSEG
**    EXCEPTIONS
**      FORMATTING_ERROR           = 1
**      INTERNAL_ERROR             = 2
**      SEND_ERROR                 = 3
**      USER_CANCELED              = 4
**      OTHERS                     = 5
*             .
*   IF SY-SUBRC <> 0.
** Implement suitable error handling here
*   ENDIF.
*ENDLOOP.



*CALL FUNCTION 'SSF_CLOSE'
** IMPORTING
**   JOB_OUTPUT_INFO        =
** EXCEPTIONS
**   FORMATTING_ERROR       = 1
**   INTERNAL_ERROR         = 2
**   SEND_ERROR             = 3
**   OTHERS                 = 4
*          .
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

*============================================================= ====================================
*     get data
*=================================================================================================

*FORM GET_DATA.
*  SELECT  *  FROM MSEG   INTO CORRESPONDING FIELDS OF TABLE IT_MSEG WHERE MBLNR = WA_MKPF-MBLNR AND  MJAHR = WA_MKPF-MJAHR AND SHKZG = 'S'.
*  READ  TABLE IT_MSEG INTO WA_MSEG WITH KEY MBLNR = WA_MKPF-MBLNR  MJAHR = WA_MKPF-MJAHR.
*  SELECT  SINGLE *  FROM T001W  INTO WA_TOPLNT  WHERE  WERKS = WA_MSEG-WERKS.
*  SELECT  SINGLE *  FROM T001W  INTO WA_FRPLNT  WHERE  WERKS = WA_MSEG-UMWRK.
*ENDFORM.
