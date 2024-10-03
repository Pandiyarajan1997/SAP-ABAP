**&---------------------------------------------------------------------*
**&  Include           ZXMBCU02
**&---------------------------------------------------------------------*

"Coding by Savariar requirment by Mr.Umapathy as on 09/06/2015


*BREAK-POINT.

IF sy-tcode = 'MIGO'.

  IF i_mseg-bwart EQ '101'.

    DATA : lv_vendtype TYPE j_1imovend-j_1ivtyp.

    DATA : gt_j_1imovend TYPE TABLE OF lfa1, " J_1IMOVEND Replaced With LFA1 Added by <IT-CAR Tool> during Code Remediation - tool
           wa_j_1imovend TYPE lfa1. " J_1IMOVEND Replaced With LFA1 Added by <IT-CAR Tool> during Code Remediation - tool

    SELECT * FROM lfa1 INTO TABLE gt_j_1imovend WHERE lifnr = i_mseg-lifnr.

    READ TABLE gt_j_1imovend INTO wa_j_1imovend WITH KEY lifnr = i_mseg-lifnr.
    IF sy-subrc = 0.
      lv_vendtype = wa_j_1imovend-j_1ivtyp.
    ENDIF.

    IF lv_vendtype EQ 'SD' OR lv_vendtype EQ 'DI' OR lv_vendtype EQ 'FD'.
*         MESSAGE 'Please Activate The MRP Indicator' TYPE 'W' DISPLAY LIKE 'E'.

      CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
        EXPORTING
          textline1 = 'Your Taken Recipt At-Dealer PO,So Please Activate The MRP Indicator Must'
          titel     = 'MRP Indicator Must'.

*      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
*       EXPORTING
*         TITEL              = 'MRP Indicator Must '
*         TEXTLINE1          = 'Your Taken Recipt At-Delear PO,So Please Activate The MRP Indicator Must'.

    ENDIF.
  ENDIF.


*BREAK-POINT.

  DATA : v_bsart TYPE ekko-bsart.
  DATA : v_lifnr TYPE ekko-lifnr.

  DATA : v_plant TYPE ekpo-werks.
  DATA : v_bill TYPE mkpf-frbnr.
  DATA:  v_year TYPE mkpf-mjahr.

  DATA : v_lfbnr TYPE mseg-lfbnr.

  DATA : v_vendor TYPE ekko-lifnr.
  DATA : v_werks TYPE mseg-mblnr.
  DATA : v_mblnr TYPE mseg-mblnr.

  DATA : bill TYPE ty_t_mseg.


  DATA : gt_ekko TYPE TABLE OF ekko,
         wa_ekko TYPE ekko.
  DATA: gt_ekpo TYPE TABLE OF ekpo,
        wa_ekpo TYPE ekpo.

  TYPES : BEGIN OF gs_mkpf,
            mblnr TYPE mkpf-mblnr,
            bktxt TYPE mkpf-bktxt,
            frbnr TYPE mkpf-frbnr,
          END OF gs_mkpf.

  DATA : gt_mkpf TYPE TABLE OF gs_mkpf,
         wa_mkpf TYPE gs_mkpf.


  TYPES : BEGIN OF gs_mseg,

            mblnr TYPE mseg-mblnr,
            mjahr TYPE mseg-mjahr,
            bwart TYPE mseg-bwart,
            werks TYPE mseg-werks,
            lifnr TYPE mseg-lifnr,
            ebeln TYPE mseg-ebeln,
            smbln TYPE mseg-smbln,
            lfbnr TYPE mseg-lfbnr,

            elikz TYPE mseg-elikz,

          END OF gs_mseg.

  DATA : gt_mseg TYPE TABLE OF gs_mseg,
         wa_mseg TYPE gs_mseg.


  TYPES : BEGIN OF gs_final,

            mblnr  TYPE mseg-mblnr,
            mjahr  TYPE mseg-mjahr,
            bwart  TYPE mseg-bwart,
            werks  TYPE mseg-werks,
            lifnr  TYPE mseg-lifnr,
            smbln  TYPE mseg-smbln,
            billad TYPE mkpf-frbnr,
            lfbnr  TYPE mseg-lfbnr,
            ebeln  TYPE mseg-ebeln,

            elikz  TYPE mseg-elikz,

          END OF gs_final.

  DATA : gt_final TYPE TABLE OF gs_final,
         wa_final TYPE gs_final.


  SELECT *  FROM ekko INTO TABLE  gt_ekko  WHERE ebeln = i_mseg-ebeln.


  IF gt_ekko[] IS NOT INITIAL.

    SELECT * FROM ekpo INTO TABLE gt_ekpo FOR ALL ENTRIES IN gt_ekko WHERE ebeln = gt_ekko-ebeln.

  ENDIF.

  IF i_mkpf-frbnr IS NOT INITIAL.

    SELECT mblnr bktxt frbnr FROM mkpf INTO TABLE gt_mkpf WHERE frbnr = i_mkpf-frbnr.

  ENDIF.

  IF gt_mkpf[] IS NOT INITIAL.

    SELECT
          mblnr
          mjahr
          bwart
          werks
          lifnr
          ebeln
          smbln
          lfbnr
          elikz

     FROM mseg INTO TABLE gt_mseg FOR ALL ENTRIES IN gt_mkpf WHERE mblnr = gt_mkpf-mblnr.

  ENDIF.


  LOOP AT gt_ekko INTO wa_ekko.

    SELECT SINGLE bsart FROM ekko INTO v_bsart WHERE ebeln = wa_ekko-ebeln.

    SELECT SINGLE lifnr FROM ekko INTO v_lifnr WHERE ebeln = wa_ekko-ebeln.

    CLEAR wa_ekko.

  ENDLOOP.

  LOOP AT gt_ekpo INTO wa_ekpo.

    SELECT SINGLE werks FROM ekpo INTO v_plant WHERE ebeln = wa_ekpo-ebeln.
    CLEAR wa_ekpo.

  ENDLOOP.



  LOOP AT gt_mkpf INTO wa_mkpf.

    wa_final-billad = wa_mkpf-frbnr.

    READ TABLE gt_mseg INTO wa_mseg WITH KEY mblnr = wa_mkpf-mblnr.

    IF sy-subrc = 0.

      wa_final-mblnr = wa_mseg-mblnr.
      wa_final-mjahr = wa_mseg-mjahr.
      wa_final-werks = wa_mseg-werks.
      wa_final-bwart = wa_mseg-bwart.
      wa_final-lifnr = wa_mseg-lifnr.
      wa_final-lfbnr = wa_mseg-lfbnr.
      wa_final-smbln = wa_mseg-smbln.

      wa_final-ebeln = wa_mseg-ebeln.
      wa_final-elikz = wa_mseg-elikz.

      v_werks = wa_mseg-werks.
      v_vendor = wa_mseg-lifnr.
      v_mblnr = wa_mseg-mblnr.

      SELECT SINGLE lfbnr FROM mseg INTO v_lfbnr WHERE mblnr = wa_mseg-mblnr.

      SELECT SINGLE mjahr FROM mseg INTO v_year WHERE mblnr = wa_mseg-mblnr.

    ENDIF.

    APPEND wa_final TO gt_final.


*    IF WA_FINAL-EBELN EQ I_MSEG-EBELN.
*
*      DELETE GT_FINAL WHERE LFBNR = V_LFBNR.
*
*    ENDIF.

    CLEAR wa_final.

  ENDLOOP.


  CASE v_bsart.

    WHEN 'ZNB'.

      IF i_mkpf-blart = 'WE'.

        IF i_mkpf-frbnr IS NOT INITIAL .

          IF i_mseg-bwart NE '102' AND i_mseg-bwart NE '162'.

            LOOP AT gt_final INTO wa_final.

              IF i_mkpf-frbnr EQ wa_final-billad AND wa_final-werks EQ v_plant AND wa_final-lifnr EQ v_lifnr AND wa_final-mjahr EQ v_year.

                MESSAGE 'Bill Of Lading Column In LR Number already Exists in the same vendor' TYPE 'E' DISPLAY LIKE 'I'.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

      ENDIF.


    WHEN 'ZIM'.

      IF i_mkpf-blart = 'WE'.

        IF i_mkpf-frbnr IS NOT INITIAL ."AND IS_MSEG-BWART EQ '101'.

          IF i_mseg-bwart NE '102'.

            LOOP AT gt_final INTO wa_final.

              IF i_mkpf-frbnr EQ wa_final-billad AND wa_final-werks EQ v_plant AND wa_final-lifnr EQ v_lifnr.

                MESSAGE 'Bill Of Lading Column In LR Number already Exists in the same vendor' TYPE 'E' DISPLAY LIKE 'I'.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

      ENDIF.

    WHEN 'ZSC'.

      IF i_mkpf-blart = 'WE'.

        IF i_mkpf-frbnr IS NOT INITIAL ."AND IS_MSEG-BWART EQ '101'.

          IF i_mseg-bwart NE '102' AND i_mseg-bwart NE '544'.

            LOOP AT gt_final INTO wa_final.

              IF i_mkpf-frbnr EQ wa_final-billad AND wa_final-werks EQ v_plant AND wa_final-lifnr EQ v_lifnr.

                MESSAGE 'Bill Of Lading Column In LR Number already Exists in the same vendor' TYPE 'E' DISPLAY LIKE 'I'.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

      ENDIF.



  ENDCASE.




ENDIF.

*------------------------------------------------------------------------------------------------------------------------------
*Requirment given by N.Umapathy / Technical Mr.Ram

IF sy-tcode EQ 'MIGO'.
  DATA  : lv_bal TYPE zsto_mhc-zprice .
  DATA : lv_per TYPE zsto_mhc-zprice .
  DATA : lv_cal TYPE zsto_mhc-zprice .
  DATA : lv_diff TYPE zsto_mhc-zprice.
  DATA : lv_ovd TYPE zsto_mhc-zprice.
  DATA : lv_qty TYPE mseg-menge.
  DATA: lv_amt TYPE char4 . "TYPE ZSTO_MHC-ZPRICE.
  DATA : msg1(400).

  SELECT SINGLE zprice FROM zsto_mhc INTO lv_per WHERE zmaterial = i_mseg-matnr AND zvendor = i_mseg-lifnr.

  IF lv_per IS NOT INITIAL .
    lv_bal = i_mseg-lsmng - i_mseg-erfmg .
    lv_cal = ( i_mseg-erfmg / i_mseg-lsmng ) * 100 .
    lv_diff = 100 - lv_cal.
    IF lv_per => lv_diff .
      CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
        EXPORTING
          textline1 = 'GR Received With in the Defined Tolerence For Vendor Evaporation'
          titel     = 'Freight Vendor Tolerance Detail'.
    ENDIF .
    IF lv_per  < lv_diff .
      lv_ovd = lv_diff - lv_per.
      lv_qty = ( i_mseg-lsmng * lv_ovd ) / 100.
      SELECT *  FROM ekko INTO TABLE  gt_ekko  WHERE ebeln = i_mseg-ebeln.
      IF gt_ekko[] IS NOT INITIAL.
        SELECT * FROM ekpo INTO TABLE gt_ekpo FOR ALL ENTRIES IN gt_ekko WHERE ebeln = gt_ekko-ebeln.
        LOOP AT gt_ekpo INTO wa_ekpo WHERE matnr = i_mseg-matnr .
          lv_amt = lv_qty * wa_ekpo-netpr.
        ENDLOOP .
      ENDIF.

      SHIFT i_mseg-matnr LEFT DELETING LEADING '0'.

      CONCATENATE 'GR Qty Exceed Tolerance ' i_mseg-matnr ' ,So debit the amt' lv_amt 'For Freight Vendor' INTO msg1 SEPARATED BY space.

      CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
        EXPORTING
          "          TEXTLINE1 = 'GR Qty Exceed The tolerence limit,so debit the amount for shortage ea at the freight vendor'
          textline1 = msg1
          titel     = 'Vendor Evaporation control Details for Freight Vendor'.

    ENDIF.
  ENDIF.
ENDIF.

*Requirment : At the time of MIGO receiving plant material price control as "s" price mandatory at material master level.

IF sy-tcode EQ 'MIGO'.

  IF i_mseg-bwart = '101'  .

    DATA : it_mbew TYPE TABLE OF mbew,
           wa_mbew TYPE mbew.
    DATA : msg2(400).

    SELECT * FROM mbew INTO TABLE it_mbew  WHERE matnr = i_mseg-matnr AND bwkey = i_mseg-werks .

    LOOP AT it_mbew INTO wa_mbew .
      IF wa_mbew-vprsv = 'S ' .
        IF wa_mbew-stprs IS INITIAL  .
          CONCATENATE i_mseg-matnr 'material master price as empty at receiving plant' i_mseg-werks  INTO msg2 SEPARATED BY space.
          MESSAGE msg2 TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.
      ENDIF.
    ENDLOOP .
  ENDIF .
ENDIF.
*  ----------------------------------------------------------------------------------------------------------------------
""""""""""""""""""""""""""""""""""""""""""""""""""""""


IF sy-tcode EQ 'MIGO'.
  IF v_bsart = 'ZSTO' .
    DATA : sgtxt TYPE bseg-sgtxt .

    TYPES : rfposxext TYPE TABLE OF bseg .
    DATA : it_sgtxt TYPE TABLE OF bseg .
    DATA : wa_sgtxt TYPE bseg .
    " UPDATE BSEG SET SGTXT = 'INTER COMPANY TRANSFER' WHERE EBELN = I_MSEG-EBELN .
    wa_sgtxt = 'INTER COMPANY TRANSFER' .
    " UPDATE RFPOSXEXT FROM WA_SGTXT . " TRANSPORTING SGTXT  . " TRANSPORTING SGTXT.
  ENDIF.
ENDIF.

"""""""""""""""""""""""""""""""""""""""""""""""""""""
"For restrict transit not more than 5days.

*DATA : LV_DAT TYPE MKPF-CPUDT ,
*       LA_DAT TYPE MKPF-CPUDT,
*       LV_BSA TYPE EKKO-BSART.
*
*DATA : LV_NUM TYPE ZTRANSIT_RELEASE-MBLNR .
*DATA : LV_MBLNR TYPE MSEG-MBLNR.
*  IF SY-TCODE EQ 'MIGO'.
*  IF I_MSEG-BWART = '101'  .
*   IF I_MKPF-BLART = 'WE' .
*         SELECT SINGLE BSART INTO LV_BSA FROM EKKO WHERE EBELN = I_MSEG-EBELN.
*      IF LV_BSA EQ 'UB' OR LV_BSA EQ 'ZUB' OR LV_BSA EQ 'ZSTO' .
*         SELECT SINGLE CPUDT INTO LV_DAT FROM MKPF WHERE XBLNR = I_MKPF-XBLNR .
*         SELECT SINGLE MBLNR INTO LV_NUM FROM ZTRANSIT_RELEASE WHERE MBLNR = I_MKPF-XBLNR .
*            CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*              EXPORTING
*            DATE      = LV_DAT
*            DAYS      = 05
*            MONTHS    = 00
*            SIGNUM    = '+'
*            YEARS     = 00
*             IMPORTING
*            CALC_DATE = LA_DAT .
*  IF LV_DAT NE '00000000' .
*   IF SY-DATUM > LA_DAT .
*      IF LV_NUM IS INITIAL.
*            MESSAGE : 'Receipt not allowed days exceeded' TYPE 'E' DISPLAY LIKE 'I'.
*      ENDIF.
*  ENDIF.
*   ENDIF.
*   ENDIF.
*   ENDIF.
*""""""""" For 351
* IF I_MKPF-BLART = 'WA' .
*  SELECT SINGLE CPUDT_MKPF INTO LV_DAT FROM MSEG WHERE EBELN = I_MSEG-EBELN AND EBELP = I_MSEG-EBELP AND LINE_ID = I_MSEG-LINE_ID .
*  SELECT SINGLE MBLNR INTO LV_MBLNR FROM MSEG WHERE EBELN = I_MSEG-EBELN AND EBELP = I_MSEG-EBELP AND LINE_ID = I_MSEG-LINE_ID AND BUDAT_MKPF = I_MKPF-BLDAT .
*  SELECT SINGLE MBLNR INTO LV_NUM FROM ZTRANSIT_RELEASE WHERE MBLNR = LV_MBLNR .
*   CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*    EXPORTING
*      DATE      = LV_DAT
*      DAYS      = 05
*      MONTHS    = 00
*      SIGNUM    = '+'
*      YEARS     = 00
*    IMPORTING
*      CALC_DATE = LA_DAT .
*   IF LV_DAT NE '00000000' .
*  IF SY-DATUM > LA_DAT .
*   IF LV_NUM IS INITIAL.
*       MESSAGE : 'Receipt not allowed days exceeded' TYPE 'E' DISPLAY LIKE 'I'.
*   ENDIF.
*   ENDIF.
*  ENDIF.
*ENDIF.
*ENDIF.
*ENDIF.

*******************************************************************************************
"Added By Samsudeen M on 28.04.2023
"Purpose: For Storing the Item wise text in Material Document Table MSEG
"Reference by: Balamurugan Cinthamani & Gopal Raja
IF e_sgtxt IS INITIAL.
  e_sgtxt = i_mseg-sgtxt.
ENDIF.
******************************************************************************************
"Added By: Samsudeen M on 17.05.2023
"Purpose: For Delivery Note Duplication it will throw error
"Referece by: Ramakrishnan J & Gopal Raja
DATA: lt_tab TYPE esp1_message_tab_type.
*DATA: ls_tab TYPE esp1_message_wa_type.
*DATA: l_msg TYPE string.
**Only for Purchase Order Based GRN this Check is Applicable
*IF i_mseg-bwart = '101' AND i_mseg-kzbew = 'B'.
*  SELECT SINGLE low FROM tvarvc
*    INTO @DATA(l_tvarvc)
*    WHERE name = 'SWITCH_CHECK_FOR_MIGO'
*    AND type = 'P'.
*  "If it is equal to X then only checks happen
*  IF l_tvarvc = abap_true.
*    SELECT SINGLE xblnr FROM mkpf
*      INTO @DATA(l_deliverynote)
*      WHERE xblnr = @i_mkpf-xblnr.
*    IF sy-subrc = 0.
*      CLEAR l_msg.
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*        EXPORTING
*          msgid               = 'ZMIGO'
*          msgnr               = '000'
*          msgv1               = l_deliverynote
*        IMPORTING
*          message_text_output = l_msg.
*      MESSAGE l_msg TYPE 'E' DISPLAY LIKE 'I'.
*    ENDIF.
*  ENDIF.
*ENDIF.
********************************************************************************************
