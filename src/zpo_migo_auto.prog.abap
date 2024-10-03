*&---------------------------------------------------------------------*
*& Module Pool       ZPO_MIGO_AUTO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


INCLUDE ZMIGO_AUTO_TOP                          .    " global Data

* INCLUDE ZMIGO_AUTO_O01                          .  " PBO-Modules
* INCLUDE ZMIGO_AUTO_I01                          .  " PAI-Modules
* INCLUDE ZMIGO_AUTO_F01                          .  " FORM-Routines

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ZPO_MIGO_AUTO'.
  SET TITLEBAR 'ZPO_MIGO_AUTO'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: F_CODE TYPE SY-UCOMM.

  F_CODE = SY-UCOMM.

  CASE F_CODE.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'F_DIS'.
      CLEAR :WA_FINAL,WA_ZPO_MIGO_HEADER,WA_ZPO_MIGO_ITEM.
      REFRESH: IT_FINAL,IT_FCAT.
*    *PO_MIGO_AUTO Document Header Table
      IF LO_CONTAINER IS NOT INITIAL.
        CALL METHOD LO_CONTAINER->FREE( ) .
      ENDIF.

      SELECT ASN_NO
              EBELN
              MBLNR
              BUDAT
              XBLNR
              BKTXT
              FRGRL FROM ZPO_MIGO_HEADER INTO TABLE IT_ZPO_MIGO_HEADER WHERE ASN_NO = S_ASN AND FRGRL <> 'X'.

      IF SY-SUBRC = 0 AND IT_ZPO_MIGO_HEADER[] IS NOT INITIAL.
        SELECT
          ASN_NO
          EBELN
          EBELP
          MATNR
          MENGE
          ERFMG
          LSMNG
          MEINS
          INSMK
          LGORT
          CHARG
          LICHA
          VENDOR_INV
          FROM ZPO_MIGO_ITEM INTO TABLE IT_ZPO_MIGO_ITEM
          FOR ALL ENTRIES IN IT_ZPO_MIGO_HEADER WHERE EBELN = IT_ZPO_MIGO_HEADER-EBELN AND ASN_NO = IT_ZPO_MIGO_HEADER-ASN_NO.


        READ TABLE IT_ZPO_MIGO_HEADER INTO WA_ZPO_MIGO_HEADER INDEX 1. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
        PO_NUM = WA_ZPO_MIGO_HEADER-EBELN.

        LOOP AT IT_ZPO_MIGO_ITEM INTO WA_ZPO_MIGO_ITEM.
          WA_FINAL-ASN_NO = WA_ZPO_MIGO_ITEM-ASN_NO.
          WA_FINAL-EBELN = WA_ZPO_MIGO_ITEM-EBELN.
          WA_FINAL-EBELP = WA_ZPO_MIGO_ITEM-EBELP.
          WA_FINAL-MATNR = WA_ZPO_MIGO_ITEM-MATNR.
          WA_FINAL-MENGE = WA_ZPO_MIGO_ITEM-MENGE.
          WA_FINAL-ERFMG = WA_ZPO_MIGO_ITEM-ERFMG.
          WA_FINAL-LSMNG = WA_ZPO_MIGO_ITEM-LSMNG.
          WA_FINAL-MEINS = WA_ZPO_MIGO_ITEM-MEINS.
          WA_FINAL-INSMK = WA_ZPO_MIGO_ITEM-INSMK.
          WA_FINAL-LGORT = WA_ZPO_MIGO_ITEM-LGORT.
          WA_FINAL-CHARG = WA_ZPO_MIGO_ITEM-CHARG.
          WA_FINAL-LICHA = WA_ZPO_MIGO_ITEM-LICHA.
          WA_FINAL-VENDOR_INV = WA_ZPO_MIGO_ITEM-VENDOR_INV.
          APPEND WA_FINAL TO IT_FINAL.
        ENDLOOP.
        PO_DELI = WA_ZPO_MIGO_ITEM-VENDOR_INV.
        CLEAR :WA_FINAL,WA_ZPO_MIGO_ITEM.
      ELSE.
        MESSAGE 'No Data Exist....' TYPE 'E'.
      ENDIF.
      CREATE OBJECT LO_CONTAINER
       EXPORTING
*          PARENT                      =
         CONTAINER_NAME              = 'CONTAINER'
*          STYLE                       =
*          LIFETIME                    = lifetime_default
*          REPID                       =
*          DYNNR                       =
*          NO_AUTODEF_PROGID_DYNNR     =
*        EXCEPTIONS
*          CNTL_ERROR                  = 1
*          CNTL_SYSTEM_ERROR           = 2
*          CREATE_ERROR                = 3
*          LIFETIME_ERROR              = 4
*          LIFETIME_DYNPRO_DYNPRO_LINK = 5
*          OTHERS                      = 6
         .
      IF SY-SUBRC <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CREATE OBJECT LO_ALVGRID
        EXPORTING
*          I_SHELLSTYLE      = 0
*          I_LIFETIME        =
          I_PARENT          = LO_CONTAINER
*          I_APPL_EVENTS     = space
*          I_PARENTDBG       =
*          I_APPLOGPARENT    =
*          I_GRAPHICSPARENT  =
*          I_NAME            =
*          I_FCAT_COMPLETE   = SPACE
*        EXCEPTIONS
*          ERROR_CNTL_CREATE = 1
*          ERROR_CNTL_INIT   = 2
*          ERROR_CNTL_LINK   = 3
*          ERROR_DP_CREATE   = 4
*          OTHERS            = 5
          .
      IF SY-SUBRC <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      LS_LAYOUT-CWIDTH_OPT = 'X'.

      PERFORM BUILD_FIELDCATALOG.

      CALL METHOD LO_ALVGRID->SET_TABLE_FOR_FIRST_DISPLAY
             EXPORTING
*            I_BUFFER_ACTIVE               =
*            I_BYPASSING_BUFFER            =
*            I_CONSISTENCY_CHECK           =
*            I_STRUCTURE_NAME              = 'ZCUST_PAY_AUTO'
*            IS_VARIANT                    =
*            I_SAVE                        =
*            I_DEFAULT                     = 'X'
            IS_LAYOUT                     = LS_LAYOUT
*            IS_PRINT                      =
*            IT_SPECIAL_GROUPS             =
*            IT_TOOLBAR_EXCLUDING          =
*            IT_HYPERLINK                  =
*            IT_ALV_GRAPHICS               =
*            IT_EXCEPT_QINFO               =
*            IR_SALV_ADAPTER               =
             CHANGING
            IT_OUTTAB                     =  IT_FINAL
            IT_FIELDCATALOG               =  IT_FCAT                     " for Editable ALV
*            IT_SORT                       =
*            IT_FILTER                     =
*          EXCEPTIONS
*            INVALID_PARAMETER_COMBINATION = 1
*            PROGRAM_ERROR                 = 2
*            TOO_MANY_LINES                = 3
*            OTHERS                        = 4
                   .
      IF SY-SUBRC <> 0.
*         Implement suitable error handling here
      ENDIF.



    WHEN 'F_UP'.

      DATA: PO_NUM1 LIKE PO_NUM ,
            PO_DATE1 LIKE PO_DATE ,
            PO_DELI1 LIKE PO_DELI,
            LV_PO_DATE TYPE CHAR10,
            HEADER_TX1 LIKE HEADER_TX.

      PO_NUM1 =  PO_NUM.
      PO_DATE1 = PO_DATE .
      PO_DELI1 = PO_DELI.
      HEADER_TX1 = HEADER_TX.

      CONCATENATE PO_DATE1+6(2) PO_DATE1+4(2) PO_DATE1+0(4) INTO LV_PO_DATE SEPARATED BY '.' .

      REFRESH : IT_ZPO_MIGO_HEADER,IT_ZPO_MIGO_ITEM.

*PO_MIGO_AUTO Document Header Table
      SELECT ASN_NO
              EBELN
              MBLNR
              BUDAT
              XBLNR
              BKTXT
              FRGRL FROM ZPO_MIGO_HEADER INTO TABLE IT_ZPO_MIGO_HEADER WHERE ASN_NO = S_ASN AND FRGRL <> 'X'.

      IF SY-SUBRC = 0 AND IT_ZPO_MIGO_HEADER[] IS NOT INITIAL.
        SELECT
          ASN_NO
          EBELN
          EBELP
          MATNR
          MENGE
          ERFMG
          LSMNG
          MEINS
          INSMK
          LGORT
          CHARG
          LICHA
          VENDOR_INV
          FROM ZPO_MIGO_ITEM INTO TABLE IT_ZPO_MIGO_ITEM
          FOR ALL ENTRIES IN IT_ZPO_MIGO_HEADER WHERE EBELN = IT_ZPO_MIGO_HEADER-EBELN AND ASN_NO = IT_ZPO_MIGO_HEADER-ASN_NO.
*      ENDIF.

**Retrieve PO documents from Header (EKKO) table
*      IF SY-SUBRC = 0 AND IT_ZPO_MIGO_HEADER[] IS NOT INITIAL.
*        SELECT EBELN BEDAT FROM EKKO
*        INTO TABLE IT_EKKO FOR ALL ENTRIES IN IT_ZPO_MIGO_HEADER
*        WHERE EBELN = IT_ZPO_MIGO_HEADER-EBELN.
*      ENDIF.

*Retrieve PO document details from Item (EKPO) table
        IF SY-SUBRC = 0 AND IT_ZPO_MIGO_HEADER[] IS NOT INITIAL.

          SELECT EBELN EBELP LOEKZ ELIKZ MATNR WERKS LGORT MENGE MEINS INSMK
            FROM EKPO
            INTO TABLE IT_EKPO_DEL
            FOR ALL ENTRIES IN IT_ZPO_MIGO_HEADER
            WHERE EBELN = IT_ZPO_MIGO_HEADER-EBELN.

          SELECT EBELN EBELP LOEKZ ELIKZ MATNR WERKS LGORT MENGE MEINS INSMK
            FROM EKPO
            INTO TABLE IT_EKPO
            FOR ALL ENTRIES IN IT_ZPO_MIGO_ITEM
            WHERE EBELN = IT_ZPO_MIGO_ITEM-EBELN AND EBELP = IT_ZPO_MIGO_ITEM-EBELP.



          DATA: LV_DATE1 TYPE SY-DATUM ,
                LV_DATE TYPE CHAR10.

          LV_DATE1 = SY-DATUM  .
          CONCATENATE LV_DATE1+6(2) LV_DATE1+4(2) LV_DATE1+0(4) INTO LV_DATE SEPARATED BY '.' .

          "commented by ram
          LOOP AT IT_ZPO_MIGO_HEADER INTO WA_ZPO_MIGO_HEADER.
            DATA : LV(3) TYPE N .
            DATA : LV1(3) TYPE N,
                   ST_TYPE TYPE CHAR1.

            PERFORM BDC_DYNPRO      USING 'SAPLMIGO' '0001'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=OK_GO'.
            PERFORM BDC_FIELD       USING 'GODYNPRO-ACTION'
                                          'A01'.
            PERFORM BDC_FIELD       USING 'GODYNPRO-REFDOC'
                                          'R01'.
            PERFORM BDC_FIELD       USING 'GODEFAULT_TV-BWART'
                                          '101'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'GODYNPRO-PO_NUMBER'.
            PERFORM BDC_FIELD       USING 'GODYNPRO-PO_NUMBER'
                                          WA_ZPO_MIGO_HEADER-EBELN. "'4000000351'.
            PERFORM BDC_FIELD       USING 'GOHEAD-BLDAT'
                                           LV_PO_DATE. "'08.01.2021'.
            PERFORM BDC_FIELD       USING 'GOHEAD-BUDAT'
                                           LV_PO_DATE. "'08.01.2021'.
            PERFORM BDC_FIELD       USING 'GOHEAD-WEVER'
                                          '2'.
            PERFORM BDC_DYNPRO      USING 'SAPLMIGO' '0001'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=MIGO_OK_LINE_CLICK'.
            PERFORM BDC_FIELD       USING 'GODEFAULT_TV-BWART'
                                          '101'.
            PERFORM BDC_FIELD       USING 'GOHEAD-BLDAT'
                                          LV_PO_DATE. "'08.01.2021'.
            PERFORM BDC_FIELD       USING 'GOHEAD-BUDAT'
                                          LV_PO_DATE. "'08.01.2021'.
            PERFORM BDC_FIELD       USING 'GOHEAD-WEVER'
                                          '1'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'GOITEM-ZEILE(01)'.

            PERFORM BDC_FIELD       USING 'GODYNPRO-DETAIL_ZEILE'
                               '   1'.

*                 perform bdc_field       using 'GOITEM-ERFMG'
*                                          ENTERQTY. "'2,425.000'.
*            perform bdc_field       using 'GOITEM-LSMNG'
*                                          DELIVERYQTY. "'1.000'.
            PERFORM BDC_FIELD       USING 'GOITEM-BWART'
                                          '101'.
*          PERFORM BDC_FIELD       USING 'GOITEM-LGOBE'
*                                        'RM Stor.Loc.'.
            PERFORM BDC_DYNPRO      USING 'SAPLMIGO' '0001'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=OK_GO'.
            PERFORM BDC_FIELD       USING 'GODEFAULT_TV-BWART'
                                          '101'.
            PERFORM BDC_FIELD       USING 'GOHEAD-BLDAT'
                                          LV_PO_DATE."'08.01.2021'.
            PERFORM BDC_FIELD       USING 'GOHEAD-BUDAT'
                                          LV_PO_DATE. "'08.01.2021'.
            PERFORM BDC_FIELD       USING 'GOHEAD-WEVER'
                                          '1'.

            PERFORM BDC_FIELD       USING 'GOHEAD-LFSNR'
                                          PO_DELI1."'TEST1'.
            PERFORM BDC_FIELD       USING 'GOHEAD-BUDAT'
                                          LV_PO_DATE."'22.10.2020'.
            PERFORM BDC_FIELD       USING 'GOHEAD-BKTXT'
                                          HEADER_TX1."'WRWER6'.

            LOOP AT IT_ZPO_MIGO_ITEM INTO WA_ZPO_MIGO_ITEM WHERE EBELN = WA_ZPO_MIGO_HEADER-EBELN.
              READ TABLE IT_ZPO_MIGO_ITEM INTO WA_ZPO_MIGO_ITEM WITH KEY EBELN = WA_ZPO_MIGO_HEADER-EBELN EBELP = WA_ZPO_MIGO_ITEM-EBELP.
              READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_ZPO_MIGO_ITEM-EBELN EBELP = WA_ZPO_MIGO_ITEM-EBELP.

              IF WA_EKPO-LOEKZ <> 'S'.

*           PO_DELI1 = WA_ZPO_MIGO_ITEM-VENDOR_INV.
                WRITE WA_ZPO_MIGO_ITEM-LSMNG TO DELIVERYQTY .
                CONDENSE DELIVERYQTY.
                WRITE WA_ZPO_MIGO_ITEM-ERFMG TO ENTERQTY .
                CONDENSE ENTERQTY.

                LV = WA_ZPO_MIGO_ITEM-EBELP.
                DATA TEMP_EBELP TYPE ZPO_MIGO_ITEM-EBELP.
                IF WA_EKPO-EBELP > 10.
                  TEMP_EBELP = WA_EKPO-EBELP.
                  TEMP_EBELP = TEMP_EBELP - 10.
                  READ TABLE IT_EKPO_DEL INTO WA_EKPO_DEL WITH KEY EBELN = WA_ZPO_MIGO_ITEM-EBELN EBELP = TEMP_EBELP.
                  IF WA_EKPO_DEL-LOEKZ = 'S' or WA_EKPO_DEL-LOEKZ = 'L'.
                    LV = WA_EKPO_DEL-EBELP.
                  ENDIF.
                ENDIF.
                LV = LV / 10.
                LV1 = LV.

*            CASE WA_EKPO-INSMK.
**              WHEN ' '.
**                ST_TYPE = ' '.
*              WHEN 'X'.
*                ST_TYPE = '2'.
*              WHEN 'S'.
*                ST_TYPE = '3'.
*            ENDCASE.

                IF WA_EKPO-INSMK = 'S'.
                  ST_TYPE = '3'.
                ELSEIF WA_EKPO-INSMK = 'X'.
                  ST_TYPE = '2'.
                ELSE.
                  ST_TYPE = '2'.
                ENDIF.

                PERFORM BDC_FIELD       USING 'GODYNPRO-DETAIL_ZEILE'
                                              LV . "'   1'.""""""
                PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                              'GOITEM-LSMNG'.
*            perform bdc_field       using 'GOITEM-ERFME'
*                                          'L'.
*    perform bdc_field       using 'GOITEM-ERFMG'
*                                          ENTERQTY. "'2,425.000'.
*            perform bdc_field       using 'GOITEM-LSMNG'
*                                          DELIVERYQTY. "'1.000'.
                PERFORM BDC_FIELD       USING 'GOITEM-MIGO_ELIKZ'
                                              '1'.
                PERFORM BDC_FIELD       USING 'GOITEM-BWART'
                                              '101'.
*            PERFORM BDC_FIELD       USING 'GOITEM-MIGO_INSMK'
*                                         ST_TYPE." '2'.
                PERFORM BDC_FIELD       USING 'GOITEM-LGOBE'
                                              WA_EKPO-LGORT."'RM Stor.Loc.'.
                PERFORM BDC_DYNPRO      USING 'SAPLMIGO' '0001'.
                PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                              '=OK_GO'.
                PERFORM BDC_FIELD       USING 'GODEFAULT_TV-BWART'
                                              '101'.
                PERFORM BDC_FIELD       USING 'GOHEAD-BLDAT'
                                             LV_PO_DATE."'08.01.2021'.
                PERFORM BDC_FIELD       USING 'GOHEAD-BUDAT'
                                              LV_PO_DATE. "'08.01.2021'.

                PERFORM BDC_FIELD       USING 'GOHEAD-WEVER'
                                                 '1'.
                PERFORM BDC_FIELD       USING 'GODYNPRO-DETAIL_ZEILE'
                                               LV. "'   2'.  """"""
                PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                              'GOITEM-LSMEH'.
*            perform bdc_field       using 'GOITEM-ERFME'
*                                          'L'.
                PERFORM BDC_FIELD       USING 'GOITEM-ERFMG'
                                              ENTERQTY. "'2,425.000'.
                PERFORM BDC_FIELD       USING 'GOITEM-LSMNG'
                                              DELIVERYQTY. "'1.000'.
*            perform bdc_field       using 'GOITEM-LSMEH'
*                                          'L'.
*                PERFORM BDC_FIELD       USING 'GOITEM-MIGO_ELIKZ'
*                                              '1'.
                PERFORM BDC_FIELD       USING 'GOITEM-BWART'
                                              '101'.
*            PERFORM BDC_FIELD       USING 'GOITEM-MIGO_INSMK'
*                                          ST_TYPE." '2'.
                PERFORM BDC_FIELD       USING 'GOITEM-LGOBE'
                                               WA_EKPO-LGORT."'RM Stor.Loc.'.
                PERFORM BDC_FIELD       USING 'GODYNPRO-DETAIL_TAKE'
                                              'X'.

*PERFORM BDC_DYNPRO      USING 'SAPLMIGO' '0001'.
*PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                              '=GO_OK_EXCISE_ACTION'.

*          PERFORM BDC_DYNPRO      USING 'SAPLMIGO' '0001'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                        '=OK_GO'.
*          PERFORM BDC_FIELD       USING 'GODYNPRO-ACTION'
*                                        'A01'.
*          PERFORM BDC_FIELD       USING 'GODYNPRO-REFDOC'
*                                        'R01'.
*          PERFORM BDC_FIELD       USING 'GODEFAULT_TV-BWART'
*                                        '101'.
*          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                        'GODYNPRO-PO_NUMBER'.
*          PERFORM BDC_FIELD       USING 'GODYNPRO-PO_NUMBER'
*                                        WA_ZPO_MIGO_HEADER-EBELN."'4000000351'.
*          PERFORM BDC_FIELD       USING 'GOHEAD-BLDAT'
*                                        '28.11.2020'.
*          PERFORM BDC_FIELD       USING 'GOHEAD-BUDAT'
*                                        '28.11.2020'.
*          PERFORM BDC_FIELD       USING 'GOHEAD-WEVER'
*                                        '1'.
*          PERFORM BDC_DYNPRO      USING 'SAPLMIGO' '0001'.
*
*          PERFORM BDC_FIELD       USING 'GODEFAULT_TV-BWART'
*                                        '101'.
*          PERFORM BDC_FIELD       USING 'GOHEAD-BLDAT'
*                                        LV_PO_DATE."'22.10.2020'.
*          PERFORM BDC_FIELD       USING 'GOHEAD-LFSNR'
*                                        PO_DELI1."'TEST1'.
*          PERFORM BDC_FIELD       USING 'GOHEAD-BUDAT'
*                                        LV_PO_DATE."'22.10.2020'.
*          PERFORM BDC_FIELD       USING 'GOHEAD-BKTXT'
*                                        HEADER_TX1."'WRWER6'.
*
*
*          LOOP AT IT_ZPO_MIGO_ITEM INTO WA_ZPO_MIGO_ITEM WHERE EBELN = WA_ZPO_MIGO_HEADER-EBELN.
*          READ TABLE IT_ZPO_MIGO_ITEM INTO WA_ZPO_MIGO_ITEM WITH KEY EBELN = WA_ZPO_MIGO_HEADER-EBELN EBELP = WA_ZPO_MIGO_item-EBELP.
**           PO_DELI1 = WA_ZPO_MIGO_ITEM-VENDOR_INV.
*          WRITE WA_ZPO_MIGO_ITEM-LSMNG TO DELIVERYQTY .
*          CONDENSE DELIVERYQTY.
*          WRITE WA_ZPO_MIGO_ITEM-ERFMG TO ENTERQTY .
*          CONDENSE ENTERQTY.
*          LV = WA_ZPO_MIGO_ITEM-EBELP.
*          LV = LV / 10.
*          LV1 = LV.
*          shift LV left deleting leading '0'.
*PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                              '=OK_GO'.
**PERFORM BDC_FIELD       USING 'GOITEM-ZEILE'
**                                LV.
*              PERFORM BDC_FIELD       USING 'GOHEAD-WEVER'
*                                            LV."'1'.
*              PERFORM BDC_DYNPRO      USING 'SAPLMIGO' '0001'.
*              PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                            '=MIGO_OK_LINE_CLICK'.
*              PERFORM BDC_FIELD       USING 'GODYNPRO-DETAIL_ZEILE'
*                                            LV1."'   2'.
*               PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                        '=MIGO_OK_NEXT_IT'.
**              PERFORM BDC_FIELD       USING 'GOITEM-ERFME'
**                                            'L'.
*              PERFORM BDC_FIELD       USING 'GOITEM-ERFMG'
*                                            ENTERQTY."'5'.
*              PERFORM BDC_FIELD       USING 'GOITEM-LSMNG'
*                                            DELIVERYQTY."'5'.
*              PERFORM BDC_FIELD       USING 'GOITEM-MIGO_ELIKZ'
*                                            '1'.
*              PERFORM BDC_FIELD       USING 'GOITEM-BWART'
*                                            '101'.
*              PERFORM BDC_FIELD       USING 'GOITEM-MIGO_INSMK'
*                                            ' '."'2'.
*              PERFORM BDC_FIELD       USING 'GOITEM-LGOBE'
*                                            '0001'.
                PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                              'GOITEM-VFDAT'.
**              PERFORM BDC_FIELD       USING 'GOITEM-CHARG'
**                                            '0004109010'.
                PERFORM BDC_FIELD       USING 'GOITEM-LICHA'
                                              WA_ZPO_MIGO_ITEM-LICHA."'R109/11191'.
*            PERFORM BDC_FIELD       USING 'GOITEM-HSDAT'
*                                          LV_DATE."'28.11.2020'.
*            PERFORM BDC_FIELD       USING 'GOITEM-VFDAT'
*                                          LV_DATE."'28.11.2020'.
                PERFORM BDC_FIELD       USING 'GODYNPRO-DETAIL_TAKE'
                                              'X'.
                PERFORM BDC_DYNPRO      USING 'SAPLMIGO' '0001'.
*            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                          '=GO_OK_EXCISE_ACTION'.
*            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                            'J_1IEXHEAD-EXCISE_ACTION'.
*            PERFORM BDC_FIELD       USING 'J_1IEXHEAD-EXCISE_ACTION'
*                                            '06'.
*            PERFORM BDC_FIELD       USING 'J_1IEXHEAD-EXGRP'
*                                            '25'.
*              perform bdc_dynpro      using 'SAPMSSY0' '0120'.
*              perform bdc_field       using 'BDC_OKCODE'
*                              '=&ONT'.
              ELSE.
                MESSAGE 'PO Material Line Item Deleted....' TYPE 'W'.
              ENDIF.
              CLEAR : WA_EKPO,DELIVERYQTY,WA_EKPO_DEL,ENTERQTY,LV,WA_ZPO_MIGO_ITEM,ST_TYPE.
            ENDLOOP.
            CLEAR :WA_EKKO,WA_EKPO,WA_EKPO_DEL,DELIVERYQTY,ENTERQTY,LV,ST_TYPE.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                             '=OK_POST1'.
            CALL TRANSACTION 'MIGO' USING IT_BDCDATA MODE 'A'
                                            UPDATE 'S' MESSAGES INTO IT_BDCMSG.

            READ TABLE IT_BDCMSG INTO WA_MESSTAB WITH KEY MSGTYP = 'E'.
            IF SY-SUBRC = 0.
              P_LV_CHECK = 'E'.
              LV_MVAR5 = WA_MESSTAB-MSGV1.
              LV_MVAR6 = WA_MESSTAB-MSGV2.
              LV_MVAR7 = WA_MESSTAB-MSGV3.
              LV_MVAR8 = WA_MESSTAB-MSGV4.
              CALL FUNCTION 'MESSAGE_PREPARE'
                EXPORTING
                  LANGUAGE               = SY-LANGU
                  MSG_ID                 = WA_MESSTAB-MSGID
                  MSG_NO                 = WA_MESSTAB-MSGNR
                  MSG_VAR1               = LV_MVAR1
                  MSG_VAR2               = LV_MVAR2
                  MSG_VAR3               = LV_MVAR3
                  MSG_VAR4               = LV_MVAR4
                IMPORTING
                  MSG_TEXT               = WA_MESSAGE-MESSAGE
                EXCEPTIONS
                  FUNCTION_NOT_COMPLETED = 1
                  MESSAGE_NOT_FOUND      = 2
                  OTHERS                 = 3.

              WA_MESSAGE-MESS_TYPE = 'E'.
              WA_MESSAGE-STATUS = 'Error' .

              APPEND WA_MESSAGE TO IT_MESSAGE.
              CLEAR WA_MESSAGE.


            ENDIF.
            READ TABLE IT_BDCMSG INTO WA_MESSTAB WITH KEY MSGTYP = 'S' MSGNR = '012'.
            IF SY-SUBRC = 0.
              P_LV_CHECK = 'S'.
              LV_MVAR5 = WA_MESSTAB-MSGV1.
              LV_MVAR6 = WA_MESSTAB-MSGV2.
              LV_MVAR7 = WA_MESSTAB-MSGV3.
              LV_MVAR8 = WA_MESSTAB-MSGV4.

              CALL FUNCTION 'MESSAGE_PREPARE'
                EXPORTING
                  LANGUAGE               = SY-LANGU
                  MSG_ID                 = WA_MESSTAB-MSGID
                  MSG_NO                 = WA_MESSTAB-MSGNR
                  MSG_VAR1               = LV_MVAR5
                  MSG_VAR2               = LV_MVAR6
                  MSG_VAR3               = LV_MVAR7
                  MSG_VAR4               = LV_MVAR8
                IMPORTING
                  MSG_TEXT               = WA_MESSAGE-MESSAGE
                EXCEPTIONS
                  FUNCTION_NOT_COMPLETED = 1
                  MESSAGE_NOT_FOUND      = 2
                  OTHERS                 = 3.

*            WA_ZPO_MIGO_HEADER-MBLNR = LV_MVAR5.

*            modify ZPO_MIGO_HEADER from WA_ZPO_MIGO_HEADER .

              UPDATE ZPO_MIGO_HEADER SET  MBLNR = LV_MVAR5 BUDAT = SY-DATUM FRGRL = 'X' WHERE ASN_NO = S_ASN AND EBELN = WA_ZPO_MIGO_HEADER-EBELN.

            ENDIF.

            REFRESH : IT_BDCDATA,IT_BDCMSG,IT_MESSAGE,IT_FCAT.
            CLEAR : IT_BDCDATA,WA_EKKO,WA_EKPO,WA_EKPO_DEL,WA_EKKO,WA_EKPO,WA_ZPO_MIGO_HEADER,WA_ZPO_MIGO_ITEM,DELIVERYQTY,ENTERQTY,LV,ST_TYPE.
          ENDLOOP.

        ENDIF.
      ELSE.
        MESSAGE 'No Data Exist....' TYPE 'E'.
      ENDIF.
  ENDCASE.
  REFRESH : IT_BDCDATA,IT_BDCMSG,IT_MESSAGE,IT_FCAT.
  CLEAR : IT_BDCDATA,WA_EKKO,WA_EKPO,WA_EKKO,WA_EKPO_DEL,WA_EKPO,DELIVERYQTY,ENTERQTY,LV,ST_TYPE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG.

  WA_FCAT-FIELDNAME = 'ASN_NO'.
  WA_FCAT-SCRTEXT_L = 'ASN NUMBER'.
  WA_FCAT-COL_POS = '1'.
  WA_FCAT-OUTPUTLEN = 40.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'EBELN'.
  WA_FCAT-SCRTEXT_L = 'PO NUMBER'.
  WA_FCAT-COL_POS = '2'.
  WA_FCAT-OUTPUTLEN = 15.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'EBELP'.
  WA_FCAT-SCRTEXT_L = 'Item'.
  WA_FCAT-COL_POS = '3'.
  WA_FCAT-OUTPUTLEN = 5.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'MATNR'.
  WA_FCAT-SCRTEXT_L = 'Material Code'.
  WA_FCAT-COL_POS = '4'.
  WA_FCAT-OUTPUTLEN = 15.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'MENGE'.
  WA_FCAT-SCRTEXT_L = 'Quantity'.
  WA_FCAT-COL_POS = '5'.
  WA_FCAT-OUTPUTLEN = 5.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'ERFMG'.
  WA_FCAT-SCRTEXT_L = 'Quantity UnE'.
  WA_FCAT-COL_POS = '6'.
  WA_FCAT-OUTPUTLEN = 5.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'LSMNG'.
  WA_FCAT-SCRTEXT_L = 'Del. Note Qty'.
  WA_FCAT-COL_POS = '7'.
  WA_FCAT-OUTPUTLEN = 5.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'MEINS'.
  WA_FCAT-SCRTEXT_L = 'Base Unit'.
  WA_FCAT-COL_POS = '8'.
  WA_FCAT-OUTPUTLEN = 5.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

*  WA_FCAT-FIELDNAME = 'INSMK'.
*  WA_FCAT-SCRTEXT_L = 'Stock Type'.
*  WA_FCAT-COL_POS = '8'.
*  WA_FCAT-OUTPUTLEN = 10.
*  APPEND WA_FCAT TO IT_FCAT.
*  CLEAR WA_FCAT.

*  WA_FCAT-FIELDNAME = 'LGORT'.
*  WA_FCAT-SCRTEXT_L = 'S.LOC'.
*  WA_FCAT-COL_POS = '9'.
*  WA_FCAT-OUTPUTLEN = 10.
*  APPEND WA_FCAT TO IT_FCAT.
*  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'CHARG'.
  WA_FCAT-SCRTEXT_L = 'BATCH'.
  WA_FCAT-COL_POS = '9'.
  WA_FCAT-OUTPUTLEN = 10.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'LICHA'.
  WA_FCAT-SCRTEXT_L = 'V.BATCH'.
  WA_FCAT-COL_POS = '10'.
  WA_FCAT-OUTPUTLEN = 10.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'VENDOR_INV'.
  WA_FCAT-SCRTEXT_L = 'Vendor Invoice'.
  WA_FCAT-COL_POS = '11'.
  WA_FCAT-OUTPUTLEN = 15.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.
ENDFORM.                    "BUILD_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO  USING PROGRAM DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = PROGRAM.
  IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND  IT_BDCDATA.
ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM BDC_FIELD  USING FNAM FVAL.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = FNAM.
  IT_BDCDATA-FVAL = FVAL.
  APPEND  IT_BDCDATA.
ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Module  HIDE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HIDE OUTPUT.
  IF S_ASN IS INITIAL.
    LOOP AT SCREEN.

      IF SCREEN-GROUP1 = 'POH'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " HIDE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPALYDATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE DISPALYDATA INPUT.
* LOOP AT SCREEN.
*    IF SCREEN-GROUP1 = 'POH'.
*      screen-invisible = '0'.     "this one works
*      screen-input     = '1'.     "this one works
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.
*ENDMODULE.                 " DISPALYDATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_DATE INPUT.
  CALL FUNCTION 'F4_DATE'
   EXPORTING
     DATE_FOR_FIRST_MONTH               = PO_DATE
     DISPLAY                            = ' '
*     FACTORY_CALENDAR_ID                = ' '
*     GREGORIAN_CALENDAR_FLAG            = ' '
*     HOLIDAY_CALENDAR_ID                = ' '
*     PROGNAME_FOR_FIRST_MONTH           = ' '
*     DATE_POSITION                      = ' '
   IMPORTING
     SELECT_DATE                        = PO_DATE
*     SELECT_WEEK                        =
*     SELECT_WEEK_BEGIN                  =
*     SELECT_WEEK_END                    =
*   EXCEPTIONS
*     CALENDAR_BUFFER_NOT_LOADABLE       = 1
*     DATE_AFTER_RANGE                   = 2
*     DATE_BEFORE_RANGE                  = 3
*     DATE_INVALID                       = 4
*     FACTORY_CALENDAR_NOT_FOUND         = 5
*     HOLIDAY_CALENDAR_NOT_FOUND         = 6
*     PARAMETER_CONFLICT                 = 7
*     OTHERS                             = 8
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDMODULE.                 " F4_DATE  INPUT
