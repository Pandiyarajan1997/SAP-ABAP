FUNCTION ZBAPI_VENDOR_QM_REJECTION .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LIFNR) TYPE  LIFNR OPTIONAL
*"     VALUE(BUDAT_LOW) TYPE  BUDAT
*"     VALUE(BUDAT_HIGH) TYPE  BUDAT
*"  EXPORTING
*"     VALUE(RETURN) TYPE  BAPIRET2
*"  TABLES
*"      OUTPUT STRUCTURE  ZBAPI_QAREJ_OUTPUT
*"----------------------------------------------------------------------


  INCLUDE Z_QAREJECTION_REPORT_VARI_TOP.

data : wa_output TYPE ZBAPI_QAREJ_OUTPUT.

IF LIFNR IS NOT INITIAL.

  SELECT LIFNR
         MATNR
         WERKS
         BWART
         GRUND
         MBLNR
         ZEILE
         MJAHR
         BUDAT_MKPF
         EBELN
         EBELP
         CHARG
         MENGE
         MEINS
         TCODE2_MKPF FROM MSEG INTO TABLE IT_MSEG WHERE  LIFNR = LIFNR AND
                                                         BUDAT_MKPF >= BUDAT_LOW AND
                                                         BUDAT_MKPF <= BUDAT_HIGH AND
                                                         BWART = '101' AND TCODE2_MKPF = 'MIGO_GR'.
  ELSE.

    SELECT LIFNR
         MATNR
         WERKS
         BWART
         GRUND
         MBLNR
         ZEILE
         MJAHR
         BUDAT_MKPF
         EBELN
         EBELP
         CHARG
         MENGE
         MEINS
         TCODE2_MKPF FROM MSEG INTO TABLE IT_MSEG WHERE  BUDAT_MKPF >= BUDAT_LOW AND
                                                         BUDAT_MKPF <= BUDAT_HIGH AND
                                                         BWART = '101' AND TCODE2_MKPF = 'MIGO_GR'.

ENDIF.


  IF IT_MSEG IS NOT INITIAL.


    SELECT PRUEFLOS
           MBLNR
           MJAHR
           ZEILE FROM QAMB INTO TABLE IT_QAMB1
                 FOR ALL ENTRIES IN IT_MSEG
                 WHERE MBLNR = IT_MSEG-MBLNR AND
                       MJAHR = IT_MSEG-MJAHR AND
                       ZEILE = IT_MSEG-ZEILE.

    SELECT PRUEFLOS
         MBLNR
         MJAHR
         ZEILE FROM QAMB INTO TABLE IT_QAMB2
               FOR ALL ENTRIES IN IT_QAMB1
               WHERE PRUEFLOS = IT_QAMB1-PRUEFLOS.

      SELECT PRUEFLOS FROM QMEL INTO TABLE IT_QMEL
                      FOR ALL ENTRIES IN IT_QAMB2
                       WHERE PRUEFLOS = IT_QAMB2-PRUEFLOS AND
                             QMART = 'F2'.

    SELECT LIFNR
       MATNR
       WERKS
       BWART
       GRUND
       MBLNR
       ZEILE
       MJAHR
       BUDAT_MKPF
       EBELN
       EBELP
       CHARG
       MENGE
       MEINS
       TCODE2_MKPF FROM MSEG INTO TABLE IT_MSEG1 FOR ALL ENTRIES IN IT_QAMB2
                                                 WHERE  MBLNR = IT_QAMB2-MBLNR AND
                                                        MJAHR = IT_QAMB2-MJAHR AND
                                                        ZEILE = IT_QAMB2-ZEILE AND
                                                        BWART = '350'.

    DATA I TYPE I.

    LOOP AT  IT_QAMB2 INTO WA_QAMB2.

      READ TABLE IT_MSEG INTO WA_MSEG WITH KEY MBLNR = WA_QAMB2-MBLNR
                                               MJAHR = WA_QAMB2-MJAHR
                                               ZEILE = WA_QAMB2-ZEILE
                                               BWART = '101'.

      IF SY-SUBRC = 0.
        I = I + 1.
        WA_FINAL-PRUEFLOS = WA_QAMB2-PRUEFLOS.
        WA_FINAL-LIFNR = WA_MSEG-LIFNR.

*        SHIFT WA_MSEG-MATNR LEFT DELETING LEADING '0'.
CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
  EXPORTING
    INPUT              = WA_MSEG-MATNR
 IMPORTING
   OUTPUT              =  WA_FINAL-MATNR
* EXCEPTIONS
*   LENGTH_ERROR       = 1
*   OTHERS             = 2
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.



*        WA_FINAL-MATNR = WA_MSEG-MATNR.
        WA_FINAL-WERKS = WA_MSEG-WERKS.
        WA_FINAL-BUDAT = WA_MSEG-BUDAT.
        WA_FINAL-MBLNR = WA_MSEG-MBLNR.
        WA_FINAL-MJAHR = WA_MSEG-MJAHR.
        WA_FINAL-EBELN = WA_MSEG-EBELN.

        WA_FINAL-EBELP = WA_MSEG-EBELP.

        WA_FINAL-CHARG = WA_MSEG-CHARG.

        WA_FINAL-MENGE = WA_MSEG-MENGE.

        WA_FINAL-MEINS = WA_MSEG-MEINS.


      ENDIF.

      READ TABLE IT_MSEG1 INTO WA_MSEG1 WITH KEY MBLNR = WA_QAMB2-MBLNR
                                               MJAHR = WA_QAMB2-MJAHR
                                               ZEILE = WA_QAMB2-ZEILE
                                               BWART = '350'.

      IF SY-SUBRC = 0.

        I = I + 1.
        WA_FINAL-MENGE1 = WA_MSEG1-MENGE.

        WA_FINAL-MEINS1 = WA_MSEG1-MEINS.
        WA_FINAL-BWART = WA_MSEG1-BWART.
        WA_FINAL-GRUND = WA_MSEG1-GRUND.

        READ TABLE IT_QMEL INTO WA_QMEL WITH KEY PRUEFLOS = WA_FINAL-PRUEFLOS.

        IF SY-SUBRC = 0.

           WA_FINAL-CATEG = 'Online Rejection'.

        ELSE.

          WA_FINAL-CATEG = 'GRN Rejection'.

        ENDIF.
      ENDIF.

      AT END OF PRUEFLOS. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation

        IF I = 2.

          APPEND WA_FINAL TO IT_FINAL.
          CLEAR WA_FINAL.

        ELSE.

          CLEAR I.

        ENDIF.
      ENDAT.

      CLEAR: WA_QAMB2, WA_MSEG1, WA_MSEG.

    ENDLOOP.


    IF IT_FINAL IS NOT INITIAL.


      SELECT LIFNR NAME1
             FROM LFA1 INTO TABLE IT_LFA1
             FOR ALL ENTRIES IN IT_FINAL
             WHERE LIFNR = IT_FINAL-LIFNR.


      SELECT MATNR MAKTX FROM MAKT
                         INTO TABLE IT_MAKT
                         FOR ALL ENTRIES IN IT_FINAL
                         WHERE MATNR = IT_FINAL-MATNR.


      SELECT WERKS
             NAME1 FROM T001W INTO TABLE IT_T001W
                   FOR ALL ENTRIES IN IT_FINAL
                   WHERE WERKS = IT_FINAL-WERKS.

      SELECT BWART
             GRUND
             GRTXT FROM T157E INTO TABLE IT_T157E
                   FOR ALL ENTRIES IN IT_FINAL
                   WHERE BWART = IT_FINAL-BWART
                   AND   GRUND = IT_FINAL-GRUND.

      SELECT LIFNR
             MATNR
             CHARG
             LICHA FROM MCH1 INTO TABLE IT_MCH1
                   FOR ALL ENTRIES IN IT_FINAL
                   WHERE LIFNR = IT_FINAL-LIFNR AND
                         MATNR = IT_FINAL-MATNR AND
                         CHARG = IT_FINAL-CHARG.

      SELECT MBLNR
             MJAHR
             XBLNR FROM MKPF INTO TABLE IT_MKPF
                   FOR ALL ENTRIES IN IT_FINAL
                   WHERE MBLNR = IT_FINAL-MBLNR AND
                         MJAHR = IT_FINAL-MJAHR.



    LOOP AT IT_FINAL INTO WA_FINAL.

        READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_FINAL-LIFNR.

        WA_FINAL-NAME = WA_LFA1-NAME1.

        READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_FINAL-MATNR.

        WA_FINAL-MAKTX = WA_MAKT-MAKTX.

        READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS.

        WA_FINAL-NAME1 = WA_T001W-NAME1.

        READ TABLE IT_T157E INTO WA_T157E WITH KEY BWART = WA_FINAL-BWART
                                                   GRUND = WA_FINAL-GRUND.

        TRANSLATE WA_T157E-GRTXT TO UPPER CASE.

        WA_FINAL-GRTXT = WA_T157E-GRTXT.

        READ TABLE IT_MCH1 INTO WA_MCH1 WITH KEY LIFNR = WA_FINAL-LIFNR
                                                 MATNR = WA_FINAL-MATNR
                                                 CHARG = WA_FINAL-CHARG.

        WA_FINAL-LICHA = WA_MCH1-LICHA.

        READ TABLE IT_MKPF INTO WA_MKPF WITH KEY MBLNR = WA_FINAL-MBLNR
                                                 MJAHR = WA_FINAL-MJAHR.

        WA_FINAL-XBLNR = WA_MKPF-XBLNR.


        MODIFY IT_FINAL FROM WA_FINAL.
        CLEAR: WA_FINAL, WA_LFA1 , WA_MAKT, WA_T001W, WA_T157E.

      ENDLOOP.

      SORT IT_FINAL BY BUDAT EBELN.

      LOOP AT IT_FINAL inTO WA_FINAL.

        MOVE-CORRESPONDING WA_FINAL TO WA_output.

        if wa_final-grtxt is INITIAL.
        wa_output-GRTXT = wa_final-GRTXT.
        endif.
        append wa_output to output.
        clear :  wa_output , wa_final.

      ENDLOOP.

    ENDIF.

  ELSE.



  ENDIF.





















ENDFUNCTION.
