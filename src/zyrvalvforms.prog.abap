*----------------------------------------------------------------------*
*   INCLUDE RVALVFORMS                                                 *
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM OKCODE_ABSA                                              *
*---------------------------------------------------------------------*
*       Belege absagen                                                *
*---------------------------------------------------------------------*
FORM okcode_absa.
  CALL SCREEN '0201'
    STARTING AT  5  9
    ENDING   AT 60 11.
*  Bei '##' wurde Übernahme abgebrochen
  IF tvagt-abgru EQ '##'. CLEAR tvagt-abgru. EXIT. ENDIF.
  LOOP AT postab WHERE selkz EQ 'X'.   "****
*
    MOVE tvagt-abgru TO postab-abgru_new.
    PERFORM fpostab_fuellen USING 'A' sy-subrc.
  ENDLOOP.                             "****

ENDFORM.                    "OKCODE_ABSA
*---------------------------------------------------------------------*
*       FORM FPOSTAB_FUELLEN                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_AKTION                                                      *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM fpostab_fuellen USING f_aktion rc.

  DATA: lv_icon_displ TYPE icon-id.

  CLEAR: firstrow, tabix, zaehl, fpostab.
  selkz =  f_aktion.
  postab-selkz = f_aktion.             "****

  PERFORM zeilen_farbe_bestimmen USING selkz.
  PERFORM zeilen_icon_bestimmen USING selkz
                              CHANGING lv_icon_displ.

  kr_layout-no_input          =  'X'.
  READ TABLE fpostab WITH KEY vbeln = postab-vbeln BINARY SEARCH.  "****
  IF sy-subrc NE 0.                    "***
    MOVE-CORRESPONDING postab TO fpostab.                         "***
    fpostab-function = f_aktion.       "**
    INSERT fpostab INDEX sy-tabix.     "***
  ELSE.                                "***
    CLEAR fpostab.                     "***
  ENDIF.                               "**
  farb-farb2 = col.
  postab-col = farb.
  kr_layout-info_fieldname    = 'POSTAB-COL'.    "Zeilenfarbe
  postab-status_bel = lv_icon_displ.             " Icon
  MODIFY postab.                       "***

  CLEAR rc.
ENDFORM.                    "FPOSTAB_FUELLEN
*******************FORM ZEILEN_FARBE_BESTIMMEN**************************
FORM zeilen_farbe_bestimmen USING z_selkz.

  CASE z_selkz.
    WHEN 'A'.
      col = col_absagen.
    WHEN 'C'.
      col = col_pruefen.
    WHEN 'F'.
      col = col_freigeben.
    WHEN 'P'.
      col = col_pruefen.
    WHEN 'Q'.
      col = col_pruefen.
    WHEN 'W'.
      col = col_weiterleiten.
    WHEN OTHERS.
      col = col_normal.
  ENDCASE.

ENDFORM.                    "ZEILEN_FARBE_BESTIMMEN
*---------------------------------------------------------------------*
*       FORM OKCODE_ABSR                                              *
*---------------------------------------------------------------------*
*       abgesagte Belege kurzrücknehmen                                *
*---------------------------------------------------------------------*
FORM okcode_absr.

  PERFORM fpostab_loeschen USING 'A' sy-subrc.

ENDFORM.                    "OKCODE_ABSR
*---------------------------------------------------------------------*
*       FORM FPOSTAB_LOESCHEN                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_AKTION                                                      *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM fpostab_loeschen USING f_aktion rc.

  CLEAR: firstrow, tabix, zaehl, fpostab.

  LOOP AT postab WHERE selkz EQ f_aktion.   "***
    CLEAR postab-selkz.                "***
    READ TABLE fpostab WITH KEY vbeln = postab-vbeln    "***
                             function = f_aktion BINARY SEARCH. "***
    DELETE fpostab INDEX sy-tabix.
    CLEAR farb-farb2.
    col = col_normal.
    farb-farb2 = col.
    postab-col = farb.
    postab-status_bel = ''.

    IF f_aktion EQ 'A'. CLEAR tvagt-abgru. ENDIF.
    IF f_aktion EQ 'W'."#EC CI_USAGE_OK[2227014]
       CLEAR: t024b-sbgrp,"#EC CI_USAGE_OK[2227014]
        vbkred-sbgrp. ENDIF."#EC CI_USAGE_OK[2227014]
    "Added by SPLABAP during code remediation
    CLEAR rc.
    MODIFY postab.                     "***
  ENDLOOP.                             "***

ENDFORM.                    "FPOSTAB_LOESCHEN
*---------------------------------------------------------------------*
*       FORM  OKCODE_AEND                                             *
*---------------------------------------------------------------------*
*       Wechsel von der Anzeige der Einzelposten zur Aenderung        *
*       der Einzelposten.                                             *
*---------------------------------------------------------------------*
FORM okcode_aend.


  CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_SET'
    EXPORTING
      is_layout = kr_layout.
*         IT_FIELDCAT    =
*         IT_SORT        =
*         IT_FILTER      =
*         IS_LIST_SCROLL =

ENDFORM.                    "OKCODE_AEND
*---------------------------------------------------------------------*
*       FORM OKCODE_AUSW                                              *
*---------------------------------------------------------------------*
*       Auswählen eines Feldes auf der Liste verzweigt                *
*       in die entsprechende Detailanzeige zu diesem Feld:            *
*       BELNR: Beleganzeige                                           *
*       BUREG: Buchungsregelgruppe                                    *
*       ANBTR: Betrag/Bereichsanzeige                                 *
*       Falls ein anderes Feld der Zeile ausgewählt wird,             *
*       wird nicht woanders hin verzweigt.                            *
*---------------------------------------------------------------------*
FORM okcode_ausw USING pi_tabix TYPE sytabix.

  CLEAR: fname, char.
  IF NOT postab-vbeln IS INITIAL.
    PERFORM detail_liste USING pi_tabix.
  ELSE.
    MESSAGE s410.
  ENDIF.
  CLEAR postab.

ENDFORM.                    "OKCODE_AUSW
*eject
*---------------------------------------------------------------------*
*       FORM DETAIL_LISTE                                             *
*---------------------------------------------------------------------*
*       Verzweigt von Listfeldern in die Einzelanzeige- bzw.          *
*       Einzelaenderungstransaktion                                   *
*---------------------------------------------------------------------*
FORM detail_liste USING pi_tabix TYPE sytabix.
  CASE feld.
    WHEN 'KNKLI'.
      PERFORM partneranschrift USING postab-knkli.
    WHEN 'NAME1'.
      PERFORM partneranschrift USING postab-knkli.
    WHEN 'VBELN'.
      PERFORM berechtigung_pruefen USING rcode.
      IF rcode = 0.
        PERFORM auftrag USING postab-vbeln postab-vbtyp.
      ELSE.
        PERFORM auftrag_anzeigen USING postab-vbeln postab-vbtyp.
      ENDIF.
    WHEN 'POSNR'.
      PERFORM berechtigung_pruefen USING rcode.
      IF rcode = 0.
        PERFORM auftrag_positioniert USING postab-vbeln postab-posnr
                                           postab-vbtyp.
      ELSE.
        PERFORM auftrag_positioniert_anzeigen
                        USING postab-vbeln postab-posnr postab-vbtyp.
      ENDIF.
    WHEN 'CMGST'.
      PERFORM okcode_bels USING pi_tabix.
    WHEN OTHERS.
      IF feld CS 'CMPS'.
        PERFORM okcode_bels USING pi_tabix.
      ELSE.
        PERFORM berechtigung_pruefen USING rcode.
        IF rcode = 0.
          PERFORM auftrag USING postab-vbeln postab-vbtyp.
        ELSE.
          PERFORM auftrag_anzeigen USING postab-vbeln postab-vbtyp.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    "DETAIL_LISTE

*eject
*---------------------------------------------------------------------*
*       FORM PARTNERANSCHRIFT
*---------------------------------------------------------------------*
*       Zeigt die Anschrift des ausgewählten Partners                 *
*---------------------------------------------------------------------*
*  -->  KUNNR     Kundennummer                                        *
*  <--  Window mit Anschriftsdaten                                    *
*---------------------------------------------------------------------*
FORM partneranschrift USING k01-kunnr.

  DATA: anschrift(40) TYPE c.
  anschrift = TEXT-065.

  CALL FUNCTION 'VIEW_KNA1'
    EXPORTING
      kunde     = k01-kunnr
    IMPORTING
      anschrift = kna1.

  IF cl_iav_mapping_util=>is_iav_active( ) = abap_true.

    cl_iav_mapping_util=>get_address_as_kna1(
      EXPORTING iv_adrnr                 = kna1-adrnr
                iv_application_component = 'SD_CUSTOMER'
      CHANGING  cs_kna1                  = kna1 ).

  ENDIF.

  MOVE-CORRESPONDING kna1 TO adrs.

  CALL FUNCTION 'RV_ADDRESS_WINDOW_DISPLAY'"#EC CI_USAGE_OK[2214585]
    EXPORTING
      adrswa_in = adrs
      fadrtype  = space.

ENDFORM.                    "PARTNERANSCHRIFT
*eject
*---------------------------------------------------------------------*
*       FORM BERECHTIGUNG_PRUEFEN                                     *
*---------------------------------------------------------------------*
*       Pruefung, ob der Anwender die Berechtigung besitzt, mit der   *
*       Kreditfreigabe zu arbeiten.                                   *
*---------------------------------------------------------------------*
*  -->  AKTIVITÄT                                                     *
*---------------------------------------------------------------------*
FORM berechtigung_pruefen USING b_rc.
  DATA: aendern(2) VALUE '02'.
  CLEAR b_rc.
* Shipping
  IF cl_sd_doc_category_util=>is_delivery_outgoing( postab-vbtyp ). "JT - VBTYP_LIEF (RVVBTYP)
    AUTHORITY-CHECK OBJECT 'V_LIKP_VST'
      ID 'VSTEL' FIELD postab-vstel
      ID 'ACTVT' FIELD aendern.
    IF sy-subrc <> 0.
      b_rc = 4.
      EXIT.
    ENDIF.
  ELSE.
* Sales
    AUTHORITY-CHECK OBJECT 'V_VBAK_AAT'
      ID 'AUART' DUMMY
      ID 'ACTVT' FIELD aendern.
    IF sy-subrc <> 0.
      b_rc = 4.
      EXIT.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
         ID 'VKORG' FIELD postab-vkorg
         ID 'VTWEG' FIELD postab-vtweg
         ID 'SPART' FIELD postab-spart
         ID 'ACTVT' FIELD aendern.
    IF sy-subrc <> 0.
      b_rc = 4.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    "BERECHTIGUNG_PRUEFEN

*eject
*---------------------------------------------------------------------*
*       FORM AUFTRAG
*---------------------------------------------------------------------*
*       Verzweigt in die VAX2 zur Anzeige der Auftraege, Angebote, ...*
*---------------------------------------------------------------------*
FORM auftrag USING a01-vbeln a01-vbtyp.

  CALL FUNCTION 'RV_CALL_CHANGE_TRANSACTION'
    EXPORTING
      vbeln = a01-vbeln
      vbtyp = a01-vbtyp.

ENDFORM.                    "AUFTRAG
*---------------------------------------------------------------------*
*       FORM AUFTRAG_anzeigen
*---------------------------------------------------------------------*
*       Verzweigt in die VAX3 zur Anzeige der Auftraege, Angebote, ...*
*---------------------------------------------------------------------*
FORM auftrag_anzeigen USING a01-vbeln a01-vbtyp.

  CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
    EXPORTING
      vbeln = a01-vbeln
      vbtyp = a01-vbtyp.

ENDFORM.                    "AUFTRAG_ANZEIGEN
*eject
*---------------------------------------------------------------------*
*       FORM AUFTRAG_POSITIONIERT
*---------------------------------------------------------------------*
*       Verzweigt in die VA02 zur Anzeige der Auftraege               *
*       und positioniert auf die ausgewaehlte Position                *
*---------------------------------------------------------------------*
FORM auftrag_positioniert USING a01-vbeln a01-posnr a01-vbtyp.

  CALL FUNCTION 'RV_CALL_CHANGE_TRANSACTION'
    EXPORTING
      vbeln = a01-vbeln
      posnr = a01-posnr
      vbtyp = a01-vbtyp.

ENDFORM.                    "AUFTRAG_POSITIONIERT
*eject
*---------------------------------------------------------------------*
*       FORM AUFTRAG_POSITIONIERT_ANZEIGEN
*---------------------------------------------------------------------*
*       Verzweigt in die VX03 zur Anzeige der Auftraege               *
*       und positioniert auf die ausgewaehlte Position                *
*---------------------------------------------------------------------*
FORM auftrag_positioniert_anzeigen USING a01-vbeln a01-posnr a01-vbtyp.

  CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
    EXPORTING
      vbeln = a01-vbeln
      posnr = a01-posnr
      vbtyp = a01-vbtyp.

ENDFORM.                    "AUFTRAG_POSITIONIERT_ANZEIGEN
*eject
*---------------------------------------------------------------------*
*       FORM OKCODE_BELS                                              *
*---------------------------------------------------------------------*
*       Zeigt den Kopf oder Positionsstatus eines Beleges             *
*---------------------------------------------------------------------*
FORM okcode_bels USING pi_tabix TYPE sytabix.

  DATA: lucomm LIKE sy-ucomm.
  IF NOT postab-vbeln IS INITIAL.
    WHILE lucomm <> 'E12 '.
      PERFORM status_anzeigen USING postab-vbeln postab-posnr lucomm.
      CASE lucomm.
        WHEN 'OBJ+'.
          ADD 1 TO pi_tabix.
          READ TABLE postab INDEX pi_tabix.
          IF sy-subrc > 0.
            MESSAGE s444.
            SUBTRACT 1 FROM pi_tabix.
          ENDIF.
*       PERFORM NAECHSTES_OBJEKT USING LLILLI POSTAB-VBELN POSTAB-POSNR
*         '+'.
        WHEN 'OBJ-'.
          SUBTRACT 1 FROM pi_tabix.
          READ TABLE postab INDEX pi_tabix.
          IF sy-subrc > 0.
            MESSAGE s443.
            ADD 1 TO pi_tabix.
          ENDIF.
*      PERFORM NAECHSTES_OBJEKT USING LLILLI POSTAB-VBELN POSTAB-POSNR
      ENDCASE.
    ENDWHILE.
*REAK-POINT.
  ELSE.
    MESSAGE s410.
  ENDIF.
  CLEAR postab. CLEAR dummy.

ENDFORM.                    "OKCODE_BELS
*eject
*---------------------------------------------------------------------*
*       FORM STATUS_ANZEIGEN                                          *
*---------------------------------------------------------------------*
*       Bringt die Kopf- bzw. Postitionsstatusinformation zur         *
*       Anzeige, je nachdem wie die Zugriffsart der aktuellen         *
*       Anzeigevariante ist.                                          *
*---------------------------------------------------------------------*
*  -->  S01-VBELN Vertriebsbelegnummer                                *
*  -->  S01-POSNR Positionsnummer                                     *
*  <--  S01-UCOMM Benutzereingabe                                     *
*---------------------------------------------------------------------*
FORM status_anzeigen USING s01-vbeln s01-posnr s01-ucomm.
*------------------Kopfstatusinformation------------------------------*
  CLEAR vbuk.
  vbuk-vbeln = s01-vbeln.
  PERFORM vbuk_lesen.
  MOVE-CORRESPONDING kvbuk TO vbuk.
  CALL FUNCTION 'RV_DOCUMENT_HEAD_STATUS_TEXTS'
    EXPORTING
      vbuk_in       = vbuk
      window_senden = 'X'
      kreditlimit   = 'X'
    IMPORTING
      vbstt_wa      = vbstt
      fcode         = s01-ucomm.

ENDFORM.                    "STATUS_ANZEIGEN
*---------------------------------------------------------------------*
*       FORM OKCODE_BELA                                              *
*---------------------------------------------------------------------*
*       Anzeige Belegänderungen                                       *
*---------------------------------------------------------------------*
FORM okcode_bela.

  IF NOT postab-vbeln IS INITIAL.
    IF cl_sd_doc_category_util=>is_any_delivery( postab-vbtyp ). "JT7g
      SUBMIT wscdshow WITH gf_vbeln INCL postab-vbeln
                     AND RETURN.
    ELSE.
      SUBMIT rvscd100 WITH vbeln INCL postab-vbeln
                     AND RETURN.
    ENDIF.
  ELSE.
    MESSAGE s410.
  ENDIF.
  CLEAR postab. CLEAR dummy.

ENDFORM.                    "OKCODE_BELA
*eject
*---------------------------------------------------------------------*
*       FORM OKCODE_BELF                                              *
*---------------------------------------------------------------------*
*       Anspringen Belegfluss                                         *
*---------------------------------------------------------------------*
FORM okcode_belf.

  CLEAR: vbco6.
  CLEAR: postab_key.
**  IF DUMMY NE SPACE.
  IF NOT postab-vbeln IS INITIAL.
    vbco6-mandt = sy-mandt.
    vbco6-vbeln = postab-vbeln.
    vbco6-posnr = postab-posnr.
    CALL DIALOG 'RV_DOCUMENT_FLOW'
      EXPORTING
        vbco6 FROM vbco6.
  ELSE.
    MESSAGE s410.
  ENDIF.

ENDFORM.                    "OKCODE_BELF

*eject
*---------------------------------------------------------------------*
*       FORM OKCODE_BELG                                              *
*---------------------------------------------------------------------*
*       Zeigt den Beleg                                               *
*---------------------------------------------------------------------*
FORM okcode_belg.

  IF NOT postab-vbeln IS INITIAL.
    PERFORM berechtigung_pruefen USING rcode.
    IF rcode = 0.
      PERFORM auftrag USING postab-vbeln postab-vbtyp.
    ELSE.
      PERFORM auftrag_anzeigen USING postab-vbeln postab-vbtyp.
    ENDIF.
  ELSE.
    MESSAGE s410.
  ENDIF.
  CLEAR postab. CLEAR dummy.

ENDFORM.                    "OKCODE_BELG
*eject
*---------------------------------------------------------------------*
*       FORM OKCODE_CCAU                                              *
*---------------------------------------------------------------------*
*       Authorisierung der Zahlungskarten                             *
*---------------------------------------------------------------------*
FORM okcode_ccau.

  DATA: errorflag.
  LOOP AT postab WHERE selkz EQ 'X'.   "****
*
    PERFORM fpostab_fuellen USING 'C' sy-subrc.
    IF NOT fpostab-vbeln IS INITIAL.
      IF cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ). "ABCDEFGHIKLW
        CALL FUNCTION 'SD_ORDER_PAYMENTCARD_AUTHORISA'
          EXPORTING
            vbeln         = fpostab-vbeln
          IMPORTING
            e_kvbuk       = kvbuk
          EXCEPTIONS
            error_message = 4.
      ENDIF.
      IF sy-subrc NE 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
          IMPORTING
            message_text_output = news-text.
        news-vbeln   = fpostab-vbeln.
        news-msgty   = sy-msgty.
        news-msgid   = sy-msgid.
        news-msgno   = sy-msgno.
        APPEND news.
        errorflag = 'X'.
      ELSE.
*       PERFORM KREDITSTATUS_ERMITTELN(SAPLV45P)
*                           USING    KVBUK
*                                    KVBUK
*                                    SPACE
*                           CHANGING VBUK.
*       MOVE VBUK-CMGST TO KVBUK-CMGST.
*       MOVE-CORRESPONDING KVBUK TO POSTAB.
*       MOVE-CORRESPONDING KVBUK TO VBUK.
*
*       CALL FUNCTION 'SD_FIRST_CREDIT_STATUS'
*            EXPORTING
*                 XVBUK  = VBUK
*            IMPORTING
*                 STATUS = POSTAB-CSTAT.
*
*       MODIFY POSTAB INDEX TABIX.
        news-vbeln   = fpostab-vbeln.
        news-text    = TEXT-pr1.
        APPEND news.
      ENDIF.
    ENDIF.
    CLEAR news.
  ENDLOOP.                             "****

* SY-LSIND = SY-LSIND - 1.
* PERFORM NORMALLISTE_AUSGEBEN.
  DESCRIBE TABLE news LINES sy-tfill.
  IF NOT ( errorflag IS INITIAL ).
    MESSAGE i012.
  ENDIF.

  LOOP AT fpostab WHERE function = 'C'.
  ENDLOOP.
  IF sy-subrc EQ 0.
    MOVE 'PRKN' TO exctab-okcod. APPEND exctab.
    MOVE 'PRZN' TO exctab-okcod. APPEND exctab.
*   PERFORM STATUS_SETZEN USING LS-LSTAT.
  ENDIF.

ENDFORM.                    "OKCODE_CCAU
*---------------------------------------------------------------------*
*       FORM OKCODE_ABSR                                              *
*---------------------------------------------------------------------*
*       abgesagte Belege kzurücknehmen                                *
*---------------------------------------------------------------------*
FORM okcode_ccar.

  PERFORM fpostab_loeschen USING 'C' sy-subrc.

ENDFORM.                    "OKCODE_CCAR

*eject
*---------------------------------------------------------------------*
*       FORM  OKCODE_EF15                                             *
*---------------------------------------------------------------------*
*       Behandlung des OK-Codes 'F15' = 'Ende'.                       *
*---------------------------------------------------------------------*
*FORM OKCODE_EF15.
*
*  PERFORM SICHERN_NOTWENDIG CHANGING FLG_WEITER.
*  IF FLG_WEITER NE 'J'.
*    PERFORM RETURN_TO_MENU.
*  ELSE.
*    SY-LSIND = SY-LSIND - 1.
*    PERFORM PROTOKOLL.
*  ENDIF.
*
*ENDFORM.                    "OKCODE_EF15

*eject
*----------------------------------------------------------------------
* FORM SICHERN_NOTWENDIG                                              *
*----------------------------------------------------------------------
*  prüft, ob sichern notwendig                                        *
*  popup, sichern gewünscht                                           *
*  setzt Flag, für weiter bearbeitung (wenn gesichert) oder abbruch.  *
*----------------------------------------------------------------------
FORM sichern_notwendig CHANGING u_weiter.

  u_weiter = 'J'.                      " default
  DESCRIBE TABLE fpostab LINES sy-tfill.
  IF sy-tfill > 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = TEXT-113
        textline1 = TEXT-101
        textline2 = TEXT-102
      IMPORTING
        answer    = hlp_answer.
    CASE hlp_answer.
      WHEN 'J'.
        PERFORM sichern.
      WHEN OTHERS.
        u_weiter = hlp_answer.
    ENDCASE.
  ENDIF.

ENDFORM.                    "SICHERN_NOTWENDIG

*---------------------------------------------------------------------*
*       FORM  SICHERN                                                 *
*---------------------------------------------------------------------*
* - Freigabe gesperrte Belege                                         *
*---------------------------------------------------------------------*
FORM sichern.

  DATA: BEGIN OF xlips OCCURS 0.
          INCLUDE STRUCTURE lips.
  DATA: END   OF xlips.
  DATA: da_vgbel LIKE xlips-vgbel.
  DATA: ls_vbak           TYPE vbak,
        ls_likp           TYPE likp,
        lv_type           TYPE swo_objtyp,
        lv_handle         TYPE sysuuid_c,
        lv_msgty          LIKE sy-msgty,
        lv_msgid          LIKE sy-msgid,
        lv_msgno          LIKE sy-msgno,
        lv_obj_key        TYPE swo_typeid,
        lv_action_allowed TYPE boolean.
  DATA: BEGIN OF qpostab OCCURS 50,
          vbeln    LIKE vbkred-vbeln,
          vbtyp    LIKE vbkred-vbtyp,
          function,
        END   OF qpostab.

  REFRESH news.
  CLEAR news.

* Refresh credit internal memeory                           "K11K097488
  CALL FUNCTION 'SD_CREDIT_REFRESH'                         "K11K097488
    EXPORTING                                               "K11K097488
      check_no_refresh = ' '.                               "K11K097488
* make sure: first reject, release and forward, then recheck K11K097488
  LOOP AT fpostab.
    "ABCDEFGHIJKLRTX7W
    CHECK ( cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ) OR "ABCDEFGHIKLW
            cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
            fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
            fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
            fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif ) AND         "7
          fpostab-function = 'F'.

    IF cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ).        " ABCDEFGHIKLW
      CALL FUNCTION 'SD_ORDER_CREDIT_RELEASE'
        EXPORTING
          vbeln         = fpostab-vbeln
        EXCEPTIONS
          error_message = 4.
    ELSEIF cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR           "JT
           fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                      "R
           fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                       "X
           fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif.               "7
      CALL FUNCTION 'SD_DELIVERY_CREDIT_RELEASE'
        EXPORTING
          vbeln         = fpostab-vbeln
        EXCEPTIONS
          error_message = 4.
    ENDIF.
    IF sy-subrc NE 0.
      PERFORM protokoll_erweitern.
    ELSE.
      news-vbeln = fpostab-vbeln.
      news-text  = TEXT-pr0.
      APPEND news.
    ENDIF.
    DELETE fpostab.
    CLEAR   news.
  ENDLOOP.

  LOOP AT fpostab.
    "ABCDEFGHIJKLRTX7W
    CHECK ( cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ) OR "ABCDEFGHIKLW
            cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
            fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
            fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
            fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif ) AND         "7
          fpostab-function = 'A'.

    IF cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ).        "ABCDEFGHIKLW
      CALL FUNCTION 'SD_ORDER_CREDIT_CANCEL'
        EXPORTING
          vbeln         = fpostab-vbeln
          abgru         = fpostab-abgru_new
        EXCEPTIONS
          error_message = 4.
      IF sy-subrc NE 0.
        PERFORM protokoll_erweitern.
      ELSE.
        news-vbeln = fpostab-vbeln.
        IF fpostab-abgru_new IS INITIAL.
          news-text  = TEXT-pr5.
        ELSE.
          news-text  = TEXT-pr2.
        ENDIF.
        APPEND news.
      ENDIF.
    ELSEIF cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
           fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
           fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
           fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif.              "7
      REFRESH xlips.
      SELECT * FROM lips INTO      TABLE xlips
        WHERE vbeln EQ fpostab-vbeln.
      SORT xlips BY mandt vgbel vgpos.
      CALL FUNCTION 'SD_DELIVERY_CREDIT_CANCEL'
        EXPORTING
          vbeln         = fpostab-vbeln
        EXCEPTIONS
          error_message = 4.
      IF sy-subrc NE 0.
        PERFORM protokoll_erweitern.
      ELSE.
        news-vbeln = fpostab-vbeln.
        news-text  = TEXT-pr4.
        APPEND news.
*       cancel related orders
        CLEAR da_vgbel.
        LOOP AT xlips WHERE vbeln EQ fpostab-vbeln.
          IF da_vgbel NE xlips-vgbel.
*         ON CHANGE OF XLIPS-VGBEL.
            da_vgbel = xlips-vgbel.
            CALL FUNCTION 'SD_ORDER_CREDIT_CANCEL'
              EXPORTING
                abgru         = fpostab-abgru_new
                vbeln         = xlips-vgbel
              TABLES
                tlips         = xlips
              EXCEPTIONS
                error_message = 4.
            IF sy-subrc NE 0.
              PERFORM protokoll_erweitern.
            ELSE.
              news-vbeln = xlips-vgbel.
              IF fpostab-abgru_new IS INITIAL.
                news-text  = TEXT-pr5.
              ELSE.
                news-text  = TEXT-pr2.
              ENDIF.
              APPEND news.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    DELETE fpostab.
    CLEAR   news.
  ENDLOOP.

  LOOP AT fpostab.
    "ABCDEFGHIJKLRTX7W
    CHECK ( cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ) OR "ABCDEFGHIKLW
            cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
            fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
            fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
            fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif ) AND         "7
          fpostab-function = 'W'.

    IF cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ).        "ABCDEFGHIKLW
      CALL FUNCTION 'SD_ORDER_CREDIT_FORWARD'
        EXPORTING
          vbeln         = fpostab-vbeln
          sbgrp         = fpostab-sbgrp_new
        EXCEPTIONS
          error_message = 4.
    ELSEIF cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
           fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
           fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
           fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif.              "7
      CALL FUNCTION 'SD_DELIVERY_CREDIT_FORWARD'
        EXPORTING
          sbgrp = fpostab-sbgrp_new
          vbeln = fpostab-vbeln.
    ENDIF.
    IF sy-subrc NE 0.
      PERFORM protokoll_erweitern.
    ELSE.
      news-vbeln = fpostab-vbeln.
      news-text  = TEXT-pr3.
      APPEND news.
    ENDIF.
    DELETE fpostab.
    CLEAR   news.
  ENDLOOP.

* paymentcard authorisation
  LOOP AT fpostab.
    "ABCDEFGHIJKLRTX7W
    CHECK ( cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ) OR "ABCDEFGHIKLW
            cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
            fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
            fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
            fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif ) AND         "7
          fpostab-function = 'C'.

    IF cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ). "ABCDEFGHIKLW
*     orders
      CALL FUNCTION 'SD_ORDER_PAYMENTCARD_AUTHORISA'
        EXPORTING
          flg_update    = 'X'
          vbeln         = fpostab-vbeln
        EXCEPTIONS
          error_message = 4.
      IF sy-subrc NE 0.
        PERFORM protokoll_erweitern.
      ELSE.
        news-vbeln   = fpostab-vbeln.
        news-text    = TEXT-pr6.
        APPEND news.
      ENDIF.
    ELSEIF cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
           fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
           fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
           fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif.              "7
*     delivery notes
      REFRESH xlips.
      SELECT * FROM lips INTO      TABLE xlips
        WHERE vbeln EQ fpostab-vbeln.
      SORT xlips BY mandt vgbel vgpos.
*     authorisation of the related orders
      CLEAR da_vgbel.
      LOOP AT xlips WHERE vbeln EQ fpostab-vbeln.
        IF da_vgbel NE xlips-vgbel.
*         ON CHANGE OF XLIPS-VGBEL.
          da_vgbel = xlips-vgbel.
          CALL FUNCTION 'SD_ORDER_PAYMENTCARD_AUTHORISA'
            EXPORTING
              flg_update    = 'X'
              vbeln         = da_vgbel
            EXCEPTIONS
              error_message = 4.
          IF sy-subrc NE 0.
            PERFORM protokoll_erweitern.
          ELSE.
            news-vbeln = xlips-vgbel.
            news-text  = TEXT-pr6.
            APPEND news.
          ENDIF.
        ENDIF.
      ENDLOOP.
*     recheck of delivery note after having rechecked the order
      CALL FUNCTION 'SD_DELIVERY_CREDIT_RECHECK'
        EXPORTING
          flg_update    = 'X'
          vbeln         = fpostab-vbeln
        EXCEPTIONS
          error_message = 4.
      IF sy-subrc NE 0.
        PERFORM protokoll_erweitern.
      ELSE.
        news-vbeln = fpostab-vbeln.
        news-text  = TEXT-pr1.
        APPEND news.
      ENDIF.
    ENDIF.
    DELETE fpostab.
    CLEAR   news.
  ENDLOOP.

* recheck is processed at the end
  LOOP AT fpostab.
    "ABCDEFGHIJKLRTX7W
    CHECK ( cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ) OR "ABCDEFGHIKLW
            cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
            fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
            fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
            fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif ) AND         "7
          fpostab-function = 'P'.

    IF cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ).        "ABCDEFGHIKLW
      CALL FUNCTION 'SD_ORDER_CREDIT_RECHECK'
        EXPORTING
          flg_update    = 'X'
          vbeln         = fpostab-vbeln
        EXCEPTIONS
          error_message = 4.
    ELSEIF cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
           fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
           fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
           fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif.              "7
      CALL FUNCTION 'SD_DELIVERY_CREDIT_RECHECK'
        EXPORTING
          flg_update    = 'X'
          vbeln         = fpostab-vbeln
        EXCEPTIONS
          error_message = 4.
    ENDIF.
    IF sy-subrc NE 0.
      PERFORM protokoll_erweitern.
    ELSE.
      news-vbeln   = fpostab-vbeln.
      news-text    = TEXT-pr1.
      APPEND news.
    ENDIF.
    DELETE fpostab.
    CLEAR   news.
  ENDLOOP.

* get all documents, which have to be rechecked for credit rescheduling
  PERFORM credit_rescheduling_activate(saplvkmp).
  LOOP AT postab WHERE selkz EQ 'Q'.
    READ TABLE fpostab WITH KEY vbeln = postab-vbeln BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING fpostab TO qpostab. APPEND qpostab.
    ENDIF.
  ENDLOOP.

  LOOP AT qpostab.
    MOVE-CORRESPONDING qpostab TO fpostab.

    "ABCDEFGHIJKLRTX7W
    CHECK ( cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ) OR "ABCDEFGHIKLW
            cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
            fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
            fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
            fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif ) AND         "7
          fpostab-function = 'Q'.

    IF cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ).        "ABCDEFGHIKLW
*       get open credit values of all orders
      PERFORM credit_rescheduling_get_db(sapdbkmv)
        USING fpostab-vbeln 'A'.
    ELSEIF cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR           "JT
           fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                      "R
           fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                       "X
           fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif.               "7
*       get open credit values of all deliveries
      PERFORM credit_rescheduling_get_db(sapdbkmv)
        USING fpostab-vbeln 'L'.
    ENDIF.
  ENDLOOP.
* credit rescheduling
* first delivery notes
  LOOP AT qpostab.
    MOVE-CORRESPONDING qpostab TO fpostab.
    "JRTX7
    CHECK ( cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
            fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
            fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
            fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif ) AND         "7
          fpostab-function = 'Q'.

    CALL FUNCTION 'SD_DELIVERY_CREDIT_RECHECK'
      EXPORTING
        flg_update    = 'X'
        vbeln         = fpostab-vbeln
      EXCEPTIONS
        error_message = 4.
    IF sy-subrc NE 0.
      PERFORM protokoll_erweitern.
    ELSE.
      news-vbeln   = fpostab-vbeln.
      news-text    = TEXT-pr1.
      APPEND news.
    ENDIF.
    DELETE qpostab.
    CLEAR   news.
  ENDLOOP.
* then orders
  LOOP AT qpostab.
    MOVE-CORRESPONDING qpostab TO fpostab.
    "ABCDEFGHIKLW
    CHECK cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ) AND   "ABCDEFGHIKLW
          fpostab-function = 'Q'.

    CALL FUNCTION 'SD_ORDER_CREDIT_RECHECK'
      EXPORTING
        flg_update    = 'X'
        vbeln         = fpostab-vbeln
      EXCEPTIONS
        error_message = 4.
    IF sy-subrc NE 0.
      PERFORM protokoll_erweitern.
    ELSE.
      news-vbeln   = fpostab-vbeln.
      news-text    = TEXT-pr1.
      APPEND news.
    ENDIF.
    DELETE qpostab.
    CLEAR   news.
  ENDLOOP.
  PERFORM credit_rescheduling_deactivate(saplvkmp).

ENDFORM.                    "SICHERN

*&---------------------------------------------------------------------*
*&      Form  PROTOKOLL_ERWEITERN
*&---------------------------------------------------------------------*
FORM protokoll_erweitern.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = sy-msgid
      msgnr               = sy-msgno
      msgv1               = sy-msgv1
      msgv2               = sy-msgv2
      msgv3               = sy-msgv3
      msgv4               = sy-msgv4
    IMPORTING
      message_text_output = news-text.
  news-vbeln   = fpostab-vbeln.
  news-msgty   = sy-msgty.
  news-msgid   = sy-msgid.
  news-msgno   = sy-msgno.
  APPEND news.

ENDFORM.                               " PROTOKOLL_ERWEITERN

*---------------------------------------------------------------------*
*       FORM  OKCODE_EF17                                             *
*---------------------------------------------------------------------*
*       Behandlung des OK-Codes 'F17' = 'Andere Selektionskriterien'  *
*       Evtl hier Sicherheitsabfrage einbauen
*---------------------------------------------------------------------*
FORM okcode_ef17.

  PERFORM sichern_notwendig CHANGING flg_weiter.
  IF flg_weiter NE 'A'.
*      PERFORM RETURN_TO_MENU.
    LEAVE TO TRANSACTION sy-tcode.
  ELSE.
    sy-lsind = sy-lsind - 1.
  ENDIF.

ENDFORM.                    "OKCODE_EF17
*---------------------------------------------------------------------*
*       FORM OKCODE_FREIGABE                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM okcode_freigabe.

  LOOP AT postab WHERE selkz EQ 'X'.   "****
*
    PERFORM fpostab_fuellen USING 'F' sy-subrc.
  ENDLOOP.                             "****

ENDFORM.                    "OKCODE_FREIGABE
*---------------------------------------------------------------------*
*       FORM OKCODE_FRER                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM okcode_frer.

  PERFORM fpostab_loeschen USING 'F' sy-subrc.

ENDFORM.                    "OKCODE_FRER
*&---------------------------------------------------------------------*
*&      Form  OKCODE_F31
*&---------------------------------------------------------------------*
*       Aufruf Kreditübersicht                                         *
*----------------------------------------------------------------------*
FORM okcode_f31.

  DATA: v_kunde LIKE vbkred-knkli,
        v_kkber LIKE vbkred-kkber,
        v_sbgrp LIKE vbkred-sbgrp.

  IF NOT postab-vbeln IS INITIAL.
    GET PARAMETER ID 'KUN' FIELD v_kunde.
    GET PARAMETER ID 'KKB' FIELD v_kkber.
    GET PARAMETER ID 'KBG' FIELD v_sbgrp.

    SUBMIT rfdkli40 WITH konto = postab-knkli "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
                    WITH kkber = postab-kkber "#EC CI_USAGE_OK[2270335] " Added by <IT-CAR Tool> during Code Remediation
                    WITH sbgrp = postab-sbgrp
                    VIA SELECTION-SCREEN AND RETURN.

    SET PARAMETER ID 'KUN' FIELD v_kunde.
    SET PARAMETER ID 'KKB' FIELD v_kkber.
    SET PARAMETER ID 'KBG' FIELD v_sbgrp.
  ELSE.
    MESSAGE s410.
  ENDIF.

ENDFORM.                               " OKCODE_F31
*---------------------------------------------------------------------*
*       FORM OKCODE_KUST                                              *
*---------------------------------------------------------------------*
*       Verzweigt in die Anzeige Kundenstamm (VD02)                   *
*---------------------------------------------------------------------*
FORM okcode_kust.

  DATA: k_vkorg LIKE tvkot-vkorg,
        k_vtweg LIKE tvtwt-vtweg.

  GET PARAMETER ID 'VKO' FIELD k_vkorg.
  GET PARAMETER ID 'VTW' FIELD k_vtweg.

  IF NOT postab-vbeln IS INITIAL.
    SET PARAMETER ID 'KUN' FIELD postab-knkli.
    IF NOT ( postab-vkorg IS INITIAL ) .
      SET PARAMETER ID 'VKO' FIELD postab-vkorg.
    ENDIF.
    IF NOT ( postab-vtweg IS INITIAL ) .
      SET PARAMETER ID 'VTW' FIELD postab-vtweg.
    ENDIF.
    CALL TRANSACTION 'VD02'. "#EC CI_USAGE_OK[2265093] " Added by <IT-CAR Tool> during Code Remediation
  ELSE.
    MESSAGE s410.
  ENDIF.
  SET PARAMETER ID 'VKO' FIELD k_vkorg.
  SET PARAMETER ID 'VTW' FIELD k_vtweg.

ENDFORM.                    "OKCODE_KUST

*eject
*---------------------------------------------------------------------*
*       FORM OKCODE_FD32                                              *
*---------------------------------------------------------------------*
*       Verzweigt in die Anzeige Kundenstamm (FD32)                   *
*---------------------------------------------------------------------*
FORM okcode_fd32 USING datenbereich.

  DATA: dynpros(80).


  IF NOT postab-vbeln IS INITIAL.
    SET PARAMETER ID 'KUN' FIELD postab-knkli.
    SET PARAMETER ID 'KKB' FIELD postab-kkber.
    CASE datenbereich.
      WHEN '0'.
        dynpros = '/105'.              "Übersicht
      WHEN '1'.
        dynpros = '/110/120'.          "Allg. Daten
      WHEN '2'.
        dynpros = '/210/220'.          "Kontrolbereichsdaten.
    ENDCASE.
    SET PARAMETER ID 'CDY' FIELD dynpros.
    CALL TRANSACTION 'FD32' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[2270335] " Added by <IT-CAR Tool> during Code Remediation
  ELSE.
    MESSAGE s410.
  ENDIF.

ENDFORM.                    "OKCODE_FD32
*---------------------------------------------------------------------*
*       FORM PROTOKOLL_LANGTEXT                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM protokoll_langtext.

  IF news-msgid IS INITIAL.
    MESSAGE i837(sd).
    EXIT.
  ENDIF.
  CLEAR dokhl.
  dokhl-id    = 'NA'.
  dokhl-langu = sy-langu.
  dokhl-typ   = news-msgty.
  dokhl-langu = sy-langu.
  dokhl-langu = sy-langu.
  dokhl-langu = sy-langu.
  concatenate news-msgid news-msgno into dokhl-object.
  CALL FUNCTION 'DOCU_CALL'
    EXPORTING
      id         = dokhl-id
      langu      = dokhl-langu
      object     = dokhl-object
      typ        = dokhl-typ
      displ      = 'X'
      displ_mode = '2'.
  CLEAR news.

ENDFORM.                    "PROTOKOLL_LANGTEXT
*---------------------------------------------------------------------*
*       FORM OKCODE_PANS                                              *
*---------------------------------------------------------------------*
*       Zeigt die Partneranschrift                                    *
*---------------------------------------------------------------------*
FORM okcode_pans.

  IF NOT postab-vbeln IS INITIAL.
    PERFORM partneranschrift USING postab-knkli.
  ELSE.
    MESSAGE s410.
  ENDIF.

ENDFORM.                    "OKCODE_PANS
*---------------------------------------------------------------------*
*       FORM OKCODE_PRUE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM okcode_prue.

  DATA: errorflag.

***********************************************************************
* FSCM: SAP Credit Management (since ERP 2005)
  STATICS: sv_cm_checked,
           sv_ukm_erp2005.

  IF sv_cm_checked IS INITIAL.
    CALL FUNCTION 'UKM_IS_ACTIVE'
      IMPORTING
        e_erp2005 = sv_ukm_erp2005.
    sv_cm_checked = 'X'.
  ENDIF.
**#S/4HANA Start        #n2336198, only FSCM-CR supported in S/4HANA
**  IF NOT sv_ukm_erp2005 IS INITIAL.
**S/4HANA# End        #n2336198
*   suppress popups
    CALL FUNCTION 'SD_MESSAGE_HANDLING_FSCM'.
**  ENDIF.
***********************************************************************

  LOOP AT postab WHERE selkz EQ 'X'.   "****
*

    PERFORM fpostab_fuellen USING 'P' sy-subrc.
    IF NOT fpostab-vbeln IS INITIAL.
**      IF    fpostab-vbtyp CA 'ABCDEFGHIKLW'.
 IF cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ).     "ABCDEFGHIKLW
        CALL FUNCTION 'SD_ORDER_CREDIT_RECHECK'
          EXPORTING
            vbeln         = fpostab-vbeln
          IMPORTING
            e_kvbuk       = kvbuk
          EXCEPTIONS
            error_message = 4.
**      ELSEIF fpostab-vbtyp CA 'JRTX7'.

      ELSEIF cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR        "JT
           fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
           fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
           fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif.              "7

        CALL FUNCTION 'SD_DELIVERY_CREDIT_RECHECK'
          EXPORTING
            flg_update    = ' '
            vbeln         = fpostab-vbeln
          IMPORTING
            e_kvbuk       = kvbuk
          EXCEPTIONS
            error_message = 4.
      ENDIF.
      IF sy-subrc NE 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
          IMPORTING
            message_text_output = news-text.
        news-vbeln   = fpostab-vbeln.
        news-msgty   = sy-msgty.
        news-msgid   = sy-msgid.
        news-msgno   = sy-msgno.
        APPEND news.
        errorflag = 'X'.
      ELSE.
        PERFORM kreditstatus_ermitteln(saplv45p)
                            USING    kvbuk
                                     kvbuk
                                     space
                                     postab-vbkla
                            CHANGING vbuk.
        MOVE vbuk-cmgst TO kvbuk-cmgst.
        MOVE-CORRESPONDING kvbuk TO postab.
        MOVE-CORRESPONDING kvbuk TO vbuk.

        CALL FUNCTION 'SD_FIRST_CREDIT_STATUS'
          EXPORTING
            xvbuk  = vbuk
          IMPORTING
            status = postab-cstat.

        MODIFY postab.
        news-vbeln   = fpostab-vbeln.
        news-text    = text-pr1.
        APPEND news.
      ENDIF.
    ENDIF.
    CLEAR news.
  ENDLOOP.                             "****

*  SY-LSIND = SY-LSIND - 1.
*  PERFORM NORMALLISTE_AUSGEBEN.
  DESCRIBE TABLE news LINES sy-tfill.
  IF NOT ( errorflag IS INITIAL ).
    MESSAGE i012.
  ENDIF.

  LOOP AT fpostab WHERE function = 'P'.
  ENDLOOP.
  IF sy-subrc EQ 0.
    MOVE 'PRKN' TO exctab-okcod. APPEND exctab.
    MOVE 'PRZN' TO exctab-okcod. APPEND exctab.
*    PERFORM STATUS_SETZEN USING LS-LSTAT.
  ENDIF.

ENDFORM.                    "OKCODE_PRUE
*---------------------------------------------------------------------*
*       FORM OKCODE_PRUR                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM okcode_prur.
* Aktuelle offenen Werte, die sich aus den Prüfungen ergeben haben,
* werden zurückgenommen.
  PERFORM sav_s066_s067_refresh(saplvkmp).

  PERFORM fpostab_loeschen USING 'P' sy-subrc.

  LOOP AT fpostab WHERE function = 'P'.
  ENDLOOP.
  IF sy-subrc NE 0.
    LOOP AT exctab.
      CHECK exctab-okcod = 'PRKN' OR
            exctab-okcod = 'PRZN'.
      DELETE exctab.
    ENDLOOP.
    PERFORM status_setzen USING ls-lstat.
  ENDIF.

ENDFORM.                    "OKCODE_PRUR
*---------------------------------------------------------------------*
*       FORM OKCODE_PRKN                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM okcode_prkn.

  DATA: errorflag.

***********************************************************************
* FSCM: SAP Credit Management (since ERP 2005)
  STATICS: sv_cm_checked,
           sv_ukm_erp2005.

  IF sv_cm_checked IS INITIAL.
    CALL FUNCTION 'UKM_IS_ACTIVE'
      IMPORTING
        e_erp2005 = sv_ukm_erp2005.
    sv_cm_checked = 'X'.
  ENDIF.
**#S/4HANA Start        #n2336198, only FSCM-CR supported in S/4HANA
**  IF NOT sv_ukm_erp2005 IS INITIAL.
**S/4HANA# End        #n2336198
*   suppress popups
    CALL FUNCTION 'SD_MESSAGE_HANDLING_FSCM'.
**  ENDIF.
***********************************************************************

  DATA:    BEGIN OF qpostab OCCURS 50,
             vbeln    LIKE vbkred-vbeln,
             vbtyp    LIKE vbkred-vbtyp,
             function,
           END   OF qpostab.
  DATA: da_fpla LIKE fpla.             "CCARD

  LOOP AT postab WHERE selkz EQ 'X'.   "****
*
    PERFORM fpostab_fuellen USING 'Q' sy-subrc.
* IF NOT FPOSTAB-VBELN IS INITIAL
* AND FPOSTAB-FUNCTION = 'Q'.
    MOVE-CORRESPONDING fpostab TO qpostab. APPEND qpostab.
* ENDIF.
  ENDLOOP.                             "****

**  ENDDO.
*
  PERFORM credit_rescheduling_activate(saplvkmp).

  LOOP AT qpostab.
    MOVE-CORRESPONDING qpostab TO fpostab.
    "ABCDEFGHIJKLRTX7W
    CHECK ( cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ) OR "ABCDEFGHIKLW
            cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
            fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
            fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
            fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif ) AND         "7
          fpostab-function = 'Q'.

    IF cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ).       "ABCDEFGHIKLW'
*     get open credit values of all orders
      PERFORM credit_rescheduling_get_db(sapdbkmv)
        USING fpostab-vbeln 'A'.
    ELSEIF cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
           fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
           fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
           fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif.              "7
*     get open credit values of all deliveries
      PERFORM credit_rescheduling_get_db(sapdbkmv)
        USING fpostab-vbeln 'L'.
    ENDIF.
  ENDLOOP.
* first delivery notes
  LOOP AT qpostab.
    MOVE-CORRESPONDING qpostab TO fpostab.
    IF fpostab-function = 'Q' AND
       ( cl_sd_doc_category_util=>is_delivery_outgoing( fpostab-vbtyp ) OR          "JT
         fpostab-vbtyp EQ if_sd_doc_category=>goods_movement OR                     "R
         fpostab-vbtyp EQ if_sd_doc_category=>handling_unit OR                      "X
         fpostab-vbtyp EQ if_sd_doc_category=>delivery_shipping_notif ).            "7
      CALL FUNCTION 'SD_DELIVERY_CREDIT_RECHECK'
        EXPORTING
          flg_update    = ' '
          vbeln         = fpostab-vbeln
        IMPORTING
          e_kvbuk       = kvbuk
        EXCEPTIONS
          error_message = 4.
      IF sy-subrc NE 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
          IMPORTING
            message_text_output = news-text.
        news-vbeln   = fpostab-vbeln.
        APPEND news.
        errorflag = 'X'.
      ELSE.
        PERFORM kreditstatus_ermitteln(saplv45p)
                            USING    kvbuk
                                     kvbuk
                                     space
                                     postab-vbkla
                            CHANGING vbuk.

        LOOP AT postab WHERE vbeln = fpostab-vbeln.
          tabix = sy-tabix.
          EXIT.
        ENDLOOP.

        MOVE vbuk-cmgst TO kvbuk-cmgst.
        MOVE-CORRESPONDING kvbuk TO postab.
        MOVE-CORRESPONDING kvbuk TO vbuk.

        CALL FUNCTION 'SD_FIRST_CREDIT_STATUS'
          EXPORTING
            xvbuk  = vbuk
          IMPORTING
            status = postab-cstat.

        MODIFY postab INDEX tabix.
        news-vbeln   = fpostab-vbeln.
        news-text    = TEXT-pr1.
        APPEND news.
      ENDIF.
    ENDIF.
  ENDLOOP.
* then orders
  LOOP AT qpostab.
    MOVE-CORRESPONDING qpostab TO fpostab.
    IF fpostab-function = 'Q' AND
       cl_sd_doc_category_util=>is_any_sales_wo_mast_contract( fpostab-vbtyp ). "ABCDEFGHIKLW
      CALL FUNCTION 'SD_ORDER_CREDIT_RECHECK'
        EXPORTING
          vbeln         = fpostab-vbeln
        IMPORTING
          e_kvbuk       = kvbuk
        EXCEPTIONS
          error_message = 4.
      IF sy-subrc NE 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
          IMPORTING
            message_text_output = news-text.
        news-vbeln   = fpostab-vbeln.
        APPEND news.
        errorflag = 'X'.
      ELSE.
        PERFORM kreditstatus_ermitteln(saplv45p)
                            USING    kvbuk
                                     kvbuk
                                     space
                                     postab-vbkla
                            CHANGING vbuk.

        LOOP AT postab WHERE vbeln = fpostab-vbeln.
          tabix = sy-tabix.
          EXIT.
        ENDLOOP.

        MOVE vbuk-cmgst TO kvbuk-cmgst.
        MOVE-CORRESPONDING kvbuk TO postab.
        MOVE-CORRESPONDING kvbuk TO vbuk.

        CALL FUNCTION 'SD_FIRST_CREDIT_STATUS'
          EXPORTING
            xvbuk  = vbuk
          IMPORTING
            status = postab-cstat.
* Text to the status of FPLA            "CCARD
        MOVE-CORRESPONDING postab TO da_fpla.
        CALL FUNCTION 'SD_CCARD_LIST_FPLA_TEXTS'
          EXPORTING
            fpla_i     = da_fpla
          IMPORTING
            aust1txt_e = postab-aust1txt
            aust2txt_e = postab-aust2txt
            aust3txt_e = postab-aust3txt.

        MODIFY postab INDEX tabix.
        news-vbeln   = fpostab-vbeln.
        news-text    = TEXT-pr1.
        APPEND news.
      ENDIF.
    ENDIF.
  ENDLOOP.
*
* SY-LILLI = TFILL.     "test
* READ LINE SY-LILLI INDEX SY-LISTI. "test
* SY-LSIND = SY-LSIND - 1.
* PERFORM NORMALLISTE_AUSGEBEN.
  DESCRIBE TABLE news LINES sy-tfill.
  IF NOT ( errorflag IS INITIAL ).
    MESSAGE i012.
  ENDIF.

  PERFORM credit_rescheduling_deactivate(saplvkmp).

  LOOP AT fpostab WHERE function = 'Q'.
  ENDLOOP.
  IF sy-subrc EQ 0.
    MOVE 'PRUE' TO exctab-okcod. APPEND exctab.
    MOVE 'PRUR' TO exctab-okcod. APPEND exctab.
*   PERFORM STATUS_SETZEN USING LS-LSTAT.
  ENDIF.

ENDFORM.                    "OKCODE_PRKN
*---------------------------------------------------------------------*
*       FORM OKCODE_PRZN                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM okcode_przn.

* Aktuelle offenen Werte, die sich aus den Prüfungen ergeben haben,
* werden zurückgenommen.
  PERFORM sav_s066_s067_refresh(saplvkmp).
*
  PERFORM fpostab_loeschen USING 'Q' sy-subrc.

  LOOP AT fpostab WHERE function = 'Q'.
  ENDLOOP.
  IF sy-subrc NE 0.
    LOOP AT exctab.
      CHECK exctab-okcod = 'PRUE' OR
            exctab-okcod = 'PRUR'.
      DELETE exctab.
    ENDLOOP.
    PERFORM status_setzen USING ls-lstat.
  ENDIF.

ENDFORM.                    "OKCODE_PRZN
*---------------------------------------------------------------------*
*       FORM OKCODE_REP1                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM okcode_rep1.

  DATA: BEGIN OF rstisel OCCURS 10.
          INCLUDE STRUCTURE rstisel.
  DATA: END OF   rstisel.

  IF dummy NE space OR NOT postab-vbeln IS INITIAL.
    MOVE 'KKBER'      TO  rstisel-field.
    MOVE 'I'          TO  rstisel-sign.
    MOVE 'EQ'         TO  rstisel-option.
    MOVE postab-kkber TO  rstisel-low.
    APPEND rstisel. CLEAR rstisel.
    MOVE 'KNKLI'      TO  rstisel-field.
    MOVE 'I'          TO  rstisel-sign.
    MOVE 'EQ'         TO  rstisel-option.
    MOVE postab-knkli TO  rstisel-low.
    APPEND rstisel. CLEAR rstisel.
  ELSE.
    LOOP AT kkber.
      MOVE-CORRESPONDING kkber TO rstisel.
      MOVE 'KKBER'      TO  rstisel-field.
      APPEND rstisel. CLEAR rstisel.
    ENDLOOP.
    LOOP AT knkli.
      MOVE-CORRESPONDING knkli TO rstisel.
      MOVE 'KNKLI'      TO  rstisel-field.
      APPEND rstisel. CLEAR rstisel.
    ENDLOOP.
  ENDIF.
  PERFORM open_amount_display
    TABLES rstisel USING 'S066'.

ENDFORM.                    "OKCODE_REP1
*---------------------------------------------------------------------*
*       FORM OPEN_AMOUNT_DISPLAY                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RSTISEL                                                       *
*  -->  MCINF                                                         *
*---------------------------------------------------------------------*
FORM open_amount_display TABLES rstisel STRUCTURE rstisel
                         USING  mcinf.
  DATA: con_speri_mon LIKE tmc4-speri       VALUE 'M',
        con_speri_woc LIKE tmc4-speri       VALUE 'W',
        con_speri_tag LIKE tmc4-speri       VALUE 'T',
        con_speri_bup LIKE tmc4-speri       VALUE 'P'.
  DATA: sav_peri LIKE tmc4-speri.
  DATA: BEGIN OF rstifields OCCURS 10.
          INCLUDE STRUCTURE rstifields.
  DATA: END   OF rstifields.
  TABLES: dfies.
  TABLES: tmc8.

  PERFORM periodizitaet_ermitteln(rmcs1000) USING    mcinf
                                            CHANGING sav_peri.
  CASE sav_peri.
    WHEN con_speri_tag.
      rstifields-field = 'SPTAG'.
      rstifields-rfield = 'SPTAG'.
      CLEAR dfies.
      PERFORM get_field(rddfie00)
        USING mcinf
              'SPTAG'
              sy-langu
        CHANGING dfies
                 sy-subrc.
      MOVE: dfies-rollname TO rstifields-rollname,
            dfies-domname  TO rstifields-domname.
      APPEND rstifields. CLEAR rstifields.
      MOVE 'SPTAG'      TO  rstisel-field.
      APPEND rstisel. CLEAR rstisel.
    WHEN con_speri_woc.
      rstifields-field = 'SPWOC'.
      rstifields-rfield = 'SPWOC'.
      CLEAR dfies.
      PERFORM get_field(rddfie00)
        USING mcinf
              'SPWOC'
              sy-langu
        CHANGING dfies
                 sy-subrc.
      MOVE: dfies-rollname TO rstifields-rollname,
            dfies-domname  TO rstifields-domname.
      APPEND rstifields. CLEAR rstifields.
      MOVE 'SPWOC'      TO  rstisel-field.
      APPEND rstisel. CLEAR rstisel.
    WHEN con_speri_mon.
      rstifields-field = 'SPMON'.
      rstifields-rfield = 'SPMON'.
      CLEAR dfies.
      PERFORM get_field(rddfie00)
        USING mcinf
              'SPMON'
              sy-langu
        CHANGING dfies
                 sy-subrc.
      MOVE: dfies-rollname TO rstifields-rollname,
            dfies-domname  TO rstifields-domname.
      APPEND rstifields. CLEAR rstifields.
      MOVE 'SPMON'      TO  rstisel-field.
      APPEND rstisel. CLEAR rstisel.
    WHEN con_speri_bup.
      rstifields-field = 'SPBUP'.
      rstifields-rfield = 'SPBUP'.
      CLEAR dfies.
      PERFORM get_field(rddfie00)
        USING mcinf
              'SPBUP'
              sy-langu
        CHANGING dfies
                 sy-subrc.
      MOVE: dfies-rollname TO rstifields-rollname,
            dfies-domname  TO rstifields-domname.
      APPEND rstifields. CLEAR rstifields.
      MOVE 'SPBUP'      TO  rstisel-field.
      APPEND rstisel. CLEAR rstisel.
  ENDCASE.
  LOOP AT rstisel WHERE field EQ 'KKBER'.
  ENDLOOP.
  IF sy-subrc EQ 0.
    rstifields-field = 'KKBER'.
    rstifields-rfield = 'KKBER'.
    CLEAR dfies.
    PERFORM get_field(rddfie00)
      USING mcinf
            'KKBER'
            sy-langu
      CHANGING dfies
               sy-subrc.
    MOVE: dfies-rollname TO rstifields-rollname,
          dfies-domname  TO rstifields-domname.
    APPEND rstifields. CLEAR rstifields.
  ENDIF.
  LOOP AT rstisel WHERE field EQ 'KNKLI'.
  ENDLOOP.
  IF sy-subrc EQ 0.
    rstifields-field = 'KNKLI'.
    rstifields-rfield = 'KNKLI'.
    CLEAR dfies.
    PERFORM get_field(rddfie00)
      USING mcinf
            'KNKLI'
            sy-langu
      CHANGING dfies
               sy-subrc.
    MOVE: dfies-rollname TO rstifields-rollname,
          dfies-domname  TO rstifields-domname.
    APPEND rstifields. CLEAR rstifields.
  ENDIF.
*  SELECT SINGLE * FROM tmc8 WHERE uname EQ '$SAP'
  "Added by SPLABAP during code remediation
  SELECT  * FROM tmc8 UP TO 1 ROWS WHERE uname EQ '$SAP'
                            AND   mcapp EQ '01'
                            AND   mcana EQ '6066'
    ORDER BY PRIMARY KEY.
    ENDSELECT.
  IF sy-subrc NE 0.
    tmc8-uname = '$SAP'.
    tmc8-mcapp = '01'.
    tmc8-kenni = space.
*   S066
    tmc8-mcana = '6066'.
    tmc8-tabname = 'S066'.
*   1. Eintrag
    tmc8-nr = '01'.
    tmc8-fieldname = 'OEIKW'.
    INSERT tmc8.
*   2. Eintrag
    tmc8-nr = '02'.
    tmc8-fieldname = 'AOEIW'.
    INSERT tmc8.
*   S067
    tmc8-mcana = '6067'.
    tmc8-tabname = 'S067'.
*   1. Eintrag
    tmc8-nr = '01'.
    tmc8-fieldname = 'OLIKW'.
    INSERT tmc8.
*   2. Eintrag
    tmc8-nr = '02'.
    tmc8-fieldname = 'OFAKW'.
    INSERT tmc8.
*   3. Eintrag
    tmc8-nr = '03'.
    tmc8-fieldname = 'AOLIW'.
    INSERT tmc8.
*   4. Eintrag
    tmc8-nr = '04'.
    tmc8-fieldname = 'AOFAW'.
    INSERT tmc8.
  ENDIF.
  CALL FUNCTION 'MCS_ANALYSE_CALL'
    EXPORTING
      i_mcinf                       = mcinf
      i_speri                       = sav_peri
      i_mcapp                       = '01'
      i_vrsio                       = '000'
      i_flg_via_selscreen           = space
    TABLES
      t_sel                         = rstisel
      t_fields                      = rstifields
    EXCEPTIONS
      evaluative_info_structure     = 1
      info_structure_not_generated  = 2
      no_analysis_selected          = 3
      info_structure_does_not_exist = 4
      OTHERS                        = 5.

ENDFORM.                    "OPEN_AMOUNT_DISPLAY
*---------------------------------------------------------------------*
*       FORM OKCODE_REP2                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM okcode_rep2.

  DATA: BEGIN OF rstisel OCCURS 10.
          INCLUDE STRUCTURE rstisel.
  DATA: END OF   rstisel.

  IF NOT postab-vbeln IS INITIAL.
    MOVE 'KKBER'      TO  rstisel-field.
    MOVE 'I'          TO  rstisel-sign.
    MOVE 'EQ'         TO  rstisel-option.
    MOVE postab-kkber TO  rstisel-low.
    APPEND rstisel. CLEAR rstisel.
    MOVE 'KNKLI'      TO  rstisel-field.
    MOVE 'I'          TO  rstisel-sign.
    MOVE 'EQ'         TO  rstisel-option.
    MOVE postab-knkli TO  rstisel-low.
    APPEND rstisel. CLEAR rstisel.
  ELSE.
    LOOP AT kkber.
      MOVE-CORRESPONDING kkber TO rstisel.
      MOVE 'KKBER'      TO  rstisel-field.
      APPEND rstisel. CLEAR rstisel.
    ENDLOOP.
    LOOP AT knkli.
      MOVE-CORRESPONDING knkli TO rstisel.
      MOVE 'KNKLI'      TO  rstisel-field.
      APPEND rstisel. CLEAR rstisel.
    ENDLOOP.
  ENDIF.
  PERFORM open_amount_display
    TABLES rstisel USING 'S067'.

ENDFORM.                    "OKCODE_REP2
*---------------------------------------------------------------------*
*       FORM OKCODE_SAVE                                              *
*---------------------------------------------------------------------*
*       Aufteilungsregel erfallen                                     *
*       Aufruf des Aufteilungsregel Erfassungsdialog für alle
*       angekreuzten Zeilen der Postab auf der aktuellen              *
*       Stufe der Listanzeige (POSTAB-ACTIV)                          *
*---------------------------------------------------------------------*
FORM okcode_save.

* PERFORM SICHERN.
  CALL SCREEN '0203'.
*  sy-ucomm = 'PROT'.
*  CALL SCREEN '204'.
  PERFORM okcode_prot_alv.

ENDFORM.                    "OKCODE_SAVE
*&---------------------------------------------------------------------*
*&      Form  OKCODE_VKM1
*&---------------------------------------------------------------------*
*       Aufruf Gesperrte zum Kunden                                    *
*----------------------------------------------------------------------*
FORM okcode_vkm1.

  DATA: v_kunde LIKE vbkred-knkli,
        v_kkber LIKE vbkred-kkber,
        v_sbgrp LIKE vbkred-sbgrp.

  IF NOT postab-vbeln IS INITIAL.
    GET PARAMETER ID 'KUN' FIELD v_kunde.
    GET PARAMETER ID 'KKB' FIELD v_kkber.
    GET PARAMETER ID 'KBG' FIELD v_sbgrp.

    SET PARAMETER ID 'KUN' FIELD postab-knkli.
    SET PARAMETER ID 'KKB' FIELD postab-kkber.
    SET PARAMETER ID 'KBG' FIELD space.

    CALL TRANSACTION 'VKM1' AND SKIP FIRST SCREEN.

    SET PARAMETER ID 'KUN' FIELD v_kunde.
    SET PARAMETER ID 'KKB' FIELD v_kkber.
    SET PARAMETER ID 'KBG' FIELD v_sbgrp.
  ELSE.
    MESSAGE s410.
  ENDIF.

ENDFORM.                               " OKCODE_VKM1
*&---------------------------------------------------------------------*
*&      Form  OKCODE_VKM2
*&---------------------------------------------------------------------*
*       Aufruf Freigegebene zum Kunden                                 *
*----------------------------------------------------------------------*
FORM okcode_vkm2.

  DATA: v_kunde LIKE vbkred-knkli,
        v_kkber LIKE vbkred-kkber,
        v_sbgrp LIKE vbkred-sbgrp.

  IF NOT postab-vbeln IS INITIAL.
    GET PARAMETER ID 'KUN' FIELD v_kunde.
    GET PARAMETER ID 'KKB' FIELD v_kkber.
    GET PARAMETER ID 'KBG' FIELD v_sbgrp.

    SET PARAMETER ID 'KUN' FIELD postab-knkli.
    SET PARAMETER ID 'KKB' FIELD postab-kkber.
    SET PARAMETER ID 'KBG' FIELD space.

    CALL TRANSACTION 'VKM2' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[2217124] " Added by <IT-CAR Tool> during Code Remediation

    SET PARAMETER ID 'KUN' FIELD v_kunde.
    SET PARAMETER ID 'KKB' FIELD v_kkber.
    SET PARAMETER ID 'KBG' FIELD v_sbgrp.
  ELSE.
    MESSAGE s410.
  ENDIF.

ENDFORM.                               " OKCODE_VKM2
*&---------------------------------------------------------------------*
*&      Form  OKCODE_VKM5
*&---------------------------------------------------------------------*
*       Aufruf Kreditstammblatt                                        *
*----------------------------------------------------------------------*
FORM okcode_vkm5.

  DATA: v_kunde LIKE vbkred-knkli,
        v_kkber LIKE vbkred-kkber.

  IF NOT postab-vbeln IS INITIAL.
    GET PARAMETER ID 'KUN' FIELD v_kunde.
    GET PARAMETER ID 'KKB' FIELD v_kkber.

    SET PARAMETER ID 'KUN' FIELD postab-knkli.
    SET PARAMETER ID 'KKB' FIELD postab-kkber.

    CALL TRANSACTION 'F.35' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[2270335] " Added by <IT-CAR Tool> during Code Remediation

    SET PARAMETER ID 'KUN' FIELD v_kunde.
    SET PARAMETER ID 'KKB' FIELD v_kkber.
  ELSE.
    MESSAGE s410.
  ENDIF.

ENDFORM.                               " OKCODE_VKM5
*&---------------------------------------------------------------------*
*&      Form  OKCODE_WEIT
*&---------------------------------------------------------------------*
*       Vertriebsvorgang wird einer anderen Bearbeitergruppe          *
*       zugeordnet
*---------------------------------------------------------------------*
FORM okcode_weit.

  MOVE postab-kkber TO vbkred-kkber.
  MOVE postab-sbgrp TO vbkred-sbgrp.
  CALL SCREEN '0202'
    STARTING AT  5  9
    ENDING   AT 60 11.
  MOVE vbkred-sbgrp TO t024b-sbgrp. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
*  Bei '##' wurde Übernahme abgebrochen
  IF t024b-sbgrp EQ '###'. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
    CLEAR: t024b-sbgrp,"#EC CI_USAGE_OK[2227014]
     vbkred-sbgrp. EXIT. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
  ENDIF.
*
  LOOP AT postab WHERE selkz EQ 'X'.   "****
*
    IF  NOT ( t024b-sbgrp IS INITIAL ). "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
      MOVE t024b-sbgrp TO postab-sbgrp_new. "#EC CI_USAGE_OK[2227014] " Added by <IT-CAR Tool> during Code Remediation
    ENDIF.
    PERFORM fpostab_fuellen USING 'W' sy-subrc.
  ENDLOOP.                             "****

*                                  T024B-SBGRP.

ENDFORM.                               " OKCODE_WEIT
*&---------------------------------------------------------------------*
*&      Form  OKCODE_WEIR
*&---------------------------------------------------------------------*
*       Zuordnung wird zurückgenommen                                  *
*----------------------------------------------------------------------*
FORM okcode_weir.

  PERFORM fpostab_loeschen USING 'W' sy-subrc.

ENDFORM.                               " OKCODE_WEIR
*---------------------------------------------------------------------*
*       FORM OKCODE_ZUKV                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM okcode_zukv.

  CALL FUNCTION 'SD_RISK_EXTERNAL_MENUE'
    EXPORTING
      i_document  = postab-vbeln
      i_aktyp     = 'A'
    EXCEPTIONS
      no_contract = 4.
  IF sy-subrc = 4.
    MESSAGE s781(v1).
  ENDIF.

ENDFORM.                    "OKCODE_ZUKV
*---------------------------------------------------------------------*
*       FORM TEXT_AUS_181T                                            *
*---------------------------------------------------------------------*
*       Aus der Tabelle T181T wird der gewünschte Text bereitgestellt *
*---------------------------------------------------------------------*
*  ---> T01-TXTAG  Textnummer des zu lesenden Textes                  *
* <---  T01-ZFELD  Zielfeld fuer den gelesenen Text                   *
*---------------------------------------------------------------------*
FORM text_aus_181t USING t01-txtnr t01-zfeld.
  t181t-spras = sy-langu.
  t181t-txtag = t01-txtnr.
  READ TABLE t181t.
  IF sy-subrc = 0.
    t01-zfeld = t181t-bildt.
  ELSE.
    t01-zfeld = space.
  ENDIF.
ENDFORM.                    "TEXT_AUS_181T
*---------------------------------------------------------------------*
*       FORM   TOP01_FUELLEN                                          *
*---------------------------------------------------------------------*
*       Im Feld TOP01 wird eine Überschriftszeile zusammengestellt.   *
*---------------------------------------------------------------------*
FORM top01_fuellen
  USING t01-text1 t01-text2 t01-text3 t01-text4.

  CLEAR: top01, dummy.
  top01(27)    = t01-text1.
  top01+45     = t01-text2.
  top01+56(14) = t01-text3.
  top01+71     = t01-text4.            "Kompromiss, um Werte anzeigen"
  "zu koennen
ENDFORM.                    "TOP01_FUELLEN
*eject
*---------------------------------------------------------------------*
*       FORM   TOP03_FUELLEN                                          *
*---------------------------------------------------------------------*
*       Im Feld TOP03 wird eine Überschriftszeile zusammengestellt.   *
*---------------------------------------------------------------------*
FORM top03_fuellen
  USING t03-text1 t03-text2 t03-text3 t03-text4.

  CLEAR: top03, dummy.
  top03(27)    = t03-text1.
  top03+45     = t03-text2.
  top03+56(14) = t03-text3.
  top03+71     = t03-text4.
ENDFORM.                    "TOP03_FUELLEN

*---------------------------------------------------------------------*
*       FORM   TOP02_FUELLEN                                          *
*---------------------------------------------------------------------*
*       Im Feld TOP02 wird eine Überschriftszeile zusammengestellt.   *
*---------------------------------------------------------------------*
FORM top02_fuellen
  USING t02-text1 t02-text2 t02-text3 t02-text4.

  CLEAR: top02, dummy.
  top02(27)    = t02-text1.
  top02+45     = t02-text2.
  top02+56(14) = t02-text3.
  top02+71     = t02-text4.
ENDFORM.                    "TOP02_FUELLEN

*---------------------------------------------------------------------*
*       FORM AUTHORITY_CHECK_CUSTOMER                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  US_VBKRED                                                     *
*  -->  CH_RC                                                         *
*---------------------------------------------------------------------*
FORM authority_check_customer USING    us_vbkred STRUCTURE vbkred
                              CHANGING ch_rc.
  ON CHANGE OF us_vbkred-ctlpc
  OR           us_vbkred-kkber
  OR           us_vbkred-klprz
  OR           us_vbkred-sbgrp.
    CALL FUNCTION 'CREDIT_AUTHORITYCHECK_CUSTOMER'
      EXPORTING
        actvt = '23'
        ctlpc = us_vbkred-ctlpc
        kkber = us_vbkred-kkber
        klprz = us_vbkred-klprz
        sbgrp = us_vbkred-sbgrp
      IMPORTING
        subrc = ch_rc.
    CHECK ch_rc NE 0.
    CHECK auth_message_knkk_sent NE 'X'.
    auth_message_knkk_sent = 'X'.
    IF ch_rc    EQ 8
    OR ch_rc    EQ 24
    OR ch_rc    EQ 28
    OR ch_rc    EQ 32
    OR ch_rc    EQ 34.
      MESSAGE e014 WITH 'V_KNKK_KRE'.
    ELSEIF ch_rc    EQ 4
    OR     ch_rc    EQ 12
    OR     ch_rc    EQ 16
    OR     ch_rc    EQ 24.
      MOVE-CORRESPONDING us_vbkred TO message_knkk_var1.
      MESSAGE i015 WITH message_knkk_var1 us_vbkred-klprz
                        '23' ch_rc.
    ENDIF.
  ENDON.
ENDFORM.                    "AUTHORITY_CHECK_CUSTOMER

*---------------------------------------------------------------------*
*       FORM AUTHORITY_CHECK_DOCUMENT                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  US_VBKRED                                                     *
*  -->  CH_RC                                                         *
*---------------------------------------------------------------------*
FORM authority_check_document CHANGING us_vbkred STRUCTURE vbkred
                                       ch_rc.
  DATA: da_sizec LIKE vbkred-sizec.

  IF da_sizec NE us_vbkred-sizec OR
     us_vbkred-sizec IS INITIAL.
    da_sizec = us_vbkred-sizec.
    IF us_vbkred-sizec IS INITIAL.
      CALL FUNCTION 'CREDIT_AUTHORITYCHECK_DOCUMENT'
        EXPORTING
          actvt  = '23'
          kkber  = us_vbkred-kkber
          kwkkb  = us_vbkred-kwkkb
          cmwae  = us_vbkred-cmwae
        IMPORTING
          rsizec = us_vbkred-sizec
          subrc  = ch_rc.
    ELSE.
      CALL FUNCTION 'CREDIT_AUTHORITYCHECK_DOCUMENT'
        EXPORTING
          actvt = '23'
          sizec = us_vbkred-sizec
        IMPORTING
          subrc = ch_rc.
    ENDIF.
  ENDIF.
  CHECK ch_rc NE 0.
  CHECK auth_message_vbuk_sent NE 'X'.
  auth_message_vbuk_sent = 'X'.
  IF ch_rc    EQ 8
  OR ch_rc    EQ 24
  OR ch_rc    EQ 28
  OR ch_rc    EQ 32
  OR ch_rc    EQ 34.
    MESSAGE e014 WITH 'V_VBUK_KRE'.
  ELSEIF ch_rc    EQ 4
  OR     ch_rc    EQ 12
  OR     ch_rc    EQ 16
  OR     ch_rc    EQ 24.
    MESSAGE i013 WITH us_vbkred-sizec '23' ch_rc.
  ENDIF.
ENDFORM.                    "AUTHORITY_CHECK_DOCUMENT
*eject
FORM tausender_werte.

  LOOP AT postab.
    DIVIDE postab-amtbl BY 1000.
    DIVIDE postab-kwkkb BY 1000.
    DIVIDE postab-kwkkf BY 1000.
    DIVIDE postab-oeikw BY 1000.
    DIVIDE postab-olikw BY 1000.
    DIVIDE postab-ofakw BY 1000.
    DIVIDE postab-klimk BY 1000.
    DIVIDE postab-kwkkd BY 1000.                           "P30K0022860
    postab-awaer = text-tsd.
    postab-awaer+1 = postab-cmwae.
    MODIFY postab.
  ENDLOOP.

  LOOP AT xvbkredet.
    DIVIDE xvbkredet-kwkkb BY 1000.
    DIVIDE xvbkredet-kwkkd BY 1000.
    MODIFY xvbkredet.
  ENDLOOP.

ENDFORM.                               " TAUSENDER_WERTE
*---------------------------------------------------------------------*
*       FORM STATUS_SETZEN                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  S01-LSTAT                                                     *
*---------------------------------------------------------------------*
FORM status_setzen USING s01-lstat.
  ls-lstat = s01-lstat.
  CASE ls-lstat.
    WHEN '0'.
      char = 'AEEN'.
    WHEN '1'.
      char = 'VEEN'.
    WHEN '2'.
      char = 'ANEN'.
    WHEN '3'.
      char = 'VNEN'.
    WHEN '4'.
      char = 'NESV'.
    WHEN '5'.
      char = 'NNSV'.
    WHEN '6'.
      char = 'NNSH'.
    WHEN '7'.
      char = 'NNDN'.
    WHEN '8'.
      char = 'VNMB'.
    WHEN '9'.
      char = 'VNME'.
  ENDCASE.
  CASE cardmode.
    WHEN 'X'.                          "CCARD
      char+4 = 'C'.
    WHEN 'F'.                          "Finanzdokumente
      exctab = 'CCAU'. APPEND exctab.
      exctab = 'CCAR'. APPEND exctab.
      exctab = 'PRKN'. APPEND exctab.
      exctab = 'PRZN'. APPEND exctab.
    WHEN OTHERS.
*------- Nada
  ENDCASE.
  CASE sy-tcode+2(2).
    WHEN 'M2'.                         "RECHECK BELEGE
      exctab = 'FREI'. APPEND exctab.
      exctab = 'FRER'. APPEND exctab.
  ENDCASE.
  SET PF-STATUS char(5) EXCLUDING exctab.
ENDFORM.                    "STATUS_SETZEN
*---------------------------------------------------------------------*
*       FORM ZUGRIFFSART_VERGLEICHEN                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(Z01-ZUART)                                              *
*  -->  Z01-NACHL                                                     *
*---------------------------------------------------------------------*
FORM zugriffsart_vergleichen
               USING value(z01-zuart)
                     z01-nachl.

  PERFORM zugriffsart_bestimmen USING anzgr ls-varnr.

  IF sy-subrc = 0.
*------- Maximale Lesetiefe bestimmen und merken ---------------------*
    IF t180z-zuart < max-zuart.
      z01-nachl = 'X'.
      MOVE t180z-zuart TO max-zuart.
    ELSE.
      z01-nachl = space.
    ENDIF.
  ELSE.
    MESSAGE a497 WITH 'T180Z'.
  ENDIF.
ENDFORM.                    "ZUGRIFFSART_VERGLEICHEN
*---------------------------------------------------------------------*
*       FORM ZUGRIFFSART_BESTIMMEN                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  Z01-ANZGR                                                     *
*  -->  Z01-VARNR                                                     *
*---------------------------------------------------------------------*
FORM zugriffsart_bestimmen USING z01-anzgr z01-varnr.
  SELECT SINGLE * FROM t180z
   WHERE rname = sy-repid
  AND   trvog = trvog
   AND   anzgr = z01-anzgr
   AND   varnr = z01-varnr.
ENDFORM.                    "ZUGRIFFSART_BESTIMMEN

*---------------------------------------------------------------------*
*       FORM vbuk_lesen                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM vbuk_lesen.
  rcode = 4.
  UNPACK sy-mandt TO vbuk_key-mandt.
  vbuk_key-vbeln = vbuk-vbeln .
  READ TABLE lvbuk WITH KEY vbuk_key.
  IF sy-subrc NE 0.
*    SELECT SINGLE *
*      INTO lvbuk
*      FROM vbuk
*      WHERE ( vbeln = vbuk-vbeln ).
    CALL FUNCTION 'SD_VBUK_READ_FROM_DOC'
      EXPORTING
        i_vbeln             = vbuk-vbeln
      IMPORTING
        es_vbuk             = lvbuk
      EXCEPTIONS
        vbeln_not_found     = 1
        vbtyp_not_supported = 2
        vbobj_not_supported = 3
        OTHERS              = 4.
    CHECK sy-subrc = 0.
    APPEND lvbuk .
  ENDIF.
  vbuk = lvbuk.
ENDFORM.                    "VBUK_LESEN

*&---------------------------------------------------------------------*
*&      Form  OKCODE_LEGE_ALV
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*
FORM okcode_lege_alv .

  CLEAR gt_col_lege.
  REFRESH gt_col_lege.
* Interne Tabelle fuellen:
  gt_col_lege-text = text-c01.
  gt_col_lege-col_code = 'C60'.    " color legend
  gt_col_lege-icon_displ = icon_reject.
  APPEND  gt_col_lege.

  CLEAR   gt_col_lege.
  gt_col_lege-text = text-c02.
  gt_col_lege-col_code = 'C50'.    " color legend
  gt_col_lege-icon_displ = icon_release.
  APPEND  gt_col_lege.

  CLEAR   gt_col_lege.
  gt_col_lege-text = text-c03.
  gt_col_lege-col_code = 'C30'.    " color legend
  gt_col_lege-icon_displ = icon_check.
  APPEND  gt_col_lege.

  CLEAR   gt_col_lege.
  gt_col_lege-text = text-c04.
  gt_col_lege-col_code = 'C40'.   " color legend
  gt_col_lege-icon_displ = icon_submit.
  APPEND  gt_col_lege.

* ALV rufen:

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title              = text-160
      i_selection          = ' '
      i_linemark_fieldname = 'COL_CODE'
      i_tabname            = 'GT_COL_LEGE'
      i_structure_name     = 'RVKRED01_ALV'
    TABLES
      t_outtab             = gt_col_lege
    EXCEPTIONS
      program_error        = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " OKCODE_LEGE_ALV
*&---------------------------------------------------------------------*
*&      Form  OKCODE_PROT_ALV
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*
FORM okcode_prot_alv .

  LOOP AT news.
    READ TABLE postab WITH KEY vbeln = news-vbeln.
    IF sy-subrc EQ 0.
*     Beleg aus Übersichtslisteliste löschen:
      DELETE postab INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  CLEAR: news-msgid, news-msgno, news-msgty.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
      i_callback_program             =  k_repid
      i_callback_pf_status_set       = 'HANDLE_EVENT_PF_STATUS'
      i_callback_user_command        = 'HANDLE_EVENT_USER_PROT'
      i_structure_name               = 'RVKRED012_ALV'
*     IS_LAYOUT                      =
*     IT_FIELDCAT                    =
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
*     IT_SORT                        =
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
*     I_SAVE                         = ' '
*     IS_VARIANT                     =
*     IT_EVENTS                      =
*     IT_EVENT_EXIT                  =
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*     IR_SALV_LIST_ADAPTER           =
*     IT_EXCEPT_QINFO                =
*     I_SUPPRESS_EMPTY_DATA          = ABAP_FALSE
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                       = news
   EXCEPTIONS
     program_error                  = 1
     OTHERS                         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " OKCODE_PROT_ALV

*&---------------------------------------------------------------------*
*&      Form  handle_event_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_event_pf_status USING rt_extab TYPE slis_t_extab. "#EC *

  SET PF-STATUS 'PROT' EXCLUDING rt_extab.


ENDFORM.                    " handle_event_pf_status

*&---------------------------------------------------------------------*
*&      Form  handle_event_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_event_user_prot USING r_ucomm     TYPE syucomm
                               rs_selfield TYPE slis_selfield. "#EC *


  CASE r_ucomm.
    WHEN 'LPRO'.
      READ TABLE news INDEX rs_selfield-tabindex.
      IF sy-subrc = 0.
        PERFORM protokoll_langtext.
      ENDIF.
    WHEN 'ERW'.
      rs_selfield-exit = 'X'.
  ENDCASE.
ENDFORM.                    " handle_event_user_prot

*&---------------------------------------------------------------------*
*&      Form  zeilen_icon_bestimmen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SELKZ  text
*      <--P_LV_ICON_DISPL  text
*----------------------------------------------------------------------*
FORM zeilen_icon_bestimmen  USING    p_selkz
                            CHANGING p_lv_icon_displ TYPE icon-id.
  CASE p_selkz.
    WHEN 'A'.
      p_lv_icon_displ = icon_reject.
    WHEN 'C'.
      p_lv_icon_displ = icon_check.
    WHEN 'F'.
      p_lv_icon_displ = icon_release.
    WHEN 'P'.
      p_lv_icon_displ = icon_check.
    WHEN 'Q'.
      p_lv_icon_displ = icon_check.
    WHEN 'W'.
      p_lv_icon_displ = icon_submit.
    WHEN OTHERS.
      p_lv_icon_displ = ''.
  ENDCASE.

ENDFORM.                    " zeilen_icon_bestimmen
