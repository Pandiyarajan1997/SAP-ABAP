INCLUDE ZYRVKREDTO.
*INCLUDE zzrvkredto.
*INCLUDE rvkredto.
DATA  okcode_ef15.


PARAMETERS: callmode(1) TYPE c NO-DISPLAY.
*------------------ Screen für ALV dispaly ----------------------------*
* Variante
SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME TITLE text-s02.
PARAMETERS: p_vari LIKE disvariant-variant.
PARAMETERS: code LIKE disvariant-log_group NO-DISPLAY.
PARAMETERS:     p_uname TYPE uname. " Added by anila on 27.02.2017
SELECTION-SCREEN END OF BLOCK 0.
*----------------------------------------------------------------------*
INITIALIZATION.
  MOVE 'I'    TO gbstk-sign.
  MOVE 'BT'   TO gbstk-option.
  MOVE 'A'    TO gbstk-low.
  MOVE 'B'    TO gbstk-high.
  APPEND gbstk.
*--------------------  ALV Verarbeitungsblock -------------------------*
  k_repid = sy-repid.
  k_tabname_header = 'POSTAB'.
  k_tabname_item   = 'XVBKREDET'.

* define keyinfo
  CLEAR kr_keyinfo.
  kr_keyinfo-header01 = 'VBELN'.
  kr_keyinfo-item01   = 'VBELN'.
  kr_keyinfo-header02 =  space.
  kr_keyinfo-item02   = 'POSNR'.
  GET PARAMETER ID 'KKL' FIELD flg_kkl.
*  PERFORM fieldcat_init  USING kr_fieldcat[].
*  PERFORM eventtab_build USING kr_events[].
*  PERFORM sp_group_build USING kr_sp_group[].
*
* Schalter Varianten benutzerspezifisch/allgemein speicherbar setzen
* Set Options: save variants userspecific or general
  k_save = 'A'.
  PERFORM variant_init.
** Get default variant
  kr_variant = k_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = k_save
    CHANGING
      cs_variant = kr_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_vari = kr_variant-variant.
  ENDIF.
* Process on value request
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant USING    k_variant
                                  k_save
                         CHANGING kr_variant.

* PAI
AT SELECTION-SCREEN.
  PERFORM pai_of_selection_screen.

START-OF-SELECTION.

GET vbkred.
  PERFORM vbkred_fuellen.

GET vbkredet.
  MOVE vbkredet TO xvbkredet. APPEND xvbkredet.

END-OF-SELECTION.

  SORT xvbkredet BY mandt vbeln posnr etenr.
* set flag to save open credit values for working in dialogue.
  CALL FUNCTION 'SD_CREDIT_OPEN_VALUES_SAVE'.
*
  DESCRIBE TABLE postab LINES sy-tfill.

  IF sy-tfill = 0.
    MESSAGE s420.
    IF callmode = 'X'.
      LEAVE PROGRAM.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.


  TYPES: BEGIN OF ty_custdays,
          bukrs TYPE bukrs,
          kunnr TYPE kunnr,
          zwidy TYPE zwidy,
          regio TYPE regio,
         END OF ty_custdays.

  TYPES: BEGIN OF ty_lineitems.
  INCLUDE  STRUCTURE bapi3007_2.
  TYPES:  due_date        TYPE netdt,
  END OF ty_lineitems.
  DATA:  lv_succ TYPE i.
  DATA : lv_fl TYPE c.
  DATA: ls_bapireturn TYPE bapireturn,
        lt_openitems  TYPE STANDARD TABLE OF bapi3007_2,
        ls_openitems  TYPE bapi3007_2,
        lt_lineitems TYPE STANDARD TABLE OF ty_lineitems,
        ls_lineitems TYPE ty_lineitems,
        lt_tvko TYPE STANDARD TABLE OF tvko,
        ls_tvko TYPE tvko,
        lt_kna1 TYPE STANDARD TABLE OF kna1,
        ls_kna1 TYPE kna1,
        lt_zdrauth TYPE STANDARD TABLE OF zdrauth,
        ls_zdrauth TYPE zdrauth,
        lt_zdrauth_temp TYPE STANDARD TABLE OF zdrauth,
        ls_zdrauth_temp TYPE zdrauth,
        lt_zdrauth_temp1 TYPE STANDARD TABLE OF zdrauth,
        ls_zdrauth_temp1 TYPE zdrauth,
        lt_custdays TYPE STANDARD TABLE OF ty_custdays,
        ls_custdays TYPE ty_custdays,
        ls_faede   TYPE faede,
        lv_text(90),
        lv_index TYPE sy-tabix,
        lv_zwidy TYPE zwidy,
        lv_due_days TYPE zwidy,
        lv_kunnr TYPE kunnr,
        lv_uname TYPE uname.

  DATA :lv_index1 TYPE i,
        flag1 TYPE c,
        flag2 TYPE c,
        status1 TYPE cstat,
        flag TYPE c,
        flag3 TYPE c,
        flag4 TYPE c,
        days TYPE zwidy,
        lv_name TYPE string,
        lv_status TYPE string.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv,
        is_layout TYPE  slis_layout_alv.

  DATA:    BEGIN OF lt_postab OCCURS 200.
          INCLUDE STRUCTURE vbkred.
  DATA:      status_bel TYPE icon-id,
             abgru_new LIKE vbap-abgru,
             sbgrp_new LIKE vbak-sbgrp,
             col(3)   TYPE c.            " Farbfeld für ALV
  DATA:    END   OF lt_postab.

  RANGES:         uname2  FOR zdrauth-uname,
                  regio2  FOR zdrauth-regio,
                  vbeln2    FOR vbkred-vbeln.

  SORT postab BY vbeln.

  IMPORT  flag FROM MEMORY ID 'DEF'.
  IF flag = 'X'.
    IMPORT  status1 FROM MEMORY ID 'ABC'.
    DELETE postab WHERE cstat NE status1.
    FREE MEMORY ID 'ABC'.
    FREE MEMORY ID 'DEF'.
  ENDIF.
  IMPORT flag1 FROM MEMORY ID 'GHI'.
  FREE MEMORY ID 'GHI'.
  IMPORT flag3 FROM MEMORY ID 'STO1'.
  FREE MEMORY ID 'STO'.
  IF flag3 = 'X'.
    IMPORT vbeln2[] FROM MEMORY ID 'ZYX1'.
    DELETE postab WHERE vbeln NOT IN vbeln2.
    FREE MEMORY ID 'ZYX1'.
  ENDIF.

  DELETE postab WHERE  cstat NE 'Dynamic check' AND cstat NE 'Oldest item'.
  IF postab[] IS NOT INITIAL.

    lt_postab[] = postab[].
    SORT lt_postab BY vkorg knkli.
    DELETE ADJACENT DUPLICATES FROM lt_postab COMPARING vkorg knkli.

    DESCRIBE TABLE postab LINES lv_succ.

    SORT postab BY vkorg knkli.

    SELECT * FROM tvko INTO TABLE lt_tvko FOR ALL ENTRIES IN postab
                                          WHERE vkorg = postab-vkorg ORDER BY PRIMARY KEY.
    IF p_uname IS INITIAL.
      SELECT * FROM zdrauth INTO TABLE  lt_zdrauth.
    ELSE.
      TRANSLATE  p_uname TO UPPER CASE .
      SELECT * FROM zdrauth INTO TABLE  lt_zdrauth
                             WHERE uname = p_uname.
    ENDIF.
    SELECT * FROM kna1 INTO TABLE lt_kna1
                       FOR  ALL ENTRIES IN postab
                       WHERE kunnr = postab-knkli ORDER BY PRIMARY KEY.

    LOOP AT lt_postab.
      CLEAR ls_custdays.
      READ TABLE lt_tvko INTO ls_tvko WITH KEY vkorg = lt_postab-vkorg BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
      ls_custdays-bukrs = ls_tvko-bukrs.
      ls_custdays-kunnr = lt_postab-knkli.

      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = lt_postab-knkli BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
      ls_custdays-regio = ls_kna1-regio.
      APPEND ls_custdays TO lt_custdays .
    ENDLOOP.

    SORT lt_custdays BY bukrs kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_custdays COMPARING bukrs kunnr.

    LOOP AT lt_custdays INTO ls_custdays.
      CLEAR ls_bapireturn.
      FREE: lt_openitems, lt_lineitems.
      CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'"#EC CI_USAGE_OK[2628704]
      "Added by SPLABAP during code remediation
        EXPORTING
          companycode = ls_custdays-bukrs
          customer    = ls_custdays-kunnr
          keydate     = sy-datum
          noteditems  = ' '
          secindex    = ' '
        IMPORTING
          return      = ls_bapireturn
        TABLES
          lineitems   = lt_openitems.

      DELETE lt_openitems WHERE db_cr_ind = 'H'.

      LOOP AT lt_openitems INTO ls_openitems.
        MOVE-CORRESPONDING ls_openitems TO ls_lineitems.
        CLEAR: ls_faede.
        ls_faede-shkzg = ls_lineitems-db_cr_ind.
        ls_faede-koart = 'D'.
        ls_faede-zfbdt = ls_lineitems-bline_date.
        ls_faede-zbd1t = ls_lineitems-dsct_days1.
        ls_faede-zbd2t = ls_lineitems-dsct_days2.
        ls_faede-zbd3t = ls_lineitems-netterms.
        ls_faede-rebzg = ls_lineitems-inv_ref.
        ls_faede-bldat = ls_lineitems-doc_date.
        CALL FUNCTION 'DETERMINE_DUE_DATE'
          EXPORTING
            i_faede                    = ls_faede
          IMPORTING
            e_faede                    = ls_faede
          EXCEPTIONS
            account_type_not_supported = 1
            OTHERS                     = 2.
        ls_lineitems-due_date = ls_faede-netdt.
        IF ls_faede-netdt LT sy-datum.
          APPEND ls_lineitems TO lt_lineitems.
        ENDIF.
        CLEAR: ls_openitems, ls_lineitems.
      ENDLOOP.

      SORT lt_lineitems BY due_date.
      READ TABLE lt_lineitems INTO  ls_lineitems INDEX 1.

      IF sy-subrc IS INITIAL.
        ls_custdays-zwidy =  ls_lineitems-due_date - sy-datum.
      ENDIF.
      MODIFY lt_custdays FROM ls_custdays TRANSPORTING zwidy.
      CLEAR ls_custdays.
    ENDLOOP.

    SORT lt_zdrauth BY zwidy.
    lt_zdrauth_temp[] = lt_zdrauth[].

    DELETE lt_zdrauth_temp WHERE zwidy LT days.

    LOOP AT postab.
      lv_index1 = sy-tabix.
      CLEAR: ls_tvko,lv_due_days.
      READ TABLE lt_tvko INTO ls_tvko WITH KEY vkorg = postab-vkorg BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation

      FREE: lt_openitems[], lt_lineitems[], ls_bapireturn, lv_kunnr.

      lv_kunnr = postab-knkli.
      CLEAR ls_kna1.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = lv_kunnr BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
      CLEAR ls_custdays.
      READ TABLE lt_custdays INTO ls_custdays WITH KEY bukrs = ls_tvko-bukrs
                                                       kunnr = lv_kunnr.

      IF postab-cstat = 'Dynamic check' AND ls_custdays-zwidy EQ 0.
* this customer's doc is blocked due to credit limit exceeeded
* and current status of customer is also not due
        CLEAR : ls_zdrauth_temp, ls_zdrauth.

        postab-regio = ls_kna1-regio.
        postab-status = postab-cstat.
        LOOP AT lt_zdrauth INTO ls_zdrauth WHERE bukrs = ls_tvko-bukrs AND regio = ls_kna1-regio AND zstatus = 'X'.
          IF ls_custdays-zwidy LE ls_zdrauth-zwidy.
            lv_name  = ls_zdrauth-uname.
            CONCATENATE lv_name postab-uname INTO postab-uname SEPARATED BY ','.
            MODIFY postab TRANSPORTING status uname regio zwidy WHERE vbeln = postab-vbeln   .
          ENDIF.
        ENDLOOP.
        MODIFY postab TRANSPORTING status uname regio zwidy WHERE vbeln = postab-vbeln   .
      ELSE.
        postab-zwidy = ls_custdays-zwidy.
        postab-regio = ls_kna1-regio.
        IF postab-cstat = 'Dynamic check'.
          postab-status = 'Dynamic Check and oldest item'.
        ELSE.
          postab-status = postab-cstat.
        ENDIF.
        LOOP AT lt_zdrauth_temp INTO ls_zdrauth_temp WHERE bukrs = ls_tvko-bukrs AND regio = ls_kna1-regio .
          IF ls_custdays-zwidy GT ls_zdrauth_temp-zwidy AND p_uname IS NOT INITIAL.
            DELETE postab INDEX lv_index1.
            CONTINUE.
          ENDIF.
*            ELSE.
          IF ls_custdays-zwidy LE ls_zdrauth_temp-zwidy.
            lv_name  = ls_zdrauth_temp-uname.
            CONCATENATE lv_name postab-uname INTO postab-uname SEPARATED BY ','.
            MODIFY postab TRANSPORTING status uname regio zwidy WHERE vbeln = postab-vbeln   .
          ENDIF.
*          ENDIF.
        ENDLOOP.

        MODIFY postab TRANSPORTING status uname regio zwidy WHERE vbeln = postab-vbeln   .
      ENDIF.
    ENDLOOP.
    IF status1 EQ 'Dynamic check'.
      lv_status = 'Dynamic Check and oldest item'.
      DELETE postab WHERE status = lv_status .
    ENDIF.

    IF p_uname IS NOT INITIAL.
      DELETE postab WHERE uname = ' ' .
    ENDIF.

    PERFORM fieldcatalog.
    PERFORM alv_display.
  ELSE.
    MESSAGE s420.
  ENDIF.



* Start Commented by anila
*  PERFORM user_sort USING rcode.
*  PERFORM tausender_werte.
*  rv75a-strep = 'X'.
*  aktyp       = 'V'.
*  ASSIGN (viewname) TO <struktur>.
*  PERFORM layout_build USING kr_layout."wegen Seltionskennzeichen
*  IF NOT flg_kkl IS INITIAL.
*    PERFORM reuse_alv_hierseq_list_display.
*  ELSE.
*    PERFORM reuse_alv_list_display.
*  ENDIF.

************* End Commented by anila

* ------ Module --------------------------------------------------------
* ------ Form-Routinen -------------------------------------------------
* - Include mit der Endung 'C' enthalten in der Regel Routinen, die auch
* -         von anderen Anzeigen genutz werden können, entsprechnde
* -         Routinen in SP heiße MV75AxxC

INCLUDE ZYRVKREDB0.
*  INCLUDE zzrvkredb0.
*  INCLUDE rvkredb0.
INCLUDE ZYMV75AFFC.
*  INCLUDE zzmv75affc.
*  INCLUDE mv75affc.
INCLUDE ZYMV75AFIC.
*  INCLUDE zzmv75afic.
*  INCLUDE mv75afic.
INCLUDE ZYRVKREDM0.
*  INCLUDE zzrvkredm0.
*  INCLUDE rvkredm0.                    "Module PAI und PBO
INCLUDE ZYRVKREDV0.
*  INCLUDE zzrvkredv0.
*  INCLUDE rvkredv0.
* ------ PF15 ------------------------------------------------------- *
INCLUDE ZYMENURETN.
*  INCLUDE zzmenuretn.
*  INCLUDE menuretn.

* ------ RV-spezifische Routinen ------------------------------------ *

* Kundenmodifikationen
INCLUDE ZYRVKREFZ1.
*  INCLUDE zzrvkrefz1.
*  INCLUDE rvkrefz1.
  INCLUDE zzrvkrefz2.
*  INCLUDE rvkrefz2.
  INCLUDE zzrvkrefz3.
*  INCLUDE rvkrefz3.
*--------------------- ALV-spezifische Includes -----------------------*
INCLUDE ZYRVKREALV.
*  INCLUDE zzrvkrealv.
*  INCLUDE rvkrealv.
INCLUDE ZYRVKALVAT.
*  INCLUDE zzrvkalvat.
*  INCLUDE rvkalvat.
INCLUDE ZYRVALVFORMS.
*  INCLUDE zzrvalvforms.
*  INCLUDE rvalvforms.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  LISTAUSGABE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE listausgabe OUTPUT.

  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  CASE sy-ucomm.
    WHEN 'PROT'.
      PERFORM okcode_prot_alv.
    WHEN 'LEGE'.
      PERFORM okcode_lege_alv.
  ENDCASE.
  LEAVE SCREEN.
ENDMODULE.                             " LISTAUSGABE  OUTPUT
" FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcatalog .

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VBELN'.
  ls_fieldcat-seltext_l  = 'Sales order'.
  ls_fieldcat-seltext_m  = 'Sales order'.
  ls_fieldcat-seltext_s  = 'Sales.ord.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = 'UNAME'.
  ls_fieldcat-seltext_l  = 'Authorised Users'.
  ls_fieldcat-seltext_m  = 'Authorised Users'.
  ls_fieldcat-seltext_s  = 'Auth. Usrs.'.
  ls_fieldcat-emphasize = 'X'.
*    ls_fieldcat-tech         = 'X'.
*    ls_fieldcat-sp_group = 'A'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = 'ZWIDY'.
  ls_fieldcat-seltext_l  = 'With in days'.
  ls_fieldcat-seltext_m  = 'Within. days'.
  ls_fieldcat-seltext_s  = 'Wit.days.'.
*    ls_fieldcat-tech         = 'X'.
*    ls_fieldcat-sp_group = 'A'.
  APPEND ls_fieldcat TO lt_fieldcat.


  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KNKLI'.
  ls_fieldcat-seltext_l  = 'Customer account number'.
  ls_fieldcat-seltext_m  = 'Customer number'.
  ls_fieldcat-seltext_s  = 'Cust.num.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NAME1'.
  ls_fieldcat-seltext_l  = 'Customer name'.
  ls_fieldcat-seltext_m  = 'Customer name'.
  ls_fieldcat-seltext_s  = 'Cust.name.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATUS'.
  ls_fieldcat-seltext_l  = 'Status'.
  ls_fieldcat-seltext_m  = 'Status'.
  ls_fieldcat-seltext_s  = 'Status'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = 'REGIO'.
  ls_fieldcat-seltext_l  = 'Cutomer region'.
  ls_fieldcat-seltext_m  = 'Cutomer reg.'.
  ls_fieldcat-seltext_s  = 'Cust. reg.'.
*    ls_fieldcat-tech         = 'X'.
*    ls_fieldcat-sp_group = 'A'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KKBER'.
  ls_fieldcat-seltext_l  = 'Credit control area'.
  ls_fieldcat-seltext_m  = 'Credit cntrl area'.
  ls_fieldcat-seltext_s  = 'Crdt.cntrl.area'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VKORG'.
  ls_fieldcat-seltext_l  = 'Sales Organization'.
  ls_fieldcat-seltext_m  = 'Sales Org.'.
  ls_fieldcat-seltext_s  = 'Sales Org.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VTWEG'.
  ls_fieldcat-seltext_l  = 'Distribution Channel'.
  ls_fieldcat-seltext_m  = 'Distribution Chanel.'.
  ls_fieldcat-seltext_s  = 'Dist. Chnl.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SPART'.
  ls_fieldcat-seltext_l  = 'Division'.
  ls_fieldcat-seltext_m  = 'Division'.
  ls_fieldcat-seltext_s  = 'Division'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ORT01'.
  ls_fieldcat-seltext_l  = 'City'.
  ls_fieldcat-seltext_m  = 'City'.
  ls_fieldcat-seltext_s  = 'City'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ADRNR'.
  ls_fieldcat-seltext_l  = 'Address'.
  ls_fieldcat-seltext_m  = 'Address'.
  ls_fieldcat-seltext_s  = 'Address'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ERDAT'.
  ls_fieldcat-seltext_l  = 'Created On'.
  ls_fieldcat-seltext_m  = 'Created On'.
  ls_fieldcat-seltext_s  = 'Created On'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ERNAM'.
  ls_fieldcat-seltext_l  = 'Created by'.
  ls_fieldcat-seltext_m  = 'Created by'.
  ls_fieldcat-seltext_s  = 'Created by'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'OFAKW'.
  ls_fieldcat-seltext_l  = 'Open Billing Document credit value'.
  ls_fieldcat-seltext_m  = 'Opn. Bill. Doc crdt. val.'.
  ls_fieldcat-seltext_s  = 'Opn Bil Doc val.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AWAER'.
  ls_fieldcat-seltext_l  = 'Currency'.
  ls_fieldcat-seltext_m  = 'Currency'.
  ls_fieldcat-seltext_s  = 'Currency'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ZTERM'.
  ls_fieldcat-seltext_l  = 'Payment Terms'.
  ls_fieldcat-seltext_m  = 'Payment Terms'.
  ls_fieldcat-seltext_s  = 'Paymt.Trms'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SKFOR'.
  ls_fieldcat-seltext_l  = 'Total receivables'.
  ls_fieldcat-seltext_m  = 'Tot. receivables'.
  ls_fieldcat-seltext_s  = 'Tot. recbles.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NETWR'.
  ls_fieldcat-seltext_l  = 'Net value'.
  ls_fieldcat-seltext_m  = 'Net value'.
  ls_fieldcat-seltext_s  = 'Net val.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TAGEF'.
  ls_fieldcat-seltext_l  = 'Credit limit'.
  ls_fieldcat-seltext_m  = 'Credit limit'.
  ls_fieldcat-seltext_s  = 'Crdt lmt.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VKBUR'.
  ls_fieldcat-seltext_l  = 'Sales Office'.
  ls_fieldcat-seltext_m  = 'Sales Office'.
  ls_fieldcat-seltext_s  = 'Sal. Ofc.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VKGRP'.
  ls_fieldcat-seltext_l  = 'Sales Group'.
  ls_fieldcat-seltext_m  = 'Sales Group'.
  ls_fieldcat-seltext_s  = 'Sal. Grp.'.
  APPEND ls_fieldcat TO lt_fieldcat.



ENDFORM.                    "fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM alv_display .

  is_layout-colwidth_optimize = 'X'.

  IF postab IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
*        I_INTERFACE_CHECK                 = ' '
*        I_BYPASSING_BUFFER                = ' '
*        I_BUFFER_ACTIVE                   = ' '
*        I_CALLBACK_PROGRAM                = ' '
*        I_CALLBACK_PF_STATUS_SET          = ' '
*        I_CALLBACK_USER_COMMAND           = ' '
*        I_CALLBACK_TOP_OF_PAGE            = ' '
*        I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*        I_CALLBACK_HTML_END_OF_LIST       = ' '
*        I_STRUCTURE_NAME                  =
*        I_BACKGROUND_ID                   = ' '
*        I_GRID_TITLE                      =
*        I_GRID_SETTINGS                   =
        is_layout                         = is_layout
       it_fieldcat                       = lt_fieldcat[]
*        IT_EXCLUDING                      =
*        IT_SPECIAL_GROUPS                 =
*        IT_SORT                           =
*        IT_FILTER                         =
*        IS_SEL_HIDE                       =
*        I_DEFAULT                         = 'X'
*        I_SAVE                            = ' '
*        IS_VARIANT                        =
*        IT_EVENTS                         =
*        IT_EVENT_EXIT                     =
*        IS_PRINT                          =
*        IS_REPREP_ID                      =
*        I_SCREEN_START_COLUMN             = 0
*        I_SCREEN_START_LINE               = 0
*        I_SCREEN_END_COLUMN               = 0
*        I_SCREEN_END_LINE                 = 0
*        I_HTML_HEIGHT_TOP                 = 0
*        I_HTML_HEIGHT_END                 = 0
*        IT_ALV_GRAPHICS                   =
*        IT_HYPERLINK                      =
*        IT_ADD_FIELDCAT                   =
*        IT_EXCEPT_QINFO                   =
*        IR_SALV_FULLSCREEN_ADAPTER        =
*      IMPORTING
*        E_EXIT_CAUSED_BY_CALLER           =
*        ES_EXIT_CAUSED_BY_USER            =
      TABLES
        t_outtab                          = postab[]
     EXCEPTIONS
       program_error                     = 1
       OTHERS                            = 2
              .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.


ENDFORM.                    " ALV_DISPLAY
