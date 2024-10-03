FUNCTION zbapi_glacc_exp_depreciation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMP_CODE) TYPE  BUKRS
*"     VALUE(GL_ACCOUNT) TYPE  BKK_R_HKONT_BCA OPTIONAL
*"     VALUE(FISC_YEAR) TYPE  GJAHR
*"     VALUE(PERIOD) TYPE  MONAT
*"     VALUE(ALV_CHECK) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      LT_GL_EXPENSE STRUCTURE  ZSTR_GLACC_EXP_DEPRECIATION
*"----------------------------------------------------------------------


  TYPES: BEGIN OF ty_bsis,
           bukrs TYPE bukrs,
           hkont TYPE hkont,
           gjahr TYPE gjahr,
           belnr TYPE belnr_d,
           budat TYPE budat,
           bldat TYPE bldat,
           blart TYPE blart,
           waers TYPE waers,
           monat TYPE monat,
           shkzg TYPE shkzg,
           dmbtr TYPE dmbtr,
           werks TYPE werks_d,
           kostl TYPE kostl,
           sgtxt TYPE sgtxt,
           prctr TYPE prctr,
           geber TYPE bp_geber,
         END OF ty_bsis.

  DATA: lt_bsis TYPE TABLE OF ty_bsis,
        ls_bsis TYPE ty_bsis.

  DATA: lt_cost_txt     TYPE STANDARD TABLE OF cskt,
        ls_cost_txt     TYPE cskt,
        lt_accounts_txt TYPE STANDARD TABLE OF skat,
        ls_accounts_txt TYPE skat.

  DATA : lr_hkont TYPE bkk_r_hkont_bca,
         wa_hkont LIKE LINE OF lr_hkont.

  DATA: ls_gl_expense TYPE zstr_glacc_exp_depreciation.

  DATA : lv_amnt TYPE acdoca-wsl.

  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name = 'DEPRECIATION_GL_ACNT'
                                                   AND   type = 'S'.
  IF sy-subrc = 0.
    LOOP AT lt_tvarvc INTO DATA(wa_tvarvc).

      wa_hkont-sign   = wa_tvarvc-sign.
      wa_hkont-option = wa_tvarvc-opti.
      wa_hkont-low    = wa_tvarvc-low.

      APPEND wa_hkont TO lr_hkont.
      CLEAR wa_hkont.
    ENDLOOP.

    IF gl_account[] IS NOT INITIAL.
      REFRESH : lr_hkont.
      LOOP AT gl_account INTO DATA(wa_tvarvc2).

        wa_hkont-sign   = wa_tvarvc2-sign.
        wa_hkont-option = wa_tvarvc2-option.
        wa_hkont-low    = wa_tvarvc2-low.

        APPEND wa_hkont TO lr_hkont.
        CLEAR wa_hkont.
      ENDLOOP.

    ENDIF.

  ENDIF.
***************fetch from bkpf & acdoca for asset depretiation value**********
  SELECT b~rbukrs,b~gjahr,b~belnr,b~rldnr,b~bldat,b~budat,b~rcntr,b~anln1,b~rwcur,
         b~poper,b~racct,b~prctr,b~wsl,b~werks,b~sgtxt,b~rfund,b~drcrk,b~blart
                                               FROM acdoca AS b
                                               INTO TABLE @DATA(lt_asset)
                                               WHERE rldnr    = '0L'
                                               AND   b~rbukrs = @comp_code
                                               AND   b~gjahr  = @fisc_year
                                               AND   poper    = @period
                                               AND   racct  IN @lr_hkont
                                               AND   blart    = 'AF'.
  IF sy-subrc = 0.
    SORT lt_asset BY racct.

    SELECT * FROM cskt
             INTO TABLE lt_cost_txt
             FOR ALL ENTRIES IN lt_bsis
             WHERE kokrs = lt_bsis-bukrs
             AND kostl = lt_bsis-kostl
             AND spras EQ sy-langu.


    SELECT * FROM skat
             INTO TABLE lt_accounts_txt
             FOR ALL ENTRIES IN lt_bsis
             WHERE saknr = lt_bsis-hkont
             AND ktopl EQ 'YAIN'
             AND spras EQ sy-langu.

    SORT lt_cost_txt[] BY kostl.
    SORT lt_accounts_txt[] BY saknr.

    LOOP AT lt_asset ASSIGNING FIELD-SYMBOL(<fs_asset>).

      CLEAR ls_gl_expense.
      CLEAR ls_accounts_txt.

      READ TABLE lt_accounts_txt INTO ls_accounts_txt WITH KEY saknr = <fs_asset>-racct.
      IF sy-subrc EQ 0.
        ls_gl_expense-glacc_des = ls_accounts_txt-txt50.
      ENDIF.

      CLEAR ls_cost_txt.
      READ TABLE lt_cost_txt INTO ls_cost_txt WITH KEY kostl = <fs_asset>-rcntr.
      IF sy-subrc EQ 0.
        ls_gl_expense-costcenter_des = ls_cost_txt-ltext.
      ENDIF.

      ls_gl_expense-bukrs        = <fs_asset>-rbukrs.
      ls_gl_expense-plant        = <fs_asset>-werks.
      ls_gl_expense-gl_account   = <fs_asset>-racct.
      ls_gl_expense-fisc_year    = <fs_asset>-gjahr.
      ls_gl_expense-fisc_period  = <fs_asset>-poper.
      ls_gl_expense-doc_number   = <fs_asset>-belnr.
      ls_gl_expense-doc_date     = <fs_asset>-bldat.
      ls_gl_expense-posting_date = <fs_asset>-budat.
      ls_gl_expense-costcenter   = <fs_asset>-rcntr.
      ls_gl_expense-amount       = <fs_asset>-wsl.
      ls_gl_expense-currency     = <fs_asset>-rwcur.
      ls_gl_expense-profit_cntr  = <fs_asset>-prctr.
      ls_gl_expense-sgtxt        = <fs_asset>-sgtxt.
      ls_gl_expense-geber        = <fs_asset>-rfund.
      ls_gl_expense-doc_type     = <fs_asset>-blart.
      ls_gl_expense-dc_indicator = <fs_asset>-drcrk.

      APPEND ls_gl_expense TO lt_gl_expense.

    ENDLOOP.

  ENDIF.

  IF alv_check = abap_true AND lt_gl_expense[] IS NOT INITIAL.

    CALL FUNCTION 'Z_POPUP_ALV'
*   EXPORTING
*     I_REPID                   =
*     I_START_COLUMN            = 25
*     I_START_LINE              = 6
*     I_END_COLUMN              = 200
*     I_END_LINE                = 20
*     I_TITLE                   = 'List Display'
*     I_STATUS_FIELD_NAME       = ''
*     I_HYPERLINK_COLUMN        =
*     I_HYPERLINK_DATA          =
*     I_HIDE_COLUMN             =
*     I_HIDE_COLUMN_EXT         =
*     I_POPUP                   =
*     I_LAYOUT                  =
      TABLES
        it_alv = lt_gl_expense.


  ENDIF.

ENDFUNCTION.
