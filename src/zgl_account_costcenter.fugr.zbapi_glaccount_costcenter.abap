FUNCTION zbapi_glaccount_costcenter.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      LT_GLACCOUNTS STRUCTURE  ZSTR_ACCOUNTS
*"      LT_COSTCENTER STRUCTURE  ZSTR_COSTCENTER
*"----------------------------------------------------------------------
*  Date Created - 29.04.2022
*& Created By   - KPABAP (samsudeen)
*& Description  - BAPI for Gl/Account AND Costcenter data
*  Reference - Ramakrishnan
*---------------------------------------------------------------------------
  TYPES: BEGIN OF lty_accounts,
           bukrs TYPE skb1-bukrs,
           saknr TYPE skb1-saknr,
         END OF lty_accounts.

  TYPES: BEGIN OF lty_costcenter,
           kokrs TYPE csks-kokrs,
           kostl TYPE csks-kostl,
           bukrs TYPE csks-bukrs,
         END OF lty_costcenter.


  DATA: lt_accounts     TYPE TABLE OF lty_accounts,
        ls_accounts     TYPE lty_accounts,
        lt_accounts_txt TYPE STANDARD TABLE OF skat,
        ls_accounts_txt TYPE skat,
        lt_cost         TYPE TABLE OF lty_costcenter,
        ls_cost         TYPE lty_costcenter,
        lt_cost_txt     TYPE STANDARD TABLE OF cskt,
        ls_cost_txt     TYPE cskt.

  DATA: ls_glaccounts TYPE zstr_accounts.
  DATA: ls_costcenter TYPE zstr_costcenter.

  REFRESH: lt_glaccounts,lt_costcenter,lt_accounts, lt_accounts_txt, lt_cost, lt_cost_txt.

  CLEAR: ls_accounts,ls_accounts_txt,ls_cost,ls_cost_txt.

************Taking Description of GL/accounts*******************
  SELECT * FROM skat
           INTO TABLE lt_accounts_txt
           WHERE ktopl EQ 'YAIN'
           AND spras EQ sy-langu.

  SELECT bukrs
         saknr FROM skb1
         INTO TABLE lt_accounts
         FOR ALL ENTRIES IN lt_accounts_txt
         WHERE saknr = lt_accounts_txt-saknr.

  IF lt_accounts[] IS NOT INITIAL.
    LOOP AT lt_accounts INTO ls_accounts.
      CLEAR ls_glaccounts.
      ls_glaccounts-company_code = ls_accounts-bukrs.
      ls_glaccounts-gl_account_no = ls_accounts-saknr.

*****Reading table lt_accounts_txt for GL/account description************
      READ TABLE lt_accounts_txt INTO ls_accounts_txt WITH KEY saknr = ls_accounts-saknr.
      IF sy-subrc EQ 0.
        ls_glaccounts-short_des = ls_accounts_txt-txt20.
        ls_glaccounts-long_des = ls_accounts_txt-txt50.
        APPEND ls_glaccounts TO lt_glaccounts.
      ENDIF.
    ENDLOOP.

    SORT lt_glaccounts BY  company_code gl_account_no.

  ENDIF.

*-------------------------------------------------------------------------------------------------------------------------------

  IF lt_glaccounts[] IS NOT INITIAL.
**********COSTCENTER DETAILS FOR LT_COSTCENTER*****************
    SELECT kokrs
           kostl
           bukrs FROM csks
           INTO TABLE lt_cost.
*********Taking Description of Costcenter*******************
    SELECT * FROM cskt
           INTO TABLE lt_cost_txt
           FOR ALL ENTRIES IN lt_cost
           WHERE kostl = lt_cost-kostl
           AND spras EQ sy-langu.

    LOOP AT lt_cost INTO ls_cost.

      CLEAR ls_costcenter.
      ls_costcenter-controlling_area = ls_cost-kokrs.
      ls_costcenter-company_code = ls_cost-bukrs.
      ls_costcenter-costcenter = ls_cost-kostl.

*****Reading table lt_cost_txt for costcenter description************
      READ TABLE lt_cost_txt INTO ls_cost_txt WITH KEY kostl = ls_cost-kostl.
      IF sy-subrc EQ 0.
        ls_costcenter-short_des = ls_cost_txt-ktext.
        ls_costcenter-long_des = ls_cost_txt-ltext.
        APPEND ls_costcenter TO lt_costcenter.
      ENDIF.
    ENDLOOP.
    IF sy-subrc EQ 0.
      SORT lt_costcenter BY controlling_area company_code costcenter.
    ENDIF.
  ENDIF.

ENDFUNCTION.
