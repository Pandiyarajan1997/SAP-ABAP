FUNCTION zbapi_credit_blkrls.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FROM_DATE) TYPE  DATUM OPTIONAL
*"     VALUE(TO_DATE) TYPE  DATUM OPTIONAL
*"  TABLES
*"      LT_CREDIT_BLK STRUCTURE  ZSTR_CREDIT_BLK
*"----------------------------------------------------------------------
*=======================================================================
  "Created_by: Ramakrishnan
  "Created_on: 29.07.2022
  " Description: Credit blocked customer details
  "Referece by: Suresh & Praveen Kumar
*========================================================================

  TYPES: BEGIN OF ty_cdhdr,           "CDHDR table
           objectclas TYPE cdobjectcl,
           objectid   TYPE cdobjectv,
           changenr   TYPE cdchangenr,
           username   TYPE cdusername,
           udate      TYPE cddatum,
           utime      TYPE cduzeit,
           tcode      TYPE cdtcode,
         END OF ty_cdhdr,
         BEGIN OF ty_cdpos,           "CDPOS Table
           objectclas TYPE cdobjectcl,
           objectid   TYPE cdobjectv,
           changenr   TYPE cdchangenr,
           tabname    TYPE tabname,
           fname      TYPE fieldname,
           value_new  TYPE cdfldvaln,
           value_old  TYPE cdfldvalo,
         END OF ty_cdpos,
         BEGIN OF ty_vbak,           "VBAK Table
           vbeln TYPE vbeln_va,
           erdat TYPE erdat,
           erzet TYPE erzet,
           ernam TYPE ernam,
           netwr TYPE netwr_ak,
           waerk TYPE waerk,
           kunnr TYPE kunnr,
         END OF ty_vbak.

  DATA: lt_cdhdr TYPE TABLE OF ty_cdhdr,
        ls_cdhdr TYPE ty_cdhdr.

  DATA: lt_cdpos TYPE TABLE OF ty_cdpos,
        ls_cdpos TYPE ty_cdpos.

  DATA: lt_vbak TYPE TABLE OF ty_vbak,
        ls_vbak TYPE ty_vbak.

  DATA: lv_userid TYPE syuname." Variable for Userid
  DATA: lv_userid1 TYPE syuname." Variable for Userid

  DATA: ls_fm_struc  TYPE qisrsuser_data,
        ls_fm_struc1 TYPE qisrsuser_data.

  DATA: ls_credit_blk TYPE zstr_credit_blk. "Output Structure

  REFRESH: lt_cdhdr.

  IF from_date IS NOT INITIAL AND to_date IS NOT INITIAL.

****************Fetching Change Based on Input *******************
    SELECT objectclas
           objectid
           changenr
           username
           udate
           utime
           tcode FROM cdhdr
                 INTO TABLE lt_cdhdr
                 WHERE objectclas EQ 'VERKBELEG'
                 AND tcode EQ 'VKM4' AND
                 ( udate BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0.
      SORT lt_cdhdr[] BY objectid.
    ENDIF.

  ELSEIF from_date IS INITIAL AND to_date IS INITIAL.

****************Fetching Change Based on Input *******************
    SELECT objectclas
          objectid
          changenr
          username
          udate
          utime
          tcode FROM cdhdr
                INTO TABLE lt_cdhdr
                WHERE objectclas EQ 'VERKBELEG'
                AND tcode EQ 'VKM4'.
    IF sy-subrc EQ 0.
      SORT lt_cdhdr[] BY objectid.
    ENDIF.

  ELSEIF from_date IS NOT INITIAL.

****************Fetching Change Based on Input *******************
    SELECT objectclas
           objectid
           changenr
           username
           udate
           utime
           tcode FROM cdhdr
                 INTO TABLE lt_cdhdr
                 WHERE objectclas EQ 'VERKBELEG'
                 AND tcode EQ 'VKM4' AND
                 udate EQ from_date.
    IF sy-subrc EQ 0.
      SORT lt_cdhdr[] BY objectid.
    ENDIF.

  ENDIF.

  IF lt_cdhdr[] IS NOT INITIAL.

********** Fetching CDPOS table Data ***************************
    SELECT objectclas
           objectid
           changenr
           tabname
           fname
           value_new
           value_old FROM cdpos
                     INTO TABLE lt_cdpos
                     FOR ALL ENTRIES IN lt_cdhdr
                     WHERE objectclas EQ lt_cdhdr-objectclas
                     AND objectid EQ lt_cdhdr-objectid
                     AND tabname EQ 'VBAK'
                     AND fname EQ 'CMGST'
                     AND value_new EQ 'D'
                     AND value_old EQ 'B'.
    IF sy-subrc EQ 0.
      SORT lt_cdpos[] BY objectid.
    ENDIF.

    IF lt_cdpos[] IS NOT INITIAL.
********* Fetching VBAK table ***************************************
      SELECT vbeln
             erdat
             erzet
             ernam
             netwr
             waerk
             kunnr FROM vbak
                   INTO TABLE lt_vbak.
      IF sy-subrc EQ 0.
        SORT lt_vbak[] BY vbeln.
      ENDIF.

      IF lt_vbak[] IS NOT INITIAL.
*********** Fetching customer Name *************
        SELECT kunnr,name1 FROM kna1 INTO TABLE @DATA(lt_kna1)
                           FOR ALL ENTRIES IN @lt_vbak
                           WHERE kunnr EQ @lt_vbak-kunnr.
        IF sy-subrc EQ 0.
          SORT lt_kna1[] BY kunnr.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.

  REFRESH:lt_credit_blk.
  CLEAR ls_cdhdr.
  LOOP AT lt_cdhdr INTO ls_cdhdr.

    CLEAR ls_credit_blk.
    ls_credit_blk-released_by = ls_cdhdr-username.

    CLEAR: lv_userid,ls_fm_struc.
    lv_userid = ls_cdhdr-username.
    CALL FUNCTION 'ISR_GET_USER_DETAILS'
      EXPORTING
        id_user_id   = lv_userid "Release ID
      CHANGING
        is_user_data = ls_fm_struc.

    CLEAR ls_credit_blk-release_by_user.
    ls_credit_blk-release_by_user = ls_fm_struc-fullname. "Release ID Name

    ls_credit_blk-released_date = ls_cdhdr-udate. "Release Date
    ls_credit_blk-release_time = ls_cdhdr-utime. "Release Time

    CLEAR ls_vbak.
    READ TABLE lt_vbak INTO ls_vbak WITH KEY vbeln = ls_cdhdr-objectid.
    IF sy-subrc EQ 0.
      ls_credit_blk-sales_document = ls_vbak-vbeln.
      ls_credit_blk-blocked_by = ls_vbak-ernam.

      CLEAR: lv_userid1,ls_fm_struc1.
      lv_userid1 = ls_vbak-ernam.

      CALL FUNCTION 'ISR_GET_USER_DETAILS'
        EXPORTING
          id_user_id   = lv_userid1  "Blocked ID
        CHANGING
          is_user_data = ls_fm_struc1.

      CLEAR ls_credit_blk-block_by_user.
      ls_credit_blk-block_by_user = ls_fm_struc1-fullname. "Blocked ID Name
      ls_credit_blk-blocked_date = ls_vbak-erdat. "Blocked Date
      ls_credit_blk-blocked_time = ls_vbak-erzet. "Blocked Time
      ls_credit_blk-customer = ls_vbak-kunnr.
      ls_credit_blk-amount_value = ls_vbak-netwr. "Amount value
      ls_credit_blk-amount_crcy = ls_vbak-waerk.  "Amount Currency
    ENDIF.

    READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_credit_blk-customer.
    IF sy-subrc EQ 0.
      ls_credit_blk-customer_name = ls_kna1-name1.
    ENDIF.

    APPEND ls_credit_blk TO lt_credit_blk.
  ENDLOOP.

ENDFUNCTION.
