FUNCTION zbapi_sd_cus_so_mapping.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      LT_KUNNR TYPE  /ACCGO/CAS_TT_KUNNR_RANGE OPTIONAL
*"      LT_FINAL STRUCTURE  ZSTSD_CUS_SO_MAPPING
*"----------------------------------------------------------------------
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 26.10.2023
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                : DEVK934554
*
*  Business Logic            : customer so mapping - XN missing & FN mismatch
*=======================================================================


******************Data Dec************************************

  DATA : ls_final    TYPE zstsd_cus_so_mapping,
         lt_position TYPE TABLE OF zhr_so_to_top,
         lv_pos      TYPE plans.

  DATA: lr_kunnr TYPE RANGE OF kunnr.

  IF lt_kunnr[] IS  NOT INITIAL.

    LOOP AT lt_kunnr INTO DATA(lw_kunnr).

      IF lw_kunnr-high IS NOT INITIAL.

        APPEND VALUE #( sign   = 'I'
                        option = 'BT'
                        low    = lw_kunnr-low
                        high   = lw_kunnr-high ) TO lr_kunnr.
      ELSE.

        APPEND VALUE #( sign   = 'I'
                        option = 'EQ'
                        low    = lw_kunnr-low ) TO lr_kunnr.

      ENDIF.

    ENDLOOP.
  ENDIF.

*********************Fetching the datas from zmis_cust_st , kna1 , knvv , knvp*********************

  SELECT customer_no,
         status FROM zmis_cust_st INTO TABLE @DATA(lt_mis) WHERE customer_no IN @lr_kunnr
                                                           AND status = '01'.

  IF lt_mis IS NOT INITIAL.

    SELECT a~kunnr,
           a~name1,
           b~vkorg,
           b~vtweg,
           b~spart,
           b~kdgrp,
           c~parvw,
           c~lifnr,
           c~pernr FROM kna1 AS a INNER JOIN knvv AS b ON a~kunnr = b~kunnr INNER JOIN knvp AS c ON a~kunnr = c~kunnr
                   INTO TABLE @DATA(lt_cust) FOR ALL ENTRIES IN @lt_mis  WHERE a~kunnr = @lt_mis-customer_no
                                                                         AND   c~parvw IN ( 'L1' , 'L2' , 'L3' , 'L5' , 'XN' , 'ZS' ).

    SELECT * FROM t151t INTO TABLE @DATA(lt_text).

  ENDIF.

**********sort it for binary search******************

  SORT : lt_cust BY kunnr vkorg vtweg spart parvw,
         lt_text BY kdgrp.

*****************Move to final it************************

  LOOP AT lt_cust INTO DATA(ls_cust).

****************customer grp desc*******************

    READ TABLE lt_text INTO DATA(ls_text) WITH KEY kdgrp = ls_cust-kdgrp BINARY SEARCH.

************check XN missing Customer *************************

    READ TABLE lt_cust INTO DATA(lv_lifnr) TRANSPORTING lifnr WITH KEY kunnr = ls_cust-kunnr
                                                                       vkorg = ls_cust-vkorg
                                                                       vtweg = ls_cust-vtweg
                                                                       spart = ls_cust-spart
                                                                       parvw = 'XN' BINARY SEARCH.

    IF sy-subrc NE 0.

      DELETE lt_cust WHERE kunnr = ls_cust-kunnr AND
                           vkorg = ls_cust-vkorg AND
                           vtweg = ls_cust-vtweg AND
                           spart = ls_cust-spart.

      ls_final-kunnr = ls_cust-kunnr.
      ls_final-name1 = ls_cust-name1.
      ls_final-vkorg = ls_cust-vkorg.
      ls_final-vtweg = ls_cust-vtweg.
      ls_final-spart = ls_cust-spart.
      ls_final-kdgrp = ls_cust-kdgrp.
      ls_final-remarks = 'XN is Missing'.
      ls_final-descr  = ls_text-ktext.

      APPEND ls_final TO lt_final.              "append to final it
      CLEAR  : ls_final,ls_text.
      CONTINUE.

    ELSE.

************check partner function mismatching*************************

      IF lv_lifnr-lifnr IS NOT INITIAL.

        REFRESH : lt_position.

        lv_pos = lv_lifnr-lifnr.

        CALL FUNCTION 'ZHR_SO_TO_TOP_POSITION'        "get data from postion fm
          EXPORTING
            so_position  = lv_pos
          TABLES
            it_positions = lt_position.

        IF lt_position IS NOT INITIAL.

          SORT lt_position BY pernr.

          READ TABLE lt_position TRANSPORTING NO FIELDS WITH KEY pernr = ls_cust-pernr BINARY SEARCH.

          IF sy-subrc NE 0.

            READ TABLE lt_position INTO DATA(ls_pos) WITH KEY parvw_unc = ls_cust-parvw.

            IF sy-subrc EQ 0.

              ls_final-pos_pernr = ls_pos-pernr.
              ls_final-kunnr = ls_cust-kunnr.
              ls_final-name1 = ls_cust-name1.
              ls_final-vkorg = ls_cust-vkorg.
              ls_final-vtweg = ls_cust-vtweg.
              ls_final-spart = ls_cust-spart.
              ls_final-kdgrp = ls_cust-kdgrp.
              ls_final-parvw = ls_cust-parvw.
              ls_final-lifnr = ls_cust-lifnr.
              ls_final-pernr = ls_cust-pernr.
              ls_final-remarks = 'Partner Function is Mismatching'.
              ls_final-descr  = ls_text-ktext.

              APPEND ls_final TO lt_final.              "append to final it
              CLEAR  : ls_final,ls_pos,ls_text.
              CONTINUE.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    IF ls_cust-parvw = 'XN'.

      ls_final-kunnr = ls_cust-kunnr.
      ls_final-name1 = ls_cust-name1.
      ls_final-vkorg = ls_cust-vkorg.
      ls_final-vtweg = ls_cust-vtweg.
      ls_final-spart = ls_cust-spart.
      ls_final-kdgrp = ls_cust-kdgrp.
      ls_final-parvw = ls_cust-parvw.
      ls_final-lifnr = ls_cust-lifnr.
      ls_final-pernr = ls_cust-pernr.
      ls_final-descr  = ls_text-ktext.

      APPEND ls_final TO lt_final.              "append to final it
      CLEAR  : ls_final,ls_pos,ls_text.
      CONTINUE.

    ENDIF.

  ENDLOOP.

ENDFUNCTION.
