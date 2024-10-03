*&---------------------------------------------------------------------*
*& Report ZORDER_SETTLEMENT_AUTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zorder_settlement_auto.
SUBMIT RKO7CO88 "USING SELECTION-SET 'TEST01'
      WITH  kokrs = '1000'
      WITH aufnr = '000041042382'
      WITH PERIO = '005'
      WITH GJAHR =  '2024'
      WITH AUSF =  'X'
      WITH VAART =  '1'
      WITH BUPERIO =  '005'
      WITH SELEXP =  'X'
      WITH GRNDLIST =  'X'
      WITH COUNT =  '10'
      WITH RFC_NO =  '0'
      WITH BUDAT =  sy-datum
       AND RETURN .

*DATA : w_end_date         TYPE sy-datum,
*       w_end_time         TYPE sy-uzeit,
*       w_start_date       LIKE sy-datum,
*       w_start_time       LIKE sy-uzeit,
**       w_unit           LIKE t006-msehi VALUE 'MIN',    " TAG for Days,  H for Hours, MIN Minutes,
*       w_unit             LIKE t006-msehi VALUE 'TAG',    " TAG for Days,  H for Hours, MIN Minutes,
*       w_duration         TYPE i VALUE '1',
*       lv_start_timestamp TYPE string,
*       lv_end_timestamp   TYPE string.
*
*DATA: lt_bdcdata TYPE TABLE OF bdcdata,
*      ls_bdcdata TYPE bdcdata,
*      lt_bdcmsg  TYPE TABLE OF bdcmsgcoll,
*      lv_tcode   TYPE tcode VALUE 'KKS2'. " Tcode for maintaining controlling area settings
*TYPES : BEGIN OF ty_list,
*          text(5000) TYPE c,
*        END OF ty_list.
*
*DATA: it_list  TYPE STANDARD TABLE OF abaplist,
*      it_list1 TYPE STANDARD TABLE OF ty_list,
*      wa_list1 TYPE  ty_list.
*START-OF-SELECTION.
*
*  w_end_date  = sy-datum.
*  w_end_time  = sy-uzeit.
*  CALL FUNCTION 'START_TIME_DETERMINE'
*    EXPORTING
*      duration   = w_duration     " 20 hour
*      unit       = w_unit         " H for hours
**     FACTORY_CALENDAR           =
*    IMPORTING
*      start_date = w_start_date
*      start_time = w_start_time
*    CHANGING
*      end_date   = w_end_date
*      end_time   = w_end_time.
*  CONCATENATE  w_start_date w_start_time INTO lv_start_timestamp.
*  CONCATENATE  w_end_date w_end_time INTO lv_end_timestamp.
*
*  SELECT ca~idat2,  ca~kokrs, ca~aedat, ca~aezeit,  ca~aufnr, ca~objnr, js~stat,ca~werks
*   FROM caufv AS ca
*    INNER JOIN jest AS js ON ca~objnr = js~objnr
*    WHERE js~stat = 'I0045'  AND concat( ca~aedat, ca~aezeit ) BETWEEN @lv_start_timestamp AND @lv_end_timestamp
*  AND ca~werks IN ( '1401' , '1003' )
*  AND ca~aufnr NOT IN ( SELECT aufnr FROM zordersettlement ) AND    ca~objnr NOT IN ( SELECT objnr FROM auak ) INTO TABLE @DATA(it_data).
**  SELECT * FROM @it_data1 AS d1
**    WHERE d1~objnr NOT IN ( SELECT objnr FROM cosb ) INTO TABLE @DATA(it_data).
*BREAK-POINT.
*  IF it_data IS NOT INITIAL.
*    LOOP AT  it_data INTO DATA(WA_data).
*      DATA :lv_comp_code     TYPE bapi0002_2-comp_code,
*
*            lv_fiscal_year   TYPE bapi0002_4-fiscal_year,
*            lv_fiscal_period TYPE bapi0002_4-fiscal_period,
*            it_return        TYPE bapireturn1.
*      lv_comp_code =  1000.
*      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
*        EXPORTING
*          companycodeid = lv_comp_code
*          posting_date  = WA_data-idat2
*        IMPORTING
*          fiscal_year   = lv_fiscal_year
*          fiscal_period = lv_fiscal_period
*          return        = it_return.
*
*      SHIFT wa_data-aufnr LEFT DELETING LEADING '0'.
*            SUBMIT rkkks1n0 USING SELECTION-SET 'TEST01'
*      WITH  kokrs = WA_data-kokrs
*      WITH aufnr = WA_data-aufnr
*      WITH pa_poper = lv_fiscal_period
*      WITH pa_gjahr =  lv_fiscal_year
*       AND RETURN
*     .
*     leave to LIST-PROCESSING.
*
*
*
*
*
*
*    ENDLOOP.
*
*  ENDIF.
