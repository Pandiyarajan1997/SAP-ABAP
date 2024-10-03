FUNCTION zbapi_batch_number.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CREATED_ON) TYPE  ERSDA
*"  TABLES
*"      LT_BATCH STRUCTURE  ZSTR_BATCH
*"----------------------------------------------------------------------
*Date Created - 07.05.2022
*& Created By - KPABAP (Shamsudeen)
*& Description - Bapi For Batch Numbers From Table (MCHA).
*TR NO = DEVK931683
*------------------------------------------------------------------------------------

  TYPES: BEGIN OF ty_mcha,
           matnr TYPE matnr,
           werks TYPE werks_d,
           charg TYPE charg_d,
           ersda TYPE ersda,
         END OF ty_mcha.

  DATA: lt_mcha TYPE STANDARD TABLE OF ty_mcha,
        ls_mcha TYPE ty_mcha.
  DATA: ls_batch  TYPE zstr_batch.


  IF created_on IS NOT INITIAL.
***************Based on Created_on Input Fetching MATNR,WERKS,CHARG From MCHA*********************
    SELECT matnr
           werks
           charg
           ersda FROM mcha
           INTO TABLE lt_mcha
           WHERE ersda EQ Created_on.

    LOOP AT lt_mcha INTO ls_mcha.
      ls_batch-matnr =  ls_mcha-matnr.
      ls_batch-werks = ls_mcha-werks.
      ls_batch-charg = ls_mcha-charg.
      ls_batch-ersda = ls_mcha-ersda.
      APPEND ls_batch TO lt_batch.
    ENDLOOP.

  ENDIF.




ENDFUNCTION.
