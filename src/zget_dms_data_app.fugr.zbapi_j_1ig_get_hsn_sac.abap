FUNCTION zbapi_j_1ig_get_hsn_sac.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_WERKS) TYPE  WERKS_D OPTIONAL
*"  TABLES
*"      IT_HSN STRUCTURE  ZSTR_PEPUP_HSN
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------


  TYPES : BEGIN OF gs_marc,
            matnr TYPE marc-matnr,
            werks TYPE marc-werks,
            steuc TYPE marc-steuc,
          END OF gs_marc.

  DATA : gt_marc TYPE TABLE OF gs_marc,
         wa_marc TYPE gs_marc.

  CLEAR : it_hsn .
  IF im_werks IS NOT INITIAL.


    SELECT matnr werks steuc INTO TABLE it_hsn FROM marc
                                        WHERE werks = im_werks.

    DELETE it_hsn WHERE steuc EQ ' '.

  ELSE.   " IF im_werks IS INITIAL.
*    MESSAGE E001(J_1IG_MSGS).

    SELECT matnr,status FROM mara INTO TABLE @DATA(lt_mara) WHERE status = @abap_false.
    IF lt_mara IS NOT INITIAL.
      SELECT matnr werks steuc INTO TABLE it_hsn FROM marc FOR ALL ENTRIES IN lt_mara WHERE matnr = lt_mara-matnr.
      DELETE it_hsn WHERE steuc EQ ' '.
      SORT it_hsn BY matnr.
      DELETE ADJACENT DUPLICATES FROM it_hsn COMPARING matnr.
    ENDIF.
  ENDIF.

*  ELSEIF im_asnum IS NOT INITIAL.
*    SELECT SINGLE taxtariffcode INTO ex_hsn_sac FROM asmd
*                                                WHERE asnum = im_asnum.
  "ENDIF.



ENDFUNCTION.
