FUNCTION ZJ_1IG_GET_HSN_SAC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_WERKS) TYPE  WERKS_D OPTIONAL
*"  TABLES
*"      IT_HSN STRUCTURE  ZSTR_PEPUP_HSN
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------


  TYPES : BEGIN OF GS_MARC,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
          STEUC TYPE MARC-STEUC,
        END OF GS_MARC.

  DATA : GT_MARC TYPE TABLE OF GS_MARC,
               WA_MARC TYPE GS_MARC.

  CLEAR : IT_HSN .
  IF IM_WERKS IS NOT INITIAL.


    SELECT MATNR WERKS STEUC INTO TABLE IT_HSN FROM MARC
                                        WHERE WERKS = IM_WERKS.

    DELETE IT_HSN WHERE STEUC EQ ' '.

  ELSE.   " IF im_werks IS INITIAL.
    MESSAGE E001(J_1IG_MSGS).

  ENDIF.

*  ELSEIF im_asnum IS NOT INITIAL.
*    SELECT SINGLE taxtariffcode INTO ex_hsn_sac FROM asmd
*                                                WHERE asnum = im_asnum.
  "ENDIF.



ENDFUNCTION.
