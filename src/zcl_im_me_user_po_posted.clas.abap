class ZCL_IM_ME_USER_PO_POSTED definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_PURCHDOC_POSTED .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ME_USER_PO_POSTED IMPLEMENTATION.


METHOD IF_EX_ME_PURCHDOC_POSTED~POSTED.
  " Created By Govind On 12-01-2015 Requirement Given By Umapathy Regarding Sales Po Rect.
  DATA : LV_ERDAT TYPE EKKO-AEDAT.
  DATA : LV_AEDAT TYPE SY-DATUM.
  LV_AEDAT = SY-DATUM+06.
  LV_ERDAT = IM_EKKO-BEDAT+06.
  IF IM_EKKO-ERNAM = '717299' OR IM_EKKO-ERNAM = '715608'
     OR  IM_EKKO-ERNAM = '912396' OR IM_EKKO-ERNAM = '324033'
     OR IM_EKKO-ERNAM = '254040' OR IM_EKKO-ERNAM = '534008'
    OR IM_EKKO-ERNAM = '451473' OR IM_EKKO-ERNAM = '464016'
    OR IM_EKKO-ERNAM = '324032' OR IM_EKKO-ERNAM = '814002'
    OR IM_EKKO-ERNAM = '394024' OR IM_EKKO-ERNAM = '353611'
     OR IM_EKKO-ERNAM = '322032'  .
    IF SY-TCODE = 'ME21N'.
      IF IM_EKKO-BSART = 'UB' OR IM_EKKO-BSART = 'YUB'.
        IF IM_EKKO-RESWK = '1100'.
          IF LV_AEDAT BETWEEN '01' AND  '10' AND  LV_ERDAT BETWEEN  '01' AND  '10' .
          ELSE.
            MESSAGE 'Concern Document Type Purchse Order Not allowed after 10th in Every Month' TYPE 'E'.
            LEAVE TO  CURRENT TRANSACTION.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.



ENDMETHOD.
ENDCLASS.
