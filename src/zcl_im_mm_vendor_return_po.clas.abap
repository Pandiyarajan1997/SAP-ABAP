class ZCL_IM_MM_VENDOR_RETURN_PO definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_PURCHDOC_POSTED .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_VENDOR_RETURN_PO IMPLEMENTATION.


METHOD IF_EX_ME_PURCHDOC_POSTED~POSTED.
*  Requirment given by : N.Umapathy
*  Developer : Savariar
*  Requirment : Mandatory field for Returen PO item
*  Date:08.06.2015

  DATA: WA_EKPO LIKE LINE OF IM_EKPO.
  DATA: LV_RETPO TYPE EKPO-RETPO.
  IF SY-TCODE = 'ME21N' OR SY-TCODE = 'ME22N'.
    IF IM_EKKO-BSART = 'ZVR'.
      LOOP AT IM_EKPO INTO WA_EKPO.
        LV_RETPO = WA_EKPO-RETPO.
        IF LV_RETPO  IS INITIAL.
          MESSAGE 'Return Po Must To activate Return Po Check Box item ' TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*   Requirment given by : N.Umapathy
*  Developer : Savariar
*  Requirment : Any one Fright condition only allow for PO screen ( Qty / value )
*  Date:08.06.2015
 IF SY-TCODE = 'ME21N' OR SY-TCODE = 'ME22N'.

DATA : LV_KSCHL TYPE KOMV-KSCHL.
DATA : LV_KBETR TYPE KOMV-KBETR.

DATA : LV_KSCHL1 TYPE KOMV-KSCHL.
DATA : LV_KBETR1 TYPE KOMV-KBETR.

DATA : WA_IM_KOMV LIKE LINE OF IM_KOMV.
LOOP AT IM_KOMV INTO WA_IM_KOMV.
IF WA_IM_KOMV-KSCHL = 'FRB1' AND  WA_IM_KOMV-KBETR <> 0  .
  LV_KSCHL = WA_IM_KOMV-KSCHL.
  LV_KBETR = WA_IM_KOMV-KBETR.
  ENDIF.
  IF  WA_IM_KOMV-KSCHL = 'FRC1'  AND  WA_IM_KOMV-KBETR <> 0.
   LV_KSCHL1 = WA_IM_KOMV-KSCHL.
   LV_KBETR1 = WA_IM_KOMV-KBETR.
    ENDIF.

    CLEAR  WA_IM_KOMV.
    ENDLOOP.
IF LV_KSCHL = 'FRB1' AND  LV_KBETR <> 0  .
  IF LV_KSCHL1 = 'FRC1' AND  LV_KBETR1 <> 0  .
MESSAGE 'Kindly Reomve Any One Fright Condition Type To Refer Purchase Info Record or Master data' TYPE 'E' DISPLAY LIKE 'I'.
ENDIF.
ENDIF.
ENDIF.


ENDMETHOD.
ENDCLASS.
