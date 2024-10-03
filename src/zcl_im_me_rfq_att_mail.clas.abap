class ZCL_IM_ME_RFQ_ATT_MAIL definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_PURCHDOC_POSTED .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ME_RFQ_ATT_MAIL IMPLEMENTATION.


method IF_EX_ME_PURCHDOC_POSTED~POSTED.
"commented
*  DATA: L_EMAIL  TYPE AD_SMTPADR,
*        L_ADRNR  TYPE LFA1-ADRNR,
*        L_EKPO   TYPE TABLE OF UEKPO.
*
*  CLEAR: L_EMAIL,L_ADRNR,L_EKPO.
*"RFQ Attachment send to mail to Vendor mail address
*  IF IM_EKKO-BSART EQ 'ZSR' AND ( SY-TCODE EQ 'ME45'  AND ( SY-UCOMM = 'BU' OR SY-UCOMM = ' ' OR SY-UCOMM = 'FRGU' ) )
*          AND IM_EKKO-LIFNR IS NOT INITIAL  "" Vendor No
*          AND IM_EKKO-EBELN IS NOT INITIAL  "" RFQ No
*          AND IM_EKKO-FRGGR EQ 'RQ'         "" Release Group
*          AND IM_EKKO-FRGKE EQ '1'          "" RFQ Number With Final Release
*          AND IM_EKKO-PROCSTAT EQ '05'.     "" Release Completed
*
*    SELECT SINGLE ADRNR FROM LFA1 INTO L_ADRNR  WHERE LIFNR = IM_EKKO-LIFNR.
*
*"Vendor mail id
*    IF L_ADRNR IS NOT INITIAL.
*      SELECT SINGLE SMTP_ADDR FROM ADR6 INTO L_EMAIL WHERE ADDRNUMBER = L_ADRNR.
*    ENDIF.
*
*    SELECT * FROM EKPO INTO TABLE L_EKPO WHERE EBELN = IM_EKKO-EBELN AND LOEKZ = ' '.
*
*    IF L_EMAIL IS NOT INITIAL AND L_EKPO IS NOT INITIAL.
*      INCLUDE ZMM_RFQ_ATT_EMAIL.
*    ENDIF.
*
*  ENDIF.
"commented
endmethod.
ENDCLASS.
