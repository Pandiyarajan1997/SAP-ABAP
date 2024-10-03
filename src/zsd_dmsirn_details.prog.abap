*&---------------------------------------------------------------------*
*& Report ZSD_DMSIRN_DETAILS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSD_DMSIRN_DETAILS.

TABLES : vbrk,kna1.

* structure of the table that need to be displayed inside the tcode after selection

TYPES : BEGIN OF praveen,
        BUKRS type BUKRS,
        DISTRB type Z_DIST_CODE, " data elemnt which can be seen in the table of ZDMS_INVOICE_IRN while doubleclick on DISTRB"
        Distname type NAME1_GP,
        DOCNO type J_1IG_DOCNO,
**        fkdat type fkdat,
*        netwr type netwr,
*        mwsbk type mwsbp,
*        total type netwr,
*DOC_TYPE type  J_1IG_DOCTYP,

  END OF praveen.

  TYPES : BEGIN OF Kumar,

    KUNNR type KUNNR,
    NAME1 type NAME1_GP,

    END OF kumar.


  DATA : it_Praveen type table of Praveen,
         it_kumar type table of kumar.

*Selection screen design

SELECT-OPTIONS : BillDOC for vbrk-VBELN, " this is used for removing to select option NO INTERVALS no-EXTENSION.
                 DISTRB for kna1-KUNnr.

*Select querry from ZDMS_INVOICE_IRN and KNA1

Select BUKRS
 DISTRB
  DOCNO
*   netwr
   from ZDMS_INVOICE_IRN into CORRESPONDING FIELDS OF TABLE it_praveen WHERE DOCNO in BillDOC and DISTRB in DISTRB.

  Select KUNNR name1
  from KNA1 into CORRESPONDING FIELDS OF TABLE it_kumar.

    loop at it_praveen ASSIGNING FIELD-SYMBOL(<fs_praveen>).
      READ TABLE it_kumar ASSIGNING FIELD-SYMBOL(<fs_kumar>) WITH KEY kunnr =  <fs_praveen>-DISTRB.

      <fs_praveen>-DISTNAME = <fs_kumar>-NAME1.

      ENDLOOP.

*display the data in the report
CALL FUNCTION 'Z_POPUP_ALV'
  TABLES
    IT_ALV                    = IT_PRAVEEN.
