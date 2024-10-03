*&---------------------------------------------------------------------*
*& Report ZSD_VBRK_SALES_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSD_VBRK_SALES_DATA.

tables vbrk.

* structure of the table that need to be displayed inside the tcode after selection

TYPES : BEGIN OF st_vbrk,
        VBEln type vbeln,
        fkart type fkart,
        Billtext type char20,
        Vkorg type vkorg,
        fkdat type fkdat,
        netwr type netwr,
        mwsbk type mwsbp,
        total type netwr,
  END OF ST_VBRK.

DATA : it_vbrk type table of ST_VBRK.



*Selection screen design

SELECT-OPTIONS : BillDaTe for vbrk-FKDAT, " this is used for removing to select option NO INTERVALS no-EXTENSION.
                 BillDOC for VBRK-vbeln,
                 Billtype for VBRK-FKART,
                 CUSTOMER FOR VBRK-KUNAG.

*Select querry from VBRK

Select vbeln
 fkart
  vkorg
  fkdat
   netwr
  MWSBK from vbrk into CORRESPONDING FIELDS OF TABLE it_vbrk WHERE fkdat in BillDaTe and vbeln in billdoc and fkart in billtype.


*Customized fields that need to be added
                   .
loop at it_vbrk ASSIGNING FIELD-SYMBOL(<fs_vbrk>).

* BELOW DETAILS IS FOR DESCRIBING BILLING TYPES
  IF <fs_vbrk>-fkart = 'YBBR'.

    <fs_vbrk>-BILLTEXT = 'Regular Invoice'.

 ELSEIF <fs_vbrk>-fkart = 'YDMS'.

   <fs_vbrk>-BILLTEXT = 'DMS Invoice'.

    ELSEIF <fs_vbrk>-fkart = 'YBRE'.

   <fs_vbrk>-BILLTEXT = 'REG return Invoice'.

    ELSEIF <fs_vbrk>-fkart = 'YRMS'.

   <fs_vbrk>-BILLTEXT = 'DMS RET Invoice'.

    ELSEIF <fs_vbrk>-fkart = 'S1'.

   <fs_vbrk>-BILLTEXT = 'CANCELLATION'.

  ENDIF.

*BELOW DETAILS IS FOR ADDITION OF NET AND TAX VALUE

  <fs_vbrk>-TOTAL = <fs_vbrk>-NETWR + <fs_vbrk>-MWSBK.


  ENDLOOP.

*display the data in the report
CALL FUNCTION 'Z_POPUP_ALV'
  TABLES
    IT_ALV                    = it_vbrk .
