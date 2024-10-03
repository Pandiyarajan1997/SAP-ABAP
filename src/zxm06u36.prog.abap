*&---------------------------------------------------------------------*
*&  Include           ZXM06U36
*&---------------------------------------------------------------------*

  " Created By Govind On 12-01-2015 Requirement Given By Umapathy Regarding Sales Po Rect.



  IF sy-tcode EQ 'ME21N' OR sy-tcode EQ 'ME22N'.
    IF i_ekko-bsart EQ 'YUB'.
      IF i_ekko-verkf EQ ' '.
        MESSAGE 'Enter the customer name in communication tab' TYPE 'E' .
      ENDIF.
    ENDIF.
  ENDIF.
*---------------------------------------------------------------------------------------------------
*  ------------------------------------------------------------------------------------------------------------



  IF sy-tcode EQ 'ME21N' OR sy-tcode EQ 'ME22N'.
    IF i_ekko-bukrs EQ '1000'.
      DATA: lv_mat TYPE mara-matnr.

*    TYPES : BEGIN OF GS_EKPO,
*            EBELN TYPE EKPO-EBELN,
*      END OF GS_EKPO.

      DATA : gt_ekpo TYPE TABLE OF bekpo.
      DATA : wa_ekpo TYPE bekpo .

      " LOOP AT GT_EKPO INTO WA_EKPO .

      IF tekpo-pstyp = ' '   .

        " SELECT * FROM EKPO INTO TABLE GT_EKPO WHERE EBELN = I_EKKO-EBELN .
        " ENDLOOP.
        SELECT SINGLE material FROM zmm_mat_block INTO lv_mat WHERE material = tekpo-matnr.
        IF lv_mat IS NOT INITIAL.

          "   IF BEKPO-PSTYP EQ '0' OR BEKPO-PSTYP EQ ''.
          MESSAGE e001(zmb).
          LEAVE TO CURRENT TRANSACTION.

          "  ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------------------------------------
*Requirment : Company code data mandatory for Po header screen

  IF sy-tcode EQ 'ME21N' OR sy-tcode EQ 'ME22N'.
    IF i_ekko-bsart EQ 'NB' OR i_ekko-bsart EQ 'ZNB' OR i_ekko-bsart EQ 'ZIM' OR i_ekko-bsart EQ 'ZSC'
      OR i_ekko-bsart EQ'ZVR' OR i_ekko-bsart EQ 'ZSR' OR i_ekko-bsart EQ 'ZCA' OR i_ekko-bsart EQ 'ZDE'.


      DATA : gt_lfb1 TYPE TABLE OF lfb1,
             wa_lfb1 TYPE lfb1.
      DATA : msg1(400).


      SELECT * FROM lfb1 INTO TABLE gt_lfb1 WHERE bukrs = i_ekko-bukrs AND lifnr = i_lfa1-lifnr .

      IF gt_lfb1[] IS INITIAL .

        CONCATENATE  'PO Header 'i_ekko-bukrs 'Company Code data has not been created 'INTO msg1 SEPARATED BY space.
        MESSAGE msg1 TYPE 'E'.

      ENDIF.
    ENDIF.

  ENDIF.
****Samsudeen Added on 29.08.2022
  IF sy-tcode EQ 'ME23N'.
    LOOP AT SCREEN.
      IF screen-group1 = 'B1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.














*DATA : I_EKKO-EBELN TYPE G_EBELN.
*
*IF I_EKKO-EBELN NE G_EBELN.  "PBO 101 screen
*  CLEAR G_IND.   " Here G_IND is global variable declare it in top include
*ENDIF.
*MOVE I_EKKO-EBELN TO G_EBELN.
*G_IND = G_IND + 1.
*
*IF G_IND EQ 1.
*  MOVE : I_EKKO-EBELN TO EKKO-EBELN,
*               I_EKKO-BUKRS TO EKKO-BUKRS,
*               I_EKKO-LIFNR TO EKKO-LIFNR,
*               I_EKKO-EKORG TO EKKO-EKORG,
*               I_EKKO-EKGRP TO EKKO-EKGRP,
*               I_EKKO-KUNNR TO EKKO-KUNNR,
*ENDIF.

*
*
*DATA : LV_ERDAT TYPE EKKO-AEDAT.
*DATA : LV_AEDAT TYPE SY-DATUM.
*
*LV_AEDAT = SY-DATUM+06.
*LV_ERDAT = I_EKKO-BEDAT+06.
*
*
*IF I_EKKO-ERNAM = '717299' OR I_EKKO-ERNAM = '715608'
*   OR  I_EKKO-ERNAM = '912396' OR I_EKKO-ERNAM = '324033'
*   OR I_EKKO-ERNAM = '254040' OR I_EKKO-ERNAM = '534008'
*  OR I_EKKO-ERNAM = '451473' OR I_EKKO-ERNAM = '464016'
*  OR I_EKKO-ERNAM = '324032' OR I_EKKO-ERNAM = '814002'
*  OR I_EKKO-ERNAM = '394024' OR I_EKKO-ERNAM = '353611'
*   OR I_EKKO-ERNAM = '322032'  .
*
*IF SY-TCODE = 'ME21N'.
*    IF I_EKKO-BSART = 'UB'.
*      IF I_EKKO-RESWK = '1100'.
*      IF LV_AEDAT BETWEEN '01' AND  '10' AND  LV_ERDAT BETWEEN  '01' AND  '10' .
*      ELSE.
*        MESSAGE 'UB Document Type Purchse Order Not allowed after 10th in Every Month' TYPE 'I' DISPLAY LIKE 'E'.
*        LEAVE TO TRANSACTION 'ME21N'.
*      ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*ENDIF.
