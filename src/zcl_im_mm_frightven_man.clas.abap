class ZCL_IM_MM_FRIGHTVEN_MAN definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_PROCESS_PO_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_FRIGHTVEN_MAN IMPLEMENTATION.


method IF_EX_ME_PROCESS_PO_CUST~CHECK.

**&----------------------- Begin of addition by Velraj (CLSS) on 20160927 -------------------------
  "commented by velraj (CLSS) on 09-01-2017 as they dont want the invoice number to be validated
*  PERFORM CHECK_PO in PROGRAM ZMM_S_PO_ENHANCEMENT USING IM_HEADER IM_HOLD IM_PARK CHANGING CH_FAILED.
**&----------------------- End of addition by Velraj (CLSS) on 20160927 ---------------------------


*  Condition allowded for - ZFR2, ZMLO, ZMUL.
IF SY-TCODE = 'ME21N' OR SY-TCODE = 'ME22N'.


  DATA: LT_CONDITIONS TYPE MMPUR_TKOMV.
  DATA: LS_CONDITIONS LIKE LINE OF LT_CONDITIONS.




  IM_HEADER->GET_CONDITIONS( IMPORTING EX_CONDITIONS = LT_CONDITIONS ).


  LOOP AT  LT_CONDITIONS INTO LS_CONDITIONS.


    IF LS_CONDITIONS-KSCHL EQ 'ZFR2'.

      IF LS_CONDITIONS-LIFNR IS INITIAL.

        MESSAGE 'Please Enter the Freight Vendor in Condition Detail in ZFR2' TYPE 'E'.

      ENDIF.

      IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = 'Check Freight Vendor'
            TXT1  = 'Please Check the Frieght Vendor Before Proceeding'
            TXT2  = '(Applicable to ZFR1 Condition Type)'
*           TXT3  = ' '
*           TXT4  = ' '
          .


      ENDIF.

    ENDIF.


    IF LS_CONDITIONS-KSCHL EQ 'ZMLO'.

      IF LS_CONDITIONS-LIFNR IS INITIAL.

        MESSAGE 'Please Enter the Material Loading Vendor in Condition Detail in ZMLO' TYPE 'E'.

      ENDIF.

      IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = 'Check Freight Vendor'
            TXT1  = 'Please Check the Frieght Vendor Before Proceeding'
            TXT2  = 'Applicable to ZFC1 Condition Type'
*           TXT3  = ' '
*           TXT4  = ' '
          .


      ENDIF.

    ENDIF.

    IF LS_CONDITIONS-KSCHL EQ 'ZMUL'.

      IF LS_CONDITIONS-LIFNR IS INITIAL.

        MESSAGE 'Please Enter the Material Un-loading Vendor in Condition Detail in ZMUL' TYPE 'E'.

      ENDIF.

      IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = 'Check Freight Vendor'
            TXT1  = 'Please Check the Frieght Vendor Before Proceeding'
            TXT2  = 'Applicable to ZFC1 Condition Type'
*           TXT3  = ' '
*           TXT4  = ' '
          .


      ENDIF.

    ENDIF.


    CLEAR: LS_CONDITIONS.

  ENDLOOP.
  ENDIF.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~CLOSE.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER_REFKEYS.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM_REFKEYS.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~INITIALIZE.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~OPEN.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~POST.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ACCOUNT.
endmethod.


METHOD if_ex_me_process_po_cust~process_header.
**&----------------------- Begin of addition by Velraj (CLSS) on 20161007 -------------------------
  "commented by velraj (CLSS) on 09-01-2017 as they dont want the invoice number to be validated
*  PERFORM process_header IN PROGRAM zmm_s_po_enhancement USING im_header.
**&----------------------- End of addition by Velraj (CLSS) on 20161007 -------------------------
ENDMETHOD.


method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ITEM.

*  * BADI implementation for poping up the messages for condition type ZFR1, ZFC1 , FRC1, FRB1,ZMTR, ZMHC,ZFR2,ZMLO,ZMUL.

  DATA: LT_CONDITIONS TYPE MMPUR_TKOMV.
  DATA: LS_CONDITIONS LIKE LINE OF LT_cONDITIONS.

  IM_ITEM->GET_CONDITIONS( IMPORTING EX_CONDITIONS = LT_CONDITIONS ).

  LOOP AT  LT_CONDITIONS INTO LS_CONDITIONS.

    IF LS_CONDITIONS-KSCHL EQ 'ZFR1'.

      IF LS_CONDITIONS-LIFNR IS INITIAL.

        MESSAGE 'Please Enter the Freight Vendor in Condition Detail in ZFR1' TYPE 'E'.

      ENDIF.

      IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL         = 'Check Freight Vendor'
            TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
            TXT2          =  '(Applicable to ZFR1 Condition Type)'
*           TXT3          = ' '
*           TXT4          = ' '
                  .


      ENDIF.

    ENDIF.

    IF LS_CONDITIONS-KSCHL EQ 'ZFR2'.

      IF LS_CONDITIONS-LIFNR IS INITIAL.

        MESSAGE 'Please Enter the Freight Vendor in Condition Detail in ZFR2' TYPE 'E'.

      ENDIF.

      IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL         = 'Check Freight Vendor'
            TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
            TXT2          =  '(Applicable to ZFR1 Condition Type)'
*           TXT3          = ' '
*           TXT4          = ' '
                  .


      ENDIF.

    ENDIF.


    IF LS_CONDITIONS-KSCHL EQ 'FRB1'.

      IF LS_CONDITIONS-LIFNR IS INITIAL.

        MESSAGE 'Please Enter the Freight Vendor in Condition Detail in FRB1' TYPE 'E'.

       ENDIF.


      IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL         = 'Check Freight Vendor'
            TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
            TXT2          =  '(Applicable to FRB1 Condition Type)'
*           TXT3          = ' '
*           TXT4          = ' '
                  .


      ENDIF.


     ENDIF.

     IF LS_CONDITIONS-KSCHL EQ 'ZFC1'.

       IF LS_CONDITIONS-LIFNR IS INITIAL.

         MESSAGE 'Please Enter the Freight Vendor in Condition Detail in ZFC1' TYPE 'E'.

       ENDIF.

       IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

             CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                 TITEL         = 'Check Freight Vendor'
                 TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
                 TXT2          = 'Applicable to ZFC1 Condition Type'
*                TXT3          = ' '
*                TXT4          = ' '
                       .


       ENDIF.

       ENDIF.

       IF LS_CONDITIONS-KSCHL EQ 'FRC1'.

       IF LS_CONDITIONS-LIFNR IS INITIAL.

         MESSAGE 'Please Enter the Freight Vendor in Condition Detail in FRC1' TYPE 'E'.

       ENDIF.

       IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

             CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                 TITEL         = 'Check Freight Vendor'
                 TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
                 TXT2          = 'Applicable to ZFC1 Condition Type'
*                TXT3          = ' '
*                TXT4          = ' '
                       .


       ENDIF.

       ENDIF.

       IF LS_CONDITIONS-KSCHL EQ 'ZMTR'.

       IF LS_CONDITIONS-LIFNR IS INITIAL.

         MESSAGE 'Please Enter the Material Handling Vendor in Condition Detail in ZMTR' TYPE 'E'.

       ENDIF.

       IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

             CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                 TITEL         = 'Check Freight Vendor'
                 TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
                 TXT2          = 'Applicable to ZFC1 Condition Type'
*                TXT3          = ' '
*                TXT4          = ' '
                       .


       ENDIF.

       ENDIF.
       IF LS_CONDITIONS-KSCHL EQ 'ZMHC'.

       IF LS_CONDITIONS-LIFNR IS INITIAL.

         MESSAGE 'Please Enter the Material Handling Vendor in Condition Detail in ZMHC' TYPE 'E'.

       ENDIF.

       IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

             CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                 TITEL         = 'Check Freight Vendor'
                 TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
                 TXT2          = 'Applicable to ZFC1 Condition Type'
*                TXT3          = ' '
*                TXT4          = ' '
                       .


       ENDIF.

       ENDIF.

       IF LS_CONDITIONS-KSCHL EQ 'ZMLO'.

       IF LS_CONDITIONS-LIFNR IS INITIAL.

         MESSAGE 'Please Enter the Material Loading Vendor in Condition Detail in ZMLO' TYPE 'E'.

       ENDIF.

       IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

             CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                 TITEL         = 'Check Freight Vendor'
                 TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
                 TXT2          = 'Applicable to ZFC1 Condition Type'
*                TXT3          = ' '
*                TXT4          = ' '
                       .


       ENDIF.

       ENDIF.

       IF LS_CONDITIONS-KSCHL EQ 'ZMUL'.

       IF LS_CONDITIONS-LIFNR IS INITIAL.

         MESSAGE 'Please Enter the Material Un-loading Vendor in Condition Detail in ZMUL' TYPE 'E'.

       ENDIF.

       IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

             CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                 TITEL         = 'Check Freight Vendor'
                 TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
                 TXT2          = 'Applicable to ZFC1 Condition Type'
*                TXT3          = ' '
*                TXT4          = ' '
                       .


       ENDIF.

       ENDIF.

       IF LS_CONDITIONS-KSCHL EQ 'JFR1'.

       IF LS_CONDITIONS-LIFNR IS INITIAL.

         MESSAGE 'Please Enter the Fright Vendor in Condition Detail in JFR1' TYPE 'E'.

       ENDIF.

       IF LS_CONDITIONS-KSCHL IS NOT INITIAL.

             CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                 TITEL         = 'Check Freight Vendor'
                 TXT1          = 'Please Check the Frieght Vendor Before Proceeding'
                 TXT2          = 'Applicable to ZFC1 Condition Type'
*                TXT3          = ' '
*                TXT4          = ' '
                       .


       ENDIF.

       ENDIF.

CLEAR: LS_CONDITIONS.

  ENDLOOP.

endmethod.


method IF_EX_ME_PROCESS_PO_CUST~PROCESS_SCHEDULE.
endmethod.
ENDCLASS.
