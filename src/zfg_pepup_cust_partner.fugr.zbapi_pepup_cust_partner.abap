FUNCTION ZBAPI_PEPUP_CUST_PARTNER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_VKORG) TYPE  VKORG
*"     VALUE(P_VTWEG) TYPE  VTWEG
*"     VALUE(P_SPART) TYPE  SPART
*"     VALUE(P_PARVW) TYPE  PARVW
*"  TABLES
*"      IT_PARTNER STRUCTURE  ZSTR_PEPUP_PARTNER
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------
*&    AUTHOR          : RAMACHANDRAN
*&    CREATED ON      : 24.06.2020
*&    COMPANY         : SPHINAX INFO SYSTEMS
*&    OBJECTIVE       : THIS FUNCTION MODULE CAN BE USED TO GET SALES DATA(KNVP) FOR A GIVEN COMPANY CODE
*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------

TYPES : BEGIN OF TY_KNVP,
        KUNNR TYPE KNVP-KUNNR,
        VKORG TYPE KNVP-VKORG,
        VTWEG TYPE KNVP-VTWEG,
        SPART TYPE KNVP-SPART,
        PARVW TYPE KNVP-PARVW,
        PARZA TYPE KNVP-PARZA,
        KUNN2 TYPE KNVP-KUNN2,
        LIFNR TYPE KNVP-LIFNR,
        PERNR TYPE KNVP-PERNR,
        PARNR TYPE KNVP-PARNR,
        KNREF TYPE KNVP-KNREF,
        DEFPA TYPE KNVP-DEFPA,
      END OF TY_KNVP.

  DATA: IT_KNVP TYPE STANDARD TABLE OF TY_KNVP,
        WA_KNVP TYPE TY_KNVP.

  DATA : P1_VKORG TYPE KNVP-VKORG.


      DATA: WA_PARTNER TYPE ZSTR_PEPUP_PARTNER.
*&------------------------------------------ Data Retrieval - Begin -------------------------------------------------
   SELECT
        KUNNR
        VKORG
        VTWEG
        SPART
        PARVW
        PARZA
        KUNN2
        LIFNR
        PERNR
        PARNR
        KNREF
        DEFPA
              INTO TABLE IT_KNVP
          FROM KNVP WHERE VKORG EQ P_VKORG
                             AND VTWEG EQ P_VTWEG
                          AND SPART EQ P_SPART
                          AND PARVW EQ P_PARVW .

           .
*&------------------------------------------ Data Retrieval - END -------------------------------------------------

*&------------------------------------------ Data logic - Begin -------------------------------------------------
     IF NOT IT_KNVP IS INITIAL.

       SORT : IT_KNVP  BY KUNNR VKORG VTWEG SPART.

        LOOP AT IT_KNVP INTO WA_KNVP.
      MOVE-CORRESPONDING WA_KNVP TO  WA_PARTNER.
            APPEND WA_PARTNER TO IT_PARTNER.
      CLEAR: WA_PARTNER, WA_KNVP.

    ENDLOOP.

     ENDIF.

*&------------------------------------------ Data logic - End -------------------------------------------------

*&--------------------------------------------- BAPI Return Messages--------------------------------------------------------------------
  IF IT_PARTNER[] IS NOT INITIAL.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'S'   'SUCCESS!!! - Customer Mapping Table Data Taken !!!'.
  ELSE.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'E'   'Error!!! - Customer Mapping Table Data Not Taken !!!'.
  ENDIF.

ENDFUNCTION.
