*&---------------------------------------------------------------------*
*& Report  ZINVOICE_ADJ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZINVOICE_ADJ.

DATA : lv_cus TYPE knb1-kunnr.
DATA : lv_com TYPE knb1-BUKRS.

DATA: GS_BAPIRETURN TYPE BAPIRETURN,
      GT_LINEITEMS  TYPE STANDARD TABLE OF BAPI3007_2.

        TYPES : BEGIN OF GS_KNB1,
                  KUNNR TYPE KNB1-KUNNR,
                  BUKRS TYPE KNB1-BUKRS,
                  ERDAT TYPE KNB1-ERDAT,
                  ERNAM TYPE KNB1-ERNAM,
                  SPERR TYPE KNB1-SPERR,
                  LOEVM TYPE KNB1-LOEVM,
                END OF GS_KNB1.

DATA :    IT_OPEN  TYPE STANDARD TABLE OF BAPI3007_2,
          WA_OPEN  LIKE LINE OF IT_OPEN.

   DATA : IT_KNB1 TYPE TABLE OF GS_KNB1,
               WA_KNB1 TYPE GS_KNB1.


"PS_BAPIRETURN LIKE GS_BAPIRETURN
"PT_LINEITEMS LIKE GT_LINEITEMS.

lv_cus = 100000 .
lv_com = 1000.


SELECT         KUNNR
                 BUKRS
                 ERDAT
                 ERNAM
                 SPERR
                 LOEVM
  FROM KNB1 INTO TABLE IT_KNB1 WHERE   BUKRS = '1700'." OR BUKRS = '2000' OR BUKRS = '4000' )  .

  DELETE it_knb1 WHERE kunnr ne '0010000430' .

  loop at it_knb1 INTO wa_knb1.

CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'"#EC CI_USAGE_OK[2628704]"Added by SPLABAP during code remediation
  EXPORTING
    COMPANYCODE       =  wa_knb1-bukrs "'1700' "wa_knb1-bukrs
    CUSTOMER          =  wa_knb1-kunnr "'10000430' "
    KEYDATE           = SY-DATUM
*   NOTEDITEMS        = ' '
*   SECINDEX          = ' '
 IMPORTING
   RETURN            =  GS_BAPIRETURN
  TABLES
    LINEITEMS         = GT_LINEITEMS .
          .
SORT : IT_OPEN BY CUSTOMER.
SORT : GT_LINEITEMS BY FISC_YEAR DOC_NO .

ENDLOOP.
