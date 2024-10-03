*----------------------------------------------------------------------*
***INCLUDE RLE_DELNOTE_FORMS .
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*
FORM GET_DATA
     USING    IS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ
     CHANGING CS_ADDR_KEY           LIKE ADDR_KEY
              CS_DLV_DELNOTE        TYPE LEDLV_DELNOTE
              CF_RETCODE.

  DATA: LS_DELIVERY_KEY TYPE  LESHP_DELIVERY_KEY.
  LS_DELIVERY_KEY-VBELN = NAST-OBJKY.

  CALL FUNCTION 'LE_SHP_DLV_OUTP_READ_PRTDATA'
       EXPORTING
            IS_DELIVERY_KEY       = LS_DELIVERY_KEY
            IS_PRINT_DATA_TO_READ = IS_PRINT_DATA_TO_READ
            IF_PARVW              = NAST-PARVW
            IF_PARNR              = NAST-PARNR
            IF_LANGUAGE           = NAST-SPRAS
       IMPORTING
            ES_DLV_DELNOTE        = CS_DLV_DELNOTE
       EXCEPTIONS
            RECORDS_NOT_FOUND     = 1
            RECORDS_NOT_REQUESTED = 2
            OTHERS                = 3.
  IF SY-SUBRC <> 0.
*  error handling
    CF_RETCODE = SY-SUBRC.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

* get nast partner adress for communication strategy
  PERFORM GET_ADDR_KEY USING    CS_DLV_DELNOTE-HD_ADR
                       CHANGING CS_ADDR_KEY.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM SET_PRINT_DATA_TO_READ                                   *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*
FORM SET_PRINT_DATA_TO_READ
         USING    IF_FORMNAME LIKE TNAPR-SFORM
         CHANGING CS_PRINT_DATA_TO_READ TYPE LEDLV_PRINT_DATA_TO_READ
                  CF_RETCODE.

  FIELD-SYMBOLS: <FS_PRINT_DATA_TO_READ> TYPE XFELD.
  DATA: LT_FIELDLIST TYPE TSFFIELDS.
  DATA: LS_FIELDLIST TYPE LINE OF TSFFIELDS.
  DATA: LF_FIELD1 TYPE LINE OF TSFFIELDS.
  DATA: LF_FIELD2 TYPE LINE OF TSFFIELDS.
  DATA: LF_FIELD3 TYPE LINE OF TSFFIELDS.

  CALL FUNCTION 'SSF_FIELD_LIST'
    EXPORTING
      FORMNAME                = IF_FORMNAME
*     VARIANT                 = ' '
    IMPORTING
      FIELDLIST               = LT_FIELDLIST
   EXCEPTIONS
     NO_FORM                  = 1
     NO_FUNCTION_MODULE       = 2
     OTHERS                   = 3.
  IF SY-SUBRC <> 0.
*  error handling
    CF_RETCODE = SY-SUBRC.
    PERFORM PROTOCOL_UPDATE.
    CLEAR LT_FIELDLIST.
  ELSE.
* set print data requirements
    LOOP AT LT_FIELDLIST INTO LS_FIELDLIST.
      SPLIT LS_FIELDLIST AT '-' INTO LF_FIELD1 LF_FIELD2 LF_FIELD3.
* <<<< START_OF_INSERTION_HP_350342 >>>>
      IF LF_FIELD1 = 'IS_DLV_DELNOTE' AND LF_FIELD2 = 'IT_SERNR'.
         LF_FIELD2 = 'IT_SERNO'.
      ENDIF.
* <<<< END_OF_INSERTION_HP_350342 >>>>
      ASSIGN COMPONENT LF_FIELD2 OF STRUCTURE
                       CS_PRINT_DATA_TO_READ TO <FS_PRINT_DATA_TO_READ>.
      IF SY-SUBRC = 0.
        <FS_PRINT_DATA_TO_READ> = 'X'.
      ENDIF.
    ENDLOOP.

* header data is always required
    CS_PRINT_DATA_TO_READ-HD_GEN = 'X'.
* adress is always required for print param
    CS_PRINT_DATA_TO_READ-HD_ADR = 'X'.
* organisational data is always required for include texts
    CS_PRINT_DATA_TO_READ-HD_ORG = 'X'.
*organisational data address is always required       "n_520906
    CS_PRINT_DATA_TO_READ-HD_ORG_ADR  = 'X'.          "n_520906

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_addr_key
*&---------------------------------------------------------------------*
FORM GET_ADDR_KEY USING    IT_HD_ADR   TYPE LEDLV_DELNOTE-HD_ADR
                  CHANGING CS_ADDR_KEY LIKE ADDR_KEY.

  FIELD-SYMBOLS <FS_HD_ADR> TYPE LINE OF LEDLV_DELNOTE-HD_ADR.

  READ TABLE IT_HD_ADR ASSIGNING <FS_HD_ADR>
                       WITH KEY DELIV_NUMB = NAST-OBJKY
                                PARTN_ROLE = NAST-PARVW.
  IF SY-SUBRC = 0.
    CS_ADDR_KEY-ADDRNUMBER = <FS_HD_ADR>-ADDR_NO.
    CS_ADDR_KEY-PERSNUMBER = <FS_HD_ADR>-PERSON_NUMB.
    CS_ADDR_KEY-ADDR_TYPE  = <FS_HD_ADR>-ADDRESS_TYPE.
  ENDIF.

ENDFORM.                               " get_addr_key
