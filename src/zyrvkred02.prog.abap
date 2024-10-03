REPORT zyrvkred02 .
TABLES: vbkred,zdrauth.

TYPES : BEGIN OF ty_uname,
        uname TYPE zdrauth-uname,
        END OF ty_uname,
        BEGIN OF ty_region,
         regio TYPE zdrauth-regio,
        END OF ty_region.
DATA: params LIKE  pri_params.
DATA: variant_in  LIKE disvariant.
DATA: variant_out LIKE disvariant.
DATA:  k_save(1) TYPE c VALUE 'A'.

DATA status1 TYPE cstat.
DATA: flag TYPE c,
      flag1 TYPE c,
      flag4 TYPE c,
      uname1 TYPE zdrauth-uname,
      regio1 TYPE zdrauth-regio,
      flag2  TYPE c,
      flag3  TYPE c,
      gt_uname TYPE TABLE OF ty_uname,
      gt_regio TYPE TABLE OF ty_region,
       gt_uname1 TYPE TABLE OF ty_uname,
       gt_regio1 TYPE TABLE OF ty_region.
RANGES: uname2    FOR zdrauth-uname,
        regio2    FOR zdrauth-regio,
        vbeln2    FOR vbkred-vbeln.


RANGES: gbstk FOR vbuk-gbstk.
MOVE 'I'    TO gbstk-sign.
MOVE 'EQ'   TO gbstk-option.
MOVE 'A'    TO gbstk-low.
APPEND gbstk.
MOVE 'I'    TO gbstk-sign.
MOVE 'EQ'   TO gbstk-option.
MOVE 'B'    TO gbstk-low.
APPEND gbstk.

*Declarations for dropdown
DATA: it_list     TYPE vrm_values.
DATA: wa_list    TYPE vrm_value.
DATA: it_values   TYPE TABLE OF dynpread,
      wa_values  TYPE dynpread,
      days1      TYPE zdrauth-zwidy.

SELECTION-SCREEN BEGIN OF BLOCK credit WITH FRAME TITLE text-f01.
SELECT-OPTIONS: kkber    FOR vbkred-kkber MEMORY ID kkb,
                sbgrp    FOR vbkred-sbgrp MEMORY ID kbg.
PARAMETERS:     status   LIKE vbkred-cstat MEMORY ID abc  AS LISTBOX VISIBLE LENGTH 20."added to filter output based on status
SELECT-OPTIONS: vbeln    FOR vbkred-vbeln MEMORY ID bua.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: cmngv    FOR vbkred-cmngv,
                knkli    FOR vbkred-knkli MEMORY ID kun
                         MATCHCODE OBJECT debi,
                ctlpc    FOR vbkred-ctlpc,
                grupp    FOR vbkred-grupp.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK credit.
* Variante
SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME TITLE text-s02.
PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK 0.

* FOr Dynamic List box
INITIALIZATION.
  wa_list-key = 'C'.
  wa_list-text = 'Oldest item'.
  APPEND wa_list TO it_list.
  wa_list-key = 'D'.
  wa_list-text = 'Dynamic check'.
  APPEND wa_list TO it_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'STATUS'
      values          = it_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

AT SELECTION-SCREEN OUTPUT.
  SET CURSOR FIELD 'CMNGV-LOW'.

INITIALIZATION.
  SET CURSOR FIELD 'CMNGV-LOW'.

* Process on value request
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  MOVE 'RVKRED01' TO variant_in-report.
  MOVE 'VKM1' TO variant_in-log_group.
  PERFORM f4_for_variant(rvkred01) USING    variant_in
                                            k_save
                                   CHANGING variant_out.
  MOVE variant_out-variant TO p_vari.

END-OF-SELECTION.

  LOOP AT vbeln .
    vbeln2-sign = vbeln-sign.
    vbeln2-option = vbeln-option.
    vbeln2-low  = vbeln-low.
    vbeln2-high = vbeln-high.
    APPEND vbeln2.
  ENDLOOP.
  IF sy-batch IS INITIAL.
    CLEAR:status1,flag.
    IF status IS NOT INITIAL .
      IF status = 'C'.
        status = 'Oldest item'.
      ELSEIF status = 'D'.
        status = 'Dynamic check'.
      ENDIF.
      status1 = status.
      flag = 'X'.
      EXPORT status1 TO MEMORY ID 'ABC'.
      EXPORT flag TO MEMORY ID 'DEF'.
    ENDIF.
    IF vbeln IS NOT INITIAL.
      flag3 = 'X'.
      EXPORT vbeln2[] TO MEMORY ID 'ZYX1'.
      "SET PARAMETER ID 'MEM'  FIELD vbeln2.
      EXPORT flag3 TO MEMORY ID 'STO1'.
    ENDIF.

    SUBMIT rvkred01 WITH kkber IN kkber
                    WITH sbgrp IN sbgrp
                    WITH status EQ status
                    WITH VBELN IN VBELN
                    WITH cmngv IN cmngv
                    WITH knkli IN knkli
                    WITH ctlpc IN ctlpc
                    WITH grupp IN grupp
                    WITH cmgst EQ 'B'
                    WITH gbstk IN gbstk
                    WITH order EQ 'X'
                    WITH deliv EQ 'X'
                    WITH callmode EQ 'X'
                    WITH p_vari   EQ p_vari
                    WITH code EQ 'VKM1'
                    AND RETURN.
  ELSE.
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        no_dialog      = 'X'
        mode           = 'CURRENT'
      IMPORTING
        out_parameters = params.

    SUBMIT rvkred01 TO SAP-SPOOL SPOOL PARAMETERS params
                    WITHOUT SPOOL DYNPRO
                    WITH kkber IN kkber
                    WITH sbgrp IN sbgrp
                    WITH cmngv IN cmngv
                    WITH knkli IN knkli
                    WITH ctlpc IN ctlpc
                    WITH grupp IN grupp
                    WITH cmgst EQ 'B'
                    WITH gbstk IN gbstk
                    WITH p_vari   EQ p_vari
                    WITH code EQ 'VKM1'
                    WITH order EQ 'X'
                    WITH deliv EQ 'X'
                    WITH callmode EQ 'X'
                    AND RETURN.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  PERNR_F4HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pernr_f4help .
  SELECT uname FROM zdrauth INTO TABLE gt_uname.
SORT GT_UNAME BY UNAME. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM gt_uname COMPARING uname.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'UNAME'
      dynpprog        = sy-repid    " Program name
      dynpnr          = sy-dynnr    " Screen number
      dynprofield     = 'UNAME'   " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = gt_uname " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.                    " PERNR_F4HELP
*&---------------------------------------------------------------------*
*&      Form  PERNR_F4HELP1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pernr_f4help1 .
  SELECT regio FROM zdrauth INTO TABLE gt_regio.
SORT GT_REGIO BY REGIO. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM gt_regio COMPARING regio.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'REGIO'
      dynpprog        = sy-repid    " Program name
      dynpnr          = sy-dynnr    " Screen number
      dynprofield     = 'REGIO'   " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = gt_regio " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.                    " PERNR_F4HELP1
