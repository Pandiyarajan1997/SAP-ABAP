*&---------------------------------------------------------------------*
*& Report ZFI_VEND_ADVANCE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_vend_advance.
TABLES: bsik,bsid,lfa1,kna1.",sscrfields.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(29) TEXT-005 FOR FIELD p_bukrs.
    PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(29) TEXT-004 FOR FIELD p_budat.
    PARAMETERS: p_budat TYPE bsik-budat OBLIGATORY.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN: BEGIN OF LINE.
    PARAMETERS : r1 TYPE c RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND r1.
    SELECTION-SCREEN COMMENT 2(25) TEXT-002 FOR FIELD r1.
    SELECT-OPTIONS: s_vendor FOR bsik-lifnr NO INTERVALS.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:  r2 TYPE c RADIOBUTTON GROUP a.
    SELECTION-SCREEN COMMENT 2(25) TEXT-003 FOR FIELD r2.

    SELECT-OPTIONS: s_cust FOR bsid-kunnr NO INTERVALS.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK a1.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE TEXT-006.
  PARAMETERS p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK a2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_layouts USING cl_salv_layout=>restrict_none CHANGING p_layout.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name EQ '%_S_CUST_%_APP_%-TEXT'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name EQ '%_S_VENDOR_%_APP_%-TEXT'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name EQ 'S_VENDOR-LOW'.
      IF r1 = abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name EQ 'S_CUST-LOW'.
      IF r2 = abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  TYPES: BEGIN OF ty_output,
           lifnr TYPE lifnr, " BSIK Vendor code
           name1 TYPE name1, " LFA 1vendor name
           ktokk TYPE ktokk, " LFA 1 vendor type
           belnr TYPE belnr, " BSIK
           blart TYPE blart, " document type
           bldat TYPE bldat, " docu date
           budat TYPE budat, " posting date
           dmbtr TYPE dmbtr, " amount
         END OF ty_output.


START-OF-SELECTION.
  IF r1 = abap_true.
    SELECT a~bukrs,
           a~lifnr,
           b~name1,
           b~ktokk,
           c~txt30,
           a~gjahr,
           a~belnr,
           a~blart,
           a~bldat,
           a~budat,
           a~sgtxt,
           a~shkzg,
           a~dmbtr
      INTO TABLE @DATA(lt_vend)
           FROM bsik AS a INNER JOIN lfa1 AS b ON b~lifnr = a~lifnr
                          INNER JOIN t077y AS c ON c~spras = @sy-langu AND
                                                   c~ktokk = b~ktokk
           WHERE a~bukrs = @p_bukrs AND
                 a~lifnr IN @s_vendor AND
                 a~budat LE @p_budat AND
    a~blart IN ('KZ','KG','RE').
    DELETE lt_vend WHERE blart = 'RE' AND shkzg NE 'S'.
    LOOP AT lt_vend ASSIGNING FIELD-SYMBOL(<fs1>).
      IF <fs1>-shkzg = 'H'.
        <fs1>-dmbtr = <fs1>-dmbtr * -1.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'Z_POPUP_ALV'
      EXPORTING
        i_repid            = sy-repid
        i_title            = 'Vendor Advance Payment Details'
        i_hyperlink_column = 'BELNR'
        i_hyperlink_data   = 'FB03'
        i_hide_column      = 'SHKZG'
        i_layout           = p_layout
      TABLES
        it_alv             = lt_vend.

  ELSE.
    SELECT a~bukrs,
           a~kunnr,
           b~name1,
           b~ktokd,
           c~txt30,
           a~gjahr,
           a~belnr,
           a~blart,
           a~bldat,
           a~budat,
           a~sgtxt,
           a~shkzg,
           a~dmbtr
      INTO TABLE @DATA(lt_cust)
           FROM bsid AS a INNER JOIN kna1 AS b ON b~kunnr = a~kunnr
                          INNER JOIN t077x AS c ON c~spras = @sy-langu AND
                                                   c~ktokd = b~ktokd
           WHERE a~bukrs = @p_bukrs AND
                 a~kunnr IN @s_cust[] AND
                 a~budat LE @p_budat AND
    a~blart IN ('DZ','DG','AB','RV').
    DELETE lt_cust WHERE blart = 'RV' AND shkzg NE 'H'.
    LOOP AT lt_cust ASSIGNING FIELD-SYMBOL(<fs>).
      IF <fs>-shkzg = 'H'.
        <fs>-dmbtr = <fs>-dmbtr * -1.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'Z_POPUP_ALV'
      EXPORTING
        i_repid            = sy-repid
        i_title            = 'Customer Advance Payment Details'
        i_hyperlink_column = 'BELNR'
        i_hyperlink_data   = 'FB03'
        i_hide_column      = 'SHKZG'
      TABLES
        it_alv             = lt_cust.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F4_LAYOUTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IF_SALV_C_LAYOUT=>RESTRICT_NON  text
*      <--P_P_LAYOUT  text
*----------------------------------------------------------------------*
FORM f4_layouts USING i_restrict TYPE salv_de_layout_restriction
CHANGING c_layout TYPE disvariant-variant.

  DATA: ls_layout TYPE salv_s_layout_info,
        ls_key    TYPE salv_s_layout_key.

  ls_key-report = sy-repid.

  ls_layout = cl_salv_layout_service=>f4_layouts(
  s_key    = ls_key
  restrict = i_restrict ).

  c_layout = ls_layout-layout.

ENDFORM.                    " F4_LAYOUTS
