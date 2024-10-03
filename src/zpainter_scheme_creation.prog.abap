*&---------------------------------------------------------------------*
*& Module Pool       ZSCHEME_CREATION
*&
*&--------------------------------------------------------------------------*
*&
*&
*&--------------------------------------------------------------------------*

PROGRAM zpainter_scheme_creation.

TABLES : t001 , t001w ,t005u , zpainter_sch_log,zpainter_master."ZSCHEME_MASTER.

CONTROLS : tc TYPE TABLEVIEW USING SCREEN 0002.


TYPES : BEGIN OF ty_log,
          bukrs    TYPE zpainter_sch_log-bukrs,
          zscheme  TYPE zpainter_sch_log-zscheme,
          zsch_des TYPE zpainter_sch_log-zsch_des,
          matkl    TYPE zpainter_sch_log-matkl,
          "EWBEZ    TYPE ZSCHEME_LOG-EWBEZ ,
          zpoints  TYPE zpainter_sch_log-zpoints,
          regio    TYPE zpainter_sch_log-regio,
          name1    TYPE zpainter_sch_log-name1,
          from_per TYPE zpainter_sch_log-from_per,
          to_per   TYPE zpainter_sch_log-to_per,
          erdat    TYPE zpainter_sch_log-erdat,
          ernam    TYPE zpainter_sch_log-ernam,
        END OF ty_log.

TYPES : BEGIN OF ty_delete,
          bukrs    TYPE zpainter_sch_log-bukrs,
          zscheme  TYPE zpainter_sch_log-zscheme,
          regio    TYPE zpainter_sch_log-regio,
          from_per TYPE zpainter_sch_log-from_per,
          to_per   TYPE zpainter_sch_log-to_per,
        END OF ty_delete.

TYPES : BEGIN OF ty_tc,
*        MATGRP   TYPE TWEWT-EXTWG,
*        EWBEZ    TYPE ZSCHEME_LOG-EWBEZ ,
          matkl       TYPE zpainter_sch_log-matkl,
          description TYPE t023t-wgbez,
          zpoints     TYPE zpainter_sch_log-zpoints,
          sel(1)      TYPE c,
        END OF ty_tc.

DATA : it_tc  TYPE TABLE OF ty_tc WITH HEADER LINE,
       it_tc1 TYPE TABLE OF ty_tc WITH HEADER LINE.
DATA : wa_tc TYPE ty_tc.


DATA : it_log TYPE TABLE OF ty_log,
       wa_log TYPE ty_log.

DATA : it_delete TYPE TABLE OF ty_delete.
DATA : wa_delete TYPE ty_delete.
*DATA :  IT_LOGDATA TYPE TABLE OF TY_LOGDATA,
*        WA_LOGDATA TYPE TY_LOGDATA.

DATA : company   TYPE t001-bukrs,
       regio     TYPE t005u-bland,
       schemecod TYPE zpainter_master-zscheme,
       schemedes TYPE zpainter_master-zsch_des,
       fromdate  TYPE zpainter_sch_log-from_per,
       todate    TYPE zpainter_sch_log-to_per.

DATA : lv_company TYPE t001-bukrs,
       lv_regio   TYPE t005u-bland,
       lv_scheme  TYPE zpainter_master-zscheme,
       lv_desc    TYPE zpainter_master-zsch_des,
       lv_from    TYPE zpainter_sch_log-from_per,
       lv_to      TYPE zpainter_sch_log-to_per.

DATA :  "LV_REGIO TYPE T005U-BLAND, "regio
        lv_regioname TYPE t005u-bezei. "regio name

TYPES :BEGIN OF ty_reg,
         regio     TYPE t005u-bland, "regio
         regioname TYPE t005u-bezei, "regio name
       END OF ty_reg.

DATA: it_reg TYPE TABLE OF ty_reg,
      wa_reg TYPE ty_reg.

DATA : lv_bland   TYPE t005u-bland, "regio
       lv_bezei   TYPE t005u-bezei, "regio name
       lv_schname TYPE zpainter_master-zsch_des,
       lv_from1   TYPE zpainter_sch_log-from_per,
       lv_to1     TYPE zpainter_sch_log-to_per.

DATA : it_master TYPE TABLE OF zpainter_master.
DATA : wa_master TYPE zpainter_master.

DATA : wa_save TYPE ty_tc.

DATA : it_save1 TYPE TABLE OF zpainter_sch_log.
DATA : wa_save1 TYPE zpainter_sch_log.

DATA : gv_flag TYPE c.

DATA :fill2 TYPE i .

DATA : sel(1) TYPE c .

DATA : lv_vtext TYPE vtext .

DATA : lv_text TYPE twewt-ewbez .

CONSTANTS: screen_on(1)  TYPE c VALUE '1',
           screen_off(1) TYPE c VALUE '0'.

DEFINE screen_editable.
  LOOP AT SCREEN.
    CHECK screen-name = &1.
    screen-input      = screen_on.
    MODIFY SCREEN.
  ENDLOOP.
END-OF-DEFINITION.

DEFINE screen_display.
  LOOP AT SCREEN.
    CHECK screen-name = &1.
    screen-input      = screen_off.
    MODIFY SCREEN.
  ENDLOOP.
END-OF-DEFINITION.



SELECTION-SCREEN BEGIN OF BLOCK b1.

  SELECT-OPTIONS : s_bukrs  FOR t001-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY.
  SELECT-OPTIONS : s_regio FOR t005u-bland NO INTERVALS NO-EXTENSION OBLIGATORY.
  SELECT-OPTIONS : s_scheme FOR zpainter_master-zsch_des NO INTERVALS NO-EXTENSION OBLIGATORY . "MATCHCODE OBJECT ZSH_SCHEME_MASTER NO INTERVALS NO-EXTENSION OBLIGATORY .
  SELECT-OPTIONS : s_date   FOR zpainter_master-from_per NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK b1.


AT SELECTION-SCREEN.


  IF s_bukrs IS NOT INITIAL
    AND s_regio IS NOT INITIAL
    AND s_scheme IS NOT INITIAL.


    SELECT * FROM zpainter_master
      INTO TABLE it_master
      WHERE bukrs IN s_bukrs
      AND   regio IN s_regio.

    SELECT * UP TO 1 ROWS FROM zpainter_master
      INTO wa_master
      WHERE bukrs IN s_bukrs
      AND   regio IN s_regio
      AND   zscheme IN s_scheme ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation



    IF wa_master IS NOT INITIAL.

      s_date-sign = 'I'.
      s_date-option = 'BT'.
      s_date-low = wa_master-from_per.
      s_date-high = wa_master-to_per.
      APPEND s_date.

    ENDIF.

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_regio-low.
  TYPES : BEGIN OF ty_regio,
            bland TYPE t005u-bland,
            bezei TYPE t005u-bezei,
          END OF ty_regio.
  DATA : it_regio TYPE TABLE OF ty_regio,
         wa_regio TYPE ty_regio.

  SELECT bland bezei FROM t005u INTO TABLE it_regio WHERE spras = sy-langu AND land1 = 'IN' .

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield    = 'BLAND'
*     PVALKEY     = ' '
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'S_REGIO'
*     STEPL       = 0
*     WINDOW_TITLE           =
*     VALUE       = ' '
      value_org   = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY     = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB    =
*   IMPORTING
*     USER_RESET  =
    TABLES
      value_tab   = it_regio
*     FIELD_TAB   =
*     RETURN_TAB  =
*     DYNPFLD_MAPPING        =
*   EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS      = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_scheme-low.
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_werks-low.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield    = 'ZSCHEME'
*     PVALKEY     = ' '
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'S_SCHEME'
*     STEPL       = 0
*     WINDOW_TITLE           =
*     VALUE       = ' '
      value_org   = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY     = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB    =
* IMPORTING
*     USER_RESET  =
    TABLES
      value_tab   = it_master
*     FIELD_TAB   =
*     RETURN_TAB  =
*     DYNPFLD_MAPPING        =
* EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS      = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN .

    IF screen-name = 'S_DATE-LOW' OR screen-name = 'S_DATE-HIGH'.

      screen-input = 0.
      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

  SELECT * FROM zpainter_master
    INTO TABLE it_master
    WHERE bukrs IN s_bukrs
    AND   regio IN s_regio.


START-OF-SELECTION .
  READ TABLE it_master INTO wa_master WITH KEY zscheme = s_scheme-low.
  IF sy-subrc = 0.
    CALL SCREEN 0002.
  ELSE.
    MESSAGE 'Enter the vaild Scheme' TYPE 'E'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE USER_COMMAND_0001 INPUT.
*
*BREAK-POINT.
*
*
*LV_COMPANY = LV_COMPANY.
*LV_PLANT   = LV_PLANT.
*LV_SCHEME  = LV_SCHEME.
*LV_FROM    = LV_FROM.
*LV_TO      = LV_TO.
*
*COMPANY   = LV_COMPANY.
*PLANT     = LV_PLANT.
*SCHEMECOD = LV_SCHEME.
*
*IF LV_SCHEME IS NOT INITIAL.
*
*SELECT SINGLE ZSCH_DES
*  FROM ZSCHEME_MASTER
*  INTO LV_DESC
*  WHERE ZSCHEME = LV_SCHEME.
*
*ENDIF.
*
*SCHEMEDES = LV_DESC.
*FROMDATE  = LV_FROM.
*TODATE    = LV_TO.
*
*IF LV_PLANT IS NOT INITIAL.
*
*  SELECT SINGLE NAME1
*    FROM T001W
*    INTO SALES_OFF
*    WHERE WERKS = LV_PLANT.
*
*ENDIF.
*
*SALES_OFF = SALES_OFF.
*
*
**CASE SY-UCOMM.
**
**  WHEN 'CREATE'.
**
**        SELECT SINGLE * FROM ZSCHEME_LOG
**          INTO WA_LOG
**          WHERE BUKRS=
**          AND   ZSCHEME =
**          AND
**        CALL SCREEN 0002.
**
**ENDCASE.
**CALL SCREEN 0002.
*
*
*
*ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_data OUTPUT.

  DATA : l_index TYPE i.

  l_index = tc-top_line + sy-stepl - 1.

  READ TABLE it_tc INDEX l_index. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation

  it_tc-matkl = it_tc-matkl.
  it_tc-description = it_tc-description.
  "IT_TC-EWBEZ = IT_TC-EWBEZ.

  it_tc-zpoints = it_tc-zpoints.

  MODIFY it_tc INDEX tc-current_line.


  IF sy-ucomm <> 'CHANGE'.

    IF it_tc-matkl IS NOT INITIAL.

      screen_display 'IT_TC-MATKL'.


    ENDIF.

    IF it_tc-matkl IS NOT INITIAL.

      screen_display 'IT_TC-DESCRIPTION'.


    ENDIF.

    IF  it_tc-zpoints IS NOT INITIAL.


      screen_display 'IT_TC-ZPOINTS'.

    ENDIF.

  ENDIF.

  IF sy-ucomm = 'CHANGE'.

    IF it_tc-matkl IS NOT INITIAL.

      screen_display 'IT_TC-MATKL'.


    ENDIF.
  ENDIF.
ENDMODULE.                 " DISPLAY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATE_DATAS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_datas INPUT.
*  BREAK-POINT.
*  DATA : SEL(1) TYPE C .
*
*  DATA : LV_VTEXT TYPE VTEXT .
*
*  DATA : LV_TEXT TYPE TWEWT-EWBEZ .

  REFRESH it_tc1[].

*  DELETE IT_TC WHERE MATGRP IS INITIAL.
  it_tc1[] = it_tc[].

  READ TABLE it_tc1 INDEX tc-current_line. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation

  IF sy-subrc <> 0 .

    it_tc1-matkl = it_tc-matkl.
    it_tc1-description = it_tc-description.
    it_tc1-zpoints = it_tc-zpoints.

*    SELECT SINGLE EWBEZ
*    FROM TWEWT
*    INTO LV_TEXT
*    WHERE EXTWG = IT_TC-ZEINR
*    AND   SPRAS = 'EN' .
*
*    IT_TC-EWBEZ = LV_TEXT .

    IF it_tc-matkl IS NOT INITIAL.
      SELECT SINGLE wgbez FROM t023t INTO @DATA(lv_schdes1)
                          WHERE matkl = @it_tc-matkl.
      it_tc-description = lv_schdes1.
      SET CURSOR FIELD 'IT_TC-ZPOINTS'.

    ENDIF.

*    CLEAR : LV_TEXT .

    INSERT it_tc INDEX tc-current_line.


  ELSE.
*    IT-MANDT = SY-MANDT .
    it_tc1-matkl = it_tc-matkl.
    it_tc1-description = it_tc-description.
*    IT_TC1-EWBEZ = IT_TC-EWBEZ.
    it_tc1-zpoints = it_tc-zpoints.

*    SELECT SINGLE EWBEZ
*  FROM TWEWT
*  INTO LV_TEXT
*  WHERE EXTWG = IT_TC-MATGRP
*  AND   SPRAS = 'EN' .
*    IT_TC-EWBEZ = LV_TEXT .

    IF it_tc-matkl IS NOT INITIAL.
      SELECT SINGLE wgbez FROM t023t INTO @DATA(lv_schdes)
                                WHERE matkl = @it_tc-matkl.
      it_tc-description = lv_schdes.
      SET CURSOR FIELD 'IT_TC-ZPOINTS'.

    ENDIF.

*    CLEAR : LV_TEXT .

*BREAK-POINT.

    MODIFY it_tc INDEX tc-current_line.

    IF sel IS NOT INITIAL .

      it_tc-sel = 'X' .

    ENDIF.

    MODIFY it_tc INDEX tc-current_line.

    CLEAR : sel , it_tc-sel  .

*    ENDIF .

    CLEAR : sel , it_tc-sel  .

  ENDIF.

ENDMODULE.                 " UPDATE_DATAS  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.
  SET PF-STATUS 'ZSTATUS2'.
  SET TITLEBAR 'ZTITLE2'.


  DESCRIBE TABLE it_tc LINES fill2.
  tc-lines = fill2.

  CASE sy-ucomm.

    WHEN 'BACK'.

      LEAVE TO SCREEN 0.
*    SET SCREEN 0.

    WHEN 'EXIT'.

      LEAVE PROGRAM.

    WHEN 'CANCEL'.

      LEAVE PROGRAM.

    WHEN 'CHANGE'.

      PERFORM change.


  ENDCASE.

  IF it_tc[] IS INITIAL .

    SELECT a~matkl
           b~wgbez
           a~zpoints
           INTO TABLE it_tc
           FROM zpainter_sch_log AS a
           INNER JOIN t023t AS b ON b~matkl = a~matkl
           WHERE a~bukrs    IN s_bukrs
           AND   a~regio    IN s_regio
           AND   a~zscheme  IN s_scheme
           AND   a~from_per EQ s_date-low
           AND   a~to_per   EQ s_date-high
           AND   b~spras = sy-langu.
  ENDIF .

  DELETE it_tc WHERE matkl  EQ ' '.

*IF IT_TC[]  IS NOT INITIAL.
*
*   LOOP AT SCREEN .
*
*      IF SCREEN-NAME = 'IT_TC-MATGRP' OR  SCREEN-NAME = 'IT_TC-EWBEZ' OR  SCREEN-NAME = 'IT_TC-ZPOINTS'.
*          SCREEN-INPUT = 0.
*          MODIFY SCREEN.
*      ENDIF.
*
*
*     ENDLOOP.


*ENDIF.
*BREAK-POINT.
  SELECT zsch_des
    UP TO 1 ROWS FROM zpainter_master
    INTO lv_desc
    WHERE zscheme = s_scheme-low ORDER BY PRIMARY KEY.
  ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

  SELECT bland bezei FROM t005u INTO TABLE it_reg WHERE spras = sy-langu AND land1 = 'IN' AND bland = s_regio-low.
*  SELECT SINGLE NAME1
*    FROM T001W
*    INTO SALES_OFF
*    WHERE WERKS = S_WERKS-LOW.
  LOOP AT it_reg INTO wa_reg.
    wa_reg-regio = wa_reg-regio.
    wa_reg-regioname = wa_reg-regioname.
  ENDLOOP.

*    BREAK-POINT.
  "APPEND IT_REG to WA_REG.
*  LV_PLANT1   = S_WERKS-LOW.
  "LV_PLNAME   = LV_REGIONAME.
  lv_regio = wa_reg-regio.
  lv_regioname = wa_reg-regioname.
  lv_schname  = lv_desc.
  lv_from1    = s_date-low.
  lv_to1      = s_date-high.

ENDMODULE.                 " STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'ZSTATUS1'.
  SET TITLEBAR 'ZTITLE1'.

  CASE sy-ucomm.

    WHEN 'BACK'.

      LEAVE PROGRAM.

    WHEN 'EXIT'.

      LEAVE PROGRAM.

  ENDCASE.


ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK'.
      LEAVE SCREEN.
    WHEN 'SAVE'.
      SELECT * FROM zpainter_sch_log INTO TABLE @DATA(lt_log).

      LOOP AT it_tc INTO wa_save.
        LOOP AT lt_log INTO DATA(ls_log) WHERE zscheme = s_scheme-low
                                               AND matkl = wa_save-matkl.

        ENDLOOP.
        IF sy-subrc NE 0.
          wa_save1-bukrs    = s_bukrs-low.
          wa_save1-regio    = s_regio-low.
          wa_save1-zscheme  = s_scheme-low.
          wa_save1-from_per = s_date-low.
          wa_save1-to_per   = s_date-high.
          wa_save1-matkl    = wa_save-matkl.
          wa_save1-zsch_des = wa_master-zsch_des.
*        WA_SAVE1-EWBEZ    = WA_SAVE-EWBEZ.
          wa_save1-zpoints  = wa_save-zpoints.
          wa_save1-name1   = wa_reg-regioname.
          wa_save1-erdat   = sy-datum.
          wa_save1-ernam   = sy-uname.

          IF wa_save1-matkl IS NOT INITIAL AND wa_save1-zpoints IS NOT INITIAL.

            APPEND wa_save1 TO it_save1.
            MODIFY zpainter_sch_log FROM wa_save1.
            gv_flag = 'X'.
          ELSE.
            MESSAGE 'Give the Required Fields' TYPE 'E'.

          ENDIF.
          CLEAR : wa_save1 , wa_save.
          REFRESH : it_save1.
        ENDIF.
      ENDLOOP.

      IF gv_flag = 'X'.

        MESSAGE 'Data Saved Successfully' TYPE 'S'.

      ELSE.

        MESSAGE 'Data Not Modified' TYPE 'E'.

      ENDIF.
      CLEAR :wa_reg.
*BREAK-POINT.

    WHEN 'DELETE'.

      READ TABLE it_tc INTO wa_tc WITH KEY sel = 'X'.

      IF sy-subrc = 0.


        DELETE FROM  zpainter_sch_log WHERE bukrs = s_bukrs-low
                           AND   regio = s_regio-low
                           AND   zscheme = s_scheme-low
                           AND   from_per = s_date-low
                           AND   to_per   = s_date-high
                           AND   matkl    = wa_tc-matkl.

        DELETE it_tc WHERE sel = 'X'.

        IF sy-subrc = 0.

          MESSAGE 'Record Deleted Successfully' TYPE 'S'.

        ENDIF.


      ENDIF.


  ENDCASE.



ENDMODULE.                 " USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change .

*  IF IT_TC-MATGRP IS NOT INITIAL.
*
*    SCREEN_DISPLAY 'IT_TC-MATGRP'.
*
*    ENDIF.
*
*  IF  IT_TC-ZPOINTS IS NOT INITIAL.
*
*
*    SCREEN_DISPLAY 'IT_TC-ZPOINTS'.
*
*    ENDIF.



ENDFORM.                    " CHANGE

**---Added by Samsudeen on 08.09.2022---*
*MODULE f4 INPUT.
*  DATA: lt_ret TYPE TABLE OF ddshretval,
*        ls_ret TYPE ddshretval.
*  SELECT scheme, description FROM zsch_points INTO TABLE @DATA(lt_shlp).
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
**     DDIC_STRUCTURE  = ' '
*      retfield        = 'SCHEME'
**     PVALKEY         = ' '
*      dynpprog        = sy-cprog
*      dynpnr          = sy-dynnr
*      dynprofield     = 'IT_TC-ADD_MATGRP'
**     STEPL           = 0
**     WINDOW_TITLE    =
**     VALUE           = ' '
*      value_org       = 'S'  "'C'
**     MULTIPLE_CHOICE = ' '
**     DISPLAY         = ' '
**     CALLBACK_PROGRAM       = ' '
**     CALLBACK_FORM   = ' '
**     CALLBACK_METHOD =
**     MARK_TAB        =
**      IMPORTING
**     USER_RESET      =
*    TABLES
*      value_tab       = lt_shlp
**     FIELD_TAB       =
*      return_tab      = lt_ret
**     DYNPFLD_MAPPING =
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*  IF sy-subrc = 0.
*  ENDIF.
*
*ENDMODULE.
