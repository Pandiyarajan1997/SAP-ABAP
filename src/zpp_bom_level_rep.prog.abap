*&---------------------------------------------------------------------*
*& Report ZPP_BOM_LEVEL_REP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& Functional                  : Ragu M                                *
*& Developer                   : Himansu Patnaik                       *
*& Created On                  : 26.08.2022                            *
*& Description                 : BOM Material Detailed level by level  *
*                                view.                                 *
*& Report Name                 : ZPP_BOM_LEVEL_REP                     *
*& Report Name                 : ZCS11                                 *
*& Transport Request           : DEVK932103                            *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*                          Modification Log                            *
*----------------------------------------------------------------------*
* Date         |     Name          |   Request No.  |   Description    *
*              |                   |                |                  *
*              |                   |                |                  *
*              |                   |                |                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zpp_bom_level_rep.
*----------------------------------------------------------------------*
*-Data Types Declaration                                               *
*----------------------------------------------------------------------*

TYPES :  BEGIN OF ty_final_bom,
           matnr TYPE matnr,
           dglvl TYPE dglvl,
           bmeng TYPE basmn,
           emeng TYPE emeng.
           INCLUDE STRUCTURE stpox.
TYPES :  END       OF ty_final_bom.

DATA : gt_final_bom TYPE STANDARD TABLE OF ty_final_bom,
       gs_final_bom TYPE ty_final_bom,
       gt_stb       TYPE STANDARD TABLE OF stpox,
       gt_alv       TYPE REF TO cl_salv_table,
       gt_message   TYPE REF TO cx_salv_msg.

DATA : lv_matnr TYPE mast-matnr,
       lv_werks TYPE mast-werks,
       lv_stufe TYPE char11.

*-----------------------------------------------------------------*
*-SELECTION SCREEN                                                *
*-----------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : s_matnr FOR lv_matnr OBLIGATORY,
                   s_werks FOR lv_werks OBLIGATORY.

  PARAMETERS : p_date  TYPE sy-datum DEFAULT sy-datlo MODIF ID sp2,
               p_emeng TYPE emeng.

SELECTION-SCREEN END OF BLOCK b1.

*-----------------------------------------------------------------*
*           AT SELECTION-SCREEN OUTPUT.                           *
*-----------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_date IS NOT INITIAL.
      IF screen-group1 = 'SP2'.
        screen-input = '0'.
        screen-invisible = '0'.
        screen-required = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

*-----------------------------------------------------------------*
*       CLASS lcl_Perf_Eval DEFINITION                            *
*-----------------------------------------------------------------*
CLASS lcl_bom_level DEFINITION.

  PUBLIC SECTION.

    METHODS get_data.
    METHODS get_alv_instance.
    METHODS disp_alv.

ENDCLASS.                   "lcl_perf_eval DEFINITION
*-----------------------------------------------------------------*
*       CLASS lcl_Perf_Eval IMPLEMENTATION                        *
*-----------------------------------------------------------------*
CLASS lcl_bom_level IMPLEMENTATION.

  METHOD get_data.

    SELECT a~matnr,a~werks,a~stlnr,b~stlst,b~andat,b~wrkan
                    FROM mast AS a INNER JOIN stko AS b
                               ON a~stlnr  = b~stlnr
                               INTO TABLE @DATA(lt_mast)
                                WHERE a~matnr IN @s_matnr
                                  AND b~wrkan IN @s_werks
                                  AND b~stlst = '01'.

    IF lt_mast IS NOT INITIAL.
      DELETE ADJACENT DUPLICATES FROM lt_mast COMPARING matnr.
      LOOP AT lt_mast ASSIGNING FIELD-SYMBOL(<fs_mast>).

        CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
          EXPORTING
            capid                 = 'PP01'
            datuv                 = p_date
            emeng                 = p_emeng
            mehrs                 = 'X'
            mtnrv                 = <fs_mast>-matnr
            werks                 = <fs_mast>-werks
          TABLES
            stb                   = gt_stb
          EXCEPTIONS
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            OTHERS                = 9.
        IF sy-subrc <> 0.
        ENDIF.

        IF gt_stb IS NOT INITIAL.
          LOOP AT gt_stb ASSIGNING FIELD-SYMBOL(<fs_stb>).
            CLEAR lv_stufe.
            MOVE-CORRESPONDING <fs_stb> TO gs_final_bom.

            lv_stufe = <fs_stb>-stufe.
            TRANSLATE lv_stufe USING ' .'.
            lv_stufe+10(1) = ' '.
            IF <fs_stb>-stufe < 9.
              <fs_stb>-stufe = 9 - <fs_stb>-stufe.
              SHIFT lv_stufe BY <fs_stb>-stufe PLACES.
            ENDIF.
            gs_final_bom-dglvl  = lv_stufe.

            AT FIRST.
              gs_final_bom-matnr = <fs_mast>-matnr.

              SELECT SINGLE stlnr,stlal,bmeng FROM stko
              INTO @DATA(ls_stko) WHERE stlal = @<fs_stb>-stlal
                                    AND stlnr = @<fs_stb>-stlnr.
              IF sy-subrc = 0.
                gs_final_bom-bmeng = ls_stko-bmeng.
              ENDIF.
              gs_final_bom-emeng  = p_emeng.
            ENDAT.
            APPEND gs_final_bom TO gt_final_bom.
            CLEAR gs_final_bom.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDMETHOD.
  METHOD get_alv_instance.
    DATA : functions       TYPE REF TO cl_salv_functions_list,
           layout_settings TYPE REF TO cl_salv_layout,
           layout_key      TYPE salv_s_layout_key,
           columns         TYPE REF TO cl_salv_columns_table.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = gt_alv
          CHANGING
            t_table      = gt_final_bom.

      CATCH cx_salv_msg INTO gt_message.
        DATA(lv_string) = gt_message->get_text( ).
        MESSAGE lv_string TYPE 'E'.
    ENDTRY.

    functions = gt_alv->get_functions( ).
    functions->set_all( ).

    layout_settings = gt_alv->get_layout( ).

    layout_key-report = sy-repid.
    layout_settings->set_key( layout_key ).
    layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

    columns = gt_alv->get_columns( ).

    columns->set_optimize( ).

  ENDMETHOD.
  METHOD disp_alv.
    CALL METHOD gt_alv->display.
  ENDMETHOD.

ENDCLASS.
*-----------------------------------------------------------------*
*       START-OF-SELECTION.                                       *
*-----------------------------------------------------------------*
START-OF-SELECTION.

  DATA lo_cl_bom TYPE REF TO lcl_bom_level.
  CREATE OBJECT lo_cl_bom.

  lo_cl_bom->get_data( ).
  lo_cl_bom->get_alv_instance( ).
  lo_cl_bom->disp_alv( ).
