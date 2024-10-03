FUNCTION-POOL zfg_asn_migo.                 "MESSAGE-ID ..
DATA go_alv TYPE REF TO cl_salv_table.
DATA: gr_display   TYPE REF TO cl_salv_display_settings.
DATA: gr_layout TYPE REF TO cl_salv_layout.
DATA: gs_key TYPE salv_s_layout_key.
FIELD-SYMBOLS <fs_field> TYPE any.
* INCLUDE LZFG_ASN_MIGOD...                  " Local class definition
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_link_click FOR EVENT link_click  "Hotspot Handler
        OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.                    "lcl_events DEFINITION
DATA: gr_event_handler TYPE REF TO lcl_events.

FIELD-SYMBOLS <fst_alv_tab> TYPE STANDARD TABLE.
FIELD-SYMBOLS <fs_hyperlink_data> TYPE any .
*
*----------------------------------------------------------------------*
*       CLASS lcl_events IMPLEMENTATION
*----------------------------------------------------------------------*
*  SAL Event Handler Methods                                           *
*----------------------------------------------------------------------*
CLASS lcl_events IMPLEMENTATION.
  METHOD on_link_click.
    IF <fs_hyperlink_data> = 'FB03'.

      READ TABLE <fst_alv_tab> ASSIGNING FIELD-SYMBOL(<lw_data>) INDEX row.
      FIELD-SYMBOLS: <lfs_belnr> TYPE any.
      FIELD-SYMBOLS: <lfs_bukrs> TYPE any.
      FIELD-SYMBOLS: <lfs_gjahr> TYPE any.
      DATA(l_belnr) = |<lw_data>-{ column }|.
      DATA(l_bukrs) = |<lw_data>-bukrs|.
      DATA(l_gjahr) = |<lw_data>-gjahr|.

      ASSIGN (l_belnr) TO <lfs_belnr>.
      ASSIGN (l_bukrs) TO <lfs_bukrs>.
      ASSIGN (l_gjahr) TO <lfs_gjahr>.

      SET PARAMETER ID 'BLN' FIELD <lfs_belnr>.
      SET PARAMETER ID 'BUK' FIELD <lfs_bukrs>.
      SET PARAMETER ID 'GJR' FIELD <lfs_gjahr>.
      CALL TRANSACTION <fs_hyperlink_data> AND SKIP FIRST SCREEN.

    ENDIF.
  ENDMETHOD.                    "on_link_click
*
ENDCLASS.                    "lcl_events IMPLEMENTATION
