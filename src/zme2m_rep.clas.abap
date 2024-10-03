class ZME2M_REP definition
  public
  final
  create public .

*"* public components of class ZME2M_REP
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_CHANGE_OUTTAB_CUS .
protected section.
*"* protected components of class ZME2M_REP
*"* do not include other source files here!!!
private section.
*"* private components of class ZME2M_REP
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZME2M_REP IMPLEMENTATION.


METHOD IF_EX_ME_CHANGE_OUTTAB_CUS~FILL_OUTTAB.

* When processing this source code, you activate the following functionality:
* The reporting transactions for purchasing documents provide three main views
* for display: basic list, delivery schedule, and account assignment. All
* three views contain a column "Material". If the material of a purchasing
* document item is a manufacturer part number (MPN) then this MPN is shown
* as "Material". The internal inventory managed material is not visible.
* The following source code replaces the MPN by the inventory managed material.

  DATA: ls_ekpo TYPE ekpo.

  FIELD-SYMBOLS: <fs_outtab>   TYPE ANY,
                 <fs_ebeln>    TYPE ebeln,
                 <fs_ebelp>    TYPE ebelp,
                 <fs_material> TYPE matnr.

* check that a purchasing document view is displayed
  CHECK im_struct_name EQ 'MEREP_OUTTAB_PURCHDOC'   OR    "view: basic list
        im_struct_name EQ 'MEREP_OUTTAB_SCHEDLINES' OR    "view: delivery schedule
        im_struct_name EQ 'MEREP_OUTTAB_ACCOUNTING'.      "view: account assignment

* loop at the output table and assign a field symbol
  LOOP AT ch_outtab ASSIGNING <fs_outtab>.

*-- assign the purchasing document number to a field symbol
    ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_ebeln>.
    CHECK sy-subrc = 0.
*-- assign the purchasing document item number to a field symbol
    ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_outtab> TO <fs_ebelp>.
    CHECK sy-subrc = 0.
*-- assign the manufacturer part number to a field symbol
    ASSIGN COMPONENT 'EMATN' OF STRUCTURE <fs_outtab> TO <fs_material>.
    CHECK sy-subrc = 0.

*-- read the corresponding purchasing document item
    CALL FUNCTION 'ME_EKPO_SINGLE_READ'
      EXPORTING
        pi_ebeln         = <fs_ebeln>
        pi_ebelp         = <fs_ebelp>
      IMPORTING
        po_ekpo          = ls_ekpo
      EXCEPTIONS
        no_records_found = 1
        OTHERS           = 2.
    CHECK sy-subrc = 0.

*-- assign the inventory managed material to the field "Material" in the output table
    <fs_material> = ls_ekpo-matnr.

  ENDLOOP.
FIELD-SYMBOLS : <FS_TABREL> TYPE MEREP_OUTTAB_PURCHDOC.
DATA : LV_TABIX TYPE INT3.
  IF SY-TCODE EQ 'ME2M' .
  "  BREAK-POINT.
      IF IM_STRUCT_NAME EQ 'MEREP_OUTTAB_PURCHDOC'.
    AUTHORITY-CHECK OBJECT 'ZNETPR'
             ID 'ACTVT' field  '03' .  "'03'.
    IF SY-SUBRC <> 0 .
             LOOP AT CH_OUTTAB ASSIGNING <FS_TABREL> .
                 <FS_TABREL>-NETPR = '0.00' .
                 modify ch_outtab from <FS_TABREL>.  "#EC CI_FLDEXT_OK[2522971]
            ENDLOOP.

ELSE.


  ENDIF.

    ENDIF.
    ENDIF.

ENDMETHOD.
ENDCLASS.
