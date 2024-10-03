class ZCL_EXM_IM_ACC_DOCUMENT definition
  public
  final
  create public .

*"* public components of class ZCL_EXM_IM_ACC_DOCUMENT
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ACC_DOCUMENT .
protected section.
*"* protected components of class ZCL_EXM_IM_ACC_DOCUMENT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_EXM_IM_ACC_DOCUMENT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_EXM_IM_ACC_DOCUMENT IMPLEMENTATION.


METHOD if_ex_acc_document~change .

***********************************************************************
* Example to move fields from BAPI parameter EXTENSION2 to structure  *
* ACCIT (accounting document line items).                             *
* The dictionary structure (content for EXTENSION2-STRUCTURE) must    *
* contain field POSNR, (TYPE POSNR_ACC) to indentify the correct line *
* item of the internal table ACCIT.                                   *
***********************************************************************

  DATA: wa_extension   TYPE bapiparex,
        ext_value(960) TYPE c,
        wa_accit       TYPE accit,
        l_ref          TYPE REF TO data.

  FIELD-SYMBOLS: <l_struc> TYPE any,
                 <l_field> TYPE any.
  CONSTANTS: lc_posnr TYPE posnr_acc VALUE '000000002'.
  SORT c_extension2 BY structure.

*  LOOP AT c_extension2 INTO wa_extension.
*    AT NEW structure.
*      CREATE DATA l_ref TYPE (wa_extension-structure).
*      ASSIGN l_ref->* TO <l_struc>.
*    ENDAT.
*    CONCATENATE wa_extension-valuepart1 wa_extension-valuepart2
*                wa_extension-valuepart3 wa_extension-valuepart4
*           INTO ext_value.
*    MOVE ext_value TO <l_struc>.
*    ASSIGN COMPONENT 'POSNR' OF STRUCTURE <l_struc> TO <l_field>.
*    READ TABLE c_accit WITH KEY posnr = <l_field>
*          INTO wa_accit.
*    IF sy-subrc IS INITIAL.
*      MOVE-CORRESPONDING <l_struc> TO wa_accit.
*      MODIFY c_accit FROM wa_accit INDEX sy-tabix.
*    ENDIF.
*  ENDLOOP.
  IF sy-tcode = 'ZFI_INV_POST'.
    LOOP AT c_extension2 INTO wa_extension WHERE structure = 'BAPIACHE09'.
      IF wa_extension-valuepart1 = 'XMWST'.
        LOOP AT c_accit INTO wa_accit WHERE posnr = lc_posnr.
          wa_accit-xmwst = 'X'.
          MODIFY c_accit FROM wa_accit INDEX sy-tabix TRANSPORTING xmwst.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMETHOD.                    "IF_EX_ACC_DOCUMENT~CHANGE


method IF_EX_ACC_DOCUMENT~FILL_ACCIT.
endmethod.
ENDCLASS.
