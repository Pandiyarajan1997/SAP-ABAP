class ZCL_MATERIAL_COPY definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_BADI_MATERIAL_CC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MATERIAL_COPY IMPLEMENTATION.


  METHOD if_ex_badi_material_cc~material_assign_number.


    IF sy-tcode EQ 'ZMMCC_R'.
* Data Declaration
      DATA:
        ls_pre03 TYPE pre03.
* Get hold of the material type from the material master data
      SELECT SINGLE mtart
        FROM mara
        INTO @DATA(lv_mtart)
       WHERE matnr = @p_matnr.
      IF sy-subrc EQ 0 AND ( lv_mtart EQ 'HALB' OR lv_mtart EQ 'FERT').
* Changing material number according to requirement
         ls_pre03-matnr = 'Z' && p_matnr.
         APPEND ls_pre03 TO matnr_ueb.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method IF_EX_BADI_MATERIAL_CC~MATERIAL_CHANGE_DATA.

  endmethod.
ENDCLASS.
