*----------------------------------------------------------------------*
***INCLUDE LZFG_ALPNF01.
*----------------------------------------------------------------------*
FORM new_entry_mat.
  IF zvalpn_material-ckey IS NOT INITIAL.
    SELECT SINGLE FROM zalpn_categ FIELDS * WHERE ckey = @zvalpn_material-ckey INTO @DATA(ls_category).
    IF sy-subrc = 0.
      zvalpn_material-category = ls_category-category.
    ENDIF.
  ENDIF.

*  IF zvalpn_material-sckey IS NOT INITIAL.
*    SELECT SINGLE FROM zalpn_subcateg FIELDS * WHERE ckey = @zvalpn_material-ckey and  sckey = @zvalpn_material-sckey INTO @DATA(ls_subcategory).
*    IF sy-subrc = 0.
*      zvalpn_material-subcategory = ls_subcategory-subcategory.
*      ELSE.
*        MESSAGE |The Sub-Category is not avilable in the Category { zvalpn_material-category }| TYPE 'E'.
*    ENDIF.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  VALIDATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validation INPUT.
  IF zvalpn_material-ckey IS INITIAL.
    MESSAGE 'Please Maintain Category' TYPE 'E'.
  ELSE.
    SELECT SINGLE FROM zalpn_categ FIELDS * WHERE ckey = @zvalpn_material-ckey INTO @DATA(ls_category).
    IF sy-subrc <> 0.
      MESSAGE |{ zvalpn_material-ckey } does not exist in ZALPN_CATEG Table. Entry Failed | TYPE  'E'.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALIDATIONSC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validationsc INPUT.
  IF zvalpn_material-sckey IS INITIAL.
    MESSAGE 'Please Maintain Sub-Category' TYPE  'E'.
*      vim_abort_saving = 'X'.
*      sy-subrc = 4.
  ELSE.
    SELECT SINGLE FROM zalpn_subcateg FIELDS * WHERE sckey = @zvalpn_material-sckey INTO @DATA(ls_subcategory).
    IF sy-subrc <> 0.
      MESSAGE |{ zvalpn_material-sckey } does not exist in ZALPN_SUBCATEG Table. Entry Failed | TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.
FORM new_entry.
  DATA: lv_curr_num TYPE p,
        gt_subcat   TYPE STANDARD TABLE OF zalpn_subcateg,
        lv_lines type i.

  REFRESH gt_subcat[].
  SELECT FROM zalpn_subcateg FIELDS * WHERE ckey = @zvalpn_subcateg-ckey INTO TABLE @gt_subcat.
  IF sy-subrc = 0.
    PERFORM number_ranges TABLES gt_subcat
                          USING '01' 'ZSC_NRC' '' zvalpn_subcateg-ckey
                          CHANGING lv_curr_num .
        lv_lines = lines( gt_subcat ).
    IF exind <> lv_curr_num.
    clear lv_curr_num.

        lv_curr_num = exind + lv_lines.
          UPDATE NRIV SET  nrlevel    = lv_curr_num
                  WHERE  object = 'ZSC_NRC' AND  subobject = zvalpn_subcateg-ckey.
      COMMIT WORK.
    ENDIF.
    zvalpn_subcateg-sckey = lv_curr_num.

  ELSE.

    IF exind = 1.
    SELECT SINGLE FROM nriv FIELDS * WHERE object = 'ZSC_NRC' AND subobject = @zvalpn_subcateg-ckey INTO @DATA(l_nriv).
    If sy-subrc <> 0.
    INSERT NRIV FROM @( VALUE #(
        client     = sy-mandt
        object     = 'ZSC_NRC'
        subobject  = zvalpn_subcateg-ckey
        nrrangenr  = '01'
        fromnumber = '0001'
        tonumber   = '9999'
        nrlevel    = 1
    ) ).
    ENDIF.
    COMMIT WORK.
         zvalpn_subcateg-sckey = 0001.
    ELSE.
      SELECT SINGLE FROM nriv FIELDS * WHERE object = 'ZSC_NRC' AND subobject = @zvalpn_subcateg-ckey INTO @l_nriv.
          PERFORM number_ranges TABLES gt_subcat
                          USING '01' 'ZSC_NRC' l_nriv-nrlevel zvalpn_subcateg-ckey
                          CHANGING lv_curr_num .
     IF exind <> lv_curr_num.
    clear lv_curr_num.
        lv_curr_num = exind .
          UPDATE NRIV SET  nrlevel    = lv_curr_num
                  WHERE  object = 'ZSC_NRC' AND  subobject = zvalpn_subcateg-ckey.
            COMMIT WORk.
    ENDIF.
       zvalpn_subcateg-sckey = lv_curr_num.
    ENDIF.
  ENDIF.
ENDFORM.
FORM number_ranges TABLES p_tsubcat
                   USING p_nr p_obj p_exind p_subobj
                   CHANGING VALUE(pv_curr_num).
  TRY.
    data(p_subcat) =  lines( p_tsubcat ).
      cl_numberrange_runtime=>number_get(
        EXPORTING
*     ignore_buffer     =                  " Ignore Buffer
          nr_range_nr       =   p_nr             " Interval Number
          object            =   p_obj              " Number Range Object
          quantity          =   COND #( WHEN p_subcat is NOT INITIAL THEN CONV #( p_subcat ) ELSE CONV #( p_exind ) )              " Number of Numbers in Buffer
          subobject         =   CONV #( p_subobj )               " Subobject
*     toyear            =                  " To Fiscal Year
        IMPORTING
          number            =   DATA(lv_latest_num)               " Returned number
          returncode        =    DATA(lv_code)              " Return code
          returned_quantity =  DATA(lv_qty)                " Number of returned numbers
      ).
    CATCH cx_nr_object_not_found. " Error: Object does not exist
    CATCH cx_number_ranges.       " Error changing intervals and objects
  ENDTRY.

  CLEAR: pv_curr_num.
IF p_subcat is NOT INITIAL.
  ASSERT lv_qty =  lines( p_tsubcat ) .
  pv_curr_num = lv_latest_num - lv_qty.
  pv_curr_num += 1.
  ELSE.
  pv_curr_num = lv_latest_num - lv_qty.
  pv_curr_num += 1.
    ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Module ENTRY_CHECK OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE entry_check OUTPUT.
    IF zvalpn_material-sckey IS NOT INITIAL.
    SELECT SINGLE FROM zalpn_subcateg FIELDS * WHERE ckey = @zvalpn_material-ckey and  sckey = @zvalpn_material-sckey INTO @ls_subcategory.
    IF sy-subrc = 0.
      zvalpn_material-subcategory = ls_subcategory-subcategory.
      ELSE.
        MESSAGE |The Sub-Category is not avilable in the Category { zvalpn_material-category }| TYPE 'E'.
    ENDIF.
  ENDIF.
*  IF  zvalpn_subcateg-subcategory is INITIAL.
*MESSAGE 'Please Enter Sub-category' TYPE 'E'.
*  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ENTRY_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE entry_check INPUT.
  IF zvalpn_material-ckey is NOT INITIAL.
    SELECT SINGLE FROM zalpn_categ FIELDS * WHERE ckey = @zvalpn_material-ckey INTO @DATA(ls_categ).
      IF sy-subrc <> 0.
        MESSAGE |Category not Present. Please Maintain Category| TYPE 'E'.
        ELSEIF zvalpn_material-sckey IS NOT INITIAL.
    SELECT SINGLE FROM zalpn_subcateg FIELDS * WHERE ckey = @zvalpn_material-ckey and  sckey = @zvalpn_material-sckey INTO @ls_subcategory.
    IF sy-subrc = 0.
      zvalpn_material-subcategory = ls_subcategory-subcategory.
      ELSE.
          zvalpn_material-category =  ls_categ-category.
        MESSAGE |The Sub-Category is not available in the Category { ls_categ-category }| TYPE 'E'.
    ENDIF.
  ENDIF.
  ENDIF.

ENDMODULE.
FORM CHECK_ENTRY.

  ENDFORM.
