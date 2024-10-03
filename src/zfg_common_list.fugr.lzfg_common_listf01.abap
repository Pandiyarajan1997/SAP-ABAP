*----------------------------------------------------------------------*
***INCLUDE LZFG_COMMON_LISTF01.
*----------------------------------------------------------------------*
FORM new_entry.
  SELECT SINGLE FROM ZCusttype_extnsn
  FIELDS akont
  WHERE custtype = @zvcusttype_extns-custtype AND CCODE = @zvcusttype_extns-ccode
  INTO @DATA(lv_akont).
  IF sy-subrc = 0.
    zvcusttype_extns-akont = lv_akont.
    ELSE.
      CONDENSE vim_position_info.
       IF vim_position_info+11(1) = 1.
     zvcusttype_extns-akont = <vim_ctotal>+5(8).
      ELSEIF vim_position_info+11(1) > 1.
        zvcusttype_extns-akont = <vim_ctotal>+3(8).
         ENDIF.
*      data(lv_akont) = TOTAL+5(8).
  ENDIF.
    IF zvcusttype_extns-akont is INITIAL.
        MESSAGE 'Please Enter Reconcilliation Account' TYPE 'E'.
    ELSEIF zvcusttype_extns-vkorg IS INITIAL.
    MESSAGE 'Pleas Enter Sales organization' TYPE 'E'.
  ELSEIF zvcusttype_extns-dist_channel IS INITIAL.
    MESSAGE 'Pleas Enter Distribution Channel' TYPE 'E'.
  ELSEIF zvcusttype_extns-divison IS INITIAL.
    MESSAGE 'Pleas Enter Divison' TYPE 'E'.
  ENDIF.

*  IF zv_api_comm_list-field_type IS NOT INITIAL ."AND zapi_common_list-field_key IS NOT INITIAL.
*
* select single from zapi_chk_table_t FIELDS field_descr WHERE field_type = @zv_api_comm_list-field_type
*   into @zv_api_comm_list-field_descr.

*   ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ENTRY_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE entry_check INPUT.

  SELECT SINGLE FROM ZCusttype_extnsn
  FIELDS akont
  WHERE custtype = @zvcusttype_extns-custtype AND CCODE = @zvcusttype_extns-ccode
  INTO @DATA(lv_akont).
  IF sy-subrc = 0.
       zvcusttype_extns-akont = lv_akont.
     ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ENTRY_CHECK OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE entry_check OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

  SELECT SINGLE FROM ZCusttype_extnsn
  FIELDS akont
  WHERE custtype = @zvcusttype_extns-custtype AND CCODE = @zvcusttype_extns-ccode
  INTO @lv_akont.
  IF sy-subrc = 0.
   LOOP AT SCREEN.
     IF Screen-name = 'ZVCUSTTYPE_EXTNS-AKONT'.
       screen-input = 0.
       zvcusttype_extns-akont = lv_akont.
        Append VALUE #( akont = lv_akont ) to zvcusttype_extns_extract.
       Append VALUE #( akont = lv_akont ) to ZVCUSTTYPE_EXTNS_TOTAL.
              MODIFY SCREEN.
       Exit.
     ENDIF.
     ENDLOOP.
     ELSE.
       CONDENSE vim_position_info.
       IF vim_position_info+6(1) <> 0 ." and zvcusttype_extns-ccode is NOT INITIAL.
          LOOP AT SCREEN.
     IF Screen-name = 'ZVCUSTTYPE_EXTNS-AKONT'.
       screen-input = 0.
       MODIFY SCREEN.
       zvcusttype_extns-akont = lv_akont.
        Append VALUE #( akont = lv_akont ) to zvcusttype_extns_extract.
       Append VALUE #( akont = lv_akont ) to ZVCUSTTYPE_EXTNS_TOTAL.
       Exit.
     ENDIF.
     ENDLOOP.
         ENDIF.
  ENDIF.
ENDMODULE.
FORM FILL_ENTRY.

BREAK-POINT.

  ENDFORM.
