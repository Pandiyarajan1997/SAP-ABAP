FUNCTION-POOL ZFG_DMS_PO_RETURN.            "MESSAGE-ID ..

* INCLUDE LZFG_DMS_PO_RETURND...             " Local class definition
DATA: gw_bdcdata TYPE bdcdata,
      gt_bdcdata TYPE TABLE OF bdcdata WITH EMPTY KEY,
      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
      gw_bdcmsg  TYPE bdcmsgcoll.
DATA lv_msg_text TYPE string.
DATA: lv_datum(10) TYPE c,
      lv_date1(10) TYPE c.
DATA lv_amount(13) TYPE c.


FORM bdc_dynpro  USING program TYPE any
                        dynpro TYPE any.
  CLEAR:gw_bdcdata.
  gw_bdcdata-program  = program.
  gw_bdcdata-dynpro   = dynpro.
  gw_bdcdata-dynbegin = 'X'.
  APPEND gw_bdcdata TO gt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_field
*&---------------------------------------------------------------------*
FORM bdc_field  USING  fnam TYPE any
                      fval TYPE any.
  CLEAR:gw_bdcdata.
  gw_bdcdata-fnam = fnam.
  gw_bdcdata-fval = fval.
  APPEND gw_bdcdata TO gt_bdcdata.
ENDFORM.
