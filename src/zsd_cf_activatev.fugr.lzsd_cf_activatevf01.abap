*----------------------------------------------------------------------*
***INCLUDE LZSD_CF_ACTIVATEVF01.
*----------------------------------------------------------------------*

FORM new_entry.
  IF zsd_cf_activatev-zcustype IS INITIAL .
    MESSAGE 'Customer Finance Type blank is not allowed' TYPE 'E' DISPLAY LIKE 'E'.
  ENDIF.

  zsd_cf_activatev-cdate = sy-datum.
  zsd_cf_activatev-ctime = sy-uzeit.
  zsd_cf_activatev-cname = sy-uname.
  zsd_cf_activatev-aedat = sy-datum.
  zsd_cf_activatev-aetim = sy-uzeit.
  zsd_cf_activatev-aenam = sy-uname.
ENDFORM.
FORM delete.
  MESSAGE 'Customer Deletion is not allowed' TYPE 'E' DISPLAY LIKE 'E'.
ENDFORM.
FORM update.

  SELECT SINGLE * FROM zsd_cf_activate
    INTO @DATA(lw_data)
    WHERE kunnr = @zsd_cf_activatev-kunnr.

  lw_data-aedat = sy-datum.
  lw_data-aetim = sy-uzeit.
  lw_data-aenam = sy-uname.
  MODIFY zsd_cf_activate FROM lw_data.
  LEAVE TO TRANSACTION 'ZSD_CF'.
ENDFORM.
