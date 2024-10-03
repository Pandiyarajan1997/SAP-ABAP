*----------------------------------------------------------------------*
***INCLUDE LZFG_TRIP_TABLEF03.
*----------------------------------------------------------------------*

FORM ZFREIGHT_FULL.

  IF ZFREIGHT_FLOAD-FTYPE IS INITIAL .
    MESSAGE 'Please Enter Freight Type' TYPE 'E'.
  ELSEIF ZFREIGHT_FLOAD-TCAP IS INITIAL.
    MESSAGE 'Please Enter Truck Type' TYPE 'E'.
  ELSEIF ZFREIGHT_FLOAD-CCODE IS INITIAL.
    MESSAGE 'Please Enter Company Code' TYPE 'E'.
  ELSEIF ZFREIGHT_FLOAD-TNAME IS INITIAL.
    MESSAGE 'Please Enter Vendor Code' TYPE 'E'.
  ELSEIF ZFREIGHT_FLOAD-FLOC IS INITIAL.
    MESSAGE 'Please Enter Plant Code' TYPE 'E'.
  ELSEIF ZFREIGHT_FLOAD-TLOC IS INITIAL.
    MESSAGE 'Please Enter Area Name' TYPE 'E'.
  ELSEIF ZFREIGHT_FLOAD-VFROM IS INITIAL.
    MESSAGE 'Please Enter Valid From Date' TYPE 'E'.
  ELSEIF ZFREIGHT_FLOAD-VTO IS  INITIAL.
    MESSAGE 'Please Enter Valid To Date' TYPE 'I'.
  ELSEIF ZFREIGHT_FLOAD-ZPPT IS INITIAL.
    MESSAGE 'Please Enter Price' TYPE 'I'.
  ENDIF.

  IF ZFREIGHT_FLOAD-VFROM IS NOT INITIAL.
  IF ZFREIGHT_FLOAD-VFROM < SY-DATUM .
    MESSAGE 'Please Enter Valid Date' TYPE 'E'.
 ENDIF.
 ENDIF.

IF ZFREIGHT_FLOAD-VTO IS NOT INITIAL.
 IF ZFREIGHT_FLOAD-VTO < ZFREIGHT_FLOAD-VFROM .
   MESSAGE 'Please Enter Valid Date' TYPE 'E' .
 ENDIF.
ENDIF.

ENDFORM.
