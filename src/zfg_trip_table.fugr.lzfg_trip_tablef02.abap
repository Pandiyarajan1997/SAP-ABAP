*----------------------------------------------------------------------*
***INCLUDE LZFG_TRIP_TABLEF02.
*----------------------------------------------------------------------*

FORM ZFREIGHT_HALF.


  IF ZFREIGHT_HLOAD-FTYPE IS INITIAL .
    MESSAGE 'Please Enter Freight Type' TYPE 'E'.
  ELSEIF ZFREIGHT_HLOAD-CCODE IS INITIAL.
    MESSAGE 'Please Enter Company Code' TYPE 'E'.
  ELSEIF ZFREIGHT_HLOAD-TNAME IS INITIAL.
    MESSAGE 'Please Enter Vendor Code' TYPE 'E'.
  ELSEIF ZFREIGHT_HLOAD-FLOC IS INITIAL.
    MESSAGE 'Please Enter Plant Code' TYPE 'E'.
  ELSEIF ZFREIGHT_HLOAD-TLOC IS INITIAL.
    MESSAGE 'Please Enter Area Name' TYPE 'E'.
  ELSEIF ZFREIGHT_HLOAD-VFROM IS INITIAL.
    MESSAGE 'Please Enter Valid From Date' TYPE 'E'.
  ELSEIF ZFREIGHT_HLOAD-VTO IS  INITIAL.
    MESSAGE 'Please Enter Valid To Date' TYPE 'I'.
  ELSEIF ZFREIGHT_HLOAD-ZPRI IS INITIAL.
    MESSAGE 'Please Enter Price' TYPE 'I'.
  ENDIF.

IF ZFREIGHT_HLOAD-VFROM IS NOT INITIAL.
  IF ZFREIGHT_HLOAD-VFROM < SY-DATUM .
    MESSAGE 'Please Enter Valid Date' TYPE 'E'.
 ENDIF.
 ENDIF.

IF ZFREIGHT_HLOAD-VTO IS NOT INITIAL.
 IF ZFREIGHT_HLOAD-VTO < ZFREIGHT_HLOAD-VFROM .
   MESSAGE 'Please Enter Valid Date' TYPE 'E' .
 ENDIF.
ENDIF.

ENDFORM.
