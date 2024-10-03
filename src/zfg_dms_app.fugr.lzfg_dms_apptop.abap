FUNCTION-POOL zfg_dms_app.                  "MESSAGE-ID ..

* INCLUDE LZFG_DMS_APPD...                   " Local class definition

DATA: gw_bdcdata TYPE bdcdata,
      gt_bdcdata TYPE TABLE OF bdcdata WITH EMPTY KEY,
      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
      gw_bdcmsg  TYPE bdcmsgcoll.
