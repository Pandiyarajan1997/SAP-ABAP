FUNCTION-POOL YSD_FG_EINVOICE.              "MESSAGE-ID ..

* INCLUDE LZSD_FG_EINVOICED...               " Local class definition


*******Cleartax Sandbox Cancel Eway bill API Credintals **************************
DATA: v_str           TYPE string,
      v_str1          TYPE string,
      it_bapiret      TYPE TABLE OF bapiret2,
      wa_bapiret      TYPE bapiret2,
      gv_eway_url     TYPE string, "VALUE 'https://einvoicing.internal.cleartax.co/v2/eInvoice/ewaybill/cancel',
      gv_eway_gstin   TYPE string," VALUE '29AAFCD5862R000',
      gv_eway_ownerid TYPE string, " VALUE '4abbf226-bd72-4f2a-a3c6-5bbf4d9b47f9',
      gv_eway_auth    TYPE string. "VALUE '1.9a34270c-c285-47d1-a32b-310bc1accf76_7d0170344c8adc0557dc94ced6c96d110e83fbd6cf935e15407d08064b6446d1'.
