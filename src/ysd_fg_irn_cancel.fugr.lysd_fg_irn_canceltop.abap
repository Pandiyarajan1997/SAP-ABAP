FUNCTION-POOL YSD_FG_IRN_CANCEL.            "MESSAGE-ID ..

* INCLUDE LZSD_FG_IRN_CANCELD...             " Local class definition

DATA: v_msg TYPE c.
*******Cleartax Sandbox IRN Cancel Einvoice API Credintals **************************
DATA: gv_url     TYPE string,  "VALUE 'https://einvoicing.internal.cleartax.co/v2/eInvoice/cancel',
      gv_gstin   TYPE string ,  "VALUE '29AAFCD5862R000',
      gv_ownerid TYPE string, " VALUE '4abbf226-bd72-4f2a-a3c6-5bbf4d9b47f9',
      gv_auth    TYPE string. " VALUE '1.9a34270c-c285-47d1-a32b-310bc1accf76_7d0170344c8adc0557dc94ced6c96d110e83fbd6cf935e15407d08064b6446d1'.

  DATA : GV_EFUSERNAME TYPE STRING,
    GV_EFPASSWORD TYPE STRING,
    GV_EINVUSERNAME TYPE STRING,
    GV_EINVPASSWORD TYPE STRING.
