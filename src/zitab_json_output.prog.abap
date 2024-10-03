*&---------------------------------------------------------------------*
*& Report zitab_json_output
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zitab_json_output.

TYPES:BEGIN OF ty_amt,
        value          TYPE string,
        formattedValue TYPE string,
        currency       TYPE string,
      END OF ty_amt,
      BEGIN OF ty_input,
        paymentId             TYPE string,
        merchantPaymentRefId  TYPE string,
        amount                TYPE ty_amt,
        paymentDate           TYPE string,
        redirectUrl           TYPE string,
        redirectConfirmUrl    TYPE string,
        redirectCancelUrl     TYPE string,
        callbackUrl           TYPE string,
        paymenturl            TYPE string,
        status                TYPE string,
        autoCapture           TYPE string,
        uncapturedAmount      TYPE ty_amt,
        accountId             TYPE string,
        merchantCustomerRefId TYPE string,
      END OF ty_input.

DATA:gt_rf_cust TYPE STANDARD TABLE OF ty_input,
     gs_rf_cust LIKE LINE OF gt_rf_cust,
     wa_cust type  zsd_sf_cust_inv.

data: lv_str1 type string VALUE `"False"`,
      lv_str2 TYPE string VALUE 'False'.
SELECT FROM zsd_sf_cust_inv
FIELDS *
WHERE status = '14'
INTO TABLE @DATA(gt_data).
if sy-subrc ne 0.
WRITE: / 'No Data Found'.
Else.
loop at gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

<fs_data>-response_status = 'AUTH_APPROVED'.
*wa_cust-response_status = 'AUTH_APPROVED'.
modify zsd_sf_cust_inv from <fs_data>.
ENDLOOP.

gt_rf_cust = VALUE #( FOR ls_data IN gt_data (  paymentId             =   ls_data-payment_refid
                                                merchantPaymentRefId  =   ls_data-invoicekey
                                                amount-value                =   ls_data-invoiceamount
                                                amount-currency   = 'INR'
                                                amount-formattedvalue =  |₹{ CONV netwr( ls_data-invoiceamount ) }|
                                                paymentDate           =   ls_data-payment_date
                                                redirecturl =  ' '
                                                redirectconfirmurl = ' '
                                                redirectcancelurl = ' '
                                                callbackurl = ' '
                                                paymenturl = ls_data-payment_url
                                                status                =   ls_data-response_status
                                                 autoCapture = 'False'
                                                uncapturedamount-value                =   ls_data-invoiceamount
                                                uncapturedamount-currency   = 'INR'
                                                uncapturedamount-formattedvalue =  |₹{ CONV netwr( ls_data-invoiceamount ) }|
                                                accountId             =   ls_data-accountid
                                                merchantCustomerRefId =   ls_data-custno

                                                ) ).


DATA(lv_json) = /ui2/cl_json=>serialize(
  data        = gt_rf_cust
  compress    = abap_true
  pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

*REPLACE lv_str1 with lv_str2 into lv_json.
" Display JSON in ABAP
CALL TRANSFORMATION sjson2html SOURCE XML lv_json
                           RESULT XML DATA(lvc_html).

cl_abap_browser=>show_html(
  title = 'Sample JSON'
  html_string = cl_abap_codepage=>convert_from( lvc_html ) ).
  ENDIF.
