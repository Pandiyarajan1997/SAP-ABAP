*&---------------------------------------------------------------------*
*&  Include           LZJSONF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELLER_DTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EI_IN  text
*----------------------------------------------------------------------*
FORM seller_dtls  USING    p_ei_in TYPE ZST_IRN_HDR.
  CONCATENATE  '"SellerDtls": {'  c_gst c_quatation_mark p_ei_in-gstin c_quatation_mark c_comma
                          c_lglnm c_quatation_mark p_ei_in-name1  c_quatation_mark c_comma
                          c_trdnm c_null c_comma
                          c_addr1 c_quatation_mark p_ei_in-street  c_quatation_mark c_comma
                          c_addr2 c_null c_comma
                          c_loc   c_quatation_mark p_ei_in-location  c_quatation_mark  c_comma
                          c_pin   p_ei_in-post_code1 c_comma
                          c_stcd  c_quatation_mark  p_ei_in-region  c_quatation_mark  c_comma
                          c_ph    '"9445864640"'       c_comma       "Phone no.
                          c_em    '"spl_gst@sheenlac.in"'             "Email id
                          c_curly_brackets_close c_comma INTO lv_selldtls.

ENDFORM.                    " SELLER_DTLS
*&---------------------------------------------------------------------*
*&      Form  BUYER_DTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EI_IN  text
*----------------------------------------------------------------------*
FORM buyer_dtls  USING    p_ei_in TYPE ZST_IRN_HDR.
  CONCATENATE '"BuyerDtls": {'  c_gst c_quatation_mark p_ei_in-stcd3 c_quatation_mark c_comma
                          c_lglnm c_quatation_mark p_ei_in-buy_name1     c_quatation_mark c_comma
                          c_trdnm c_null c_comma
                          c_pos   '"' p_ei_in-pos '"' c_comma                                           ""Invalid Min 1 max 2 number
                          c_addr1 c_quatation_mark p_ei_in-stras         c_quatation_mark c_comma
                          c_addr2 c_null c_comma
                          c_loc   c_quatation_mark p_ei_in-ort01         c_quatation_mark  c_comma
                          c_pin   p_ei_in-pstlz  c_comma
                          c_stcd  c_quatation_mark p_ei_in-regio        c_quatation_mark  c_comma
                          c_ph    P_EI_IN-TEL_NO      c_comma             "Phone no.
                          c_em    P_EI_IN-BUY_MAIL                        "Email id
                          c_curly_brackets_close c_comma INTO lv_buydtls.

ENDFORM.                    " BUYER_DTLS
*&---------------------------------------------------------------------*
*&      Form  DISPATCH_DTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EI_IN  text
*----------------------------------------------------------------------*
FORM dispatch_dtls  USING    p_ei_in TYPE ZST_IRN_HDR.
  CONCATENATE '"DispDtls": {'
                          c_nm    c_quatation_mark p_ei_in-dis_name1  c_quatation_mark c_comma
                          c_addr1 c_quatation_mark p_ei_in-dis_add1   c_quatation_mark c_comma
                          c_addr2 c_null c_comma
                          c_loc   c_quatation_mark p_ei_in-dis_loc    c_quatation_mark  c_comma
                          c_pin   p_ei_in-dis_postcode c_comma
                          c_stcd  c_quatation_mark p_ei_in-dis_region c_quatation_mark
                          c_curly_brackets_close c_comma INTO lv_dispdtls.

ENDFORM.                    " DISPATCH_DTLS
*&---------------------------------------------------------------------*
*&      Form  SHIPDTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EI_IN  text
*----------------------------------------------------------------------*
FORM shipdtls  USING    p_ei_in type ZST_IRN_HDR.

   CONCATENATE '"ShipDtls": {'
                          c_gst   c_quatation_mark p_ei_in-sh_gstin    c_quatation_mark c_comma
                          c_lglnm c_quatation_mark p_ei_in-sh_name1    c_quatation_mark c_comma
                          c_trdnm c_null c_comma
                          c_addr1 c_quatation_mark p_ei_in-sh_add1     c_quatation_mark c_comma
                          c_addr2 c_null c_comma
                          c_loc   c_quatation_mark p_ei_in-sh_loc      c_quatation_mark  c_comma
                          c_pin   p_ei_in-sh_postcode  c_comma
                          c_stcd  c_quatation_mark p_ei_in-sh_region    c_quatation_mark
                          c_curly_brackets_close c_comma INTO lv_shipdtls.


ENDFORM.                    " SHIPDTLS
*&---------------------------------------------------------------------*
*&      Form  VALDTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EI_IN  text
*----------------------------------------------------------------------*
FORM valdtls  USING    p_ei_in TYPE ZST_IRN_HDR.

*******Val details***********
    CONCATENATE '"ValDtls":{' '"AssVal":'   v_assval  c_comma
                             '"CgstVal":'   v_cgst c_comma
                             '"SgstVal":'   v_sgst c_comma
                             '"IgstVal":'   v_igst c_comma
                             '"CesVal":'    c_zero c_comma
                             '"StCesVal":'  c_zero c_comma
                             '"Discount":'  c_zero c_comma
                             '"OthChrg":'   c_zero c_comma
                             '"RndOffAmt":' c_zero c_comma
                             '"TotInvVal":'  v_amtval c_comma
                             '"TotInvValFc":' c_zero
                              c_curly_brackets_close c_comma
                              INTO lv_valdtls.


ENDFORM.                    " VALDTLS
*&---------------------------------------------------------------------*
*&      Form  PAYDTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EI_IN  text
*----------------------------------------------------------------------*
FORM paydtls  USING    p_ei_in TYPE ZST_IRN_HDR .
 CONCATENATE '"PayDtls": {'
     '"Nm":'       c_null c_comma
     '"AccDet":'   c_null c_comma
     '"Mode":'     c_null c_comma
     '"FininsBr":' c_null c_comma
     '"PayTerm":'  c_zero c_comma
     '"PayInstr":' c_null c_comma
     '"CrTrn":'    c_null c_comma
     '"DirDr":'    c_null c_comma
     '"CrDay":'    c_zero c_comma
     '"PaidAmt":'  c_zero c_comma
     '"PaymtDue":' c_zero
     '},'  INTO lv_paydtls.
ENDFORM.                    " PAYDTLS
*&---------------------------------------------------------------------*
*&      Form  REFDTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EI_IN  text
*----------------------------------------------------------------------*
FORM refdtls  USING    p_ei_in TYPE ZST_IRN_HDR.
   CONCATENATE   '"RefDtls":{'
        '"InvRm":' c_null c_comma
        '"DocPerdDtls": {'
        '"InvStDt":'  c_quatation_mark  p_ei_in-inv_st    c_quatation_mark c_comma
        '"InvEndDt":' c_quatation_mark  p_ei_in-inv_end   c_quatation_mark
        c_curly_brackets_close c_comma


        '"PrecDocDtls": [{'
            '"InvNo":' c_quatation_mark  gv_ref   c_quatation_mark c_comma            "Reference Doc no
            '"InvDt":'  c_quatation_mark  p_ei_in-inv_dt  c_quatation_mark c_comma
            '"OthRefNo":'    c_zero
         ' }],'

        '"ContrDtls": [{'
            '"RecAdvRefr":' c_null c_comma
            '"RecAdvDt":'   '"' lv_date '"' c_comma
            '"Tendrefr":'   c_null c_comma
            '"Contrrefr":'  c_null c_comma
            '"Extrefr":'    c_null c_comma
            '"Projrefr":'   c_null c_comma
            '"Porefr":'     c_null c_comma
            '"PoRefDt":'    c_null   "'"09/09/2020"' "
         '}]'

*         '},'        INTO lv_refdtls.
         '}'        INTO lv_refdtls.

ENDFORM.                    " REFDTLS
*&---------------------------------------------------------------------*
*&      Form  EWBDTLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EI_IN  text
*----------------------------------------------------------------------*
FORM ewbdtls  USING    p_ei_in TYPE ZST_IRN_HDR.

   CONCATENATE ',"EwbDtls": {'

                        '"TransId":'    p_ei_in-transid c_comma
                        '"TransName":'  p_ei_in-transname c_comma
                        '"Distance":'   p_ei_in-distance c_comma
                        '"TransDocNo":' c_null  c_comma
                        '"TransDocDt":' c_null  c_comma
                        '"VehNo":'      p_ei_in-vehno c_comma
                        '"VehType":'    p_ei_in-vehtyp c_comma
                        '"TransMode":'  p_ei_in-transmode       "'"1"'
                        c_curly_brackets_close INTO lv_ewbdtls.

ENDFORM.                    " EWBDTLS
