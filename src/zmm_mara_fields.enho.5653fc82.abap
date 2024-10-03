"Name: \PR:SAPL1001UEB\EX:BAPI_MATERIAL_SAVEDATA_25\EI
ENHANCEMENT 0 ZMM_MARA_FIELDS.
* IS2ERP: Attention: In the following part there are many nested modifications
*         belonging to MGV_MATNR_LAMA, /SAPMP/MATMA_DRUM and /NFM/MAIN. Because there
*         is currently no technical solution to handle this, I decided to assign the whole
*         part to switch DIMP_GENERAL. (D022867)
*  DATA: length_matnr_short TYPE i.

  TRANSLATE headdata-material_long TO UPPER CASE.        "#EC TRANSLANG
  CLEAR bapi_error.

  IF headdata-material_long = space.
    CLEAR tmerrdat.
    sy-msgid = message_id_m3.
    sy-msgty = message_error.
    sy-msgno = '262'.
    bapi_error = 'X'.
  ENDIF.

  PERFORM pstat_fuellen USING    headdata
                        CHANGING h_pstat.

  PERFORM mara_uebergeben USING headdata-material_long
                                headdata-ind_sector
                                headdata-matl_type
                                clientdata
                                clientdatax
                                h_pstat.
* set CWM specific MARA data
  PERFORM /cwm/mara_uebergeben USING headdata-material_long
                                     headdata-ind_sector
                                     headdata-matl_type
                                     clientdatacwm
                                     clientdatacwmx
                                     h_pstat.


  PERFORM makt_uebergeben TABLES materialdescription
                          USING  headdata-material_long.

  PERFORM marc_uebergeben USING headdata-material_long
                                plantdata
                                plantdatax.

* begin of IS2ERP --- /SAPMP/MATMA_DRUM
* IS-MP - C5007732
* SPAU FOR DIMP: if interface of this function module has changed,
*                adapt
*                INTERFACE and  CODE of function module
*                '/SAPMP/BAPI_MATERIAL_SAVEDATA'
*                as well as
*                PROGRAM and PARAMETERS of method 'SaveData'
*                of business objecttype 'IsMpStandardMaterial'
*                (/SAPMP/STM) in business object repository
* set IS-MP client data
**Begin of Modification 02-10-2024
*************************************ADDED BY ZAKIR FOR MAPPING MANDATORY MARA FIELDS*********************************************
*Implemented Enhancement Section
"Adding Mandatory fields what are maintained in MM01 and in future too. Make use of this space to populate
"the required fields which is not available directly in clientdata.
*NOTE: To pass the fields to tmara_ueb, Must Append BAPI_MARA structure with the required fields  and
" assign  it will reflect in clientdata structure and further computation this would carry on to tmara_ueb.

tmara_ueb-brgew               = clientdata-zzgross_wt.
tmara_ueb-volum               = clientdata-zzvolume.
tmara_ueb-voleh               = clientdata-zzvoleh.
tmara_ueb-ecom_category       = clientdata-zzecom_category.
tmara_ueb-ecom_mat_name       = clientdata-zzecom_mat_name.
tmara_ueb-ecom_name           = clientdata-zzecom_name.
tmara_ueb-reference_material  = clientdata-zzref_material.
tmara_ueb-zterm               = clientdata-zzterm.

ASSIGN tmara_ueb[ 1 ] TO FIELD-SYMBOL(<fs_mara>).
IF <fs_mara> IS ASSIGNED.
  <fs_mara>-brgew               = clientdata-zzgross_wt.
  <fs_mara>-volum               = clientdata-zzvolume.
  <fs_mara>-voleh               = clientdata-zzvoleh.
  <fs_mara>-ecom_category       = clientdata-zzecom_category.
  <fs_mara>-ecom_mat_name       = clientdata-zzecom_mat_name.
  <fs_mara>-ecom_name           = clientdata-zzecom_name.
  <fs_mara>-reference_material  = clientdata-zzref_material.
  <fs_mara>-zterm               = clientdata-zzterm.
 ENDIF.
  CALL FUNCTION '/SAPMP/1001UEB_SET_CLD_PLD'
    CHANGING
      ch_mara_ueb   = tmara_ueb
      ch_mara_ueb_t = tmara_ueb[]
      ch_mfieldres  = tmfieldres[]
      ch_bapi_error = bapi_error.
* end of IS2ERP --- /SAPMP/MATMA_DRUM
* begin of IS2ERP --- /NFM/MAIN
* /NFM/ processing - C5007732
* transfer structural weights and charge weights on plant level
  CALL FUNCTION '/NFM/1001UEB_TRANSFER_PLANT'
    EXPORTING
      im_material          = headdata-material_long
      im_transcount        = init_tranc
    TABLES
      t_structuralweights  = nfmstructuralweights
      t_structuralweightsx = nfmstructuralweightsx
      t_chargeweights      = nfmchargeweights
      t_chargeweightsx     = nfmchargeweightsx
      t_mfieldres          = tmfieldres
    CHANGING
      ch_delind            = d_ind
      ch_bapi_error        = bapi_error.
* end of IS2ERP --- /NFM/MAIN

  PERFORM mpop_uebergeben USING headdata-material_long
                                forecastparameters
                                forecastparametersx.

  PERFORM mpgd_uebergeben USING headdata-material_long
                                planningdata
                                planningdatax.

  PERFORM mard_uebergeben USING headdata-material_long
                                storagelocationdata
                                storagelocationdatax.

  PERFORM marm_uebergeben TABLES unitsofmeasure
                                 unitsofmeasurex
                          USING  headdata-material_long.

  PERFORM mean_uebergeben TABLES internationalartnos
                          USING  headdata-material_long.

  PERFORM mlan_uebergeben TABLES taxclassifications
                          USING  headdata-material_long
                                 salesdata                "note 2240523
                                 salesdatax               "note 2240523
                                 plantdata                "note 2240523
                                 plantdatax.              "note 2240523

  PERFORM mbew_uebergeben USING headdata-material_long
                                valuationdata
                                valuationdatax.

  PERFORM mlgn_uebergeben USING headdata-material_long
                                warehousenumberdata
                                warehousenumberdatax.

  PERFORM mlgt_uebergeben USING headdata-material_long
                                storagetypedata
                                storagetypedatax.

  PERFORM mvke_uebergeben USING headdata-material_long
                                salesdata
                                salesdatax.

* begin of IS2ERP --- /NFM/MAIN
* /NFM/ processing
* transfer charge weights on sales area level
  IF bapi_error IS INITIAL.
    CALL FUNCTION '/NFM/1001UEB_TRANSFER_SAREA'
      EXPORTING
        im_material      = headdata-material_long
        im_transcount    = init_tranc
      TABLES
        t_chargeweights  = nfmchargeweights
        t_chargeweightsx = nfmchargeweightsx
        t_mfieldres      = tmfieldres
      CHANGING
        ch_delind        = d_ind.
  ENDIF.
* end of IS2ERP --- /NFM/MAIN

  PERFORM mltx_uebergeben TABLES materiallongtext
                          USING  headdata-material_long
                                 ls_isc_mltx_uebergeben.  "IS2ERP

  PERFORM mfhm_uebergeben TABLES prtdata
                                 prtdatax
                          USING  headdata-material_long.
  headdata-material_external  = ls_isc_mltx_uebergeben-pi_material_external.

  PERFORM extensionin_uebergeben TABLES extensionin
                                        extensioninx
                                 USING  headdata-material_long
                                        ls_isc_ext_uebergeben "IS2ERP
                                        headdata-ind_sector
                                        headdata-matl_type
                                        h_pstat.

  IF bapi_error IS INITIAL.

* note 602267
    IF NOT flag_cad_call IS INITIAL.
      call_mode2 = call_mode2_cad.
    ELSE.
      CLEAR call_mode2.
    ENDIF.

    CALL FUNCTION 'MATERIAL_MAINTAIN_DARK'
      EXPORTING
        flag_muss_pruefen   = 'X'
        sperrmodus          = 'E'
        max_errors          = 0
        p_kz_no_warn        = 'N'
        kz_prf              = headdata-inp_fld_check
        kz_verw             = 'X'
        kz_aend             = 'X'
        kz_dispo            = 'X'
*       KZ_TEST             = ' '
*       NO_DATABASE_UPDATE  = ' '
        call_mode           = call_mode_bapi
        call_mode2          = call_mode2
*       USER                = SY-UNAME
*       SUPPRESS_ARRAY_READ = ' '
        flg_mass            = flag_online
        iv_change_doc_tcode = '(BAPI)'             "note 1050255&1682898
*    IMPORTING
*       MATNR_LAST          =
*       NUMBER_ERRORS_TRANSACTION =
      TABLES
        amara_ueb           = tmara_ueb
        amakt_ueb           = tmakt_ueb
        amarc_ueb           = tmarc_ueb
        amard_ueb           = tmard_ueb
        amfhm_ueb           = tmfhm_ueb
        amarm_ueb           = tmarm_ueb
        amea1_ueb           = tmean_ueb
        ambew_ueb           = tmbew_ueb
        asteu_ueb           = tsteu_ueb
        astmm_ueb           = tstmm_ueb
        amlgn_ueb           = tmlgn_ueb
        amlgt_ueb           = tmlgt_ueb
        ampgd_ueb           = tmpgd_ueb
        ampop_ueb           = tmpop_ueb
*       AMVEG_UEB           =
*       AMVEU_UEB           =
        amvke_ueb           = tmvke_ueb
        altx1_ueb           = tmltx_ueb
*       AMPRW_UEB           =
        amfieldres          = tmfieldres
        amerrdat            = tmerrdat
      EXCEPTIONS
        kstatus_empty       = 1
        tkstatus_empty      = 2
        t130m_error         = 3
        internal_error      = 4
        too_many_errors     = 5
        update_error        = 6
        error_message       = 8
        OTHERS              = 7.
  ELSE.
    CLEAR sy-subrc.
  ENDIF.
ENDENHANCEMENT.
