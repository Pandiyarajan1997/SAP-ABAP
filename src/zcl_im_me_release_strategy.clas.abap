class ZCL_IM_ME_RELEASE_STRATEGY definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_PURCHDOC_POSTED .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ME_RELEASE_STRATEGY IMPLEMENTATION.


METHOD if_ex_me_purchdoc_posted~posted.

  DATA: jobname           TYPE tbtcjob-jobname VALUE 'PO_',
        jobcount          TYPE tbtcjob-jobcount,
        ls_ekko           TYPE ekko,
        arc_params        TYPE arc_params,
        print_params      TYPE pri_params,
        valid             TYPE c,
        job_rel           TYPE btch0000-char1 VALUE 'X',
        time              TYPE tbtcjob-sdlstrttm VALUE '000015',
        time1             TYPE tbtcjob-sdlstrttm,
        v_plant           TYPE bwkey,
        gv_mrp            TYPE estkz,
        wa_ekpo           TYPE uekpo,
        starttime         TYPE tbtcstrt.

*  Plant 1401 with PR Status 'B' - PO Release trigger after 15 secnds
  CLEAR: gv_mrp,wa_ekpo,v_plant,jobname,jobcount.

  READ TABLE im_ekpo INTO wa_ekpo INDEX 1.     ""Purchase Document

  IF sy-subrc IS INITIAL.

    SELECT estkz UP TO 1 ROWS FROM eban INTO gv_mrp WHERE banfn = wa_ekpo-banfn   "Purchase Requisition
                                                AND estkz = 'B' ORDER BY PRIMARY KEY.              ""Indicator as 'B'
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

    IF gv_mrp IS NOT INITIAL AND im_ekko-statu EQ 'B'.       "Automatic PO

      IF sy-tcode EQ 'ME21N' OR sy-tcode EQ 'ME59N'
                  OR sy-tcode EQ 'SM37' OR  sy-batch EQ 'X' .

        SELECT SINGLE bwkey FROM t001k INTO v_plant WHERE bukrs = im_ekko-bukrs "Companycode
                                                      AND bwkey = '1401'.       "Plant

        IF v_plant IS NOT INITIAL.

          time1 = sy-uzeit + time.
          "Concatenating Jobname with PO No.
          CONCATENATE jobname im_ekko-ebeln INTO jobname.
          ls_ekko = im_ekko.


          CALL FUNCTION 'JOB_OPEN'
            EXPORTING
              jobname   = jobname
              sdlstrtdt = sy-datum
              sdlstrttm = time1
            IMPORTING
              jobcount  = jobcount.

          CALL FUNCTION 'GET_PRINT_PARAMETERS'
            EXPORTING
              destination            = 'LP01'
              no_dialog              = 'X'
            IMPORTING
              out_archive_parameters = arc_params
              out_parameters         = print_params
              valid                  = valid.

          IF sy-subrc IS INITIAL.

            SUBMIT zrelease_po
            WITH  p_po   = im_ekko-ebeln
            AND  RETURN
            USER  sy-uname
            VIA  JOB  jobname
            NUMBER jobcount
            TO SAP-SPOOL
            SPOOL     PARAMETERS  print_params
            ARCHIVE   PARAMETERS arc_params
            WITHOUT SPOOL DYNPRO.

          ENDIF.

          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              jobcount         = jobcount
              jobname          = jobname
              sdlstrtdt        = sy-datum
              sdlstrttm        = time1
              strtimmed        = ' '
*             dont_release     = 'X'
            IMPORTING
              job_was_released = job_rel.

        ENDIF.  "Plant
      ENDIF.      "Tcode
    ENDIF.          "PR Status as MRP and PO status as 'B'

  ENDIF.

ENDMETHOD.
ENDCLASS.
