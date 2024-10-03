*&---------------------------------------------------------------------*
*& Include          ZFI_SUNDARAM_REVFILE_CLS
*&---------------------------------------------------------------------*

**********************************************************
CLASS lcl_sf_rev_file DEFINITION.
  PUBLIC SECTION.
    METHODS: get_inv_revfile IMPORTING ftype TYPE c,
      read_process_invrev IMPORTING sepr TYPE c,
      read_process_canrev IMPORTING sepr TYPE c,
      alv_display IMPORTING ftype TYPE c.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_csv_file,
             invoiceno         TYPE string,
             partycode         TYPE string,
             status            TYPE string,
             comments          TYPE string,
             revfile_name      TYPE /sapdmc/mg_filname,
             revfile_recd_date TYPE erdat,
             revfile_recd_time TYPE erzzt,
           END OF ty_csv_file.

    DATA: lv_dir       TYPE eps2filnam,
          it_file_list TYPE TABLE OF eps2fili,
          wa_file_list TYPE eps2fili,
          file_count   TYPE i.

    DATA: gt_inv_table  TYPE TABLE OF ty_csv_file,
          gt_canc_table TYPE TABLE OF ty_csv_file,
          gt_alv_table  TYPE TABLE OF ty_csv_file.

    DATA: gt_ZSD_SF_CUST_INV TYPE TABLE OF zsd_sf_cust_inv.

    DATA: wa_csv_file TYPE ty_csv_file.
ENDCLASS.

CLASS lcl_sf_rev_file IMPLEMENTATION.

  METHOD get_inv_revfile.

    DATA lv_command TYPE sxpgcolist-name.

    IF ftype = 'I'.
      lv_command = 'ZSF_INV_REV'. " to get the invoice reverse file
    ELSEIF ftype = 'C'.
      lv_command = 'ZSF_CAN_REV'. " to get the Cancel invoice reverse file
    ELSEIF ftype = 'J'.
      lv_command = 'ZSF_INV_COPY'. " to copy the invoice reverse file to log folder and delete from man folder
    ELSEIF ftype = 'D'.
      lv_command = 'ZSF_CAN_COPY'. " to copy the Cancel invoice reverse file to log folder and delete from main folder
    ENDIF.

    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = lv_command
*       ADDITIONAL_PARAMETERS         =
*       OPERATINGSYSTEM               = SY-OPSYS
*       TARGETSYSTEM                  = SY-HOST
*       DESTINATION                   =
*       STDOUT                        = 'X'
*       STDERR                        = 'X'
*       TERMINATIONWAIT               = 'X'
*       TRACE                         =
*       DIALOG                        =
* IMPORTING
*       STATUS                        =
*       EXITCODE                      =
* TABLES
*       EXEC_PROTOCOL                 =
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.
    IF sy-subrc <> 0.
      IF  sy-subrc = 1.
        WRITE : 'No Authorization to Access OS command'.
        EXIT.
      ELSE.
        WRITE : 'Connectivity to HSBC server failed [OS Command]'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD read_process_invrev.

    DATA: shared_path   TYPE rlgrap-filename,
          lV_FNAME_TEMP TYPE string,
          lv_record     TYPE string.

    DATA: wa_CUST_INV TYPE zsd_sf_cust_inv.

    CLEAR lv_dir.
    IF sy-sysid = 'DEV' OR sy-sysid = 'QAS'.
      lv_dir = '/sapmnt/Sundaram/Inv_rev/'.
    ELSE.
      lv_dir = '/sapmnt/PRD/Sundaram/Inv_rev/'.
    ENDIF.


    REFRESH : it_file_list.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = lv_dir
*       FILE_MASK              = ' '
*     IMPORTING
*       DIR_NAME               =
*       FILE_COUNTER           =
*       ERROR_COUNTER          =
      TABLES
        dir_list               = it_file_list
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

* select all records from invocie custom table where status is sent to finance
    SELECT *
      FROM zsd_sf_cust_inv
      INTO TABLE gt_ZSD_SF_CUST_INV
      WHERE status = '13'
        AND invstatus = 'INV'.


    REFRESH gt_inv_table.

    LOOP AT it_file_list INTO wa_file_list.

      CONCATENATE lv_dir wa_file_list-name INTO lV_FNAME_TEMP.
      CONDENSE lV_FNAME_TEMP.

*      WRITE:/ 'File ', lV_FNAME_TEMP, 'to Be Processed'.

      OPEN DATASET lV_FNAME_TEMP FOR INPUT IN TEXT MODE ENCODING DEFAULT.

      DO.

        READ DATASET lV_FNAME_TEMP INTO lv_record.

        IF sy-subrc = 0.
          IF lv_record+0(7) = 'Invoice'.
            CONTINUE.
          ENDIF.

          CLEAR: wa_csv_file.
          SPLIT lv_record AT sepr INTO wa_csv_file-invoiceno
                                          wa_csv_file-partycode
                                          wa_csv_file-status
                                          wa_csv_file-comments.

          wa_csv_file-revfile_name =  lV_FNAME_TEMP.
          wa_csv_file-revfile_recd_date = sy-datum.
          wa_csv_file-revfile_recd_time = sy-uzeit.

          APPEND wa_csv_file TO gt_inv_table.

        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDLOOP.

    LOOP AT gt_inv_table INTO wa_csv_file.

      CLEAR wa_cust_inv.
      READ TABLE gt_ZSD_SF_CUST_INV INTO wa_cust_inv WITH KEY invoicekey = wa_csv_file-invoiceno.
      IF sy-subrc = 0.

        IF wa_csv_file-status = 'Accepted'.
          wa_cust_inv-status = '14'.
        ELSE.
          wa_cust_inv-status = '15'.
        ENDIF.

        wa_cust_inv-revfile_remarks   = wa_csv_file-comments.
        wa_cust_inv-revfile_name      = wa_csv_file-revfile_name.
        wa_cust_inv-revfile_recd_date = wa_csv_file-revfile_recd_date.
        wa_cust_inv-revfile_recd_time = wa_csv_file-revfile_recd_time.

        MODIFY zsd_sf_cust_inv FROM wa_cust_inv.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD read_process_canrev.

    DATA: shared_path   TYPE rlgrap-filename,
          lV_FNAME_TEMP TYPE string,
          lv_record     TYPE string.

    DATA: wa_CUST_INV TYPE zsd_sf_cust_inv.

    CLEAR lv_dir.

    IF sy-sysid = 'DEV' OR sy-sysid = 'QAS'.
      lv_dir = '/sapmnt/Sundaram/Can_rev/'.
    ELSE.
      lv_dir = '/sapmnt/PRD/Sundaram/Can_rev/'.
    ENDIF.


    REFRESH : it_file_list.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = lv_dir
*       FILE_MASK              = ' '
*     IMPORTING
*       DIR_NAME               =
*       FILE_COUNTER           =
*       ERROR_COUNTER          =
      TABLES
        dir_list               = it_file_list
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


* select all records from invocie custom table where status is sent to finance
    SELECT *
      FROM zsd_sf_cust_inv
      INTO TABLE gt_ZSD_SF_CUST_INV
      WHERE status = '13'
        AND invstatus = 'CAN'.


    REFRESH gt_inv_table.

    LOOP AT it_file_list INTO wa_file_list.

      CONCATENATE lv_dir wa_file_list-name INTO lV_FNAME_TEMP.
      CONDENSE lV_FNAME_TEMP.

*      WRITE:/ 'File ', lV_FNAME_TEMP, 'to Be Processed'.

      OPEN DATASET lV_FNAME_TEMP FOR INPUT IN TEXT MODE ENCODING DEFAULT.

      DO.

        READ DATASET lV_FNAME_TEMP INTO lv_record.

        IF sy-subrc = 0.
          IF lv_record+0(7) = 'Invoice'.
            CONTINUE.
          ENDIF.
*p_septr
          CLEAR: wa_csv_file.
          SPLIT lv_record AT sepr INTO wa_csv_file-invoiceno
                                          wa_csv_file-partycode
                                          wa_csv_file-status
                                          wa_csv_file-comments.

          wa_csv_file-revfile_name =  lV_FNAME_TEMP.
          wa_csv_file-revfile_recd_date = sy-datum.
          wa_csv_file-revfile_recd_time = sy-uzeit.

          APPEND wa_csv_file TO gt_inv_table.

        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDLOOP.

    LOOP AT gt_inv_table INTO wa_csv_file.

      CLEAR wa_cust_inv.
      READ TABLE gt_ZSD_SF_CUST_INV INTO wa_cust_inv WITH KEY invoicekey = wa_csv_file-invoiceno.
      IF sy-subrc = 0.

        IF wa_csv_file-status = 'Accepted'.
          wa_cust_inv-status = '14'.
        ELSE.
          wa_cust_inv-status = '15'.
        ENDIF.

        wa_cust_inv-can_revfile_remarks   = wa_csv_file-comments.
        wa_cust_inv-can_revfile_name      = wa_csv_file-revfile_name.
        wa_cust_inv-can_revfile_recd_date = wa_csv_file-revfile_recd_date.
        wa_cust_inv-can_revfile_recd_time = wa_csv_file-revfile_recd_time.

        MODIFY zsd_sf_cust_inv FROM wa_cust_inv.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD alv_display.
    DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties

    REFRESH gt_alv_table.

*    IF ftype = 'I'.
    APPEND LINES OF gt_inv_table[] TO gt_alv_table[].
*    else.
*      APPEND LINES OF gt_canc_table[] TO gt_alv_table[].
*    ENDIF.


* create the alv object
    IF gt_alv_table[] IS NOT INITIAL.
      TRY.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = lo_gr_alv
            CHANGING
              t_table      = gt_alv_table.
        CATCH cx_salv_msg.
      ENDTRY.
      lo_gr_alv->display( ).
    ELSE.
      WRITE:/ 'No File to Read'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
