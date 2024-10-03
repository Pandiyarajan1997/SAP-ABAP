FUNCTION zcheck_active_backgrd_job.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(PROGRAM_NAME) TYPE  SYST_CPROG
*"  EXCEPTIONS
*"      PROGRAM_NAME_MISSING
*"      EXCESS_JOB
*"----------------------------------------------------------------------
*Created by: Samsudeen M
*Created On: 03.05.2023
*Purpose : To check the Active Background Job for Any Program
*Reference: Puratchiveeran & Ramakrishnan
*-----------------------------------------------------------------------

  IF program_name IS INITIAL.
    RAISE program_name_missing. "If Program name itself missing
  ELSE.
    SELECT * FROM tbtcp INTO TABLE @DATA(lt_backjobs)
                        WHERE progname = @program_name
                        AND status = 'R'.
    IF sy-subrc = 0.
      DESCRIBE TABLE lt_backjobs LINES DATA(lv_lines).
      IF lv_lines GT 1.
        "More than One Active Job
        RAISE excess_job.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
