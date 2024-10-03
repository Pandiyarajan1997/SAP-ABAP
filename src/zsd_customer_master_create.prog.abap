*&---------------------------------------------------------------------*
*& Report ZSD_CUSTOMER_MASTER_CREATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_customer_master_create.
**** Data Declarations ***
INCLUDE zsd_customer_master_top.
*** Selection screen Design ****
INCLUDE zsd_customer_master_sel.
**** Subroutine for Program ***
INCLUDE zsd_customer_master_forms.

INCLUDE zsd_customer_master_pbo.

INCLUDE zsd_customer_master_pai.

*Added by Samsudeen M on 16.12.2023
AT SELECTION-SCREEN OUTPUT.
  "Adjusting Selection Screen Fields based on Radio Button
  PERFORM modify_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_flname.
  PERFORM value_request USING p_flname.

INITIALIZATION.
  refresh gt_pa0000.
  SELECT pernr stat2 FROM pa0000 INTO TABLE gt_pa0000 WHERE begda <= sy-datum and endda >= sy-datum.

START-OF-SELECTION.

**** Initial Validations ***
  IF r6 NE abap_true AND p_flname IS INITIAL.
    MESSAGE 'Give file to Upload' TYPE 'S'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF r5 = abap_true. " All process in one step
*** Excel conversion Initial Steps ***
    PERFORM upload_xl1.
*** Actual Business Partner Crestion Starts ***
    PERFORM f_create_bp_all.
**** Alv Display for Logs ****
    PERFORM f_display_alv.

  ELSEIF r1 = abap_true. "Create only business partner
*** Excel Conversion for Creation ***
    PERFORM upload_xl2.
*** Process for BP Creation ***
    PERFORM f_create_businesspartner.

  ELSEIF r3 = abap_true.
*****Sales Area Extesion *********
*** Excel conversion Initial Steps ***
    PERFORM upload_xl1.
*** Actual Process for Sales Area extension ****
    IF r3 = abap_true.
      DATA(lv_r2) = 'X'.
      EXPORT lv_r2 TO MEMORY ID 'EXTN'.
    ENDIF.

    PERFORM f_sales_area_extn.
**** Alv Display for Logs ****
    PERFORM f_display_alv.
    CLEAR lv_r2.
    "Company Code Extension
  ELSEIF r2 = abap_true.
    "Company Code Extension
    PERFORM f_compcode_extend.
    "Alv Display
    PERFORM f_display_ccalv.

*Added By samsudeen On 16.12.2023 for DMS Customer Extension
  ELSEIF r6 = abap_true.
    " Data Fetching For DMS Extension
    PERFORM sub_dealer_data_fetch.
    " Data Manipulation
    PERFORM data_manipulation.
    IF gt_dms_extn[] IS NOT INITIAL.
      CALL SCREEN 0100.
    ENDIF.
  ENDIF.
