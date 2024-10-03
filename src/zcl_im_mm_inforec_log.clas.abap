class ZCL_IM_MM_INFOREC_LOG definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_INFOREC_SEND .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_INFOREC_LOG IMPLEMENTATION.


  METHOD if_ex_me_inforec_send~send_changes.
*--------------------------------------------------------------------------------------------
    "Created by: Samsudeen M
    "Created on: 04.02.2022
    "Purpose: If any changes or new creation of info record for Plant "1005"
    "         which is the Sheenlac Paints Unit V it is stord in Log table for
    "Copying the same info record Data to Tech Service Plant
    "Requirement : Purchase Team
    "Reference : Ramakrishnan J & Gopalraja
*---------------------------------------------------------------------------------------------
    DATA: lv_infnr TYPE infnr.
    IF sy-tcode = 'ME11' OR sy-tcode = 'ME12' OR sy-tcode = 'ME13'.
      DATA(lt_eina_new) = n_eina.
      DATA(lt_eina_old) = o_eine.
      DATA(lt_eine_new) = n_eine.
      DATA(lt_eine_old) = o_eine.
*---- If changes happens means it is stored in log table -------------------------------------------*
      IF lt_eina_new IS NOT INITIAL AND lt_eine_new IS NOT INITIAL.
        READ TABLE lt_eina_new ASSIGNING FIELD-SYMBOL(<fs_eina_new>) INDEX 1.
        IF sy-subrc = 0.
          READ TABLE lt_eine_new ASSIGNING FIELD-SYMBOL(<fs_eine_new>) WITH KEY infnr = <fs_eina_new>-infnr.
          IF sy-subrc = 0 AND <fs_eine_new>-werks = '1005'.
            DATA(ls_upd_log) = VALUE zmm_inforec_log( matnr = <fs_eina_new>-matnr
                                                      supplier = <fs_eina_new>-lifnr
                                                      werks = <fs_eine_new>-werks
                                                      erdat = sy-datum
                                                      price = <fs_eine_new>-effpr
                                                      prdat = <fs_eine_new>-prdat  ).
*---- Checking the Log table whether data is already in table or not -----------*
            SELECT SINGLE * FROM zmm_inforec_log INTO @DATA(ls_log) WHERE matnr =  @<fs_eina_new>-matnr
                                                                    AND  erdat = @sy-datum.
            IF sy-subrc NE 0.
              IF ls_upd_log-prdat GE sy-datum.
                INSERT zmm_inforec_log FROM ls_upd_log.
                IF sy-subrc = 0.

                ENDIF.
              ENDIF.
            ELSE.
              IF ls_upd_log-prdat GE sy-datum.
                MODIFY zmm_inforec_log FROM ls_upd_log.
                IF sy-subrc = 0.

                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
