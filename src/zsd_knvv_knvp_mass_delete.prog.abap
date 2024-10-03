*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 01.07.2024
*
*  Requester Name            : Ramakrishnan
*
*  Business Logic            : backup and delete the KNVV & KNVP for vkorg - 6000 & except 10
*
*  Released on Date          :
*=======================================================================
REPORT zsd_knvv_knvp_mass_delete.

DATA : lv_kunnr TYPE kna1-kunnr,
       lv_vkorg TYPE knvv-vkorg.
SELECT-OPTIONS : so_kunnr FOR lv_kunnr,
                 so_vkorg FOR lv_vkorg.

SELECTION-SCREEN SKIP 1.

PARAMETERS : r1 RADIOBUTTON GROUP g1 DEFAULT 'X',
             r2 RADIOBUTTON GROUP g1.

INITIALIZATION.
  so_vkorg-low = '6000'.
  APPEND so_vkorg.

START-OF-SELECTION.

  IF r1 = 'X'.
    SELECT * FROM knvv INTO TABLE @DATA(lt_knvv) WHERE kunnr IN @so_kunnr
                                                 AND   vkorg IN @so_vkorg
                                                 AND   vtweg EQ '20'
                                                 AND   spart NE '10'.
    IF sy-subrc = 0.
      DATA(lv_linesorg) = lines( lt_knvv ).
      LOOP AT lt_knvv INTO DATA(ls_knvv).

        MODIFY zknvv_backup FROM ls_knvv.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE : 'Error in KNVV Table Backup' TYPE 'E'.
        ENDIF.

      ENDLOOP.

****************check the original & copy data are same - KNVV************
      SELECT * FROM zknvv_backup INTO TABLE @DATA(lt_knvvcpy)
                                 WHERE kunnr IN @so_kunnr
                                 AND   vkorg IN @so_vkorg
                                 AND   vtweg EQ '20'
                                 AND   spart NE '10'.
      IF sy-subrc = 0.
        DATA(lv_linescpy) = lines( lt_knvvcpy ).

        IF lv_linesorg <> lv_linescpy.
          MESSAGE : 'Data mismatch in KNVV Table Backup' TYPE 'E'.
        ENDIF.

      ENDIF.
******************standard table deleting portion - KNVV**************
      LOOP AT lt_knvv INTO ls_knvv.

        DELETE knvv FROM ls_knvv.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE : 'Error in KNVV Table Delete' TYPE 'E'.
        ENDIF.

      ENDLOOP.
****************check KNVV data deleted or not************
      SELECT * FROM knvv INTO TABLE @lt_knvv
                                 WHERE kunnr IN @so_kunnr
                                 AND   vkorg IN @so_vkorg
                                 AND   vtweg EQ '20'
                                 AND   spart NE '10'.
      IF sy-subrc = 0.
        MESSAGE : 'Data not fully deleted - KNVV' TYPE 'E'.
      ENDIF.
      MESSAGE : 'Process Completed' TYPE 'S'.
    ELSE.
      MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ELSEIF r2 = 'X'.
    SELECT * FROM knvp INTO TABLE @DATA(lt_knvp)
                                 WHERE kunnr IN @so_kunnr
                                 AND   vkorg IN @so_vkorg
                                 AND   vtweg EQ '20'
                                 AND   spart NE '10'.
    IF sy-subrc = 0.
      CLEAR : lv_linesorg.
      lv_linesorg = lines( lt_knvp ).

      LOOP AT lt_knvp INTO DATA(ls_knvp).

        MODIFY zknvp_backup FROM ls_knvp.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE : 'Error in KNVV Table Backup' TYPE 'E'.
        ENDIF.

      ENDLOOP.
****************check the original & copy data are same - KNVP************
      SELECT * FROM zknvp_backup INTO TABLE @DATA(lt_knvpcpy)
                                 WHERE kunnr IN @so_kunnr
                                 AND   vkorg IN @so_vkorg
                                 AND   vtweg EQ '20'
                                 AND   spart NE '10'.
      IF sy-subrc = 0.
        CLEAR : lv_linescpy.
        lv_linescpy = lines( lt_knvvcpy ).

        IF lv_linesorg <> lv_linescpy.
          MESSAGE : 'Data mismatch in KNVP Table Backup' TYPE 'E'.
        ENDIF.
      ENDIF.
******************standard table deleting portion - KNVV**************
      LOOP AT lt_knvp INTO ls_knvp.

        DELETE knvp FROM ls_knvp.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE : 'Error in KNVP Table Delete' TYPE 'E'.
        ENDIF.

      ENDLOOP.
****************check KNVV data deleted or not************
      SELECT * FROM knvp INTO TABLE @lt_knvp
                                 WHERE kunnr IN @so_kunnr
                                 AND   vkorg IN @so_vkorg
                                 AND   vtweg EQ '20'
                                 AND   spart NE '10'.
      IF sy-subrc = 0.
        MESSAGE : 'Data not fully deleted - KNVP' TYPE 'E'.
      ENDIF.
      MESSAGE : 'Process Completed' TYPE 'S'.
    ELSE.
      MESSAGE : 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
