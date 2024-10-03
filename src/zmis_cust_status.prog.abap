*&---------------------------------------------------------------------*
*& Report ZMIS_CUST_STATUS
*&---------------------------------------------------------------------*
*& Date Created - 29.03.2022
*& Created By - KPABAP (Shamsudeen)
*& Description - Program to get the customer (dealer/distributor
*&               based on Region and customer type.
*                This program allows the user to set customer status
*                as active/incative/duplicate. This data is accessed by
*                MIS Portal to get the customer status to display in the
*                Map
*  TR No    -    DEVK931523, DEVK931565
*&---------------------------------------------------------------------*
* Change Date   - 16.04.2022
* TR NO         - DEVK931589
* Changed By    - Ramkrishnan (701322)
* Reference     - Discussion with Malathi
* Description   - Add KATR6 (Custom Region/Zone and Customer no to
*                 selection screen and fetch data based on it
*&----------------------------------------------------------------------

REPORT zmis_cust_status.

TABLES: kna1.

TYPES: BEGIN OF lty_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
         regio TYPE regio,
         mcod3 TYPE mcdd3,
         land1 TYPE land1_gp,
         ktokd TYPE ktokd,
         loevm TYPE loevm_x,
         katr6 TYPE katr6,
       END OF lty_kna1,

       BEGIN OF lty_disp,
         kunnr    TYPE kunnr,
         name1    TYPE name1_gp,
         regio    TYPE regio,
         bezei    TYPE bezei20,
         katr6    TYPE katr6,
         vtext    TYPE vtext,
         mcod3    TYPE mcdd3,
         status   TYPE z_status,
         status_1 TYPE zupdate,
       END OF lty_disp,

       BEGIN OF lty_fields,
         fieldname TYPE fieldname,
         rollname  TYPE rollname,
         position  TYPE tabfdpos,
         datatype  TYPE datatype_d,
         leng      TYPE ddleng,
       END OF lty_fields.

DATA: lt_kna1     TYPE STANDARD TABLE OF lty_kna1,
      ls_kna1     TYPE lty_kna1,
      lt_t005u    TYPE STANDARD TABLE OF t005u,
      ls_t005u    TYPE t005u,
      lt_tvk6t    TYPE STANDARD TABLE OF tvk6t,
      ls_tvk6t    TYPE tvk6t,
      lt_cust     TYPE STANDARD TABLE OF zmis_cust_st,
      ls_cust     TYPE zmis_cust_st,
      lt_disp     TYPE STANDARD TABLE OF lty_disp,
      ls_disp     TYPE lty_disp,
      lt_fields   TYPE STANDARD TABLE OF lty_fields,
      ls_fields   TYPE lty_fields,
      lt_fcat     TYPE lvc_t_fcat,
      ls_fcat     TYPE lvc_s_fcat,
      lt_exclude  TYPE ui_functions,
      ls_excl     TYPE ui_func,
      it_dropdown TYPE lvc_t_drop,
      ty_dropdown TYPE lvc_s_drop,
      ls_layout   TYPE lvc_s_layo.

DATA: lv_container TYPE scrfname VALUE 'ALV_GRID_CONT'.

CLASS lcl_handle_events DEFINITION DEFERRED.

DATA: lo_grid      TYPE REF TO cl_gui_alv_grid,
      lo_events    TYPE REF TO lcl_handle_events,
      lo_container TYPE REF TO cl_gui_custom_container.


*Selection Screen
SELECT-OPTIONS: s_kunnr FOR kna1-kunnr,   "Customer No
                s_cust  FOR kna1-ktokd,   "Customer Type
                s_regio FOR kna1-regio,   "Region
                s_adlrg FOR kna1-katr6.   "Additional Region


*---------------------------------------------------------------------
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------
* Define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      data_changed  FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD data_changed.
    DATA: ls_row   TYPE lvc_s_modi,
          lv_index TYPE i.
    CLEAR:ls_disp, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'STATUS'.
    IF sy-subrc = 0 AND ls_row-value IS NOT INITIAL.
      READ  TABLE lt_disp INTO ls_disp INDEX ls_row-row_id.
      ls_disp-status = ls_row-value.
      IF  ls_disp-status EQ '01'.
        ls_disp-status_1 = 'Active'.
      ELSEIF ls_disp-status EQ '02'.
        ls_disp-status_1 = 'Inactive'.
      ELSEIF ls_disp-status EQ '03'.
        ls_disp-status_1 = 'Duplicate'.

      ELSEIF ( ls_disp-status NE '01' OR ls_disp-status NE '02' OR ls_disp-status NE '03' ).
*        ROLLBACK WORK.
        ls_disp-status = ' '.
        MODIFY lt_disp FROM ls_disp INDEX ls_row-row_id.
        lo_grid->refresh_table_display( ).
        MESSAGE 'ENTER VALID NO' TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.
      MODIFY lt_disp FROM ls_disp INDEX ls_row-row_id.
      lo_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  REFRESH: lt_kna1,lt_disp,lt_t005u.

  SELECT kunnr
         name1
         regio
         mcod3
         land1
         ktokd
         loevm
         katr6
    FROM kna1
    INTO TABLE lt_kna1
   WHERE kunnr IN s_kunnr
     AND ktokd IN s_cust
     AND regio IN s_regio
*     AND loevm NE 'X' "Commented by Samsudeen M on 28.04.2023
     AND katr6 IN s_adlrg.

  SORT lt_kna1 BY kunnr.
  SELECT * FROM t005u INTO TABLE lt_t005u FOR ALL ENTRIES IN lt_kna1
                                           WHERE spras EQ sy-langu AND
                                                land1 EQ lt_kna1-land1.

*get region from attribute 6 help table from KNA1 for field katr6
  SELECT * FROM tvk6t INTO TABLE lt_tvk6t WHERE spras = sy-langu.


  SELECT * FROM zmis_cust_st INTO TABLE lt_cust FOR ALL ENTRIES IN lt_kna1
                                        WHERE customer_no = lt_kna1-kunnr.



  LOOP AT lt_kna1 INTO ls_kna1.
    CLEAR ls_disp.
    ls_disp-kunnr = ls_kna1-kunnr.
    ls_disp-name1 = ls_kna1-name1.
    ls_disp-regio = ls_kna1-regio.
    ls_disp-katr6 = ls_kna1-katr6.
    ls_disp-mcod3 = ls_kna1-mcod3.

* get city based on the region
    CLEAR ls_t005u.
    READ TABLE lt_t005u INTO ls_t005u WITH KEY bland = ls_disp-regio.
    IF sy-subrc = 0.
      ls_disp-bezei = ls_t005u-bezei.
    ENDIF.

* get the custom region based on katr6
    READ TABLE lt_tvk6t INTO ls_tvk6t WITH KEY katr6 = ls_kna1-katr6.
    IF sy-subrc = 0.
      ls_disp-vtext = ls_tvk6t-vtext.
    ENDIF.

    CLEAR ls_cust.
    READ TABLE lt_cust INTO ls_cust WITH KEY customer_no = ls_kna1-kunnr.
    IF sy-subrc = 0.
      ls_disp-status = ls_cust-status.
    ENDIF.


    IF  ls_disp-status EQ '01'.
      ls_disp-status_1 = 'Active'.
    ELSEIF ls_disp-status EQ '02'.
      ls_disp-status_1 = 'Inactive'.
    ELSEIF ls_disp-status EQ '03'.
      ls_disp-status_1 = 'Duplicate'.
    ENDIF.

    APPEND ls_disp TO lt_disp.

  ENDLOOP.

END-OF-SELECTION.

  REFRESH lt_fcat.

  PERFORM f_fieldcat USING  'KUNNR' 'Customer No' 1 space 10 space space space.
  PERFORM f_fieldcat USING  'NAME1' 'Customer Nme' 2 space 35 space space space.
  PERFORM f_fieldcat USING  'REGIO' 'Region' 3 space 3 space space space.
  PERFORM f_fieldcat USING  'BEZEI' 'Description' 4 space 20 space space space.
  PERFORM f_fieldcat USING  'MCOD3' 'City' 5 space 35 space space space.
  PERFORM f_fieldcat USING  'KATR6' 'Custom Region' 6 space 3 space space space.
  PERFORM f_fieldcat USING  'VTEXT' 'Description' 7 space 20 space space space.
  PERFORM f_fieldcat USING  'STATUS' 'Status' 8 'X' 2 'X' 'zmis_cust_st' 'STATUS'.
  PERFORM f_fieldcat USING  'STATUS_1' 'status desc' 9 space 15 space space space.

*clear ls_fcat.
*ty_dropdown-handle = '1'.
*    ty_dropdown-value = '01'.
*    APPEND ty_dropdown TO it_dropdown.
*    ty_dropdown-handle = '1'.
*    ty_dropdown-value = '02'.
*    APPEND ty_dropdown TO it_dropdown.
*    ty_dropdown-handle = '1'.
*    ty_dropdown-value = '03'.
*    APPEND ty_dropdown TO it_dropdown.
*
* LOOP AT lt_fcat INTO ls_fcat.
*    CASE ls_fcat-fieldname.
*** To assign dropdown in the fieldcataogue
*      WHEN 'STATUS'.
*        ls_fcat-drdn_hndl = '1'.
*        ls_fcat-outputlen = 2.
*        MODIFY lt_fcat FROM ls_fcat.
*    ENDCASE.
*  ENDLOOP.


  CALL SCREEN 100.

FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4 f_var5 f_var6 f_var7 f_var8.
  ls_fcat-fieldname = f_var1.
  ls_fcat-coltext = f_var2.
  ls_fcat-col_pos = f_var3.
  ls_fcat-edit = f_var4.
  ls_fcat-outputlen = f_var5.
  ls_fcat-f4availabl = f_var6.
  ls_fcat-ref_table = f_var7.
  ls_fcat-ref_field = f_var8.
  APPEND ls_fcat TO lt_fcat.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF_STATUS'.
  SET TITLEBAR 'ZMIS'.

  IF lo_container IS INITIAL.
    CREATE OBJECT lo_container
      EXPORTING
        container_name = lv_container.
    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = lo_container.

    CREATE OBJECT lo_events.

    SET HANDLER: lo_events->data_changed FOR lo_grid.

    ls_layout-cwidth_opt = 'X'.
    ls_layout-col_opt = 'X'.
    ls_layout-zebra = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = ls_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = lt_disp
        it_fieldcatalog      = lt_Fcat.

** Register for EDIT Fields and Events
    CALL METHOD lo_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD lo_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      REFRESH lt_cust.
      LOOP AT lt_disp INTO ls_disp WHERE status IS NOT INITIAL.
        CLEAR: ls_cust.
        ls_cust-customer_no = ls_disp-kunnr.
        ls_cust-status = ls_disp-status.

        READ TABLE lt_cust INTO ls_cust WITH KEY customer_no = ls_disp-kunnr.
        IF sy-subrc = 0.
          IF ls_disp-status = ls_cust-status.
            CONTINUE.
          ELSE.
            ls_cust-changed_by =  sy-uname.
            ls_cust-changed_on = sy-datum.
          ENDIF.
        ELSE.
          ls_cust-created_by  = sy-uname.
          ls_cust-create_on = sy-datum.
          ls_cust-changed_by =  sy-uname.
          ls_cust-changed_on = sy-datum.
        ENDIF.

        APPEND ls_cust TO lt_cust.
      ENDLOOP.
      IF lt_cust[] IS NOT INITIAL.
        MODIFY zmis_cust_st FROM TABLE lt_cust.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          MESSAGE TEXT-001 TYPE 'S'.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDMODULE.
