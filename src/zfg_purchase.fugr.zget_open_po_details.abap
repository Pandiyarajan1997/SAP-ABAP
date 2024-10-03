FUNCTION zget_open_po_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IN_BSART) TYPE  /ACCGO/CAS_TT_BSART_RANGE OPTIONAL
*"     REFERENCE(IN_MTART) TYPE  /ACCGO/CMN_TT_MTART_RETAIL OPTIONAL
*"     REFERENCE(IN_PLANT) TYPE  /ACCGO/CAS_TT_WERKS_RANGE OPTIONAL
*"     REFERENCE(FLAG) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      T_OPEN_PO STRUCTURE  ZSTR_OPEN_PO
*"----------------------------------------------------------------------
  "&Created on: 14.12.2022
  "&created by: Samsudeen M
  "&Purpose: Selecting open Purchase order
  "& Reference: Ramakrishnan J , Gopalraja
*----------------------------------------------------------------------
  DATA: lr_bsart TYPE RANGE OF ekko-bsart,
        ls_bsart LIKE LINE OF lr_bsart.
  DATA: lr_werks TYPE RANGE OF ekpo-werks,
        ls_werks LIKE LINE OF lr_werks.
  DATA: lr_mtart TYPE RANGE OF ekpo-mtart,
        ls_mtart LIKE LINE OF lr_mtart.

  DATA: gt_open_po TYPE STANDARD TABLE OF zstr_open_po,
        wa_open_po TYPE zstr_open_po.

  IF in_bsart IS NOT INITIAL.
    LOOP AT in_bsart INTO DATA(is_bsart).
      MOVE-CORRESPONDING is_bsart TO ls_bsart.
      APPEND ls_bsart TO lr_bsart.
      CLEAR ls_bsart.
    ENDLOOP.
  ENDIF.

  IF in_plant IS NOT INITIAL.
    LOOP AT in_plant INTO DATA(is_werks).
      MOVE-CORRESPONDING is_werks TO ls_werks.
      APPEND ls_werks TO lr_werks.
      CLEAR ls_werks.
    ENDLOOP.
  ENDIF.

  IF in_mtart IS NOT INITIAL .
    LOOP AT in_mtart INTO DATA(is_mtart).
      MOVE-CORRESPONDING is_mtart TO ls_mtart.
      APPEND ls_mtart TO lr_mtart.
      CLEAR ls_mtart.
    ENDLOOP.
  ENDIF.
** selecting all po based on document type **
  SELECT ebeln, bsart, lifnr FROM ekko
                             INTO TABLE @DATA(lt_ekko)
                             WHERE bstyp = 'F'
                             AND bsart IN @lr_bsart
                             AND memorytype EQ ''.
  IF sy-subrc = 0.
    SORT lt_ekko[] BY ebeln lifnr.
*** Vendor Name fetching ***
    SELECT lifnr, name1 FROM lfa1
                        INTO TABLE @DATA(lt_lfa1)
                        FOR ALL ENTRIES IN @lt_ekko
                        WHERE lifnr EQ @lt_ekko-lifnr.
    IF sy-subrc = 0.
      SORT lt_lfa1[] BY lifnr.
    ENDIF.
**** Fetching all open Po details based on input ****
    SELECT ebeln,
           ebelp,
           txz01,
           matnr,
           bukrs,
           werks,
           menge,
           netpr,
           netwr,
           mtart,
           elikz FROM ekpo
                 INTO TABLE @DATA(lt_open_po)
                 FOR ALL ENTRIES IN @lt_ekko
                 WHERE ebeln EQ @lt_ekko-ebeln
                 AND loekz EQ ''
                 AND werks IN @lr_werks
                 AND mtart IN @lr_mtart
                 AND elikz NE 'X'.
    IF sy-subrc = 0.
      SORT lt_open_po[] BY ebeln ebelp matnr.
**** Selecting GR Quantity for calculation *****
      SELECT ebeln,
             ebelp,
             menge,
             wemng FROM eket
                   INTO TABLE @DATA(lt_eket)
                   FOR ALL ENTRIES IN @lt_open_po
                   WHERE ebeln EQ @lt_open_po-ebeln
                   AND ebelp EQ @lt_open_po-ebelp.
      IF sy-subrc = 0.
        SORT lt_eket[] BY ebeln ebelp.
      ENDIF.
    ENDIF.
  ENDIF.

  REFRESH gt_open_po.

  LOOP AT lt_open_po ASSIGNING FIELD-SYMBOL(<fls_open_po>).
    CLEAR wa_open_po.
    MOVE-CORRESPONDING <fls_open_po> TO wa_open_po.
    wa_open_po-lifnr = VALUE #( lt_ekko[ ebeln = <fls_open_po>-ebeln ]-lifnr OPTIONAL ).
    wa_open_po-name1 = VALUE #( lt_lfa1[ lifnr = wa_open_po-lifnr ]-name1 OPTIONAL ).
    DATA(lv_balnce) = VALUE #( lt_eket[ ebeln = <fls_open_po>-ebeln ebelp = <fls_open_po>-ebelp ]-wemng OPTIONAL ).
    wa_open_po-po_bal_quan = ( <fls_open_po>-menge - lv_balnce ).
    wa_open_po-po_bal_amount = (  wa_open_po-po_bal_quan * <fls_open_po>-netpr ).
    wa_open_po-gr_quantity = lv_balnce.
    APPEND wa_open_po TO gt_open_po.
  ENDLOOP.

  IF gt_open_po[] IS NOT INITIAL.
    SORT lt_open_po[] BY ebeln ebelp.
    IF flag = 'X'.
      DELETE gt_open_po[] WHERE po_bal_quan = '0'.
    ENDIF.
    APPEND LINES OF gt_open_po TO t_open_po.
  ENDIF.





ENDFUNCTION.
