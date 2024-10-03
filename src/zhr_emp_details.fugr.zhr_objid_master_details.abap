FUNCTION zhr_objid_master_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OTYPE) TYPE  HRP1000-OTYPE OPTIONAL
*"  TABLES
*"      IT_OBJECTS STRUCTURE  ZHRP1000_ST
*"      IT_OBJID TYPE  /BCV/T_AUT_HROBJID_RANGE OPTIONAL
*"----------------------------------------------------------------------
*&Created By: Samsudeen M
*&Created On: 16.08.2023
*&Purpose: All Masters related to HR like Organization Unit,
*          Position, Job,
*Reference: Ramakrishnan J
*------------------------------------------------------------------------
  DATA: lr_otype TYPE RANGE OF hrp1000-otype.

  REFRESH: lr_otype.
  IF otype IS NOT INITIAL.
    lr_otype = VALUE #( ( sign   = 'I' option = 'EQ' low    = otype ) ).
  ELSE.
    lr_otype = VALUE #( ( sign   = 'I' option = 'EQ' low    = 'O' )
                        ( sign   = 'I' option = 'EQ' low    = 'C' )
                        ( sign   = 'I' option = 'EQ' low    = 'S' ) ).
  ENDIF.
* Organization Unit ID, Job ID, Position ID Masters
  SELECT * FROM hrp1000 INTO TABLE @DATA(lt_hrp1000_o)
    WHERE plvar = '01'
    AND otype IN @lr_otype
    AND objid IN @it_objid
    AND begda LE @sy-datum
    AND endda GE @sy-datum
    AND langu = @sy-langu.
  IF sy-subrc EQ 0.
    SORT lt_hrp1000_o[] BY otype objid ASCENDING.
  ENDIF.

  it_objects[] = CORRESPONDING #( lt_hrp1000_o MAPPING otype = otype
                                                       objid = objid
                                                       begda = begda
                                                       endda = endda
                                                       short = short
                                                       stext = stext ).

  DELETE it_objects[] WHERE objid BETWEEN '50000000' AND '59999999'.

ENDFUNCTION.
