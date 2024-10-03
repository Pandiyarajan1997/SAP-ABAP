"Name: \TY:/SAPMP/CL_IM_CONF_GM\IN:IF_EX_WORKORDER_GOODSMVT\ME:BACKFLUSH\SE:BEGIN\EI
ENHANCEMENT 0 ZPENDING_ORDER_VALIDATION.
" Order Pending Validation Added by Nttdata2 @06082024

**Additional Validation to not allow Goods Movement after Teco is completed
*SELECT SINGLE aufnr,idat2 from caufv Into @DATA(ls_teco) WHERE aufnr = @I_ORDER_HEADER-aufnr.
*IF sy-subrc = 0.
*  IF ls_teco-idat2 is NOT INITIAL.
*    cl_demo_output=>begin_section( 'Process Order Teco Already completed' ).
*    cl_demo_output=>write_data( ls_teco ).
*    cl_demo_output=>end_section( ).
*
*    cl_demo_output=>display( ).
*    LEAVE PROGRAM.
*  ENDIF.
*ENDIF.


select rs~matnr,rs~charg from caufv as cf1
  inner join resb as rs on  cf1~rsnum = rs~rsnum
  INNER JOIN mara AS ma ON rs~matnr = ma~matnr
  where  cf1~aufnr = @I_ORDER_HEADER-aufnr AND  ( ma~mtart = 'HALB' OR ma~mtart = 'FERT' )  into table @data(IT_PENDING_MAT).
delete it_pending_mat where charg EQ ' '.

SELECT rs~aufnr as ORDERNO ,rs~matnr as MATERIAL,rs~charg as BATCH
FROM afpo AS rs
INNER JOIN caufv AS  ca ON rs~aufnr = ca~aufnr
INNER JOIN mara AS ma ON rs~matnr = ma~matnr
  INNER JOIN @IT_PENDING_MAT AS T1 ON RS~MATNR = T1~MATNR and rs~charg = t1~charg
WHERE ca~erdat > '20240721'  AND ( ma~mtart = 'HALB' OR ma~mtart = 'FERT' ) and ca~aufnr NE  @I_ORDER_HEADER-aufnr
AND ca~objnr NOT IN ( SELECT objnr FROM auak ) GROUP BY rs~aufnr, rs~matnr ,rs~charg INTO TABLE @DATA(it_pending_order1).

IF it_pending_order1 IS NOT INITIAL.


    cl_demo_output=>begin_section( 'Pending Orders for Settlement' ).
    cl_demo_output=>write_data( it_pending_order1 ).
    cl_demo_output=>end_section( ).

    cl_demo_output=>display( ).
    LEAVE PROGRAM.

ENDIF.

ENDENHANCEMENT.
