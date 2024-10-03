CLASS zcl_amdp_cust_blk DEFINITION
  PUBLIC
  FINAL .

  PUBLIC SECTION.
    INTERFACES: if_amdp_marker_hdb.

    TYPES: BEGIN OF ty_kna1,
             kunnr     TYPE kunnr,
             name1     TYPE name1,
             werks     TYPE werks_d,
             stras     TYPE stras,
             ort01     TYPE ort01,
             pstlz     TYPE pstlz,
             Regio     TYPE regio,
             ktokd     TYPE ktokd,
             lifnr     TYPE lifnr,
             katr2     TYPE katr2,
             stcd3     TYPE stcd3,
             j_1ipanno TYPE j_1ipanno,
             adrnr     TYPE adrnr,
             telf1     TYPE telf1,
             telf2     TYPE telf2,
           END OF ty_kna1,
           BEGIN OF ty_adr6,
             addrnumber TYPE ad_addrnum,
             smtp_addr  TYPE ad_smtpadr,
           END OF ty_adr6,
           BEGIN OF ty_bp001,
             kunnr         TYPE kunnr,
             partner       TYPE bu_partner,
             group_feature TYPE bp_group_feature,
           END OF ty_bp001,
           BEGIN OF ty_knvv,
             kunnr TYPE kunnr,
             vkorg TYPE vkorg,
             kdgrp TYPE kdgrp,
             vkbur TYPE vkbur,
           END OF ty_knvv,
           BEGIN OF ty_pa0001,
             pernr TYPE persno,
             ename TYPE emnam,
           END OF ty_pa0001,
           BEGIN OF ty_hrp1000,
             objid TYPE hrobjid,
             stext TYPE stext,
           END OF ty_hrp1000,
           BEGIN OF ty_einvdist,
             distributor TYPE kunnr,
           END OF ty_einvdist,
           BEGIN OF ty_test,
             bukrs TYPE bukrs,
             vkorg TYPE vkorg,
             kunnr TYPE kunnr,
             block TYPE char1,
           END OF ty_test,

           tt_kna1         TYPE STANDARD TABLE OF ty_kna1 WITH EMPTY KEY,
           tt_tvk2t        TYPE STANDARD TABLE OF tvk2t WITH EMPTY KEY,
           tt_adr6         TYPE STANDARD TABLE OF ty_adr6 WITH EMPTY KEY,
           tt_bp001        TYPE STANDARD TABLE OF ty_bp001 WITH EMPTY KEY,
           tt_tp24t        TYPE STANDARD TABLE OF tp24t WITH EMPTY KEY,
           tt_knvv         TYPE STANDARD TABLE OF ty_knvv WITH EMPTY KEY,
           tt_t151t        TYPE STANDARD TABLE OF t151t WITH EMPTY KEY,
           tt_knvp         TYPE STANDARD TABLE OF knvp WITH EMPTY KEY,
           tt_pa0001       TYPE STANDARD TABLE OF ty_pa0001 WITH EMPTY KEY,
           tt_hrp1000      TYPE STANDARD TABLE OF ty_hrp1000 WITH EMPTY KEY,
           tt_einvdist     TYPE STANDARD TABLE OF ty_einvdist WITH EMPTY KEY,
           tt_cust_blk_chk TYPE STANDARD TABLE OF zcust_blk_chk WITH EMPTY KEY,
*          Lt_customer     TYPE STANDARD TABLE OF zcust_blk_chk WITH EMPTY KEY,
           tt_test         TYPE STANDARD TABLE OF ty_test WITH EMPTY KEY.

    CLASS-METHODS: Get_Cust_blk FOR TABLE FUNCTION zc_tab_func_cust_blk,
                   get_data
                       AMDP OPTIONS READ-ONLY
                         CDS SESSION CLIENT iv_client
                         IMPORTING
                                  VALUE(iv_client)       TYPE mandt
                                   VALUE(iv_bukrs)        TYPE bukrs
                                   VALUE(iv_vkorg)        TYPE vkorg
*                VALUE(lt_kna1)         TYPE tt_kna1
*                VALUE(lt_tvk2t)        TYPE tt_tvk2t
*                VALUE(lt_adr6)         TYPE tt_adr6
*                VALUE(et_bp001)        TYPE tt_bp001
*                VALUE(et_tp24t)        TYPE tt_tp24t
*                VALUE(et_knvv)         TYPE tt_knvv
*                VALUE(et_t151t)        TYPE tt_t151t
*                VALUE(et_knvp)         TYPE tt_knvp
*                VALUE(et_pa001)        TYPE tt_pa0001
*                VALUE(et_hrp1000)      TYPE tt_hrp1000
*                VALUE(et_einvdist)     TYPE tt_einvdist
*                VALUE(et_cust_blk_chk) TYPE tt_cust_blk_chk
                         EXPORTING VALUE(it_blk_data)     TYPE tt_cust_blk_chk .
  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_amdp_cust_blk IMPLEMENTATION.

  METHOD get_cust_blk BY DATABASE FUNCTION
                    FOR HDB LANGUAGE SQLSCRIPT
                    USING ZCL_AMDP_CUST_BLK=>GET_DATA.

       DECLARE gt_blk_data TABLE
                                    ( mandt   "$ABAP.type( mandt )",
                                     bukrs      "$ABAP.type( bukrs )",
                                     vkorg      "$ABAP.type( vkorg )",
                                     kunnr     "$ABAP.type( kunnr )",
                                     name1    "$ABAP.type( name1_gp )",
                                     werks    "$ABAP.type( werks_d     )",
                                     block    "$ABAP.type( block     )",
                                     dist      NVARCHAR( 20 ),
                                     dist_name   "$ABAP.type( name1_gp )",
                                     dist_werks      "$ABAP.type( werks_d     )",
                                     dist_block    "$ABAP.type( loevm_x     )",
                                     stras    NVARCHAR( 80 ),
                                     ort01   NVARCHAR( 80 ),
                                     pstlz    "$ABAP.type( pstlz     )",
                                     regio    "$ABAP.type( regio     )",
                                     smtp_addr    "$ABAP.type( ad_smtpadr    )",
                                     telf1    "$ABAP.type( telf1     )",
                                     telf2   "$ABAP.type( telf2    )",
                                     ktokd            "$ABAP.type(  ktokd            )",
                                     vkbur             "$ABAP.type( vkbur             )",
                                     kdgrp             "$ABAP.type( kdgrp             )",
                                     ktext             "$ABAP.type( ktext             )",
                                     lifnr             "$ABAP.type( lifnr             )",
                                     katr2             "$ABAP.type( katr2             )",
                                     vtext             "$ABAP.type( vtext             )",
                                     stcd3             "$ABAP.type( stcd3             )",
                                     j_1ipanno         "$ABAP.type( j_1ipanno         )",
                                     partner           "$ABAP.type( partner           )",
                                     group_feature     "$ABAP.type( bp_group_feature   )",
                                     group_feature_na  "$ABAP.type( bp_group_feature_name   )",
                                     so_pernr          "$ABAP.type( zso_pernr   )",
                                     so_pernr_name     "$ABAP.type( zch_emnam   )",
                                     so_position       "$ABAP.type( zso_position      )",
                                     so_position_name  "$ABAP.type( zso_postext    )",
                                     ch_pernr          "$ABAP.type( zch_pernr          )",
                                     ch_pernr_name     "$ABAP.type( zch_emnam       )",
                                     ch_position       "$ABAP.type( zch_position        )",
                                     ch_position_name  "$ABAP.type( zch_emnam    )",
                                     zeinv_flag        "$ABAP.type( zeinv_flag     )",
                                     zeinv_chk         "$ABAP.type( zeinv_chk    )"
           );

     DECLARE iv_client "$ABAP.type( mandt )";
     DECLARE iv_bukrs "$ABAP.type( bukrs )";
     DECLARE iv_vkorg "$ABAP.type( vkorg )";

    iv_bukrs = :p_bukrs;
    iv_vkorg = :p_vkorg;
    iv_client = :p_clnt;
*  DECLARE lv_index INT := 1;
*    RETURN SELECT * FROM zc_cust_block_check( :p_bukrs , :p_vkorg );
    call "ZCL_AMDP_CUST_BLK=>GET_DATA"(
             iv_client   => :iv_client,
             iv_bukrs    => :iv_bukrs,
             iv_vkorg    => :iv_vkorg,
            it_blk_data => :gt_blk_data
         );

    Return SELECT * from :gt_blk_data;

  ENDMETHOD.

  METHOD get_data BY DATABASE PROCEDURE
                  FOR HDB LANGUAGE SQLSCRIPT
                  OPTIONS READ-ONLY
                  USING Zc_Cust_Block_Check
                        kna1
                        adr6
                        zcust_blk_chk
                        tvk2t
                        tp24t
                        cvi_cust_link
                        but000
                        bp001
                        t151t
                        knvv
                        zdist_einv_dtls
                        zcust_gst_chk
                        knvp
                        hrp1000
                        pa0001
*                        knb1
*                        knvv
                        .
    ---Top Declaration
*            declare it_blk_data table like :et_blk_data;
*           DECLARE iv_client NVARCHAR(3) := session_context('CDS_CLIENT');
*           declare is_bukrs nvarchar( 4 ) := :iv_bukrs;
           declare lv_index INTEGER := 1;
           declare lv_line INTEGER;
           declare lt_cust_data TABLE (
                                        bukrs "$ABAP.type( bukrs )",
                                        vkorg "$ABAP.type( vkorg )" ,
                                        kunnr "$ABAP.type( kunnr )" ,
                                        block "$ABAP.type( char1 )" );
           declare lt_kna1 TABLE (   mandt "$ABAP.type( mandt )",
                                    kunnr "$ABAP.type( kunnr )"     ,
                                   name1   NVARCHAR( 35 ) ,
                                   werks     "$ABAP.type( werks_d )",
                                   stras     NVARCHAR( 80 ),
                                   ort01     NVARCHAR( 80 ),
                                   pstlz     "$ABAP.type( pstlz )",
                                   Regio     "$ABAP.type( Regio )",
                                   ktokd     "$ABAP.type( ktokd )",
                                   lifnr     "$ABAP.type( lifnr )",
                                   katr2     "$ABAP.type( katr2 )",
                                   stcd3     "$ABAP.type( stcd3 )",
                                   j_1ipanno "$ABAP.type( j_1ipanno )",
                                   adrnr     "$ABAP.type( adrnr )",
                                   telf1     "$ABAP.type( telf1 )",
                                   telf2     "$ABAP.type( telf2 )"
                                      );
           DECLARE lt_adr6 TABLE (   -- mandt "$ABAP.type( mandt )",
                                    addrnumber "$ABAP.type( ad_addrnum )",
                                     smtp_addr  "$ABAP.type( ad_smtpadr )",
                                     row_num  integer );
                       declare gt_adr6 table like :lt_adr6;
           DECLARE lt_bp001 TABLE (   --mandt "$ABAP.type( mandt )",
                                    kunnr         "$ABAP.type( kunnr )",
                                     partner       "$ABAP.type( bu_partner )",
                                     group_feature "$ABAP.type( bp_group_feature )",
                                     row_num  integer );
                       declare gt_bp001 TABLE like :lt_bp001;
           DECLARE lt_knvv TABLE (   --mandt "$ABAP.type( mandt )",
                                    kunnr "$ABAP.type( kunnr )",
                                    vkorg "$ABAP.type( vkorg )",
                                    kdgrp "$ABAP.type( kdgrp )",
                                    vkbur "$ABAP.type( vkbur )" );
            DECLARE ls_knvv TABLE (   --mandt "$ABAP.type( mandt )",
                                    kunnr "$ABAP.type( kunnr )",
                                    vkorg "$ABAP.type( vkorg )",
                                    kdgrp "$ABAP.type( kdgrp )",
                                    vkbur "$ABAP.type( vkbur )" );
           DECLARE lt_pa0001 TABLE ( -- mandt "$ABAP.type( mandt )",
                                    pernr "$ABAP.type( persno )",
                                     ename "$ABAP.type( emnam )");
           DECLARE lt_hrp1000 TABLE (  --mandt "$ABAP.type( mandt )",
                                    objid "$ABAP.type( hrobjid )",
                                      stext "$ABAP.type( stext )" );
           DECLARE lt_einvdist TABLE (  distributor "$ABAP.type( kunnr )" );
           DECLARE lt_gstchk TABLE (  --mandt "$ABAP.type( mandt )",
                                    kunnr "$ABAP.type( kunnr )",
                                     msg_type "$ABAP.type( bapi_mtype )" );
           DECLARE lt_tvk2t TABLE (  --mandt "$ABAP.type( mandt )",
                                    spras "$ABAP.type( spras )",
                                    katr2 "$ABAP.type( katr2 )",
                                    vtext "$ABAP.type( vtext )" );
           DECLARE lt_tp24t TABLE (  --mandt "$ABAP.type( mandt )",
                                    spras "$ABAP.type( spras )",
                                    group_feature "$ABAP.type( bp_group_feature )",
                                    group_feature_na "$ABAP.type( bp_group_feature_name )" );
           DECLARE lt_t151t TABLE ( -- mandt "$ABAP.type( mandt )",
                                    spras "$ABAP.type( spras )",
                                    kdgrp "$ABAP.type( kdgrp )",
                                    ktext "$ABAP.type( ktext )" );
           DECLARE lt_knvp TABLE (  -- mandt "$ABAP.type( mandt )",
                                    kunnr "$ABAP.type( kunnr )",
                                    vkorg "$ABAP.type( vkorg  )",
                                    vtweg "$ABAP.type( vtweg  )",
                                    spart "$ABAP.type( spart  )",
                                    parvw "$ABAP.type( parvw    )",
                                    parza "$ABAP.type( parza  )",
                                    kunn2 "$ABAP.type( kunn2   )",
                                    lifnr "$ABAP.type( lifnr   )",
                                    pernr "$ABAP.type( pernr_d   )",
                                    parnr "$ABAP.type( parnr  )",
                                    knref "$ABAP.type( knref   )",
                                    defpa "$ABAP.type( defpa  )" );

           DECLARE et_cust_blk_chk TABLE
                                    ( mandt   "$ABAP.type( mandt )",
                                     bukrs      "$ABAP.type( bukrs )",
                                     vkorg      "$ABAP.type( vkorg )",
                                     kunnr     "$ABAP.type( kunnr )",
                                     name1    "$ABAP.type( name1_gp )",
                                     werks    "$ABAP.type( werks_d     )",
                                     block    "$ABAP.type( block     )",
                                     dist      NVARCHAR( 20 ),
                                     dist_name   "$ABAP.type( name1_gp )",
                                     dist_werks      "$ABAP.type( werks_d     )",
                                     dist_block    "$ABAP.type( loevm_x     )",
                                     stras    NVARCHAR( 80 ),
                                     ort01   NVARCHAR( 80 ),
                                     pstlz    "$ABAP.type( pstlz     )",
                                     regio    "$ABAP.type( regio     )",
                                     smtp_addr    "$ABAP.type( ad_smtpadr    )",
                                     telf1    "$ABAP.type( telf1     )",
                                     telf2   "$ABAP.type( telf2    )",
                                     ktokd            "$ABAP.type(  ktokd            )",
                                     vkbur             "$ABAP.type( vkbur             )",
                                     kdgrp             "$ABAP.type( kdgrp             )",
                                     ktext             "$ABAP.type( ktext             )",
                                     lifnr             "$ABAP.type( lifnr             )",
                                     katr2             "$ABAP.type( katr2             )",
                                     vtext             "$ABAP.type( vtext             )",
                                     stcd3             "$ABAP.type( stcd3             )",
                                     j_1ipanno         "$ABAP.type( j_1ipanno         )",
                                     partner           "$ABAP.type( partner           )",
                                     group_feature     "$ABAP.type( bp_group_feature   )",
                                     group_feature_na  "$ABAP.type( bp_group_feature_name   )",
                                     so_pernr          "$ABAP.type( zso_pernr   )",
                                     so_pernr_name     "$ABAP.type( zch_emnam   )",
                                     so_position       "$ABAP.type( zso_position      )",
                                     so_position_name  "$ABAP.type( zso_postext    )",
                                     ch_pernr          "$ABAP.type( zch_pernr          )",
                                     ch_pernr_name     "$ABAP.type( zch_emnam       )",
                                     ch_position       "$ABAP.type( zch_position        )",
                                     ch_position_name  "$ABAP.type( zch_emnam    )",
                                     zeinv_flag        "$ABAP.type( zeinv_flag     )",
                                     zeinv_chk         "$ABAP.type( zeinv_chk    )"
           );
           DECLARE ts_cust_blk_chk table like :et_cust_blk_chk;
           DECLARE ls_knvp TABLE like :lt_knvp;
           DECLARE lv_kunnr "$ABAP.type( kunnr )";
           DECLARE ls_kunnr "$ABAP.type( kunnr )";
           DECLARE lv_katr2 "$ABAP.type( katr2 )";
           DECLARE lv_addrnumber "$ABAP.type( adrnr )";
           DECLARE lv_group_feature NVARCHAR( 10 );
           DECLARE lv_pos1 INTEGER;   -- For Kna1
           DECLARE lv_pos2 INTEGER;   -- For lt_tvkt2
    ---Select Queries
     lt_kna1 =   SELECT  mandt,kunnr, name1, werks, stras, ort01, pstlz, Regio, ktokd, lifnr,
                         katr2, stcd3, j_1ipanno, adrnr, telf1, telf2
                 FROM kna1 where mandt = :iv_client ORDER BY kunnr ;

     gt_adr6 =  SELECT  a.addrnumber,
                    a.smtp_addr,
               ROW_NUMBER() over( PARTITION by a.addrnumber , a.smtp_addr order by a.addrnumber, a.smtp_addr) as row_num
                     from adr6 as a
                INNER JOIN :lt_kna1 as b
                on a.addrnumber = b.adrnr
                where b.mandt = :iv_client
                ORDER BY addrnumber, smtp_addr ;

      lt_adr6 = select * from :gt_adr6 where row_num = 1;
     et_cust_blk_chk = SELECT * FROM zcust_blk_chk where mandt = :iv_client ORDER BY bukrs , vkorg,  kunnr ;
     --attribute 2 description
     lt_tvkt2 = SELECT spras, katr2, vtext  FROM tvk2t WHERE mandt = :iv_client and spras = session_context( 'LOCALE_SAP' )  ORDER BY katr2;
     --group feature description
     lt_tp24t = SELECT spras, group_feature, group_feature_na  FROM tp24t  WHERE mandt = :iv_client and  spras = session_context( 'LOCALE_SAP' ) ORDER BY group_feature ;
     --Partner and GUID
     gt_bp001 = SELECT a.kunnr,c.partner,c.group_feature,
                ROW_NUMBER ( ) OVER( partition by a.kunnr, c.partner , c.group_feature ) as Row_num
                 from kna1 as a
                INNER JOIN cvi_cust_link as b on a.kunnr = b.customer
                INNER JOIN but000 as d on b.partner_guid = d.partner_guid
                INNER JOIN bp001 as c on c.partner = d.partner
                where mandt = :iv_client
                ORDER BY kunnr;

        lt_bp001 = select * from :gt_bp001 where row_num = 1;
     --Customer Group Description
     lt_t151t = SELECT spras,kdgrp,ktext from t151t WHERE mandt = :iv_client and  spras = session_context( 'LOCALE_SAP' ) ORDER BY kdgrp ;

    ---Reading Customer Block Initial Data
      lt_cust_data = select bukrs, vkorg, kunnr, block from zc_cust_block_check( :iv_bukrs , :iv_vkorg )
                            ORDER BY kunnr  ;
*      gt_cust_data = select a.kunnr,  b.bukrs, c.vkorg,
*                     case  when a.aufsd <> ' '  or a.faksd <> ' ' or a.lifsd <> ' '  or a.loevm <> ' '
*              or a.sperr <> ' ' or a.cassd <> ' ' or a.nodel <> ' ' or b.nodel <> ' '
*              or b.loevm <> ' ' or c.loevm <> ' ' then 'X' else ' ' end as block    from kna1 as a
*                    inner join  knb1 as b on a.kunnr = b.kunnr
*                    INNER join knvv as c on a.kunnr = c.kunnr
*                    where b.bukrs = :iv_bukrs
*                      and c.vkorg = :iv_vkorg
*                      and c.vtweg = '20'
*                      and c.spart = '10'    ;
*                                                                WHERE iv_client = session_context( 'CLIENT' );
    ---Get employee names from PA0001
    lt_pa0001 = SELECT pernr,ename FROM pa0001 where begda <= session_context( 'SAP_SYSTEM_DATE' )
                                 and endda >= session_context( 'SAP_SYSTEM_DATE' ) ORDER BY pernr;
    --- Get position master data
    lt_hrp1000 = SELECT objid, stext FROM hrp1000 where plvar = '01' and otype = 'S'
                                 and objid BETWEEN '20000000' AND '29999999'
                                 and begda <= session_context( 'SAP_SYSTEM_DATE' )
                                 and endda >= session_context( 'SAP_SYSTEM_DATE' )
                                 ORDER BY objid;
    ---Fetching sales area data
      lt_knvv = SELECT  kunnr, vkorg, kdgrp, vkbur FROM knvv
                    where mandt = :iv_client and vkorg in ( '1000', '6000' , 'SDMS' )
                    and vtweg = '20' and spart = '10'
                    ORDER BY kunnr , vkorg;

    ---Fetching e-invoice distributor flag
    lt_einvdist = select distributor from zdist_einv_dtls where mandt = :iv_client ORDER BY distributor ;

    ---Fetching Gst checked data
    lt_gstchk = select kunnr,msg_type from zcust_gst_chk where mandt = :iv_client ORDER BY kunnr;

    ---Fetching SK PF data from knvp table
    lt_knvp = select
                kunnr,
                vkorg,
                vtweg,
                spart,
                parvw,
                parza,
                kunn2,
                lifnr,
                pernr,
                parnr,
                knref,
                defpa
     FROM knvp where  mandt = :iv_client and
     vkorg in ( '1000','6000','SDMS' )
     and vtweg = '20' and spart = '10'
     and parvw in ( 'SK','L5','XN','ZC','XZ' ) ;
     lv_line = record_count( :lt_cust_data ); -- No. of records
    IF lv_line <> 0 THEN
    --For loop
    FOR lv_index in 1..lv_line
    do
           lv_kunnr = :lt_cust_data.kunnr[ :lv_index ];
    it_blk_data.mandt[ :lv_index ] = session_context( 'CLIENT' );
    it_blk_data.bukrs[ :lv_index ] = :lt_cust_data.bukrs[ :lv_index ];
    it_blk_data.vkorg[ :lv_index ] = :lt_cust_data.vkorg[ :lv_index ];
    it_blk_data.kunnr[ :lv_index ] = :lt_cust_data.kunnr[ :lv_index ];
    it_blk_data.block[ :lv_index ] = :lt_cust_data.block[ :lv_index ];

    if EXISTS ( select top 1  * from :lt_kna1 where kunnr = lv_kunnr )
    then
    --Search function to get Index of the record
      lv_pos1 = :lt_kna1.search( (KUNNR), lv_kunnr );
    it_blk_data.name1[ :lv_index ] = :lt_kna1.name1[ :lv_pos1 ];
    it_blk_data.werks[ :lv_index ] = :lt_kna1.werks[ :lv_pos1 ];
    it_blk_data.stras[ :lv_index ] = :lt_kna1.stras[ :lv_pos1 ];
    it_blk_data.ort01[ :lv_index ] = :lt_kna1.ort01[ :lv_pos1 ];
    it_blk_data.Regio[ :lv_index ] = :lt_kna1.Regio[ :lv_pos1 ];
    it_blk_data.pstlz[ :lv_index ] = :lt_kna1.pstlz[ :lv_pos1 ];
    it_blk_data.ktokd[ :lv_index ] = :lt_kna1.ktokd[ :lv_pos1 ];
    it_blk_data.lifnr[ :lv_index ] = :lt_kna1.lifnr[ :lv_pos1 ];
    it_blk_data.katr2[ :lv_index ] = :lt_kna1.katr2[ :lv_pos1 ];
    it_blk_data.stcd3[ :lv_index ] = :lt_kna1.stcd3[ :lv_pos1 ];
    it_blk_data.j_1ipanno[ :lv_index ] = :lt_kna1.j_1ipanno[ :lv_pos1 ];
    it_blk_data.telf1[ :lv_index ] = :lt_kna1.telf1[ :lv_pos1 ];
    it_blk_data.telf2[ :lv_index ] = :lt_kna1.telf2[ :lv_pos1 ];

    lv_katr2 = :lt_kna1.katr2[ lv_pos1 ];
    lv_pos2 = :lt_tvkt2.search( (KATR2), lv_katr2 );
    if ( lv_pos2 is not null ) then
    it_blk_data.vtext[ :lv_index ] = :lt_tvkt2.vtext[ lv_pos2 ];
     end if;

    lv_addrnumber = :lt_kna1.adrnr[ lv_pos1 ];
    lv_pos2 = :lt_adr6.search( (ADDRNUMBER), lv_addrnumber );
    if ( lv_pos2 is not null ) then
        it_blk_data.smtp_addr[ :lv_index ] = :lt_adr6.smtp_addr[ lv_pos2 ];
     end if;

    lv_pos2 = :lt_bp001.search( (KUNNR), lv_kunnr );
    it_blk_data.partner[ :lv_index ] = :lt_bp001.partner[ lv_pos2 ];
    if ( lv_pos2 is not null) then
    it_blk_data.group_feature[ :lv_index ] = :lt_bp001.group_feature[ lv_pos2 ];
    end if;

    lv_group_feature = :lt_bp001.group_feature[ lv_pos2 ];
    If ( lv_group_feature is not null )
    then
      lv_pos2 = :lt_tp24t.search( (GROUP_FEATURE), lv_group_feature );
      if ( lv_pos2 is not null ) then
      it_blk_data.group_feature_na[ :lv_index ] = :lt_tp24t.group_feature_na[ lv_pos2 ];
      end if;
    end if ;

     if EXISTS ( select top 1  * from :lt_knvv where kunnr = lv_kunnr AND vkorg = '1000')
     then
     ls_knvv = select top 1 * from :lt_knvv where kunnr = lv_kunnr and vkorg = '1000';
     it_blk_data.kdgrp[ :lv_index ] = :ls_knvv.kdgrp[ 1 ];
     it_blk_data.vkbur[ :lv_index ] = :ls_knvv.vkbur[ 1 ];
     END IF ;

     if EXISTS ( select top 1 * FROM :lt_knvp where kunnr = lv_kunnr and  vkorg = '1000' and parvw = 'L5')
     then
     ls_knvp = select top 1 * from :lt_knvp where kunnr = lv_kunnr and vkorg = '1000' and parvw = 'L5' ;

      it_blk_data.so_pernr[ :lv_index ] = :ls_knvp.pernr[ 1 ];
      lv_pos2 = :lt_pa0001.search( (PERNR) , :ls_knvp.pernr[ 1 ]);
      IF ( lv_pos2 <> '0' ) then
      it_blk_data.so_pernr_name[ :lv_index ] = :lt_pa0001.ename[ lv_pos2 ];
      END IF;
     END IF ;

    --Null lt_knvv and lt_knvp
*    ls_knvv =  select  NULL as kunnr,
*                       NULL as vkorg,
*                       NULL as kdgrp,
*                       NULL as vkbur from dummy where 1 = 2;
*
*    ls_knvp = select NULL as kunnr,
*                     NULL as vkorg,
*                     NULL as vtweg,
*                     NULL as spart,
*                     NULL as parvw,
*                     NULL as parza,
*                     NULL as kunn2,
*                     NULL as lifnr,
*                     NULL as pernr,
*                     NULL as parnr,
*                     NULL as knref,
*                     NULL as defpa   from dummy where 1= 2;
*   lt_knvp = select
    --So Pernr and Name
    IF EXISTS ( SELECT top 1 * from :lt_knvp where kunnr = lv_kunnr and vkorg = '1000' and parvw = 'ZC' )
    then
    ls_knvp = SELECT top 1 * from :lt_knvp where kunnr = lv_kunnr and vkorg = '1000' and parvw = 'ZC';

    it_blk_data.ch_pernr[ :lv_index ] = :ls_knvp.pernr[ 1 ];
    lv_pos2 = :lt_pa0001.search( (PERNR), :ls_knvp.pernr[ 1 ] );
    IF ( lv_pos2 <> '0' ) then
    it_blk_data.ch_pernr_name[ :lv_index ] = :lt_pa0001.ename[ lv_pos2 ];
    END IF;
    END IF ;

    --SO Position and name
    IF EXISTS ( SELECT top 1 * from :lt_knvp where kunnr = lv_kunnr and vkorg = '1000' and parvw = 'XN' )
    then
    ls_knvp = SELECT top 1 * from :lt_knvp where kunnr = lv_kunnr and vkorg = '1000' and parvw = 'XN';

    it_blk_data.so_position[ :lv_index ] = :ls_knvp.lifnr[ 1 ];
    lv_pos2 = :lt_hrp1000.search( (OBJID), :ls_knvp.lifnr[ 1 ] );
    IF ( lv_pos2 <> '0' ) then
    it_blk_data.so_position_name[ :lv_index ] = :lt_hrp1000.stext[ lv_pos2 ];
    END IF;
    END IF ;

   --check flag for e-invoice distributor
   IF EXISTS ( SELECT TOP 1 * from :lt_einvdist WHERE distributor = lv_kunnr ) then
    it_blk_data.zeinv_flag[ :lv_index ] = 'X' ;
   END IF ;
*     et_cust_blk_chk = SELECT top 1 * from :et_cust_blk_chk WHERE bukrs = 'DMS1' and vkorg = 'SDMS'
*                                                          and kunnr = lv_kunnr ;
*       ls_kunnr = :et_cust_blk_chk.kunnr[ 1 ];
*    IF( ls_kunnr is not null ) then
*    IF ( lv_kunnr <> ls_kunnr ) then
*    INSERT INTO :it_blk_data select * from :et_blk_data;
    END IF;

    --So Pernr and Name
    IF EXISTS ( SELECT top 1 * from :lt_knvp where kunnr = lv_kunnr and vkorg = '1000' and parvw = 'ZC' )
    then
    ls_knvp = SELECT top 1 * from :lt_knvp where kunnr = lv_kunnr and vkorg = '1000' and parvw = 'ZC';

    it_blk_data.ch_pernr[ :lv_index ] = :ls_knvp.pernr[ 1 ];
    lv_pos2 = :lt_pa0001.search( (PERNR), :ls_knvp.pernr[ 1 ] );
    IF ( lv_pos2 <> '0' ) then
    it_blk_data.ch_pernr_name[ :lv_index ] = :lt_pa0001.ename[ lv_pos2 ];
    END IF;
    END IF ;

    --SO Position and name
    IF EXISTS ( SELECT top 1 * from :lt_knvp where kunnr = lv_kunnr and vkorg = '1000' and parvw = 'XN' )
    then
    ls_knvp = SELECT top 1 * from :lt_knvp where kunnr = lv_kunnr and vkorg = '1000' and parvw = 'XN';

    it_blk_data.so_position[ :lv_index ] = :ls_knvp.lifnr[ 1 ];
    lv_pos2 = :lt_hrp1000.search( (OBJID), :ls_knvp.lifnr[ 1 ] );
    IF ( lv_pos2 <> '0' ) then
    it_blk_data.so_position_name[ :lv_index ] = :lt_hrp1000.stext[ lv_pos2 ];
    END IF;
    END IF ;

   --check flag for e-invoice distributor
   IF EXISTS ( SELECT TOP 1 * from :lt_einvdist WHERE distributor = lv_kunnr ) then
    it_blk_data.zeinv_flag[ :lv_index ] = 'X' ;
   END IF ;

*      IF EXISTS ( SELECT top 1 * from :et_cust_blk_chk WHERE bukrs = :iv_bukrs and vkorg = :iv_vkorg
*                                                          and kunnr = lv_kunnr ) then
*    tt_cust_blk_chk = SELECT top 1 * from :et_cust_blk_chk WHERE bukrs = :iv_bukrs and vkorg = :iv_vkorg
*                                                          and kunnr = lv_kunnr ;
*          ls_kunnr = :tt_cust_blk_chk.kunnr[ 1 ];
*    IF (  :it_blk_data.kunnr[ :lv_index ]   <>  :tt_cust_blk_chk.kunnr[ 1 ]
*      and :it_blk_data.vkorg[ :lv_index ]   <>  :tt_cust_blk_chk.BUKRS[ 1 ]
*      and :it_blk_data.kunnr[ :lv_index ]   <>  :tt_cust_blk_chk.VKORG[ 1 ]
*      and :it_blk_data.block[ :lv_index ]   <>  :tt_cust_blk_chk.KUNNR[ 1 ]
*      and :it_blk_data.NAME1[ :lv_index ]   <>  :tt_cust_blk_chk.NAME1[ 1 ]
*      and :it_blk_data.WERKS[ :lv_index ]   <>  :tt_cust_blk_chk.WERKS[ 1 ]
*      and :it_blk_data.BLOCK[ :lv_index ]   <>  :tt_cust_blk_chk.BLOCK[ 1 ]
*      and :it_blk_data.DIST[ :lv_index ]    <>  :tt_cust_blk_chk.DIST[ 1 ]
*      and :it_blk_data.DIST_NAME[ :lv_index ]   <>  :tt_cust_blk_chk.DIST_NAME[ 1 ]
*      and :it_blk_data.DIST_WERKS[ :lv_index ]  <>  :tt_cust_blk_chk.DIST_WERKS[ 1 ]
*      and :it_blk_data.DIST_BLOCK[ :lv_index ]  <>  :tt_cust_blk_chk.DIST_BLOCK[ 1 ]
*      and :it_blk_data.STRAS[ :lv_index ]   <>  :tt_cust_blk_chk.STRAS[ 1 ]
*      and :it_blk_data.ORT01[ :lv_index ]   <>  :tt_cust_blk_chk.ORT01[ 1 ]
*      and :it_blk_data.PSTLZ[ :lv_index ]   <>  :tt_cust_blk_chk.PSTLZ[ 1 ]
*      and :it_blk_data.REGIO[ :lv_index ]   <>  :tt_cust_blk_chk.REGIO[ 1 ]
*      and :it_blk_data.SMTP_ADDR[ :lv_index ]   <>  :tt_cust_blk_chk.SMTP_ADDR[ 1 ]
*      and :it_blk_data.TELF1[ :lv_index ]   <>  :tt_cust_blk_chk.TELF1[ 1 ]
*      and :it_blk_data.TELF2[ :lv_index ]   <>  :tt_cust_blk_chk.TELF2[ 1 ]
*      and :it_blk_data.KTOKD[ :lv_index ]   <>  :tt_cust_blk_chk.KTOKD[ 1 ]
*      and :it_blk_data.VKBUR[ :lv_index ]   <>  :tt_cust_blk_chk.VKBUR[ 1 ]
*      and :it_blk_data.KDGRP[ :lv_index ]   <>  :tt_cust_blk_chk.KDGRP[ 1 ]
*      and :it_blk_data.KTEXT[ :lv_index ]   <>  :tt_cust_blk_chk.KTEXT[ 1 ]
*      and :it_blk_data.LIFNR[ :lv_index ]   <>  :tt_cust_blk_chk.LIFNR[ 1 ]
*      and :it_blk_data.KATR2[ :lv_index ]   <>  :tt_cust_blk_chk.KATR2[ 1 ]
*      and :it_blk_data.VTEXT[ :lv_index ]   <>  :tt_cust_blk_chk.VTEXT[ 1 ]
*      and :it_blk_data.STCD3[ :lv_index ]   <>  :tt_cust_blk_chk.STCD3[ 1 ]
*      and :it_blk_data.J_1IPANNO[ :lv_index ]   <>  :tt_cust_blk_chk.J_1IPANNO[ 1 ]
*      and :it_blk_data.PARTNER[ :lv_index ]     <>  :tt_cust_blk_chk.PARTNER[ 1 ]
*      and :it_blk_data.GROUP_FEATURE[ :lv_index ]   <>  :tt_cust_blk_chk.GROUP_FEATURE[ 1 ]
*      and :it_blk_data.GROUP_FEATURE_NA[ :lv_index ]    <>  :tt_cust_blk_chk.GROUP_FEATURE_NA[ 1 ]
*      and :it_blk_data.SO_PERNR[ :lv_index ]    <>  :tt_cust_blk_chk.SO_PERNR[ 1 ]
*      and :it_blk_data.SO_PERNR_NAME[ :lv_index ]   <>  :tt_cust_blk_chk.SO_PERNR_NAME[ 1 ]
*      and :it_blk_data.SO_POSITION[ :lv_index ]     <>  :tt_cust_blk_chk.SO_POSITION[ 1 ]
*      and :it_blk_data.SO_POSITION_NAME[ :lv_index ]    <>  :tt_cust_blk_chk.SO_POSITION_NAME[ 1 ]
*      and :it_blk_data.CH_PERNR[ :lv_index ]    <>  :tt_cust_blk_chk.CH_PERNR[ 1 ]
*      and :it_blk_data.CH_PERNR_NAME[ :lv_index ]   <>  :tt_cust_blk_chk.CH_PERNR_NAME[ 1 ]
*      and :it_blk_data.CH_POSITION[ :lv_index ]     <>  :tt_cust_blk_chk.CH_POSITION[ 1 ]
*      and :it_blk_data.CH_POSITION_NAME[ :lv_index ]    <>  :tt_cust_blk_chk.CH_POSITION_NAME[ 1 ]
*      and :it_blk_data.ZEINV_FLAG[ :lv_index ]  <>  :tt_cust_blk_chk.ZEINV_FLAG[ 1 ]
*      and :it_blk_data.ZEINV_CHK[ :lv_index ]   <>  :tt_cust_blk_chk.ZEINV_CHK[ 1 ] )
*      then
*            et_blk_data = select * from :it_blk_data;
*      END if;
*      ELSE
*      et_blk_data = select * from :it_blk_data;
*      End if;
*    INSERT INTO :tt_cust_blk_chk SELECT * from :et_blk_data;
*   lv_name1 = select top 1 name1 from :et_kna1 where kunnr = :lt_cust_data.kunnr;
*    et_blk_data.name1[ :lv_index ] = :lv_name1
   END FOR ;
*     WHILE lv_index BETWEEN 1 and lv_line
*     Do
   END IF ;

*   et_blk_data = select * from :it_blk_data;
*   it_bulk_data = select * from :et_bulk_data;

ENDMETHOD.

ENDCLASS.
