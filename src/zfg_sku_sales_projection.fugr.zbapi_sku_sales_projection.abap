FUNCTION ZBAPI_SKU_SALES_PROJECTION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_CANCELLED) TYPE  FLAG DEFAULT SPACE
*"     VALUE(P_PRICE_CHECK) TYPE  FLAG DEFAULT SPACE
*"     VALUE(P_DFLAG) TYPE  FLAG DEFAULT SPACE
*"  TABLES
*"      GT_VBRK STRUCTURE  ZSKU_VBRK
*"      GT_VBRP STRUCTURE  ZSKU_VBRP
*"      GT_VBATCH STRUCTURE  ZSKU_VBATCH
*"      GT_VDISC STRUCTURE  ZSKU_VDISC
*"      GT_VTAX STRUCTURE  ZSKU_VTAX
*"      GT_SALES STRUCTURE  ZSALES_PRO
*"      RETURN STRUCTURE  BAPIRET2
*"      RETURN_COMPARE STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------


*&-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*&-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*&    AUTHOR          : VELRAJ
*&    CREATED ON      : 04-05-2015
*&    COMPANY         : CLSS
*&    OBJECTIVE       : THIS FUNCTION MODULE CAN BE USED TO UPDATE INVOICES TO SAP FROM SALESPRO PORTAL
*&    NOTE            : ZTABLES HAVE BEEN CREATED TO UPDATE THE DATA.
*&                    : ZSKU_VBRK, ZSKU_VBRP, ZSKU_VBATCH, ZSKU_VDISC, ZSKU_VTAX
*&                    : THESE TABLES WILL ACT AS A COPY OF THE TABLES CREATED FOR SALESPRO PORTAL IN JAVA
*&-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*&-------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  TYPES: BEGIN OF TY_DBVBRK,
          VKORG TYPE ZSKU_VBRK-VKORG,
          WERKS TYPE ZSKU_VBRK-WERKS,
          SKU_ID TYPE ZSKU_VBRK-SKU_ID,
          VBELN TYPE ZSKU_VBRK-VBELN,
          DOC_YEAR TYPE ZSKU_VBRK-DOC_YEAR,
          END OF TY_DBVBRK.


  DATA: GS_VBRK     TYPE ZSKU_VBRK,
        GS_VBRP     TYPE ZSKU_VBRP,
        GS_VBATCH   TYPE ZSKU_VBATCH,
        GS_VDISC    TYPE ZSKU_VDISC,
        GS_VTAX     TYPE ZSKU_VTAX,
        LV_MESSAGE  TYPE STRING,
        LV_INDEX    TYPE SY-TABIX,
        LV_MESSAGE_V1  TYPE SYMSGV,
        LV_MESSAGE_V2  TYPE SYMSGV,
        LV_MESSAGE_V3  TYPE SYMSGV,
        LV_MESSAGE_V4  TYPE SYMSGV,
        GT_DBVBRK   TYPE STANDARD TABLE OF TY_DBVBRK.

  DATA: GS_SALES TYPE ZSALES_PRO .


  FIELD-SYMBOLS: <FS_VBRK> TYPE ZSKU_VBRK.

  FIELD-SYMBOLS: <FS_ZSALES_PRO> TYPE ZSALES_PRO.

*&---------------------------------------- Declarations - End -------------------------------------------

   SORT: GT_VBRK   BY VKORG WERKS SKU_ID VBELN DOC_YEAR,
   GT_VBRP   BY VKORG WERKS SKU_ID VBELN DOC_YEAR POSNR,
        GT_VBATCH BY VKORG WERKS SKU_ID VBELN DOC_YEAR POSNR MATNR LGORT CHARG,
        GT_VDISC  BY VKORG WERKS SKU_ID VBELN DOC_YEAR POSNR MATNR DISC_SCHEME_ID DISC_TYPE,
        GT_VTAX   BY VKORG WERKS SKU_ID VBELN DOC_YEAR POSNR MATNR TAX_CONDITION.

" ADDED BY RAM ON 30/10/15 For Sales Projection
  SORT: GT_SALES BY ZZWERKS ZZMATNR.

     "   LOOP AT GT_SALES ASSIGNING <FS_ZSALES_PRO>.

        "    APPEND <FS_ZSALES_PRO> TO GT_SALES_TEMP.
         "           UPDATE ZSALES_PRO FROM <FS_ZSALES_PRO>.

              "       UPDATE ZSALES_PRO FROM GT_SALES.


              MODIFY ZSALES_PRO FROM TABLE GT_SALES.

      "              CLEAR <FS_ZSALES_PRO>.
        " ENDLOOP.


*
*          READ TABLE GT_SALES TRANSPORTING NO FIELDS WITH KEY ZZWERKS = <FS_ZSALES_PRO>-ZZWERKS ZZMATNR = <FS_ZSALES_PRO>-ZZMATNR ZZMTART = <FS_ZSALES_PRO>-ZZMTART . " VBELN = <FS_VBRK>-VBELN DOC_YEAR = <FS_VBRK>-DOC_YEAR.
*    IF SY-SUBRC = 0.
*      LV_INDEX = SY-TABIX.
**
*      LOOP AT GT_SALES INTO GS_SALES FROM LV_INDEX.
*        APPEND GS_SALES TO GT_SALES_TEMP.
**        AT END OF DOC_YEAR.
**          CLEAR LV_INDEX.
**          EXIT.
**        ENDAT.
*"
*        UPDATE ZSALES_PRO FROM <FS_ZSALES_PRO>.
*      ENDLOOP.
*ENDIF.

"ENDED BY RAM ON 30/10/15 For Sales Projection

*  IF P_DFLAG = 'X'.
*    P_PRICE_CHECK = SPACE.
*  ENDIF.
*
*  IF ( P_CANCELLED = 'X' AND GT_VBRK[] IS NOT INITIAL ) .
**    OR ( P_DFLAG = 'X' AND GT_VBRK[] IS NOT INITIAL ).
*    SELECT VKORG
*           WERKS
*           SKU_ID
*           VBELN
*           DOC_YEAR
*      FROM ZSKU_VBRK
*      INTO TABLE GT_DBVBRK
*      FOR ALL ENTRIES IN GT_VBRK
*      WHERE VKORG   = GT_VBRK-VKORG
*      AND   WERKS   = GT_VBRK-WERKS
*      AND   SKU_ID  = GT_VBRK-SKU_ID
*      AND   VBELN   = GT_VBRK-VBELN
*      AND   DOC_YEAR = GT_VBRK-DOC_YEAR.
*  ENDIF.


*  LOOP AT GT_VBRK ASSIGNING <FS_VBRK>.
*    APPEND <FS_VBRK> TO GT_VBRK_TEMP.
*    LV_MESSAGE_V1 = <FS_VBRK>-WERKS.
*    LV_MESSAGE_V2 = <FS_VBRK>-SKU_ID.
*    LV_MESSAGE_V3 = <FS_VBRK>-DOC_YEAR.
*    LV_MESSAGE_V4 = <FS_VBRK>-VBELN.
*
*    IF P_CANCELLED = 'X'.
*      SORT GT_DBVBRK BY VKORG WERKS SKU_ID VBELN DOC_YEAR.
*      READ TABLE GT_DBVBRK TRANSPORTING NO FIELDS WITH KEY VKORG = <FS_VBRK>-VKORG WERKS = <FS_VBRK>-WERKS SKU_ID = <FS_VBRK>-SKU_ID VBELN = <FS_VBRK>-VBELN DOC_YEAR = <FS_VBRK>-DOC_YEAR.
*      IF SY-SUBRC = 0.
*        UPDATE ZSKU_VBRK FROM <FS_VBRK>.
*        <FS_VBRK>-ISCANCELLED = 'C'.
*      ELSE.
*        CONCATENATE 'There is no VBRK Entry in SAP for the given VBRK entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*        PERFORM RAISE_MESSAGE TABLES RETURN USING 'E' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*        CLEAR: LV_MESSAGE.
*      ENDIF.
*      CONTINUE.
*    ENDIF.
*
*    IF P_DFLAG = 'X'.
*      DELETE FROM ZSKU_VBRK WHERE VKORG = <FS_VBRK>-VKORG AND WERKS = <FS_VBRK>-WERKS AND SKU_ID = <FS_VBRK>-SKU_ID AND DOC_YEAR = <FS_VBRK>-DOC_YEAR AND VBELN = <FS_VBRK>-VBELN.
*      IF SY-SUBRC = 0.
*        DELETE FROM ZSKU_VBRP WHERE VKORG = <FS_VBRK>-VKORG AND WERKS = <FS_VBRK>-WERKS AND SKU_ID = <FS_VBRK>-SKU_ID AND DOC_YEAR = <FS_VBRK>-DOC_YEAR AND VBELN = <FS_VBRK>-VBELN.
*        DELETE FROM ZSKU_VBATCH WHERE VKORG = <FS_VBRK>-VKORG AND WERKS = <FS_VBRK>-WERKS AND SKU_ID = <FS_VBRK>-SKU_ID AND DOC_YEAR = <FS_VBRK>-DOC_YEAR AND VBELN = <FS_VBRK>-VBELN.
*        DELETE FROM ZSKU_VTAX WHERE VKORG = <FS_VBRK>-VKORG AND WERKS = <FS_VBRK>-WERKS AND SKU_ID = <FS_VBRK>-SKU_ID AND DOC_YEAR = <FS_VBRK>-DOC_YEAR AND VBELN = <FS_VBRK>-VBELN.
*        DELETE FROM ZSKU_VDISC WHERE VKORG = <FS_VBRK>-VKORG AND WERKS = <FS_VBRK>-WERKS AND SKU_ID = <FS_VBRK>-SKU_ID AND DOC_YEAR = <FS_VBRK>-DOC_YEAR AND VBELN = <FS_VBRK>-VBELN.
*
*        CONCATENATE 'The entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-DOC_YEAR '-' <FS_VBRK>-VBELN SPACE ' Successfully deleted from all the relevant ztables.' INTO LV_MESSAGE.
*        PERFORM RAISE_MESSAGE TABLES RETURN USING 'S' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*      ELSE.
*        CONCATENATE 'Error!!! There is no ZSKU_VBRK Entry in SAP for the given entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*        PERFORM RAISE_MESSAGE TABLES RETURN USING 'E' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*      ENDIF.
*      CLEAR LV_MESSAGE.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE GT_VBRP TRANSPORTING NO FIELDS WITH KEY VKORG = <FS_VBRK>-VKORG WERKS = <FS_VBRK>-WERKS SKU_ID = <FS_VBRK>-SKU_ID VBELN = <FS_VBRK>-VBELN DOC_YEAR = <FS_VBRK>-DOC_YEAR.
*    IF SY-SUBRC = 0.
*      LV_INDEX = SY-TABIX.
*
*      LOOP AT GT_VBRP INTO GS_VBRP FROM LV_INDEX.
*        APPEND GS_VBRP TO GT_VBRP_TEMP.
*        AT END OF DOC_YEAR.
*          CLEAR LV_INDEX.
*          EXIT.
*        ENDAT.
*      ENDLOOP.
*
*    ELSE.
*      CONCATENATE 'There is no VBRP Entry for the VBRK entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*      PERFORM RAISE_MESSAGE TABLES RETURN USING 'E' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*      CLEAR: LV_INDEX, LV_MESSAGE.
*      CONTINUE.
*    ENDIF.
*
*
*    READ TABLE GT_VBATCH TRANSPORTING NO FIELDS WITH KEY VKORG = <FS_VBRK>-VKORG WERKS = <FS_VBRK>-WERKS SKU_ID = <FS_VBRK>-SKU_ID VBELN = <FS_VBRK>-VBELN DOC_YEAR = <FS_VBRK>-DOC_YEAR.
*    IF SY-SUBRC = 0.
*      LV_INDEX = SY-TABIX.
*
*      LOOP AT GT_VBATCH INTO GS_VBATCH FROM LV_INDEX.
*        APPEND GS_VBATCH TO GT_VBATCH_TEMP.
*        AT END OF DOC_YEAR.
*          CLEAR LV_INDEX.
*          EXIT.
*        ENDAT.
*      ENDLOOP.
*
*    ELSE.
*      CONCATENATE 'There is no VBATCH Entry for the VBRK entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*      PERFORM RAISE_MESSAGE TABLES RETURN USING 'E' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*      CLEAR: LV_INDEX, LV_MESSAGE.
*      CONTINUE.
*    ENDIF.
*
*
*    READ TABLE GT_VTAX TRANSPORTING NO FIELDS WITH KEY VKORG = <FS_VBRK>-VKORG WERKS = <FS_VBRK>-WERKS SKU_ID = <FS_VBRK>-SKU_ID VBELN = <FS_VBRK>-VBELN DOC_YEAR = <FS_VBRK>-DOC_YEAR.
*    IF SY-SUBRC = 0.
*      LV_INDEX = SY-TABIX.
*
*      LOOP AT GT_VTAX INTO GS_VTAX FROM LV_INDEX.
*        APPEND GS_VTAX TO GT_VTAX_TEMP.
*        AT END OF DOC_YEAR.
*          CLEAR LV_INDEX.
*          EXIT.
*        ENDAT.
*      ENDLOOP.
*
*    ELSE.
*      CONCATENATE 'There is no VTAX Entry for the VBRK entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*      PERFORM RAISE_MESSAGE TABLES RETURN USING 'E' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*      CLEAR: LV_INDEX, LV_MESSAGE.
*      CONTINUE.
*    ENDIF.
*
*
*    READ TABLE GT_VDISC TRANSPORTING NO FIELDS WITH KEY VKORG = <FS_VBRK>-VKORG WERKS = <FS_VBRK>-WERKS SKU_ID = <FS_VBRK>-SKU_ID VBELN = <FS_VBRK>-VBELN DOC_YEAR = <FS_VBRK>-DOC_YEAR.
*    IF SY-SUBRC = 0.
*      LV_INDEX = SY-TABIX.
*
*      LOOP AT GT_VDISC INTO GS_VDISC FROM LV_INDEX.
*        APPEND GS_VDISC TO GT_VDISC_TEMP.
*        AT END OF DOC_YEAR.
*          CLEAR LV_INDEX.
*          EXIT.
*        ENDAT.
*      ENDLOOP.
*    ENDIF.
*
*    IF P_PRICE_CHECK = 'X'.
*      PERFORM CHECK_INVOICE_PRICE TABLES RETURN_COMPARE USING  GT_VBRK_TEMP GT_VBRP_TEMP GT_VDISC_TEMP.
*    ENDIF.
*
*    MODIFY ZSKU_VBRP FROM TABLE GT_VBRP_TEMP.
*    IF SY-SUBRC = 0.
*      MODIFY ZSKU_VBATCH FROM TABLE GT_VBATCH_TEMP.
*      IF SY-SUBRC = 0.
*        MODIFY ZSKU_VTAX FROM TABLE GT_VTAX_TEMP.
*        IF SY-SUBRC = 0.
*          MODIFY ZSKU_VDISC FROM TABLE GT_VDISC_TEMP.
*          IF SY-SUBRC = 0.
*            <FS_VBRK>-CLOSED = 'X'.                             " Closed = X indicated the successful updation of the invoice to SAP
*            MODIFY ZSKU_VBRK FROM <FS_VBRK>.
*            IF SY-SUBRC = 0.
*              CONCATENATE 'The entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR SPACE ' Successfully inserted/modified in all the relevant ztables.' INTO LV_MESSAGE.
*              PERFORM RAISE_MESSAGE TABLES RETURN USING 'S' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*            ELSE.
*              CONCATENATE 'Problem in updating ZSKU_VBRK TABLE for the VBRK entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*              PERFORM RAISE_MESSAGE TABLES RETURN USING 'E' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*              ROLLBACK WORK.
*            ENDIF.
*          ELSE.
*            CONCATENATE 'Problem in updating ZSKU_VDISC TABLE for the VBRK entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*            PERFORM RAISE_MESSAGE TABLES RETURN USING 'S' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*            ROLLBACK WORK.
*          ENDIF.
*        ELSE.
*          CONCATENATE 'Problem in updating ZSKU_TAX TABLE for the VBRK entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*          PERFORM RAISE_MESSAGE TABLES RETURN USING 'E' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*          ROLLBACK WORK.
*        ENDIF.
*      ELSE.
*        CONCATENATE 'Problem in updating ZSKU_VBATCH TABLE for the VBRK entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*        PERFORM RAISE_MESSAGE TABLES RETURN USING 'E' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*        ROLLBACK WORK.
*      ENDIF.
*    ELSE.
*      CONCATENATE 'Problem in updating ZSKU_VBRP TABLE for the VBRK entry: ' SPACE <FS_VBRK>-VKORG '-' <FS_VBRK>-WERKS '-' <FS_VBRK>-SKU_ID '-' <FS_VBRK>-VBELN '-' <FS_VBRK>-DOC_YEAR INTO LV_MESSAGE.
*      PERFORM RAISE_MESSAGE TABLES RETURN USING 'E' LV_MESSAGE LV_MESSAGE_V1 LV_MESSAGE_V2 LV_MESSAGE_V3 LV_MESSAGE_V4  .
*      ROLLBACK WORK.
*    ENDIF.
*
*    CLEAR: GT_VBRK_TEMP, GT_VBRP_TEMP,GS_VBRP, GT_VBATCH_TEMP, GS_VBATCH, GT_VTAX_TEMP, GS_VTAX, GT_VDISC_TEMP, GS_VDISC.
*    CLEAR: LV_MESSAGE_V1, LV_MESSAGE_V2, LV_MESSAGE_V3, LV_MESSAGE_V4.
*  ENDLOOP.

  CLEAR: GV_INDEX_MSG.
ENDFUNCTION.
