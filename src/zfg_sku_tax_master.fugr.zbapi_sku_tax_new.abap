FUNCTION ZBAPI_SKU_TAX_NEW .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_MATNR) TYPE  MARA-MATNR OPTIONAL
*"     VALUE(P_KUNNR) TYPE  KNA1-KUNNR OPTIONAL
*"     VALUE(P_VKBUR) TYPE  KNVV-VKBUR OPTIONAL
*"     VALUE(P_KSCHL) TYPE  KONH-KSCHL OPTIONAL
*"  TABLES
*"      IT_TAX_MASTER TYPE  ZTT_CNF_TAX_MASTER
*"      S_DATE STRUCTURE  ZSTR_SKU_DATE
*"      S_MTART TYPE  ZTT_CNF_MTART
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------
*&    AUTHOR          : VELRAJ
*&    CREATED ON      : 16.04.2015
*&    COMPANY         : CLSS
*&    OBJECTIVE       : THIS FUNCTION MODULE CAN BE USED TO GET TAX DETAILS USED FOR SALESPRO BUSINESS
*&----------------------------------------------------------------------------------------------------------
*&----------------------------------------------------------------------------------------------------------

***************************************************************************************************
*&---------------------Condition Types and tables
*ZIVP - VAT - A843, A846
*ZIVC - CST - A844, A847
*JIVA - Additional Tax - A834, A831, A835
*ZVSR - VAT Surcharge - A845, A848


* A831 - Sales Off./Material
* A834 - Sales Off./Customer
* A835 - Sales Off.
* A843 - Sales Off./Country/Region/TaxCl6Cust
* A844 - Sales Off./Country/Region/TaxCl7Cust
* A845 - Sales Off./Country/Region/TaxCl8Cust
* A846 - Sales Off./Country/Region/TaxCl6Cust/Material
* A847 - Sales Off./Country/Region/TaxCl7Cust/Material
* A848 - Sales Off./Country/Region/TaxCl8Cust/Material
*****************************************************************************************************

*&-------------------------------------- DECLARATIONS - BEGIN -----------------------------------------------

  DATA: WA_TAX_MASTER   TYPE      ZSTR_CNF_TAX_MASTER,
        LV_MATNR        TYPE      MARA-MATNR.
  FIELD-SYMBOLS: <FS_TAX> TYPE   ZSTR_CNF_TAX_MASTER.
*&-------------------------------------- DECLARATIONS - END---------------------------------------------------

  PERFORM TAX_MASTER_INITIALIZATION TABLES    R_MTART
                                              S_MTART
                                              R_MATNR
                                              R_WERKS
                                              S_DATE
                                              R_KNUMH
                                              R_KUNNR
                                              R_VKORG
                                              R_KSCHL
                                              R_VKBUR
                                              R_REGIO
                                               RT_KONH
                                              RETURN
                                     USING    P_BUKRS
                                              P_MATNR
                                              P_KUNNR
                                              P_VKBUR
                                              P_KSCHL.

*&-------------------------------------- Data Retrieval - BEGIN -----------------------------------------------
*&------------------------------------ Get Material List for the plant
  IF NOT R_WERKS[] IS INITIAL AND  P_KUNNR IS INITIAL.
    PERFORM GET_MARARCRM TABLES R_MATNR R_WERKS R_MTART CHANGING GTH_MARARCRM.
  ELSEIF P_KUNNR IS INITIAL.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'E'   'Error!!! - No Relevant Data Found for the given input!!!'.
    EXIT.
  ENDIF.
*&------------------------------------------------------------- tax data retrieval
  IF NOT GTH_MARARCRM IS INITIAL AND NOT R_VKBUR[] IS INITIAL AND NOT R_KSCHL[] IS INITIAL AND P_KUNNR IS INITIAL.
    PERFORM GET_A831  TABLES R_KSCHL R_VKBUR         R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A831.
    PERFORM GET_A846  TABLES R_KSCHL R_VKBUR R_REGIO R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A846.
    PERFORM GET_A847  TABLES R_KSCHL R_VKBUR R_REGIO R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A847.
    PERFORM GET_A848  TABLES R_KSCHL R_VKBUR R_REGIO R_MATNR R_KNUMH RETURN USING GTH_MARARCRM GC_KAPPL CHANGING GT_A848.
  ENDIF.

  IF NOT R_VKBUR[] IS INITIAL AND P_MATNR IS INITIAL AND P_KUNNR IS INITIAL.
    PERFORM GET_A834  TABLES R_KSCHL R_VKBUR R_KUNNR R_KNUMH RETURN USING GC_KAPPL CHANGING GT_A834.
    PERFORM GET_A835  TABLES R_KSCHL R_VKBUR         R_KNUMH RETURN USING GC_KAPPL CHANGING GT_A835.
    PERFORM GET_A843  TABLES R_KSCHL R_VKBUR R_REGIO R_KNUMH RETURN USING GC_KAPPL CHANGING GT_A843.
    PERFORM GET_A844  TABLES R_KSCHL R_VKBUR R_REGIO R_KNUMH RETURN USING GC_KAPPL CHANGING GT_A844.
    PERFORM GET_A845  TABLES R_KSCHL R_VKBUR R_REGIO R_KNUMH RETURN USING GC_KAPPL CHANGING GT_A845.
  ELSEIF NOT P_KUNNR IS INITIAL AND P_MATNR IS INITIAL.
    PERFORM GET_A834  TABLES R_KSCHL R_VKBUR R_KUNNR R_KNUMH RETURN USING GC_KAPPL CHANGING GT_A834.
  ENDIF.
*&-------------------------------------- Data Retrieval - END -----------------------------------------------

*&------------------------------------- Data Processing - BEGIN -----------------------------------------------\
  LOOP AT GT_A831 INTO GS_A831.
    WA_TAX_MASTER-TABLE_NAME = 'A831'.
    MOVE-CORRESPONDING GS_A831 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, GS_A831.
  ENDLOOP.

  LOOP AT GT_A834 INTO GS_A834.
    WA_TAX_MASTER-TABLE_NAME = 'A834'.
    MOVE-CORRESPONDING GS_A834 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, GS_A834.
  ENDLOOP.

  LOOP AT GT_A835 INTO GS_A835.
    WA_TAX_MASTER-TABLE_NAME = 'A835'.
    MOVE-CORRESPONDING GS_A835 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, GS_A835.
  ENDLOOP.

  LOOP AT GT_A843 INTO GS_A843.
    WA_TAX_MASTER-TABLE_NAME = 'A843'.
    MOVE-CORRESPONDING GS_A843 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, GS_A843.
  ENDLOOP.

  LOOP AT GT_A844 INTO GS_A844.
    WA_TAX_MASTER-TABLE_NAME = 'A844'.
    MOVE-CORRESPONDING GS_A844 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, GS_A844.
  ENDLOOP.

  LOOP AT GT_A845 INTO GS_A845.
    WA_TAX_MASTER-TABLE_NAME = 'A845'.
    MOVE-CORRESPONDING GS_A845 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, GS_A845.
  ENDLOOP.

  LOOP AT GT_A846 INTO GS_A846.
    WA_TAX_MASTER-TABLE_NAME = 'A846'.
    MOVE-CORRESPONDING GS_A846 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, GS_A846.
  ENDLOOP.

  LOOP AT GT_A847 INTO GS_A847.
    WA_TAX_MASTER-TABLE_NAME = 'A847'.
    MOVE-CORRESPONDING GS_A847 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, GS_A847.
  ENDLOOP.

  LOOP AT GT_A848 INTO GS_A848.
    WA_TAX_MASTER-TABLE_NAME = 'A848'.
    MOVE-CORRESPONDING GS_A848 TO WA_TAX_MASTER.
    APPEND WA_TAX_MASTER TO IT_TAX_MASTER.
    CLEAR: WA_TAX_MASTER, GS_A848.
  ENDLOOP.

  SORT: GTH_MARARCRM     BY MATNR,
        IT_TAX_MASTER   BY MATNR.

  LOOP AT IT_TAX_MASTER ASSIGNING <FS_TAX>.
    CHECK <FS_TAX>-TABLE_NAME = 'A831'  OR <FS_TAX>-TABLE_NAME = 'A846' OR
          <FS_TAX>-TABLE_NAME = 'A847'  OR <FS_TAX>-TABLE_NAME = 'A848'.

    IF LV_MATNR = <FS_TAX>-MATNR.
      DELETE IT_TAX_MASTER.
    ELSE.
      READ TABLE GTH_MARARCRM TRANSPORTING NO FIELDS WITH KEY MATNR = <FS_TAX>-MATNR.
      IF SY-SUBRC <> 0.
        LV_MATNR = <FS_TAX>-MATNR.
        DELETE IT_TAX_MASTER.
      ENDIF.
    ENDIF.
  ENDLOOP.


  SORT: IT_TAX_MASTER BY VKBUR KSCHL KNUMH.
*&------------------------------------- Data Processing - END -----------------------------------------------


*&-------------------------------------- Return Messages -------------------------------------------------
  IF IT_TAX_MASTER[] IS NOT INITIAL.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'S'   'Success!!! - Tax master is updated!!!'.
  ELSE.
    PERFORM RAISE_MESSAGE TABLES  RETURN   USING   'E'   'Error!!! - Tax master is not updated!!!'.
  ENDIF.

  CLEAR: GV_INDEX_MSG, R_KNUMH[], R_KUNNR[], R_MATNR[], R_KSCHL[], R_VKORG[], R_WERKS[], R_MTART[], S_MTART[], S_DATE[].
  CLEAR: GTH_MARARCRM, GS_MARARCRM, R_VKBUR[].
  CLEAR: GT_A831, GS_A831, GT_A834, GS_A834, GT_A835, GS_A835, GT_A843, GS_A843, GT_A844, GS_A844.
  CLEAR: GT_A845, GS_A845, GT_A846, GS_A846, GT_A847, GS_A847, GT_A848, GS_A848.
ENDFUNCTION.
