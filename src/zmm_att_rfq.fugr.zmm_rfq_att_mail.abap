FUNCTION ZMM_RFQ_ATT_MAIL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_EKKO) TYPE  EKKO
*"     VALUE(I_MAIL_ID) TYPE  AD_SMTPADR
*"  TABLES
*"      IM_EKPO TYPE  MMPR_UEKPO OPTIONAL
*"  EXCEPTIONS
*"      NO_MAIL_ID_FOUND
*"----------------------------------------------------------------------



SELECT  BRELGUID
        RELTYPE
        INSTID_A
        TYPEID_A
        CATID_A
        INSTID_B
        TYPEID_B
          FROM SRGBTBREL INTO TABLE IT_SRGBTBREL WHERE RELTYPE EQ 'ATTA' AND INSTID_A eq I_EKKO-EBELN ."IN SO_PONO . "and TYPEID_A eq 'BUS2010' and CATID_A eq 'BO'.
IF IT_SRGBTBREL IS NOT INITIAL.
"DELETE it_SRGBTBREL WHERE INSTID_B eq 'FOL35000000000004EXT45000000000596'.

  LO_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).

" sort it_SRGBTBREL by instid_B .

DATA : WA_DATA TYPE SOFOLENTI1.
DATA : OBJ_CON TYPE TABLE OF SOLISTI1    .

      LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
      I_TYPE = 'HTM'
      I_TEXT =  MESSAGE_BODY
      I_SUBJECT = 'Request for quotation - Ref No: "SAP RFQ NUMBER' ).

      MESS_LINE-LINE = '<html>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<body>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<B> Dear Sir/Madam,</B>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = 'We procurement team at Sheenlac Paints Ltd, would be grateful if you would provide us with a quote for the Product/Spare/Service with specification/scope mentioned below.'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      "V_STR = ' "Date of Quotation Deadline date '.
      "CONCATENATE I_EKKO-ANGDT+6(2) '.' I_EKKO-ANGDT+4(2) '.' I_EKKO-ANGDT(4) INTO V_STR1.

      CONCATENATE V_STR V_STR1 INTO V_STR2 SEPARATED BY SPACE.

*      CONCATENATE 'Quotations may be submitted via email on or before ' '<B>'  V_STR2 '"' '</B>'
*                   ' to the mandatory designated submission email address rfq@sheenlac.in .'
*                   INTO MESS_LINE-LINE.
*      APPEND MESS_LINE TO MESSAGE_BODY.
*      CLEAR MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<table style="MARGIN: 10px" bordercolor="black" '.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR MESS_LINE.

      MESS_LINE-LINE = 'cellspacing="0" cellpadding="3" width="650"'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR MESS_LINE.

      MESS_LINE-LINE =  'border="1"><tbody><tr>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR MESS_LINE.

      MESS_LINE-LINE =   '<th>S/No</th>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR MESS_LINE.

      MESS_LINE-LINE =   '<th>Material Description</th>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR MESS_LINE.

      MESS_LINE-LINE =   '<th>Quantity</th>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR MESS_LINE.

      MESS_LINE-LINE =   '<th>UoM</th>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR MESS_LINE.
*    ENDIF.

      LOOP AT IM_EKPO INTO LS_EKPO.


        LV_SNO = LV_SNO + '1'.
        CONCATENATE '<tr><td style="text-align:center">' LV_SNO '</td> '      "S.No
        INTO MESS_LINE-LINE.
        APPEND MESS_LINE TO MESSAGE_BODY.
        CLEAR MESS_LINE.

        CONCATENATE '<td>' LS_EKPO-TXZ01 '</td> '   "Material Desc
        INTO MESS_LINE-LINE.
        APPEND MESS_LINE TO MESSAGE_BODY.
        CLEAR MESS_LINE.

        WRITE LS_EKPO-KTMNG TO MESS_LINE-LINE.        "Quantity
        CONDENSE MESS_LINE-LINE.
        CONCATENATE '<td style="text-align:center">' MESS_LINE-LINE '</td> '
        INTO MESS_LINE-LINE.
        APPEND MESS_LINE TO MESSAGE_BODY.
        CLEAR MESS_LINE.

        WRITE LS_EKPO-MEINS TO MESS_LINE-LINE.       "UOM
        CONDENSE MESS_LINE-LINE.
        CONCATENATE '<td style="text-align:center">' MESS_LINE-LINE '</td> '
        INTO MESS_LINE-LINE.
        APPEND MESS_LINE TO MESSAGE_BODY.
        CLEAR MESS_LINE.
      ENDLOOP.


      MESS_LINE-LINE = '</tbody> </table>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR MESS_LINE.


*      mess_line-line = '<br/>'.
*      APPEND mess_line TO message_body.
*      CLEAR: mess_line.
      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

       d_str1 = ' "Date of Quotation Deadline date '.
      CONCATENATE i_ekko-angdt+6(2) '.' i_ekko-angdt+4(2) '.' i_ekko-angdt(4) INTO d_str2.

      CONCATENATE d_str1 d_str2 INTO d_str3 SEPARATED BY space.

      MESS_LINE-LINE = 'Quotation along with applicable terms and conditions may be submitted via email on or before Deadline Date ........ to the email address ........@sheenlac.in'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

*      MESS_LINE-LINE = 'Thank you and we look forward to receiving your quotation.'.
*      APPEND MESS_LINE TO MESSAGE_BODY.
*      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

*          mess_line-line = ' '.
*          APPEND mess_line TO message_body.
*          CLEAR: mess_line.

      MESS_LINE-LINE = 'Yours Sincerely'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = 'For Sheenlac Paints Ltd,'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '<br/>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = 'Procurement Team.'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '</body>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

      MESS_LINE-LINE = '</html>'.
      APPEND MESS_LINE TO MESSAGE_BODY.
      CLEAR: MESS_LINE.

 CONCATENATE 'Request for quotation - Ref No:' i_ekko-ebeln INTO sub SEPARATED BY space.


 LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
      I_TYPE = 'HTM'
      I_TEXT =  MESSAGE_BODY
      I_SUBJECT = SUB ).
      "ATTACHMENT_SUBJECT = ''.

LOOP AT IT_SRGBTBREL INTO WA_SRGBTBREL.

  CALL FUNCTION 'TEXT_SPLIT'
    EXPORTING
      LENGTH             = 22
      TEXT               = WA_SRGBTBREL-INSTID_B
*     AS_CHARACTER       =
  IMPORTING
    LINE               = lv_str4
     REST               = lv_str1 .

       CALL FUNCTION 'TEXT_SPLIT'
    EXPORTING
      LENGTH             = 20
      TEXT               = lv_str4
*     AS_CHARACTER       =
  IMPORTING
    LINE               = lv_str5
     REST               = lv_str2
            .

  CALL FUNCTION 'TEXT_SPLIT'
    EXPORTING
      LENGTH             = 17
      TEXT               = lv_str5
*     AS_CHARACTER       =
  IMPORTING
    LINE               = lv_str6
     REST               = lv_str3
            .

SELECT SINGLE FILE_EXT INTO WA_FEXT FROM sood WHERE OBJNO eq lv_str1 and OBJYR eq lv_str2 AND OBJTP EQ LV_STR3.
  TRANSLATE WA_FEXT TO UPPER CASE.


     CALL FUNCTION 'SO_DOCUMENT_READ_API1'
       EXPORTING
         DOCUMENT_ID                      =  WA_SRGBTBREL-INSTID_B " 'FOL35000000000004EXT45000000000596'
*        FILTER                           = 'X '
      IMPORTING
       DOCUMENT_DATA                    = WA_DATA
      TABLES
*        OBJECT_HEADER                    =
       OBJECT_CONTENT                   = OBJ_CON[]
*        OBJECT_PARA                      =
*        OBJECT_PARB                      =
*        ATTACHMENT_LIST                  =
*        RECEIVER_LIST                    =
  "     CONTENTS_HEX                     = contents_hex[]
      EXCEPTIONS
        DOCUMENT_ID_NOT_EXIST            = 1
        OPERATION_NO_AUTHORIZATION       = 2
        X_ERROR                          = 3
        OTHERS                           = 4
               .
     IF SY-SUBRC <> 0.
* Implement suitable error handling here
     ENDIF.

IF WA_FEXT EQ 'XLS'.

           TRY.
          LO_DOCUMENT->ADD_ATTACHMENT(
          EXPORTING
          I_ATTACHMENT_TYPE = 'XLS'
          I_ATTACHMENT_SUBJECT = WA_DATA-OBJ_DESCR
          I_ATTACHMENT_SIZE = WA_DATA-DOC_SIZE
          I_ATT_CONTENT_TEXT = OBJ_CON[] ).
        CATCH CX_DOCUMENT_BCS INTO LX_DOCUMENT_BCS.
      ENDTRY.
* Add attachment
* Pass the document to send request
     LO_SEND_REQUEST->SET_DOCUMENT( LO_DOCUMENT ).
ENDIF.


IF WA_FEXT EQ 'PDF'.
           TRY.
          LO_DOCUMENT->ADD_ATTACHMENT(
          EXPORTING
          I_ATTACHMENT_TYPE = 'PDF'
          I_ATTACHMENT_SUBJECT = WA_DATA-OBJ_DESCR
          I_ATTACHMENT_SIZE = WA_DATA-DOC_SIZE
          I_ATT_CONTENT_TEXT = OBJ_CON[] ).
        CATCH CX_DOCUMENT_BCS INTO LX_DOCUMENT_BCS.
      ENDTRY.
* Add attachment
* Pass the document to send request
     LO_SEND_REQUEST->SET_DOCUMENT( LO_DOCUMENT ).
ENDIF.

ENDLOOP.

*if it_SRGBTBREL IS INITIAL.
*  LO_SEND_REQUEST->SET_DOCUMENT( LO_DOCUMENT ).
*  ENDIF.
    "create send request
    "  LO_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).


  "    CONCATENATE 'Request for quotation - Ref No:' I_EKKO-EBELN INTO SUBJECT SEPARATED BY SPACE.

*      LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
*      I_TYPE = 'HTM'
*      I_TEXT =  MESSAGE_BODY
*      I_SUBJECT = 'Request for quotation - Ref No: "SAP RFQ NUMBER' ).



   "Create sender
     "LO_SENDER = CL_SAPUSER_BCS=>CREATE( SY-UNAME ).
     SENDER_MAIL = 'ramachandran@sphinaxinfosystems.com' .
     LO_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( SENDER_MAIL ).


       LO_SEND_REQUEST->SET_SENDER( LO_SENDER ).

      IN_MAILID = 'ramachandran@sphinaxinfosystems.com'. "LV_SMTP_ADDR.

      LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( IN_MAILID ).

*Set recipient
   LO_SEND_REQUEST->ADD_RECIPIENT(
      EXPORTING
      I_RECIPIENT = LO_RECIPIENT
       I_EXPRESS = ABAP_TRUE
      ).

* Send email
      LO_SEND_REQUEST->SEND(
      EXPORTING
      I_WITH_ERROR_SCREEN = ABAP_TRUE
      RECEIVING
      RESULT = LV_SENT_TO_ALL ).
      CLEAR : IN_MAILID,IN_MAILID1,IN_MAILID2,IN_MAILID3.
      COMMIT WORK.
      """""""""""""""""""""""""""""""""""""""""""""
ENDIF.


  ENDFUNCTION.
