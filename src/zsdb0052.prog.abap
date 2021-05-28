*----------------------------------------------------------------------*
* Program ID  : ZSDB0051                                               *
* Title       : cancel billing                                         *
* Author      : E00033                                                 *
* Create Date : 2020.10.14                                             *
* T_CODE      : ZSDB0051                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.10.14   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDB0052 MESSAGE-ID ZSD.

PARAMETERS : P_VBELN LIKE VBRK-VBELN,
             P_BUDAT LIKE VBRK-FKDAT.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  DATA: LT_RETURN    LIKE BAPIRETURN1 OCCURS 0 WITH HEADER LINE,
        LT_SUCCESS   LIKE BAPIVBRKSUCCESS OCCURS 0 WITH HEADER LINE,
        LV_VBELN_CAN LIKE LT_SUCCESS-BILL_DOC.

  CLEAR : LT_RETURN, LT_SUCCESS,
          LT_RETURN[], LT_SUCCESS[].


  CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
    EXPORTING
      BILLINGDOCUMENT = P_VBELN
      BILLINGDATE     = P_BUDAT
    TABLES
      RETURN          = LT_RETURN
      SUCCESS         = LT_SUCCESS.

  READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    READ TABLE LT_SUCCESS INDEX 1.
    IF SY-SUBRC = 0.
      CLEAR LV_VBELN_CAN.
      LV_VBELN_CAN = LT_SUCCESS-BILL_DOC.
      SET PARAMETER ID 'ZSDBILLCAC' FIELD LV_VBELN_CAN.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
