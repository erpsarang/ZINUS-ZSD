*----------------------------------------------------------------------*
* Program ID  : ZSDB0051                                               *
* Title       : Create Sales Order to Billing                          *
* Author      : E00033                                                 *
* Create Date : 2020.10.14                                             *
* T_CODE      : ZSDB0051                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.10.14   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDB0051 MESSAGE-ID ZSD.

PARAMETERS : P_VBELN LIKE LIKP-VBELN,
             P_BUDAT LIKE LIKP-LFDAT.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  DATA : LT_MESG  LIKE TABLE OF MESG WITH HEADER LINE,
         LS_MKPF  LIKE EMKPF,
         LV_MBLNR TYPE MBLNR.

  CLEAR: LT_MESG, LT_MESG[],
         LS_MKPF.

  SELECT VBELN,
         VBTYP,
         WADAT_IST
      INTO TABLE @DATA(LT_DATA)
      FROM LIKP
      WHERE VBELN EQ @P_VBELN.
  IF SY-SUBRC = 0.

    LOOP AT LT_DATA INTO DATA(LS_DATA).

      CHECK P_BUDAT = LS_DATA-WADAT_IST.

      CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE'
        EXPORTING
          I_VBELN                   = LS_DATA-VBELN "P_VBELN
          I_BUDAT                   = P_BUDAT
          I_TCODE                   = 'VL09'
          I_VBTYP                   = LS_DATA-VBTYP
        IMPORTING
          ES_EMKPF                  = LS_MKPF
        TABLES
          T_MESG                    = LT_MESG
        EXCEPTIONS
          ERROR_REVERSE_GOODS_ISSUE = 1
          OTHERS                    = 2.

      IF SY-SUBRC = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        CLEAR LV_MBLNR.
        LV_MBLNR = LS_MKPF-MBLNR.
        SET PARAMETER ID 'ZSDGICAC' FIELD LV_MBLNR.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      CLEAR : LS_DATA.

    ENDLOOP.
  ENDIF.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
