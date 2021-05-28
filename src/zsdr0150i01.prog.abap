*&---------------------------------------------------------------------*
*& Include          ZSDR0150I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE. " EXIT_9000 INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  CASE OK_CODE.
    WHEN 'CREATE'.
      IF P_CRE = 'X'.
        PERFORM CREATE_SO_BILLING.
      ELSEIF P_DISP = 'X'.
        PERFORM CREATE_BILLING.
      ENDIF.

    WHEN 'CAN_DOC'.
      PERFORM CANCEL_DOCUMENTS.

    WHEN 'SEND'.
      PERFORM SEND_DATA.

    WHEN 'UPDATE'.
      PERFORM UPDATE_DATA.
  ENDCASE.

ENDMODULE. " USER_COMMAND_9000 INPUT
