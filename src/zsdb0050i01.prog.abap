*&---------------------------------------------------------------------*
*& Include          ZSDB0050I01
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
      PERFORM CREATE_SO_TO_BILLING_NEW.
    WHEN 'CANCEL'.
      PERFORM CANCEL_DOCUMENTS.
    WHEN 'DELETE'.
      PERFORM DELETE_DATA.
  ENDCASE.

ENDMODULE. " USER_COMMAND_9000 INPUT
