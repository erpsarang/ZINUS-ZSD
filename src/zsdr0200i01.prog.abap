*&---------------------------------------------------------------------*
*& Include          ZSDR0200I01
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
    WHEN 'APPLY'.
      PERFORM APPLY_PRICE.
    WHEN 'REFRESH'.
      PERFORM GET_DATA.
  ENDCASE.

ENDMODULE. " USER_COMMAND_9000 INPUT
