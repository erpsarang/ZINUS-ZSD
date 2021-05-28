*&---------------------------------------------------------------------*
*& Include          ZSDR0030_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.
  SAVE_OK = OK_CODE.
  CASE SAVE_OK.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR SAVE_OK.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
*  SAVE_OK = OK_CODE.
*
*  CLEAR SAVE_OK.
ENDMODULE.                 " USER_COM
