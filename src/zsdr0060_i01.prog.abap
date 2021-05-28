*&---------------------------------------------------------------------*
*& Include          ZSDR0060_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE exit INPUT.
  save_ok = ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR save_ok.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CASE save_ok.
    WHEN TEXT-007. "SAVE
      PERFORM push_save.
  ENDCASE.
  CLEAR save_ok.
ENDMODULE.                 " USER_COM
