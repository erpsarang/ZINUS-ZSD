*&---------------------------------------------------------------------*
*& Include          ZSDR0090_I01
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
  SAVE_OK = OK_CODE.
  CASE SAVE_OK.
    WHEN 'EXEC'.
      GV_CHK = GV_CHK + 1.
      PERFORM RESERV_CREATE.
    WHEN 'DELT'.
      PERFORM RESERV_DELETE.
*    WHEN 'COMP'.
*      PERFORM RESERV_COMPLETE.
    WHEN 'TRAN'.
      PERFORM TRANS_POST.
    WHEN 'CHAN'.
      PERFORM RESERV_CHANGE.
    WHEN 'GIOT'. "GI others
      PERFORM GI_OTHERS.
  ENDCASE.
  CLEAR SAVE_OK.
ENDMODULE.                 " USER_COM
