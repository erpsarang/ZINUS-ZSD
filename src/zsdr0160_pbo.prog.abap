*&---------------------------------------------------------------------*
*& Include          ZSDR0160_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE DISPLAY_0100 OUTPUT.

  IF GCL_DOCKING1 IS INITIAL.
    PERFORM SET_OBJECTS_0100 USING GCL_DOCKING1 GCL_ALV1.
    PERFORM FILL_FIELD_CATEGORY.
    PERFORM SET_EXCLUDE_TOOLBAR.
    PERFORM SET_LAYOUT USING GS_LAYO1.
    PERFORM SET_VARIANT.
    PERFORM CREATE_EVENT_RECEIVER USING GCL_ALV1.
    PERFORM SET_DISPLAY USING GCL_ALV1 GS_LAYO1 GT_TOOLBAR1[]
                              'GT_DISP[]' GT_FCAT1[] GT_SORT1[].
  ELSE.
    PERFORM REFRESH_TABLE_DISPLAY USING GCL_ALV1.
  ENDIF.

ENDMODULE.
