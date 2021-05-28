*----------------------------------------------------------------------*
***INCLUDE LZSDFG_0007F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form JOB_OPEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_JOBNAME
*&      --> LV_JOBC
*&---------------------------------------------------------------------*
FORM JOB_OPEN  USING    P_JOBNAME P_JOBC
                        P_E_RETURN TYPE BAPIRET2.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      JOBNAME          = P_JOBNAME
    IMPORTING
      JOBCOUNT         = P_JOBC
    EXCEPTIONS
      CANT_CREATE_JOB  = 01
      INVALID_JOB_DATA = 02
      JOBNAME_MISSING  = 03
      OTHERS           = 04.

  IF SY-SUBRC NE 0.
    P_E_RETURN-TYPE = 'E'.
    P_E_RETURN-TYPE = 'Job Open Error'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUBMIT_PROG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_JOBNAME
*&      --> LV_JOBC
*&      --> I_VBELN
*&---------------------------------------------------------------------*
FORM SUBMIT_PROG  USING    P_JOBNAME
                           P_JOBC
                           PV_VBELN
                           P_RETURN.

  SUBMIT ZSDR0052  VIA JOB P_JOBNAME
                   NUMBER  P_JOBC
                   WITH    P_VBELN  = PV_VBELN
                   AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLOSE_JOB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_JOBNAME
*&      --> LV_JOBC
*&      --> TARGET_DATE
*&      --> TARGET_TIME
*&---------------------------------------------------------------------*
FORM CLOSE_JOB  USING    P_JOBNAME
                         P_JOBC
                         P_DATE
                         P_TIME
                         P_E_RETURN TYPE BAPIRET2.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      JOBNAME              = P_JOBNAME
      JOBCOUNT             = P_JOBC
      SDLSTRTDT            = P_DATE
      SDLSTRTTM            = P_TIME
    EXCEPTIONS
      CANT_START_IMMEDIATE = 01
      INVALID_STARTDATE    = 02
      JOBNAME_MISSING      = 03
      JOB_CLOSE_FAILED     = 04
      JOB_NOSTEPS          = 05
      JOB_NOTEX            = 06
      LOCK_FAILED          = 07
      OTHERS               = 08.

  IF SY-SUBRC NE 0.
    P_E_RETURN-TYPE = 'E'.
    P_E_RETURN-TYPE = 'Job Close Error'.
  ENDIF.
ENDFORM.
