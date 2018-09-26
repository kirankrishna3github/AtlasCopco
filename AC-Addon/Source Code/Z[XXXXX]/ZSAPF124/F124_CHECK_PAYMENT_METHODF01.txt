*----------------------------------------------------------------------*
***INCLUDE F124_CHECK_PAYMENT_METHODF01 .
*----------------------------------------------------------------------*
FORM CHECK_PAYMENT_METHOD  USING    P_ZLSCH LIKE T042Z-ZLSCH
                                    P_BUKRS LIKE T001-BUKRS
                           CHANGING P_IGNORE_DOC.
  DATA: LD_PMT_TYPE TYPE C.

  CHECK GD_EBPP_ACTIVE = 'X'.
  CLEAR P_IGNORE_DOC.
  CALL FUNCTION 'APAR_EBPP_TYPE_PAYMENT_METHOD'             "#EC *
  EXPORTING
    I_ZLSCH                      = P_ZLSCH
*       I_LAND1                      =
*       I_ZBUKR                     =
    I_BUKRS                      = P_BUKRS
  IMPORTING
    E_TYPE                       = LD_PMT_TYPE
  EXCEPTIONS
    INVALID_CALL                 = 1
    NO_PAYING_COMPANY_CODE       = 2
    OTHERS                       = 3.
*----- Ignore documents with EBPP-payment method
  IF SY-SUBRC <> 0 OR LD_PMT_TYPE NE SPACE.
    P_IGNORE_DOC = 'X'.
    gd_EBPP_mess = 'X'.
  ENDIF.

ENDFORM.                    " check_payment_method
