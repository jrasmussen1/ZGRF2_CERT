*----------------------------------------------------------------------*
***INCLUDE LZCERTI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_default INPUT.
  CASE gv_okcode.
    WHEN 'CANCEL'.
      CLEAR: gv_data, gv_continue.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CONTINUE'.
      gv_continue = 'X'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE gv_okcode.
    WHEN 'CANCEL'.
      CLEAR: gv_data, gv_continue.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CONTINUE'.
      IF gv_rb_treat1 = 'X'.
        gv_data-surface = ' '.
      ELSE.
        gv_data-surface = 'X'.
      ENDIF.
      gv_continue = 'X'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.

MODULE user_command_0200 INPUT.
  CASE gv_okcode.
    WHEN 'CANCEL'.
      CLEAR: gv_data, gv_continue.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CONTINUE'.
      IF gv_cb_name2 = 'X'.
        gv_header-sold_to_name = gv_header-name1 && | | && gv_header-name2.
      ELSE.
        gv_header-sold_to_name = gv_header-name1.
      ENDIF.

      READ TABLE gt_docs WITH KEY fname = space.
      IF sy-subrc = 0.
        MESSAGE e999(zm) WITH 'File for the certificate is missing'.
        EXIT.
      ENDIF.

      gv_continue = 'X'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  CHECK_CRN_BASE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_crn_base INPUT.

  CHECK gv_okcode NE 'CANCEL'.
  CHECK gv_data-crn_base IS NOT INITIAL.

** READ TABLE gt_bom_mat ASSIGNING FIELD-SYMBOL(<g_bom_mat>) WITH KEY gv_data-crn_base BINARY SEARCH.
*  READ TABLE gt_bom_mat WITH KEY gv_data-crn_base BINARY SEARCH TRANSPORTING NO FIELDS.
*  IF sy-subrc NE 0.
*    MESSAGE e999(zm) WITH 'CRN base material does not exist in BOM'.
*  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_MOTOR_PN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_motor_pn INPUT.

  CHECK gv_okcode NE 'CANCEL'.
  CHECK gv_atex-motor_pn IS NOT INITIAL.

  READ TABLE gt_bom_mat WITH KEY gv_atex-motor_pn BINARY SEARCH TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    MESSAGE e999(zm) WITH 'Motor part number does not exist in BOM'.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDATE_LANGU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validate_langu INPUT.
  CHECK gv_okcode NE 'CANCEL'.

  IF gv_langu NE 'EN'.   "All forms must exist in English

*   Convert language code from 2 to 1 character
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = gv_langu
      IMPORTING
        output           = gv_header-langu
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
*     Implement suitable error handling here
*     This should never happen
      CHECK 1 = 1.
    ENDIF.

    SELECT COUNT( * )
      FROM zqcert_forms
      WHERE langu = gv_header-langu
        AND matnr = gv_header-material.

    IF sy-subrc = 4.
      MESSAGE e998(zm) WITH 'Form does not exist in language' gv_langu.
    ENDIF.
  ELSE.
    gv_header-langu = 'EN'.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0250  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0250 INPUT.

  CASE sy-ucomm.
    WHEN 'CLICK'.
      GET CURSOR FIELD gv_field VALUE gv_value.

*     Show PDF form.
      IF gv_field = 'GT_DOCS-FNAME' AND gv_value IS NOT INITIAL.
        LOOP AT gt_docs ASSIGNING FIELD-SYMBOL(<g_doc>).
          IF <g_doc>-fname = gv_value.
            WRITE icon_okay AS ICON TO <g_doc>-status.   "Set status to shown
            gwa_docs = <g_doc>.
          ENDIF.
        ENDLOOP.

        PERFORM get_doc_file USING gwa_docs.
      ENDIF.

  ENDCASE.
ENDMODULE.
