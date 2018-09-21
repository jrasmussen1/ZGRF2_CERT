*----------------------------------------------------------------------
* Title           :   Maintain ZQCERT_CUSTM
* Reference       :   Digital Certificates
* Developer       :   GMASELU
* Funct. Analyst  :   (GMAHNU) Helene Ulsøe
* Create Date     :   12.03.2018
* Description     :   Maintain table ZQCERT_CUSTM
*-------------------------MODIFICATION LOG-----------------------------
* Date            Developer  Funct. Analyst Correction Description
* <YYYY.MM.DD>    <Initials> <Initials>     CRXXXXXXXX <Description>
*----------------------------------------------------------------------
REPORT zcert_custm_maintain MESSAGE-ID sv.


TABLES zqcert_custm.


CONTROLS gt_tc TYPE TABLEVIEW USING SCREEN 0100.


TYPES:
  BEGIN OF ty_value,
    mandt	LIKE zqcert_custm-mandt,
    matnr	LIKE zqcert_custm-matnr,
    kunnr LIKE zqcert_custm-kunnr,
    name1	LIKE zqcert_custm-name1,
    land1	LIKE zqcert_custm-land1,
    ernam	LIKE zqcert_custm-ernam,
    erdat	LIKE zqcert_custm-erdat,
    check TYPE c,

  END OF ty_value.


DATA:
  gt_value      TYPE TABLE OF ty_value,
  gt_custm      TYPE TABLE OF zqcert_custm,
  gt_exfunc     TYPE TABLE OF sy-ucomm,
  gs_value      LIKE LINE OF gt_value,
  gs_custm      TYPE zqcert_custm,

  gv_okcode     TYPE ok,
  gv_flg_change TYPE c,
  gv_flg_dsch   TYPE c,
  gv_first_call TYPE c,
  gv_input      TYPE c,
  gv_lines      TYPE i.



SELECT-OPTIONS:
s_matnr FOR zqcert_custm-matnr,
s_kunnr FOR zqcert_custm-kunnr.


START-OF-SELECTION.

  REFRESH gt_value.

  SELECT * INTO TABLE gt_custm FROM zqcert_custm
   WHERE matnr IN s_matnr
     AND kunnr IN s_kunnr.
*     AND ernam IN s_ernam.
  IF sy-subrc <> 0.
    CLEAR sy-subrc.
  ENDIF.

  LOOP AT gt_custm INTO gs_custm.
    MOVE-CORRESPONDING gs_custm TO gs_value.
    APPEND gs_value TO gt_value.
  ENDLOOP.

  SORT gt_value BY matnr kunnr.

  gv_first_call = 'X'.
  gv_flg_dsch   = 'D'.
  gv_input      = '1'.
  APPEND 'DELT' TO gt_exfunc.

  CALL SCREEN 0100.






*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100' EXCLUDING gt_exfunc[].
  SET TITLEBAR '0100'.

  IF gv_first_call = 'X'.
    gv_input = '0'.
    CLEAR gv_first_call.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  PERFORM user_command_100.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  CASE gv_okcode.
    WHEN 'EXIT' OR 'CANL'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  GO_BACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM go_back .

  DATA: lv_answ TYPE c.

  IF gv_flg_change = 'X'.     "Data changed without save
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_button_2 = 'Save&Leave'(t01)
        titlebar      = 'Process Lines'(t02)
        text_question = 'Are you sure to quit? All unsaved changes will be lost!'(qt1)
      IMPORTING
        answer        = lv_answ.
    IF sy-subrc <> 0.
    ENDIF.

    CASE lv_answ.
      WHEN '1'.         "Yes
        CLEAR: gv_flg_change.
        LEAVE TO SCREEN 0.
      WHEN '2'.         "Save&Leave
        PERFORM save_data.
        LEAVE TO SCREEN 0.

    ENDCASE.

  ELSE.         "No data changed
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data .

  CLEAR gv_flg_change.
  DELETE zqcert_custm FROM TABLE gt_custm.

  CHECK sy-subrc = 0.

  REFRESH gt_custm.
  LOOP AT gt_value INTO gs_value.
    MOVE-CORRESPONDING gs_value TO gs_custm.
    APPEND gs_custm TO gt_custm.
  ENDLOOP.

  INSERT zqcert_custm FROM TABLE gt_custm.
  IF sy-subrc = 0.
    MESSAGE s018.
  ENDIF.
  LEAVE TO SCREEN 0100.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SWITCH_DISPLAY_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_display_change .

  IF gv_flg_dsch = 'D'.   "Display -> Change
    gv_flg_dsch = 'U'.
    gv_input = '1'.
    DESCRIBE TABLE gt_value LINES gt_tc-lines.
    ADD 27 TO gt_tc-lines.

    REFRESH gt_exfunc.

  ELSEIF gv_flg_dsch = 'U'.   "Change -> Display
    gv_flg_dsch = 'D'.
    gv_input = '0'.
    APPEND 'DELT' TO gt_exfunc.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  READ_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_value OUTPUT.

  PERFORM read_line_value.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHANGE_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_value INPUT.

  PERFORM change_line_value.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  DELETE_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_entry .

  DELETE gt_value WHERE check = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_check INPUT.

  PERFORM change_line_value_check.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr INPUT.

  PERFORM check_matnr.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_KUNNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_kunnr INPUT.

  PERFORM check_kunnr.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  READ_LINE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_line_value .

  CLEAR gs_value.

  READ TABLE gt_value INTO gs_value INDEX gt_tc-current_line.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING gs_value TO zqcert_custm.

    LOOP AT SCREEN.
      IF screen-name = 'ZQCERT_CUSTM-MATNR' OR
         screen-name = 'ZQCERT_CUSTM-KUNNR'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ELSE.
    CLEAR zqcert_custm.
    LOOP AT SCREEN.
      IF screen-name = 'ZQCERT_CUSTM-MATNR' OR
         screen-name = 'ZQCERT_CUSTM-KUNNR'.
        screen-input = gv_input.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_100 .
  DATA lv_okcode TYPE ok.

  lv_okcode = gv_okcode.
  CLEAR gv_okcode.

  CASE lv_okcode.
    WHEN 'BACK'.
      PERFORM go_back.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'DSCH'.
      PERFORM switch_display_change.
    WHEN 'NEW'.
      REFRESH gt_exfunc.
      gv_input = '1'.
      DESCRIBE TABLE gt_value LINES gt_tc-lines.
      gt_tc-top_LINE = gt_tc-lines + 1.
      ADD 25 TO gt_tc-lines.
    WHEN 'DELT'.
      PERFORM delete_entry.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr .
  DATA lv_matnr    TYPE matnr.

  IF zqcert_custm-matnr IS NOT INITIAL.
    SELECT SINGLE matnr INTO lv_matnr FROM mara
     WHERE matnr = zqcert_custm-matnr.
    IF sy-subrc <> 0.
      MESSAGE e305(m3) WITH zqcert_custm-matnr.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_LINE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_line_value .
  DATA: ls_value    LIKE LINE OF gt_value,
        ls_custm    TYPE zqcert_custm,
        lv_adrnr    LIKE adrc-addrnumber,
        lv_addr_sel TYPE addr1_sel,
        lv_addr     TYPE addr1_val,
        lv_return   LIKE szad_field-returncode.


  CHECK zqcert_custm-matnr IS NOT INITIAL.
  CHECK zqcert_custm-kunnr IS NOT INITIAL.

  READ TABLE gt_value INTO ls_value WITH KEY matnr = zqcert_custm-matnr
                                             kunnr = zqcert_custm-kunnr.
  IF sy-subrc = 0.
    MESSAGE e009.
  ELSE.
    READ TABLE gt_custm INTO ls_custm
    WITH KEY matnr = zqcert_custm-matnr
             kunnr = zqcert_custm-kunnr.
    IF sy-subrc <> 0.
      SELECT SINGLE * INTO ls_custm FROM zqcert_custm
       WHERE matnr = zqcert_custm-matnr AND
             kunnr = zqcert_custm-kunnr.
      IF sy-subrc = 0.
        MESSAGE e009.
      ENDIF.
    ENDIF.

  ENDIF.

  gs_value-mandt = sy-mandt.
  gs_value-matnr = zqcert_custm-matnr.
  gs_value-kunnr = zqcert_custm-kunnr.
  gs_value-ernam = sy-uname.
  gs_value-erdat = sy-datum.

*  Check if an international address exist

*  SELECT adrnr INTO lv_adrnr FROM vbpa UP TO 1 ROWS    "Removed JRA 22.03.2018
*   WHERE parvw = 'AG'
*     AND kunnr = zqcert_custm-kunnr
*    .
*  ENDSELECT.

  SELECT SINGLE adrnr                                   "Added JRA 22.03.2018
    INTO lv_adrnr
    FROM kna1
    WHERE kunnr = zqcert_custm-kunnr.

  IF sy-subrc = 0.
    SELECT COUNT( * )
      FROM adrc
      WHERE addrnumber = lv_adrnr
        AND nation = 'I'.

    IF sy-subrc = 0 AND sy-dbcnt = 1.
      lv_addr_sel-nation = 'I'.
    ELSE.
      lv_addr_sel-nation = ''.
    ENDIF.

    lv_addr_sel-addrnumber = lv_adrnr.

    CALL FUNCTION 'ADDR_GET'
      EXPORTING
        address_selection = lv_addr_sel
      IMPORTING
        address_value     = lv_addr     " Return data for an address
        returncode        = lv_return   " Return code: ' '(ok), 'I'nfo, 'W'arning, 'E'rror
      EXCEPTIONS
        parameter_error   = 1
        address_not_exist = 2
        version_not_exist = 3
        internal_error    = 4
        address_blocked   = 5
        OTHERS            = 6.
    IF sy-subrc = 0.
      CONCATENATE lv_addr-name1 lv_addr-name2 INTO gs_value-name1
      SEPARATED BY space.
    ENDIF.

  ENDIF.


  MODIFY gt_value FROM gs_value INDEX gt_tc-current_line.
  IF sy-subrc <> 0.
    APPEND gs_value TO gt_value.
  ENDIF.

  gv_flg_change = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_LINE_VALUE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_line_value_check .
  DATA: lv_matnr TYPE matnr,
        ls_value LIKE LINE OF gt_value.

  READ TABLE gt_value INTO ls_value WITH KEY matnr = zqcert_custm-matnr
                                             kunnr = zqcert_custm-kunnr
                                             BINARY SEARCH.
  IF sy-subrc = 0.
    ls_value-check = gs_value-check.
    MODIFY gt_value FROM ls_value INDEX gt_tc-current_line
    TRANSPORTING check .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_KUNNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_kunnr .

  IF zqcert_custm-kunnr IS NOT INITIAL.
    SELECT SINGLE land1 INTO gs_value-land1
      FROM kna1 WHERE kunnr = zqcert_custm-kunnr.
    IF sy-subrc <> 0.
      MESSAGE e153(f2) WITH zqcert_custm-kunnr.
    ENDIF.
  ENDIF.

ENDFORM.
