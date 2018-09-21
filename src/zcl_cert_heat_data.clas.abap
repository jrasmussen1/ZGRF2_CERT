CLASS zcl_cert_heat_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA gv_sernr TYPE gernr .
    DATA gv_head_data TYPE zqcert_comp_data .
    DATA gv_entity TYPE zcl_z_heat_data_mpc=>ts_component .
    DATA gt_entity TYPE zcl_z_heat_data_mpc=>tt_component .

    METHODS constructor .
    METHODS get .
    METHODS validate
      IMPORTING
        VALUE(im_vendor_code) TYPE zvendor_code
        VALUE(im_heat_val)    TYPE zheat_data
      EXPORTING
        !ex_error             TYPE abap_bool .
    METHODS send_email
      IMPORTING
        !im_comp TYPE zqcert_comp_data .
    METHODS vendor_cert_exist
      IMPORTING
        VALUE(im_matnr)       TYPE matnr_d
        VALUE(im_vendor_code) TYPE zvendor_code
        VALUE(im_heat_data)   TYPE zheat_data
      RETURNING
        VALUE(re_found)       TYPE abap_bool .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gs_tdwa TYPE tdwa .
    CONSTANTS gc_cer TYPE dokar VALUE 'CER' ##NO_TEXT.
    DATA:
      gt_formats TYPE TABLE OF zqcert_format .

    METHODS format_error
      IMPORTING
        VALUE(im_heat_data)    TYPE zheat_data
        VALUE(im_format)       TYPE zcheckformat
      RETURNING
        VALUE(re_format_error) TYPE abap_bool .
ENDCLASS.



CLASS ZCL_CERT_HEAT_DATA IMPLEMENTATION.


  METHOD constructor.

    CHECK 1 = 1.

*    me->gv_sernr = im_sernr.
*    me->load_formats( ).


*  Test data
*   gv_entity-comp_no = '001'.
*   gv_entity-comp_no = '001'.
*
*gv_entity-SERNR.
*gv_entity-COMP_NO.
*gv_entity-COMP_TXT.
*gv_entity-MATNR.
*gv_entity-LIFNR.
*gv_entity-HEAT_DATA.

  ENDMETHOD.


  METHOD format_error.

    DATA: lv_char TYPE char1,
          lv_type TYPE char1.

    re_format_error = abap_false.

*   Check number of characters.
    IF strlen( im_heat_data ) NE strlen( im_format ).
      re_format_error = abap_true.
      RETURN.
    ENDIF.

    DATA(lv_len) = -1.

*   Validate each character
    DO strlen( im_heat_data ) TIMES.
      lv_len  = lv_len + 1.
      lv_char = im_heat_data+lv_len(1).
      lv_type = im_format+lv_len(1).

      CASE lv_type.
        WHEN '9'.
          IF lv_char NOT BETWEEN '0' AND '9'.
            re_format_error = abap_true.
            RETURN.
          ENDIF.
        WHEN 'A'.
          IF lv_char NOT BETWEEN 'A' AND 'Z'.
            re_format_error = abap_true.
            RETURN.
          ENDIF.
        WHEN 'X'.
          "Can be all characters....
        WHEN OTHERS.
          IF lv_char NE lv_type.
            re_format_error = abap_true.
            RETURN.
          ENDIF.
      ENDCASE.
    ENDDO.

  ENDMETHOD.


  METHOD get.

    CHECK 1 = 1.

*    DATA: lv_sernr TYPE gernr.
*
*    CLEAR gt_entity[].
*
** Move something to sernr.
**   lv_sernr =
*
*
**   Get heat data for serial number
*    SELECT *
*      FROM zqcert_comp_data
*      INTO CORRESPONDING FIELDS OF TABLE gt_entity
*      WHERE sernr = gv_sernr.
*
**   Read component text
*    LOOP AT gt_entity ASSIGNING FIELD-SYMBOL(<l_entity>).
*      SELECT SINGLE comp_txt
*        INTO <l_entity>-comp_txt
*        FROM zqcert_comp
*        WHERE comp_no = <l_entity>-comp_no
*          AND spras = sy-langu.                          "To be changed.
*
*      IF sy-subrc NE 0.
**       message E999...   Should never happen
*      ENDIF.
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD send_email.

    DATA: lt_recipient TYPE TABLE OF zemail,
          lv_email     TYPE ad_smtpadr. " Email ID

    DATA: lo_send_request TYPE REF TO cl_bcs VALUE IS INITIAL,
          lt_message_body TYPE bcsy_text VALUE IS INITIAL,
          lo_document     TYPE REF TO cl_document_bcs VALUE IS INITIAL,
          lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL.

    DATA: lv_result  TYPE abap_bool,
          lv_matnr   TYPE matnr,
          lv_maktx   TYPE maktx,
          lv_nametx  TYPE ad_namtext,
          lv_subject TYPE so_obj_des.

    DATA: lo_document_bcs TYPE REF TO cx_document_bcs VALUE IS INITIAL.

*   Get the email address to send mail to
    SELECT email
      INTO TABLE lt_recipient
       FROM zqcert_email
       WHERE werks = im_comp-werks.

    IF sy-subrc NE 0 OR lines( lt_recipient ) = 0.
      RETURN.
    ENDIF.

*   Add leading zeroes from material number
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = im_comp-matnr
      IMPORTING
        output = lv_matnr.

*   Read material text
    SELECT SINGLE maktx
      INTO lv_maktx
      FROM makt
      WHERE matnr = lv_matnr
        AND spras = sy-langu.

    IF sy-subrc NE 0.
      CLEAR lv_maktx.
    ENDIF.

*   Read user name
    SELECT name_textc
      INTO lv_nametx
      FROM user_addr UP TO 1 ROWS
      WHERE bname = sy-uname.
    ENDSELECT.

    IF sy-subrc NE 0.
      CLEAR lv_nametx.
    ENDIF.

*   Create mail content
    APPEND 'Dear certificate team' TO lt_message_body.
    APPEND ' ' TO lt_message_body.
    APPEND 'The following raw material certificates was not accessible in the database. Please update the database with the following certificates:' TO lt_message_body.
    APPEND ' ' TO lt_message_body.
    APPEND 'Vendor:' && | | && im_comp-vendor_code TO lt_message_body.
    APPEND 'Material:' && | | && im_comp-matnr && | | && lv_maktx TO lt_message_body.
    APPEND 'Heat number:' && | | && im_comp-heat_data TO lt_message_body.
    APPEND 'User:' && | | && lv_nametx TO lt_message_body.
    APPEND ' ' TO lt_message_body.
    APPEND 'This email is auto-generated and cannot be replied.' TO lt_message_body.
    APPEND ' ' TO lt_message_body.

*   Define subject of e-mail
    lv_subject = 'Certificate missing for' && | | && im_comp-sernr.

    TRY.
*   Create persistent send request
        lo_send_request = cl_bcs=>create_persistent( ).

        lo_document = cl_document_bcs=>create_document(
          i_type = 'RAW'
          i_text = lt_message_body
          i_subject = lv_subject ).

* Pass the document to send request
        lo_send_request->set_document( lo_document ).

*Add Recipients (E-MAIL ADDRESS)
        LOOP AT lt_recipient ASSIGNING FIELD-SYMBOL(<l_recipient>).
          lv_email = <l_recipient>.
          lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).

          lo_send_request->add_recipient(
            EXPORTING
              i_recipient = lo_recipient
              i_express = 'X' ).
        ENDLOOP.

* Trigger E-Mail immediately
        lo_send_request->set_send_immediately( 'X' ).    "This will bypass SOST!!!???

* Send email
        lo_send_request->send(
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result = lv_result ).

      CATCH cx_send_req_bcs.
      CATCH cx_root.

    ENDTRY.


  ENDMETHOD.


  METHOD validate.

    DATA: lv_format1 TYPE zcheckformat,
          lv_format2 TYPE zcheckformat,
          lv_format3 TYPE zcheckformat,
          lv_error2  TYPE abap_bool,
          lv_error3  TYPE abap_bool.

    SELECT checkrule
      INTO @DATA(lv_chrule)
      FROM zqcert_vendor UP TO 1 ROWS
      WHERE vendor_code = @im_vendor_code.
    ENDSELECT.

    IF sy-subrc NE 0 OR lv_chrule IS INITIAL.
      RETURN.  "If no check rule then nothing to check.
    ENDIF.

    SELECT SINGLE format1 format2 format3
         FROM zqcert_format
         INTO ( lv_format1, lv_format2, lv_format3 )
         WHERE format_key = lv_chrule.

    IF sy-subrc NE 0.
      "Message - error should never happen
      "Stop program
      RETURN.
    ENDIF.

**   Set default value to no error
*    ex_error = abap_false.

*   Check format 1 - will always be there
    DATA(lv_error1) = me->format_error(
                          EXPORTING
                           im_heat_data = im_heat_val
                           im_format    = lv_format1 ).

*   Check format 2
    IF lv_format2 IS INITIAL.
      lv_error2 = abap_true.
    ELSE.
      lv_error2 = me->format_error(
                      EXPORTING
                        im_heat_data = im_heat_val
                        im_format    = lv_format2 ).
    ENDIF.

*   Check format 3
    IF lv_format3 IS INITIAL.
      lv_error3 = abap_true.
    ELSE.
      lv_error3 = me->format_error(
                      EXPORTING
                        im_heat_data = im_heat_val
                        im_format    = lv_format3 ).
    ENDIF.

    IF lv_error1 = abap_false OR
       lv_error2 = abap_false OR
       lv_error3 = abap_false.
      ex_error = abap_false.
    ELSE.
      ex_error = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD vendor_cert_exist.

    DATA: lt_cltext TYPE TABLE OF cltext,
          ls_cltext TYPE cltext,
          lt_zksml  TYPE TABLE OF zksml,
          ls_zksml  TYPE zksml,
          lt_ccomw  TYPE TABLE OF comw,
          ls_ccomw  TYPE comw,
          ls_origin TYPE cv100_radio_buttons,
          lt_draw   TYPE TABLE OF draw,
          ls_draw   TYPE draw,
          lv_cadkz  TYPE draw-cadkz.

    DATA: lt_dokar TYPE TABLE OF cv100_rangesdokar,
          ls_dokar TYPE cv100_rangesdokar.

    re_found = abap_false.

*   Find classification data for vendor certificates (CER)
    SELECT * INTO gs_tdwa FROM tdwa UP TO 1 ROWS
      WHERE dokar = gc_cer.
    ENDSELECT.

    CHECK sy-subrc = 0.

    CLEAR: lt_cltext[].
    ls_cltext-class = gs_tdwa-klasse.
    ls_cltext-klart = gs_tdwa-klassenart.
    APPEND ls_cltext TO lt_cltext.

    CALL FUNCTION 'CLMA_CLASS_TEXT'
      EXPORTING
        classtype         = gs_tdwa-klassenart
        language          = sy-langu
        mode              = 'E'
      TABLES
        tklas             = lt_cltext
      EXCEPTIONS
        classtype_missing = 1.

    CHECK sy-subrc = 0.

*   Class data for certificates
    READ TABLE lt_cltext INTO ls_cltext INDEX 1.
    IF sy-subrc = 0.

      SELECT * INTO TABLE lt_zksml
        FROM zksml
        WHERE clint = ls_cltext-clint
        AND   klart = ls_cltext-klart
        AND   class = ls_cltext-class.
      IF sy-subrc = 0.

        CLEAR lt_ccomw[].
        LOOP AT lt_zksml INTO ls_zksml.
          IF im_matnr IS NOT INITIAL AND ls_zksml-atnam = 'MAT_NUM'.
            CLEAR ls_ccomw.
            MOVE-CORRESPONDING ls_zksml TO ls_ccomw.
            WRITE im_matnr TO ls_ccomw-atwrt NO-ZERO.
            CONDENSE ls_ccomw-atwrt.
            ls_ccomw-atcod = 1.
            ls_ccomw-slcod = 1.
            ls_ccomw-statu = 'H'.
            ls_ccomw-atfor = 'CHAR'.
            APPEND ls_ccomw TO lt_ccomw.
          ELSEIF im_heat_data IS NOT INITIAL AND ls_zksml-atnam = 'HEAT_NUM'.
            CLEAR ls_ccomw.
            MOVE-CORRESPONDING ls_zksml TO ls_ccomw.
            WRITE im_heat_data TO ls_ccomw-atwrt NO-ZERO.
            CONDENSE ls_ccomw-atwrt.
            ls_ccomw-atcod = 1.
            ls_ccomw-slcod = 1.
            ls_ccomw-statu = 'H'.
            ls_ccomw-atfor = 'CHAR'.
            APPEND ls_ccomw TO lt_ccomw.
          ELSEIF im_vendor_code IS NOT INITIAL AND ls_zksml-atnam = 'SUPPL_CODE'.
            CLEAR ls_ccomw.
            MOVE-CORRESPONDING ls_zksml TO ls_ccomw.
            WRITE im_vendor_code TO ls_ccomw-atwrt NO-ZERO.
            CONDENSE ls_ccomw-atwrt.
            ls_ccomw-atcod = 1.
            ls_ccomw-slcod = 1.
            ls_ccomw-statu = 'H'.
            ls_ccomw-atfor = 'CHAR'.
            APPEND ls_ccomw TO lt_ccomw.
          ENDIF.
        ENDLOOP.

        ls_origin-all_originals = 'X'.

        ls_dokar-sign   = 'I'.
        ls_dokar-option = 'EQ'.
        ls_dokar-low    = gc_cer.
        APPEND ls_dokar TO lt_dokar.

*       Find documents for specified classification fields
        CALL FUNCTION 'CV100_DOCUMENT_SEARCH'
          EXPORTING
            max_rows             = 100
            classno              = gs_tdwa-klasse
            classtype            = gs_tdwa-klassenart
            pf_cs_active         = 'X'
            originals            = ls_origin
          IMPORTING
            more_documents       = lv_cadkz
          TABLES
            tdraw                = lt_draw
            stdokar              = lt_dokar
            clsc_class_selection = lt_ccomw
          EXCEPTIONS
            no_result            = 1
            bad_query            = 2
            not_authorized       = 3
            OTHERS               = 4.

*       Check if there is a released document
        IF sy-subrc = 0.
          LOOP AT lt_draw INTO ls_draw WHERE dokst = 'RE'.
            re_found = abap_true.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
