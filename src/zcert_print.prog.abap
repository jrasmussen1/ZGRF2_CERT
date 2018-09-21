*----------------------------------------------------------------------
* Title           :   Create certificates
* Reference       :   P1722380 Digital Certificates
* Developer       :   (GMAJRA) Jacob Rasmussen
* Funct. Analyst  :   (GMAHNU) Helene Ulsøe
* Create Date     :   Nov. 2017
* Description     :   P1722380 Digital Certificates
*                     Read data from sales order and material classification
*                     and calls Adobe forms that generates certificates.
*-------------------------MODIFICATION LOG-----------------------------
* Date            Developer  Funct. Analyst Correction Description
* <YYYY.MM.DD>    <Initials> <Initials>     CRXXXXXXXX <Description>
*----------------------------------------------------------------------
REPORT zcert_print.

DATA: go_cert TYPE REF TO zcl_certificate.

DATA: gt_so_tab          TYPE zttcert_so_item,
      gv_so_item         TYPE zscert_so_item,
      gv_header          TYPE zscert_header,
      gv_cert_pump       TYPE zscert_pump_char,
      gt_cert_comp       TYPE zttcert_comp_data,
      gt_cert_comp_extra TYPE zttcert_comp_data_extra,
      gv_roughness       TYPE zscert_roughness,
      gv_atex            TYPE zscert_atex,
      gv_vibration       TYPE zscert_vibration,
      gv_send_mail       TYPE xfeld.

DATA: go_exc         TYPE REF TO zcx_grf_qm_cert,       "Exceptions
      gv_exc_info    TYPE zexc_info,
      gv_exc_options TYPE zexc_options.

DATA: gv_pdf_output TYPE fpformoutput.

DATA: gt_doc      TYPE zcl_certificate=>ty_doc_tab,
      gt_miss_doc TYPE zcl_certificate=>ty_miss_doc_tab.

DATA: go_alv       TYPE REF TO cl_salv_table.


* Class definition to handle double-click in ALV list
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      on_click_send   FOR EVENT added_function OF cl_salv_events.
ENDCLASS.                    "lcl_handle_events DEFINITION



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_werks TYPE vbrp-werks OBLIGATORY,         "Plant
            p_vbeln TYPE vbap-vbeln OBLIGATORY.         "Sales order number

SELECT-OPTIONS: s_posnr FOR gv_so_item-posnr.           "Sales order item
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_aufnr TYPE viqmel-aufnr.                   "Service order number
PARAMETERS: p_refno TYPE vbap-vbeln.                     "Reference sales order
SELECTION-SCREEN END OF BLOCK b2.



INITIALIZATION.
  gv_exc_options-include_user_info = 'X'.   "Default setting for log entries

  PERFORM set_default_plant CHANGING p_werks.   "Set default value for plant

AT SELECTION-SCREEN.
* Check authorization to plant
  PERFORM check_plant.

* Check order exist and authorization to sales org.
  PERFORM check_sales_order USING p_vbeln.

AT SELECTION-SCREEN ON BLOCK b2.
  IF p_aufnr IS NOT INITIAL AND p_refno IS NOT INITIAL.
    MESSAGE i999(zm) WITH 'Do not enter both service and sales order'.
  ENDIF. .

  IF p_aufnr IS INITIAL AND p_refno IS INITIAL.
    PERFORM check_sold_to USING p_vbeln.
  ENDIF.


* Check service order number
AT SELECTION-SCREEN ON p_aufnr.
  IF p_aufnr IS NOT INITIAL.
    PERFORM check_service_order.
  ENDIF.


* Check reference sales order
AT SELECTION-SCREEN ON p_refno.
  IF p_refno IS NOT INITIAL.
    PERFORM check_sales_order USING p_refno.
  ENDIF.


AT SELECTION-SCREEN ON p_vbeln.
  CREATE OBJECT go_cert
    EXPORTING
      im_werks = p_werks
      im_vbeln = p_vbeln.

  go_cert->lock_sales_order( im_vbeln = p_vbeln ).


START-OF-SELECTION.

  go_cert->get_so_item(
   EXPORTING
     im_werks     = p_werks         " Plant
     im_vbeln     = p_vbeln         " Sales and Distribution Document Number
     im_posnr     = s_posnr[]       " Item number
    IMPORTING
      ex_so_item = gt_so_tab ).     " Certificate item for sales order

  IF gt_so_tab[] IS INITIAL.
    MESSAGE i997(zm) WITH 'No certificate material numbers found for' 'sales order' p_vbeln.
    EXIT.
  ENDIF.

  go_cert->update_item_w_doc_info( CHANGING ch_so_item = gt_so_tab ).


* Show ALV list
  PERFORM so_item_alv.



FORM so_item_alv.

  DATA: lo_functions    TYPE REF TO cl_salv_functions_list.

  DATA: lo_columns      TYPE REF TO cl_salv_columns_table.
  DATA: lo_col          TYPE REF TO cl_salv_column.

  DATA: lo_events       TYPE REF TO cl_salv_events_table.
  DATA: lo_events_handl TYPE REF TO lcl_handle_events.

  DATA: lo_selections   TYPE REF TO cl_salv_selections.


  LOOP AT gt_so_tab ASSIGNING FIELD-SYMBOL(<l_data>).
    IF <l_data>-created = abap_true.
      WRITE icon_checked AS ICON TO <l_data>-created_icon.
    ELSE.
      CLEAR <l_data>-created_icon.
    ENDIF.

  ENDLOOP.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_so_tab ).

    CATCH cx_salv_msg.
  ENDTRY.

*  lo_functions = go_alv->get_functions( ).
*  lo_functions->set_layout_change( 'X' ).
*  lo_functions->set_layout_load( 'X' ).
*  lo_functions->set_layout_maintain( 'X' ).
*  lo_functions->set_layout_save( 'X' ).
*  lo_functions->set_print( 'X' ).
*  lo_functions->set_print_preview( 'X' ).
*  lo_functions->set_all( 'X' ).

*SET PF-STATUS 'MAIN'.
  go_alv->set_screen_status(
     pfstatus      =  'MAIN'
     report       = sy-repid
     set_functions = go_alv->c_functions_all ).



* Set up selections.
  lo_selections = go_alv->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

* Change attributes for columns
  lo_columns = go_alv->get_columns( ).
  lo_columns->set_optimize( abap_true ).

  TRY.
      lo_col = lo_columns->get_column( 'STATUS' ).
      lo_col->set_technical( if_salv_c_bool_sap=>true ).

      lo_col = lo_columns->get_column( 'STATUS_ICON' ).
      lo_col->set_technical( if_salv_c_bool_sap=>true ).

      lo_col = lo_columns->get_column( 'CREATED' ).
      lo_col->set_technical( if_salv_c_bool_sap=>true ).

      lo_col = lo_columns->get_column( 'INCL_TCR' ).
      lo_col->set_technical( if_salv_c_bool_sap=>true ).

      lo_col = lo_columns->get_column( 'INCL_CER' ).
      lo_col->set_technical( if_salv_c_bool_sap=>true ).

      lo_col = lo_columns->get_column( 'INCL_SCR' ).
      lo_col->set_technical( if_salv_c_bool_sap=>true ).

      lo_col = lo_columns->get_column( 'INCL_TCR' ).
      lo_col->set_technical( if_salv_c_bool_sap=>true ).

      lo_col = lo_columns->get_column( 'EXT_TCRNO' ).
      lo_col->set_technical( if_salv_c_bool_sap=>true ).

      lo_col = lo_columns->get_column( 'DOC_TAB' ).
      lo_col->set_technical( if_salv_c_bool_sap=>true ).


      lo_col = lo_columns->get_column( 'SEQNO' ).
      lo_col->set_short_text('Pump no.').
      lo_col->set_medium_text('Pump no.').
      lo_col->set_long_text('Pump no.').

      lo_col = lo_columns->get_column( 'CERT_MATNR' ).
      lo_col->set_short_text('Cert.').
      lo_col->set_medium_text('Certificate').
      lo_col->set_long_text('Certificate').

      lo_col = lo_columns->get_column( 'CERT_MAKTX' ).
      lo_col->set_short_text('Cert.Desc.').
      lo_col->set_medium_text('Cert. Description').
      lo_col->set_long_text('Certificate Description').

      lo_col = lo_columns->get_column( 'SERNR' ).
      lo_col->set_output_length( 18 ).

    CATCH cx_salv_not_found.
      MESSAGE e999(zm) WITH 'Error in ALV definition!'.
      LEAVE PROGRAM.
  ENDTRY.

  lo_events = go_alv->get_event( ).
  CREATE OBJECT lo_events_handl.
  SET HANDLER lo_events_handl->on_double_click FOR lo_events.
  SET HANDLER lo_events_handl->on_click_send FOR lo_events.

* Registering edit
*  CALL METHOD go_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*

* Show ALV list
  DO.
    go_alv->display( ).

    IF sy-ucomm = '&F03' OR sy-ucomm = '&F15' OR sy-ucomm = '&F12'.  "Back / Exit buttons
      go_cert->unlock_sales_order( im_vbeln = p_vbeln ).
      EXIT.
    ENDIF.

    go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDDO.

ENDFORM.


FORM process_cert.

  DATA: lv_continue TYPE abap_bool.

  go_cert->get_header_data(
    EXPORTING
      im_werks     = p_werks
      im_vbeln     = p_vbeln                  " Sales and Distribution Document Number
      im_posnr     = gv_so_item-posnr         " Item number of the SD document
      im_aufnr     = p_aufnr                  " Service order number
      im_refno     = p_refno                  " Reference sales order no
      im_matnr     = gv_so_item-cert_matnr
      im_sernr     = gv_so_item-sernr
     IMPORTING
       ex_cert_head = gv_header ).   " Certificate header fields

  TRY.
      go_cert->get_classification(
        EXPORTING
          im_matnr     =     gv_header-material
        IMPORTING
          ex_cert_pump =     gv_cert_pump ).      " Pump classification data

    CATCH zcx_grf_qm_cert INTO go_exc.
      PERFORM set_log_header USING '3' 'E'
                             CHANGING gv_exc_info.

      go_exc->log_exception(
         EXPORTING
           im_exc_type = zcx_grf_root=>gc_exc_type_technical
           im_exc_info = gv_exc_info
           im_options  = gv_exc_options ).

      MESSAGE e999(zm) WITH 'Error in reading classification data, see log for details'.
  ENDTRY.

* Attach extra documents
  IF gv_so_item-incl_cer = 'X'.
    go_cert->get_vendor_cert(
      EXPORTING
        im_werks = gv_header-werks
        im_sernr = gv_so_item-sernr
      IMPORTING
        ex_doc_tab = gv_so_item-doc_tab ).

    SORT gv_so_item-doc_tab BY docfile.
  ENDIF.

  IF gv_so_item-incl_scr = 'X'.
    go_cert->get_supp_cert(
      EXPORTING
        im_werks = gv_header-werks
        im_sernr = gv_so_item-sernr
        im_cert_matnr = gv_so_item-cert_matnr
      IMPORTING
        ex_doc_tab = gv_so_item-doc_tab ).

    SORT gv_so_item-doc_tab BY docfile.
  ENDIF.

  IF gv_so_item-incl_tcr = 'X'.
    go_cert->get_test_report(
      EXPORTING
        im_werks     = gv_header-werks
        im_sernr     = gv_so_item-sernr
        im_ext_tcrno = gv_so_item-ext_tcrno
      IMPORTING
        ex_doc_tab = gv_so_item-doc_tab ).

    SORT gv_so_item-doc_tab BY docfile.
  ENDIF.


* Pop-up with data from item note
  go_cert->call_pop_up_general(
      EXPORTING
        im_so_item = gv_so_item
      IMPORTING
        ex_continue = lv_continue
      CHANGING
        ch_cert_header = gv_header ).

  IF lv_continue = abap_false.
    EXIT.
  ENDIF.


  IF go_cert->gv_form_name = 'ZF_CERT_02' OR go_cert->gv_form_name = 'ZF_CERT_08'.
*   Read Heat data
    go_cert->get_comp_data(
      EXPORTING
        im_werks     = gv_header-werks
        im_sernr     = gv_so_item-sernr
        im_spras     = gv_header-langu
      IMPORTING
        ex_comp_data = gt_cert_comp ).

*   Read material grade information
    go_cert->get_comp_grade(
      EXPORTING
        im_cert_pump  = gv_cert_pump
        im_cert_matnr = gv_so_item-cert_matnr
        im_spras      = gv_header-langu
      IMPORTING
        ex_grade_tab = gt_cert_comp_extra
    ).

  ENDIF.


  IF go_cert->gv_form_name = 'ZF_CERT_08' OR go_cert->gv_form_name = 'ZF_CERT_09'.

*   Read material grade information
    go_cert->get_comp_grade(
      EXPORTING
        im_cert_pump  = gv_cert_pump
        im_cert_matnr = gv_so_item-cert_matnr
        im_spras      = gv_header-langu
      IMPORTING
        ex_grade_tab = gt_cert_comp_extra
    ).

  ENDIF.


* Check if roughness certificate
  IF go_cert->gv_form_name = 'ZF_CERT_04'.
    go_cert->call_pop_up_roughness(
      EXPORTING
        im_sernr     = gv_so_item-sernr
      IMPORTING
        ex_continue = lv_continue
      CHANGING
        ch_roughness = gv_roughness ).

    IF lv_continue = abap_false.
      EXIT.
    ENDIF.
  ENDIF.

* Check if vibration certificate
  IF go_cert->gv_form_name = 'ZF_CERT_03'.
    go_cert->call_pop_up_vibration(
      EXPORTING
        im_sernr     = gv_so_item-sernr
        im_cert_pump = gv_cert_pump
      IMPORTING
        ex_continue = lv_continue
      CHANGING
        ch_vibration = gv_vibration ).

    IF lv_continue = abap_false.
      EXIT.
    ENDIF.
  ENDIF.

* Check if Motor test report or Atex certificate
  IF go_cert->gv_form_name = 'ZF_CERT_06' OR go_cert->gv_form_name = 'ZF_CERT_07'.
    CLEAR gv_atex.

    IF gv_cert_pump-tech_data_dcr_14 = 'Y'.  "Only show pop-up if there is a motor on the pump
*     Show pop-up
      go_cert->call_pop_up_atex(
        EXPORTING
          im_matnr     = gv_so_item-matnr
          im_sernr     = gv_so_item-sernr
        IMPORTING
          ex_continue = lv_continue
        CHANGING
          ch_atex = gv_atex ).

      IF lv_continue = abap_false.
        EXIT.
      ENDIF.
    ENDIF.

  ENDIF.

  PERFORM generate_pdf.

ENDFORM.


FORM generate_pdf.

  DATA: lv_continue TYPE abap_bool.

  TRY.
* -> gmadrla 05.01.2018
      go_cert->create_pdf_preview(
        EXPORTING
          im_pdf_form_name   = go_cert->gv_form_name    " PDF form name
          im_cert_pump       = gv_cert_pump    " Certificate pump classification data
          im_cert_comp       = gt_cert_comp
          im_cert_comp_extra = gt_cert_comp_extra
          im_roughness       = gv_roughness
          im_atex            = gv_atex
          im_vibration       = gv_vibration
        CHANGING
          ch_cert_header     = gv_header    " Certificate header fields
      ).

*
*      go_cert->create_pdf(
*        EXPORTING
*          im_pdf_form_name = go_cert->gv_form_name
*          im_cert_pump = gv_cert_pump
*          im_cert_comp = gt_cert_comp
*          im_cert_comp_extra = gv_cert_comp_extra
*          im_roughness = gv_roughness
*          im_atex      = gv_atex
*          im_vibration = gv_vibration
*        IMPORTING
*          ex_pdf_output = gv_pdf_output
*        CHANGING
*          ch_cert_header = gv_header ).
* <- gmadrla 05.01.2018

    CATCH zcx_grf_qm_cert INTO go_exc.
      PERFORM set_log_header USING '3' 'E'
                             CHANGING gv_exc_info.

      go_exc->log_exception(
         EXPORTING
           im_exc_type = zcx_grf_root=>gc_exc_type_technical
           im_exc_info = gv_exc_info
           im_options  = gv_exc_options ).

      MESSAGE e999(zm) WITH 'PDF form could not be generated, see log for details'.
  ENDTRY.

  go_cert->call_pop_up_confirmation( IMPORTING ex_continue = lv_continue ).

  IF lv_continue = abap_false.
    EXIT.
  ENDIF.

* -> gmadrla 05.01.2018
  TRY.
      go_cert->create_pdf_noview(
        EXPORTING
          im_pdf_form_name   = go_cert->gv_form_name    " PDF form name
          im_cert_pump       = gv_cert_pump    " Certificate pump classification data
          im_cert_comp       = gt_cert_comp
          im_cert_comp_extra = gt_cert_comp_extra
          im_roughness       = gv_roughness
          im_atex            = gv_atex
          im_vibration       = gv_vibration
        IMPORTING
          ex_pdf_output      = gv_pdf_output
        CHANGING
          ch_cert_header     = gv_header    " Certificate header fields
      ).
* <- gmadrla 05.01.2018

    CATCH zcx_grf_qm_cert INTO go_exc.
      PERFORM set_log_header USING '3' 'E'
                             CHANGING gv_exc_info.

      go_exc->log_exception(
         EXPORTING
           im_exc_type = zcx_grf_root=>gc_exc_type_technical
           im_exc_info = gv_exc_info
           im_options  = gv_exc_options ).

      MESSAGE e999(zm) WITH 'PDF form could not be generated, see log for details'.
  ENDTRY.

  TRY.
      go_cert->create_document(
         EXPORTING
           im_so_item     = gv_so_item
           im_cert_header = gv_header
           im_cert_pump   = gv_cert_pump
           im_pdf_output  = gv_pdf_output   "PDF file in XML format
            ).

    CATCH zcx_grf_qm_cert INTO go_exc.
      PERFORM set_log_header USING '3' 'E'
                             CHANGING gv_exc_info.

      go_exc->log_exception(
         EXPORTING
           im_exc_type = zcx_grf_root=>gc_exc_type_technical
           im_exc_info = gv_exc_info
           im_options  = gv_exc_options ).

      MESSAGE e999(zm) WITH 'Creation of document failed, see log for details'.
  ENDTRY.

  MESSAGE i999(zm) WITH 'Certificate has been created successfully'.

* Update table used in ALV.
  go_cert->update_item_w_doc_info( CHANGING ch_so_item = gt_so_tab ).

ENDFORM.


FORM set_default_plant CHANGING c_werks.
* Find plant from user parameter WRK
  SELECT parva
    INTO c_werks
    FROM usr05 UP TO 1 ROWS
    WHERE bname = sy-uname
      AND parid = 'WRK'.
  ENDSELECT.

  IF sy-subrc NE 0.
    CLEAR c_werks.
  ENDIF.
ENDFORM.


FORM check_plant.
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
           ID 'ACTVT' FIELD '03'
           ID 'WERKS' FIELD p_werks.

  IF sy-subrc <> 0.
    MESSAGE e998(zm) WITH 'No authorization to plant' p_werks.
  ENDIF.

ENDFORM.


FORM check_sales_order USING u_vbeln TYPE vbeln.
  DATA: lv_vkorg TYPE vbak-vkorg,
        lv_vtweg TYPE vbak-vtweg,
        lv_spart TYPE vbak-spart,
        lv_vbtyp TYPE vbak-vbtyp.

  SELECT SINGLE vkorg vtweg spart vbtyp
    FROM vbak
    INTO ( lv_vkorg, lv_vtweg, lv_spart, lv_vbtyp )
    WHERE vbeln = u_vbeln.

  IF sy-subrc NE 0.
    MESSAGE e999(zm) WITH 'Sales order does not exist'.
    RETURN.
  ELSE.
    IF NOT ( lv_vbtyp = 'I' OR lv_vbtyp = 'C' ).
      MESSAGE e998(zm) WITH 'Sales order does not have the right' 'document category'.
      RETURN.
    ENDIF.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
           ID 'VKORG' FIELD lv_vkorg
           ID 'VTWEG' FIELD lv_vtweg
           ID 'SPART' FIELD lv_spart
           ID 'ACTVT' FIELD '03'.

  IF sy-subrc <> 0.
    MESSAGE e998(zm) WITH 'No authorization to sales org.' lv_vkorg.
  ENDIF.

ENDFORM.


FORM check_service_order.

  SELECT SINGLE COUNT( * )
    FROM afih
    WHERE aufnr = p_aufnr.

  IF sy-subrc NE 0.
    MESSAGE e999(zm) WITH 'Service order does not exist'.
    RETURN.
  ENDIF.
ENDFORM.


FORM check_sold_to USING u_vbeln TYPE vbeln.
  DATA: lv_kunnr TYPE vbpa-kunnr.

  SELECT SINGLE kunnr
  FROM vbpa
  INTO lv_kunnr
  WHERE vbeln = u_vbeln
    AND posnr = '000000'
    AND parvw = 'AG'.       "Sold_to


  IF sy-subrc NE 0.
    MESSAGE e998(zm) WITH 'No sold-to party found in order ' u_vbeln.
  ENDIF.

  IF lv_kunnr BETWEEN '0000000001' AND '0000000999'.
    MESSAGE e999(zm) WITH 'Reference order must be entered'.
  ENDIF.

ENDFORM.


FORM set_log_header USING u_prio TYPE char1
                          u_type TYPE char1
                    CHANGING c_exc_info TYPE zexc_info.
  c_exc_info-area = 'Z_GRF_QM'.        "AREA in log
  c_exc_info-appl = 'CERTIFICATE'.     "SUBAREA in log
  c_exc_info-prio = u_prio.            "Priority - problem class in log. 1=Critical, 2=High, 3=Medium, 4=Low
  c_exc_info-type = u_type.            "Message type - I,W,E
  c_exc_info-tcode = sy-tcode.
  c_exc_info-opmode = 'D'.             "Dialog
ENDFORM.



CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_double_click.

    DATA: lv_field   TYPE sval,
          lt_fields  TYPE TABLE OF sval,
          lv_sernr22 TYPE char22,
          lv_kunnr   TYPE kunnr,
          lv_matnr   TYPE matnr,
          lv_txt1    TYPE char100.

    DATA: lv_retcode TYPE char1,
          lo_columns TYPE REF TO cl_salv_columns_table.

    READ TABLE gt_so_tab ASSIGNING FIELD-SYMBOL(<l_so_item>) INDEX row.
    IF sy-subrc = 0.

*     Check if certificate is allowed.
      SELECT COUNT( * )
        FROM zqcert_forms
        WHERE langu = 'EN'
          AND matnr = <l_so_item>-cert_matnr
          AND ecm_text NE space.

      IF sy-subrc = 4. "No records found
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Information'
            txt1  = 'Certificate is not released!'
            txt2  = space.
        RETURN.
      ENDIF.

      IF <l_so_item>-cert_matnr = '000000000098981818'.
        SELECT SINGLE kunnr
          INTO lv_kunnr
          FROM vbak
          WHERE vbeln = p_vbeln.

        IF sy-subrc NE 0.
          MESSAGE e999(zm) WITH 'Sales order not found!'.
          LEAVE PROGRAM.
        ENDIF.

        SELECT COUNT( * )
          FROM zqcert_custm
          WHERE matnr = <l_so_item>-matnr
            AND kunnr = lv_kunnr.

        IF sy-subrc NE 0.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = <l_so_item>-matnr
            IMPORTING
              output = lv_matnr.

          lv_txt1 = 'Customer' && | | && lv_kunnr && | | && 'and' && | | && 'product number ' && | | && lv_matnr && | | && 'is not approved'.
          CALL FUNCTION 'POPUP_TO_INFORM'
            EXPORTING
              titel = 'Information'
              txt1  = lv_txt1
              txt2  = 'for EC1935 statement.'
              txt3  = 'Please contact the super-user for update of EC 1935 information'
              txt4  = '(ZCERT14 needs to be updated).'.
          RETURN.
        ENDIF.
      ENDIF.

      IF <l_so_item>-sernr IS INITIAL.
*       Show pop-up where serial number can be entered
        CLEAR: lv_field, lt_fields[].
        lv_field-tabname = 'EQUI'.
        lv_field-fieldname = 'GROES'.   "'GERNR'
        lv_field-fieldtext = 'Serial number'.

        lv_field-value = go_cert->get_sernr_default_value( <l_so_item>-matnr ).
        APPEND lv_field TO lt_fields.

        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
            popup_title     = 'Please enter serial number'
*           START_COLUMN    = '5'
*           START_ROW       = '5'
          IMPORTING
            returncode      = lv_retcode
          TABLES
            fields          = lt_fields
          EXCEPTIONS
            error_in_fields = 1
            OTHERS          = 2.

        IF sy-subrc <> 0.
*         will never happen
          MESSAGE e998(zm) WITH 'Error in function module' 'POPUP_GET_VALUES'.
          LEAVE PROGRAM.
        ENDIF.

        IF lv_retcode NE 'A'.  "A=abort
          READ TABLE lt_fields INTO lv_field INDEX 1.
          gv_so_item = <l_so_item>.
          gv_so_item-sernr = lv_field-value.

          lv_sernr22 = lv_field-value.

          IF go_cert->validate_sernr( im_sernr = lv_sernr22 ).
            PERFORM process_cert.
          ELSE.
            MESSAGE i011(zgrf_qm_cert).    "Format of serial number is not correct
          ENDIF.
        ENDIF.
      ELSE.
        gv_so_item = <l_so_item>.
        PERFORM process_cert.
      ENDIF.
    ENDIF.

*   Refresh ALV list
    lo_columns = go_alv->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ). "full refresh necessary cause of data changes

  ENDMETHOD.


  METHOD on_click_send.

    DATA: lo_selections   TYPE REF TO cl_salv_selections.
    DATA: lt_sel_rows     TYPE salv_t_row.

    DATA: lt_sel_so_item  TYPE zttcert_so_item.

*   Set up selections.
    lo_selections = go_alv->get_selections( ).
    lt_sel_rows = lo_selections->get_selected_rows( ).

    IF lines( lt_sel_rows ) = 0.
      MESSAGE i999(zm) WITH 'No lines selected'.
      RETURN.
    ENDIF.

    CLEAR lt_sel_so_item[].
    LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<l_row>).

      READ TABLE gt_so_tab ASSIGNING FIELD-SYMBOL(<l_so_item>) INDEX <l_row>.
      CHECK sy-subrc = 0 .

      IF <l_so_item>-sernr IS INITIAL.
        MESSAGE i997(zm) WITH 'Certificate for line' <l_row> 'is not created'.
        RETURN.
      ELSE.
        APPEND <l_so_item> TO lt_sel_so_item.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_sel_so_item ASSIGNING FIELD-SYMBOL(<l_sel_item>).
      IF sy-tabix = 1.
        go_cert->get_header_data(
           EXPORTING
             im_werks     = p_werks
             im_vbeln     = p_vbeln         " Sales and Distribution Document Number
             im_posnr     = <l_sel_item>-posnr         " Item number of the SD document
             im_aufnr     = p_aufnr
             im_refno     = p_refno
             im_matnr     = <l_sel_item>-cert_matnr
             im_sernr     = <l_sel_item>-sernr
            IMPORTING
              ex_cert_head = gv_header ).   " Certificate header fields
      ENDIF.

*     Read attached documents and add them to item table
      IF <l_sel_item>-incl_cer = 'X'.
        go_cert->get_vendor_cert(
          EXPORTING
            im_werks = gv_header-werks
            im_sernr = <l_sel_item>-sernr
          IMPORTING
            ex_doc_tab = <l_sel_item>-doc_tab ).
      ENDIF.

      IF <l_sel_item>-incl_scr = 'X'.
        go_cert->get_supp_cert(
          EXPORTING
            im_werks = gv_header-werks
            im_sernr = <l_sel_item>-sernr
            im_cert_matnr = <l_sel_item>-cert_matnr
          IMPORTING
            ex_doc_tab = <l_sel_item>-doc_tab ).
      ENDIF.

      IF <l_sel_item>-incl_tcr = 'X'.
        go_cert->get_test_report(
          EXPORTING
            im_werks     = gv_header-werks
            im_sernr     = <l_sel_item>-sernr
            im_ext_tcrno = <l_sel_item>-ext_tcrno
          IMPORTING
            ex_doc_tab = <l_sel_item>-doc_tab ).
      ENDIF.
    ENDLOOP.

    TRY.
        go_cert->send_email( im_item_tab = lt_sel_so_item
                             im_cert_header = gv_header ).

      CATCH zcx_grf_qm_cert INTO go_exc.
        PERFORM set_log_header USING '3' 'E'
                               CHANGING gv_exc_info.

        go_exc->log_exception(
           EXPORTING
             im_exc_type = zcx_grf_root=>gc_exc_type_technical
             im_exc_info = gv_exc_info
             im_options  = gv_exc_options ).

        MESSAGE e999(zm) WITH 'Creation of email failed, see log for details'.
    ENDTRY.

    COMMIT WORK.

  ENDMETHOD.

ENDCLASS.
