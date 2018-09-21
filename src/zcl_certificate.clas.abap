CLASS zcl_certificate DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_recipient TYPE TABLE OF zemail .
    TYPES:
      ty_posnr     TYPE RANGE OF zscert_so_item-posnr .
    TYPES:
      BEGIN OF ty_doc,
        dokar TYPE dokar,
        doknr TYPE doknr,
        dokvr TYPE dokvr,
        doktl TYPE doktl,
      END OF ty_doc .
    TYPES:
      BEGIN OF ty_miss_doc,
        sernr       TYPE gernr,
        matnr       TYPE matnr,
        vendor_code TYPE zvendor_code,
        heat_data   TYPE zheat_data,
      END OF ty_miss_doc .
    TYPES:
      ty_doc_tab      TYPE TABLE OF ty_doc .
    TYPES:
      ty_miss_doc_tab TYPE TABLE OF ty_miss_doc .

    DATA gt_bom_matnr TYPE ztt_matnr .
    DATA gv_form_name TYPE hr99s_pdfname .
    DATA gv_doktl TYPE doktl_d .
    DATA gt_doc TYPE ty_doc_tab .
    DATA gt_miss_doc TYPE ty_miss_doc_tab .

    METHODS check_user_sign
      RETURNING
        VALUE(re_ok) TYPE abap_bool .
    METHODS get_header_data
      IMPORTING
        VALUE(im_werks) TYPE werks_d
        VALUE(im_vbeln) TYPE vbeln
        VALUE(im_posnr) TYPE posnr
        VALUE(im_aufnr) TYPE aufnr
        VALUE(im_refno) TYPE vbeln
        VALUE(im_matnr) TYPE matnr
        VALUE(im_sernr) TYPE gernr
      EXPORTING
        !ex_cert_head   TYPE zscert_header .
    METHODS get_so_item
      IMPORTING
        VALUE(im_werks) TYPE werks_d
        VALUE(im_vbeln) TYPE vbeln
        !im_posnr       TYPE ty_posnr
      EXPORTING
        !ex_so_item     TYPE zttcert_so_item .
*    METHODS so_item_alv
*      IMPORTING
*        VALUE(im_so_item) TYPE zttcert_so_item.
    METHODS get_classification
      IMPORTING
        VALUE(im_matnr) TYPE matnr
      EXPORTING
        !ex_cert_pump   TYPE zscert_pump_char
      RAISING
        zcx_grf_qm_cert .
    METHODS get_classification_cert
      IMPORTING
        VALUE(im_matnr) TYPE matnr
      EXPORTING
        !ex_cert_desc   TYPE zscert_header-cert_desc .
    METHODS get_comp_data
      IMPORTING
        VALUE(im_werks) TYPE werks_d
        VALUE(im_sernr) TYPE gernr
        VALUE(im_spras) TYPE spras
      EXPORTING
        !ex_comp_data   TYPE zttcert_comp_data .
    METHODS create_pdf
      IMPORTING
        VALUE(im_pdf_form_name) TYPE fpname
        !im_cert_pump           TYPE zscert_pump_char
        !im_cert_comp           TYPE zttcert_comp_data
        !im_cert_comp_extra     TYPE zttcert_comp_data_extra
        !im_roughness           TYPE zscert_roughness
        !im_atex                TYPE zscert_atex
        !im_vibration           TYPE zscert_vibration
      EXPORTING
        !ex_pdf_output          TYPE fpformoutput
      CHANGING
        !ch_cert_header         TYPE zscert_header
      RAISING
        zcx_grf_qm_cert .
    METHODS create_document
      IMPORTING
        !im_so_item     TYPE zscert_so_item
        !im_cert_header TYPE zscert_header
        !im_cert_pump   TYPE zscert_pump_char
        !im_pdf_output  TYPE fpformoutput
      RETURNING
        VALUE(re_ok)    TYPE abap_bool
      RAISING
        zcx_grf_qm_cert .
    METHODS send_email
      IMPORTING
        !im_item_tab    TYPE zttcert_so_item
        !im_cert_header TYPE zscert_header
      RAISING
        zcx_grf_qm_cert .
    METHODS call_pop_up_general
      IMPORTING
        !im_so_item     TYPE zscert_so_item
      EXPORTING
        !ex_continue    TYPE abap_bool
      CHANGING
        !ch_cert_header TYPE zscert_header .
    METHODS call_pop_up_roughness
      IMPORTING
        VALUE(im_sernr) TYPE gernr
      EXPORTING
        !ex_continue    TYPE abap_bool
      CHANGING
        !ch_roughness   TYPE zscert_roughness .
    METHODS call_pop_up_atex
      IMPORTING
        VALUE(im_matnr) TYPE matnr_d
        VALUE(im_sernr) TYPE gernr
      EXPORTING
        !ex_continue    TYPE abap_bool
      CHANGING
        VALUE(ch_atex)  TYPE zscert_atex .
    METHODS call_pop_up_vibration
      IMPORTING
        VALUE(im_sernr)     TYPE gernr
        !im_cert_pump       TYPE zscert_pump_char
      EXPORTING
        !ex_continue        TYPE abap_bool
      CHANGING
        VALUE(ch_vibration) TYPE zscert_vibration .
    METHODS call_pop_up_confirmation
      EXPORTING
        !ex_continue TYPE abap_bool .
    METHODS get_sernr_default_value
      IMPORTING
        VALUE(im_matnr) TYPE matnr
      RETURNING
        VALUE(re_sernr) TYPE gernr .
*    CLASS-METHODS validate_sernr
*      IMPORTING
*        VALUE(im_sernr) TYPE gernr
*        VALUE(im_matnr) TYPE matnr
*        VALUE(im_werks) TYPE werks_d
*      RETURNING
*        VALUE(re_ok)    TYPE abap_bool .
    CLASS-METHODS validate_sernr
      IMPORTING
        VALUE(im_sernr) TYPE char22
      RETURNING
        VALUE(re_ok)    TYPE abap_bool .
    METHODS constructor
      IMPORTING
        VALUE(im_werks) TYPE werks_d
        VALUE(im_vbeln) TYPE vbeln .
    METHODS get_next_version_for_doc
      IMPORTING
        VALUE(im_vbeln)      TYPE vbeln
        VALUE(im_vbelp)      TYPE vbelp OPTIONAL
        VALUE(im_vkorg)      TYPE vkorg OPTIONAL
        VALUE(im_sernr)      TYPE gernr OPTIONAL
        VALUE(im_matnr)      TYPE matnr OPTIONAL
        VALUE(im_cert_matnr) TYPE matnr OPTIONAL
        VALUE(im_next)       TYPE char1 OPTIONAL
      EXPORTING
        !ex_next_dokvr       TYPE bapi_doc_aux-docversion
        !ex_doknr            TYPE bapi_doc_aux-docnumber .
    METHODS get_released_version
      IMPORTING
        VALUE(im_vbeln)      TYPE vbeln OPTIONAL
        VALUE(im_vbelp)      TYPE vbelp OPTIONAL
        VALUE(im_sernr)      TYPE gernr OPTIONAL
        VALUE(im_cert_matnr) TYPE matnr OPTIONAL
        VALUE(im_seqnr)      TYPE zseqn2 OPTIONAL
        VALUE(im_prod_site)  TYPE werks_d OPTIONAL
      EXPORTING
        !ex_released_docs    TYPE zttcert_version .
    METHODS update_item_w_doc_info
      CHANGING
        !ch_so_item TYPE zttcert_so_item .
    METHODS lock_sales_order
      IMPORTING
        VALUE(im_vbeln) TYPE vbeln .
    METHODS unlock_sales_order
      IMPORTING
        VALUE(im_vbeln) TYPE vbeln .
    METHODS create_pdf_preview
      IMPORTING
        VALUE(im_pdf_form_name) TYPE fpname
        !im_cert_pump           TYPE zscert_pump_char
        !im_cert_comp           TYPE zttcert_comp_data
        !im_cert_comp_extra     TYPE zttcert_comp_data_extra
        !im_roughness           TYPE zscert_roughness
        !im_atex                TYPE zscert_atex
        !im_vibration           TYPE zscert_vibration
      CHANGING
        !ch_cert_header         TYPE zscert_header
      RAISING
        zcx_grf_qm_cert .
    METHODS create_pdf_noview
      IMPORTING
        VALUE(im_pdf_form_name) TYPE fpname
        !im_cert_pump           TYPE zscert_pump_char
        !im_cert_comp           TYPE zttcert_comp_data
        !im_cert_comp_extra     TYPE zttcert_comp_data_extra
        !im_roughness           TYPE zscert_roughness
        !im_atex                TYPE zscert_atex
        !im_vibration           TYPE zscert_vibration
      EXPORTING
        !ex_pdf_output          TYPE fpformoutput
      CHANGING
        !ch_cert_header         TYPE zscert_header
      RAISING
        zcx_grf_qm_cert .
    METHODS read_class_setup
      IMPORTING
        !im_dokar     TYPE dokar
      EXPORTING
        !ex_zksml_tab TYPE zttzksml .
    METHODS get_vendor_cert
      IMPORTING
        !im_werks   TYPE werks_d
        !im_sernr   TYPE gernr
      EXPORTING
        !ex_doc_tab TYPE tt_bapi_doc_files2 .
    METHODS get_supp_cert
      IMPORTING
        !im_werks      TYPE werks_d
        !im_sernr      TYPE gernr
        !im_cert_matnr TYPE matnr
      EXPORTING
        !ex_doc_tab    TYPE tt_bapi_doc_files2 .
    METHODS get_test_report
      IMPORTING
        !im_werks     TYPE werks_d
        !im_sernr     TYPE gernr
        !im_ext_tcrno TYPE z_ext_tcrno
      EXPORTING
        !ex_doc_tab   TYPE tt_bapi_doc_files2 .
    METHODS get_comp_grade
      IMPORTING
        !im_cert_pump  TYPE zscert_pump_char
        !im_cert_matnr TYPE matnr
        !im_spras      TYPE spras
      EXPORTING
        !ex_grade_tab  TYPE zttcert_comp_data_extra .
    METHODS get_service_order_data
      IMPORTING
        !im_aufnr     TYPE aufnr
      EXPORTING
        !ex_cert_head TYPE zscert_header .
  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      ty_lt_docfiles   TYPE STANDARD TABLE OF bapi_doc_files2 WITH DEFAULT KEY,
      ty_lt_access     TYPE STANDARD TABLE OF scms_acinf WITH DEFAULT KEY,
      ty_lt_sdokcntbin TYPE STANDARD TABLE OF sdokcntbin WITH DEFAULT KEY.

    CONSTANTS: gc_dcr TYPE dokar VALUE 'DCR',   "Digital certificates - master
               gc_tcr TYPE dokar VALUE 'TCR',   "Test reports
               gc_cer TYPE dokar VALUE 'CER',   "Vendor certificates
               gc_scr TYPE dokar VALUE 'SCR'.   "Supplier certificates

    CONSTANTS: gc_destination_q20 TYPE rfcdest VALUE 'Q21LOG100',
               gc_destination_p20 TYPE rfcdest VALUE 'P21LOG100'.

    CONSTANTS: gc_storagecat_x20 TYPE cv_storage_cat VALUE 'Z_X20_001',
               gc_storagecat_p20 TYPE cv_storage_cat VALUE 'Z_P20_000'.

    DATA gv_werks TYPE werks_d .
    DATA gv_vbeln TYPE vbeln .
    DATA gv_plant_id TYPE char2 .                "Plant ID used in DUTID fx P1
    DATA gv_cx_root TYPE REF TO cx_root .        "Exception class
    DATA gv_destination TYPE rfcdest .
    DATA gv_storagecat TYPE cv_storage_cat.
    DATA gt_cert_matnr TYPE TABLE OF matnr .
    DATA gv_sel_row TYPE i .

    METHODS get_email_recipients
      IMPORTING
        VALUE(im_vbeln) TYPE vbeln
      EXPORTING
        !ex_recipients  TYPE ty_recipient .
    METHODS get_material_grade
      IMPORTING
        VALUE(im_matnr) TYPE matnr_d
      EXPORTING
        !ex_grade       TYPE zcert_grade .
    METHODS mat_bom_breakdown
      IMPORTING
        VALUE(im_matnr) TYPE matnr_d
        VALUE(im_werks) TYPE werks_d .
    METHODS get_mat_for_class
      IMPORTING
        VALUE(im_class) TYPE klasse_d
      EXPORTING
        !ex_matnr_tab   TYPE ztt_matnr .

    METHODS get_so_item_note
      IMPORTING
        !im_so_item  TYPE zscert_so_item
      EXPORTING
        !ex_text_tab TYPE lop_tdline_tab .

    METHODS get_external_doc
      IMPORTING
        !im_dokar TYPE dokar
        !im_doknr TYPE doknr
      EXPORTING
        !ex_draw  TYPE draw.

    METHODS get_pdf_as_binary
      IMPORTING
                im_doc     TYPE bapi_doc_files2
      EXPORTING
                ex_xstring TYPE xstring
                ex_fname   TYPE filep
      RAISING   zcx_grf_qm_cert.

    METHODS read_address
      IMPORTING
        im_adrnr       TYPE adrnr
      RETURNING
        VALUE(re_addr) TYPE addr1_val.



ENDCLASS.



CLASS ZCL_CERTIFICATE IMPLEMENTATION.


  METHOD call_pop_up_atex.
    DATA: lv_continue TYPE xfeld.

*   Read all BOM materials into table GT_BOM_MATNR
    me->mat_bom_breakdown(
      EXPORTING
        im_matnr = im_matnr
        im_werks = gv_werks ).

*   Read special ATEX fields
*    SELECT SINGLE *
*      FROM zqcert_atex
*      INTO ch_atex
*      WHERE sernr = im_sernr.
*
*    IF sy-subrc = 4. "No record exist - new record will be created
*      CLEAR ch_atex.
*      ch_atex-sernr = im_sernr.
*    ENDIF.

    CLEAR ch_atex.

    CALL FUNCTION 'Z_CERT_POP_UP_ATEX'
      EXPORTING
        im_bom_mat  = gt_bom_matnr     "Materials from BOM
      IMPORTING
        ex_continue = lv_continue      "Continue processing
      CHANGING
        ch_atex     = ch_atex.    "Data for atex certificate

*    IF lv_continue = abap_true.
*      MODIFY zqcert_atex FROM ch_atex.
*      IF sy-subrc NE 0.
*        MESSAGE e013(zgrf_qm_cert) WITH 'ZQCERT_ATEX'.    "Update of table &1 failed!
*      ENDIF.
*    ENDIF.

    ex_continue = lv_continue.

  ENDMETHOD.


  METHOD call_pop_up_confirmation.
    DATA lv_continue  TYPE xfeld.

*   Check if a valid file for signing the document exist
    IF me->check_user_sign( ).
      CALL FUNCTION 'Z_CERT_POP_UP_CONFIRMATION'
        IMPORTING
          ex_continue = lv_continue.     "Continue processing

      ex_continue = lv_continue.
    ELSE.
      DATA(lv_txt1) = 'No signature file found for user' && | | && sy-uname.

      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Information'
          txt1  = lv_txt1
          txt2  = 'Certificate will not be saved'.

      ex_continue = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD call_pop_up_general.

    DATA: lv_continue TYPE xfeld,
          lt_text_tab TYPE lop_tdline_tab.

    CALL METHOD get_so_item_note(
      EXPORTING
        im_so_item  = im_so_item
      IMPORTING
        ex_text_tab = lt_text_tab ).

    CALL FUNCTION 'Z_CERT_POP_UP_GENERAL'
      EXPORTING
        im_text_tab    = lt_text_tab           "Text from item note
        im_doc_tab     = im_so_item-doc_tab
      IMPORTING
        ex_continue    = lv_continue           "Continue processing
      CHANGING
        ch_cert_header = ch_cert_header.       "Header Data for certificate

    IF lv_continue = abap_false.
      ex_continue = abap_false.
      RETURN.
    ELSE.
      ex_continue = abap_true.
    ENDIF.

*   Read headings from table using right language
    SELECT SINGLE form header_txt1 header_txt2 header_txt3 ecm_text
      INTO ( me->gv_form_name,
             ch_cert_header-header_txt1,
             ch_cert_header-header_txt2,
             ch_cert_header-header_txt3,
             ch_cert_header-doc_name )
      FROM zqcert_forms
      WHERE langu = ch_cert_header-langu
        AND matnr = ch_cert_header-cert_matnr.

    IF sy-subrc NE 0.
      MESSAGE e014(zgrf_qm_cert) WITH 'ZQCERT_FORMS' ch_cert_header-langu ch_cert_header-cert_matnr.  "Entry not found in table &1 for &2 &3
    ENDIF.

  ENDMETHOD.


  METHOD call_pop_up_roughness.

    DATA: lv_continue TYPE xfeld.

*    SELECT SINGLE *
*      FROM zqcert_roughness
*      INTO ch_roughness
*      WHERE sernr = im_sernr.
*
*    IF sy-subrc = 4. "No record exist - new record will be created
*      CLEAR ch_roughness.
*      ch_roughness-sernr = im_sernr.
*    ENDIF.

    CLEAR ch_roughness.

    CALL FUNCTION 'Z_CERT_POP_UP_ROUGHNESS'
      EXPORTING
        im_bom_mat   = gt_bom_matnr     "Materials from BOM
      IMPORTING
        ex_continue  = lv_continue      "Continue processing
      CHANGING
        ch_roughness = ch_roughness.    "Data for roughness certificate

*    IF lv_continue = abap_true.
*      MODIFY zqcert_roughness FROM ch_roughness.
*      IF sy-subrc NE 0.
*        MESSAGE e013(zgrf_qm_cert) WITH 'ZQCERT_ROUGHNESS'.    "Update of table &1 failed!
*      ENDIF.
*    ENDIF.

    ex_continue = lv_continue.

  ENDMETHOD.


  METHOD call_pop_up_vibration.

    DATA: lv_continue TYPE xfeld.

*    SELECT SINGLE *
*      FROM zqcert_vibration
*      INTO ch_vibration
*      WHERE sernr = im_sernr.

*   IF sy-subrc = 4. "No record exist - new record will be created
    CLEAR ch_vibration.
*      ch_vibration-sernr = im_sernr.

*     Set default values
*      ch_vibration-voltage = im_cert_pump-zzcmbamp.
*      ch_vibration-flow = im_cert_pump-skiltqnsi.
*      ch_vibration-head = im_cert_pump-skilthnsi.
    ch_vibration-voltage = im_cert_pump-tech_data_dcr_20.
    ch_vibration-flow    = im_cert_pump-tech_data_dcr_04.
    ch_vibration-head    = im_cert_pump-tech_data_dcr_05.
*    ENDIF.

    CALL FUNCTION 'Z_CERT_POP_UP_VIBRATION'
      IMPORTING
        ex_continue  = lv_continue      "Continue processing
      CHANGING
        ch_vibration = ch_vibration.    "Data for vibration certificate

*    IF lv_continue = abap_true.
*      MODIFY zqcert_vibration FROM ch_vibration.
*      IF sy-subrc NE 0.
*        MESSAGE e013(zgrf_qm_cert) WITH 'ZQCERT_VIBRATION'.    "Update of table &1 failed!
*      ENDIF.
*    ENDIF.

    ex_continue = lv_continue.

  ENDMETHOD.


  METHOD check_user_sign.

    DATA: lo_client TYPE REF TO if_http_client.
    DATA: lv_code   TYPE i.
    DATA: lv_url    TYPE crmt_pers_url.

    DATA: lv_found  TYPE xfeld,
          lv_parkey TYPE zparkey,
          lv_value  TYPE zvalue.

*   Read path to signature file from ZX08
    lv_parkey = sy-sysid.

    CALL METHOD zcl_param=>read
      EXPORTING
        area    = 'QM'
        subarea = 'CERT'
        parname = 'PATH_USER_SIGNATURE'
        parkey  = lv_parkey
      IMPORTING
        found   = lv_found
        value   = lv_value.

    IF lv_found IS INITIAL.
      "Error
    ELSE.
      IF lv_value = 'TEST'.
        re_ok = abap_true.
        RETURN.
      ENDIF.

      "http://grusapx20.sap.group.grundfos.com:8002/Adobe_Signs/SIGN_GBJLLZ.png
      lv_url = 'http://' && lv_value && 'SIGN_' && sy-uname && '.png'.
    ENDIF.

    cl_http_client=>create_by_url(
      EXPORTING
        url    = lv_url
      IMPORTING
        client = lo_client ).

    lo_client->send( ).
    lo_client->receive( ).

    lo_client->response->get_status(
          IMPORTING
            code = lv_code ).

    IF lv_code = '200'.  "OK
      re_ok = abap_true.
    ELSE.
      re_ok = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    DATA: lv_found  TYPE xfeld,
          lv_parkey TYPE zparkey,
          lv_value  TYPE zvalue.

    gv_werks = im_werks.
    gv_vbeln = im_vbeln.

*   Read master data for forms
    SELECT matnr
      FROM zqcert_forms
      INTO TABLE gt_cert_matnr
      WHERE langu = 'E'.

    IF sy-subrc NE 0.
      MESSAGE e999(zm) WITH 'No data found in table ZQCERT_FORMS'.
    ENDIF.

*   Read plant parameter from ZX08
    lv_parkey = gv_werks.

    CALL METHOD zcl_param=>read
      EXPORTING
        area    = 'QM'
        subarea = 'CERT'
        parname = 'ZCERT_PRINT'
        parkey  = lv_parkey
      IMPORTING
        found   = lv_found
        value   = lv_value.

    IF lv_found IS INITIAL.
    ELSE.
      gv_plant_id = lv_value(2).
    ENDIF.

*   Destination is used for getting user title in HR system
    CASE sy-sysid.
      WHEN 'Q20'.
        gv_destination = gc_destination_q20.
        gv_storagecat = gc_storagecat_p20.
      WHEN 'P20'.
        gv_destination = gc_destination_p20.
        gv_storagecat = gc_storagecat_p20.
      WHEN 'X20'.
        gv_destination = gc_destination_q20.
        gv_storagecat = gc_storagecat_x20.
      WHEN OTHERS.
        MESSAGE e998(zm) WITH 'No storage category defined for' sy-sysid.
        LEAVE PROGRAM.
    ENDCASE.
  ENDMETHOD.


  METHOD create_document.

    CONSTANTS: lc_ftype   TYPE atwrt      VALUE '.pdf',
               lc_docpath TYPE dms_path   VALUE '/u/GDMuser/L2S/',
               lc_classtp TYPE klassenart VALUE '017',
               lc_classnm TYPE klasse_d   VALUE 'CERTIFICATE_REPORT'.

    DATA: lv_newfile TYPE string,
          lv_dokvr   TYPE dokvr,
          lv_return  TYPE bapiret2.

    DATA: lv_descr                 TYPE char40,
          lt_docstruct             TYPE TABLE OF bapi_doc_files2,
          lwa_docstruct            TYPE bapi_doc_files2,
          lwa_docdata_2            TYPE bapi_doc_draw2,
          lwa_docdatax             TYPE bapi_doc_drawx2,
          lt_charval               TYPE TABLE OF bapi_characteristic_values,
          lwa_charval              TYPE bapi_characteristic_values,
          lt_bapi_class_allocation TYPE TABLE OF bapi_class_allocation,
          lwa_classallo            TYPE bapi_class_allocation,
          lv_doc_no                TYPE doknr,
          lv_file_name             TYPE filep,
          lv_cmatnr                TYPE matnr,
          lv_laiso                 TYPE laiso,
          lv_lagrf                 TYPE laiso,
          lt_res_tab               TYPE match_result_tab,
          lv_offset                TYPE int4,
          lv_yyyyww                TYPE char6.

    DATA: lv_vbeln      TYPE vbeln,
          lv_vbelp      TYPE vbelp,
          lv_vkorg      TYPE vkorg,
          lv_sernr      TYPE gernr,
          lv_matnr      TYPE matnr,
          lv_cert_matnr TYPE matnr,
          lv_next_dokvr TYPE dokvr,
          lv_doknr      TYPE doknr.

    DATA: lv_protf      TYPE drap-protf,
          lt_cvddockey  TYPE TABLE OF cvddockey,
          lwa_cvddockey TYPE cvddockey.

    DATA: lwa_docfiles TYPE bapi_doc_files2.

    DATA: lv_fname    TYPE filep,
          lv_fname1   TYPE string,
          lv_fname2   TYPE string,
          lv_tstamp   TYPE timestamp,
          lv_tstamp_c TYPE char14.

    DATA: lv_xstring TYPE xstring.

    DATA: lt_doc_tab TYPE tt_bapi_doc_files2.

    DATA: lr_excep TYPE REF TO cx_root.

**&******************************************************************************************
**& Build document number, filename and description
**&******************************************************************************************
    WRITE im_cert_header-cert_matnr TO lv_cmatnr NO-ZERO.

*&******************************************************************************************
*& Characteristics
*&******************************************************************************************
    lwa_charval-classtype = lc_classtp.       "'017'.
    lwa_charval-classname = lc_classnm.       "'CERTIFICATE_REPORT'.

    lwa_charval-charname = 'SOLD_TO_PARTY'.   " Sold-to-party
    WRITE im_cert_header-sold_to_no TO lwa_charval-charvalue NO-ZERO.
    CONDENSE lwa_charval-charvalue.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'SALES_ORDER'.  " Sales Order
    WRITE im_cert_header-sd_doc TO lwa_charval-charvalue NO-ZERO.
    CONDENSE lwa_charval-charvalue.
    lv_vbeln = lwa_charval-charvalue.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'SALES_ORDER_ITEM'.  " Sales Order
    WRITE im_cert_header-itm_number TO lwa_charval-charvalue NO-ZERO.
    CONDENSE lwa_charval-charvalue.
    lv_vbelp = lwa_charval-charvalue.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'SALES_COMPANY'.
    lwa_charval-charvalue = im_cert_header-sales_org.
    lv_vkorg = lwa_charval-charvalue.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'DUT_ID_SERIAL'.        "DUT ID
    lwa_charval-charvalue = im_cert_header-sernr.
    lv_sernr = lwa_charval-charvalue.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'PART_NO'.
    WRITE im_cert_header-material TO lwa_charval-charvalue NO-ZERO.
    CONDENSE lwa_charval-charvalue.
    lv_matnr = lwa_charval-charvalue.
    APPEND lwa_charval TO lt_charval.

    IF im_cert_header-sernr CS 'P'.
      FIELD-SYMBOLS <l_match> LIKE LINE OF lt_res_tab.
      FIND ALL OCCURRENCES OF 'P' IN
           im_cert_header-sernr
           RESULTS lt_res_tab.

      LOOP AT lt_res_tab ASSIGNING <l_match>.
        lv_offset = <l_match>-offset.
      ENDLOOP.
      lv_offset = lv_offset + 2.
* Year 2000 and something
      CONCATENATE '20' im_cert_header-sernr+lv_offset(4) INTO lv_yyyyww.

      lwa_charval-charname  = 'YEAR_WEEK'.
      lwa_charval-charvalue = lv_yyyyww.

      APPEND lwa_charval TO lt_charval.
    ENDIF.

    lwa_charval-charname = 'CERT_TEMPLATE'.
    lwa_charval-charvalue = lv_cmatnr.
    lv_cert_matnr = lwa_charval-charvalue.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'PUMP_MODEL'.  "Model A or B
    lwa_charval-charvalue = im_cert_pump-tech_data_dcr_09.    "model2.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'PRODUCTION_SITE'.
    lwa_charval-charvalue = im_cert_header-werks.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'PUMPTYPE'.
    lwa_charval-charvalue = im_cert_pump-tech_data_dcr_22.   "pumptype.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'EXT_REPORT'.
    IF lines( im_so_item-doc_tab ) = 0.
      lwa_charval-charvalue = 'No'.
    ELSE.
      lwa_charval-charvalue = 'Yes'.
    ENDIF.
    APPEND lwa_charval TO lt_charval.

    lwa_charval-charname = 'PUMP_SEQU_NO'.
    lwa_charval-charvalue = im_so_item-seqno.
    APPEND lwa_charval TO lt_charval.

*   find two character language codes
    SELECT SINGLE laiso INTO lv_laiso
      FROM t002
      WHERE spras = im_cert_header-langu.
    IF sy-subrc EQ 0.
      lv_lagrf = lv_laiso.
    ELSE.
      lv_lagrf = 'EN'.
    ENDIF.

    lwa_charval-charname = 'LANGUAGE_CODE'.
    lwa_charval-charvalue = lv_lagrf.
    APPEND lwa_charval TO lt_charval.

    IF im_cert_header-service_order IS NOT INITIAL.
      lwa_charval-charname = 'REFERENCE_ORDER'.
      lwa_charval-charvalue = im_cert_header-service_order.
      APPEND lwa_charval TO lt_charval.
    ELSEIF im_cert_header-ref_order IS NOT INITIAL.
      lwa_charval-charname = 'REFERENCE_ORDER'.
      lwa_charval-charvalue = im_cert_header-ref_order.
      APPEND lwa_charval TO lt_charval.
    ENDIF.


*&******************************************************************************************
* set old document to history
*&******************************************************************************************
    IF im_so_item-dokvr IS NOT INITIAL.

* set old document to History
      REFRESH: lt_cvddockey[].

      lv_protf = sy-uname.

      lwa_cvddockey-dokar = gc_dcr.
      lwa_cvddockey-doknr = im_so_item-doknr.
      lwa_cvddockey-dokvr = im_so_item-dokvr.
      lwa_cvddockey-doktl = '000'.

      APPEND lwa_cvddockey TO lt_cvddockey.

      CALL FUNCTION 'Z_ZD010_CHANGE_DOC_STATUS'
        EXPORTING
          i_status               = 'HI'
          i_protf                = lv_protf
          i_do_changes           = 'X'
          i_do_succ_checks       = 'X'
        TABLES
          lt_dockey              = lt_cvddockey
        EXCEPTIONS
          doc_not_found          = 1
          illegal_status         = 2
          general_failure        = 3
          status_undefined       = 4
          lock_failed            = 5
          update_status_failed   = 6
          update_protocol_failed = 7
          OTHERS                 = 8.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid         = zcx_grf_qm_cert=>gc_change_doc_status
            gv_return_code = CONV num4( sy-subrc ).
      ENDIF.
    ENDIF.
*
*&******************************************************************************************
* get next version for document
*&******************************************************************************************
    WRITE im_cert_header-sd_doc TO lwa_charval-charvalue NO-ZERO.

    CALL METHOD me->get_next_version_for_doc
      EXPORTING
        im_vbeln      = lv_vbeln
        im_vbelp      = lv_vbelp
        im_vkorg      = lv_vkorg
        im_sernr      = lv_sernr
        im_matnr      = lv_matnr
        im_cert_matnr = lv_cert_matnr
        im_next       = 'X'
      IMPORTING
        ex_next_dokvr = lv_next_dokvr
        ex_doknr      = lv_doknr.

    lv_dokvr = lv_next_dokvr.

*&******************************************************************************************
*& Build document number, filename and description
*&******************************************************************************************
* gv_doc_no - Example: 99063043P117170001_BJ02
    CONCATENATE im_cert_header-sernr '_' im_cert_header-werks    "99063043P117170001_BJ02
      INTO lv_doc_no.

* gv_file_name - Example: CRN1S-15_A99063043P117170001_BJ02_96507929.pdf
*    CONCATENATE im_cert_pump-model2 lv_doc_no INTO lv_file_name.    "A + 99063043P117170001_BJ02
    CONCATENATE im_cert_pump-tech_data_dcr_09 lv_doc_no INTO lv_file_name.    "A + 99063043P117170001_BJ02
*    gv_file_name = gv_doc_no.     "99063043P117170001_BJ02

*    IF im_cert_pump-combtype IS NOT INITIAL.
*      CONCATENATE im_cert_pump-combtype '_' lv_file_name
*      INTO lv_file_name.                       "CRN1S-15_ + A99063043P117170001_BJ02
*    ENDIF.
    IF im_cert_pump-tech_data_dcr_18 IS NOT INITIAL.
      CONCATENATE im_cert_pump-tech_data_dcr_18 '_' lv_file_name
      INTO lv_file_name.                       "CRN1S-15_ + A99063043P117170001_BJ02
    ENDIF.

* CRN1S-15_A99063043P117170001_BJ02 + _96507929_SHORT-TEXT + .pdf
    CONCATENATE lv_file_name '_' lv_cmatnr '_' im_cert_header-cert_desc '_' lv_dokvr lc_ftype INTO lv_file_name.

* lv_descr - Example: A99063043P117170001_BJ02_96507929_EN
*    CONCATENATE im_cert_pump-model2 lv_doc_no '_' lv_cmatnr '_' lv_lagrf
*        INTO lv_descr.                        " A + 99063043P117170001_BJ02 + _96507929 + _EN
    CONCATENATE im_cert_pump-tech_data_dcr_09 lv_doc_no '_' lv_cmatnr '_' lv_lagrf
        INTO lv_descr.                        " A + 99063043P117170001_BJ02 + _96507929 + _EN

*&******************************************************************************************
*& Write file to ?
*&******************************************************************************************

    CONCATENATE lc_docpath lv_file_name INTO lv_newfile.

    TRY.
        OPEN DATASET lv_newfile FOR OUTPUT IN BINARY MODE.

        TRANSFER im_pdf_output-pdf TO lv_newfile.

      CATCH cx_sy_file_authority INTO lr_excep.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid     = zcx_grf_qm_cert=>gc_open_dataset_error
            gv_errtext = lr_excep->get_text( ).

      CATCH cx_sy_file_io INTO lr_excep.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid     = zcx_grf_qm_cert=>gc_open_dataset_error
            gv_errtext = lr_excep->get_text( ).

      CATCH cx_sy_file_open INTO lr_excep.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid     = zcx_grf_qm_cert=>gc_open_dataset_error
            gv_errtext = lr_excep->get_text( ).

      CATCH cx_sy_file_open_mode INTO lr_excep.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid     = zcx_grf_qm_cert=>gc_open_dataset_error
            gv_errtext = lr_excep->get_text( ).

      CATCH cx_sy_too_many_files INTO lr_excep.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid     = zcx_grf_qm_cert=>gc_open_dataset_error
            gv_errtext = lr_excep->get_text( ).
    ENDTRY.

    CLOSE DATASET lv_newfile.

*&******************************************************************************************
*& Check if already exists - DMS and classification
*&******************************************************************************************
* FM ?
*&******************************************************************************************
*& update DMS and classification
*&******************************************************************************************
    CLEAR: lwa_docdata_2, lwa_docstruct, lt_docstruct[].

*     DCR document as PDF attachment
    lwa_docstruct-documenttype    = gc_dcr.
    lwa_docstruct-documentpart    = '000'.
    lwa_docstruct-documentnumber  = lv_doknr.
    lwa_docstruct-documentversion = lv_dokvr.
    lwa_docstruct-storagecategory = gv_storagecat.
    lwa_docstruct-wsapplication   = 'PDF'.
    lwa_docstruct-docpath         = lc_docpath.
    lwa_docstruct-docfile         = lv_file_name.
    lwa_docstruct-description     = lv_descr.
    lwa_docstruct-language        = im_cert_header-langu.
    APPEND lwa_docstruct TO lt_docstruct.

*     General document data
    lwa_docdata_2-documenttype    = lwa_docstruct-documenttype.
    lwa_docdata_2-documentnumber  = lv_doknr.
    lwa_docdata_2-documentversion = lwa_docstruct-documentversion.
    lwa_docdata_2-documentpart    = lwa_docstruct-documentpart.
    lwa_docdata_2-description     = lwa_docstruct-description.
    lwa_docdata_2-username        = sy-uname.
    lwa_docdata_2-statusextern    = 'CR'.    "Create
    lwa_docdata_2-statusintern    = 'CR'.

*   Attaching external documents CER, TCR + SCR
*   The same document can exist more than once, but should only be attached one time
    lt_doc_tab[] = im_so_item-doc_tab[].
    SORT lt_doc_tab BY docfile.
    DELETE ADJACENT DUPLICATES FROM lt_doc_tab COMPARING docfile.

    LOOP AT lt_doc_tab ASSIGNING FIELD-SYMBOL(<l_doc>).

      me->get_pdf_as_binary(
        EXPORTING
          im_doc          = <l_doc>
        IMPORTING
          ex_xstring      = lv_xstring
          ex_fname        = lv_fname ).

*     Create new file
      GET TIME STAMP FIELD lv_tstamp.
      lv_tstamp_c = lv_tstamp.
      SPLIT lv_fname AT '.' INTO lv_fname1 lv_fname2.
      CONCATENATE lwa_docfiles-docpath lv_fname1 '_' lv_tstamp_c '.' lv_fname2 INTO lv_newfile.

      TRY.
          OPEN DATASET lv_newfile FOR OUTPUT IN BINARY MODE.

          TRANSFER lv_xstring TO lv_newfile.

        CATCH cx_sy_file_authority INTO lr_excep.
          RAISE EXCEPTION TYPE zcx_grf_qm_cert
            EXPORTING
              textid     = zcx_grf_qm_cert=>gc_open_dataset_error
              gv_errtext = lr_excep->get_text( ).

        CATCH cx_sy_file_io INTO lr_excep.
          RAISE EXCEPTION TYPE zcx_grf_qm_cert
            EXPORTING
              textid     = zcx_grf_qm_cert=>gc_open_dataset_error
              gv_errtext = lr_excep->get_text( ).

        CATCH cx_sy_file_open INTO lr_excep.
          RAISE EXCEPTION TYPE zcx_grf_qm_cert
            EXPORTING
              textid     = zcx_grf_qm_cert=>gc_open_dataset_error
              gv_errtext = lr_excep->get_text( ).


        CATCH cx_sy_file_open_mode INTO lr_excep.
          RAISE EXCEPTION TYPE zcx_grf_qm_cert
            EXPORTING
              textid     = zcx_grf_qm_cert=>gc_open_dataset_error
              gv_errtext = lr_excep->get_text( ).

        CATCH cx_sy_too_many_files INTO lr_excep.
          RAISE EXCEPTION TYPE zcx_grf_qm_cert
            EXPORTING
              textid     = zcx_grf_qm_cert=>gc_open_dataset_error
              gv_errtext = lr_excep->get_text( ).

      ENDTRY.

      CLOSE DATASET lv_newfile.

      lwa_docstruct-documenttype    = <l_doc>-documenttype.
      lwa_docstruct-documentpart    = <l_doc>-documentpart.
      lwa_docstruct-documentnumber  = <l_doc>-documentnumber.
      lwa_docstruct-documentversion = <l_doc>-documentversion.
      lwa_docstruct-storagecategory = lwa_docfiles-storagecategory.
      lwa_docstruct-wsapplication   = lwa_docfiles-wsapplication.
      lwa_docstruct-docpath         = lwa_docfiles-docpath.
      lwa_docstruct-docfile         = lv_newfile.
      lwa_docstruct-description     = lwa_docfiles-description.
      lwa_docstruct-language        = 'E'.
      APPEND lwa_docstruct TO lt_docstruct.
    ENDLOOP.

    lwa_docdatax-documenttype    = 'X'.
    lwa_docdatax-documentnumber  = 'X'.
    lwa_docdatax-documentversion = 'X'.
    lwa_docdatax-documentpart    = 'X'.
    lwa_docdatax-description     = 'X'.
    lwa_docdatax-username        = 'X'.
    lwa_docdatax-statusextern    = 'X'.
    lwa_docdatax-statusintern    = 'X'.

    lwa_classallo-classtype = lc_classtp.       "'017'.
    lwa_classallo-classname = lc_classnm.       "'CERTIFICATE_REPORT'.
    APPEND lwa_classallo TO lt_bapi_class_allocation.

*   Call of this function module will remove authorization check for transaction CV01.
    CALL FUNCTION 'CV115_TCODE_CHECK'
      EXPORTING
        iv_no_tcode_check = abap_true.

    CALL FUNCTION 'API_DOCUMENT_MAINTAIN2'
      EXPORTING
        pf_transaction       = 'CV01'
*       PS_API_CONTROL       =
*       PF_REPLACE_FILE      = ' '
        documentdata         = lwa_docdata_2
        documentdatax        = lwa_docdatax
*       HOSTNAME             = ' '
*       CHANGE_NUMBER        =
*       VALID_FROM           =
*       REVISION_LEVEL       =
*       SENDCOMPLETEBOM      = ' '
        pf_ftp_dest          = 'SAPFTPA'
*       PF_HTTP_DEST         = ' '
*       CAD_MODE             = ' '
*       ACCEPT_EMPTY_BOM     = ' '
*       DEFAULTCLASS         =
*       MUL_ITEMS_DELETE     = ' '
*       PF_GUI               =
*       PF_ACTION            =
*       NO_EXIT_CALL         = ' '
      IMPORTING
        documenttype         = lwa_docdata_2-documenttype
        documentnumber       = lwa_docdata_2-documentnumber
        documentpart         = lwa_docdata_2-documentpart
        documentversion      = lwa_docdata_2-documentversion
        return               = lv_return
*       PFX_NEW_STATUS       =
      TABLES
        characteristicvalues = lt_charval
        classallocations     = lt_bapi_class_allocation
        documentfiles        = lt_docstruct.

    IF lv_return-type CA 'EAW'.
      ROLLBACK WORK.
      IF lv_return-id = '26' AND lv_return-number = '002'.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid = zcx_grf_qm_cert=>gc_doc_already_exists.
      ELSE.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid = zcx_grf_qm_cert=>gc_error_when_create_dir.
      ENDIF.
    ELSE.
      COMMIT WORK AND WAIT.

      REFRESH: lt_cvddockey[].

      lv_protf = sy-uname.

      lwa_cvddockey-dokar = lwa_docdata_2-documenttype.
      lwa_cvddockey-doknr = lwa_docdata_2-documentnumber.
      lwa_cvddockey-dokvr = lwa_docdata_2-documentversion.
      lwa_cvddockey-doktl = lwa_docdata_2-documentpart.

      APPEND lwa_cvddockey TO lt_cvddockey.

      CALL FUNCTION 'Z_ZD010_CHANGE_DOC_STATUS'
        EXPORTING
          i_status               = 'RE'
          i_protf                = lv_protf
          i_do_changes           = 'X'
          i_do_succ_checks       = 'X'
        TABLES
          lt_dockey              = lt_cvddockey
        EXCEPTIONS
          doc_not_found          = 1
          illegal_status         = 2
          general_failure        = 3
          status_undefined       = 4
          lock_failed            = 5
          update_status_failed   = 6
          update_protocol_failed = 7
          OTHERS                 = 8.

      IF sy-subrc <> 0.
        ROLLBACK WORK.

        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid         = zcx_grf_qm_cert=>gc_error_when_release_set
            gv_return_code = CONV num4( sy-subrc ).
      ELSE.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

*&******************************************************************************************
* cleaning up UNIX files (delete)
*&******************************************************************************************
    DELETE DATASET lv_newfile.

  ENDMETHOD.


  METHOD create_pdf.

    DATA: lv_fm_name    TYPE funcname,
          lv_params     TYPE sfpoutputparams,
          lv_docparams  TYPE sfpdocparams,
          lv_formoutput TYPE fpformoutput.

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'               " Will contain the name of generated Function Module Name...
          EXPORTING
            i_name     = im_pdf_form_name
          IMPORTING
            e_funcname = lv_fm_name.

      CATCH cx_root INTO gv_cx_root.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid       = zcx_grf_qm_cert=>gc_form_not_found    "No PDF form found
            gv_form_name = im_pdf_form_name.
    ENDTRY.


*    lv_params-device = 'PRINTER'.

    lv_params-getpdf = ' '.


*ie_outputparams-getpdf = 'X'.

*    lv_params-noprint = 'X'.
*    lv_params-dynamic = 'X'.   ??

    lv_params-nodialog = 'X'.
    lv_params-preview  = 'X'.
    lv_params-dest     = '%L01'. " Destination
    lv_params-reqimm   = 'X'.
    lv_params-pdfchangesrestricted = 'X'.  "No changes allowed

*ie_outputparams-reqnew = 'X'.
* out-reqdel = 'X'.
*ie_outputparams-reqfinal = 'X'.

*   Open the Form for Printing
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = lv_params
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_form_open_error
          gv_form_name   = im_pdf_form_name
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.


    lv_docparams-langu   = sy-langu.  "To be changed....
    ch_cert_header-langu = lv_docparams-langu.

    TRY.
        CALL FUNCTION lv_fm_name                                " Generated Function Module(/1BCDWB/SM00000167)...
          EXPORTING
            /1bcdwb/docparams  = lv_docparams
            im_head            = ch_cert_header
            im_pump_char       = im_cert_pump
            im_comp            = im_cert_comp
            im_comp_extra      = im_cert_comp_extra
            im_roughness       = im_roughness
            im_atex            = im_atex
            im_vibration       = im_vibration
          IMPORTING
            /1bcdwb/formoutput = ex_pdf_output
          EXCEPTIONS
            usage_error        = 1
            system_error       = 2
            internal_error     = 3
            OTHERS             = 4.

      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid         = zcx_grf_qm_cert=>gc_form_excution_error
            gv_form_name   = im_pdf_form_name
            gv_return_code = 9999.

    ENDTRY.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_form_excution_error
          gv_form_name   = im_pdf_form_name
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.

*Close The Form For Printing
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_form_close_error
          gv_form_name   = im_pdf_form_name
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.

  ENDMETHOD.


  METHOD create_pdf_noview.
* this method CREATE_PDF_NOVIEW is only used for creating file for CV04N
* preview not allowed
    DATA: lv_fm_name    TYPE funcname,
          lv_params     TYPE sfpoutputparams,
          lv_docparams  TYPE sfpdocparams,
          lv_formoutput TYPE fpformoutput.

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'               " Will contain the name of generated Function Module Name...
          EXPORTING
            i_name     = im_pdf_form_name
          IMPORTING
            e_funcname = lv_fm_name.

      CATCH cx_root INTO gv_cx_root.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid       = zcx_grf_qm_cert=>gc_form_not_found    "No PDF form found
            gv_form_name = im_pdf_form_name.
    ENDTRY.


*    lv_params-device = 'PRINTER'.

    lv_params-getpdf = 'X'.

*    lv_params-noprint = 'X'.
    lv_params-nodialog = 'X'.
*    lv_params-preview  = 'X'.
*    lv_params-dest     = '%L01'. " Destination
    lv_params-nopreview = 'X'.
    lv_params-reqimm   = 'X'.
    lv_params-pdfchangesrestricted = 'X'.  "No changes allowed

*   Open the Form for Printing
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = lv_params
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_form_open_error
          gv_form_name   = im_pdf_form_name
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.


    lv_docparams-langu   = sy-langu.  "To be changed....
    ch_cert_header-langu = lv_docparams-langu.

    TRY.
        CALL FUNCTION lv_fm_name                                " Generated Function Module(/1BCDWB/SM00000167)...
          EXPORTING
            /1bcdwb/docparams  = lv_docparams
            im_head            = ch_cert_header
            im_pump_char       = im_cert_pump
            im_comp            = im_cert_comp
            im_comp_extra      = im_cert_comp_extra
            im_roughness       = im_roughness
            im_atex            = im_atex
            im_vibration       = im_vibration
            im_preview         = ' '   "1st ='X' - text = preview. 2nd = ' ' - text = demo(in X and Q)
          IMPORTING
            /1bcdwb/formoutput = ex_pdf_output
          EXCEPTIONS
            usage_error        = 1
            system_error       = 2
            internal_error     = 3
            OTHERS             = 4.

      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid         = zcx_grf_qm_cert=>gc_form_excution_error
            gv_form_name   = im_pdf_form_name
            gv_return_code = 9999.

    ENDTRY.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_form_excution_error
          gv_form_name   = im_pdf_form_name
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.

*Close The Form For Printing
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_form_close_error
          gv_form_name   = im_pdf_form_name
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.

  ENDMETHOD.


  METHOD create_pdf_preview.
* this method CREATE_PDF_PREVIEW is only used for presenting the form for the user
* print and changes not allowed

    DATA: lv_fm_name    TYPE funcname,
          lv_params     TYPE sfpoutputparams,
          lv_docparams  TYPE sfpdocparams,
          lv_formoutput TYPE fpformoutput.

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'               " Will contain the name of generated Function Module Name...
          EXPORTING
            i_name     = im_pdf_form_name
          IMPORTING
            e_funcname = lv_fm_name.

      CATCH cx_root INTO gv_cx_root.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid       = zcx_grf_qm_cert=>gc_form_not_found    "No PDF form found
            gv_form_name = im_pdf_form_name.
    ENDTRY.



    lv_params-getpdf = ' '.

*    lv_params-noprint  = 'X'.
    lv_params-nodialog = 'X'.
    lv_params-preview  = 'X'.
    lv_params-dest     = '%L01'. " Destination
    lv_params-reqimm   = 'X'.
    lv_params-pdfchangesrestricted = 'X'.  "No changes allowed


*   Open the Form for Printing
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = lv_params
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_form_open_error
          gv_form_name   = im_pdf_form_name
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.


    lv_docparams-langu   = sy-langu.  "To be changed....
    ch_cert_header-langu = lv_docparams-langu.

    TRY.
        CALL FUNCTION lv_fm_name                                " Generated Function Module(/1BCDWB/SM00000167)...
          EXPORTING
            /1bcdwb/docparams = lv_docparams
            im_head           = ch_cert_header
            im_pump_char      = im_cert_pump
            im_comp           = im_cert_comp
            im_comp_extra     = im_cert_comp_extra
            im_roughness      = im_roughness
            im_atex           = im_atex
            im_vibration      = im_vibration
            im_preview        = 'X'   "1st ='X' - text = preview. 2nd = ' ' - text = demo(in X and Q)
*          IMPORTING
*           /1bcdwb/formoutput = ex_pdf_output
          EXCEPTIONS
            usage_error       = 1
            system_error      = 2
            internal_error    = 3
            OTHERS            = 4.

      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid         = zcx_grf_qm_cert=>gc_form_excution_error
            gv_form_name   = im_pdf_form_name
            gv_return_code = 9999.

    ENDTRY.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_form_excution_error
          gv_form_name   = im_pdf_form_name
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.

*Close The Form For Printing
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_form_close_error
          gv_form_name   = im_pdf_form_name
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.

  ENDMETHOD.


  METHOD get_classification.

    DATA: ls_components TYPE abap_compdescr.
    DATA: lo_strucdescr TYPE REF TO cl_abap_structdescr.
    DATA: ls_cert_pump  TYPE zscert_pump_char.

    DATA: lt_class      TYPE TABLE OF sclass,
          lt_objectdata TYPE TABLE OF clobjdat,
          lv_object     TYPE ausp-objek,
          lt_sel_char   TYPE TABLE OF sel_char,
          lv_atinn      TYPE atinn.

    CLEAR lt_sel_char[].


    lo_strucdescr ?= cl_abap_typedescr=>describe_by_data( ls_cert_pump ).

    LOOP AT lo_strucdescr->components INTO ls_components.
      CHECK ls_components-name(2) NE 'ZZ'.      "Field starting with ZZ are calculated fields

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = ls_components-name
        IMPORTING
          output = lv_atinn.

      IF sy-subrc = 0.
        APPEND lv_atinn TO lt_sel_char.
      ELSE.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid              = zcx_grf_qm_cert=>gc_classification_atinn
            gv_characteric_name = ls_components-name.
      ENDIF.
    ENDLOOP.

    lv_object = im_matnr.

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
*       CLASS                = ' '
*       CLASSTEXT            = 'X'
        classtype            = '001'
*       CLINT                = 0
        features             = 'X'
*       LANGUAGE             = SY-LANGU
        object               = lv_object
*       OBJECTTABLE          = ' '
*       KEY_DATE             = SY-DATUM
*       INITIAL_CHARACT      = 'X'
*       NO_VALUE_DESCRIPT    =
*       CHANGE_SERVICE_CLF   = 'X'
*       INHERITED_CHAR       = ' '
*       CHANGE_NUMBER        = ' '
      TABLES
        t_class              = lt_class
        t_objectdata         = lt_objectdata
        i_sel_characteristic = lt_sel_char
*       T_NO_AUTH_CHARACT    =
      EXCEPTIONS
        no_classification    = 1
        no_classtypes        = 2
        invalid_class_type   = 3
        OTHERS               = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_classification_error
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.
*
    LOOP AT lt_objectdata ASSIGNING FIELD-SYMBOL(<l_objdata>).
      DATA(lv_fname) = 'ex_cert_pump-' && <l_objdata>-atnam.

      ASSIGN (lv_fname) TO FIELD-SYMBOL(<l_fs>).
      IF sy-subrc = 0.
        SELECT atfor
          INTO @DATA(lv_atfor)
          FROM cabn UP TO 1 ROWS
          WHERE atnam = @<l_objdata>-atnam.
        ENDSELECT.

        IF sy-subrc = 0.
          IF lv_atfor = 'CHAR'.
            <l_fs> = <l_objdata>-ausp1.
          ELSEIF lv_atfor = 'NUM'.
            TRY.
                <l_fs> = <l_objdata>-atflv.

              CATCH cx_sy_conversion_no_number.
                RAISE EXCEPTION TYPE zcx_grf_qm_cert
                  EXPORTING
                    textid              = zcx_grf_qm_cert=>gc_classification_conv_no_num
                    gv_characteric_name = CONV atnam( lv_fname ).

              CATCH cx_root.
                RAISE EXCEPTION TYPE zcx_grf_qm_cert
                  EXPORTING
                    textid              = zcx_grf_qm_cert=>gc_classification_conv
                    gv_characteric_name = CONV atnam( lv_fname ).
            ENDTRY.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_classification_cert.
    CONSTANTS: lc_cert_desc(8) VALUE 'TEKST014'.

    DATA: lt_class      TYPE TABLE OF sclass,
          lt_objectdata TYPE TABLE OF clobjdat,
          lv_object     TYPE ausp-objek,
          lt_sel_char   TYPE TABLE OF sel_char,
          lv_atinn      TYPE atinn.

    CLEAR lt_sel_char[].

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = lc_cert_desc
      IMPORTING
        output = lv_atinn.

    IF sy-subrc = 0.
      APPEND lv_atinn TO lt_sel_char.
    ELSE.
      EXIT.
    ENDIF.

    lv_object = im_matnr.

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
*       CLASS                = ' '
*       CLASSTEXT            = 'X'
        classtype            = '001'
*       CLINT                = 0
        features             = 'X'
*       LANGUAGE             = SY-LANGU
        object               = lv_object
*       OBJECTTABLE          = ' '
*       KEY_DATE             = SY-DATUM
*       INITIAL_CHARACT      = 'X'
*       NO_VALUE_DESCRIPT    =
*       CHANGE_SERVICE_CLF   = 'X'
*       INHERITED_CHAR       = ' '
*       CHANGE_NUMBER        = ' '
      TABLES
        t_class              = lt_class
        t_objectdata         = lt_objectdata
        i_sel_characteristic = lt_sel_char
*       T_NO_AUTH_CHARACT    =
      EXCEPTIONS
        no_classification    = 1
        no_classtypes        = 2
        invalid_class_type   = 3
        OTHERS               = 4.

    IF sy-subrc = 0.
      LOOP AT lt_objectdata ASSIGNING FIELD-SYMBOL(<l_objdata>).
        ex_cert_desc = <l_objdata>-ausp1.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_comp_data.
    DATA: lv_matnr TYPE matnr_d.

    CLEAR ex_comp_data[].

*   Read Heat data components and values
    SELECT c~comp_txt a~matnr b~vendor_name a~heat_data
      FROM zqcert_comp_data AS a JOIN zqcert_vendor AS b ON a~vendor_code = b~vendor_code
           JOIN zqcert_comp AS c ON a~comp_no = c~comp_no
      INTO CORRESPONDING FIELDS OF TABLE ex_comp_data
      WHERE a~werks = im_werks
        AND a~sernr = im_sernr
        AND c~spras = im_spras
      ORDER BY a~comp_no.

    IF sy-subrc NE 0.   "No data found for serial number
      RETURN.
    ENDIF.

    LOOP AT ex_comp_data ASSIGNING FIELD-SYMBOL(<l_comp_data>).
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <l_comp_data>-matnr
        IMPORTING
          output = lv_matnr.

      me->get_material_grade(
        EXPORTING
          im_matnr = lv_matnr
        IMPORTING
          ex_grade = <l_comp_data>-grade ).
      TRANSLATE <l_comp_data>-grade TO UPPER CASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_comp_grade.

    DATA: lv_parkey TYPE zparkey,
          lv_found  TYPE xfeld,
          lv_value  TYPE zvalue.

    DATA: lv_pump_type  TYPE zpumptype,
          lv_pump_size  TYPE zpumpsize,
          lv_cert_matnr TYPE matnr,
          ls_grade_tab  TYPE zscert_comp_data_extra.

    CLEAR ex_grade_tab[].

    lv_parkey = im_cert_pump-tech_data_dcr_22.   "Pump type.

    CALL METHOD zcl_param=>read
      EXPORTING
        area    = 'QM'
        subarea = 'CERT'
        parname = 'PUMPTYPE'
        parkey  = lv_parkey
      IMPORTING
        found   = lv_found
        value   = lv_value.

    IF lv_found = 'X'.
      lv_pump_type = lv_value(10).
    ELSE.
      "Error ......
    ENDIF.

    lv_pump_size = im_cert_pump-tech_data_dcr_21.   "Pump size.

    SELECT a~line_id, b~comp_txt, b~comp_txt2, a~mat_grade, a~rawmat_grade, a~cert_templ1, a~cert_templ2, a~cert_templ3, a~cert_templ4, a~cert_templ5
      INTO TABLE @DATA(lt_grade_tab)
      FROM zqcert_grade AS a JOIN zqcert_grade_txt AS b
        ON a~line_id = b~line_id
      WHERE a~pump_type = @lv_pump_type
        AND a~pump_size = @lv_pump_size
        AND b~spras = @im_spras
      ORDER BY a~line_id.

    IF sy-subrc NE 0.
      "error
      EXIT.
    ENDIF.

*   Remove leading zeroes from certificate material number
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = im_cert_matnr
      IMPORTING
        output = lv_cert_matnr.

*   Find the lines that should be used at the form
    LOOP AT lt_grade_tab ASSIGNING FIELD-SYMBOL(<l_grade>).
      IF <l_grade>-cert_templ1 = lv_cert_matnr OR
         <l_grade>-cert_templ2 = lv_cert_matnr OR
         <l_grade>-cert_templ3 = lv_cert_matnr OR
         <l_grade>-cert_templ4 = lv_cert_matnr OR
         <l_grade>-cert_templ5 = lv_cert_matnr.

        ls_grade_tab-lineid = <l_grade>-line_id.
        ls_grade_tab-comp_txt = <l_grade>-comp_txt.
        ls_grade_tab-comp_txt2 = <l_grade>-comp_txt2.
        ls_grade_tab-mat_grade = <l_grade>-mat_grade.
        ls_grade_tab-rawmat_grade = <l_grade>-rawmat_grade.
        APPEND ls_grade_tab TO ex_grade_tab.
      ENDIF.
    ENDLOOP.


**   Find the lines that should be used at the form
*    CALL METHOD zcl_param=>read
*      EXPORTING
*        area    = 'QM'
*        subarea = 'CERT'
*        parname = 'PARTS_EN10204_2_2'
*        parkey  = lv_parkey
*      IMPORTING
*        found   = lv_found
*        value   = lv_value.

*    IF lv_found = 'X'.
*      SPLIT lv_value  AT '#' INTO TABLE lt_lineid.
*      SORT lt_lineid.
*
*      LOOP AT ex_grade_tab ASSIGNING FIELD-SYMBOL(<l_grade>).
*        lv_tabix = sy-tabix.
*        READ TABLE lt_lineid WITH KEY lineid = <l_grade> BINARY SEARCH TRANSPORTING NO FIELDS.
*        IF sy-subrc NE 0.
*          DELETE ex_grade_tab INDEX lv_tabix.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.

  ENDMETHOD.


  METHOD get_email_recipients.
    CLEAR ex_recipients[].

*   Find sales org.
    SELECT SINGLE vkorg
      INTO @DATA(lv_vkorg)
      FROM vbak
      WHERE vbeln = @im_vbeln.

    IF sy-subrc = 0.
*     Get mail addresses for sales org.
      SELECT email
        INTO TABLE ex_recipients
        FROM zqcert_email
        WHERE vkorg = lv_vkorg.

      IF sy-subrc NE 0.
        CHECK 1 = 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_external_doc.

    SELECT *
      FROM draw UP TO 1 ROWS
      INTO ex_draw
      WHERE dokar = im_dokar
        AND doknr LIKE im_doknr
        AND dokst = 'RE'.
    ENDSELECT.

    IF sy-subrc = 0.

    ELSE.
      CLEAR ex_draw.
    ENDIF.

  ENDMETHOD.


  METHOD get_header_data.

    DATA: lv_vbpavb TYPE vbpavb,
          lv_pnum   TYPE ad_persnum,
          lv_pernr  TYPE pernr_bi,
          lv_adrnr  TYPE adrnr,
          lv_addr   TYPE addr1_val.

    DATA: lv_land1 TYPE land1,
          lv_bukrs TYPE bukrs,
          lv_bwkey TYPE bwkey,
          lv_cadnr TYPE adrnr.

    DATA: lv_vbeln TYPE vbeln.

    CLEAR: ex_cert_head.

*   Set sales doc
    ex_cert_head-sd_doc = im_vbeln.
    ex_cert_head-itm_number = im_posnr.

*   Set plant
    ex_cert_head-werks = im_werks.

*   Set certificate material number
    ex_cert_head-cert_matnr = im_matnr.

*   Set serial number
    ex_cert_head-sernr = im_sernr.

*   Set service order number
    ex_cert_head-service_order = im_aufnr.

*   Set reference order number
    ex_cert_head-ref_order = im_refno.

*   Grundfos sales order number
    IF im_aufnr IS NOT INITIAL.
      ex_cert_head-grf_order = im_aufnr.     "Service order
    ELSEIF im_refno IS NOT INITIAL.
      ex_cert_head-grf_order = im_refno.     "Reference sales order
    ELSE.
      ex_cert_head-grf_order = im_vbeln.     "Sales order
    ENDIF.

    IF im_refno IS INITIAL.
      lv_vbeln = im_vbeln.
    ELSE.
      lv_vbeln = im_refno.
    ENDIF.

*   Read order header
    SELECT SINGLE vkorg kunnr
      INTO (ex_cert_head-sales_org, ex_cert_head-sold_to_no)
      FROM vbak
      WHERE vbeln = lv_vbeln.

    IF sy-subrc NE 0.
      MESSAGE e014(zgrf_qm_cert) WITH 'VBAK' lv_vbeln space.  "Entry not found in table &1 for &2 &3
      LEAVE PROGRAM.
    ENDIF.

*   Read customer PO number
    SELECT SINGLE bstkd
      INTO ex_cert_head-purch_no
      FROM vbkd
      WHERE vbeln = lv_vbeln
        AND posnr = '000000'.

    IF sy-subrc NE 0.
      MESSAGE e014(zgrf_qm_cert) WITH 'VBKD' lv_vbeln space.  "Entry not found in table &1 for &2 &3
      LEAVE PROGRAM.
    ENDIF.

*   Read order item
    SELECT SINGLE matnr
      INTO ex_cert_head-material
      FROM vbap
      WHERE vbeln = im_vbeln
        AND posnr = im_posnr.

    IF sy-subrc NE 0.
      MESSAGE e014(zgrf_qm_cert) WITH 'VBAP' im_vbeln im_posnr.  "Entry not found in table &1 for &2 &3
      LEAVE PROGRAM.
    ENDIF.

*   Read sold-to party name from sales order or service order
    IF im_aufnr IS NOT INITIAL.
      me->get_service_order_data(
        EXPORTING
          im_aufnr     = im_aufnr
        IMPORTING
          ex_cert_head = ex_cert_head ).
    ELSE.
      SELECT SINGLE adrnr
        INTO lv_adrnr
        FROM vbpa
        WHERE vbeln = lv_vbeln
          AND posnr = '000000'
          AND parvw = 'AG'.

      IF sy-subrc NE 0.
        MESSAGE e014(zgrf_qm_cert) WITH 'VBPA' lv_vbeln 'AG'.  "Entry not found in table &1 for &2 &3
        LEAVE PROGRAM.
      ENDIF.

      lv_addr = read_address( lv_adrnr ).

      ex_cert_head-name1 = lv_addr-name1.
      ex_cert_head-name2 = lv_addr-name2.
    ENDIF.

*   Read user name
    SELECT SINGLE persnumber
      INTO lv_pnum
      FROM usr21
      WHERE bname = sy-uname.

    IF sy-subrc NE 0.
      MESSAGE e014(zgrf_qm_cert) WITH 'USR21' sy-uname space.  "Entry not found in table &1 for &2 &3
      LEAVE PROGRAM.
    ENDIF.

    SELECT name_text
      INTO ex_cert_head-usr_name
      FROM adrp UP TO 1 ROWS
      WHERE persnumber = lv_pnum.
    ENDSELECT.

    IF sy-subrc NE 0.
      MESSAGE e014(zgrf_qm_cert) WITH 'ADRP' lv_pnum space.    "Entry not found in table &1 for &2 &3
      LEAVE PROGRAM.
    ENDIF.

*   Get users employee number
    SELECT SINGLE parva
      INTO lv_pernr
      FROM usr05
      WHERE bname = sy-uname
        AND parid = 'PER'.

    IF sy-subrc NE 0.
      MESSAGE e014(zgrf_qm_cert) WITH 'USR05' sy-uname space.   "Entry not found in table &1 for &2 &3
      LEAVE PROGRAM.
    ENDIF.

*   Get title of user from HR system
    CALL FUNCTION 'Z_GET_EMP_POSITION' DESTINATION gv_destination
      EXPORTING
        im_pernr         = lv_pernr
      IMPORTING
*       EX_POSITION_SHORT       =
        ex_position_long = ex_cert_head-usr_title.
*       EX_DEPT_NO              =
*       EX_DEPT_TEXT            =

*   Get value for certificate description
    get_classification_cert(
      EXPORTING
        im_matnr        = im_matnr
      IMPORTING
        ex_cert_desc    = ex_cert_head-cert_desc ).


*************************************************************************************
*   Get address for signature
    GET PARAMETER ID 'ZX_COMPANYCODE' FIELD lv_bukrs.

    SELECT SINGLE adrnr bwkey ort01 stras pstlz land1
      INTO (lv_cadnr, lv_bwkey, ex_cert_head-ort01, ex_cert_head-stras, ex_cert_head-pstlz, lv_land1)
      FROM t001w
      WHERE werks = ex_cert_head-werks.

    IF sy-subrc NE 0.
      CLEAR: lv_land1.
    ENDIF.

    SELECT SINGLE landx
      INTO ex_cert_head-landx
      FROM t005t
      WHERE spras = sy-langu
      AND   land1 = lv_land1.

    IF sy-subrc NE 0.
      CLEAR: lv_land1.
    ENDIF.

    SELECT SINGLE butxt
      INTO ex_cert_head-butxt
      FROM t001
      WHERE bukrs = lv_bukrs.

    IF sy-subrc NE 0.
      CLEAR: lv_land1.
    ENDIF.

* 14.03.2018 get address layout -
*            into ex_cert_head-ADDRESS1
*                 ex_cert_head-ADDRESS2
* need to figure out, how to use the address so it looks okay for country :)

*CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
* EXPORTING
**   ADRSWA_IN                            =
**   ADDRESS_1                            =
**   ADDRESS_2                            =
**   ADDRESS_3                            =
*   ADDRESS_TYPE                         = '1'
*   ADDRESS_NUMBER                       = lv_cadnr
**   ADDRESS_HANDLE                       = ' '
**   PERSON_NUMBER                        = ' '
**   PERSON_HANDLE                        = ' '
**   SENDER_COUNTRY                       = ' '
**   RECEIVER_LANGUAGE                    = ' '
*   NUMBER_OF_LINES                      = 10
**   STREET_HAS_PRIORITY                  = ' '
**   LINE_PRIORITY                        = ' '
**   COUNTRY_NAME_IN_RECEIVER_LANGU       = ' '
**   LANGUAGE_FOR_COUNTRY_NAME            = ' '
**   NO_UPPER_CASE_FOR_CITY               = ' '
**   IV_NATION                            = ' '
**   IV_NATION_SPACE                      = ' '
**   IV_PERSON_ABOVE_ORGANIZATION         = ' '
**   IS_BUPA_TIME_DEPENDENCY              = ' '
**   IV_COUNTRY_NAME_SEPARATE_LINE        = ' '
**   IV_LANGU_CREA                        = ' '
**   IV_DISPLAY_COUNTRY_IN_SHRTFORM       = ' '
**   BLK_EXCPT                            =
** IMPORTING
**   ADRSWA_OUT                           =
**   ADDRESS_PRINTFORM                    =
**   ADDRESS_SHORT_FORM                   =
**   ADDRESS_SHORT_FORM_S                 =
**   ADDRESS_DATA_CARRIER                 =
**   ADDRESS_DATA_CARRIER_0               =
**   NUMBER_OF_USED_LINES                 =
**   NAME_IS_EMPTY                        =
**   ADDRESS_NOT_FOUND                    =
**   ADDRESS_PRINTFORM_TABLE              =
**   ADDRESS_SHORT_FORM_WO_NAME           =
**   EV_NATION                            =
** EXCEPTIONS
**   ADDRESS_BLOCKED                      = 1
**   PERSON_BLOCKED                       = 2
**   CONTACT_PERSON_BLOCKED               = 3
**   ADDR_TO_BE_FORMATED_IS_BLOCKED       = 4
**   OTHERS                               = 5
*          .
*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.

  ENDMETHOD.


  METHOD get_material_grade.
* Read Characteristic BASIC_MATERIAL to find grade.
    DATA: lt_class      TYPE TABLE OF sclass,
          lt_objectdata TYPE TABLE OF clobjdat,
          lv_object     TYPE ausp-objek,
          lt_sel_char   TYPE TABLE OF sel_char,
          lv_atinn      TYPE atinn.

    CLEAR lt_sel_char[].

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'BASIC_MATERIAL'
      IMPORTING
        output = lv_atinn.

    IF sy-subrc = 0.
      APPEND lv_atinn TO lt_sel_char.

      lv_object = im_matnr.

      CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
        EXPORTING
          classtype            = '001'
          object               = lv_object
          inherited_char       = 'X'
        TABLES
          t_class              = lt_class
          t_objectdata         = lt_objectdata
          i_sel_characteristic = lt_sel_char
        EXCEPTIONS
          no_classification    = 1
          no_classtypes        = 2
          invalid_class_type   = 3
          OTHERS               = 4.

      IF sy-subrc = 0.
        READ TABLE lt_objectdata ASSIGNING FIELD-SYMBOL(<l_objdata>) INDEX 1.
        IF sy-subrc = 0.
          ex_grade = <l_objdata>-ausp1.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD get_mat_for_class.
    CHECK 1 = 1.  "To avoid ATC error.

*    DATA: lt_class   TYPE TABLE OF sclass,
*          lt_objects TYPE TABLE OF clobjekte.
*
*
*    CHECK im_class NE 'xxxxx'.    " To be removed.... when we have the new class for shaft
*
*    CLEAR: lt_class[], lt_objects[].
*
*    CALL FUNCTION 'CLAF_OBJECTS_OF_CLASS'
*      EXPORTING
*        class              = im_class   " Class number (external)
**       classes            = SPACE    " Include subordinate classes
*        classtext          = space   " Include class description (default: without)
*        classtype          = '001'    " Class type
*        features           = space    " Include characteristics (default: with)
**       language           = SY-LANGU    " Language
**       object_high        = SPACE    " to object number (optional)
**       object_low         = SPACE    " from object number (optional)
**       update_mode        = SPACE    " X = call in update, space = dialog
**       key_date           =     " Valid from
*        initial_charact    = ' '    " Display initial characteristics
*        no_value_descript  = 'X'     " Value instead of value description
*        change_service_clf = ' '
**       inherited_char     = SPACE    " With inherited characteristics
**       objecttable        = SPACE
**       change_number      = SPACE    " Change Number
*      TABLES
*        t_class            = lt_class " Class data
**       t_objectdata       =     " Classification data of object for classes
*        t_objects          = lt_objects " Objects for the class
**       i_sel_characteristic =     " Characteristics as additional selection criteria
**       t_no_auth_charact  =     " Characteristics without organizational area authorization
*      EXCEPTIONS
*        no_classification  = 1
*        invalid_class_type = 2
*        OTHERS             = 3.
*
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ELSE.
*      LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<l_object>).
*        APPEND <l_object>-objekt TO ex_matnr_tab.
*      ENDLOOP.
*    ENDIF.

  ENDMETHOD.


  METHOD get_next_version_for_doc.
    DATA: lt_zksml  TYPE zttzksml,
          ls_zksml  TYPE zksml,
          lt_ccomw  TYPE TABLE OF comw,
          ls_ccomw  TYPE comw,
          ls_origin TYPE cv100_radio_buttons,
          lt_draw   TYPE TABLE OF draw,
          ls_draw   TYPE draw,
          lv_lines  TYPE bapi_doc_aux-docversion.

    DATA: lt_dokar TYPE TABLE OF cv100_rangesdokar,
          ls_dokar TYPE cv100_rangesdokar,
          lt_loedk TYPE TABLE OF cv100_rangesloedk,
          ls_loedk TYPE cv100_rangesloedk,
          lt_cadkz TYPE TABLE OF cv100_rangescadkz,
          ls_cadkz TYPE cv100_rangescadkz,
          lv_cadkz TYPE draw-cadkz.

    DATA: lt_char_val TYPE TABLE OF bapi_characteristic_values,
          ls_doc_data TYPE bapi_doc_draw2,
          lv_dokar    TYPE bapi_doc_aux-doctype,
          lv_doknr    TYPE bapi_doc_aux-docnumber,
          lv_doktl    TYPE bapi_doc_aux-docpart,
          lv_dokvr    TYPE bapi_doc_aux-docversion.

* get classification data using input data
* find documents for specified input data
* get next version number

* only find next version number if identification is unique
    IF im_next IS NOT INITIAL.
      CHECK im_vbeln IS NOT INITIAL.
      CHECK im_vbelp IS NOT INITIAL.
      CHECK im_sernr IS NOT INITIAL.
      CHECK im_cert_matnr IS NOT INITIAL.
    ENDIF.

*   Find classification data for Certificates DCR
    me->read_class_setup( EXPORTING im_dokar = gc_dcr
                          IMPORTING ex_zksml_tab = lt_zksml ).

    CHECK lt_zksml[] IS NOT INITIAL.

    CLEAR: lt_ccomw[].

    LOOP AT lt_zksml INTO ls_zksml.
      CLEAR: ls_ccomw.
      MOVE-CORRESPONDING ls_zksml TO ls_ccomw.

      IF im_vbeln IS NOT INITIAL AND ls_zksml-atnam = 'SALES_ORDER'.
        WRITE im_vbeln TO ls_ccomw-atwrt NO-ZERO.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF im_vbelp IS NOT INITIAL AND ls_zksml-atnam = 'SALES_ORDER_ITEM'.
        WRITE im_vbelp TO ls_ccomw-atwrt NO-ZERO.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
*      ELSEIF im_vkorg IS NOT INITIAL AND ls_zksml-atnam = 'SALES_COMPANY'.
*        ls_ccomw-atwrt = im_vkorg.
*        ls_ccomw-atcod = 1.
*        ls_ccomw-slcod = 1.
*        ls_ccomw-statu = 'H'.
*        ls_ccomw-atfor = 'CHAR'.
*        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF im_sernr IS NOT INITIAL AND ls_zksml-atnam = 'DUT_ID_SERIAL'.
        WRITE im_sernr TO im_sernr NO-ZERO.
        CONDENSE im_sernr.
        ls_ccomw-atwrt = im_sernr.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF im_matnr IS NOT INITIAL AND ls_zksml-atnam = 'PART_NO'.
        WRITE im_matnr TO im_matnr NO-ZERO.
        CONDENSE im_matnr.
        ls_ccomw-atwrt = im_matnr.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF im_cert_matnr IS NOT INITIAL AND ls_zksml-atnam = 'CERT_TEMPLATE'.
        WRITE im_cert_matnr TO ls_ccomw-atwrt NO-ZERO.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ENDIF.

    ENDLOOP.
    CHECK lt_ccomw IS NOT INITIAL.
    ls_origin-all_originals = 'X'.

    ls_dokar-sign   = 'I'.
    ls_dokar-option = 'EQ'.
    ls_dokar-low    = gc_dcr.
    APPEND ls_dokar TO lt_dokar.

    ls_loedk-sign = 'I'.
    ls_loedk-option = 'EQ'.
    APPEND ls_loedk TO lt_loedk.
    ls_loedk-low = 'X'.
    APPEND ls_loedk TO lt_loedk.

    ls_cadkz-sign = 'I'.
    ls_cadkz-option = 'EQ'.
    APPEND ls_cadkz TO lt_cadkz.
    ls_cadkz-low = 'X'.
    APPEND ls_cadkz TO lt_cadkz.

* find documents for specified input data
    CALL FUNCTION 'CV100_DOCUMENT_SEARCH'
      EXPORTING
        max_rows             = 500   "restrict
*       sdttrg               = sdttrg
*       dappl                = sappli
*       sdatum1              = sdatum1
*       sdatum2              = sdatum2
        slang                = 'E'  "slanguage
        classno              = ls_zksml-class "classno
        classtype            = ls_zksml-klart "classtype
*       fulltext_searchstring = fulltext_searchstring
*       fulltext_or          = fulltext_or
*       fulltext_and         = fulltext_and
*       fulltext_searchtype  = fulltext_searchtype
        pf_cs_active         = 'X'  "pf_cs_active
*       pf_list_type         = pf_list_type
*       folder_key           = folder_key
*       latest               = latest
*       latestreleased       = latestreleased
*       filename             = filen "note 1756510 and 1770907
*       dir_without_appln    = dir_napp "note 1756510 and 1770907
        originals            = ls_origin  "ls_cv100_radio_buttons "note 1756510 and 1770907
*       user                 = sy-uname    "user "note 1756510 and 1770907
      IMPORTING
        more_documents       = lv_cadkz  "gf_more_documents
      TABLES
        tdraw                = lt_draw
*       FOUND_OBJECTS_FTS    =
*       TDOCS2LOIO           =
        stdokar              = lt_dokar
*       stdoknr              = stdoknr
*       stdokvr              = stdokvr
*       stdoktl              = stdoktl
        stloedk              = lt_loedk
        stcadkz              = lt_cadkz
*       stdwnam              = stdwnam
*       stbegru              = stbegru
*       staennr              = staennr
*       stlabor              = stlabor
*       stdoksa              = stdoksa
*       stdktxt              = stdktxt
*       RANGES_DTTRG         =
*       RANGES_DAPPL         =
*       RANGES_FILENAME      =
*       QUERY_PHIO_PROP      =
*       QUERY_LOIO_PROP      =
*       searchtext           = searchtext
*       objectlinks          = objectlinks
*       class_selection      = class_selection
        clsc_class_selection = lt_ccomw   "clsc_class_selection
      EXCEPTIONS
        no_result            = 1
        bad_query            = 2
        not_authorized       = 3
        OTHERS               = 4.

    IF sy-subrc = 0.
      DESCRIBE TABLE lt_draw LINES lv_lines.
      IF lv_lines GE 1.
        SORT lt_draw BY dokvr DESCENDING.   "find 'higest' version no and add 1
        READ TABLE lt_draw INTO ls_draw INDEX 1.
        lv_dokvr = ls_draw-dokvr.
        lv_dokvr = lv_dokvr + 1.
        WHILE lv_dokvr+1(1) = ' '.
          SHIFT lv_dokvr RIGHT BY 1 PLACES.
          IF lv_dokvr(1) = ' '.
            lv_dokvr(1) = '0'.
          ENDIF.
        ENDWHILE.
      ELSE.
        lv_dokvr = '00'.
      ENDIF.
    ELSE.
      lv_dokvr = '00'.
    ENDIF.

    ex_next_dokvr = lv_dokvr.
    ex_doknr = ls_draw-doknr.

  ENDMETHOD.


  METHOD get_pdf_as_binary.

    DATA lv_docdata TYPE bapi_doc_draw2.
    DATA lt_docfiles TYPE ty_lt_docfiles.
    DATA lv_bapireturn TYPE bapiret2.
    DATA lt_access TYPE ty_lt_access.
    DATA lt_sdokcntbin TYPE ty_lt_sdokcntbin.
    DATA lv_size TYPE int4.

    CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
      EXPORTING
        documenttype    = im_doc-documenttype
        documentnumber  = im_doc-documentnumber
        documentpart    = im_doc-documentpart
        documentversion = im_doc-documentversion
        getactivefiles  = abap_true
      IMPORTING
        documentdata    = lv_docdata
        return          = lv_bapireturn
      TABLES
        documentfiles   = lt_docfiles.

    IF lv_bapireturn-type CA 'EA'.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid             = zcx_grf_qm_cert=>gc_read_doc_failed
          gv_documenttype    = im_doc-documenttype
          gv_documentnumber  = im_doc-documentnumber
          gv_documentpart    = im_doc-documentpart
          gv_documentversion = im_doc-documentversion.
    ENDIF.

    READ TABLE lt_docfiles ASSIGNING FIELD-SYMBOL(<l_docfile>) INDEX 1.   "There is always only one attachment
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid             = zcx_grf_qm_cert=>gc_attachment_not_found
          gv_documenttype    = im_doc-documenttype
          gv_documentnumber  = im_doc-documentnumber
          gv_documentpart    = im_doc-documentpart
          gv_documentversion = im_doc-documentversion.
    ENDIF.


* New code for testing SEAL - starts here
*DATA: ls_files_api type CVAPI_DOC_FILE.
*data: lt_files_api type TABLE OF CVAPI_DOC_FILE.
*
*data: ls_doc_file type BAPI_DOC_FILES2.
*
*data: lt_drao type TABLE OF drao.
*
*data: ls_message type  MESSAGES.
*
*      ls_files_api-appnr           = <l_docfile>-originaltype.
*      ls_files_api-lo_objid         = <l_docfile>-application_id.
*      ls_files_api-ph_objid         = <l_docfile>-file_id.
*
*      REFRESH lt_files_api.
*      APPEND ls_files_api TO lt_files_api.
*
*      CALL FUNCTION 'CVAPI_DOC_CHECKOUTVIEW'
*        EXPORTING
*          pf_dokar                 = lv_docdata-documenttype
*          pf_doknr                 = lv_docdata-documentnumber
*          pf_dokvr                 = lv_docdata-documentversion
*          pf_doktl                 = lv_docdata-documentpart
*          pf_ftp_dest              = 'SAPFTPA'
*          pf_http_dest             = 'SAPHTTPA'
*          pf_content_provide       = 'TBL'
*          pf_std_url               = 'X'
*       IMPORTING
*          psx_message              = ls_message
*        TABLES
*          pt_files                 = lt_files_api
*          ptx_content              = lt_drao
*                .
*
*DATA: lv_drao type drao.
*
*data: lt_content type TABLE OF /SEAL/BAS_CR_OBJ_CONTENT.
*
*
*read table lt_drao index 1 into lv_drao.
*
*
*CALL FUNCTION '/SEAL/BAS_DM_CV_DRAO_2_CONT'
** IMPORTING
**   E_DOCSIZE       =
*  TABLES
*    t_drao          = lt_drao
*    t_content       = lt_content.
*
* lv_size = lv_drao-orln.
*
*    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*      EXPORTING
*        input_length = lv_size
*      IMPORTING
*        buffer       = ex_xstring
*      TABLES
*        binary_tab   = lt_content
*      EXCEPTIONS
*        failed       = 1
*        OTHERS       = 2.

* New code for testing SEAL - stops here

    CLEAR: lt_access[], lt_sdokcntbin[].

    CALL FUNCTION 'SCMS_DOC_READ'
      EXPORTING
        stor_cat              = <l_docfile>-storagecategory
        doc_id                = <l_docfile>-file_id
      TABLES
        access_info           = lt_access
        content_bin           = lt_sdokcntbin
      EXCEPTIONS
        bad_storage_type      = 1
        bad_request           = 2
        unauthorized          = 3
        comp_not_found        = 4
        not_found             = 5
        forbidden             = 6
        conflict              = 7
        internal_server_error = 8
        error_http            = 9
        error_signature       = 10
        error_config          = 11
        error_format          = 12
        error_parameter       = 13
        error                 = 14
        OTHERS                = 15.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_error_func_module
          gv_fm_name     = 'SCMS_DOC_READ'
          gv_return_code = CONV num4( sy-subrc ).

    ENDIF.

    READ TABLE lt_access ASSIGNING FIELD-SYMBOL(<l_access>) INDEX 1.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid = zcx_grf_qm_cert=>gc_file_size_not_found.
    ENDIF.

    lv_size = <l_access>-comp_size.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_size
      IMPORTING
        buffer       = ex_xstring
      TABLES
        binary_tab   = lt_sdokcntbin
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_grf_qm_cert
        EXPORTING
          textid         = zcx_grf_qm_cert=>gc_error_func_module
          gv_fm_name     = 'SCMS_BINARY_TO_XSTRING'
          gv_return_code = CONV num4( sy-subrc ).
    ENDIF.

    ex_fname = <l_docfile>-docfile.
  ENDMETHOD.


  METHOD get_released_version.

    DATA: lt_zksml  TYPE zttzksml,
          ls_zksml  TYPE zksml,
          lt_ccomw  TYPE TABLE OF comw,
          ls_ccomw  TYPE comw,
          ls_origin TYPE cv100_radio_buttons,
          lt_draw   TYPE TABLE OF draw,
          ls_draw   TYPE draw,
          lv_lines  TYPE bapi_doc_aux-docversion.

    DATA: lt_dokar TYPE TABLE OF cv100_rangesdokar,
          ls_dokar TYPE cv100_rangesdokar,
          lt_loedk TYPE TABLE OF cv100_rangesloedk,
          ls_loedk TYPE cv100_rangesloedk,
          lt_cadkz TYPE TABLE OF cv100_rangescadkz,
          ls_cadkz TYPE cv100_rangescadkz,
          lv_cadkz TYPE draw-cadkz.

    DATA: lt_char_val TYPE TABLE OF bapi_characteristic_values,
          ls_doc_data TYPE bapi_doc_draw2,
          lv_dokar    TYPE bapi_doc_aux-doctype,
          lv_doknr    TYPE bapi_doc_aux-docnumber,
          lv_doktl    TYPE bapi_doc_aux-docpart,
          lv_dokvr    TYPE bapi_doc_aux-docversion.

    DATA: lt_details TYPE TABLE OF zscert_version,
          ls_details TYPE zscert_version.


* get classification data using input data
    CLEAR lt_details[].

*   Find classification data for Certificates DCR
    me->read_class_setup( EXPORTING im_dokar = gc_dcr
                          IMPORTING ex_zksml_tab = lt_zksml ).

    CHECK lt_zksml[] IS NOT INITIAL.


    CLEAR: lt_ccomw[].

    LOOP AT lt_zksml INTO ls_zksml.
      CLEAR: ls_ccomw.
      MOVE-CORRESPONDING ls_zksml TO ls_ccomw.

      IF im_vbeln IS NOT INITIAL AND ls_zksml-atnam = 'SALES_ORDER'.
        WRITE im_vbeln TO ls_ccomw-atwrt NO-ZERO.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF im_vbelp IS NOT INITIAL AND ls_zksml-atnam = 'SALES_ORDER_ITEM'.
        WRITE im_vbelp TO ls_ccomw-atwrt NO-ZERO.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF im_sernr IS NOT INITIAL AND ls_zksml-atnam = 'DUT_ID_SERIAL'.
        WRITE im_sernr TO im_sernr NO-ZERO.
        CONDENSE im_sernr.
        ls_ccomw-atwrt = im_sernr.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF im_cert_matnr IS NOT INITIAL AND ls_zksml-atnam = 'CERT_TEMPLATE'.
        WRITE im_cert_matnr TO ls_ccomw-atwrt NO-ZERO.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF im_seqnr IS NOT INITIAL AND ls_zksml-atnam = 'PUMP_SEQU_NO'.
        ls_ccomw-atwrt = im_seqnr.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF im_prod_site IS NOT INITIAL AND ls_zksml-atnam = 'PRODUCTION_SITE'.
        ls_ccomw-atwrt = im_prod_site.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ENDIF.
    ENDLOOP.

    CHECK lt_ccomw IS NOT INITIAL.
    ls_origin-all_originals = 'X'.

    ls_dokar-sign   = 'I'.
    ls_dokar-option = 'EQ'.
    ls_dokar-low    = gc_dcr.
    APPEND ls_dokar TO lt_dokar.

    ls_loedk-sign = 'I'.
    ls_loedk-option = 'EQ'.
    APPEND ls_loedk TO lt_loedk.
    ls_loedk-low = 'X'.
    APPEND ls_loedk TO lt_loedk.

    ls_cadkz-sign = 'I'.
    ls_cadkz-option = 'EQ'.
    APPEND ls_cadkz TO lt_cadkz.
    ls_cadkz-low = 'X'.
    APPEND ls_cadkz TO lt_cadkz.

* find documents for specified input data
    CALL FUNCTION 'CV100_DOCUMENT_SEARCH'
      EXPORTING
        max_rows             = 500   "restrict
*       sdttrg               = sdttrg
*       dappl                = sappli
*       sdatum1              = sdatum1
*       sdatum2              = sdatum2
        slang                = 'E'  "slanguage
        classno              = ls_zksml-class "classno
        classtype            = ls_zksml-klart "classtype
*       fulltext_searchstring = fulltext_searchstring
*       fulltext_or          = fulltext_or
*       fulltext_and         = fulltext_and
*       fulltext_searchtype  = fulltext_searchtype
        pf_cs_active         = 'X'  "pf_cs_active
*       pf_list_type         = pf_list_type
*       folder_key           = folder_key
*       latest               = latest
*       latestreleased       = latestreleased
*       filename             = filen "note 1756510 and 1770907
*       dir_without_appln    = dir_napp "note 1756510 and 1770907
        originals            = ls_origin  "ls_cv100_radio_buttons "note 1756510 and 1770907
*       user                 = sy-uname    "user "note 1756510 and 1770907
      IMPORTING
        more_documents       = lv_cadkz  "gf_more_documents
      TABLES
        tdraw                = lt_draw
*       FOUND_OBJECTS_FTS    =
*       TDOCS2LOIO           =
        stdokar              = lt_dokar
*       stdoknr              = stdoknr
*       stdokvr              = stdokvr
*       stdoktl              = stdoktl
        stloedk              = lt_loedk
        stcadkz              = lt_cadkz
*       stdwnam              = stdwnam
*       stbegru              = stbegru
*       staennr              = staennr
*       stlabor              = stlabor
*       stdoksa              = stdoksa
*       stdktxt              = stdktxt
*       RANGES_DTTRG         =
*       RANGES_DAPPL         =
*       RANGES_FILENAME      =
*       QUERY_PHIO_PROP      =
*       QUERY_LOIO_PROP      =
*       searchtext           = searchtext
*       objectlinks          = objectlinks
*       class_selection      = class_selection
        clsc_class_selection = lt_ccomw   "clsc_class_selection
      EXCEPTIONS
        no_result            = 1
        bad_query            = 2
        not_authorized       = 3
        OTHERS               = 4.

* find classification data for documents
    IF sy-subrc = 0.
      LOOP AT lt_draw INTO ls_draw.
* only return released documents
        IF ls_draw-dokst NE 'RE'.
          CONTINUE.
        ENDIF.

        ls_details-dokar = ls_draw-dokar.
        ls_details-doknr = ls_draw-doknr.
        ls_details-dokvr = ls_draw-dokvr.
        ls_details-vbeln = im_vbeln.
        ls_details-vbelp = im_vbelp.
        ls_details-status = ls_draw-dokst.

        lv_dokar = ls_draw-dokar.
        lv_doknr = ls_draw-doknr.
        lv_doktl = ls_draw-doktl.
        lv_dokvr = ls_draw-dokvr.

        CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
          EXPORTING
            documenttype         = lv_dokar
            documentnumber       = lv_doknr
            documentpart         = lv_doktl
            documentversion      = lv_dokvr
*           GETOBJECTLINKS       = ' '
*           GETCOMPONENTS        = ' '
*           GETSTATUSLOG         = ' '
*           GETLONGTEXTS         = ' '
            getactivefiles       = ' '
            getdocdescriptions   = ' '
            getdocfiles          = ' '
            getclassification    = 'X'
*           GETSTRUCTURE         = ' '
*           GETWHEREUSED         = ' '
*           HOSTNAME             = ' '
            inherited            = ' '
*           PF_BAPI_CALL         =
          IMPORTING
            documentdata         = ls_doc_data
*           RETURN               =
          TABLES
*           OBJECTLINKS          =
*           DOCUMENTDESCRIPTIONS =
*           LONGTEXTS            =
*           STATUSLOG            =
*           DOCUMENTFILES        =
*           COMPONENTS           =
            characteristicvalues = lt_char_val
*           CLASSALLOCATIONS     =
*           DOCUMENTSTRUCTURE    =
*           WHEREUSEDLIST        =
          .

* Check return codes from table RETURN...

* fill details for export
        READ TABLE lt_char_val ASSIGNING FIELD-SYMBOL(<l_val1>) WITH KEY charname = 'DUT_ID_SERIAL'.
        IF sy-subrc = 0.
          ls_details-sernr = <l_val1>-charvalue.
        ENDIF.

        READ TABLE lt_char_val ASSIGNING FIELD-SYMBOL(<l_val2>) WITH KEY charname = 'CERT_TEMPLATE'.
        IF sy-subrc = 0.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input  = <l_val2>-charvalue(18)
            IMPORTING
              output = ls_details-cert_matnr.
        ENDIF.

        READ TABLE lt_char_val ASSIGNING FIELD-SYMBOL(<l_val3>) WITH KEY charname = 'LANGUAGE_CODE'.
        IF sy-subrc = 0.
          ls_details-laiso = <l_val3>-charvalue.
        ENDIF.

        READ TABLE lt_char_val ASSIGNING FIELD-SYMBOL(<l_val4>) WITH KEY charname = 'PUMP_SEQU_NO'.
        IF sy-subrc = 0.
          ls_details-seqnr = <l_val4>-charvalue.
        ENDIF.

        APPEND ls_details TO lt_details.
      ENDLOOP.

    ENDIF.

    ex_released_docs[] = lt_details[].

  ENDMETHOD.


  METHOD get_sernr_default_value.
    DATA: lv_matnr TYPE matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = im_matnr
      IMPORTING
        output = lv_matnr.

    re_sernr = lv_matnr && gv_plant_id.
  ENDMETHOD.


  METHOD get_service_order_data.

    DATA: lv_adrnr TYPE adrnr,
          lv_parnr TYPE i_parnr,
          lv_addr  TYPE addr1_val.

    DATA(lv_objnr) = 'OR' && im_aufnr.

    SELECT SINGLE adrnr parnr INTO (lv_adrnr, lv_parnr)
    FROM ihpa
    WHERE objnr EQ lv_objnr
      AND parvw EQ 'AG'.

    IF sy-subrc NE 0.
      MESSAGE e014(zgrf_qm_cert) WITH 'IHPA' im_aufnr 'AG'.  "Entry not found in table &1 for &2 &3
      LEAVE PROGRAM.
    ENDIF.

    IF lv_adrnr IS INITIAL.
      SELECT SINGLE adrnr
        INTO lv_adrnr
        FROM kna1
        WHERE kunnr EQ lv_parnr.

      IF sy-subrc NE 0.
        MESSAGE e014(zgrf_qm_cert) WITH 'KNA1' lv_parnr ''.  "Entry not found in table &1 for &2 &3
        LEAVE PROGRAM.
      ENDIF.
    ENDIF.

    lv_addr = read_address( lv_adrnr ).

    ex_cert_head-sold_to_no = lv_parnr.
    ex_cert_head-name1 = lv_addr-name1.
    ex_cert_head-name2 = lv_addr-name2.

  ENDMETHOD.


  METHOD get_so_item.

    DATA: lt_so_item TYPE zttcert_so_item,
          lv_so_item TYPE zscert_so_item.

    CLEAR ex_so_item[].

*   Read order items
    SELECT vbeln, posnr, matnr, kwmeng
      FROM vbap
      INTO TABLE @DATA(lt_items)
      WHERE vbeln = @im_vbeln
        AND posnr IN @im_posnr
        AND kwmeng > 0.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CLEAR lt_so_item[].

    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<l_item>).

*     Read all materials from BOM
      me->mat_bom_breakdown(
        EXPORTING
          im_matnr = <l_item>-matnr
          im_werks = im_werks ).

      IF gt_bom_matnr[] IS INITIAL.
        CONTINUE.
      ENDIF.

*   Find certificate material at the BOM
      LOOP AT gt_cert_matnr ASSIGNING FIELD-SYMBOL(<l_cert_matnr>).
        READ TABLE gt_bom_matnr WITH KEY table_line = <l_cert_matnr> BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lv_so_item-vbeln = <l_item>-vbeln.
          lv_so_item-posnr = <l_item>-posnr.
          lv_so_item-matnr = <l_item>-matnr.
          lv_so_item-cert_matnr = <l_cert_matnr>.

*         Read pump material text
          SELECT SINGLE maktx
            INTO lv_so_item-maktx
            FROM makt
            WHERE matnr = <l_item>-matnr
              AND spras = sy-langu.

          IF sy-subrc NE 0.
            "This will never happen
            CLEAR lv_so_item-maktx.
          ENDIF.

*         Read certificate material text
          SELECT SINGLE maktx
            INTO lv_so_item-cert_maktx
            FROM makt
            WHERE matnr = lv_so_item-cert_matnr
              AND spras = sy-langu.

          IF sy-subrc NE 0.
            "This will never happen
            CLEAR lv_so_item-cert_maktx.
          ENDIF.

*         Read fields for extra documents that needs to be added to the DCR document
          SELECT incl_cer incl_scr incl_tcr ext_tcrno
            INTO ( lv_so_item-incl_cer,
                   lv_so_item-incl_scr,
                   lv_so_item-incl_tcr,
                   lv_so_item-ext_tcrno )
            FROM zqcert_forms UP TO 1 ROWS
            WHERE matnr = lv_so_item-cert_matnr.
          ENDSELECT.

          IF sy-subrc NE 0.
            MESSAGE e014(zgrf_qm_cert) WITH 'ZQCERT_FORMS' lv_so_item-cert_matnr space.  "Entry not found in table &1 for &2 &3
            LEAVE PROGRAM.
          ENDIF.

          IF lv_so_item-incl_tcr = 'X' AND lv_so_item-ext_tcrno IS INITIAL.
            MESSAGE e016(zgrf_qm_cert).   "Certificate cannot be created due to missing test certificate type
            LEAVE PROGRAM.
          ENDIF.

          DO <l_item>-kwmeng TIMES.
            lv_so_item-seqno = sy-index.
            APPEND lv_so_item TO lt_so_item.
          ENDDO.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    ex_so_item[] = lt_so_item[].

  ENDMETHOD.


  METHOD get_so_item_note.

    DATA: lv_name  TYPE tdobname,
          lt_lines TYPE TABLE OF tline,
          lv_spras TYPE spras.

    CLEAR ex_text_tab[].

    CONCATENATE im_so_item-vbeln im_so_item-posnr INTO lv_name.

*   Find the language of the std. text
    SELECT tdspras
      INTO lv_spras
      FROM stxh UP TO 1 ROWS
      WHERE tdobject = 'VBBP'
        AND tdname =  lv_name
        AND tdid   = '0002'.
    ENDSELECT.

    IF sy-subrc <> 0.
      "No item note found
      EXIT.
    ENDIF.

*   Read std. text 'item note'
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = '0002'      " Text ID of text to be read
        language                = lv_spras    " Language of text to be read
        name                    = lv_name     " Name of text to be read
        object                  = 'VBBP'      " Object of text to be read
      TABLES
        lines                   = lt_lines    " Lines of text read
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
***      Raise exception...

*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<l_line>).
        APPEND <l_line>-tdline TO ex_text_tab.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_supp_cert.

    DATA: lt_zksml  TYPE zttzksml,
          ls_zksml  TYPE zksml,
          lt_ccomw  TYPE TABLE OF comw,
          ls_ccomw  TYPE comw,
          ls_origin TYPE cv100_radio_buttons,
          lt_draw   TYPE TABLE OF draw,
          ls_draw   TYPE draw,
          lv_cadkz  TYPE draw-cadkz.

    DATA: lt_dokar      TYPE TABLE OF cv100_rangesdokar,
          ls_dokar      TYPE cv100_rangesdokar,
          lv_doc        TYPE bapi_doc_files2,
          lv_docdata    TYPE bapi_doc_draw2,
          lv_bapireturn TYPE bapiret2.

*   Find classification data for supplier certificates (SCR)
    me->read_class_setup( EXPORTING im_dokar = gc_scr
                          IMPORTING ex_zksml_tab = lt_zksml ).

    CLEAR lt_ccomw[].

    LOOP AT lt_zksml INTO ls_zksml.
      IF ls_zksml-atnam = 'CERT_TEMPLATE'.
        CLEAR ls_ccomw.
        MOVE-CORRESPONDING ls_zksml TO ls_ccomw.
        WRITE im_cert_matnr TO ls_ccomw-atwrt NO-ZERO.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF ls_zksml-atnam = 'DUT_ID_SERIAL'.
        CLEAR ls_ccomw.
        MOVE-CORRESPONDING ls_zksml TO ls_ccomw.
        WRITE im_sernr TO ls_ccomw-atwrt NO-ZERO.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF ls_zksml-atnam = 'PRODUCTION_SITE'.
        CLEAR ls_ccomw.
        MOVE-CORRESPONDING ls_zksml TO ls_ccomw.
        ls_ccomw-atwrt = im_werks.
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
    ls_dokar-low    = gc_scr.
    APPEND ls_dokar TO lt_dokar.

*   Find documents for specified classification fields
    CALL FUNCTION 'CV100_DOCUMENT_SEARCH'
      EXPORTING
        max_rows             = 100
        classno              = ls_zksml-class
        classtype            = ls_zksml-klart
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

    IF sy-subrc = 0.

*     Check if there is a released document
      READ TABLE lt_draw INTO ls_draw WITH KEY dokst = 'RE'.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
          EXPORTING
            documenttype    = ls_draw-dokar
            documentnumber  = ls_draw-doknr
            documentpart    = ls_draw-doktl
            documentversion = ls_draw-dokvr
            getactivefiles  = abap_false
          IMPORTING
            documentdata    = lv_docdata
            return          = lv_bapireturn.

        IF lv_bapireturn-type CA 'EA'.
          "Should never happen
          MESSAGE e997(zm) WITH 'Document not found!' ls_draw-dokar ls_draw-doknr.
          LEAVE PROGRAM.
        ENDIF.

        lv_doc-documenttype    = ls_draw-dokar.
        lv_doc-documentnumber  = ls_draw-doknr.
        lv_doc-documentpart    = ls_draw-doktl.
        lv_doc-documentversion = ls_draw-dokvr.
        lv_doc-docfile         = ls_draw-filep.
        lv_doc-description     = 'Supplier Certificate for' && | | && im_cert_matnr.
        APPEND lv_doc TO ex_doc_tab.
      ENDIF.
    ENDIF.

    IF lines( ex_doc_tab ) = 0.
      CLEAR lv_doc.
      lv_doc-description = 'Supplier Certificate for' && | | && im_cert_matnr.
      APPEND lv_doc TO ex_doc_tab.
    ENDIF.

  ENDMETHOD.


  METHOD get_test_report.
    DATA: lt_zksml  TYPE zttzksml,
          ls_zksml  TYPE zksml,
          lt_ccomw  TYPE TABLE OF comw,
          ls_ccomw  TYPE comw,
          ls_origin TYPE cv100_radio_buttons,
          lt_draw   TYPE TABLE OF draw,
          ls_draw   TYPE draw,
          lv_cadkz  TYPE draw-cadkz.

    DATA: lt_dokar      TYPE TABLE OF cv100_rangesdokar,
          ls_dokar      TYPE cv100_rangesdokar,
          lv_doc        TYPE bapi_doc_files2,
          lv_docdata    TYPE bapi_doc_draw2,
          lv_bapireturn TYPE bapiret2.

*   Find classification data for supplier certificates (SCR)
    me->read_class_setup( EXPORTING im_dokar = gc_tcr
                          IMPORTING ex_zksml_tab = lt_zksml ).

    CLEAR lt_ccomw[].

    LOOP AT lt_zksml INTO ls_zksml.
      IF ls_zksml-atnam = 'DUT_ID_SERIAL'.
        CLEAR ls_ccomw.
        MOVE-CORRESPONDING ls_zksml TO ls_ccomw.
        WRITE im_sernr TO ls_ccomw-atwrt NO-ZERO.
        CONDENSE ls_ccomw-atwrt.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF ls_zksml-atnam = 'PRODUCTION_SITE'.
        CLEAR ls_ccomw.
        MOVE-CORRESPONDING ls_zksml TO ls_ccomw.
        ls_ccomw-atwrt = im_werks.
        ls_ccomw-atcod = 1.
        ls_ccomw-slcod = 1.
        ls_ccomw-statu = 'H'.
        ls_ccomw-atfor = 'CHAR'.
        APPEND ls_ccomw TO lt_ccomw.
      ELSEIF ls_zksml-atnam = 'PDF_TYPE'.
        CLEAR ls_ccomw.
        MOVE-CORRESPONDING ls_zksml TO ls_ccomw.
        ls_ccomw-atwrt = im_ext_tcrno.
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
    ls_dokar-low    = gc_tcr.
    APPEND ls_dokar TO lt_dokar.

*   Find documents for specified classification fields
    CALL FUNCTION 'CV100_DOCUMENT_SEARCH'
      EXPORTING
        max_rows             = 100
        classno              = ls_zksml-class
        classtype            = ls_zksml-klart
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

    IF sy-subrc = 0.

*     Check if there is a released document
      READ TABLE lt_draw INTO ls_draw WITH KEY dokst = 'RE'.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
          EXPORTING
            documenttype    = ls_draw-dokar
            documentnumber  = ls_draw-doknr
            documentpart    = ls_draw-doktl
            documentversion = ls_draw-dokvr
            getactivefiles  = abap_false
          IMPORTING
            documentdata    = lv_docdata
            return          = lv_bapireturn.

        IF lv_bapireturn-type CA 'EA'.
          "Should never happen
          MESSAGE e997(zm) WITH 'Document not found!' ls_draw-dokar ls_draw-doknr.
          LEAVE PROGRAM.
        ENDIF.

        lv_doc-documenttype    = ls_draw-dokar.
        lv_doc-documentnumber  = ls_draw-doknr.
        lv_doc-documentpart    = ls_draw-doktl.
        lv_doc-documentversion = ls_draw-dokvr.
        lv_doc-docfile         = ls_draw-filep.
        lv_doc-description     = 'Test report'.       "&& | | && im_cert_matnr.
        APPEND lv_doc TO ex_doc_tab.
      ENDIF.
    ENDIF.

    IF lines( ex_doc_tab ) = 0.
      CLEAR lv_doc.
      lv_doc-description = 'Test report'.   " && | | && im_cert_matnr.
      APPEND lv_doc TO ex_doc_tab.
    ENDIF.

  ENDMETHOD.


  METHOD get_vendor_cert.

    TYPES: BEGIN OF ty_heat_data,
             matnr       TYPE zqcert_comp_data-matnr,
             vendor_code TYPE zqcert_comp_data-vendor_code,
             heat_data   TYPE zqcert_comp_data-heat_data,
             comp_no     TYPE zqcert_comp_data-comp_no,
           END OF ty_heat_data.

    DATA: lt_heat_data TYPE TABLE OF ty_heat_data.

    DATA: lv_doknr      TYPE doknr,
          ls_draw       TYPE draw,
          lv_doc        TYPE bapi_doc_files2,
          lv_docdata    TYPE bapi_doc_draw2,
          lv_bapireturn TYPE bapiret2.

    DATA: lv_comp_txt   TYPE zqcert_comp-comp_txt.

    SELECT matnr vendor_code heat_data comp_no
        FROM zqcert_comp_data
        INTO TABLE lt_heat_data
        WHERE werks = im_werks
          AND sernr = im_sernr
        ORDER BY comp_no.

    IF sy-subrc NE 0.   "No data found for serial number
*      ex_not_found = abap_true.
*      error message - this should never happen...
      RETURN.
    ENDIF.

    LOOP AT lt_heat_data ASSIGNING FIELD-SYMBOL(<l_heat>).
      lv_doknr = <l_heat>-matnr && '_' && <l_heat>-vendor_code && '_' && <l_heat>-heat_data && '%'.

      me->get_external_doc(
        EXPORTING
          im_dokar = gc_cer
          im_doknr = lv_doknr
        IMPORTING
          ex_draw  = ls_draw
      ).

      SELECT SINGLE comp_txt
        INTO lv_comp_txt
        FROM zqcert_comp
        WHERE comp_no = <l_heat>-comp_no
          AND spras = 'E'.

      IF sy-subrc NE 0.
        CLEAR lv_comp_txt.
      ENDIF.

      CLEAR lv_doc.
      CONCATENATE lv_comp_txt <l_heat>-matnr <l_heat>-vendor_code <l_heat>-heat_data INTO lv_doc-description SEPARATED BY space.

      IF ls_draw IS INITIAL.
        APPEND lv_doc TO ex_doc_tab.
      ELSE.
        CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
          EXPORTING
            documenttype    = ls_draw-dokar
            documentnumber  = ls_draw-doknr
            documentpart    = ls_draw-doktl
            documentversion = ls_draw-dokvr
            getactivefiles  = abap_false
          IMPORTING
            documentdata    = lv_docdata
            return          = lv_bapireturn.

        IF lv_bapireturn-type CA 'EA'.
          "Should never happen
          MESSAGE e997(zm) WITH 'Document not found!' ls_draw-dokar ls_draw-doknr.
          LEAVE PROGRAM.
        ENDIF.

        lv_doc-documenttype    = ls_draw-dokar.
        lv_doc-documentnumber  = ls_draw-doknr.
        lv_doc-documentpart    = ls_draw-doktl.
        lv_doc-documentversion = ls_draw-dokvr.
        lv_doc-docfile         = ls_draw-filep.
        lv_doc-description     = lv_doc-description.
        APPEND lv_doc TO ex_doc_tab.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD lock_sales_order.

    CALL FUNCTION 'ENQUEUE_EZVVBAKE_CERT'
      EXPORTING
        mandt          = sy-mandt
        vbeln          = im_vbeln
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD mat_bom_breakdown.

    DATA: lt_stb TYPE TABLE OF stpox.

    CLEAR gt_bom_matnr[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        capid                 = 'GUNV'       " Application ID
        datuv                 = sy-datum     " Valid On
        emeng                 = 1            " Required quantity
        mehrs                 = 'X'          " Multi-level explosion
        mtnrv                 = im_matnr     " Materialnumber
        stlan                 = '8'          " BOM usage
        werks                 = im_werks     " Plant
      TABLES
        stb                   = lt_stb       " Collective item data table
      EXCEPTIONS
        alt_not_found         = 1
        call_invalid          = 2
        material_not_found    = 3
        missing_authorization = 4
        no_bom_found          = 5
        no_plant_data         = 6
        no_suitable_bom_found = 7
        conversion_error      = 8
        OTHERS                = 9.

    IF sy-subrc <> 0.
      RETURN.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      LOOP AT lt_stb ASSIGNING FIELD-SYMBOL(<l_stb>).
        APPEND <l_stb>-idnrk TO gt_bom_matnr.
      ENDLOOP.

      SORT gt_bom_matnr.
      DELETE ADJACENT DUPLICATES FROM gt_bom_matnr.
    ENDIF.

  ENDMETHOD.


  METHOD read_address.

    DATA:  "lv_nation   TYPE adrc-nation,
      lv_addr_sel TYPE addr1_sel,
      lv_addr     TYPE addr1_val,
      lv_return   TYPE ad_retcode.

*   Check if an international address exist
    SELECT COUNT( * )
      FROM adrc
      WHERE addrnumber = im_adrnr
        AND nation = 'I'.

    IF sy-subrc = 0 AND sy-dbcnt = 1.
      lv_addr_sel-nation = 'I'.
    ENDIF.

*   Read address for customer to find the right name
    lv_addr_sel-addrnumber = im_adrnr.

    CALL FUNCTION 'ADDR_GET'
      EXPORTING
        address_selection = lv_addr_sel
      IMPORTING
        address_value     = re_addr     " Return data for an address
        returncode        = lv_return   " Return code: ' '(ok), 'I'nfo, 'W'arning, 'E'rror
      EXCEPTIONS
        parameter_error   = 1
        address_not_exist = 2
        version_not_exist = 3
        internal_error    = 4
        address_blocked   = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      MESSAGE e015(zgrf_qm_cert) WITH sy-subrc.        "Read of customer address failed with return code &1
      LEAVE PROGRAM.
    ENDIF.

    IF lv_return = 'E'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      LEAVE PROGRAM.
    ENDIF.

  ENDMETHOD.


  METHOD read_class_setup.

    DATA: ls_tdwa   TYPE tdwa,
          lt_cltext TYPE TABLE OF cltext,
          ls_cltext TYPE cltext,
          lt_zksml  TYPE TABLE OF zksml.

    CLEAR ex_zksml_tab[].

*   Find classification data for class
    SELECT * INTO ls_tdwa FROM tdwa UP TO 1 ROWS
      WHERE dokar = im_dokar.
    ENDSELECT.

    CHECK sy-subrc = 0.

    CLEAR: lt_cltext[].
    ls_cltext-class = ls_tdwa-klasse.
    ls_cltext-klart = ls_tdwa-klassenart.
    APPEND ls_cltext TO lt_cltext.

    CALL FUNCTION 'CLMA_CLASS_TEXT'
      EXPORTING
        classtype         = ls_tdwa-klassenart
        language          = sy-langu
        mode              = 'E'
      TABLES
        tklas             = lt_cltext
      EXCEPTIONS
        classtype_missing = 1.

    CHECK sy-subrc = 0.

*   Class data for certificates
    READ TABLE lt_cltext INTO ls_cltext INDEX 1.
    CHECK sy-subrc = 0.

    SELECT * INTO TABLE ex_zksml_tab
      FROM zksml
      WHERE clint = ls_cltext-clint
      AND   klart = ls_cltext-klart
      AND   class = ls_cltext-class.

    IF sy-subrc NE  0.
      CHECK 1 = 1.
    ENDIF.

  ENDMETHOD.


  METHOD send_email.

    DATA: lt_recipient    TYPE TABLE OF zemail,
          lv_email        TYPE ad_smtpadr, " Email ID
*          lo_send_request TYPE REF TO cl_bcs VALUE IS INITIAL,
          lt_message_body TYPE bcsy_text VALUE IS INITIAL,
          lo_document     TYPE REF TO cl_document_bcs VALUE IS INITIAL,
          lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL.

    DATA: lt_solix  TYPE solix_tab,   "Attachment data
          lv_size   TYPE int4,        "Size of Attachment
          lv_result TYPE abap_bool.

    DATA: lo_document_bcs TYPE REF TO cx_document_bcs VALUE IS INITIAL.

    DATA: lr_bcs_exception     TYPE REF TO cx_bcs,
          lr_send_req_bcs_excp TYPE REF TO cx_send_req_bcs,
          lr_address_bcs       TYPE REF TO cx_address_bcs.

    DATA: lr_send_request       TYPE REF TO cl_bcs.

    DATA: lt_recipients TYPE bcsy_re3,
          ls_recipient  TYPE bcss_re3.

    DATA: lv_subject    TYPE so_obj_des.
    DATA: lt_attachments TYPE bcsy_doc.
    DATA: lv_fname TYPE filep.
    DATA: lv_xstring TYPE xstring.
    DATA: lt_doc_tab TYPE tt_bapi_doc_files2.
    DATA: lv_length TYPE so_obj_len.
    DATA: ls_doc TYPE bapi_doc_files2.


* Get the email address to send mail to
    me->get_email_recipients(
      EXPORTING
        im_vbeln      = im_cert_header-sd_doc
      IMPORTING
        ex_recipients = lt_recipient ).

*Add Recipients (E-MAIL ADDRESS)
    LOOP AT lt_recipient ASSIGNING FIELD-SYMBOL(<l_recipient>).
      lv_email = <l_recipient>.

      CLEAR ls_recipient.
      TRY.
          ls_recipient-recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).

        CATCH cx_address_bcs INTO lr_address_bcs.
          RAISE EXCEPTION TYPE zcx_grf_qm_cert
            EXPORTING
              textid     = zcx_grf_qm_cert=>gc_email_error
              gv_errtext = lr_address_bcs->get_text( ).
      ENDTRY.

      ls_recipient-sndex = 'X'.    "Send express
*     ls_recipient-SNDCP "Send as CC
*     ls_recipient-SNDBC "Send as BCC
      APPEND ls_recipient TO lt_recipients.
    ENDLOOP.


* Create mail content
    APPEND 'Dear' && | | && im_cert_header-sales_org TO lt_message_body.
    APPEND ' ' TO lt_message_body.
    APPEND 'Attached you will find the pump certificates for order' && | | && im_cert_header-sd_doc && '.' TO lt_message_body.
    APPEND ' ' TO lt_message_body.
    APPEND 'Best regards,' TO lt_message_body.
    APPEND im_cert_header-usr_name TO lt_message_body.

    TRY.
*       Add PDF files as attachments to the document
        LOOP AT im_item_tab ASSIGNING FIELD-SYMBOL(<l_item>).

          lt_doc_tab[] = <l_item>-doc_tab[].
          SORT lt_doc_tab BY docfile.
          DELETE ADJACENT DUPLICATES FROM lt_doc_tab COMPARING docfile.

*         Add DCR document to table
          CLEAR ls_doc.
          ls_doc-documenttype = 'DCR'.
          ls_doc-documentnumber = <l_item>-doknr.
          ls_doc-documentpart = '000'.
          ls_doc-documentversion = <l_item>-dokvr.
          APPEND ls_doc TO lt_doc_tab.


          LOOP AT lt_doc_tab ASSIGNING FIELD-SYMBOL(<l_doc>).

            me->get_pdf_as_binary(
              EXPORTING
                im_doc          = <l_doc>
              IMPORTING
                ex_xstring      = lv_xstring
                ex_fname        = lv_fname ).

*           Remove Path from lv_fname.
            CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
              EXPORTING
                full_name     = lv_fname   " File name with path
              IMPORTING
                stripped_name = lv_fname
              EXCEPTIONS
                x_error       = 1
                OTHERS        = 2.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_grf_qm_cert
                EXPORTING
                  textid         = zcx_grf_qm_cert=>gc_error_func_module
                  gv_return_code = CONV num4( sy-subrc ).
            ENDIF.

*           Convert PDF and add as attachment
            CALL METHOD cl_bcs_convert=>xstring_to_solix
              EXPORTING
                iv_xstring = lv_xstring
              RECEIVING
                et_solix   = lt_solix.

            lv_length = lines( lt_solix ) * 255.

            lo_document = cl_document_bcs=>create_document(
                                        i_type    = 'PDF'
                                        i_hex     = lt_solix
                                        i_length  = lv_length
                                        i_subject = CONV so_obj_des( lv_fname ) ).

            APPEND lo_document TO lt_attachments.
          ENDLOOP.
        ENDLOOP.

** Pass the document to send request
*        lo_send_request->set_document( lo_document ).
*
*
**   Add E-mail address as blind copy
*    lv_parkey = sy-uname.
*
*    CALL METHOD zcl_param=>read
*      EXPORTING
*        area    = 'QM'
*        subarea = 'CERTIFICATE'
*        parname = 'EMAIL_ADR_FOR_BCC'
*        parkey  = lv_parkey
*      IMPORTING
*        found   = lv_found
*        value   = lv_value.
*
*    IF lv_found = 'X'.
*      lv_email = lv_value.
*      lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).
*
*      lo_send_request->add_recipient(
*       EXPORTING
*         i_recipient = lo_recipient
*         i_express = 'X'
*         i_blind_copy = 'X' ).
*    ENDIF.
*
*
** Trigger E-Mail immediately
*    lo_send_request->set_send_immediately( 'X' ).    "This will bypass SOST!!!???
*
** Send email
*    lo_send_request->send(
*      EXPORTING
*        i_with_error_screen = 'X'
*      RECEIVING
*        result = lv_result ).
*

        lv_subject = 'Certificates for order No.: ' && | | && im_cert_header-sd_doc && | | && im_cert_header-sales_org .

        cl_bcs=>short_message(
                  EXPORTING
                    i_subject       = lv_subject         " Document Title
                    i_text          = lt_message_body    " Default Text
                    i_recipients    = lt_recipients      " BCS: Table of Recipients with Send Attributes
                    i_attachments   =  lt_attachments   " Attachments
                    i_starting_at_x = 5     " Call As Dialog Box: Start X Coordinate
                    i_starting_at_y = 5    " Call As Dialog Box: Start Y Coordinate
                  RECEIVING
                    result          =   lr_send_request ).    " Send Request

      CATCH cx_send_req_bcs INTO lr_send_req_bcs_excp.
        IF lr_send_req_bcs_excp->error_type = cx_send_req_bcs=>cancelled.   "User has decided to cancel mail
          RETURN.
        ENDIF.

        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid     = zcx_grf_qm_cert=>gc_email_error
            gv_errtext = lr_send_req_bcs_excp->get_text( ).

      CATCH cx_bcs INTO lr_bcs_exception.
        RAISE EXCEPTION TYPE zcx_grf_qm_cert
          EXPORTING
            textid     = zcx_grf_qm_cert=>gc_email_error
            gv_errtext = lr_send_req_bcs_excp->get_text( ).

    ENDTRY.

  ENDMETHOD.


  METHOD unlock_sales_order.

    CALL FUNCTION 'DEQUEUE_EZVVBAKE_CERT'
      EXPORTING
        mandt = sy-mandt
        vbeln = im_vbeln.

  ENDMETHOD.


  METHOD update_item_w_doc_info.

    DATA: lv_so_item TYPE zscert_so_item,
          lt_doc_ver TYPE zttcert_version,
          lt_doc     TYPE zttcert_version.

*   select certificates with sales order and pos. and find serial numbers and document data
    CLEAR lt_doc[].
    SORT ch_so_item BY vbeln posnr cert_matnr seqno.

    LOOP AT ch_so_item INTO lv_so_item.
      AT NEW posnr.

        me->get_released_version(
          EXPORTING
            im_vbeln      = lv_so_item-vbeln       " Sales and Distribution Document Number
            im_vbelp      = lv_so_item-posnr       " Sales document item
          IMPORTING
            ex_released_docs = lt_doc_ver
        ).

        APPEND LINES OF lt_doc_ver TO lt_doc.
      ENDAT.
    ENDLOOP.

    LOOP AT lt_doc ASSIGNING FIELD-SYMBOL(<l_doc>).
      READ TABLE ch_so_item ASSIGNING FIELD-SYMBOL(<l_so_item>) WITH KEY vbeln = <l_doc>-vbeln
                                                                         posnr = <l_doc>-vbelp
                                                                         cert_matnr = <l_doc>-cert_matnr
                                                                         seqno = <l_doc>-seqnr.
      IF sy-subrc = 0.
        <l_so_item>-sernr = <l_doc>-sernr.
        <l_so_item>-created = 'X'.
        <l_so_item>-langu = <l_doc>-laiso.
        <l_so_item>-doknr = <l_doc>-doknr.
        <l_so_item>-dokvr = <l_doc>-dokvr.
      ENDIF.

    ENDLOOP.

*   Attach extra documents
*    loop at ch_so_item ASSIGNING FIELD-SYMBOL(<l_item>).
*
*      if <l_item>-created = 'X'.  "DCR document already exist.
*        <l_item>-status = 'X'.
*      else.
*        if <l_item>-incl_cer = 'X'.
*
*          me->get_vendor_cert(
*            exporting
*              im_werks = me->gv_werks
*              im_sernr = <l_item>-sernr
*            IMPORTING
*              ex_doc_tab      = <l_item>-doc_tab
*              ex_miss_doc_tab = <l_item>-miss_doc_tab ).
*        endif.
*
*      endif.
*
*    endloop.


  ENDMETHOD.


  METHOD validate_sernr.

    DATA: lv_matnr   TYPE matnr,
          lv_equnr   TYPE equnr,
          lv_sernr8  TYPE char8,
          lv_sernr10 TYPE char10,
          lv_sernr   TYPE gernr.

    re_ok = abap_true.

    CLEAR: lv_sernr8, lv_sernr10.

    CASE strlen( im_sernr ).
      WHEN 16.   "SAP serial number
        lv_matnr  = im_sernr(8).
        lv_sernr8 = im_sernr+8(8).

      WHEN 18.  "DUT-id
        lv_matnr = im_sernr(8).
        lv_sernr10 = im_sernr+8(10).

      WHEN 20. "prefix + SAP serial number
        IF im_sernr(4) = '9437'.        "Check if correct identifier
          lv_matnr = im_sernr+4(8).
          lv_sernr8 = im_sernr+12(8).
        ELSE.
          re_ok = abap_false.
          RETURN.
        ENDIF.

      WHEN 22. "prefix + DUT-id
        IF im_sernr(4) = '9446'.      "Check if correct identifier
          lv_matnr = im_sernr+4(8).
          lv_sernr10 = im_sernr+12(10).
        ELSE.
          re_ok = abap_false.
          RETURN.
        ENDIF.

      WHEN OTHERS.
        re_ok = abap_false.
        RETURN.
    ENDCASE.

*   Add leading zeroes to material number
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lv_matnr
      IMPORTING
        output = lv_matnr.

    IF lv_sernr8 IS NOT INITIAL.
      IF NOT lv_sernr8 CO '0123456789'.
        re_ok = abap_false.
        RETURN.
      ENDIF.

*     Add leading zeroes to serial number
      CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
        EXPORTING
          input  = lv_sernr8
        IMPORTING
          output = lv_sernr.

      SELECT COUNT( * )
        FROM equi
        WHERE sernr = lv_sernr
          AND matnr = lv_matnr
          AND eqtyp = 'G'.

      IF sy-subrc = 4.
        re_ok = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF lv_sernr10 IS NOT INITIAL.
      IF lv_sernr10(1) = 'P' AND
         lv_sernr10+1(9) CO '0123456789' AND
         lv_sernr10+1(1) BETWEEN '1' AND '9' AND                            "Plant
         lv_sernr10+2(2) > '16' AND lv_sernr10+2(2) <= sy-datum+2(2) AND    "Year
         lv_sernr10+4(2) BETWEEN '01' AND '53'.                             "week
      ELSE.
        re_ok = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

*   Check material number exist
    SELECT COUNT( * )
      FROM mara
      WHERE matnr = lv_matnr
        AND lvorm = space.

    IF sy-subrc = 4.
      re_ok = abap_false.
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
