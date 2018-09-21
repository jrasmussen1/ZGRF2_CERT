class ZCL_Z_HEAT_DATA_DPC_EXT definition
  public
  inheriting from ZCL_Z_HEAT_DATA_DPC
  create public .

public section.
protected section.

  methods BOMMATERIALSET_GET_ENTITYSET
    redefinition .
  methods CERTHEADERSET_GET_ENTITY
    redefinition .
  methods COMPONENTSET_CREATE_ENTITY
    redefinition .
  methods COMPONENTSET_DELETE_ENTITY
    redefinition .
  methods COMPONENTSET_GET_ENTITYSET
    redefinition .
  methods VENDORCODESET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_HEAT_DATA_DPC_EXT IMPLEMENTATION.


  METHOD bommaterialset_get_entityset.
**TRY.
*CALL METHOD SUPER->BOMMATERIALSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

check 1 = 1.

*    DATA : lv_bommatnr TYPE LINE OF zcl_z_heat_data_mpc=>tt_bommaterial.
*
*    DATA : lv_key_tab TYPE /iwbep/s_mgw_name_value_pair,
*           lv_sernr   TYPE gernr,
*           lv_matnr   TYPE matnr_d,
*           lv_werks   TYPE werks_d,
*           lv_error   TYPE abap_bool.
*
*    DATA : lt_stb TYPE TABLE OF stpox.
*
*    DATA : lo_message_container TYPE REF TO /iwbep/if_message_container.
*
*    READ TABLE it_key_tab INTO lv_key_tab WITH KEY name = 'Sernr'.
*    CHECK sy-subrc = 0.
*
*    lv_sernr = lv_key_tab-value.
*    REPLACE 'p' IN lv_sernr WITH 'P'.
*
*    lv_matnr = lv_sernr(8).
*
*    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*      EXPORTING
*        input  = lv_matnr
*      IMPORTING
*        output = lv_matnr.
*
*    lv_werks = 'BJ02'. "to be set from user data
*
** Read BOM for material...
*    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*      EXPORTING
*        capid                 = 'GUNV'       " Application ID
*        datuv                 = sy-datum     " Valid On
*        emeng                 = 1            " Required quantity
*        mehrs                 = 'X'          " Multi-level explosion
*        mtnrv                 = lv_matnr     " Mat.
*        stlan                 = '8'          " BOM usage
*        werks                 = lv_werks     " Plant
*      TABLES
*        stb                   = lt_stb       " Collective item data table
*      EXCEPTIONS
*        alt_not_found         = 1
*        call_invalid          = 2
*        material_not_found    = 3
*        missing_authorization = 4
*        no_bom_found          = 5
*        no_plant_data         = 6
*        no_suitable_bom_found = 7
*        conversion_error      = 8
*        OTHERS                = 9.
*
*    IF sy-subrc <> 0.
*      lo_message_container = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
*
*      lo_message_container->add_message(
*         EXPORTING
*           iv_msg_type               = 'E'               " Message Type
*           iv_msg_id                 = 'ZGRF_QM_CERT'    " Message Class
*           iv_msg_number             = '074'             " Message Number
*       ).
*
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          message_container = lo_message_container.
*    ELSE.
*      CLEAR lv_bommatnr.
*      lv_bommatnr-sernr = lv_sernr.
*
*      LOOP AT lt_stb ASSIGNING FIELD-SYMBOL(<l_stb>).
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = <l_stb>-idnrk
*          IMPORTING
*            output = lv_bommatnr-matnr.
*
*        APPEND lv_bommatnr TO et_entityset.
*      ENDLOOP.
*
*      SORT et_entityset BY sernr matnr.
*      DELETE ADJACENT DUPLICATES FROM et_entityset.
*    ENDIF.

  ENDMETHOD.


  METHOD certheaderset_get_entity.

    DATA : lv_key_tab TYPE /iwbep/s_mgw_name_value_pair,
           lv_sernr   TYPE gernr.

    DATA: lt_comp_data TYPE TABLE OF zqcert_comp_data.

    READ TABLE it_key_tab INTO lv_key_tab WITH KEY name = 'Sernr'.
    IF sy-subrc = 0.
      lv_sernr = lv_key_tab-value.
      REPLACE 'p' IN lv_sernr WITH 'P'.

      SELECT *
        FROM zqcert_comp_data
        INTO TABLE lt_comp_data
        WHERE sernr = lv_key_tab-value.

      IF sy-subrc NE 0.
        CHECK 1 = 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD componentset_create_entity.
    DATA: lv_comp_input TYPE zcl_z_heat_data_mpc=>ts_component,
          lv_comp_data  TYPE zqcert_comp_data,
          lv_update     TYPE abap_bool,
          lv_matnr      TYPE matnr_d,
          lv_werks      TYPE werks_d,
          lv_error      TYPE abap_bool.

    DATA: lv_matnr_db  TYPE zqcert_comp_data-matnr,
          lv_vendor_db TYPE zqcert_comp_data-vendor_code,
          lv_heat_db   TYPE zqcert_comp_data-heat_data.

    DATA lo_heat TYPE REF TO zcl_cert_heat_data.
    DATA lo_message_container TYPE REF TO /iwbep/if_message_container.

    io_data_provider->read_entry_data( IMPORTING es_data = lv_comp_input ).

    IF lv_comp_input-matnr IS INITIAL AND lv_comp_input-vendor_code IS INITIAL.
      RETURN.
    ENDIF.

    REPLACE 'p' IN lv_comp_input-sernr WITH 'P'.

    TRANSLATE lv_comp_input-heat_data TO UPPER CASE.

*   Validate material number.
*   Check if material number exist
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lv_comp_input-matnr
      IMPORTING
        output = lv_matnr.

    SELECT COUNT(*)
      FROM mara
      WHERE matnr = lv_matnr
        AND lvorm = space.

    IF sy-subrc = 4.
      lo_message_container = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

      lo_message_container->add_message(
        EXPORTING
          iv_msg_type               = 'E'
          iv_msg_id                 = 'ZGRF_QM_CERT'
          iv_msg_number             = '071'             " Invalid material number
          iv_message_target         = 'MATNR'           " Used by front-end to highlight the right cell
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*   Validate heat data value.
    CREATE OBJECT lo_heat.

    lo_heat->validate(
      EXPORTING
        im_vendor_code = lv_comp_input-vendor_code
        im_heat_val    = lv_comp_input-heat_data
      IMPORTING ex_error = lv_error ).

    IF lv_error = abap_true.
      lo_message_container = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

      lo_message_container->add_message(
        EXPORTING
          iv_msg_type               = 'E'
          iv_msg_id                 = 'ZGRF_QM_CERT'
          iv_msg_number             = '072'             " Invalid heat data value
          iv_message_target         = 'HEAT_VALUE'      " Used by front-end to place red frame
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*  Find plant from user parameter WRK
    SELECT parva
      INTO lv_werks
      FROM usr05 UP TO 1 ROWS
      WHERE bname = sy-uname
        AND parid = 'WRK'.
    ENDSELECT.

    IF sy-subrc = 4.
      lo_message_container = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

      lo_message_container->add_message(
        EXPORTING
          iv_msg_type               = 'E'
          iv_msg_id                 = 'ZGRF_QM_CERT'
          iv_msg_number             = '075'             " No plant defined for user
          iv_message_target         = 'MATNR'           " Used by front-end to highlight the right cell
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    lv_comp_data-werks = lv_werks.
    lv_comp_data-sernr = lv_comp_input-sernr.
    lv_comp_data-comp_no = lv_comp_input-comp_no.
    lv_comp_data-matnr = lv_comp_input-matnr.
    lv_comp_data-vendor_code = lv_comp_input-vendor_code.
    lv_comp_data-heat_data = lv_comp_input-heat_data.
    lv_comp_data-erdat = sy-datum.
    lv_comp_data-erzet = sy-uzeit.
    lv_comp_data-uname = sy-uname.

*   Update DB table
    lv_update = abap_false.

    SELECT SINGLE matnr vendor_code heat_data
      INTO ( lv_matnr_db, lv_vendor_db, lv_heat_db )
      FROM zqcert_comp_data
      WHERE werks = lv_comp_data-werks
        AND sernr = lv_comp_data-sernr
        AND comp_no = lv_comp_data-comp_no.

    IF sy-subrc = 0.  "Entry already exist - check if values have been changed
      IF lv_comp_data-matnr NE lv_matnr_db OR
         lv_comp_data-vendor_code NE lv_vendor_db OR
         lv_comp_data-heat_data NE lv_heat_db.
        lv_update = abap_true.  "update of existing record
      ENDIF.
    ELSE.
      lv_update = abap_true.    "It is a new record
    ENDIF.

    IF lv_update = abap_true.
      MODIFY zqcert_comp_data FROM lv_comp_data.
      IF sy-subrc NE 0.
        lo_message_container = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

        lo_message_container->add_message(
          EXPORTING
            iv_msg_type               = 'E'
            iv_msg_id                 = 'ZGRF_QM_CERT'
            iv_msg_number             = '073'             " Error in update of heat data
        ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.

      ELSE.
*       Check if vendor certificate exist for material
        IF NOT lo_heat->vendor_cert_exist( im_matnr = lv_comp_data-matnr
                                           im_vendor_code = lv_comp_data-vendor_code
                                           im_heat_data = lv_comp_data-heat_data ).
          lo_heat->send_email( lv_comp_data ).
        ENDIF.

        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD componentset_delete_entity.
**TRY.
*CALL METHOD SUPER->COMPONENTSET_DELETE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA: lwa_key_tab TYPE /iwbep/s_mgw_name_value_pair,
          lv_werks    type werks_d,
          lv_sernr    TYPE gernr,
          lv_compno   TYPE zcomp_no.

    READ TABLE it_key_tab INTO lwa_key_tab WITH KEY name = 'Sernr'.
    CHECK sy-subrc = 0.

    lv_sernr = lwa_key_tab-value.
    REPLACE 'p' IN lv_sernr WITH 'P'.

    READ TABLE it_key_tab INTO lwa_key_tab WITH KEY name = 'CompNo'.
    CHECK sy-subrc = 0.

    lv_compno = lwa_key_tab-value.

*   Find plant from user parameter WRK
    SELECT parva
      INTO lv_werks
      FROM usr05 UP TO 1 ROWS
      WHERE bname = sy-uname
        AND parid = 'WRK'.
    ENDSELECT.

    IF sy-subrc = 4.
      return.
    ENDIF.

    DELETE FROM zqcert_comp_data WHERE werks = lv_werks
                                   AND sernr = lv_sernr
                                   AND comp_no = lv_compno.
    IF sy-subrc = 0.
      "OK row deleted
      COMMIT WORK.
    ELSE.
      "Also ok - we don't know if the rows exist in table...
    ENDIF.

  ENDMETHOD.


  METHOD componentset_get_entityset.

    DATA : lv_comp TYPE LINE OF zcl_z_heat_data_mpc=>tt_component.

    DATA : lv_key_tab TYPE /iwbep/s_mgw_name_value_pair,
           lv_sernr   TYPE char22,
           lv_lang2   TYPE char2,
           lv_langu   TYPE lang,
           lv_error   TYPE abap_bool,
           lv_werks   TYPE werks_d.

    DATA lo_message_container TYPE REF TO /iwbep/if_message_container.


    READ TABLE it_key_tab INTO lv_key_tab WITH KEY name = 'Sernr'.
    CHECK sy-subrc = 0.

    lv_sernr = lv_key_tab-value.
    REPLACE 'p' IN lv_sernr WITH 'P'.

    READ TABLE it_key_tab INTO lv_key_tab WITH KEY name = 'Language'.
    CHECK sy-subrc = 0.

    lv_lang2 = lv_key_tab-value.

*   Convert language from 2 characters to 1 character
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = lv_lang2
      IMPORTING
        output           = lv_langu
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      EXIT.  "This should never happen
    ENDIF.

*   Check serial number
    IF zcl_certificate=>validate_sernr( lv_sernr ).
      lv_error = abap_false.
    ELSE.
      lv_error = abap_true.
    ENDIF.

*    IF lv_error = abap_false.
*     Read BOM for material...
*     Save materials from BOM using shared objects???
*    ENDIF.

    IF lv_error = abap_true.
      lo_message_container = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

      lo_message_container->add_message(
        EXPORTING
          iv_msg_type               = 'E'               " Message Type
          iv_msg_id                 = 'ZGRF_QM_CERT'    " Message Class
          iv_msg_number             = '070'             " Message Number
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.

    ELSE.
*     Remove identifier from serial number.
      IF strlen( lv_sernr ) > 18.
        lv_sernr = lv_sernr+4(18).
      ENDIF.

*     Find plant from user parameter WRK
      SELECT parva
        INTO lv_werks
        FROM usr05 UP TO 1 ROWS
        WHERE bname = sy-uname
          AND parid = 'WRK'.
      ENDSELECT.

      IF sy-subrc = 4.
        lo_message_container = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

        lo_message_container->add_message(
          EXPORTING
            iv_msg_type               = 'E'
            iv_msg_id                 = 'ZGRF_QM_CERT'
            iv_msg_number             = '075'             " No plant found for user
        ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
      ENDIF.


*     Read all components
      CLEAR lv_comp.

      SELECT comp_no comp_txt
        INTO (lv_comp-comp_no, lv_comp-comp_txt)
        FROM zqcert_comp
        WHERE spras = lv_langu
        ORDER BY comp_no.

        APPEND lv_comp TO et_entityset.
      ENDSELECT.

      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

      LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<l_comp>).
        SELECT SINGLE matnr vendor_code heat_data
          FROM zqcert_comp_data
          INTO CORRESPONDING FIELDS OF <l_comp>
          WHERE werks = lv_werks
            AND sernr = lv_sernr
            AND comp_no = <l_comp>-comp_no.

        IF sy-subrc NE 0.
          CHECK 1 = 1.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD vendorcodeset_get_entityset.

    DATA: lt_vendorcode  TYPE TABLE OF zvendor_code,
          lwa_vendorcode TYPE LINE OF zcl_z_heat_data_mpc=>tt_vendorcode.

    SELECT vendor_code
      INTO TABLE lt_vendorcode
      FROM zqcert_vendor
      ORDER BY vendor_code.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CLEAR lwa_vendorcode.

    LOOP AT lt_vendorcode ASSIGNING FIELD-SYMBOL(<l_vcode>).
      lwa_vendorcode-zvendor_code = <l_vcode>.
      APPEND lwa_vendorcode TO et_entityset.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
