*&---------------------------------------------------------------------*
*& Report ZHTML_PROGRAM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZHTML_PROGRAM.


CLASS lcl_html_report DEFINITION.

  PUBLIC SECTION.
    DATA: lo_dock TYPE REF TO cl_gui_docking_container,
          lo_cont TYPE REF TO cl_gui_container,
          lo_html TYPE REF TO cl_gui_html_viewer.

    METHODS:
        generate_html_contains,
        build_html
        RETURNING VALUE(rt_html) TYPE w3_htmltab,
        on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer
                     IMPORTING action
                               frame
                               getdata
                               postdata
                               query_table.

    ENDCLASS.
DATA: lo_report TYPE REF TO lcl_html_report.
*
** Selection Screen
SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen on Java SAP GUI
SELECTION-SCREEN END OF SCREEN 1001.
** Initialization
INITIALIZATION.


**********************************************************************
*     START-SELECTION                                                *
**********************************************************************
    START-OF-SELECTION.
    PERFORM adjust_toolbar USING '1001'.
    CALL SELECTION-SCREEN 1001. " trigger screen
CREATE OBJECT lo_report.
  lo_report->generate_html_contains( ).
   CLASS lcl_html_report IMPLEMENTATION.
     METHOD generate_html_contains.

    DATA: t_event_tab TYPE cntl_simple_events.
    DATA: ls_event LIKE LINE OF t_event_tab.
    DATA: doc_url(80),
          lt_html TYPE TABLE OF w3_html.

*   Create a docking control at bottom
    CHECK lo_dock IS INITIAL.
    CREATE OBJECT lo_dock
      EXPORTING
        repid = sy-cprog
        dynnr = sy-dynnr
        ratio = 80
        side  = cl_gui_docking_container=>dock_at_bottom
        NAME  = 'DOCK_CONT'.
    IF sy-subrc <> 0.
      MESSAGE 'Error in the Docking control' TYPE 'S'.
      EXIT.
    ENDIF.
*
    CREATE OBJECT lo_html
      EXPORTING
        parent = lo_dock.
    IF sy-subrc NE 0.
      MESSAGE 'Error in the HTML control' TYPE 'S'.
      EXIT.
    ENDIF.

    lt_html = me->build_html( ).

* register event
    ls_event-eventid = lo_html->m_id_sapevent.
    ls_event-appl_event = 'x'.
    APPEND ls_event TO t_event_tab.
    lo_html->set_registered_events(
        EXPORTING
           EVENTS = t_event_tab ).
*
    SET HANDLER me->on_sapevent
                FOR lo_html.
    lo_html->load_data( IMPORTING assigned_url = doc_url
                          CHANGING  data_table = lt_html ).
*      lo_html->load_html_document(
*           EXPORTING
*                document_id  = lv_html_file_name
*           IMPORTING
*                assigned_url = doc_url
*           EXCEPTIONS
*                OTHERS       = 1 ).

    lo_html->show_url( url = doc_url ).
*
    lo_html->set_ui_flag( lo_html->uiflag_no3dborder ).
    ENDMETHOD.
    METHOD build_html.

    DEFINE add_html.
      APPEND &1 TO rt_html.
    END-OF-DEFINITION.

    add_html:
      '<html>',
      '<style type="text/css">',
      'HTML { overflow: auto; height: 100%;}',
      'body{margin-left: 50%;  padding: 0; background: blue; }',
      '.fbutton{',
      ' font-size:16px;',
      ' font-weight:bold;',
      ' width:100px;',
      ' height:100px;',
      ' background-color: #4CAF50;',
      ' border-style:double;',
      ' cursor: pointer;}',
      '</style>',
      '<body>',

      '</body>',
      '</html>'.

  ENDMETHOD.
  METHOD on_sapevent.
    DATA: ls_query_table LIKE LINE OF query_table.
    DATA: lv_string TYPE STRING.
    CASE action.
      WHEN 'MY_BUTTON_1'.
        lv_string = 'HTML button says: '.
        READ TABLE query_table INTO ls_query_table WITH KEY NAME = 'PARAM1'.
        CONCATENATE lv_string ls_query_table-VALUE
          INTO lv_string
          SEPARATED BY SPACE.
        READ TABLE query_table INTO ls_query_table WITH KEY NAME = 'PARAM2'.
        CONCATENATE lv_string ls_query_table-VALUE
          INTO lv_string
          SEPARATED BY SPACE.
        MESSAGE lv_string TYPE 'I'.
    ENDCASE.
*
  ENDMETHOD.
    ENDCLASS.
*&---------------------------------------------------------------------*
*& Form ADJUST_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM ADJUST_TOOLBAR  USING pv_dynnr TYPE sy-dynnr.
  DATA: ls_header               TYPE rpy_dyhead,
        lt_containers           TYPE dycatt_tab,
        lt_fields_to_containers TYPE dyfatc_tab,
        lt_flow_logic           TYPE swydyflow,
        lv_no_toolbar           LIKE ls_header-no_toolbar.

  CALL FUNCTION 'RPY_DYNPRO_READ'
    EXPORTING
      progname             = sy-cprog
      dynnr                = pv_dynnr
    IMPORTING
      header               = ls_header
    TABLES
      containers           = lt_containers
      fields_to_containers = lt_fields_to_containers
      flow_logic           = lt_flow_logic
    EXCEPTIONS
      cancelled            = 1
      not_found            = 2
      permission_error     = 3
      OTHERS               = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN. " Ignore errors, just exit
  ENDIF.



  IF ls_header-no_toolbar = lv_no_toolbar.
    RETURN. " No change required
  ENDIF.
  IF ls_header-no_toolbar = lv_no_toolbar.
    RETURN. " No change required
  ENDIF.

  ls_header-no_toolbar = lv_no_toolbar.

  CALL FUNCTION 'RPY_DYNPRO_INSERT'
    EXPORTING
      header                 = ls_header
      suppress_exist_checks  = abap_true
    TABLES
      containers             = lt_containers
      fields_to_containers   = lt_fields_to_containers
      flow_logic             = lt_flow_logic
    EXCEPTIONS
      cancelled              = 1
      already_exists         = 2
      program_not_exists     = 3
      not_executed           = 4
      missing_required_field = 5
      illegal_field_value    = 6
      field_not_allowed      = 7
      not_generated          = 8
      illegal_field_position = 9
      OTHERS                 = 10.
  IF sy-subrc <> 2 AND sy-subrc <> 0.
    RETURN. " Ignore errors, just exit
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module LAYOYT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE LAYOYT OUTPUT.
 DATA:    lo_dock TYPE REF TO cl_gui_docking_container,
          lo_cont TYPE REF TO cl_gui_container,
          lo_html TYPE REF TO cl_gui_html_viewer.
 DATA: doc_url(80),
          lt_html TYPE TABLE OF w3_html.
lo_html->load_data( IMPORTING assigned_url = doc_url
                          CHANGING  data_table = lt_html ).
ENDMODULE.
