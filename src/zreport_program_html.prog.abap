*&---------------------------------------------------------------------*
*& Report zreport_program_html
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zreport_program_html.
CLASS lcl_report DEFINITION.
*
  PUBLIC SECTION.
    DATA: lo_dock TYPE REF TO cl_gui_docking_container,
          lo_cont TYPE REF TO cl_gui_container,
          lo_html TYPE REF TO cl_gui_html_viewer.
    METHODS:
      generate_html_button,
      build_html
        RETURNING VALUE(rt_html) TYPE  w3_htmltab,
      on_sapevent
        FOR EVENT sapevent OF cl_gui_html_viewer
          IMPORTING action          " Action from button
                    frame
                    getdata
                    postdata
                    query_table.    " Additional parameters
*
ENDCLASS.                    "lcl_report DEFINITION
*
DATA: lo_report TYPE REF TO lcl_report.
*
** Selection Screen
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE aaa.
PARAMETERS: p_test AS CHECKBOX.
SELECTION-SCREEN: END   OF BLOCK blk1.
*
** Initialization
INITIALIZATION.
*  %_p_test_%_app_%-text = 'HTML Button test'.
  CREATE OBJECT lo_report.
  lo_report->generate_html_button( ).
*
** Start of Selection
START-OF-SELECTION.

*
*----------------------------------------------------------------------*
* Local Class Implementation
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.
  METHOD generate_html_button.
*   local data
    DATA: t_event_tab TYPE cntl_simple_events.
    DATA: ls_event LIKE LINE OF t_event_tab.
    DATA: doc_url(80),
          lt_html TYPE TABLE OF w3_html.
*
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
*
* Build HTML
    lt_html = me->build_html( ).
*
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

  ENDMETHOD.                    "generate_html_button
*
  METHOD build_html.

    DEFINE add_html.
      APPEND &1 TO rt_html.
    END-OF-DEFINITION.

    add_html:
      '<html>',
      '<style type="text/css">',
      'HTML { overflow: auto; height: 100%;}',
      'body{margin-left: 50%;  padding: 0; background: white; }',
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
      '<FORM name="zzhtmlbutton"',
      '       method=post',
      '       action=SAPEVENT:MY_BUTTON_1?PARAM1=Hello&PARAM2=Zevolving>',
      '<input type=submit name="TEST_BUTTON"',
      '      class="fbutton" value="Say Hello!"',
      '      title="">',
      '</form>',
      '</body>',
      '</html>'.

  ENDMETHOD.                    "build_html
*
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
  ENDMETHOD.                    "on_sapevent
*
ENDCLASS.
