*&---------------------------------------------------------------------*
*& Report ZALV_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZALV_REPORT.
TABLES: sflight.
**********************************************************************
* Data Declarion *****************************************************
*TYPES: BEGIN OF ty_alv_tab,
*                carrid TYPE sflight-carrid,
*                connid TYPE sflight-connid,
*                connid TYPE sflight-connid.
DATA: it_sflight  TYPE STANDARD TABLE OF sflight,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gt2_fieldcat TYPE slis_t_fieldcat_alv,
      gt_output   TYPE STANDARD TABLE OF sflight,
      gt2_output  TYPE STANDARD TABLE OF scarr,
      wa          TYPE sflight,
      v_events    TYPE slis_t_event,
      wa_event    TYPE slis_alv_event.

**********************************************************************
***** Selection screen ***********************************************
SELECTION-SCREEN: BEGIN OF SCREEN 500 AS WINDOW TITLE text-001.
  PARAMETERS: P_airco TYPE sflight-carrid.
  SELECTION-SCREEN:
BEGIN OF LINE,
 PUSHBUTTON 12(10) text-002 USER-COMMAND exe,
END OF LINE,
END OF SCREEN 500.



**********************************************************************
** Initialization ****************************************************
INITIALIZATION.


**********************************************************************
** START-OF-SELECTION-SCREEN
**********************************************************************
START-OF-SELECTION.
CALL SELECTION-SCREEN 500 STARTING AT 30 15.


AT SELECTION-SCREEN.
**********************************************************************
** AT-SELECTION-SCREEN************************************************
**********************************************************************
  CASE sy-ucomm.
    WHEN 'EXE'.
      PERFORM get_data.
      PERFORM build_alv_report.
      PERFORM f_output.
       ENDCASE.
**********************************************************************
** SLECTION-SCREEN-OUTPUT
**********************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM decative_buttons.
*&---------------------------------------------------------------------*
*& Form DECATIVE_BUTTONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DECATIVE_BUTTONS .
DATA name(20) TYPE c.

DATA itab TYPE TABLE OF sy-ucomm.
APPEND 'NONE' TO itab.
APPEND 'SPOS' TO ITAB.
APPEND 'SAVE' TO ITAB.
APPEND 'NEXT' TO ITAB.

*p = sy-ucomm.

CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'

EXPORTING

p_status = sy-pfkey

TABLES

p_exclude = itab.
*   DATA itab TYPE TABLE OF sy-ucomm.
*  DATA(gv_repid) = sy-repid.
*
*  IF sy-dynnr = 0500.
*    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
*         EXPORTING
*              p_status  = 'STATUS'
*              p_program = gv_repid
*         TABLES
*              p_exclude = itab.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA .
SELECT * FROM sflight INTO TABLE it_sflight WHERE carrid EQ p_airco.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_ALV_REPORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BUILD_ALV_REPORT .
gt_output = IT_SFLIGHT[].
 CHECK gt_output IS NOT INITIAL.
 IF gt_output IS NOT INITIAL.
     PERFORM fill_field USING  'CARRID'         'GT_OUTPUT'      'CARRID'         text-003 .
     PERFORM fill_field USING  'CONNID'         'GT_OUTPUT'      'CONNID'         text-004 .
     PERFORM fill_field USING  'FLDATE'         'GT_OUTPUT'      'FLDATE'         text-005 .
     PERFORM fill_field USING  'PRICE'          'GT_OUTPUT'      'PRICE'          text-006 .
     PERFORM fill_field USING  'PLANETYPE'      'GT_OUTPUT'      'PLANETYPE'      text-007 .
   ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> TEXT_003
*&---------------------------------------------------------------------*
FORM FILL_FIELD  USING   p_tab_field_name
                         p_ref_field_name
                         p_ref_field_table
                         p_long_text.

DATA: ls_fieldcat TYPE slis_fieldcat_alv,
      lv_ser_no   TYPE i.

  CLEAR ls_fieldcat.
*  gv_pos = gv_pos + 1.
  lv_ser_no = lv_ser_no + 1.

  ls_fieldcat-col_pos       = lv_ser_no.
  ls_fieldcat-fieldname     = p_tab_field_name.
  ls_fieldcat-ref_fieldname = p_ref_field_name.
  ls_fieldcat-ref_tabname   = p_ref_field_table.
  ls_fieldcat-seltext_l     = p_long_text.
  ls_fieldcat-reptext_ddic  = p_long_text.
  APPEND ls_fieldcat TO  gt_fieldcat.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_OUTPUT .
  PERFORM top_of_page.
CONSTANTS:cv_a(1)        TYPE c VALUE 'A'.
  DATA: ls_layout    TYPE slis_layout_alv.

  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra = abap_true.

  IF gt_output IS NOT INITIAL.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
      it_fieldcat            = gt_fieldcat[]
      i_callback_top_of_page = 'TOP_OF_PAGE '"slis_ev_top_of_page
      i_save                 = cv_a "'A'
      is_layout              = ls_layout
    TABLES
      t_outtab               = gt_output.
  ELSE.
    MESSAGE 'There is no data generated' TYPE 'I'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form TOP_PAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE .
DATA: header TYPE slis_t_listheader,
wa TYPE slis_listheader.
* TITLE AREA

wa-typ = 'S'.
wa-info = 'Flight Report'.
APPEND wa TO header.

wa-typ = 'S'.
WRITE sy-datum to wa-info mm/dd/yyyy.
CONCATENATE 'Date:' wa-info INTO wa-info SEPARATED BY space.
APPEND wa to header.

wa-typ = 'S'.
CONCATENATE 'System User:' sy-uname INTO wa-info SEPARATED BY space.
APPEND wa TO header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = header
      i_logo              = 'ZLOGO'.

ENDFORM.

**********************************************************************
*** Second ALV report ************************************************
**********************************************************************

*&--------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->R_UCOMM    text
*      -->,          text
*      -->RS_SLEFIELDtext
*---------------------------------------------------------------------*

FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE R_UCOMM.
    WHEN '&IC1'.
      READ TABLE gt_output INTO DATA(ls_output) INDEX RS_SELFIELD-TABINDEX.
      PERFORM get_data_2th USING ls_output-carrid.
      PERFORM BUILD_FIELDCATLOG.
      PERFORM f2_output.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCATLOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCATLOG .
  gt_output = IT_SFLIGHT[].
 CHECK gt_output IS NOT INITIAL.
 IF gt_output IS NOT INITIAL.
     PERFORM fill_field2 USING  'CARRID'       'GT2_OUTPUT'      'CARRID'          text-003 .
     PERFORM fill_field2 USING  'CARRNAME'    'GT2_OUTPUT'      'CARRNAME'         text-008 .
     PERFORM fill_field2 USING  'CURRCODE'    'GT2_OUTPUT'      'CURRCODE'         text-009 .
     PERFORM fill_field2 USING  'URL'         'GT2_OUTPUT'      'URL'              text-010 .
   ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FIELD2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> TEXT_003
*&---------------------------------------------------------------------*
FORM FILL_FIELD2  USING p_tab_field_name
                         p_ref_field_name
                         p_ref_field_table
                         p_long_text.


DATA: ls_fieldcat TYPE slis_fieldcat_alv,
      lv_ser_no   TYPE i.

  CLEAR: ls_fieldcat.
*  gv_pos = gv_pos + 1.
  lv_ser_no = lv_ser_no + 1.

  ls_fieldcat-col_pos       = lv_ser_no.
  ls_fieldcat-fieldname     = p_tab_field_name.
  ls_fieldcat-ref_fieldname = p_ref_field_name.
  ls_fieldcat-ref_tabname   = p_ref_field_table.
  ls_fieldcat-seltext_l     = p_long_text.
  ls_fieldcat-reptext_ddic  = p_long_text.
  APPEND ls_fieldcat TO  gt2_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F2_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F2_OUTPUT .
  PERFORM top_of_page.
  CONSTANTS:cv_a(1)        TYPE c VALUE 'A'.
  DATA: ls_layout    TYPE slis_layout_alv.

  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra = abap_true.

  IF gt2_output IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
*      I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
      it_fieldcat            = gt2_fieldcat[]
      i_callback_top_of_page = 'TOP_OF_PAGE '"slis_ev_top_of_page
      i_save                 = cv_a "'A'
      is_layout              = ls_layout
    TABLES
      t_outtab               = gt2_output.
  ELSE.
    MESSAGE 'There is no data generated' TYPE 'I'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_2TH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_2TH using CARRID.
  SELECT * FROM SCARR INTO TABLE gt2_output WHERE carrid = CARRID.
ENDFORM.
