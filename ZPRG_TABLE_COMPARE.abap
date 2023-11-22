*&---------------------------------------------------------------------*
*& Report ZPRG_TABLE_COMPARE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprg_table_compare.

DATA gt_filters   TYPE TABLE OF rfc_db_opt WITH HEADER LINE.
DATA gt_fields    TYPE TABLE OF rfc_db_fld WITH HEADER LINE.

DATA gr_data_dest TYPE REF TO data.
DATA gr_data_locl TYPE REF TO data.
DATA gt_data_dest TYPE STANDARD TABLE OF tab512 WITH EMPTY KEY.
DATA gt_data_locl TYPE STANDARD TABLE OF tab512 WITH EMPTY KEY.

**********************************************************************
*** Tela de selecao
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
  p_tabl TYPE dd02l-tabname  DEFAULT 'T52C5'.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_prd  RADIOBUTTON GROUP g1 USER-COMMAND chg,
  p_qas  RADIOBUTTON GROUP g1,
  p_dest TYPE rfcdes-rfcdest DEFAULT ''.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS p_fld1 TYPE rfc_db_opt-text DEFAULT 'CCYCL'.
PARAMETERS p_val1 TYPE rfc_db_opt-text DEFAULT 'ZPA0'.
SELECTION-SCREEN SKIP.
PARAMETERS p_ope2 TYPE rfc_db_opt-text DEFAULT 'AND'.
PARAMETERS p_fld2 TYPE rfc_db_opt-text DEFAULT 'ABART'.
PARAMETERS p_val2 TYPE rfc_db_opt-text DEFAULT '*'.
SELECTION-SCREEN SKIP.
PARAMETERS p_ope3 TYPE rfc_db_opt-text DEFAULT ''.
PARAMETERS p_fld3 TYPE rfc_db_opt-text.
PARAMETERS p_val3 TYPE rfc_db_opt-text.
SELECTION-SCREEN SKIP.
PARAMETERS p_ope4 TYPE rfc_db_opt-text.
PARAMETERS p_fld4 TYPE rfc_db_opt-text.
PARAMETERS p_val4 TYPE rfc_db_opt-text.
SELECTION-SCREEN SKIP.
PARAMETERS p_ope5 TYPE rfc_db_opt-text.
PARAMETERS p_fld5 TYPE rfc_db_opt-text.
PARAMETERS p_val5 TYPE rfc_db_opt-text.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN.
  "Define destination
  CASE abap_true.
    WHEN p_prd. p_dest = 'S4PCLNT200'.
    WHEN p_qas. p_dest = 'S4QCLNT200'.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_DEST'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

**********************************************************************
*** Logica principal
START-OF-SELECTION.
  "Preenche tabela de filtros
  PERFORM f_add_filter USING p_fld1 p_val1 p_ope2.
  PERFORM f_add_filter USING p_fld2 p_val2 p_ope3.
  PERFORM f_add_filter USING p_fld3 p_val3 p_ope4.
  PERFORM f_add_filter USING p_fld4 p_val4 p_ope5.
  PERFORM f_add_filter USING p_fld5 p_val5 ''.

  "Le tabela no ambiente de destino
  PERFORM f_read TABLES gt_data_dest[] USING p_dest p_tabl.

  "Le tabela no ambiente local
  PERFORM f_read TABLES gt_data_locl[] USING 'NONE' p_tabl.

END-OF-SELECTION.

  "Le tabela de destino
  PERFORM f_parse TABLES gt_data_dest[] CHANGING gr_data_dest.

  "Le tabela local
  PERFORM f_parse TABLES gt_data_locl[] CHANGING gr_data_locl.

  "Exibe ALVs
  PERFORM f_display.

*&---------------------------------------------------------------------*
*& Form F_ADD_FILTER
*&---------------------------------------------------------------------*
FORM f_add_filter  USING    p_field
                            p_value
                            p_opera.

  DATA ls_filter LIKE LINE OF gt_filters[].
  DATA lv_value  TYPE c LENGTH 255.

  CHECK p_field IS NOT INITIAL.

  CONCATENATE '''' p_value '''' INTO lv_value.
  CONCATENATE p_field '=' lv_value p_opera INTO ls_filter-text SEPARATED BY space.
  APPEND ls_filter TO gt_filters[].

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_READ
*&---------------------------------------------------------------------*
FORM f_read  TABLES   pt_data
             USING    p_destination
                      p_tablename.

  CALL FUNCTION 'RFC_READ_TABLE'
    DESTINATION p_destination
    EXPORTING
      query_table          = p_tablename
    TABLES
      options              = gt_filters[]
      fields               = gt_fields[]
      data                 = pt_data
    EXCEPTIONS
      table_not_available  = 1
      table_without_data   = 2
      option_not_valid     = 3
      field_not_valid      = 4
      not_authorized       = 5
      data_buffer_exceeded = 6
      OTHERS               = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_PARSE
*&---------------------------------------------------------------------*
FORM f_parse  TABLES   pt_in
              CHANGING pr_out.

  FIELD-SYMBOLS:
    <fs_in>  TYPE tab512,
    <fs_out> TYPE any,
    <ft_out> TYPE ANY TABLE.

  CREATE DATA pr_out TYPE STANDARD TABLE OF (p_tabl) WITH DEFAULT KEY.
  ASSIGN pr_out->* TO <ft_out>.

  LOOP AT pt_in[] ASSIGNING <fs_in>.
    ASSIGN <fs_in> TO <fs_out> CASTING TYPE (p_tabl).
    INSERT <fs_out> INTO TABLE <ft_out>.
  ENDLOOP.
  SORT <ft_out>.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DISPLAY
*&---------------------------------------------------------------------*
FORM f_display .

  DATA:
    lr_splitter  TYPE REF TO cl_gui_splitter_container,
    lr_container TYPE REF TO cl_gui_container.
  FIELD-SYMBOLS:
    <fs_table> TYPE STANDARD TABLE.

  lr_splitter = NEW #( parent                  = cl_gui_container=>screen0
                       no_autodef_progid_dynnr = abap_true
                       rows                    = 1
                       columns                 = 2 ).

  "Cria ALV esquerda com dados do ambiente de destino
  lr_container = lr_splitter->get_container( row = 1 column = 1 ).
  ASSIGN gr_data_dest->* TO <fs_table>.
  PERFORM f_create_salv TABLES <fs_table> USING lr_container 'Destino'.

  "Cria ALV direita com dados do ambiente local
  lr_container = lr_splitter->get_container( row = 1 column = 2 ).
  ASSIGN gr_data_locl->* TO <fs_table>.
  PERFORM f_create_salv TABLES <fs_table> USING lr_container 'Local'.

  WRITE space.  "forca exibicao das SALVs com splitter

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CREATE_SALV
*&---------------------------------------------------------------------*
FORM f_create_salv  TABLES   pt_data
                    USING    pr_container
                             p_title.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container  = pr_container
        IMPORTING
          r_salv_table = DATA(lr_salv)
        CHANGING
          t_table      = pt_data[]
      ).

      "Seta titulo
      lr_salv->get_display_settings( )->set_list_header( p_title ).

      "Habilita funcoes
      lr_salv->get_functions( )->set_all( if_salv_c_bool_sap=>true ).

      "Seta todas colunas como otimizada
      lr_salv->get_columns( )->set_optimize( ).

      "Seta ZEBRA
      lr_salv->get_display_settings( )->set_striped_pattern( if_salv_c_bool_sap=>true ).

      "Habilita botao de salvar layout
      DATA(lr_layout) = lr_salv->get_layout( ).
      lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
      lr_layout->set_key( VALUE salv_s_layout_key( report = sy-repid handle = p_title(1) ) ).

      "Seta linha selecionavel
      lr_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      "Exibe ALV
      lr_salv->display( ).

    CATCH cx_salv_msg INTO DATA(lo_except).
      MESSAGE lo_except->if_message~get_longtext( ) TYPE 'E'.
  ENDTRY.

ENDFORM.