*&---------------------------------------------------------------------*
*& Report ZART_CLASSES_ABSTRATAS_OO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZART_CLASSES_ABSTRATAS_OO.

CLASS lcl_sflight DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_rows TYPE i,
      selecionar_dados,

      set_t_sflight IMPORTING iv_t_sflight  TYPE ty_flights,
      get_t_sflight RETURNING VALUE(return) TYPE ty_flights,

      set_v_rows IMPORTING iv_v_rows TYPE i,
      get_v_rows RETURNING VALUE(return) TYPE i.

  PRIVATE SECTION.
    DATA:
      t_sflight TYPE ty_flights,
      v_rows    TYPE i.
ENDCLASS.

CLASS lcl_relatorio DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_sflight TYPE REF TO lcl_sflight,

      set_o_sflight IMPORTING iv_o_sflight  TYPE REF TO lcl_sflight,
      get_o_sflight RETURNING VALUE(return) TYPE REF TO lcl_sflight,

      exibir_relatorio ABSTRACT.

  PROTECTED SECTION.
    DATA:
      o_sflight TYPE REF TO lcl_sflight.
ENDCLASS.

CLASS lcl_relatorio_write DEFINITION INHERITING FROM lcl_relatorio.
  PUBLIC SECTION.
    METHODS:
      exibir_relatorio REDEFINITION.
ENDCLASS.

CLASS lcl_relatorio_alv DEFINITION INHERITING FROM lcl_relatorio.
  PUBLIC SECTION.
    METHODS:
      exibir_relatorio REDEFINITION.
ENDCLASS.


PARAMETERS: p_rows TYPE i DEFAULT 20 OBLIGATORY.
SELECTION-SCREEN: SKIP.
PARAMETERS: p_write RADIOBUTTON GROUP grp1,
            p_alv   RADIOBUTTON GROUP grp1.


START-OF-SELECTION.
  DATA: lo_sflight   TYPE REF TO lcl_sflight,
        lo_relatorio_write TYPE REF TO lcl_relatorio_write,
        lo_relatorio_alv   TYPE REF TO lcl_relatorio_alv.

  "Cria Objeto LO_SFLIGHT
  CREATE OBJECT lo_sflight
    EXPORTING
      iv_rows = p_rows.

  CASE abap_true.
    WHEN p_write.
      CREATE OBJECT lo_relatorio_write
        EXPORTING
          io_sflight = lo_sflight.

      lo_relatorio_write->exibir_relatorio( ).

    WHEN p_alv.
      CREATE OBJECT lo_relatorio_alv
        EXPORTING
          io_sflight = lo_sflight.

      lo_relatorio_alv->exibir_relatorio( ).

  ENDCASE.



CLASS lcl_sflight IMPLEMENTATION.
  "$. Region Getters and Setters
  METHOD set_t_sflight.
    me->t_sflight = iv_t_sflight.
  ENDMETHOD.
  METHOD get_t_sflight.
    return = me->t_sflight.
  ENDMETHOD.

  METHOD set_v_rows.
    me->v_rows = iv_v_rows.
  ENDMETHOD.
  METHOD get_v_rows.
    return = me->v_rows.
  ENDMETHOD.
  "$. Endregion Getters and Setters

  METHOD selecionar_dados.
    SELECT *
        UP TO me->v_rows ROWS
      INTO TABLE t_sflight
      FROM sflight.
  ENDMETHOD.

  METHOD constructor.
    me->v_rows = iv_rows.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_relatorio IMPLEMENTATION.
  METHOD constructor.
    me->o_sflight = io_sflight.
    me->o_sflight->selecionar_dados( ).
  ENDMETHOD.

  METHOD set_o_sflight.
    me->o_sflight = iv_o_sflight.
  ENDMETHOD.
  METHOD get_o_sflight.
    return = me->o_sflight.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_relatorio_write IMPLEMENTATION.
  METHOD exibir_relatorio.
    DATA: lt_sflight TYPE ty_flights,
          ls_sflight LIKE LINE OF lt_sflight.

    lt_sflight = me->o_sflight->get_t_sflight( ).

    LOOP AT lt_sflight INTO ls_sflight.
      WRITE:/ ls_sflight-carrid,
              ls_sflight-connid,
              ls_sflight-fldate,
              ls_sflight-price,
              ls_sflight-currency,
              ls_sflight-planetype,
              ls_sflight-seatsmax,
              ls_sflight-seatsocc,
              ls_sflight-paymentsum,
              ls_sflight-seatsmax_b,
              ls_sflight-seatsocc_b,
              ls_sflight-seatsmax_f,
              ls_sflight-seatsocc_f.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_relatorio_alv IMPLEMENTATION.
  METHOD exibir_relatorio.
    DATA: lo_alv     TYPE REF TO cl_salv_table,
          lt_sflight TYPE ty_flights.

    lt_sflight = me->get_o_sflight( )->get_t_sflight( ).

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = lo_alv
      CHANGING
        t_table        = lt_sflight ).

    lo_alv->display( ).
  ENDMETHOD.
ENDCLASS.