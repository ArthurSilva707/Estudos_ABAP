*&---------------------------------------------------------------------*
*& Report ZART_READ_ONLY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZART_READ_ONLY.

**********************************************************************
* DEFINIÇÕES DE CLASSE
**********************************************************************
CLASS lcl_sflight DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      _pesquisar_voo IMPORTING iv_carrid TYPE sflight-carrid
                               iv_connid TYPE sflight-connid
                               iv_fldate TYPE sflight-fldate
                     RETURNING VALUE(result) TYPE REF TO lcl_sflight.

    METHODS:
      constructor IMPORTING is_sflight TYPE sflight,
      escrever_em_tela,

      set_s_sflight IMPORTING iv_s_sflight  TYPE sflight,
      get_s_sflight RETURNING VALUE(return) TYPE sflight.

    DATA:
      v_carrid  TYPE sflight-carrid READ-ONLY,
      v_connid  TYPE sflight-connid READ-ONLY,
      v_fldate  TYPE sflight-fldate READ-ONLY.

  PRIVATE SECTION.
    CLASS-DATA:
      _t_flights TYPE TABLE OF REF TO lcl_sflight.

    DATA:
      s_sflight TYPE sflight.
ENDCLASS.


**********************************************************************
* TELA DE SELEÇÃO
**********************************************************************
PARAMETERS: p_carrid TYPE sflight-carrid DEFAULT 'AA',
            p_connid TYPE sflight-connid DEFAULT '0017',
            p_fldate TYPE sflight-fldate DEFAULT '20231230'.


**********************************************************************
* EXECUÇÃO DO PROGRAMA
**********************************************************************
START-OF-SELECTION.
  DATA: lo_sflight TYPE REF TO lcl_sflight.

  lo_sflight = lcl_sflight=>_pesquisar_voo( iv_carrid = p_carrid
                                            iv_connid = p_connid
                                            iv_fldate = p_fldate ).

  IF lo_sflight IS BOUND.
    lo_sflight->escrever_em_tela( ).

  ELSE.
    MESSAGE 'Voo não encontrado' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.

  ENDIF.


**********************************************************************
* IMPLEMENTAÇÕES DE CLASSE
**********************************************************************
CLASS lcl_sflight IMPLEMENTATION.
  "$. Region Getters and Setters
  METHOD set_s_sflight.
    me->s_sflight = iv_s_sflight.
  ENDMETHOD.
  METHOD get_s_sflight.
    return = me->s_sflight.
  ENDMETHOD.
  "$. Endregion Getters and Setters

  METHOD class_constructor.
    DATA: lo_sflight TYPE REF TO lcl_sflight,
          lt_sflight TYPE TABLE OF sflight.

    FIELD-SYMBOLS: <ls_sflight> LIKE LINE OF lt_sflight.

    SELECT *
      INTO TABLE lt_sflight
      FROM sflight.

    LOOP AT lt_sflight ASSIGNING <ls_sflight>.
      CREATE OBJECT lo_sflight
        EXPORTING
          is_sflight = <ls_sflight>.

      APPEND lo_sflight TO _t_flights.
    ENDLOOP.
  ENDMETHOD.

  METHOD _pesquisar_voo.
    READ TABLE _t_flights INTO result WITH KEY table_line->v_carrid = iv_carrid
                                               table_line->v_connid = iv_connid
                                               table_line->v_fldate = iv_fldate.
  ENDMETHOD.

  METHOD constructor.
    me->v_carrid  = is_sflight-carrid.
    me->v_connid  = is_sflight-connid.
    me->v_fldate  = is_sflight-fldate.
    me->s_sflight = is_sflight.
  ENDMETHOD.

  METHOD escrever_em_tela.
*    me->v_carrid = 'ZZ'.

    WRITE:/ me->v_carrid,
            me->v_connid,
            me->v_fldate,
            me->s_sflight-price,
            me->s_sflight-currency,
            me->s_sflight-planetype,
            me->s_sflight-seatsmax,
            me->s_sflight-seatsocc,
            me->s_sflight-paymentsum,
            me->s_sflight-seatsmax_b,
            me->s_sflight-seatsocc_b,
            me->s_sflight-seatsmax_f,
            me->s_sflight-seatsocc_f.
  ENDMETHOD.
ENDCLASS.