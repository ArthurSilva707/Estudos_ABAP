*&---------------------------------------------------------------------*
*& Report ZART_CLASSES_AMIGAS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZART_CLASSES_AMIGAS.


***********************************************************************
** DEFINIÇÕES DE CLASSE
***********************************************************************
*CLASS lcl_gerador_linhas DEFINITION DEFERRED.
*CLASS lcl_programa       DEFINITION DEFERRED.
*
*
*CLASS lcl_gerador_linhas DEFINITION FRIENDS lcl_programa.
*  PUBLIC SECTION.
*    METHODS:
*      constructor,
*      gerar_linhas_em_tela.
*
*  PRIVATE SECTION.
*    DATA:
*      v_linhas TYPE i.
*ENDCLASS.
*
*
*CLASS lcl_programa DEFINITION.
*  PUBLIC SECTION.
*    CLASS-METHODS:
*      _start_of_selection IMPORTING iv_linhas TYPE i,
*      _end_of_selection.
*
*  PRIVATE SECTION.
*    CLASS-DATA:
*      _o_gerador TYPE REF TO lcl_gerador_linhas.
*ENDCLASS.
*
*
***********************************************************************
** TELA DE SELEÇÃO
***********************************************************************
*PARAMETERS: p_linhas TYPE i DEFAULT 5.
*
*
***********************************************************************
** EXECUÇÃO PROGRAMA
***********************************************************************
*START-OF-SELECTION.
*  lcl_programa=>_start_of_selection( p_linhas ).
*
*END-OF-SELECTION.
*  lcl_programa=>_end_of_selection( ).
*
*
***********************************************************************
** IMPLEMENTAÇÕES DE CLASSE
***********************************************************************
*CLASS lcl_gerador_linhas IMPLEMENTATION.
*  METHOD constructor.
*    me->v_linhas = 10.
*  ENDMETHOD.
*
*  METHOD gerar_linhas_em_tela.
*    DO me->v_linhas TIMES.
*      WRITE:/ sy-index, '------------------------------'.
*    ENDDO.
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS lcl_programa IMPLEMENTATION.
*  METHOD _start_of_selection.
*    CREATE OBJECT _o_gerador.
*    _o_gerador->v_linhas = iv_linhas.
*  ENDMETHOD.
*
*  METHOD _end_of_selection.
*    _o_gerador->gerar_linhas_em_tela( ).
*  ENDMETHOD.
*ENDCLASS.

*********************************************************************
* DEFINIÇÕES DE CLASSE
*********************************************************************
CLASS lcl_gerador_linhas DEFINITION DEFERRED.
CLASS lcl_programa       DEFINITION DEFERRED.


CLASS lcl_gerador_linhas_fabrica DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      _fabricar_gerador IMPORTING iv_linhas TYPE i
                        RETURNING VALUE(result) TYPE REF TO lcl_gerador_linhas.
ENDCLASS.

CLASS lcl_gerador_linhas DEFINITION CREATE PRIVATE FRIENDS lcl_gerador_linhas_fabrica.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_linhas TYPE i,
      gerar_linhas_em_tela.

  PRIVATE SECTION.
    DATA:
      v_linhas TYPE i.
ENDCLASS.

CLASS lcl_programa DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      _start_of_selection IMPORTING iv_linhas TYPE i,
      _end_of_selection.

  PRIVATE SECTION.
    CLASS-DATA:
      _o_gerador TYPE REF TO lcl_gerador_linhas.
ENDCLASS.



*********************************************************************
* TELA DE SELEÇÃO
*********************************************************************
PARAMETERS: p_linhas TYPE i DEFAULT 3.



*********************************************************************
* EXECUÇÃO PROGRAMA
*********************************************************************
START-OF-SELECTION.
  lcl_programa=>_start_of_selection( p_linhas ).

END-OF-SELECTION.
  lcl_programa=>_end_of_selection( ).



*********************************************************************
* IMPLENENTAÇÕES DE CLASSE
*********************************************************************
CLASS lcl_gerador_linhas IMPLEMENTATION.
  METHOD constructor.
    me->v_linhas = iv_linhas.
  ENDMETHOD.

  METHOD gerar_linhas_em_tela.
    DO me->v_linhas TIMES.
      WRITE:/ sy-index, '------------------------------'.
    ENDDO.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_programa IMPLEMENTATION.
  METHOD _start_of_selection.
    _o_gerador = lcl_gerador_linhas_fabrica=>_fabricar_gerador( iv_linhas ).
  ENDMETHOD.

  METHOD _end_of_selection.
    _o_gerador->gerar_linhas_em_tela( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_gerador_linhas_fabrica IMPLEMENTATION.
  METHOD _fabricar_gerador.
    CREATE OBJECT result
      EXPORTING
        iv_linhas = iv_linhas.
  ENDMETHOD.
ENDCLASS.