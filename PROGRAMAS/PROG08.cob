      ******************************************************************
      * Autor.....: Alexandre Trevisani (PROVA COBOL)
      * Data......: Julho/2019
      * Programa..: PROG08 - Cálculo de Distancia entre 2 pontos
      * Objetivos.: Utilizaçãoo na Distribuição Cliente/Vendedor
      * Parâmetros: (E) Esperado e (D) Devolvidos :
      * LK-LATITUDE-C - (E) Latidude  do Cliente
      * LK-LONGITUDE-C- (E) Longitude do Cliente
      * LK-LATITUDE-V - (E) Latitude  do Vendedor
      * LK-LONGITUDE-V- (E) Longitude do Vendedor
      * LK-DISTANCIA  - (D) DistÃ¢ncia Calculada em Metros
      * LK-STATUS-RET - (D) CÃ³digo Retorno '0' OK, Diferente de '0'
      *                     ERRO = '1' - 'LATITUDE  CLIENTE  INVALIDA'
      *                     ERRO = '2' - 'LONGITUDE CLIENTE  INVALIDA '
      *                     ERRO = '3' - 'LATITUDE  VENDEDOR INVALIDA'
      *                     ERRO = '4' - 'LONGITUDE VENDEDOR INVALIDA'
      * LK-MENSAGEM  -  (D) Mensagem ok ou do erro ocorrido
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PROG08.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE     SECTION.

       01   WS-AREA-CALCULO.
           05 WS-LATITUDE-C      PIC S9(03)V9(8).
           05 RD-LATITUDE-C      REDEFINES WS-LATITUDE-C.
              10 WS-LAT-I-C      PIC S9(03).
              10 WS-LAT-D-C      PIC V9(8).

           05 WS-LONGITUDE-C     PIC S9(03)V9(8).
           05 RD-LONGITUDE-C     REDEFINES WS-LONGITUDE-C.
              10 WS-LOG-I-C      PIC S9(03).
              10 WS-LOG-D-C      PIC V9(8).

           05 WS-LATITUDE-V      PIC S9(03)V9(8).
           05 RD-LATITUDE-V      REDEFINES WS-LATITUDE-V.
           10 WS-LAT-I-V         PIC S9(03).
           10 WS-LAT-D-V         PIC V9(8).

           05 WS-LONGITUDE-V     PIC S9(03)V9(8).
           05 RD-LONGITUDE-V     REDEFINES WS-LONGITUDE-V.
              10 WS-LOG-I-V      PIC S9(03).
              10 WS-LOG-D-V      PIC V9(8).

           05 WS-CALCULO         PIC S9(03)V9(8).
           05 RD-CALCULO         REDEFINES WS-CALCULO.
              10 WS-CAL-I        PIC S9(03).
              10 WS-CAL-D        PIC V9(8).

           05 WS-GRAU-C-LA       PIC 9(06)V99.
           05 RD-GRAU-C-LA       REDEFINES WS-GRAU-C-LA.
             10 WS-GRAU-C-LA-G   PIC 99.
             10 WS-GRAU-C-LA-M   PIC 99.
             10 WS-GRAU-C-LA-S   PIC 99V99.

           05 WS-GRAU-C-LO       PIC 9(06)V99.
           05 RD-GRAUC-LO        REDEFINES WS-GRAU-C-LO.
              10 WS-GRAU-C-LO-G  PIC 99.
              10 WS-GRAU-C-LO-M  PIC 99.
              10 WS-GRAU-C-LO-S  PIC 99V99.

           05 WS-GRAU-V-LA       PIC 9(06)V99.
           05 RD-GRAU-V-LA       REDEFINES WS-GRAU-V-LA.
              10 WS-GRAU-V-LA-G  PIC 99.
              10 WS-GRAU-V-LA-M  PIC 99.
              10 WS-GRAU-V-LA-S  PIC 99V99.

           05 WS-GRAU-V-LO       PIC 9(06)V99.
           05 RD-GRAU-V-LO       REDEFINES WS-GRAU-V-LO.
              10 WS-GRAU-V-LO-G  PIC 99.
              10 WS-GRAU-V-LO-M  PIC 99.
              10 WS-GRAU-V-LO-S  PIC 99V99.

           05 WS-DLA-CLI          PIC 9(04)V9999.
           05 RD-DLA-CLI          REDEFINES WS-DLA-CLI.
              10 WS-DLA-CLI-G     PIC 99.
              10 WS-DLA-CLI-M     PIC 99.
              10 WS-DLA-CLI-S     PIC 99V99.

           05 WS-DLA-VEN          PIC 9(04)V9999.
           05 RD-DLA-VEN          REDEFINES WS-DLA-VEN.
              10 WS-DLA-VEN-G     PIC 99.
              10 WS-DLA-VEN-M     PIC 99.
              10 WS-DLA-VEN-S     PIC 99V99.

           05 WS-AREAS-AUXILIARES.
              10 WS-DIS-DLA-M     PIC 9(05)V9999.
              10 WS-DIS-DLO-M     PIC 9(05)V9999.
              10 WS-DIF-DLA-M     PIC 9(05)V9999.
              10 WS-DIF-DLO-M     PIC 9(05)V9999.
              10 WS-SOMATORIA     PIC 9(12)V99999999.
              10 WS-DISTANCIA     PIC 9(05)V9999.
              10 WS-RESTO         PIC 9v99999999.

        LINKAGE        SECTION.

       COPY   LK_CALC_DISTANCIA.

       PROCEDURE    DIVISION  USING  LK-PARAMETROS.

       INICIO.
      *

              PERFORM VALIDA-LINKAGE

              IF LK-STATUS-RET = '0'
                 PERFORM CALCULA-DISTANCIA
                 MOVE WS-DISTANCIA           TO LK-DISTANCIA
               END-IF

             GOBACK
               .
           FIM-PROGRAMA.
              EXIT.

      *-------------------------------------------------------------------------
          VALIDA-LINKAGE.
      *-------------------------------------------------------------------------

              MOVE '0'                          TO LK-STATUS-RET

              IF LK-LATITUDE-C = ZEROS OR
                 LK-LATITUDE-C NOT NUMERIC
                 MOVE 'LATITUDE CLIENTE INVALIDA    ' TO LK-MENSAGEM
                 MOVE '1'                             TO LK-STATUS-RET
                 MOVE ZEROS                           TO LK-DISTANCIA
              END-IF

              IF LK-LONGITUDE-C = ZEROS OR
                 LK-LONGITUDE-C NOT NUMERIC
                 MOVE 'LONGITUDE CLIENTE INVALIDA   ' TO LK-MENSAGEM
                 MOVE '2'                             TO LK-STATUS-RET
                 MOVE ZEROS                           TO LK-DISTANCIA
              END-IF

              IF LK-LATITUDE-V = ZEROS OR
                 LK-LATITUDE-V NOT NUMERIC
                 MOVE 'LATITUDE VENDEDOR INVALIDA   ' TO LK-MENSAGEM
                 MOVE '3'                             TO LK-STATUS-RET
                 MOVE ZEROS                           TO LK-DISTANCIA
              END-IF

              IF LK-LONGITUDE-V = ZEROS OR
                 LK-LONGITUDE-V NOT NUMERIC
                 MOVE 'LONGITUDE VENDEDOR INVALIDA  ' TO LK-MENSAGEM
                 MOVE '4'                             TO LK-STATUS-RET
                 MOVE ZEROS                           TO LK-DISTANCIA
              END-IF
              .

          VALIDA-LINKAGE-FIM.
              EXIT.

      *-------------------------------------------------------------------------
          CALCULA-DISTANCIA.
      *-------------------------------------------------------------------------

      *     COMPUTE LK-LONGITUDE-C = LK-LONGITUDE-C * -1
      *     COMPUTE LK-LONGITUDE-V = LK-LONGITUDE-V * -1
      *-------------------------------------------
      *      TRANSFORMA LATITUDE DE DECIMAL EM GRAU
      *-------------------------------------------
           MOVE LK-LATITUDE-C    TO WS-LATITUDE-C
           MOVE WS-LAT-I-C        TO WS-GRAU-C-LA-G
           COMPUTE WS-CALCULO =
                   ((LK-LATITUDE-C - WS-GRAU-C-LA-G) * 60)
           MOVE WS-CAL-I       TO WS-GRAU-C-LA-M
           COMPUTE WS-GRAU-C-LA-S = (WS-CAL-D * 60)

           MOVE LK-LATITUDE-V    TO WS-LATITUDE-V
           MOVE WS-LAT-I-V    TO WS-GRAU-V-LA-G
           COMPUTE WS-CALCULO =
                   ((LK-LATITUDE-V - WS-GRAU-V-LA-G) * 60)
           MOVE WS-CAL-I       TO WS-GRAU-V-LA-M
           COMPUTE WS-GRAU-V-LA-S = (WS-CAL-D * 60)

      *--------------------------------------------
      *      TRANSFORMA LONGITUDE DE DECIMAL EM GRAU
      *--------------------------------------------

           MOVE LK-LONGITUDE-C    TO WS-LONGITUDE-C
           MOVE WS-LOG-I-C    TO WS-GRAU-C-LO-G
           COMPUTE WS-CALCULO =
                   ((LK-LONGITUDE-C - WS-GRAU-C-LO-G) * 60)
           MOVE WS-CAL-I       TO WS-GRAU-C-LO-M
           COMPUTE WS-GRAU-C-LO-S = (WS-CAL-D * 60)

           MOVE LK-LONGITUDE-V    TO WS-LONGITUDE-V
           MOVE WS-LOG-I-V    TO WS-GRAU-V-LO-G
           COMPUTE WS-CALCULO =
                   ((LK-LONGITUDE-V - WS-GRAU-V-LO-G) * 60)
           MOVE WS-CAL-I       TO WS-GRAU-V-LO-M
           COMPUTE WS-GRAU-V-LO-S = (WS-CAL-D * 60)

      *----------------------------------------------------
      *    TRANSFORMA LATITUDE/LONGITUDE DE GRAU EM METROS
      *----------------------------------------------------

           COMPUTE WS-DIF-DLA-M = (WS-GRAU-C-LA - WS-GRAU-V-LA)
           COMPUTE WS-DIF-DLO-M = (WS-GRAU-C-LO - WS-GRAU-V-LO)

           COMPUTE WS-DIS-DLA-M = ((WS-DIF-DLA-M / 60) * 1,852)
           COMPUTE WS-DIS-DLO-M = ((WS-DIF-DLO-M / 60) * 1,852)

      *----------------------------------------------------
      *      CALCULA A DISTANCIA ENTRE OS 2 PONTOS EM METROS
      *----------------------------------------------------

           COMPUTE WS-SOMATORIA =
                  (WS-DIS-DLA-M * WS-DIS-DLA-M) +
                  (WS-DIS-DLO-M * WS-DIS-DLO-M)

           COMPUTE WS-DISTANCIA =  (WS-SOMATORIA ** (1/2))
           .
       CALCULA-DISTANCIA-FIM.
           EXIT.

           END PROGRAM PROG08.
