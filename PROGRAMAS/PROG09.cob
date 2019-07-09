      ******************************************************************
      * Autor.....: Alexandre Trevisani (PROVA COBOL)
      * Data......: Julho/2019
      * Programa..: PROG09 - Distribuicão Clientes/Vendedor
      * Chamadas..: PROG08 - Calcula a Distancia entre Cliente/Vendedor
      * ObservaçÃo: Apos Calculo, Atualiza Cod vendedor no Cad. Cliente
      *           : Geracão do arquivo PLANILHA.CSV no final do Processo
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PROG09.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT CLIENTES ASSIGN TO "C:\COBOL\CLIENTES.ARQ"
              ORGANIZATION         IS INDEXED
              ACCESS  MODE         IS SEQUENTIAL
              FILE STATUS          IS CLI-STAT
              ALTERNATE RECORD KEY IS CLI-COD
              ALTERNATE RECORD KEY IS CLI-NOME  WITH DUPLICATES
              LOCK MODE IS MANUAL WITH LOCK ON MULTIPLE RECORDS
              RECORD KEY           IS CLI-KEY.

       SELECT VENDEDOR ASSIGN TO "C:\COBOL\VENDEDOR.ARQ"
              ORGANIZATION         IS INDEXED
              ACCESS  MODE         IS RANDOM
              FILE STATUS          IS VEN-STAT
              ALTERNATE RECORD KEY IS VEN-COD
              ALTERNATE RECORD KEY IS VEN-NOME  WITH DUPLICATES
              LOCK MODE IS MANUAL WITH LOCK ON MULTIPLE RECORDS
              RECORD KEY          IS VEN-KEY.

       SELECT DISTRIB ASSIGN TO "C:\COBOL\DISTRIB.ARQ"
              ORGANIZATION         IS INDEXED
              ACCESS  MODE         IS RANDOM
              FILE STATUS          IS DIS-STAT
              RECORD KEY           IS DIS-KEY.

       SELECT PLANILHA ASSIGN TO "C:\COBOL\PLANILHA.CSV"
              ORGANIZATION         IS LINE SEQUENTIAL
              FILE STATUS          IS CSV-STAT.

       DATA            DIVISION.
       FILE            SECTION.

       COPY FD_CLIENTES.

       COPY FD_VENDEDOR.

       FD DISTRIB.

       01 DISTRIB-DIS.
           05 DIS-KEY.
              10 DIS-CODC   PIC 9(007).
           05 DIS-CODV      PIC 9(003).
           05 DIS-METROS    PIC 9(006)V999.

       FD PLANILHA.

       01 PLANILHA-CSV.
          05 CSV-REG        PIC X(100).

       WORKING-STORAGE SECTION.

       01  WS-MODULO.
           05 FILLER         PIC X(30) VALUE
              "PROVA COBOL - DISTRIBUICAO".
           05 FILLER         PIC X(12) VALUE "MENSAGEM :".
           05 WS-MENSAG      PIC X(40) VALUE SPACES.

       77 CLI-STAT           PIC 9(02).
           88 FSC-OK         VALUE ZEROS.
           88 FSC-DUPL       VALUE 22.
           88 FSC-FIM-REG    VALUE 10.
           88 FSC-NAO-EXISTE VALUE 35.

       77 VEN-STAT          PIC 9(02).
           88 FSV-OK         VALUE ZEROS.
           88 FSV-FIM-REG    VALUE 10.
           88 FSV-NAO-EXISTE VALUE 35.

       77 DIS-STAT           PIC 9(02).
           88 FSD-OK         VALUE ZEROS.
           88 FSD-FIM-REG    VALUE 10.
           88 FSD-NAO-EXISTE VALUE 35.

       77 CSV-STAT           PIC 9(02).
           88 FSR-OK         VALUE ZEROS.

       77 WS-ERRO            PIC X.
           88 E-SIM          VALUES ARE "S" "s".
           88 E-NAO          VALUES ARE "N" "n".

       77 WS-NUML            PIC 999.
       77 WS-NUMC            PIC 999.
       77 COR-FUNDO          PIC 9 VALUE 1.
       77 COR-FRENTE         PIC 9 VALUE 6.

       77 WS-STATUS          PIC X(40).
       77 WS-MSGERRO         PIC X(80).
       77 WS-READ-C          PIC 9(03) VALUE ZEROS.
       77 WS-READ-V          PIC 9(03) VALUE ZEROS.
       77 WS-PRT             PIC 9(03) VALUE ZEROS.

       01 LINHA-TITULO1.
           05 FILLER         PIC X(030) VALUE
           'PROVA DE COBOL '.
           05 FILLER         PIC X(066) VALUE
           '                 PLANILHA DE DISTRIBUICAO CLI/VEND'.

       01 LINHA-TITULO2.
           05 FILLER         PIC X(013)  VALUE 'COD. CLIENTE;'.
           05 FILLER         PIC X(013)  VALUE 'RAZAO SOCIAL;'.
           05 FILLER         PIC X(014)  VALUE 'COD. VENDEDOR;'.
           05 FILLER         PIC X(014)  VALUE 'NOME VENDEDOR;'.
           05 FILLER         PIC X(010)  VALUE 'DISTANCIA;'.


       01 LINHA-DET.
           05 CSV-CODC       PIC X(007)  VALUE SPACES.
           05 FILLER         PIC X(001)  VALUE ';'.
           05 CSV-RAZS       PIC X(040)  VALUE SPACES.
           05 FILLER         PIC X(001)  VALUE ';'.
           05 CSV-CODV       PIC X(003)  VALUE SPACES.
           05 FILLER         PIC X(001)  VALUE ';'.
           05 CSV-NOMEV      PIC X(030)  VALUE SPACES.
           05 FILLER         PIC X(001)  VALUE ';'.
           05 CSV-DISTANCIA  PIC BB.BB9,999.
           05 FILLER         PIC X(002)  VALUE 'Km'.
           05 FILLER         PIC X(001)  VALUE ';'.

       COPY LK_CALC_DISTANCIA.

       COPY screenio.

       SCREEN SECTION.
       01 SS-CLS.
           05 SS-FILLER.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
               10 LINE WS-NUML COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-CABECALHO.
               10 LINE 01 COLUMN 02 PIC X(80) FROM WS-MODULO
                  HIGHLIGHT FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-STATUS.
               10 LINE WS-NUML COLUMN 2 ERASE EOL PIC X(30)
                  FROM WS-STATUS HIGHLIGHT
                  FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.

           05 SS-TELA-PROCESSO.
              10 LINE 11 COLUMN 10 VALUE
                 "PROCESSANDO CLIENTE  : ".
              10 COLUMN PLUS 2 PIC X(07) USING CLI-COD.
              10 LINE 11 COLUMN 45 VALUE
                 "PROCESSANDO VENDEDOR : ".
              10 COLUMN PLUS 2 PIC X(03) USING VEN-COD.
              10 LINE 11 COLUMN 45 VALUE
                 "CALCULANDO DISTANCIA : ".
              10 COLUMN PLUS 2 PIC BB.BB9,9999 USING LK-DISTANCIA.
              10 SS-PRINT.
              12 LINE 17 COLUMN 13 VALUE
                 "LENDO  CLIENTE NUMERO  : ".
              12 COLUMN PLUS 2 PIC 9(03) USING WS-READ-C.
              12 LINE 18 COLUMN 13 VALUE
                 "LENDO  VENDEDOR NUMERO : ".
              12 COLUMN PLUS 2 PIC 9(03) USING WS-READ-V.
              12 LINE 19 COLUMN 13 VALUE
                 "GRAVANDO  REGISTRO     : ".
              12 COLUMN PLUS 2 PIC 9(03) USING WS-PRT.

       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.

      *---------------------------------------------------------------
       PROCEDURE DIVISION.
      *---------------------------------------------------------------
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT  WS-NUML FROM LINES
           ACCEPT  WS-NUMC FROM COLUMNS
           DISPLAY SS-CLS

           PERFORM ABRIR-ARQUIVO
           PERFORM CALCULA-DISTANCIA
           PERFORM GERA-PLANILHA
           PERFORM FINALIZA
           .
       FIM-PROCESSO.
           EXIT.

      *-----------------------------------------------------------*
       CALCULA-DISTANCIA.
      *-----------------------------------------------------------*

           PERFORM LER-CLIENTE

           PERFORM UNTIL FSC-FIM-REG
                   PERFORM LER-VENDEDOR UNTIL FSV-FIM-REG
                   PERFORM LER-CLIENTE
           END-PERFORM
           .
       CALCULA-DISTANCIA-FIM.
           EXIT.

      *-----------------------------------------------------------*
       LER-CLIENTE.
      *-----------------------------------------------------------*

           READ CLIENTES NEXT AT END
                PERFORM GERA-PLANILHA
           END-READ

           IF  FSC-OK
               ADD 01            TO WS-READ-C
               MOVE CLI-LATIT    TO LK-LATITUDE-C
               MOVE CLI-LONGI    TO LK-LONGITUDE-C
           ELSE
               IF CLI-STAT NOT = 10
                  MOVE 'ERRO LEITURA CLIENTE ' TO WS-MSGERRO
                  PERFORM MOSTRA-ERRO
                  PERFORM FINALIZA
               END-IF
           END-IF
           .

       LER-CLIENTE-FIM.
           EXIT.

      *-----------------------------------------------------------*
       LER-VENDEDOR.
      *-----------------------------------------------------------*

           READ VENDEDOR NEXT AT END
                PERFORM ATUALIZA-DADOS
           END-READ

           IF  (NOT FSV-OK) AND (NOT FSV-FIM-REG)
                MOVE 'ERRO LEITURA VENDEDOR ' TO WS-MSGERRO
                PERFORM MOSTRA-ERRO
                PERFORM FINALIZA
           END-IF

           IF FSV-OK
              ADD 01 TO WS-READ-V
              MOVE VEN-LATIT    TO LK-LATITUDE-V
              MOVE VEN-LONGI    TO LK-LONGITUDE-V
              CALL "PROG08" USING LK-PARAMETROS END-CALL

              IF LK-STATUS-RET NOT = '0'
                 PERFORM MOSTRA-ERRO
              ELSE
                 PERFORM VERIFICA-DISTANCIA
              END-IF
           END-IF
           .
       LER-VENDEDOR-FIM.
           EXIT.
      *-----------------------------------------------------------*
       VERIFICA-DISTANCIA.
      *-----------------------------------------------------------*

           IF WS-READ-V = 01
              MOVE CLI-COD         TO DIS-CODC
              MOVE VEN-COD         TO DIS-CODV
              MOVE LK-DISTANCIA    TO DIS-METROS
           ELSE
              IF LK-DISTANCIA < DIS-METROS
                 MOVE LK-DISTANCIA TO DIS-METROS
              END-IF
           END-IF
        .

       VERIFICA-DISTANCIA-FIM.
           EXIT.

      *-----------------------------------------------------------*
       ATUALIZA-DADOS.

           MOVE VEN-COD            TO CLI-VEND
           REWRITE CLIENTES-CLI END-REWRITE

           IF NOT FSC-OK AND NOT FSC-DUPL
                  MOVE 'ERRO ATUALIZACAO CLIENTE ' TO WS-MSGERRO
                  PERFORM MOSTRA-ERRO
                  PERFORM FINALIZA
               END-IF

           WRITE DISTRIB-DIS END-WRITE

           IF NOT FSD-OK
              PERFORM MOSTRA-ERRO
           END-IF

           MOVE ZEROS      TO WS-READ-V
           .
       ATUALIZA-DADOS-FIM.
           EXIT.

      *-----------------------------------------------------------*
       GERA-PLANILHA.
      *-----------------------------------------------------------*
           CLOSE CLIENTES  OPEN INPUT CLIENTES
           CLOSE VENDEDOR  OPEN INPUT VENDEDOR
           CLOSE DISTRIB   OPEN INPUT DISTRIB

           PERFORM IMP-TITULO

           PERFORM UNTIL FSD-FIM-REG
                   READ DISTRIB NEXT END-READ
                   MOVE DIS-CODC    TO CLI-COD
                   READ CLIENTES INVALID KEY
                        PERFORM MOSTRA-ERRO
                        PERFORM FINALIZA
                   END-READ

                   MOVE DIS-CODV    TO VEN-COD
                   READ VENDEDOR INVALID KEY
                        MOVE 'ERRO LEITURA VENDEDOR '  TO WS-MSGERRO
                        PERFORM MOSTRA-ERRO
                        PERFORM FINALIZA
                   END-READ

                   MOVE CLI-COD    TO CSV-CODC
                   MOVE CLI-NOME   TO CSV-RAZS
                   MOVE CLI-COD    TO CSV-CODC
                   MOVE CLI-NOME   TO CSV-RAZS
                   MOVE DIS-METROS TO CSV-DISTANCIA
                   PERFORM GRAVA-CSV
           END-PERFORM
           .
       GERA-PLANILHA-FIM.
           EXIT.

      *-----------------------------------------------------------*
       IMP-TITULO.
      *-----------------------------------------------------------*
           WRITE PLANILHA-CSV    FROM LINHA-TITULO1
           WRITE PLANILHA-CSV    FROM LINHA-TITULO2
           .
       IMP-TITULO-FIM.
           EXIT.

      *-----------------------------------------------------------*
       GRAVA-CSV.
      *-----------------------------------------------------------*

           ADD   01             TO WS-PRT
           MOVE  CLI-COD        TO CSV-CODC
           MOVE  CLI-NOME       TO CSV-RAZS
           MOVE  VEN-COD        TO CSV-CODV
           MOVE  VEN-NOME       TO CSV-NOMEV
           MOVE  DIS-METROS     TO CSV-DISTANCIA
           WRITE PLANILHA-CSV      FROM LINHA-DET AFTER 1
               .
       GRAVA-CSV-FIM.

      *-----------------------------------------------------------*
       ABRIR-ARQUIVO.
      *-----------------------------------------------------------*
           OPEN I-O CLIENTES

           IF FSC-NAO-EXISTE
              MOVE 'ARQUIVO CLIENTES NAO ENCONTRADO' TO WS-MSGERRO
              PERFORM MOSTRA-ERRO
              PERFORM FINALIZA
           END-IF

           OPEN INPUT VENDEDOR

           IF FSV-NAO-EXISTE
              MOVE 'ARQUIVO VENDEDORES NAO ENCONTRADO' TO WS-MSGERRO
              PERFORM MOSTRA-ERRO
              PERFORM FINALIZA
           END-IF

           OPEN OUTPUT DISTRIB

           OPEN OUTPUT PLANILHA
           .
       ABRIR-ARQUIVO-FIM.
           EXIT.

      *-----------------------------------------------------------*
       MOSTRA-ERRO.
      *-----------------------------------------------------------*
           DISPLAY SS-ERRO
           ACCEPT SS-ERRO
           DISPLAY SS-STATUS
           .
       MOSTRA-ERRO-FIM.
           EXIT.

      *-----------------------------------------------------------*
        FINALIZA.
      *-----------------------------------------------------------*
           CLOSE CLIENTES VENDEDOR DISTRIB PLANILHA
           GOBACK.
