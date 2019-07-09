      ******************************************************************
      * Autor.....: Alexandre Trevisani (PROVA COBOL)
      * Data......: Julho/2019
      * Programa..: PROG06 - Importação de Dados de VENDEDORES
      * Parametros: Leitura de um Arquivo.TXT informado pelo usuário
      * Observação: No processo é gerado um relatório (ERROIMPV.TXT) com
      *             erros ocorridos, identificados por um "*" na frente
      *             do campo ou "D" na frente do Código indicando que
      *             o VENDEDOR já existe. (Duplicidade)
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PROG06.
       ENVIRONMENT         DIVISION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.

       SELECT VENDEDOR ASSIGN TO "C:\COBOL\VENDEDOR.ARQ"
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS RANDOM
              FILE STATUS  IS VEN-STAT
              RECORD KEY   IS VEN-KEY.

       SELECT IMPORTV ASSIGN TO WS-ARQUIVO
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE  IS SEQUENTIAL
              FILE STATUS  IS IMP-STAT.

       SELECT ERROIMPV ASSIGN TO "C:\COBOL\ERROIMPV.TXT"
              ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS REL-STAT.

       DATA DIVISION.
       FILE SECTION.

       COPY FD_VENDEDOR.


       FD IMPORTV.

       01 IMPORTV-IMP.
           05 IMP-COD       PIC X(003).
           05 IMP-CPF       PIC X(011).
           05 IMP-NOME      PIC X(040).
           05 IMP-LATIT     PIC X(013).
           05 IMP-LONGI     PIC X(013).
           05 FILLER        PIC X(003).

       FD ERROIMPV.

       01 ERROIMPV-REL.
          05 REL-IMP        PIC X(100).

       WORKING-STORAGE SECTION.

       01 WS-MODULO.
           05 FILLER        PIC X(30) VALUE
              "PROVA COBOL - IMP. VENDEDORES".
           05 FILLER        PIC X(12) VALUE "MENSAGEM :".
           05 WS-MENSAG     PIC X(40) VALUE SPACES.

       77 WS-OPCAO          PIC X  VALUE SPACES.
       77 WS-ARQUIVO        PIC X(20) VALUE "C:\COBOL\IMPVEN.TXT".

       77 VEN-STAT          PIC 9(02).
           88 FS-OK         VALUE ZEROS.
           88 FS-FIM-REG    VALUE 10.
           88 FS-NAO-EXISTE VALUE 35.
           88 FS-CANCELA    VALUE 99.

       77 IMP-STAT           PIC 9(02).
           88 FSI-OK         VALUE ZEROS.
           88 FSI-FIM-REG    VALUE 10.
           88 FSI-NAO-EXISTE VALUE 35.

       77 REL-STAT           PIC 9(02).
           88 FSR-OK         VALUE ZEROS.

       77 WS-ERRO           PIC X.
           88 E-SIM         VALUE "S".
           88 E-NAO         VALUE "N".

       77 WS-NUML           PIC 999.
       77 WS-NUMC           PIC 999.
       77 COR-FUNDO         PIC 9 VALUE 1.
       77 COR-FRENTE        PIC 9 VALUE 6.

       77 WS-STATUS         PIC X(40).
       77 WS-MSGERRO        PIC X(80).
       77 WS-READ           PIC 9(03) VALUE ZEROS.
       77 WS-WRITE          PIC 9(03) VALUE ZEROS.
       77 WS-PRT            PIC 9(03) VALUE ZEROS.
       77 WS-ERRO-COD       PIC 9(03) VALUE ZEROS.
       77 WS-ERRO-CPF       PIC 9(03) VALUE ZEROS.
       77 WS-ERRO-CORD      PIC 9(03) VALUE ZEROS.
       77 WS-ERRO-NOME      PIC 9(03) VALUE ZEROS.
       77 WS-ERRO-DUPL      PIC 9(03) VALUE ZEROS.

       01 LINHA-CAB0.
           05 FILLER        PIC X(020) VALUE SPACES.
           05 FILLER        PIC X(080) VALUE
           '                RELACAO ERROS NA IMPORTACAO VENDEDORES'.

       01 LINHA-CAB1.
           05 FILLER        PIC X(010)  VALUE 'CODIGO'.
           05 FILLER        PIC X(017)  VALUE 'CPF'.
           05 FILLER        PIC X(043)  VALUE 'RAZAO SOCIAL'.
           05 FILLER        PIC X(016)  VALUE 'LATITUDE'.
           05 FILLER        PIC X(016)  VALUE 'LONGITUDE'.

       01 LINHA-DET.
           05 REL-COD       PIC 9(003).
           05 COD-REL       PIC X(001).
           05 FILLER        PIC X(002)  VALUE SPACES.
           05 REL-CPF       PIC 9(011).
           05 CPF-REL       PIC X(001).
           05 FILLER        PIC X(002)  VALUE SPACES.
           05 REL-NOME      PIC X(040).
           05 NOME-REL      PIC X(001).
           05 FILLER        PIC X(002)  VALUE SPACES.
           05 REL-LATIT     PIC X(13).
           05 LATIT-REL     PIC X(001).
           05 FILLER        PIC X(002)  VALUE SPACES.
           05 REL-LONGI     PIC X(13).
           05 LONGI-REL     PIC X(001).

       01 LINHA-BRANCO      PIC X(87) VALUE SPACES.

       COPY screenio.

       COPY LK_CNPJCPF.

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

       01 SS-FUNCAO.
           05 SS-ARQUIVO.
               10 LINE 08 COLUMN 12 VALUE
                  "INFORME O ARQUIVO A IMPORTAR : ".
               10 LINE 08 COL PLUS 1 USING WS-ARQUIVO.

       01 SS-TELA-REGISTRO.
           05 LINE 11 COLUMN 13 VALUE "LENDO         : ".
           05 COLUMN PLUS 2 PIC 9(03) USING WS-READ.
           05 LINE 12 COLUMN 13 VALUE "GRAVANDO      : ".
           05 COLUMN PLUS 2 PIC 9(03) USING WS-WRITE.
           05 LINE 13 COLUMN 13 VALUE "ERRO-COD      : ".
           05 COLUMN PLUS 2 PIC 9(03) USING WS-ERRO-COD.
           05 LINE 14 COLUMN 13 VALUE "ERRO-CPF     : ".
           05 COLUMN PLUS 2 PIC 9(03) USING WS-ERRO-CPF.
           05 LINE 15 COLUMN 13 VALUE "WS-ERRO-NOME  : ".
           05 COLUMN PLUS 2 PIC 9(03) USING WS-ERRO-NOME.
           05 LINE 16 COLUMN 13 VALUE "ERRO-CORD     : ".
           05 COLUMN PLUS 2 PIC 9(03) USING WS-ERRO-CORD.
           05 LINE 17 COLUMN 13 VALUE "WS-ERRO-DUPL  : ".
           05 COLUMN PLUS 2 PIC 9(03) USING WS-ERRO-DUPL.
           05 LINE 18 COLUMN 13 VALUE "IMPRIMINDO    : ".
           05 COLUMN PLUS 2 PIC 9(03) USING WS-PRT.


       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.

       PROCEDURE DIVISION.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT  WS-NUML FROM LINES
           ACCEPT  WS-NUMC FROM COLUMNS
           DISPLAY SS-CLS
           DISPLAY SS-FUNCAO
           ACCEPT  SS-ARQUIVO
           PERFORM ABRIR-ARQUIVO
           DISPLAY SS-TELA-REGISTRO
           WRITE ERROIMPV-REL FROM LINHA-CAB0
           WRITE ERROIMPV-REL FROM LINHA-CAB1
           WRITE ERROIMPV-REL FROM LINHA-BRANCO AFTER 1

           PERFORM PROCESSA UNTIL FSI-FIM-REG

           PERFORM FINALIZA
           .

      *----------------------------------------------------------------
       PROCESSA.
      *----------------------------------------------------------------
           READ IMPORTV NEXT AT END
                  PERFORM FINALIZA
           END-READ

           ADD 01 TO WS-READ

           DISPLAY IMP-COD   AT 1150
           DISPLAY IMP-CPF   AT 1250
           DISPLAY IMP-NOME  AT 1350
           DISPLAY IMP-LATIT AT 1450
           DISPLAY IMP-LONGI AT 1550

           SET E-NAO         TO TRUE
           INITIALIZE        LINHA-DET

           IF IMP-COD = ZEROS OR IMP-COD NOT NUMERIC
              ADD 01         TO WS-ERRO-COD
              MOVE '*'       TO COD-REL
              SET E-SIM      TO TRUE
           END-IF

            DISPLAY 'VALIDAR CPF' AT 2050
            MOVE IMP-CPF     TO LK-CPF
            MOVE 'J'         TO LK-TPC
            CALL "PROG07" END-CALL

           IF LK-RCC = 'N'
              ADD 1          TO WS-ERRO-CPF
              MOVE '*'       TO CPF-REL
              SET E-SIM      TO TRUE
           END-IF

           IF IMP-CPF = ZEROS OR IMP-CPF NOT NUMERIC
              ADD 1          TO WS-ERRO-CPF
              MOVE '*'       TO CPF-REL
              SET E-SIM      TO TRUE
           END-IF

           IF IMP-NOME = SPACES
              ADD 01         TO WS-ERRO-NOME
              MOVE '*'       TO NOME-REL
              SET E-SIM      TO TRUE
           END-IF

           IF IMP-LATIT = SPACES
              ADD 01         TO WS-ERRO-CORD
              MOVE '*'       TO LATIT-REL
              SET E-SIM      TO TRUE
           END-IF

           IF IMP-LONGI = SPACES
              ADD 01         TO WS-ERRO-CORD
              MOVE '*'       TO LONGI-REL
              SET E-SIM      TO TRUE
           END-IF

           IF E-NAO
              WRITE VENDEDOR-REG FROM IMPORTV-IMP
                    INVALID KEY
                            ADD 01    TO WS-ERRO-DUPL
                            SET E-SIM TO TRUE
                            MOVE 'D'  TO COD-REL
                    NOT INVALID KEY ADD 01 TO WS-WRITE
              END-WRITE
           END-IF

           IF E-SIM
               ADD 01 TO WS-PRT
               MOVE IMP-COD     TO REL-COD
               MOVE IMP-CPF     TO REL-CPF
               MOVE IMP-NOME    TO REL-NOME
               MOVE IMP-LATIT   TO REL-LATIT
               MOVE IMP-LONGI   TO REL-LONGI
               WRITE ERROIMPV-REL FROM LINHA-DET AFTER 1
           END-IF

           DISPLAY SS-TELA-REGISTRO

           .
       PROCESSA-FIM.

      *------------------------------------------------------------
       ABRIR-ARQUIVO.
      *----------------------------------------------------------------
           OPEN I-O VENDEDOR

           IF FS-NAO-EXISTE THEN
               OPEN OUTPUT VENDEDOR
               CLOSE VENDEDOR
               OPEN I-O VENDEDOR
           END-IF.

           OPEN INPUT IMPORTV

           IF FSI-NAO-EXISTE
              MOVE 'ARQUIVO IMPORTACAO NAO ENCONTRADO' TO WS-MSGERRO
              PERFORM MOSTRA-ERRO
             PERFORM FINALIZA
           END-IF

           OPEN OUTPUT ERROIMPV
           .

      *----------------------------------------------------------------
       MOSTRA-ERRO.
      *----------------------------------------------------------------
           DISPLAY SS-ERRO
           ACCEPT  SS-ERRO
           DISPLAY SS-STATUS
           .
        FINALIZA.
           CLOSE VENDEDOR IMPORTV ERROIMPV
           GOBACK.
