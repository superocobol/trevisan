      ******************************************************************
      * Autor.....: Alexandre Trevisani (PROVA COBOL)
      * Data......: Julho/2019
      * Programa..: PROG02 - Relação do Cadastro de Clientes
      * Parametros: Opção ordem Ascendente/Decrecente
      *             Opção Classificação por Código ou Razão Social
      *             Filtro por Código ou Razão Social
      * Observação: Relatório Gerado em um Arquivo Sequencial
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PROG02.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.

       SELECT CLIENTES ASSIGN TO "C:\COBOL\CLIENTES.ARQ"
              ORGANIZATION IS INDEXED
              ACCESS  MODE IS SEQUENTIAL
              FILE STATUS IS CLI-STAT
              ALTERNATE RECORD KEY IS CLI-COD
              ALTERNATE RECORD KEY IS CLI-NOME  WITH DUPLICATES
              LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS
              RECORD KEY IS CLI-KEY.

       SELECT REL-CLI ASSIGN TO "C:\COBOL\REL-CLI.TXT"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS REL-STAT.

       DATA DIVISION.
       FILE SECTION.

       COPY FD_CLIENTES.

       FD REL-CLI.

       01 REL-CLI-REL.
          05 REL-IMP        PIC X(120).

       WORKING-STORAGE SECTION.

       01 WS-MODULO.
           05 FILLER        PIC X(30) VALUE
              "PROVA COBOL - REL. CLIENTES".
           05 FILLER        PIC X(12) VALUE "MENSAGEM :".
           05 WS-MENSAG     PIC X(40) VALUE SPACES.

       77 CLI-STAT          PIC 9(02).
           88 FS-OK         VALUE ZEROS.
           88 FS-FIM-REG    VALUE 10.
           88 FS-NAO-EXISTE VALUE 35.

       77 TECLA-DIG         PIC 9(02).
           88 WS-CANCELA    VALUE 25.

       77 REL-STAT          PIC 9(02).
           88 FSR-OK        VALUE ZEROS.

       77 WS-ERRO           PIC X.
           88 ERRO-SIM      VALUES ARE "S".
           88 ERRO-NAO      VALUES ARE "N".

       77 WS-NUML           PIC 999.
       77 WS-NUMC           PIC 999.
       77 COR-FUNDO         PIC 9 VALUE 1.
       77 COR-FRENTE        PIC 9 VALUE 6.

       77 WS-STATUS         PIC X(40).
       77 WS-MSGERRO        PIC X(80).
       77 WS-READ           PIC 9(03) VALUE ZEROS.
       77 WS-PRT            PIC 9(03) VALUE ZEROS.
       77 WS-ORDEM          PIC X     VALUE SPACES.
       77 WS-CLASS          PIC X     VALUE SPACES.
       77 WS-VEND           PIC 9(03) VALUE ZEROS.
       77 WS-CODI           PIC 9(07) VALUE ZEROS.
       77 WS-NOME           PIC X(30) VALUE SPACES.
       77 WS-CTLIN          PIC 9(02) VALUE 60.
       77 WS-CTPAG          PIC 9(03) VALUE ZEROS.
       77 WS-CONTADOR       PIC 9(02) VALUE ZEROS.
       77 WS-MAXIMO         PIC 9(02) VALUE ZEROS.
       77 WS-IDX1           PIC 9(02) VALUE ZEROS.
       77 WS-CTD            PIC 9(01) VALUE ZEROS.
       77 WS-FIM-PESQ       PIC X(01) VALUE SPACES.
       77 WS-SEL            PIC X(01) VALUE SPACES.

       01 LINHA-CAB0.
           05 FILLER        PIC X(030) VALUE
           'PROVA DE COBOL '.
           05 FILLER        PIC X(066) VALUE
           '                 RELACAO CADASTRO DE CLIENTES'.
           05 FILLER        PIC X(09) VALUE 'PAGINA : '.
           05 PAG-REL       PIC ZZ9.

       01 LINHA-CAB1.
           05 FILLER        PIC X(014)  VALUE 'CODIGO'.
           05 FILLER        PIC X(016)  VALUE 'CNPJ'.
           05 FILLER        PIC X(042)  VALUE 'RAZAO SOCIAL'.
           05 FILLER        PIC X(016)  VALUE 'LATITUDE'.
           05 FILLER        PIC X(012)  VALUE 'LONGITUDE'.
           05 FILLER        PIC X(008)  VALUE 'VENDEDOR'.

       01 LINHA-CAB2.
           05 FILLER        PIC X(014)  VALUE '======'.
           05 FILLER        PIC X(016)  VALUE '===='.
           05 FILLER        PIC X(042)  VALUE '==========='.
           05 FILLER        PIC X(016)  VALUE '========'.
           05 FILLER        PIC X(012)  VALUE '========='.
           05 FILLER        PIC X(008)  VALUE '========'.

       01 LINHA-DET.
           05 REL-COD       PIC X(007)  VALUE SPACES.
           05 FILLER        PIC X(002)  VALUE SPACES.
           05 REL-CNPJ.
              10 CNPJ-01    PIC 9(002).
              10 FILLER     PIC X       VALUE '.'.
              10 CNPJ-02    PIC 9(003)  VALUE ZEROS.
              10 FILLER     PIC X       VALUE '.'.
              10 CNPJ-03    PIC 9(003)  VALUE ZEROS.
              10 FILLER     PIC X       VALUE '/'.
              10 CNPJ-04    PIC 9(004)  VALUE ZEROS.
              10 FILLER     PIC X       VALUE '-'.
              10 CNPJ-05    PIC 9(002)  VALUE ZEROS.
           05 FILLER        PIC X(002)  VALUE SPACES.
           05 REL-NOME      PIC X(040).
           05 FILLER        PIC X(002)  VALUE SPACES.
           05 REL-LATIT     PIC -999,99999999.
           05 FILLER        PIC X(002)  VALUE SPACES.
           05 REL-LONGI     PIC -999,99999999.
           05 FILLER        PIC X(004)  VALUE SPACES.
           05 REL-VEND      PIC 9(003)  VALUE ZEROS.

       01 LINHA-TOT.
           05 FILLER        PIC X(14) VALUE 'TOTAL LIDOS :'.
           05 TOT-LIDOS     PIC 999.
           05 FILLER        PIC X(05) VALUE SPACES.
           05 FILLER        PIC X(21) VALUE 'TOTAL SELECIONADOS :'.
           05 TOT-PRT       PIC 999.

       01 LINHA-BRANCO      PIC X(100) VALUE SPACES.

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

           05 SS-TELA-FILTRO.
              10 LINE 11 COLUMN 13 VALUE
                 "ORDEM ASCENDENTE OU DECRECENTE 'A' OU 'D'       : ".
              10 COLUMN PLUS 2 PIC X(01) USING WS-ORDEM.
              10 LINE 12 COLUMN 13 VALUE
                 "CLASSIFICACAO: CODIGO / RAZAO SOCIAL 'C' OU 'R' : ".
              10 COLUMN PLUS 2 PIC X(01) USING WS-CLASS.
              10 LINE 13 COLUMN 13 VALUE
                 "RAZAO SOCIAL                                    : ".
              10 COLUMN PLUS 2 PIC X(40) USING WS-NOME.
              10 LINE 14 COLUMN 13 VALUE
                 "CODIGO CLIENTE                                  : ".
              10 COLUMN PLUS 2 PIC 9(07) USING WS-CODI.
              10 LINE 15 COLUMN 13 VALUE
                 "CODIGO DO VENDEDOR                              : ".
              10 COLUMN PLUS 2 PIC 9(03) USING WS-VEND.
              10 SS-PRINT.
              12 LINE 17 COLUMN 13 VALUE
                 "LENDO  REGISTRO NUMERO                          : ".
              12 COLUMN PLUS 2 PIC 9(03) USING WS-READ.
              12 LINE 18 COLUMN 13 VALUE
                 "CODIGO DO CLIENTE                               : ".
              12 COLUMN PLUS 2 PIC 9(03) USING CLI-COD.
              12 LINE 19 COLUMN 13 VALUE
                 "IMPRIMINDO  REGISTRO                            : ".
              12 COLUMN PLUS 2 PIC 9(03) USING WS-PRT.

       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 6 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.

       PROCEDURE DIVISION.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT WS-NUML FROM LINES
           ACCEPT WS-NUMC FROM COLUMNS
           PERFORM ABRIR-ARQUIVO
           .
       DIG-OPCAO.
           DISPLAY SS-CLS
           MOVE ZEROS TO WS-CTD WS-READ WS-PRT
           MOVE 'S'   TO WS-SEL
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS
           DISPLAY SS-STATUS
           ACCEPT  SS-TELA-FILTRO

           IF WS-ORDEM NOT = 'A' AND WS-ORDEM NOT = 'D'
              MOVE 'DIGITE "A" OU "D" NA ORDEM'           TO WS-MSGERRO
              PERFORM MOSTRA-ERRO
              MOVE 'N'  TO WS-SEL
           END-IF

           IF WS-CLASS NOT = 'C' AND WS-CLASS NOT = 'R'
              MOVE 'DIGITE "C" OU "R" NA CLASSIFICACAO'   TO WS-MSGERRO
              PERFORM MOSTRA-ERRO
              MOVE 'N'  TO WS-SEL
           END-IF

           IF WS-NOME NOT = SPACES ADD 1 TO WS-CTD END-IF
           IF WS-CODI NOT = ZEROS  ADD 1 TO WS-CTD END-IF
           IF WS-VEND NOT = ZEROS  ADD 1 TO WS-CTD END-IF

           IF WS-CTD > 1
              MOVE 'SELECIONE APENAS 1 FILTRO '           TO WS-MSGERRO
              PERFORM MOSTRA-ERRO
              MOVE 'N'  TO WS-SEL
           END-IF

           IF WS-SEL = 'N'  GO DIG-OPCAO END-IF

           IF WS-NOME NOT EQUAL SPACES
              INSPECT WS-NOME TALLYING WS-CONTADOR FOR ALL ' '
              COMPUTE WS-CONTADOR = (40 - WS-CONTADOR)
              COMPUTE WS-MAXIMO   = (40 - WS-CONTADOR)
           END-IF

           IF WS-ORDEM = 'A'
              IF WS-CLASS = 'C'
                 MOVE LOW-VALUES TO CLIENTES-CLI
                 START CLIENTES KEY > CLI-COD
                       INVALID KEY
                       DISPLAY 'NAO ENCONTREI REGISTROS' AT 2050
                 END-START
              ELSE
                 MOVE SPACES  TO  CLIENTES-CLI
                 START CLIENTES KEY > CLI-NOME
                       INVALID KEY
                       DISPLAY 'NAO ENCONTREI REGISTROS' AT 2050
                 END-START
              END-IF
           ELSE
              IF WS-CLASS = 'C'
                 MOVE HIGH-VALUES TO CLIENTES-CLI
                 START CLIENTES KEY <= CLI-COD
                       INVALID KEY
                       DISPLAY 'NAO ENCONTREI REGISTROS' AT 2050
                 END-START
              ELSE
                 MOVE HIGH-VALUES TO CLIENTES-CLI
                 START CLIENTES KEY < CLI-NOME
                       INVALID KEY
                       DISPLAY 'NAO ENCONTREI REGISTROS' AT 2050
                 END-START
              END-IF
           END-IF

           PERFORM PROCESSA UNTIL FS-FIM-REG

           PERFORM FINALIZA
           .
       FIM-PROCESSO.
           EXIT.

      *-----------------------------------------------------------*
       PROCESSA.
      *-----------------------------------------------------------*F

           IF WS-ORDEM = 'A'
                 READ CLIENTES NEXT AT END
                      PERFORM TOTAIS
                      PERFORM FINALIZA
                 END-READ
           ELSE
                 READ CLIENTES PREVIOUS AT END
                      PERFORM TOTAIS
                      PERFORM FINALIZA
                 END-READ
           END-IF

           ADD 01 TO WS-READ

           SET ERRO-NAO        TO TRUE

           IF WS-CODI NOT EQUAL ZEROS AND
              WS-CODI NOT EQUAL CLI-COD
              SET ERRO-SIM      TO TRUE
           END-IF

           IF WS-VEND NOT EQUAL ZEROS AND
              WS-VEND NOT EQUAL CLI-VEND
              SET ERRO-SIM      TO TRUE
           END-IF

           IF WS-NOME NOT EQUAL SPACES
              PERFORM VERIFICA-NOME
           END-IF

           IF ERRO-NAO
              IF WS-CTLIN > 55
                 PERFORM IMP-CABEC
                 PERFORM IMP-DETALHE
              ELSE
                 PERFORM IMP-DETALHE
              END-IF
           END-IF

           DISPLAY SS-PRINT
           .
       PROCESSA-FIM.
           EXIT.

      *-----------------------------------------------------------*
       IMP-CABEC.
      *-----------------------------------------------------------*
           ADD  01              TO WS-CTPAG
           MOVE WS-CTPAG        TO PAG-REL
           MOVE 05              TO WS-CTLIN
           WRITE REL-CLI-REL    FROM LINHA-CAB0
           WRITE REL-CLI-REL    FROM LINHA-BRANCO
           WRITE REL-CLI-REL    FROM LINHA-CAB1
           WRITE REL-CLI-REL    FROM LINHA-CAB2
           .
       IMP-CABEC-FIM.
           EXIT.

      *-----------------------------------------------------------*
       IMP-DETALHE.
      *-----------------------------------------------------------*
           ADD  01             TO WS-PRT
           ADD  01             TO WS-CTLIN
           MOVE CLI-COD        TO REL-COD
           MOVE CLI-CNPJ(1:2)  TO CNPJ-01
           MOVE CLI-CNPJ(3:3)  TO CNPJ-02
           MOVE CLI-CNPJ(6:3)  TO CNPJ-03
           MOVE CLI-CNPJ(9:4)  TO CNPJ-04
           MOVE CLI-CNPJ(13:2) TO CNPJ-05
           MOVE CLI-NOME       TO REL-NOME
           MOVE CLI-LATIT      TO REL-LATIT
           MOVE CLI-LONGI      TO REL-LONGI
           MOVE CLI-VEND       TO REL-VEND
           WRITE REL-CLI-REL FROM LINHA-DET AFTER 1
               .
       IMP-DETALHE-FIM.

      *-----------------------------------------------------------*
       ABRIR-ARQUIVO.
      *-----------------------------------------------------------*
           OPEN INPUT CLIENTES

           IF FS-NAO-EXISTE
              MOVE 'ARQUIVO CLIENTES NAO ENCONTRADO' TO WS-MSGERRO
              PERFORM MOSTRA-ERRO
              PERFORM FINALIZA
           END-IF

           OPEN OUTPUT REL-CLI
           .
       ABRIR-ARQUIVO-FIM.
           EXIT.

      *-----------------------------------------------------------*
       TOTAIS.
      *-----------------------------------------------------------*
           MOVE WS-READ         TO TOT-LIDOS
           MOVE WS-PRT          TO TOT-PRT
           WRITE REL-CLI-REL    FROM LINHA-BRANCO AFTER 1
           WRITE REL-CLI-REL    FROM LINHA-TOT AFTER 1
           .
       TOTAIS-FIM.
           EXIT.

      *-----------------------------------------------------------*
       VERIFICA-NOME.
      *-----------------------------------------------------------*

           MOVE 01  TO WS-IDX1
           MOVE 'N'    TO WS-FIM-PESQ
           SET ERRO-SIM   TO TRUE

           PERFORM UNTIL WS-FIM-PESQ EQUAL 'S'
                   IF CLI-NOME(WS-IDX1:WS-CONTADOR) = WS-NOME
                      MOVE 'S'  TO WS-FIM-PESQ
                      SET ERRO-NAO TO TRUE
                   ELSE
                   ADD 01 TO WS-IDX1
                       IF WS-IDX1 = WS-MAXIMO
                         MOVE 'S'  TO WS-FIM-PESQ
                       END-IF
                   END-IF
           END-PERFORM
           .

       VERIFICA-NOME-FIM.
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
           CLOSE CLIENTES REL-CLI
           GOBACK.
