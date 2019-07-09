      ******************************************************************
      * Autor.....: Alexandre Trevisani (PROVA COBOL)
      * Data......: Julho/2019
      * Programa..: PROG07 - Verificação da Integridade do CPF/CNPJ
      * Parametros Esperados/Devolvidos
      * LK-NUM PIC 9(15). (E) Valor numérico cpf ou cnpj
      * LK-TPC PIC X(01). (E) Tipo 'J' = juridico, 'F' = fisico
      * LK-RCC PIC X(01). (D) Retorno 'S' valido ou 'N' = Invalido
      * LK-ECC PIC X(18). (D) campo formatado cpf ou cnpj
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PROG07.
      *
       ENVIRONMENT        DIVISION.
       CONFIGURATION       SECTION.
       SPECIAL-NAMES.       DECIMAL-POINT  IS  COMMA.
      *
       DATA              DIVISION.
       WORKING-STORAGE       SECTION.
       01  WS-REG.
           03 WS-NUM       PIC 9(02).
           03 WS-TOT       PIC 9(03).
           03 I01          PIC 9(02).
      *
       01  WS-CPF          PIC 9(15).
       01  WS-CGR  REDEFINES  WS-CPF.
           03 WS-COD       PIC 9(13).
           03 WS-DIG       PIC 9(02).
      *
       01  ED-CNPJ.
           03 ED-CG1       PIC 99.999.999/9999.
           03 FILLER       PIC X(01)  VALUE '-'.
           03 ED-CG2       PIC 9(02).
      *
       01  ED-CPF.
           03 FILLER       PIC X(04)  VALUE SPACES.
           03 ED-CP1       PIC 999.999.999.
           03 FILLER       PIC X(01)  VALUE '-'.
           03 ED-CP2       PIC 9(02).
      *
       01  TABVJ1.
           03 FILLER       PIC X(13)  VALUE '0543298765432'.
       01  TABRJ1  REDEFINES  TABVJ1.
           03 TB-VJ1       PIC 9(01)  OCCURS  13.
      *
       01  TABVJ2.
           03 FILLER       PIC X(14)  VALUE '06543298765432'.
       01  TABRJ2  REDEFINES  TABVJ2.
           03 TB-VJ2       PIC 9(01)  OCCURS  14.
      *
       01  TABVF1.
           03 FILLER       PIC X(26)  VALUE
              '00000000100908070605040302'.
       01  TABRF1  REDEFINES  TABVF1.
           03 TB-VF1       PIC 9(02)  OCCURS  13.
      *
       01  TABVF2.
              03 FILLER    PIC X(28)  VALUE
                 '0000000011100908070605040302'.
       01  TABRF2  REDEFINES  TABVF2.
           03 TB-VF2       PIC 9(02)  OCCURS  14.
      *
       LINKAGE             SECTION.
       01  PARCPF.
           03 LK-CPF       PIC 9(15).
           03 LK-RD1  REDEFINES  LK-CPF.
              05 LK-CDC    PIC 9(13).
              05 LK-DG1    PIC 9(01).
              05 LK-DG2    PIC 9(01).
           03 LK-RD2  REDEFINES  LK-CPF.
              05 LK-CR1    PIC 9(01)  OCCURS  13.
              05 LK-DIG    PIC 9(02).
           03 LK-RD3  REDEFINES  LK-CPF.
              05 LK-CR2    PIC 9(01)  OCCURS  14.
              05 FILLER    PIC 9(01).
           03 LK-RD4  REDEFINES  LK-CPF.
              05 LK-CG1    PIC 9(13).
              05 LK-CG2    PIC 9(02).
           03 LK-RD5  REDEFINES  LK-CPF.
              05 LK-CP1    PIC 9(04).
              05 LK-CP2    PIC 9(09).
              05 FILLER    PIC 9(02).
           03 LK-TPC       PIC X(01).
           03 LK-RCC       PIC X(01).
           03 LK-ECC       PIC X(18).
      *
      /**************************************************************
       PROCEDURE   DIVISION   USING    PARCPF.
      ***************************************************************
       R010-INIC-PGM.

           MOVE LK-CPF  TO  WS-CPF  MOVE SPACES TO LK-ECC

           IF (LK-TPC NOT = 'F' AND  LK-TPC NOT = 'J')
           OR (LK-TPC =  'F' AND  LK-CP1  >  ZEROS)
              MOVE 'N'  TO LK-RCC
              GO  R010-EXIT-PGM
           END-IF

           MOVE ZEROS TO WS-TOT  I01

           IF LK-TPC  = 'F'  GO  R010-LOOP-VF1 END-IF
           .
       R010-LOOP-VJ1.
           ADD 01 TO I01
           IF I01 > 13       GO  R010-CALC-DJ1 END-IF
           COMPUTE WS-NUM  =  LK-CR1 (I01)  *  TB-VJ1 (I01)
           ADD     WS-NUM  TO WS-TOT  GO   R010-LOOP-VJ1
           .
       R010-CALC-DJ1.
           COMPUTE WS-NUM  = WS-TOT  /   11
           COMPUTE WS-NUM  = WS-TOT  - (WS-NUM   *   11)
           IF WS-NUM  < 02
              MOVE   ZERO TO LK-DG1
           ELSE
              COMPUTE LK-DG1  = 11  -  WS-NUM
           END-IF
           MOVE ZEROS TO WS-TOT  MOVE ZEROS TO  I01
           .
       R010-LOOP-VJ2.
           ADD 01  TO I01
           IF I01 > 14 GO R010-CALC-DJ2 END-IF
           COMPUTE WS-NUM  =        LK-CR2 (I01)  *  TB-VJ2 (I01)
           ADD     WS-NUM  TO       WS-TOT  GO       R010-LOOP-VJ2
           .
       R010-CALC-DJ2.
           COMPUTE WS-NUM  = WS-TOT  /   11.
           COMPUTE WS-NUM  = WS-TOT  -   (WS-NUM   *   11)
           IF WS-NUM  <  02
              MOVE   ZERO TO LK-DG2
           ELSE
              COMPUTE LK-DG2  =       11  -  WS-NUM
           END-IF
           GO         R010-EDIT-CPF
           .
      *
       R010-LOOP-VF1.
           ADD 01 TO I01
           IF I01 > 13
              GO       R010-CALC-DF1
           END-IF
           COMPUTE WS-NUM  = LK-CR1 (I01)  *  TB-VF1 (I01)
           ADD WS-NUM  TO WS-TOT
           GO       R010-LOOP-VF1
           .
       R010-CALC-DF1.
           COMPUTE WS-NUM  =  WS-TOT  /   11
           COMPUTE WS-NUM  =  WS-TOT  -   (WS-NUM   *   11)
           IF  WS-NUM  <  02
               MOVE   ZERO TO LK-DG1
           ELSE
               COMPUTE LK-DG1  =  11  -  WS-NUM
           END-IF
           MOVE ZEROS   TO  WS-TOT  MOVE ZEROS TO  I01
           .
       R010-LOOP-VF2.
           ADD 01 TO I01
           IF I01 > 14
              GO    R010-CALC-DF2
           END-IF
           COMPUTE WS-NUM  =   LK-CR2 (I01)  *  TB-VF2 (I01)
           ADD  WS-NUM  TO WS-TOT
           GO       R010-LOOP-VF2
           .
       R010-CALC-DF2.
           COMPUTE WS-NUM  =  WS-TOT  /   11
           COMPUTE WS-NUM  =  WS-TOT  -   (WS-NUM   *   11)
           IF  WS-NUM  <  02
               MOVE   ZERO TO LK-DG2
           ELSE
               COMPUTE LK-DG2  =  11  -  WS-NUM
           END-IF
           .
       R010-EDIT-CPF.
           IF LK-DIG  =  WS-DIG  MOVE 'S' TO LK-RCC END-IF
           IF LK-DIG NOT  =  WS-DIG  MOVE 'N' TO LK-RCC END-IF
           MOVE SPACES  TO  LK-ECC  MOVE WS-CPF TO LK-CPF
           IF LK-TPC  =  'J'
              MOVE LK-CG1  TO ED-CG1
              MOVE LK-CG2  TO ED-CG2
              MOVE ED-CPF  TO LK-ECC
           ELSE
              MOVE LK-CP2  TO ED-CP1
              MOVE LK-DIG  TO ED-CP2
              MOVE ED-CPF TO LK-ECC
           END-IF
           .
       R010-EXIT-PGM.
            GOBACK.
