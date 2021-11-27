       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP012.
      *AUTOR."GUSTAVO GONZAGA DE FARIAS".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQCARG ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS COD
                      ALTERNATE RECORD KEY IS DENOM WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.

       DATA DIVISION.
       FILE SECTION.
       FD ARQCARG
              LABEL RECORD IS STANDARD
              VALUE OF FILE-ID IS "ARQCARG.DAT".
       01 REGCARG.
          03 COD                PIC 9(03).
          03 DENOM              PIC X(45).
          03 TPSALARIO          PIC X(01).
          03 SALARIOBASE        PIC 9(06)V99.
          03 CARGSTATUS         PIC X(01).

       WORKING-STORAGE SECTION.
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 CONLIN       PIC 9(03) VALUE 001.
       01 CODENTR      PIC 9(03) VALUE ZEROS.
       01 DENOMENTR    PIC X(45) VALUE SPACES.
       01 IND          PIC 9(02) VALUE ZEROS.
       01 INDSALARIO   PIC 9(01) VALUE ZEROS.
       01 TXTTSALARIO  PIC X(20) VALUE SPACES.

       01 TABSALARIO.
           03 TBSALARIO      PIC X(20) OCCURS 3 TIMES.

       01 TABSTATUS.
           03 FILLER PIC X(15) VALUE "ATIVA".
           03 FILLER PIC X(15) VALUE "SUSPENSA".
           03 FILLER PIC X(15) VALUE "DESATIVADA".

       01 TABSTATUSX      REDEFINES TABSTATUS.
           03 TBSTATUS    PIC X(15) OCCURS 3 TIMES.

       01 TXTSTATUS.
           03 TXTSTATUS2 PIC X(14) VALUE SPACES.

       01 DET2.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETCOD          PIC 999.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETDENOM        PIC X(20) VALUE SPACES.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETTPSALARIO    PIC X(20) VALUE SPACES.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETSALARIOBASE  PIC 9(06)V99.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETCARGSTATUS   PIC X(20) VALUE SPACES.


       SCREEN SECTION.
       01  TELACONSCARGO.
           05 BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "-------------------------|CONSULTA DE".
           05  LINE 02  COLUMN 41
               VALUE  "CARGOS|-----------------------------".
           05  LINE 04  COLUMN 01
               VALUE  "  COD |    DENOMINACAO      | TIPO SALARIO ".
           05  LINE 04  COLUMN 41
               VALUE  "  |  SALARIO BASE   | STATUS  ".
           05  LINE 22  COLUMN 01
               VALUE  "  MENSAGEM:".

           05  TCODENTR
               LINE 05  COLUMN 03  PIC 9(03)
               USING  CODENTR.
           05  TDENOMENTR
               LINE 05  COLUMN 06  PIC X(45)
               USING  DENOMENTR.

       PROCEDURE DIVISION.
       INICIO.

           MOVE "HORISTA"      TO TBSALARIO(1)
           MOVE "DIARISTA"   TO TBSALARIO(2)
           MOVE "MENSALISTA"     TO TBSALARIO(3).


       INC-OP1.
           OPEN INPUT ARQCARG
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO ARQCARG NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQCARG"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.

       INC-001.
           MOVE SPACES TO DENOMENTR
           MOVE ZEROS TO CODENTR

           DISPLAY  TELACONSCARGO.

       INC-001A.
           DISPLAY (22, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/DENOM."
           ACCEPT TCODENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQCARG
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CODENTR TO COD
           START ARQCARG KEY IS NOT LESS COD INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.

       INC-001B.
           DISPLAY (22, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA     F2=CONSULTA P/COD CARGO "
           ACCEPT TDENOMENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQCARG
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001B.
           MOVE DENOMENTR TO DENOM
           START ARQCARG KEY IS NOT LESS DENOM INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       INC-RD2.
           DISPLAY (22, 13) LIMPA
           READ ARQCARG NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQCARG"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-SAL.
       
           IF TPSALARIO = "H"
               MOVE 1 TO INDSALARIO
           ELSE
               IF TPSALARIO = "D"
                  MOVE 2 TO INDSALARIO
               ELSE
                   IF TPSALARIO = "M"
                      MOVE 3 TO INDSALARIO.

       ROT-STATUS.
       
           IF CARGSTATUS = "A"
               MOVE "ATIVA" TO TXTSTATUS
           ELSE
               IF CARGSTATUS = "S"
                  MOVE "SUSPENSA" TO TXTSTATUS
               ELSE
                   IF CARGSTATUS = "D"
                      MOVE "DESATIVADA" TO TXTSTATUS.
               

       ROT-MONTAR.
           MOVE COD  TO DETCOD 
           MOVE DENOM TO DETDENOM
           MOVE TBSALARIO(INDSALARIO) TO TXTTSALARIO   
           MOVE TXTTSALARIO  TO DETTPSALARIO
           MOVE SALARIOBASE TO DETSALARIOBASE
           MOVE TXTSTATUS2    TO DETCARGSTATUS

           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 01) DET2
           ADD 1 TO CONLIN
           IF CONLIN < 17
                  GO TO INC-RD2.

       ROT-SOL.
           DISPLAY (22, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (22, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELACONSCARGO
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.

       NOV-SOL.
           DISPLAY (22, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (22, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.

       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE ARQCARG.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.

       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (22, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (22, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.