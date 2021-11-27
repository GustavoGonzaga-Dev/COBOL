       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR.
      *AUTOR. GUSTAVO GONZAGA DE FARIAS.
      **************************************
      *        CADASTRO DE VEICULOS        *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.

      *
      *-----------------------------------------------------------------
       DATA DIVISION.
 
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 PLACA         PIC X(07) VALUE SPACES.
       01 PROPRIETARIO  PIC X(30) VALUE SPACES.
       01 MARCA         PIC X(01) VALUE SPACES.
       01 ANOFABRICACAO PIC 9(04) VALUE ZEROS.
       01 SITUACAO      PIC X(01) VALUE SPACES.	  
	   01 TPMARCA       PIC 9(01) VALUE ZEROS.
	   01 TEXTPMARCA    PIC X(10) VALUE SPACES.
	   01 AUX           PIC 9(02) VALUE ZEROS.
	   
	   01 TABMARCA.
          03 TBMARCA    PIC X(10) OCCURS 9 TIMES.

       01 TABSITUACAO1.
          03 FILLER     PIC X(15) VALUE "MMANUTENCAO".
          03 FILLER     PIC X(15) VALUE "BBATIDO".
          03 FILLER     PIC X(15) VALUE "SSUCATA".
          03 FILLER     PIC X(15) VALUE "OORIGINAL".
          03 FILLER     PIC X(15) VALUE "NNORMAL".
      *
       01 TABSITUACAO REDEFINES TABSITUACAO1.
          03 TBSITUACAO   PIC X(15) OCCURS 5 TIMES.
      *
       01 TXTSITUACAO.
          03 TXTSITUA1 PIC X(01) VALUE SPACES.
          03 TXTSITUA2 PIC X(14) VALUE SPACES. 
		  
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  " CADASTRO DE VEICULOS".
           05  LINE 05  COLUMN 01 
               VALUE  "      PLACA          :".
           05  LINE 07  COLUMN 01 
               VALUE  "      PROPRIETARIO   :".
           05  LINE 09  COLUMN 01 
               VALUE  "      ANO FABRICACAO :".
           05  LINE 11  COLUMN 01 
               VALUE  "      SITUACAO       :".
           05  LINE 13  COLUMN 01 
               VALUE  "      MARCA          :".
                  
      *-----------------------------------------------------------------	
       PROCEDURE DIVISION.
       INICIO.
           MOVE "FORD"       TO TBMARCA(1)
           MOVE "GM "        TO TBMARCA(2)
           MOVE "FIAT"       TO TBMARCA(3)
           MOVE "TOYOTA"     TO TBMARCA(4)
           MOVE "HONDA"      TO TBMARCA(5)
           MOVE "NISSAN"     TO TBMARCA(6)
           MOVE "CHEVROLET"  TO TBMARCA(7)
           MOVE "KIA"        TO TBMARCA(8)
           MOVE "VOLKSWAGEN" TO TBMARCA(9).
      *
       R1.
           DISPLAY TELA.
           ACCEPT (05, 23) PLACA
           ACCEPT (07, 23) PROPRIETARIO
           ACCEPT (09, 23) ANOFABRICACAO.
       R2.
           MOVE 1 TO AUX
           ACCEPT (11, 23) SITUACAO.
       R2A.
           MOVE TBSITUACAO(AUX) TO TXTSITUACAO
           IF TXTSITUA1 NOT = SITUACAO
              ADD 1 TO AUX
              IF AUX < 10
                 GO TO R2A
              ELSE
                 DISPLAY (23, 14) "TIPO DE SITUACAO INCORRETA"
                 STOP " PRESSIONE ENTER PARA TENTAR NOVAMENTE"
                 GO TO R2 
           ELSE
               DISPLAY (11, 25) TXTSITUA2.
		
       R3.
		   ACCEPT (13, 23) TPMARCA.
           IF TPMARCA = 0 
                 DISPLAY (23, 14) "APENAS NUMEROS DE 1 A 9 "
                 STOP "  PRESSIONE ENTER PARA TENTAR NOVAMENTE"
                 GO TO R3.
           MOVE TBMARCA(TPMARCA) TO TEXTPMARCA
           DISPLAY (13, 25) TEXTPMARCA.
           STOP RUN.