       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR2D.
      *AUTHOR. ANGELO LOTIERZO FILHO.
      **************************************
      * MANUTENCAO DO CADASTRO DE AMIGOS   *
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
       01 APELIDO       PIC X(12) VALUE SPACES.
       01 NOME          PIC X(30) VALUE SPACES.
       01 EMAIL         PIC X(30) VALUE SPACES.
       01 SEXO          PIC X(01) VALUE SPACES.
       01 GENERO        PIC X(01) VALUE SPACES.
       01 TPAMIGO       PIC 9(01) VALUE ZEROS.
       01 TEXTPAMIGO    PIC X(10) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.


       01 TABAMIGO.
          03 TBAMIGO    PIC X(10) OCCURS 9 TIMES.

       01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "THETEROSEXUAL".
          03 FILLER     PIC X(15) VALUE "HHOMESEXUAL".
          03 FILLER     PIC X(15) VALUE "BBISSESUXUAL".
          03 FILLER     PIC X(15) VALUE "PPANSEXUAL".
          03 FILLER     PIC X(15) VALUE "AA           ".
          03 FILLER     PIC X(15) VALUE "BB           ".
          03 FILLER     PIC X(15) VALUE "CC           ".
          03 FILLER     PIC X(15) VALUE "DD            ".
          03 FILLER     PIC X(15) VALUE "NNAO DECLARADO".
      *
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 9 TIMES.
      *
       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES. 

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  " AMIGOS".
           05  LINE 05  COLUMN 01 
               VALUE  "      APELIDO       :".
           05  LINE 07  COLUMN 01 
               VALUE  "      NOME          :".
           05  LINE 09  COLUMN 01 
               VALUE  "      EMAIL         :".
           05  LINE 11  COLUMN 01 
               VALUE  "      SEXO          :".
           05  LINE 13  COLUMN 01 
               VALUE  "      GENERO        :".
           05  LINE 15  COLUMN 01 
               VALUE  "      TIPO DE AMIGO :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
       
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "FACULDADE" TO TBAMIGO(1)
           MOVE "IGREJA   " TO TBAMIGO(2)
           MOVE "BAIRRO"    TO TBAMIGO(3)
           MOVE "COLEGIO"   TO TBAMIGO(4)
           MOVE "BALADA"    TO TBAMIGO(5)
           MOVE "FUTEBOL"   TO TBAMIGO(6)
           MOVE "VIAGEM"    TO TBAMIGO(7)
           MOVE "EX"        TO TBAMIGO(8)
           MOVE "NAMOR.EX"  TO TBAMIGO(9).
      *
       R1.
           DISPLAY TELA2.
           ACCEPT (05, 23) APELIDO
           ACCEPT (07, 23) NOME
           ACCEPT (09, 23) EMAIL.
       R5.
           ACCEPT (11, 23) SEXO
           IF SEXO = "M" OR SEXO = "m"
              MOVE "M" TO SEXO
              DISPLAY (11, 25) "MASCULINO"
           ELSE
              IF SEXO = "F"
                 DISPLAY (11, 25) "FEMENINO"
              ELSE
                 DISPLAY (23, 14) "DIGITE M=MASCULINO   F=FEMENINO  "
                 STOP "  DIGITE ENTER PARA CONTINUAR"
                 GO TO R5.
       R6.
           MOVE 1 TO IND
           ACCEPT (13, 23) GENERO.
       R6A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 DISPLAY (23, 14) "TIPO DE GENERO INCORRETO"
                 STOP "DIGITE ENTER PARA CONTINUAR"
                 GO TO R6
           ELSE
               DISPLAY (13, 25) TXTGENERO2.

       R7.
           ACCEPT (15, 23) TPAMIGO.
           IF TPAMIGO = 0 
                 DISPLAY (23, 14) "DIGITE APENAS DE 1 A 9 "
                 STOP "  DIGITE ENTER PARA CONTINUAR"
                 GO TO R7.
           MOVE TBAMIGO(TPAMIGO) TO TEXTPAMIGO
           DISPLAY (15, 25) TEXTPAMIGO.
           STOP RUN.