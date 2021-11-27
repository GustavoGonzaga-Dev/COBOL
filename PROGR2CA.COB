       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR2C.
      *AUTHOR. ANGELO LOTIERZO FILHO.
      **************************************
      * MANUTENCAO DO XXXXXXXX  *
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
      *
       01 TABAMIGO.
          03 TBAMIGO    PIC X(10) OCCURS 10 TIMES.
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
           MOVE "EX(A)"     TO TBAMIGO(8)
           MOVE "NAMOR.EX(A)" TO TBAMIGO(9).
           MOVE "ESQUECI"   TO TBAMIGO(10).
      *
       R1.
           DISPLAY TELA2.
           ACCEPT (05, 23) APELIDO
           ACCEPT (07, 23) NOME
           ACCEPT (09, 23) EMAIL.
       R5.
           ACCEPT (11, 23) SEXO
           IF SEXO = "M"
              DISPLAY (11, 25) "MASCULINO"
           ELSE
              IF SEXO = "F"
                 DISPLAY (11, 25) "FEMENINO"
              ELSE
                 DISPLAY (23, 14) "DIGITE M=MASCULINO   F=FEMENINO  "
                 STOP "  DIGITE ENTER PARA CONTINUAR"
                 GO TO R5.

           ACCEPT (13, 23) GENERO.  
       R7.
           ACCEPT (15, 23) TPAMIGO.

           MOVE TBAMIGO(TPAMIGO + 1) TO TEXTPAMIGO
           DISPLAY (15, 25) TEXTPAMIGO.
           STOP RUN.