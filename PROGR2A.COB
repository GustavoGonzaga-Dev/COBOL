       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR2.
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
      *
       R1.
           DISPLAY TELA2.
           ACCEPT (05, 23) APELIDO
           ACCEPT (07, 23) NOME
           ACCEPT (09, 23) EMAIL
           ACCEPT (11, 23) SEXO
           ACCEPT (13, 23) GENERO
           ACCEPT (15, 23) TPAMIGO.
           STOP RUN.