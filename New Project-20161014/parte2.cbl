       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP.
        
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
        
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAESTRO     ASSIGN TO DISK
                               	  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS MAE-ESTADO.
           
           SELECT SUCURSALES     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS SUC-ESTADO.

	       SELECT LISTADO ASSIGN TO PRINTER "Estadisticas.dat".

*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*   
     
       DATA DIVISION.
       FILE SECTION.
        
       FD LISTADO     LABEL RECORD OMITTED.
       01 LINEA-LISTADO PIC X(87).
        
             
       FD MAESTRO     LABEL RECORD IS STANDARD
                         VALUE OF FILE-ID IS "NovTimes.dat".
       01 MAES.
           03 MAES-NUMERO       PIC X(5).
           03 MAES-FECHA.
                 05 MAES-ANIO     PIC 9(4).
                 05 MAES-MES      PIC 9(2).
                 05 MAES-DIA      PIC 9(2).
           03 MAES-SUCURSAL     PIC X(3).
           03 MAES-TIPO-CLASE   PIC X(4).
           03 MAES-HORAS        PIC 9(2)V99.

       FD SUCURSALES     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "Sucursales.dat".
       01 REG-SUCURSALES.
           03 SUC-SUCURSAL       PIC X(3).
           03 SUC-RAZON          PIC X(25).
           03 SUC-DIRE           PIC X(20).
           03 SUC-TEL            PIC X(20).
           03 SUC-CUIT           PIC 9(11).
        

        WORKING-STORAGE SECTION.        
        77 MAE-ESTADO      PIC XX.
        77 SUC-ESTADO      PIC XX.
        77 EOF-MAESTRO         PIC XX VALUE "NO".
           88 EOF-M               VALUE "SI".
        77 EOF-SUCURSALES  PIC XX VALUE "NO".
           88 EOF-S               VALUE "SI". 

        01 FECHA.          
           03 ANIO-ACTUAL PIC 9(4).
           03 MES-ACTUAL  PIC 9(2).
           03 DIA-ACTUAL  PIC 9(2).

        01 ANIO        PIC 9(4).
        01 ANIO-BASE   PIC 9(4).
        01 MES         PIC 9(2).  
        01 INDICE      PIC 9.
        01 INDICE-TABLA PIC 9(2).

        01 ENCABE-FECHA-HOJA.
           03 FILLER                PIC XX VALUE SPACES.
           03 ENCABE-F              PIC X(6) VALUE "Fecha ".
           03 ENCABE-FECHA-DD   PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 ENCABE-FECHA-MM   PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 ENCABE-FECHA-AAAA PIC 9(4).                 
           03 FILLER                PIC X(56) VALUE SPACES.
           03 FILLER                PIC X(5) VALUE "Hoja ".
           03 ENCABE-HOJA       PIC 9(3).

        01 ENCABE-TITULO.
           03 FILLER    PIC X(5) VALUE SPACES.
           03 FILLER    PIC X(57) VALUE "Listado de Estadistica Horas".
           03 FILLER    PIC X(18) VALUE SPACES. 

       01 LINEA-IMP.
           03 FILLER PIC XX VALUE SPACES.
           03 NOM-SUC PIC X(23) VALUE SPACES.
           03 FILLER PIC XX VALUE SPACES.       
           03 CAN-ENE PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-FEB PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-MAR PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-ABR PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-MAY PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-JUN PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-JUL PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-AGO PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-SEP PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-OCT PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-NOV PIC 9(3).
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-DIC PIC 9(3).
           03 FILLER  PIC XX VALUE SPACES.
           03 CAN-TOTAL-ANIO PIC 9(6).


       01 VEC-SUC.
           05 SUC OCCURS 3 TIMES.
                 10 SUCUR PIC X(3).

       01 TABLA-ESTADISTICA.
           05 TABLA-M OCCURS 15 TIMES.
             10 TABLA-A OCCURS 13 TIMES.
                 15 TABLA-CELL PIC 9(3).

*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      

       PROCEDURE DIVISION.
       COMIENZO.

           PERFORM 0100-INICIO.
           PERFORM 0200-CARGO-SUCUR.
           PERFORM 0300-CARGA-EN-TABLA.
           PERFORM 0400-IMPRESION.
           PERFORM 0500-FIN. 
           STOP RUN.


*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

       0100-INICIO.
           OPEN INPUT SUCURSALES.
           IF SUC-ESTADO NOT = ZERO
                 DISPLAY "Error abriendo Sucursales FS: " SUC-ESTADO
                 STOP RUN.
           OPEN INPUT MAESTRO.   
           IF MAE-ESTADO NOT = ZERO
                 DISPLAY "Error abriendo Maestro FS: " MAE-ESTADO
                 STOP RUN.
           OPEN OUTPUT LISTADO.
           ACCEPT FECHA FROM DATE.
           MOVE ANIO-ACTUAL TO ANIO-BASE.
           SUBTRACT 5 FROM ANIO-BASE.
         
       0200-CARGO-SUCUR.
           MOVE 1 TO INDICE.
           READ SUCURSALES AT END MOVE "SI" TO EOF-SUCURSALES.   
           PERFORM 0210-ASIGNO-SUCURSALES UNTIL EOF-SUCURSALES = "SI".

       0210-ASIGNO-SUCURSALES.
           MOVE SUC-SUCURSAL TO SUC(INDICE).
           ADD 1 TO INDICE.
           READ SUCURSALES AT END MOVE "SI" TO EOF-SUCURSALES.   

       0300-CARGA-EN-TABLA.
           READ MAESTRO AT END MOVE "SI" TO EOF-MAESTRO.
           PERFORM 0310-CARGO-TABLA UNTIL EOF-MAESTRO = "SI"
                              AND MAES-ANIO > ANIO-BASE.

       0310-CARGO-TABLA.
           SUBTRACT ANIO-BASE FROM MAES-ANIO GIVING ANIO.
           MOVE MAES-MES TO MES. 
           IF MAES-SUCURSAL EQUAL SUC(1)
                 PERFORM 0320-OPCION1.
           IF MAES-SUCURSAL EQUAL SUC(2)
                 PERFORM 0330-OPCION2.
           IF MAES-SUCURSAL EQUAL SUC(3)
                 PERFORM 0340-OPCION3.
           READ MAESTRO AT END MOVE "SI" TO EOF-MAESTRO.

       0320-OPCION1. 
           ADD MAES-HORAS TO TABLA-CELL(ANIO,MES).

       0330-OPCION2.
           ADD 5 TO ANIO GIVING INDICE-TABLA. 
           ADD MAES-HORAS TO TABLA-CELL(INDICE-TABLA,MES).

       0340-OPCION3. 
           ADD 10 TO ANIO GIVING INDICE-TABLA.
           ADD MAES-HORAS TO TABLA-CELL(INDICE-TABLA,MES).


       0400-IMPRESION.
           DISPLAY "Estoy imprimiendo!!".

       0500-FIN.
           CLOSE MAESTRO.
           CLOSE SUCURSALES.
           CLOSE LISTADO.
           DISPLAY "Fin del programa.".

