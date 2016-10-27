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
                 05 MAES-DIA     PIC 9(2).
                 05 MAES-MES      PIC 9(2).
                 05 MAES-ANIO      PIC 9(4).
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
        01 INDICE      PIC 9(2).
        01 INDICE-TABLA PIC 9(2).
        01 TOTAL       PIC 9(4).
        01 TOTAL-ABSOLUTO PIC 9(6).

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
           03 FILLER    PIC X(15) VALUE SPACES.
           03 FILLER    PIC X(57) VALUE "Listado de Estadistica Horas".
           03 FILLER    PIC X(10) VALUE SPACES. 

       77 LINEA-EN-BLANCO PIC X(80) VALUE SPACES.

       01 TITULO-IMP.
           03 FILLER PIC XX VALUE SPACES.
           03 TIT-SUC PIC X(23) VALUE "Sucursal".
           03 FILLER PIC XX VALUE SPACES.       
           03 TIT-ENE PIC X(3) VALUE "Ene".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-FEB PIC X(3) VALUE "Feb".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-MAR PIC X(3) VALUE "Mar".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-ABR PIC X(3) VALUE "Abr".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-MAY PIC X(3) VALUE "May".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-JUN PIC X(3) VALUE "Jun".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-JUL PIC X(3) VALUE "Jul".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-AGO PIC X(3) VALUE "Ago".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-SEP PIC X(3) VALUE "Sep".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-OCT PIC X(3) VALUE "Oct".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-NOV PIC X(3) VALUE "Nov".
           03 FILLER  PIC X VALUE SPACES.
           03 TIT-DIC PIC X(3) VALUE "Dic".
           03 FILLER  PIC XX VALUE SPACES.
           03 TIT-TOTAL-ANIO PIC X(6) VALUE " Total".   

       01 LINEA-IMP.
           03 FILLER PIC XX VALUE SPACES.
           03 NOM-SUC PIC X(23) VALUE SPACES.
           03 FILLER PIC XX VALUE SPACES.       
           03 CAN-ENE PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-FEB PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-MAR PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-ABR PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-MAY PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-JUN PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-JUL PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-AGO PIC 9(3) VALUE ZEROS..
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-SEP PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-OCT PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-NOV PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC X VALUE SPACES.
           03 CAN-DIC PIC 9(3) VALUE ZEROS.
           03 FILLER  PIC XX VALUE SPACES.
           03 CAN-TOTAL-ANIO PIC 9(6) VALUE ZEROS.


       01 VEC-SUC.
           05 SUC OCCURS 3 TIMES.
                 10 SUCUR PIC X(3).

       01 TABLA-ESTADISTICA.
           05 TABLA-M OCCURS 15 TIMES.
             10 TABLA-A OCCURS 12 TIMES.
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
           MOVE ZEROS TO TOTAL-ABSOLUTO.

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
           MOVE 2016 TO ANIO-ACTUAL.
           MOVE 2016 TO ANIO-BASE.
           SUBTRACT 5 FROM ANIO-BASE.
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
           MOVE ZEROS TO INDICE-TABLA.

       0340-OPCION3. 
           ADD 10 TO ANIO GIVING INDICE-TABLA.         
           ADD MAES-HORAS TO TABLA-CELL(INDICE-TABLA,MES).
           MOVE ZEROS TO INDICE-TABLA.


       0400-IMPRESION.
           PERFORM 0410-IMPRIMIR-ENCABE.
           PERFORM 0420-IMPRIMIR-TITULO.
           DISPLAY LINEA-EN-BLANCO.
           WRITE LINEA-LISTADO FROM LINEA-EN-BLANCO.
           PERFORM 0430-IMPRIMIR-TABLA.
           PERFORM 0440-IMPRIMIR-TOTALES-POR-MES.
           DISPLAY LINEA-EN-BLANCO.
           WRITE LINEA-LISTADO FROM LINEA-EN-BLANCO.

       0410-IMPRIMIR-ENCABE.
           MOVE 20 TO ENCABE-FECHA-DD.
           MOVE 10 TO ENCABE-FECHA-MM.
           MOVE 2016 TO ENCABE-FECHA-AAAA.
           MOVE 001 TO ENCABE-HOJA.
           DISPLAY ENCABE-FECHA-HOJA.
           WRITE LINEA-LISTADO FROM ENCABE-FECHA-HOJA.

       0420-IMPRIMIR-TITULO.
           DISPLAY ENCABE-TITULO.
           WRITE LINEA-LISTADO FROM ENCABE-TITULO.
           DISPLAY TITULO-IMP.
           WRITE LINEA-LISTADO FROM TITULO-IMP.
 
       0430-IMPRIMIR-TABLA.    
           MOVE 01 TO INDICE-TABLA.
           MOVE SUC(1) TO NOM-SUC.
           PERFORM 0431-IMPRIMIR-MESES-SUC UNTIL INDICE-TABLA = 6.
           MOVE SUC(2) TO NOM-SUC.
           PERFORM 0431-IMPRIMIR-MESES-SUC UNTIL INDICE-TABLA = 11.
           MOVE SUC(3) TO NOM-SUC.
           PERFORM 0431-IMPRIMIR-MESES-SUC UNTIL INDICE-TABLA = 16.

       0440-IMPRIMIR-TOTALES-POR-MES.
           MOVE "Totales " TO NOM-SUC.
           MOVE 1 TO INDICE-TABLA.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-ENE.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-FEB.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-MAR.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-ABR.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-MAY.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-JUN.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-JUL.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-AGO.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-SEP.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-OCT.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-NOV.
           PERFORM 0441-SUMA-TOTAL-MES.
           MOVE TOTAL TO CAN-DIC.
           MOVE TOTAL-ABSOLUTO TO CAN-TOTAL-ANIO.
           DISPLAY LINEA-IMP.
           WRITE LINEA-LISTADO FROM LINEA-IMP.

       0431-IMPRIMIR-MESES-SUC.
           MOVE ZEROS TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 1) TO CAN-ENE.
           ADD CAN-ENE TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 2) TO CAN-FEB.
           ADD CAN-FEB TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 3) TO CAN-MAR.
           ADD CAN-MAR TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 4) TO CAN-ABR.
           ADD CAN-ABR TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 5) TO CAN-MAY.
           ADD CAN-MAY TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 6) TO CAN-JUN.
           ADD CAN-JUN TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 7) TO CAN-JUL.
           ADD CAN-JUL TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 8) TO CAN-AGO.
           ADD CAN-AGO TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 9) TO CAN-SEP.
           ADD CAN-SEP TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 10) TO CAN-OCT.
           ADD CAN-OCT TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 11) TO CAN-NOV.
           ADD CAN-NOV TO TOTAL.
           MOVE TABLA-CELL(INDICE-TABLA, 12) TO CAN-DIC.
           ADD CAN-DIC TO TOTAL.
           MOVE TOTAL TO CAN-TOTAL-ANIO.
           ADD TOTAL TO TOTAL-ABSOLUTO.
           DISPLAY LINEA-IMP.
           WRITE LINEA-LISTADO FROM LINEA-IMP.
           ADD 1 TO INDICE-TABLA.

       0441-SUMA-TOTAL-MES.
           MOVE 000 TO TOTAL.
           ADD TABLA-CELL(1, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(2, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(3, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(4, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(5, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(6, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(7, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(8, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(9, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(10, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(11, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(12, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(13, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(14, INDICE-TABLA) TO TOTAL.
           ADD TABLA-CELL(15, INDICE-TABLA) TO TOTAL.
           ADD 1 TO INDICE-TABLA.

       0500-FIN.
           CLOSE MAESTRO.
           CLOSE SUCURSALES.
           CLOSE LISTADO.
           DISPLAY "Fin".

