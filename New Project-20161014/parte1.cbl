        IDENTIFICATION DIVISION.
        PROGRAM-ID. TP.
        
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.
        
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          
            SELECT NOV-TIMES1     ASSIGN TO DISK
                               	  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS NOV-TIMES1-ESTADO.
        
            SELECT NOV-TIMES2     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS NOV-TIMES2-ESTADO.
        
            SELECT NOV-TIMES3     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS NOV-TIMES3-ESTADO.
                                   
            SELECT PROFESORES     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS PROF-ESTADO.

            SELECT SUCURSALES     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS SUCURSALES-ESTADO.

            SELECT TIPOS_CLASE    ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS TIPOS_CLASE-ESTADO.
        
            SELECT MAE-TIMES ASSIGN TO PRINTER "Times.dat".
            SELECT LISTADO ASSIGN TO PRINTER "Listado.dat".
        
        DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

        FILE SECTION.
        
        FD LISTADO     LABEL RECORD OMITTED.
        01 LINEA-LISTADO PIC X(87).
        
             
        FD NOV-TIMES1     LABEL RECORD IS STANDARD
                         VALUE OF FILE-ID IS "NovTimes1.dat".
        01 REG-NOV-TIMES1.
            03 NOV-TIMES1-NUMERO       PIC X(5).
            03 NOV-TIMES1-FECHA.
                05 NOV-TIMES1-ANIO     PIC 9(4).
                05 NOV-TIMES1-MES      PIC 9(2).
                05 NOV-TIMES1-DIA      PIC 9(2).
            03 NOV-TIMES1-SUCURSAL     PIC X(3).
            03 NOV-TIMES1-TIPO-CLASE   PIC X(4).
            03 NOV-TIMES1-HORAS        PIC 9(2)V99.
                
        FD NOV-TIMES2     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "NovTimes2.dat".
        01 REG-NOV-TIMES2.
            03 NOV-TIMES2-NUMERO      PIC X(5).
            03 NOV-TIMES2-FECHA.
                05 NOV-TIMES2-ANIO    PIC 9(4).
                05 NOV-TIMES2-MES     PIC 9(2).
                05 NOV-TIMES2-DIA     PIC 9(2).
            03 NOV-TIMES2-SUCURSAL    PIC X(3).
            03 NOV-TIMES2-TIPO-CLASE  PIC X(4).
            03 NOV-TIMES2-HORAS       PIC 9(2)V99.
                
        FD NOV-TIMES3     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "NovTimes3.dat".
        01 REG-NOV-TIMES3.
            03 NOV-TIMES3-NUMERO      PIC X(5).
            03 NOV-TIMES3-FECHA.
                05 NOV-TIMES3-ANIO    PIC 9(4).
                05 NOV-TIMES3-MES     PIC 9(2).
                05 NOV-TIMES3-DIA     PIC 9(2).
            03 NOV-TIMES3-SUCURSAL    PIC X(3).
            03 NOV-TIMES3-TIPO-CLASE   PIC X(4).
            03 NOV-TIMES3-HORAS        PIC 9(2)V99.    
       


        FD PROFESORES     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "Profesores.dat".
        01 REG-PROFESORES.
            03 PROF-NUMERO            PIC X(5).
            03 PROF-DNI               PIC 9(8).
            03 PROF-NOMBRE            PIC X(25).
            03 PROF-DIRE              PIC X(20).
            03 PROF-TEL               PIC X(20).


        FD SUCURSALES     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "Sucursales.dat".
        01 REG-SUCURSALES.
            03 SUC-SUCURSAL       PIC X(3).
            03 SUC-RAZON          PIC X(25).
            03 SUC-DIRE           PIC X(20).
            03 SUC-TEL            PIC X(20).
            03 SUC-CUIT           PIC 9(11).

     
        FD TIPOS_CLASE    LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "TiposClase.dat".
        01 REG-TIPOS_CLASE.
           03 TIP-TIP_CLASE       PIC X(4).
           03 TIP-DESC            PIC X(20).
           03 TIP-TARIFA          PIC 9(5)V99.  


        WORKING-STORAGE SECTION.        
        77 NOV-TIMES1-ESTADO PIC XX.
        77 NOV-TIMES2-ESTADO PIC XX.
        77 NOV-TIMES3-ESTADO PIC XX.
        77 PROF-ESTADO PIC XX.
        77 SUCURSALES-ESTADO PIC XX.        
        77 TIPOS_CLASE-ESTADO PIC XX.      
        77 EOF-NOVTIMES1 PIC XX VALUE "NO".
            88 EOF-NOV-TIMES1 VALUE "SI".
        77 EOF-NOVTIMES2 PIC XX VALUE "NO".
            88 EOF-NOV-TIMES2 VALUE "SI".
        77 EOF-NOVTIMES3 PIC XX VALUE "NO".
            88 EOF-NOV-TIMES3 VALUE "SI".    
        77 EOF-PROF PIC XX VALUE "NO".
            88 EOF-PROFESORES VALUE "SI".    
        77 EOF-MAE-TIMES PIC XX VALUE "NO".
            88 EOF-MAE-TIMES VALUE "SI".
        77 EOF-SUC PIC XX VALUE "NO".
            88 EOF-SUCURSALES VALUE "NO".
        77 EOF-CLASES PIC XX VALUE "NO".
            88 EOF-TIPOS_CLASE VALUE "SI".
            
            
        01 CLAVE-NOV-TIMES1.
            03 CLAVE-NOV-TIMES1-NUMERO   PIC X(5).
            03 CLAVE-NOV-TIMES1-FECHA    PIC 9(8).
            03 CLAVE-NOV-TIMES1-SUCURSAL PIC X(3).
        01 CLAVE-NOV-TIMES2.
            03 CLAVE-NOV-TIMES2-NUMERO   PIC X(5).
            03 CLAVE-NOV-TIMES2-FECHA    PIC 9(8).
            03 CLAVE-NOV-TIMES2-SUCURSAL PIC X(3).
        01 CLAVE-NOV-TIMES3.
            03 CLAVE-NOV-TIMES3-NUMERO   PIC X(5).
            03 CLAVE-NOV-TIMES3-FECHA    PIC 9(8).
            03 CLAVE-NOV-TIMES3-SUCURSAL PIC X(3).
        01 MENOR-CLAVE.
            03 MENOR-CLAVE-NUMERO        PIC X(5).
            03 MENOR-CLAVE-FECHA         PIC 9(8).
            03 MENOR-CLAVE-SUCURSAL      PIC X(3).

        01 LINEA-A-ESCRIBIR PIC 9(2) VALUE 1.
        01 HORAS-TOTALES PIC 9(2)V99.
        01 HORAS-PROFESOR PIC 9(2)V99.
        01 HORAS-FECHA PIC 9(2)V99.
        01 PROFESOR-ANTERIOR PIC X(5) VALUE '     '.
        01 IMPORTE PIC 9(7)V99.
        01 IMPORTE-TOTAL PIC 9(7)V99 VALUE 0.
        01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10 WS-CURRENT-YEAR     PIC X(04).
               10 WS-CURRENT-MONTH    PIC X(02).
               10 WS-CURRENT-DAY     PIC X(02).
           05  WS-CURRENT-TIME.
               10 WS-CURRENT-HOUR     PIC  9(2).
               10  WS-CURRENT-MINUTE  PIC  9(2).
               10  WS-CURRENT-SECOND  PIC  9(2).
               10  WS-CURRENT-MS      PIC  9(2).
               10  WS-GMT-SIGN        PIC X(01).
               10  WS-GMT-TIME        PIC X(04).


       01 SUBINDICE PIC 9(2) VALUE 1.
       01 TABLA-SUCURSALES.
           02 TAB-SUCURSALES OCCURS 100 TIMES INDEXED BY SUC-INDICE.
               03 TAB-SUC-SUCURSAL PIC X(3).
               03 TAB-SUC-RAZON PIC X(25).
               03 TAB-SUC-DIRE PIC X(20).
               03 TAB-SUC-TEL PIC X(20).
               03 TAB-SUC-CUIT PIC 9(11).
       01 TABLA-TIPOS_CLASE.
           02 TAB-TIPOS_CLASE OCCURS 50 TIMES INDEXED BY TIP-INDICE.
               03 TAB-TIP-TIP_CLASE PIC X(4).
               03 TAB-TIP-DESC PIC X(20).
               03 TAB-TIP-TARIFA PIC 9(5)V99.
    
        PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      * COMIENZO.
        
        PERFORM 0100-INICIO.
        PERFORM 0200-LEER-NOV-TIMES1. 
        PERFORM 0300-LEER-NOV-TIMES2. 
        PERFORM 0400-LEER-NOV-TIMES3.
        PERFORM 0500-LEER-PROFESORES.
        PERFORM 0600-LEER-SUCURSALES.
        PERFORM 0700-LEER-TIPOS_CLASE.
        PERFORM 0800-CARGAR-TABLAS.
        MOVE 0 TO HORAS-TOTALES.
        PERFORM FIN.        
        STOP RUN.
        
      *----------    PERFORM INICIO      -------------------------*
      *-----------------------------------------------------------*
        0100-INICIO.           
            OPEN INPUT NOV-TIMES1.
            OPEN INPUT NOV-TIMES2.
            OPEN INPUT NOV-TIMES3.
            OPEN INPUT PROFESORES.
            OPEN INPUT SUCURSALES.
            OPEN INPUT TIPOS_CLASE.
            OPEN OUTPUT MAE-TIMES.
            OPEN OUTPUT LISTADO.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0200-LEER-NOV-TIMES1.
         READ NOV-TIMES1
            AT END MOVE "SI" TO EOF-NOVTIMES1.
         MOVE NOV-TIMES1-FECHA TO CLAVE-NOV-TIMES1-FECHA.
         MOVE NOV-TIMES1-NUMERO TO CLAVE-NOV-TIMES1-NUMERO.
         MOVE NOV-TIMES1-SUCURSAL TO CLAVE-NOV-TIMES1-SUCURSAL.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0300-LEER-NOV-TIMES2.
         READ NOV-TIMES2
            AT END MOVE "SI" TO EOF-NOVTIMES2.
         MOVE NOV-TIMES2-FECHA TO CLAVE-NOV-TIMES2-FECHA.
         MOVE NOV-TIMES2-NUMERO TO CLAVE-NOV-TIMES2-NUMERO.
         MOVE NOV-TIMES2-SUCURSAL TO CLAVE-NOV-TIMES2-SUCURSAL.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0400-LEER-NOV-TIMES3.
         READ NOV-TIMES3
            AT END MOVE "SI" TO EOF-NOVTIMES3.
         MOVE NOV-TIMES3-FECHA TO CLAVE-NOV-TIMES3-FECHA.
         MOVE NOV-TIMES3-NUMERO TO CLAVE-NOV-TIMES3-NUMERO.
         MOVE NOV-TIMES3-SUCURSAL TO CLAVE-NOV-TIMES3-SUCURSAL.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0500-LEER-PROFESORES.
         READ PROFESORES AT END MOVE "SI" TO EOF-PROF.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------* 
        0600-LEER-SUCURSALES.
         READ SUCURSALES AT END MOVE "SI" TO EOF-SUC.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0700-LEER-TIPOS_CLASE.
         READ TIPOS_CLASE AT END MOVE "SI" TO EOF-CLASES.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0800-CARGAR-TABLAS.
         PERFORM 0900-CARGAR-TIPOS_CLASE UNTIL EOF-TIPOS_CLASE.
         MOVE 1 TO SUBINDICE.
         PERFORM 1000-CARGAR-SUCURSALES UNTIL EOF-SUCURSALES.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0900-CARGAR-TIPOS_CLASE.
         DISPLAY "CARGAR TIPO CLASE".

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        1000-CARGAR-SUCURSALES.
         DISPLAY "CARGAR SUCURSALES".

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       FIN.
            CLOSE NOV-TIMES1.
            CLOSE NOV-TIMES2.
            CLOSE NOV-TIMES3.
            CLOSE PROFESORES.
            CLOSE SUCURSALES.
            CLOSE TIPOS_CLASE.
            CLOSE MAE-TIMES.
            CLOSE LISTADO.

