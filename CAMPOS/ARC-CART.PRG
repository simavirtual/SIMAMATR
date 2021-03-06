/*                       SIMA - CARTERA ACADEMICA
		     DECLARACION DE ARCHIVOS DEL SISTEMA

**************************************************************************
* NOMBRE DEL PROGRAMA......: ARC-CART.PRG                                *
* TITULO DEL PROGRAMA......: DECLARACION DE ARCHIVOS DEL SISTEMAS        *
* PROGRAMAS QUE LO INVOCAN.: La mayoria de funciones del sistema         *
* PROGRAMAS QUE INVOCA.....: Ninguno                                     *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: FEB 15/96 JUE A
       Colombia, Bucaramanga        INICIO: 03:00 PM   FEB 15/96 JUE

REQUERIMIENTOS:

1- Para que el programa funcione se debe invocar utilizando la clausula
   #INCLUDE dentro de la funcion que lo va a utilizar.

OBJETIVOS:

1- Declara todas las variables que contiene la declaraci�n de todos los
   archivos que conforma el sistema.

2- El objetivo principal de esta declaraci�n es estandarizar el modo
   de acceso a los archivos.

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

*>>>>DECLARACION DE ARCHIVOS GENERALES
       LOCAL PathWeb := '..\simaweb'        // Path del Sitio

       LOCAL PathSys := 'SIMACART'          // Path del Sistema
       LOCAL PathSis := 'BASES'             // Path General del sistema
       LOCAL ExtFile := '.DAT'              // Extension para las bases
       LOCAL cExtNtx := '.NEL'              // Extension para indices
       LOCAL aUseDbf := {}                  // Vector para Apertura de Varias Dbfs
       LOCAL lSiRed  := .T.                 // .T. Apertura de Archivos MultiUsuario
       LOCAL lNoRed  := .F.                 // .F. Apertura de Archivo MonoUsuario


       LOCAL PathPrn := 'PRINTER'           // Path para impresiones
       LOCAL PathDoc := 'c:\misdoc'         // Path de Documentos del sima
       LOCAL FileTem := 'TEMPO'             // Archivo datos temporal
       LOCAL fNtxTem := 'TEMPO'             // Archivo Indice temporal
       LOCAL cExtTem := '.TEM'              // Extension indice temporal

       LOCAL nHanXml := -1                  // Manejador del Archivo
       LOCAL FileXml := ''                  // Archivo Xml
     *�Archivos espec�ficos

       LOCAL FileAaa := 'TEMP'+ExtFile
     *�Archivo de Usuarios

       LOCAL fErrSys := 'ERRORSYS'+ExtFile
     *�Archivo de Errores

       LOCAL FileClr := 'CFGCOLOR'+ExtFile
     *�Archivo de Configuraci�n del Color

       LOCAL FilePrn := 'DRIVERS'+ExtFile
     *�Archivo de Configuraci�n de Impresoras

       LOCAL FileDat := 'DATO0104'+ExtFile
     *�Archivo de Datos del Sistema

       LOCAL FileInf := 'INFORSYS'+ExtFile
     *�Archivo de informes del sistema

       LOCAL FileIso := 'FILESISO'+ExtFile
     *�Archivo de documentos de Iso

       LOCAL FileMzp := 'CONFIREC'+ExtFile
     *�Archivo de Configuraci�n MZP KIOSERA

       LOCAL FileXsl := 'infprn'+ExtFile
     *�Archivo Xsl para los informes.

       LOCAL FileJpg := 'fondo.jpg'
     *�Archivo jpg fondo de los informes
*>>>>FIN DECLARACION DE ARCHIVOS GENERALES

*>>>>DECLARACION DE ARCHIVOS DEL SISTEMA
       LOCAL FileEmp := 'EMPRESA'+ExtFile
     *�Archivo de Empresa

       LOCAL fSimCar := 'SIMACART'+ExtFile
     *�Archivo de Configuraci�n del sistema

       LOCAL fCarAno := 'CART'     // Ej: CART1996.DAT
     *�Archivo de Configuraci�n del a�o

       LOCAL fCarTbl := 'CART'     // Ej: CARTBTO.DAT
     *�Archivo de Configuraci�n del nivel

       LOCAL FileCli := ''                            // Ej: CLI1101.094
     *�Archivo de Clientes

       LOCAL FilePag := 'PAGOS'+ExtFile               // Ej: PAG1101.094
       LOCAL fNtxPag := 'PAGOS'+cExtNtx               // En Matriculas
     *�Archivo de Pagos

       LOCAL FileCoE := 'CONCEEST'+ExtFile            // Ej: CONCEPEST.DAT
     *�Archivo de Conceptos de los Estudiantes

       LOCAL FilePaC := 'PAGOSCON'+ExtFile            // Ej: PAGOSCON.DAT
     *�Archivo de Pagos de Conceptos

       LOCAL FileBan := 'BANCOS'+ExtFile
     *�Archivo de Bancos

       LOCAL FileCon := 'CONCEPTO'+ExtFile
     *�Archivo de Concepto

       LOCAL FileTar := 'TARIFAS'+ExtFile
     *�Archivo de Tarifas

       LOCAL FileDes := 'DESCUENT'+ExtFile
       LOCAL fNtxDes := 'DESCUENT'+cExtNtx
     *�Archivo de Descuentos

       LOCAL FileTra := ''                       // PA199905.DAT  Para el Mensual
       LOCAL fNtxTra := ''                       // 19990506.DAT  Para el Diario
     *�Archivo de Trasaciones

       LOCAL FileMoB := ''                       // Ej: 01199902.DAT
       LOCAL fNtxMoB := ''
     *�Archivo de Movimientos del Banco

       LOCAL FileTmv := 'TIPMOV'                      // Ej: TIPMOV01.DAT
     *�Archivo de tipos de movimiento.

       LOCAL FileCmv := 'CONCEMOV'+ExtFile
       LOCAL fNtxCmv := 'CONCEMOV'+cExtNtx
     *�Archivo de Conceptos de Movimiento

       LOCAL FileCob := 'COBROS'                      // Ej: COBROS02.DAT
       LOCAL fNtxCob := 'COBROS'                      //     COBROS02.MEL
     *�Archivo de Cobros

       LOCAL FileMor := 'MOROSO'                      // Ej: MOROSO02.DAT
       LOCAL fNtxMor := 'MOROSO'
     *�Archivo de Morosos

       LOCAL FileCoP := 'COBPAG'                      // Ej: COBPAG02.DAT
     *�Archivo de Cobros pagados

       LOCAL FileFpg := 'FORMAPAG'+ExtFile           // Ej: FORMAPAG.DAT
     *�Archivo de Forma de pago

       LOCAL FileRec := 'RECIBOS'+ExtFile             // Ej: RECIBOS.DAT
       LOCAL fNtxRec := 'RECIBOS'+cExtNtx
     *�Archivo de Control de Recibos

       LOCAL FilePuc := 'PUC'+ExtFile                 // Ej: PUC.DAT
       LOCAL fNtxPuc := 'PUC'+cExtNtx
     *�Archivo de Plan de Cuentas

       LOCAL FileRut := 'RUTAS'+ExtFile               // Ej: RUTAS.DAT
       LOCAL fNtxRut := 'RUTAS'+cExtNtx
     *�Archivo de Rutas

       LOCAL FileURu := 'USRRUTAS'+ExtFile               // Ej: USRRUTAS.DAT
       LOCAL fNtxURu := 'USRRUTAS'+cExtNtx
     *�Archivo de Usuarios de Rutas

       LOCAL FileFia := 'FIADORES'+ExtFile               // Ej: FIADORES.DAT
       LOCAL fNtxFia := 'FIADORES'+cExtNtx
     *�Archivo de Fiadores

       LOCAL FileCvn := 'CONVENIO'+ExtFile               // Ej: CONVENIO.DAT
       LOCAL fNtxCvn := 'CONVENIO'+cExtNtx
     *�Archivo de Convenios

       LOCAL PathOnl := ''
       LOCAL FileOnl := 'ONL'                            // Pagos en l�nea
     *�Pagos en l�nea
*>>>>DECLARACION DE ARCHIVOS DEL SISTEMA

*>>>>DECLARACION DE ARCHIVOS COMPARTIDOS
       LOCAL fSimaCo := 'SIMACONT'+ExtFile
     *�Archivo de Configuraci�n del sistema

       LOCAL FilePer := 'PERSONAS'+ExtFile
       LOCAL fNtxPer := 'PERSONAS'+cExtNtx
       LOCAL Ntx1Per := 'PERSONIT'+cExtNtx
     *�Archivo de Personas

       LOCAL FilePro := 'PROFES'           // Ej: PROFES95.DAT
     *�Archivo de Profesores

       LOCAL FileAlu := 'MAE'
       LOCAL fNtxAlu := 'MAE'
     *�Archivo de Alumnos

       LOCAL fPazEst := 'PAZEST'	           // Ej: PAZEST01.DAT
     *�Archivo de Paz y Salvos

       LOCAL fDpzEst := 'DEUPAZ'	           // Ej: DEUPAZ01.DAT
     *�Archivo de Deudas del Paz y Salvo

       LOCAL FMaeBto := 'MAEBTO'            // Ej: MAEBTO95.DAT
       LOCAL fNtxBto := 'MAEBTO'
       LOCAL Ntx1Bto := 'APEBTO'            // Ej: APEBTO95.NEL
       LOCAL Ntx2Bto := 'NOMBTO'            // Ej: NOMBTO95.NEL
     *�Archivo de Alumnos de Bachillerato

       LOCAL FMaePri := 'MAEPRI'            // Ej: MAEPRI94.DAT
       LOCAL fNtxPri := 'MAEPRI'
       LOCAL Ntx1Pri := 'APEPRI'            // Ej: APEPRI95.NEL
       LOCAL Ntx2Pri := 'NOMPRI'            // Ej: NOMPRI95.NEL
     *�Archivo de Alumnos de Primaria

       LOCAL FMaeJar := 'MAEJAR'            // Ej: MAEJAR94.DAT
       LOCAL fNtxJar := 'MAEJAR'
       LOCAL Ntx1Jar := 'APEJAR'            // Ej: APEJAR95.NEL
       LOCAL Ntx2Jar := 'NOMJAR'            // Ej: NOMJAR95.NEL
     *�Archivo de Alumnos de Jard�n

       LOCAL FMaeGua := 'MAEGUA'            // Ej: MAEGUA94.DAT
       LOCAL fNtxGua := 'MAEGUA'
       LOCAL Ntx1Gua := 'APEGUA'            // Ej: APEGUA95.NEL
       LOCAL Ntx2Gua := 'NOMGUA'            // Ej: NOMGUA95.NEL
     *�Archivo de Alumnos de Guarderia

       LOCAL FileGru := 'GRU'
     *�Archivo de Grupos

       LOCAL FGruGua := 'GRUGUA'            // Ej: GRUGUA94.DAT
       LOCAL NGruGua := ''
     *�Archivo de Grupos de Guarder�a

       LOCAL FGruJar := 'GRUJAR'            // Ej: GRUJAR94.DAT
       LOCAL NGruJar := ''
     *�Archivo de Grupos de Jard�n

       LOCAL FGruPri := 'GRUPRI'            // Ej: GRUPRI94.DAT
       LOCAL NGruPri := ''
     *�Archivo de Grupos de Primaria

       LOCAL FGruBto := 'GRUBTO'            // Ej: GRUBTO94.DAT
       LOCAL NGruBto := ''
     *�Archivo de Grupos de Bachillerato

       LOCAL FileNiv := 'NIVELS'
     *�Archivo de Niveles

       LOCAL FileAdm := 'ADMI'               // Ej: ADMISI95.DAT
       LOCAL fNtxAdm := ''
     *�Archivo de Admisiones

       LOCAL FCaFAno := 'CAFE'              // Ej: CAFE2002.DAT
     *�Archivo de Configuraci�n del a�o

       LOCAL FPerCaF := 'PERSONAL'+ExtFile  // Ej: PERSONAL.DAT
     *�Personal de cafeteria.

       LOCAL FileFam := 'FAMILIA'+ExtFile   // Ej: FAMILIA.DAT
       LOCAL fNtxFam := 'FAMILIA'+cExtNtx
     *�Archivo de Familias
*>>>>FIN DECLARACION DE ARCHIVOS COMPARTIDOS