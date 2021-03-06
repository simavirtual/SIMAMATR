/*SISTEMA DE MATRICULA ACADEMICA

MODULO      : MATRICULAS - INFORMES
SUBMODULO...: OTROS DOCUMENTOS - POR ALUMNOS. ANTIGUOS

**************************************************************************
* TITULO..: IMPRESION OTROS DOCUMENTOS. POR ALUMNOS ANTIGUOS             *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: OCT 16/2002 MIE A
       Colombia, Bucaramanga        INICIO: 03:00 PM   OCT 16/2002 MIE

OBJETIVOS:

1- Permite imprimir Otros Documentos de los estudiantes antiguos
   por alumnos.

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Matr_6372(aParam1,aParam2,aParam3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aParam1                              // Parametros Generales
       aParam2                              // Parametros Generales
       aParam3                              // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE CONSTANTES
       #INCLUDE "inkey.ch"                  // Declaraci�n de teclas
       #INCLUDE "ARC-MATR.PRG"      // Archivos del Sistema
*>>>>FIN DECLARACION DE CONSTANTES

*>>>>DECLARACION DE VARIABLES
       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL lHayErr := .F.                 // .T. Hay Error
     *�Variables generales

       LOCAL nOpcPrn := 0                   // Opci�n de Impresi�n

       LOCAL lAnoAct := .T.                 // A�o Actual
       LOCAL cPatAnt := ''                  // Path A�os anteriores
       LOCAL cAnoAnt := ''                  // A�o Anterior

       LOCAL     i,j := 0                   // Contador
       LOCAL cNalias := ''                  // Alias Antiguos o nuevos
       LOCAL aTitulo := {}                  // Titulos de las Columnas
       LOCAL aTamCol := {}                  // Tama�o de las Columnas
       LOCAL aNroCol := {}                  // N�meros de Columnas
       LOCAL nNroFil := 0                   // Fila de lectura
       LOCAL nNroCol := 0                   // Columna de lectura
       LOCAL cMsgTxt := ''                  // Mensaje Temporal
       LOCAL GetList := {}                  // Variable del Sistema

       LOCAL nNroCop := 0                   // N�mero de Copias
       LOCAL cDocume := ''                  // Documentos
       LOCAL nLenLin := 0                   // Longitud de la l�nea
       LOCAL nNroDoc := 1                   // N�mero del Documento
       LOCAL cNroDoc := ''                  // N�mero del Documento
       LOCAL cTxtHlp := ''                  // Texto de Ayuda
       LOCAL cTitHlp := ''                  // T�tulo para la Ayuda

       LOCAL cTxtUno := ''                  // Texto temporal Uno
       LOCAL cTxtDos := ''                  // Texto temporal Dos
       LOCAL cTxtTre := ''                  // Texto temporal tres
       LOCAL cTxtCua := ''                  // Texto temporal Cuatro

       LOCAL aMezIni := {}                  // Campos a Mesclar iniciales
       LOCAL aMezcla := {}                  // Campos a Mesclar
       LOCAL aFirmas := {}                  // Firmantes del Certificado
       LOCAL bCodBlo := NIL                 // Code Block.
       LOCAL aDatCnt := {}                  // Datos del Contratante

       LOCAL lOficio := .F.                 // .T. Tama�o Oficio
       LOCAL lNuevos := .F.                 // .F. Estudiantes Antiguos
       LOCAL cObserv := ''                  // Observaci�n para la matr�cula
       LOCAL cDocNit := ''                  // Documento del Contrante

       LOCAL dFecMatTes := CTOD('00/00/00') // Fecha de la Matr�cula
       LOCAL cHorMatTes := ''               // Hora de la Matr�cula

       LOCAL cCodigoTes := ''               // C�digo del estudiante
       LOCAL cNombreTes := ''               // Nombre del estudiante
       LOCAL cCodigoTgr := ''               // C�digo del Grupo
       LOCAL cCodigoTni := ''               // C�digo del Nivel
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DECLARACION PARAMETROS GENERALES
       LOCAL lShared := .T.                 // .T. Sistema Compartido
       LOCAL nModCry := 0                   // Modo de Protecci�n
       LOCAL cCodSui := ''                  // C�digo del Sistema
       LOCAL cNomSis := ''                  // Nombre del Sistema
     *�Detalles del Sistema

       LOCAL cEmpPal := ''                  // Nombre de la Empresa principal
       LOCAL cNitEmp := ''                  // Nit de la Empresa
       LOCAL cNomEmp := ''                  // Nombre de la Empresa
       LOCAL cNomSec := ''                  // Nombre de la Empresa Secundario
       LOCAL cCodEmp := ''                  // C�digo de la Empresa
     *�Detalles de la Empresa

       LOCAL cNomUsr := ''                  // Nombre del Usuario
       LOCAL cAnoUsr := ''                  // A�o del usuario
       LOCAL cAnoSis := ''                  // A�o del sistema
       LOCAL cPatSis := ''                  // Path del sistema
     *�Detalles del Usuario

       LOCAL nFilPal := 0                   // Fila Inferior Men� principal
       LOCAL nFilInf := 0                   // Fila Inferior del SubMen�
       LOCAL nColInf := 0                   // Columna Inferior del SubMen�
     *�Detalles Tecnicos

       LOCAL PathW01 := ''                  // Sitio del Sistema No.01
       LOCAL PathW02 := ''                  // Sitio del Sistema No.02
       LOCAL PathW03 := ''                  // Sitio del Sistema No.03
       LOCAL PathW04 := ''                  // Sitio del Sistema No.04
       LOCAL PathW05 := ''                  // Sitio del Sistema No.05
       LOCAL PathW06 := ''                  // Sitio del Sistema No.06
       LOCAL PathW07 := ''                  // Sitio del Sistema No.07
       LOCAL PathW08 := ''                  // Sitio del Sistema No.08
       LOCAL PathW09 := ''                  // Sitio del Sistema No.09
       LOCAL PathW10 := ''                  // Sitio del Sistema No.10
     *�Sitios del Sistema

       LOCAL PathUno := ''                  // Path de Integraci�n Uno
       LOCAL PathDos := ''                  // Path de Integraci�n Dos
       LOCAL PathTre := ''                  // Path de Integraci�n Tres
       LOCAL PathCua := ''                  // Path de Integraci�n Cuatro
     *�Path de Integraci�n

       LOCAL cMaeAlu := ''                  // Maestros habilitados
       LOCAL cMaeAct := ''                  // Maestro Activo
       LOCAL cJorTxt := ''                  // Jornada escogida
     *�Detalles Acad�micos

       LOCAL aParams := {}                  // Parametros Generales
*>>>>FIN DECLARACION PARAMETROS GENERALES

*>>>>LECTURA PARAMETROS GENERALES
       aParams := aParams(aParam1,aParam2,aParam3)
       IF  !lParam0101(aParams,;
		       @lShared,@nModCry,@cCodSui,@cNomSis,;
		       @cEmpPal,@cNitEmp,@cNomEmp,@cNomSec,@cCodEmp,;
		       @cNomUsr,@cAnoUsr,@cAnoSis,@cPatSis,;
		       @nFilPal,@nFilInf,@nColInf,;
		       @PathW01,@PathW02,@PathW03,@PathW04,@PathW05,;
		       @PathW06,@PathW07,@PathW08,@PathW09,@PathW10,;
		       @PathUno,@PathDos,@PathTre,@PathCua,;
		       @cMaeAlu,@cMaeAct,@cJorTxt)
	  CloseAll()
	  RETURN NIL
       ENDIF
       CloseAll()
*>>>>FIN LECTURA PARAMETROS GENERALES

*>>>>SELECION DEL A�O ANTERIOR
       cAnoAnt := STR((VAL(cAnoUsr)-1),4)
       cPatAnt := cPatSis
       cPatAnt := STUFF(cPatAnt,1,4,cAnoAnt)
       cPatAnt := PathUno+'\'+cPatAnt
       PathAlu := PathUno+'\'+cPatSis
*>>>>FIN SELECION DEL A�O ANTERIOR

*>>>>SELECCION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,PathUno+'\'+PathSis+'\'+;
		       fSimaCo,'SCO',NIL,lShared)                     .OR.;
	  !lUseDbf(.T.,PathSis+'\'+fSimMtr,'MTR',NIL,lShared)         .OR.;
	  !lUseDbf(.T.,PathSis+'\'+FilePrn,'PRN',NIL,lShared)         .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileNiv+cAnoSis+ExtFile,'NIV',NIL,lShared)     .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+;
		       fMtrAno+cAnoUsr+ExtFile,'ANO',NIL,lShared)     .OR.;
	  !lUseDbf(.T.,PathUno+'\'+PathSis+'\'+;
		       FilePer,'PER',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+FileCnt+ExtFile,'CNT',NIL,lShared) .OR.;
	  !lUseMaeMtr(lShared,cPatAnt,cMaeAlu,SUBS(cAnoAnt,3,2),lAnoAct)
	  cError('ABRIENDO ARCHIVOS EN MATRICULAS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN SELECCION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       DO CASE
       CASE SCO->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION GENERAL')

       CASE MTR->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION DEL SISTEMA')

       CASE PRN->(RECCOUNT()) == 0
	    cError('NO EXISTEN IMPRESIORAS GRABADAS')

       CASE NIV->(RECCOUNT()) == 0
	    cError('NO EXISTEN LOS NIVELES GRABADOS')

       CASE ANO->nTipMtrAno == 0
	    cError('NO SE HA ESPECIFICADO EL NUMERO DE HOJA DE MATRICULA')

       CASE 'JAA' $ cMaeAlu .AND. JAA->(RECCOUNT()) == 0
	    cError('No Existen Estudiantes de Jardin para '+cAnoAnt)

       CASE 'PRA' $ cMaeAlu .AND. PRA->(RECCOUNT()) == 0
	    cError('No Existen Estudiantes de Primaria para '+cAnoAnt)

       CASE 'BTA' $ cMaeAlu .AND. BTA->(RECCOUNT()) == 0
	    cError('No Existen Estudiantes de Bachillerato para '+cAnoAnt)

       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>LOCALIZACION DE LA IMPRESORA
       IF !lLocCodigo('nCodigoPrn','PRN',MTR->nCodigoPrn)
	  cError('NO EXISTE LA IMPRESORA QUE ESTA HABILITADA')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       IF lPregunta('DESEA CAMBIAR LA IMPRESORA POR DEFECTO:'+;
		     ALLTRIM(PRN->cNombrePrn)+'? No Si')
	  IF nSelPrn(nFilInf,nColInf+1) == 0
	     CLOSE ALL
	     RETURN NIL
	  ENDIF
	  IF !lPregunta('ESCOGIO LA IMPRESORA:'+ALLTRIM(PRN->cNombrePrn)+'. '+;
		       'DESEA CONTINUAR? Si No')
	     CLOSE ALL
	     RETURN NIL
	  ENDIF
       ENDIF
*>>>>FIN LOCALIZACION DE LA IMPRESORA

*>>>>LECTURA DEL NUMERO DE LINEAS
       cSavPan := SAVESCREEN(0,0,24,79)
       nLenLin := 150
       @ nFilInf+1,nColInf SAY 'CARACTERES POR LINEA';
			   GET nLenLin PICT '999'

       READ
       IF nLenLin == 0
	  CloseAll()
	  RETURN NIL
       ENDIF
       RESTSCREEN(0,0,24,79,cSavPan)
*>>>>FIN LECTURA DEL NUMERO DE LINEAS

*>>>>LECTURA DEL NUMERO DEL DOCUMENTO
       cSavPan := SAVESCREEN(0,0,24,79)
       SET CURSOR ON
       TitBuscar(18,@nNroFil,@nNroCol,'DIGITOS DE DOS EN DOS:',;
				      'DOCUMENTOS A IMPRIMIR')
       cDocume := SPACE(20)
       @ nNroFil,nNroCol GET cDocume PICT '99999999999999999999'
       READ
       IF EMPTY(cDocume) .OR. !lPar(LEN(ALLTRIM(cDocume)))
	  RESTSCREEN(0,0,24,79,cSavPan)
	  CloseAll()
	  RETURN NIL
       ENDIF
       RESTSCREEN(0,0,24,79,cSavPan)
*>>>>FIN LECTURA DEL NUMERO DEL DOCUMENTO

*>>>>VALIDACION DE LOS DOCUMENTOS
       FOR i := 1 TO LEN(ALLTRIM(cDocume))/2

	   nNroDoc := VAL(SUBS(cDocume,i*2-1,2))
	   cNroDoc := STR(nNroDoc,3)
	   lCorrecion(@cNroDoc)

	   IF !FILE("Bases\Docum"+cNroDoc+'.Doc')
	      cError('EL DOCUMENTO NO EXISTE',;
		     'Bases\Docum'+cNroDoc+'.Doc')
	      CloseAll()
	      RETURN NIL
	   ENDIF

       ENDFOR
*>>>>FIN VALIDACION DE LOS DOCUMENTOS

*>>>>LECTURA DEL NUMERO DE COPIAS
       TitBuscar(18,@nNroFil,@nNroCol,'COPIAS','COPIAS')
       nNroCop := 1
       @ nNroFil,nNroCol GET nNroCop PICT "99"
       READ
       IF EMPTY(nNroCop)
	  CloseAll()
	  RETURN NIL
       ENDIF
       RESTSCREEN(0,0,24,79,cSavPan)
*>>>>FIN LECTURA DEL NUMERO DE COPIAS

*>>>>EDICION DEL DOCUMENTO
       cTxtUno := 'Inserte el texto: (FECHA) => '+;
		   'para incluir la Fecha del Sistema'+;
		   CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (PADRE) => '+;
		    'para incluir el nombre del padre'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (PADNIT) => '+;
		    'para incluir la c�dula del Padre'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (PADLUG) => '+;
		    'para incluir el lugar de la c�dula del Padre'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (MADRE) => '+;
		    'para incluir el nombre de la madre'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (MADNIT) => '+;
		    'para incluir la c�dula de la Madre'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (MADLUG) => '+;
		    'para incluir el lugar de la c�dula de la Madre'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (ACUDIENTE) => '+;
		    'para incluir el nombre del acudiente'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (ACUNIT) => '+;
		    'para incluir la c�dula del Acudiente'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (ACULUG) => '+;
		    'para incluir el lugar de la c�dula del Acudiente'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (ESTUDIANTE) => '+;
		    'para incluir el nombre del Estudiante'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (CODIGO) => '+;
		    'para incluir el c�digo del Estudiante'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (GRUPO) => '+;
		    'para incluir el grupo del Estudiante'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (NIVEL) => '+;
		    'para incluir el Nivel o Grado del Estudiante'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR1)  => '+;
		    'para incluir el valor1'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR2)  => '+;
		    'para incluir el valor2'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR3)  => '+;
		    'para incluir el valor3'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR4)  => '+;
		    'para incluir el valor4'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR5)  => '+;
		    'para incluir el valor5'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR6)  => '+;
		    'para incluir el valor6'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR7)  => '+;
		    'para incluir el valor7'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR8)  => '+;
		    'para incluir el valor8'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR9)  => '+;
		    'para incluir el valor9'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR10)  => '+;
		    'para incluir el valor10'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)

       cTxtDos := 'Inserte el texto: (VALOR11)  => '+;
		    'para incluir el valor11'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR12)  => '+;
		    'para incluir el valor12'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR13)  => '+;
		    'para incluir el valor13'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR14)  => '+;
		    'para incluir el valor14'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR15)  => '+;
		    'para incluir el valor15'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR16)  => '+;
		    'para incluir el valor16'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR17)  => '+;
		    'para incluir el valor17'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR18)  => '+;
		    'para incluir el valor18'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR19)  => '+;
		    'para incluir el valor19'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (VALOR20)  => '+;
		    'para incluir el valor20'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)

       cTxtTre := 'Inserte el texto: (NOMGER)  => '+;
		    'para incluir el Nombre del Rector'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (DOCGER)  => '+;
		    'para incluir la c�dula del Rector'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (C1DOC)  => '+;
		    'para incluir la c�dula del Contratante No. 1'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (C1LUG)  => '+;
		    'para incluir e lugar de la c�dula del Contratante No. 1'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (C2DOC)  => '+;
		    'para incluir la c�dula del Contratante No. 2'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (C2LUG)  => '+;
		    'para incluir e lugar de la c�dula del Contratante No. 2'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (C3DOC)  => '+;
		    'para incluir la c�dula del Contratante No. 3'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (C3LUG)  => '+;
		    'para incluir e lugar de la c�dula del Contratante No. 3'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (C4DOC)  => '+;
		    'para incluir la c�dula del Contratante No. 4'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: (C4LUG)  => '+;
		    'para incluir e lugar de la c�dula del Contratante No. 4'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)

       cTxtCua := 'Inserte el texto: <JI>  => '+;
		    'para justificar el texto a la izquierda'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: <JD>  => '+;
		    'para justificar el texto a la derecha'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: <JF>  => '+;
		    'para selecionar justificaci�n completa'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: <T1>  => '+;
		    'Tama�o de letra No. 1 : PICA(10cpi)'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: <T2>  => '+;
		    'Tama�o de letra No. 2 : ELITE(12cpi)'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: <C1>  => '+;
		    'Activar impresi�n Condensado'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)+;
		  'Inserte el texto: <C0>  => '+;
		    'Desactivar impresi�n Condensado'+;
		    CHR(13)+CHR(10)+CHR(13)+CHR(10)

       cTxtHlp := cTxtUno+cTxtDos+cTxtTre+cTxtCua
       cTitHlp := 'TEXTOS PARA LA SUSTITUCION'

       CamSusPrn(@aMezIni)
       AADD(aMezIni,{'<SLP>',{||Eject()}})

       AADD(aMezIni,{'(YEAR)',cAnoUsr})
       AADD(aMezIni,{'(FECHA)',cFecha(DATE(),10)})

       lOficio := !lPregunta('DESEA IMPRIMIR EN TAMANO CARTA? Si No')
       IF !lPregunta('VA A IMPRIMIR EN TAMA�O '+IF(lOficio,'OFICIO','CARTA')+;
		     ' .DESEA CONTINUAR? No Si')
	  RESTSCREEN(0,0,24,79,cSavPan)
	  RETURN NIL
       ENDIF
       RESTSCREEN(0,0,24,79,cSavPan)
*>>>>FIN EDICION DEL DOCUMENTO

*>>>>ACTIVACION DE LA IMPRESORA
       IF MTR->lPrnArcMtr
	  SET DEVICE TO PRINT
       ELSE
	  FilePrn := 'Docum'+cNroDoc
	  nOpcPrn := nPrinter_On(cNomUsr,@FilePrn,MTR->cOpcPrnMtr,.F.,.F.)
	  IF EMPTY(nOpcPrn)
	      RETURN NIL
	  ENDIF
       ENDIF
       SendCodes(PCL('Reset'))

       SET DEVICE TO SCREEN
*>>>>FIN ACTIVACION DE LA IMPRESORA

*>>>>IMPRESION DE LOS ENCABEZADOS
       nNroFil := nMarco(nFilPal+1,'POR FAVOR ENTRE LOS CODIGOS DE LOS '+;
				   'ESTUDIANTES',22,'�')
       nNroCol := 1
       aTamCol := {06,40,06,06}
       aTitulo := {'CODIGO','NOMBRE','GRUPO ','REPITE'}

       cMsgTxt := cRegPrint(aTitulo,aTamCol,@aNroCol)
       @ nNroFil,nNroCol SAY cMsgTxt
*>>>>FIN IMPRESION DE LOS ENCABEZADOS

*>>>>GRABACION DE LOS REGISTROS
       nNroFil++
       bCodBlo  := {||Eject()}
       DO WHILE .T.

**********LECTURA DEL CODIGO
	    SET DEVICE TO SCREEN
	    cSavPan := SAVESCREEN(nNroFil,0,nNroFil,79)

	    cCodigoTes := cLeeCodEst(nNroFil,aNroCol[1],;
				     cMaeAct,@cMaeAct)

	    IF LASTKEY() == K_ESC .OR. EMPTY(cCodigoTes) .OR.;
	       EMPTY(cMaeAct)
	       EXIT
	    ENDIF
	    cNombreTes := ALLTRIM(RTRIM(&cMaeAct->cApelliEst)+' '+;
				  &cMaeAct->cNombreEst)

	    @ nNroFil,aNroCol[2] SAY cNombreTes
	    @ nNroFil,aNroCol[3] SAY SUBS(&cMaeAct->cCodigoGru,1,2)
	    @ nNroFil,aNroCol[4] SAY IF(&cMaeAct->lRepiteEst,'SI','NO')
**********FIN LECTURA DEL CODIGO

**********VALIDACION DEL CODIGO
	    DO CASE
	    CASE !(&cMaeAct->lSiCupoEst)
		 cError(cNombreTes+' NO TIENE CUPO')
		 LOOP

	    CASE SUBS(&cMaeAct->cCodigoGru,1,2) = '11' .AND. !&cMaeAct->lRepiteEst
		 cError(cNombreTes+' ES BACHILLER')
		 LOOP
	    ENDCASE
**********FIN VALIDACION DEL CODIGO

**********ANALISIS DEL SIGUIENTE NIVEL
	    IF &cMaeAct->lRepiteEst

*--------------REPITE EL NIVEL
		 IF SUBS(&cMaeAct->cCodigoGru,1,2) == '17'
		    cCodigoTgr := '00'+SUBS(&cMaeAct->cCodigoGru,3,2)
		 ELSE
		    cCodigoTgr := &cMaeAct->cCodigoGru
		 ENDIF
*--------------FIN REPITE EL NIVEL

	    ELSE

*--------------PROMOCION AL SIGUIENTE NIVEL
		 IF SUBS(&cMaeAct->cCodigoGru,1,2) == '16'
		    cCodigoTgr := '00'
		 ELSE
		    IF SUBS(&cMaeAct->cCodigoGru,1,2) == '17'
		       cCodigoTgr := '01'
		    ELSE
		       cCodigoTgr := STR(VAL(SUBS(&cMaeAct->cCodigoGru,1,2))+1,2)
		    ENDIF
		 ENDIF
		 lCorrecion(@cCodigoTgr)
		 cCodigoTgr += SUBS(&cMaeAct->cCodigoGru,3,2)
*--------------FIN PROMOCION AL SIGUIENTE NIVEL

	    ENDIF
**********FIN ANALISIS DEL SIGUIENTE NIVEL

**********SELECCION DE LAS AREAS DE TRABAJO
	    cCodigoTni := SUBS(cCodigoTgr,1,2)
	    cNalias := cMaeActNiv(cCodigoTni)
	    IF cNalias # cMaeAct
	       IF !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cNalias+'\'+;
			       FileNiv+cAnoSis+ExtFile,'NIV',NIL,lShared)
		  cError('ABRIENDO EL ARCHIVO DE NIVELES')
	       ENDIF
	    ENDIF
**********FIN SELECCION DE LAS AREAS DE TRABAJO

**********LOCALIZACION DEL NIVEL
	    IF !lLocCodigo('cCodigoNiv','NIV',cCodigoTni)
	       cError('EL NIVEL '+cCodigoTni+' NO EXISTE')
	       LOOP
	    ENDIF
**********FIN LOCALIZACION DEL NIVEL

**********CAMPOS A SUSTITUIR EN EL DOCUMENTO
	    aDatCnt := aLocAllCnt(cMaeAct)

	    cNombreTes := RTRIM(&cMaeAct->cNombreEst)+' '+;
			  RTRIM(&cMaeAct->cApelliEst)

	    aMezcla := {}
	    AADD(aMezcla,{'(FECHA)' ,cFecha(DATE(),10)})

	    AADD(aMezcla,{'(PADRE)' ,ALLTRIM(&cMaeAct->cNomPadEst)+' '+;
				     ALLTRIM(&cMaeAct->cApePadEst)})
	    AADD(aMezcla,{'(PADNIT)',ALLTRIM(&cMaeAct->cPadNitEst)})
	    AADD(aMezcla,{'(PADLUG)',ALLTRIM(&cMaeAct->cPadLugEst)})

	    AADD(aMezcla,{'(MADRE)' ,ALLTRIM(&cMaeAct->cNomMadEst)+' '+;
				     ALLTRIM(&cMaeAct->cApeMadEst)})
	    AADD(aMezcla,{'(MADNIT)',ALLTRIM(&cMaeAct->cMadNitEst)})
	    AADD(aMezcla,{'(MADLUG)',ALLTRIM(&cMaeAct->cMadLugEst)})

	    AADD(aMezcla,{'(ACUDIENTE)',ALLTRIM(&cMaeAct->cNomAcuEst)+' '+;
					ALLTRIM(&cMaeAct->cApeAcuEst)})
	    AADD(aMezcla,{'(ACUNIT)',ALLTRIM(&cMaeAct->cAcuNitEst)})
	    AADD(aMezcla,{'(ACULUG)',ALLTRIM(&cMaeAct->cAcuLugEst)})

	    AADD(aMezcla,{'(ESTUDIANTE)',cNombreTes})
	    AADD(aMezcla,{'(CODIGO)',&cMaeAct->cCodigoEst})

	    AADD(aMezcla,{'(GRUPO)',&cMaeAct->cCodigoGru})
	    AADD(aMezcla,{'(NIVEL)',cNomNiv(SUBS(cCodigoTgr,1,2))})

	    AADD(aMezcla,{'(NOMGER)',ALLTRIM(SCO->cGerentCon)})
	    AADD(aMezcla,{'(DOCGER)',ALLTRIM(SCO->cNitGerCon)})

	    AADD(aMezcla,{'(C1NOM)',cLocateCnt(cMaeAct,'C1NOM',aDatCnt)})
	    AADD(aMezcla,{'(C1DOC)',cLocateCnt(cMaeAct,'C1DOC',aDatCnt)})
	    AADD(aMezcla,{'(C1LUG)',cLocateCnt(cMaeAct,'C1LUG',aDatCnt)})
	    AADD(aMezcla,{'(C1TIP)',cLocateCnt(cMaeAct,'C1TIP',aDatCnt)})

	    AADD(aMezcla,{'(C2NOM)',cLocateCnt(cMaeAct,'C2NOM',aDatCnt)})
	    AADD(aMezcla,{'(C2DOC)',cLocateCnt(cMaeAct,'C2DOC',aDatCnt)})
	    AADD(aMezcla,{'(C2LUG)',cLocateCnt(cMaeAct,'C2LUG',aDatCnt)})
	    AADD(aMezcla,{'(C2TIP)',cLocateCnt(cMaeAct,'C2TIP',aDatCnt)})

	    AADD(aMezcla,{'(C3NOM)',cLocateCnt(cMaeAct,'C3NOM',aDatCnt)})
	    AADD(aMezcla,{'(C3DOC)',cLocateCnt(cMaeAct,'C3DOC',aDatCnt)})
	    AADD(aMezcla,{'(C3LUG)',cLocateCnt(cMaeAct,'C3LUG',aDatCnt)})
	    AADD(aMezcla,{'(C3TIP)',cLocateCnt(cMaeAct,'C3TIP',aDatCnt)})

	    AADD(aMezcla,{'(C4NOM)',cLocateCnt(cMaeAct,'C4NOM',aDatCnt)})
	    AADD(aMezcla,{'(C4DOC)',cLocateCnt(cMaeAct,'C4DOC',aDatCnt)})
	    AADD(aMezcla,{'(C4LUG)',cLocateCnt(cMaeAct,'C4LUG',aDatCnt)})
	    AADD(aMezcla,{'(C4TIP)',cLocateCnt(cMaeAct,'C4TIP',aDatCnt)})

	    AADD(aMezcla,{'(VALOR1)',;
			    ALLTRIM(TRANS(NIV->nVlrUnoNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR2)',;
			    ALLTRIM(TRANS(NIV->nVlrDosNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR3)',;
			    ALLTRIM(TRANS(NIV->nVlrTreNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR4)',;
			    ALLTRIM(TRANS(NIV->nVlrCuaNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR5)',;
			    ALLTRIM(TRANS(NIV->nVlrCinNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR6)',;
			    ALLTRIM(TRANS(NIV->nVlrSeiNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR7)',;
			    ALLTRIM(TRANS(NIV->nVlrSieNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR8)',;
			    ALLTRIM(TRANS(NIV->nVlrOchNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR9)',;
			    ALLTRIM(TRANS(NIV->nVlrNueNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR10)',;
			    ALLTRIM(TRANS(NIV->nVlrDieNiv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR11)',;
			    ALLTRIM(TRANS(NIV->nVlr011Niv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR12)',;
			    ALLTRIM(TRANS(NIV->nVlr012Niv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR13)',;
			    ALLTRIM(TRANS(NIV->nVlr013Niv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR14)',;
			    ALLTRIM(TRANS(NIV->nVlr014Niv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR15)',;
			    ALLTRIM(TRANS(NIV->nVlr015Niv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR16)',;
			    ALLTRIM(TRANS(NIV->nVlr016Niv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR17)',;
			    ALLTRIM(TRANS(NIV->nVlr017Niv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR18)',;
			    ALLTRIM(TRANS(NIV->nVlr018Niv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR19)',;
			    ALLTRIM(TRANS(NIV->nVlr019Niv,'####,###.##'))})
	    AADD(aMezcla,{'(VALOR20)',;
			    ALLTRIM(TRANS(NIV->nVlr020Niv,'####,###.##'))})

	    AADD(aMezcla,{'<SLP>',bCodBlo})
	    AADD(aMezcla,{'<T1>',PCL({'Pica'})})
	    AADD(aMezcla,{'<T2>',PCL({'Elite'})})
	    AADD(aMezcla,{'<C1>',PCL({'CondenOn'})})
	    AADD(aMezcla,{'<C0>',PCL({'CondenOf'})})

	    FOR i := 1 TO LEN(aMezIni)
		AADD(aMezcla,{aMezIni[i,1],aMezIni[i,2]})
	    NEXT
**********FIN CAMPOS A SUSTITUIR EN EL DOCUMENTO

/*
**********FIRMAS DEL CONTRATANTE
	    aFirmas := {{cLocateCnt(cMaeAct,'C1NOM'),;
				    REPL('_',20)+'DE'+REPL('_',20),;
				    'NOMBRE:'+REPL('_',44)},;
			{cLocateCnt(cMaeAct,'C2NOM'),;
				    '_________DE____',;
				    'NOMBRE:___________________'}}

	    AADD(aMezcla,{'(FIRCNT)',{||Firmas(PCL('n17Stan'),4,aFirmas)}})
**********FIN FIRMAS DEL CONTRATANTE
*/

**********FIRMAS DE LOS CODEUDORES

**********FIN FIRMAS DE LOS CODEUDORES

**********PREGUNTA DE DECISION
	    IF lPregunta('DESEA IMPRIMIR EL DOCUMENTO? Si No',21)

	       SET DEVICE TO PRINT
	       FOR i := 1 TO nNroCop

		   FOR j := 1 TO LEN(ALLTRIM(cDocume))/2
		       nNroDoc := VAL(SUBS(cDocume,j*2-1,2))
		       cNroDoc := STR(nNroDoc,3)
		       lCorrecion(@cNroDoc)
		       PrnFileDoc("Bases\Docum"+cNroDoc+'.Doc',;
				  aMezcla,nLenLin,NIL,NIL,lOficio)
		   ENDFOR

	       ENDFOR
	       SET DEVICE TO SCREEN
	    ENDIF
**********FIN PREGUNTA DE DECISION

**********INCREMENTO DE LAS FILAS
	    nNroFil++
	    IF nNroFil > 19

*--------------IMPRESION DEL ULTIMO REGISTRO
		 nNroFil := nMarco(nFilPal+1,'POR FAVOR ENTRE LOS CODIGOS '+;
					     'DE LOS ESTUDIANTES',22,'�')
		 @ nNroFil,nNroCol SAY cMsgTxt

		 nNroFil++
		 SET COLOR TO I
		 @ nNroFil,aNroCol[1] SAY cCodigoTes
		 @ nNroFil,aNroCol[2] SAY cNombreTes
		 @ nNroFil,aNroCol[3] SAY SUBS(&cMaeAct->cCodigoGru,1,2)
		 @ nNroFil,aNroCol[4] SAY IF(&cMaeAct->lRepiteEst,'SI','NO')
		 SET COLOR TO

		 nNroFil++
*--------------FIN IMPRESION DEL ULTIMO REGISTRO

	    ENDIF
**********FIN INCREMENTO DE LAS FILAS

       ENDDO
       VerPrn(nOpcPrn,FilePrn)
       CloseAll()
       RETURN NIL
*>>>>FIN GRABACION DE LOS REGISTROS
