/*SISTEMA DE MATRICULA ACADEMICA

MODULO      : MATRICULAS
SUBMODULO...: LISTAS DE CLASE     MODULO No. 692

**************************************************************************
* TITULO..: LISTAS DE CLASE PROVISIONALES POR NIVELES                    *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: DIC 06/1994 MAR A
       Bucaramanga, Colombia	    INICIO: 10:30 AM   DIC 06/1994 MAR

OBJETIVOS:

1- Debe estar en uso el archivo de nuevos indexado por grupo y apellido

2- Permite imprimir las listas de clase provisionales por niveles

3- Retorna Nil

SINTAXIS:

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Matri_692(aParam1,aParam2,aParam3,;
		   cOpcSys,nNroIso,cCodIso,aTitIso,;
		   cPiePag,aPieIso,nTotPie,aMezIso,;
		   bInsIso)

*>>>>DESCRIPCION DE PARAMETROS
/*     aParam1                              // Parametros Generales
       aParam2                              // Parametros Generales
       aParam3                              // Parametros Generales
       cOpcSys                              // Opci�n del Sistema
       nNroIso                              // N�mero Iso del Informe
       cCodIso                              // C�digo Iso del Informe
       aTitIso                              // T�tulo Iso del Informe
       cPiePag                              // Pie de P�gina por defecto
       aPieIso      		            // Textos del pie de p�gina
       nTotPie                              // Total de Pie de p�ginas
       aMezIso                              // Campos a Mesclar
       bInsIso                              // Block de Gestion Documental */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "ARC-MATR.PRG"      // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL lHayErr := .F.                 // .T. Hay Error
       LOCAL lAnoAct := .T.                 // A�o Actual

       LOCAL nCopias := 1                   // N�meros de copias a imprimir
       LOCAL nNroCop := 1                   // N�mero de copia
       LOCAL nRegIni := 0                   // Registro inicial
       LOCAL nRegFin := 0                   // Registro Final
       LOCAL nGruIni := 0                   // Grupo Inicial
       LOCAL cCodGru := ''                  // C�dgio del grupo

       LOCAL GetList := {}                  // Variable del Sistema

       FIELD cCodigoGru                     // C�digo del Grupo
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
	  RETURN NIL
       ENDIF
*>>>>FIN LECTURA PARAMETROS GENERALES

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !lUseNiv(lShared,PathUno+'\'+cPatSis,cMaeAct,cAnoSis,lAnoAct)
	  cError('ABRIENDO EL ARCHIVOS')
	  CloseDbf('NIV')
	  RETURN NIL
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       DO CASE
       CASE NIV->(RECCOUNT()) == 0
	    cError('NO ESTAN GRABADOS LOS NIVELES')

       OTHERWISE
	    lHayErr :=.F.
       ENDCASE

       IF lHayErr
	  Closedbf('NIV')
	  RETURN NIL
       ENDIF
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>CAPTURA DE LOS NIVELES POR INTERVALO
       IF !lInterNiv(nFilInf+1,nColInf,@nRegIni,@nRegFin)
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN CAPTURA DE LOS NIVELES POR INTERVALO

*>>>>LECTURA DEL NUMERO DE COPIAS
       @ nFilInf+3,nColInf SAY 'NRO COPIAS   :' GET nCopias PICT "99"
       READ
       IF nCopias == 0
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN LECTURA DEL NUMERO DE COPIAS

*>>>>IMPRESION DE N COPIAS
       nNroCop := 1
       DO WHILE nNroCop <= nCopias

**********IMPRESION POR NIVELES
	    SELECT NIV
	    GO nRegIni
	    DO WHILE RECNO() <= nRegFin

*==============IMPRESION DE LOS GRUPOS
		 nGruIni := 0
		 DO WHILE nGruIni <= NIV->nNroGruNiv

*-------------------FILTRACION DEL MAESTRO
		      cCodGru := NIV->cCodigoNiv+'0'+ALLTRIM(STR(nGruIni,1))
		      SELECT &cMaeAct
		      SET FILTER TO cCodigoGru == cCodGru
		      GO TOP
		      IF EOF()
*			 cError('NO HAY ALUMNOS PARA EL GRUPO '+cCodGru)
			 nGruIni++
			 LOOP
		      ENDIF
*-------------------FIN FILTRACION DEL MAESTRO

*-------------------IMPRESION DE LA LINEA DE ESTADO
		      LineaEstado('IMPRIMIENDO EL NIVEL: '+NIV->cCodigoNiv+;
			     '�GRUPO: '+cCodGRu+;
			     '�NRO COPIAS:'+STR(nNroCop,2)+' DE:'+;
			     STR(nCopias,2)+'�POR FAVOR ESPERE ...',cNomSis)
*-------------------FIN IMPRESION DE LA LINEA DE ESTADO

*-------------------LISTAS DE CLASE
		      DO CASE
		      CASE ANO->nTipLisAno == 0
			   ListaPrv00(cCodGru,cNomEmp,cNomSis,cNomUsr,;
				      cAnoUsr,cMaeAct,cJorTxt,cOpcSys,;
				      nNroIso,cCodIso,aTitIso,cPiePag,;
				      aPieIso,nTotPie,aMezIso,bInsIso)
			 *�Lista de Clase por defecto

		      CASE ANO->nTipLisAno == 1
			   ListaPrv01(cCodGru,cNomEmp,cNomSis,cNomUsr,;
				      cAnoUsr,cMaeAct,cJorTxt,cOpcSys,;
				      nNroIso,cCodIso,aTitIso,cPiePag,;
				      aPieIso,nTotPie,aMezIso,bInsIso)
			 *�Lista de Clase sin Encabezado

		      CASE ANO->nTipLisAno == 99
			   ListaPrv99(cCodGru,cNomEmp,cNomSis,cNomUsr,;
				      cAnoUsr,cMaeAct,cJorTxt,cOpcSys,;
				      nNroIso,cCodIso,aTitIso,cPiePag,;
				      aPieIso,nTotPie,aMezIso,bInsIso)
			 *�Lista de Clase por defecto

		      ENDCASE
*-------------------FIN LISTAS DE CLASE


		    nGruIni++

		    SET FILTER TO

		 ENDDO
*==============FIN IMPRESION DE LOS GRUPOS

	       SELECT NIV
	       SKIP

	    ENDDO
**********FIN IMPRESION POR NIVELES

	  nNroCop++

       ENDDO
       RETURN NIL
*>>>>FIN IMPRESION DE N COPIAS