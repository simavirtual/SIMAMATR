/*************************************************************************
* TITULO ..: FORMULARIO DE SOLICITUD DE CUPO No. 03                      *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: ENE 02/96 MAR A
       Colombia, Bucaramanga        INICIO: 02:00 PM   ENE 02/96 MAR

OBJETIVOS:

1- Imprime el formulario de la solicitud de cupo No. 03

2- Formulario dise�ado para el Instituto San Bernardo y Colegio la Merced

3- Retorna NIL

SINTAXIS:

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Mtr_001_03(cCodigo,cCodNiv,cNombre,cApelli,lExAlum,;
		    cNomSis,cNomEmp,cAnoUsr,lRapido,cObsUno,;
		    cObsDos,cObsTre,cJorTxt,cObsInc,;
		    aMarInf,cCodIso,aTitIso,aPieIso,aObsIso)

*>>>>PARAMETROS DE LA FUNCION
/*     cCodigo                              // C�digo de la Solicitud
       cCodNiv			            // C�digo del Nivel
       cNombre                              // Nombres del aspirante
       cApelli                              // Apellidos del aspirante
       lExAlum                              // .T. Ex-Alumno
       cNomSis                              // Nombre del Sistema
       cNomEmp                              // Nombre de la Empresa
       cAnoUsr                              // A�o del usuario
       lRapido                              // .T. Impresi�n r�pida
       cObsUno                              // Primera Observaci�n
       cObsDos                              // Segunda Observaci�n
       cObsTre                              // Tercera Observaci�n
       cJorTxt                              // Jornada escogida
       cObsInc                              // Observaci�n para la Inscripci�n
       aMarInf			            // Margenes del informe
       cCodIso                              // C�digo Iso del Informe
       aTitIso                              // T�tulo Iso del Informe
       aPieIso      		            // Textos del pie de p�gina
       aObsIso                              // Observaciones del informe */
*>>>>FIN PARAMETROS DE LA FUNCION

*>>>>DECLARACION DE VARIABLES
       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL aTitulo := {}                  // Titulos de las Columnas
       LOCAL aTamCol := {}                  // Tama�o de las Columnas
       LOCAL aNroCol := {}                  // N�meros de Columnas
       LOCAL cTxtPrn := ''                  // Texto de impresi�n
       LOCAL nLenPrn := 0                   // Longitud de impresi�n
*>>>>FIN DECLARACION DE VARIABLES

*>>>>ANALISIS SI EL CANDIDATO ESTA RETIRADO
       IF ADM->lRetiroAdm
	  cError(ALLTRIM(ADM->cApelliEst)+' '+ALLTRIM(ADM->cNombreEst)+;
		 ' ESTA RETIRADO')
	  RETURN NIL
       ENDIF
*>>>>FIN ANALISIS SI EL CANDIDATO ESTA RETIRADO

*>>>>ACTIVACION DE LA IMPRESORA
       cSavPan := SAVESCREEN(0,0,24,79)
       Termometro(0,"IMPRIMIENDO LA INSCRIPCION")
       Termometro(50)

       DO WHILE !Printer_on(MTR->lPrnArcMtr); ENDDO

       SendCodes(PCL({'Reset','Carta',;
		      'GraphiOn','CondenOf','ProporOf','DraftOn'}))

       lRapido := IF(lRapido == NIL,.T.,lRapido)
       IF !lRapido
	  SendCodes(PCL({'NegraOn','DobGolOn'}))
       ENDIF
       nLenPrn := PCL('n12Stan')
*>>>>FIN ACTIVACION DE LA IMPRESORA

*>>>>ENCABEZADO DEL FORMULARIO
       SendCodes(PCL('Elite'))
       SendCodes(PCL({'DobAncOn','DobAltOn'}))
       cTxtPrn := 'SOLICITUD DE CUPO NUMERO: '+cCodigo
       @ PROW()+aMarInf[1,1],((nLenPrn/2)-LEN(cTxtPrn))/2 SAY cTxtPrn
       SendCodes(PCL({'DobAncOf','DobAltOf','Elite'}))

       cTxtPrn := 'NIVEL '+cNomNiv(cCodNiv)+' '+cAnoUsr+'. JORNADA: '+cJorTxt
       @ PROW()+1,(nLenPrn-LEN(cTxtPrn))/2 SAY cTxtPrn
*>>>>FIN ENCABEZADO DEL FORMULARIO

*>>>>CODIGO ISO DEL FORMULARIO
       IF !EMPTY(cCodIso)
	  SendCodes(PCL({'CondenOn'}))
	  @ PROW()+1,02 SAY cCodIso
	  SendCodes(PCL({'CondenOf','Elite'}))
	  @ PROW()+1,01 SAY 'FECHA DE SOLICITUD: '+ALLTRIM(cFecha(DATE()))
       ELSE
	  @ PROW()+2,01 SAY 'FECHA DE SOLICITUD: '+ALLTRIM(cFecha(DATE()))
       ENDIF
*>>>>FIN CODIGO ISO DEL FORMULARIO

*>>>>DETALLES FAMILIARES
       @ PROW()+1,01 SAY 'NOMBRE: '+RTRIM(cApelli)+' '+cNombre+' '+;
			 'SEXO: M__ F__'

       @ PROW()+1,01 SAY ''
       ForPrnEst()

       @ PROW()+1,01 SAY ''
       ForPrnPad()

       @ PROW()+1,01 SAY ''
       ForPrnMad()

       @ PROW()+1,01 SAY ''
       ForPrnAcu()
*>>>>FIN DETALLES FAMILIARES

*>>>>COLEGIOS DE PROCEDENCIA
       LineaPrn({SPACE(35)+'COLEGIOS DE PROCEDENCIA'},1)
       LineaPrn({'PREESCOLAR:'},1)
       LineaPrn({'A�O'        ,SPACE(02),'NIVEL'      ,SPACE(10),;
		 'INSTITUCION',SPACE(30),'CIUDAD'},1)
       LineaPrn({REPL('_',04),SPACE(01),REPL('_',15),SPACE(00),;
		 REPL('_',40),SPACE(01),REPL('_',23)},1)
       LineaPrn({'PRIMARIA:'},1)
       LineaPrn({REPL('_',04),SPACE(01),REPL('_',15),SPACE(00),;
		 REPL('_',40),SPACE(01),REPL('_',23)},1)
       LineaPrn({'BACHILLERATO:'},1)
       LineaPrn({REPL('_',04),SPACE(01),REPL('_',15),SPACE(00),;
		 REPL('_',40),SPACE(01),REPL('_',23)},1)
       LineaPrn({REPL('_',04),SPACE(01),REPL('_',15),SPACE(00),;
		 REPL('_',40),SPACE(01),REPL('_',23)},1)
       LineaPrn({REPL('_',04),SPACE(01),REPL('_',15),SPACE(00),;
		 REPL('_',40),SPACE(01),REPL('_',23)},1)
       LineaPrn({REPL('_',04),SPACE(01),REPL('_',15),SPACE(00),;
		 REPL('_',40),SPACE(01),REPL('_',23)},1)
       LineaPrn({REPL('_',04),SPACE(01),REPL('_',15),SPACE(00),;
		 REPL('_',40),SPACE(01),REPL('_',23)},1)
       LineaPrn({REPL('_',04),SPACE(01),REPL('_',15),SPACE(00),;
		 REPL('_',40),SPACE(01),REPL('_',23)},1)
       LineaPrn({REPL('_',04),SPACE(01),REPL('_',15),SPACE(00),;
		 REPL('_',40),SPACE(01),REPL('_',23)},1)
*>>>>FIN COLEGIOS DE PROCEDENCIA

*>>>>OBSERVACIONES
       IF EMPTY(cObsInc)
	  LineaPrn({'OBSERVACIONES: ',REPL('_',74)},2)
       ELSE
	  @ PROW()+2,00 SAY 'OBSERVACIONES: '
	  SendCodes(PCL('CondenOn'))
	  @ PROW(),25 SAY cObsInc
	  SendCodes(PCL('CondenOf'))
       ENDIF
       @ PROW()+1,01 SAY REPL('_',90)
       @ PROW()+1,01 SAY REPL('_',90)
*>>>>FIN OBSERVACIONES

*>>>>OBSERVACIONES ISO
       IF !EMPTY(aObsIso[1]+aObsIso[2]+aObsIso[3])
	  SendCodes(PCL({'NegraOf','CondenOn'}))
	  @ PROW()+2,01 SAY aObsIso[1]
	  @ PROW()+1,01 SAY aObsIso[2]
	  @ PROW()+1,01 SAY aObsIso[3]
       ENDIF
       EJECT
       SET DEVICE TO SCREEN
       Termometro(100)
       RESTSCREEN(0,0,24,79,cSavPan)
       RETURN NIL
*>>>>FIN OBSERVACIONES ISO