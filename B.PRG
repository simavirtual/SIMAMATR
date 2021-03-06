       AADD(aDbfAlu,{"cCodigoEst","Character",006,0}) // C�digo del Estudiante
       AADD(aDbfAlu,{"cCodEstEst","Character",007,0}) // C�digo del Estudiante
       AADD(aDbfAlu,{"cPasWorEst","Character",010,0}) // PassWord del Estudiante
       AADD(aDbfAlu,{"cUsrWwwEst","Character",012,0}) // Usuario del Internet
       AADD(aDbfAlu,{"cPasWwwEst","Character",010,0}) // PassWord del Internet
       AADD(aDbfAlu,{"lHayWwwEst","Logical"  ,001,0}) // .T. Acceso a Internet
       AADD(aDbfAlu,{"lRetiroEst","Logical"  ,001,0}) // .T. Retirado
       AADD(aDbfAlu,{"dFecRetEst","Date"     ,008,0}) // Fecha del retiro
       AADD(aDbfAlu,{"nPerRetEst","Numeric"  ,002,0}) // Periodo del Retiro
       AADD(aDbfAlu,{"nUltRecEst","Numeric"  ,002,0}) // Ultimo Recibo que se debe facturar
       AADD(aDbfAlu,{"nAproboNot","Numeric"  ,002,0}) // Indicardor de Aprobaci�n
       AADD(aDbfAlu,{"nAprAntNot","Numeric"  ,002,0}) // Indicardor de Aprobaci�n Anterior
       AADD(aDbfAlu,{"lRepiteEst","Logical"  ,001,0}) // .T. Repite a�o
       AADD(aDbfAlu,{"lSiCupoEst","Logical"  ,001,0}) // .T. Tiene Cupo
       AADD(aDbfAlu,{"lSiMatrEst","Logical"  ,001,0}) // .T. Si matriculado para sgte a�o
       AADD(aDbfAlu,{"dFecMatEst","Date"     ,008,0}) // Fecha de matricula
       AADD(aDbfAlu,{"cHorMatEst","Character",008,0}) // Hora de matricula
       AADD(aDbfAlu,{"lEstNewEst","Logical"  ,001,0}) // .T. Estudiante Nuevo .F. Estudiante Antiguo
       AADD(aDbfAlu,{"cFolFinEst","Character",012,0}) // Folios del libro final
       AADD(aDbfAlu,{"lGrupOkEst","Logical"  ,001,0}) // .T. Grupo OK no mezclar grupos
       AADD(aDbfAlu,{"cGruAntGru","Character",006,0}) // Codigo del grupo anterior
       AADD(aDbfAlu,{"cCodigoGru","Character",004,0}) // Codigo del grupo
       AADD(aDbfAlu,{"cAnoIngEst","Character",004,0}) // Codigo de Ingreso del Estudiante
       AADD(aDbfAlu,{"nNroLisEst","Numeric"  ,003,0}) // N�mero de lista.
       AADD(aDbfAlu,{"nMorosoEst","Numeric"  ,002,0}) // C�digo de Clasificaci�n de los morosos.
       AADD(aDbfAlu,{"nNomFacEst","Numeric"  ,001,0}) // C�digo del Nombre de la Factura 0=>Alumno 1=>Padre 2=>Madre 3=>Acudiente
       AADD(aDbfAlu,{"nEstratEst","Numeric"  ,002,0}) // Estatro del Estudiante

       AADD(aDbfAlu,{"cNomEpsEst","Character",030,0}) // Nombre de la Eps del Estudiante
       AADD(aDbfAlu,{"cGrupRhEst","Character",004,0}) // Nombre de la Eps del Estudiante

       AADD(aDbfAlu,{"cConcepEst","Character",016,0}) // Conceptos del Estudiante

       AADD(aDbfAlu,{"cCodigoRut","Character",010,0}) // Codigo de la Ruta
       AADD(aDbfAlu,{"cCodigoBus","Character",006,0}) // Codigo del Bus

       AADD(aDbfAlu,{"cBoletiEst","Character",010,0}) // Correci�n del Boletin. Dos caracteres por periodo
     *�Detalles Generales

       AADD(aDbfAlu,{"cApelliEst","Character",030,0}) // Apellido del Estudiante
       AADD(aDbfAlu,{"cNombreEst","Character",030,0}) // Nombre del Estudiante
       AADD(aDbfAlu,{"dFecNacEst","Date"     ,008,0}) // Fecha de nacimiento
       AADD(aDbfAlu,{"cLugNacEst","Character",020,0}) // Lugar de nacimiento
       AADD(aDbfAlu,{"cDocNitEst","Character",016,0}) // Documento de Identidad
       AADD(aDbfAlu,{"cLugNitEst","Character",020,0}) // Lugar del Documento
       AADD(aDbfAlu,{"cTipNitEst","Character",003,0}) // Tipo de Documento TI=>Tarjeta de Identidad CC=>Cedula de Ciudadania CE => Cedula de Extrajeria NI => Nit
       AADD(aDbfAlu,{"lFotNitEst","Logical"  ,001,0}) // .T. Fotocopia
       AADD(aDbfAlu,{"lSexFemEst","Logical"  ,001,0}) // .T. Sexo Femenino .F. Sexo Masculino
       AADD(aDbfAlu,{"cDireccEst","Character",040,0}) // Direccion de la casa
       AADD(aDbfAlu,{"cBarrioEst","Character",016,0}) // Barrio de la direcci�n
       AADD(aDbfAlu,{"cCiudadEst","Character",030,0}) // Ciudad del Estudiante
       AADD(aDbfAlu,{"cDepartEst","Character",030,0}) // Departamento del Estudiante
       AADD(aDbfAlu,{"cTelefnEst","Character",014,0}) // Telefono de la casa
       AADD(aDbfAlu,{"cTelCelEst","Character",014,0}) // *Telefono celular del Estudiante
       AADD(aDbfAlu,{"cMaiEstEst","Character",040,0}) // E-MAIL de la Casa o del Estudiante
       AADD(aDbfAlu,{"cParNitEst","Character",016,0}) // C�dula de Parentesco
       AADD(aDbfAlu,{"cCodFamEst","Character",060,0}) // C�digo de Estudiantes de los familiares
     *�Datos del Estudiante

       AADD(aDbfAlu,{"nTotFacEst","Numeric",12,2})    // Total Facturado
       AADD(aDbfAlu,{"nTotPagEst","Numeric",12,2})    // Total Pagado

       AADD(aDbfAlu,{"cNitCo1Est","Character",016,0}) // Cedula Contrante No.1
       AADD(aDbfAlu,{"cTipCo1Est","Character",001,0}) // Tipo de Documento
       AADD(aDbfAlu,{"lNoRCo1Est","Logical"  ,001,0}) // .T. No Reportar .F. Si Reportar
       AADD(aDbfAlu,{"cNitCo2Est","Character",016,0}) // Cedula Contrante No.2
       AADD(aDbfAlu,{"cTipCo2Est","Character",001,0}) // Tipo de Documento
       AADD(aDbfAlu,{"lNoRCo2Est","Logical"  ,001,0}) // .T. No Reportar .F. Si Reportar
       AADD(aDbfAlu,{"cNitCo3Est","Character",016,0}) // Cedula Contrante No.3
       AADD(aDbfAlu,{"cTipCo3Est","Character",001,0}) // Tipo de Documento
       AADD(aDbfAlu,{"lNoRCo3Est","Logical"  ,001,0}) // .T. No Reportar .F. Si Reportar
       AADD(aDbfAlu,{"cNitCo4Est","Character",016,0}) // Cedula Contrante No.4
       AADD(aDbfAlu,{"cTipCo4Est","Character",001,0}) // Tipo de Documento
       AADD(aDbfAlu,{"lNoRCo4Est","Logical"  ,001,0}) // .T. No Reportar .F. Si Reportar
     *�Datos de los Contrantes 1,2,3,4

       AADD(aDbfAlu,{"cAsiEneEst","Character",031,0}) // Asistencia de Enero
       AADD(aDbfAlu,{"cAsiFebEst","Character",031,0}) // Asistencia de Febrero
       AADD(aDbfAlu,{"cAsiMarEst","Character",031,0}) // Asistencia de Marzo
       AADD(aDbfAlu,{"cAsiAbrEst","Character",031,0}) // Asistencia de Abril
       AADD(aDbfAlu,{"cAsiMayEst","Character",031,0}) // Asistencia de Mayo
       AADD(aDbfAlu,{"cAsiJunEst","Character",031,0}) // Asistencia de Junio
       AADD(aDbfAlu,{"cAsiJulEst","Character",031,0}) // Asistencia de Julio
       AADD(aDbfAlu,{"cAsiAgoEst","Character",031,0}) // Asistencia de Agosto
       AADD(aDbfAlu,{"cAsiSepEst","Character",031,0}) // Asistencia de Septiembre
       AADD(aDbfAlu,{"cAsiOctEst","Character",031,0}) // Asistencia de Octubre
       AADD(aDbfAlu,{"cAsiNovEst","Character",031,0}) // Asistencia de Noviembre
       AADD(aDbfAlu,{"cAsiDicEst","Character",031,0}) // Asistencia de Diciembre
     *�Control de Asistencia del Estudiante

       AADD(aDbfAlu,{"lCarnetEst","Logical"  ,001,0}) // .T. Carnet Perdido .F. Carnet Activo

       AADD(aDbfAlu,{"cResEneEst","Character",031,0}) // Servicio de Restaurante de Enero
       AADD(aDbfAlu,{"cResFebEst","Character",031,0}) // Servicio de Restaurante de Febrero
       AADD(aDbfAlu,{"cResMarEst","Character",031,0}) // Servicio de Restaurante de Marzo
       AADD(aDbfAlu,{"cResAbrEst","Character",031,0}) // Servicio de Restaurante de Abril
       AADD(aDbfAlu,{"cResMayEst","Character",031,0}) // Servicio de Restaurante de Mayo
       AADD(aDbfAlu,{"cResJunEst","Character",031,0}) // Servicio de Restaurante de Junio
       AADD(aDbfAlu,{"cResJulEst","Character",031,0}) // Servicio de Restaurante de Julio
       AADD(aDbfAlu,{"cResAgoEst","Character",031,0}) // Servicio de Restaurante de Agosto
       AADD(aDbfAlu,{"cResSepEst","Character",031,0}) // Servicio de Restaurante de Septiembre
       AADD(aDbfAlu,{"cResOctEst","Character",031,0}) // Servicio de Restaurante de Octubre
       AADD(aDbfAlu,{"cResNovEst","Character",031,0}) // Servicio de Restaurante de Noviembre
       AADD(aDbfAlu,{"cResDicEst","Character",031,0}) // Servicio de Restaurante de Diciembre
     *�Servicion de Restaurante del Estudiante

       AADD(aDbfAlu,{"pNOMPADEST","Character",002,0}) // Posicion del Nombre del Padre
       AADD(aDbfAlu,{"cApePadEst","Character",030,0}) // Apellido del padre
       AADD(aDbfAlu,{"cNomPadEst","Character",040,0}) // Nombre del padre
       AADD(aDbfAlu,{"lPadQepEst","Logical"  ,001,0}) // .T. Fallecido
       AADD(aDbfAlu,{"dNacPadEst","Date"     ,008,0}) // Fecha de nacimiento
       AADD(aDbfAlu,{"cLugPadEst","Character",020,0}) // Lugar de nacimiento
       AADD(aDbfAlu,{"cPadNitEst","Character",016,0}) // C�dula del padre
       AADD(aDbfAlu,{"cPadLugEst","Character",016,0}) // Lugar de la c�dula
       AADD(aDbfAlu,{"cPadTntEst","Character",003,0}) // Tipo de Documento TI=>Tarjeta de Identidad CC=>Cedula de Ciudadania CE => Cedula de Extrajeria NI => Nit
       AADD(aDbfAlu,{"lPadFotEst","Logical"  ,001,0}) // .T. Fotocopia de la Cedula
       AADD(aDbfAlu,{"cProPadEst","Character",026,0}) // Profesi�n del padre
       AADD(aDbfAlu,{"cEmpPadEst","Character",026,0}) // Empresa del padre
       AADD(aDbfAlu,{"cCarPadEst","Character",020,0}) // Cargo del padre
       AADD(aDbfAlu,{"cDirPadEst","Character",040,0}) // Direccion del Padre
       AADD(aDbfAlu,{"cBarPadEst","Character",016,0}) // Barrio de la direcci�n del Padre
       AADD(aDbfAlu,{"cCiuPadEst","Character",030,0}) // Ciudad del Padre
       AADD(aDbfAlu,{"cDepPadEst","Character",030,0}) // Departamento del Padre
       AADD(aDbfAlu,{"cTelPadEst","Character",014,0}) // Telefono del padre
       AADD(aDbfAlu,{"cCelPadEst","Character",014,0}) // *Telefono celular del Padre
       AADD(aDbfAlu,{"cFaxPadEst","Character",014,0}) // *Fax del Padre
       AADD(aDbfAlu,{"cBipPadEst","Character",014,0}) // *Biper del Padre
       AADD(aDbfAlu,{"cMaiPadEst","Character",040,0}) // E-MAIL del padre
     *�Datos del Padre

       AADD(aDbfAlu,{"pNOMMADEST","Character",002,0}) // Posicion del Nombre de la Madre
       AADD(aDbfAlu,{"cApeMadEst","Character",030,0}) // Nombre de la madre
       AADD(aDbfAlu,{"cNomMadEst","Character",040,0}) // Nombre de la madre
       AADD(aDbfAlu,{"lMadQepEst","Logical"  ,001,0}) // .T. Fallecido
       AADD(aDbfAlu,{"dNacMadEst","Date"     ,008,0}) // Fecha de nacimiento
       AADD(aDbfAlu,{"cLugMadEst","Character",020,0}) // Lugar de nacimiento
       AADD(aDbfAlu,{"cMadNitEst","Character",016,0}) // C�dula de la madre
       AADD(aDbfAlu,{"cMadLugEst","Character",016,0}) // Lugar de la c�dula
       AADD(aDbfAlu,{"cMadTntEst","Character",003,0}) // Tipo de Documento TI=>Tarjeta de Identidad CC=>Cedula de Ciudadania CE => Cedula de Extrajeria NI => Nit
       AADD(aDbfAlu,{"lMadFotEst","Logical"  ,001,0}) // .T. Fotocopia de la Cedula
       AADD(aDbfAlu,{"cProMadEst","Character",026,0}) // Profesi�n de la madre
       AADD(aDbfAlu,{"cEmpMadEst","Character",026,0}) // Empresa de la madre
       AADD(aDbfAlu,{"cCarMadEst","Character",020,0}) // Cargo de la madre
       AADD(aDbfAlu,{"cDirMadEst","Character",040,0}) // Direccion de la Madre
       AADD(aDbfAlu,{"cBarMadEst","Character",016,0}) // Barrio de la direcci�n de la Madre
       AADD(aDbfAlu,{"cCiuMadEst","Character",030,0}) // Ciudad de la Madre
       AADD(aDbfAlu,{"cDepMadEst","Character",030,0}) // Departamento de la Madre
       AADD(aDbfAlu,{"cTelMadEst","Character",014,0}) // Telefono de la madre
       AADD(aDbfAlu,{"cCelMadEst","Character",014,0}) // *Telefono celular de la Madre
       AADD(aDbfAlu,{"cFaxMadEst","Character",014,0}) // *Fax de la Madre
       AADD(aDbfAlu,{"cBipMadEst","Character",014,0}) // *Biper de la Madre
       AADD(aDbfAlu,{"cMaiMadEst","Character",040,0}) // E-MAIL de la Madre
     *�Datos de la Madre

       AADD(aDbfAlu,{"pNOMACUEST","Character",002,0}) // Posicion del Nombre del Acudiente
       AADD(aDbfAlu,{"cApeAcuEst","Character",030,0}) // Apellido del Acudiente
       AADD(aDbfAlu,{"cNomAcuEst","Character",040,0}) // Nombre del Acudiente
       AADD(aDbfAlu,{"lAcuQepEst","Logical"  ,001,0}) // .T. Fallecido
       AADD(aDbfAlu,{"dNacAcuEst","Date"     ,008,0}) // Fecha de nacimiento
       AADD(aDbfAlu,{"cLugAcuEst","Character",020,0}) // Lugar de nacimiento
       AADD(aDbfAlu,{"cAcuNitEst","Character",016,0}) // C�dula del Acudiente
       AADD(aDbfAlu,{"cAcuLugEst","Character",016,0}) // Lugar del Acudiente
       AADD(aDbfAlu,{"cAcuTntEst","Character",003,0}) // Tipo de Documento TI=>Tarjeta de Identidad CC=>Cedula de Ciudadania CE => Cedula de Extrajeria NI => Nit
       AADD(aDbfAlu,{"lAcuFotEst","Logical"  ,001,0}) // .T. Fotocopia de la Cedula
       AADD(aDbfAlu,{"cProAcuEst","Character",026,0}) //*Profesi�n del Acudiente
       AADD(aDbfAlu,{"cEmpAcuEst","Character",026,0}) //*Empresa del Acudiente
       AADD(aDbfAlu,{"cCarAcuEst","Character",020,0}) //*Cargo del Acudiente
       AADD(aDbfAlu,{"cDirAcuEst","Character",040,0}) // Direccion de la casa del Acudiente
       AADD(aDbfAlu,{"cBarAcuEst","Character",016,0}) // Barrio de la direcci�n del Acudiente
       AADD(aDbfAlu,{"cCiuAcuEst","Character",030,0}) // Ciudad del Acudiente
       AADD(aDbfAlu,{"cDepAcuEst","Character",030,0}) // Departamento de la Madre
       AADD(aDbfAlu,{"cTe1AcuEst","Character",014,0}) // Telefono No. 1 del Acudiente
       AADD(aDbfAlu,{"cTe2AcuEst","Character",014,0}) // Telefono No. 2 del Acudiente
       AADD(aDbfAlu,{"cCelAcuEst","Character",014,0}) // *Telefono celular del Acudiente
       AADD(aDbfAlu,{"cFaxAcuEst","Character",014,0}) // *Fax del Acudiente
       AADD(aDbfAlu,{"cBipAcuEst","Character",014,0}) // *Biper del Acudiente
       AADD(aDbfAlu,{"cMaiAcuEst","Character",040,0}) // E-MAIL del Acudiente
       AADD(aDbfAlu,{"cParAcuEst","Character",016,0}) // Parentesco del Acudiente
     *�Datos del Acudiente

       AADD(aDbfAlu,{"cPazSdoEst","Character",020,0}) // Paz y Salvo. Espacio en Blanco =>SI Paz y Salvo. Diferente de Espacio =>NO Paz y Salvo
       AADD(aDbfAlu,{"cObsLibEst","Character",500,0}) // Observaci�n para el libro.
       AADD(aDbfAlu,{"cObsRetEst","Character",100,0}) // Observaci�n del Retiro.
       AADD(aDbfAlu,{"cCole13Est","Character",084,0}) // Procedencia de Preescolar
       AADD(aDbfAlu,{"cCole00Est","Character",084,0}) // Procedencia de Transici�n
       AADD(aDbfAlu,{"cCole01Est","Character",084,0}) // Procedencia de Primero
       AADD(aDbfAlu,{"cCole02Est","Character",084,0}) // Procedencia de Segundo
       AADD(aDbfAlu,{"cCole03Est","Character",084,0}) // Procedencia de Tercero
       AADD(aDbfAlu,{"cCole04Est","Character",084,0}) // Procedencia de Cuarto
       AADD(aDbfAlu,{"cCole05Est","Character",084,0}) // Procedencia de Quinto
       AADD(aDbfAlu,{"cCole06Est","Character",084,0}) // Procedencia de Sexto
       AADD(aDbfAlu,{"cCole07Est","Character",084,0}) // Procedencia de Septimo
       AADD(aDbfAlu,{"cCole08Est","Character",084,0}) // Procedencia de Octavo
       AADD(aDbfAlu,{"cCole09Est","Character",084,0}) // Procedencia de Noveno
       AADD(aDbfAlu,{"cCole10Est","Character",084,0}) // Procedencia de D�cimo
       AADD(aDbfAlu,{"cCole11Est","Character",084,0}) // Procedencia de Und�cimo
       AADD(aDbfAlu,{"cCole12Est","Character",084,0}) // Procedencia de DuoD�cimo
       AADD(aDbfAlu,{"cVotEscEst","Character",030,0}) // Voto Escolar del Estudiante
       AADD(aDbfAlu,{"cCodigoMat","Character",004,0}) // C�digo de la materia del area Especializada
       AADD(aDbfAlu,{"cAnoEstMae","Character",068,0}) // A�os en que estudio en el colegio

/*
       AADD(aDbfAlu,{"cApeRe1Est","Character",030,0}) // Apellidos de la Referencia No. 1
       AADD(aDbfAlu,{"cNomRe1Est","Character",030,0}) // Nombres de la Referencia No. 1
       AADD(aDbfAlu,{"cParRe1Est","Character",016,0}) // Parentesco de la Referencia No. 1
       AADD(aDbfAlu,{"cRe1NitEst","Character",016,0}) // C�dula de la Referencia No.1
       AADD(aDbfAlu,{"cRe1LugEst","Character",016,0}) // Lugar de la Referencia No. 1
       AADD(aDbfAlu,{"cRe1TntEst","Character",003,0}) // Tipo de Documento TI=>Tarjeta de Identidad CC=>Cedula de Ciudadania CE => Cedula de Extrajeria NI => Nit
       AADD(aDbfAlu,{"cDirRe1Est","Character",040,0}) // Direccion de la casa de la Referencia No. 1
       AADD(aDbfAlu,{"cBarRe1Est","Character",016,0}) // Barrio de la direcci�n de la Referencia No. 1
       AADD(aDbfAlu,{"cCiuRe1Est","Character",030,0}) // Ciudad de la Referencia
       AADD(aDbfAlu,{"cTe1Re1Est","Character",014,0}) // Telefono No. 1 de la Referencia No. 1
       AADD(aDbfAlu,{"cTe2Re1Est","Character",014,0}) // Telefono No. 2 de la Referencia No. 1
       AADD(aDbfAlu,{"cApeRe2Est","Character",030,0}) // Nombre de la Referencia No. 2
       AADD(aDbfAlu,{"cNomRe2Est","Character",030,0}) // Nombre de la Referencia No. 2
       AADD(aDbfAlu,{"cParRe2Est","Character",016,0}) // Parentesco de la Referencia No. 2
       AADD(aDbfAlu,{"cRe2NitEst","Character",016,0}) // C�dula de la Referencia No. 2
       AADD(aDbfAlu,{"cRe2LugEst","Character",016,0}) // Lugar de la Referencia No. 2
       AADD(aDbfAlu,{"cRe2TntEst","Character",002,0}) // Tipo de Documento TI=>Tarjeta de Identidad CC=>Cedula de Ciudadania CE => Cedula de Extrajeria NI => Nit
       AADD(aDbfAlu,{"cDirRe2Est","Character",040,0}) // Direccion de la casa de la Referencia No. 2
       AADD(aDbfAlu,{"cBarRe2Est","Character",016,0}) // Barrio de la direcci�n de la Referencia No. 2
       AADD(aDbfAlu,{"cTe1Re2Est","Character",014,0}) // Telefono No. 1 de la Referencia No. 2
       AADD(aDbfAlu,{"cTe2Re2Est","Character",014,0}) // Telefono No. 2 de la Referencia No. 2
       AADD(aDbfAlu,{"cApeFi1Est","Character",030,0}) // Apellidos del Fiador No. 1
       AADD(aDbfAlu,{"cNomFi1Est","Character",030,0}) // Nombres del Fiador No. 1
       AADD(aDbfAlu,{"cParFi1Est","Character",016,0}) // Parentesco del Fiador No. 1
       AADD(aDbfAlu,{"cFi1NitEst","Character",016,0}) // C�dula del Fiador No. 1
       AADD(aDbfAlu,{"cFi1LugEst","Character",016,0}) // Lugar del Fiador No. 1
       AADD(aDbfAlu,{"cFi1TntEst","Character",002,0}) // Tipo de Documento TI=>Tarjeta de Identidad CC=>Cedula de Ciudadania CE => Cedula de Extrajeria NI => Nit
       AADD(aDbfAlu,{"cDirFi1Est","Character",040,0}) // Direccion de la casa  Fiador No. 1
       AADD(aDbfAlu,{"cBarFi1Est","Character",016,0}) // Barrio de la direcci�n Fiador No. 1
       AADD(aDbfAlu,{"cTe1Fi1Est","Character",014,0}) // Telefono No. 1 del Fiador No. 1
       AADD(aDbfAlu,{"cTe2Fi1Est","Character",014,0}) // Telefono No. 2 del Fiador No. 1
       AADD(aDbfAlu,{"cApeFi2Est","Character",030,0}) // Apellidos del Fiador No. 2
       AADD(aDbfAlu,{"cNomFi2Est","Character",030,0}) // Nombres del Fiador No. 2
       AADD(aDbfAlu,{"cParFi2Est","Character",016,0}) // Parentesco del Fiador No. 2
       AADD(aDbfAlu,{"cFi2NitEst","Character",016,0}) // C�dula del Fiador No. 2
       AADD(aDbfAlu,{"cFi2LugEst","Character",016,0}) // Lugar del Fiador No. 2
       AADD(aDbfAlu,{"cFi2TntEst","Character",002,0}) // Tipo de Documento TI=>Tarjeta de Identidad CC=>Cedula de Ciudadania CE => Cedula de Extrajeria NI => Nit
       AADD(aDbfAlu,{"cDirFi2Est","Character",040,0}) // Direccion de la casa Fiador No. 2
       AADD(aDbfAlu,{"cBarFi2Est","Character",016,0}) // Barrio de la direcci�n Fiador No. 2
       AADD(aDbfAlu,{"cTe1Fi2Est","Character",014,0}) // Telefono No. 1 del Fiador No. 2
       AADD(aDbfAlu,{"cTe2Fi2Est","Character",014,0}) // Telefono No. 2 del Fiador No. 2
*/