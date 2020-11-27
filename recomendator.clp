(defclass Sala "Concepto que representa una sala dentro de un museo, donde habrán diferentes tipos de obras de arte."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass PinturaDelMes
    (is-a Sala)
    (role concrete)
    (pattern-match reactive)
)

(defclass SalaArtista "Sala dedicada especificamente a un artista"
    (is-a Sala)
    (role concrete)
    (pattern-match reactive)
)

(defclass SalaCronologica "Sala que contendrá obras correspondientes a una época en especifico"
    (is-a Sala)
    (role concrete)
    (pattern-match reactive)
)

(defclass SalaHighlights "Salas que recogerán las mejores obras"
    (is-a Sala)
    (role concrete)
    (pattern-match reactive)
)

(defclass SalaHighlightsColeccion "Salas que recogerán las mejores obras de una colección"
    (is-a SalaHighlights)
    (role concrete)
    (pattern-match reactive)
)

(defclass SalaHighlightsEpoca "Salas que recogerán las mejores obras de una época en conreto."
    (is-a SalaHighlights)
    (role concrete)
    (pattern-match reactive)
)

(defclass SalaHistorias "Salas en las cuales se expondrán cuadros que cuenten historias respecto a hechos ocurridos o ficticios."
    (is-a Sala)
    (role concrete)
    (pattern-match reactive)
)

(defclass SalaLlegadasRecientes "Sala donde se expondrán obras que han llegado recientemente al museo"
    (is-a Sala)
    (role concrete)
    (pattern-match reactive)
)

(defclass ObraDeArte
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot periodoPictorico
        (type STRING)
        (create-accessor read-write))
    ;;; Obras relacionadas con la actual
    (multislot ObrasRelacionadas
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Epoca a la que pertenece la obra
    (multislot epoca
        (type STRING)
        (create-accessor read-write))
    ;;; Atributo que representa el año de creación de una obra
    (multislot anyoDeCreacion
        (type INTEGER)
        (create-accessor read-write))
    (multislot tematica
        (type STRING)
        (create-accessor read-write))
    (multislot esExitoso
        (type SYMBOL)
        (create-accessor read-write))
    (multislot ObraEnElMuseo
        (type INSTANCE)
        (create-accessor read-write))
    (multislot profundidad
        (type FLOAT)
        (create-accessor read-write))
    (multislot ObraEnSala
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Estilo artístico con el que se identifica la obra.
    (multislot estilo
        (type STRING)
        (create-accessor read-write))
    (multislot AutorObra
        (type INSTANCE)
        (create-accessor read-write))
    (multislot complejidad
        (type FLOAT)
        (create-accessor read-write))
    (multislot titulo
        (type STRING)
        (create-accessor read-write))
    (multislot TematicaCuadro
        (type INSTANCE)
        (create-accessor read-write))
    (multislot escuela
        (type STRING)
        (create-accessor read-write))
    (multislot EpocaCuadro
        (type INSTANCE)
        (create-accessor read-write))
    (multislot corriente
        (type STRING)
        (create-accessor read-write))
    (multislot descripcion
        (type STRING)
        (create-accessor read-write))
)

(defclass Antiguedades
    (is-a ObraDeArte)
    (role concrete)
    (pattern-match reactive)
)

(defclass ArteDecorativo
    (is-a ObraDeArte)
    (role concrete)
    (pattern-match reactive)
)

(defclass Cuadro
    (is-a ObraDeArte)
    (role concrete)
    (pattern-match reactive)
    ;;; superficie del cuadro en centimetros cuadrados
    (multislot superficie
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass Escultura
    (is-a ObraDeArte)
    (role concrete)
    (pattern-match reactive)
)

(defclass RestosDeCivilizacion
    (is-a ObraDeArte)
    (role concrete)
    (pattern-match reactive)
)

(defclass Visitante
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Seran las horas que un visitante quiere pasar en un museo por dia
    (multislot horasPorDia
        (type SYMBOL)
        (create-accessor read-write))
    (multislot nacionalidad
        (type STRING)
        (create-accessor read-write))
    (multislot nivelConocimientoArte
        (type INTEGER)
        (create-accessor read-write))
    ;;; Preferencias que tendrá un visitante al visitar el museo
    (multislot PreferenciasVisitante
        (type INSTANCE)
        (create-accessor read-write))
)

(defclass VisitanteFamilia
    (is-a Visitante)
    (role concrete)
    (pattern-match reactive)
)

(defclass VisitanteGrupo
    (is-a Visitante)
    (role concrete)
    (pattern-match reactive)
)

(defclass VisitanteGrupoGrande
    (is-a VisitanteGrupo)
    (role concrete)
    (pattern-match reactive)
)

(defclass VistanteGrupoPequeño
    (is-a VisitanteGrupo)
    (role concrete)
    (pattern-match reactive)
)

(defclass VisitanteIndividual
    (is-a Visitante)
    (role concrete)
    (pattern-match reactive)
)

(defclass Recomendacion "Clase que representa las recomendaciones de itinerarios que se envian al visitante"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Duración de la visita en horas. Se calcula a partir del tiempo durante el cual se mira un cuadro. Este tiempo se calcula en función de la importancia del cuadro, del artista, el conocimiento del visitante, el número de personas del grupo visitante y si tienen preferencias por algún estilo en especial.
    (multislot duracionVisita
        (type INTEGER)
        (create-accessor read-write))
)

(defclass Preferencias
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot preferenciaExitoso
        (type SYMBOL)
        (create-accessor read-write))
    (multislot preferenciaEpoca
        (type STRING)
        (create-accessor read-write))
    (multislot preferenciaAutor
        (type STRING)
        (create-accessor read-write))
    ;;; Implica la preferencia del visitante respecto al tipo de obra (paisaje, retrato...)
    (multislot preferenciaTipoObra
        (type STRING)
        (create-accessor read-write))
    (multislot preferenciaComplejidad
        (type INTEGER)
        (create-accessor read-write))
    (multislot preferenciaNacionalidad
        (type STRING)
        (create-accessor read-write))
    (multislot preferenciaTipoDeObra
        (type STRING)
        (create-accessor read-write))
    ;;; Dias que el visitante pasará en el museo
    (multislot diasVisita
        (type INTEGER)
        (create-accessor read-write))
)

(defclass Accion "Clase que engloba todas las acciones del sistema"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass Museo
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot SalaDeUnMuseo
        (type INSTANCE)
        (create-accessor read-write))
)

(defclass Evento "Clase que engloba todos los eventos que tendrá el sistema"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass Pintor
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot anyoMuerte
        (type SYMBOL)
        (create-accessor read-write))
    (multislot corriente
        (type STRING)
        (create-accessor read-write))
    (multislot nacionalidad
        (type STRING)
        (create-accessor read-write))
    (multislot anyoNacimiento
        (type INTEGER)
        (create-accessor read-write))
    (multislot periodoPictorico
        (type STRING)
        (create-accessor read-write))
    (multislot nombreAutor
        (type SYMBOL)
        (create-accessor read-write))
    (multislot escuela
        (type STRING)
        (create-accessor read-write))
    (multislot descripcion
        (type STRING)
        (create-accessor read-write))
)

(defclass Tematica "Tematica del cuadro"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass Respuesta "Clase que engloba todas las respuestas del sistema"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass ConocimientoArte "Esta clase contendrá el conocimiento de cada época que tiene el visitante"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot conocimientoEdadContemporanea
        (type SYMBOL)
        (create-accessor read-write))
    (multislot conocimientoEdadMedia
        (type SYMBOL)
        (create-accessor read-write))
    (multislot conocimientoPrehistoria
        (type SYMBOL)
        (create-accessor read-write))
    (multislot conocimientoEdadModerna
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass Epoca "Clase que representa una época en concreto"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(definstances instances
    ([VenusDelEspejoVelazquez] of Cuadro
         (superficie  21594)
         (anyoDeCreacion  1647)
         (AutorObra  [DiegoVelazquez])
         (TematicaCuadro  [Religion])
         (EpocaCuadro  [Barroco])
    )

    ([CaballeroSonriente] of Cuadro
         (superficie  5561)
         (anyoDeCreacion  1624)
         (AutorObra  [FransHals])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Barroco])
    )

    ([RetratoDeUnaMujerNoble] of Cuadro
         (superficie  5412)
         (anyoDeCreacion  1580)
         (AutorObra  [LaviniaFontana])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Manierismo])
    )

    ([PuenteDeNarni] of Cuadro
         (AutorObra  [CamileCorot])
    )

    ([ElDesesperado] of Cuadro
         (AutorObra  [GustaveCourbet])
    )

    ([Lycianna] of Cuadro
         (superficie  5000)
         (anyoDeCreacion  1918)
         (AutorObra  [JohnWilliam])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([LaviniaFontana] of Pintor
    )

    ([LaMatanzaDeQuios] of Cuadro
         (AutorObra  [EugeneDelacroix])
    )

    ([AmtpmopCprradomo] of Pintor
    )

    ([JohnWilliam] of Pintor
    )

    ([Almiar] of Cuadro
         (AutorObra  [ClaudeMonet])
    )

    ([WinslowHomer] of Pintor
         (nombreAutor  "Winslow Homer")
    )

    ([LaRondaDeNoche] of Cuadro
         (superficie  158631)
         (anyoDeCreacion  1641)
         (AutorObra  [Rembrandt])
         (TematicaCuadro  [Historica])
         (EpocaCuadro  [Barroco])
    )

    ([LaUltimaCena] of Cuadro
         (superficie  40480000)
         (anyoDeCreacion  1496)
         (AutorObra  [LeonardoDaVinci])
         (TematicaCuadro  [Religion])
         (EpocaCuadro  [Renacentismo])
    )

    ([LosComedoresDePatatas] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([ElJovenAzul] of Cuadro
         (superficie  19360)
         (anyoDeCreacion  1770)
         (AutorObra  [ThomasGainsborough])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Rococo])
    )

    ([CamileCorot] of Pintor
    )

    ([Tiziano] of Pintor
    )

    ([ThomasGainsborough] of Pintor
    )

    ([LaLibertadGuiandoAlPueblo] of Cuadro
         (AutorObra  [EugeneDelacroix])
    )

    ([Neoclacisismo] of Epoca
    )

    ([ElAmorVictorioso] of Cuadro
         (superficie  17628)
         (anyoDeCreacion  1601)
         (AutorObra  [Caravaggio])
         (TematicaCuadro  [NaturalezaMuerta])
         (EpocaCuadro  [Barroco])
    )

    ([NaturalezaMuerta] of Tematica
    )

    ([MigueAngel] of Pintor
    )

    ([PlazaDeSanMarcos] of Cuadro
         (superficie  25000)
         (anyoDeCreacion  1723)
         (AutorObra  [Canaletto])
         (TematicaCuadro  [Paisaje])
         (EpocaCuadro  [Rococo])
    )

    ([ElNiñoDeLaPeonza] of Cuadro
         (superficie  5092)
         (anyoDeCreacion  1735)
         (AutorObra  [JeanSimeon])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Rococo])
    )

    ([JeanSimeon] of Pintor
    )

    ([JacquesLousDavid] of Pintor
    )

    ([Parmigianino] of Pintor
    )

    ([LaMajaDesnuda] of Cuadro
         (AutorObra  [FranciscoDeGoya])
    )

    ([LaFamiliaDeCarlosIV] of Cuadro
         (AutorObra  [FranciscoDeGoya])
    )

    ([RailroadSunset] of Cuadro
         (superficie  6500)
         (anyoDeCreacion  1929)
         (AutorObra  [EdwardHopper])
         (TematicaCuadro  [Paisaje])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([HombreDeVitruvio] of Cuadro
         (superficie  910)
         (anyoDeCreacion  1492)
         (AutorObra  [LeonardoDaVinci])
         (TematicaCuadro  [Historica])
         (EpocaCuadro  [Renacentismo])
    )

    ([IliaRepin] of Pintor
    )

    ([WilliamHogarth] of Pintor
    )

    ([ElGreco] of Pintor
    )

    ([ElColumpio] of Cuadro
         (superficie  5265)
         (anyoDeCreacion  1767)
         (AutorObra  [JeanHonoreFragonard])
         (TematicaCuadro  [Religion])
         (EpocaCuadro  [Rococo])
    )

    ([LaPastora] of Cuadro
         (AutorObra  [WilliamAdolphe])
    )

    ([RetratoDeJaneSeymour] of Cuadro
         (superficie  7500)
         (anyoDeCreacion  1536)
         (AutorObra  [HansHolbein])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Renacentismo])
    )

    ([Renacentismo] of Epoca
    )

    ([LosSirgadoresDeVolga] of Cuadro
         (AutorObra  [IliaRepin])
    )

    ([Retrato] of Tematica
    )

    ([ElRaptoDeLasSabinas] of Cuadro
         (superficie  11000)
         (anyoDeCreacion  1797)
         (AutorObra  [JacquesLousDavid])
         (TematicaCuadro  [Historica])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([Canaletto] of Pintor
    )

    ([DiegoVelazquez] of Pintor
    )

    ([VIsionOfSantJerome] of Cuadro
         (superficie  6000)
         (anyoDeCreacion  1526)
         (AutorObra  [Parmigianino])
         (TematicaCuadro  [Religion])
         (EpocaCuadro  [Manierismo])
    )

    ([Paisaje] of Tematica
    )

    ([Caravaggio] of Pintor
    )

    ([MujerJudiaVendiendoNaranjas] of Cuadro
         (AutorObra  [AleksanderGyermski])
    )

    ([PieterBrueghel] of Pintor
    )

    ([HabitacionesJuntoAlMar] of Cuadro
         (superficie  7446)
         (anyoDeCreacion  1951)
         (AutorObra  [EdwardHopper])
         (TematicaCuadro  [Paisaje])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([AmorSacroYAmorProfano] of Cuadro
         (superficie  32922)
         (anyoDeCreacion  1515)
         (AutorObra  [Tiziano])
         (TematicaCuadro  [Religion])
         (EpocaCuadro  [Renacentismo])
    )

    ([JeanHonoreFragonard] of Pintor
    )

    ([LaNocheEstrellada] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([EdwardHopper] of Pintor
    )

    ([JarronConClaveles] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([VistaDeToledo] of Cuadro
         (superficie  2064)
         (anyoDeCreacion  1598)
         (AutorObra  [ElGreco])
         (TematicaCuadro  [Paisaje])
         (EpocaCuadro  [Manierismo])
    )

    ([VincentVanGogh] of Pintor
    )

    ([Rembrandt] of Pintor
    )

    ([LaMajaVestida] of Cuadro
         (AutorObra  [FranciscoDeGoya])
    )

    ([AleksanderGyermski] of Pintor
    )

    ([ElAquelarre] of Cuadro
         (AutorObra  [FranciscoDeGoya])
    )

    ([LaMuerteDeMarat] of Cuadro
         (superficie  21120)
         (anyoDeCreacion  1793)
         (AutorObra  [JacquesLousDavid])
         (TematicaCuadro  [Historica])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([AntonioCanova] of Pintor
    )

    ([VenusDelEspejo] of Cuadro
         (superficie  12896)
         (AutorObra  [Tiziano])
         (TematicaCuadro  [Religion])
         (EpocaCuadro  [Manierismo])
    )

    ([EugeneDelacroix] of Pintor
    )

    ([Realismo] of Epoca
    )

    ([FranciscoDeGoya] of Pintor
    )

    ([FransHals] of Pintor
    )

    ([LaCerradura] of Cuadro
         (superficie  6532)
         (anyoDeCreacion  1778)
         (AutorObra  [JeanHonoreFragonard])
         (TematicaCuadro  [Historica])
         (EpocaCuadro  [Rococo])
    )

    ([GustaveCourbet] of Pintor
    )

    ([AugusteRodin] of Pintor
    )

    ([JoanDeBolonia] of Pintor
    )

    ([CestoConFrutas] of Cuadro
         (superficie  2300)
         (anyoDeCreacion  1596)
         (AutorObra  [Caravaggio])
         (TematicaCuadro  [NaturalezaMuerta])
         (EpocaCuadro  [Barroco])
    )

    ([ElNacimientoDeVenus] of Cuadro
         (superficie  47816)
         (anyoDeCreacion  1485)
         (AutorObra  [SandroBoticelli])
         (TematicaCuadro  [Religion])
         (EpocaCuadro  [Renacentismo])
    )

    ([Impresionismo] of Epoca
    )

    ([ClaudeMonet] of Pintor
    )

    ([HansHolbein] of Pintor
    )

    ([Manierismo] of Epoca
    )

    ([Autoretrato] of Cuadro
         (anyoDeCreacion  1745)
         (AutorObra  [WilliamHogarth])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Rococo])
    )

    ([WilliamAdolphe] of Pintor
    )

    ([ElCaballeroDeLaMano] of Cuadro
         (superficie  5412)
         (anyoDeCreacion  1580)
         (AutorObra  [ElGreco])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Manierismo])
    )

    ([Gioconda] of Cuadro
         (superficie  4081)
         (anyoDeCreacion  1503)
         (AutorObra  [LeonardoDaVinci])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Renacentismo])
    )

    ([LeonardoDaVinci] of Pintor
    )

    ([SandroBoticelli] of Pintor
    )

    ([ElJardinDelArtistaEnGiverny] of Cuadro
         (AutorObra  [ClaudeMonet])
    )

    ([AFavorDeLaBrisa] of Cuadro
         (AutorObra  [WinslowHomer])
         (titulo  "A favor de la brisa")
    )

    ([Religion] of Tematica
    )

    ([UnBañoDeAsnieres] of Cuadro
         (AutorObra  [GeogrgesPierre])
    )

    ([LaTrinidad] of Cuadro
         (superficie  53700)
         (anyoDeCreacion  1578)
         (AutorObra  [ElGreco])
         (TematicaCuadro  [Religion])
         (EpocaCuadro  [Manierismo])
    )

    ([FranciscoGuitierrez] of Pintor
    )

    ([JuegosDeNiños] of Cuadro
         (superficie  18998)
         (anyoDeCreacion  1560)
         (AutorObra  [PieterBrueghel])
         (TematicaCuadro  [Historica])
         (EpocaCuadro  [Renacentismo])
    )

    ([AutoretratoDeVincent] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([Romanticismo] of Epoca
    )

    ([Rococo] of Epoca
    )

    ([GeogrgesPierre] of Pintor
    )

    ([Historica] of Tematica
    )

    ([Lirios] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([LosDiscipulosDeEmaus] of Cuadro
         (superficie  27580)
         (anyoDeCreacion  1602)
         (AutorObra  [Caravaggio])
         (TematicaCuadro  [Historica])
         (EpocaCuadro  [Barroco])
    )

    ([BernardinoCampiPintandoASofiaAnguissola] of Cuadro
         (superficie  7000)
         (anyoDeCreacion  1559)
         (AutorObra  [SofiaAnguissola])
         (TematicaCuadro  [Retrato])
         (EpocaCuadro  [Manierismo])
    )

    ([SofiaAnguissola] of Pintor
    )

    ([LasMeninas] of Cuadro
         (superficie  9000)
         (anyoDeCreacion  1656)
         (AutorObra  [DiegoVelazquez])
         (TematicaCuadro  [Historica])
         (EpocaCuadro  [Barroco])
    )

    ([Barroco] of Epoca
    )

)

; Modulo principal

(defmodule MAIN (export ?ALL))

; Modulo donde se recogen los datos de los visitantes

(defmodule getVisitorsData 
	(import MAIN ?ALL)
	(export ?ALL)
)

(defmodule getVisitorsPreferences 
        (import MAIN ?ALL)
        (import getVisitorsData deftemplate ?ALL)
        (export ?ALL)
)

(deffunction numberWithRangesQuestion (?question ?ini ?fin)
                (format t "%s" ?question)
                (printout t crlf)
                (bind ?answer (read))
                (while (not (and ( >= ?answer ?ini) (<= ?answer ?fin))) do 
                        (format t "%s?" ?question)
                        (bind ?answer (read)))
                (printout t crlf)
                (printout t crlf)
                ;devolvemos la respuesta
                ?answer
)

(deffunction numberQuestion (?question)
        (format t "%s" ?question)
                (printout t crlf)
        (bind ?answer (read))
        (while (not( > ?answer 0)) do 
            (format t "%s?" ?question)
            (bind ?answer (read)))
        (printout t crlf)
        (printout t crlf)
        ;devolvemos la respuesta
        ?answer
)

(deffunction stringQuestion (?question)
                (format t "%s" ?question)
                (printout t crlf)
                (bind ?answer (read))
                (while (not( neq ?answer " ")) do 
                        (format t "%s?" ?question)
                        (bind ?answer (read)))
                (printout t crlf)
                (printout t crlf)
                ;devolvemos la respuesta
                ?answer
)

(deffunction booleanQuestion (?question)
		(format t "%s" ?question)
                (printout t crlf)
		(bind ?answer (read))
		
		(while (not( or (eq ?answer si) (eq ?answer no))) do 
			(format t "%s?" ?question)
			(bind ?answer (read)))
		(printout t crlf)
		(printout t crlf)
		;devolvemos la respuesta
		(if (eq ?answer si)
			then TRUE
			else FALSE)
)

(defglobal ?*preHistoriaKnowledge* = 0)
(defglobal ?*edadAntiguaKnowledge* = 0)
(defglobal ?*edadMediaKnowledge* = 0)
(defglobal ?*edadModernaKnowledge* = 0)
(defglobal ?*edadContemporaneaKnowledge* = 0)

(deffunction artKnowledgeQuestions ($?questions)
                (printout t "A continuacion se te van a realizar unas preguntas para evaluar" crlf)
                (printout t "tus conocimientos sobre arte, en caso de que seais varias personas" crlf)
                (printout t "facilitanos el nivel medio aproximado de los miembros del grupo" crlf)
                (printout t crlf)
                (printout t crlf)
                ;inicializamos la lista que contendrá las respuestas
                (bind ?answers (create$ ))

                (loop-for-count (?i 1 (length$ $?questions)) do
                        (bind ?answer (numberWithRangesQuestion (nth$ ?i ?questions) 1 5))
                        ;(format t "%s" (nth$ ?i ?questions))
                        ;(printout t crlf)
                        ;(bind ?answer (read))
                        (bind $?answers(insert$ $?answers (+ (length$ $?answers) 1) ?answer))
                        (printout t crlf)
                )

                (loop-for-count (?i 1 3) do
                        (bind ?*preHistoriaKnowledge* (+ ?*preHistoriaKnowledge* (nth$ ?i ?answers)))
                )

                (loop-for-count (?i 4 10) do
                        (bind ?*edadAntiguaKnowledge* (+ ?*edadAntiguaKnowledge* (nth$ ?i ?answers)))
                )

                (loop-for-count (?i 11 19) do
                        (bind ?*edadMediaKnowledge* (+ ?*edadMediaKnowledge* (nth$ ?i ?answers)))
                )

                (loop-for-count (?i 20 23) do
                        (bind ?*edadModernaKnowledge* (+ ?*edadModernaKnowledge* (nth$ ?i ?answers)))
                )

                (loop-for-count (?i 24 28) do
                        (bind ?*edadContemporaneaKnowledge* (+ ?*edadContemporaneaKnowledge* (nth$ ?i ?answers)))
                )                  
               
                (bind ?list (create$ ?*preHistoriaKnowledge* ?*edadAntiguaKnowledge* ?*edadMediaKnowledge* ?*edadModernaKnowledge* ?*edadContemporaneaKnowledge*))
                ?list

)

;Función que crea todas las preguntas necesarias para evaluar el nivel de arte del visitante
(deffunction createArtQuestions (?random)
        ;prehistoria
        (bind ?p1 "Valora del 1 al 5 tus conocimientos sobre Arte paleolitico (h. 40.000 - 10.000 a.C.)")
        (bind ?p2 "Valora del 1 al 5 tus conocimientos sobre Arte mesolitico (h. 10.000 - 4000 a.C.)")
        (bind ?p3 "Valora del 1 al 5 tus conocimientos sobre Arte neolitico (h. 4000 - 2000 a.C.)")

        ;Edad antigua
        (bind ?p4 "Valora del 1 al 5 tus conocimientos sobre Arte egipcio (h. 5300 - 30 a.C.)")
        (bind ?p5 "Valora del 1 al 5 tus conocimientos sobre Arte mesopotamico (h. 4000 - 539 a.C.)")
        (bind ?p6 "Valora del 1 al 5 tus conocimientos sobre Arte minoico (h. 3000 - 1400 a.C.)")
        (bind ?p7 "Valora del 1 al 5 tus conocimientos sobre Arte micenico (h. 1500 - 1100 a.C.)")
        (bind ?p8 "Valora del 1 al 5 tus conocimientos sobre Arte griego (h. 1000 - 320 a.C.)")
        (bind ?p9 "Valora del 1 al 5 tus conocimientos sobre Arte etrusco (h. 800 - 100 a.C.)")
        (bind ?p10 "Valora del 1 al 5 tus conocimientos sobre Arte romano (h. 400 a.C - 476 d.C.)")

        ;Edad media
        (bind ?p11 "Valora del 1 al 5 tus conocimientos sobre Arte paleocristiano (h. s.I - IV)")
        (bind ?p12 "Valora del 1 al 5 tus conocimientos sobre Arte visigodo (h. 415 - 711)")
        (bind ?p13 "Valora del 1 al 5 tus conocimientos sobre Arte bizantino (h. 330 – 1.453)")
        (bind ?p14 "Valora del 1 al 5 tus conocimientos sobre Arte mozarabe (h. 711 - 1.000)")
        (bind ?p15 "Valora del 1 al 5 tus conocimientos sobre Arte carolingio (h. 780 - 900)")
        (bind ?p16 "Valora del 1 al 5 tus conocimientos sobre Arte otoniano (h. 950 - 1050)")
        (bind ?p17 "Valora del 1 al 5 tus conocimientos sobre Arte romanico (s. XI - XIII)")
        (bind ?p18 "Valora del 1 al 5 tus conocimientos sobre Arte gotico (s. XII - XVI)")
        (bind ?p19 "Valora del 1 al 5 tus conocimientos sobre Arte mudejar (s.XII - XVI)")

        ;Edad Moderna
        (bind ?p20 "Valora del 1 al 5 tus conocimientos sobre Renacimiento (s.XV - XVI)")
        (bind ?p21 "Valora del 1 al 5 tus conocimientos sobre Manierismo (h. 1530 - 1.600)")
        (bind ?p22 "Valora del 1 al 5 tus conocimientos sobre Barroco (h. 1600 - 1750)")
        (bind ?p23 "Valora del 1 al 5 tus conocimientos sobre Rococo (1720 - 1740)")

        ;Edad contemporanea
        (bind ?p24 "Valora del 1 al 5 tus conocimientos sobre Neoclasicismo (1730 - 1820)")
        (bind ?p25 "Valora del 1 al 5 tus conocimientos sobre Romanticismo (desde finales del s. XVIII hasta mediados del s.XIX)")
        (bind ?p26 "Valora del 1 al 5 tus conocimientos sobre Realismo (s.XIX)")
        (bind ?p27 "Valora del 1 al 5 tus conocimientos sobre Impresionismo (mediados s.XIX)")
        (bind ?p28 "Valora del 1 al 5 tus conocimientos sobre Simbolismo (finales del s.XIX)")

        (bind ?list (create$ ?p1 ?p2 ?p3 ?p4 ?p5 ?p6 ?p7 ?p8 ?p9 ?p10 ?p11 ?p12 ?p13 ?p14 ?p15 ?p16 ?p17 ?p18 ?p19 ?p20 ?p21 ?p22 ?p23 ?p24 ?p25 ?p26 ?p27 ?p28))
        ?list

)

(deffunction multiAnswerQuestion (?question)
        (format t "%s" ?question)
        (printout t crlf)
        (bind ?answer (readline))
        ; Separamos el string ("asd" "asd2" ...)
        (bind ?ans (str-explode ?answer))

        ?ans

)

(defrule MAIN::initialRule "Regla inicial que se ejecuta al ejecutar el programa"
		=>
		(printout t"-----------------------------------------------------------" crlf)
		(printout t"-----------------------------------------------------------" crlf)
		(printout t"-------------Personaliza tu visita al museo----------------" crlf)
		(printout t"-----------------------------------------------------------" crlf)
		(printout t"-----------------------------------------------------------" crlf)
		(printout t crlf)
		(printout t crlf)
		(printout t crlf)
		(printout t "A partir de dar respuesta a las siguientes preguntas, podras" crlf)
		(printout t "   obtener una experiencia personalizada en VicSer museum" crlf) 
		(printout t "              Aprovecha al maximo tu tiempo!" crlf)
		(printout t crlf)
		(printout t crlf)
		(printout t crlf)
		
		;Pasamos al módulo donde pediremos los datos a los visitantes
		(focus getVisitorsPreferences)
)



(defrule getVisitorsPreferences::getVisitorsData "Regla para obtener las preferencias de un visitante"
                =>
                >(make-instance visitante of Visitante)
                (bind ?size (numberQuestion "Cual es el tamanyo de tu grupo?"))
                (bind ?isFamily (booleanQuestion "Visitas el museo con tu familia?"))
                (bind ?nacionalidad (stringQuestion "Cual es vuestra nacionalidad?"))
                (bind ?daysPerVist (numberQuestion "Cuantos dias vas a dedicar a la visita del museo?"))
                (bind ?hoursPerDay (numberQuestion "Cuantas horas vas a querer dedicar cada dia?"))

                (bind ?artQuestions (createArtQuestions "asd"))
                ;Llamamos a la función que nos va a hacer preguntitas para saber el nivel de arte que tenemos
                (bind ?artKnowledgeAnswers (artKnowledgeQuestions $?artQuestions))

                (loop-for-count (?i 1 (length$ $?artKnowledgeAnswers)) do
                    (format t "%d" (nth$ ?i ?artKnowledgeAnswers))
                    (printout t crlf)
                )

                (format t "%d" ?size)
                (if (eq ?isFamily TRUE)
                    then (printout t "TRUEEE")
                    else (printout t "FALSEEE"))

)

(defrule getVisitorsPreferences::getVisitorsPreferences "Regla para obtener las preferencias de un visitante"
                =>
                (bind ?preferredAuthors (multiAnswerQuestion "Que autores prefieres? Separalos con un espacio"))
                (loop-for-count (?i 1 (length$ $?preferredAuthors)) do
                        (format t "%s" (nth$ ?i ?preferredAuthors))
                        (printout t crlf)
                )
                
                (bind ?preferredComplexityLevel (numberWithRangesQuestion "Del 1 al 10, cual es tu nivel medio de complejidad preferido?" 1 10))

                (bind ?preferredPeriods (multiAnswerQuestion "Que periodos pictoricos prefieres? Separalos con un espacio"))
                (loop-for-count (?i 1 (length$ $?preferredAuthors)) do
                        (format t "%s" (nth$ ?i ?preferredAuthors))
                        (printout t crlf)
                )

                (bind ?preferredFamous (booleanQuestion "Tienes preferencia por las obras mas famosas que tenemos en el museo?"))
                (bind ?preferredSameNationality (booleanQuestion "Tienes preferencia por las obras hechas por autores de tu nacionalidad?"))

                (printout t crlf)
                (printout t crlf)

                (printout t "A continuacion se te va a preguntar que tipo de obra prefieres, a escoger de entre: ")
                (printout t crlf)
                (printout t "cuadros, esculturas, restos de civilizaciones, antiguedades y arte decorativo.")
                (printout t crlf)
                (printout t crlf)
                (printout t "tendras que escribirlos en orden de prioridad y separados por un espacio, para el caso de")
                (printout t crlf)
                (printout t "restos de civilizaciones escribe restos, y para arte decorativo escribe decorativo")
                (printout t crlf)
                (printout t crlf)

                (bind ?preferredTypeOfArt (multiAnswerQuestion "Que tipo de obra prefieres?"))
                (loop-for-count (?i 1 (length$ $?preferredAuthors)) do
                        (format t "%s" (nth$ ?i ?preferredAuthors))
                        (printout t crlf)
                )

                ; A PARTIR DE AQUÍ MOVER LO DE ABAJO A OTRO MODULO

)

(defrule printCuadro
                ?cuadro <- (object (is-a Cuadro) (titulo ?t))
                (test (eq ?t "A favor de la brisa"))
                =>
                (printout t (send ?cuadro get-titulo) crlf)
)

