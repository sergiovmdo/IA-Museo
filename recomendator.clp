(defclass ObraDeArte
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Atributo que representa el año de creación de una obra
    (multislot anyoDeCreacion
        (type INTEGER)
        (create-accessor read-write))
    (multislot corriente
        (type STRING)
        (create-accessor read-write))
    ;;; Epoca a la que pertenece la obra
    (multislot epoca
        (type STRING)
        (create-accessor read-write))
    (multislot TematicaCuadro
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Obras relacionadas con la actual
    (multislot ObrasRelacionadas
        (type INSTANCE)
        (create-accessor read-write))
    (multislot periodoPictorico
        (type STRING)
        (create-accessor read-write))
    (multislot ObraEnSala
        (type INSTANCE)
        (create-accessor read-write))
    (multislot complejidad
        (type FLOAT)
        (create-accessor read-write))
    (multislot esExitoso
        (type SYMBOL)
        (create-accessor read-write))
    (multislot ObraEnElMuseo
        (type INSTANCE)
        (create-accessor read-write))
    (multislot titulo
        (type STRING)
        (create-accessor read-write))
    (multislot AutorObra
        (type INSTANCE)
        (create-accessor read-write))
    (multislot EpocaCuadro
        (type INSTANCE)
        (create-accessor read-write))
    (multislot escuela
        (type STRING)
        (create-accessor read-write))
    (multislot profundidad
        (type FLOAT)
        (create-accessor read-write))
    ;;; Estilo artístico con el que se identifica la obra.
    (multislot estilo
        (type STRING)
        (create-accessor read-write))
    (multislot tematica
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

(defclass Respuesta "Clase que engloba todas las respuestas del sistema"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass Preferencias
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot preferenciaComplejidad
        (type INTEGER)
        (create-accessor read-write))
    ;;; Implica la preferencia del visitante respecto al tipo de obra (paisaje, retrato...)
    (multislot preferenciaTipoObra
        (type STRING)
        (create-accessor read-write))
    (multislot preferenciaTipoDeObra
        (type STRING)
        (create-accessor read-write))
    (multislot preferenciaEpoca
        (type STRING)
        (create-accessor read-write))
    ;;; Dias que el visitante pasará en el museo
    (multislot diasVisita
        (type INTEGER)
        (create-accessor read-write))
    (multislot preferenciaAutor
        (type STRING)
        (create-accessor read-write))
    (multislot preferenciaExitoso
        (type SYMBOL)
        (create-accessor read-write))
    (multislot preferenciaNacionalidad
        (type STRING)
        (create-accessor read-write))
)

(defclass Museo
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot SalaDeUnMuseo
        (type INSTANCE)
        (create-accessor read-write))
)

(defclass Visitante
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Seran las horas que un visitante quiere pasar en un museo por dia
    (single-slot horasPorDia
        (type INTEGER)
        (create-accessor read-write))
    (single-slot esFamilia
        (type STRING)
        (create-accessor read-write))
    ;;; Preferencias que tendrá un visitante al visitar el museo
    (single-slot PreferenciasVisitante
        (type INSTANCE)
        (create-accessor read-write))
    (single-slot tamanyoGrupo
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nacionalidad
        (type STRING)
        (create-accessor read-write))
    (single-slot nivelConocimientoArte
        (type INTEGER)
        (create-accessor read-write))
)

(defclass VisitanteGrupoGrande
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass Tematica "Tematica del cuadro"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass Pintor
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot descripcion
        (type STRING)
        (create-accessor read-write))
    (multislot corriente
        (type STRING)
        (create-accessor read-write))
    (multislot anyoNacimiento
        (type INTEGER)
        (create-accessor read-write))
    (multislot periodoPictorico
        (type STRING)
        (create-accessor read-write))
    (multislot nacionalidad
        (type STRING)
        (create-accessor read-write))
    (multislot anyoMuerte
        (type SYMBOL)
        (create-accessor read-write))
    (multislot nombreAutor
        (type SYMBOL)
        (create-accessor read-write))
    (multislot escuela
        (type STRING)
        (create-accessor read-write))
)

(defclass Accion "Clase que engloba todas las acciones del sistema"
    (is-a USER)
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

(defclass Epoca "Clase que representa una época en concreto"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass VistanteGrupoPequeño
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass Evento "Clase que engloba todos los eventos que tendrá el sistema"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass ConocimientoArte "Esta clase contendrá el conocimiento de cada época que tiene el visitante"
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot conocimientoEdadMedia
        (type SYMBOL)
        (create-accessor read-write))
    (multislot conocimientoPrehistoria
        (type SYMBOL)
        (create-accessor read-write))
    (multislot conocimientoEdadContemporanea
        (type SYMBOL)
        (create-accessor read-write))
    (multislot conocimientoEdadModerna
        (type SYMBOL)
        (create-accessor read-write))
)

(definstances instances
    ([Romanticismo] of Epoca
    )

    ([ElRaptoDeLasSabinas] of Cuadro
         (superficie  11000)
         (anyoDeCreacion  1797)
         (TematicaCuadro  [Historica])
         (AutorObra  [JacquesLousDavid])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([JohnWilliam] of Pintor
    )

    ([LaNocheEstrellada] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([Renacentismo] of Epoca
    )

    ([ElCaballeroDeLaMano] of Cuadro
         (superficie  5412)
         (anyoDeCreacion  1580)
         (TematicaCuadro  [Retrato])
         (AutorObra  [ElGreco])
         (EpocaCuadro  [Manierismo])
    )

    ([LaMajaVestida] of Cuadro
         (AutorObra  [FranciscoDeGoya])
    )

    ([WilliamHogarth] of Pintor
    )

    ([Manierismo] of Epoca
    )

    ([RetratoDeUnaMujerNoble] of Cuadro
         (superficie  5412)
         (anyoDeCreacion  1580)
         (TematicaCuadro  [Retrato])
         (AutorObra  [LaviniaFontana])
         (EpocaCuadro  [Manierismo])
    )

    ([SofiaAnguissola] of Pintor
    )

    ([Lycianna] of Cuadro
         (superficie  5000)
         (anyoDeCreacion  1918)
         (TematicaCuadro  [Retrato])
         (AutorObra  [JohnWilliam])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([Canaletto] of Pintor
    )

    ([Parmigianino] of Pintor
    )

    ([Barroco] of Epoca
    )

    ([LaPastora] of Cuadro
         (AutorObra  [WilliamAdolphe])
    )

    ([JeanHonoreFragonard] of Pintor
    )

    ([PieterBrueghel] of Pintor
    )

    ([LosSirgadoresDeVolga] of Cuadro
         (AutorObra  [IliaRepin])
    )

    ([LosDiscipulosDeEmaus] of Cuadro
         (superficie  27580)
         (anyoDeCreacion  1602)
         (TematicaCuadro  [Historica])
         (AutorObra  [Caravaggio])
         (EpocaCuadro  [Barroco])
    )

    ([Religion] of Tematica
    )

    ([LeonardoDaVinci] of Pintor
    )

    ([Realismo] of Epoca
    )

    ([PlazaDeSanMarcos] of Cuadro
         (superficie  25000)
         (anyoDeCreacion  1723)
         (TematicaCuadro  [Paisaje])
         (AutorObra  [Canaletto])
         (EpocaCuadro  [Rococo])
    )

    ([JoanDeBolonia] of Pintor
    )

    ([GustaveCourbet] of Pintor
    )

    ([JuegosDeNiños] of Cuadro
         (superficie  18998)
         (anyoDeCreacion  1560)
         (TematicaCuadro  [Historica])
         (AutorObra  [PieterBrueghel])
         (EpocaCuadro  [Renacentismo])
    )

    ([LaFamiliaDeCarlosIV] of Cuadro
         (AutorObra  [FranciscoDeGoya])
    )

    ([HabitacionesJuntoAlMar] of Cuadro
         (superficie  7446)
         (anyoDeCreacion  1951)
         (TematicaCuadro  [Paisaje])
         (AutorObra  [EdwardHopper])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([ElColumpio] of Cuadro
         (superficie  5265)
         (anyoDeCreacion  1767)
         (TematicaCuadro  [Religion])
         (AutorObra  [JeanHonoreFragonard])
         (EpocaCuadro  [Rococo])
    )

    ([LasMeninas] of Cuadro
         (superficie  9000)
         (anyoDeCreacion  1656)
         (TematicaCuadro  [Historica])
         (AutorObra  [DiegoVelazquez])
         (EpocaCuadro  [Barroco])
    )

    ([JacquesLousDavid] of Pintor
    )

    ([AmtpmopCprradomo] of Pintor
    )

    ([AutoretratoDeVincent] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([RetratoDeJaneSeymour] of Cuadro
         (superficie  7500)
         (anyoDeCreacion  1536)
         (TematicaCuadro  [Retrato])
         (AutorObra  [HansHolbein])
         (EpocaCuadro  [Renacentismo])
    )

    ([ElJardinDelArtistaEnGiverny] of Cuadro
         (AutorObra  [ClaudeMonet])
    )

    ([ElAmorVictorioso] of Cuadro
         (superficie  17628)
         (anyoDeCreacion  1601)
         (TematicaCuadro  [NaturalezaMuerta])
         (AutorObra  [Caravaggio])
         (EpocaCuadro  [Barroco])
    )

    ([Tiziano] of Pintor
    )

    ([LaviniaFontana] of Pintor
    )

    ([UnBañoDeAsnieres] of Cuadro
         (AutorObra  [GeogrgesPierre])
    )

    ([VincentVanGogh] of Pintor
    )

    ([ElGreco] of Pintor
    )

    ([WinslowHomer] of Pintor
         (nombreAutor  "Winslow Homer")
    )

    ([NaturalezaMuerta] of Tematica
    )

    ([BernardinoCampiPintandoASofiaAnguissola] of Cuadro
         (superficie  7000)
         (anyoDeCreacion  1559)
         (TematicaCuadro  [Retrato])
         (AutorObra  [SofiaAnguissola])
         (EpocaCuadro  [Manierismo])
    )

    ([FranciscoDeGoya] of Pintor
    )

    ([Almiar] of Cuadro
         (AutorObra  [ClaudeMonet])
    )

    ([GeogrgesPierre] of Pintor
    )

    ([Paisaje] of Tematica
    )

    ([FransHals] of Pintor
    )

    ([LaUltimaCena] of Cuadro
         (superficie  40480000)
         (anyoDeCreacion  1496)
         (TematicaCuadro  [Religion])
         (AutorObra  [LeonardoDaVinci])
         (EpocaCuadro  [Renacentismo])
    )

    ([MigueAngel] of Pintor
    )

    ([VenusDelEspejo] of Cuadro
         (superficie  12896)
         (TematicaCuadro  [Religion])
         (AutorObra  [Tiziano])
         (EpocaCuadro  [Manierismo])
    )

    ([AntonioCanova] of Pintor
    )

    ([LosComedoresDePatatas] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([AFavorDeLaBrisa] of Cuadro
         (titulo  "A favor de la brisa")
         (AutorObra  [WinslowHomer])
    )

    ([ElJovenAzul] of Cuadro
         (superficie  19360)
         (anyoDeCreacion  1770)
         (TematicaCuadro  [Retrato])
         (AutorObra  [ThomasGainsborough])
         (EpocaCuadro  [Rococo])
    )

    ([RailroadSunset] of Cuadro
         (superficie  6500)
         (anyoDeCreacion  1929)
         (TematicaCuadro  [Paisaje])
         (AutorObra  [EdwardHopper])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([LaRondaDeNoche] of Cuadro
         (superficie  158631)
         (anyoDeCreacion  1641)
         (TematicaCuadro  [Historica])
         (AutorObra  [Rembrandt])
         (EpocaCuadro  [Barroco])
    )

    ([JarronConClaveles] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([LaMajaDesnuda] of Cuadro
         (AutorObra  [FranciscoDeGoya])
    )

    ([Rembrandt] of Pintor
    )

    ([Caravaggio] of Pintor
    )

    ([ElAquelarre] of Cuadro
         (AutorObra  [FranciscoDeGoya])
    )

    ([ElNiñoDeLaPeonza] of Cuadro
         (superficie  5092)
         (anyoDeCreacion  1735)
         (TematicaCuadro  [Retrato])
         (AutorObra  [JeanSimeon])
         (EpocaCuadro  [Rococo])
    )

    ([Autoretrato] of Cuadro
         (anyoDeCreacion  1745)
         (TematicaCuadro  [Retrato])
         (AutorObra  [WilliamHogarth])
         (EpocaCuadro  [Rococo])
    )

    ([WilliamAdolphe] of Pintor
    )

    ([JeanSimeon] of Pintor
    )

    ([MujerJudiaVendiendoNaranjas] of Cuadro
         (AutorObra  [AleksanderGyermski])
    )

    ([CaballeroSonriente] of Cuadro
         (superficie  5561)
         (anyoDeCreacion  1624)
         (TematicaCuadro  [Retrato])
         (AutorObra  [FransHals])
         (EpocaCuadro  [Barroco])
    )

    ([Retrato] of Tematica
    )

    ([Rococo] of Epoca
    )

    ([LaMatanzaDeQuios] of Cuadro
         (AutorObra  [EugeneDelacroix])
    )

    ([ElNacimientoDeVenus] of Cuadro
         (superficie  47816)
         (anyoDeCreacion  1485)
         (TematicaCuadro  [Religion])
         (AutorObra  [SandroBoticelli])
         (EpocaCuadro  [Renacentismo])
    )

    ([ClaudeMonet] of Pintor
    )

    ([PuenteDeNarni] of Cuadro
         (AutorObra  [CamileCorot])
    )

    ([VistaDeToledo] of Cuadro
         (superficie  2064)
         (anyoDeCreacion  1598)
         (TematicaCuadro  [Paisaje])
         (AutorObra  [ElGreco])
         (EpocaCuadro  [Manierismo])
    )

    ([Lirios] of Cuadro
         (AutorObra  [VincentVanGogh])
    )

    ([SandroBoticelli] of Pintor
    )

    ([VenusDelEspejoVelazquez] of Cuadro
         (superficie  21594)
         (anyoDeCreacion  1647)
         (TematicaCuadro  [Religion])
         (AutorObra  [DiegoVelazquez])
         (EpocaCuadro  [Barroco])
    )

    ([Neoclacisismo] of Epoca
    )

    ([CestoConFrutas] of Cuadro
         (superficie  2300)
         (anyoDeCreacion  1596)
         (TematicaCuadro  [NaturalezaMuerta])
         (AutorObra  [Caravaggio])
         (EpocaCuadro  [Barroco])
    )

    ([ElDesesperado] of Cuadro
         (AutorObra  [GustaveCourbet])
    )

    ([ThomasGainsborough] of Pintor
    )

    ([LaMuerteDeMarat] of Cuadro
         (superficie  21120)
         (anyoDeCreacion  1793)
         (TematicaCuadro  [Historica])
         (AutorObra  [JacquesLousDavid])
         (EpocaCuadro  [Neoclacisismo])
    )

    ([CamileCorot] of Pintor
    )

    ([Historica] of Tematica
    )

    ([AmorSacroYAmorProfano] of Cuadro
         (superficie  32922)
         (anyoDeCreacion  1515)
         (TematicaCuadro  [Religion])
         (AutorObra  [Tiziano])
         (EpocaCuadro  [Renacentismo])
    )

    ([EugeneDelacroix] of Pintor
    )

    ([EdwardHopper] of Pintor
    )

    ([FranciscoGuitierrez] of Pintor
    )

    ([HombreDeVitruvio] of Cuadro
         (superficie  910)
         (anyoDeCreacion  1492)
         (TematicaCuadro  [Historica])
         (AutorObra  [LeonardoDaVinci])
         (EpocaCuadro  [Renacentismo])
    )

    ([IliaRepin] of Pintor
    )

    ([Impresionismo] of Epoca
    )

    ([LaCerradura] of Cuadro
         (superficie  6532)
         (anyoDeCreacion  1778)
         (TematicaCuadro  [Historica])
         (AutorObra  [JeanHonoreFragonard])
         (EpocaCuadro  [Rococo])
    )

    ([Gioconda] of Cuadro
         (superficie  4081)
         (anyoDeCreacion  1503)
         (TematicaCuadro  [Retrato])
         (AutorObra  [LeonardoDaVinci])
         (EpocaCuadro  [Renacentismo])
    )

    ([AleksanderGyermski] of Pintor
    )

    ([VIsionOfSantJerome] of Cuadro
         (superficie  6000)
         (anyoDeCreacion  1526)
         (TematicaCuadro  [Religion])
         (AutorObra  [Parmigianino])
         (EpocaCuadro  [Manierismo])
    )

    ([HansHolbein] of Pintor
    )

    ([AugusteRodin] of Pintor
    )

    ([LaTrinidad] of Cuadro
         (superficie  53700)
         (anyoDeCreacion  1578)
         (TematicaCuadro  [Religion])
         (AutorObra  [ElGreco])
         (EpocaCuadro  [Manierismo])
    )

    ([DiegoVelazquez] of Pintor
    )

    ([LaLibertadGuiandoAlPueblo] of Cuadro
         (AutorObra  [EugeneDelacroix])
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
                        (bind $?answers(insert$ $?answers (+ (length$ $?answers) 1) ?answer))
                        (printout t crlf)
                )

                (loop-for-count (?i 1 4) do
                        (bind ?*edadModernaKnowledge* (+ ?*edadModernaKnowledge* (nth$ ?i ?answers)))
                )

                (loop-for-count (?i 5 9) do
                        (bind ?*edadContemporaneaKnowledge* (+ ?*edadContemporaneaKnowledge* (nth$ ?i ?answers)))
                )                  
               
                (bind ?list (create$ ?*edadModernaKnowledge* ?*edadContemporaneaKnowledge*))
                ?list

)

;Función que crea todas las preguntas necesarias para evaluar el nivel de arte del visitante
(deffunction createArtQuestions (?random)
        ;prehistoria
       ; (bind ?p1 "Valora del 1 al 5 tus conocimientos sobre Arte paleolitico (h. 40.000 - 10.000 a.C.)")
       ; (bind ?p2 "Valora del 1 al 5 tus conocimientos sobre Arte mesolitico (h. 10.000 - 4000 a.C.)")
        ;(bind ?p3 "Valora del 1 al 5 tus conocimientos sobre Arte neolitico (h. 4000 - 2000 a.C.)")

        ;Edad antigua
        ;(bind ?p4 "Valora del 1 al 5 tus conocimientos sobre Arte egipcio (h. 5300 - 30 a.C.)")
        ;(bind ?p5 "Valora del 1 al 5 tus conocimientos sobre Arte mesopotamico (h. 4000 - 539 a.C.)")
       ; (bind ?p6 "Valora del 1 al 5 tus conocimientos sobre Arte minoico (h. 3000 - 1400 a.C.)")
        ;(bind ?p7 "Valora del 1 al 5 tus conocimientos sobre Arte micenico (h. 1500 - 1100 a.C.)")
        ;(bind ?p8 "Valora del 1 al 5 tus conocimientos sobre Arte griego (h. 1000 - 320 a.C.)")
        ;(bind ?p9 "Valora del 1 al 5 tus conocimientos sobre Arte etrusco (h. 800 - 100 a.C.)")
        ;(bind ?p10 "Valora del 1 al 5 tus conocimientos sobre Arte romano (h. 400 a.C - 476 d.C.)")

        ;Edad media
        ;(bind ?p11 "Valora del 1 al 5 tus conocimientos sobre Arte paleocristiano (h. s.I - IV)")
        ;(bind ?p12 "Valora del 1 al 5 tus conocimientos sobre Arte visigodo (h. 415 - 711)")
        ;(bind ?p13 "Valora del 1 al 5 tus conocimientos sobre Arte bizantino (h. 330 – 1.453)")
        ;(bind ?p14 "Valora del 1 al 5 tus conocimientos sobre Arte mozarabe (h. 711 - 1.000)")
        ;(bind ?p15 "Valora del 1 al 5 tus conocimientos sobre Arte carolingio (h. 780 - 900)")
        ;(bind ?p16 "Valora del 1 al 5 tus conocimientos sobre Arte otoniano (h. 950 - 1050)")
        ;(bind ?p17 "Valora del 1 al 5 tus conocimientos sobre Arte romanico (s. XI - XIII)")
        ;(bind ?p18 "Valora del 1 al 5 tus conocimientos sobre Arte gotico (s. XII - XVI)")
        ;(bind ?p19 "Valora del 1 al 5 tus conocimientos sobre Arte mudejar (s.XII - XVI)")

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

        (bind ?list (create$ ?p20 ?p21 ?p22 ?p23 ?p24 ?p25 ?p26 ?p27 ?p28))
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
                (declare (salience 2))
                =>
                >(make-instance visitante of Visitante)
                (bind ?size (numberQuestion "Cual es el tamanyo de tu grupo?"))
                >(send [visitante] put-tamanyoGrupo ?size)

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

