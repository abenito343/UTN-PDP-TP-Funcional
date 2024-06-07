  
module Library where
import PdePreludat

type Nombre = String
type Edad = Number
type Felicidonios = Number
type Habilidad = String
type Sueno =  Persona -> Persona 
type Carrera = String
type Ciudad = String

data Persona = Persona {
    nombre :: Nombre,
    edad :: Edad,
    suenos :: [Sueno],
    felicidonios :: Felicidonios,
    habilidades :: [Habilidad]
} deriving Show


schwarzenegger :: Persona
schwarzenegger =  Persona {
    nombre = "Schwarzenegger", 
    edad = 25, 
    suenos = [(suenoRecibirse "arquitectura"), (suenoViajar ["Tigre", "Paris"])],
    felicidonios = 101,
    habilidades = ["carpinteria"]
   
} --muy feliz

schwarzeneggerViajo :: Persona
schwarzeneggerViajo =  Persona {
    nombre = "Schwarzenegger", 
    edad = 26, 
    suenos = [(suenoRecibirse "arquitectura"), (suenoViajar ["Tigre", "Paris"])],
    felicidonios = 301,
    habilidades = ["carpinteria"]
} --muy feliz

schwarzeneggerFuenteCopada :: Persona
schwarzeneggerFuenteCopada =  Persona {
    nombre = "Schwarzenegger", 
    edad = 26, 
    suenos = [],
    felicidonios = 12301,
    habilidades = ["arquitectura","carpinteria"]
}


ariel :: Persona
ariel = Persona {
    nombre = "Ariel",
    edad = 26,
    suenos = [(suenoRecibirse "ingenieria"), (suenoViajar ["Dubai"]) ],
    felicidonios = 100, 
    habilidades = ["pintura"]
} --moderadamente feliz

arielEnamorado :: Persona
arielEnamorado = Persona {
    nombre = "Ariel",
    edad = 26,
    suenos = [(suenoRecibirse "ingenieria"), (suenoViajar ["Dubai"]) ],
    felicidonios = 150, 
    habilidades = ["pintura"]
}

arielEnamoradoTriplePower :: Persona
arielEnamoradoTriplePower = Persona {
    nombre = "Ariel",
    edad = 26,
    suenos = [(suenoRecibirse "ingenieria"), (suenoViajar ["Dubai"]) ],
    felicidonios = 250, 
    habilidades = ["pintura"]
}

arielIngeniero :: Persona
arielIngeniero = Persona{
    nombre = "Ariel",
    edad = 26,
    suenos = [(suenoRecibirse "ingenieria"), (suenoViajar ["Dubai"])],
    felicidonios = 10100,
    habilidades = ["ingenieria","pintura"]
}
melina :: Persona
melina = Persona {
    nombre = "Melina",
    edad = 50,
    suenos = [(suenoRecibirse "abogacia")],
    felicidonios = 50,
    habilidades = ["danza"]
}-- poco feliz

erik:: Persona
erik =  Persona {
    nombre = "erik", 
    edad = 30, 
    suenos = [(suenoRecibirse "medicina")],
    felicidonios = 0,
    habilidades = ["mecanico"]
} 

erikRecibido:: Persona
erikRecibido =  Persona {
    nombre = "erik", 
    edad = 31, 
    suenos = [(suenoRecibirse "medicina")],
    felicidonios = 8300,
    habilidades = ["medicina", "mecanico"]
} 
maria :: Persona
maria = Persona {
    nombre = "Maria",
    edad = 42,
    suenos = [(suenoRecibirse "psicologia"), (suenoViajar ["suiza"]) ],
    felicidonios = 100, 
    habilidades = ["bondad"]
}
mariaminimalista :: Persona
mariaminimalista = Persona {
    nombre = "Maria",
    edad = 42,
    suenos = [ (suenoViajar ["suiza"]) ],
    felicidonios = 10100, 
    habilidades = ["psicologia","bondad"]
}

ned :: Persona
ned =  Persona {
    nombre = "Ned Flanders", 
    edad = 30, 
    suenos = [suenoConformistas, suenoRecibirse "Parroco"],
    felicidonios = 0,
    habilidades = ["resar"]
} 

homero:: Persona
homero =  Persona {
    nombre = "homero", 
    edad = 15, 
    suenos = [],
    felicidonios = 15,
    habilidades = ["mecanico"]
} 

listadefuentes ::[Fuente]
listadefuentes  =[fuenteMinimalista,fuenteCopada ,fuenteSorda]

listadefuentes2 ::Number-> [Fuente]
listadefuentes2 numero= [fuenteAPedido numero,fuenteSorda]

listadefuentes3 :: [Fuente]
listadefuentes3 = [fuenteSorda]

listadefuentes4 :: [Fuente]
listadefuentes4 = [fuenteMinimalista,fuenteSorda]

listadesuenos :: [Sueno]
listadesuenos = [(suenoViajar ["suiza"]),(suenoRecibirse "ingenieria"),(suenoViajar ["suiza","holanda"])]
listadesuenosValiosos = [(suenoRecibirse "ingenieria"),(suenoViajar ["suiza","holanda"])]



personaMuyFeliz :: Persona -> Bool
personaMuyFeliz = (> 100) . felicidonios

personaModeradamenteFeliz :: Persona -> Bool
personaModeradamenteFeliz = between 50 100 . felicidonios

coeficienteDeSatisfaccion :: Persona -> Number
coeficienteDeSatisfaccion persona
    | personaMuyFeliz persona = felicidonios persona * edad persona
    | personaModeradamenteFeliz persona = cantidadDeSuenos persona * felicidonios persona
    | otherwise = (truncate . (/ 2) . felicidonios) persona

cantidadDeSuenos :: Persona -> Number
cantidadDeSuenos = length . suenos


gradoDeAmbicion :: Persona -> Number
gradoDeAmbicion persona
    | personaMuyFeliz persona = felicidonios persona * cantidadDeSuenos persona
    | personaModeradamenteFeliz persona = edad persona * cantidadDeSuenos persona
    | otherwise = ((2 *) . cantidadDeSuenos) persona

between :: Ord a => a -> a -> a -> Bool
between bajo alto medio = bajo < medio && medio <= alto

cantidadDeCaracteres :: String -> Number
cantidadDeCaracteres = length

nombreLargo  ::  Persona  ->  Bool
nombreLargo = (> 10) . cantidadDeCaracteres . nombre

personaSuertuda :: Persona -> Bool
personaSuertuda = even . (3 *) . coeficienteDeSatisfaccion

ultimaLetra :: String -> Char
ultimaLetra = last

nombreLindo :: Persona -> Bool
nombreLindo = (== 'a') . ultimaLetra . nombre


suenoRecibirse :: Carrera -> Sueno
suenoRecibirse carrera = felicidoniosPorCarrera carrera . agregarCarrera carrera

felicidoniosPorCarrera :: Carrera -> Persona -> Persona
felicidoniosPorCarrera nombreCarrera = incrementarFelicidonios (1000 * length  nombreCarrera)

agregarCarrera :: Carrera -> Persona -> Persona
agregarCarrera carrera persona = persona {habilidades = carrera : habilidades persona}

suenoViajar :: [Ciudad] -> Sueno
suenoViajar ciudades persona = (felicidoniosPorViaje ciudades) persona {edad =  ((+ 1) . edad) persona}

felicidoniosPorViaje :: [Ciudad] -> Persona -> Persona
felicidoniosPorViaje ciudades persona  = incrementarFelicidonios (100 * length  ciudades) persona

incrementarFelicidonios :: Number -> Persona -> Persona
incrementarFelicidonios incremento persona = persona { felicidonios = incremento + felicidonios persona}

suenoEnamorarse :: Persona -> Sueno
suenoEnamorarse persona enamorado = enamorado {felicidonios = felicidonios enamorado + felicidonios persona}

suenoConformistas :: Sueno
suenoConformistas persona = persona

suenoPackMedicina :: Sueno
suenoPackMedicina persona = (suenoRecibirse "medicina" . suenoViajar ["Berazategui", "París"] . incrementarFelicidonios 100) persona

triplicarSueno :: Sueno -> Sueno
triplicarSueno funcionSueno = funcionSueno . funcionSueno . funcionSueno


felicidoniosPersona :: Persona -> Number
felicidoniosPersona = felicidonios

-- PUNTO 4
type Fuente = Persona -> Persona

-- integrante 1
fuenteMinimalista :: Fuente
fuenteMinimalista = quitarPrimerSuenio . cumplirPrimerSuenio

cumplirSuenio :: Persona -> Sueno -> Persona
cumplirSuenio persona suenioACumplir = suenioACumplir persona

cumplirPrimerSuenio :: Persona -> Persona
cumplirPrimerSuenio persona = (cumplirSuenio persona . head . suenos) persona

quitarPrimerSuenio :: Persona -> Persona
quitarPrimerSuenio persona = persona {suenos = (tail . suenos) persona}

-- integrante 2
fuenteCopada :: Fuente
fuenteCopada =  quitarSuenios . cumplirTodosLosSuenos

quitarSuenios :: Persona -> Persona
quitarSuenios persona = persona {suenos = []}

-- integrante 3
fuenteAPedido :: Number -> Fuente
fuenteAPedido nroDeSueno persona = (cumplirSuenio persona . tomarSuenioNro nroDeSueno) persona

tomarSuenioNro :: Number -> Persona -> Sueno
tomarSuenioNro posicion = (!! posicion) . suenos

-- Integrador
fuenteSorda :: Fuente
fuenteSorda = id

{- Para la defensa del TP (cualquier integrante):
¿cómo se modelaron cada una de las fuentes? ¿por qué?

-}

-- PUNTO 5
type CriterioFuente = Persona -> Number

-- integrante 1
criterioSatisfecha :: CriterioFuente
criterioSatisfecha = felicidoniosPersona

-- integrante 2
criterioVieja :: CriterioFuente
criterioVieja = edad

-- integrante 3
criterioHabilidades :: CriterioFuente
criterioHabilidades = length.habilidades

--
mejorFuenteSegunCriterio :: CriterioFuente -> Persona -> Fuente -> Fuente -> Fuente
mejorFuenteSegunCriterio criterio persona fuente1 fuente2 
    | criterio (fuente1 persona) > criterio (fuente2 persona) = fuente1
    | otherwise = fuente2

fuenteGanadora :: CriterioFuente -> Persona->[Fuente]->Fuente
fuenteGanadora criterio persona = foldl1 (mejorFuenteSegunCriterio criterio persona) 

{- Para la defensa del TP (cualquier integrante):
¿dónde aparecen los conceptos aplicación parcial y orden superior? Justifique.

-}

-- PUNTO 6
-- integrante 1
sonValiosos :: [Sueno] ->Persona ->  [Sueno]
sonValiosos suenos persona = filter (personaQuedaMuyFeliz persona) suenos

personaQuedaMuyFeliz ::  Persona  ->Sueno-> Bool
personaQuedaMuyFeliz persona sueno = ((>100).felicidoniosPersona.(aplicarSuenoFlipeado sueno)) persona

aplicarSuenoFlipeado :: Sueno ->Persona -> Persona
aplicarSuenoFlipeado sueno persona = cumplirSuenio persona sueno

-- integrante 2
tieneSuenioRaro :: Persona -> Bool
tieneSuenioRaro persona = (any (suenioRaro persona) . suenos) persona

suenioRaro :: Persona -> Sueno -> Bool
suenioRaro persona = (felicidonios persona ==) . felicidonios . cumplirSuenio persona

-- integrante 3
felicidadResultante ::  Fuente -> Persona -> Number
felicidadResultante fuente persona  = felicidonios (fuente persona)

cumplirTodosLosSuenos :: Persona -> Persona
cumplirTodosLosSuenos persona = (foldl cumplirSuenio persona . suenos) persona

felicidadResultantes :: [Persona] -> Number
felicidadResultantes = sum . map (felicidadResultante cumplirTodosLosSuenos)

-- Integrador
cuantasVecesSeDebeCumplir :: Persona -> Sueno -> Number
cuantasVecesSeDebeCumplir persona suenio
    | ((> 1000) . felicidonios) persona = 0
    | otherwise = 1 + cuantasVecesSeDebeCumplir (suenio persona) suenio


{- Para la defensa del TP (cualquier integrante):
¿cómo se relacionan las distintas soluciones respecto al concepto de declaratividad? Justifique.

Es importante el concepto de declaratividad debido a que en el punto, se utilizan muchas sentencias que involucran a los felicidonios,
y si no hubiera una buena declaratividad seria muy dificil intentar interpretar el codigo a la hora de revisarlo, mejorarlo, o
hasta para implementar una solucion.
-}

{- PUNTO 7
Modelar una persona con sueños infinitos. Cada integrante debe responder: teniendo en cuenta el requerimiento 
que le tocó al modelar la fuente en el punto 4, ¿es posible que la fuente pueda utilizarse con esa persona que 
tiene infinitos sueños? Justifique su respuesta con un ejemplo concreto: “a esta persona P0 con infinitos sueǹos 
S0 y la Fuente F1 la invoco en la consola y sucede X porque... (etc. etc. etc.)” y relaciónelo con algún concepto 
visto en la cursada.

Integrante 1: Fuente Minimalista
erik:: Persona
erik =  Persona {
    nombre = "erik", 
    edad = 30, 
    suenos = repeat (suenoRecibirse "medicina"),
    felicidonios = 0,
    habilidades = ["mecanico"]
} 
Si es posible, gracias a que haskell utiliza lazy evaluation, esto hace que el programa no evalue completamente la lista infinita ,
sino lo que se pide , y en este caso en particular lo que se pide si lo puede ejecutar, porque solo trabaja con el primer elemento, no con la lista entera,
 ya que lo toma, tanto para aplicarlo como para eliminarlo.

Integrante 2: Fuente Copada
melina :: Persona
melina = Persona {
    nombre = "Melina",
    edad = 50,
    suenos = repeat (suenoRecibirse "abogacia"),
    felicidonios = 50,
    habilidades = ["danza"]
}
No es posible ya que si tengo una lista infinita de sueños nunca llegaria al final, por lo tanto nose podrian
cumplir todos los sueños de la persona.
Por ejemplo utilizando a melina no se podria, ya que al tener infinitos sueños la consola se me tida, ya que 
no tiene un tope, y va a seguir cumpliendo los sueños.

Integrante 3: Fuente a pedido
ariel :: Persona
ariel = Persona {
    nombre = "Ariel",
    edad = 26,
    suenos = repeat (suenoRecibirse "ingenieria"),
    felicidonios = 100, 
    habilidades = ["pintura"]
}
A esta persona infinita con infinitos sueños y la fuente a pedido la invoco en la consola
y se puede ejecutar por que solo le cumple el enesimo sueño y ya gracias a haskell que utiliza lazy evaluation,
 no se pone a ver cuantos sueños tiene ni hay alguna razon por la que se quede ejecutando o haga 
algun loop infinito,la funcion va a la posicion del sueño que le mandamos y cumple ese sueño 
sin borrarlo, asi que tampoco tiene que reasignar la lista infita. Se podria ejecutar sin problemas.
-}