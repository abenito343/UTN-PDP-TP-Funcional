module Spec where
import PdePreludat
import Library
import Test.Hspec


correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de coeficienteDeSatisfaccion" $ do
    it "El coeficiente de satisfacción de una persona muy feliz" $ do
      coeficienteDeSatisfaccion schwarzenegger `shouldBe` 2525
    it "El coeficiente de satisfacción de una persona moderadamente feliz" $ do
      coeficienteDeSatisfaccion ariel `shouldBe` 200
    it "El coeficiente de satisfacción de una persona poco feliz" $ do
      coeficienteDeSatisfaccion melina  `shouldBe` 25
      
  describe "Test de gradoDeAmbicion" $ do
    it "El grado de ambición de una persona muy feliz" $ do
      gradoDeAmbicion schwarzenegger `shouldBe` 202
    it "El grado de ambición de una persona moderadamente feliz" $ do
      gradoDeAmbicion ariel `shouldBe` 52
    it "El grado de ambición de una persona poco feliz" $ do
      gradoDeAmbicion melina `shouldBe` 2

  describe "Test de nombreLindo" $ do
    it "Una persona que No tiene nombre lindo" $ do
      nombreLindo ariel `shouldBe` False
    it "Una persona que se tiene un nombre lindo" $ do
      nombreLindo melina `shouldBe` True

  describe "Test de personaSuertuda" $ do
    it "Una persona no suertuda" $ do -- da 125 el coeficienteDeSatisfaccion x 3
      personaSuertuda melina `shouldBe` False
    it "Una persona suertuda" $ do -- da 600 el coeficienteDeSatisfaccion x 3
      personaSuertuda ariel `shouldBe` True

  describe "Test de nombreLargo" $ do
    it "Una persona con nombre largo" $ do -- la persona tiene un nombre de más de 10 caracteres.
      nombreLargo schwarzenegger `shouldBe` True
    it "Una persona sin nombre largo" $ do -- la persona no tiene un nombre de más de 10 caracteres.
      nombreLargo ariel `shouldBe` False

  describe "Test de suenoConformista" $ do
    it "Mantiene a la persona sin cambios" $ do
      (show . suenoConformistas) melina `shouldBe` show melina

  describe "Test de suenoViajar" $ do
    it "Una persona que viaja" $ do
      (show . suenoViajar ["Tigre", "Paris"]) schwarzenegger `shouldBe` show schwarzeneggerViajo

  describe "Test de suenoEnamorarse" $ do
    it "Una persona que se enamora" $ do
      (show . suenoEnamorarse melina) ariel  `shouldBe` show arielEnamorado 

  describe "Test de suenoPackMedicina" $ do
    it "Una persona que se recibe en medicina, viaja a Berazategui y Paris" $ do
      (show . suenoPackMedicina) erik `shouldBe` show erikRecibido

  describe "Test de triplicarSueno" $ do
    it "Una persona que triplica sus sueños" $ do
      (show . (triplicarSueno (suenoEnamorarse melina))) ariel `shouldBe` show arielEnamoradoTriplePower

-------------
  describe "Test de fuenteMinimalista" $ do
    it "Una persona que cumple su primer deseo" $ do
      (show . fuenteMinimalista) maria `shouldBe` show mariaminimalista

  describe "Test de fuenteCopada" $ do
    it "Una vez que la fuente le cumplió todos los deseos la persona se queda sin suenos" $ do
      (null . suenos . fuenteCopada) schwarzenegger `shouldBe` True
    it "La persona cumplio todos sus suenios" $ do
      (show . fuenteCopada) schwarzenegger `shouldBe` show schwarzeneggerFuenteCopada

  describe "Test de fuenteAPedido" $ do
    it "Una persona cumple su enésimo sueño" $ do
      (show . fuenteAPedido 0) ariel `shouldBe` show arielIngeniero

  describe "Test de fuenteSorda" $ do
    it "No le cumple ningun suenio" $ do
      (show . fuenteSorda) schwarzenegger `shouldBe` show schwarzenegger

  describe "Test de FuenteGanadora" $ do
    it "La fuente que mas satisfecha deja a una persona" $ do
     ( show . fuenteGanadora criterioSatisfecha maria) listadefuentes `shouldBe` show fuenteCopada
    it "La fuente mas vieja que deja a una persona" $ do
     ( show . fuenteGanadora criterioVieja homero) listadefuentes3 `shouldBe` show fuenteSorda
    it "La fuente con mas habilidades que deja a una persona" $ do
     ( show . fuenteGanadora criterioHabilidades maria) (listadefuentes2 1) `shouldBe` show fuenteAPedido
   

  describe "Test de sonValiosos" $ do
    it "los suenos valiosos que se le dan a erik" $ do
      (show . sonValiosos listadesuenos) erik `shouldBe` show listadesuenosValiosos

  describe "Test de tieneSuenioRaro" $ do
    it "Tiene un sueno que al cumplirse no modifica los felicidonios de la persona" $ do
      tieneSuenioRaro ned `shouldBe` True
    it "No tiene un sueno que al cumplirse no modifica los felicidonios de la persona" $ do
      tieneSuenioRaro melina `shouldBe` False

  describe "Test de felicidadResultantes" $ do
    it "La felicidad total del grupo, luego de cumplir sus sueños" $ do
      felicidadResultantes [homero,ned,erik]`shouldBe` 15015

  describe "Test de cuantasVecesSeDebeCumplir" $ do
    it "La persona tiene mas de 1000 felicidonios" $ do
      cuantasVecesSeDebeCumplir schwarzeneggerFuenteCopada suenoConformistas `shouldBe` 0
    it "La persona no tiene mas de 1000 felicidonios" $ do
      cuantasVecesSeDebeCumplir ned (suenoRecibirse "Parroco") `shouldBe` 1


-- Punto 1 c
-- ¿qué representa las cursivas muy feliz, poco feliz, moderadamente feliz?
-- representan el grado de felicidad que tiene cada persona, determinado por la cantidad de felicidonios que tiene cada una

-- ¿Cuál es el criterio para elegir 50, 100 y 101 felicidonios dentro de las pruebas? Justifique
-- El criterio es probar todos los limites que tienen las funciones, ya que para mas de 100 felicidonios pasa algo,
-- si entra entre 50 y 100 felicidonios pasa otra cosa y con menos de 50 pasa otra cosa distinta a las anteriores