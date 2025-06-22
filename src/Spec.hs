module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      estaIncluidaEn "ola" "Hola que tal" `shouldBe` True
  describe "Tests de queLeEnvio" $ do
    it "Sólo se queda con los paquetes correspondientes" $ do
      queLeEnvio "192.168.1.102" "192.168.1.103" [paqA, paqB] `shouldBe` [paqA]
    it "Ordena los paquetes de acuerdo al número" $ do
      queLeEnvio "192.168.1.102" "192.168.1.103" [paqA, paqD] `shouldBe` [paqD, paqA]
  describe "otros tests (no pedidos)" $ do
    it "dejaPasar no deja pasar si no cumple con la máscara" $ do
      dejaPasar firewall1 paqA `shouldBe` False
    it "dejaPasar deja pasar si cumple con la máscara" $ do
      dejaPasar firewall1 paqB `shouldBe` True
    it "dejaPasar no deja pasar si no cumple con la regla" $ do
      dejaPasar firewall1 paqG `shouldBe` False
    it "estaCompleta falla si faltan números de paquete" $ do
      estaCompleta "10.1.1.55" "192.168.1.102" paquetesEjemplo `shouldBe` False
    it "estaCompletaanda si están los paquetes completos" $ do
      estaCompleta "192.168.1.102" "192.168.1.103" paquetesEjemplo `shouldBe` True
    it "Conocer el mensaje ordena y concatena" $ do
      mensaje "10.1.1.55" "192.168.1.102" paquetesEjemplo `shouldBe` "<br/> genial <h1> Proximo"
    it "El mensaje seguro pasa por el firewall" $ do
      mensajeSeguro firewall2 "192.168.1.102" "192.168.1.103" paquetesEjemplo `shouldBe` "Hola checómoestás?"
    it "El mensaje seguro incompleto avisa" $ do
      mensajeSeguro firewall1 "10.1.1.55" "192.168.1.102" paquetesEjemplo `shouldBe` "Mensaje incompleto"
