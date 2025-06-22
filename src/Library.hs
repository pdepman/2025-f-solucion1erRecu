-- Apellido y Nombre: 

module Library where
import PdePreludat
import Data.List (isInfixOf, sortOn)

estaIncluidaEn :: String -> String -> Bool
estaIncluidaEn = isInfixOf

ordenarSegun :: Ord b => (a -> b) -> [a] -> [a]
ordenarSegun = sortOn

-- Punto 1)
data Paquete = UnPaquete {
    origen   :: String,
    destino  :: String,
    numero   :: Number,
    datos    :: String
} deriving (Show, Eq)

paqA, paqB, paqC, paqD, paqE, paqF, paqG :: Paquete
paqA = UnPaquete "192.168.1.102" "192.168.1.103" 5 "cómo"
paqB = UnPaquete "10.1.1.55" "192.168.1.102" 679 "<h1> Proximo"
paqC = UnPaquete "10.1.1.55" "192.168.1.102" 676 "<br/> genial "
paqD = UnPaquete "192.168.1.102" "192.168.1.103" 4 "Hola che"
paqE = UnPaquete "192.168.1.102" "192.168.1.103" 6 "estás?"
paqF = UnPaquete "10.1.1.55" "10.1.1.56" 7 "gbmtp"
paqG = UnPaquete "192.168.1.102" "10.1.1.56" 1002 "Haz una apuesta"

paquetesEjemplo :: [Paquete]
paquetesEjemplo = [paqA, paqB, paqC, paqD, paqE, paqF, paqG]

-- Punto 2)

type Regla = Paquete -> Bool

data Firewall = Firewall {
    mascara :: String,
    reglas  :: [Regla]
}

dejaPasar :: Firewall -> Paquete -> Bool
dejaPasar firewall paq = subMascara (mascara firewall) paq && all ($ paq) (reglas firewall)

positivos :: Regla
positivos = (>0) . numero

listaNegra :: [String] -> Regla
listaNegra lista = not . flip elem lista . origen

subMascara :: String -> Regla
subMascara mascara paq = empiezaIgualQue mascara (origen paq) || empiezaIgualQue mascara (destino paq)

empiezaIgualQue :: String -> String -> Bool
empiezaIgualQue una otra = una == take (length una) otra

palabraClave :: String -> Regla
palabraClave palabra = not . estaIncluidaEn palabra . datos

-- Punto 3)
firewall1 :: Firewall
firewall1 = Firewall "10.1" [palabraClave "apuesta", palabraClave "xxx", positivos]

firewall2 :: Firewall 
firewall2 = Firewall "192" [subMascara "192.168.1", palabraClave "apuesta", listaNegra ["192.168.1.104", "10.1.1.55"]]

-- Punto 4a)

queLeEnvio :: String -> String -> [Paquete] -> [Paquete]
queLeEnvio orig dest = ordenarSegun numero . filter (envioA orig dest)

envioA :: String -> String -> Paquete -> Bool
envioA orig dest paq = origen paq == orig && destino paq == dest

-- Punto 4c)
estaCompleta :: String -> String -> [Paquete] -> Bool
estaCompleta orig dest = consecutivos . queLeEnvio orig dest

consecutivos :: [Paquete] -> Bool
consecutivos [] = True
consecutivos [_] = True
consecutivos (p1:p2:ps) = numero p2 == numero p1 + 1 && consecutivos (p2:ps)

-- Punto 4d)
mensaje :: String -> String -> [Paquete] -> String
mensaje orig dest = concat . map datos . queLeEnvio orig dest

-- Punto 4e) 
mensajeSeguro :: Firewall -> String -> String -> [Paquete] -> String
mensajeSeguro firewall orig dest paqs | estaCompleta orig dest paqs = mensaje orig dest (filter (dejaPasar firewall) paqs)
                                    | otherwise = "Mensaje incompleto"
