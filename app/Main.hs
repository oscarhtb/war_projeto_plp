module Main where
import MenuInicial (menuInicial)
import PosicionamentoInicial (substituirSublista, iterateOverShuffle, shuffleList)
import ListaDeObjetivos (listaDeObjetivos)
import War (war)

-- main :: IO ()
-- main = do
--     slist <- shuffleList [1..6]
--     let resultado = listaDeObjetivos 4 slist
--     print resultado  -- Imprime o resultado

-- main :: IO ()
-- main = do
--     let result = determinePlayer 3 9
--     print result

main :: IO()
main = war
