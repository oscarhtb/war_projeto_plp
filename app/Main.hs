module Main where

import Data.Time.Clock.POSIX (getPOSIXTime)
import MenuInicial (menuInicial)
import PosicionamentoInicial (substituirSublista, iterateOverShuffle, shuffleList)
import ListaDeObjetivos (listaDeObjetivos)
import ShuffleListPura(shuffleListPura)
import War (war)

main :: IO ()
main = do
    num <- getPOSIXTime
    let seed = floor (num * 1000000)
    print (shuffleListPura seed [1..6])

-- main :: IO ()
-- main = do
--     slist <- shuffleList [1..6]
--     let resultado = listaDeObjetivos 4 slist
--     print resultado  -- Imprime o resultado

-- main :: IO ()
-- main = do
--     let result = determinePlayer 3 9
--     print result

-- main :: IO()
-- main = war
