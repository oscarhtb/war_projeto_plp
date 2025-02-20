module Main where
import MenuInicial (menuInicial)
import PosicionamentoInicial (substituirSublista, iterateOverShuffle, shuffleList)

main :: IO ()
main = do
    shuffled <- shuffleList [1..24]
    let resultado = iterateOverShuffle shuffled (replicate 24 [0,1]) 4
    print resultado  -- Imprime o resultado

-- main :: IO ()
-- main = do
--     let result = determinePlayer 3 9
--     print result

