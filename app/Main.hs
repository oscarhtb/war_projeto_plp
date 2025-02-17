module Main where
import MenuInicial (menuInicial)
import PosicionamentoInicial (substituirSublista)

main :: IO ()
main = do
    let resultado = substituirSublista [[0, 1], [2, 1], [0, 1], [4, 1]] 1 [10000, 1]
    print resultado  -- Imprime o resultado

