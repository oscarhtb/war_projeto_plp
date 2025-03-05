module MostrarObjetivos where

import System.Process (callCommand)

-- qtd é a quantidade de jogadores NÃO BOTS!
exporObjetivo :: Int -> [Int] -> IO ()
exporObjetivo qtd objs = do
    print "Agora vamos mostrar os objetivos. Se prepare!"
    print "Pressione ENTER para ver os objetivos"
    _ <- getLine
    clearScreen
    exporObjetivoRec qtd 1 objs

exporObjetivoRec :: Int -> Int -> [Int] -> IO ()
exporObjetivoRec _ _ [] = do
    putStrLn "Todos os objetivos foram exibidos."
    putStrLn "Pressione ENTER para continuar"
    _ <- getLine
    clearScreen
exporObjetivoRec qtd indice (h:t)
    | indice > qtd = do
        putStrLn "Pressione ENTER para continuar"
        _ <- getLine
        clearScreen
        putStrLn "Todos os objetivos foram exibidos."
    | otherwise = do
        putStrLn $ "Pressione ENTER"
        _ <- getLine
        clearScreen
        putStrLn $ "Pressione ENTER para ver o objetivo do jogador " ++ show indice
        _ <- getLine
        clearScreen
        putStrLn $ "Objetivo do jogador " ++ show indice
        putStrLn $ "Objetivo: " ++ imprimeObjetivo h
        exporObjetivoRec qtd (indice + 1) t

clearScreen :: IO ()
clearScreen = callCommand "clear" 

imprimeObjetivo::Int->String
imprimeObjetivo indice
    |indice == 1 = quatorzeTerritorios
    |indice == 2 = dozeTerritoriosComDois
    |indice == 3 = americaOceania
    |indice == 4 = asiaEuropa
    |indice == 5 = europaOceaniaAfrica
    |indice == 6 = americaEuropa
    |indice == 7 = eliminarJogadorDois
    |otherwise = "número inválido"

quatorzeTerritorios :: String
quatorzeTerritorios = "Conquistar QUATORZE TERRITÓRIOS à sua escolha."

dozeTerritoriosComDois :: String
dozeTerritoriosComDois = "Conquistar DOZE TERRITÓRIOS à sua escolha e ocupar cada um deles com pelo menos dois exércitos."

americaOceania :: String
americaOceania = "Conquistar na totalidade a AMÉRICA e a OCEANIA."

asiaEuropa :: String
asiaEuropa = "Conquistar na totalidade a ÁSIA e a EUROPA."

europaOceaniaAfrica :: String
europaOceaniaAfrica = "Conquistar na totalidade a EUROPA, a OCEANIA e a ÁFRICA."

americaEuropa :: String
americaEuropa = "Conquistar na totalidade a AMÉRICA e a EUROPA."

eliminarJogadorDois :: String
eliminarJogadorDois = "Seu objetivo é fazer com que o JOGADOR 2 seja ELIMINADO. Caso você seja o jogador dois, seu objetivo passar a ser conquistar QUATORZE TERRITÓRIOS à sua escolha"