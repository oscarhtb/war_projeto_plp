module MostrarObjetivos where
import System.Process (callCommand)

-- 1. conq 14 territórios
-- 2. ter pelo menos 2 jogadores em 12 ou mais territórios
-- 3. conq todas as américas e a oceania
-- 4. conq ásia e europa
-- 5. conq europa, oceania e áfrica
-- 6. conq américa e europa

-- qtd é a qunatidade de jogadores NÃO BOTS!
exporObjetivo :: Int -> [Int] -> IO ()
exporObjetivo qtd objs = do
    print "Agora vamos mostrar os objetivos. Se prepare!"
    print "Pressione ENTER para ver os objetivos"
    _ <- getLine
    clearScreen
    exporObjetivoRec qtd 1 objs

exporObjetivoRec :: Int -> Int -> [Int] -> IO ()
exporObjetivoRec qtd a [] = do
    putStrLn "Todos os objetivos foram exibidos."
    putStrLn "Pressione ENTER para continuar"
    _ <- getLine
    clearScreen
exporObjetivoRec qtd indice (h:t)
    | indice > qtd = putStrLn "Todos os objetivos foram exibidos."
    | otherwise = do
        putStrLn $ "Pressione ENTER"
        _ <- getLine
        clearScreen
        putStrLn $ "Pressione ENTER para ver o objetivo do jogador " ++ show indice
        _ <- getLine
        clearScreen
        putStrLn $ "Objetivo do jogador " ++ show indice
        putStrLn $ "Objetivo: " ++ imprimeObjetivo h
        --_ <- getLine
        --clearScreen
        exporObjetivoRec qtd (indice + 1) t

clearScreen :: IO ()
clearScreen = callCommand "clear"  -- "pode ser outro comando :("

imprimeObjetivo::Int->String
imprimeObjetivo indice
    |indice == 1 = quatorzeTerritorios
    |indice == 2 = dozeTerritoriosComDois
    |indice == 3 = americaOceania
    |indice == 4 = asiaEuropa
    |indice == 5 = europaOceaniaAfrica
    |indice == 6 = americaEuropa
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