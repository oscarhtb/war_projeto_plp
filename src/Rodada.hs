module Rodada where
import System.Console.ANSI
import System.Random (mkStdGen, randomRs, StdGen)
import MapeamentoTerritorios (mapeiaTerritorio)
import RepresentacaoTerritorios (imprimeMapa, defineCor)
import ShuffleListPura (gerarSeed, shuffleListPura)

-- [numJogadores, numBots], objetivoSorteado, índice do jogador da vez, mapaSorteado, 
rodada::Int->[Int]->[Int]->Int->[[Int]]->IO()
rodada numRodada jogadoresInfo objetivos indiceJogador mapa = do
    imprimeMapa mapa

    defineCor indiceJogador
    putStrLn $ "Vez do jogador " ++ (show indiceJogador)
    setSGR [Reset]

    -- alocacao
    novoMapa <- menuAlocacaoTerritorios mapa indiceJogador 5

    if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa
    else do
        inputAtaque
        rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa
    


inputAtaque::[[Int]]->[[Int]]->IO [[Int]]
inputAtaque mapa = do
    print "Voce deseja atacar? (1)Sim (0)Nao"
    resposta <- readLn::IO Int
    if resposta == 1 then do
        putStrLn "Qual territorio voce quer usar para atacar?"
        terr <- getLine
        putStrLn "Qual territorio voce deseja invadir?"
        alvo <- getLine
        putrStrLn "Com quantos exercitos voce deseja atacar"
        qtd <- readLn :: IO Int
        randomSeed <- gerarSeed
        -- calcular ação de ataque
        
        inputAtaque (acaoDeAtaque mapa terr alvo qtd (gerarJogadasDosDados randomSeed (qtd + (min 3 ((mapa !! (alvo - 1)) !! 1)))))
    else
        return (mapa)

acaoDeAtaque::[[Int]]->Int->Int->Int->[Int]->[[Int]]
acaoDeAtaque mapa terr alvo qtd dados =
    batalhaMapa

batalhaMapa::[[Int]]->Int->Int->[[Int]]
batalhaMapa mapa perdasAtaque perdasDefesa =

-- [5, 2, 4, 4, 4] [3, 3, 3, 3, 3]
-- [4, 2, 2] [3, 0, 0]
-- [4, 0, 0] [3, 2, 1]
-- [4, 3, 2, 1] [4] [3, 2, 1]
-- [4] [3]

-- dividir a lista de dados baseado no qtd, ordenar elas, passar as duas para o preencheDados e dividir o retorno

calculaPerdasAtaque::[Int]->Int->Int --faz um take pra pegar os atacantes, ordena-os, pega os defensores, ordena-os
calculaPerdasAtaque dados qtdAtaque = 
    vantagem () --l1 são os atacantes


gerarJogadasDosDados :: Int -> Int -> [Int]
gerarJogadasDosDados seed n = take n (randomRs (1, 6) (mkStdGen seed))

vantagem :: [Int] -> [Int] -> Int
vantagem l1 l2 = length $ filter id $ zipWith (>) l1 l2

vantagem2 :: [Int] -> [Int] -> Int
vantagem2 l1 l2 = length $ filter id $ zipWith (<=) l1 l2


menuAlocacaoTerritorios::[[Int]]->Int->Int->IO [[Int]]
menuAlocacaoTerritorios mapa indiceJogador qtdAdicoes = do
    putStrLn $ "Voce pode alocar " ++ (show qtdAdicoes) ++ " exercitos"
    putStrLn "Digite o território no qual voce deseja adicionar (em forma de sigla): "
    terr <- getLine
    putStrLn "Quantos exercitos você deseja adicionar? "
    qtd <- readLn :: IO Int
    if (qtdAdicoes - qtd) == 0 then return (substituirSublista mapa (mapeiaTerritorio terr) [indiceJogador, ((mapa !! (mapeiaTerritorio terr)) !! 1) + qtd])
    else menuAlocacaoTerritorios (substituirSublista mapa (mapeiaTerritorio terr) [indiceJogador, ((mapa !! (mapeiaTerritorio terr)) !! 1) + qtd]) indiceJogador (qtdAdicoes - qtd)


substituirSublista :: [[a]] -> Int -> [a] -> [[a]]
substituirSublista listaDeListas pos novaSublista 
    | pos < 0 || pos > length listaDeListas = listaDeListas -- Verifica se a posição é válida 
    | otherwise = let (antes, depois) = splitAt (pos - 1) listaDeListas in antes ++ [novaSublista] ++ tail depois


-- testar depois
preencheDados::[Int]->[Int]->[Int] --vai receber as listas ordenadas e preenchê-las de acordo com a quantidade
-- lista 1 são os atacantes, lista 2 os defensores
preencheDados lista1 lista2 =
    if ((length lista1) == (length lista2)) then (lista1 ++ lista2)
    else if ((length lista1) < (length lista2)) then preencheDados (lista1 ++ [0]) lista2
    else preencheDados lista1 (lista2 ++ [0])

-- mapa, indicePaisAtacante, indiceAlvo, qtdExercitos
-- atacar::[[Int]]->Int->Int->Int->[[Int]]



-- ataque::