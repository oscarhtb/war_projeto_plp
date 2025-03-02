module Rodada where
import System.Console.ANSI
import MapeamentoTerritorios (mapeiaTerritorio)
import RepresentacaoTerritorios (imprimeMapa, defineCor)

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
    


inputAtaque::IO()
inputAtaque = do
    print "Voce deseja atacar? (1)Sim (0)Nao"
    resposta <- readLn::IO Int
    if resposta == 1 then do 
        print "chamada de ataque" --aqui vai ser chamada a função de ataque
        inputAtaque
    else
        return ()

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

-- mapa, indicePaisAtacante, indiceAlvo, qtdExercitos
-- atacar::[[Int]]->Int->Int->Int->[[Int]]



-- ataque::