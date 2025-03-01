module Rodada where

-- [numJogadores, numBots], objetivoSorteado, índice do jogador da vez, mapaSorteado, 
rodada::[Int]->[Int]->Int->[[Int]]->IO()
rodada jogadoresInfo objetivos indiceJogador mapa = do
    print mapa

    putStrLn $ "Vez do jogador " ++ (show indiceJogador)
    inputAtaque

    rodada jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapa


inputAtaque::IO()
inputAtaque = do
    print "Voce deseja atacar? (1)Sim (0)Nao"
    resposta <- readLn::IO Int
    if resposta == 1 then do 
        print "chamada de ataque" --aqui vai ser chamada a função de ataque
        inputAtaque
    else
        return ()



-- mapa, indicePaisAtacante, indiceAlvo, qtdExercitos
-- atacar::[[Int]]->Int->Int->Int->[[Int]]



-- ataque::