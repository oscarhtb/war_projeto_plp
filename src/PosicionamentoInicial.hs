module PosicionamentoInicial where
import System.Random (newStdGen, randomRIO)
import System.Random.Shuffle (shuffle')


-- TA FUNCIONANDO, vamos usar isso para fazer a divisão.
shuffleList :: [a] -> IO [a]
shuffleList xs = do
    gen <- newStdGen  -- Gera um número aleatório como semente
    return $ shuffle' xs (length xs) gen  -- Embaralha a lista
-- ordenar aleatoriamente uma lista de 1 a 24

--TA FUNCIONANDO
--função para determinar a qual jogador pertence aquele território
determinePlayer::Int->Int->Int
determinePlayer qtd ind =
    determinePlayerRec qtd ind 1

-- 24 / qtd é o n de terr de cada jogador
determinePlayerRec::Int->Int->Int->Int
determinePlayerRec qtd ind nJogador =
    if (div 24 qtd) * nJogador >= ind then nJogador
    else determinePlayerRec qtd ind (nJogador+1)




-- recebendo o mapa, e a posição dele que eu quero mudar,
-- devo inserir o valor que eu tenho na posição,
-- e retornar 

-- meu gpt deu essa função de modificar o mapa
-- updateSublist :: [[a]] -> Int -> [a] -> [[a]]
-- updateSublist xs idx newSublist = [if i == idx then newSublist else x | (i, x) <- zip [0..] xs]

--gpt de gabi mandou isso
--está 1 indexado agora
substituirSublista :: [[a]] -> Int -> [a] -> [[a]]
substituirSublista listaDeListas pos novaSublista 
    | pos < 0 || pos > length listaDeListas = listaDeListas -- Verifica se a posição é válida 
    | otherwise = let (antes, depois) = splitAt (pos - 1) listaDeListas in antes ++ [novaSublista] ++ tail depois


-- TA FUNCIONANDO
--passar por todos os elementos da shuffledList
iterateOverShuffle::[Int]->[[Int]]->Int->[[Int]]
iterateOverShuffle slist mapa qtd =
    iterateOverShuffleRec slist mapa qtd 1

iterateOverShuffleRec::[Int]->[[Int]]->Int->Int->[[Int]]
iterateOverShuffleRec [] mapa qtd i = mapa
iterateOverShuffleRec (h:t) mapa qtd i =
    iterateOverShuffleRec t (substituirSublista mapa h [determinePlayer qtd i, 1]) qtd (i+1)





-- sorteiaTerritorios::[[Int]]->Int->[[Int]]
-- sorteiaTerritorios mapa qtd = 
     



-- atribuiGrupo::Int->[Int]->[Int]
-- atribuiGrupo qtd mapa = do
--     grupo <- randomRIO (1, qtd)
--     return (grupo : qtd)



-- mapa = lista de pares | par - [jogadorDono, númeroExércitos]
-- tenho que receber quantos jogadores tem
-- posicionamentoInicial::qtdJogadores->[[Int]]
-- posicionamentoInicial qtd = 
--     let mapa = replicate 24 [0,1] --estado inicial do mapa, falta checar se está certo
--     -- a divisão de territórios vai ser um sorteio, onde cada jogador vai receber 24/qtd territórios
--     -- divisão será aleatória, e cada jogador vai ter inicialmente 1 exército em cada um
--     -- o retorno da função é o mapa modificado aleatoriamente pela divisão
--     -- o retorno vai ser a chamada da função sorteiaTerritorios
--     in sorteiaTerritorios mapa qtd