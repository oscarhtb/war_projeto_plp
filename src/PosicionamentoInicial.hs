-- Funções para construir o mapa com as posições iniciais sorteadas

module PosicionamentoInicial where
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')



-- -- TA FUNCIONANDO, vamos usar isso para fazer a divisão.
-- shuffleList :: [Int] -> IO [Int]
-- shuffleList xs = do
--     gen <- newStdGen  -- Gera um número aleatório como semente
--     return $ shuffle' xs (length xs) gen  -- Embaralha a lista
-- -- ordenar aleatoriamente uma lista de 1 a 24


--função para determinar a qual jogador pertence aquele território
-- chamada no substituir sublista
determinePlayer::Int->Int->Int
determinePlayer qtd ind =
    determinePlayerRec qtd ind 1

-- chamada recursiva
determinePlayerRec::Int->Int->Int->Int
determinePlayerRec qtd ind nJogador =
    if (div 24 qtd) * nJogador >= ind then nJogador
    else determinePlayerRec qtd ind (nJogador+1)


-- função que modifica uma linha da matriz de mapa
substituirSublista :: [[a]] -> Int -> [a] -> [[a]]
substituirSublista listaDeListas pos novaSublista 
    | pos < 0 || pos > length listaDeListas = listaDeListas -- Verifica se a posição é válida 
    | otherwise = let (antes, depois) = splitAt (pos - 1) listaDeListas in antes ++ [novaSublista] ++ tail depois


-- função principal do arquivo.
-- retorna a matriz que representa o mapa com os territórios aleatoriamente divididos
iterateOverShuffle::[Int]->[[Int]]->Int->[[Int]]
iterateOverShuffle slist mapa qtd =
    iterateOverShuffleRec slist mapa qtd 1

-- chamada recursiva
iterateOverShuffleRec::[Int]->[[Int]]->Int->Int->[[Int]]
iterateOverShuffleRec [] mapa qtd i = mapa
iterateOverShuffleRec (h:t) mapa qtd i =
    iterateOverShuffleRec t (substituirSublista mapa h [determinePlayer qtd i, 1]) qtd (i+1)