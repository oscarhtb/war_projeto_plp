module PosicionamentoInicial where

import Utils (substituirSublista)

-- retorna a matriz que representa o mapa com os territórios aleatoriamente divididos
posicionamentoInicial::[Int]->[[Int]]->Int->[[Int]]
posicionamentoInicial slist mapa qtd =
    posicionamentoInicialRec slist mapa qtd 1

posicionamentoInicialRec::[Int]->[[Int]]->Int->Int->[[Int]]
posicionamentoInicialRec [] mapa _ _ = mapa
posicionamentoInicialRec (h:t) mapa qtd i =
    posicionamentoInicialRec t (substituirSublista mapa h [determinePlayer qtd i, 1]) qtd (i+1)


determinePlayer::Int->Int->Int
determinePlayer qtd ind =
    determinePlayerRec qtd ind 1

determinePlayerRec::Int->Int->Int->Int
determinePlayerRec qtd ind nJogador =
    if (div 24 qtd) * nJogador >= ind then nJogador
    else determinePlayerRec qtd ind (nJogador+1)