module ChecagemDeObjetivos where

-- checa os objetivos de todos os jogadores, retorna o índice do vencedor ou -1
checagemVitoria::[Int]->[Int]->[[Int]]->Int
checagemVitoria jogadoresInfo listaDeObjetivos mapa =
    checagemVitoriaRec (transformacaoDeLista jogadoresInfo) listaDeObjetivos mapa

checagemVitoriaRec::[Int]->[Int]->[[Int]]->Int
checagemVitoriaRec [] obj mapa = -1
checagemVitoriaRec (h:t) (head:tail) mapa =
    if checaObjetivo head h mapa then h
    else checagemVitoriaRec t tail mapa



checaObjetivo numObjetivo indiceJogador mapa =
    (funcoesDeObjetivo !! (numObjetivo - 1)) indiceJogador mapa

-- transforma jogadores info em uma lista que representa os indices dos jogadores
transformacaoDeLista::[Int]->[Int]
transformacaoDeLista jogadoresInfo = [1 .. (sum jogadoresInfo)]

checa14territorios::Int->[[Int]]->Bool
checa14territorios indiceJogador mapa = 
    contagemDeTerritorios indiceJogador mapa >= 14

checa12territoriosComDois::Int->[[Int]]->Bool
checa12territoriosComDois indiceJogador mapa =
    contagemDeTerritoriosComMaisDeUmExercito indiceJogador mapa >= 12

checaAmericaOceania::Int->[[Int]]->Bool
checaAmericaOceania indiceJogador mapa =
    (checaAmerica indiceJogador mapa) && (checaOceania indiceJogador mapa)

checaAsiaEuropa::Int->[[Int]]->Bool
checaAsiaEuropa indiceJogador mapa =
    (checaAsia indiceJogador mapa) && (checaEuropa indiceJogador mapa)

checaEuropaOceaniaAfrica::Int->[[Int]]->Bool
checaEuropaOceaniaAfrica indiceJogador mapa =
    (checaEuropa indiceJogador mapa) && (checaOceania indiceJogador mapa) && (checaAfrica indiceJogador mapa)

checaAmericaEuropa::Int->[[Int]]->Bool
checaAmericaEuropa indiceJogador mapa =
    (checaAmerica indiceJogador mapa) && (checaEuropa indiceJogador mapa)


funcoesDeObjetivo::[Int->[[Int]]->Bool]
funcoesDeObjetivo = 
    [
        checa14territorios,
        checa12territoriosComDois,
        checaAmericaOceania,
        checaAsiaEuropa,
        checaEuropaOceaniaAfrica,
        checaAmericaEuropa
    ]


contagemDeTerritorios::Int->[[Int]]->Int
contagemDeTerritorios indiceJogador [] = 0
contagemDeTerritorios indiceJogador (h:t) =
    if (h !! 0) == indiceJogador then (1 + (contagemDeTerritorios indiceJogador t))
    else contagemDeTerritorios indiceJogador t

contagemDeTerritoriosComMaisDeUmExercito::Int->[[Int]]->Int
contagemDeTerritoriosComMaisDeUmExercito indiceJogador [] = 0
contagemDeTerritoriosComMaisDeUmExercito indiceJogador (h:t) =
    if (h !! 0) == indiceJogador && ((h !! 1) > 1) then (1 + (contagemDeTerritoriosComMaisDeUmExercito indiceJogador t))
    else contagemDeTerritoriosComMaisDeUmExercito indiceJogador t


checaAmerica::Int->[[Int]]->Bool
checaAmerica indiceJogador mapa =
    ((mapa !! 0) !! 0) == indiceJogador && ((mapa !! 1) !! 0) == indiceJogador && ((mapa !! 2) !! 0) == indiceJogador && ((mapa !! 3) !! 0) == indiceJogador && ((mapa !! 4) !! 0) == indiceJogador && ((mapa !! 5) !! 0) == indiceJogador && ((mapa !! 6) !! 0) == indiceJogador

checaOceania::Int->[[Int]]->Bool
checaOceania indiceJogador mapa =
    ((mapa !! 23) !! 0) == indiceJogador && ((mapa !! 22) !! 0) == indiceJogador

checaAsia::Int->[[Int]]->Bool
checaAsia indiceJogador mapa =
    ((mapa !! 10) !! 0) == indiceJogador && ((mapa !! 16) !! 0) == indiceJogador && ((mapa !! 17) !! 0) == indiceJogador && ((mapa !! 18) !! 0) == indiceJogador && ((mapa !! 19) !! 0) == indiceJogador && ((mapa !! 20) !! 0) == indiceJogador && ((mapa !! 21) !! 0) == indiceJogador && ((mapa !! 15) !! 0) == indiceJogador

checaEuropa::Int->[[Int]]->Bool
checaEuropa indiceJogador mapa =
    ((mapa !! 7) !! 0) == indiceJogador && ((mapa !! 8) !! 0) == indiceJogador && ((mapa !! 9) !! 0) == indiceJogador

checaAfrica::Int->[[Int]]->Bool
checaAfrica indiceJogador mapa =
    (((mapa !! 11) !! 0) == indiceJogador) && (((mapa !! 12) !! 0) == indiceJogador) && (((mapa !! 13) !! 0) == indiceJogador) && (((mapa !! 14) !! 0) == indiceJogador)
