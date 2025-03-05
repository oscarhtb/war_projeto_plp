module Movimento where

import MapeamentoTerritorios (mapeiaTerritorio)
import Adjacencia (checarAdjacencia, matrizAdjacencia)
import Utils (substituirSublista, ehInteiro)
import RepresentacaoTerritorios (imprimeMapa)


inputMovimento::[[Int]]->Int->[Int]->IO [[Int]]
inputMovimento mapa indiceJogador movimentosFeitos = do
    imprimeMapa mapa
    putStrLn "Você deseja mover algum exercito? Sim(1) Nao(0)"
    inputUsuario <- getLine
    if not (ehInteiro inputUsuario) then do
        putStrLn "Entrada inválida :("
        inputMovimento mapa indiceJogador movimentosFeitos
    else
        let resposta = read inputUsuario :: Int

        in if (resposta /= 1) && (resposta /= 0) then do
            putStrLn "Entrada inválida :("
            inputMovimento mapa indiceJogador movimentosFeitos
        else if resposta == 1 then do
            putStrLn "De qual territorio voce deseja transferir os exercitos?"
            res <- getLine
            if ((mapeiaTerritorio res) == -1) || not (pertence mapa (mapeiaTerritorio res) indiceJogador) || movimentoJaRealizado movimentosFeitos (mapeiaTerritorio res) then do
                putStrLn "Entrada inválida :("
                inputMovimento mapa indiceJogador movimentosFeitos
            else do
                putStrLn "E para qual territorio desejas transferir?"
                res2 <- getLine
                if ((mapeiaTerritorio res2) == -1) || not (pertence mapa (mapeiaTerritorio res2) indiceJogador) || not (checarAdjacencia (mapeiaTerritorio res) (mapeiaTerritorio res2) matrizAdjacencia) then do
                    putStrLn "Entrada inválida :("
                    inputMovimento mapa indiceJogador movimentosFeitos
                else do
                    putStrLn "E quantos exercitos serão transferidos? (min: 1, max: 3)"
                    qtd <- readLn :: IO Int
                    if (qtd < 1) || (qtd > 3) || not (possuiExercitosSuficientes mapa (mapeiaTerritorio res) qtd) then do
                        putStrLn "Entrada inválida :("
                        inputMovimento mapa indiceJogador movimentosFeitos
                    else
                        inputMovimento (realizaMovimento mapa (mapeiaTerritorio res) (mapeiaTerritorio res2) qtd) indiceJogador (movimentosFeitos ++ [(mapeiaTerritorio res)])
        else return (mapa)

possuiExercitosSuficientes::[[Int]]->Int->Int->Bool
possuiExercitosSuficientes mapa territorio qtd =
    ((mapa !! (territorio - 1)) !! 1) > qtd

pertence::[[Int]]->Int->Int->Bool

pertence mapa territorio indiceJogador =
    (((mapa !! (territorio - 1)) !! 0) == indiceJogador) && territorio /= -1

movimentoJaRealizado::[Int]->Int->Bool
movimentoJaRealizado movimentos territorio =
    elem territorio movimentos

realizaMovimento::[[Int]]->Int->Int->Int->[[Int]]
realizaMovimento mapa origem destino qtd =
    substituirSublista (substituirSublista mapa destino [(mapa !! (destino - 1)) !! 0, ((mapa !! (destino - 1)) !! 1) + qtd]) origem [(mapa !! (origem - 1)) !! 0, ((mapa !! (origem - 1)) !! 1) - qtd]
