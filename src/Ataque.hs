module Ataque where

import System.Random (mkStdGen, randomRs)
import Data.List (sort)

import RandomUtils (gerarSeed)
import Adjacencia (checarAdjacencia, matrizAdjacencia)
import MapeamentoTerritorios (mapeiaTerritorio)
import Utils (pertence, verificaObjetivos)
import RepresentacaoTerritorios (imprimeMapa)

import Utils (substituirSublista, ehInteiro)

acaoDeAtaque::[[Int]]->Int->Int->Int->[Int]->[[Int]]
acaoDeAtaque mapa terr alvo qtd dados =
    batalhaMapa mapa (calculaPerdasAtaque dados qtd) (calculaPerdasDefesa dados qtd) terr alvo

batalhaMapa::[[Int]]->Int->Int->Int->Int->[[Int]]
batalhaMapa mapa perdasAtaque perdasDefesa terr alvo =
    substituirSublista (substituirSublista mapa terr [((mapa !! (terr - 1)) !! 0), ((mapa !! (terr - 1)) !! 1) - perdasAtaque]) alvo [((mapa !! (alvo - 1)) !! 0), ((mapa !! (alvo - 1)) !! 1) - perdasDefesa]

calculaPerdasAtaque::[Int]->Int->Int
calculaPerdasAtaque dados qtdAtaque = 
    let aux = preencheDados (reverse (sort (take qtdAtaque dados))) (reverse (sort (drop qtdAtaque dados)))
    in vantagem2 (take (div (length aux) 2) aux) (drop (div (length aux) 2) aux)

calculaPerdasDefesa::[Int]->Int->Int
calculaPerdasDefesa dados qtdAtaque =
    let aux = preencheDados2 (reverse (sort (take qtdAtaque dados))) (reverse (sort (drop qtdAtaque dados)))
    in vantagem (take (div (length aux) 2) aux) (drop (div (length aux) 2) aux)


gerarJogadasDosDados :: Int -> Int -> [Int]
gerarJogadasDosDados seed n = take n (randomRs (1, 6) (mkStdGen seed))

vantagem :: [Int] -> [Int] -> Int
vantagem l1 l2 = length $ filter id $ zipWith (>) l1 l2

vantagem2 :: [Int] -> [Int] -> Int
vantagem2 l1 l2 = length $ filter id $ zipWith (<=) l1 l2

preencheDados::[Int]->[Int]->[Int]
preencheDados lista1 lista2 =
    if ((length lista1) == (length lista2)) then (lista1 ++ lista2)
    else if ((length lista1) < (length lista2)) then preencheDados lista1 (init lista2)
    else preencheDados lista1 (lista2 ++ [0])

preencheDados2::[Int]->[Int]->[Int]
preencheDados2 lista1 lista2 =
    if ((length lista1) == (length lista2)) then (lista1 ++ lista2)
    else if ((length lista1) < (length lista2)) then preencheDados2 (lista1 ++ [0]) lista2
    else preencheDados2 (init lista1) lista2

qtdMaxAtaque::[[Int]]->Int->Int
qtdMaxAtaque mapa terr = min 3 (((mapa !! (terr - 1)) !! 1) - 1)

maxTransf::[[Int]]->Int->Int
maxTransf mapa terr =
    min 3 (((mapa !! (terr - 1)) !! 1) - 1)

temTerritorioConquistado::[[Int]]->Int
temTerritorioConquistado mapa = temTerritorioConquistadoRec mapa 1

temTerritorioConquistadoRec::[[Int]]->Int->Int
temTerritorioConquistadoRec [] _ = -1
temTerritorioConquistadoRec (hd:tl) indice =
    if (hd !! 1) == 0 then indice else temTerritorioConquistadoRec tl (indice + 1)

inputAtaque::[[Int]]->Int->[Int]->[Int]->IO [[Int]]
inputAtaque mapa indiceJogador jogadoresInfo objetivos = do
    verificaObjetivos jogadoresInfo objetivos mapa
    imprimeMapa mapa
    print "Voce deseja atacar? (1)Sim (0)Nao"
    inputUsuario <- getLine
    if not (ehInteiro inputUsuario) then do
        putStrLn "Entrada invalida :("
        inputAtaque mapa indiceJogador jogadoresInfo objetivos
    else
        let resposta = read inputUsuario :: Int
        in if resposta == 1 then do
            putStrLn "Qual territorio voce quer usar para atacar?"
            terr <- getLine
            if (not (pertence mapa (mapeiaTerritorio terr) indiceJogador)) || (((mapa !! ((mapeiaTerritorio terr) - 1)) !! 1) < 2) then do
                putStrLn "Entrada invalida :("
                inputAtaque mapa indiceJogador jogadoresInfo objetivos
            else do
                putStrLn "Qual territorio voce deseja invadir?"
                alvo <- getLine
                if ((mapeiaTerritorio alvo) == -1) || (pertence mapa (mapeiaTerritorio alvo) indiceJogador) || (not (checarAdjacencia (mapeiaTerritorio terr) (mapeiaTerritorio alvo) matrizAdjacencia)) then do
                    putStrLn "Entrada invalida :("
                    inputAtaque mapa indiceJogador jogadoresInfo objetivos
                else do
                    putStrLn "Com quantos exercitos voce deseja atacar?"
                    inputUsuario2 <- getLine
                    if not (ehInteiro inputUsuario2) then do
                        putStrLn "Entrada Invalida :("
                        inputAtaque mapa indiceJogador jogadoresInfo objetivos
                    else
                        let qtd = read inputUsuario2 :: Int
                        in if (qtd < 1) || (qtd > (qtdMaxAtaque mapa (mapeiaTerritorio terr))) then do
                            putStrLn "Entrada Invalida :("
                            inputAtaque mapa indiceJogador jogadoresInfo objetivos
                        else do
                            randomSeed <- gerarSeed
                            let jogadaDados = (gerarJogadasDosDados randomSeed (qtd + (min 3 ((mapa !! ((mapeiaTerritorio alvo) - 1)) !! 1))))
                            putStr "DADOS DE ATAQUE: "
                            putStrLn (show (reverse (sort (take qtd jogadaDados))))
                            putStr "DADOS DE DEFESA: "
                            putStrLn (show (reverse (sort (drop qtd jogadaDados))))
                            putStrLn ("O jogador atacante perdeu " ++ show (calculaPerdasAtaque jogadaDados qtd) ++ " exercitos")
                            putStrLn ("E o defensor perdeu " ++ show (calculaPerdasDefesa jogadaDados qtd))

                            let mapaPosAtaque = acaoDeAtaque mapa (mapeiaTerritorio terr) (mapeiaTerritorio alvo) qtd jogadaDados
                            
                            if (temTerritorioConquistado mapaPosAtaque /= -1) then do
                                putStrLn ("Você conquistou o território! Quantos exércitos você deseja transferir? (min: 1, max: " ++ (show (maxTransf mapaPosAtaque (mapeiaTerritorio terr))) ++ ")")
                                inputUsuario3 <- getLine
                                if not (ehInteiro inputUsuario3) then do
                                    putStrLn "Entrada invalida :("
                                    inputAtaque mapa indiceJogador jogadoresInfo objetivos
                                else do
                                    let transfere = read inputUsuario3 :: Int
                                    let aux = substituirSublista mapaPosAtaque (temTerritorioConquistado mapaPosAtaque) [indiceJogador, transfere]
                                    inputAtaque (substituirSublista aux (mapeiaTerritorio terr) [indiceJogador, ((aux !! ((mapeiaTerritorio terr) - 1)) !! 1) - transfere]) indiceJogador jogadoresInfo objetivos
                            else inputAtaque (mapaPosAtaque) indiceJogador jogadoresInfo objetivos
        else
            return (mapa)
