module Rodada where
import System.Console.ANSI
import System.Random (mkStdGen, randomRs, StdGen)
import Data.List (sort)
import System.Exit (exitSuccess)
import MapeamentoTerritorios (mapeiaTerritorio)
import RepresentacaoTerritorios (imprimeMapa, defineCor)
import ShuffleListPura (gerarSeed, shuffleListPura)
import Movimento (inputMovimento)
import ChecagemDeObjetivos (checagemVitoria)
import MostrarObjetivos (imprimeObjetivo)

rodada::Int->[Int]->[Int]->Int->[[Int]]->IO()
rodada numRodada jogadoresInfo objetivos indiceJogador mapa = do
    
    defineCor indiceJogador
    putStrLn $ "Vez do jogador " ++ (show indiceJogador)
    setSGR [Reset]

    novoMapa <- menuAlocacaoTerritorios mapa indiceJogador 5 jogadoresInfo objetivos
    verificaObjetivos jogadoresInfo objetivos mapa

    if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa
    else do
        mapaPosAtaque <- inputAtaque novoMapa indiceJogador jogadoresInfo objetivos
        mapaPosMover <- inputMovimento mapaPosAtaque indiceJogador []
        rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapaPosMover
    
verificaObjetivos::[Int]->[Int]->[[Int]]->IO()
verificaObjetivos jogInfo objetivos mapa = do
    let vitorioso = checagemVitoria jogInfo objetivos mapa
    if vitorioso /= -1 then do
        defineCor vitorioso
        putStrLn ("O jogador " ++ (show vitorioso) ++ " venceu!")
        putStrLn ("Seu objetivo de " ++ (imprimeObjetivo (objetivos !! (vitorioso - 1))) ++ " foi atingido com sucesso!")
        imprimeMapa mapa
        exitSuccess
    else return ()

inputAtaque::[[Int]]->Int->[Int]->[Int]->IO [[Int]]
inputAtaque mapa indiceJogador jogadoresInfo objetivos = do
    verificaObjetivos jogadoresInfo objetivos mapa
    imprimeMapa mapa
    print "Voce deseja atacar? (1)Sim (0)Nao"
    resposta <- readLn::IO Int
    if resposta == 1 then do
        putStrLn "Qual territorio voce quer usar para atacar?"
        terr <- getLine
        putStrLn "Qual territorio voce deseja invadir?"
        alvo <- getLine
        putStrLn "Com quantos exercitos voce deseja atacar?"
        qtd <- readLn :: IO Int
        randomSeed <- gerarSeed
        let jogadaDados = (gerarJogadasDosDados randomSeed (qtd + (min 3 ((mapa !! ((mapeiaTerritorio alvo) - 1)) !! 1))))
        putStrLn $ "Jogada dos dados: " ++ show jogadaDados
        let mapaPosAtaque = acaoDeAtaque mapa (mapeiaTerritorio terr) (mapeiaTerritorio alvo) qtd jogadaDados
        -- func para checar se o mapa possui alguma posição com zero exércitos, o que indicaria território conquistado
        if (temTerritorioConquistado mapaPosAtaque /= -1) then do
            putStrLn "Você conquistou o território! Quantos exércitos você deseja transferir? (min: 1, max: 3)" -- na verdade o máximo depende da sua qtd de exercitos
            transfere <- readLn :: IO Int
            let aux = substituirSublista mapaPosAtaque (temTerritorioConquistado mapaPosAtaque) [indiceJogador, transfere]
            inputAtaque (substituirSublista aux (mapeiaTerritorio terr) [indiceJogador, ((aux !! ((mapeiaTerritorio terr) - 1)) !! 1) - transfere]) indiceJogador jogadoresInfo objetivos
        else inputAtaque (mapaPosAtaque) indiceJogador jogadoresInfo objetivos
    else
        return (mapa)

acaoDeAtaque::[[Int]]->Int->Int->Int->[Int]->[[Int]]
acaoDeAtaque mapa terr alvo qtd dados =
    batalhaMapa mapa (calculaPerdasAtaque dados qtd) (calculaPerdasDefesa dados qtd) terr alvo

batalhaMapa::[[Int]]->Int->Int->Int->Int->[[Int]]
batalhaMapa mapa perdasAtaque perdasDefesa terr alvo =
    substituirSublista (substituirSublista mapa terr [((mapa !! (terr - 1)) !! 0), ((mapa !! (terr - 1)) !! 1) - perdasAtaque]) alvo [((mapa !! (alvo - 1)) !! 0), ((mapa !! (alvo - 1)) !! 1) - perdasDefesa]


-- [5, 2, 4, 4, 4] [3, 3, 3, 3, 3]
-- [4, 2, 2] [3, 0, 0]
-- [4, 0, 0] [3, 2, 1]
-- [4, 3, 2, 1] [4] [3, 2, 1]
-- [4] [3]

-- dividir a lista de dados baseado no qtd, ordenar elas, passar as duas para o preencheDados e dividir o retorno

-- testado, aparentemente funcionando
calculaPerdasAtaque::[Int]->Int->Int --faz um take pra pegar os atacantes, ordena-os, pega os defensores, ordena-os
calculaPerdasAtaque dados qtdAtaque = 
    let aux = preencheDados (reverse (sort (take qtdAtaque dados))) (reverse (sort (drop qtdAtaque dados)))
    in vantagem2 (take (div (length aux) 2) aux) (drop (div (length aux) 2) aux)

-- preciso testar ainda
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


menuAlocacaoTerritorios::[[Int]]->Int->Int->[Int]->[Int]->IO [[Int]]
menuAlocacaoTerritorios mapa indiceJogador qtdAdicoes jogInfo objetivos = do
    verificaObjetivos jogInfo objetivos mapa
    imprimeMapa mapa
    putStrLn $ "Voce pode alocar " ++ (show qtdAdicoes) ++ " exercitos"
    putStrLn "Digite o território no qual voce deseja adicionar (em forma de sigla): "
    terr <- getLine
    putStrLn "Quantos exercitos você deseja adicionar? "
    qtd <- readLn :: IO Int
    if (qtdAdicoes - qtd) == 0 then return (substituirSublista mapa (mapeiaTerritorio terr) [indiceJogador, ((mapa !! ((mapeiaTerritorio terr) - 1)) !! 1) + qtd])
    else menuAlocacaoTerritorios (substituirSublista mapa (mapeiaTerritorio terr) [indiceJogador, ((mapa !! ((mapeiaTerritorio terr) - 1)) !! 1) + qtd]) indiceJogador (qtdAdicoes - qtd) jogInfo objetivos


substituirSublista :: [[a]] -> Int -> [a] -> [[a]]
substituirSublista listaDeListas pos novaSublista 
    | pos < 0 || pos > length listaDeListas = listaDeListas -- Verifica se a posição é válida 
    | otherwise = let (antes, depois) = splitAt (pos - 1) listaDeListas in antes ++ [novaSublista] ++ tail depois


-- testar depois
preencheDados::[Int]->[Int]->[Int] --vai receber as listas ordenadas e preenchê-las de acordo com a quantidade
-- lista 1 são os atacantes, lista 2 os defensores
preencheDados lista1 lista2 =
    if ((length lista1) == (length lista2)) then (lista1 ++ lista2)
    else if ((length lista1) < (length lista2)) then preencheDados lista1 (init lista2)
    else preencheDados lista1 (lista2 ++ [0])

preencheDados2::[Int]->[Int]->[Int] --vai receber as listas ordenadas e preenchê-las de acordo com a quantidade
-- lista 1 são os atacantes, lista 2 os defensores
preencheDados2 lista1 lista2 =
    if ((length lista1) == (length lista2)) then (lista1 ++ lista2)
    else if ((length lista1) < (length lista2)) then preencheDados2 (lista1 ++ [0]) lista2
    else preencheDados2 (init lista1) lista2

temTerritorioConquistadoRec::[[Int]]->Int->Int
temTerritorioConquistadoRec [] indice = -1
temTerritorioConquistadoRec (head:tail) indice =
    if (head !! 1) == 0 then indice else temTerritorioConquistadoRec tail (indice + 1)

temTerritorioConquistado::[[Int]]->Int
temTerritorioConquistado mapa = temTerritorioConquistadoRec mapa 1

-- mapa, indicePaisAtacante, indiceAlvo, qtdExercitos
-- atacar::[[Int]]->Int->Int->Int->[[Int]]



-- ataque::