module Rodada where
import System.Console.ANSI
import System.Random (mkStdGen, randomRs, StdGen)
import Text.Read (readMaybe)
import Data.List (sort)
import System.Exit (exitSuccess)
import MapeamentoTerritorios (mapeiaTerritorio)
import RepresentacaoTerritorios (imprimeMapa, defineCor)
import ShuffleListPura (gerarSeed, shuffleListPura)
import Movimento (inputMovimento)
import ChecagemDeObjetivos (checagemVitoria)
import MostrarObjetivos (imprimeObjetivo)
import Adjacencia (checarAdjacencia, matrizAdjacencia)
import BotJogadas (botAlocacaoTerritorios, botAtaca)
import Salvamento (salvarJogo)

rodada::Int->[Int]->[Int]->Int->[[Int]]->IO()
rodada numRodada jogadoresInfo objetivos indiceJogador mapa = do
    if (contagemDeTerritorios indiceJogador mapa) == 0 then rodada numRodada jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapa

    else if (indiceJogador == (primeiroJogador mapa 1)) && (numRodada /= 1) then do
        putStrLn "Voce deseja salvar e sair do jogo? Sim (1) Nao (0)"
        opcao <- getLine
        case opcao of
            "1" -> do
                putStrLn "Escreva o nome do arquivo no qual voce deseja salvar o jogo (obrigatorio ter .txt no fim, ex: jogoSalvo.txt)"
                nomeArquivo <- getLine
                salvarJogo [mapa, [jogadoresInfo, objetivos], [[numRodada, indiceJogador]]] nomeArquivo
            "0" -> do
                defineCor indiceJogador
                putStrLn $ "Vez do jogador " ++ (show indiceJogador)
                setSGR [Reset]

                if ehBot jogadoresInfo indiceJogador then do
                    novoMapa <- botAlocacaoTerritorios mapa indiceJogador 5 jogadoresInfo objetivos
                    verificaObjetivos jogadoresInfo objetivos mapa

                    if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa
                    else do
                        mapaPosAtaque <- botAtaca novoMapa indiceJogador jogadoresInfo objetivos
                        rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapaPosAtaque
                else do
                    novoMapa <- menuAlocacaoTerritorios mapa indiceJogador 5 jogadoresInfo objetivos
                    verificaObjetivos jogadoresInfo objetivos mapa

                    if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa
                    else do
                        mapaPosAtaque <- inputAtaque novoMapa indiceJogador jogadoresInfo objetivos
                        mapaPosMover <- inputMovimento mapaPosAtaque indiceJogador []
                        rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapaPosMover
    else do
        defineCor indiceJogador
        putStrLn $ "Vez do jogador " ++ (show indiceJogador)
        setSGR [Reset]
        if ehBot jogadoresInfo indiceJogador then do
            novoMapa <- botAlocacaoTerritorios mapa indiceJogador 5 jogadoresInfo objetivos
            verificaObjetivos jogadoresInfo objetivos mapa

            if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa
            else do
                mapaPosAtaque <- botAtaca novoMapa indiceJogador jogadoresInfo objetivos
                rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapaPosAtaque
        else do
            novoMapa <- menuAlocacaoTerritorios mapa indiceJogador 5 jogadoresInfo objetivos
            verificaObjetivos jogadoresInfo objetivos mapa

            if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa
            else do
                mapaPosAtaque <- inputAtaque novoMapa indiceJogador jogadoresInfo objetivos
                mapaPosMover <- inputMovimento mapaPosAtaque indiceJogador []
                rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapaPosMover


primeiroJogador::[[Int]]->Int->Int
primeiroJogador mapa indiceJogador =
    if (contagemDeTerritorios indiceJogador mapa) > 0 then indiceJogador
    else primeiroJogador mapa (indiceJogador + 1)

    
verificaObjetivos::[Int]->[Int]->[[Int]]->IO()
verificaObjetivos jogInfo objetivos mapa = do
    let vitorioso = checagemVitoria jogInfo objetivos mapa
    if vitorioso /= -1 then do
        defineCor vitorioso
        putStrLn ("O jogador " ++ (show vitorioso) ++ " venceu!")
        putStrLn ("Seu objetivo de " ++ (imprimeObjetivo (objetivos !! (vitorioso - 1))) ++ " foi atingido com sucesso!")
        setSGR [Reset]
        imprimeMapa mapa
        exitSuccess
    else return ()

inputAtaque::[[Int]]->Int->[Int]->[Int]->IO [[Int]]
inputAtaque mapa indiceJogador jogadoresInfo objetivos = do
    verificaObjetivos jogadoresInfo objetivos mapa
    imprimeMapa mapa
    print "Voce deseja atacar? (1)Sim (0)Nao"
    inputUsuario <- getLine
    if not (ehInteiro inputUsuario) then do
        putStrLn "Entrada invalida :("
        inputAtaque mapa indiceJogador jogadoresInfo objetivos
    else --é inteiro
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
                    else --é inteiro
                        let qtd = read inputUsuario2 :: Int
                        in if (qtd < 1) || (qtd > (qtdMaxAtaque mapa (mapeiaTerritorio terr))) then do
                            putStrLn "Entrada Invalida :("
                            inputAtaque mapa indiceJogador jogadoresInfo objetivos
                        else do
                            randomSeed <- gerarSeed
                            let jogadaDados = (gerarJogadasDosDados randomSeed (qtd + (min 3 ((mapa !! ((mapeiaTerritorio alvo) - 1)) !! 1))))
                            --putStrLn $ "Jogada dos dados: " ++ show jogadaDados
                            -----------deixando a jogada dos dados mais explícita---------------
                            putStr "DADOS DE ATAQUE: "
                            putStrLn (show (reverse (sort (take qtd jogadaDados))))
                            putStr "DADOS DE DEFESA: "
                            putStrLn (show (reverse (sort (drop qtd jogadaDados))))
                            putStrLn ("O jogador atacante perdeu " ++ show (calculaPerdasAtaque jogadaDados qtd) ++ " exercitos")
                            putStrLn ("E o defensor perdeu " ++ show (calculaPerdasDefesa jogadaDados qtd))

                            -- fim ---------------------------------------------------

                            let mapaPosAtaque = acaoDeAtaque mapa (mapeiaTerritorio terr) (mapeiaTerritorio alvo) qtd jogadaDados
                            
                            if (temTerritorioConquistado mapaPosAtaque /= -1) then do -- fazer maxTransf
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

acaoDeAtaque::[[Int]]->Int->Int->Int->[Int]->[[Int]]
acaoDeAtaque mapa terr alvo qtd dados =
    batalhaMapa mapa (calculaPerdasAtaque dados qtd) (calculaPerdasDefesa dados qtd) terr alvo

batalhaMapa::[[Int]]->Int->Int->Int->Int->[[Int]]
batalhaMapa mapa perdasAtaque perdasDefesa terr alvo =
    substituirSublista (substituirSublista mapa terr [((mapa !! (terr - 1)) !! 0), ((mapa !! (terr - 1)) !! 1) - perdasAtaque]) alvo [((mapa !! (alvo - 1)) !! 0), ((mapa !! (alvo - 1)) !! 1) - perdasDefesa]

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
    if not (pertence mapa (mapeiaTerritorio terr) indiceJogador) then do
        putStrLn "Entrada invalida :("
        menuAlocacaoTerritorios mapa indiceJogador qtdAdicoes jogInfo objetivos
    else do
        putStrLn "Quantos exercitos você deseja adicionar? "
        inputUsuario <- getLine
        if not (ehInteiro inputUsuario) then do
            putStrLn "Entrada invalida :("
            menuAlocacaoTerritorios mapa indiceJogador qtdAdicoes jogInfo objetivos
        else
            let qtd = read inputUsuario :: Int
            in if (qtd > qtdAdicoes) || (qtd < 1) then do
                putStrLn "Entrada invalida :("
                menuAlocacaoTerritorios mapa indiceJogador qtdAdicoes jogInfo objetivos
            else
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

pertence::[[Int]]->Int->Int->Bool
pertence mapa territorio indiceJogador =
    (territorio /= -1) && ((mapa !! (territorio - 1)) !! 0) == indiceJogador

ehInteiro :: String -> Bool
ehInteiro s = case readMaybe s :: Maybe Int of
                Just _  -> True
                Nothing -> False

qtdMaxAtaque::[[Int]]->Int->Int
qtdMaxAtaque mapa terr = min 3 (((mapa !! (terr - 1)) !! 1) - 1)

maxTransf::[[Int]]->Int->Int
maxTransf mapa terr =
    min 3 (((mapa !! (terr - 1)) !! 1) - 1)

contagemDeTerritorios::Int->[[Int]]->Int
contagemDeTerritorios indiceJogador [] = 0
contagemDeTerritorios indiceJogador (h:t) =
    if (h !! 0) == indiceJogador then (1 + (contagemDeTerritorios indiceJogador t))
    else contagemDeTerritorios indiceJogador t

ehBot::[Int]->Int->Bool
ehBot jogadoresInfo indiceJogador = indiceJogador > (jogadoresInfo !! 0)