module BotJogadas where
import System.Random (mkStdGen, randomR)
import System.Exit (exitSuccess)
import Data.List (sort)
import Adjacencia (matrizAdjacencia)
import Ataque (acaoDeAtaque, calculaPerdasAtaque, calculaPerdasDefesa, gerarJogadasDosDados)
import RepresentacaoTerritorios (imprimeMapa, defineCor)
import System.Console.ANSI
import Utils (substituirSublista)
import MostrarObjetivos (imprimeObjetivo)
import RandomUtils (gerarSeed)
import MapeamentoTerritorios (retornaSigla)
import ChecagemDeObjetivos (checagemVitoria)

botAtaca::[[Int]]->Int->[Int]->[Int]->IO [[Int]]
botAtaca mapa indiceJogador jogadoresInfo objetivos = do
    imprimeMapa mapa
    verificaObjetivos jogadoresInfo objetivos mapa
    seed <- gerarSeed
    let possibilidadesDeAtaque = (filtraAtaques mapa funcAtaque (possiveisAtaques mapa indiceJogador matrizAdjacencia (possiveisAtacantes mapa indiceJogador 1)) indiceJogador) --dá todas as possibilidades de ataque em forma de pares [território, alvo]
    if (null possibilidadesDeAtaque || (pegaElementoAleatorio [1,0] seed) == 0) then do
        putStrLn ("O jogador " ++ (show indiceJogador) ++ " (BOT) não quer atacar. Passou a vez")
        return mapa
    else do
        let ataque = pegaElementoAleatorioMatriz possibilidadesDeAtaque seed
        let qtd = pegaElementoAleatorio [1 .. (maxAtaque mapa (ataque !! 0))] seed
        putStrLn ("O jogador " ++ (show indiceJogador) ++ " (BOT) usou o territorio " ++ (retornaSigla (ataque !! 0)) ++ " para atacar o territorio " ++ (retornaSigla (ataque !! 1)) ++ " com " ++ (show qtd) ++ " exercitos.")
        let jogadaDados = (gerarJogadasDosDados seed (qtd + (min 3 ((mapa !! ((ataque !! 1) - 1)) !! 1))))

        putStr "DADOS DE ATAQUE: "
        putStrLn (show (reverse (sort (take qtd jogadaDados))))
        putStr "DADOS DE DEFESA: "
        putStrLn (show (reverse (sort (drop qtd jogadaDados))))
        putStrLn ("O jogador atacante perdeu " ++ show (calculaPerdasAtaque jogadaDados qtd) ++ " exercitos")
        putStrLn ("E o defensor perdeu " ++ show (calculaPerdasDefesa jogadaDados qtd))
        
        let mapaPosAtaque = acaoDeAtaque mapa (ataque !! 0) (ataque !! 1) qtd jogadaDados -- trazer acaoDeAtaque
        if (temTerritorioConquistado mapaPosAtaque /= -1) then do -- trazer temTerritorioConquistado
            let transfere = pegaElementoAleatorio [1 .. (maxAtaque mapaPosAtaque (ataque !! 0))] seed
            let auxMapa = substituirSublista mapaPosAtaque (temTerritorioConquistado mapaPosAtaque) [indiceJogador, transfere]
            putStrLn ("O jogador " ++ (show indiceJogador) ++ " (BOT) transferiu " ++ (show transfere) ++ " exercitos para o territorio conquistado.")
            botAtaca (substituirSublista auxMapa (ataque !! 0) [indiceJogador, ((auxMapa !! ((ataque !! 0) - 1)) !! 1) - transfere]) indiceJogador jogadoresInfo objetivos
        else botAtaca (mapaPosAtaque) indiceJogador jogadoresInfo objetivos

botAlocacaoTerritorios::[[Int]]->Int->Int->[Int]->[Int]->IO [[Int]]
botAlocacaoTerritorios mapa indiceJogador qtdAdicoes jogadoresInfo objetivos = do
    verificaObjetivos jogadoresInfo objetivos mapa
    seed <- gerarSeed
    let territorioEscolhido = pegaElementoAleatorio (territoriosPossuidos indiceJogador 1 mapa) seed
    let qtd = pegaElementoAleatorio [1..qtdAdicoes] seed
    putStrLn ("O jogador " ++ (show indiceJogador) ++ " adicionou " ++ (show qtd) ++ " exercitos em " ++ (retornaSigla territorioEscolhido))

    let novoMapa = substituirSublista mapa territorioEscolhido [indiceJogador, ((mapa !! (territorioEscolhido - 1)) !! 1) + qtd]
    
    if (qtdAdicoes - qtd) == 0 then return novoMapa
    else botAlocacaoTerritorios novoMapa indiceJogador (qtdAdicoes - qtd) jogadoresInfo objetivos


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

pegaElementoAleatorio::[Int]->Int->Int
pegaElementoAleatorio lista seed = 
    let gen = mkStdGen seed
        (indice, _) = randomR (0, length lista - 1) gen
    in lista !! indice

pegaElementoAleatorioMatriz::[[Int]]->Int->[Int]
pegaElementoAleatorioMatriz lista seed = 
    let gen = mkStdGen seed
        (indice, _) = randomR (0, length lista - 1) gen
    in lista !! indice

territoriosPossuidos::Int->Int->[[Int]]->[Int]
territoriosPossuidos _ 25 _ = []
territoriosPossuidos indiceJogador indice mapa =
    if ((mapa !! (indice - 1)) !! 0) == indiceJogador then ((territoriosPossuidos indiceJogador (indice + 1) mapa) ++ [indice])
    else territoriosPossuidos indiceJogador (indice + 1) mapa

possiveisAtacantes::[[Int]]->Int->Int->[Int]
possiveisAtacantes _ _ 25 = []
possiveisAtacantes mapa indiceJogador indice =
    if (((mapa !! (indice - 1)) !! 0) == indiceJogador) && (((mapa !! (indice - 1)) !! 1) > 1) then ((possiveisAtacantes mapa indiceJogador (indice + 1)) ++ [indice])
    else possiveisAtacantes mapa indiceJogador (indice + 1)

possiveisAtaques::[[Int]]->Int->[[Int]]->[Int]->[[Int]]
possiveisAtaques _ _ _ [] = []
possiveisAtaques mapa indiceJogador adj (h:t) =
    let viz = pegaVizinhosTerritorio matrizAdjacencia h
    in (aux viz h) ++ possiveisAtaques mapa indiceJogador adj t

aux::[Int]->Int->[[Int]]
aux [] _ = []
aux (h:t) terr = 
    (aux t terr) ++ [[terr, h]]

filtraAtaques :: [[Int]] -> ([Int] -> Int -> [[Int]] -> Bool) -> [[Int]] -> Int -> [[Int]]
filtraAtaques _ _ [] _ = []
filtraAtaques mapa func (h:t) indiceJogador =
    if func h indiceJogador mapa
        then h : filtraAtaques mapa func t indiceJogador
    else filtraAtaques mapa func t indiceJogador

funcAtaque::[Int]->Int->[[Int]]->Bool
funcAtaque [terr, alvo] indiceJogador mapa =
    (pertence mapa terr indiceJogador) && (not (pertence mapa alvo indiceJogador))
funcAtaque _ _ _ = False

pertence::[[Int]]->Int->Int->Bool
pertence mapa territorio indiceJogador =
    (((mapa !! (territorio - 1)) !! 0) == indiceJogador) && territorio /= -1

pegaVizinhosTerritorio::[[Int]]->Int->[Int]
pegaVizinhosTerritorio adj terr = adj !! (terr - 1)

maxAtaque::[[Int]]->Int->Int
maxAtaque mapa terr =
    min 3 (((mapa !! (terr - 1)) !! 1) - 1)

temTerritorioConquistadoRec::[[Int]]->Int->Int
temTerritorioConquistadoRec [] _ = -1
temTerritorioConquistadoRec (hd:tl) indice =
    if (hd !! 1) == 0 then indice else temTerritorioConquistadoRec tl (indice + 1)

temTerritorioConquistado::[[Int]]->Int
temTerritorioConquistado mapa = temTerritorioConquistadoRec mapa 1
