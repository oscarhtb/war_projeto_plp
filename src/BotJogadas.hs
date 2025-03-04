module BotJogadas where
import System.Random (mkStdGen, randomR)
import System.Exit (exitSuccess)
import Adjacencia (checarAdjacencia, matrizAdjacencia)
import RepresentacaoTerritorios (imprimeMapa, defineCor)
import System.Console.ANSI
import MostrarObjetivos (imprimeObjetivo)
import ShuffleListPura (gerarSeed)
import MapeamentoTerritorios (retornaSigla)
import ChecagemDeObjetivos (checagemVitoria)

botAtaca mapa indiceJogador jogadoresInfo objetivos = do
    verificaObjetivos jogadoresInfo objetivos mapa
    seed <- gerarSeed

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

territoriosPossuidos::Int->Int->[[Int]]->[Int]
territoriosPossuidos indiceJogador 25 mapa = []
territoriosPossuidos indiceJogador indice mapa =
    if ((mapa !! (indice - 1)) !! 0) == indiceJogador then ((territoriosPossuidos indiceJogador (indice + 1) mapa) ++ [indice])
    else territoriosPossuidos indiceJogador (indice + 1) mapa

possiveisAtacantes::[[Int]]->Int->Int->[Int]
possiveisAtacantes mapa indiceJogador 25 = []
possiveisAtacantes mapa indiceJogador indice =
    if (((mapa !! (indice - 1)) !! 0) == indiceJogador) && (((mapa !! (indice - 1)) !! 1) > 1) then ((possiveisAtacantes mapa indiceJogador (indice + 1)) ++ [indice])
    else possiveisAtacantes mapa indiceJogador (indice + 1)


substituirSublista :: [[a]] -> Int -> [a] -> [[a]]
substituirSublista listaDeListas pos novaSublista 
    | pos < 0 || pos > length listaDeListas = listaDeListas -- Verifica se a posição é válida 
    | otherwise = let (antes, depois) = splitAt (pos - 1) listaDeListas in antes ++ [novaSublista] ++ tail depois

possiveisAtaques::[[Int]]->Int->[[Int]]->[Int]->[[Int]] -- retorna todos os possíveis ataques do bot na forma [[GE, EG], [AR, BR], [NY, MX]]
possiveisAtaques mapa indiceJogador adj [] = []
possiveisAtaques mapa indiceJogador adj (h:t) =
    let viz = pegaVizinhosTerritorio matrizAdjacencia h
    in (aux viz h) ++ possiveisAtaques mapa indiceJogador adj t

aux::[Int]->Int->[[Int]] -- recebe lista de vizinhos de terr e terr, retorna uma matriz com todos os pares entre [terr, viz[i]]
aux [] terr = [] 
aux (h:t) terr = 
    (aux t terr) ++ [[terr, h]]

filtraAtaques:: ([Int] -> Int -> Bool) -> [[Int]] -> Int -> [[Int]]
filtraAtaques func [] indiceJogador = []
filtraAtaques func (h:t) indiceJogador =
    if func h indiceJogador then h : filtraAtaques func t indiceJogador
    else filtraAtaques func t indiceJogador

func::[Int]->Int->Bool
func [terr, alvo] indiceJogador =
    (pertence mapa terr indiceJogador) && (not (pertence mapa alvo indiceJogador))

pertence::[[Int]]->Int->Int->Bool
pertence mapa territorio indiceJogador =
    (((mapa !! (territorio - 1)) !! 0) == indiceJogador) && territorio /= -1


pegaVizinhosTerritorio::[[Int]]->Int->[Int] -- retorna uma lista de vizinhos
pegaVizinhosTerritorio adj terr = adj !! (terr - 1)


