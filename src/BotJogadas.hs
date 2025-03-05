module BotJogadas where
import System.Random (mkStdGen, randomR, randomRs)
import System.Exit (exitSuccess)
import Data.List (sort)
import Adjacencia (checarAdjacencia, matrizAdjacencia)
import RepresentacaoTerritorios (imprimeMapa, defineCor)
import System.Console.ANSI
import MostrarObjetivos (imprimeObjetivo)
import ShuffleListPura (gerarSeed)
import MapeamentoTerritorios (retornaSigla)
import ChecagemDeObjetivos (checagemVitoria)

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

        -----------------------------------------------------
        -- putStrLn ("Resultado dos dados: " ++ (show jogadaDados))
        putStr "DADOS DE ATAQUE: "
        putStrLn (show (reverse (sort (take qtd jogadaDados))))
        putStr "DADOS DE DEFESA: "
        putStrLn (show (reverse (sort (drop qtd jogadaDados))))
        putStrLn ("O jogador atacante perdeu " ++ show (calculaPerdasAtaque jogadaDados qtd) ++ " exercitos")
        putStrLn ("E o defensor perdeu " ++ show (calculaPerdasDefesa jogadaDados qtd))
        ------------------------------------------------------
        let mapaPosAtaque = acaoDeAtaque mapa (ataque !! 0) (ataque !! 1) qtd jogadaDados -- trazer acaoDeAtaque
        if (temTerritorioConquistado mapaPosAtaque /= -1) then do -- trazer temTerritorioConquistado
            let transfere = pegaElementoAleatorio [1 .. (maxAtaque mapaPosAtaque (ataque !! 0))] seed
            let aux = substituirSublista mapaPosAtaque (temTerritorioConquistado mapaPosAtaque) [indiceJogador, transfere]
            putStrLn ("O jogador " ++ (show indiceJogador) ++ " (BOT) transferiu " ++ (show transfere) ++ " exercitos para o territorio conquistado.")
            botAtaca (substituirSublista aux (ataque !! 0) [indiceJogador, ((aux !! ((ataque !! 0) - 1)) !! 1) - transfere]) indiceJogador jogadoresInfo objetivos
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

filtraAtaques :: [[Int]] -> ([Int] -> Int -> [[Int]] -> Bool) -> [[Int]] -> Int -> [[Int]]
filtraAtaques mapa func [] indiceJogador = []
filtraAtaques mapa func (h:t) indiceJogador =
    if func h indiceJogador mapa
        then h : filtraAtaques mapa func t indiceJogador
    else filtraAtaques mapa func t indiceJogador

funcAtaque::[Int]->Int->[[Int]]->Bool
funcAtaque [terr, alvo] indiceJogador mapa =
    (pertence mapa terr indiceJogador) && (not (pertence mapa alvo indiceJogador))


funcMover::[Int]->Int->[[Int]]->Bool
funcMover [terr, alvo] indiceJogador mapa =
    (pertence mapa terr indiceJogador) && (pertence mapa alvo indiceJogador)

pertence::[[Int]]->Int->Int->Bool
pertence mapa territorio indiceJogador =
    (((mapa !! (territorio - 1)) !! 0) == indiceJogador) && territorio /= -1

pegaVizinhosTerritorio::[[Int]]->Int->[Int] -- retorna uma lista de vizinhos
pegaVizinhosTerritorio adj terr = adj !! (terr - 1)

maxAtaque mapa terr =
    min 3 (((mapa !! (terr - 1)) !! 1) - 1)

gerarJogadasDosDados :: Int -> Int -> [Int]
gerarJogadasDosDados seed n = take n (randomRs (1, 6) (mkStdGen seed))

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

vantagem :: [Int] -> [Int] -> Int
vantagem l1 l2 = length $ filter id $ zipWith (>) l1 l2

vantagem2 :: [Int] -> [Int] -> Int
vantagem2 l1 l2 = length $ filter id $ zipWith (<=) l1 l2

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

