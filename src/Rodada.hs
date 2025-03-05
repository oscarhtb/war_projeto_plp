module Rodada where
import System.Console.ANSI
import Utils (substituirSublista, ehInteiro, verificaObjetivos, pertence)
import Ataque (inputAtaque)
import MapeamentoTerritorios (mapeiaTerritorio)
import RepresentacaoTerritorios (imprimeMapa, defineCor)
import Movimento (inputMovimento)
import BotJogadas (botAlocacaoTerritorios, botAtaca)
import Salvamento (salvarJogo)

rodada::Int->[Int]->[Int]->Int->[[Int]]->Bool->IO()
rodada numRodada jogadoresInfo objetivos indiceJogador mapa recemCarregado = do
    if (contagemDeTerritorios indiceJogador mapa) == 0 then rodada numRodada jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapa False

    else if (indiceJogador == (primeiroJogador mapa 1)) && (numRodada /= 1 && not recemCarregado) then do
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

                    if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa False
                    else do
                        mapaPosAtaque <- botAtaca novoMapa indiceJogador jogadoresInfo objetivos
                        rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapaPosAtaque False
                else do
                    novoMapa <- menuAlocacaoTerritorios mapa indiceJogador 5 jogadoresInfo objetivos
                    verificaObjetivos jogadoresInfo objetivos mapa

                    if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa False
                    else do
                        mapaPosAtaque <- inputAtaque novoMapa indiceJogador jogadoresInfo objetivos
                        mapaPosMover <- inputMovimento mapaPosAtaque indiceJogador [] jogadoresInfo objetivos
                        rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapaPosMover False
            _ -> do
                putStrLn "Opcao invalida. Por favor, digite 1 para salvar ou 0 para continuar."
                rodada numRodada jogadoresInfo objetivos indiceJogador mapa False
    else do
        defineCor indiceJogador
        putStrLn $ "Vez do jogador " ++ (show indiceJogador)
        setSGR [Reset]
        if ehBot jogadoresInfo indiceJogador then do
            novoMapa <- botAlocacaoTerritorios mapa indiceJogador 5 jogadoresInfo objetivos
            verificaObjetivos jogadoresInfo objetivos mapa

            if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa False
            else do
                mapaPosAtaque <- botAtaca novoMapa indiceJogador jogadoresInfo objetivos
                rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapaPosAtaque False
        else do
            novoMapa <- menuAlocacaoTerritorios mapa indiceJogador 5 jogadoresInfo objetivos
            verificaObjetivos jogadoresInfo objetivos mapa

            if (numRodada <= (sum jogadoresInfo)) then rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) novoMapa False
            else do
                mapaPosAtaque <- inputAtaque novoMapa indiceJogador jogadoresInfo objetivos
                mapaPosMover <- inputMovimento mapaPosAtaque indiceJogador [] jogadoresInfo objetivos
                rodada (numRodada + 1) jogadoresInfo objetivos ((mod indiceJogador (sum jogadoresInfo)) + 1) mapaPosMover False


primeiroJogador::[[Int]]->Int->Int
primeiroJogador mapa indiceJogador =
    if (contagemDeTerritorios indiceJogador mapa) > 0 then indiceJogador
    else primeiroJogador mapa (indiceJogador + 1)

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


contagemDeTerritorios::Int->[[Int]]->Int
contagemDeTerritorios _ [] = 0
contagemDeTerritorios indiceJogador (h:t) =
    if (h !! 0) == indiceJogador then (1 + (contagemDeTerritorios indiceJogador t))
    else contagemDeTerritorios indiceJogador t

ehBot::[Int]->Int->Bool
ehBot jogadoresInfo indiceJogador = indiceJogador > (jogadoresInfo !! 0)