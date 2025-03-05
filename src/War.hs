module War where

import System.Directory (doesFileExist)

import Rodada (rodada)
import MenuInicial (menuInicial)
import PosicionamentoInicial (posicionamentoInicial)
import RandomUtils (embaralhaLista, gerarSeed)
import MostrarObjetivos (exporObjetivo)
import RepresentacaoTerritorios (imprimeMapa)
import Salvamento (carregarJogo)

war :: IO ()
war = do
    seed1 <- gerarSeed
    seed2 <- gerarSeed
    let objetivosSorteados = embaralhaLista seed1 [1..7]
    let territoriosSorteados = embaralhaLista seed2 [1..24]

    putStrLn "Você deseja iniciar um novo jogo (0) ou continuar um jogo salvo (1)?"
    resposta <- getLine
    case resposta of
        "0" -> do
            jogadoresInfo <- menuInicial
            let numJogadores = sum jogadoresInfo
            let objetivos = take numJogadores objetivosSorteados
            let mapa = posicionamentoInicial territoriosSorteados (replicate 24 [0,1]) numJogadores
            print "Os territorios foram sorteados!"
            imprimeMapa mapa
            exporObjetivo (jogadoresInfo !! 0) objetivos
            rodada 1 jogadoresInfo objetivos 1 mapa False
        "1" -> do
            putStrLn "Digite o nome do jogo que você quer carregar:"
            nomeDoArquivo <- getLine
            existe <- doesFileExist nomeDoArquivo
            if existe then do
                estadoJogoMaybe <- carregarJogo nomeDoArquivo
                case estadoJogoMaybe of
                    Just estadoJogo -> do
                        let numRodada = ((estadoJogo !! 2) !! 0) !! 0
                        let jogInfo = (estadoJogo !! 1) !! 0
                        let obj = (estadoJogo !! 1) !! 1
                        let indiceJogador = ((estadoJogo !! 2) !! 0) !! 1
                        let m = estadoJogo !! 0
                        rodada numRodada jogInfo obj indiceJogador m True
                    Nothing -> putStrLn "Erro ao carregar o jogo. O arquivo pode estar corrompido ou no formato errado."
            else do
                putStrLn "Arquivo não encontrado! Tente novamente."
                war
        _ -> do
            putStrLn "Opcao invalida. Por favor, digite 1 para salvar ou 0 para continuar."
            war

