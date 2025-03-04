module War where
import Rodada (rodada)
import MenuInicial (menuInicial)
import PosicionamentoInicial (iterateOverShuffle)
import ListaDeObjetivos (listaDeObjetivos)
import ShuffleListPura (shuffleListPura, gerarSeed)
import MostrarObjetivos (exporObjetivo)
import RepresentacaoTerritorios (imprimeMapa)
import Salvamento (carregarJogo)
import System.Directory (doesFileExist)


-- ordem cronológica:
-- chamar menu inicial (prints no terminal, recebe entrada) -> retorna [numJogadores, numBots]
-- sortearObjetivos (sortear os objetivos) -> retorna lista de objetivos e os imprime no terminal
-- mapaSorteado (recebe qtdJogadores) -> o mapa sorteado -> mostrar o mapa sorteado no terminal

-- war::IO()
-- war = do
--     slist <- shuffleList [1..6]
--     slistmapa <- shuffleList [1..24]
--     let jogadoresInfo = menuInicial
--     rodada jogadoresInfo (listaDeObjetivos (sum jogadoresInfo) slist) (iterateOverShuffle slistmapa (replicate 24 [0,1]) (sum jogadoresInfo))

war :: IO ()
war = do
    seed1 <- gerarSeed
    seed2 <- gerarSeed
    let slist = shuffleListPura seed1 [1..6]
    let slistmapa = shuffleListPura seed2 [1..24]

    -- perguntar se ele quer começar um novo jogo (n) ou continuar um jogo antigo (a)
    putStrLn "Você deseja iniciar um novo jogo (0) ou continuar um jogo salvo (1)?"
    resposta <- getLine
    case resposta of -- [mapa, jogadoresInfo, objetivos, ] [mapa, [jogadoresInfo, objetivos], [[numRodada, indiceJogador]]]
        "0" -> do

            jogadoresInfo <- menuInicial  -- Extraímos a lista de IO [Int]
            
            let numJogadores = sum jogadoresInfo  -- Agora podemos calcular sum
            
            let objetivos = listaDeObjetivos numJogadores slist  -- Extraímos a lista de IO [Int]
            
            
            let mapa = iterateOverShuffle slistmapa (replicate 24 [0,1]) numJogadores
            print "Os territorios foram sorteados!"
            -- print mapa
            imprimeMapa mapa

            exporObjetivo (jogadoresInfo !! 0) objetivos
            
            rodada 1 jogadoresInfo objetivos 1 mapa  -- Chamada correta da função
        
        "1" -> do
            -- checar se arquivo existe
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
                        rodada numRodada jogInfo obj indiceJogador m

                    Nothing -> putStrLn "Erro ao carregar o jogo. O arquivo pode estar corrompido ou no formato errado."
            else do
                putStrLn "Arquivo não encontrado! Tente novamente."
                war

