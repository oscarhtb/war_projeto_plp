module War where
import Rodada (rodada)
import MenuInicial (menuInicial)
import PosicionamentoInicial (iterateOverShuffle, shuffleList)
import ListaDeObjetivos (listaDeObjetivos)
import ShuffleListPura (shuffleListPura, gerarSeed)


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
    let slist = shuffleListPura gerarSeed [1..6]
    let slistmapa shuffleListPura gerarSeed [1..24]
    jogadoresInfo <- menuInicial  -- Extraímos a lista de IO [Int]
    
    let numJogadores = sum jogadoresInfo  -- Agora podemos calcular sum
    
    let objetivos = listaDeObjetivos numJogadores slist  -- Extraímos a lista de IO [Int]
    
    
    let mapa = iterateOverShuffle slistmapa (replicate 24 [0,1]) numJogadores
    print "Os territorios foram sorteados!"
    print mapa
    
    rodada jogadoresInfo objetivos mapa  -- Chamada correta da função
