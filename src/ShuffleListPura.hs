module ShuffleListPura where
import System.Random (mkStdGen, randomR, StdGen)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Embaralha uma lista usando Fisher-Yates e um gerador puro
shuffleListPura :: Int -> [a] -> [a]
shuffleListPura seed xs = shuffle xs (mkStdGen seed)

shuffle :: [a] -> StdGen -> [a]
shuffle [] _ = []
shuffle xs gen = 
    let (n, newGen) = randomR (0, length xs - 1) gen  -- Escolhe um índice aleatório
        picked = xs !! n  -- Pega o elemento na posição n
        rest = take n xs ++ drop (n + 1) xs  -- Remove o elemento da lista
    in picked : shuffle rest newGen  -- Continua embaralhando os elementos restantes

gerarSeed::IO Int
gerarSeed = do
    posix <- getPOSIXTime
    return (floor (posix * 1000000))
