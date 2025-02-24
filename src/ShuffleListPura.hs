module ShuffleListPura where
import System.Random (mkStdGen, randomR, StdGen)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- embaralha a lista a partir de uma seed
shuffleListPura :: Int -> [a] -> [a]
shuffleListPura seed xs = shuffle xs (mkStdGen seed)

-- utilizada pela função de embaralhar
shuffle :: [a] -> StdGen -> [a]
shuffle [] _ = []
shuffle xs gen = 
    let (n, newGen) = randomR (0, length xs - 1) gen
        picked = xs !! n
        rest = take n xs ++ drop (n + 1) xs
    in picked : shuffle rest newGen

-- gera a seed aleatória
gerarSeed::IO Int
gerarSeed = do
    posix <- getPOSIXTime
    return (floor (posix * 1000000))
