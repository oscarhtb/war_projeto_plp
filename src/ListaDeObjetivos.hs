module ListaDeObjetivos where

import System.Random
import System.Random.Shuffle (shuffle')

shuffleList :: [a] -> IO [a]
shuffleList xs = do
    gen <- newStdGen  -- Gera um número aleatório como semente
    return $ shuffle' xs (length xs) gen  -- Embaralha a lista

-- ta funcionando, mas precisamos receber uma shuffleList pura
listaDeObjetivos::Int->[Int]->[Int] --qtd->lista de objetivos
listaDeObjetivos qtd slist =
    take qtd slist

