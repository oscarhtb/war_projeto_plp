module MenuInicial where

qtdMaxBots::Int->Int
qtdMaxBots n = 4 - n


-- pergunta quantos jogadores vai ter
-- quantos deles vão ser bots

-- essa versão não trata entradas inválidas
menuInicial::IO [Int]
menuInicial = do
 putStrLn "Quantos jogadores terão na partida? (1 a 4):" -- é a quantidade de jogadores REAIS
 numJogadores <- readLn::IO Int
 if numJogadores == 4 then return [4, 0] else do
    putStrLn $ "Quantos bots você quer ter? (1 a " ++ show (qtdMaxBots numJogadores) ++ "):"
    numBots <- readLn::IO Int
    return [numJogadores, numBots]



