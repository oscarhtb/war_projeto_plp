module MenuInicial where

import Utils (ehInteiro)

menuInicial::IO [Int]
menuInicial = do
 putStrLn "Quantos jogadores terão na partida? (1 a 4):"
 inputUsuario <- getLine
 if not (ehInteiro inputUsuario) then do
    putStrLn "Entrada inválida :("
    menuInicial
 else
    let numJogadores = read inputUsuario :: Int
    in if (numJogadores > 4) || (numJogadores < 1) then do
        putStrLn "Entrada inválida :("
        menuInicial
    else if numJogadores == 4 then return [4,0]
    else do
        putStrLn ("Quantos bots seu jogo terá? (" ++ (show (minBots numJogadores)) ++ " a " ++ (show (maxBots numJogadores)) ++ ")")
        inputUsuarioBots <- getLine
        if not (ehInteiro inputUsuarioBots) then do
            putStrLn "Entrada inválida :("
            menuInicial
        else
            let numBots = read inputUsuarioBots :: Int
            in if (numBots < (minBots numJogadores)) || (numBots > (maxBots numJogadores)) then do
                putStrLn "Entrada inválida :("
                menuInicial
            else
                return ([numJogadores, numBots])

maxBots::Int->Int
maxBots numJogadores = 4 - numJogadores

minBots::Int->Int
minBots 1 = 1
minBots _ = 0

