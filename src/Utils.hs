module Utils where

import Text.Read (readMaybe)
import System.Console.ANSI
import System.Exit (exitSuccess)

import RepresentacaoTerritorios (imprimeMapa, defineCor)
import ChecagemDeObjetivos (checagemVitoria)
import MostrarObjetivos (imprimeObjetivo)

substituirSublista :: [[a]] -> Int -> [a] -> [[a]]
substituirSublista listaDeListas pos novaSublista 
    | pos < 0 || pos > length listaDeListas = listaDeListas
    | otherwise = let (antes, depois) = splitAt (pos - 1) listaDeListas in antes ++ [novaSublista] ++ tail depois

ehInteiro :: String -> Bool
ehInteiro s = case readMaybe s :: Maybe Int of
                Just _  -> True
                Nothing -> False

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

pertence::[[Int]]->Int->Int->Bool
pertence mapa territorio indiceJogador =
    (territorio /= -1) && ((mapa !! (territorio - 1)) !! 0) == indiceJogador


