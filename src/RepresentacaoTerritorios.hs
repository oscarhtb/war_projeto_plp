module RepresentacaoTerritorios where
import System.Console.ANSI

import MapeamentoTerritorios (retornaSigla)

imprimeMapa::[[Int]]->IO()
imprimeMapa mapa = do
    putStrLn "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣄⣠⣀⡀⣀⣠⣤⣤⣤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣄⢠⣠⣼⣿⣿⣿⣟⣿⣿⣿⣿"
    
    imprimeSiglasColorido ((mapa !! 2) !! 0) 2
    putStrLn "⣿⣿⡿⠋⠀⠀⠀⢠⣤⣦⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⢦⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    putStr "*⠀⠀⠀⠀⠀⠀⠀⣼⣿⣟⣾⣿⣽⣿⣿⣅⠈⠉⠻⣿"

    imprimeNumeroColorido ((mapa !! 2) !! 0) ((mapa !! 2) !! 1)
    putStrLn "⣿⡿⠇*⠀⠀⠀⠀⠉⠀⠀⠀⠀⠀⢀⡶⠒⢉⡀⢠⣤⣶⣶⣿⣷⣆⣀⡀⠀⢲⣖⠒⠀⠀⠀⠀⠀⠀*"
    putStr "⢀⣤⣾⣶⣦⣤⣤⣶⣿⣿⣿⣿⣿⣿⣽⡿⠻⣷⣀⠀⢻⣿⣿⣿⡿⠟⠀ * ⠀⠀⣤⣶⣶⣤⣀⣀⣬⣷⣦⣿⣿⣿⣿⣿⣿⣿"
    
    imprimeSiglasColorido ((mapa !! 16) !! 0) 16
    putStrLn "⣿⣿⣿⣿⣿⣿⣶⣦⣤⣦⣼⣀⠀"
    putStr "⠈⣿"

    imprimeSiglasColorido ((mapa !! 0) !! 0) 0
    putStr "⣿⣿⣿⣿⣿⣿"

    imprimeSiglasColorido ((mapa !! 1) !! 0) 1
    putStr "⣿⣿⡿⠛⠓⣿⣿⠟⠁⠘⣿⡟⠁⠀⠘⠛⠁⠀*⢠⣾⣿⢿⣿⣿⣿⣿"

    imprimeSiglasColorido ((mapa !! 9) !! 0) 9
    putStr "⣿⣿⣿⣿⣿⣿⣿"

    imprimeNumeroColorido  ((mapa !! 16) !! 0)  ((mapa !! 16) !! 1)
    putStr "⣿⣿⣿⣿"
    
    imprimeSiglasColorido ((mapa !! 17) !! 0) 17
    putStrLn "⣿⣿⣿⡿⠏⠙⠁"
    putStr " ⠸"

    imprimeNumeroColorido ((mapa !! 0) !! 0) ((mapa !! 0) !! 1)
    putStr "⠈⠙⣿⣿⣿"

    imprimeNumeroColorido ((mapa !! 1) !! 0) ((mapa !! 1) !! 1)
    putStr "⣿⣷⣦⡄⣿⣿⣿⣆⠀⠀⠀⠀⠀⠀⠀"
    
    imprimeSiglasColorido ((mapa !! 7) !! 0) 7
    putStr "⣆⢘"
    
    imprimeSiglasColorido ((mapa !! 8) !! 0) 8
    putStr "⣼⣿⣿⣿⣿"
    
    imprimeNumeroColorido ((mapa !! 9) !! 0) ((mapa !! 9) !! 1)
    putStr "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿"
    
    imprimeNumeroColorido ((mapa !! 17) !! 0) ((mapa !! 17) !! 1)
    putStrLn "⡿⠀⠀⠀⠀⠀"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠘⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿"

    imprimeSiglasColorido ((mapa !! 3) !! 0) 3
    putStr "⠦⠀⠀⠀⠀⠀"

    imprimeNumeroColorido ((mapa !! 7) !! 0) ((mapa !! 7) !! 1)
    putStr "⣿⣿"
    
    imprimeNumeroColorido ((mapa !! 8) !! 0) (((mapa !! 8) !! 1))
    putStr "⣿⣿⣿⣿⣿⣿⣿⣿⣿"
    
    imprimeSiglasColorido ((mapa !! 15) !! 0) 15
    putStrLn "⣿⣿⣿⣿⣿⡿⡗⠀⠈*⠀⠀⠀⠀⠀"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⢻⣿⣿"
    
    imprimeSiglasColorido ((mapa !! 4) !! 0) 4
    putStr "⣿⣿⣿⣿⣿"
    
    imprimeNumeroColorido ((mapa !! 3) !! 0) ((mapa !! 3) !! 1) 
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⢿⣿⣉⣿⡿⢿⢷⣾"
    
    imprimeSiglasColorido ((mapa !! 10) !! 0) 10
    putStr "⣞⣿⣿"
    
    imprimeNumeroColorido ((mapa !! 15) !! 0) ((mapa !! 15) !! 1)
    putStr "⣿⣿"
    
    imprimeSiglasColorido ((mapa !! 18) !! 0) 18
    putStr "⣿⣿⣿⠋⣠⠟"
    
    imprimeSiglasColorido ((mapa !! 19) !! 0) 19
    putStrLn "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠹⣿"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠹⣿"
    
    imprimeNumeroColorido ((mapa !! 4) !! 0) ((mapa !! 4) !! 1)
    putStr "⠿⣿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣾⣿⣿⣷⣦⣶⣦⣼⣿"
    
    imprimeNumeroColorido ((mapa !! 10) !! 0) ((mapa !! 10) !! 1)
    putStr "⣞⣿⣿⣿⣿⣿"
    
    imprimeNumeroColorido ((mapa !! 18) !! 0) ((mapa !! 18) !! 1)
    putStr "⣿⣷⠈⠛⠁"
    
    imprimeNumeroColorido ((mapa !! 19) !! 0) ((mapa !! 19) !! 1)
    putStrLn "⠀⠀⠀⠀⠀"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠻⣿⣤⡖⠛⠶⠤⡀⠀⠀⠀⠀⠀⠀⠀⢰⣿"
    
    imprimeSiglasColorido ((mapa !! 11) !! 0) 11
    putStr "⣿⣿⣿"
    
    imprimeSiglasColorido ((mapa !! 12) !! 0) 12
    putStrLn "⣿⣿⣿⣿⡿⠁⠙⣿⣿⠿⢻⣿⣿⡿⠋⢩⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⠧⣤⣦⣤⣄⡀⠀⠀⠀ * ⠘"
    
    imprimeNumeroColorido ((mapa !! 11) !! 0) ((mapa !! 11) !! 1)
    putStr "⣿⣿⣿"
    
    imprimeNumeroColorido ((mapa !! 12) !! 0) ((mapa !! 12) !! 1)
    putStr "⣿⣿⡇⠀⠀⠀"
    
    imprimeSiglasColorido ((mapa !! 20) !! 0) 20
    putStr "⠀⠈⣹"
    
    imprimeSiglasColorido ((mapa !! 21) !! 0) 21
    putStrLn "⢀⣿⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣿⣿⣿"
    
    imprimeSiglasColorido ((mapa !! 5) !! 0) 5
    putStr "⣤⣀⡀*⠀⠀⠀ ⠀⠈⢽⣿⣿⣿⣿⣿⠋⠀⠀⠀⠀"
    
    imprimeNumeroColorido ((mapa !! 20) !! 0) ((mapa !! 20) !! 1)
    putStr "⠀"
    
    imprimeNumeroColorido ((mapa !! 21) !! 0) ((mapa !! 21) !! 1)
    putStrLn "⣿⣷⢲⣦⣤⡀⢀⡀⠀⠀⠀⠀⠀⠀"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢿⣿⣿"
    
    imprimeNumeroColorido ((mapa !! 5) !! 0) ((mapa !! 5) !! 1)
    putStr "⣿⠟⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣷"
    
    imprimeSiglasColorido ((mapa !! 14) !! 0) 14
    putStrLn "⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠂⠛⣆⣤⡜⣟⠋⠙⠂⠀⠀⠀⠀"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹"
    
    imprimeSiglasColorido ((mapa !! 6) !! 0) 6
    putStr "⣿⠟⠀⠀⠀⠀⠀⠀⠀⠀ ⠘⣿"
    
    imprimeSiglasColorido ((mapa !! 13) !! 0) 13
    putStr "⣿⠉"
    
    imprimeNumeroColorido ((mapa !! 14) !! 0) ((mapa !! 14) !! 1)
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⣤⣾⣿"
    
    imprimeSiglasColorido ((mapa !! 22) !! 0) 22
    putStrLn "⣿⣆⠀⠰⠄⠀⠉⠀⠀" 
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    
    imprimeNumeroColorido ((mapa !! 6) !! 0) ((mapa !! 6) !! 1)
    putStr "⡿⠃⠀⠀⠀⠀⠀⠀⠀⠀ ⠀⠀"
    
    imprimeNumeroColorido ((mapa !! 13) !! 0) ((mapa !! 13) !! 1)
    putStr "⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢻⣿"
    
    imprimeNumeroColorido ((mapa !! 22) !! 0) ((mapa !! 22) !! 1)
    putStr "⣿⣿⠇*"
    
    imprimeSiglasColorido ((mapa !! 23) !! 0) 23
    putStrLn "⠀⠀⠀"
    putStr "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⡿⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢻⡇⠀"
    
    imprimeNumeroColorido ((mapa !! 23) !! 0) ((mapa !! 23) !! 1)
    putStrLn "⣼⠗⠀⠀"
    putStrLn "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠃⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⠙⠁⠀⠀"
    putStrLn "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⠒⠀⠀"
    
imprimeSiglasColorido::Int->Int->IO()
imprimeSiglasColorido indice terr = do
    defineCor indice
    putStr (retornaSigla (terr + 1))
    setSGR [Reset]

imprimeNumeroColorido::Int->Int->IO()
imprimeNumeroColorido indice numero = do
    defineCor indice
    putStr (formataNumero numero)
    setSGR [Reset]

formataNumero :: Int -> String
formataNumero n = replicate (3 - length str) '0' ++ str
  where str = show n

defineCor :: Int -> IO ()
defineCor n
    | n == 1    = setSGR [SetColor Foreground Vivid Cyan]
    | n == 2    = setSGR [SetColor Foreground Vivid Yellow]
    | n == 3    = setSGR [SetColor Foreground Vivid Green]
    | otherwise = setSGR [SetColor Foreground Vivid Magenta]
