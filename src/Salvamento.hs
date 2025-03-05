module Salvamento where

import Text.Read (readMaybe)

salvarJogo :: [[[Int]]] -> FilePath -> IO ()
salvarJogo lista caminho = writeFile caminho (show lista)

carregarJogo :: FilePath -> IO (Maybe [[[Int]]])
carregarJogo caminho = do
  conteúdo <- readFile caminho
  return (readMaybe conteúdo)
