module Main where

import Data.Time.Clock.POSIX (getPOSIXTime)
import MenuInicial (menuInicial)
import PosicionamentoInicial (substituirSublista, iterateOverShuffle)
import ListaDeObjetivos (listaDeObjetivos)
import ShuffleListPura (shuffleListPura)
import MostrarObjetivos (exporObjetivo)
import War (war)


main :: IO()
main = war
