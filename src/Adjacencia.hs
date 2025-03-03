module Adjacencia where

matrizAdjacencia::[[Int]]
matrizAdjacencia =
    [
        [2, 18], --vizinhos do 1
        [1, 5, 4, 3], --vizinhos do 2
        [8, 2, 4], --vizinhos do 3
        [3, 2, 5], --vizinhos do 4
        [4, 2, 6], --vizinhos do 5
        [5, 7, 12], --vizinhos do 6
        [6], --vizinhos do 7
        [3, 9], --vizinhos do 8
        [8, 12, 10], --vizinhos do 9
        [9, 11, 16, 17], --vizinhos do 10
        [13, 10, 16], --vizinhos do 11
        [6, 9, 13], --vizinhos do 12
        [15, 14], --vizinhos do 13
        [15, 13], --vizinhos do 14
        [14, 13], --vizinhos do 15
        [11, 10, 17, 19, 21], --vizinhos do 16
        [16, 18, 10, 19], --vizinhos do 17
        [1, 17, 20], --vizinhos do 18
        [21, 22, 20, 16, 17], --vizinhos do 19
        [18, 19], --vizinhos do 20
        [16, 19, 22], --vizinhos do 21
        [23, 21, 19], --vizinhos do 22
        [24, 22], --vizinhos do 23
        [23] --vizinhos do 24
    ]

checarAdjacencia::Int->Int->[[Int]]->Bool
checarAdjacencia terr1 terr2 adj = (any (== terr1) (adj !! (terr2 - 1)))