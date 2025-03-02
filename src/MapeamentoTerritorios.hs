module MapeamentoTerritorios where

-- uma lista na qual cada elemento representa os vizinhos do território que representa seu índice
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
        [23], --vizinhos do 24
    ]


retornaSigla::Int->String
retornaSigla indice
    |indice == 1 = "AL"
    |indice == 2 = "CA"
    |indice == 3 = "GL"
    |indice == 4 = "NY"
    |indice == 5 = "MX"
    |indice == 6 = "BR"
    |indice == 7 = "AR"
    |indice == 8 = "UK"
    |indice == 9 = "GE"
    |indice == 10 = "SP"
    |indice == 11 = "TU"
    |indice == 12 = "MR"
    |indice == 13 = "EG"
    |indice == 14 = "SA"
    |indice == 15 = "MA"
    |indice == 16 = "MO"
    |indice == 17 = "SI"
    |indice == 18 = "VL"
    |indice == 19 = "CH"
    |indice == 20 = "JP"
    |indice == 21 = "IN"
    |indice == 22 = "VI"
    |indice == 23 = "AU"
    |indice == 24 = "NZ"
    |otherwise = "número inválido"

mapeiaTerritorio::String->Int
mapeiaTerritorio territorio
    |territorio == "AL" = 1
    |territorio == "CA" = 2
    |territorio == "GL" = 3
    |territorio == "NY" = 4
    |territorio == "MX" = 5
    |territorio == "BR" = 6
    |territorio == "AR" = 7
    |territorio == "UK" = 8
    |territorio == "GE" = 9
    |territorio == "SP" = 10
    |territorio == "TU" = 11
    |territorio == "MR" = 12
    |territorio == "EG" = 13
    |territorio == "SA" = 14
    |territorio == "MA" = 15
    |territorio == "MO" = 16
    |territorio == "SI" = 17
    |territorio == "VL" = 18
    |territorio == "CH" = 19
    |territorio == "JP" = 20
    |territorio == "IN" = 21
    |territorio == "VI" = 22
    |territorio == "AU" = 23
    |territorio == "NZ" = 24
    |otherwise = -1



