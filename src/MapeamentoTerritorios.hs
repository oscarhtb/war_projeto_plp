module MapeamentoTerritorios where

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


