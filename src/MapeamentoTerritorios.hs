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



