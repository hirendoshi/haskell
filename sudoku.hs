import Data.List

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

easy :: Grid
easy                  =  ["2....1.38",
                          "........5",
                          ".7...6...",
                          ".......13",
                          ".981..257",
                          "31....8..",
                          "9..8...2.",
                          ".5..69784",
                          "4..25...."]

blank :: Grid
blank = replicate 9 (replicate 9 '.')

rows :: Matrix a -> [Row a]
rows  = id

cols :: Matrix a -> [Row a]
cols  = transpose 


