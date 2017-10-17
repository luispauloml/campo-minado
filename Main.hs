module Main where

import System.Random
import Data.Matrix
import System.Environment
import Text.Printf

import Basico
import Dinamica
import Entrada
import Tipos
import UI

-- Gerar inicio do jogo (g: semente; n: quantidade; t: tamanho)
estadoInicial :: StdGen -> Int -> Int -> Campo
estadoInicial g n t = elementwise3 (\a b c -> (a,b,c)) blank matrixBombas pontos
  where listaBombas    = gerarBombas g n t
        matrixBombas   = posicionarBombas t listaBombas
        blank  = matrix t t (const Coberto)
        pontos = gerarPontos t listaBombas


main :: IO ()
main = undefined