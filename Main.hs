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

        
jogar :: StdGen -> Int -> Int -> IO ()
jogar g n t = do printf $ showCampo $ descobrirTudo $ gerarCampo g n t
                 x <- getLine
                 print x

main :: IO ()
main = do a <- getArgs
          g <- getStdGen
          if length a /= 3
          then usoMain
          else let n = read . head        $ a :: Int
                   t = read . head . tail $ a :: Int
                   m = head (a !! 2)
                   in if m == 'g'
                      the:n error "Modo gráfico ainda nào foi implementado."
                      else if n > t 
                      then error "Número de bombas deve ser menor que o tamanho."
                      else jogar g n t
          
          
          
