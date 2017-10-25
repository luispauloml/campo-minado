module Main where

import Data.Matrix
import Text.Printf
import System.Environment
import System.Random
import System.Console.ANSI (clearScreen)

import Basico
import Dinamica
import Entrada
import Tipos
import UI
                 
jogar :: StdGen -> Int -> Int -> IO ()
jogar g n t =
  let mapa        = gerarCampo g n t
      imprimir    = printf . showCampo
  in do imprimir mapa
        loopTexto mapa
        getChar >>= (\_ -> return ())

jogarG :: StdGen -> Int -> Int -> IO()
jogarG = undefined

main :: IO ()
main = do a <- getArgs
          g <- getStdGen
          if length a /= 3
          then usoMain
          else let n = read . head        $ a :: Int
                   t = read . head . tail $ a :: Int
                   m = head (a !! 2)
                   in if m == 'g'
                      then error "Modo gráfico ainda nào foi implementado."
                      else if n > t*t 
                      then error $ "Número de bombas deve ser menor "++
                                   "que o quadrado do tamanho."
                      else do clearScreen
                              jogar g n t
          
          
          
