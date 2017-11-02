module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Text.Printf
import System.Environment
import System.Random
import System.Console.ANSI (clearScreen)

import Dinamica
import Tipos
import Graficos
import UI

-- Jogo no modo texto
jogar :: StdGen -> Int -> Int -> IO ()
jogar g n t =
  let mapa        = gerarCampo g n t
      imprimir    = printf . showCampo
  in do imprimir mapa
        loopTexto mapa
        getLine >>= (\_ -> return ())

-- Jogo com gráficos
jogarG :: StdGen -> Int -> Int -> IO()
jogarG g n t = playIO (InWindow  "Campo Minado" (s, s) (40,40)) 
                     (greyN 0.95) 
                     30
                     (gerarCampo g n t) 
                     (renderCampo t)
                     (capturaClique t)
                     (\a b -> return b)
  where s = t * (fromEnum $ fst tamBloco)

main :: IO ()
main = do a <- getArgs
          g <- getStdGen
          if length a /= 3
          then usoMain
          else let n = read . head        $ a :: Int
                   t = read . head . tail $ a :: Int
                   m = head (a !! 2)
                   in if m == 'g'
                      then jogarG g n t
                      else do clearScreen
                              jogar g n t
          
 
          
