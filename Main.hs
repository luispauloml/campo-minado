module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import System.Random
import System.Console.ANSI (clearScreen)

import Dinamica
import Tipos
import Graficos
import UI

-- Jogo no modo texto
jogar :: StdGen -- semepte para RNG
      -> Int    -- quantidade de bombas
      -> Int    -- tamanho do campo
      -> IO()   -- saída
jogar g n t =
  let mapa        = gerarCampo g n t
      imprimir    = putStrLn . showCampo
  in do imprimir mapa
        loopTexto mapa
        getLine >> return ()

-- Jogo com gráficos
jogarG :: StdGen -- semente para RNG
       -> Int    -- quantidade de bombas
       -> Int    -- tamanho do campo
       -> IO()   -- saída
jogarG g n t = let s = t * (fromEnum $ fst tamBloco)
               in playIO (InWindow  "Campo Minado" (s, s) (40,40)) 
                         (greyN 0.95)        -- cor de fundo
                         30                  -- fps
                         (gerarCampo g n t)  -- mapa inicial
                         (renderCampo t)     -- função para renderizar
                         (capturaClique t)   -- eventos de mouse
                         (\_ c -> return c)  -- passo de tempo

main :: IO ()
main = do a <- getArgs
          g <- getStdGen
          if length a /= 3
          then usoMain
          else let n = read (a !! 0) :: Int
                   t = read (a !! 1) :: Int
                   m = head (a !! 2)
               in if m == 'g'
                  then jogarG g n t
                  else if m == 't'
                       then clearScreen >> jogar g n t
                       else usoMain
