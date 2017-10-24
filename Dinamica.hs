module Dinamica where

import System.Random
import Data.Matrix
import Data.List

import Tipos
import Basico

-- Gerar posicao das bombas (g: semente; n: quantidade; t: tamanho)
gerarBombas :: StdGen -> Int -> Int -> [Posicao]
gerarBombas g n t =  if n > t*t
                     then take (t*t) $ nub . pares $ randomRs (1,t) g
                     else take n $ nub . pares $ randomRs (1,t) g
  where pares [] = []
        pares (a:b:ps) = (a,b) : pares ps
        
-- Gerar tabuleiro de bombas (tam: tamanho; pos: posicoes das bombas)
posicionarBombas :: Int -> [Posicao] -> Matrix Bool
posicionarBombas tam pos = substML (repeat True) pos $ matrix tam tam (const False)
        
-- Gerar tabuleiro de pontucao (tam: tamanho; pos: posicoes das bombas)
gerarPontos :: Int -> [Posicao] -> Matrix Int
gerarPontos tam pos = worker pos $ matrix tam tam (const 0)
  where worker []     m = m
        worker (p:ps) m = worker ps $ elementwise (+) m $ pontos
                                    $ filtrar . vizinhanca $ p
        pontos  ps = substML (repeat 1) ps $ matrix tam tam (const 0)
        filtrar    = filter (\(i,j) -> if i < 1 || i > tam 
                                       || j < 1 || j > tam
                                       then False 
                                       else True )

-- Gerar tabuleiro completo
gerarCampo :: StdGen -> Int -> Int -> Campo
gerarCampo g n t = elementwise3 (,,) (matrix t t (const Coberto)) mb mp
  where b  = gerarBombas g n t
        mb = posicionarBombas t b
        mp = gerarPontos t b

-- Descobrir o mapa
descobrirMapa :: Posicao -> Campo -> Campo
descobrirMapa (i,j) c = floodMatrix (i,j) avaliar descobrir parar c
  where avaliar   (s,b,p) | s == Descoberto = True
                          | p > 0           = True
                          | otherwise       = False
        descobrir (s,b,p) | s == Coberto    = (Descoberto,b,p)
                          | otherwise       = (s,b,p)
        parar             = descobrir 
        
-- Marcar casa
marcarCasa :: Posicao ->  Campo -> Campo
marcarCasa (i,j) m = let (s,b,p) = getElem i j m in
                     case s of Marcado    -> setElem (Coberto,b,p) (i,j) m
                               Coberto    -> setElem (Marcado,b,p) (i,j) m
                               Descoberto -> m

-- Descobrir todo o mapa
descobrirTudo :: Campo -> Campo
descobrirTudo campo = matrix (nrows campo) (ncols campo) worker
  where worker (i,j) = (\(_,b,p) -> (Descoberto,b,p)) (getElem i j campo)
                 
-- Verifica se o jogo continua ou é interrompido
gameOver :: Campo -> GameOver
gameOver jogo = worker Derrota $ toList jogo
  where worker Derrota  [] = Vitoria
        worker Continua [] = Continua
        worker m ((s,b,p):xs) 
            |  s == Descoberto               && b == True  = Derrota
            | (s == Coberto || s == Marcado) && b == False = worker Continua xs
            | otherwise = worker m xs
        
-- Para testes
semente = mkStdGen 1
qtde = 5 :: Int
tamanho = 10 :: Int
        
teste1 = gerarBombas semente qtde tamanho
teste2 = posicionarBombas tamanho teste1
teste3 = gerarPontos tamanho teste1
teste4 = gerarCampo semente qtde tamanho