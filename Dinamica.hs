module Dinamica where

import System.Random
import Data.Matrix
import Data.List

import Tipos
import Basico

-- Gerar posicao das bombas (g: semente; n: quantidade; t: tamanho)
gerarBombas :: StdGen -> Int -> Int -> [Posicao]
gerarBombas g n t =  take n $ nub . pares $ randomRs (1,t) g
  where pares [] = []
        pares (a:b:ps) = (a,b) : pares ps
        
-- Gerar tabuleiro de bombas (tam: tamanho; pos: posicoes das bombas)
posicionarBombas :: Int -> [Posicao] -> Matrix Bool
posicionarBombas tam pos = substML (repeat True) pos $ matrix tam tam (const False)
        
-- Gerar tabuleiro de pontucao (tam: tamanho; pos: posicoes das bombas)
gerarPontos :: Int -> [Posicao] -> Matrix Int
gerarPontos tam pos = worker pos $ matrix tam tam (const 0)
  where worker []     m = m
        worker (p:ps) m = worker ps $ somar m $ filtar . vizinhanca $ p
        somar m ps      = elementwise (+) m $ substML (repeat 1) ps $ matrix tam tam (const 0)
        filtar ps       = filter (\p -> if fst p < 1 || fst p > tam || 
                                           snd p < 1 || snd p > tam then False else True) ps

-- Descobrir o mapa
descobrirMapa :: Posicao -> Campo -> Campo
descobrirMapa (i,j) c = floodMatrix (i,j) avaliar descobrir parar c
  where avaliar   (s,b,p) | s == Descoberto = True
                          | p > 0           = True
                          | otherwise       = False
        descobrir (s,b,p) | s == Coberto    = (Descoberto,b,p)
                          | otherwise       = (s,b,p)
        parar     (s,b,p) | s == Coberto    = (Descoberto,b,p)
                          | otherwise       = (s,b,p)
        
-- Marcar casa
marcarCasa :: Posicao ->  Campo -> Campo
marcarCasa (i,j) m = setElem (worker x) (i,j) m
  where x = getElem i j m
        s = (\(a,_,_) -> a) x
        b = (\(_,a,_) -> a) x
        p = (\(_,_,a) -> a) x
        worker a | s == Marcado    = (Coberto,b,p)
                 | s == Coberto    = (Marcado,b,p)
                 | s == Descoberto = (s,b,p)
                 
-- Descobrir todo o mapa
descobrirTudo :: Campo -> Campo
descobrirTudo campo = matrix (nrows campo) (ncols campo) worker
  where worker (i,j) = (\(_,b,p) -> (Descoberto,b,p)) (getElem i j campo)
                 
-- Verifica se o jogo continua ou é interrompido
gameOver :: Campo -> GameOver
gameOver jogo = worker casas jogo
  where casas = [(a,b) | a <- [1..(nrows jogo)] , b <- [1..(ncols jogo)]]
        worker []     _ = Vitoria
        worker (p:ps) m
            |  s p == Descoberto                 && b p == True  = Derrota
            | (s p == Coberto || s p == Marcado) && b p == False = Continua
            | (s p == Coberto || s p == Marcado) && b p == True  = worker ps m
            | otherwise = worker ps m
        s x = (\(a,_,_) -> a) $ getElem (fst x) (snd x) jogo
        b x = (\(_,a,_) -> a) $ getElem (fst x) (snd x) jogo
        

semente = mkStdGen 1
qtde = 4 :: Int
tamanho = 50 :: Int
        
teste1 = gerarBombas semente qtde tamanho
teste2 = posicionarBombas tamanho teste1
teste3 = gerarPontos tamanho teste1
teste4 = elementwise3 (\a b c -> (a,b,c)) (matrix tamanho tamanho (const Coberto)) teste2 teste3
