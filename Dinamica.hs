module Dinamica where

import System.Random
import Data.Matrix
import Data.List

import Tipos

-- Gerar posicao das bombas (g: semente; n: quantidade; t: tamanho)
gerarBombas :: StdGen -> Int -> Int -> [Posicao]
gerarBombas g n t =  if n >= t*t
                     then error $ "Número de bombas deve ser menor " ++
                                  "que o quadrado do tamanho do campo"
                     else take n $ nub . pares $ randomRs (1,t) g
  where pares [] = []
        pares (a:b:ps) = (a,b) : pares ps

-- Gerar tabuleiro de bombas (tam: tamanho; pos: posicoes das bombas)
posicionarBombas :: Int -> [Posicao] -> Matrix Bool
posicionarBombas tam pos = matrix tam tam (\p -> p `elem` pos)

-- Gerar tabuleiro de pontucao (tam: tamanho; pos: posicoes das bombas)
gerarPontos :: Int -> [Posicao] -> Matrix Int
gerarPontos tam pos = matrix tam tam (worker pos)
  where worker pos (i,j) = length $ filter (flip elem pos) $ vizinhanca (i,j)
        vizinhanca (x,y) = [ (x-1,y-1) , (x  ,y-1) , (x+1,y-1)
                           , (x-1,y  ) , (x  ,y  ) , (x+1,y  )
                           , (x-1,y+1) , (x  ,y+1) , (x+1,y+1) ]

-- Gerar tabuleiro completo completamente coberto
gerarCampo :: StdGen -> Int -> Int -> Campo
gerarCampo g n t = elementwise3 (,,) (matrix t t (const Coberto)) bombas pontos
  where b      = gerarBombas g n t
        bombas = posicionarBombas t b
        pontos = gerarPontos t b
        elementwise3 f ma mb mc = elementwise (\c (a,b) -> f a b c) mc
                                $ elementwise (\a b -> (a,b)) ma mb

-- Descobrir o mapa após uma jogada
descobrirMapa :: Campo -> Posicao -> Campo
descobrirMapa c (i,j) = floodMatrix (i,j) avaliar descobrir parar c
  where avaliar   (s,b,p) | s == Descoberto = True
                          | p > 0           = True
                          | otherwise       = False
        descobrir (s,b,p) | s == Coberto    = (Descoberto,b,p)
                          | s == Marcado    = (Descoberto,b,p)
                          | otherwise       = (s,b,p)
        parar             = descobrir

-- Flood fill em 4 direções para matrizes
floodMatrix :: (Int,Int)   -- Posicao incial
            -> (a -> Bool) -- Funçao de condiçao
            -> (a -> a)    -- Funçao de mudança (se condição foi falsa)
            -> (a -> a)    -- Função de parada (se condição foi verdadeira)
            -> Matrix a    -- Matriz de entrada
            -> Matrix a    -- Matriz de saída
floodMatrix (i,j) cond mud par ent
    |  i < 1 || i > nrows ent   
    || j < 1 || j > ncols ent = ent
    | cond x == True  =      setElem (par x) (i,j) ent
    | cond x == False = f' $ setElem (mud x) (i,j) ent
      where x    = getElem i j ent
            f' m = floodMatrix (i+1,j) cond mud par $
                   floodMatrix (i-1,j) cond mud par $
                   floodMatrix (i,j+1) cond mud par $
                   floodMatrix (i,j-1) cond mud par m        

-- Marcar casa
marcarCasa :: Campo -> Posicao ->  Campo
marcarCasa m (i,j) = let (s,b,p) = getElem i j m in
                     case s of Marcado    -> setElem (Coberto,b,p) (i,j) m
                               Coberto    -> setElem (Marcado,b,p) (i,j) m
                               Descoberto -> m

-- Descobrir todo o mapa
descobrirTudo :: Campo -> Campo
descobrirTudo campo = matrix (nrows campo) (ncols campo) worker
  where worker (i,j) = (\(_,b,p) -> (Descoberto,b,p)) (getElem i j campo)

-- Ação no mapa
acaoMapa :: Campo -> (Posicao, Estado) -> Campo
acaoMapa m ((i,j),Marcado) = marcarCasa    m (i,j)
acaoMapa m ((i,j),_)       = descobrirMapa m (i,j)

-- Verifica se o jogo continua ou é interrompido
gameOver :: Campo -> GameOver
gameOver jogo = worker True $ toList jogo
  where worker True   [] = Vitoria
        worker False  [] = Continua
        worker m ((s,b,p):xs) 
            |  s == Descoberto               && b     = Derrota
            | (s == Coberto || s == Marcado) && not b = worker False xs
            | otherwise = worker m xs
