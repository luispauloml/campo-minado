module Basico where

import Data.Matrix

import Tipos
  
-- Operacao elemento por elemento com tres matrizes
elementwise3 :: (a -> b -> c -> d) -> Matrix a -> Matrix b -> Matrix c -> Matrix d
elementwise3 f ma mb mc = elementwise (\(a,b) c -> f a b c) m' mc
  where m' = elementwise (\a b -> (a,b)) ma mb
  
-- Substituir valores numa matriz a partir de listas
substML :: [a] -> [(Int,Int)] -> Matrix a -> Matrix a
substML []      _     m = m
substML  _     []     m = m
substML (v:vs) (p:ps) m = substML vs ps (worker v p m)
  where worker x (i,j) e | i < 1 || i > nrows e || j < 1 || j > nrows e = e
                         | otherwise = setElem x (i,j) e

-- Vizinhaca de uma posicao
vizinhanca :: Posicao -> [Posicao]
vizinhanca (x,y) = [ (x-1,y-1) , (x  ,y-1) , (x+1,y-1)
                   , (x-1,y  ) ,             (x+1,y  )
                   , (x-1,y+1) , (x  ,y+1) , (x+1,y+1) ]
                   
-- Flood fill em 4 direções para matrizes
floodMatrix :: (Int,Int)   -- Posicao incial
            -> (a -> Bool) -- Funçao de condiçao
            -> (a -> a)    -- Funçao de mudança (se condição foi falsa)
            -> (a -> a)    -- Função de parada (se condição foi verdadeira)
            -> Matrix a    -- Matrix de entrada
            -> Matrix a    -- Matrix de saída
floodMatrix (i,j) cond mud par ent
    | i < 1 || j < 1 || i > nrows ent || j > ncols ent = ent
    | cond x == True  =      setElem (par x) (i,j) ent
    | cond x == False = f' $ setElem (mud x) (i,j) ent
      where x    = getElem i j ent
            f' m = floodMatrix (i+1,j) cond mud par $
                   floodMatrix (i-1,j) cond mud par $
                   floodMatrix (i,j+1) cond mud par $
                   floodMatrix (i,j-1) cond mud par m




