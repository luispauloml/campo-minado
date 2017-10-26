module Basico where

import Data.Matrix

import Tipos
  
-- Operacao elemento por elemento com tres matrizes
elementwise3 :: (a -> b -> c -> d) -> Matrix a -> Matrix b -> Matrix c -> Matrix d
elementwise3 f ma mb mc = elementwise (\c (a,b) -> f a b c) mc
                        $ elementwise (\a b -> (a,b)) ma mb
  
-- Substituir valores numa matriz a partir de listas
substML :: [a] -> [(Int,Int)] -> Matrix a -> Matrix a
substML []     _      m = m
substML _      []     m = m
substML (v:vs) (p:ps) m = substML vs ps (worker v p m)
  where worker x (i,j) e | i < 1 || i > nrows e || j < 1 || j > nrows e = e
                         | otherwise = setElem x (i,j) e

-- Vizinhaca de uma posicao (inclusive)
vizinhanca :: Posicao -> [Posicao]
vizinhanca (x,y) = [ (x-1,y-1) , (x  ,y-1) , (x+1,y-1)
                   , (x-1,y  ) , (x  ,y  ) , (x+1,y  )
                   , (x-1,y+1) , (x  ,y+1) , (x+1,y+1) ]
                   
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

-- Converte indices das matrizes para pixels
ind2pix :: Float -> Float -> (Int, Int) -> (Float, Float)
ind2pix janela bloco (i,j) = (x0 + xp, y0 + yp)
  where x0 = -janela / 2
        y0 =  janela / 2
        xp =  (fromIntegral j) * bloco - (bloco / 2)
        yp = -(fromIntegral i) * bloco + (bloco / 2)
        
-- Converte pixels para indice das matrizes (c: largura da casa, b: borda da casa)
pix2pos :: Int -> (Float, Float) -> (Float,Float) -> Maybe Posicao
pix2pos t (c,b) (x,y)
    | (abs x) > lmax || (abs y) > lmax                                = Nothing
    | (x' `mod` c') < b' `div` 2 || (x' `mod` c') > (b' `div` 2) + c' = Nothing
    | (y' `mod` c') < b' `div` 2 || (y' `mod` c') > (b' `div` 2) + c' = Nothing
    | otherwise = return $ (1 + y' `div` c' , 1 + x' `div` c')
        where lmax = c * (fromIntegral t) / 2
              (c',b') = (round c, round b)
              (x',y') = (round $ lmax + x, round $ abs $ y - lmax) :: (Int, Int)


