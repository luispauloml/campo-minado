module Graficos where

import Data.Char (ord, chr)
import Data.List (intercalate)
import Data.Matrix
import Graphics.Gloss
import System.Console.ANSI (clearScreen)

import Tipos
import Dinamica

---- ## MODO GRÁFICO ## -------------------------------------------------------
-- tamanho do bloco e borda interna (para modo gráfico)
tamBloco = (25,2) :: (Float, Float)

-- Renderiza o campo completamente
renderCampo :: Int -> Campo -> IO Picture
renderCampo t m = 
  let cs = toList m                           -- todas as casas
      ds = toList $ descobrirTudo m           -- todas as casas descobertas (em caso de derrota)
      ps = [(i,j) | i <- [1..t], j <- [1..t]] -- posições
      gO = gameOver m                         -- verifica se o jogo continua
  in case gO of Continua -> return $ pictures $ zipWith (casa2bloco t gO) cs ps
                _        -> return $ pictures $ zipWith (casa2bloco t gO) ds ps

-- Converte uma casa em uma figura do Gloss (t: tamanho do campo; g: GameOver; (i,j): índice)
casa2bloco :: Int -> GameOver -> Casa -> (Int,Int) -> Picture
casa2bloco t g (s,b,p) (i,j) =
  let -- converter de (i,j) para pixels
      pos = ind2pix ((fst tamBloco) * fromIntegral t) (fst tamBloco) (i,j)
      -- adaptação da função translate
      translate' (a,b) = translate a b
      -- adaptação da função traslate posicionar texto de potuação (empirico)
      translateT (a,b) = translate (a - 35 * e) (b - 50 * e)
      -- lado do quadrado que forma uma casa
      l = (fst tamBloco) - (snd tamBloco)
      -- reduzir escala para posicionar os textos de pontuação (empirico)
      e = ((fst tamBloco) - 3) / 200
  in case s of Marcado    -> translate' pos $ color violet      $ rectangleSolid l l
               Coberto    -> translate' pos $ color (greyN 0.5) $ rectangleSolid l l
               Descoberto -> if p == 0 -- agir de acordo com a pontuação da casa
                              then translate' pos $ color (greyN 0.5) $ rectangleWire l l
                              else if b && (g == Derrota)
                                   then pictures [ translate' pos $ color red $
                                                   rectangleSolid l l
                                                 , translateT pos $ scale e e $
                                                   color white $ text "@" ]
                                   else if b && (g == Vitoria)
                                        then pictures [ translate' pos $ color violet $
                                                        rectangleSolid l l
                                                      , translateT pos $ scale e e $
                                                        color white $ text "@" ]
                                        else pictures [ translateT pos $ scale e e $
                                                       text (show p)
                                                      , translate' pos $ 
                                                        color (greyN 0.5) $ 
                                                        rectangleWire l l ]

-- Converte indices das matrizes para pixels
ind2pix :: Float -> Float -> (Int, Int) -> (Float, Float)
ind2pix janela bloco (i,j) = (x0 + xp, y0 + yp)
  where x0 = -janela / 2
        y0 =  janela / 2
        xp =  (fromIntegral j) * bloco - (bloco / 2)
        yp = -(fromIntegral i) * bloco + (bloco / 2)

---- ## MODO TEXTO ## ---------------------------------------------------------
-- Renderiza o campo no modo texto
showCampo :: Campo -> String
showCampo campo = intercalate "\n" 
                $ ("   " ++ ['A'..(chr $ (ncols campo) + ord 'A' - 1)]) : worker 
  where  worker = zipWith (++) (map showNum [1..(nrows campo)]) $ converter campo
         showNum x = if x < 10 then " " ++ (show x) ++ " " else (show x) ++ " "
         converter m = map extrairEstado $ toLists m
         extrairEstado []     = []
         extrairEstado ((s,b,p):ps)
             | s == Marcado              = '#'               : extrairEstado ps
             | s == Coberto              = '-'               : extrairEstado ps
             | s == Descoberto && b      = '@'  {-bomba-}    : extrairEstado ps
             | s == Descoberto && p == 0 = ' '               : extrairEstado ps
             | otherwise                 = (head . show $ p) : extrairEstado ps
