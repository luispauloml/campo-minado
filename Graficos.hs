module Graficos where

import Text.Printf 
import Text.Read 
import Control.Monad
import Data.Char (ord, chr)
import Data.List (intercalate)
import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Console.ANSI (clearScreen)

import Basico
import Tipos
import Dinamica


testeMundo :: Campo
testeMundo = descobrirTudo $ descobrirMapa (5,6) $ marcarCasa (10,8) $ descobrirMapa (1,1) teste4

-- tamanho do bloco e borda interna
tamBloco = (25,2) :: (Float, Float)

renderCampo :: Int -> Campo -> IO Picture
renderCampo t m = 
  let cs = toList m
      ds = toList $ descobrirTudo m
      ps = [(i,j) | i <- [1..t], j <- [1..t]]
      gO = gameOver m
  in  case gO of Continua -> return $ pictures $ zipWith (casa2bloco t gO) cs ps
                 _        -> return $ pictures $ zipWith (casa2bloco t gO) ds ps

  
             
casa2bloco :: Int -> GameOver -> Casa -> (Int,Int) -> Picture
casa2bloco t g (s,b,p) (i,j)
    | s == Descoberto = if p == 0 
                        then translate' pos $ color (greyN 0.5) $ rectangleWire l l
                        else if b && (g == Derrota)
                             then pictures [ translate' pos $ color red $
                                             rectangleSolid l l
                                           , translateT pos $ scale e e $
                                             text "@" ]
                             else if b && (g == Vitoria)
                                  then pictures [ translate' pos $ color violet $
                                                  rectangleSolid l l
                                                , translateT pos $ scale e e $
                                                  text "@" ]
                                  else pictures [ translateT pos $ scale e e $
                                                  text (show p)
                                                , translate' pos $ 
                                                  color (greyN 0.5) $ 
                                                  rectangleWire l l ]
    | s == Marcado    = translate' pos $ color violet      $ rectangleSolid l l
    | s == Coberto    = translate' pos $ color (greyN 0.5) $ rectangleSolid l l
      where pos = ind2pix ((fst tamBloco) * fromIntegral t) (fst tamBloco) (i,j)
            translate' (a,b) = translate a b
            translateT (a,b) = translate (a - 35 * e) (b - 50 * e)
            l = (fst tamBloco) - (snd tamBloco) 
            e = ((fst tamBloco) - 3) / 200













---- ## MODO TEXTO ## ----------------------------------------------------------------
-- Renderiza o campo no modo texto
showCampo :: Campo -> String
showCampo campo = (++) ( intercalate "\n"
                       $ ("   " ++ ['A'..(chr $ (ncols campo) + ord 'A' - 1)]) : worker 
                       ) "\n"
  where  worker = zipWith (++) (map showNum [1..(nrows campo)]) $ converter campo
         showNum x = if x < 10 then " " ++ (show x) ++ " " else (show x) ++ " "
         converter m = map extrairEstado $ toLists m
         extrairEstado []     = []
         extrairEstado ((s,b,p):ps)
             | s == Marcado                 = '#'               : extrairEstado ps
             | s == Coberto                 = '-'               : extrairEstado ps
             | s == Descoberto && b /= True 
                               && p == 0    = ' '               : extrairEstado ps
             | s == Descoberto && b /= True = (head . show $ p) : extrairEstado ps
             | s == Descoberto && b == True = '*'               : extrairEstado ps

                               
                               
        
                  
                    
    
                              
                              
                              

       
