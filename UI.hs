module UI where

import Data.List
import Data.Matrix
import Data.Char

import Tipos

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

