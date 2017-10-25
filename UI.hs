module UI where

import Data.Char
import Data.List
import Data.Matrix
import Data.Maybe
import System.Console.ANSI
import Text.Printf

import Tipos
import Entrada
import Dinamica

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

-- Pedir entrada do jogador
pedirEntrada :: IO (Maybe (Posicao, Estado))
pedirEntrada = do e <- getLine
                  return $ parseEntrada e
                
-- Lê jogada e atualiza o campo
atualizarCampoTexto :: Campo -> IO Campo
atualizarCampoTexto mapa =
 let t = nrows mapa 
     m = validarMove t
 in do e <- pedirEntrada
       if isNothing (m e)
       then do clearScreen
               putStrLn "Entrada inválida."
               usoEntrada
               printf $ showCampo $ mapa
               atualizarCampoTexto mapa
       else let s = acaoMapa mapa $ fromJust (m e)
            in return s
            
loopTexto :: Campo -> IO ()
loopTexto m =
  let imprimir = printf . showCampo
  in do case gameOver m of Derrota  -> do clearScreen
                                          putStrLn "Você detonou uma bomba!"
                                          imprimir $ descobrirTudo m
                           Vitoria  -> do clearScreen
                                          putStrLn "Você identificou todas as bombas!"
                                          imprimir $ descobrirTudo m
                           Continua -> do mn <- atualizarCampoTexto m
                                          clearScreen
                                          imprimir mn
                                          loopTexto mn
      

         
         
         
         