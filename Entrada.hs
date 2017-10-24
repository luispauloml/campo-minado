module Entrada where

import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.List.Split (endBy)
import Control.Monad
import Data.Char (ord, chr)

import Tipos

-- Mensagem de uso do campo-minado
usoMain :: IO ()
usoMain = do 
    printf $ "campo-minado exige três argumentos:\n" ++
             "  1) Quantidade de bombas;\n" ++
             "  2) Tamanho do campo;\n" ++
             "  3) Modo texto ou gráfico: \"t\" ou \"g\".\n" ++
             "  Obs: no modo texto, recomendável menor que 26 e no máximo 57.\n" ++
             "<<Se modo texto por escolhido>>\n"
    usoEntrada

-- Mensage de instrução para jogar no modo texto
usoEntrada :: IO ()
usoEntrada = do
    printf $ "Para entar o movimento deve-se digitar separados por espaço:\n" ++
             "  1) Letra da coluna;\n" ++
             "  2) Número da linha;\n" ++
             "  3) O marca # se quiser marcar a posição (opcional).\n"

-- Parser para entrada de texto
parseEntrada :: String -> Maybe (Posicao, Estado)
parseEntrada s = entrada2move $ filter (not . null) $ endBy " " s

-- Converte string de entrada
entrada2move :: [String] -> Maybe (Posicao, Estado)
entrada2move s = let c     = length s
                     nlin  = return $ ord (head . head $ s) - ord 'A' + 1
                     ncol  = readMaybe (s !! 1) :: Maybe Int
                     marc  = if head (s !! 2) == '#' then Just 1 else Nothing
                 in case c of 2 -> sequence [nlin,ncol,(Just 0)] >>= saida
                              3 -> sequence [nlin,ncol,marc] >>= saida
                              _ -> Nothing
                 where saida = (\ (a:b:c:xs) -> return ( (a,b)
                                                       , if c == 1 
                                                         then Marcado 
                                                         else Descoberto ) )

-- Valida a jogada de acordo com o tamanho do campo
validarMove :: Maybe (Posicao, Estado) -> Int -> Maybe (Posicao, Estado)
validarMove m t = m >>= (\((l,c),s) -> if l < 1 || l > t || c < 1 || c > t
                                       then return ((l,c),s)
                                       else Nothing )
                                       

  
  

                               
                               
        
                  
                    
    
                              
                              
                              

       
