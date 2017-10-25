module Entrada where

import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.List.Split (endBy)
import Control.Monad
import Data.Char (ord, chr)
import Data.Tuple (swap)

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
    getChar >>= (\_ -> return ())

-- Mensage de instrução para jogar no modo texto
usoEntrada :: IO ()
usoEntrada = do
    printf $ "Para entar o movimento deve-se digitar separados por espaço:\n" ++
             "  1) Letra da coluna;\n" ++
             "  2) Número da linha;\n" ++
             "  3) A marca \'#\' se quiser marcar a posição (opcional).\n" ++
             "  Obs.: Para abortar, entre \'q\'."

-- Parser para entrada de texto
parseEntrada :: String -> Maybe (Posicao, Estado)
parseEntrada s = if s == "q" || s == "Q"
                 then error "Jogo interrompido."
                 else entrada2move $ filter (not . null) $ endBy " " s

-- Converte string de entrada
entrada2move :: [String] -> Maybe (Posicao, Estado)
entrada2move s = let c     = length s
                     ncol  = return $ ord (head . head $ s) - ord 'A' + 1
                     nlin  = readMaybe (s !! 1) :: Maybe Int
                     marc  = readMaybe (s !! 2) :: Maybe Estado
                 in case c of 2 -> liftM2 (,) (liftM2 (,) nlin ncol) $ Just Descoberto
                              3 -> liftM2 (,) (liftM2 (,) nlin ncol) $ marc
                              _ -> Nothing

-- Valida a jogada de acordo com o tamanho do campo
validarMove :: Int -> Maybe (Posicao, Estado) -> Maybe (Posicao, Estado)
validarMove t m = m >>= (\((l,c),s) -> if l < 1 || l > t || c < 1 || c > t
                                       then Nothing
                                       else return ((l,c),s) )



  
  

                               
                               
        
                  
                    
    
                              
                              
                              

       
