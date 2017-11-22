module UI where

import Control.Monad
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.List.Split (endBy)
import Data.Matrix
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Console.ANSI (clearScreen)
import Text.Read (readMaybe)

import Tipos
import Dinamica
import Graficos

-- Mensagem de uso do campo-minado
usoMain :: IO ()
usoMain = do 
    putStrLn $ "campo-minado exige três argumentos:\n" ++
               "  1) Quantidade de bombas;\n" ++
               "  2) Tamanho do campo;\n" ++
               "  3) Modo texto ou gráfico: \"t\" ou \"g\".\n" ++
               "  Obs: no modo texto, recomendável menor que 26 e no máximo 57.\n" ++
               "<<Se modo texto por escolhido>>"
    usoEntrada
    getLine >> return ()

-- Mensage de instrução para jogar no modo texto
usoEntrada :: IO ()
usoEntrada = putStrLn $ "Para entar o movimento deve-se digitar separados por espaço:\n" ++
                        "  1) Letra da coluna;\n" ++
                        "  2) Número da linha;\n" ++
                        "  3) A marca \'#\' se quiser marcar a posição (opcional).\n" ++
                        "  Obs.: Para abortar, entre \'q\'."

                                       
---- ## MODO GRÁFICO ## --------------------------------------------------------------
-- Capturar clique (t: tamanho do campo)
capturaClique :: Int -> Event -> Campo -> IO Campo
capturaClique t (EventKey k s m (x,y)) c
   | gameOver c == Derrota || gameOver c == Vitoria 
        = return c
   | k == MouseButton LeftButton && s == Down 
        = return $ maybe c (descobrirMapa c) $ pix2pos t (x,y)
   | k == MouseButton RightButton && s == Down
        = return $ maybe c (marcarCasa c)    $ pix2pos t (x,y)
   | otherwise 
        = return c
capturaClique _ _ c = return c

-- Converte pixels para indice das matrizes (c: largura da casa, b: borda da casa)
pix2pos :: Int -> (Float,Float) -> Maybe Posicao
pix2pos t (x,y)
    | (abs x) > lmax || (abs y) > lmax                                = Nothing
    | (x' `mod` c') < b' `div` 2 || (x' `mod` c') > (b' `div` 2) + c' = Nothing
    | (y' `mod` c') < b' `div` 2 || (y' `mod` c') > (b' `div` 2) + c' = Nothing
    | otherwise = return $ (1 + y' `div` c' , 1 + x' `div` c')
        where (c,b)   = tamBloco
              lmax    = c * (fromIntegral t) / 2
              (c',b') = (round c, round b)
              (x',y') = (round $ lmax + x, round $ abs $ y - lmax) :: (Int, Int)

---- ## MODO TEXTO ## ----------------------------------------------------------------
-- Pedir entrada do jogador
pedirEntrada :: IO (Maybe (Posicao, Estado))
pedirEntrada = parseEntrada `fmap` getLine
  where parseEntrada s = if s == "q" || s == "Q"
                         then error "Jogo interrompido."
                         else entrada2move $ filter (not . null) $ endBy " " s 

-- Converte string de entrada
entrada2move :: [String] -> Maybe (Posicao, Estado)
entrada2move s = let c     = length s
                     ncol  = Just $ ord (head (s !! 0)) - (ord 'A') + 1
                     nlin  = readMaybe (s !! 1) :: Maybe Int
                     marc  = readMaybe (s !! 2) :: Maybe Estado
                 in case c of 2 -> liftM2 (,) (liftM2 (,) nlin ncol) $ Just Descoberto
                              3 -> liftM2 (,) (liftM2 (,) nlin ncol) $ marc
                              _ -> Nothing

-- Lê jogada e atualiza o campo
atualizarCampoTexto :: Campo -> IO Campo
atualizarCampoTexto mapa = do e <- validarMove (nrows mapa) `fmap` pedirEntrada
                              if isNothing e
                              then do clearScreen
                                      putStrLn "Entrada inválida."
                                      usoEntrada
                                      putStrLn $ showCampo mapa
                                      atualizarCampoTexto mapa
                              else let s = acaoMapa mapa $ fromJust e
                                   in  return s

-- Valida a jogada de acordo com o tamanho do campo
validarMove :: Int -> Maybe (Posicao, Estado) -> Maybe (Posicao, Estado)
validarMove t m = m >>= (\ ((l,c),s) -> if l < 1 || l > t || c < 1 || c > t
                                        then Nothing
                                        else return ((l,c),s) )

-- Loop do jogo
loopTexto :: Campo -> IO ()
loopTexto m =
  let imprimir = putStrLn . showCampo
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
