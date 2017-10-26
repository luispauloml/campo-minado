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
import Text.Printf (printf)
import Text.Read (readMaybe)

import Basico
import Tipos
import Dinamica
import Graficos

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
             "  Obs.: Para abortar, entre \'q\'.\n"
             
             
-- Valida a jogada de acordo com o tamanho do campo
validarMove :: Int -> Maybe (Posicao, Estado) -> Maybe (Posicao, Estado)
validarMove t m = m >>= (\((l,c),s) -> if l < 1 || l > t || c < 1 || c > t
                                       then Nothing
                                       else return ((l,c),s) )

                                       
---- ## MODO GRÁFICO ## --------------------------------------------------------------
-- Capturar clique (t: tamanho do campo)
capturaClique :: Int -> Event -> Campo -> IO Campo
capturaClique t (EventKey k s m (x,y)) c
   | gameOver c == Derrota || gameOver c == Vitoria 
        = return c
   | k == MouseButton LeftButton && s == Down 
        = return $ maybe c (flip descobrirMapa c) $ pix2pos t tamBloco (x,y)
   | k == MouseButton RightButton && s == Down
        = return $ maybe c (flip marcarCasa c)    $ pix2pos t tamBloco (x,y)
   | otherwise 
        = return c
capturaClique _ _ c = return c

---- ## MODO TEXTO ## ----------------------------------------------------------------
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

-- Loop do jogo
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

