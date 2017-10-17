module Tipos where

import Data.Matrix

-- par ordenado
type Posicao = (Int, Int) 

-- estado geral do jogo
data GameOver = Continua  -- jogo continua
              | Derrota   -- sinaliza derrota no jogo
              | Vitoria   -- sinaliza vitória no jogo
              deriving (Eq,Show)
            
-- informações sobre a casa
type Casa = (Estado, Bool, Int)

-- estado do campo minado
type Campo = Matrix Casa

-- estado de um casa do campo
data Estado = Marcado       -- casa foi sinalizada com possível bomba
            | Coberto       -- casa ainda coberta
            | Descoberto    -- casa descoberta
            deriving (Eq)
            
instance Show Estado where
    show Marcado    = "M"
    show Coberto    = "C"
    show Descoberto = "D"
    