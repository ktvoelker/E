
module ImportResolution (phase) where

import AST
import Message

data Module = Module deriving (Eq, Show) -- TODO

phase :: Namespace -> E ([Module], Namespace)
phase ns = return ([], ns) -- TODO

