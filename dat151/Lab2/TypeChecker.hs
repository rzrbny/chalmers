module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CPP.Abs
import CPP.Print
import CPP.ErrM

type Sig = Map Id ([Type],Type)

typecheck :: Program -> Err ()
typecheck (PDefs defs) = do return ()

buildBuiltin :: Err Sig
buildBuiltin = undefined

buildTable :: [Def] -> Sig
buildTable []                     = Map.empty
buildTable ((DFun t id as ss):ds) = Map.insert id (getTypes as,t)
                                    $ buildTable ds
  where getTypes []               = [] :: [Type]
        getTypes ((ADecl t _):ds) = t:getTypes ds 
