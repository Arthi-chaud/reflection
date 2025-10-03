module Data.Reflection.Observe (observeValueType, observeType) where

import Control.Monad
import Data.Reflection.EDSL
import Data.Reflection.Internal
import Data.Reflection.Type
import Language.Haskell.TH (Exp, Name, Q)
import qualified Language.Haskell.TH as TH

-- | Observes the type of a value
observeValueType :: (EDSL expr) => Name -> (Type -> Q expr) -> Q Exp
observeValueType n f = TH.reify n >>= (varInfoToType >=> f >=> compile)

-- | Similar to 'observeValueType': allows observing a type using its 'Name'
observeType :: (EDSL expr) => Name -> (Type -> Q expr) -> Q Exp
observeType n f = TH.reify n >>= (typeInfoToType >=> f >=> compile)
