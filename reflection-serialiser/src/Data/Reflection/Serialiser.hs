module Data.Reflection.Serialiser (
    -- * Serialisable objects
    Serialisable (..),
    serialise,

    -- * Code generator
    genSerialisable,

    -- * Definition of a serialisation format
    Format,
) where

import Data.Reflection.Serialiser.Format
import Data.Reflection.Serialiser.Serialisable
import Data.Reflection.Serialiser.Serialisable.TH
