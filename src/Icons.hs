module Icons where

import Data.String(IsString)

class IsString a => Icon a where
    iconName :: a -> String
