{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module InlineTypes where
import Language.C.Inline.Context
import Language.C.Inline.HaskellIdentifier

import Data.Either (fromRight)
import Language.C.Types (TypeSpecifier(TypeName))
import Language.C.Types.Parse (cIdentifierFromString,)

import qualified Data.Map as Map

data StdVector a
data StdString
data LtAlert


