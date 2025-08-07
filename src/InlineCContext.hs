{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module InlineCContext where
import Data.Typeable (Typeable)
import Language.C.Types (TypeSpecifier(TypeName))
import Language.C.Types.Parse (cIdentifierFromString,)
import Language.C.Inline.Context
import Language.C.Inline.HaskellIdentifier
import Data.Either (fromRight)
import Language.Haskell.TH.Syntax (Q, Type, runQ, mkName, lift)
import Language.Haskell.TH (reify, lookupValueName, location)

import qualified Data.Map as Map
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

import Debug.Trace

data StdPair a b

instance Show (Q Type) where
  show x = "Some Quasi Type"

pairFirstAntiQuoter :: AntiQuoter HaskellIdentifier
pairFirstAntiQuoter = AntiQuoter
  { aqParser = do
      hId <- C.parseIdentifier
      useCpp <- C.parseEnableCpp
      let hName = mkName $ unHaskellIdentifier hId
          cId = mangleHaskellIdentifier useCpp hId
          hType = fromRight (error "Some issue") $ cIdentifierFromString useCpp (unHaskellIdentifier hId)
      traceShowM (hId, hName, hType, useCpp, hName)
      return (cId, C.TypeSpecifier mempty $ C.Template hType [C.Void, C.Void], hId)
  , aqMarshaller = \_purity _cTypes cTy hId -> do
      traceShowM (_purity, _cTypes, cTy, hId)
      hName <- lookupValueName $ unHaskellIdentifier hId
      hName' <- case hName of
                  Nothing -> fail $ "Cannot capture Haskell variable " ++ unHaskellIdentifier hId ++ ", because it's not in scope for pairCtx"
                  Just hsName -> return hsName
      let hType = TH.AppE (TH.VarE 'reify) (TH.VarE hName')

      error "Marshaller not defined"
  }

pairCtx :: Context
pairCtx = mempty
  { ctxAntiQuoters = Map.fromList
      [ ("pair-first", SomeAntiQuoter pairFirstAntiQuoter) ]
  , ctxTypesTable = Map.fromList [ (TypeName $ fromRight (error "No std::pair clib found") $ cIdentifierFromString True "std::pair", [t| StdPair |]) ]
  }

