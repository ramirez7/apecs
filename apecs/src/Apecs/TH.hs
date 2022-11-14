{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Apecs.TH
  ( makeWorld
  , makeWorldFixed
  , makeWorldNoEC
  , makeWorldFixedNoEC
  , makeWorldAndComponents
  , makeWorldAndComponentsFixed
  , makeMapComponents
  , makeMapComponentsFor
  ) where

import           Control.Monad
import           Language.Haskell.TH

import           Apecs.Core
import           Apecs.Stores
import           Apecs.Util          (EntityCounter)

genName :: String -> Q Name
genName s = mkName . show <$> newName s

-- | Same as 'makeWorld', but does not include an 'EntityCounter'
--   You don't typically want to use this, but it's exposed in case you know what you're doing.
makeWorldNoEC :: String -> [Name] -> Q [Dec]
makeWorldNoEC worldName cTypes = do
  cTypesNames <- forM cTypes $ \t -> do
    rec <- genName "rec"
    return (ConT t, rec)

  let wld = mkName worldName
      has = mkName "Has"
      sys = mkName "SystemT"
      m = VarT $ mkName "m"
      wldDecl = DataD [] wld [] Nothing [RecC wld records] []

      makeRecord (t,n) = (n, Bang NoSourceUnpackedness SourceStrict, ConT (mkName "Storage") `AppT` t)
      records = makeRecord <$> cTypesNames

      makeInstance (t,n) =
        InstanceD Nothing [ConT (mkName "Monad") `AppT` m] (ConT has `AppT` ConT wld `AppT` m `AppT` t)
          [ FunD (mkName "getStore") [Clause []
              (NormalB$ ConE sys `AppE` (VarE (mkName "asks") `AppE` VarE n))
            [] ]
          ]

      initWorldName = mkName $ "init" ++ worldName
      initSig = SigD initWorldName (AppT (ConT (mkName "IO")) (ConT wld))
      initDecl = FunD initWorldName [Clause []
        (NormalB$ iterate (\wE -> AppE (AppE (VarE $ mkName "<*>") wE) (VarE $ mkName "explInit")) (AppE (VarE $ mkName "return") (ConE wld)) !! length records)
        [] ]

      hasDecl = makeInstance <$> cTypesNames

  return $ wldDecl : initSig : initDecl : hasDecl

makeWorldFixedNoEC :: String -> [Name] -> Q [Dec]
makeWorldFixedNoEC worldName cTypes = do
  cTypesNamesKinds <- forM cTypes $ \t -> do
    rec <- genName "rec"
    k <- reifyType t
    return (ConT t, k, rec)

  let wld = mkName worldName
      has = mkName "Has"
      sys = mkName "SystemT"
      m = VarT $ mkName "m"
      wldDecl = DataD [] wld [] Nothing [RecC wld records] []

      makeRecord (t, k, n) = case k of
        StarT -> (n, Bang NoSourceUnpackedness SourceStrict, ConT (mkName "Storage") `AppT` t)
        AppT (AppT ArrowT StarT) StarT -> (n, Bang NoSourceUnpackedness SourceStrict, ConT (mkName "Storage") `AppT` (AppT t (ConT wld)))
        _ -> error $ unwords ["Invalid component type for:", pprint t, "::", pprint k]
      records = makeRecord <$> cTypesNamesKinds

      makeInstance (t,k,n) = case k of
        StarT ->
          InstanceD Nothing [ConT (mkName "Monad") `AppT` m] (ConT has `AppT` ConT wld `AppT` m `AppT` t)
            [ FunD (mkName "getStore") [Clause []
                (NormalB$ ConE sys `AppE` (VarE (mkName "asks") `AppE` VarE n))
              [] ]
            ]
        AppT (AppT ArrowT StarT) StarT ->
          InstanceD Nothing [ConT (mkName "Monad") `AppT` m] (ConT has `AppT` ConT wld `AppT` m `AppT` (AppT t (ConT wld)))
            [ FunD (mkName "getStore") [Clause []
                (NormalB$ ConE sys `AppE` (VarE (mkName "asks") `AppE` VarE n))
              [] ]
            ]
        _ -> error $ unwords ["Invalid component type for:", pprint t, "::", pprint k]
      initWorldName = mkName $ "init" ++ worldName
      initSig = SigD initWorldName (AppT (ConT (mkName "IO")) (ConT wld))
      initDecl = FunD initWorldName [Clause []
        (NormalB$ iterate (\wE -> AppE (AppE (VarE $ mkName "<*>") wE) (VarE $ mkName "explInit")) (AppE (VarE $ mkName "return") (ConE wld)) !! length records)
        [] ]

      hasDecl = makeInstance <$> cTypesNamesKinds

  return $ wldDecl : initSig : initDecl : hasDecl

-- | Creates 'Component' instances with 'Map' stores
makeMapComponents :: [Name] -> Q [Dec]
makeMapComponents = mapM makeMapComponent

makeMapComponent :: Name -> Q Dec
makeMapComponent = makeMapComponentFor ''Map

-- | Allows customization of the store to be used. For example, the base 'Map' or an STM Map.
makeMapComponentFor :: Name -> Name -> Q Dec
makeMapComponentFor store comp = do
  let ct = pure $ ConT comp
      st = pure $ ConT store
      a = VarT $ mkName "a"
  k <- reifyType comp
  case k of
    StarT -> head <$> [d| instance Component $ct where type Storage $ct = $st $ct |]
    AppT (AppT ArrowT StarT) StarT ->
      let ctf = (`AppT` a) <$> ct
      in head <$> [d| instance Component $ctf where type Storage $ctf = $st $ctf |]
    _ -> error "laksjdgflaj"

makeMapComponentsFor :: Name -> [Name] -> Q [Dec]
makeMapComponentsFor store = mapM (makeMapComponentFor store)

-- | Calls 'makeWorld' and 'makeMapComponents', i.e. makes a world and also defines 'Component' instances with a 'Map' store.
makeWorldAndComponents :: String -> [Name] -> Q [Dec]
makeWorldAndComponents worldName cTypes = do
  wdecls <- makeWorld worldName cTypes
  cdecls <- makeMapComponents cTypes
  return $ wdecls ++ cdecls

makeWorldAndComponentsFixed :: String -> [Name] -> Q [Dec]
makeWorldAndComponentsFixed worldName cTypes = do
  wdecls <- makeWorldFixed worldName cTypes
  cdecls <- makeMapComponents cTypes
  return $ wdecls ++ cdecls

{-|

The typical way to create a @world@ record, associated 'Has' instances, and initialization function.

> makeWorld "MyWorld" [''Component1, ''Component2, ...]

turns into

> data MyWorld = MyWorld Component1 Component2 ... EntityCounter
> instance MyWorld `Has` Component1 where ...
> instance MyWorld `Has` Component2 where ...
> ...
> instance MyWorld `Has` EntityCounter where ...
>
> initMyWorld :: IO MyWorld
> initMyWorld = MyWorld <$> initStore <*> initStore <*> ... <*> initStore

-}
makeWorld :: String -> [Name] -> Q [Dec]
makeWorld worldName cTypes = makeWorldNoEC worldName (cTypes ++ [''EntityCounter])

makeWorldFixed :: String -> [Name] -> Q [Dec]
makeWorldFixed worldName cTypes = makeWorldFixedNoEC worldName (cTypes ++ [''EntityCounter])
