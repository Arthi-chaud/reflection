{-# LANGUAGE LambdaCase #-}

module Data.Reflection.Internal (varInfoToType, typeInfoToType) where

import Data.List (singleton)
import qualified Data.Reflection.Type as Refl
import Language.Haskell.TH

varInfoToType :: Info -> Q Refl.Type
varInfoToType = \case
    (VarI _ ty _) -> thTypeToType ty
    _ -> fail "Expected a variable name"

typeInfoToType :: Info -> Q Refl.Type
typeInfoToType = \case
    (TyConI dec) -> decToType dec
    (TyVarI n _) -> return $ Refl.TypeVariable n
    (PrimTyConI n _ _) -> return $ Refl.Prim n
    _ -> fail "Expected a type constructor name"

thTypeToType :: Type -> Q Refl.Type
thTypeToType = \case
    (ConT ty) -> reify ty >>= typeInfoToType
    (AppT (ConT ty) _) -> reify ty >>= typeInfoToType
    (AppT ListT ty) -> return $ Refl.List ty
    (AppT ty _) -> thTypeToType ty
    (VarT n) -> return $ Refl.TypeVariable n
    ty -> fail $ "Don't know how to handle this type: " ++ show ty

decToType :: Dec -> Q Refl.Type
decToType = \case
    (NewtypeD _ n _ _ con _) -> Refl.ADT n . singleton <$> thConToConstructor con
    (DataD _ n _ _ cons _) -> Refl.ADT n <$> mapM thConToConstructor cons
    (TySynD _ _ ty) -> thTypeToType ty
    f -> fail $ "Expected a data/newtype declaration: " ++ show f

thConToConstructor :: (MonadFail m) => Con -> m Refl.Constructor
thConToConstructor = \case
    (NormalC n bts) -> return $ Refl.SimpleCons n (snd <$> bts)
    (RecC n vbt) -> return $ Refl.RecordCons n $ (\(fName, _, ty) -> (fName, ty)) <$> vbt
    (InfixC (_, t1) n (_, t2)) -> return $ Refl.SimpleCons n [t1, t2]
    (ForallC _ _ con) -> thConToConstructor con
    con -> fail $ "Don't know how to handle this constructor: " ++ show con
