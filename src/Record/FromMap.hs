{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Record.FromMap (
    fromMap,
    GFromMap,
    FromMapError (..),
) where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Text (Text, pack)
import GHC.Base (Constraint, Type)
import GHC.Generics

data FromMapError e
    = ConvertError
        { fieldName :: Text
        , error :: e
        }
    | NoKeyError
        { fieldName :: Text
        }
    deriving (Show)

fromMap ::
    forall c v a e.
    (Generic a, GFromMap c v (Rep a)) =>
    (forall r. c r => v -> Either e r) ->
    Map.Map Text v ->
    Either (FromMapError e) a
fromMap conv m = to <$> gFromMap @c conv m

class GFromMap (c :: Type -> Constraint) v rep where
    gFromMap ::
        (forall r. c r => v -> Either e r) ->
        Map.Map Text v ->
        Either (FromMapError e) (rep a)

instance (Selector s, c a) => GFromMap c v (M1 S s (K1 _i a)) where
    gFromMap conv mmap =
        let name = pack (selName (SelectorProxy @s)) :: Text
            value = do
                v <- Map.lookup name mmap & note (NoKeyError name)
                first (ConvertError name) $ conv v
         in M1 <$> (K1 <$> value)

note :: e -> Maybe v -> Either e v
note e Nothing = Left e
note _ (Just v) = Right v

data SelectorProxy s (f :: Type -> Type) a = SelectorProxy

instance GFromMap c v f => GFromMap c v (M1 D _i f) where
    gFromMap conv mmap = M1 <$> (gFromMap @c conv mmap)

instance GFromMap c v f => GFromMap c v (M1 C _i f) where
    gFromMap conv mmap = M1 <$> gFromMap @c conv mmap

instance (GFromMap c v f, GFromMap c v g) => GFromMap c v (f :*: g) where
    gFromMap conv mmap = (:*:) <$> gFromMap @c conv mmap <*> gFromMap @c conv mmap

instance GFromMap c v U1 where
    gFromMap _ _ = return U1
