{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StarIsType #-}
module Record.ToMap (
    toMap,
    toMap',
    GToMap,
) where

import qualified Data.Map as Map
import Data.Text (Text, pack)
import GHC.Generics
import GHC.Base (Constraint, liftA2, Type)
import Data.Void (absurd)

toMap ::
    forall c v a.
    (Generic a, GToMap c v (Rep a)) =>
    (forall r. c r => r -> v) ->
    a ->
    Map.Map Text v
toMap conv a = either absurd id $ toMap' @c (pure <$> conv) a

toMap' ::
    forall c v a e.
    (Generic a, GToMap c v (Rep a)) =>
    (forall r. c r => r -> Either e v) ->
    a ->
    Either e (Map.Map Text v)
toMap' conv a = gToMap @c conv (from a)

class GToMap (c :: Type -> Constraint) v rep where
    gToMap ::
        (forall r. c r => r -> Either e v) ->
        rep x ->
        Either e (Map.Map Text v)

instance (Selector s, c a) => GToMap c v (M1 S s (K1 _i a)) where
    gToMap conv a =
        let name = pack (selName (SelectorProxy @s))
            valueE = conv $ unK1 $ unM1 a
         in Map.singleton name <$> valueE

data SelectorProxy s (f :: Type -> Type) a = SelectorProxy

instance GToMap c v f => GToMap c v (M1 D _i f) where
    gToMap conv a = gToMap @c conv (unM1 a)

instance GToMap c v f => GToMap c v (M1 C _i f) where
    gToMap conv a = gToMap @c conv (unM1 a)

instance (GToMap c v f, GToMap c v g) => GToMap c v (f :*: g) where
    gToMap conv (f :*: g) = liftA2 Map.union (gToMap @c conv f) (gToMap @c conv g)

instance GToMap c v U1 where
    gToMap _ _ = pure Map.empty
