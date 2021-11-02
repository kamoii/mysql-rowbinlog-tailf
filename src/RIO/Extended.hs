{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module RIO.Extended where

import Control.Exception (ErrorCall (ErrorCall))
import RIO
import Prelude ()

throwE :: Show e => Either e a -> IO a
throwE (Left e) = throwIO $ ErrorCall (show e)
throwE (Right a) = pure a

putUtf8Builder :: MonadIO m => Utf8Builder -> m ()
putUtf8Builder = hPutBuilder stdout . getUtf8Builder

putUtf8BuilderLn :: MonadIO m => Utf8Builder -> m ()
putUtf8BuilderLn = putUtf8Builder . (<> "\n")

putTextLn :: MonadIO m => Text -> m ()
putTextLn = putUtf8Builder . display . (<> "\n")

whileJust_ :: Monad m => m (Maybe a) -> (a -> m c) -> m ()
whileJust_ v f =
    v >>= \case
        Nothing -> pure ()
        Just a -> f a *> whileJust_ v f
