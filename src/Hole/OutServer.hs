{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Hole.OutServer
  ( OutServerEnv
  , OutServerT
  , runOutServerT
  , startOutServer
  , stopOutServerT
  , initOutServerEnv
  ) where


import           Control.Monad              (forever, unless, when)
import           Control.Monad.Cont         (callCC, runContT)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Either                (isLeft)
import           Metro.Class                (Servable (..), TransportConfig)
import           System.Log.Logger          (errorM, infoM)
import           UnliftIO

data OutServerEnv serv = OutServerEnv
  { outServer :: serv
  , outState  :: TVar Bool
  }


newtype OutServerT serv m a = OutServerT {unOutServerT :: ReaderT (OutServerEnv serv) m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (OutServerEnv serv)
    )

instance MonadTrans (OutServerT serv) where
  lift = OutServerT . lift

instance MonadUnliftIO m => MonadUnliftIO (OutServerT serv m) where
  withRunInIO inner = OutServerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runOutServerT r)

runOutServerT :: OutServerEnv serv -> OutServerT serv m a -> m a
runOutServerT sEnv = flip runReaderT sEnv . unOutServerT

initOutServerEnv
  :: (MonadIO m, Servable serv)
  => ServerConfig serv
  -> m (OutServerEnv serv)
initOutServerEnv sc = do
  outServer <- newServer sc
  outState  <- newTVarIO True
  pure OutServerEnv { .. }

serveForever
  :: (MonadUnliftIO m, Servable serv)
  => (TransportConfig (STP serv) -> m ())
  -> OutServerT serv m ()
serveForever action = do
  liftIO $ infoM "Hole.OutServer" "HoleOutServer started"
  state <- asks outState
  (`runContT` pure) $ callCC $ \exit -> forever $ do
    e <- lift $ tryServeOnce action
    when (isLeft e) $ do
      liftIO $ errorM "Hole.OutServer" $ "Error: " ++ show e
      exit ()
    alive <- readTVarIO state
    unless alive $ exit ()
  liftIO $ infoM "Hole.OutServer" "HoleOutServer closed"

tryServeOnce
  :: (MonadUnliftIO m, Servable serv)
  => (TransportConfig (STP serv) -> m ())
  -> OutServerT serv m (Either SomeException ())
tryServeOnce action = tryAny (serveOnce action)

serveOnce
  :: (MonadUnliftIO m, Servable serv)
  => (TransportConfig (STP serv) -> m ())
  -> OutServerT serv m ()
serveOnce action = do
  OutServerEnv {..} <- ask
  servOnce outServer $ \case
    Nothing       -> return ()
    Just (_, stp) -> lift $ action stp

startOutServer
  :: (MonadUnliftIO m, Servable serv)
  => OutServerEnv serv
  -> (TransportConfig (STP serv) -> m ())
  -> m ()
startOutServer sEnv action = do
  runOutServerT sEnv $ serveForever action
  liftIO $ servClose $ outServer sEnv

stopOutServerT :: (MonadIO m, Servable serv) => OutServerT serv m ()
stopOutServerT = do
  OutServerEnv {..} <- ask
  atomically $ writeTVar outState False
  liftIO $ servClose outServer
