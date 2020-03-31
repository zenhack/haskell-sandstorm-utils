{-# LANGUAGE DuplicateRecordFields #-}
module Sandstorm
    ( RawApp(..)
    , runRawApp
    ) where

import Zhp

import Data.Default (def)

import Control.Concurrent (threadDelay)

import qualified Capnp.Gen.Sandstorm.Grain.Pure as Grain
import qualified Capnp.Rpc                      as Rpc
import qualified Network.Socket                 as Network

type RawApp = Rpc.Supervisor -> Grain.SandstormApi -> IO Grain.MainView

runRawApp :: RawApp -> IO ()
runRawApp rawApp = do
    let fd = 3
    Network.setNonBlockIfNeeded fd
    socket <- Network.mkSocket fd
    (mainView, setMainView) <- Rpc.newPromiseClient
    -- We trust the sandstorm supervisor, so set maxBound for our message size
    -- limit:
    let transport = Rpc.socketTransport socket maxBound
    Rpc.handleConn transport def
        { Rpc.withBootstrap = Just $ \sup client -> do
            app <- rawApp sup (Rpc.fromClient client)
            Rpc.fulfill setMainView (Rpc.toClient app)
            forever $ threadDelay 1000000
        , Rpc.getBootstrap = \_sup ->
            pure $ Just mainView
        }
