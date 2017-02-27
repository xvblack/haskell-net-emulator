{-# LANGUAGE FlexibleContexts #-}
module Echo (echo) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Control.Monad.Free as F
-- import Control.Concurrent.TMVar
import Control.Monad (forM, forever)
import Debug.Trace
import Data.Serialize
import GHC.Generics
import Control.Concurrent.MonadIO
import Control.Concurrent.STM.MonadIO hiding (modifyTMVar)
import Network
import Emulator
import Program
import Wire

echo :: (SerializableTo MessageUnion w, Show a) => NetProgram a w ()
echo = do
  peers <- getPeers
  self <- getSelf
  forM peers $ \peer -> do
    send peer $ SimpleMessage $ "A" ++ show self
  forever $ do
    receive >>= \m -> case m of
      Just (peer, (SimpleMessage msg)) -> do
        send peer $ SimpleMessage $ "Echo " ++ msg
      otherwise -> return ()