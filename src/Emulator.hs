{-# LANGUAGE DeriveFunctor, DeriveGeneric, DefaultSignatures, AllowAmbiguousTypes #-}
module Emulator where

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
import Wire

data MessageInQueue w = MessageInQueue {
  sender :: Int,
  message :: w
}

data EmulatorState w = EmulatorState {messages :: M.Map Int (S.Seq (Int, w))} deriving (Show)

emptyEmulatorState addrs = EmulatorState {
  messages = M.fromList $ zip addrs $ repeat S.empty
}

data EmulatorProgram t w = EmulatorProgram {
    addrs :: [Int],
    init :: t,
    step :: EmulatorState w -> t -> (t, EmulatorState w)
}