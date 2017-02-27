{-# LANGUAGE DeriveFunctor, DeriveGeneric, DefaultSignatures, AllowAmbiguousTypes, MultiParamTypeClasses, FlexibleInstances #-}
module Wire where

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

-- class (Show a) => Message a where
--   serialize :: a -> B.ByteString
--   deserialize :: B.ByteString -> a 

-- data AnyMessage = forall a. (Serialize a) => AnyMessage {unMessage :: a}
-- instance Show AnyMessage where
--   show (AnyMessage m) = show m

-- instance Message String where
--   serialize = B.pack
--   deserialize = B.unpack

class SerializableTo a b where
  serialize :: a -> b
  deserialize :: b -> a

instance SerializableTo a a where
  serialize = id
  deserialize = id

instance Serialize a => SerializableTo a B.ByteString where
  serialize = runPut . put
  deserialize = fromRight . runGet get where
    fromRight (Right r) = r
    fromRight (Left l) = error "Either is not Right"

data MessageUnion = SimpleMessage {unSimpleMessage :: String} deriving (Show, Generic)
instance Serialize MessageUnion 
-- instance Message MessageUnion where
--   serialize (SimpleMessage m) = B.pack m
--   deserialize bs = SimpleMessage $ B.unpack bs