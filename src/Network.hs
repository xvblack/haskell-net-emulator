{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Network (
    NetworkM
    ) where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.Maybe

class NetworkM m a where
    canReach :: a -> a -> m Bool

data NetworkStatus a = NetworkStatus {
    disconnected :: M.Map (a, a) Bool
}

instance (Ord a, Monad m) => NetworkM (StateT (NetworkStatus a) m) a where
    canReach addr1 addr2 = do
        status <- get
        return $ not $ isJust $ M.lookup (addr1, addr2) $ disconnected status
