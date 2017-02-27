{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Network (
    NetworkM
    ) where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.Maybe
import Emulator

class NetworkM m a where
    canReach :: a -> a -> m Bool

-- data NetworkStatus a = NetworkStatus {
--     disconnected :: M.Map (a, a) Bool
-- }

-- instance (Monad m, NetworkM m a, MonadTrans t) => NetworkM (t m) a where
--     canReach addr1 addr2 = lift $ canReach addr1 addr2

instance (Ord a, Monad m, MonadState (EmulatorState w) m) => NetworkM m a where
    canReach addr1 addr2 = return True

    -- canReach addr1 addr2 = do
    --     status <- get
    --     return $ not $ isJust $ M.lookup (addr1, addr2) $ disconnected status
