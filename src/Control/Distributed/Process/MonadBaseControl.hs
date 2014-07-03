{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module only exports instances for 'MonadBase' 'IO' and
-- 'MonadBaseControl' 'IO' for the 'Process' monad. This is for use
-- in conjunction with a library requiring these instances, such as the
-- <http://hackage.haskell.org/package/lifted-base lifted-base> package.
--
-- example usage:
--
-- >import Control.Distributed.Process.MonadBaseControl()
-- >import Control.Concurrent.MVar.Lifted (withMVar)
--
-- >processWithMVar :: MVar a -> (a -> Process b) -> Process b
-- >processWithMvar = withMVar
module Control.Distributed.Process.MonadBaseControl
  (
  ) where

import Control.Distributed.Process.Internal.Types
  ( Process(..)
  , LocalProcess
  )


import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Reader (ReaderT)

deriving instance MonadBase IO Process

instance MonadBaseControl IO Process where
  newtype StM Process a = StProcess {_unSTProcess :: StM (ReaderT LocalProcess IO) a}
  restoreM (StProcess m) = Process $ restoreM m
  liftBaseWith f = Process $ liftBaseWith $ \ rib -> f (fmap StProcess . rib . unProcess)
