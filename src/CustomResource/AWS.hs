module CustomResource.AWS
  ( module Control.Lens.Getter
  , module Control.Lens.Lens
  , module Control.Lens.Setter
  , AWS
  , AWSConstraint
  , AWSRequest
  , MonadAWS
  , send
  , withEnv
  )
where

import Control.Lens.Getter ((^.), view)
import Control.Lens.Lens ((&), (<&>))
import Control.Lens.Setter ((.~), (?~), set)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.AWS (AWSConstraint, runAWST)
import Control.Monad.Trans.Resource (runResourceT)
import CustomResource.Prelude
import Network.AWS (AWS, MonadAWS, newLogger, send)
import Network.AWS.Auth (Credentials(Discover))
import Network.AWS.Env (envLogger, newEnv)
import Network.AWS.Types (AWSRequest, LogLevel(Info))
import System.IO (stderr)

withEnv :: (MonadCatch m, MonadUnliftIO m) => AWS a -> m a
withEnv action = do
  logger <- newLogger Info stderr
  env    <- newEnv Discover <&> set envLogger logger

  liftIO . runResourceT $ runAWST env action
