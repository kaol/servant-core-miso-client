-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client.JSaddle
  (
    client
  , ClientM
  , runClientM
  , runClientM'
  , withClientM

    -- * Configuration
  , ClientEnv(..)
  , mkClientEnv
  , getDefaultBaseUrl

  , module Servant.Client.Core.Reexport
  ) where

import Servant.Client.JSaddle.Internal.Client
import Servant.Client.JSaddle.Internal.Types
import Servant.Client.Core.Reexport
