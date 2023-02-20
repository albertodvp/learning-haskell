-- |

module AppRWST where
import           Control.Monad.RWS (RWST)

import           AppTypes          (AppEnv)

type MyApp logEntry state = RWST AppEnv [logEntry] state IO

