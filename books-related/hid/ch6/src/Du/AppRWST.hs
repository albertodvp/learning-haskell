-- |

module Du.AppRWST where
import           Control.Monad.RWS (RWST, evalRWST)

import           Du.AppTypes       (AppConfig, AppEnv, initialEnv)

type MyApp logEntry state = RWST AppEnv [logEntry] state IO


runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app = evalRWST app . initialEnv
