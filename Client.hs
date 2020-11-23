module Client where

import Control.Applicative (Alternative (..), liftA3)
import Control.Monad ()
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
-- import Data.ProtocolBuffers

-- import qualified Data.Time.Clock as Clock
import GHC.Generics as Gen
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
