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
-- import Network.Info as Info 
import Network.Socket
import System.IO

-- maintain messages & other simple shared data types

-- Functions 

main :: IO ()
main = do 
-- run with command line arguments of: name, port
putStrLn "what is your name?"
name <- getLine 
putStrLn "What port are you publishing over?"
port <- getLine 
putStrLn "What is your VPN IP address?"
ip <- getLine 

loop name port ip where 
    loop = 