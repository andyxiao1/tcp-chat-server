module Parser where

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

newtype Parser a = P {doParse :: String -> Maybe (a, String)}

help :: String
help = undefined -- case str of
-- (':' : 'h' : xs) -> undefined -- print out the help menu!

-- stepper :: Block -> IO ()
-- stepper b = go b Map.empty [] []
--   where
--     go block store bList sList = do
--       putBlock block
--       putStr "imp> "
--       str <- getLine
--       case str of
--         "n" -> do
--           let s = step block
--           go (S.evalState s store) (S.execState s store) (block : bList) (store : sList)
--         "b" -> case (bList, sList) of
--           (x : xs, y : ys) -> go x y xs ys
--           (_, _) -> go block store bList sList
--         'v' : ' ' : xs ->
--           case Map.lookup xs store of
--             Just val -> putStrLn (PP.render (pp val)) >> go block store bList sList
--             Nothing -> putStrLn "Unknown var" >> go block store bList sList
--         "x" -> return () -- quit the stepper
--         _ -> putStrLn "?" >> go block store bList sList -- unknown command
--     putBlock :: Block -> IO ()
--     putBlock (Block []) = putStrLn "done"
--     putBlock (Block (s : _)) = putStr "-->" >> putStrLn (PP.render (pp s))

-----------------------------
-- Test Cases
-----------------------------
