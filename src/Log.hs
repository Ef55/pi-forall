module Log (Log(..), isInfo) where

import qualified Data.List as List
import Control.Monad.Writer (MonadWriter (tell))

data Log
  = Info String
  | Warn String

instance Show Log where
  show (Info s) = "info: " <> s
  show (Warn s) = "warn: " <> s

isInfo :: Log -> Bool
isInfo (Info _) = True
isInfo _ = False

-- info = tell . List.singleton . Info
-- warn = tell . List.singleton . Warn