module Stack.Types.CMD
  ( CMD(..)
  ) where

import System.Process.Read (EnvOverride)
import Path (Path, Abs, Dir)
import Data.Text (Text)
import GHC.IO.Handle (Handle)

-- | CMD holds common infos needed to running a process in most cases
data CMD = CMD
  { cmdDirectoryToRunIn :: Maybe (Path Abs Dir) -- ^ directory to run in
  , cmdCommandToRun :: FilePath -- ^ command to run
  , cmdEnvOverride::EnvOverride
  , cmdCommandLineArguments :: [String] -- ^ command line arguments
  }
