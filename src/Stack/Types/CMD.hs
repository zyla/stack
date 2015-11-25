module Stack.Types.Cmd
  ( Cmd(..)
  ) where

import System.Process.Read (EnvOverride)
import Path (Path, Abs, Dir)
import Data.Text (Text)
import GHC.IO.Handle (Handle)

-- | Cmd holds common infos needed to running a process in most cases
data Cmd = Cmd
  { cmdDirectoryToRunIn :: Maybe (Path Abs Dir) -- ^ directory to run in
  , cmdCommandToRun :: FilePath -- ^ command to run
  , cmdEnvOverride::EnvOverride
  , cmdCommandLineArguments :: [String] -- ^ command line arguments
  }
