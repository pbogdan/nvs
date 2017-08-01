module Nvs.Types where

import Protolude

data NvsError
  = FileParseError FilePath
                   Text
  | ShellCommandError ExitCode
                      Text
  deriving (Eq, Show)
