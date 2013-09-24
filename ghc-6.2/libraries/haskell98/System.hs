module System (
#ifndef STANDALONE
    ExitCode(ExitSuccess,ExitFailure),
    getArgs, getProgName, getEnv, system, exitWith, exitFailure
#endif /* ! STANDALONE */
  ) where

#ifndef STANDALONE
import System.Exit
import System.Environment
import System.Cmd
#endif /* ! STANDALONE */
