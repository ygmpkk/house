module CPUTime (
#ifndef STANDALONE
    getCPUTime, cpuTimePrecision 
#endif /* ! STANDALONE */
  ) where

#ifndef STANDALONE
import System.CPUTime
#endif /* ! STANDALONE */
