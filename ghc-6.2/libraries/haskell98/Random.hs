module Random (
#ifndef STANDALONE
   RandomGen(next, split, genRange),
   StdGen, mkStdGen,
   Random( random,   randomR, randoms,  randomRs, randomIO, randomRIO ),
   getStdRandom, getStdGen, setStdGen, newStdGen
#endif /* ! STANDALONE */
  ) where

#ifndef STANDALONE
import System.Random
#endif /* ! STANDALONE */
