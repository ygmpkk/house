module TestWord ( tests ) where

import Random
import Bits
import Word
import QuickCheck
import QuickCheckUtils
import QuickCheckBatch

instance Random Word8 where
  randomR (a,b) g = case (randomR (toInteger a,toInteger b) g) of
		     (r,g) -> (fromInteger r,g)
instance Random Word16 where
  randomR (a,b) g = case (randomR (toInteger a,toInteger b) g) of
		     (r,g) -> (fromInteger r,g)
instance Random Word32 where
  randomR (a,b) g = case (randomR (toInteger a,toInteger b) g) of
		     (r,g) -> (fromInteger r,g)
instance Random Word64 where
  randomR (a,b) g = case (randomR (toInteger a,toInteger b) g) of
		     (r,g) -> (fromInteger r,g)

instance Arbitrary Word8 where
   arbitrary = frequency [ (1,return minBound)
			 , (1,return maxBound)
			 , (6,choose (minBound,maxBound)) ]
   coarbitrary n = variant (hashWord n)
instance Arbitrary Word16 where
   arbitrary = frequency [ (1,return minBound)
			 , (1,return maxBound)
			 , (6,choose (minBound,maxBound)) ]
   coarbitrary n = variant (hashWord n)
instance Arbitrary Word32 where
   arbitrary = frequency [ (1,return minBound)
			 , (1,return maxBound)
			 , (6,choose (minBound,maxBound)) ]
   coarbitrary n = variant (hashWord n)
instance Arbitrary Word64 where
   arbitrary = frequency [ (1,return minBound)
			 , (1,return maxBound)
			 , (6,choose (minBound,maxBound)) ]
   coarbitrary n = variant (hashWord n)

hashWord :: (Integral a,Bits a) => a -> Int
hashWord w = toInt (hashWord' w (bitSize w - 4) .&. 0xf)

hashWord' w 0 = w
hashWord' w n = (w `shiftR` n) `xor` hashWord' w (n - 4)

isAssocWord32 fn = isAssociative fn
	where types = (fn :: Word32 -> Word32 -> Word32) 

isCommWord32 fn = isCommutable fn
	where types = (fn :: Word32 -> Word32 -> Word32) 

------------------------------------------------------------------------------
-- True for all Words

prop_negate w = negate w == 0 - w

prop_range w = (to minBound <= to w) && (to w <= to maxBound)
	where
	    to n = (toInteger (n `asTypeOf` w)) :: Integer

bitRange w = forAll (choose (0,bitSize w - 1))

prop_complement w = complement w == -1 - w

prop_bit1 w = 
	bitRange w $ \ i ->
		 bit i == (2 `asTypeOf` w) ^ i

prop_bit2 w = 
	bitRange w $ \ i ->
		toInteger (bit i `asTypeOf` w) == 2 ^ i

prop_setBit w = 
	bitRange w $ \ i ->
		setBit w i == w .|. bit i

prop_clearBit w = 
	bitRange w $ \ i ->
		clearBit w i  == w .&. (bit i `xor` maxBound)
prop_testBit w = 
	bitRange w $ \ i ->
		testBit w i == ((w .&. bit i) /= 0)

prop_shiftL1 w = shiftL w 0 == w
prop_shiftR1 w = shiftR w 0 == w
prop_rotateL1 w = rotateL w 0 == w
prop_rotateR1 w = rotateR w 0 == w

prop_shiftL2 w = 
	bitRange w $ \ i ->
	bitRange w $ \ j ->
		(i + j < bitSize w) ==>
		(w `shiftL` i) `shiftL` j == shiftL w (i+j)

prop_shiftR2 w = 
	bitRange w $ \ i ->
	bitRange w $ \ j ->
		(i + j < bitSize w) ==>
		(w `shiftR` i) `shiftR` j == shiftR w (i+j)

prop_rotateL2 w = 
	bitRange w $ \ i ->
	bitRange w $ \ j ->
	   (w `rotateL` i) `rotateL` j == rotateL w (i+j)

prop_rotateR2 w = 
	bitRange w $ \ i ->
	bitRange w $ \ j ->
	   (w `rotateR` i) `rotateR` j == rotateR w (i+j)
	
prop_shiftL3 w = 
	forAll (choose (bitSize w,maxBound :: Int)) $ \ n -> shiftL w n == 0
prop_shiftR3 w = 
	forAll (choose (bitSize w,maxBound :: Int)) $ \ n -> shiftR w n == 0

prop_test1 w = bitRange w $ \ i -> (w `setBit` i) `testBit` i == True
prop_test2 w = bitRange w $ \ i -> (w `clearBit` i) `testBit` i == False

test_wordCommon w name = runTests ("wordCommon-"++name) testOptions $
	[ run' prop_negate 
	, run' prop_range
	, run' prop_complement
	, run' prop_bit1
	, run' prop_bit2
	, run' prop_setBit
	, run' prop_clearBit
	, run' prop_testBit
	, run' prop_shiftL1
	, run' prop_shiftR1
	, run' prop_rotateL1
	, run' prop_rotateR1
	, run' prop_shiftL2
	, run' prop_shiftR2
	, run' prop_rotateL2
	, run' prop_rotateR2
	, run' prop_shiftL3
	, run' prop_shiftR3
	, run' prop_test1
	, run' prop_test2
	]
	where run' arg = run (runWord arg)
	      runWord fn arg = fn (arg `asTypeOf` w)
------------------------------------------------------------------------------


test_wordCommonWord8 = test_wordCommon (0 :: Word8) "Word8"
test_wordCommonWord16 = test_wordCommon (0 :: Word16) "Word16"
test_wordCommonWord32 = test_wordCommon (0 :: Word32) "Word32"
test_wordCommonWord64 = test_wordCommon (0 :: Word64) "Word64"


tests = do
	test_wordCommonWord8
	test_wordCommonWord16
	test_wordCommonWord32
	test_wordCommonWord64


------------------------------------------------------------------------------

testOptions :: TestOptions
testOptions = TestOptions 
                 { no_of_tests = 1000   -- number of tests to run
                 , length_of_tests = 1	-- 1 second max per check
					-- where a check == n tests
                 , debug_tests = False	-- True => debugging info
                 }

------------------------------------------------------------------------------

