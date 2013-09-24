module Win32Misc( timeGetTime ) where
import GraphicsCore( getTime )
import Word( Word32 )

timeGetTime :: IO Word32
timeGetTime = do
  t <- getTime
  return $ fromInteger (t `mod` 1000000000)
