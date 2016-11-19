-- #hide
module Data.Time.Clock.CTimeval where

#ifndef mingw32_HOST_OS
-- All Unix-specific, this

#if __GLASGOW_HASKELL__ >= 709 || __GLASGOW_HASKELL__ < 702
import Foreign
#else
import Foreign.Safe
#endif
import Foreign.C

data CTimeval = MkCTimeval CLong CLong

instance Storable CTimeval where
    sizeOf _ = (sizeOf (undefined :: CLong)) * 2
    alignment _ = alignment (undefined :: CLong)
    peek p = do
        s   <- peekElemOff (castPtr p) 0
        mus <- peekElemOff (castPtr p) 1
        return (MkCTimeval s mus)
    poke p (MkCTimeval s mus) = do
        pokeElemOff (castPtr p) 0 s
        pokeElemOff (castPtr p) 1 mus

foreign import java unsafe "@static java.lang.System.currentTimeMillis" gettimeofday :: IO CLong

-- | Get the current POSIX time from the system clock.
getCTimeval :: IO CTimeval
getCTimeval = do
  t <- gettimeofday
  return $ MkCTimeval (t `div` 1000) ((t `mod` 1000) * 1000)
#endif
