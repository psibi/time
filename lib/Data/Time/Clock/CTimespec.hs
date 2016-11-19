-- #hide
module Data.Time.Clock.CTimespec where

import Foreign
import Foreign.C

data CTimespec = MkCTimespec CTime CLong

-- CTime represents the number of whole seconds of elapsed time.
-- CLong represents the rest of elapsed time as number of nanoseconds
-- Reference: https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html

foreign import java unsafe "@static ghcvm.time.Utils.getCurrentTimeInSeconds" getCurrentTimeInSeconds :: IO Int64

-- | Get the current POSIX time from the system clock.
getCTimespec :: IO CTimespec
getCTimespec = do
  curSeconds <- getCurrentTimeInSeconds
  return $ MkCTimespec (fromIntegral curSeconds) (fromIntegral 0)


