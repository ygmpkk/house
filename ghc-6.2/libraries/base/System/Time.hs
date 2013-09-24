{-# OPTIONS -optc-DSTANDALONE #-}
{-# LINE 1 "Time.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "Time.hsc" #-}
-- |
-- Module      :  System.Time
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard Time library.
--
-----------------------------------------------------------------------------

{-
Haskell 98 Time of Day Library
------------------------------

The Time library provides standard functionality for clock times,
including timezone information (i.e, the functionality of "time.h",
adapted to the Haskell environment), It follows RFC 1129 in its use of
Coordinated Universal Time (UTC).

2000/06/17 <michael.weber@post.rwth-aachen.de>:
RESTRICTIONS:
  * min./max. time diff currently is restricted to
    [minBound::Int, maxBound::Int]

  * surely other restrictions wrt. min/max bounds


NOTES:
  * printing times

    `showTime' (used in `instance Show ClockTime') always prints time
    converted to the local timezone (even if it is taken from
    `(toClockTime . toUTCTime)'), whereas `calendarTimeToString'
    honors the tzone & tz fields and prints UTC or whatever timezone
    is stored inside CalendarTime.

    Maybe `showTime' should be changed to use UTC, since it would
    better correspond to the actual representation of `ClockTime'
    (can be done by replacing localtime(3) by gmtime(3)).


BUGS:
  * add proper handling of microsecs, currently, they're mostly
    ignored

  * `formatFOO' case of `%s' is currently broken...


TODO:
  * check for unusual date cases, like 1970/1/1 00:00h, and conversions
    between different timezone's etc.

  * check, what needs to be in the IO monad, the current situation
    seems to be a bit inconsistent to me

  * check whether `isDst = -1' works as expected on other arch's
    (Solaris anyone?)

  * add functions to parse strings to `CalendarTime' (some day...)

  * implement padding capabilities ("%_", "%-") in `formatFOO'

  * add rfc822 timezone (+0200 is CEST) representation ("%z") in `formatFOO'
-}

module System.Time
     (

{-# LINE 98 "Time.hsc" #-}
     ) where


{-# LINE 705 "Time.hsc" #-}
