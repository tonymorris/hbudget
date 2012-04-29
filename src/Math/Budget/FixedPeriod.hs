module Math.Budget.FixedPeriod
(
  FixedPeriod
, everySecondsL
, everyMonthsL
, isEverySecond
, isEveryMinute
, isEveryHour
, isEveryDay
, isEveryWeek
, isEveryFortnight
, isEveryMonth
, isEveryYear
, everySeconds
, everyMinutes
, everyHours
, everyDays
, everyWeeks
, everyFortnights
, everyMonths
, everyYears
) where

import Data.Lens.Partial.Common
import Control.Comonad.Trans.Store
import Control.Monad.State
import Data.List
import Prelude
import qualified Prelude as P

data FixedPeriod =
  EverySeconds Integer
  | EveryMonths Int
  deriving (Eq, Ord)

instance Show FixedPeriod where
  show z =
    let conj q s = if q == 0
                     then
                       ""
                     else
                       show q ++ ' ' : (if q == 1 then s else s ++ "s")
        display u = intercalate ", " . filter (not . P.null) . evalState (mapM (\(x, s) -> state $ \r -> let (a, b) = divMod r x in (conj a s, b)) u)
    in case z of
      EverySeconds n ->
        display [(2 * 7 * 24 * 60 * 60, "fortnight"), (7 * 24 * 60 * 60, "week"), (24 * 60 * 60, "day"), (60 * 60, "hour"), (60, "minute"), (1, "second")] n
      EveryMonths n ->
        display [(12, "year"), (1, "month")] n

everySecondsL ::
  PartialLens FixedPeriod Integer
everySecondsL =
  PLens $ \p -> case p of
    EverySeconds n -> Just (store everySeconds n)
    EveryMonths _ -> Nothing

everyMonthsL ::
  PartialLens FixedPeriod Int
everyMonthsL =
  PLens $ \p -> case p of
    EveryMonths n -> Just (store everyMonths n)
    EverySeconds _ -> Nothing

isEverySecond ::
  FixedPeriod
  -> Bool
isEverySecond =
  anyPL everySecondsL (== 1)

isEveryMinute ::
  FixedPeriod
  -> Bool
isEveryMinute =
  anyPL everySecondsL (== 60)

isEveryHour ::
  FixedPeriod
  -> Bool
isEveryHour =
  anyPL everySecondsL (== 60 * 60)

isEveryDay ::
  FixedPeriod
  -> Bool
isEveryDay =
  anyPL everySecondsL (== 60 * 60 * 24)

isEveryWeek ::
  FixedPeriod
  -> Bool
isEveryWeek =
  anyPL everySecondsL (== 60 * 60 * 24 * 7)

isEveryFortnight ::
  FixedPeriod
  -> Bool
isEveryFortnight =
  anyPL everySecondsL (== 60 * 60 * 24 * 7 * 2)

isEveryMonth ::
  FixedPeriod
  -> Bool
isEveryMonth =
  anyPL everyMonthsL (== 1)

isEveryYear ::
  FixedPeriod
  -> Bool
isEveryYear =
  anyPL everyMonthsL (== 12)

everySeconds ::
  Integer
  -> FixedPeriod
everySeconds =
  EverySeconds

everyMinutes ::
  Integer
  -> FixedPeriod
everyMinutes n =
  everySeconds (n * 60)

everyHours ::
  Integer
  -> FixedPeriod
everyHours n =
  everyMinutes (n * 60)

everyDays ::
  Integer
  -> FixedPeriod
everyDays n =
  everyHours (n * 24)

everyWeeks ::
  Integer
  -> FixedPeriod
everyWeeks n =
  everyDays (n * 7)

everyFortnights ::
  Integer
  -> FixedPeriod
everyFortnights n =
  everyWeeks (n * 2)

everyMonths ::
  Int
  -> FixedPeriod
everyMonths =
  EveryMonths

everyYears ::
  Int
  -> FixedPeriod
everyYears n =
  everyMonths (n * 12)