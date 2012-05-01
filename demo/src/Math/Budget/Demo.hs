module Math.Budget.Demo where

import Math.Budget
import Control.Applicative
import Data.List
import Data.List.NonEmpty hiding (iterate, zipWith, tail)
import Data.Maybe
import Data.Time
import Data.Time.Calendar.OrdinalDate

paydaydiffs ::
  ZonedTime
  -> FixedPeriods
paydaydiffs =
  (zipWith (\v w -> let UTCTime vd _ = zonedTimeToUTC v
                        UTCTime wd _ = zonedTimeToUTC w
                    in everySeconds (diffDays wd vd * 24 * 60 * 60)) <*> tail) . paydays

-- Fred is paid on the first prior business day on (and including) the 15th and last day of every calendar month.
paydays ::
  ZonedTime
  -> [ZonedTime]
paydays (ZonedTime (LocalTime d t) z) =
  let validPayday q =
        let (_, x) = mondayStartWeek q
            (_, m', r) = toGregorian q
        in not $ or [
                      x == 6 -- is Saturday
                    , x == 7 -- is Sunday
                    , m' == 4 && r == 15 -- 15 April, Bedrock Day
                    ]
      (yy, mm, dd) =
        toGregorian d
      (n, f) =
        if dd <= 15
          then
            (1, const . const $ 15)
          else
            (16, gregorianMonthLength)
      next =
        fromGregorian yy mm (f yy mm)
      rewind =
        ZonedTime (LocalTime (fromMaybe (fromGregorian yy mm n) . find validPayday . iterate (addDays (-1)) $ next) t) z
      rest =
        paydays (ZonedTime (LocalTime (addDays 1 next) t) z)
  in rewind : rest

bedrockTime ::
  Integer
  -> Int
  -> Int
  -> TimeOfDay
  -> ZonedTime
bedrockTime y m d t =
  ZonedTime (LocalTime (fromGregorian y m d) t) (hoursToTimeZone 10)

mybudget ::
  Budget
mybudget =
  budget
    [
      expensePayment
        "Rent"
        (bedrockTime 1966 11 20 (TimeOfDay 9 0 0))
        (bankDeposit "Mr Landlord" "123456" "00118421")
        (fixedInterval everyWeek)
        200
    , expensePayment
        "Electricity"
        (bedrockTime 1966 8 17 (TimeOfDay 12 0 0)) -- todo function of priors
        (internetMethod $ URI {
          uriScheme = "http"
        , uriAuthority = Just $ URIAuth {
            uriUserInfo = ""
          , uriRegName = "acmepower.com"
          , uriPort = ""
          }
        , uriPath = "/paybill"
        , uriQuery = "account=123"
        , uriFragment = ""
        })
        (fixedInterval $ everyMonths 3)
        300 -- todo function of priors
     , let start = bedrockTime 1966 6 15 (TimeOfDay 12 0 0)
       in incomePayment
            "Fred's pay"
            (bedrockTime 1966 11 20 (TimeOfDay 9 0 0))
            (bankDeposit "Fred Flintstone" "678345" "019930")
            (arbitraryInterval (paydaydiffs start))
            1000
    ]
