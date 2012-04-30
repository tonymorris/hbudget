module Math.Budget.Demo where

import Math.Budget
import Data.List
import Data.List.NonEmpty hiding (iterate)
import Data.Maybe
import Data.Time
import Data.Time.Calendar.OrdinalDate

nextPayDays ::
  ZonedTime
  -> ZonedTime
nextPayDays ZonedTime (LocalTime d t) z =
  let validPayDay q =
        let (_, x) = mondayStartWeek q
            (_, m', r) = toGregorian q
        in not $ or [
                      x == 6 -- is Saturday
                    , x == 7 -- is Sunday
                    , m' == 4 && r == 25 -- 25 April, Bedrock Day
                    ]
      (y, m, d') = toGregorian d
      count from to =
        let (yy', mm', dd') = fromMaybe (y, m, from) . find (\(yy, mm, dd) -> validPayDay (fromGregorian yy mm dd)) . iterate (\(yy, mm, dd) -> (yy, mm, dd - 1)) $ (y, m, to y m)
        in ZonedTime (LocalTime (fromGregorian yy' mm' dd') t) z
  in if d' <= 15
       then
         count 1 (const . const $ 15)
       else
         count 16 gregorianMonthLength


  {-
    let w = if d' <= 15
               then
                 count 1 (const . const $ 15)
               else
                 count 16 gregorianMonthLength
         UTCTime vd _ = zonedTimeToUTC v
         UTCTime wd _ = zonedTimeToUTC w
     in everySeconds (diffDays wd vd * 24 * 60 * 60)
    -}
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
        (knownInterval (fixedInterval everyWeek) 200)
        (bedrockTime 1966 11 20 (TimeOfDay 9 0 0))
        (bankDeposit "Fred Flintstone" "123456" "00118421")
    , expensePayment
        "Electricity"
        (functionOfPriors (fixedPriors (450.50 :| [460.37, 440]) (everyMonths 3)))
        (bedrockTime 1966 8 17 (TimeOfDay 12 0 0))
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
    ]
