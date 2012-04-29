module Math.Budget.Demo where

import Math.Budget
import Data.Time
import Network.URI

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
        undefined
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
