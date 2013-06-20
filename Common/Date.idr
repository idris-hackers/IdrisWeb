module IdrisWeb.Common.Date
{- Simple date and time records.
   Like, really simple. They'll do for now.

   Allows date serialisation and deserialisation as per
   RFC822, which is used when setting cookies.

   TODO: Timezones, timestamp, comparisons
-}

data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday


data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December

-- Not using the show typeclass for cookies, since there are so many
-- different ways of showing dates.
showCookieDay : Day -> String
showCookieDay Monday = "Mon"
showCookieDay Tuesday = "Tue"
showCookieDay Wednesday = "Wed"
showCookieDay Thursday = "Thu"
showCookieDay Friday = "Fri"
showCookieDay Saturday = "Sat"
showCookieDay Sunday = "Sun"

public
record Date : Type where 
  MkDate : (day : Int) ->
           (month : Int) ->
           (year : Int) -> Date

public
record DateTime : Type where
  MkDateTime : (day : Int) ->
               (month : Int) ->
               (year : Int) ->
               (hour : Int) -> 
               (minute : Int) ->
               (second : Int) ->
               DateTime


-- Why everyone can't just use the ISO one, I'll never know...
-- Wed, 13 Jan 2021 22:23:01 GMT
abstract
showCookieTime : DateTime -> String
showCookieTime dt 
