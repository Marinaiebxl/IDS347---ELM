module Main exposing(..)

import Browser
import File exposing(File)
import File.Select as Select
import Html exposing (Html,text)
import String exposing(Split)

--MODEL 
type Model = Loading | Done String | Error String

--UPDATE 
type Msg = FileSelected File | GotResult(Result String String)

update : Msg -> Model -> Model 
update Msg Model =   case msg of FileSelected File -> parseFile file Model 

--gotResult result = case result of -> Ok day -> Done day

err error -> Error error 

--VIEW 

view: Model -> Html Msg
view model =
case model of 
Loading ->
text "Loading.."

Done day ->
text("Day with  the smallest temperature spread is:" ++ day) 

Error error ->
text("Error:"++ error)

--PARSE CSV 

parseCsv: String -> List(List String)

parseCsv csv =
let 
rows = split "\n" csv 
toCells row = split "," row 
in 
List.map toCells rows 

-- EXTRACT DAY DATA
getDay: List String -> {day: int, high: int, low: int}
getDay row = 
case row of 
[dayStr,highStr,lowStr] ->
{day = String.toInt dayStr|> Maybe.withDefault 0
, high = String.toInt highStr|> Maybe.withDefault 0
, low = String.toInt lowStr|> Maybe.withDefault 0 }
-> {day =0, high = 0, low = 0}



-- FIND MIN SPREAD

findMinSpread : List {day: int, high: int, low: int} -> {day: int, spread: int}

findMinSpread day = 

List.foldl
(\ day minSoFar ->
let
spread = day.high - day.low
newSpread = min spread minSoFar.Spread
in 
if newSpread = = minSoFar.Spread then
else 
{day = day.day, spread = newSpread}

)
{day = 0, spread = 9999}
days

-- FIND DAY 
findDayWithSmallestSpread: List (List String) -> Result String String 
findDayWithSmallestSpread rows = 
let 
days = List.Map getDay rows 
in
findMinSpread days
|> .day
|> String.fromInt
|> Ok

--Suscriptions
suscriptions : Model -> Sub Msg
suscriptions model =
Select.file "weather.csv" FileSelected
