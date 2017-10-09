
module Treasure exposing (Treasure, initTreasure, isAnyTreasureAtLocation)

import Display exposing (Location)


type alias Treasure =
    {   location: Display.Location
        , element: String
    }


initTreasure : Location -> String -> Treasure
initTreasure location element =
    { 
        location = location
        , element = element
    }

updateTreasure : List Treasure -> Location -> List Treasure
updateTreasure currentTreasure playerLocation =
    currentTreasure

isAnyTreasureAtLocation : List Treasure -> Location -> Bool
isAnyTreasureAtLocation treasureList playerLocation =
    let
        matches = List.filter (\t -> t.location.x == playerLocation.x && t.location.y == playerLocation.y) treasureList
    in
        List.length matches > 0
