
module Treasure exposing (Treasure, initTreasure, isAnyTreasureAtLocation, updateTreasure)

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

updateTreasure : List Treasure -> Location -> Location -> List Treasure
updateTreasure treasureList player1Location player2Location =
    -- return all treasure that does NOT match the player location.  So if a player
    -- hits a treasure, this will cause the treasure to fo away
    List.filter (\t -> (t.location.x /= player1Location.x || t.location.y /= player1Location.y) && (t.location.x /= player2Location.x || t.location.y /= player2Location.y)) treasureList


isAnyTreasureAtLocation : List Treasure -> Location -> Bool
isAnyTreasureAtLocation treasureList playerLocation =
    let
        matches = List.filter (\t -> t.location.x == playerLocation.x && t.location.y == playerLocation.y) treasureList
    in
        List.length matches > 0
