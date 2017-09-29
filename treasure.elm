
module Treasure exposing (Treasure, initTreasure)

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