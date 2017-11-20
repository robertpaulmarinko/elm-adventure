module MonsterType exposing (Monster)

import Display exposing (Location)

type alias Monster =
    {   location: Display.Location
      , direction: Display.Location
      , element: String
    }
