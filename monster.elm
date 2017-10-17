module Monster exposing (Monster, initMonsters, moveMonsters)

import Display exposing (Location)
import Map exposing (WallsAsArray, ElementType, getWallElement, getWallElementType)

type alias Monster =
    {   location: Display.Location
      , direction: Display.Location
      , element: String
    }

initMonster : Location -> String -> Monster
initMonster location element =
    { 
        location = location
        -- start with monster not moving, getMonstersNewLocation will make it move
        -- and random direction the first time
        , direction = { x = 0, y = 0 }
        , element = element
    }

initMonsters : List (Int, Int) -> String -> List Monster
initMonsters points element =
    List.map (\point -> initMonster { x = Tuple.first point, y = Tuple.second point } element) points

-- the length of randomDirections and monsters should be the same
moveMonsters : Map.WallsAsArray -> List (Int, Int) -> List Monster -> List Monster
moveMonsters wallsAsArray randomDirections monsters =
    -- combine the monsters and randomDirections in a tuple of (monster, direction) and then pass into 
    -- getMonstersNewLocation function
    List.map (\monster -> getMonstersNewLocation wallsAsArray monster) <| List.map2 (,) monsters randomDirections

getMonstersNewLocation : Map.WallsAsArray -> (Monster, (Int, Int)) -> Monster
getMonstersNewLocation wallsAsArray currentLocationAndRandomDirection =
    let
        currentLocation = 
            Tuple.first currentLocationAndRandomDirection
        newDirection =
            Tuple.second currentLocationAndRandomDirection
        newLocation = 
            { x = currentLocation.location.x + currentLocation.direction.x, y = currentLocation.location.y - currentLocation.direction.y }
        wellElementType =
            Map.getWallElementType wallsAsArray newLocation
    in
        if (currentLocation.direction.x == 0 && currentLocation.direction.y == 0) then
            -- Monster is not moving, make them move in a random direction
            { currentLocation | direction = { x = Tuple.first newDirection, y = Tuple.second newDirection }}
        else
            case wellElementType of
                Map.Empty ->
                        -- allow monster to move
                        { currentLocation |
                            location = newLocation
                        }
                Map.Prize ->
                        -- allow monster to move
                        { currentLocation |
                            location = newLocation
                        }
                Map.Wall ->
                    -- monster has hit a wall, change direction
                    let 
                        x = 
                            Tuple.first newDirection
                        y = 
                            Tuple.second newDirection
                    in
                        if (x == 0 && y == 0) then
                            -- don't let the monster standstill, instead reverse direction
                            { currentLocation | direction = { x = currentLocation.direction.x * -1, y = currentLocation.direction.y * -1} }
                        else
                            -- use the random direction
                            { currentLocation | direction = { x = x, y = y} }
