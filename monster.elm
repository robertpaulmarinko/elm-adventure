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
        , direction = { x = 1, y = 0 }
        , element = element
    }

initMonsters : List (Int, Int) -> String -> List Monster
initMonsters points element =
    List.map (\point -> initMonster { x = Tuple.first point, y = Tuple.second point } element) points

moveMonsters : Map.WallsAsArray -> List Monster -> List Monster
moveMonsters wallsAsArray monsters =
    List.map (\monster -> getMonstersNewLocation wallsAsArray monster) monsters

getMonstersNewLocation : Map.WallsAsArray -> Monster -> Monster
getMonstersNewLocation wallsAsArray currentLocation =
    if currentLocation.direction.x /= 0 || currentLocation.direction.y /= 0 then
        let
            newLocation = 
                { x = currentLocation.location.x + currentLocation.direction.x, y = currentLocation.location.y - currentLocation.direction.y }
            wellElementType =
                Map.getWallElementType wallsAsArray newLocation
        in
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
                    if (currentLocation.direction.x == 1 && currentLocation.direction.y == 0) then
                        { currentLocation | direction = { x = 0, y = 1} }
                    else if (currentLocation.direction.x == 0 && currentLocation.direction.y == 1) then
                        { currentLocation | direction = { x = -1, y = 0} }
                    else if (currentLocation.direction.x == -1 && currentLocation.direction.y == 0) then
                        { currentLocation | direction = { x = 0, y = -1} }
                    else if (currentLocation.direction.x == 0 && currentLocation.direction.y == -1) then
                        { currentLocation | direction = { x = 1, y = 0} }
                    else
                        { currentLocation | direction = { x = 0, y = 0} }
    else
        -- monster is not moving, do nothing
        currentLocation
