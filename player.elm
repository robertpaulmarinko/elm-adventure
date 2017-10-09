-- This module has types and functions related to the players

module Player exposing (Player, PlayerArrow, initPlayer, getPlayersNewLocation, getPlayersArrowNewLocation)

import Display exposing (Location)
import Map exposing (WallsAsArray, ElementType, getWallElement, getWallElementType)
import Treasure exposing (Treasure)

type alias Player =
    {   location: Display.Location
      , lastDelta: Display.Location
      , score: Int
    }

type alias PlayerArrow =
    { 
        released: Bool
      , location: Display.Location
      , direction: Display.Location
    }


initPlayer : Location -> Player
initPlayer location =
    { 
        location = location
        , lastDelta = { x = 0, y = 0 }
        , score = 0
    }

getPlayersNewLocation : Map.WallsAsArray -> List Treasure -> Player -> Int -> Int -> Player
getPlayersNewLocation wallsAsArray treasures currentLocation deltaX deltaY =
    if deltaX /= 0 || deltaY /= 0 then
        let
            newLocation = 
                { x = currentLocation.location.x + deltaX, y = currentLocation.location.y - deltaY }
            playerWallElementType =
                Map.getWallElementType wallsAsArray newLocation
            lastDelta =
                { x = deltaX, y = deltaY }
        in
            case playerWallElementType of
                Map.Empty ->
                    if Treasure.isAnyTreasureAtLocation treasures newLocation then
                        -- allow player to move and increment score
                        { currentLocation |
                            location = newLocation
                            , lastDelta = lastDelta
                            , score = currentLocation.score + 1
                        }
                    else
                        -- allow player to move
                        { currentLocation |
                            location = newLocation
                            , lastDelta = lastDelta
                        }
                Map.Prize ->
                -- allow player to move and increment score
                    { currentLocation |
                        location = newLocation
                        , lastDelta = lastDelta
                        , score = currentLocation.score + 1
                    }
                Map.Wall ->
                    -- player has hit a wall, don't allow them to move
                    currentLocation
    else
        -- player has not moved, do nothing
        currentLocation

getPlayersArrowNewLocation : Map.WallsAsArray -> PlayerArrow -> PlayerArrow
getPlayersArrowNewLocation wallsAsArray currentLocation =
        let
            newLocation = 
                { x = currentLocation.location.x + currentLocation.direction.x, y = currentLocation.location.y - currentLocation.direction.y }
            playerWallElement =
                Map.getWallElement wallsAsArray newLocation
        in
            if currentLocation.released then
                if playerWallElement == ' ' then
                    -- allow arrow to move
                    { currentLocation |
                        location = newLocation
                    }
                else
                    -- player has hit a wall, don't allow them to move
                    { currentLocation |
                        released = False
                    }
            else
                -- arrow not moving
                currentLocation

