-- This module has types and functions related to the players

module Player exposing (Player, PlayerArrow, getPlayersNewLocation, getPlayersArrowNewLocation)

import Display exposing (Location)
import Map exposing (WallsAsArray, getWallElement)

type alias Player =
    {   location: Display.Location
      , lastDelta: Display.Location
    }

type alias PlayerArrow =
    { 
        released: Bool
      , location: Display.Location
      , direction: Display.Location
    }

getPlayersNewLocation : Map.WallsAsArray -> Player -> Int -> Int -> Player
getPlayersNewLocation wallsAsArray currentLocation deltaX deltaY =
        let
            newLocation = 
                { x = currentLocation.location.x + deltaX, y = currentLocation.location.y - deltaY }
            playerWallElement =
                Map.getWallElement wallsAsArray newLocation
            lastDelta =
                if deltaX /= 0 || deltaY /= 0 then
                    { x = deltaX, y = deltaY }
                else
                    currentLocation.lastDelta
        in
            if playerWallElement == ' ' then
                -- allow player to move
                { currentLocation |
                      location = { x = currentLocation.location.x + deltaX, y = currentLocation.location.y - deltaY }
                    , lastDelta = lastDelta
                }
            else
                -- player has hit a wall, don't allow them to move
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

