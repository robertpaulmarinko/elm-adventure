module Player exposing (Player, PlayerArrow, initPlayer, getPlayersNewLocation, getPlayersArrowNewLocation, checkForMonsterCollision, getPlayerPositionAfterEnteringRoom)

import Display exposing (Location)
import Map exposing (WallsAsArray, ElementType, getWallElement, getWallElementType)
import Treasure exposing (Treasure)
import Monster exposing (Monster)

import Array exposing (Array)
import Debug

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

getPlayersNewLocation : Map.WallsAsArray -> List Treasure -> List Monster -> Player -> Int -> Int -> Player
getPlayersNewLocation wallsAsArray treasures monsters currentLocation deltaX deltaY =
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
                    else if Monster.isAnyMonsterAtLocation monsters newLocation then
                        -- ran into a monster, reset score to 0
                        { currentLocation |
                            location = newLocation
                            , lastDelta = lastDelta
                            , score = 0
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

                Map.Door ->
                    -- allow player to move
                    { currentLocation |
                        location = newLocation
                        , lastDelta = lastDelta
                    }
                
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

checkForMonsterCollision : Player -> List Monster -> Player
checkForMonsterCollision player monsters =
    if Monster.isAnyMonsterAtLocation monsters player.location then
        -- ran into a monster, reset score to 0
        { player |
            score = 0
        }
    else
        -- did not run into monster
        player

getPlayerPositionAfterEnteringRoom : Player -> Map.Map -> String -> Player
getPlayerPositionAfterEnteringRoom player map previousRoom =
        let
            -- Find the door data from door the player just went through
            matchingDoors = 
                Array.filter (\door -> door.name == previousRoom) map.doors
            -- Should only return a single door, so pull it out
            matchingDoor = 
                Maybe.withDefault { name = "", playerStartX = 5, playerStartY = 5} (Array.get 0 matchingDoors)
            -- grab the players new location from the door data
            newLocation =
                { x = matchingDoor.playerStartX, y = matchingDoor.playerStartY }
        in
            {
                player |
                    location = newLocation
            }