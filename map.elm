-- This module contains types and functions related to all static elements such as
-- walls, prizes, etc.

module Map exposing (Walls, Doors, WallsAsArray, Map, MapJson, ElementType(..), getWallElement, getWallElementType, initEmptyMap, loadMap )

import Array exposing (Array)

import Display exposing (Location)

type alias Walls = List String
type alias WallsAsArray = Array (Array Char)
type alias Doors = List String

type alias Map = {
    walls: Walls
    , wallsAsArray: WallsAsArray
    , wallsMaxX: Int
    , wallsMaxY: Int
    , doors: Doors
}

type alias MapJson = {
    walls: Walls
    , doors: Doors
}
type ElementType = Empty | Wall | Prize

getWallElement : WallsAsArray -> Display.Location -> Char
getWallElement wallsAsArray location =
        case Array.get location.y wallsAsArray of
            Nothing ->
                '#'
            Just val ->
                case Array.get location.x val of
                    Nothing ->
                        '#'
                    Just val ->
                        val

getWallElementType: WallsAsArray -> Display.Location -> ElementType
getWallElementType wallsAsArray location =
    case getWallElement wallsAsArray location of
        ' ' -> Empty
        '$' -> Prize
        _ ->   Wall

initEmptyMap: Map
initEmptyMap =
    { 
        walls = []
        ,  wallsAsArray = Array.fromList []
        , wallsMaxX = 0
        , wallsMaxY = 0 
        , doors = []
    }

loadMap: MapJson -> Map  
loadMap mapJson = 
    let
        {-
            # = Wall
            B = Brick Wall

            1 = Player 1
            2 = Player 2
        -}
        wallsAsArray = 
            Array.fromList (List.map (\x -> Array.fromList (String.toList x)) mapJson.walls)

        wallsMaxX = 
            Array.length (Maybe.withDefault (Array.fromList []) (Array.get 0 wallsAsArray)) - 1
        
        wallsMaxY = 
            Array.length wallsAsArray - 1

    in
        { 
            walls = mapJson.walls
            , wallsAsArray = wallsAsArray
            , wallsMaxX = wallsMaxX
            , wallsMaxY = wallsMaxY
            , doors = mapJson.doors
        }
