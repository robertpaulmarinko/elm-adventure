-- This module contains types and functions related to all static elements such as
-- walls, prizes, etc.

module Map exposing (Walls, Doors, WallsAsArray, Map, MapJson, ElementType(..), getWallElement, getWallElementType, initEmptyMap, loadMap )

import Array exposing (Array)

import Display exposing (Location)

type alias Walls = List String
type alias WallsAsArray = Array (Array Char)
type alias Doors = Array String

type alias Map = {
    walls: Walls
    , wallsAsArray: WallsAsArray
    , wallsMaxX: Int
    , wallsMaxY: Int
    , doors: Doors
}

type alias MapJson = {
    walls: Walls
    , doors: List String
}
type ElementType = Empty | Wall | Prize | Door

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
        '0' -> Door
        '1' -> Door
        '2' -> Door
        '3' -> Door
        '4' -> Door
        '5' -> Door
        '6' -> Door
        '7' -> Door
        '8' -> Door
        '9' -> Door
        _ ->   Wall

initEmptyMap: Map
initEmptyMap =
    { 
        walls = []
        ,  wallsAsArray = Array.fromList []
        , wallsMaxX = 0
        , wallsMaxY = 0 
        , doors = Array.fromList []
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
            , doors = Array.fromList mapJson.doors
        }
