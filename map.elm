-- This module contains types and functions related to all static elements such as
-- walls, prizes, etc.

module Map exposing (Walls, WallsAsArray, Map, MapJson, DoorData, ElementType(..), getWallElement, getWallElementType, initEmptyMap, loadMap, decodeMapJson )

import Array exposing (Array)
import Json.Decode exposing (..)

import Display exposing (Location)

type alias Walls = List String
type alias WallsAsArray = Array (Array Char)

type alias DoorData = {
    name: String
    , playerStartX: Int
    , playerStartY: Int
}

type alias Map = {
    name: String
    , walls: Walls
    , wallsAsArray: WallsAsArray
    , wallsMaxX: Int
    , wallsMaxY: Int
    , doors: Array DoorData
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
        name = ""
        , walls = []
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
            name = mapJson.name
            , walls = mapJson.walls
            , wallsAsArray = wallsAsArray
            , wallsMaxX = wallsMaxX
            , wallsMaxY = wallsMaxY
            , doors = Array.fromList mapJson.doors
        }

-- --------------------------------------------------------
-- These functions are used to decode the map JSON files
-- --------------------------------------------------------
type alias MapJson = {
    name: String
    , walls: Walls
    , doors: List DoorData
}

doorDataDecoder : Decoder DoorData
doorDataDecoder =
    Json.Decode.map3 DoorData
        (field "name" string)
        (field "playerStartX" int)
        (field "playerStartY" int)

-- called after the HTTP get request is finished, decodes
-- the JSON that was retrived.
decodeMapJson : Decoder MapJson
decodeMapJson =
  Json.Decode.map3 MapJson
    (field "name" (Json.Decode.string))
    (field "walls" (Json.Decode.list Json.Decode.string))
    (field "doors" (Json.Decode.list doorDataDecoder))
