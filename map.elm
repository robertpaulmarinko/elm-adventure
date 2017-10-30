-- This module contains types and functions related to all static elements such as
-- walls, prizes, etc.

module Map exposing (Walls, WallsAsArray, Map, ElementType(..), getWallElement, getWallElementType, initEmptyMap, loadMap )

import Array exposing (Array)

import Display exposing (Location)

type alias Walls = List String
type alias WallsAsArray = Array (Array Char)

type alias Map = {
    walls: Walls
    , wallsAsArray: WallsAsArray
    , wallsMaxX: Int
    , wallsMaxY: Int
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
    }

loadMap: Map  
loadMap =
    let
        {-
            # = Wall
            B = Brick Wall

            1 = Player 1
            2 = Player 2
        -}
        walls = 
        [
            "##################################################################"
            ,"#                           #      #                             #"
            ,"#                           #      #                             #"
            ,"#                           #      #                             #"
            ,"#                           ###  ###                             #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"#                   BBBBBBB                                      #"
            ,"#                   B     B                                      #"
            ,"#                   B                                            #"
            ,"#                   B     BBBBB                                  #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"#                                                                #"
            ,"##################################################################"
        ]

        wallsAsArray = 
            Array.fromList (List.map (\x -> Array.fromList (String.toList x)) walls)

        wallsMaxX = 
            Array.length (Maybe.withDefault (Array.fromList []) (Array.get 0 wallsAsArray)) - 1
        
        wallsMaxY = 
            Array.length wallsAsArray - 1

    in
        { 
            walls = walls
            , wallsAsArray = wallsAsArray
            , wallsMaxX = wallsMaxX
            , wallsMaxY = wallsMaxY
        }
