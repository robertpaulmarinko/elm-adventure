-- This module contains types and functions related to all static elements such as
-- walls, prizes, etc.

module Map exposing (Walls, WallsAsArray, ElementType(..), getWallElement, getWallElementType )

import Array exposing (Array)

import Display exposing (Location)

type alias Walls = List String
type alias WallsAsArray = Array (Array Char)

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
