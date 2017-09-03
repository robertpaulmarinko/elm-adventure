-- This module contains types and functions related to all static elements such as
-- walls, prizes, etc.

module Map exposing (Walls, WallsAsArray, getWallElement )

import Array exposing (Array)

import Display exposing (Location)

type alias Walls = List String
type alias WallsAsArray = Array (Array Char)

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
