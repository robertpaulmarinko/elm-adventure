-- This module contains functions for rendering output to the screen

module Display exposing (Location, renderSingleElement, renderString, renderWalls, renderScore)

import Html exposing (Html, div, p, ul, li, text, map, i)
import Html.Attributes exposing ( style, class )

type alias Location =
    { x: Int
    , y : Int}

elementSize: Int
elementSize = 24

-- renders a single element to the screen
renderSingleElement : Location -> String -> Html msg
renderSingleElement location element =
  let
    left = 
      toString (location.x * elementSize) ++ "px"

    top = 
      toString (location.y * elementSize) ++ "px" 

    (className, color) = 
        case element of
            "#" -> ("fa fa-square fa-2x fa-fw", "brown") -- wall
            "B" -> ("fa fa-th-large fa-2x fa-fw", "grey") -- wall
            "$" -> ("fa fa-star fa-2x fa-fw", "green") -- prize
            "Player1" -> ("fa fa-male fa-2x fa-fw", "blue") -- player 1
            "Player2" -> ("fa fa-female fa-2x fa-fw", "yellow") -- player 2
            "A" -> ("fa fa-cog fa-spin fa-2x fa-fw", "red") -- player arrow
            "M" -> ("fa fa-github-alt fa-2x fa-fw", "grey") --monster
            "0" -> ("fa fa-toggle-up fa-2x fa-fw", "grey") -- door
            "1" -> ("fa fa-toggle-up fa-2x fa-fw", "grey") -- door
            _ -> ("", "white")
 in
    i [ style [("position", "absolute") ,("left", left), ("top", top), ("color", color) ] , class className ] [ ] 

renderString : Location -> String -> String -> String -> Html msg
renderString location string className color =
  let
    left = 
      toString (location.x * elementSize) ++ "px"

    top = 
      toString (location.y * elementSize) ++ "px" 
 in
    div [ style [("position", "absolute") ,("left", left), ("top", top), ("color", color) ] , class className ] [ text string ]


-- --------------------------------------------------------
-- These functions are used to render the static walls
-- --------------------------------------------------------
type alias WallsWithRowType = { row: Int, wallString: String }
type alias WallsWithRowAndColType = { row: Int, col: Int, wallString: String }
    
addRowIndex: List (String) -> List (WallsWithRowType)
addRowIndex rows = 
    List.indexedMap (\index wallString -> { row = index, wallString = wallString }) rows

addColIndex : WallsWithRowType -> List (WallsWithRowAndColType)
addColIndex wallsWithRow  = 
    List.indexedMap (\index ws -> { row = wallsWithRow.row, col = index, wallString = (String.fromChar ws) }) (String.toList wallsWithRow.wallString)


renderWalls : List String -> List (Html msg)
renderWalls walls =
    let
        -- adds the row number to each item
        wallsWithRow = addRowIndex walls
        -- adds the col number, but is a list of lists
        wallsWIthRowAndCol = List.map addColIndex wallsWithRow
        -- convert lists of lists into a flat list
        flatList = List.concatMap (\lrc -> lrc) wallsWIthRowAndCol
    in
        List.map (\w -> renderSingleElement { x = w.col, y = w.row } w.wallString) flatList


-- --------------------------------------------------------
-- These functions are used to render the score
-- --------------------------------------------------------
renderScore : Location -> String -> Int -> List (Html msg)
renderScore location element score =
    let
        scoreLocation = { x = location.x + 2, y = location.y}
    in
        [renderSingleElement location element
        ,renderString scoreLocation (toString score) "score-value" "white"]