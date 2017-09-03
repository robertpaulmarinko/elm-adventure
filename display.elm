-- This module contains functions for rendering output to the screen

module Display exposing (Location, renderSingleElement, renderWalls)

import Html exposing (Html, div, p, ul, li, text, map, i)
import Html.Attributes exposing ( style, class )

type alias Location =
    { x: Int
    , y : Int}

-- renders a single element to the screen
renderSingleElement : Location -> String -> Html msg
renderSingleElement location element =
  let
    left = 
      toString (location.x * 24) ++ "px"

    top = 
      toString (location.y * 24) ++ "px" 

    (className, color) = 
        case element of
            "#" -> ("fa fa-square fa-2x fa-fw", "brown") -- wall
            "B" -> ("fa fa-th-large fa-2x fa-fw", "grey") -- wall
            "1" -> ("fa fa-male fa-2x fa-fw", "blue") -- player 1
            "2" -> ("fa fa-female fa-2x fa-fw", "yellow") -- player 2
            "A" -> ("fa fa-cog fa-spin fa-2x fa-fw", "red") -- player arrow
            _ -> ("", "white")
 in
    i [ style [("position", "absolute") ,("left", left), ("top", top), ("color", color) ] , class className ] [ ] 


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


