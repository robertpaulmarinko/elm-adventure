-- This version can render the walls, the player and allow the player to move
--and stops player from going through walls
-- next, shoot arrows

-- elm-make game7.elm --output=game.js

-- exmaple wiht multiple messages and subscriptions
-- https://github.com/freakingawesome/drunk-label/blob/master/src/DrunkLabel.elm

module Main exposing (..)

import Html exposing (Html, div, p, ul, li, text, map, i)
import Html.Attributes exposing ( style, class )
import Keyboard exposing (..)
import Keyboard.Extra exposing (Key(..))
import Array exposing (Array)
import Time exposing (Time, second, millisecond)
import Char exposing (..)


type Msg
    = KeyboardMsg Keyboard.Extra.Msg
    | KeyPressed Char
    | MoveArrow Time

type alias Location =
    { x: Int
    , y : Int}

type alias Player =
    {   location: Location
      , lastDelta: Location
    }

type alias PlayerArrow =
    { 
        released: Bool
      , location: Location
      , direction: Location
    }

type alias Model =
    { pressedKeys : List Key
      , player1 : Player
      , player2 : Player
      , player1Arrow : PlayerArrow
      , player2Arrow : PlayerArrow
      , walls : List String
      , wallsAsArray : Array (Array Char)
    }




init : ( Model, Cmd Msg )
init =
    let
    {-
        # = Wall
        B = Brick Wall

        1 = Player 1
        2 = Player 2
    -}
     walls = [
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
    in
    ( { 
        pressedKeys = []
        , player1 = { 
            location = { x = 2, y = 2 }
            , lastDelta = { x = 0, y = 0 }
            }
        , player2 = { 
            location = { x = 10, y = 2 }
            , lastDelta = { x = 0, y = 0 } 
        }
        , player1Arrow = {
            location = { x = 0, y = 0 }
            , direction = { x = 0, y = 0 }
            , released = False
        }
        , player2Arrow = {
            location = { x = 0, y = 0 }
            , direction = { x = 0, y = 0 }
            , released = False
        }
        , walls =  walls
        , wallsAsArray = Array.fromList (List.map (\x -> Array.fromList (String.toList x)) walls)
        }
     , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
        let
            shiftPressed =
                List.member Shift model.pressedKeys

            arrows =
                Keyboard.Extra.arrows model.pressedKeys

            wasd =
                Keyboard.Extra.wasd model.pressedKeys

        in
            (
            { model
                | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys
                , player1 = getPlayersNewLocation model model.player1 arrows.x arrows.y
                , player2 = getPlayersNewLocation model model.player2 wasd.x wasd.y
            }
            , Cmd.none
            )
        KeyPressed char ->
            if char == ' ' then
                (
                    { model
                        | player1Arrow = { location = model.player1.location, direction = model.player1.lastDelta, released = True } 
                    }
                    , Cmd.none
                )
            else if char == 'x' || char == 'X' then
                (
                    { model
                        | player2Arrow = { location = model.player2.location, direction = model.player2.lastDelta, released = True } 
                    }
                    , Cmd.none
                )
            else            
                ( model, Cmd.none )

        MoveArrow time ->
            let
                somevalue = 1
            in
            ( 
                 {
                     model
                    | player1Arrow = getPlayersArrowNewLocation model model.player1Arrow
                    , player2Arrow = getPlayersArrowNewLocation model model.player2Arrow
                 }
                , Cmd.none 
            )

view : Model -> Html msg
view model =
        div []
            [
            renderSingleElement model.player1.location "2"
            , renderSingleElement model.player2.location "1"
            , renderPlayerArrow model.player1Arrow "A"
            , renderPlayerArrow model.player2Arrow "A"
            , div [] (renderWalls model.walls)
            ]

getPlayersNewLocation : Model -> Player -> Int -> Int -> Player
getPlayersNewLocation model currentLocation deltaX deltaY =
        let
            newLocation = 
                { x = currentLocation.location.x + deltaX, y = currentLocation.location.y - deltaY }
            playerWallElement =
                getWallElement model newLocation
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


getPlayersArrowNewLocation : Model -> PlayerArrow -> PlayerArrow
getPlayersArrowNewLocation model currentLocation =
        let
            newLocation = 
                { x = currentLocation.location.x + currentLocation.direction.x, y = currentLocation.location.y - currentLocation.direction.y }
            playerWallElement =
                getWallElement model newLocation
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


getWallElement : Model -> Location -> Char
getWallElement model location =
        case Array.get location.y model.wallsAsArray of
            Nothing ->
                '#'
            Just val ->
                case Array.get location.x val of
                    Nothing ->
                        '#'
                    Just val ->
                        val


-- --------------------------------------------------------
-- This function is used to render a single element (player, wall, etc) on the screen
-- --------------------------------------------------------

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

renderPlayerArrow : PlayerArrow -> String -> Html msg
renderPlayerArrow playerArrow element =
    if playerArrow.released then
        renderSingleElement playerArrow.location element
    else
        i [] []

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
-- end renderWalls
-- --------------------------------------------------------


-- --------------------------------------------------------
--subscriptions
-- --------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [Sub.map KeyboardMsg Keyboard.Extra.subscriptions    -- for arrow kets
     , Keyboard.presses (\code -> KeyPressed (fromCode code))  -- for other key presses
     ,Time.every (100 * millisecond) MoveArrow]  -- timer, to move arrows
  

-- --------------------------------------------------------
-- main
-- start function ,function that returns an element to draw on the page
-- --------------------------------------------------------

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



