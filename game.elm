-- This version can render the walls, the player and allow the player to move
--and stops player from going through walls
-- next, shoot arrows

-- elm-make game7.elm --output=game.js

-- exmaple wiht multiple messages and subscriptions
-- https://github.com/freakingawesome/drunk-label/blob/master/src/DrunkLabel.elm

module Main exposing (..)


-- 3rd party modules
import Html exposing (Html, div, p, ul, li, text, map, i)
import Keyboard exposing (..)
import Keyboard.Extra exposing (Key(..))
import Array exposing (Array)
import Time exposing (Time, second, millisecond)
import Char exposing (..)

-- Modules in this project
import Display
import Map
import Player
import Treasure

type Msg
    = KeyboardMsg Keyboard.Extra.Msg
    | KeyPressed Char
    | MoveArrow Time


type alias Model =
    { pressedKeys : List Key
      , player1 : Player.Player
      , player2 : Player.Player
      , player1Arrow : Player.PlayerArrow
      , player2Arrow : Player.PlayerArrow
      , walls : Map.Walls
      , wallsAsArray : Map.WallsAsArray
      , treasures : List Treasure.Treasure
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
        , player1 = Player.initPlayer { x = 2, y = 2 }
        , player2 = Player.initPlayer { x = 10, y = 2 }
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
        , treasures = [ Treasure.initTreasure { x = 4, y = 4} "$", Treasure.initTreasure { x = 6, y = 6} "$" ]
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
                , player1 = Player.getPlayersNewLocation model.wallsAsArray model.treasures model.player1 arrows.x arrows.y
                , player2 = Player.getPlayersNewLocation model.wallsAsArray model.treasures model.player2 wasd.x wasd.y
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
            ( 
                 {
                     model
                    | player1Arrow = Player.getPlayersArrowNewLocation model.wallsAsArray model.player1Arrow
                    , player2Arrow = Player.getPlayersArrowNewLocation model.wallsAsArray model.player2Arrow
                 }
                , Cmd.none 
            )

view : Model -> Html msg
view model =
        div []
            [
              Display.renderSingleElement model.player1.location "2"
            , Display.renderSingleElement model.player2.location "1"
            , renderPlayerArrow model.player1Arrow "A"
            , renderPlayerArrow model.player2Arrow "A"
            , div [] (Display.renderWalls model.walls)
            , div [] (List.map (\x -> Display.renderSingleElement x.location x.element) model.treasures)
            , div [] (Display.renderScore { x = 10, y=25 } "2" model.player1.score)
            , div [] (Display.renderScore { x = 20, y=25 } "1" model.player2.score)
            ]


renderPlayerArrow : Player.PlayerArrow -> String -> Html msg
renderPlayerArrow playerArrow element =
    if playerArrow.released then
        Display.renderSingleElement playerArrow.location element
    else
        i [] []


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



