-- This version can render the walls, the player and allow the player to move
--and stops player from going through walls
-- next, shoot arrows

-- elm-make game.elm --output=game.js

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
import Random exposing(..)
import Maybe

-- Modules in this project
import Display
import Map
import Player
import Treasure
import Monster
import CollisionChecker

type Msg
    = KeyboardMsg Keyboard.Extra.Msg
    | KeyPressed Char
    | MoveArrow Time
    | MoveMonsters Time
    | GenerateRandomLocations (List (Int, Int))
    | MoveMonstersWithRandomDirection (List (Int, Int))

type alias Model =
    { pressedKeys : List Key
      , player1 : Player.Player
      , player2 : Player.Player
      , player1Arrow : Player.PlayerArrow
      , player2Arrow : Player.PlayerArrow
      , walls : Map.Walls
      , wallsAsArray : Map.WallsAsArray
      , treasures : List Treasure.Treasure
      , monsters: List Monster.Monster
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
        , wallsAsArray = wallsAsArray
        , treasures = [  ]
        , monsters = [ ]
        }

        -- generate some random points used to place treasure and monsters
     , Random.generate GenerateRandomLocations (list 15 <| Random.pair (int 1 (wallsMaxX - 1)) (int 1 (wallsMaxY - 1)))
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomLocations locations ->
            -- Triggered by the Random.generate statement in init
            ( { model 
                | treasures = Treasure.initTreasures (List.take 10 locations) "$" 
                , monsters = Monster.initMonsters (List.drop 10 locations) "M" }
                , Cmd.none)

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
                , player1 = Player.getPlayersNewLocation model.wallsAsArray model.treasures model.monsters model.player1 arrows.x arrows.y
                , player2 = Player.getPlayersNewLocation model.wallsAsArray model.treasures model.monsters model.player2 wasd.x wasd.y
                , treasures = Treasure.updateTreasure model.treasures model.player1.location model.player2.location
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
                    , monsters = CollisionChecker.updateMonsterList model.monsters model.player1Arrow model.player2Arrow
                 }
                , Cmd.none 
            )

        MoveMonsters time ->  
            -- called with a timer, make the monsters move
            -- generate a new random direction for each monster.  If the monster hits a wall
            -- then the random direction will be used.    
            (   model
                , Random.generate MoveMonstersWithRandomDirection (list (List.length model.monsters) <| Random.pair (int -1 1) (int -1 1))
            )

        MoveMonstersWithRandomDirection randomDirection ->
            -- Make the monsters move, pass in the generated random directions.
            (
                { model | 
                    monsters = Monster.moveMonsters model.wallsAsArray randomDirection model.monsters 
                    ,player1 = Player.checkForMonsterCollision model.player1 model.monsters
                    ,player2 = Player.checkForMonsterCollision model.player2 model.monsters
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
            , div [] (List.map (\x -> Display.renderSingleElement x.location x.element) model.monsters)
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
    [Sub.map KeyboardMsg Keyboard.Extra.subscriptions    -- for arrow keys
     , Keyboard.presses (\code -> KeyPressed (fromCode code))  -- for other key presses
     ,Time.every (50 * millisecond) MoveArrow  -- timer, to move arrows
     ,Time.every (300 * millisecond) MoveMonsters]  -- timer, to move monsters
  

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




