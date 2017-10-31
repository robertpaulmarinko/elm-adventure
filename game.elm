-- This version can render the walls, the player and allow the player to move
--and stops player from going through walls
-- next, shoot arrows

-- elm-make game.elm --output=game.js

-- exmaple wiht multiple messages and subscriptions
-- https://github.com/freakingawesome/drunk-label/blob/master/src/DrunkLabel.elm

module Main exposing (..)


-- 3rd party modules
import Html exposing (Html, div, p, ul, li, text, map, i, button)
import Html.Events exposing (onClick)
import Keyboard exposing (..)
import Keyboard.Extra exposing (Key(..))
import Time exposing (Time, second, millisecond)
import Char exposing (..)
import Random exposing(..)
import Http exposing(..)
import Json.Decode exposing (..)

-- Modules in this project
import Display
import Map
import Player
import Treasure
import Monster
import CollisionChecker

type Msg 
    = StartGame
    | KeyboardMsg Keyboard.Extra.Msg
    | KeyPressed Char
    | MoveArrow Time
    | MoveMonsters Time
    | GenerateRandomLocations (List (Int, Int))
    | MoveMonstersWithRandomDirection (List (Int, Int))
    | MapLoaded  (Result Http.Error Map.MapJson)


type alias Model =
    { pressedKeys : List Key
      , player1 : Player.Player
      , player2 : Player.Player
      , player1Arrow : Player.PlayerArrow
      , player2Arrow : Player.PlayerArrow
      , map : Map.Map
      , treasures : List Treasure.Treasure
      , monsters: List Monster.Monster
      , gameStarted: Bool
    }


-- --------------------------------------------------------
-- init
-- runs when the page first loads
-- --------------------------------------------------------
init : ( Model, Cmd Msg )
init =
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
        , map = Map.initEmptyMap
        , treasures = [  ]
        , monsters = [ ]
        , gameStarted = False
        }
     , Cmd.none
    )

-- --------------------------------------------------------
-- update
-- runs whenever an event occurs, updates the mode
-- based on the event
-- --------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( 
                model
                -- make a HTTP call to get the map
                , loadMap "entrance"
            )

        -- called after map has been loaded using HTTP request
        MapLoaded (Ok mapJson) ->
            let
                map = Map.loadMap mapJson
            in
                ( 
                    {model  
                        | map = map
                        , gameStarted = True
                    }
                    -- generate some random points used to place treasure and monsters
                    , Random.generate GenerateRandomLocations (Random.list 15 <| Random.pair (Random.int 1 (map.wallsMaxX - 1)) (Random.int 1 (map.wallsMaxY - 1)))
                )


        MapLoaded (Err _) ->
            -- TODO - should add something here to handle errors
            (model, Cmd.none)

        GenerateRandomLocations locations ->
            -- Triggered by the Random.generate statement in MapLoaded
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
                , player1 = Player.getPlayersNewLocation model.map.wallsAsArray model.treasures model.monsters model.player1 arrows.x arrows.y
                , player2 = Player.getPlayersNewLocation model.map.wallsAsArray model.treasures model.monsters model.player2 wasd.x wasd.y
                , treasures = Treasure.updateTreasure model.treasures model.player1.location model.player2.location
                -- TODO - reload map if player entered a door
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
                    | player1Arrow = Player.getPlayersArrowNewLocation model.map.wallsAsArray model.player1Arrow
                    , player2Arrow = Player.getPlayersArrowNewLocation model.map.wallsAsArray model.player2Arrow
                    , monsters = CollisionChecker.updateMonsterList model.monsters model.player1Arrow model.player2Arrow
                 }
                , Cmd.none 
            )

        MoveMonsters time ->  
            -- called with a timer, make the monsters move
            -- generate a new random direction for each monster.  If the monster hits a wall
            -- then the random direction will be used.    
            (   model
                , Random.generate MoveMonstersWithRandomDirection (Random.list (List.length model.monsters) <| Random.pair (Random.int -1 1) (Random.int -1 1))
            )

        MoveMonstersWithRandomDirection randomDirection ->
            -- Make the monsters move, pass in the generated random directions.
            (
                { model | 
                    monsters = Monster.moveMonsters model.map.wallsAsArray randomDirection model.monsters 
                    ,player1 = Player.checkForMonsterCollision model.player1 model.monsters
                    ,player2 = Player.checkForMonsterCollision model.player2 model.monsters
                }
                , Cmd.none
            )

-- make a HTTP request to load a map
loadMap : String -> Cmd Msg
loadMap mapName =
  let
    url =
      "/maps/" ++ mapName ++ ".json"

    request =
      Http.get url decodeMapJson
  in
    Http.send MapLoaded request

-- called after the HTTP get request is finished, decodes
-- the JSON that was retrived.
decodeMapJson : Decoder Map.MapJson
decodeMapJson =
  Json.Decode.map2 Map.MapJson
    (field "walls" (Json.Decode.list Json.Decode.string))
    (field "doors" (Json.Decode.list Json.Decode.string))

-- --------------------------------------------------------
-- view
-- --------------------------------------------------------
  
view : Model -> Html Msg
view model =
    if model.gameStarted then
        div []
            [ 
             Display.renderSingleElement model.player1.location "Player2"
            , Display.renderSingleElement model.player2.location "Player1"
            , renderPlayerArrow model.player1Arrow "A"
            , renderPlayerArrow model.player2Arrow "A"
            , div [] (Display.renderWalls model.map.walls)
            , div [] (List.map (\x -> Display.renderSingleElement x.location x.element) model.treasures)
            , div [] (List.map (\x -> Display.renderSingleElement x.location x.element) model.monsters)
            , div [] (Display.renderScore { x = 10, y=25 } "2" model.player1.score)
            , div [] (Display.renderScore { x = 20, y=25 } "1" model.player2.score)
            ]
    else
        div []
            [ button [ onClick StartGame ] [ text "Start Game" ]
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




