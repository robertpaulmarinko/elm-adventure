module CollisionChecker exposing (updateMonsterList)

import Player exposing (PlayerArrow)
import Monster exposing (Monster)

updateMonsterList : List Monster -> Player.PlayerArrow -> Player.PlayerArrow -> List Monster
updateMonsterList monsterList player1Arrow player2Arrow =
    -- return all monsters that do NOT match the arrow location.  So if a arrow
    -- hits a monster, this will cause the monster to go away
    List.filter (\t -> (arrowNotHitMonster player1Arrow t) && (arrowNotHitMonster player2Arrow t)) monsterList

arrowNotHitMonster : Player.PlayerArrow -> Monster -> Bool
arrowNotHitMonster arrow monster =
    monster.location.x /= arrow.location.x || monster.location.y /= arrow.location.y || arrow.released == False
