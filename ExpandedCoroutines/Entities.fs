module Entities

open Monad
open Math
open Microsoft.Xna.Framework.Graphics

type DrawContext = Map<Position, char>

type GameObject<'w, 'fs, 'dc> =
  {
    Fields : 'fs
    Update : Coroutine<'w, 'fs, Unit>
    Draw   : Coroutine<'w*'fs, 'dc, Unit>
  }

type BikeFields =
  {
    Name          : char
    ID            : int
    Position      : Position
    Speed         : float
    Direction     : Direction
  }with
  static member Update =
    cs{
      let! newbike = functionthatreturns-Done(newbike, state)
    }
  static member Draw =
    fun 

type BarrierFields =
  {
    Type          : char
    Position      : Position
    LifeTime      : int
  }

type PowerupType =
  | Speed
  | Life
  | Bomb

type PowerupFields =
  {
    Type          : PowerupType
    Position      : Position
    LifeTime      : int
  }

type World =
  {
    Bikes     : List<GameObject<World, BikeFields, DrawContext>>
    Barriers  : List<GameObject<World, BarrierFields, DrawContext>>
    Powerups  : List<GameObject<World, PowerupFields, DrawContext>>
  }