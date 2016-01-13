module Entities

open CoroutineMonad
open Math
open Microsoft.Xna.Framework.Graphics

type DrawContext = Map<Position, char>

type PowerupType =
  | Speed
  | Life
  | Bomb

type GameObject<'w, 'fs, 'dc> =
  {
    Fields : 'fs
    Update : Coroutine<'w, 'fs, Unit>
    Draw   : Coroutine<'w*'fs, 'dc, Unit>
  }
  
type BikeFields<'w> =
  {
    Name          : char
    ID            : int
    Position      : Position
    Speed         : float
    Direction     : Direction
  }with
  static member Update : Coroutine<'w, List<GameObject<'w, BikeFields<'w>, DrawContext>>, bool> =
    fun (w:'w) (s:List<GameObject<'w, BikeFields<'w>, DrawContext>>) ->
      
      //check for collision
      Done(false, s)
  static member Draw : Coroutine<'w*BikeFields<'w>, DrawContext, Unit> =
    fun (world, bike) map ->
      Done((), map)

type BarrierFields<'w> =
  {
    Type          : char
    Position      : Position
    LifeTime      : int
  }with
  static member Update : Coroutine<'w, BarrierFields<'w>, bool> =
    fun world bar ->
      //check for collision
      Done(false, bar)
  static member Draw : Coroutine<'w*BarrierFields<'w>, DrawContext, Unit> =
    fun (world, bar) map ->
      Done((), map)

type PowerupFields<'w> =
  {
    Type          : PowerupType
    Position      : Position
    LifeTime      : int
  }with
  static member Update : Coroutine<'w, BarrierFields<'w>, bool> =
    fun world power ->
      //check for collision
      Done(false, power)
  static member Draw : Coroutine<'w*BarrierFields<'w>, DrawContext, Unit> =
    fun (world, power) map ->
      Done((), map)

type World =
  {
    Bikes     : List<GameObject<World, BikeFields<World>, DrawContext>>
    Barriers  : List<GameObject<World, BarrierFields<World>, DrawContext>>
    Powerups  : List<GameObject<World, PowerupFields<World>, DrawContext>>
  }with
  static member Create bikes barrs powers =
    fun w s ->
      Done((),
        { Bikes = bikes
          Barriers = barrs
          Powerups = powers})
  static member Update : Coroutine<World, World, Unit> =
    fun (w:World) (s:World) ->
      let bikes' = (cs{
                      let! bikes = BikeFields<World>.Update
                      return bikes
                    }) w s.Bikes
      let barrs' = (cs{
                      let! barrs = BarrierFields<World>.Update
                      return barrs
                    }) w s.Barriers
      let powers' = (cs{
                      let! powers = PowerupFields<World>.Update
                      return powers
                    }) w s.Powerups
      Done((), s)