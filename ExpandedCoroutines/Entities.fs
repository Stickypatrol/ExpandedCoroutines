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
  static member Remove e =
    fun w s ->
      
  static member Update : Coroutine<'w, BikeFields<'w>, bool> =
    fun world bike ->
      //check for collision
      Done(false, bike)
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
    { Bikes = bikes
      Barriers = barrs
      Powerups = powers}
  static member Update : Coroutine<World, World, Unit> =
    fun w s ->
      let bikes' = BikeFields.Update
      let barrs' = BarrierFields.Update
      let powers' = PowerupFields.Update
      Done((), World.Create bikes' barrs' powers')