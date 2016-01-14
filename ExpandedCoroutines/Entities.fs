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

and BikeFields =
    {
      Name          : char
      ID            : int
      Position      : Position
      Speed         : float
      Direction     : Direction
    }with
    static member Update : Coroutine<World, List<GameObject<World, BikeFields, DrawContext>>, Unit> =
      fun (w:World) s ->
        //check for collision
        Done((), s)
    static member Draw : Coroutine<World*BikeFields, DrawContext, Unit> =
      fun (world, bike) map ->
        Done((), map)

and BarrierFields =
  {
    Type          : char
    Position      : Position
    LifeTime      : int
  }with
  static member Update : Coroutine<World, List<GameObject<World, BarrierFields, DrawContext>>, Unit> =
    fun world s ->
      //check for collision
      Done((), s)
  static member Draw : Coroutine<World*BarrierFields, DrawContext, Unit> =
    fun (world, bar) map ->
      Done((), map)

and PowerupFields =
  {
    Type          : PowerupType
    Position      : Position
    LifeTime      : int
  }with
  static member Update : Coroutine<World, List<GameObject<World, PowerupFields, DrawContext>>, Unit> =
    fun world s ->
      //check for collision
      Done((), s)
  static member Draw : Coroutine<World*PowerupFields, DrawContext, Unit> =
    fun (world, power) map ->
      Done((), map)

and World =
  {
    Bikes     : List<GameObject<World, BikeFields, DrawContext>>
    Barriers  : List<GameObject<World, BarrierFields, DrawContext>>
    Powerups  : List<GameObject<World, PowerupFields, DrawContext>>
  }with
  static member Split =
    fun s ->
      s.Bikes, s.Barriers, s.Powerups
  static member Create bikes barrs powers =
    { Bikes = bikes
      Barriers = barrs
      Powerups = powers}
  static member Update : Coroutine<World, World, 'a> =
    cs{
      let! s = GetState
      let bikes, barrs, powers = World.Split s
      let UpdateParts =
        fun w s ->
          let bikes' = BikeFields.Update bikes
          let barrs' = BikeFields.Update barrs
          let powers' = BikeFields.Update powers
      let UpdateBikes() =
        cs{
          let! s = GetState
          do! SetState bikes
          do! BikeFields.Update
          let! s' = GetState
          do! SetState s
          return s'
        }
      let UpdateBarrs() =
        cs{
          let! s = GetState
          do! SetState barrs
          do! BarrierFields.Update
          let! s' = GetState
          do! SetState s
          return s'
        }
      let UpdatePowers() =
        cs{
          let! s = GetState
          do! SetState powers
          do! PowerupFields.Update
          let! s' = GetState
          do! SetState s
          return s'
        }
      let! bikes' = UpdateBikes()
      let! barrs' = UpdateBarrs()
      let! powers' = UpdatePowers()
      do! SetState (World.Create bikes' barrs' powers')
      return ()
    }
    
    
    
    
    (*cs{
      let Divide() =
        fun w s ->
          let bikes, barrs, powers = World.Split s
          let bikes' = (cs{
                          let! bikes' = BikeFields.Update
                          return ()
                        }) w bikes
          let barrs' =  (cs{
                          let! bikes' = BarrierFields.Update
                          return ()
                        }) w bikes
          let powers' = (cs{
                          let! powers' = PowerupFields.Update
                          return ()
                        }) w powers
          Done((World.Create bikes' barrs' powers'), s)
      do! Divide()
    }*)



      (*
      let result = (cs{
                                    let! bikes = World.getBikes
                                    let! barrs = World.getBarrs
                                    let! powers = World.getPowers
                                    return bikes, barrs, powers
                                  }) w s
      Done((),s)
    
    
    fun (w:World) (s:World) ->
      let bikes' = [for bike in bikes do cs{let! hit = BikeFields.Update w bike
                                            if hit then 
                                              yield bike'} w bike]
      let barrs' = (cs{
                      let! barrs = BarrierFields.Update
                      return barrs
                    }) w s.Barriers
      let powers' = (cs{
                      let! powers = PowerupFields.Update
                      return powers
                    }) w s.Powerups
      Done((), s)*)