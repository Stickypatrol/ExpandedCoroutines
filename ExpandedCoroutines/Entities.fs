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
    Update : Coroutine<'w, List<GameObject<'w, 'fs, 'dc>>, Unit> //i changed this from a simple 'fs to GameObject<'w, 'fs, 'dc> to make sure it worked for lists, but I'm starting to doubt whether the point was
    Draw   : Coroutine<'w*List<GameObject<'w, 'fs, 'dc>>, 'dc, Unit> //to pass entire lists of GameObjects or just single instances at a time
  }

and BikeFields =
    {
      Name          : char
      ID            : int
      Position      : Position
      Speed         : float
      Direction     : Direction
    }with
    static member Collision =
      fun (w:World) (bs:List<GameObject<World, BikeFields, DrawContext>>) ->
        let CheckID a b = not (a.Fields.ID = b.Fields.ID)
        let CheckCol a coll = List.exists (fun (e:GameObject<World, BarrierFields, DrawContext>) -> e.Fields.Position = a) coll
        let bs' = [for b in bs do if not(List.exists (fun e -> (CheckID e b && e.Fields.Position = b.Fields.Position)) w.Bikes ||
                                         CheckCol b.Fields.Position w.Barriers) then yield b]
        Done((),bs')
    static member Move =
      fun w (bs:List<GameObject<World, BikeFields, DrawContext>>) ->
        let bs' = [for b in bs do match b.Fields.Direction with
                                  | Up -> yield {b with Fields = {b.Fields with Position = b.Fields.Position + (0,1)}}
                                  | Down -> yield {b with Fields = {b.Fields with Position = b.Fields.Position + (0,-1)}}
                                  | Right -> yield {b with Fields = {b.Fields with Position = b.Fields.Position + (1,0)}}
                                  | Left -> yield {b with Fields = {b.Fields with Position = b.Fields.Position + (-1,0)}}
                  ]
        
        Done((), bs')
    static member Update : Coroutine<World, List<GameObject<World, BikeFields, DrawContext>>, Unit> =
      cs{
        do! BikeFields.Collision
        do! BikeFields.Move
        return ()
      }
    static member Draw : Coroutine<World*List<GameObject<World, BikeFields, DrawContext>>, DrawContext, Unit> =
      fun (world, bikes) (map:Map<Position, char>) ->
        let rec addToMap = //add this function
          
        Done((), map)

and BarrierFields =
  {
    ID            : int
    Type          : char
    Position      : Position
    LifeTime      : int
  }with
  static member Collision =
    fun (w:World) (br:List<GameObject<World, BarrierFields, DrawContext>>) ->
      let CheckID a b = not (a.Fields.ID = b.Fields.ID)
      let CheckCol a coll = List.exists (fun (e:GameObject<World, BikeFields, DrawContext>) -> e.Fields.Position = a) coll
      let br' = [for barr in br do if not(CheckCol barr.Fields.Position w.Bikes ||
                                          List.exists (fun e -> CheckID e barr && e.Fields.Position = barr.Fields.Position) w.Barriers) then
                                    yield barr]
      Done((),br')
  static member DecrLifetime=
    cs{
      let decr() =
        fun w (br:List<GameObject<World, BarrierFields, DrawContext>>) ->
          let br' = [for (barr:GameObject<World, BarrierFields, DrawContext>) in br do
                        if barr.Fields.LifeTime > 0 then
                          yield {barr with 
                                  Fields = {barr.Fields with
                                              LifeTime = barr.Fields.LifeTime - 1}}]
          Done((), br')
      do! decr()
      return ()
    }
  static member Update : Coroutine<World, List<GameObject<World, BarrierFields, DrawContext>>, Unit> =
    cs{
      do! BarrierFields.DecrLifetime
      do! BarrierFields.Collision
      return ()
    }
  static member Draw : Coroutine<World*List<GameObject<World, BarrierFields, DrawContext>>, DrawContext, Unit> =
    fun (world, bar) map ->
      Done((), map)

and PowerupFields =
  {
    ID            : int
    Type          : PowerupType
    Position      : Position
    LifeTime      : int
  }with
  static member Update : Coroutine<World, List<GameObject<World, PowerupFields, DrawContext>>, Unit> =
    fun world ps ->
      Done((), ps)
  static member Draw : Coroutine<World*List<GameObject<World, PowerupFields, DrawContext>>, DrawContext, Unit> =
    fun (world, power) dc ->
      Done((), dc)

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
  static member Update : Coroutine<World, List<GameObject<World, World, DrawContext>>, Unit> =
    cs{
      let! s = GetState
      let bikes, barrs, powers = World.Split s.Head.Fields
      let UpdateParts bikes barrs powers =
        fun w s ->
          let bikes' = BikeFields.Update w bikes
          let barrs' = BarrierFields.Update w barrs
          let powers' = PowerupFields.Update w powers
          Done((), s)
      do! UpdateParts bikes barrs powers
      let! state' = GetState
      do! SetState state'
      return ()
    }
  static member Draw : Coroutine<World*List<GameObject<World, World, DrawContext>>, DrawContext, Unit> =
    cs{
      let! w, worlds = GetWorld
      let bikes, barrs, powers = World.Split w
      let DrawParts bikes barrs powers =
        fun (w, ws) dc ->
          let bikes' = (BikeFields.Draw (w, bikes) dc)
          let barrs' =  (BarrierFields.Draw (w, barrs) dc)
          let powers' = (PowerupFields.Draw (w, powers) dc)
          Done((), dc)
      do! DrawParts bikes barrs powers
      let! dc' = GetState
      do! SetState dc'
      return ()
    }