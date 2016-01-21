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
    Update : Coroutine<'w, 'fs, Unit> //i changed this from a simple 'fs to GameObject<'w, 'fs, 'dc> to make sure it worked for lists, but I'm starting to doubt whether the point was
    Draw   : Coroutine<'w*'fs, 'dc, Unit> //to pass entire lists of GameObjects or just single instances at a time
  }

and BikeFields =
    {
      Name          : string
      Icon          : char
      ID            : int
      Position      : Position
      Speed         : float
      Direction     : Direction
    }with
    static member Remove =
      fun (w:World) (b:BikeFields) ->
        let CheckCol a coll = List.exists (fun (e:BarrierFields) -> e.Position = a) coll
        if List.exists (fun e -> (not(e.ID = b.ID) && e.Position = b.Position)) w.Bikes || CheckCol b.Position w.Barriers then
          Done(true,b)
        else
          Done(false,b)
    static member Move =
      fun w (b:BikeFields) ->
        let b' =  match b.Direction with
                  | Up -> {b with Position = b.Position + (0,1)}
                  | Down -> {b with Position = b.Position + (0,-1)}
                  | Right -> {b with Position = b.Position + (1,0)}
                  | Left -> {b with Position = b.Position + (-1,0)}
        Done((), b')
    static member Update : Coroutine<World, BikeFields, bool> =
      cs{
        let! x = BikeFields.Remove
        if not(x) then
          do! BikeFields.Move
          return false
        else
          return true
      }
    static member Draw : Coroutine<World*BikeFields, DrawContext, Unit> =
      fun (world, bike) (map:Map<Position, char>) -> //not implemented yet
        let map' = map.Add(bike.Position, bike.Icon)
        Done((), map')
and BarrierFields =
  {
    ID            : int
    Icon          : char
    Position      : Position
    LifeTime      : int
  }with
  static member Remove =
    fun (w:World) (br:BarrierFields) ->
      let CheckID a b = not (a.ID = b.ID)
      let CheckCol a coll = List.exists (fun (e:BikeFields) -> e.Position = a) coll
      let br' = {br with LifeTime = br.LifeTime-1}
      if not(CheckCol br'.Position w.Bikes || List.exists (fun e -> CheckID e br' && e.Position = br'.Position) w.Barriers) ||
        br'.LifeTime = 0 then
        Done(true, br')
      else
        Done(false, br')
  static member Update : Coroutine<World, BarrierFields, bool> =
    cs{
      let! x = BarrierFields.Remove
      if not(x) then
        return false
      else
        return true
    }
  static member Draw : Coroutine<World*BarrierFields, DrawContext, Unit> =
    fun (world, bar) map -> //not implemented yet
      let map' = map.Add(bar.Position, bar.Icon)
      Done((), map)

and PowerupFields =
  {
    ID            : int
    Icon          : char
    Type          : PowerupType
    Position      : Position
    LifeTime      : int
  }with
  static member Update : Coroutine<World, PowerupFields, bool> =
    fun world ps ->
      let ps' = {ps with LifeTime = ps.LifeTime-1}
      if ps'.LifeTime = 0 then
        Done(true, ps')
      else
        Done(false, ps')
  static member Draw : Coroutine<World*PowerupFields, DrawContext, Unit> =
    fun (world, power) dc -> //not implemented yet
      Done((), dc)

and World =
  {
    Bikes     : List<BikeFields>
    Barriers  : List<BarrierFields>
    Powerups  : List<PowerupFields>
  }with
  static member Split =
    fun s ->
      s.Bikes, s.Barriers, s.Powerups
  static member Create bikes barrs powers =
    { Bikes = bikes
      Barriers = barrs
      Powerups = powers
    }
  static member Update : Coroutine<World, World, Unit> =
    cs{
      let! state = GetState
      let! world = GetWorld
      let bikes, barrs, powers = World.Split state
      let bikes' = [for b in bikes do let x, b' = End BikeFields.Update world b
                                      if not(x) then
                                        yield b']
      let barrs' = [for b in barrs do let x, b' = End BarrierFields.Update world b
                                      if not(x) then
                                        yield b']
      let powers' = [for p in powers do let x, p' = End PowerupFields.Update world p
                                        if not(x) then
                                          yield p']
      do! SetState (World.Create bikes' barrs' powers')
      return ()
    }
  static member Draw : Coroutine<World*World, DrawContext, Unit> =
    cs{
      let! w, worlds = GetWorld
      let bikes, barrs, powers = World.Split w
      let DrawPart bikes barrs powers : Coroutine<(World*World), DrawContext, DrawContext>=
        fun ((w:World), (s:World)) dc ->
          let bikesdc' = List.fold (fun map bike -> let map' = snd(End BikeFields.Draw (w, bike) map)
                                                    (Map.fold (fun (map:Map<Position, char>) key value -> (map.Add(key, value))) Map.empty map')) dc bikes
          let barrsdc' = List.fold (fun map bike -> let map' = snd(End BarrierFields.Draw (w, bike) map)
                                                    (Map.fold (fun (map:Map<Position, char>) key value -> (map.Add(key, value))) Map.empty map')) bikesdc' barrs
          let completedc = List.fold (fun map bike -> let map' = snd(End PowerupFields.Draw (w, bike) map)
                                                      (Map.fold (fun (map:Map<Position, char>) key value -> (map.Add(key, value))) Map.empty map')) barrsdc' powers
          Done(completedc, completedc)
      let! dc = DrawPart bikes barrs powers
      do! SetState dc
      return ()
    }