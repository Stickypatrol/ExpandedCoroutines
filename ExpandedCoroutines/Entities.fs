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
      Name          : char
      ID            : int
      Position      : Position
      Speed         : float
      Direction     : Direction
    }with
    static member Remove =
      fun (w:World) (b:GameObject<World, BikeFields, DrawContext>) ->
        let CheckID a b = not (a.Fields.ID = b.Fields.ID)
        let CheckCol a coll = List.exists (fun (e:GameObject<World, BarrierFields, DrawContext>) -> e.Fields.Position = a) coll
        if not(List.exists (fun e -> (CheckID e b && e.Fields.Position = b.Fields.Position)) w.Bikes || CheckCol b.Fields.Position w.Barriers) then
          Done(true,b)
        else
          Done(false,b)
    static member Move =
      fun w (b:GameObject<World, BikeFields, DrawContext>) ->
        let b' =  match b.Fields.Direction with
                  | Up -> {b with Fields = {b.Fields with Position = b.Fields.Position + (0,1)}}
                  | Down -> {b with Fields = {b.Fields with Position = b.Fields.Position + (0,-1)}}
                  | Right -> {b with Fields = {b.Fields with Position = b.Fields.Position + (1,0)}}
                  | Left -> {b with Fields = {b.Fields with Position = b.Fields.Position + (-1,0)}}
        Done((), b')
    static member Update : Coroutine<World, GameObject<World, BikeFields, DrawContext>, bool> =
      cs{
        let! x = BikeFields.Remove
        if x then
          do! BikeFields.Move
          return true
        else
          return false
      }
    static member Draw : Coroutine<World*GameObject<World, BikeFields, DrawContext>, DrawContext, Unit> =
      fun (world, bike) (map:Map<Position, char>) -> //not implemented yet
        let map' = map.Add(bike.Fields.Position, bike.Fields.Name)
        Done((), map')
and BarrierFields =
  {
    ID            : int
    Type          : char
    Position      : Position
    LifeTime      : int
  }with
  static member Remove =
    fun (w:World) (br:GameObject<World, BarrierFields, DrawContext>) ->
      let CheckID a b = not (a.Fields.ID = b.Fields.ID)
      let CheckCol a coll = List.exists (fun (e:GameObject<World, BikeFields, DrawContext>) -> e.Fields.Position = a) coll
      let br' = {br with Fields = {br.Fields with LifeTime = br.Fields.LifeTime-1}}
      if not(CheckCol br'.Fields.Position w.Bikes || List.exists (fun e -> CheckID e br' && e.Fields.Position = br'.Fields.Position) w.Barriers) ||
        br'.Fields.LifeTime = 0 then
        Done(true, br')
      else
        Done(false, br')
  static member Update : Coroutine<World, GameObject<World, BarrierFields, DrawContext>, bool> =
    cs{
      let! x = BarrierFields.Remove
      if x then
        return true
      else
        return false
    }
  static member Draw : Coroutine<World*GameObject<World, BarrierFields, DrawContext>, DrawContext, Unit> =
    fun (world, bar) map -> //not implemented yet
      Done((), map)

and PowerupFields =
  {
    ID            : int
    Type          : PowerupType
    Position      : Position
    LifeTime      : int
  }with
  static member Update : Coroutine<World, GameObject<World, PowerupFields, DrawContext>, bool> =
    fun world ps ->
      let ps' = {ps with Fields = {ps.Fields with LifeTime = ps.Fields.LifeTime-1}}
      if ps'.Fields.LifeTime = 0 then
        Done(true, ps')
      else
        Done(false, ps')
  static member Draw : Coroutine<World*GameObject<World, PowerupFields, DrawContext>, DrawContext, Unit> =
    fun (world, power) dc -> //not implemented yet
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
    {
      Fields = {Bikes = bikes
                Barriers = barrs
                Powerups = powers};
      Update = World.Update;
      Draw = World.Draw;
    }
  static member Update : Coroutine<World, GameObject<World, World, DrawContext>, Unit> =
    cs{
      let! state = GetState
      let! world = GetWorld
      let bikes, barrs, powers = World.Split state.Fields
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
      
      
      (*
      let SeqLooper f =
        fun w s ->
          let rec ForSeq coll coll' f =
            match coll with
            | h::t -> ForSeq t ((f h)coll') f
            | [] -> coll'
          Done((ForSeq s [] f), s)
      let UpdateParts bi br pu =
        fun w s ->
          let bi' = SeqLooper BikeFields.Update w s
      let! bikes' = SeqLooper BikeFields.Update
      return ()
    }

    
    extract 1 list from state
    update all the parts here
    set the state to this
    *)






    (*
      let! s = GetState
      let bikes, barrs, powers = World.Split s.Fields
      let UpdateSequence (coll:List<'a>) =
        fun w s ->
          let SequenceLoop coll 
      do! UpdateParts bikes barrs powers
      let! state' = GetState
      do! SetState state'
      return ()
    *)
  static member Draw : Coroutine<World*GameObject<World, World, DrawContext>, DrawContext, Unit> =
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