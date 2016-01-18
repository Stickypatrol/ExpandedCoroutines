open Math
open CoroutineMonad
open Entities
open System

let PlayerA = 
    {
      Name = "Player A";
      Icon = 'A';
      ID = 1;
      Position = {X = 10; Y = 5};
      Speed = 1.0;
      Direction = Up
    }

let PlayerB = 
    {
      Name = "Player B";
      Icon = 'B';
      ID = 1;
      Position = {X = 50; Y = 10};
      Speed = -1.0;
      Direction = Up
    };

let DrawMap = Map.empty<Position, char>

let GameState = //first world
  { Bikes =  [
              PlayerA
              PlayerB
            ]
    Barriers = []
    Powerups = []
  }

let rec MainLoop (c1:Coroutine<'s, 's, Unit>) c2 w s =
  let c1', s' = Costep (c1 w s)
  let c2', (dc:Map<Position, char>) = Costep (c2 (w,s') Map.empty)
  //Console.Clear()
  if dc.IsEmpty then printfn "map empty"
  do Map.iter (fun pos (icon:char) -> do Console.SetCursorPosition(pos.X, pos.Y)
                                      do Console.WriteLine(icon)) dc
  let w' = s'
  System.Threading.Thread.Sleep 250
  MainLoop c1' c2' w' s'

do MainLoop World.Update World.Draw GameState GameState


(*
I need to run the update functions
and then get the STATE out of it, use it for drawing
then change the world to the current state
then run again
*)