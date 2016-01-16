open Math
open CoroutineMonad
open Entities

let PlayerA = 
    {Fields = {
                Name = 'A';
                ID = 1;
                Position = {X = 5; Y = 5};
                Speed = 1.0;
                Direction = Right
              };
      Update = BikeFields.Update;
      Draw = BikeFields.Draw;
      }

let PlayerB = 
    {Fields = {
                Name = 'B';
                ID = 1;
                Position = {X = 20; Y = 75};
                Speed = 1.0;
                Direction = Right
              };
      Update = BikeFields.Update;
      Draw = BikeFields.Draw;
      }

let DrawMap = Map.empty<Position, char>

let GameState = //first world
  { Fields = {Bikes = [
                        PlayerA;
                        PlayerB;
                      ];
              Barriers = [];
              Powerups = [];
            };
    Update = World.Update;
    Draw = World.Draw
  }

let rec MainLoop (c1:Coroutine<'s, 's, Unit>) c2 w s =
  let c1', s' = co_step (c1 w s)
  let c2', _ = co_step (c2 (w,s) Map.empty)
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