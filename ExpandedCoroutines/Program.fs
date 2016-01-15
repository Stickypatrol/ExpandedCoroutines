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

let rec UpdateLoop w =
  fun w s ->
    let  = World.Update w s


let rec mainloop =
  cs{
    do! repeat ()
  }