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
      Update = BikeFields<World>.Update;
      Draw = BikeFields<World>.Draw;
      }

let PlayerB = 
    {Fields = {
                Name = 'B';
                ID = 1;
                Position = {X = 20; Y = 75};
                Speed = 1.0;
                Direction = Right
              };
      Update = BikeFields<World>.Update;
      Draw = BikeFields<World>.Draw;
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
let rec mainloop =
  ()