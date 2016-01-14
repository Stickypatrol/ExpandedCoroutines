module Math

type Direction =
  | Up
  | Down
  | Left
  | Right

type Position =
    {
        X : int
        Y : int
    }
    with
    static member (+) (p, (x,y)) =
      {X = p.X + x; Y = p.Y + y}