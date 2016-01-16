module CoroutineMonad

type Coroutine<'w, 's, 'a> = 'w -> 's -> CoroutineStep<'w, 's, 'a>
and CoroutineStep<'w, 's, 'a> =
  | Done of 'a*'s
  | Yield of Coroutine<'w, 's, 'a>*'s

let rec bind (p: Coroutine<'w, 's, 'a>, k: 'a -> Coroutine<'w, 's, 'b>): Coroutine<'w, 's, 'b> =
    fun w s ->
      match p w s with
      | Done (x, s') -> k x w s'
      | Yield (p', s') -> Yield(bind(p', k), s')
let ret x = fun w s -> Done (x, s)

let (>>=) = bind

type CoroutineBuilder() =
  member this.Return(x: 'a): Coroutine<'w, 's, 'a> = ret x
  member this.ReturnFrom(s: Coroutine<'w, 's, 'a>) = s
  member this.Bind(p, k) = bind(p, k)
  member this.For(s:seq<'a>, k:'a->Coroutine<'w, 's, Unit>) : Coroutine<'w, 's, Unit> =
    if s |> Seq.isEmpty then
      ret ()
    else
      this.Bind(k(s |> Seq.head), fun () -> this.For(s |> Seq.tail, k))
let cs = CoroutineBuilder()

let GetOnlyState costep =
  match costep with
  | Done(a, s) -> s
  | Yield(c', s) -> s

let GetState : Coroutine<'w, 's, 's> =
  fun w s ->
    Done(s, s)

let SetState newState : Coroutine<'w, 's, Unit> =
  fun w s ->
    Done((), newState)

let GetWorld : Coroutine<'w, 's, 'w> =
  fun w s ->
    Done(w, s)

let rec repeat s =
  cs{
    do! s
    return! repeat s
  }

let yield_ = fun w s -> Yield((fun w s -> Done((),s)),s)

let wait interval =
  let time = fun w s -> Done(System.DateTime.Now, s)
  cs{
    let! t0 = time
    let rec wait() =
      cs{
        let! t = time
        let dt = (t-t0).TotalSeconds
        if dt > interval then
          return ()
        else
          return! wait()
      }
    do! wait()
  }

let Costep c =
  match c with
  | Done(a, s') -> cs{return a}, s'
  | Yield(c', s') -> c', s'

let rec End c w s =
  match c w s with
  | Done(a, s) -> a, s
  |Yield(c', s') -> End c' w s'