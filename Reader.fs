namespace Monad

  type Reader<'e, 'a> = Reader of ('e -> 'a)

  module Reader =
    let make = Reader

    let run environment = function
    | Reader action -> action environment

    let bind (f: 'a -> Reader<'e, 'b>) (r: Reader<'e, 'a>) =
      Reader
      <| fun e ->
          run e r |> f
          |> run e

    let bind2 (f: 'a -> Reader<'e, 'b>) = function
    | Reader action ->
      Reader
      <| fun e ->
           (action >> f) e
           |> run e
         