namespace AdventOfCode

  open System


  module Day4 =

    let todo<'a> : 'a = failwith "Not yet!"

    module Digits =
      let ofNumber =
        string >> List.ofSeq

    let rec hasAdjacentPair = function
    | a::b::rest when a <> b -> 
      b::rest |> hasAdjacentPair
    | a::b::_ -> 
      true
    | _ -> 
      false

    let rec neverDecreases = function
    | a::b::rest when a <= b -> 
      b::rest |> neverDecreases
    | [a] ->
      true
    | [] ->
      true
    | _ ->
      false

    let countPasswords from through =
      seq {
        for p in from .. through do
          let ds = Digits.ofNumber p
          if hasAdjacentPair ds && neverDecreases ds
            then yield p
      }
      |> Seq.length
    