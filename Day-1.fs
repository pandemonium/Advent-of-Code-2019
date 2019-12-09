namespace AdventOfCode

open System
open System.IO


module Day1 =
  let todo<'a> : 'a = failwith "Not yet!"

  module List =
    let cons x xs =
      x::xs

    let sequence (xs: 'a option list) : 'a list option =
      let prepend =
        Option.map << cons

      let rec loop acc = function
      | (Some x)::tl -> loop (prepend x acc) tl
      | None::_      -> None
      | []           -> acc

      loop (Some []) xs

    let traverse f =
      List.map f >> sequence

  type Module = int

  let loadModules =
    let parse (s: string) = 
      Int32.TryParse s |> function
      | true, x  -> x |> Some
      | false, _ ->      None

    File.ReadLines
    >> List.ofSeq
    >> List.traverse parse

  let fuelForMass mass : int =
    ((decimal mass) / 3m - 2m) 
      |> floor
      |> int

  let totalFuelForMass =
    List.unfold (fun mass' ->
      let fuel = fuelForMass mass'
      if fuel > 0
        then (fuel, fuel) |> Some
        else None
    )
    >> List.sum

  let fuelRequirements =
    List.sumBy totalFuelForMass

  let fuelRequirementsOfModules =
    loadModules 
    >> Option.map fuelRequirements