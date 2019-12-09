// Learn more about F# at http://fsharp.org

open System
open AdventOfCode

[<EntryPoint>]
let main argv =

    Day1.fuelRequirementsOfModules "module-mass.txt"
    |> (Option.iter <| printfn "%d")


    0 // return an integer exit code
