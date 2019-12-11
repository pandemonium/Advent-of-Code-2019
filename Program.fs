// Learn more about F# at http://fsharp.org

open System
open AdventOfCode
open Day2.Intcode

[<EntryPoint>]
let main argv =

  let loadText = Loader.parseContent >> Loader.load

  let x = Executive.run "day-5.txt"

  printfn "%d" x

  x
