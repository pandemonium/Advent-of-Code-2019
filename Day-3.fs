namespace AdventOfCode

  open System
  open System.IO


  module Day3 =

    let todo<'a> : 'a = failwith "Not yet!"

    type Move =
      | Right of int 
      | Down  of int 
      | Left  of int 
      | Up    of int

    module Move =
      let (|Mnemonic|) (code: string) =
        (code.Chars 0,
         code.Substring 1 |> int)

      let tryParse = function
      | Mnemonic ('R', delta) -> Right delta |> Some
      | Mnemonic ('D', delta) -> Down  delta |> Some
      | Mnemonic ('L', delta) -> Left  delta |> Some
      | Mnemonic ('U', delta) -> Up    delta |> Some
      | _ -> None

    type Path = Move list

    module Path =
      let parse (pathText: string) : Path =
        pathText.Split [| ',' |]
        |> List.ofArray
        |> List.choose Move.tryParse

    type Location = int * int

    let origo = 0, 0

    let manhattanDistance (px, py) (qx, qy) = 
      abs (qx - px) + abs (qy - py)

    let trace (x, y) = 
      function
      | Right delta -> [ for i in 1..delta -> x + i, y ]
      | Down  delta -> [ for i in 1..delta -> x, y + i ]
      | Left  delta -> [ for i in 1..delta -> x - i, y ]
      | Up    delta -> [ for i in 1..delta -> x, y - i ]
      >> List.rev

    let traceSteps =
      let step seen move =
        let prefix =
          trace 
          <| List.head seen
          <| move

        prefix @ seen
      List.fold step [origo]

    let traceStepsWithDistance =
      let distanceByLocation distance location = 
        location, distance

      traceSteps
      >> List.rev
      >> List.mapi distanceByLocation
      >> List.sortBy (fun (_, distance) -> -distance)
      >> Map.ofList
      >> Map.remove origo

    let intersectionsWithDistance (p: Path) (q: Path) = 
      let intersect (m: Map<Location, int>)
                    (n: Map<Location, int>) =
        let intersect intersections location mDistance = 
          match Map.tryFind location n with
          | Some nDistance ->
            (location, mDistance + nDistance) :: intersections
          | None ->
            intersections

        m
        |> Map.fold intersect []

      (traceStepsWithDistance p,
       traceStepsWithDistance q)
      ||> intersect
      |> List.sortBy (fun (_, distance) -> distance)

    let intersections (p: Path) (q: Path) : Location Set =
      let doTrace = traceSteps >> Set.ofList

      (doTrace p, doTrace q)
      ||> Set.intersect
      |> Set.remove origo

    let closestIntersection p q =
      intersections p q
      |> Set.map (manhattanDistance origo)
      |> Set.minElement

    let leastIntersections p q =
      intersectionsWithDistance p q
      |> List.head

    let runWithPuzzleInput computation =
      let lines = 
        File.ReadAllLines "day-3.txt"
        |> List.ofArray

      match lines with
      | p :: q :: _ ->
        let p' = Path.parse p
        let q' = Path.parse q

        leastIntersections p' q'
      
    let computeLeast = 
      runWithPuzzleInput leastIntersections

    let computeClosest = 
      runWithPuzzleInput closestIntersection